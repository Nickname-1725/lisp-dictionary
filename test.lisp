"本脚本用于实现单词本"
(load "trie-store.lisp")
(load "vocabulary.lisp")
(load "flow-chart.lisp")
(defparameter *words-db* nil)

;;;; 数据结构的存取、管理
(defun add-word (word) (push word *words-db*))
; 其中(add-word(create-word spell))
; 可以用(mark-word (add-word spell) (define-word spell))代替

(defun set-word (word key value)
  "设置特定单词的关键字值，value应为字符串"
  (setf (getf word key) value))
; 可以用(correct-def-by-id id class-string correct-string index)代替
; 可以用(push-def-by-id id class-string def-string)代替
; 可以用(remove-def-by-id id class-string index)代替
(defun find-word (spell)
  "从字典中查找单词，若无则返回nil"
  (car (remove-if-not
        (lambda (word) (eql spell (getf word :spell)))
        *words-db*)))
; 可以用(find-word spell)代替

(defun remove-word-spell (spell)
  (setf *words-db* (remove-if
                    #'(lambda (word) (eq spell (getf word :spell)))
                    *words-db*)))
; 暂无代替
(defun clean-class-word (word key)
  (set-word word key nil))
; 代替见上，或者也可以不代替

(defun display-word (word)
  (flet ((display-class-word (word key)
           (if (getf word key)
               (format t "~% ~a.~7t~a" key (getf word key)))))
    (format t ">>> ~a" (getf word :spell))
    (display-class-word word :n)
    (display-class-word word :v)
    (display-class-word word :adj)
    (display-class-word word :adv)
    (display-class-word word :prep)
    (format t "~%")))
; 可以使用(describe-def-by-id id)

;;;; 数据库的存档与加载
(defun save-db (data-base filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-standard-io-syntax
      (print data-base out))))
(defmacro load-db (data-base filename)
  `(let ((file-exists (probe-file ,filename)))
     (when file-exists
         (with-open-file (in ,filename
                        :if-does-not-exist :error)
     (with-standard-io-syntax
       (setf ,data-base (read in)))))))

(defparameter *config-root* "./config/")
(defun save-words ()
  (save-db *words-db* (concatenate 'string *config-root* "dictionary-words.db")))
(defun load-words ()
  (load-db *words-db* (concatenate 'string *config-root* "dictionary-words.db")))

;;;; 用户交互功能

;(defun user-read ()
;  "通用解析用户输入函数"
;  (let ((cmd (read-from-string
;              (concatenate 'string "(" (read-line) ")" ))))
;    (flet ((quote-it (x)
;             (list 'quote x)))
;      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;(defmacro user-eval* (allow-cmds)
;  "模板，生成user-eval类型的函数，输入参数为允许的命令列表及允许词数
;  allow-cmds: 应形如((command-1 3) (command-2 1))"
;  `(lambda (sexp)
;     (format t "~c[2J~c[H" #\escape #\escape)
;     (let* ((allow-cmds ,allow-cmds)
;            (find-cmd (assoc (car sexp) allow-cmds)))
;       (if (and find-cmd
;                (eq (length sexp) (cadr find-cmd)))
;           (eval sexp)
;           (format t "Not a valid command. (✿ ◕ __ ◕ )~%")))))

;;; 子repl模板
(defun user-cmd-description (cmd-desc)
  "依次打印命令的描述"
  (format t "~{~{- [~a~15t]: ~a~}~%~}" cmd-desc))

(defparameter *the-word* nil)
(defmacro user-repl* (cmd-desc u-eval)
  "子repl函数生成宏"
  `(lambda (spell)
     (setf *the-word* (find-word spell))
     (let ((word *the-word*))
       (labels
           ((repl (word)
              ; 此处显示查询单词的情况
              (if *the-word*
                  (progn
                    ;(format t "~c[2J~c[H" #\escape #\escape)
                    (format t "The target *~a* found. (˵u_u˵)~%~%" spell)
                    (display-word word))
                  (progn
                    ;(format t "~c[2J~c[H" #\escape #\escape)
                    (format t "The taget *~a* does not exist. (ﾉ ◕ ヮ ◕ )ﾉ~%~%" spell)))
              ; 反馈可用命令
              (user-cmd-description ,cmd-desc)
              ; 执行用户命令
              (let ((cmd (user-read)))
                (if (eq (car cmd) 'back)
                    (format t "~c[2J~c[H" #\escape #\escape)
                    (progn (funcall ,u-eval cmd)
                           (repl word))))))
         (repl word)))))

;; 主REPL命令集
(defun note-down (spell)
  (let ((word (find-word spell)))
                                        ; 此处显示查询单词的情况
    (if word
        (progn
          (format t "*~a* has already in our database.~%" spell)
          (read-line))
        (progn
          (add-word (create-word spell))
          (format t "The target *~a* has been add to our database.~%" spell)
          (read-line)
          (edit spell)))))

(defparameter look-up-call
  (user-repl*
   '(("back" "go back to the main menu."))
   (user-eval* '((back 1)))))
(defparameter edit-call
  (user-repl*
   '(("back" "go back to the main menu.")
     ("change :key new-meaning" "to change part of the speech of the target."))
   (user-eval* '((back 1) (change 3)))))
(defparameter erase-call
  (user-repl*
   '(("back" "go back to the main menu.")
     ("wipe :key" "to wipe off part of the speech of the target.")
     ("wipe-clean" "to wipe off the whole target clean."))
   (user-eval* '((back 1) (wipe 2) (wipe-clean 1)))))
;(defmacro look-up (spell) `(funcall look-up-call ,spell))
;(defmacro edit (spell) `(funcall edit-call ,spell))
;(defmacro erase (spell) `(funcall erase-call ,spell))
;(defun restore ()
;  (save-words)
;  (format t "Neatly done.~%")
;  (read-line))
;(defun quit-the-main-repl ()
;  (save-words) ; 自动存档
;  (format t "The dictionary closed. Goodbye. (⌐ ■ ᴗ ■ )~%"))

;; 子repl命令集
(defun change (key value)
  (set-word *the-word* key (prin1-to-string value)))
(defun wipe (key)
  (clean-class-word *the-word* key))
(defun wipe-clean ()
  (if (not *the-word*)
      (progn
        (format t "Quite clean. Nothing to wipe off.")
        (read-line))
      (progn (format t "are you sure you want to wipe the hole target *~a* clean? (˵u_u˵)[y/n]"
                     (getf *the-word* :spell))
             (let* ((r-l (read-line))
                    (option (read-from-string
                             (if (eq (length r-l) 0)
                                 "default" r-l))))
               (cond ((eq 'y option)
                      (remove-word-spell (getf *the-word* :spell))
                      (setf *the-word* nil)
                      (format t "~c[2J~c[H" #\escape #\escape)
                      (format t "Neatly-done.~%")
                      (read-line))
                     ((eq 'n option))
                     (t (format t "yes or no?[y/n]~%")
                        (wipe-clean)))))))

;;; 测试用例
(flow-chart:def-init *repl-user* 'main
  (format t "The dictionary opened. Wellcome back. ( ✿ ◕ ‿ ◕ )~%")
  (user-cmd-description              ; 反馈可用命令
   '(("note-down spell" "note-down a word.")
     ("look-up spell" "look up the dictionary for a word.")
     ("edit spell" "correct the fault.")
     ("erase spell" "give it a quick trim or eliminate it completely.")
     ("restore" "restore the data manually.")
     ("quit" "close the dictionary. data will be automatically restored by your little helper.(˵ ✿ ◕ ‿ ◕ ˵)"))))
(flow-chart:def-arc *repl-user* 'main 'main '() ; 错误通配
  (format t "~c[2J~c[H" #\escape #\escape)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)
(flow-chart:def-arc *repl-user* 'main 'main '(quit) ; 退出程序
  ;(save-words) ; 自动存档
  (format t "The dictionary closed. Goodbye. (⌐ ■ ᴗ ■ )~%"))

;; note-down
(flow-chart:def-state *repl-user* 'note-down
  (format t "Did you just entered ~a?" args))
(flow-chart:def-arc *repl-user* 'main 'note-down '(note-down symbol)
  (format t "Hello. You're a ~a.~%" (cadr cmd-list))
  (let ((args (cadr cmd-list)))
    'target))

;; look-up
(flow-chart:def-state *repl-user* 'look-up
  (format t "Did you just entered ~a?" args))
(flow-chart:def-arc *repl-user* 'main 'look-up '(look-up symbol)
  (format t "Hello. You're a ~a.~%" (cadr cmd-list))
  (let ((args (cadr cmd-list)))
    'target))

;; edit
(flow-chart:def-state *repl-user* 'edit
  (format t "Did you just entered ~a?" args))
(flow-chart:def-arc *repl-user* 'main 'edit '(edit symbol)
  (format t "Hello. You're a ~a.~%" (cadr cmd-list))
  (let ((args (cadr cmd-list)))
    'target))

;; erase
(flow-chart:def-state *repl-user* 'erase
  (format t "Did you just entered ~a?" args))
(flow-chart:def-arc *repl-user* 'main 'erase '(erase symbol)
  (format t "Hello. You're a ~a.~%" (cadr cmd-list))
  (let ((args (cadr cmd-list)))
    'target))

;; store
(flow-chart:def-state *repl-user* 'store
  (format t "Neatly done.~%"))
(flow-chart:def-arc *repl-user* 'main 'store '(store)
  ;(save-words)
  'target)
(flow-chart:def-arc *repl-user* 'store 'main '()
  'target)

(defun init-fun ()
;  (load-words) ; 自动加载存档
  (main-repl))
;  (sleep 0.1)
;  (quit))

