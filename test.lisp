"本脚本用于实现单词本"
(load "trie-store.lisp")
(load "vocabulary.lisp")
(load "flow-chart.lisp")
(defparameter *words-db* nil)

;;;; 数据结构的存取、管理
(defun create-word (spell)
  (copy-list `(:spell ,spell
               :n nil :v nil
               :adj nil :adv nil
               :prep nil)))
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
;;; 子repl模板
(defun user-cmd-description (cmd-desc)
  "依次打印命令的描述"
  (format t "~{~{- [~a~15t]: ~a~}~%~}" cmd-desc))

;(defparameter *the-word* nil)
(defmacro user-repl* (cmd-desc)
  "子repl函数生成宏"
  `(lambda (spell)
     ;(setf *the-word* (find-word spell))
     (let ((word (find-word spell)))
       (labels
           ((repl (word)
              ; 此处显示查询单词的情况
              (if word
                  (progn
                    ;(format t "~c[2J~c[H" #\escape #\escape)
                    (format t "The target *~a* found. (˵u_u˵)~%~%" spell)
                    (display-word word))
                  (progn
                    ;(format t "~c[2J~c[H" #\escape #\escape)
                    (format t "The taget *~a* does not exist. (ﾉ ◕ ヮ ◕ )ﾉ~%~%" spell)))
              ; 反馈可用命令
              (user-cmd-description ,cmd-desc)))
         (repl word)
         word))))

;; 主REPL命令集
(defun look-up-func (spell)
  (funcall
   (user-repl*
    '(("back" "go back to the main menu."))
    ;(user-eval* '((back 1)))
    ) spell))
(defun edit-func (spell)
  (funcall
   (user-repl*
    '(("back" "go back to the main menu.")
      ("change :key new-meaning" "to change part of the speech of the target."))
   ;(user-eval* '((back 1) (change 3)))
    ) spell))
(defun erase-func (spell)
  (funcall
   (user-repl*
    '(("back" "go back to the main menu.")
      ("wipe :key" "to wipe off part of the speech of the target.")
      ("wipe-clean" "to wipe off the whole target clean."))
      ;(user-eval* '((back 1) (wipe 2) (wipe-clean 1)))
    ) spell))

;; 子repl命令集
(defun change-func (word key value)
  (set-word word key (prin1-to-string value)))
(defun wipe-func (word key)
  (clean-class-word word key))
(defun wipe-clean-func (word)
  (if (not word)
      (progn
        (format t "Quite clean. Nothing to wipe off.")
        (read-line))
      (progn (format t "are you sure you want to wipe the hole target *~a* clean? (˵u_u˵)[y/n]"
                     (getf word :spell))
             (let* ((r-l (read-line))
                    (option (read-from-string
                             (if (eq (length r-l) 0)
                                 "default" r-l))))
               (cond ((eq 'y option)
                      (remove-word-spell (getf word :spell))
                      (setf word nil)
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
  nil)
(flow-chart:set-state-reader
 *repl-user* 'note-down
 (macroexpand `(let* ((spell args)
                      (word (find-word spell)))
                 (if word '("fail") '("succeed")))))
(flow-chart:def-arc *repl-user* 'main 'note-down '(note-down symbol)
  (let ((args (cadr cmd-list))) 'target))
(flow-chart:def-state *repl-user* 'note-down-succeed
  (let ((spell args))
    (format t "The target *~a* has been add to our database.~%" spell)))
(flow-chart:def-arc *repl-user* 'note-down 'note-down-succeed '(succeed)
  (let ((spell args))
    (add-word (create-word spell))
    'target))
(flow-chart:def-state *repl-user* 'note-down-fail
  (let ((spell args))
    (format t "*~a* has already in our database.~%" spell)))
(flow-chart:def-arc *repl-user* 'note-down 'note-down-fail '(fail)
  'target)
(flow-chart:def-arc *repl-user* 'note-down-fail 'main 'nil
  'target)

;; look-up
(flow-chart:def-state *repl-user* 'look-up
  (look-up-func args))
(flow-chart:def-arc *repl-user* 'main 'look-up '(look-up symbol)
  (let ((args (cadr cmd-list)))
    'target))
(flow-chart:def-arc *repl-user* 'look-up 'main '(back)
  'target)
(flow-chart:def-arc *repl-user* 'look-up 'look-up 'nil
  (format t "~c[2J~c[H" #\escape #\escape)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)

;; edit
(flow-chart:def-state *repl-user* 'edit
  (edit-func args))
(flow-chart:def-arc *repl-user* 'main 'edit '(edit symbol)
  (let ((args (cadr cmd-list)))
    'target))
(flow-chart:def-arc *repl-user* 'edit 'main '(back)
  'target)
(flow-chart:def-arc *repl-user* 'edit 'edit '(change symbol string)
  (let ((key (cadr cmd-list))
        (value (caddr cmd-list))
        (spell args)
        (args args))
    (change-func (find-word spell) key value)
    'target))
(flow-chart:def-arc *repl-user* 'note-down-succeed 'edit 'nil
  'target)
(flow-chart:def-arc *repl-user* 'edit 'edit 'nil
  (format t "~c[2J~c[H" #\escape #\escape)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)

;; erase
(flow-chart:def-state *repl-user* 'erase
  (erase-func args))
(flow-chart:def-arc *repl-user* 'main 'erase '(erase symbol)
  (let ((args (cadr cmd-list)))
    'target))
(flow-chart:def-arc *repl-user* 'erase 'main '(back)
  'target)
(flow-chart:def-arc *repl-user* 'erase 'erase '(wipe symbol)
  (let ((key (cadr cmd-list))
        (spell args))
    (wipe-func (find-word spell) key)
    'target))
(flow-chart:def-arc *repl-user* 'erase 'main '(wipe-clean)
  (let ((spell args))
    (wipe-clean-func (find-word spell))
    'target))
(flow-chart:def-arc *repl-user* 'erase 'erase 'nil
  (format t "~c[2J~c[H" #\escape #\escape)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)

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

