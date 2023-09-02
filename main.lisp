"本脚本用于实现单词本"
(defparameter *words-db* nil)

;;;; 数据结构的存取、管理
(defun create-word (spell)
  (copy-list `(:spell ,spell
               :n nil :v nil
               :adj nil :adv nil
               :prep nil)))
(defun add-word (word) (push word *words-db*))

(defun set-word (word key value)
  "设置特定单词的关键字值，value应为字符串"
  (setf (getf word key) value))
(defun find-word (spell)
  "从字典中查找单词，若无则返回nil"
  (car (remove-if-not
        (lambda (word) (eql spell (getf word :spell)))
        *words-db*)))

(set-word (find-word 'happy) :adj "快乐的")
(defun remove-word-spell (spell)
  (setf *words-db* (remove-if
                    #'(lambda (word) (eq spell (getf word :spell)))
                    *words-db*)))
(defun clean-class-word (word key)
  (set-word word key nil))

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

;;;; 数据库的存档与加载
(defun save-db (data-base filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print data-base out))))
(defmacro load-db (data-base filename)
  `(with-open-file (in ,filename)
     (with-standard-io-syntax
       (setf ,data-base (read in)))))

(defun save-words ()
  (save-db *words-db* "dictionary-words.db"))
(defun load-words ()
  (load-db *words-db* "dictionary-words.db"))
;(save-words)
(load-words)

;;;; 用户交互功能
(defun user-read ()
  "通用解析用户输入函数"
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")" ))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defmacro user-eval* (allow-cmds)
  "模板，生成user-eval类型的函数，输入参数为允许的命令列表及允许词数
  allow-cmds: 应形如((command-1 3) (command-2 1))"
  `(lambda (sexp)
     (let* ((allow-cmds ,allow-cmds)
            (find-cmd (assoc (car sexp) allow-cmds)))
       (if (and find-cmd
                (eq (length sexp) (cadr find-cmd)))
           (eval sexp)
           (format t "Not a valid command.~%")))))

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
                    (format t "The target *~a* found.~%~%" spell)
                    (display-word word))
                  (format t "The taget *~a* does not exist.~%~%" spell))
              ; 反馈可用命令
              (user-cmd-description ,cmd-desc)
              ; 执行用户命令
              (let ((cmd (user-read)))
                (unless (eq (car cmd) 'back)
                  (funcall ,u-eval cmd)
                  (repl word)))))
         (repl word)))))

;; 主REPL命令集
(defparameter look-up
  (user-repl*
   '(("back" "go back to the main menu."))
   (user-eval* '((back 1)))))
(defparameter edit
  (user-repl*
   '(("back" "go back to the main menu.")
     ("change :key new-meaning" "to change part of the speech of the target."))
   (user-eval* '((back 1) (change 3)))))
(defparameter erase
  (user-repl*
   '(("back" "go back to the main menu.")
     ("wipe :key" "to wipe off part of the speech of the target.")
     ("wipe-clean" "to wipe off the whole target clean."))
   (user-eval* '((back 1) (wipe 2) (wipe-clean 1)))))
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
      (progn (format t "are you sure you want to wipe the hole target *~a* clean?[y/n]"
                     (getf *the-word* :spell))
             (let ((option (read-from-string (read-line))))
               (cond ((eq 'y option)
                      (remove-word-spell (getf *the-word* :spell))
                      (setf *the-word* nil))
                     ((eq 'n option))
                     (t (format t "yes or no?[y/n]~%")
                        (wipe-clean)))))))
;(defmacro look-up (spell) `(funcall look-up-inner ,spell))
;(defmacro edit (spell) `(funcall edit-inner ,spell))

