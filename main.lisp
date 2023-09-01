"本脚本用于实现单词本"
(defparameter *words-db* nil)

;;;; 数据结构的存取、管理
(defun create-word (spell)
  (copy-list `(:spell ,spell
               :n nil :v nil
               :adj nil :adv nil
               :prep nil)))
(defun add-word (word) (push word *words-db*))

;(add-word (create-word 'happy))
;(add-word (create-word 'sad))

(defun set-word (word key value)
  "设置特定单词的关键字值，value应为字符串"
  (setf (getf word key) value))
(defun find-word (spell)
  "从字典中查找单词，若无则返回nil"
  (car (remove-if-not
        (lambda (word) (eql spell (getf word :spell)))
        *words-db*)))

(set-word (find-word 'happy) :adj "快乐的")
(defun remove-word (spell)
  (setf *words-db* (remove-if
                    (lambda (word) (eql spell (getf word :spell)))
                    *words-db*)))
(defun clean-class-word (word key)
  (set-word word key nil))

;(add-word (create-word 'muddy))
;(remove-word 'muddy)
;(set-word (find-word 'sad) :n "欣喜若狂之人")
;(clean-class-word (find-word 'sad) :n)

(defun display-word (word)
  (flet ((display-class-word (word key)
           (if (getf word key)
               (format t "~% ~a.~7t~a" key (getf word key)))))
    (format t ">>> ~a" (getf word :spell))
    (display-class-word word :n)
    (display-class-word word :v)
    (display-class-word word :adj)
    (display-class-word word :adv)
    (display-class-word word :prep)))
;(set-word (find-word 'happy) :n "快乐")
;(set-word (find-word 'happy) :v "使快乐")
;(set-word (find-word 'happy) :adv "快乐地")
;(set-word (find-word 'happy) :prep "快！")
(display-word (find-word 'happy))

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
;; [ ] 把user-repl写成一个宏，输出无参递归调用函数
;; [x] 可以把some-information写成一个连续打印提示的函数，输入的参数就是列表
;;     ((command-1 "description 1")
;;      (command-2 "discription 2"))
(defmacro user-repl* (cmd-desc u-eval)
  `(lambda ()
     (labels
         ((repl ()
            (let ((cmd (user-read))
                  (cmd-desc ,cmd-desc))
              (unless (eq (car cmd) 'back)
                (funcall ,u-eval cmd)
                (user-cmd-description cmd-desc)
                (repl)))))
       ; 此处显示查询单词的情况
       (repl))))
(defparameter look-up
  (user-repl*
   '(("back" "go back to the main menu."))
   (user-eval* *allowed-commands*)))

(defun user-cmd-description (cmd-desc)
  "依次打印命令的描述"
  (format t "~{~{- [~a~15t]: ~a~}~%~}" cmd-desc))

;; [x] 可以作为一个通用的解析用户输入的函数
(defun user-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")" ))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; [x] 可以考虑把以下整体在全局写成一个宏，由一个变量来代替函数中*allowed-commands*的位置
;; [x] 可以改造*allowed-commands*列表，引入命令的总词数
;;     ((command-1 3)
;;      (command-2 1))
;; [x] 然后再在函数内计算命令的词数，判断命令是否合理（省略多余的词/抛出提示）
(defparameter *allowed-commands* '((look-up 1) (quit 1) (back 1)))
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
(defparameter u-eval (user-eval* *allowed-commands*))
