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

(defparameter *config-root* "~/.config/lisp-dictionary/")
(defun save-words ()
  (save-db *words-db* (concatenate 'string *config-root* "dictionary-words.db")))
(defun load-words ()
  (load-db *words-db* (concatenate 'string *config-root* "dictionary-words.db")))

;;;; 用户交互功能
(defmacro clear-CLI-screen ()
  `(format t "~c[2J~c[H" #\escape #\escape))
;;; 用户功能的封装
(defun user-cmd-description (cmd-desc)
  "依次打印命令的描述"
  (format t "~{~{- [~a~15t]: ~a~}~%~}" cmd-desc))

;; 主要功能封装
(defun look-up-func (spell)
  (clear-CLI-screen)
  (format t "The target *~a* found. (˵u_u˵)~%~%" spell)
  (display-word (find-word spell))
  (user-cmd-description
   '(("edit" "correct the fault.")
     ("erase" "give it a quick trim or eliminate it completely.")
     ("back" "go back to the main menu."))))
(defun edit-func (word)
  (clear-CLI-screen)
  (display-word word)
  (user-cmd-description
   '(("back" "go back to the main menu.")
     ("change :key new-meaning" "to change part of the speech of the target."))))
(defun erase-func (word)
  (clear-CLI-screen)
  (display-word word)
  (user-cmd-description
   '(("back" "go back to the main menu.")
     ("wipe :key" "to wipe off part of the speech of the target.")
     ("wipe-clean" "to wipe off the whole target clean."))))

;; 简易功能封装
(defun change-func (word key value)
  (set-word word key (prin1-to-string value)))
(defun wipe-func (word key)
  (clean-class-word word key))

;;; CLI构造部分
(flow-chart:def-init *repl-user* 'main
  (format t "The dictionary v0.3 opened. Wellcome back. ( ✿ ◕ ‿ ◕ )~%")
  (user-cmd-description              ; 反馈可用命令
   '(("note-down spell" "note-down a word.")
     ("look-up spell" "look up the dictionary for a word.")
     ("store" "store the data manually.")
     ("quit" "close the dictionary. data will be automatically stored by your little helper, anyway.(˵ ✿ ◕ ‿ ◕ ˵)"))))
(flow-chart:def-arc (*repl-user* (main main) ()) ; 错误通配
  (clear-CLI-screen)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)
(flow-chart:def-arc (*repl-user* (main main) (quit)) ; 退出程序
  (save-words) ; 自动存档
  (format t "The dictionary closed. Goodbye. (⌐ ■ ᴗ ■ )~%"))

;; note-down
(flow-chart:def-state note-down (*repl-user* spell))
(flow-chart:set-state-reader
 *repl-user* 'note-down
 (macroexpand `(let ((word (find-word spell)))
                 (if word '("fail") '("succeed")))))
(flow-chart:def-arc (*repl-user* (main note-down) (note-down symbol))
  (let ((spell (cadr cmd-list))) 'target))
(flow-chart:def-state note-down-succeed (*repl-user* spell)
  (clear-CLI-screen)
  (format t "The target *~a* has been add to our database.~%" spell))
(flow-chart:def-arc (*repl-user* (note-down note-down-succeed) (succeed))
  (add-word (create-word spell))
  'target)
(flow-chart:def-state note-down-fail (*repl-user* spell)
  (clear-CLI-screen)
  (format t "*~a* has already been in our database.~%" spell))
(flow-chart:def-arc (*repl-user* (note-down note-down-fail) (fail))
  'target)
(flow-chart:def-arc (*repl-user* (note-down-fail main) ())
  (clear-CLI-screen)
  'target)

;; look-up
(flow-chart:def-state look-up (*repl-user* spell))
(flow-chart:def-arc (*repl-user* (main look-up) (look-up symbol))
  (let ((spell (cadr cmd-list)))
    'target))

(flow-chart:set-state-reader
 *repl-user* 'look-up
 (macroexpand `(let ((word (find-word spell)))
                 (if word '("succeed") '("fail")))))
(flow-chart:def-state look-up-succeed (*repl-user* spell))
(flow-chart:def-arc (*repl-user* (look-up look-up-succeed) (succeed))
  (look-up-func spell)
  'target)

(flow-chart:def-arc (*repl-user* (look-up-succeed main) (back))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (look-up-succeed look-up-succeed) ())
  (clear-CLI-screen)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)

(flow-chart:def-state look-up-fail (*repl-user* spell)
  (clear-CLI-screen)
  (format t "The taget *~a* does not exist. (ﾉ ◕ ヮ ◕ )ﾉ~%" spell)
  (user-cmd-description
   '(("note-down" "note-down the word.")
     ("back" "go back to the main menu."))))
(flow-chart:def-arc (*repl-user* (look-up look-up-fail) (fail))
  'target)
(flow-chart:def-arc (*repl-user* (look-up-fail note-down) (note-down))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (look-up-fail main) (back))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (look-up-fail look-up-fail) ())
  'target)

;; edit
(flow-chart:def-state edit (*repl-user* word)
  (edit-func word))
(flow-chart:def-arc (*repl-user* (look-up-succeed edit) (edit))
  (let ((word (find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (edit main) (back))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (edit edit) (change symbol string))
  (let ((key (cadr cmd-list))
        (value (caddr cmd-list)))
    (change-func word key value)
    'target))
(flow-chart:def-arc (*repl-user* (note-down-succeed edit) ())
  (let ((word (find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (edit edit) ())
  (clear-CLI-screen)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)

;; erase
(flow-chart:def-state erase (*repl-user* word)
  (erase-func word))
(flow-chart:def-arc (*repl-user* (look-up-succeed erase) (erase))
  (let ((word (find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (erase main) (back))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (erase erase) (wipe symbol))
  (let ((key (cadr cmd-list)))
    (wipe-func word key)
    'target))

(flow-chart:def-state erase-confirm (*repl-user* word)
  (format t "are you sure you want to wipe the hole target *~a* clean? (˵u_u˵)[y/n]"
          (getf word :spell))
  (finish-output))
(flow-chart:def-arc (*repl-user* (erase erase-confirm) (wipe-clean))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (erase-confirm erase-confirm) ()) ; 默认处理
  (format t "yes or no?[y/n]~%")
  'target)
(flow-chart:def-arc (*repl-user* (erase-confirm main) (y))
  (remove-word-spell (getf word :spell))
  (clear-CLI-screen)
  (format t "Neatly-done.~%")
  (read-line)
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (erase-confirm main) (n))
  (clear-CLI-screen)
  'target)

(flow-chart:def-arc (*repl-user* (erase erase) ())
  (clear-CLI-screen)
  (format t "Not a valid command. (✿ ◕ __ ◕ )~%")
  'target)

;; store
(flow-chart:def-state store (*repl-user*)
  (clear-CLI-screen)
  (format t "Neatly done.~%"))
(flow-chart:def-arc (*repl-user* (main store) (store))
  (save-words)
  'target)
(flow-chart:def-arc (*repl-user* (store main) ())
  (clear-CLI-screen)
  'target)

(defun init-fun ()
  (load-words) ; 自动加载存档
  (clear-CLI-screen)
  (funcall (eval (flow-chart:diagram-realize *repl-user*)))
  ;(sleep 0.1)
  (quit))
