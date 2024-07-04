"本脚本用于实现单词本"
(load "trie-store.lisp")
(load "vocabulary.lisp")
(load "flow-chart.lisp")
(defparameter *words-db* nil)

;;;; 数据结构的存取、管理
(defun register-word (spell)
  (trie-store:mark-word (trie-store:add-word spell)
                        (vocabulary:define-word spell)))

(defun def-append-word (word key value)
  "设置特定单词的关键字值，value应为字符串"
  (vocabulary:push-def word key value))
(defun def-correct-word (word key value index)
  "纠正特定单词的词性第index条释义, 用户意义上的index比列表意义上的index多1"
  (vocabulary:correct-def word key value (1- index)))
(defun def-remove-word (word key index)
  "清除特定单词的词性第index条释义, 用户意义上的index比列表意义上的index多1"
  (vocabulary:remove-def word key (1- index)))

(defun find-word (spell)
  "从字典中查找单词，若无则返回nil"
  (let ((trie-find (trie-store:find-word spell)))
    (when trie-find
      (vocabulary:search-word-by-id (trie-store:mark-word trie-find -1)))))
(defun fuzzy-find-word (spell)
  (trie-store:fuzzy-find-word spell))

(defun remove-word (word)
  (trie-store:remove-word (vocabulary:dump-spell-vocabulary word))
  (vocabulary:delete-word word))

(defun display-word (word)
  (format t (vocabulary:describe-def word))
    (format t "~%"));

;;;; 数据库的存档与加载
(defun save-db (data-base filename &optional (serialize-method nil))
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-standard-io-syntax
      (print (if (eql nil serialize-method)
                 data-base (funcall serialize-method data-base))
             out))))
(defmacro load-db (data-base filename &optional (deserialize-method nil))
  `(let ((file-exists (probe-file ,filename)))
     (when file-exists
       (with-open-file (in ,filename
                           :if-does-not-exist :error)
         (with-standard-io-syntax
           ,(if (eql nil deserialize-method)
                `(setf ,data-base (read in))
                `(setf ,data-base (funcall ,deserialize-method (read in)))))))))

(defparameter *config-root* "~/.config/lisp-dictionary/")
(defun save-words ()
  (save-db trie-store:*trie*
           (concatenate 'string *config-root* "trie-store.db")
           #'trie-store:serialize-trie)
  (save-db vocabulary:*vocabulary-table*
           (concatenate 'string *config-root* "vocabulary.db")
           #'vocabulary:serialize-voc-table))
(defun load-words ()
  (load-db trie-store:*trie*
      (concatenate 'string *config-root* "trie-store.db")
      #'trie-store:deserialize-trie)
  (load-db vocabulary:*vocabulary-table*
      (concatenate 'string *config-root* "vocabulary.db")
      #'vocabulary:deserialize-voc-table))

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
     ("fuzzy" "to check if the spell match a fuzzy search.")
     ("back" "go back to the main menu."))))
(defun edit-func (word)
  (clear-CLI-screen)
  (display-word word)
  (user-cmd-description
   '(("back" "go back to the main menu.")
     ("append :key new-meaning " "to append a definition with part of the speech of the target.")
     ("correct :key new-meaning indx" "to correct a definition")
     ("remove :key indx" "to remove a definition"))))
(defun erase-func (word)
  (clear-CLI-screen)
  (display-word word)
  (user-cmd-description
   '(("back" "go back to the main menu.")
     ("remove :key indx" "to remove a definition")
     ("wipe-clean" "to wipe off the whole target clean."))))

;;; CLI构造部分
(flow-chart:def-init *repl-user* 'main
  (format t "The dictionary v1.1 opened. Wellcome back. ( ✿ ◕ ‿ ◕ )~%")
  (format t "We have recorded *~a* words for now. ~%" (vocabulary:count-words))
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
(flow-chart:def-arc (*repl-user* (main note-down) (note-down string))
  (let ((spell (cadr cmd-list))) 'target))
(flow-chart:def-state note-down-succeed (*repl-user* spell)
  (clear-CLI-screen)
  (format t "The target *~a* has been add to our database.~%" spell))
(flow-chart:def-arc (*repl-user* (note-down note-down-succeed) (succeed))
  (register-word spell)
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
(flow-chart:def-arc (*repl-user* (main look-up) (look-up string))
  (let ((spell (cadr cmd-list)))
    'target))

(flow-chart:set-state-reader
 *repl-user* 'look-up
 (macroexpand `(let ((word (find-word spell)))
                 (if word '("succeed") '("fail")))))
(flow-chart:def-state look-up-succeed (*repl-user* spell)
  (look-up-func spell))
(flow-chart:def-arc (*repl-user* (look-up look-up-succeed) (succeed))
  'target)

(flow-chart:def-arc (*repl-user* (look-up-succeed main) (back))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (look-up-succeed look-up-succeed) ())
  'target)

(flow-chart:def-state look-up-fail (*repl-user* spell)
  (clear-CLI-screen)
  (format t "The taget *~a* does not exist. (ﾉ ◕ ヮ ◕ )ﾉ~%" spell)
  (user-cmd-description
   '(("note-down" "note-down the word.")
     ("fuzzy" "attempt to fuzzily match the word.")
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
(flow-chart:def-state look-up-fuzzy (*repl-user* fuzzy-match-list)
  (clear-CLI-screen)
  (format t "Here you go. ~%")
  (format t "~{~a; ~}~%" fuzzy-match-list)
  (user-cmd-description
   '(("fuzzy spell" "attempt to fuzzily match the word.")
     ("look-up spell" "look up the dictionary for the word.")
     ("back" "go back to the main menu."))))
(flow-chart:def-arc (*repl-user* (look-up-fail look-up-fuzzy) (fuzzy))
  (let ((fuzzy-match-list (fuzzy-find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (look-up-fuzzy look-up-fuzzy) (fuzzy string))
  (let* ((spell (cadr cmd-list))
         (fuzzy-match-list (fuzzy-find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (look-up-fuzzy main) (back))
  (clear-CLI-screen) 'target)
(flow-chart:def-arc (*repl-user* (look-up-fuzzy look-up) (look-up string))
  (let ((spell (cadr cmd-list)))
    'target))
(flow-chart:def-arc (*repl-user* (look-up-fuzzy look-up-fuzzy) nil)
  'target)
(flow-chart:def-arc (*repl-user* (look-up-succeed look-up-fuzzy) (fuzzy))
  (let ((fuzzy-match-list (fuzzy-find-word spell))) 'target))

;; edit
(flow-chart:def-state edit (*repl-user* word)
  (edit-func word))
(flow-chart:def-arc (*repl-user* (look-up-succeed edit) (edit))
  (let ((word (find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (edit main) (back))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (edit edit) (append symbol string))
  (let ((key (cadr cmd-list))
        (value (caddr cmd-list)))
    (def-append-word word key value)
    'target))
(flow-chart:def-arc (*repl-user* (edit edit) (correct symbol string integer))
  (let ((key (cadr cmd-list))
        (value (caddr cmd-list))
        (index (cadddr cmd-list)))
    (def-correct-word word key value index)
    'target))
(flow-chart:def-arc (*repl-user* (edit edit) (remove symbol integer))
  (let ((key (cadr cmd-list))
        (index (caddr cmd-list)))
    (def-remove-word word key index)
    'target))

(flow-chart:def-arc (*repl-user* (note-down-succeed edit) ())
  (let ((word (find-word spell)))
    'target))
(flow-chart:def-arc (*repl-user* (edit edit) ())
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
(flow-chart:def-arc (*repl-user* (erase erase) (remove symbol integer))
  (let ((key (cadr cmd-list))
        (index (caddr cmd-list)))
    (def-remove-word word key index)
    'target))

(flow-chart:def-state erase-confirm (*repl-user* word)
  (format t "are you sure you want to wipe the hole target *~a* clean? (˵u_u˵)[y/n]"
          (vocabulary:dump-spell-vocabulary word))
  (finish-output))
(flow-chart:def-arc (*repl-user* (erase erase-confirm) (wipe-clean))
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (erase-confirm erase-confirm) ()) ; 默认处理
  (format t "yes or no?[y/n]~%")
  'target)
(flow-chart:def-arc (*repl-user* (erase-confirm main) (y))
  (remove-word word)
  (clear-CLI-screen)
  (format t "Neatly-done.~%")
  (read-line)
  (clear-CLI-screen)
  'target)
(flow-chart:def-arc (*repl-user* (erase-confirm main) (n))
  (clear-CLI-screen)
  'target)

(flow-chart:def-arc (*repl-user* (erase erase) ())
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
