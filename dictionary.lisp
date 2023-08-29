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
