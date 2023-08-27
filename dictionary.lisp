"本脚本用于实现单词本"
(defparameter *words-db* nil)

(defun create-word (spell)
  (copy-list `(:spell ,spell
               :n nil :v nil
               :adj nil :adv
               :prep nil)))
(defun add-word (word) (push word *words-db*))
(defun set-word (word key value)
  "设置特定单词的关键字值，value应为字符串"
  (setf (getf word key) value))

