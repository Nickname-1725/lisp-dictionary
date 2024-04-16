
(defstruct vocabulary-word
  (spell "" :type string)
  (id -1 :type number)
  (n-def nil :type list)
  (v-def nil :type list)
  (adj-def nil :type list)
  (adv-def nil :type list)
  (prep-def nil :type list))

(defparameter *vocabulary-table* (make-hash-table))
(defun generate-id (length)
  (random (ash 2 (1- (* 4 length)))))
(defun add-vocabulary (voc-table spell)
  "给定一个拼写，向表中增加单词"
  (let ((new-id (generate-id 8)))
    (if (eql nil (gethash new-id voc-table))
        (let ((new-word (make-vocabulary-word)))
          (setf (vocabulary-word-id new-word) new-id)
          (setf (vocabulary-word-spell new-word) spell)
          (setf (gethash new-id voc-table) new-word))
        (add-vocabulary voc-table spell))))
(defun search-vocabulary (voc-table id)
  "根据id来获取word信息"
  (gethash id voc-table))
(defun push-def-vocabulary (voc-word class def-string)
  "向单词释义中添加信息"
  (if (and (slot-exists-p voc-word class))
      (if (listp (slot-value voc-word class))
          (push def-string (slot-value voc-word class))))))
(defun correct-def-vocabulary (voc-word class correct-string index)
  "更正第index条释义"
  (if (and (slot-exists-p voc-word class))
      (if (listp (slot-value voc-word class))
          (setf (nth index (slot-value voc-word class)) correct-string)))))
