
(defpackage :vocabulary
  (:use :cl)
  (:export
     :*vocabulary-table*
     :id :spell :n :v :adj :adv :prep
     :define-word
     :search-word-by-id
     :push-def-by-id
     :correct-def-by-id
     :remove-def-by-id
     :describe-def-by-id))
(in-package :vocabulary)

(defstruct vocabulary-word
  (spell "" :type string)
  (id -1 :type number)
  (n nil :type list)
  (v nil :type list)
  (adj nil :type list)
  (adv nil :type list)
  (prep nil :type list))

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
(defun push-def (voc-word class def-string)
  "向单词释义中添加信息"
  (if (slot-exists-p voc-word class)
      (if (listp (slot-value voc-word class))
          (push def-string (slot-value voc-word class)))))
(defun correct-def (voc-word class correct-string index)
  "更正第index条释义"
  (if (slot-exists-p voc-word class)
      (if (and (listp (slot-value voc-word class))
               (< index (length (slot-value voc-word class))))
          (setf (nth index (slot-value voc-word class)) correct-string))))
(defun remove-def (voc-word class index)
  "删除第index条释义"
  (if (slot-exists-p voc-word class)
      (if (and (listp (slot-value voc-word class))
               (< index (length (slot-value voc-word class))))
          (let ((item (nth index (slot-value voc-word class))))
            (delete item (slot-value voc-word class) :start index :end (1+ index))))))
(defun describe-def (voc-word)
  "获得描述给定单词义项的字符串"
  (let* ((def-string-list
           (loop for slot in '(n v adj adv prep)
                 collect
                 (let ((def-list (slot-value voc-word slot))
                       (counter 0))
                   (unless (eql nil def-list)
                     (format nil "*~a*:~8t~{~{~a) ~a~}; ~}~%" slot
                             (mapcar #'(lambda (item)
                                         (list (incf counter) item))
                                     def-list))))))
         (def-string-list (remove nil def-string-list))
         (def-string (apply #'concatenate (cons 'string def-string-list))))
    def-string))

(defun define-word (spell)
  (add-vocabulary *vocabulary-table* spell))
(defun search-word-by-id (id)
  (search-vocabulary *vocabulary-table* id))
(defun push-def-by-id (id class-string def-string)
  (let ((class (read-from-string (concatenate 'string "vocabulary:" class-string))))
    (push-def (search-vocabulary *vocabulary-table* id) class def-string)))
(defun correct-def-by-id (id class-string correct-string index)
  (let ((class (read-from-string (concatenate 'string "vocabulary:" class-string))))
    (correct-def (search-vocabulary *vocabulary-table* id) class correct-string index)))
(defun remove-def-by-id (id class-string index)
  (let ((class (read-from-string (concatenate 'string "vocabulary:" class-string))))
    (remove-def (search-vocabulary *vocabulary-table* id) class index)))
(defun describe-def-by-id (id)
  (describe-def (search-vocabulary *vocabulary-table* id)))
