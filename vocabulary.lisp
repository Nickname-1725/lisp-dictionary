
(defpackage :vocabulary
  (:use :cl)
  (:export
     :*vocabulary-table*
     :id :spell :n :v :adj :adv :prep
     :serialize-voc-table
     :deserialize-voc-table
     :define-word
     :describe-def
     :push-def
     :correct-def
     :remove-def
     :dump-spell-vocabulary
     :delete-word
     ; by-id操作, 可能可以移除
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

;;; 序列化方法
(defmethod serialize ((table hash-table) &optional (word-handler nil))
  "hash-table的序列化方法"
  (let ((serial nil))
    (maphash #'(lambda (id word)
                 (push `(:id ,id :word ,(if (eql nil word-handler) word
                                            (funcall word-handler word)))
                       serial))
             table)
    serial))
(defmethod serialize ((word vocabulary-word) &optional (unused-handler nil))
  "vocabulary-word的序列化方法"
  (declare (ignore unused-handler))
  (with-slots (spell id n v adj adv prep) word
    `(:spell ,spell :id ,id :n ,n :v ,v :adj ,adj :adv ,adv :prep ,prep)))
(defun serialize-voc-table (voc-table)
  "词汇表的序列化方法"
  (serialize voc-table #'serialize))
(defun deserialize-hashtable (input-list &optional (word-handler nil))
  "hash-table的反序列化方法"
  (let ((table (make-hash-table)))
    (mapcar
     #'(lambda (item)
         (let* ((id (getf item :id))
                (word (getf item :word))
                (word (if (eql nil word-handler) word (funcall word-handler word))))
           (setf (gethash id table) word)))
     input-list)
    table))
(defun deserialize-word (input-list)
  "vocabulary-word的反序列化方法"
  (let ((spell (getf input-list :spell)) (id (getf input-list :id))
        (n (getf input-list :n)) (v (getf input-list :v)) (adj (getf input-list :adj))
        (adv (getf input-list :adv)) (prep (getf input-list :prep)))
    (make-vocabulary-word :spell spell :id id :n n :v v :adj adj :adv adv :prep prep)))
(defun deserialize-voc-table (input-list)
  "词汇表的反序列化方法"
  (deserialize-hashtable input-list #'deserialize-word))

;;; 词汇表操作
(defun generate-id (length)
  (random (ash 2 (1- (* 4 length)))))
(defun add-vocabulary (voc-table spell)
  "给定一个拼写，向表中增加单词"
  (let ((new-id (generate-id 8)))
    (if (eql nil (gethash new-id voc-table))
        (let ((new-word (make-vocabulary-word)))
          (setf (vocabulary-word-id new-word) new-id)
          (setf (vocabulary-word-spell new-word) spell)
          (setf (gethash new-id voc-table) new-word)
          new-id)
        (add-vocabulary voc-table spell))))
(defun remove-vocabulary-by-id (voc-table id)
  "根据id来删除word"
  (remhash id voc-table))
(defun search-vocabulary (voc-table id)
  "根据id来获取word信息"
  (gethash id voc-table))
(defun push-def (voc-word class def-string)
  "向单词释义中添加信息"
  (let ((class (intern (format nil "~a" class) :vocabulary)))
    (if (slot-exists-p voc-word class)
        (if (listp (slot-value voc-word class))
            (push def-string (slot-value voc-word class))))))
(defun correct-def (voc-word class correct-string index)
  "更正第index条释义"
  (let ((class (intern (format nil "~a" class) :vocabulary)))
    (if (slot-exists-p voc-word class)
        (if (and (listp (slot-value voc-word class))
                 (< index (length (slot-value voc-word class))))
            (setf (nth index (slot-value voc-word class)) correct-string)))))
(defun remove-def (voc-word class index)
  "删除第index条释义"
  (let ((class (intern (format nil "~a" class) :vocabulary)))
    (if (slot-exists-p voc-word class)
        (if (and (listp (slot-value voc-word class))
                 (< index (length (slot-value voc-word class))))
            (setf (slot-value voc-word class)
                  (remove (nth index (slot-value voc-word class))
                          (slot-value voc-word class)
                          :start index :end (1+ index)))))))
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
    (concatenate 'string
                 (format nil ">>> ~a~%" (vocabulary-word-spell voc-word))
                 def-string)))

(defun define-word (spell)
  (add-vocabulary *vocabulary-table* spell))
(defun search-word-by-id (id)
  (search-vocabulary *vocabulary-table* id))
(defun dump-spell-vocabulary (voc-word)
  "获取单词的拼写"
  (vocabulary-word-spell voc-word))
(defun delete-word (voc-word)
  "删除一个word"
  ;(trie-store:remove-word (vocabulary-word-spell voc-word))
  (remove-vocabulary-by-id *vocabulary-table* (vocabulary-word-id voc-word)))
(defun string-word-class(class-string)
  "给定字符串寻找代表单词词性的符号"
  (let* ((class-list '(("n" n) ("v" v) ("adj" adj) ("adv" adv) ("prep" prep)))
         (class-find (find class-string class-list
                           :test #'(lambda (x item) (string-equal x (car item)))))
         (class (cadr class-find)))
    class))
(defun push-def-by-id (id class-string def-string)
  (let ((class (string-word-class class-string)))
    (push-def (search-vocabulary *vocabulary-table* id) class def-string)))
(defun correct-def-by-id (id class-string correct-string index)
  (let ((class (string-word-class class-string)))
    (correct-def (search-vocabulary *vocabulary-table* id)
                 class correct-string index)))
(defun remove-def-by-id (id class-string index)
  (let ((class (string-word-class class-string)))
    (remove-def (search-vocabulary *vocabulary-table* id) class index)))
(defun describe-def-by-id (id)
  (describe-def (search-vocabulary *vocabulary-table* id)))
