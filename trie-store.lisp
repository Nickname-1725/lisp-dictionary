(defpackage :trie-store
  (:use :cl)
  (:export
     ;:*trie*
     ;:trie-add-word
     ;:trie-find-word
     ;:trie-mark-node
     ;:trie-remove-word
     :add-word
     :find-word
     :mark-word
     :remove-word))
(in-package :trie-store)

(defstruct trie-node
  (children nil :type list)
  (id -1 :type number))
(defstruct (trie-arc
            (:constructor make-trie-arc nil)
            (:constructor trie-arc-with-char (char)))
  (char #\* :type standard-char)
  (node (make-trie-node) :type trie-node))
(defmethod serialize ((node trie-node) &optional (arc-handler nil))
  "trie-node的序列化方法"
  (when (eql nil node) nil)
  (with-slots (children id) node
    `(:children ,(if (eql nil arc-handler) children
                     (mapcar #'(lambda (arc)
                                (funcall arc-handler arc #'serialize))
                             children))
      :id ,id)))
(defmethod serialize ((arc trie-arc) &optional (node-handler nil))
  "trie-arc的序列化方法"
  (with-slots (char node) arc
    `(:char ,char :node ,(if (eql nil node-handler) node
                             (funcall node-handler node #'serialize)))))

(defun trie-access-arc (tr-node char)
  "根据字符获取trie的arc"
  (find char (trie-node-children tr-node) :test
        (lambda (char arc) (eq char (trie-arc-char arc)))))
(defun serialize-trie (trie-root)
  "trie的序列化, 输入的是树的根节点, 仍为trie-node"
  (serialize trie-root #'serialize))
(defun trie-append-arc (tr-node char)
  "根据字符添加trie的arc，返回叶子节点的node"
  (let ((arc-find (trie-access-arc tr-node char)))
    (if (eql nil arc-find)
        (let ((arc-create (trie-arc-with-char char)))
          (push arc-create (trie-node-children tr-node))
          (trie-arc-node arc-create))
        (trie-arc-node arc-find))))
(defun trie-add-word (tr-node string)
  "向trie中添加word，返回叶子节点的node"
  (let* ((char-list (coerce string 'list))
         (node-tail (reduce (lambda (node char)
                              (trie-append-arc node char))
                            char-list :initial-value tr-node)))
    node-tail))
(defun trie-find-word (tr-node string)
  "从trie中查找word，返回叶子节点的node"
  (let ((char-list (coerce string 'list)))
    (reduce (lambda (node char)
              (if (eql node nil) nil
                  (let ((arc-find (trie-access-arc node char)))
                    (if (eql nil arc-find) nil
                        (trie-arc-node arc-find)))))
            char-list :initial-value tr-node)))
(defun trie-mark-node (tr-node id &rest force-p)
  "给trie中的node标记id，返回操作后节点的id~@
   默认仅当未标记时使用；当force-p为真时强行更改id"
  (let ((previous-id (trie-node-id tr-node)))
    (if (or force-p (eql -1 previous-id))
        (setf (trie-node-id tr-node) id)
        previous-id)))
(defun trie-remove-arc (tr-node char &rest force-p)
  "根据字符移除trie的arc，返回~@
   默认仅当被移除边为叶子边时使用，当force-p为真时强行移除"
  (let ((arc-find (trie-access-arc tr-node char)))
    (unless (eql nil arc-find)
      (let ((target-node (trie-arc-node arc-find)))
        (if (or force-p
                (eql nil (trie-node-children target-node)))
            (progn ; setf宏单独出现可能会导致运行不正确
              (setf (trie-node-children tr-node)
                    (remove arc-find (trie-node-children tr-node)))))))))
(defun trie-extract-word (tr-node string)
  "从trie中查找word，返回由node组成的list"
  (labels ((extract-helper (tr-node char-list)
             (let ((arc-find (trie-access-arc tr-node (car char-list))))
               (unless (eql nil arc-find)
                 (let ((node-find (trie-arc-node arc-find)))
                   (cons node-find
                         (extract-helper node-find (cdr char-list))))))))
    (let ((char-list (coerce string 'list)))
      (extract-helper tr-node char-list))))
(defun trie-remove-word (tr-node string)
  "从trie中移除word"
  (let* ((char-list (coerce string 'list))
         (reversed-char-list (reverse char-list))
         (node-list (trie-extract-word tr-node string))
         (reversed-parent-list (cdr (reverse (cons tr-node node-list)))))
    (mapcar #'trie-remove-arc reversed-parent-list reversed-char-list)))

;;; 外部接口构造
(defparameter *trie* (make-trie-node)) ; 位于trie根部的node
(defun add-word (string) (trie-add-word *trie* string))
(defun find-word (string) (trie-find-word *trie* string))
(defun mark-word (trie-word id) (trie-mark-node trie-word id))
(defun remove-word (string) (trie-remove-word *trie* string))
