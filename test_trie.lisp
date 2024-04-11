
(defstruct trie-node
  (children nil :type list)
  (id -1 :type number))
(defstruct (trie-arc
            (:constructor make-trie-arc nil)
            (:constructor trie-arc-with-char (char)))
  (char #\* :type standard-char)
  (node (make-trie-node) :type trie-node))

(defparameter *trie* (make-trie-node)) ; 位于trie根部的node
(defun trie-access-arc (tr-node char)
  "根据字符获取trie的arc"
  (find char (trie-node-children tr-node) :test
        (lambda (char arc) (eq char (trie-arc-char arc)))))
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
;(defun trie-extract-word (tr-node string)
;  "从trie中查找word，返回由node组成的list"
;  (let ((char-list (coerce string 'list)))
;    ))

(defmacro expand-list (x-list)
  "一种宏递归展开测试"
  (unless (or (eql nil x-list)
              (eql nil (cadr x-list)))
    `(progn
       (format t "~a" (car ,x-list))
       (expand-list (quote ,(cdadr x-list))))))

(defmacro expand-list-match (a-list x-list)
  "一种宏递归匹配展开测试~@
   能够自动处理a-list或x-list长度不足的问题"
  (unless (or (eql nil x-list)
              (eql nil (cadr x-list)))
    `(progn
       (if (eql (car ,a-list) (car ,x-list))
           (progn
             (cons (car ,x-list)
              (expand-list-match
               (quote ,(cdadr a-list))
               (quote ,(cdadr x-list)))))))))
