
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
  "向trie中添加word，设置叶子节点的id"
  (let* ((char-list (coerce string 'list))
         (node-tail (reduce (lambda (node char)
                              (trie-append-arc node char))
                            char-list :initial-value tr-node)))
    (setf (trie-node-id node-tail) 1)))

(defmacro expand-list (x-list)
  "一种宏递归展开测试"
  (unless (or (eql nil x-list)
              (eql nil (cadr x-list)))
    `(progn
       (format t "~a" (car ,x-list))
       (expand-list (quote ,(cdadr x-list))))))
