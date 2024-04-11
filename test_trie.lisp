
(defstruct trie-node
  (children nil :type list)
  (id -1 :type number))
(defstruct (trie-arc
            (:constructor make-trie-arc nil)
            (:constructor trie-arc-with-char (char)))
  (char #\* :type standard-char)
  (list nil :type list))

(defparameter *trie* (make-trie-arc)) ; 位于trie根部的arc
(defmacro trie-list-add-node (trie-list char)
  `(let ((find (assoc ,char ,trie-list)))
     (if (eql nil find)
         (let ((trie-node (make-trie-node)))
           (push (list ,char trie-node) ,trie-list))
         nil)))
(defun trie-sublist-with-char (trie-list char)
  (trie-node-children (cadr (assoc char trie-list))))
(defmacro trie-list-add-word (trie-list char-list)
  (unless (eql nil char-list)
    `(let ((char (car ,char-list)))
       (trie-list-add-node ,trie-list char)
       (trie-list-add-word (trie-sublist-with-char ,trie-list char)
                           (cdr ,char-list)))))
(defun trie-add-word (trie string)
  (let ((trie-list (trie-list trie))
        (char-list (coerce string 'list)))
    (trie-list-add-word trie-list char-list)))

(defmacro expand-list (x-list)
  "一种宏递归展开测试"
  (unless (or (eql nil x-list)
              (eql nil (cadr x-list)))
    `(progn
       (format t "~a" (car ,x-list))
       (expand-list (quote ,(cdadr x-list))))))
