;;; 本脚本模拟repl流程图
(defpackage :flow-chart
  (:use :cl)
  (:export
     :def-init
     :def-state
     :def-arc
     :diagram-realize
     :set-state-reader))
(in-package :flow-chart)

(ql:quickload :cl-ppcre)
(defun command-read-default ()
  "通用解析用户输入，输出字符串列表"
  (cl-ppcre:split "\\s+" (string-trim " " (read-line))))
(defun string-list-match (string-list match-list) ; 可配合(find item seq :test ...)匹配
  "string-list是字符串列表，match-list是由命令名和参数类型组成的列表"
  (cond
    ; 若匹配match-list为(), 则视为通配
    ((eql nil match-list) t)
    ; 字符串列表长度不足，匹配失败
    ; (若模板列表长度不足，则自动截断)x 若长度不等，则同样匹配失败
    ((eql (length match-list) (length string-list))
     (let ((operator (car match-list))
           (operator-string (car string-list))
           (operand (cdr match-list))
           (operand-string (cdr string-list)))
       (if (eql operator (read-from-string operator-string)) ; 比较运算符
           (let* ((operand-matched
                    (mapcar #'(lambda (string type)
                                (if (eql type 'string) ; 判断是否为字符串
                                    string
                                    ; 判断是否为数字或符号
                                    (let ((x (read-from-string string)))
                                      (if (typep x type) x))))
                            operand-string operand))
                  (verify-operand
                    ; 检查每个元素是否不为nil
                    (reduce #'(lambda (x y) (declare (ignore x))
                                (if (eql nil y) nil t))
                            operand-matched :initial-value nil)))
             (cond
               (verify-operand (cons operator operand-matched))
               ; 没有带参数的命令直接为真
               ((eql nil operand-matched) t))))))))
(defun index-list (list)
  (let* ((indexed-list
           (reduce #'(lambda (x y)
                       (cons (list (if (eql nil x) 0 (1+ (caar x))) y)
                             x))
                   list :initial-value nil))
         (indexed-list (reverse indexed-list)))
    indexed-list))

;;; 结构体定义
(defstruct (state-node
            (:constructor make-state-node (name))
            ; 应当优先使用(不一定)
            (:constructor create-state-node (name activity))) 
  "状态节点"
  (name nil :type symbol)
  (arg-list nil :type list)
  ; 状态的指示
  (activity '(nil) :type list)
  ; 读取过程, 返回字符串列表
  (trans-read (macroexpand `(funcall ,#'command-read-default))
   :type list)
  (trans-list nil :type list)) ;
(defstruct (trans-arc
            ; 必须指定next才能构造
            (:constructor make-trans-arc (next))) 
  "状态转换边"
  (next nil :type state-node)
  ; 转换条件匹配列表
  (match-list 'nil :type list)
  ; 转换过程eval
  (eval '(nil) :type list)) 

(defstruct (diagram
            ; 必须指定start才能创建
            (:constructor make-diagram (start))) 
  "抽象出来的图结构，可由函数转换为一个递归函数"
  (start nil :type state-node)
  (all-states nil :type list))
(defun create-diagram (start)
  "构造diagram同时将其压入all-states中"
  (let ((diagram (make-diagram start)))
    (push start (diagram-all-states diagram))
    diagram))
(defun diagram-to-tree (diag)
  "用来将图结构转换为树结构, 树的节点为(node-name node-arg-list)"
  (labels ((register-make ()
             "记录节点，若为新节点则返回t，否则返回nil"
             (let ((item-list nil))
               (lambda (item)
                 (if (find item item-list) nil
                     (progn (push item item-list) t)))))
           (handler (gra-node duplicate-p)
             "将图转化为树"
             (let* ((node-name (state-node-name gra-node))
                    (leaf-p (not (funcall duplicate-p node-name)))
                    (node-name
                      (if leaf-p
                          (read-from-string (format nil "[~a]" node-name))
                          node-name))
                    (node-arg-list (state-node-arg-list gra-node)))
               (let* ((children (state-node-trans-list gra-node))
                      (children
                        (if leaf-p nil
                            (mapcar #'(lambda (arc) (trans-arc-next arc))
                                    children)))
                      (children (remove-duplicates children))
                      (sub-tree-list
                        (mapcar #'(lambda (stat-node)
                                    (handler stat-node duplicate-p))
                                children)))
                 (macroexpand `((,node-name ,@node-arg-list) ,@sub-tree-list))))))
    (let ((duplicate-p (register-make)))
      (handler (diagram-start diag) duplicate-p))))

(defun wrap (stat)
  "给定stat状态节点, 将其包裹为(name arg-list)形式"
  `(,(state-node-name stat) ,@(state-node-arg-list stat)))
(defun ill-wrap (stat)
  `(,(read-from-string (format nil "[~a]" (state-node-name stat)))
    ,@(state-node-arg-list stat)))

(defun BFS (r queue not-used)
  "从图中梳理出节点的连接关系~@
  queue是当前队列, r是积累的列表结果, not-used是未被访问的节点"
  (if (null queue) (reverse r)
      (let* ((arc-list (state-node-trans-list (car queue)))
             (new-vertexes (mapcar #'trans-arc-next arc-list))
             (new-vertexes-name
               (mapcar #'(lambda (item)
                           (if (member item not-used) (wrap item) (ill-wrap item)))
                       new-vertexes))
             (new-vertexes-filted
               (remove-if-not #'(lambda (v) (member v not-used)) new-vertexes))
             (pair-list
               (mapcar #'(lambda (v) (list (wrap (car queue)) v)) new-vertexes-name))
             (not-used
               (remove-if #'(lambda (v) (member v new-vertexes-filted)) not-used)))
        (BFS (append (reverse pair-list) r)
             (append new-vertexes-filted (cdr queue))
             not-used))))
(defun build-tree (vertex arcs)
  "由节点的连接关系重建树"
  (let* ((arc-list (remove-if-not #'(lambda (x) (equal (car x) vertex)) arcs))
         (children (mapcar #'cadr arc-list))
         (sub-tree (mapcar #'(lambda (v) (build-tree v arcs)) children)))
    (if (null children) (list vertex) (cons vertex sub-tree))))
(defun diagram-to-tree* (diag)
  "用来将图结构转换为树结构, 树的节点为(node-name node-arg-list)；使用广度优先遍历"
  (let* ((start (diagram-start diag))
         (not-used (remove start (diagram-all-states diag)))
         (bfs-arcs (remove-duplicates (BFS nil (list start) not-used) :test #'equal)))
    (build-tree (wrap start) bfs-arcs))))

(defun print-tree (tree &optional (prefix-head "") (prefix-body "") (stream t))
  "打印树结构"
  (let* ((head (car tree))
         (name (car head))
         (arg-list (cdr head)))
    (format  stream "~a~a ── ~a~%" prefix-head
             (if (eql nil arg-list) "()" arg-list) name))
  (labels ((handle-sub-tree (sub-tree attach-head attach-body)
	     (let ((next-prefix-head (concatenate 'string prefix-body attach-head))
		   (next-prefix-body (concatenate 'string prefix-body attach-body))
		   (sub-tree (if (listp sub-tree)
				 sub-tree (list sub-tree))))
	       (print-tree sub-tree
			   next-prefix-head
			   next-prefix-body
                           stream))))
    (if (cdr tree)
	(let* ((reversed-branch (reverse (cdr tree)))
	       (branch-but-last (reverse (cdr reversed-branch)))
	       (branch-last (car reversed-branch)))
	  (mapcar #'(lambda (sub-tree)
		      (handle-sub-tree sub-tree "├ " "│ "))
		  branch-but-last)
          (handle-sub-tree branch-last "└ " "  ")))))
(defmethod print-object ((diag diagram) stream)
  "用来打印diagram结构体的描述"
  (format stream "#S(diagram):~%")
  (print-tree (diagram-to-tree* diag) "" "" stream))

;;; 图节点操作
(defun access-state (diag name)
  "根据名字获取图结构中的状态节点"
  (find name (diagram-all-states diag)
        :test (lambda (x item) (eql x (state-node-name item)))))
(defun remove-state (diag name)
  "根据名字删除图结构节点(除了start以外)"
  (unless (eql name (state-node-name (diagram-start diag)))
    (setf (diagram-all-states diag)
          (remove name (diagram-all-states diag)
             :test (lambda (x item) (eql x (state-node-name item)))))))
(defun push-state (diag name)
  "根据名字创建图结构节点"
  (if (access-state diag name)
      (remove-state diag name))
  (let ((state (make-state-node name)))
    (push state (diagram-all-states diag))))
(defun set-start (diag name)
  "根据名字将节点设置为start"
  (let ((state (access-state diag name)))
    (if state
        (setf (diagram-start diag) state))))
(defun set-state-activity (diag name activity-body)
  "设置state的activity"
  (setf (state-node-activity (access-state diag name)) activity-body))
(defun set-state-arg-list (diag name arg-list)
  "设置state的arg-list"
  (setf (state-node-arg-list (access-state diag name)) arg-list))
(defun set-state-reader (diag name reader-fun)
  "设置state的读取函数(是已经编译函数的形式)"
  (setf (state-node-trans-read (access-state diag name)) reader-fun))

;;; 图边操作
(defun search-match-list (stat match-list)
  "从状态中查找匹配列表"
  (find match-list (state-node-trans-list stat) 
        :test #'(lambda (x arc)
                  (let ((match-list (trans-arc-match-list arc)))
                    (equal x match-list)))))
(defun remove-arc (stat match-list)
  "根据match-list删除state-node结构的arc"
  (setf (state-node-trans-list stat)
        (remove match-list (state-node-trans-list stat)
                :test
                #'(lambda (x arc)
                    (let ((match-list (trans-arc-match-list arc)))
                      (equal x match-list))))))
(defun push-arc (stat next-stat match-list body)
  "对于两个状态之间，定义一个转换途径"
  (if (search-match-list stat match-list)
      (remove-arc stat match-list))
  (let ((arc (make-trans-arc next-stat)))
    (setf (trans-arc-match-list arc) match-list)
    (setf (trans-arc-eval arc) body)
    (let* ((arc-list-new (cons arc (state-node-trans-list stat)))
           (arc-list-new (sort arc-list-new
                               #'(lambda (a b)
                                   (> (length (trans-arc-match-list a))
                                      (length (trans-arc-match-list b)))))))
      (setf (state-node-trans-list stat) arc-list-new))))
(defun set-arc-eval (stat match-list eval-body)
  "设置arc的eval"
  (setf (trans-arc-eval (search-match-list stat match-list)) eval-body))

;;; 转换器
(defun replace-list (list target replace)
  "在list中查找target符号(被引用的符号)，并替换为replace列表"
  (sublis (macroexpand `((,target . ,replace))) list :test #'equal))
(defmacro local-fun-def (name arg-list func-body)
  "根据函数体和名称创建局部定义函数格式的list"
  `(,name ,arg-list ,@func-body))
(defmacro implement-fun-def (start-name fun-def-list)
  "根据局部函数列表和入口函数来构造匿名函数格式的list"
  `(lambda ()
     (labels (,@fun-def-list)
       (,start-name))))

(defun eval-fun-def (stat)
  "输入state-node，输出局部定义函数的list表达"
  (let* ((arc-list (state-node-trans-list stat))
         (real-eval-list (mapcar
                          #'(lambda (arc)
                              (let* ((eval (trans-arc-eval arc))
                                     (next-node (trans-arc-next arc))
                                     (next-name (state-node-name next-node))
                                     (next-arg-list (state-node-arg-list next-node)))
                                (replace-list eval (read-from-string "'target")
                                              (cons next-name
                                                    next-arg-list))))
                          arc-list))
         (indexed-eval-list (index-list real-eval-list)))
    (mapcar #'(lambda (x)
                (macroexpand
                 `(,(read-from-string (format nil "fun-~a" (car x))) ; 函数名
                   ,(read-from-string "(cmd-list)") ; 参数列表
                   ,(read-from-string "(declare (ignorable cmd-list))")
                   ,@(car (cdr x))))) ; 函数体
            indexed-eval-list)))

(defun eval-cond-def (stat)
  "输入stat，输出(let (...) (cond ...))表达"
  (let* ((reader (state-node-trans-read stat))
         (arc-list (state-node-trans-list stat)))
    (if (eql nil arc-list) nil
        (let* ((match-lists (mapcar #'(lambda (arc) (trans-arc-match-list arc))
                                    arc-list))
               (cond-list (mapcar #'(lambda (match)
                                      (macroexpand `(string-list-match cmd-string-list
                                                                       ',match)))
                                  match-lists))
               (indexed-cond-list (index-list cond-list))
               (cond-expr (cons 'cond
                                (mapcar
                                 #'(lambda (cond-item-indexed)
                                     (let ((index (car cond-item-indexed))
                                           (cond-item (cadr cond-item-indexed)))
                                       (macroexpand
                                        `(,cond-item
                                          (,(read-from-string (format nil "fun-~a" index))
                                           ,cond-item)))))
                                 indexed-cond-list)))
               (let-expr (macroexpand `(let ((cmd-string-list ,reader))
                                   ,cond-expr))))
          let-expr))))

(defun state-func-def (stat)
  "输入state-node，输出局部定义函数的list表达"
  (let* ((name (state-node-name stat))
         (arg-list (state-node-arg-list stat))
         ; 读取和跳转部分生成器
         (outro (macroexpand `(labels ,(eval-fun-def stat) ,(eval-cond-def stat))))
         (func-body (state-node-activity stat))
         (func-body (macroexpand `(,@func-body ,outro))))
    (macroexpand `(local-fun-def ,name ,arg-list ,func-body))))

(defun diagram-realize (diag)
  "输入图结构，输出匿名函数"
  (let ((func-def-list (mapcar #'state-func-def (diagram-all-states diag)))
        (start-name (state-node-name (diagram-start diag))))
    (macroexpand `(implement-fun-def ,start-name ,func-def-list))))

;;; 宏的封装
(defmacro def-init (diag start-name &rest body)
  `(defparameter ,diag (create-diagram (create-state-node ,start-name (quote ,body)))))
(defmacro def-state (stat-name diag-&-arg-list &rest body)
  "定义状态节点~@
  用法: ~@
  (flow-chart:def-state foo-state (diag (...)) 
    (+ 1 2) ...)"
  (let ((diag (car diag-&-arg-list))
        (arg-list (cdr diag-&-arg-list)))
    `(progn
       (push-state ,diag ',stat-name)
       (set-state-activity ,diag ',stat-name ',body)
       (set-state-arg-list ,diag ',stat-name ',arg-list))))
(defmacro def-arc (|diag|-&-|stat-from-to|-&-|match-list| &rest body)
  "定义状态转移边~@
  用法: ~@
  (flow-chart:def-arc (diag (stat-from stat-to) (cmd-foo number symbol ...))~@
    (let (...) ~@
      ... 'target))"
  (let* ((diag (car |diag|-&-|stat-from-to|-&-|match-list|))
         (stat-from-to (cadr |diag|-&-|stat-from-to|-&-|match-list|))
         (match-list (caddr |diag|-&-|stat-from-to|-&-|match-list|))
         (stat-from (car stat-from-to))
         (stat-to (cadr stat-from-to)))
    `(push-arc (access-state ,diag ',stat-from)
               (access-state ,diag ',stat-to)
               ',match-list ',body)))

