
;;;; 本脚本模拟repl流程图

(ql:quickload :cl-ppcre)
(defun command-read-default ()
  "通用解析用户输入，输出字符串列表"
  (cl-ppcre:split "\\s+" (string-trim " " (read-line))))
(defun string-list-match (string-list match-list) ; 可配合(find item seq :test ...)匹配
  "string-list是字符串列表，match-list是由命令名和参数类型组成的列表"
  ; 字符串列表长度不足，匹配失败
  ; (若模板列表长度不足，则自动截断)x 若长度不等，则同样匹配失败
  (when (eql (length match-list) (length string-list))
    (let ((operator (car match-list))
          (operator-string (car string-list))
          (operand (cdr match-list))
          (operand-string (cdr string-list)))
      (if (eql operator (read-from-string operator-string)) ; 比较运算符
          (cons operator
                (mapcar #'(lambda (string type)
                            (if (eql type 'string) ; 判断是否为字符串
                                string
                                ; 判断是否为数字或符号
                                (let ((x (read-from-string string)))
                                  (if (typep x type) x))))
                        operand-string operand))))))
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
            (:constructor create-state-node (name &rest activity))) 
  "状态节点"
  (name nil :type symbol)
  ; 状态的指示
  (activity '(nil) :type list)
  ; 读取过程, 返回字符串列表
  (trans-read #'command-read-default
   :type compiled-function)
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

;;; 图节点操作
(defun access-state (diag name)
  "根据名字获取图结构中的状态节点"
  (find name (diagram-all-states diag)
        :test (lambda (x item) (eql x (state-node-name item)))))
(defun push-state (diag name)
  "根据名字创建图结构节点"
  (unless (access-state diag name)
    (let ((state (make-state-node name)))
      (push state (diagram-all-states diag)))))
(defun remove-state (diag name)
  "根据名字删除图结构节点(除了start以外)"
  (unless (eql name (state-node-name (diagram-start diag)))
    (setf (diagram-all-states diag)
          (remove name (diagram-all-states diag)
             :test (lambda (x item) (eql x (state-node-name item)))))))
(defun set-start (diag name)
  "根据名字将节点设置为start"
  (let ((state (access-state diag name)))
    (if state
        (setf (diagram-start diag) state))))
(defun set-state-activity (diag name activity-body)
  "设置state的activity"
  (setf (state-node-activity (access-state diag name)) activity-body))
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
(defun push-arc (stat next-stat match-list &rest body)
  "对于两个状态之间，定义一个转换途径"
  (unless (search-match-list stat match-list) ; 不能已经存在该match-list
    (let ((arc (make-trans-arc next-stat)))
      (setf (trans-arc-match-list arc) match-list)
      (setf (trans-arc-eval arc) body)
      (push arc (state-node-trans-list stat)))))
(defun remove-arc (stat match-list)
  "根据match-list删除state-node结构的arc"
  (setf (state-node-trans-list stat)
        (remove match-list (state-node-trans-list stat)
                :test
                #'(lambda (x arc)
                    (let ((match-list (trans-arc-match-list arc)))
                      (equal x match-list))))))
(defun set-arc-eval (stat match-list &rest eval-body)
  "设置arc的eval"
  (setf (trans-arc-eval (search-match-list stat match-list)) eval-body))

;;; 转换器
(defun replace-list (list target replace)
  "在list中查找target符号(被引用的符号)，并替换为replace列表"
  (mapcar #'(lambda (elem)
              (cond
                ((equal target elem) replace)
                ((listp elem) (replace-list elem target replace))
                (t elem)))
          list))
(defmacro local-fun-def (name func-body)
  "根据函数体和名称创建局部定义函数格式的list"
  `(,name (&rest args) args ,@func-body)) ; args用来消除警告
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
                                     (next-name (state-node-name
                                                 (trans-arc-next arc))))
                                (replace-list eval ''target
                                              (list next-name 'args))))
                          arc-list))
         (indexed-eval-list (index-list real-eval-list)))
    (mapcar #'(lambda (x)
                (macroexpand
                 `(,(read-from-string (format nil "fun-~a" (car x))) ; 函数名
                   (cmd-list &rest args) ; 参数列表
                   ,@(car (cdr x))))) ; 函数体
            indexed-eval-list)))

(defun eval-cond-def (stat)
  "输入stat，输出(let (...) (cond ...))表达"
  (let* ((reader (state-node-trans-read stat))
         (arc-list (state-node-trans-list stat))
         (match-lists (mapcar #'(lambda (arc) (trans-arc-match-list arc))
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
                                     ,cond-item args)))))
                           indexed-cond-list)))
         (let-expr (macroexpand `(let ((cmd-string-list (funcall ,reader)))
                                   ,cond-expr))))
    let-expr))

(defun state-func-def (stat)
  "输入state-node，输出局部定义函数的list表达"
  (let* ((name (state-node-name stat))
         ; 读取和跳转部分生成器
         (outro (macroexpand `(labels ,(eval-fun-def stat) ,(eval-cond-def stat))))
         (func-body (state-node-activity stat))
         (func-body (macroexpand `(,@func-body ,outro))))
    (macroexpand `(local-fun-def ,name ,func-body))))

(defun diagram-realize (diag)
  "输入图结构，输出匿名函数"
  (let ((func-def-list (mapcar #'state-func-def (diagram-all-states diag)))
        (start-name (state-node-name (diagram-start diag))))
    (macroexpand `(implement-fun-def ,start-name ,func-def-list))))

;;; 测试用例
(defparameter *diagram* (create-diagram
                         (create-state-node 'main
                                            '(format t "Hello" ))))
(push-arc (diagram-start *diagram*) ; 无法再直接查看*diagram*
          (diagram-start *diagram*)
          '(echo number) 'args '(format t "Hello. You're a ~a.~%" (cadr cmd-list))
          ''target)
; todo: 
; 1. 更改push-arc的方法，增加排序，越长的表达式越靠前排列
; 2. 更改匹配规则, 将match-list为nil的设置为永远匹配t，可用来作为错误结果的通配
; 3. 生成labels局部定义函数的功能可以抽象出来
; 4. 生成labels局部定义函数的功能应根据是否使用cmd-list/args的情况来决定入口参数是否丢弃(`(declare (ignore args))`). (或者也可以无需判断，而使用`(declare (ignorable args))`)
; 5. 生成labels局部定义函数的功能应确切地根据'target的位置判断是否替换
;    1). 在代码块的结尾
;    2). 在条件分支的代码块结尾
;    3). 在let宏下的代码块的结尾
;    否则应当拒绝替换，从而配合尾递归优化
