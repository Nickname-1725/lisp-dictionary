
;; 本脚本模拟流程图
(defstruct (state-node
            (:constructor make-state-node (name))
            ; 应当优先使用(不一定)
            (:constructor create-state-node (name trans-read &rest activity))) 
  "状态节点"
  (name nil :type symbol)
  ; 状态的指示
  (activity '(nil) :type list)
  ; 读取过程, 返回字符串列表
  (trans-read #'(lambda () (list ""))
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

(defparameter *diagram* (create-diagram
                         (create-state-node 'main
                                            #'(lambda ())
                                            '(format t "Hello" ))))
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
        (setf (diagram-start diag) state)))))

(defmacro diagram-realize (diag)
  "输入图结构，输出匿名函数"
  ; todo: 以test-macro为基础, 实现批量定义局部函数的宏
  )

(defmacro test-macro (name func-body)
  `(lambda ()
     (labels ((,name (&rest args)
                args
                ,@func-body))
       (,name)))
  )
(let ((name 'main)
      (function-body '((+ 1 3))))
  (macroexpand `(test-macro ,name ,function-body))) ; 是一个列表, 可使用eval执行
; 可使用(cadr (caddr (cadr * )))来访问局部函数列表

(let ((name 'main)
      (function-body (state-node-activity (access-state *diagram* 'main))))
  (macroexpand `(test-macro ,name ,function-body)))
; 可使用(funcall (eval *)) 来执行
