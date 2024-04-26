
(ql:quickload :cl-ppcre)
(defun command-read-default ()
  "通用解析用户输入，输出字符串列表"
  (cl-ppcre:split "\\s+" (string-trim " " (read-line))))

;; 本脚本模拟流程图
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

(defparameter *diagram* (create-diagram
                         (create-state-node 'main
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
        (setf (diagram-start diag) state))))

(defmacro local-fun-def (name func-body)
  "根据函数体和名称创建局部定义函数格式的list"
  `(,name (&rest args) args ,@func-body)) ; args用来消除警告
(defmacro implement-fun-def (start-name fun-def-list)
  "根据局部函数列表和入口函数来构造匿名函数格式的list"
  `(lambda ()
     (labels (,@fun-def-list)
       (,start-name))))

(defun state-func-def (stat)
  "输入state-node，输出局部定义函数的list表达"
  (let* ((name (state-node-name stat))
         (outro (list (list 'funcall (state-node-trans-read stat))))
         ; todo: outro这一部分将会拓展为读取和跳转部分
         (func-body (state-node-activity stat))
         (func-body (append func-body outro)))
    ; todo: 在func-body中添加关于读取和跳转的部分
    (macroexpand `(local-fun-def ,name ,func-body))))

(defun diagram-realize (diag)
  "输入图结构，输出匿名函数"
  (let ((func-def-list (mapcar #'state-func-def (diagram-all-states diag)))
        (start-name (state-node-name (diagram-start diag))))
    (macroexpand `(implement-fun-def ,start-name ,func-def-list))))

