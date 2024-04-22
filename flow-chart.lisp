
;; 本脚本模拟流程图
(defstruct (state-node
            (:constructor make-state-node nil)
            (:constructor create-state-node (activity trans-read))) ; 应当优先使用
  "状态节点"
  (name nil :type symbol)
  (activity #'(lambda ()) :type compiled-function) ; 状态活动
  (trans-read #'(lambda ()) :type compiled-function) ; 读取过程
  (trans-list nil :type list)) ;
(defstruct (trans-arc
            (:constructor make-trans-arc (next))) ; 必须指定next才能构造
  "状态转换边"
  (next nil :type state-node)
  (trans-p #'(lambda () nil) :type compiled-function) ;转换条件
  (activity #'(lambda ()) :type compiled-function)) ; 转换过程活动
(defstruct (diagram
            (:constructor make-diagram (start))) ; 必须指定start才能创建
  "抽象出来的图结构，可由函数转换为一个递归函数"
  (start nil :type state-node)
  (all-states nil :type list))
(defun create-diagram (start)
  "构造diagram同时将其压入all-states中"
  (let ((diagram (make-diagram start)))
    (push start (diagram-all-states diagram))
    diagram))
