
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

(defparameter *state-list* nil)

