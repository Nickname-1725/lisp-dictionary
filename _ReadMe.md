# 介绍
## 意义
本脚本用于实现单词本，作为一个脚本工具。
1. 可用于将网上查询到的单词意思记录到本地
2. 从存档中找到原先记录的单词，作为一个可记录的单词本
3. 当当时没有查询单词含义时，可以仅记录单词，而不输入含义
4. 当发现以前单词记录有错误时，可以更改原有记录
5. 记录内容包括：单词本身、单词的各词性对应的含义（要求以后便于拓展，比如增加例句的存放）
6. 要求交互界面为CLI，要有询问和提示信息，用户输入须为键盘输入

## 功能设计
1. CLI用户交互界面
   - 提示添加单词/词组的命令`note-down word-name`
   - 提示查询单词/词组的命令`look-up word-name`
   - 提示提示编辑单词/词组信息的命令`edit word-name`
   - 提示清除单词/词组的命令`erase word-name`
   - [r] 提示保存存档/保存存档并退出的命令`restore/quit`
   1. [x] 存入单词/词组的命令`note-down word-name`
      - 查询用户键入的单词/词组名称，若有重复录入则反馈用户
      - 存入单词后立即编辑单词的信息（跳转到3.）
   2. [x] 查询单词/词组的命令`look-up word-name`
      - 查询用户查询的单词/词组名称，若无录入则反馈用户
      - 向用户给出（单词）信息，列举词性及其对应含义的方式打印，若词性无记录含义则不显示；若无信息则反馈用户；若为词组则不需要考虑词性
      - 向用户提示返回命令`back`
   3. [x] 编辑单词/词组信息的命令`edit word-name`
      - 查询用户编辑的单词/词组名称，若无录入则反馈用户
      - 向用用户给出单词/词组信息（参考2.）
      - 向用户提示返回命令`back`
      - 向用户提示修改词性、义项键入的命令；若为词组则无需考虑词性
        `change key-name new-meaning`
      - 编辑后立即打印信息（参考2.）
   4. [x] 删除单词/词组的命令`erase word-name`
      - 查询用户键入的单词/词组的名称，若无录入则反馈用户
      - 向用户给出单词/词组信息(参考2)
      - 向用户提示返回命令`back`
      - 向用户提示清除词性、义项键入的命令 `wipe key-name`；
        向用户提示删除整个单词的命令 `wipe-clean`
2. [x] 数据结构的存取、管理
   1. [x] 查询单词（是否存在）
   2. [x] 根据键名更改单词对应的含义
   3. [x] 格式化打印单词信息
   4. [x] 根据键名重置单词对应的含义为空
   5. [x] 从数据结构中移除单词
3. [x] 数据库的存档、加载

# 功能实现
## 数据结构
1. 用于存放单词、词组的数据结构
   1. 一个用于存放单词的数据结构
      1. 方案一
        ```lisp
        (defparameter *words*
          ((word1 (
             :n nil :v nil 
             :adj (xx xxxx) :adv nil 
             :prep nil))
          (word2 (
             :n nil :v nil 
             :adj nil :adv nil 
             :prep nil))))`
        ```
      2. 方案二：
        ```lisp
        (defparameter *words-db*
          ((:spell word1 :n nil :v nil :adj "含义" :adv nil :prep nil)
           (:spell word2 :n "含义" :v nil :adj nil :adv nil :prep nil)
           (:spell word3 :n nil :v "含义" :adj nil :adv nil :prep nil)))
        ```
   2. 一个用于存放词组的数据结构
      - 方案一
        ```lisp
        (defparameter *phrases* 
          (("phrase 1" ())
           ("phrase 2" (xxx xx))))
        ```
      - 方案二
        ```lisp
        (defparameter *phrases*
          ((:spell "phrase 1" "含义")
           (:spell "phrase 2" "含义")))
        ```
2. 其中，用于存放词组的数据结构更加简单。遇到特殊词性的单词时，也可以临时存放于词组数据结构中
3. 但存放单词和存放词组时表达的方式有所不同：
   - 单词可以以符号的形式储存，且不区分字母大小写
   - 词组必须以字符串的形式储存，有时需要区分大小写（对于单词缩写），甚至包括标点符号

## 用户交互
1. 基础的用户交互界面构造
   ```
   (defun user-repl ()
     (let ((cmd (user-read)) ;读取用户输入
           (allowed-cmds '(command-A command-B etc. ))) ; 允许命令，可作为全局变量，或者函数内的变量
       (unless (eq (car cmd) 'quit-or-back) ; 退出
         (some-information) ; 用户提示信息；清屏并打印用户提示信息
         (user-eval cmd) ; 用户命令执行
         (user-repl)))) ; 递归开启下一轮循环
   ```
2. [x] 可以将上述基础构造功能作为模板（宏、高阶函数），从而实现多个用户界面，并且以相互调用的形式实现跳转
       作为模板，所有的功能都是要在内部集成的，包括`user-read`、`user-eval`
   - [x] `user-eval`函数的宏化
   - [x] `user-eval*`宏的全局变量`*allowed-commands*`参数化
   - [x] `user-read`函数保留为通用函数
   - [x] `user-repl`函数的宏化

