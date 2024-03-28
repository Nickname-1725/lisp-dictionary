(load "main.lisp")
(ext:saveinitmem #p"./build/foo-clisp" :init-function 
                 (lambda () (init-fun) (ext:quit))
                 :executable t :quiet t :norc t)
