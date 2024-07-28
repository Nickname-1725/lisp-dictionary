
(load "lisp-dictionary.asd")
(asdf:load-system :lisp-dictionary)
(ext:saveinitmem #p"./build/foo-clisp" :init-function 
                 (lambda () (init-fun) (ext:quit))
                 :executable t :quiet t :norc t)
