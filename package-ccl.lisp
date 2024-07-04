
(load "main.lisp")
(ccl:save-application #p"./build/foo-ccl" :toplevel-function #'init-fun
                      :prepend-kernel t)

