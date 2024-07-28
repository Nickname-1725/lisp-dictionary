
(load "lisp-dictionary.asd")
(asdf:load-system :lisp-dictionary)
(ccl:save-application #p"./build/foo-ccl" :toplevel-function #'init-fun
                      :prepend-kernel t)

