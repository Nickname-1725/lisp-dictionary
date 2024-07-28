;#!/home/vboxuser/.roswell/impls/x86-64/linux/sbcl-bin/2.3.2/bin/sbcl --script

(load "lisp-dictionary.asd")
(asdf:load-system :lisp-dictionary)
(sb-ext:save-lisp-and-die #p"./build/foo-sbcl" 
                          :toplevel
                          #'init-fun
                          :executable t)
