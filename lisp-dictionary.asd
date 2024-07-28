
(asdf:defsystem :lisp-dictionary
  :components ((:file "main" :depends-on ("trie-store"
                                          "vocabulary"
                                          "flow-chart"))
               (:file "trie-store")
               (:file "vocabulary")
               (:file "flow-chart"))
  :depends-on (:cl-ppcre))
