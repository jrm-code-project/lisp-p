(defsystem "lisp-p"
  :description "A validator for testing if a stream contains a Lisp program."
  :components ((:file "package")
               (:file "lisp-p" :depends-on ("package"))))
