(asdf:defsystem #:cl-slice
  :description "DSL for array slices in Common Lisp."
  :version "0.1"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :depends-on (#:alexandria
               #:anaphora
               #:let-plus)
  :serial t
  :components ((:file "cl-slice-dev")
               (:file "cl-slice")))

(asdf:defsystem #:cl-slice-tests
  :description "DSL for array slices in Common Lisp - unit tests."
  :version "0.1"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :depends-on (#:cl-slice
               #:clunit)
  :serial t
  :components ((:file "cl-slice-tests")))
