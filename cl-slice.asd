(asdf:defsystem #:cl-slice
  :description "DSL for array slices in Common Lisp."
  :version "0.1"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :depends-on (#:alexandria
               #:anaphora
               #:let-plus
               #:optima)
  :serial t
  :components ((:file "cl-slice")))
