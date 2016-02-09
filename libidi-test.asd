;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-


(asdf:defsystem :libidi-test
  :description "Unicode Bidirectional Algorithm/Test"
  :version "0.0.1"
  :serial t
  :depends-on (:libidi :cl-interpol :lift)
  :components ((:file "test-packages")
               (:file "make-tables")
               (:file "test-bidi-data")
               (:file "test-bidi")
               ))

