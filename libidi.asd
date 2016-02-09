;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-


(asdf:defsystem :libidi
  :description "Unicode Bidirectional Algorithm"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "bidi-character-types")
               (:file "bidi-data")
               (:file "bidi")
               )
  :depends-on ())

