;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package #:cl-user)

(defpackage #:libidi
  (:export #:bidi-string
           #:bidi-objects-vector
           #:bidi-objects-list)
  (:use :cl))

