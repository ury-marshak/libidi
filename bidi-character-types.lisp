;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Module LIBIDI
;;; License: ?
;;;
;;; ------

(in-package #:libidi)


(declaim (optimize (speed 2) (safety 3) (space 1) (debug 3))) ;;


(defconstant +ucd-set-size+ #x110000)


;;;--- Bidi data

(deftype bidi-char-type () '(integer 0 18))


;;; Bidirectional Character Types
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Strong
  (defconstant +BT-L+ 0)     ; Left-to-Right
  (defconstant +BT-LRE+ 1)   ; Left-to-Right Embedding
  (defconstant +BT-LRO+ 2)   ; Left-to-Right Override
  (defconstant +BT-R+ 3)     ; Right-to-Left
  (defconstant +BT-AL+ 4)    ; Right-to-Left Arabic
  (defconstant +BT-RLE+ 5)   ; Right-to-Left Embedding
  (defconstant +BT-RLO+ 6)   ; Right-to-Left Override
  ;; Weak
  (defconstant +BT-PDF+ 7)   ; Pop Directional Format
  (defconstant +BT-EN+ 8)    ; European Number
  (defconstant +BT-ES+ 9)    ; European Number Separator
  (defconstant +BT-ET+ 10)   ; European Number Terminator
  (defconstant +BT-AN+ 11)   ; Arabic Number
  (defconstant +BT-CS+ 12)   ; Common Number Separator
  (defconstant +BT-NSM+ 13)  ; Non-Spacing Mark
  (defconstant +BT-BN+ 14)   ; Boundary Neutral
  ;; Neutral
  (defconstant +BT-B+ 15)    ; Paragraph Separator
  (defconstant +BT-S+ 16)    ; Segment Separator
  (defconstant +BT-WS+ 17)   ; Whitespace
  (defconstant +BT-ON+ 18))  ; Other Neutrals
  



(defparameter *names-to-bidi-char-type* (make-hash-table :test 'equalp))

(loop for i = 0 then (1+ i)
   for sym in '(+BT-L+ +BT-LRE+ +BT-LRO+ +BT-R+ +BT-AL+ +BT-RLE+ +BT-RLO+
                +BT-PDF+ +BT-EN+ +BT-ES+ +BT-ET+ +BT-AN+ +BT-CS+ +BT-NSM+
                +BT-BN+ +BT-B+ +BT-S+ +BT-WS+ +BT-ON+)
   do (progn
        (let* ((sym-name (symbol-name sym))
               (name (subseq sym-name 4 (1- (length sym-name)))))
          (setf (gethash name *names-to-bidi-char-type*) (symbol-value sym))
          ;;(setf (gethash sym *names-to-bidi-char-type*) name)
          (setf (gethash (symbol-value sym) *names-to-bidi-char-type*) name))))


;;;--- packed data

(defparameter *char-bytespec* (byte 2 0))
(defparameter *num-chars* (1+ (ldb *char-bytespec* #x10FFFF)))

(defparameter *group-bytespec* (byte 5 (+ (byte-size *char-bytespec*)
                                          (byte-position *char-bytespec*))))
(defparameter *num-groups* (1+ (ldb *group-bytespec* #x10FFFF)))

(defparameter *plane-bytespec-position* (+ (byte-size *group-bytespec*)
                                          (byte-position *group-bytespec*)))
(defparameter *plane-bytespec* (byte (- 21 *plane-bytespec-position*) *plane-bytespec-position*))
(defparameter *num-planes* (1+ (ldb *plane-bytespec* #x10FFFF)))


(defparameter *type-planes* (make-array *num-planes* :initial-element :empty))
(defparameter *mirrored-planes* (make-array *num-planes* :initial-element :empty))

(defun access-value (planes code-point)
  (let* ((plane (ldb *plane-bytespec* code-point))
         (tempval (aref planes plane)))
    (when (eql tempval :empty)
      (error ":empty in plane ~a, codepoint: ~x" plane code-point))
    (if (not (arrayp tempval))
        tempval
        (let* ((groups tempval)
               (group (ldb *group-bytespec* code-point))
               (tempval (aref groups group)))
          (when (eql tempval :empty)
            (error ":empty in plane ~a, group ~a, codepoint: ~x" plane group code-point))
          (if (not (arrayp tempval))
              tempval
              (let* ((chars tempval)
                     (char-in-group (ldb *char-bytespec* code-point))
                     (tempval (aref chars char-in-group)))
                (when (eql tempval :empty)
                  (error ":empty in plane ~a, group ~a, char ~a, codepoint: ~x"
                         plane group char-in-group code-point))
                tempval))))))

