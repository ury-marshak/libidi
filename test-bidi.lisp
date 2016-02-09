;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;;
;;; ------

(in-package #:libidi-test)

(declaim (optimize (speed 2) (safety 3) (space 1) (debug 3))) ;;


;;;---

#.(progn (cl-interpol:enable-interpol-syntax) (values))

;;(bidi-string #?"asd\x{202E}fgh")
;; (let ((*type-planes* *test-type-planes*)) (bidi-string "asdFGH"))


(deftestsuite libidi-test () ()
  (:test
   (explicit-override-1
    (ensure-same (bidi-string #?"asd\x{202E}fgh") "asdhgf")))
  (:test
   (explicit-override-2
    (ensure-same (bidi-string #?"asd\x{202E}fgh\x{202C}jkl") "asdhgfjkl"))))



(deftestsuite libidi-test-caprtl (libidi-test)
  ()
  (:dynamic-variables
   (libidi::*type-planes* libidi::*test-type-planes*))
  (:test
   (caprtl-1
    (ensure-same (bidi-string "asdFGH") "asdHGF")))
  (:test
   (caprtl-2
    (ensure-same (bidi-string "asd FGH") "asd HGF")))
  (:test
   (caprtl-3
    (ensure-same (bidi-string "car means CAR.") "car means RAC.")))
  (:test
   (caprtl-4
    (ensure-same (bidi-string "car means CAR." :rtl) ".RAC car means")))
  (:test
   (caprtl-5
    (ensure-same (bidi-string "he said \"THE VALUES ARE 123, 456, 789, OK\".")
                 "he said \"KO ,789 ,456 ,123 ERA SEULAV EHT\".")))
  (:test
   (caprtl-6
    (ensure-same (bidi-string "IT IS A bmw 500, OK.")
                 ".KO ,bmw 500 A SI TI")))
  (:test
   (caprtl-7
    (ensure-same (bidi-string #?"he said \"\x{202B}car MEANS CAR\x{202C}.\"")
                 "he said \"RAC SNAEM car.\"")))
  (:test
   (caprtl-8
    (ensure-same (bidi-string
                  #?"DID YOU SAY '\x{202A}he said \"\x{202B}car MEANS CAR\x{202C}\"\x{202C}'?" :rtl)
                 "?'he said \"RAC SNAEM car\"' YAS UOY DID")))
  )


(defun test ()
  (describe (run-tests :suite 'libidi-test)))

;;;---

#.(progn (cl-interpol:disable-interpol-syntax) (values))
