;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 -*-
;;;
;;; Module LIBIDI
;;; License: ?
;;;
;;; ------

(in-package #:libidi)

(declaim (optimize (speed 2) (safety 3) (space 1) (debug 3))) ;;

;;;---

(deftype embedding-level () '(integer 0 62))

;;;---
(defun get-char-types (text)
  "Construct an array of bidi character types for string TEXT and return it. Return as
a second value a gen.boolean, indicating if the text was pure LTR."
  (let* ((len (length text))
         (types (make-array len :element-type 'bidi-char-type)))
    (loop repeat len
       for i from 0
       with pure-ltr-p = t
       do (let ((type (access-value *type-planes* (char-code (char text i)))))
            (setf (aref types i) type)
            (setf pure-ltr-p
                  (and pure-ltr-p
                       (find type
                             #(#.+BT-L+ #.+BT-EN+ #.+BT-ES+ #.+BT-ET+ #.+BT-CS+
                               #.+BT-NSM+ #.+BT-BN+ #.+BT-B+ #.+BT-S+ #.+BT-WS+
                               #.+BT-ON+)))))
       finally (return (values types pure-ltr-p)))))


(defun get-para-level (types base-direction)
  "Get Paragraph Level (UBA 3.3.1, P2-P3) for paragraph for which TYPES is an array
of character types."
  (if base-direction
      (ecase base-direction
        ((#.+BT-L+ :ltr) 0)
        ((#.+BT-R+ :rtl) 1))
      (loop for char-type across types
         do (case char-type
              (#.+BT-L+  (return 0))
              ((#.+BT-R+ #.+BT-AL+) (return 1)))
           finally (return 0))))


(defun remove-chars-for-X9 (text types levels n-chars-to-remove)
  "Perform removal of characters according to X9, return new text, types and levels arrays as multiple values. Moved to separate function for easier profiling. N-CHARS-TO-REMOVE is already calculated, so we take it as an argument."
  (let* ((len (length text))
         (new-len (- len n-chars-to-remove))
         (new-text (make-string new-len)))
    (when (zerop n-chars-to-remove)
      ;; short circuit when nothing to remove
      (values (replace new-text text) types levels))
    
    (let* ((new-types (make-array new-len :element-type 'bidi-char-type))
           (new-levels (make-array new-len :element-type 'embedding-level)))

      (loop for char across text
            for char-type across types
            for level across levels
            with i = 0
            unless (find char-type #(#.+BT-RLE+ #.+BT-LRE+ #.+BT-RLO+ #.+BT-LRO+ #.+BT-PDF+ #.+BT-BN+))
              do (progn
                   (setf (aref new-text i) char)
                   (setf (aref new-types i) char-type)
                   (setf (aref new-levels i) level)
                   (incf i)))
      (values new-text new-types new-levels))))


(defstruct char-run
  start
  length
  level
  sor
  eor
  text
  types
  )


(defun split-to-runs (text types levels base-embedding-level)
  (flet ((make-new-run (start end level)
           (let ((length (- end start))
                 sor-level
                 eor-level
                 (whole-text-length (length text)))

             ;; Determine sor and eor levels
             (flet ((get-level (index)
                      (if (or (< index 0)
                              (>= index whole-text-length))
                        base-embedding-level
                        (aref levels index))))
               
               (setf sor-level (max (get-level (- start 1))
                                    (get-level start)))
               (setf eor-level (max (get-level (- end 1))
                                    (get-level end))))

             ;; make the structure describing the run with arrays displaced
             ;; to original whole-string arrays
             (make-char-run :start start
                            :length length
                            :level level
                            :sor (if (oddp sor-level)
                                   +BT-R+
                                   +BT-L+)
                            :eor (if (oddp eor-level)
                                   +BT-R+
                                   +BT-L+)
                            :text (make-array length
                                              :element-type
                                              #+lispworks 'lw:simple-char
                                              #-lispworks 'character
                                              :displaced-to text
                                              :displaced-index-offset start)
                            :types (make-array length
                                               :element-type 'bidi-char-type
                                               :displaced-to types
                                               :displaced-index-offset start)))))
    
    (when (plusp (length text))
      (loop for level across levels
            and prev-level = nil then level
            for i upfrom 0
            with start = 0
            with result
            when (and prev-level
                      (/= prev-level level))
              do (push (make-new-run start i prev-level) result)
                 (setf start i)
            finally (push (make-new-run start (+ i 1) level) result)
                    (return (nreverse result))))))


(defun explicit-levels (text types para-level)
  "Process Explicit Levels and Directions (3.3.2)
Return a fresh copy of TEXT and the new TYPES and LEVELS arrays"
  ;; UBA 3.3.2
  (let* ((len (length types))
         (levels (make-array len :element-type 'embedding-level))
         (max-level 61)
         runs)
    ;; X1
    (let ((curr-level para-level) 
          dir-override
          levels-stack
          dir-overrides-stack
          (n-chars-to-remove 0))
      ;; Local functions
      (labels ((greater-level-odd-or-even (predicate new-dir-override)
               "Increase level to next satisfying the PREDICATE, if it's legal,
then push the current level and override status on the stack, set new overrde to NEW-DIR-OVERRIDE"
               (let ((new-level (1+ curr-level)))
                 (unless (funcall predicate new-level)
                   (incf new-level))
                 (unless (> new-level max-level)
                   (push curr-level levels-stack)
                   (push dir-override dir-overrides-stack)
                   (setf curr-level new-level)
                   (setf dir-override new-dir-override))))

             (greater-level-odd (new-dir-override)
               (greater-level-odd-or-even #'oddp new-dir-override))

             (greater-level-even (new-dir-override)
               (greater-level-odd-or-even #'evenp new-dir-override)))

        ;;
        (loop for char-type across types
           for i from 0
           do (progn
                (case char-type
                  ;; X2
                  (#.+BT-RLE+ (greater-level-odd nil)
                            (incf n-chars-to-remove))
                  ;; X3
                  (#.+BT-LRE+ (greater-level-even nil)
                            (incf n-chars-to-remove))
                  ;; X4
                  (#.+BT-RLO+ (greater-level-odd +BT-R+)
                            (incf n-chars-to-remove))
                  ;; X5
                  (#.+BT-LRO+ (greater-level-even +BT-L+)
                            (incf n-chars-to-remove))
                  ;; X7
                  (#.+BT-PDF+ (when levels-stack
                              (setf curr-level (pop levels-stack))
                              (setf dir-override (pop dir-overrides-stack)))
                            (incf n-chars-to-remove))
                  ;; X6
                  (otherwise (setf (aref levels i) curr-level)
                             (when dir-override
                               (setf (aref types i) dir-override))
                             (when (= char-type +BT-BN+)
                               (incf n-chars-to-remove)))))))

      ;; X9
      (multiple-value-setq (text types levels)
        (remove-chars-for-X9 text types levels n-chars-to-remove))
      ;; after this point TEXT is a copy of the original, so we can modify it

      
      ;; X10
      (setf runs (split-to-runs text types levels para-level))

      (values text types levels runs))))

(defun resolve-weak-types-for-a-run (run)
  ;; W1
  (loop for type across (char-run-types run)
        and prev-type = (char-run-sor run) then type
        for i from 0
        when (= type +BT-NSM+)
          do (setf (aref (char-run-types run) i) prev-type))

  ;; W2
  (loop for type across (char-run-types run)
        with last-strong-type = (char-run-sor run)
        for i from 0
        when (or (= type +BT-R+)
                 (= type +BT-L+)
                 (= type +BT-AL+))
          do (setf last-strong-type type)
        else
          when (and (= type +BT-EN+)
                    (= last-strong-type +BT-AL+))
            do (setf (aref (char-run-types run) i) +BT-AN+))

  ;; W3
  ;; (nsubstitute) may, but does not _have_ to modify the sequence,
  ;; so we can't use it
  (loop for type across (char-run-types run)
        for i from 0
        when (= type +BT-AL+)
          do (setf (aref (char-run-types run) i) +BT-R+))

  ;; W4
  (when (>= (char-run-length run) 3)
    (loop for i from 1 to (- (char-run-length run) 2)
          for type = (aref (char-run-types run) i)
          when (and (or (= type +BT-ES+) (= type +BT-CS+))
                    (= +BT-EN+
                       (aref (char-run-types run) (- i 1))
                       (aref (char-run-types run) (+ i 1))))
            do (setf (aref (char-run-types run) i) +BT-EN+)
          else
            when (and (= type +BT-CS+)
                      (= +BT-AN+
                         (aref (char-run-types run) (- i 1))
                         (aref (char-run-types run) (+ i 1))))
              do (setf (aref (char-run-types run) i) +BT-AN+)))

  ;; W5
  (flet ((change-to-EN (start end)
           (loop for j from start below end
                 do (setf (aref (char-run-types run) j) +BT-EN+))))
    (declare (inline change-to-EN))
    
    (loop for type across (char-run-types run)
          and prev-type = +BT-L+ then type ; Doesn't matter what, as long as it's not EN
          for i from 0
          with in-ET-run = nil
          with started-after-EN = nil
          with start = 0
          when (= type +BT-ET+)
            do (unless in-ET-run
                 (setf in-ET-run t)
                 (setf started-after-EN (= prev-type +BT-ET+))
                 (setf start i))
          else 
            when in-ET-run
              do (when (or started-after-EN
                           (= type +BT-EN+))
                   (change-to-EN start i))
                 (setf in-ET-run nil)
          finally (when (and in-ET-run
                             started-after-EN)
                    (change-to-EN start (+ i 1)))))

  ;; W6
  (loop for type across (char-run-types run)
        for i from 0
        do (case type
             ((#.+BT-ET+ #.+BT-ES+ #.+BT-CS+) (setf (aref (char-run-types run) i) +BT-ON+))))
  
  ;; W7
  (loop for type across (char-run-types run)
        with last-strong-type = (char-run-sor run)
        for i from 0
        when (or (= type +BT-R+)
                 (= type +BT-L+))
          do (setf last-strong-type type)
        else
          when (and (= type +BT-EN+)
                    (= last-strong-type +BT-L+))
            do (setf (aref (char-run-types run) i) +BT-L+))
  ;;
  )

(defun weak-types (runs)
  ;; UBA 3.3.3
  (loop for run in runs
        do (resolve-weak-types-for-a-run run)))



(defun resolve-neutral-types (runs)
  ;; UBA 3.3.4
  (flet ((resolve-neutrals-for-run (run)
           (flet ((is-neutral (type)  ;; FIXME: Are B and S possible here?
                    (case type ((#.+BT-B+ #.+BT-S+ #.+BT-WS+ #.+BT-ON+ ) t))))
             (declare (inline is-neutral))
             
             ;; N1
             (flet ((adjust-strong-type (type)
                      (ecase type
                        (#.+BT-L+ +BT-L+)
                        ((#.+BT-R+ #.+BT-EN+ #.+BT-AN+) +BT-R+)))
                    
                    (change-to-type (start end type)
                      (loop for j from start below end
                            do (setf (aref (char-run-types run) j) type))))
               
               (declare (inline adjust-strong-type change-to-type))
               
               (loop for type across (char-run-types run)
                     and prev-type = (char-run-sor run) then type
                     with last-strong-type = (char-run-sor run)
                     for i from 0
                     with start = 0
                     with in-N-run = nil
                     when (is-neutral type)
                       do (when (not in-N-run)
                            (setf in-N-run t)
                            (setf start i)
                            (setf last-strong-type (adjust-strong-type prev-type)))
                     else
                       when in-N-run
                         do (when (= last-strong-type (adjust-strong-type type))
                              (change-to-type start i last-strong-type))
                            (setf in-N-run nil)
                     finally (when (and in-N-run
                                        (= last-strong-type
                                           (adjust-strong-type (char-run-eor run))))
                               (change-to-type start (+ i 1) last-strong-type))))
             ;; N2
             (loop for type across (char-run-types run)
                   for i from 0
                   with embedding-dir = (if (evenp (char-run-level run))
                                          +BT-L+
                                          +BT-R+)
                   when (is-neutral type)
                     do (setf (aref (char-run-types run) i) embedding-dir)))))
    
    (loop for run in runs
          do (resolve-neutrals-for-run run))))



(defun resolve-implicit-levels (types levels)
  ;; UBA 3.3.5
  (loop for type across types
        for level across levels
        for i from 0
        when (evenp level)
          do (case type
               (#.+BT-R+ (incf (aref levels i)))
               ((#.+BT-AN+ #.+BT-EN+) (incf (aref levels i) 2)))
        else
          do (case type
               ((#.+BT-L+ #.+BT-AN+ #.+BT-EN+) (incf (aref levels i))))))



;; (defun in-place-reverse (array)
;;   (let ((length (length array)))
;;     (loop for i from 0
;;           for j downfrom (- length 1)
;;           until (>= i j)
;;           do (rotatef (aref array i) (aref array j))
;;           finally (return array))))

(defun in-place-reverse (array start end)
  (loop for i from start
        for j downfrom (- end 1)
        until (>= i j)
        do (rotatef (aref array i) (aref array j))
        finally (return array)))


(defun reorder-resolved (text types levels para-level)
  (let ((length (length text)))
    ;; FIXME: when optimization in main routine will be on
    ;; this check won't be necessary
    (when (plusp length)
      ;; L1 FIXME: segment/para separators etc.
      (loop for j downfrom (- length 1)
            while (= +BT-WS+ (access-value *type-planes* (char-code (char text j))))
            do (setf (aref levels j) para-level))
      ;; L2
      (multiple-value-bind (max-level min-level)
          (loop for lev across levels
                maximizing lev into max-lev
                ;;when (oddp lev)
                minimizing lev into min-lev
                finally (return (values max-lev min-lev)))

        (when (evenp min-level)
          (incf min-level))
        
        (when (<= min-level max-level)
          (flet ((do-reverse (start end)
                   (in-place-reverse text start end)
                   (in-place-reverse types start end)
                   (in-place-reverse levels start end)))
            
            (loop for level from max-level downto min-level
                  do (loop for lev across levels
                           for i from 0
                           with start = 0
                           with in-run = nil
                           if (and (not in-run)
                                   (>= lev level))
                             do (setf start i)
                                (setf in-run t)
                           if (and in-run
                                   (< lev level))
                             do (do-reverse start i)
                                (setf in-run nil)
                           finally (when in-run
                                     (do-reverse start (+ i 1)))))))))
    
    (values text types levels)))


(defun bidi-string (text &optional base-direction)
  "Main function to perform UBA on a string. BASE-DIRECTION, when specified, should be 
either :LTR or :RTL, by default the algorithm figures it out"
  (let (types
        levels
        runs
        is-pure-ltr
        para-level)
    
    (multiple-value-setq (types is-pure-ltr)
      (get-char-types text))
    
    (when is-pure-ltr
      ;; FIXME: do early exit
      )

    ;; determine paragraph level
    (setf para-level (get-para-level types base-direction))

    (multiple-value-setq (text types levels runs)
        (explicit-levels text types para-level))
    ;; after explicit-levels we have a copy of original string in TEXT

    
    (weak-types runs)

    
    (resolve-neutral-types runs)


    ;; I1-2
    (resolve-implicit-levels types levels)
    

    (multiple-value-setq (text types levels)
      (reorder-resolved text types levels para-level))

    (values text types levels)))


(defun bidi-objects-vector (objects-vector (char-getter #'(lambda (obj)
                                                            (when (characterp obj) obj)))))
