;;(in-package #:cl-user)

(in-package #:libidi)


(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defparameter *make-table-dir*
  (make-pathname :directory (pathname-directory *load-truename*)
                 :host (pathname-host *load-truename*)
                 :device (pathname-device *load-truename*)))



;;;--- String parsing stuff
(defun trim-spaces (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               string))


(defun split-string-and-trim (line character)
  ;; 
  (loop for prev-position = 0 then (1+ position)
        for position = (position character line :start prev-position)
        collect (trim-spaces (subseq line prev-position position))
        while position))


(defun strip-comments-and-split (line)
  ;; Takes the UCD line of the form "field1;field2;..;fieldn # comment"
  ;; and returns a list of fields or nil if line was empty
  (setf line (subseq line 0 (position #\# line))) ; kill comments
  (setf line (trim-spaces line))
  (unless (zerop (length line))
    (split-string-and-trim line #\;)))


(defun parse-range (string)
  ;; Read either a single code point: "093D", or a range: "093E..0940"
  ;; and return it as a cons of start and end. End equals start for a
  ;; single code point
  (let* ((codepoints (split-string-and-trim string #\.))
         (len (length codepoints))
         start
         end)
    (if (= len 1)
        (setf start (setf end (first codepoints)))
        (if (= len 3)
            (progn
              (setf start (first codepoints))
              (setf end (third codepoints)))
            (error "Cannot parse value: ~A" string)))
    (setf start (parse-integer start :radix 16))
    (setf end (parse-integer end :radix 16))
    (cons start end)))


;;;--- Bidi data types


(defstruct bidi-info
  (bidi-type 0 :type bidi-char-type)
  (mirrored nil :type boolean))


(defparameter *bidi-data* (make-array +ucd-set-size+ :element-type 'bidi-info))
(loop for i from 0 below +ucd-set-size+
   do (setf (aref *bidi-data* i)  (make-bidi-info)))



;;;--- Reading files

(defun read-bidi-class-line (line)
  (let ((data (strip-comments-and-split line))
        char-type
        range)
    (when data
      (setf range (parse-range (first data)))
      (setf char-type (gethash (second data) *names-to-bidi-char-type*))
      ;;(print (gethash char-type *names-to-bidi-char-type*))
      (destructuring-bind (start . end) range
        (loop for cp from start to end
           do (setf (bidi-info-bidi-type (aref *bidi-data* cp)) char-type))))))

(defun read-bidi-class-file ()
  (with-open-file (*standard-input*
                   (merge-pathnames
                    (make-pathname :name "DerivedBidiClass"
                                   :type "txt"
                                   :directory '(:relative "UNIDATA"))
                    *make-table-dir*)
                   :direction :input)
    (loop for line = (read-line nil nil)
       while line
       do (read-bidi-class-line line))))


(defun read-mirroring-line (line)
  (let ((data (strip-comments-and-split line))
        mirroring
        range)
    (when data
      (setf range (parse-range (first data)))
      (unless (equal (second data) "Bidi_Mirrored")
        (error "Seen ~A instead of Bidi_Mirrored"))
      (setf mirroring t)
      ;;(print (gethash char-type *names-to-bidi-char-type*))
      (destructuring-bind (start . end) range
        (loop for cp from start to end
           do (setf (bidi-info-mirrored (aref *bidi-data* cp)) mirroring))))))


(defun read-mirroring-file ()
  (with-open-file (*standard-input*
                   (merge-pathnames
                    (make-pathname :name "DerivedBinaryProperties"
                                   :type "txt"
                                   :directory '(:relative "UNIDATA"))
                    *make-table-dir*)

                   :direction :input)
    (loop for line = (read-line nil nil)
       while line
       do (read-mirroring-line line))))



(defun read-ucd-files ()
  (read-bidi-class-file)
  (read-mirroring-file))


;;;--- pack data

(defun add-value-to-groups (groups code-point value elem-type)
  (let ((group (ldb *group-bytespec* code-point))
        (char-in-group (ldb *char-bytespec* code-point)))
    
    (let ((group-current-value (aref groups group)))
      (cond ((eql group-current-value :empty) ; no data for this group yet
             (setf (aref groups group) value))
            
            ((arrayp group-current-value) ; group already broken in chars
             (setf (aref group-current-value char-in-group) value))
            
            (t                        ; group with equal values so far
             (unless (eql group-current-value value)
               ;; break the group now
               (setf (aref groups group)
                     (make-array *num-chars* :initial-element group-current-value
                                 :element-type elem-type))
               (let ((chars (aref groups group)))
                 (setf (aref chars char-in-group) value))))))))


(defun add-value-to-planes (planes code-point value elem-type)
  (let ((plane (ldb *plane-bytespec* code-point))
        (group (ldb *group-bytespec* code-point)))
    (let ((plane-current-value (aref planes plane)))
      (cond ((eql plane-current-value :empty)          ; no data for this plane yet
             (setf (aref planes plane) value))
            
            ((arrayp plane-current-value)  ; plane already broken in groups
             (add-value-to-groups plane-current-value code-point value elem-type))
            
            (t    ; plane with equal values so far
             (unless (eql plane-current-value value)
               ; break the plane now
               (setf (aref planes plane)
                     (make-array *num-groups* :initial-element :empty))
               (let ((groups (aref planes plane)))
                (loop for g from 0 to group
                   do (setf (aref groups g) plane-current-value))
                (add-value-to-groups groups code-point value elem-type))))))))


(defun init-planes (planes)
  (loop for idx from 0 below (length planes)
       do (setf (aref planes idx) :empty)))

(defun pack-data ()
  (init-planes *type-planes*)
  (init-planes *mirrored-planes*)
  (loop for code-point from 0 below +ucd-set-size+
     do (let* ((val (aref *bidi-data* code-point))
               (char-type (bidi-info-bidi-type val))
                (mirrored (bidi-info-mirrored val)))

          (add-value-to-planes *type-planes* code-point char-type 'bidi-char-type)
          (add-value-to-planes *mirrored-planes* code-point mirrored 'boolean))))


(defun analyze-data (planes)
  (loop for plane across planes
       with planes-unbroken = 0
       with groups-total = 0
       with groups-unbroken = 0
       with chars = 0
       do (if (not (arrayp plane))
              (incf planes-unbroken)
              (progn
                (incf groups-total *num-groups*)
                (loop for group across plane
                   do (if (not (arrayp group))
                          (incf groups-unbroken)
                          (incf chars (length group))
                          ))))
     finally (progn
               (format t "~&char: ~a    group: ~a" *char-bytespec* *group-bytespec*)
               (format t "~&plane-values: ~a of ~a" planes-unbroken *num-planes*)
               (format t "~&group-values: ~a of ~a" groups-unbroken groups-total)
               (format t "~&char-values: ~a" chars)
               (format t "~&total: ~a~%" (+ planes-unbroken groups-unbroken chars)))
       )
  )



(defun verify-data ()
  (loop for code-point from 0 below +ucd-set-size+
     do (let* ((val (aref *bidi-data* code-point))
               (char-type (bidi-info-bidi-type val))
               (mirrored (bidi-info-mirrored val)))
          (let ((newval (access-value *type-planes* code-point)))
            (unless (eql char-type newval)
              (error "Expected char-type: ~a, got: ~a, codepoint: ~x"
                     char-type newval code-point)))
          (let ((newval (access-value *mirrored-planes* code-point)))
            (unless (eql mirrored newval)
              (error "Expected mirrored: ~a, got: ~a, codepoint: ~x"
                     mirrored newval code-point)))
          )))

;;;---

(defun write-header ()
  (prin1 ";;;
;;; This file is generated automatically.
;;; Do not edit.
;;;
"
         (format t "~&~a" "(in-package #:libidi)")))

(defvar *element-type-to-write* "'nil"
  "Element type (as string), will be rebound for each array")

(defun write-group-elem (elem)
  (if (not (arrayp elem))
      (prin1 elem)
      (progn
        (format t "#.(make-array ~A :element-type ~A :initial-contents #( "
                (length elem)
                *element-type-to-write*)
        (loop for el across elem
           do (progn
                (prin1 el)
                (format t " ")))
        (format t "))~%"))))


(defun write-plane-elem (elem)
  (if (not (arrayp elem))
      (prin1 elem)
      (progn
        (format t "#.(make-array ~a :initial-contents #( " (length elem))
        (loop for el across elem
           do (progn
                (write-group-elem el)
                (format t " ")))
        (format t "))~%"))
      ))


(defun write-variable (variable variable-name element-type-as-string)
  (let ((varname (string-upcase variable-name))
        (*element-type-to-write* element-type-as-string))
    (format t "~%(defparameter ~a ~%" varname)
    (format t "#.(make-array ~a :initial-contents #( " (length variable))
    (loop for elem across variable
       do (progn
            (write-plane-elem elem)
            (format t " ")))
    (format t "))~%")
    (format t "~%) ;; end ~a ~%" varname)))


(defun write-output (&key (filename "bidi-data"))
  (with-standard-io-syntax
    (with-open-file (*standard-output*
                     (make-pathname :name filename
                                    :type "lisp"
                                    ;;:directory '(:relative "UNIDATA")
                                    :defaults *make-table-dir*)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (write-header)
      (write-variable *type-planes* '*type-planes* "'(integer 0 18)")
      (write-variable *mirrored-planes* '*mirrored-planes* "'boolean")
      )))

;;;---

(defun time-access ()
  (time
   (loop repeat 1000000
      with val
      do (setf val (access-value *type-planes* (random 65534)))
      finally (return val)
      )))

(defun perform-run ()
  (format t "~&reading file...") (finish-output)
  (read-ucd-files)
  
  (format t "~&packing data...") (finish-output)
  (pack-data)
  
  (format t "~&verifying...") (finish-output)
  (verify-data)
  
  (format t "~&collecting stats...~%") (finish-output)
  (analyze-data *mirrored-planes*)
  (analyze-data *type-planes*)

;;;   (format t "~&timing...") (finish-output)
;;;   (time-access)
  (write-output)
  )


;;; ---
;;; Test data
(defparameter *test-type-planes* (make-array *num-planes* :initial-element :empty))
(defparameter *test-mirrored-planes* (make-array *num-planes* :initial-element :empty))

(defun write-test-output (&key (filename "test-bidi-data"))
  (with-standard-io-syntax
      (with-open-file (*standard-output*
                       (make-pathname :name filename
                                      :type "lisp"
                                      ;;:directory '(:relative "UNIDATA")
                                      :defaults *make-table-dir*)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
        (write-header)
        (write-variable *type-planes* '*test-type-planes* "'(integer 0 18)")
        ;;      (write-variable *mirrored-planes* '*test-mirrored-planes* "'boolean")
        )))


(defun modify-data-for-tests ()
  ;; Make A..Z strong RTL
  (loop for code from (char-code #\A) to (char-code #\Z)
        do (setf (bidi-info-bidi-type (aref *bidi-data* code)) +BT-R+)))


(defun make-test-data ()
  (let ((*test-type-planes* *type-planes*)
        (*type-planes* *test-type-planes*)
        (*test-mirrored-planes* *mirrored-planes*)
        (*mirrored-planes* *test-mirrored-planes*))
    
    (format t "~&reading file...") (finish-output)
    (read-ucd-files)


    (modify-data-for-tests)
    
    (format t "~&packing data...") (finish-output)
    (pack-data)
    
    (format t "~&verifying...") (finish-output)
    (verify-data)
    
    (write-test-output)))
