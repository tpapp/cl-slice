;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-



(cl:defpackage #:cl-slice
  (:use #:cl #:alexandria #:anaphora #:cl-slice-dev #:let-plus)
  (:export
   #:slice
   #:aslice
   #:ref
   #:including
   #:nodrop
   #:head
   #:tail
   #:mask
   #:which))

(in-package #:cl-slice)

;;; generic user interface

(defgeneric ref (object &rest subscripts)
  (:documentation "Return the element of OBJECT specified by SUBSCRIPTS."))

(defgeneric (setf ref) (value object &rest subscripts))

(defgeneric slice (object &rest slices)
  (:documentation "Return the slice of OBJECT specified by SLICES."))

(defgeneric (setf slice) (value object &rest slices))

(defmacro aslice (object &rest subscripts)
  "Anaphoric macro version of SLICE that binds the first argument to IT."
  `(let ((it ,object))
     (slice it ,@subscripts)))

;;; some convenience forms

(defstruct including
  "Range, including both ends."
  start end)

(defun including (start end)
  "Range, including both ends."
  (make-including :start start :end end))

(defmethod canonical-representation (axis (slice including))
  (let+ (((&structure-r/o including- start end) slice)
         (start (canonical-representation axis start))
         (end (canonical-representation axis end)))
    (canonical-range start (1+ end))))

(defstruct nodrop
  "Select a single index, but don't drop a dimension."
  index)

(defun nodrop (index)
  "Slice a single index, but don't drop a dimension."
  (make-nodrop :index index))

(defmethod canonical-representation (axis (slice nodrop))
  (let ((start (canonical-representation axis (nodrop-index slice))))
    (canonical-range start (1+ start))))

(defun head (count)
  "First COUNT indexes."
  (check-type count alexandria:array-index)
  (cons 0 count))

(defun tail (count)
  "Last COUNT indexes."
  (check-type count alexandria:array-index)
  (cons (- count) nil))



;;; implementation for arrays

(defmethod slice ((array array) &rest slices)
  (let* ((representations (canonical-representations (array-dimensions array)
                                                     slices))
         (dimensions (representation-dimensions representations)))
    (if dimensions
        (aprog1 (make-array dimensions
                            :element-type (array-element-type array))
          (traverse-representations (subscripts representations :index index)
            (setf (row-major-aref it index)
                  (apply #'aref array subscripts))))
        (apply #'aref array representations))))

(defmethod (setf slice) ((value array) (array array) &rest slices)
  (let ((representations (canonical-representations (array-dimensions array)
                                                    slices)))
    (assert (equalp (representation-dimensions representations)
                    (array-dimensions value)) () "Incompatible dimensions.")
    (traverse-representations (subscripts representations :index index)
      (setf (apply #'aref array subscripts)
            (row-major-aref value index)))))

(defmethod (setf slice) (value (array array) &rest slices)
  (let ((representations (canonical-representations (array-dimensions array)
                                                    slices)))
    (assert (all-singleton-representations? representations))
    (setf (apply #'aref array representations) value)))

(defmethod ref ((array array) &rest subscripts)
  (let ((representations (canonical-representations (array-dimensions array)
                                                   subscripts)))
    (assert (all-singleton-representations? representations))
    (apply #'aref array representations)))

(defmethod (setf ref) (value (array array) &rest subscripts)
  (let ((representations (canonical-representations (array-dimensions array)
                                                   subscripts)))
    (assert (all-singleton-representations? representations))
    (setf (apply #'aref array representations) value)))

;;; implementation for lists

(defmethod slice ((list list) &rest slices)
  (let ((representations (canonical-representations (list (length list))
                                                   slices))
        values)
    (traverse-representations (subscripts representations)
      (push (nth (car subscripts) list) values))
    (nreverse values)))

;;; masks

(defgeneric mask (predicate sequence)
  (:documentation "Map sequence into a simple-bit-vector, using 1 when PREDICATE yields true, 0 otherwise.")
  (:method (predicate (sequence sequence))
    (map 'bit-vector (lambda (element)
                       (if (funcall predicate element)
                           1
                           0))
         sequence)))

(defgeneric which (predicate sequence)
  (:documentation "Return an index of the positions in SEQUENCE which satisfy PREDICATE.")
  (:method (predicate (sequence sequence))
    (let ((index 0)
          positions)
      (map nil (lambda (element)
                 (when (funcall predicate element)
                   (push index positions))
                 (incf index))
           sequence)
      (coerce (nreverse positions) '(simple-array fixnum (*))))))
