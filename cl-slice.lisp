;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-



(cl:defpackage #:cl-slice
  (:use #:cl #:anaphora #:cl-slice-dev #:let-plus)
  (:export
   #:slice
   #:ref
   #:including
   #:nodrop
   #:head
   #:tail))

(in-package #:cl-slice)

;;; generic user interface

(defgeneric ref (object &rest subscripts)
  (:documentation "Return the element of OBJECT specified by SUBSCRIPTS."))

(defgeneric (setf ref) (value object &rest subscripts))

(defgeneric slice (object &rest slices)
  (:documentation "Return the slice of OBJECT specified by SLICES."))

(defgeneric (setf slice) (value object &rest slices))

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
    (canonical-range start (1- end))))

(defstruct nodrop
  "Select a single index, but don't drop a dimension."
  index)

(defun nodrop (index)
  "Slice a single index, but don't drop a dimension."
  (make-nodrop :index index))

(defmethod canonical-representation (axis (slice nodrop))
  (let ((start (canonical-representation axis (nodrop-index slice))))
    (canonical-range start (1+ start))))

(defun head (index)
  "Slice up to INDEX."
  (cons 0 index))

(defun tail (index)
  (cons index nil))



;;; implementation for arrays

(defmethod slice ((array array) &rest slices)
  (declare (optimize debug))
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
    (assert (equalp (representation-dimensions representations) value))
    (traverse-representations (subscripts representations :index index)
      (setf (apply #'aref array subscripts)
            (row-major-aref value index)))))

;;; implementation for lists

(defmethod slice ((list list) &rest slices)
  (let ((representations (canonical-representations (list (length list))
                                                   slices))
        values)
    (traverse-representations (subscripts representations)
      (push (nth (car subscripts) list) values))
    (nreverse values)))
