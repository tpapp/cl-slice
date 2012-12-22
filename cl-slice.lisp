;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-



(cl:defpackage #:cl-slice
  (:use #:cl #:anaphora #:cl-slice-dev)
  (:export
   #:slice
   #:ref))

(in-package #:cl-slice)

;;; user interface

(defgeneric ref (object &rest subscripts)
  (:documentation "Return the element of OBJECT specified by SUBSCRIPTS."))

(defgeneric (setf ref) (value object &rest subscripts))

(defgeneric slice (object &rest slices)
  (:documentation "Return the slice of OBJECT specified by SLICES."))

(defgeneric (setf slice) (value object &rest slices))




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

(traverse-representations (subscripts (list (canonical-range 0 2)))
  (print subscripts))

(row-major-setup (list (canonical-range 0 2)) (lambda ()))

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
