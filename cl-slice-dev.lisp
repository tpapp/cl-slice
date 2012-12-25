;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-slice-dev
  (:use #:cl #:alexandria #:anaphora #:let-plus)
  (:export
   ;; resolving slices into canonical representations
   #:canonical-representation
   #:canonical-representations
   #:canonical-singleton
   #:canonical-range
   #:canonical-sequence
   #:axis-dimension
   ;; traversing slices
   #:all-singleton-representations?
   #:representation-dimensions
   #:row-major-setup
   #:traverse-representations))

(in-package #:cl-slice-dev)

;;; resolving slices into canonical representations

(defgeneric canonical-representation (axis slice)
  (:documentation "Canonical representation of SLICE, given information in
AXIS.  The default methods just use dimensions as AXIS.

Each slice needs to be resolved into a canonical representation, which is
either a singleton, a range, or a sequence of subscripts.  They should only be
constructed with the corresponding CANONICAL-SINGLETION, CANONICAL-RANGE and
CANONICAL-SEQUENCE functions.

CANONICAL-REPRESENTATION needs to ensure that the represented subscripts are
valid for the axis.

Unless a specialized method is found, the dimension of the axis is queried
with AXIS-DIMENSION and resolution is attempted using the latter.")
  (:method (axis slice)
    (canonical-representation (axis-dimension axis) slice))
  (:method ((axis integer) (slice null))
    (canonical-singleton axis))
  (:method ((axis integer) (slice integer))
    (canonical-singleton
     (if (minusp slice)
         (aprog1 (+ axis slice)
           (assert (<= 0 it)))
         (aprog1 slice
           (assert (< slice axis))))))
  (:method (axis (slice cons))
    (let+ (((start . end) slice)
           (start (canonical-representation axis start))
           (end (canonical-representation axis end)))
      (canonical-range start end)))
  (:method (axis (slice vector))
    (let+ (subscripts
           ((&flet collect (value)
              (push value subscripts))))
      (loop for s across slice
            do (aetypecase (canonical-representation axis s)
                 (integer (collect it))
                 (cons (loop for index from (car it) below (cdr it)
                             do (collect index)))
                 (vector (map 'nil #'collect it))))
      (canonical-sequence (nreverse subscripts))))
  (:method ((axis integer) (slice (eql t)))
    (canonical-range 0 axis))
  (:method (axis (slice bit-vector))
    (canonical-sequence (loop for bit across slice
                              for index from 0
                              when (plusp bit) collect index))))

(defun canonical-representations (axes slices)
  "Return the canonical representations of SLICES given the corresponding
AXES, checking for matching length."
  (assert (length= axes slices))
  (mapcar #'canonical-representation axes slices))

(defun canonical-singleton (index)
  "Canonical representation of a singleton index (a nonnegative integer, which
is a valid array index)."
  (assert (typep index 'array-index))
  index)

(defstruct canonical-range
  "Canonical representation of a contiguous set of array indices from
START (inclusive) to END (exclusive)."
  (start nil :type array-index)
  (end nil :type array-index))

(defun canonical-range (start end)
  "Canonical representation of a contiguous set of array indices from
START (inclusive) to END (exclusive)."
  (assert (and (typep start 'array-index)
               (typep end 'array-index)
               (< start end)))
  (make-canonical-range :start start :end end))

(defstruct canonical-sequence
  "Canonical representation of a sequence of array indexes."
  (vector nil :type (simple-array array-index (*))))

(defun canonical-sequence (sequence)
  "Canonical representation of array indexes from sequence.  May share
structure.  Vectors of the upgraded type of (SIMPLE-ARRAY ARRAY-INDEX (*)) are
preferred for efficiency, otherwise they are coerced."
  (let ((vector (coerce sequence '(simple-array array-index (*)))))
    (assert (and (plusp (length vector))
                 (every (lambda (index)
                          (typep index 'array-index))
                        vector)))
    (make-canonical-sequence :vector vector)))

(defgeneric axis-dimension (axis)
  (:documentation "Return the dimension of axis.  Needs to be defined for
non-integer axes."))

;;; walking subscripts

(defun all-singleton-representations? (representations)
  "Test if all canonical representations are singletons."
  (every #'integerp representations))

(defun representation-dimension (representation)
  "Return the dimension of a canonical-representation, or NIL for singleton
slices (they are dropped)."
  (aetypecase representation
    (array-index nil)
    (canonical-range (- (canonical-range-end it) (canonical-range-start it)))
    (canonical-sequence (length (canonical-sequence-vector it)))))

(defun representation-dimensions (representations)
  "Return a list for the dimensions of canonical representations, dropping
singletons."
  (loop for r in representations
        for d = (representation-dimension r)
        when d collect d))

(defun representation-initial-value (representation)
  "Initial value for iteration."
  (aetypecase representation
    (array-index it)
    (canonical-range (canonical-range-start it))
    (canonical-sequence (aref (canonical-sequence-vector it) 0))))

(defun representation-iterator (representation carry cons)
  "Return a closure that sets the car of CONS to the next value each time it
is called, resetting and calling CARRY when it reaches the end of its range."
  (flet ((save (value)
           (setf (car cons) value))
         (carry ()
           (funcall carry)))
    (aetypecase representation
      (array-index carry)
      (canonical-range (let+ (((&structure-r/o canonical-range- start end) it)
                              (slice start))
                         (lambda ()
                           (let ((carry? (= (incf slice) end)))
                             (when carry?
                               (setf slice start))
                             (save slice)
                             (when carry?
                               (carry))))))
      (canonical-sequence (let* ((vector (canonical-sequence-vector it))
                                 (dimension (length vector))
                                 (position 0))
                            (lambda ()
                              (when (= (incf position) dimension)
                                (setf position 0)
                                (carry))
                              (save (aref vector position))))))))

(defun row-major-setup (representations terminator)
  (let ((iterator terminator)
        (subscripts (mapcar #'representation-initial-value representations)))
    (loop for r in representations
          for cons on subscripts
          do (setf iterator
                   (representation-iterator r iterator cons)))
    (values subscripts iterator)))

;;; NOTE commented out as untested
;; (defun column-major-setup (representations terminator)
;;   (let+ (((&values subscripts iterator)
;;           (row-major-setup (reverse representations) terminator)))
;;     (values (nreverse subscripts) iterator)))

(defmacro traverse-representations ((subscripts representations
                                     &key index
                                          (setup 'row-major-setup))
                                    &body body)
  "A macro for traversing representations.  Provides SUBSCRIPTS for BODY,
using the given SETUP function.  When INDEX is given, it increases with each
iteration, starting from 0."
  (with-unique-names (block-name next)
    `(block ,block-name
       (let+ (((&values ,subscripts ,next)
               (,setup ,representations (lambda () (return-from ,block-name)))))
         (loop ,@(when index `(for ,index from 0))
               do ,@body
                  (funcall ,next))))))
