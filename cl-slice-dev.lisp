;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-slice-dev
  (:use #:cl #:alexandria #:anaphora #:let-plus)
  (:export
   #:canonical-singleton
   #:canonical-range
   #:canonical-sequence
   #:axis-dimension
   #:canonical-representation
   #:canonical-representations
   #:all-singleton-representations?
   #:representation-dimensions
   #:row-major-setup
   #:column-major-setup
   #:iterator-over-representations
   #:loop-over-representations
   #:&slice-iterator
   #:traverse-representations))

(in-package #:cl-slice-dev)

;;; resolving slices into canonical representations

(defun canonical-singleton (index)
  "Create a canonical representation of a singleton index."
  (assert (typep index 'array-index))
  index)

(defstruct (canonical-range
            (:constructor canonical-range% (start end)))
  "Canonical representation of a contiguous set of array indices from
START (inclusive) to END (exclusive)."
  (start nil :type array-index)
  (end nil :type array-index))

(defun canonical-range (start end)
  "Create a canonical representation of a contiguous set of array indices from
START (inclusive) to END (exclusive)."
  (assert (and (typep start 'array-index)
               (typep end 'array-index)
               (< start end)))
  (canonical-range% start end))

(defstruct (canonical-sequence
            (:constructor canonical-sequence% (vector)))
  "Canonical representation of a sequence of array indexes."
  (vector nil :type (simple-array array-index (*))))

(defun canonical-sequence (sequence)
  "Create a canonical representation of array indexes from sequence."
  (let ((vector (coerce sequence '(simple-array array-index (*)))))
    (assert (and (plusp (length vector))
                 (every (lambda (index)
                          (typep index 'array-index))
                        vector)))
    (canonical-sequence% vector)))

(defgeneric axis-dimension (axis)
  (:documentation "Return the dimension of axis."))

(defgeneric canonical-representation (axis slice)
  (:documentation "Canonical representation of SLICE, given information in
AXIS.  The default methods just use dimensions as AXIS.")
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
  (:method (axis (slice (eql t)))
    (canonical-range 0 (axis-dimension axis)))
  (:method (axis (slice bit-vector))
    (canonical-sequence (loop for bit across slice
                              for index from 0
                              when (plusp bit) collect index))))

(defun canonical-representations (axes slices)
  "Return the canonical representations of SLICES given the corresponding
AXES, checking for matching length."
  (assert (length= axes slices))
  (mapcar #'canonical-representation axes slices))

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
is called, calling CARRY when it reaches the end of its range."
  (flet ((save (value)
           (setf (car cons) value))
         (carry ()
           (funcall carry)))
    (aetypecase representation
      (array-index carry)
      (canonical-range (let+ (((&structure-r/o canonical-range- start end) it)
                              (slice start))
                         (lambda ()
                           (when (= (save (incf slice)) end)
                             (setf slice start)
                             (carry)))))
      (canonical-sequence (let* ((vector (canonical-sequence-vector it))
                                 (dimension (length vector))
                                 (position 0))
                            (lambda ()
                              (save (aref vector position))
                              (when (= (incf position) dimension)
                                (setf position 0)
                                (carry))))))))

(defun row-major-setup (representations terminator)
  (let ((iterator terminator)
        (subscripts (mapcar #'representation-initial-value representations)))
    (loop for r in representations
          for cons on subscripts
          do (setf iterator
                   (representation-iterator r iterator cons)))
    (values subscripts iterator)))

(defun column-major-setup (representations terminator)
  (let+ (((&values subscripts iterator)
          (row-major-setup (reverse representations) terminator)))
    (values (nreverse subscripts) iterator)))

(define-let+-expansion (&slice-iterator (subscripts next)
                        :value-var value :body-var body)
  (with-unique-names (next-var)
    `(let+ (((&values ,subscripts ,next-var) ,value)
            ((&flet ,next () (funcall ,next-var))))
       ,@body)))

(defmacro traverse-representations ((subscripts representations
                                     &key index
                                          (setup 'row-major-setup))
                                    &body body)
  (with-unique-names (block-name next)
    `(block ,block-name
       (let+ (((&slice-iterator ,subscripts ,next)
               (,setup ,representations (lambda () (return-from ,block-name)))))
         (loop ,@(when index `(for ,index from 0))
           do ,@body
                  (,next))))))
