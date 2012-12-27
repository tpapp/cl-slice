;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-slice-tests
  (:use #:cl #:cl-slice #:clunit))

(in-package #:cl-slice-tests)

(defsuite slice-suite ())

;; (run-suite 'slice-suite :use-debugger t)

(defsuite representation-suite (slice-suite))

(deffixture representation-suite (@body)
  (let ((vec10 #(0 1 2 3 4 5 6 7 8 9)))
    @body))

(deftest test-representation (representation-suite)
  ;; singletons
  (assert-equalp 1 (slice vec10 1))
  (assert-equalp 9 (slice vec10 -1))
  (assert-condition error (slice vec10 10))
  (assert-condition error (slice vec10 nil))
  (assert-condition error (slice vec10 11))
  (assert-condition error (slice vec10 -11))
  ;; ranges
  (assert-equalp #(3 4) (slice vec10 (cons 3 5)))
  (assert-equalp #(7 8 9) (slice vec10 (cons 7 nil)))
  (assert-equalp #(7 8) (slice vec10 (cons 7 -1)))
  (assert-condition error (slice vec10 (cons 5 4))) ; not increasing
  ;; vectors
  (assert-equalp #(2 3 5) (slice vec10 #(2 3 5)))
  (assert-equalp #(2 3 3 7) (slice vec10 #(2 3 3 -3)))
  ;; masks
  (assert-equalp #(3 4 7) (slice vec10 #*0001100100))
  (assert-condition error (slice vec10 #*00)) ; too short
  )

(deftest test-convenience-forms (representation-suite)
  (assert-equalp #(3 4 5) (slice vec10 (including 3 5)))
  (assert-equalp #(5) (slice vec10 (nodrop 5)))
  (assert-equalp #(0 1 2) (slice vec10 (head 3)))
  (assert-equalp #(7 8 9) (slice vec10 (tail 3))))

(defsuite array-suite (slice-suite))

(deffixture array-suite (@body)
  (let ((arr35 #2A((0 1 2 3 4)
                   (5 6 7 8 9)
                   (10 11 12 13 14))))
    @body))

(deftest array-slices (array-suite)
  (assert-equalp #(0 5 10) (slice arr35 t 0))
  (assert-equalp #2A((0) (5) (10)) (slice arr35 t (cons 0 1)))
  (assert-equalp #2A((1 4)
                     (6 9)
                     (11 14))
    (slice arr35 #(0 1 2) #(1 -1)))
  (assert-equalp #(5 6 7 8 9) (slice arr35 1 t))
  (assert-equalp #2A((5 6 7 8 9)) (slice arr35 (cons 1 2) t))
  (assert-equalp #(6 7 8) (slice arr35 1 (cons 1 -1)))
  (assert-equalp #2A((6 7 8)) (slice arr35 (cons 1 2) (cons 1 -1))))

(deftest array-singleton-slices (array-suite)
  (assert-equalp 7 (ref arr35 1 2))
  (assert-equalp 12 (ref arr35 -1 2))
  (assert-condition error (ref arr35 t t))
  (let ((a (make-array '(1 3) :initial-contents '((2 3 5)))))
    (setf (ref a 0 1) 7)
    (assert-equalp #2A((2 7 5)) a)))
