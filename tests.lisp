;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-slice-tests
  (:use #:cl #:cl-slice #:clunit))

(in-package #:cl-slice-tests)

(defsuite slice-suite ())

;; (run-suite 'representation-suite)

(defsuite representation-suite (slice-suite))

(deffixture representation-suite (@body)
  (let ((vec10 (vector 0 1 2 3 4 5 6 7 8 9)))
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
