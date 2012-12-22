;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-slice-tests
  (:use #:cl #:cl-slice #:clunit))

(in-package #:cl-slice-tests)

(defsuite slice-suite ())

(defsuite representation-suite (slice-suite))

(deffixture representation-suite (@body)
  (let ((vec10 (vector 0 1 2 3 4 5 6 7 8 9)))
    @body))

(deftest test-representation (representation-suite)
  (assert-equalp (slice vec10 1) 1)
  (assert-equalp (slice vec10 -1) 9)
  (assert-equalp (slice vec10 (cons 3 5)) #(3 4)))
