;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tests.lisp --- trivial-garbage tests.
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

(defpackage #:trivial-garbage-tests
  (:use #:cl #:trivial-garbage #:regression-test))

(in-package #:trivial-garbage-tests)

;;;; Weak Pointers

(deftest pointers.1
    (weak-pointer-p (make-weak-pointer 42))
  t)

(deftest pointers.2
    (weak-pointer-value (make-weak-pointer 42))
  42)

;;;; Weak Hashtables

#+(or :sbcl :corman)
(progn
  (pushnew 'hashtables.weak-key.1 rt::*expected-failures*)
  (pushnew 'hashtables.weak-key.2 rt::*expected-failures*))

(deftest hashtables.weak-key.1
    (let ((ht (make-weak-hash-table :weakness :key)))
      (values (hash-table-p ht)
              (hash-table-weakness ht)))
  t :key)

(deftest hashtables.weak-key.2
    (let ((ht (make-weak-hash-table :weakness :key :test 'eq)))
      (values (hash-table-p ht)
              (hash-table-weakness ht)))
  t :key)

#+(or :sbcl :cmu :corman)
(pushnew 'hashtables.weak-value.1 rt::*expected-failures*)

(deftest hashtables.weak-value.1
    (let ((ht (make-weak-hash-table :weakness :value)))
      (values (hash-table-p ht)
              (hash-table-weakness ht)))
  t :value)

(deftest hashtables.not-weak.1
    (hash-table-weakness (make-hash-table))
  nil)

;;;; Finalizers
;;;
;;; These tests are, of course, not very reliable. And they way they're
;;; written doesn't help either. :-/

(defparameter *finalized?* nil)

(defun setup-finalizers (count &optional remove)
  (setq *finalized?* (make-list count))
  (let ((obj (copy-seq "xpto")))
    (dotimes (i count)
      (let ((i i))
        (finalize obj
                  (lambda ()
                    ;;(assert (null *finalized?*))
                    (setf (nth i *finalized?*) t)))))
    (when remove
      (cancel-finalization obj)))
  (gc :full t))

(defun do-it-to-it (setup-function &rest args)
  (apply setup-function args)
  (gc :full t))

(deftest finalizers.1
    (progn
      (do-it-to-it #'setup-finalizers 1)
      (gc :full t)
      (car *finalized?*))
  t)

(deftest finalizers.2
    (progn
      (do-it-to-it #'setup-finalizers 1 t)
      (gc :full t)
      (car *finalized?*))
  nil)

(deftest finalizers.3
    (progn
      (do-it-to-it #'setup-finalizers 3)
      (gc :full t)
      *finalized?*)
  (t t t))

(deftest finalizers.4
    (progn
      (do-it-to-it #'setup-finalizers 3 t)
      (gc :full t)
      *finalized?*)
  (nil nil nil))