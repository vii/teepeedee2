;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.list :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.list)

(test proper-list-p
  (is-true (proper-list-p '()))
  (is-true (proper-list-p '(nil)))
  (is-true (proper-list-p '(nil nil)))
  (is-true (proper-list-p '(nil nil nil)))
  (is-true (proper-list-p '(nil . nil)))
  (is-true (proper-list-p '(nil nil . nil)))
  (is-true (proper-list-p '(nil nil nil . nil)))
  (is-false (proper-list-p 1))
  (is-false (proper-list-p '(a . b)))
  (let ((a (cons nil nil)))
    (setf (cdr a) a)
    (is-false (proper-list-p a)))
  (let ((a (list nil nil)))
    (setf (cdr (last a)) a)
    (is-false (proper-list-p a)))
  (let ((a (list nil nil nil nil nil)))
    (setf (cdr (last a)) a)
    (is-false (proper-list-p a)))
  (let ((a (list nil nil nil nil nil)))
    (setf (first a) a
	  (car (last a)) a
	  (cdr (last a)) a)
    (is-false (proper-list-p a))))
  
