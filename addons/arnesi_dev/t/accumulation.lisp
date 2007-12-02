;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.accumulation :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.accumulation)

(test make-reducer

  (let ((r (make-reducer #'+ 0)))
    (funcall r 0)
    (funcall r 1 2)
    (funcall r 1 2 3)
    (is (= 9 (funcall r)))))

