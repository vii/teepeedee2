;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.numbers :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.numbers)

(test mulf
  (let ((a 0))
    (is (= 0 (mulf a 10)))
    (is (= 0 a)))
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (let ((orig-a a))
      (mulf a b)
      (is (= a (* orig-a b)))))

  (let ((a 1))
    (is (= 4 (mulf a 4)))
    (is (= 1 (mulf a (/ 4))))
    (is (= 1 a))))

(test minf
  (let ((a 10))
    (is (= 5 (minf a 5)))
    (is (= 5 a)))

  (let ((a 0))
    (is (= 0 (minf a 10)))
    (is (= 0 a))))

(test parse-float
  (is (= 0 (parse-float "0")))
  (is (= -1 (parse-float "-1")))
  (is (= 1 (parse-float "1")))

  (dolist (type '(short-float single-float double-float long-float))
    (for-all ((float (gen-float :type type :bound 1000)))
      (let* ((*print-base* 10)
             (*print-radix* nil))
        (is (= float (parse-float (princ-to-string float) :type type)))))))
