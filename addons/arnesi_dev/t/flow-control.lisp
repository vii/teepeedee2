;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.flow-control :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.flow-control)

(test flow-control
  (let ((ht (make-hash-table)))
    (setf (gethash 'a ht) 1)
    (setf (gethash 'b ht) 'a)

    ;; if-bind and aif
    (is (= 3 (if-bind var (gethash 'z ht) (1+ var) 3)))
    (is (= 2 (if-bind var (gethash 'a ht) (1+ var) 3)))
    (is (= 3 (aif (gethash 'z ht) (1+ it) 3)))
    (is (= 2 (aif (gethash 'a ht) (1+ it) 3)))
    ;; when-bind and awhen
    (let ((result nil))
      (when-bind var (gethash 'z ht)
        (setf result (1+ var)))
      (is (null result))
      (when-bind var (gethash 'a ht)
        (setf result (1+ var)))
      (is (= 2 result))
      (setf result nil)
      (awhen (gethash 'z ht)
        (setf result (1+ it)))
      (is (null result))
      (awhen (gethash 'a ht)
        (setf result (1+ it)))
      (is (= 2 result)))
    ;; cond-bind and acond
    (is (= 99 (cond-bind var
                ((gethash 'z ht) (1+ var))
                ((gethash 'y ht) (1+ var))
                (t 99))))
    (is (= 2 (cond-bind var
               ((gethash 'z ht) (1+ var))
               ((gethash 'a ht) (1+ var))
                (t 99))))
    (is (= 1 (cond-bind var
              ((gethash 'z ht))
              ((gethash 'y ht))
              ((gethash 'a ht))
              (t 99))))
    (is (= 99 (acond
               ((gethash 'z ht) (1+ it))
               ((gethash 'y ht) (1+ it))
                (t 99))))
    (is (= 2 (acond
              ((gethash 'z ht) (1+ it))
              ((gethash 'a ht) (1+ it))
              (t 99))))
    (is (= 2 (acond
              ((gethash 'z ht))
              ((gethash 'a ht) (1+ it))
              (t 99))))
    ;; and-bind and aand
    (is-false (and-bind var (gethash 'z ht) (gethash var ht) (1+ var)))
    (is (= 2 (and-bind var (gethash 'b ht) (gethash var ht) (1+ var))))
    (is-false (aand (gethash 'z ht) (gethash it ht) (1+ it)))
    (is (= 2 (aand (gethash 'b ht) (gethash it ht) (1+ it))))
    ;; whichever
    (let ((result 0))
      (is (member (whichever (progn (incf result) 'a)
                             (progn (incf result) 'b)
                             (progn (incf result) 'c))
                  '(a b c)))
      (is (= 1 result)))
    ;; xor
    (let ((result 0))
      (is (eq 'a (xor (progn (incf result) 'a)
                      (progn (incf result) nil)
                      (progn (incf result) nil))))
      (is (= 3 result))
      (setf result 0)
      (is (eq 'a (xor (progn (incf result) nil)
                      (progn (incf result) 'a)
                      (progn (incf result) nil))))
      (is (= 3 result))
      (setf result 0)
      (is-false (xor (progn (incf result) 'a)
                     (progn (incf result) 'b)
                     (progn (incf result) 'c)))
      (is (= 2 result)))))
    
