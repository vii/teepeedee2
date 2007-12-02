;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.walk :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.walk)

(defun test-walk (form)
  (values (equal (unwalk-form (walk-form form)) form)
	  (unwalk-form (walk-form form))
	  form))

(test walk-constant
  (is (test-walk 1))
  (is (test-walk ''a))
  (is (test-walk "a"))
  (is (test-walk '(1 2 3)))
  (is (test-walk '#(1 2 3))))

(test walk-variable
  (is (test-walk 'var)))

(test walk-application
  (is (test-walk '(* 2 3)))
  (is (test-walk '(+ (* 3 3) (* 4 4)))))

(test walk-lambda-application
  (is (test-walk '((lambda (x) (x x)) #'(lambda (x) (x x)))))
  (is (test-walk '((lambda (x k) (k x)) (if p x y) id))))

(test walk-lambda-function
  (is (test-walk '#'(lambda (x y) (y x))))
  (is (test-walk '#'(lambda (x &key y z) (z (y x)))))
  (is (test-walk '#'(lambda (&optional port) (close port))))
  (is (test-walk '#'(lambda (x &rest args) (apply x args))))
  (is (test-walk '#'(lambda (object &key a &allow-other-keys) (values))))
  ;; Unwalking argument lists is lax.
  (is (test-walk '#'(lambda (&rest args &key a b &optional x &allow-other-keys) 2))))

(test walk-block
  (is (test-walk '(block label (get-up) (eat-food) (go-to-sleep))))
  (is (test-walk '(block label ((lambda (f x) (f (f x))) #'car))))
  (is (test-walk '(block label (reachable) (return-from label 'done) (unreachable)))))

(test walk-catch
  (is (test-walk '(catch 'done (with-call/cc* (* 2 3)))))
  (is (test-walk '(catch 'scheduler
		   (tagbody start
		      (funcall thunk)
		      (if (done-p) (throw 'scheduler 'done) (go start))))))
  (is (test-walk '(catch 'c
		   (flet ((c1 () (throw 'c 1)))
		     (catch 'c (c1) (print 'unreachable))
		     2)))))

(test walk-if
  (is (test-walk '(if p x y)))
  (is (test-walk '(if (pred x) (f x) (f-tail y #(1 2 3))))))

(test walk-flet
  (is (test-walk '(flet ((sq (x)
			  (* x x)))
		   (+ (sq 3) (sq 4)))))
  (is (test-walk '(flet ((prline (s)
			  (princ s)
			  (terpri)))
		   (prline "hello")
		   (prline "world")))))

(test walk-labels
  (is (test-walk '(labels ((fac-acc (n acc)
			    (if (zerop n)
				(land acc)
				(bounce
				 (fac-acc (1- n) (* n acc))))))
		   (fac-acc (fac-acc 10 1) 1))))
  (is (test-walk '(labels ((evenp (n)
			    (if (zerop n) t (oddp (1- n))))
			   (oddp (n)
			    (if (zerop n) nil (evenp (1- n)))))
		   (oddp 666)))))

(test walk-let
  (is (test-walk '(let ((a 2) (b 3) (c 4))
		   (+ (- a b) (- b c) (- c a)))))
  (is (test-walk '(let ((a b) (b a)) (format t "side-effect~%") (f a b)))))

(test walk-let*
  (is (test-walk '(let* ((a (random 100)) (b (* a a))) (- b a))))
  (is (test-walk '(let* ((a b) (b a)) (equal a b)))))

(test walk-load-time-value
  (is (test-walk '(load-time-value *load-pathname* nil))))

(test walk-locally
  (is (test-walk '(locally (setq *global* (whoops))))))

(test walk-macrolet
  (is (unwalk-form
       (walk-form
	'(macrolet ((+ (&body body)
		     (reverse body)))
	  (+ 1 2 3 -))))
      '(locally (- 3 2 1)))
  (is (unwalk-form
       (walk-form
	'(macrolet ())))
      '(locally ()))
  (is (unwalk-form
       (walk-form
	'(macrolet ((+ (&body body)
		     (reverse body)))
	  (princ "1111")
	  (+ 1 2 3 -))))
      '(locally
	(princ "1111")
	(- 3 2 1))))

(test walk-multiple-value-call
  (is (test-walk '(multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))))
  (is (test-walk '(multiple-value-call #'+ (floor 5 3) (floor 19 4)))))

(test walk-multiple-value-prog1
  (is (test-walk '(multiple-value-prog1
		   (values-list temp)
		   (setq temp nil)
		   (values-list temp)))))

(test walk-progn
  (is (test-walk '(progn (f a) (f-tail b) c)))
  (is (test-walk '(progn #'(lambda (x) (x x)) 2 'a))))

(test walk-progv
  (is (test-walk '(progv '(*x*) '(2) *x*))))

(test walk-setq
  (is (test-walk '(setq x '(2 #(3 5 7) 11 "13" '17))))
  (is (test-walk '(setq *global* 'symbol))))

(test walk-symbol-macrolet
  (is (unwalk-form
       (walk-form
	'(symbol-macrolet ((a (slot-value obj 'a))
			   (b (slot-value obj 'b)))
	  (+ a b))))
      '(locally
	(+ (slot-value obj 'a) (slot-value obj 'b))))
  (is (unwalk-form
       (walk-form
	'(symbol-macrolet ())))
      '(locally))
  (is (unwalk-form
       (walk-form
	'(symbol-macrolet ((a (slot-value obj 'a)))
	  (double! a)
	  (/ a 2))))
      '(locally
	(double! (slot-value obj 'a))
	(/ (slot-value obj 'a) 2))))

(test walk-tagbody
  (is (test-walk '(tagbody
		   (setq val 1)
		   (go point-a)
		   (setq val (+ val 16))
		   point-c
		   (setq val (+ val 4))
		   (go point-b)
		   (setq val (+ val 32))
		   point-a
		   (setq val (+ val 2))
		   (go point-c)
		   (setq val (+ val 64))
		   point-b
		   (setq val (+ val 8)))))
  (is (test-walk '(tagbody 
		   (setq n (f2 flag #'(lambda () (go out))))
		   out
		   (prin1 n)))))

(test walk-the
  (is (test-walk '(the number (reverse "naoh"))))
  (is (test-walk '(the string 1))))

(test walk-unwind-protect
  (is (test-walk '(unwind-protect
		   (progn (setq count (+ count 1))
			  (perform-access))
		   (setq count (- count 1)))))
  (is (test-walk '(unwind-protect
		   (progn (with-call/cc* (walk-the-plank))
			  (pushed-off-the-plank))
		   (save-life)))))
