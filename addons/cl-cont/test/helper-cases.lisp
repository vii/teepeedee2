
(in-package :cont-test)

;; nested with-call/cc
(deftest with-call/cc-nested
    (let (cc)
      (values
       (with-call/cc
	 (+ 1 (with-call/cc (let/cc k
			      (setf cc k)
			      (funcall k 2)))))
       (funcall cc 4)))
  3 5)

;; WITHOUT-CALL/CC
(deftest without-call/cc-1
    (let (cc)
      (values
       (with-call/cc
	 1 (without-call/cc 2 3) (let/cc k
				   (setf cc k)
				   (funcall k 4)))
       (funcall cc 10)))
  4 10)

(deftest without-call/cc-2
    (multiple-value-bind (res err)
	(ignore-errors
	  (with-call/cc
	    1 (without-call/cc
		  (let/cc k
		    (funcall k 4))
		3)
	    4))
      (values res (not (null err))))
  nil t)

;;; DEFUN
(defun/cc test-fn/cc-1 (a b)
  (+ a b))

(deftest defun/cc-1
    (test-fn/cc-1 1 2)
  3)

(deftest defun/cc-2
    (with-call/cc (+ 1 (test-fn/cc-1 1 2)))
  4)

(defparameter *test-fn/cc* nil)

(defun/cc test-fn/cc-2 (a b)
  (+ a (let/cc k
	 (setf *test-fn/cc* k)
	 (funcall k b))))

(deftest defun/cc-3
    (values
     (test-fn/cc-2 1 2)
     (funcall *test-fn/cc* 10))
  3 11)

(defun/cc test-fn/cc-3 (a)
  (declare (ignore a))
  1)

(deftest defun/cc-4
    (test-fn/cc-3 10)
  1)

;;; explicit FUNCALL
(deftest explicit-funcall-1
    (let (cc)
      (values
       (with-call/cc (+ 1 (funcall (lambda (a)
				     (+ (let/cc k
					  (setf cc k)
					  (funcall k 1)) a)) 5)))
       (funcall cc 2)))
  7 8)

;;; explicit APPLY
(deftest explicit-apply-1
    (let (cc)
      (values
       (with-call/cc (+ 1 (apply (lambda (a)
				     (+ (let/cc k
					  (setf cc k)
					  (funcall k 1)) a)) (list 5))))
       (funcall cc 2)))
  7 8)

(deftest explicit-apply-2
    (let (cc)
      (values
       (with-call/cc (+ 1 (apply (lambda (a b c)
				     (+ (let/cc k
					  (setf cc k)
					  (funcall k 1))
					a b c))
				 3 4 (list 5))))
       (funcall cc 2)))
  14 15)

(deftest explicit-apply-3
    (with-call/cc 
      (apply #'+ 1 2 3 (list 4 5)))
  15)

;;; LIST on ACL
(defun list-on-acl ()
  (with-call/cc (list 1 2)))

#+allegro (deftest list-on-acl-1
	      (list-on-acl)
	    (1 2))
