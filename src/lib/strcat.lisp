(in-package #:tpd2.lib)


(defun strcat-go (&rest args)
  (declare (optimize speed))
  (apply #'concatenate 'string (mapcar (lambda(arg)(force-string arg)) args)))

(declaim (inline strcat-go))

#+replace-is-not-slow-like-a-dog
(define-compiler-macro strcat (&rest original-args)
  (let ((args (merge-constant-arguments original-args :join 'strcat-go :process-one 'force-string)))
    (if (not (rest args))
	(first args)
	(with-unique-names (len result i)
	  (let ((argnames (loop for i from 0 below (length args)
			     collect (gensym (format nil "arg-~D-" i)))))
	    `(let ,(loop for arg in args
		      for argname in argnames
		      collect `(,argname ,arg))
	       (declare (optimize speed (safety 0)))
	       (declare (type string ,@argnames))
	       (let ((,len (the fixnum (+ ,@(loop for argname in argnames collect `(the fixnum (length ,argname)))))))
		 (let ((,result (make-string ,len)) (,i 0))
		   (declare (type fixnum ,i) (type string ,result))
		   ,@(loop for argname in argnames
			   collect `(replace ,result ,argname :start1 ,i)
			   collect `(incf ,i (length ,argname)))
		   (the string ,result)))))))))

;;; Maybe rewrite using replace; NO on SBCL it is slower (why !?)
;;; XXX are the fixnums at all necessary
(define-compiler-macro strcat (&rest original-args)
  (let ((args (merge-constant-arguments original-args :join 'strcat-go :process-one 'force-string)))
    (if (not (rest args))
	(first args)
	(with-unique-names (len result i c)
	  (let ((argnames (loop for i from 0 below (length args)
			     collect (gensym (format nil "arg-~D-" i)))))
	    `(let ,(loop for arg in args
		      for argname in argnames
		      collect `(,argname ,arg))
	       (declare (optimize speed (safety 0)))
	       (declare (type simple-string ,@argnames))
	       (let ((,len (the fixnum (+ ,@(loop for argname in argnames collect `(the fixnum (length ,argname)))))))
		 (let ((,result (make-string ,len))
		       (,i 0))
		   ,@(loop for arg in argnames collect
			  `(loop for ,c across ,arg do
				(setf (char ,result ,i) ,c)
				(incf ,i)))
		   (the string ,result)))))))))

(defun strcat (&rest args)
  (declare (optimize speed))
  (apply #'strcat-go args))
(declaim (ftype (function (&rest t) string) strcat))
(declaim (inline strcat))

(defun strcat-bench ()
  (declare (optimize speed))
  (let ((x "abcdefghijklmnopqrstuvw") (z "z"))
    (dotimes (i 2000000)
      (strcat x "xy" z))))
