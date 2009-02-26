(in-package #:tpd2.webapp)

(eval-always
  (defconstant +site-customization-funcs+ '(page-head page-body-start page-body-footer))
  (defconstant +site-customization-func-args+ '(title)))

(defparameter *current-site* nil)

(defmacro current-site ()
  `*current-site*)

#.`
(defstruct (site (:constructor %make-site))
  (runtime-name '*current-site*)
  (dispatcher *default-dispatcher*)
  (page-head (lambda(title)
	       `(<head
		  (<title ,title)
		  (webapp-default-page-head-contents))))
  (page-body-start 
   (lambda(title)
     `(<h1 ,title)))
  (page-body-footer
   (lambda(title)
     (declare (ignore title))
     `(webapp-default-page-footer)))
  ,@(mapcar (lambda(x)(concat-sym 'runtime- x)) +site-customization-funcs+))

(declaim (inline site-runtime-dispatcher))
(defun site-runtime-dispatcher (site)
  (site-dispatcher site))

(defmacro current-site-call (method &rest args)
  `(funcall (,(concat-sym 'site-runtime- method) 
	      (current-site)) ,@args))

(defmacro with-site ((site) &body body)
  (once-only (site)
    `(let ((*current-site* ,site))
       (macrolet ((current-site ()
		    ',site))
	 ,@body))))

(defmacro defsite (name &rest args-for-make-site)
  `(progn
     (defvar ,name)
     (eval-always
       (setf ,name (make-site :runtime-name ',name ,@args-for-make-site)))
     ',name))

(defmacro with-compile-time-site ((site) &body body)
  (check-symbols site)
  (assert (eq site (site-runtime-name (symbol-value site))))
  `(eval-always 
     (macrolet ((current-site-call (method &rest args)
		  (apply (funcall (concat-sym 'site- method) ,site) args))
		  (current-site () ',site)
		(with-site ((&optional site) &body body)
		  (declare (ignore site))
		  `(let ((*current-site* ,',site))
		     ,@body)))
       (with-site ()
	 ,@body))))

(defmacro with-frame-site (&body body)
  `(with-site ((frame-site (webapp-frame)))
     ,@body))
  
(defun make-site (&rest args)
  (let ((args (copy-list args)))
    (awhen (getf args :dispatcher)
      (typecase it
	((or string byte-vector)
	 (setf (getf args :dispatcher) (find-or-make-dispatcher it)))))
    (let ((site (apply '%make-site args)))
      (with-site (site)
	(register-action-page)
	(register-channel-page)
	(macrolet ((def-runtime-funcs (site)
		     `(progn 
			,@(loop for func in +site-customization-funcs+ collect
				`(setf (,(concat-sym 'site-runtime- func) ,site)
				       (compile nil (eval 
						     `(lambda ,+site-customization-func-args+
							(declare (ignorable ,@+site-customization-func-args+))
							,(apply (,(concat-sym 'site- func) ,site) +site-customization-func-args+)))))))))
	  (def-runtime-funcs site))

    	site))))