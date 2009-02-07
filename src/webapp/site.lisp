(in-package #:tpd2.webapp)

(eval-always
  (defconstant +site-customization-funcs+ '(page-head page-body-start page-body-footer))
  (defconstant +site-customization-func-args+ '(title)))

#.`
(defstruct (site (:constructor %make-site))
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




(defvar *default-site*)

; redefined with macrolet
(defun compile-time-default-site ()
  (when (boundp '*default-site*) *default-site*))

(defun make-site (&rest args)
  (let ((*default-site* (apply '%make-site args)))
    (register-action-page)
    (register-channel-page)
    
    (macrolet ((set-site-customization-funcs (site)
		 `(progn
		    ,@(loop for name in +site-customization-funcs+ collect
			    `(setf (,(concat-sym 'site-runtime- name) ,site)
				   (lambda ,+site-customization-func-args+
				     (funcall (,(concat-sym 'site- name) ,site) ,@+site-customization-func-args+)))))))
      (set-site-customization-funcs *default-site*))
    *default-site*))

(defmacro with-site ( (&rest args-for-make-site) &body body)
  (let ((args-for-make-site (copy-list args-for-make-site)))
    (awhen (getf args-for-make-site :dispatcher)
      (typecase it
	((or string byte-vector)
	 (setf (getf args-for-make-site :dispatcher) `(find-or-make-dispatcher ,it)))))
    `(progn
       (eval-always (setf *default-site* (make-site ,@args-for-make-site)))
       
       (macrolet ((compile-time-default-site ()
		    `(load-time-value *default-site*)))
	 ,@body))))

(defun default-site-func-expansion (func &rest args)
  (cond ((boundp '*default-site*)
	 (apply (funcall (concat-sym 'site- func) *default-site*) args))
	(t `(,(concat-sym 'site-runtime- func) 
	      (frame-site (webapp-frame)) ,@args))))
