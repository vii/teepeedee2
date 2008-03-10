(in-package #:tpd2.webapp)

(defstruct site
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
     `(webapp-default-page-footer))))

(defvar *default-site*)

(my-defun site register-special-pages ()
  (let ((*default-site* me))
     (register-action-page)
     (register-channel-page)))

(defmacro with-site ( (&rest args-for-make-site) &body body)
  (let ((args-for-make-site (copy-list args-for-make-site)))
    (awhen (getf args-for-make-site :dispatcher)
      (typecase it
	((or string byte-vector)
	 (setf (getf args-for-make-site :dispatcher) `(find-or-make-dispatcher ,it))))) 
    `(progn
       (eval-always (setf *default-site* (make-site ,@args-for-make-site)))
       (site-register-special-pages *default-site*)
       (locally ,@body))))



