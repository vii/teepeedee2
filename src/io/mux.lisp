(in-package #:tpd2.io)

(defun make-mux-array (len)
  (make-array len :element-type '(or null con) :initial-element nil))

(defstruct mux
  (fd-to-con (make-mux-array 128)
	     :type (simple-array (or null con))))

(my-defun mux empty ()
	  (my-declare-fast-inline)
	  (every #'not (my fd-to-con))) 

(my-defun mux find-fd (fd)
  (my-declare-fast-inline)	  
  (when fd
    (when (> (length (my fd-to-con)) fd)
      (aref (my fd-to-con) fd))))

(my-defun mux add (con)
  (let ((fd (con-socket con)))
    (when fd
      (debug-assert (not (my find-fd fd)))
      (when (>= fd (length (my fd-to-con)))
	(let ((new (make-mux-array  
		    (loop for length = (* 2 (length (my fd-to-con))) then (* 2 length)
			  thereis (when (> length fd) length)))))
	  (replace new (my fd-to-con))
	  (setf (my fd-to-con) new))
	(debug-assert (> (length (my fd-to-con)) fd)))
      (setf (aref (my fd-to-con) fd) con))))

(my-defun mux del (fd)
  (when (my find-fd fd)
    (debug-assert (= (con-socket (aref (my fd-to-con) fd)) fd))
    (setf (aref (my fd-to-con) fd) nil)))

(my-defun mux close-all ()
  (loop for x across (my fd-to-con)
	when x
	do (hangup x)))
