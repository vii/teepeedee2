(in-package #:tpd2.io)

(defstruct mux
  (fd-to-con (make-array 128 :element-type '(or null con) :initial-element nil)))

(my-defun mux empty ()
  (every 'not (my fd-to-con))) 

(my-defun mux find-fd (fd)
  (when fd
    (when (> (length (my fd-to-con)) fd)
      (aref (my fd-to-con) fd))))

(my-defun mux add (con)
  (let ((fd (con-socket con)))
    (when fd
      (debug-assert (not (my find-fd fd)))
      (when (>= fd (length (my fd-to-con)))
	(setf (my fd-to-con) (adjust-array (my fd-to-con) (* 2 (length (my fd-to-con))) :initial-element nil))
	(debug-assert (> (length (my fd-to-con)) fd)))
      (setf (aref (my fd-to-con) fd) con))))

(my-defun mux del (fd)
  (when (my find-fd fd)
    (setf (aref (my fd-to-con) fd) nil)))

(my-defun mux close-all ()
  (loop for x across (my fd-to-con)
	when x
	do (hangup x)))
