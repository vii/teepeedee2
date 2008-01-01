(in-package #:tpd2.lib)

(defstruct quick-queue-entry
  next
  prev)

(my-defun quick-queue-entry init ()
  (setf (my next) me)
  (setf (my prev) me)
  me)

(defstruct quick-queue
  (entries 
   (let ((len (* 16 1024)))
     (map '(vector quick-queue-entry) 'identity 
	  (loop for i below len collect 
		   (let ((entry (make-quick-queue-entry)))
		     (quick-queue-entry-init entry)
		     entry))))

   :type (vector quick-queue-entry)))

(my-defun quick-queue len ()
  (length (my entries)))

(my-defun quick-queue get (position)
  (aref (my entries) (mod position (my len))))

(my-defun quick-queue-entry add (base)
  (setf (my prev) base)
  (setf (my next) (quick-queue-entry-next base))
  (setf (quick-queue-entry-next (my prev)) me)
  (setf (quick-queue-entry-prev (my next)) me))

(my-defun quick-queue-entry del ()
  (setf (quick-queue-entry-next (my prev)) (my next))
  (setf (quick-queue-entry-prev (my next)) (my prev)))

  