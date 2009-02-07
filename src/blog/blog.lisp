(in-package #:tpd2.blog)

(defstruct blog
  name
  dir
  entries
  (site *default-site*)
  static-base-url)

(my-defun blog read-in ()
  (let ((*default-site* (my site))) 
    (setf (my entries) 
	  (sort
	   (iter:iter (iter:for path in (last (cl-fad:list-directory (my dir)) 10))
		      (let ((filename (force-string path)))
			(unless (or (find #\# filename) (find #\~ filename))
			  (let ((entry (read-in-entry me (file-namestring filename))))
			    (when entry  
			      (iter:collect entry))))))
		      #'> :key #'entry-time))))

(my-defun blog 'object-to-ml ()
  (<div :class "blog"
	(loop for entry in (my entries)
	      for n below 20
	      do (output-object-to-ml entry))))

