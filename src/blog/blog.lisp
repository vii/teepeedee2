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
	   (iter:iter (iter:for filename in (cl-fad:list-directory (my dir)))
		 (unless (or (find #\# filename) (find #\~ filename))
		   (let ((entry (read-in-blog-entry me (file-namestring filename))))
		     (when (and entry (blog-entry-ready entry)) 
		       (iter:collect entry)))))
	    #'> :key #'blog-entry-time))
    (defpage-lambda "/"
	(lambda()
	  (webapp (my filename)
	    (output-object-to-ml me))))))

(my-defun blog 'object-to-ml ()
  (<div :class "blog"
	(loop for entry in (my entries)
	      for n below 20
	      do (output-object-to-ml entry))))

