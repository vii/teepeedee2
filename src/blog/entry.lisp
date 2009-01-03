(in-package #:tpd2.blog)

(defrecord comment
  (entry-index-name :index t)
  text
  (author :index t)
  (time :initform (get-universal-time))
  trace-details)

(defmyclass (entry (:include simple-channel))
  blog
  name
  tags
  (title "Untitled")
  time
  paragraphs)

(my-defun entry 'simple-channel-body-ml ()
  (output-object-to-ml
   (my comments))) 

(defun time-string (&optional (ut (get-universal-time)))
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time ut 0)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D UTC" year month date hour minute second)))

(my-defun entry filename ()
  (strcat (its dir (my blog)) (my name)))

(my-defun entry ready ()
  (>= (get-universal-time) (my time)))

(my-defun entry url-path ()
  (with-sendbuf () (its link-base (my blog)) (my name)))

(my-defun entry link ()
  (page-link (my url-path)))

(my-defun entry index-name ()
  (strcat (its name (my blog)) ":" (my name)))

(my-defun entry body-ml ()
  (<div :class "blog-entry-body"
	(loop for p in (my paragraphs)
	      do (<p (output-raw-ml p)))
	(<p :class "time" "Posted " (time-string (my time)))))

(my-defun entry comments ()
  (datastore-retrieve-indexed 'comment 'entry-index-name (my index-name)))

(my-defun entry comment-ml ()
  (<div :class "blog-entry-post-comment"
	(let ((hidden-value (force-byte-vector (time-string))))
	  (html-action-form "Post a comment"
	      ((author "Anonymous")
	       (text nil :type '<textarea)
	       (keep-this-empty nil :type :hidden) (time hidden-value :type :hidden))

	    (cond ((and (zerop (length keep-this-empty)) (equalp hidden-value time))
		   (make-comment 
		    :author author
		    :text text
		    :trace-details ...
		    :entry-index-name (my index-name))
		   (my 'channel-notify))
		  (t
		   (webapp "Comment rejected by spam protection"
		     (<p "Sorry for the inconvenience. Please contact the blog owner with a description of the problem."))))))))

(my-defun entry 'object-to-ml ()
  (<div :class "blog-entry"
	(my body-ml)
	(call-next-method)
	(my comment-ml)))

(my-defun entry set-page ()
  (defpage-lambda (my url-path)
      (lambda()
	(webapp (my name)
	  (output-object-to-ml me)))))



(my-defun entry read-paragraphs-from-stream (stream)
  (setf (my paragraphs)
	(loop for paragraph = (loop for line = (read-line stream nil "")
				    until (if-match-bind ((*(space)) (last)) line)
				    collect line collect (string #\Newline))
	      until (not paragraph)
	      collect (match-replace-all (apply 'strcat paragraph) 
					 ("${static-base}"  (blog-static-base-url (my blog)))))))

(defun parse-time (str)
  (match-bind ( (- #\0 #\9) 
	       (year (and ((string) a0 (month

  (flet ((parse-number (str)
	   (or (and str (parse-integer str)) 0)))
    (regex-bind ((#'parse-number year) 
		 (#'parse-number month) 
		 (#'parse-number date) 
		 (#'parse-number hour)  
		 (#'parse-number minute) 
		 (#'parse-number second))
		("(\\d\\d\\d\\d)\\D*(\\d\\d?)\\D*(\\d\\d?)\\D*(?:(\\d\\d?)\\D*(\\d\\d?)\\D*(?:(\\d\\d?))?)?" str)
		(apply 'encode-universal-time (mapcar (lambda(x) (or x 0)) (list second minute hour date month year))))))

(defun read-in-blog-entry (name)
  (let ((blog-entry (make-blog-entry :name name)))
    (with-shorthand-accessor (my blog-entry)
      (with-open-file (stream (my filename))
	(setf (my time) (or (file-write-date stream) (get-universal-time)))

	(loop for line = (read-line stream nil "")
	      until (if-match-bind ( (* (space)) (last)) line)
	      do (when (if-match-bind "XXX" line)
		   (format *debug-io* "Entry not ready (XXX): ~A~&" name)
		   (return-from read-in-blog-entry))
	      do (match-bind ((* (space)) header ":" value)
			     line
			     (when (equalp header "time")
			       (setf value (parse-time value)))
			  
			  (setf (slot-value blog-entry (normally-capitalized-string-to-symbol header))
				value)))
	(my read-paragraphs-from-stream stream))
      (my publish))))

