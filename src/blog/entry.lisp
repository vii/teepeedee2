(in-package #:tpd2.blog)

(defrecord comment
  (entry-index-name :index t)
  text
  (author :index t)
  (time :initform (get-universal-time))
  trace-details)


(my-defun comment 'object-to-ml ()
  (<div :class "comment"
	(<p :style (css-attrib :white-space "pre") (my text))

	(<p :class "time" "Posted " (time-string (my time)) " by " (<span :class "author" (my author)))))

(defmyclass (entry (:include simple-channel))
  blog
  name
  tags
  (title "Untitled")
  time
  paragraphs)

(my-defun entry 'simple-channel-body-ml ()
  (<div :class "blog-entry-comments"
	(output-object-to-ml
	 (my comments))))

(defun split-into-paragraphs (str)
  (match-split (progn #\Newline (* (or #\Space #\Tab #\Return)) #\Newline)
	       str))
(defun split-into-paragraphs-by-single-line (str)
  (match-split #\Newline 
	       str))

(defun time-string (&optional (ut (get-universal-time)))
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time ut 0)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D GMT" year month date hour minute second)))

(my-defun entry filename ()
  (strcat (its dir (my blog)) (my name)))

(my-defun entry ready ()
  (>= (get-universal-time) (my time)))

(my-defun entry url-path ()
  (byte-vector-cat (its link-base (my blog)) (my name)))

(my-defun entry link ()
  (page-link (my url-path)))

(my-defun entry index-name ()
  (strcat (its name (my blog)) ":" (my name)))

(my-defun entry story-ml ()
  (<div :class "blog-entry-story"
	(loop for p in (my paragraphs)
	      do (<p (output-raw-ml p)))
	(<p :class "time" "Posted " (time-string (my time)))))

(my-defun entry comments ()
  (datastore-retrieve-indexed 'comment 'entry-index-name (my index-name)))

(my-defun entry comment-ml ()
  (<div :class "blog-entry-post-comment"
	(let ((hidden-value (force-byte-vector (time-string))))
	  (html-action-form-collapsed "Post a comment"
	      ((text nil :type <textarea)
	       (author "Anonymous")
	       (keep-this-empty nil :type :hidden) 
	       (time hidden-value :type :hidden))

	    (cond ((and (zerop (length keep-this-empty)) (equalp hidden-value time))
		   (make-comment 
		    :author author
		    :text text
		    :trace-details (frame-trace-info (webapp-frame))
		    :entry-index-name (my index-name))
		   (my 'channel-notify))
		  (t
		   (webapp "Comment rejected by spam protection"
		     (<p "Sorry for the inconvenience. Please contact the blog owner with a description of the problem."))))))))

(my-defun entry 'object-to-ml ()
  (<div :class "blog-entry"
	(my story-ml)
	(call-next-method)
	(my comment-ml)))

(my-defun entry set-page ()
  (with-site ((its site (my blog)))
    (defpage-lambda (my url-path)
	(lambda()
	  (webapp (my title)
	    (output-object-to-ml me))))))

(my-defun entry read-paragraphs-from-buffer (buffer)
  (setf (my paragraphs)
	(split-into-paragraphs
	 (match-replace-all buffer
			    ("${static-base}"  (blog-static-base-url (my blog)))))))

(defun parse-time (str)
  (match-bind 
   (macrolet ((int (name &optional (len 2))
		 `(progn t (,name (unsigned-byte :max-len ,len) 0))))
      (int year 4)
      (int month)
      (int day)
      (:? 
       (int hour)
       (int minute)
       (:? (int second))))
      str
    (encode-universal-time second minute hour day month year)))

(defun slurp-file (filename)
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let ((buf (make-byte-vector (file-length s))))
      (read-sequence buf s)
      buf)))

(defun normally-capitalized-string-to-symbol (string &optional (package (symbol-package 'normally-capitalized-string-to-symbol)))
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*package* package))
      (read-from-string (force-string string)))))

(defun read-in-entry (blog name)
  (let ((entry (make-entry :blog blog :name name)))
    (with-shorthand-accessor (my entry)
      (let ((remaining (slurp-file (my filename))))
	(setf (my time) (file-write-date (my filename)))
	(loop for line = 
	      (match-bind (line #\Newline after)
		  remaining
		(setf remaining after)
		line)
	      until (if-match-bind ( (* (space)) (last)) line)
	      do (when (if-match-bind "XXX" line)
		   (format *debug-io* "Entry not ready (XXX): ~A~&" name)
		   (return-from read-in-entry))
	      do (match-bind ((* (space)) header ":" value)
			     line
			     (when (equalp (force-string header) "time")
			       (setf value (parse-time value)))
			  
			  (setf (slot-value entry (normally-capitalized-string-to-symbol header))
				value)))
	(my read-paragraphs-from-buffer remaining))
      (my set-page))
    entry))

