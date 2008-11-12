(in-package #:tpd2.demo)

(defrecord message
    (forum-name :index t)
  text
  (author :index t)
  (time :initform (get-universal-time)))
  

(defmyclass (forum (:include simple-channel))
    name)

(defvar *fora* (list
		(make-forum :name "Ubuntu") 
		(make-forum :name "Gentoo")
		(make-forum :name "Debian")))

(defun css ())

(with-site (:page-body-start (lambda(title)
			       (declare (ignore title))
			       `(<div :class "header"	
				      (<h1  
					   (<A :href (page-link "/tlug") 
					       :class "inherit" 
					       (<span :style (css-attrib :color "red") "TLUG") " demo" ))
				      (output-object-to-ml (webapp-frame))))
			     :page-head (lambda(title)
					  `(<head
					    (<title (output-raw-ml ,title))
					    (css)
					    (webapp-default-page-head-contents))))
  (defpage "/tlug" ()
    (webapp "Select forum"
      (webapp-select-one ""
			 *fora*
			 :display (lambda(forum) (<span (its name forum)))
			 :replace
			 (lambda(forum)
			   (webapp ()
			     (webapp-display forum)))))))

(my-defun forum 'object-to-ml ()
  (<div :class "forum"
	(<h3 (my name))
	(html-action-form "Post a message"
	    (text)
	  (make-message :forum-name (my name)
			:text text
			:author (frame-username (webapp-frame)))
	  (my 'channel-notify)
	  (values))

	(<div :class "messages"
	      (output-object-to-ml
	       (datastore-retrieve-indexed 'message 'forum-name (my name))))
	(output-raw-ml (call-next-method))))

(defun time-string (ut)
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time ut 0)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D UTC" year month date hour minute second)))

(my-defun message 'object-to-ml ()
  (<div :class "message"
	(<p (my text) (<span :class "message-attribution" " by " (my author) " at " (time-string (my time))))))


