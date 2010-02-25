(in-package #:tpd2.survey)

(defstruct (question (:type list))
  text
  choices)

(defrecord survey
  (name :index t)
  text
  questions)

(defmyclass (survey-channel (:include simple-channel))
    survey)

(defrecord response
  (survey-name :index t)
  responses
  (time :initform (get-universal-time))
  (trace-details :initform (tpd2.http:servestate-origin*)))

(defun time-string (&optional (ut (get-universal-time)))
  (multiple-value-bind
	(second minute hour date month year)
      (decode-universal-time ut 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D GMT" year month date hour minute second)))

(my-defun response 'object-to-ml ()
  (<tr :class "survey-response"
       (loop for r in (my responses)
	     do (<td :class (byte-vector-cat "response-" r) r))
       (<td :class "timestamp" (time-string (my time)))))

(my-defun survey-channel responses ()
  (datastore-retrieve-indexed 'response 'survey-name (survey-name (my survey))))

(my-defun survey-channel questions ()
  (survey-questions (my survey)))

(my-defun survey-channel 'simple-channel-body-ml ()
  (<table :class "survey-channel"
	  (<thead
	    (<tr
	      (loop for q in (my questions) do (<th (question-text q)))
	      (<th :class "timestamp" "Time")))
	  (loop for c in (my responses) repeat 50 do
		(output-object-to-ml c))))

(my-defun survey channel-name ()
  (byte-vector-cat "survey:" (my name)))
(my-defun survey gen-page-lambda (path)
  (unless (find-channel (my channel-name))
    (make-survey-channel :survey me :id (my channel-name)))
  
  (let ((qvars (loop for i from 0 for q in (my questions) collect (intern (strcat 'q i)))))
    `(with-compile-time-site () 
       (defpage-lambda ,path
	   (lambda (,@qvars view)
	     (let ((chan (find-channel ,(my channel-name))))
	       (cond 
		 (view
		  (webapp "Survey results"
		    (output-object-to-ml chan)))
		 ((or ,@qvars)
		  (make-response :survey-name ,(my name)
				 :responses (list ,@qvars))
		  (channel-notify chan)
		  (webapp "Thank you for responding"
		    (output-object-to-ml chan)))
		 (t
		  (webapp ,(my name)
		    (<div :class "survey"
			  (html-action-form ("" :action-link ,path :async nil)
			      ,(loop for q in (my questions)
				     for v in qvars
				     collect `(,v nil :label ,(question-text q) :type :select-one :options ,(question-choices q))))))))))
	 :create-frame nil))))

(my-defun survey register (path)
  (eval (my gen-page-lambda path)))