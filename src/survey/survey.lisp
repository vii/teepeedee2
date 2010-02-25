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

(my-defun response 'object-to-ml ()
  (<tr :class "survey-response"
       (loop for r in (my responses)
	     do (<td r))))

(my-defun survey-channel responses ()
  (datastore-retrieve-indexed 'response 'survey-name (survey-name (my survey))))

(my-defun survey-channel questions ()
  (survey-questions (my survey)))

(my-defun survey-channel 'simple-channel-body-ml ()
  (<table :class "survey-channel"
	  (<thead
	    (loop for q in (my questions) do (<th (question-text q))))
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
	   (lambda (,@qvars .javascript.)
	     (cond ((or ,@qvars)
		    (let ((chan (find-channel ,(my channel-name))))
		      (make-response :survey-name ,(my name)
				     :responses (list ,@qvars))
		      (channel-notify chan)
		      (cond (.javascript. (webapp-respond-ajax-body))
			    (t
			     (webapp "Thank you for responding"
			       (output-object-to-ml chan))))))
		   (t
		    (webapp ,(my name)
		      (<div :class "survey"
			    (html-action-form ("" :action-link ,path :async nil)
				,(loop for q in (my questions)
				       for v in qvars
				       collect `(,v nil :label ,(question-text q) :type :select-one :options ,(question-choices q)))))))))
	 :create-frame nil))))

(my-defun survey register (path)
  (eval (my gen-page-lambda path)))