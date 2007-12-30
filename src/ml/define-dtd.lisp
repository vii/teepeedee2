(in-package #:tpd2.ml)

(define-condition ml-validation-error (error) 
  ((tag :initarg :tag) 
   (allowed-attributes :initarg :allowed-attributes)
   (allowed-children :initarg :allowed-children)))

(define-condition ml-validation-forbidden-attribute-error (ml-validation-error)
  ((forbidden-attribute :initarg :forbidden-attribute)))
(define-condition ml-validation-forbidden-child-error (ml-validation-error)
  ((forbidden-child :initarg :forbidden-child)))

(defun validate (contents &key tag attributes children)
  (multiple-value-bind (attrs body)
      (separate-keywords contents)
    (loop for (attr value) on attrs by #'cddr
	  when (not (member (force-string attr) attributes :test 'equalp))
	  do (error 'ml-validation-forbidden-attribute-error
		    :tag tag
		    :allowed-attributes attributes
		    :allowed-children children
		    :forbidden-attribute attr))
    (loop for form in body
	  do (when
		 (typecase form
		   (list
		    (when (and (symbolp (first form)) (eq #\< (char (force-string (first form)) 0))
			       (not (eq (symbol-package (first form)) (find-package :cl))))
		      (when (not (member (force-string (first form)) children :test 'equalp))
			t)))
		   (t (not (loop for child in children
				 thereis (when (listp child)
					   (assert (eq 'function (first child)))
					   (funcall (second child) form))))))
	       (error 'ml-validation-forbidden-child-error
		      :tag tag
		      :allowed-attributes attributes
		      :allowed-children children
		      :forbidden-child form)))))

(defun escape-data (value)
  (when value
    (flet ((xml-entity (c)
	     (force-byte-vector
	      (case c
		(#.(char-code #\<) "&lt;")
		(#.(char-code #\>) "&gt;")
		(#.(char-code #\&) "&amp;")
		(#.(char-code #\') "&apos;")
		(t c)))))
      (match-replace-all ((c (:char-range '(or #\< #\> #\& #\'))))
			 (xml-entity c)
			 (force-string value)))))
(declaim (inline escape-data))

(defmacro output-escaped-ml (value)
  `(output-raw-ml (escape-data ,value)))
(defmacro output-raw-ml (value)
  `(sendbuf-add ml-sendbuf ,value))

(defmacro output-object-to-ml (object)
  `(output-raw-ml (object-to-ml ,object)))

(defgeneric object-to-ml (object))

(defmacro without-ml-output (&body body)
  `(locally ,@body (values)))

(defun-consistent escape-attribute-value (value)
  (escape-data value))

(defmacro with-ml-output (&body body)
  `(macrolet ((with-ml-output (&body body)
			      `(with-sendbuf-continue (ml-sendbuf) ,@body)))
     (with-sendbuf (ml-sendbuf)
       ,@body)))

(defmacro define-dtd (pkg &rest tags-and-defpackage-arguments)
  (multiple-value-bind
	(defpackage-arguments tags)
      (mv-filter (lambda(form)(keywordp (first form))) tags-and-defpackage-arguments)
    (let ((names (mapcar 'force-first tags)))
      (flet ((name-to-str (name)
	       (if (symbolp name)
		   (strcat "<" name)
		   name)))
	(unless (find-package pkg) (make-package pkg))
	`(progn 
	   (defpackage ,pkg
	     ,@defpackage-arguments
	     (:export ,@(mapcar 'make-symbol (mapcar #'name-to-str names))))
	   ,@(loop for tag in tags collect
		   (destructuring-bind (name &key attributes children etag-optional stag-optional)
		       tag
		     (declare (ignore stag-optional))
		     (let ((tag-sym (intern (strcat "<" name) (find-package pkg))))
		       `(defmacro ,tag-sym (&body contents)
			  (validate contents :tag ',tag-sym
				    :attributes ',(mapcar (lambda(x)(force-string x)) attributes) 
				    :children ',(mapcar #'name-to-str children))
			  (multiple-value-bind (attrs body)
			      (separate-keywords contents)
			
			    `(with-ml-output
			       ,,(strcat "<" name)
			       ,@(loop for (attr value) on attrs by #'cddr
				       collect " "
				       collect (string-downcase (force-string attr))
				       collect "='"
				       collect `(escape-attribute-value ,value)
				       collect "'")
			       ">"
			       ,@(mapcar (lambda(form)
					   `(escape-data ,form)) body)
			       ,@(unless (and ,etag-optional (not body))
					 (list ,(strcat "</" name ">"))))))))))))))
