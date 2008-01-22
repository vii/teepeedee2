(in-package #:tpd2.lib)


(eval-always
  (defvar *match-helpers* nil)
  (defvar *match-macro-helpers* nil)
  (deftype match-data-type () 'simple-byte-vector)
  (deftype match-data-index-type () '(integer 0 #.most-positive-fixnum))

  (defmacro force-to-match-data-type (val)
    `(force-simple-byte-vector ,val))
  (defun match-data-type-elt (val i)
    (declare (optimize (safety 0) speed))
    (declare (type match-data-type val))
    (declare (type match-data-index-type i))
    (aref val i))
  (declaim (inline match-data-type-elt)))

(defun parse-match-data-to-integer (str &optional (base 10))
  (byte-vector-parse-integer str base))
(declaim (inline parse-match-data-to-integer))

(defun match-bind-meta-form (form env)
  (flet ((meta-sym (sym)
	   (assoc sym *match-macro-helpers*)))
    (or (meta-sym (force-first form))
	(meta-sym (force-first (macroexpand form env))))))

(defun generate-match-bind (form env)
  (typecase form
    (keyword 
     (values (list form) nil))
    (null
     (values nil nil))
    (symbol
     (values 
      `(setf ,form (:word))
      (list form)))
    (list
     (typecase (first form)
       (keyword 
	(let (vars)
	  (let ((f
		`(,(first form)
		   ,@(loop for f in (rest form)
			 collect (multiple-value-bind 
				       (m v)
				     (generate-match-bind f env)
				   (appendf vars v)
				   m)))))
	   (values f vars))))
      (symbol
       (cond ((eq (first form) 'quote)
	      (values (second form) nil))
	     (t 
	      (let (vars)
		(let ((f
		       (destructuring-bind (name &optional (matcher :word) default-value)
			   form
			 (push (list name default-value) vars)
			 (multiple-value-bind 
			       (m v)
			     (generate-match-bind matcher env)
			   (appendf vars v)
			   `(setf ,name ,m)))))
		  (values f vars))))))
      (t 
       (generate-match-bind `(:progn ,@(force-list form)) env))))
    (t (values `(:value (force-to-match-data-type ,form)) nil))))

(defmacro match-bind-internal (bindings string error &body body &environment env)
  "The bindings syntax is composed of binding-forms.

binding-form ::= symbol -> match a word and set symbol to its value
    | literal -> match value of the literal
    | :keyword -> call the match helper designated by :keyword
    | (symbol binding-form default-value) -> set the symbol to the default-value, if this form is ever executed then set it to the binding-form
    | (:keyword binding-forms*) -> call the match helper designated by keyword
    | 'form -> execute form without further consideration
"
  (multiple-value-bind (matcher vars)
      (generate-match-bind 
       (if (and (symbolp (first bindings)) 
		(not (keywordp (first bindings)))
		(not (eq 'quote (first bindings))))
	`(:progn ,@bindings)
	bindings)
       env)
       
    (once-only (string)
       `(let ,vars
	  (if (eq 'fail-match (match ,string ,matcher))
	      ,error
	      (locally ,@body))))))

(defmacro match-bind-or-return-fail-match (bindings string &body body)
  `(match-bind-internal ,bindings ,string 'fail-match ,@body))


(defmacro match-bind (bindings string &body body)
  (once-only (string)
    `(match-bind-internal ,bindings ,string (error 'match-failed :matching ',bindings :string ,string) ,@body)))

(define-condition match-failed 
    (error)
  ((regex-form :initarg :matching) 
   (string :initarg :string)))


(defmacro with-match-primitives ((string pos end) &body body)
  (with-unique-names (fail-match-action)
    `(block match-enclosure
       (let ((,fail-match-action (lambda()(return-from match-enclosure 'fail-match))))
	 (declare (type function ,fail-match-action))
	 (labels 
	     ((peek ()
		(subseq ,string (min ,pos ,end)))
	      (peek-starts-with (val &key (test 'eql))
		(and (can-peek (length val))
		     (not (loop for i below (length val)
				thereis (not (funcall test (match-data-type-elt val i) (match-data-type-elt ,string (+ ,pos i))))))))
	      (can-peek (&optional (amt 1))
		(declare (type match-data-index-type amt))
		(>= (- ,end ,pos) amt))
	      (peek-one ()
		(unless (can-peek)
		  (fail-match))
		(match-data-type-elt ,string ,pos))
	      (fail-match ()
		(funcall ,fail-match-action))
	      (eat (&optional (len 1))
		(declare (type match-data-index-type len))
		(incf ,pos len)
		(when (> ,pos ,end)
		  (fail-match))))
	   (declare (inline peek peek-starts-with can-peek peek-one fail-match eat))
	   (macrolet 
	       ((try-match (&rest matches)
		  (with-unique-names (save ret success old-fail-match-action block)
		    `(let ((,save ,',pos) ,ret ,success (,old-fail-match-action ,',fail-match-action))
		       (declare (type match-data-index-type ,save))
		       (block ,block
			 (setf ,',fail-match-action (lambda() (setf ,',pos ,save) (return-from ,block)))
			 (setf ,ret
			       (locally ,@matches))
			 (setf ,success t))
		       (setf ,',fail-match-action ,old-fail-match-action)
		       (values ,success ,ret)))))
	     ,@body))))))

(defmacro define-match-helper (name lambda-list &body body)
  (let ((helper (list* lambda-list body)))
    `(eval-always
       (setf (cdr-assoc *match-helpers* ',name) ',helper) 
       (check-for-match-syntax-errors))))

(defmacro define-match-macro-helper (name lambda-list &body body)
  (let ((helper (list* lambda-list body)))
    `(eval-always
       (setf (cdr-assoc *match-macro-helpers* ',name) ',helper) 
       (check-for-match-syntax-errors))))

(defmacro check-for-match-syntax-errors ()
  #+spend-ages-checking-for-regex-syntax-failures `(match nil nil))

(defmacro match (str &rest matching)
  (with-unique-names (pos end string)
    `(let ((,string (force-to-match-data-type ,str)))
       (without-call/cc ; this macro is too much of an ugly beast for the CPS transformer
	 (declare (optimize speed (safety 0)))
	 (let ((,pos 0) (,end (length ,string)))
	   (declare (type match-data-index-type ,pos ,end))
	   (with-match-primitives (,string ,pos ,end)
	     (macrolet
		 ,(loop for (name . helper) in *match-macro-helpers* 
			collect `(,name ,@helper))
	       (labels
		   ,(loop for (name . helper) in *match-helpers* 
			  collect `(,name ,(first helper) (declare (optimize speed (safety 0))) ,@(rest helper)))
		 (declare (ignorable ,@(loop for (name  . helper) in *match-helpers* collect `(function ,name))))
		 ,@matching))))))))


(define-match-macro-helper if-match (match &optional (then t) else)
  `(if (try-match ,match)
       ,then
       ,else))

(define-match-macro-helper aif-match (match then &optional else)
  (with-unique-names (success)
    `(multiple-value-bind (,success it)
	 (try-match ,match)
       (if ,success
	   ,then
	   ,else))))

(define-match-helper match-end ()
  (when (can-peek)
    (fail-match))
  (force-to-match-data-type nil))

(define-match-helper match-value (value &key (test 'eql))
  (let ((val (force-to-match-data-type value)))
    (when (not (peek-starts-with val :test test))
      (fail-match))
    (eat (length val))
    val))

(define-match-helper :value (value &key (test 'eql))
  (match-value value :test test))

(define-match-macro-helper with-return-matched (&body body)
  (with-unique-names (start end)
    `(let ((,start (peek)))
       ,@body
       (let ((,end (peek)))
	 (subseq ,start 0 (- (length ,start) (length ,end)))))))

(define-match-macro-helper match-multiple (min max &body body)
  (once-only (min max)
    (with-unique-names (i m)
      `(with-return-matched
	   (loop for ,i below ,min
		 do (locally ,@body))
	 (let ((,m (or ,max most-positive-fixnum)))
	   (loop for ,i from ,min below ,m
		 while (and (if-match (locally ,@body) t) (can-peek))))))))

(define-match-macro-helper match-or (&rest matches)
  (if matches
      `(aif-match ,(first matches) it (match-or ,@(rest matches)))
      `(fail-match)))

(define-match-macro-helper :or? (&rest matches)
  (if (rest matches)
    `(aif-match ,(first matches) it (:or? ,@(rest matches)))
    `(aif-match ,(first matches) it)))

(define-match-macro-helper match-any-value (&rest values)
  `(match-or ,@(loop for v in values
		     collect `(match-value ,v))))

(define-match-macro-helper match-weakly-max (max weak &rest strong)
  (with-unique-names (m)
    (once-only (max)
      `(let ((,m 0))
	 (tagbody
	  try-again
	    (if-match (locally ,@strong)
		      t
		      (progn
			(when (> (incf ,m) (or ,max most-positive-fixnum))
			  (fail-match))
			,weak
			(go try-again))))
	 (values)))))

(define-match-macro-helper match-until (&rest end)
  `(with-return-matched
       (loop until (try-match-lookahead (locally ,@end))
	     do (eat 1))))
		   
(define-match-helper whitespace ()
  (:char-range (or #\Space #\Tab #\Linefeed #\Return #\Page)))

(define-match-macro-helper try-match-lookahead (&rest matches)
  (with-unique-names (success)
    `(let (,success)
       (try-match ,@matches (setf ,success t) (fail-match))
       ,success)))

(define-match-helper match-word ()
  (let ((word (match-until (match-or (whitespace) (match-end)))))
    (when (zerop (length word))
      (fail-match))
    word))

(define-match-helper :integer (&optional (base 10))
  (let ((len 0) (str (peek)))
    (flet ((eat-one ()
	     (incf len)
	     (eat 1)))
      (when (= (char-code #\-) (peek-one))
	(eat-one))
      (iter 
	(for digit = (- (byte-to-ascii-upper (peek-one)) (char-code #\0)))
	(while (and (<= 0 digit) (> base digit)))
	(eat-one)
	(while (can-peek)))
      (when (zerop len)
	(fail-match))
      (parse-match-data-to-integer (subseq str 0 len) base))))
  
(define-match-macro-helper :+ (&rest matches)
  `(match-multiple 1 nil ,@matches))

(define-match-macro-helper :* (&rest matches)
  `(match-multiple 0 nil ,@matches))

(define-match-macro-helper :? (&rest matches)
  `(match-multiple 0 1 ,@matches))

(define-match-helper :fold-ascii-case (value)
  (match-value value :test 'eql-fold-ascii-case))

(define-match-macro-helper :or (&rest matches)
  `(match-or ,@matches))

(define-match-macro-helper :progn (&rest matches)
  `(progn ,@matches))

(define-match-helper :s ()
  (whitespace))

(define-match-helper :$ ()
  (match-end))

(define-match-helper :whitespace ()
  (:+ (:s)))

(define-match-helper :whitespace? ()
  (:* (:s)))

(define-match-helper :fail ()
  (fail-match))

(define-match-macro-helper :weakly (weak &rest strong)
  `(match-weakly-max nil ,weak ,@strong))

(define-match-macro-helper :until (&rest matches)
  `(match-until ,@matches))

(define-match-macro-helper :until-and-eat (&rest matches)
  (with-unique-names (start len)
    `(let ((,start (peek)) (,len 0))
       (declare (type fixnum ,len))
       (loop until (try-match (locally ,@matches))
	     do (eat 1)
	     (incf ,len))
       (subseq ,start 0 ,len))))

(define-match-helper :word ()
  (match-word))

(define-match-helper :rest ()
  (let ((r (peek)))
    (eat (length r))
    r))

(define-match-helper :char ()
  (prog1 (peek-one)
    (eat 1)))

(define-match-helper :string (len)
  (unless (can-peek len)
    (fail-match))
  (let ((str (subseq (peek) 0 len)))
    (eat len)
    str))

(defgeneric generate-in-char-range (range char))
(defgeneric generate-in-char-range-form (form args char))

(defmethod generate-in-char-range ((c character) char)
  (generate-in-char-range (char-code c) char))
(defmethod generate-in-char-range ((i integer) char)
  `(= ,i ,char))
(defmethod generate-in-char-range ((range list) char)
  (generate-in-char-range-form (first range) (rest range) char))

(defmethod generate-in-char-range-form ((form list) (args null) char)
  (generate-in-char-range form char))

(defmethod generate-in-char-range-form ((form (eql 'not)) args char)
  (destructuring-bind (negate)
      args
    `(not ,(generate-in-char-range negate char))))
(defmethod generate-in-char-range-form ((form (eql '-)) args char)
  (flet ((to-int (c)
	   (if (characterp c)
	       (char-code c)
	       c)))
    (destructuring-bind (from to)
	args
      (let ((l (min (to-int from) (to-int to)))
	    (m (max (to-int from) (to-int to))))
	`(and (>= ,char ,l) (>= ,m ,char))))))
(defmethod generate-in-char-range-form ((form (eql 'or)) args char)
  `(or ,@(loop for form in args 
	       collect (generate-in-char-range form char))))

(defmacro in-char-range (range char)
  (once-only (char)
    (generate-in-char-range range char)))

(define-match-macro-helper :char-range (&rest range)
  `(progn 
     (unless (in-char-range ,range (peek-one))
       (fail-match))
     (:char)))

(defmacro if-match ((bindings string) &optional (then t) else)
  `(if (eq (match-bind-or-return-fail-match ,bindings ,string t) 'fail-match)
       ,else
       ,then))

(defmacro case-match-fold-ascii-case (keyform &rest clauses)
  (generate-case-key keyform :test 'byte-vector=-fold-ascii-case :transform 'force-to-match-data-type :clauses clauses))

(defmacro match-replace-helper (match-replacements string)
  (with-unique-names (before r after)
      `(let ((,r (force-to-match-data-type "")))
	 (match-bind-internal 
	     ((,before (:until-and-eat 
			(:or
			 ,@(loop for (match replacement) on match-replacements by #'cddr 
				 collect `(:progn ,match '(setf ,r (force-to-match-data-type ,replacement))))
			 :$)))
	      (,after (:rest)))
	     ,string
	     (values (force-to-match-data-type "") (force-to-match-data-type "") (force-to-match-data-type ""))
	   (values ,before ,r ,after)))))

(defmacro match-replace-one (string &rest match-replacements)
  (with-unique-names (before r after)
    `(multiple-value-bind (,before ,r ,after)
	 (match-replace-helper ,match-replacements ,string)
       (concatenate-simple-byte-vectors (list ,before ,r ,after)))))

(defmacro match-replace-all (string &rest match-replacements)
  (with-unique-names (f s before r after)
    `(flet ((,f (,s)
	      (match-replace-helper ,match-replacements ,s)))
       (let ((,s (force-to-match-data-type ,string)))
	 (concatenate-simple-byte-vectors
	  (iter
	    (multiple-value-bind (,before ,r ,after)
		(,f ,s)
	      (declare (type match-data-type ,before ,r ,after))
	      (unless (zerop (length ,before)) (collect ,before))
	      (unless (zerop (length ,r)) (collect ,r))
	      (until (zerop (length ,after)))
	      (setf ,s ,after))))))))
