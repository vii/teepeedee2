(in-package #:tpd2.lib)

(eval-always
  (defvar *match-helpers* nil)
  (defvar *match-macro-helpers* nil))

(defun match-bind-meta-form (form env)
  (or (assoc (force-first form) *match-macro-helpers*)
      (assoc (force-first (macroexpand form env)) *match-macro-helpers*)))

(defun generate-match-bind (bindings env)
  (let (vars matchers)
    (loop for it in bindings
	  do (typecase it
	       (list (cond
		       ((eq (first it) 'quote)
			(push (second it) matchers))
		       ((match-bind-meta-form (first it) env)
			(multiple-value-bind (v m)
			    (generate-match-bind (rest it) env)
			  (appendf vars v)
			  (push `(,@(force-list (first it)) ,@m) matchers)))
		       (t
			(destructuring-bind 
			      (var &optional (matcher '(match-word)) (default-value nil))
			    it
			  (push (list var default-value) vars)
			  (push `(setf ,var ,matcher) matchers)))))
	       (null)
	       (symbol
		(cond ((keywordp it)
		       (push `(,it) matchers))
		      (t
		       (pushnew it vars)
		       (push `(setf ,it (match-word)) matchers))))
	       (t
		(push `(match-value ,it) matchers))))
    (values vars (nreverse matchers))))

(defmacro match-bind (bindings string &body body &environment env)
  (multiple-value-bind (vars matchers)
      (generate-match-bind bindings env)
    `(let ,vars
       (match ,matchers ,string)
       ,@body)))

(define-condition match-failed 
    (error)
  ())


(defmacro match (matching str)
  (with-unique-names (pos end string)
    `(let ((,string (force-to-match-data-type ,str)))
       (let ((,pos 0) (,end (length ,string)))
	 (macrolet 
	     ((try-match (&rest matches)
		(with-unique-names (save ret success)
		  `(let ((,save ,',pos) (,success t))
		     (let ((,ret
			    (handler-case (locally ,@matches)
			      (match-failed () 
				(setf ,',pos ,save)
				(setf ,success nil)
				nil))))
		       (values ,success ,ret)))))
	      ,@(loop for (name . helper) in *match-macro-helpers* 
		      collect `(,name ,@helper)))
	   (labels 
	       ((peek ()
		  (subseq ,string (min ,pos ,end)))
		(peek-starts-with (val &key (test 'eql))
		  (and (can-peek (length val))
		       (not (loop for i below (length val)
				  thereis (not (funcall test (match-data-type-elt val i) (match-data-type-elt ,string (+ ,pos i))))))))
		(can-peek (&optional (amt 1))
		  (>= (- ,end ,pos) amt))
		(peek-one ()
		  (unless (can-peek)
		    (fail-match))
		  (match-data-type-elt ,string ,pos))
		(fail-match ()
		  (error 'match-failed))		    
		(eat (&optional (len 1))
		  (incf ,pos len)
		  (when (> ,pos ,end)
		    (fail-match)))
		,@(loop for (name . helper) in *match-helpers* 
			collect `(,name ,@helper)))
	     ,@matching))))))

(defmacro check-for-match-syntax-errors ()
  `(match nil nil))


(defmacro define-match-helper (name lambda-list &body body)
  (let ((helper (list* lambda-list body)))
    (setf (cdr-assoc *match-helpers* name) helper) 
    (check-for-match-syntax-errors)))

(defmacro define-match-macro-helper (name lambda-list &body body)
  (let ((helper (list* lambda-list body)))
    (setf (cdr-assoc *match-macro-helpers* name) helper) 
    (check-for-match-syntax-errors)))

(define-match-macro-helper if-match (match then &optional else)
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
       
(defun force-to-match-data-type (val)
  (force-string val))
(defun match-data-type-elt (val i)
  (char-code (elt val i)))

(defun eql-fold-ascii-case (a b)
  (flet ((to-lower (x)
	   (if (and (>= x (char-code #\A)) (<= x (char-code #\Z)))
	       (+ (- (char-code #\a) (char-code #\A)) x)
	       x)))
    (= (to-lower a) (to-lower b))))

(define-match-helper match-value (value &key (test 'eql))
  (let ((val (force-to-match-data-type value)))
    (when (not (peek-starts-with val :test test))
      (fail-match))
    (eat (length val))
    val))


(define-match-macro-helper match-multiple (min max &body body)
  (once-only (min max)
    (with-unique-names (i m)
      `(progn 
	 (loop for ,i below ,min
	       do (locally ,@body))
	 (let ((,m (or ,max most-positive-fixnum)))
	   (loop for ,i from ,min below ,m
		 while (if-match (locally ,@body) t)))))))

(define-match-macro-helper match-or (&rest matches)
  (if matches
      `(aif-match ,(first matches) it (match-or ,@(rest matches)))
      `(fail-match)))

(define-match-macro-helper match-any-value (&rest values)
  `(match-or ,@(loop for v in values
		     collect `(match-value ,v))))

(define-match-helper match-one-char ()
  (eat 1)
  (values))

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
  (with-unique-names (start len)
    `(let ((,start (peek)) (,len 0))
       (loop until
	     (try-match-lookahead (match-or (match-end) (locally ,@end)))
	     do (eat 1)
	     (incf ,len))
       (when (zerop ,len)
	 (fail-match))
       (subseq ,start 0 ,len))))
		   
(define-match-helper whitespace ()
  (match-any-value #\Space #\Tab #\Linefeed #\Return #\Page))

(define-match-macro-helper try-match-lookahead (&rest matches)
  (with-unique-names (success)
    `(let (,success)
       (try-match ,@matches (setf ,success t) (fail-match))
       ,success)))

(define-match-helper match-word ()
  (match-until (whitespace)))

(define-match-helper integer (&optional (base 10))
  (let ((val 0) (sign 1))
    (when (= (char-code #\-) (peek-one))
      (setf sign -1)
      (eat 1))
    (iter (for digit = (- (peek-one) (char-code #\0)))
	  (for first initially t then nil)
	  (when (or (> 0 digit) (<= base digit))
	    (when first (fail-match))
	    (finish))
	  (setf val (+ digit (* val base)))
	  (eat 1)
	  (unless (can-peek)
	    (finish)))

    (* sign val)))

(define-match-helper match-end ()
  (when (can-peek)
    (fail-match))
  (values))

(define-match-macro-helper :+ (&rest matches)
  `(match-multiple 1 nil ,@matches))

(define-match-macro-helper :* (&rest matches)
  `(match-multiple 0 nil ,@matches))

(define-match-macro-helper :? (&rest matches)
  `(match-multiple 0 1 ,@matches))

(define-match-helper :fold-ascii-case (value)
  (match-value value :test 'eql-fold-ascii-case))

(define-match-helper :s ()
  (whitespace))

(define-match-helper :$ ()
  (match-end))

(define-match-helper :whitespace ()
  (:+ (:s)))

(define-match-helper :whitespace? ()
  (:* (:s)))


