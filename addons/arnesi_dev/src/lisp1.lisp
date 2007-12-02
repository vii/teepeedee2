;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Entry point

(defgeneric lisp1 (form)
  (:documentation "Translate FORM from Lisp-1 to Lisp-2.

Define methods on this generic function with DEFLISP1-WALKER."))

(defmethod lisp1 (form)
  "If FORM isn't a FORM object, we'll convert it to one, apply
the transformation and convert it back."
  (unwalk-form (lisp1 (walk-form form))))

(defmacro with-lisp1 (form)
  "Execute FORM as if it were run in a Lisp-1."
  (lisp1 form))

(defmacro deflisp1-walker (class (&rest slots) &body body)
  "Define a Lisp-1 to Lisp-2 walker.

It takes the class of a CL form object, and its slots as
arguments.  It also captures the variable FORM for convenience."
  `(defmethod lisp1 ((form ,class))
     (with-slots ,slots form
       ,@body)))

;;;; * Special Variables

(defvar *bound-vars* nil
  "When walking code, this variable contains a list of
variables (represented by symbols) which have been bound in
the variable namespace.

In essence these variables do not have to be sharp-quoted.")

(defvar *bound-funs* nil
  "When walking code, this variable contains a list of
variables (represented by symbols) which have been bound in
the function namespace.

In essence these variables must be sharp-quoted.")

(defmacro with-bound-vars (vars &body body)
  "Execute BODY with VARS added to the variable namespace and
VARS removed from the function namespace.

This should only be used when code-walking."
  `(let ((*bound-vars* (append         *bound-vars* ,vars))
	 (*bound-funs* (set-difference *bound-funs* ,vars)))
     ,@body))

(defmacro with-bound-funs (funs &body body)
  "Execute BODY with FUNS added to the function namespace and
FUNS removed from the variable namespace.

This should only be used when code-walking."  
  `(let ((*bound-funs* (append         *bound-funs* ,funs))
	 (*bound-vars* (set-difference *bound-vars* ,funs)))
     ,@body))

;;;; * Definers

(defmacro defun1 (name (&rest args) &body body)
  "Define a function with BODY written in Lisp-1 style.

This is just like DEFUN."
  (with-bound-vars (extract-argument-names args :allow-specializers nil)
    `(defun ,name ,args
       ,(lisp1 `(block ,name ,@body)))))

(defmacro defmethod1 (name (&rest args) &body body)
  "Define a method with BODY written in Lisp-1 style.

This is just like DEFMETHOD."
  (with-bound-vars (extract-argument-names args :allow-specializers t)
    `(defmethod ,name ,args
       ,(lisp1 `(block ,name ,@body)))))

;;;; * Utils

(defun lisp1s (forms)
  "Convert a list of forms to Lisp-1 style."
  (mapcar #'lisp1 forms))

(defun lisp1b (binds)
  "Convert an alist of (VAR . FORM) to Lisp-1 style."
  (mapcar (lambda (bind)
	    (cons (car bind)
		  (lisp1 (cdr bind))))
	  binds))

;;;; * Walkers

(deflisp1-walker form ()
  ;; By default all forms will stay the same.
  form)

(deflisp1-walker if-form (consequent then else)
  ;; Transform the test and branches recursively.
  (new 'if-form
       :consequent (lisp1 consequent)
       :then       (lisp1 then)
       :else       (lisp1 else)))

(deflisp1-walker lambda-function-form (arguments body)
  ;; For any function-form (ie lambda), we just transform the body.
  ;; We also must add the parameters to the variable namespace, and
  ;; remove the parameters from the function namespace.
  (with-bound-vars (mapcar #'name arguments)
    (new 'lambda-function-form
	 :arguments arguments
	 :body      (lisp1s body))))

(deflisp1-walker variable-reference (name)
  ;; If a free variable is bound in the toplevel, *and* not bound by
  ;; an enclosing lambda, then we'll return that function.  Also, if
  ;; the variable has been bound by an enclosing function binding form
  ;; then we'll return that function.  We take advantage of the fact
  ;; that the `name' slot is shared.
  (if (or (and (fboundp name) (not (member name *bound-vars*)))
	  (member name *bound-funs*))
      (change-class form 'free-function-object-form)
      form))

(deflisp1-walker application-form (operator arguments)
  ;; We transform all applications so they use explicit funcall.  We
  ;; also must take into account ((a b) c ...) which must also
  ;; transform the operator accordingly.
  (new 'free-application-form
       :operator  'funcall
       :arguments (cons (if (not (typep operator 'form))
			    (lisp1 (walk-form operator))
			    (lisp1 operator))
			(lisp1s arguments))))

(deflisp1-walker function-binding-form (binds body)
  ;; Add all the bindings to the function namespace to be sharp
  ;; quoted.
  (with-bound-funs (mapcar #'car binds)
    (new (class-name-of form)
	 :binds (lisp1b binds)
	 :body  (lisp1s body))))

(deflisp1-walker variable-binding-form (binds body)
  ;; Add all the bindings to the variable namespace so they aren't
  ;; sharp-quoted.
  (with-bound-vars (mapcar #'car binds)
    (new (class-name-of form)
	 :binds (lisp1b binds)
	 :body  (lisp1s body))))

;; Walking all the other Common Lisp forms is rather straight-forward.

(deflisp1-walker setq-form (var value)
  (new 'setq-form
       :var   var
       :value (lisp1 value)))

(deflisp1-walker progn-form (body)
  (new 'progn-form
       :body (lisp1s body)))

(deflisp1-walker progv-form (vars-form values-form)
  (new 'progv-form
       :vars-form   vars-form
       :values-form (lisp1s values-form)))

(deflisp1-walker block-form (name body)
  (new 'block-form
       :name name
       :body (lisp1s body)))

(deflisp1-walker return-from-form (target-block result)
  (new 'return-from-form
       :target-block target-block
       :result       (lisp1 result)))

(deflisp1-walker catch-form (tag body)
  (new 'catch-form
       :tag  tag
       :body (lisp1s body)))

(deflisp1-walker throw-form (tag value)
  (new 'throw-form
       :tag   tag
       :value (lisp1 value)))

(deflisp1-walker eval-when-form (body eval-when-times)
  (new 'eval-when-form
       :eval-when-times eval-when-times
       :body            (lisp1s body)))

(deflisp1-walker multiple-value-call-form (func arguments)
  (new 'multiple-value-call-form
       :func      (lisp1  func)
       :arguments (lisp1s arguments)))

(deflisp1-walker multiple-value-prog1-form (first-form other-forms)
  (new 'multiple-value-prog1-form
       :first-form  (lisp1  first-form)
       :other-forms (lisp1s other-forms)))

(deflisp1-walker symbol-macrolet-form (binds body)
  (new 'symbol-macrolet-form
       :binds (lisp1b binds)
       :body  (lisp1s body)))

(deflisp1-walker tagbody-form (body)
  (new 'tagbody-form
       :body (lisp1s body)))

(deflisp1-walker the-form (type-form value)
  (new 'the-form
       :type-form type-form
       :value     (lisp1 value)))

(deflisp1-walker unwind-protect-form (protected-form cleanup-form)
  (new 'unwind-protect-form
       :protected-form (lisp1  protected-form)
       :cleanup-form   (lisp1s cleanup-form)))

;;;; http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/82994055009163e9

;; Copyright (c) 2006, Hoan Ton-That
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Hoan Ton-That, nor the names of the
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
