;; -*- lisp -*-

(in-package :arnesi)

;;;; FUNCTION

(defmethod evaluate/cc ((func free-function-object-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env))
  (multiple-value-bind (definition cc-boundp)
      (fdefinition/cc (name func))
    (if cc-boundp
        (kontinue k definition)
        (if (fboundp (name func))
            (kontinue k (fdefinition (name func)))
            (error "Unbound function ~S." (name func))))))

(defmethod evaluate/cc ((func local-function-object-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (lookup lex-env :flet (name func) :error-p t)))

(defclass closure/cc ()
  ((code :accessor code :initarg :code)
   (env :accessor env :initarg :env))
  #+sbcl (:metaclass mopp:funcallable-standard-class))

#+sbcl
(defmethod initialize-instance :after ((fun closure/cc) &rest initargs)
  (declare (ignore initargs))
  (mopp:set-funcallable-instance-function 
   fun 
   #'(lambda (&rest args)
       (drive-interpreter/cc 
	(apply-lambda/cc fun
			 args
                         '()
			 *toplevel-k*)))))

;;;; LAMBDA

(defmethod evaluate/cc ((lambda lambda-function-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (make-instance 'closure/cc :code lambda :env lex-env)))

;;;; APPLY and FUNCALL

(defk k-for-call/cc (k)
    (value)
  (if *call/cc-returns*
      (kontinue k value)
      (throw 'done value)))

;;;; apply'ing a free (global) function

(defmethod evaluate/cc ((func free-application-form) lex-env dyn-env k)
  (cond 
    ((eql 'call/cc (operator func))
     (evaluate/cc (make-instance 'free-application-form
                                  :operator 'funcall
                                  :arguments (list (first (arguments func))
                                                   (make-instance 'constant-form :value k :source k))
                                  :source (source func))
                  lex-env dyn-env `(k-for-call/cc ,k)))
    
    ((eql 'kall (operator func))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (trace-statement "KALL'ing ~S on ~S" (first arguments) (rest arguments))
        (apply #'kontinue (first arguments) (cdr arguments)))
      (arguments func) '()
      lex-env dyn-env))

    ((and (eql 'call-next-method (operator func))
	  (second (multiple-value-list (lookup lex-env :next-method t))))
     (aif (lookup lex-env :next-method t)
	  (evaluate-arguments-then-apply
	   (lambda (arguments)
	     (apply-lambda/cc it arguments dyn-env k))
	   (arguments func) '() lex-env dyn-env)
	  (error "no next method")))

    ((and (eql 'next-method-p (operator func))
	  (second (multiple-value-list (lookup lex-env :next-method t))))
     (kontinue k (lookup lex-env :next-method t)))
    
    ((eql 'funcall (operator func))
     (evaluate-funcall/cc (arguments func) lex-env dyn-env k))
    
    ((eql 'apply (operator func))
     (evaluate-apply/cc (arguments func) '() lex-env dyn-env k))
    
    ((and (symbolp (operator func))
          (eql 'defun/cc (nth-value 1 (fdefinition/cc (operator func)))))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (trace-statement "Calling cc function ~S with arguments ~S" (operator func) arguments)
        (apply-lambda/cc (fdefinition/cc (operator func)) arguments dyn-env k))
      (arguments func) '()
      lex-env dyn-env))
    
    ((and (symbolp (operator func))
          (eql 'defmethod/cc (nth-value 1 (fdefinition/cc (operator func)))))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (trace-statement "Calling cc method ~S with arguments ~S" (operator func) arguments)
        (apply-lambda/cc (apply (operator func) arguments) arguments dyn-env k))
      (arguments func) '()
      lex-env dyn-env))
       
    (t
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (multiple-value-bind (vars vals)
            (export-specials dyn-env)
          (progv vars vals
            (trace-statement "Calling function ~S with arguments ~S"
                             (operator func) arguments)
            (apply #'kontinue k (multiple-value-list
                                    (apply (fdefinition (operator func)) arguments))))))
      (arguments func) '()
      lex-env dyn-env))))

;; returns a list of variables and values from the dynamic environment that should be exported
;; these variables will be visible in normal lisp code that is called from cc code
(defun export-specials (dyn-env)
  ;; TODO: here we could check each special whether it has to be exported or not
  ;;       this could be based on something like (declare (export var)) in the cc code
  (let ((dyn-env (remove-duplicates dyn-env
                                    :test (lambda (x y) (eq (second x) (second y)))
                                    :from-end t)))
    (values (mapcar 'second dyn-env)
            (mapcar 'cddr dyn-env))))

;;;; apply'ing a local function

(defmethod evaluate/cc ((func local-application-form) lex-env dyn-env k)
  (evaluate-arguments-then-apply
   (lambda (arguments)
     (apply-lambda/cc (lookup lex-env :flet (operator func) :error-p t) arguments dyn-env k))
   (arguments func) '()
   lex-env dyn-env))

;;;; apply'ing a lambda

(defmethod evaluate/cc ((lambda lambda-application-form) lex-env dyn-env k)
  (evaluate-funcall/cc (cons (operator lambda) (arguments lambda)) lex-env dyn-env k))

;;;; Utility methods which do the actual argument evaluation, parsing
;;;; and control transfer.

(defun evaluate-funcall/cc (arguments lex-env dyn-env k)
  (evaluate-apply/cc (append (butlast arguments)
                             (list (make-instance 'free-application-form
                                                  :operator 'list
                                                  :source `(list ,(source (car (last arguments))))
                                                  :arguments (last arguments))))
                     '()
                     lex-env dyn-env k))

(defk k-for-apply/cc (remaining-arguments evaluated-arguments lex-env dyn-env k)
    (value)
  (evaluate-apply/cc (cdr remaining-arguments) (cons value evaluated-arguments)
                     lex-env dyn-env k))

(defun evaluate-apply/cc (remaining-arguments evaluated-arguments lex-env dyn-env k)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) lex-env dyn-env
                   `(k-for-apply/cc ,remaining-arguments ,evaluated-arguments ,lex-env ,dyn-env ,k))
      (let ((arg-list (apply #'list* (reverse evaluated-arguments))))
        (apply-lambda/cc (first arg-list) (rest arg-list) dyn-env k))))

;;;; Finally this is the function which, given a closure/cc object and
;;;; a list of (evaluated) arguments parses them, setup the
;;;; environment and transfers control.

(defmethod apply-lambda/cc ((operator closure/cc) effective-arguments dyn-env k)
  (trace-statement "Applying cc closure ~S to ~S" (source (code operator)) effective-arguments)
  (let ((lex-env (env operator))
        (remaining-arguments effective-arguments)
        (remaining-parameters (arguments (code operator))))
    ;; in this code ARGUMENT refers to the values passed to the
    ;; function. PARAMETER refers to the lambda of the closure
    ;; object. we walk down the parameters and put the arguments in
    ;; the environment under the proper names.
    
    ;; first the required arguments
    (loop
       while remaining-parameters
       for parameter = (first remaining-parameters)
       do (typecase parameter
            (required-function-argument-form
             (if remaining-arguments
                 (setf lex-env (register lex-env :let (name parameter) (pop remaining-arguments)))
                 (error "Missing required arguments, expected ~S, got ~S."
                        (arguments (code operator)) effective-arguments))
             (pop remaining-parameters))
            (t (return))))

    ;; handle special variables
    (setf dyn-env (import-specials (code operator) dyn-env))

    ;; now we start the chain optional->keyword->evaluate-body. We do
    ;; this because optional and keyword parameters may have default
    ;; values which may use call/cc.
    (apply-lambda/cc/optional operator
                              remaining-parameters remaining-arguments
                              lex-env dyn-env k)))

(defun apply-lambda/cc/optional (operator remaining-parameters remaining-arguments lex-env dyn-env k)
  (flet ((done (remaining-parameters)
           (return-from apply-lambda/cc/optional
             (apply-lambda/cc/keyword
              operator remaining-parameters remaining-arguments lex-env dyn-env k))))
    (loop
       for head on remaining-parameters
       for parameter = (first head) 
       do 
       (etypecase parameter
         (rest-function-argument-form
          (setf lex-env (register lex-env :let (name parameter) remaining-arguments)))
         (optional-function-argument-form
          (if remaining-arguments
              (progn
                (setf lex-env (register lex-env :let (name parameter) (pop remaining-arguments)))
                (when (supplied-p-parameter parameter)
                  (setf lex-env (register lex-env :let (supplied-p-parameter parameter) t))))
              (return-from apply-lambda/cc/optional
                ;; we need to evaluate a default-value, since this may
                ;; contain call/cc we need to setup the continuation
                ;; and let things go from there (hence the return-from)
                (evaluate/cc (default-value parameter) lex-env dyn-env
                             `(k-for-apply/cc/optional-argument-default-value
                               ;; remaining-arguments is, by
                               ;; definition, NIL so we needn't pass
                               ;; it here.
                               ,operator ,head ,lex-env ,dyn-env ,k)))))
         ((or keyword-function-argument-form allow-other-keys-function-argument-form)
          ;; done with the optional args
          (done head)))
       finally (done head))))

(defk k-for-apply/cc/optional-argument-default-value
    (operator remaining-parameters lex-env dyn-env k)
    (value)
  (apply-lambda/cc/optional
   operator (cdr remaining-parameters)
   ;; nb: if we're evaluating the default value of an optional
   ;; arguments then we can't have anything left in the arguments
   ;; list.
   nil
   (register lex-env :let (name (first remaining-parameters)) value)
   dyn-env
   k))

(defun apply-lambda/cc/keyword (operator remaining-parameters remaining-arguments lex-env dyn-env k)
  ;; now any keyword parameters
  (loop
     for head on remaining-parameters
     for parameter = (first head) 
     do (typecase parameter
          (keyword-function-argument-form
           (assert (evenp (length remaining-arguments))
                   (remaining-arguments)
                   "Odd number of arguments in ~S being applied to ~S."
                   remaining-arguments
                   (source (code operator)))
           (let ((value (getf remaining-arguments
                              (effective-keyword-name parameter)
                              parameter)))
             (if (eql parameter value)
                 ;; no such keyword. need to evaluate the default value
                 (return-from apply-lambda/cc/keyword
                   (evaluate/cc (default-value parameter) lex-env dyn-env
                                `(k-for-apply-lambda/cc/keyword-default-value
                                  ,operator ,head ,remaining-arguments
                                  ,lex-env ,dyn-env ,k)))
                 ;; keyword passed in explicitly.
                 (progn
                   (let ((value (getf remaining-arguments (effective-keyword-name parameter))))
                     (remf remaining-arguments (effective-keyword-name parameter))
                     (setf lex-env (register lex-env :let (name parameter) value))
                   (when (supplied-p-parameter parameter)
                     (setf lex-env (register lex-env :let (supplied-p-parameter parameter) t))))))))
          (allow-other-keys-function-argument-form
           (when (cdr remaining-parameters)
             (error "Bad lambda list: ~S" (arguments (code operator))))
           (return))
          (t (unless (null remaining-parameters)
               (error "Bad lambda list: ~S" (arguments (code operator)))))))
  (evaluate-progn/cc (body (code operator)) lex-env dyn-env k))

(defk k-for-apply-lambda/cc/keyword-default-value
    (operator remaining-parameters remaining-arguments lex-env dyn-env k)
    (value)
  (apply-lambda/cc/keyword operator
                           (cdr remaining-parameters) remaining-arguments
                           (register lex-env :let (name (first remaining-parameters)) value)
                           dyn-env
                           k))

(defmethod apply-lambda/cc ((operator function) effective-arguments dyn-env k)
  "Method used when we're applying a regular, non cc, function object."
  (declare (ignore dyn-env))
  (trace-statement "Applying function ~S to ~S" operator effective-arguments)
  (apply #'kontinue k (multiple-value-list (apply operator effective-arguments))))

(defmethod apply-lambda/cc ((operator symbol) effective-arguments dyn-env k)
  "Method used when we're applying a regular, non cc, function object."
  (apply-lambda/cc (symbol-function operator) effective-arguments dyn-env k))

;;;; Small helper function

(defk k-for-evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments lex-env dyn-env)
    (value)
  (evaluate-arguments-then-apply
   handler
   remaining-arguments (cons value evaluated-arguments)
   lex-env dyn-env))

(defun evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments lex-env dyn-env)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) lex-env dyn-env
                    `(k-for-evaluate-arguments-then-apply ,handler ,(cdr remaining-arguments)
                                                          ,evaluated-arguments ,lex-env ,dyn-env))
      (funcall handler (reverse evaluated-arguments))))

;; Copyright (c) 2002-2006, Edward Marco Baringer
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
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
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
