;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Portable lexical environment access

(defgeneric environment-p (environment)
  (:documentation "Returns T if ENVIRONMENT is a lexical
  environment object (something suitable for passing to
  macroexpand-1 or similar)"))

(defgeneric lexical-variables (environment)
  (:documentation "Return the names of all the local variables
  in ENVIRONMENT. Does not return neither symbol-macrolets nor
  ignared variables."))

(defgeneric lexical-functions (environment)
  (:documentation "Returns the names of all the local functions
  in ENVIRONMENT. Names may be symbols of lists of the form (setf
  name)."))

(defgeneric lexical-macros (environment)
  (:documentation "Returns the lexical macro definitions in
  ENVIRONMENT. The return value is a list of elements of form
  (SYMBOL . MACRO-FUNCTION. MACRO-FUNCTION can be called like
  functions returned by macro-function."))

(defgeneric lexical-symbol-macros (environment)
  (:documentation "Returns the lexical symbol macro definitions 
  in ENVIRONMENT. The return value is a list of elements of form
  (SYMBOL . EXPANSION)."))

(defmethod lexical-variables ((environment t))
  '())

(defmethod lexical-functions ((environment t))
  '())

(defmethod lexical-macros ((environment t))
  '())

(defmethod lexical-symbol-macros ((environment t))
  '())

;;;; ** OpenMCL

#+openmcl
(defmethod environment-p ((e ccl::lexical-environment))
  t)

#+openmcl
(defmethod lexical-variables ((environment ccl::lexical-environment))
  (loop
     for env = environment
          then (ccl::lexenv.parent-env env)
     while (and env
                (not (ccl::istruct-typep env 'ccl::definition-environment)))
     for vars = (ccl::lexenv.variables env)
     when (listp vars)
     ;; we now weed out all symbol-macros and ignored variables
     append (remove-if (lambda (var-name)
                         (let ((decs (assoc var-name (ccl::lexenv.vdecls env))))
                           (and decs
                                (eql 'cl:ignore (second decs))
                                (eql 'cl:t (cddr decs)))))
                       (mapcar (lambda (var)
                                 ;; ccl::var-name is a macro, se we can't do #'ccl::var-name directly
                                 (ccl::var-name var))
                               (remove-if (lambda (var-spec)
                                            (and (ccl::var-ea var-spec)
                                                 (consp (ccl::var-ea var-spec))
                                                 (eql :symbol-macro (car (ccl::var-ea var-spec)))))
                                          vars)))))

#+openmcl
(defmethod lexical-functions ((environment ccl::lexical-environment))
  (loop
     for env = environment
          then (ccl::lexenv.parent-env env)
     while (and env
                (not (ccl::istruct-typep env 'ccl::definition-environment)))
     for funs = (ccl::lexenv.functions env)
     when (listp funs)
     ;; we now weed out all symbol-macros and ignored variables
     append (mapcar (lambda (func-spec)
                      ;; convert the function name to a "real" function name
                      (let ((name (first func-spec)))
                        (if (eql (symbol-package (first func-spec))
                                 (find-package :SETF))
                            (list 'cl:setf (read-from-string (symbol-name name)))
                            name)))
                    (remove-if (lambda (func-spec)
                                 ;; weed out all the macrolets
                                 (eql 'ccl::macro (second func-spec)))
                               funs))))

;;;; ** SBCL
 
#+sbcl
(defmethod environment-p ((environment sb-kernel:lexenv))
  t)

#+sbcl
(defmethod lexical-variables ((environment sb-kernel:lexenv))
  (loop
     for var-spec in (sb-c::lexenv-vars environment)
     when (and (atom (cdr var-spec))
               (not (and (typep (cdr var-spec) 'sb-c::lambda-var)
			 (sb-c::lambda-var-ignorep (cdr var-spec)))))
     collect (car var-spec)))

#+sbcl
(defmethod lexical-functions ((environment sb-kernel:lexenv))
  (loop
   for fun-spec in (sb-c::lexenv-funs environment)
   when (not (consp (cdr fun-spec)))
   collect (car fun-spec)))

#+sbcl
(defmethod lexical-macros ((environment sb-kernel:lexenv))
  (loop
   for mac-spec in (sb-c::lexenv-funs environment)
   when (and (consp (cdr mac-spec))
	     (eq 'sb-sys::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

#+sbcl
(defmethod lexical-symbol-macros ((environment sb-kernel:lexenv))
  (loop
   for mac-spec in (sb-c::lexenv-vars environment)
   when (and (consp (cdr mac-spec))
	     (eq 'sb-sys::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

;;;; ** CMUCL

#+cmu
(defmethod environment-p ((environment c::lexenv))
  t)

#+cmu
(defmethod lexical-variables ((environment c::lexenv))
  (loop
     for var-spec in (c::lexenv-variables environment)
     ;; variable refs are (NAME . LAMBDA-VAR), we want to void
     ;; symbol-macrolets which are (NAME SYSTEM:MACRO . EXPANSION)
     when (and (atom (cdr var-spec))
               ;; don't return ignored vars
               (not (eq (type-of (cdr var-spec)) 'c::global-var))
               (not (c::lambda-var-ignorep (cdr var-spec))))
     collect (car var-spec)))

#+cmu
(defmethod lexical-functions ((environment c::lexenv))
  (loop
     for func-spec in (c::lexenv-functions environment)
     ;; flet and labels function look like ((FLET ACTUAL-NAME) . STUFF)
     if (and (consp (first func-spec))
             (member (car (first func-spec)) '(flet labels)))
       collect (second (first func-spec))
     ;; macrolets look like (NAME SYSTEM:MACRO . STUFF)
     else if (and (consp (cdr func-spec))
                  (eql 'system:macro (second func-spec)))
     ;; except that we don't return macros for now
     do (progn)
     ;; handle the case  (NAME . #<C::FUNCTIONAL>)
     else if (typep (cdr func-spec) 'C::FUNCTIONAL)
       collect (car func-spec)
     ;; if we get here we're confused :(
     else
       do (error "Sorry, don't know how to handle the lexcial function spec ~S."
                 func-spec)))

#+cmu
(defmethod lexical-macros ((environment c::lexenv))
  (loop
   for mac-spec in (c::lexenv-functions environment)
   when (and (consp (cdr mac-spec))
	     (eq 'system::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

#+cmu
(defmethod lexical-symbol-macros ((environment c::lexenv))
  (loop
   for mac-spec in (c::lexenv-variables environment)
   when (and (consp (cdr mac-spec))
	     (eq 'system::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

;;;; ** CLISP

#+clisp
(defmethod environment-p ((environment vector))
  (= 2 (length environment)))

#+clisp
(defun walk-vector-tree (function vector-tree)
  (labels ((%walk (vector-tree)
             (loop
                for index upfrom 0 by 2
                for tree-top = (aref vector-tree index)
                if (null tree-top)
                  do (return-from %walk nil)
                else if (vectorp tree-top)
                  do (return-from %walk
                       (%walk tree-top))
                else
                  do (funcall function
                              (aref vector-tree index)
                              (aref vector-tree (1+ index))))))
    (%walk vector-tree)))

#+clisp
(defmethod lexical-variables ((environment vector))
  (let ((vars '()))
    (when (aref environment 0)
      (walk-vector-tree (lambda (var-name var-spec)
                          (unless (system::symbol-macro-p var-spec)
                            (push var-name vars)))
                        (aref environment 0)))
    vars))

#+clisp
(defmethod lexical-functions ((environment vector))
  (let ((vars '()))
    (when (aref environment 1)
      (walk-vector-tree (lambda (func-name func-spec)
                          (push func-name vars))
                        (aref environment 1)))
    vars))

#+clisp
(defmethod lexical-macros ((environment vector))
  (let ((macros '()))
    (when (aref environment 1)
      (walk-vector-tree 
       (lambda (macro-name macro-spec)
	 (if (system::macrop macro-spec)
	     (push (cons macro-name 
			 (macro-function macro-name environment))
		   macros)))
       (aref environment 1)))
    macros))

#+clisp
(defmethod lexical-symbol-macros ((environment vector))
  (let (symbol-macros '())
    (when (aref environment 0)
      (walk-vector-tree 
       (lambda (macro-name macro-spec)
	 (if (system::symbol-macro-p macro-spec)
	     (push (cons macro-name
			 (macroexpand-1 macro-name environment))
		   symbol-macros)))
       (aref environment 0)))
    symbol-macros))
      
;;;; ** LispWorks

#+(and lispworks macosx)
(defmethod environment-p ((environment system::augmented-environment))
  t)

#+(and lispworks macosx)
(defmethod lexical-variables ((environment system::augmented-environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

#+(and lispworks macosx)
(defmethod lexical-functions ((environment system::augmented-environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (eql 'compiler::macro (slot-value (cdr fenv) 'compiler::function-or-macro)))
                     (slot-value environment 'compiler::fenv))))

#+(and lispworks macosx)
(defmethod environment-p ((environment compiler::environment))
  t)

#+(and lispworks macosx)
(defmethod lexical-variables ((environment compiler::environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

#+(and lispworks macosx)
(defmethod lexical-functions ((environment compiler::environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (macro-function (car fenv) environment))
                     (slot-value environment 'compiler::fenv))))

#+(and lispworks (or win32 linux))
(defmethod environment-p ((environment lexical::environment))
  t)

#+(and lispworks (or win32 linux))
(defun lexical-runtime-p (value)
  (and (symbolp value)
       (eq (symbol-package value) nil)))

#+(and lispworks (or win32 linux))
(defmethod lexical-variables ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::variables)
        if (lexical-runtime-p (cdr candidate))
        collect (car candidate)))

#+(and lispworks (or win32 linux))
(defmethod lexical-functions ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::functions)
        if (lexical-runtime-p (cdr candidate))
        collect (car candidate)))


#+(and lispworks (or win32 linux))
(defmethod lexical-symbol-macros ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::variables)
        unless (lexical-runtime-p (cdr candidate))
        collect candidate))

#+(and lispworks (or win32 linux))
(defmethod lexical-macros ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::functions)
        unless (lexical-runtime-p (cdr candidate))
        collect candidate))

;;;; ** Allegro

#+(and allegro (version>= 7 0))
(defmethod environment-p ((env sys::augmentable-environment)) t)

#+(and allegro (version>= 7 0))
(defmethod lexical-variables ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-variables
     (lambda (symbol type rest)
       (declare (ignore rest))
       (when (and (eq type :lexical)
                  (sys:variable-information symbol env))
	 (push symbol fns)))
     env)
    fns))

#+(and allegro (version>= 7 0))
(defmethod lexical-functions ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-functions
     (lambda (name type rest)
       (when (and (eq type :function)
                  (sys:function-information name env))
	 (push name fns)))
     env)
    fns))

#+(and allegro (version>= 7 0))
(defmethod lexical-macros ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-functions
     (lambda (name type rest)
       (when (eq type :macro)
         (push (cons name (car rest)) fns)))
     env)
    fns))

#+(and allegro (version>= 7 0))
(defmethod lexical-symbol-macros ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-variables
     (lambda (symbol type rest)
       (when (eq type :symbol-macro)
         (push (cons symbol (car rest)) fns)))
     env)
    fns))


;; These functions are a half-assed implementation of section 8.5 in CLtL2
;; (environment manipulation)
;; I really don't feel like implementing THAT interface for every supported
;; Lisp.

(defgeneric augment-with-variable (env var))

(defgeneric augment-with-function (env fun))

(defgeneric augment-with-macro (env mac def))

(defgeneric augment-with-symbol-macro (env symmac def))

(defmethod augment-with-variable ((env t) var)
  (declare (ignore var))
  env)

(defmethod augment-with-function ((env t) fun)
  (declare (ignore fun))
  env)

(defmethod augment-with-macro ((env t) mac def)
  (declare (ignore mac def))
  env)

(defmethod augment-with-symbol-macro ((env t) symmac def)
  (declare (ignore symmac def))
  env)

#+sbcl
(defmethod augment-with-variable ((env sb-kernel:lexenv) var)
  (sb-c::make-lexenv :default env :vars (list (cons var t))))

#+sbcl
(defmethod augment-with-function ((env sb-kernel:lexenv) fun)
  (sb-c::make-lexenv :default env :funs (list (cons fun t))))

#+sbcl
(defmethod augment-with-macro ((env sb-kernel:lexenv) mac def)
  (sb-c::make-lexenv :default env :funs (list (list* mac 'sb-sys::macro def))))

#+sbcl
(defmethod augment-with-symbol-macro ((env sb-kernel:lexenv) symmac def)
  (sb-c::make-lexenv :default env :vars (list (list* symmac 'sb-sys::macro def))))

#+cmu
(defmethod augment-with-variable ((env c::lexenv) var)
  (c::make-lexenv :default env 
		  :variables (list (cons var (c::make-lambda-var :name var)))))

#+cmu
(defmethod augment-with-function ((env c::lexenv) fun)
  (c::make-lexenv :default env 
		  :functions (list (cons fun (lambda () 42)))))

#+cmu
(defmethod augment-with-macro ((env c::lexenv) mac def)
  (c::make-lexenv :default env 
		  :functions (list (list* mac 'system::macro def))))

#+cmu
(defmethod augment-with-symbol-macro ((env c::lexenv) symmac def)
  (c::make-lexenv :default env 
		  :variables (list (list* symmac 'system::macro def))))


#+clisp
(defun augment-with-var-and-fun (env &key var fun)
  (let* ((old-vars (aref env 0))
	 (old-funs (aref env 1))
	 (new-vars (if (eq var nil)
		       (make-array '(1) :initial-contents (list old-vars))
		       (make-array '(3) :initial-contents (list (car var) (cdr var) old-vars))))
	 (new-funs (if (eq fun nil)
		       (make-array '(1) :initial-contents (list old-funs))
		       (make-array '(3) :initial-contents (list (car fun) (cdr fun) old-funs)))))
    (make-array '(2) :initial-contents (list new-vars new-funs))))

;; I don't know whether t is an acceptable value to store here,
;; but CLISP does not complain.
#+clisp
(defmethod augment-with-variable ((env vector) var)
  (augment-with-var-and-fun env :var (cons var t)))

#+clisp
(defmethod augment-with-function ((env vector) fun)
  (augment-with-var-and-fun env :fun (cons fun t)))

#+clisp
(defmethod augment-with-macro ((env vector) mac def)
  (augment-with-var-and-fun env :fun (cons mac (system::make-macro def))))

#+clisp
(defmethod augment-with-symbol-macro ((env vector) symmac def)
  (augment-with-var-and-fun env :var
			    (cons symmac 
				  (system::make-symbol-macro def))))


#+(and lispworks (or win32 linux))
(defmethod augment-with-variable ((env lexical::environment) var)
  (harlequin-common-lisp:augment-environment
   env :variable (list var)))

#+(and lispworks (or win32 linux))
(defmethod augment-with-function ((env lexical::environment) fun)
  (harlequin-common-lisp:augment-environment
   env :function (list fun)))

#+(and lispworks (or win32 linux))
(defmethod augment-with-macro ((env lexical::environment) mac def)
  (harlequin-common-lisp:augment-environment
   env :macro (list (list mac def))))

#+(and lispworks (or win32 linux))
(defmethod augment-with-symbol-macro ((env lexical::environment) symmac def)
  (harlequin-common-lisp:augment-environment
   env :symbol-macro (list (list symmac def))))

#+(and allegro (version>= 7 0))
(defmethod augment-with-variable ((env sys::augmentable-environment) var)
  (system:augment-environment env :variable (list var)))

#+(and allegro (version>= 7 0))
(defmethod augment-with-function ((env sys::augmentable-environment) fun)
  (system:augment-environment env :function (list fun)))

#+(and allegro (version>= 7 0))
(defmethod augment-with-macro ((env sys::augmentable-environment) mac def)
  (system:augment-environment env :macro (list (list mac def))))

#+(and allegro (version>= 7 0))
(defmethod augment-with-symbol-macro ((env sys::augmentable-environment) symmac def)
  (system:augment-environment env :symbol-macro (list (list symmac def))))


(defun macroexpand-all (form &optional env)
  (unwalk-form (walk-form form nil (make-walk-env env))))

;; Sort of parse-macro from CLtL2.

(defun parse-macro-definition (name lambda-list body env)
  (declare (ignore name))
  (let* ((environment-var nil) 
	 (lambda-list-without-environment
	  (loop 
	   for prev = nil then i
	   for i in lambda-list
	   if (not (or (eq '&environment i) (eq '&environment prev)))
	   collect i
	   if (eq '&environment prev)
	   do (if (eq environment-var nil)
		  (setq environment-var i)
		  (error "Multiple &ENVIRONMENT clauses in macro lambda list: ~S" lambda-list))))
	 (handler-env (if (eq environment-var nil) (gensym "ENV-") environment-var))
	 whole-list lambda-list-without-whole)
    (if (eq '&whole (car lambda-list-without-environment))
	(setq whole-list (list '&whole (second lambda-list-without-environment))
	      lambda-list-without-whole (cddr lambda-list-without-environment))
	(setq whole-list '()
	      lambda-list-without-whole lambda-list-without-environment))
    (with-unique-names (handler-args form-name)
      `(lambda (,handler-args ,handler-env)
	 ,@(if (eq environment-var nil) 
	       `((declare (ignore ,handler-env)))
	       nil)
	 (destructuring-bind (,@whole-list ,form-name ,@lambda-list-without-whole)
	     ,handler-args
	   (declare (ignore ,form-name))
	   ,@(mapcar (lambda (form) (macroexpand-all form env)) body))))))

    
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
