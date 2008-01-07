;;; Code shared accross the entire weblocks framework
(defpackage #:cl-cont
  (:nicknames #:cont)
  (:use :cl)
  (:documentation
   "A library that implements continuations by transforming Common
  Lisp code to continuation passing style."))

(in-package :cont)

(export '(defcpstransformer cpstransformer copy-transformation-context))

(defparameter *special-form-transformers* (make-hash-table :size 25)
  "A hashtable that maps symbols to functions. Each function acts as a
  transformer of a special form and must accept three arguments: an
  s-expression to transform to CPS-style, an an s-expression to
  continue to, and an environment object.")

(defmacro defcpstransformer (name lambda-list &body body)
  "A macro that allows defining special case CPS transformers."
  `(setf (gethash ',name *special-form-transformers*)
	 (lambda ,lambda-list
	   ,@body)))

(defun cpstransformer (name)
  "Returns a CPS transformer named by a given NAME."
  (gethash name *special-form-transformers*))

(defmacro expand-form (form &environment env)
  "A helper macro that expands forms by considering local macros."
  (multiple-value-bind (expansion expanded-p)
      (macroexpand form env)
    `(values ',expansion ',expanded-p)))

(defstruct (call/cc-context (:conc-name ctx-))
  "A structure that represents a context used during call/cc
transformation."
  (block-tags (make-hash-table))
  (go-tags (make-hash-table))
  (local-functions nil))

(defun copy-hash-table (ht)
  "Shallow hashtable copy."
  (let ((nht (make-hash-table :size (hash-table-size ht))))
    (maphash (lambda (key value)
	       (setf (gethash key nht) value))
	     ht)
    nht))

(defun copy-transformation-context (ctx)
  "Copies transformation context. This is used for cases when the
context has to be 'frozen' in time and used at a later transformation
stage."
  (let ((transf-env (copy-structure ctx)))
    (setf (ctx-block-tags transf-env) (copy-hash-table (ctx-block-tags transf-env))
	  (ctx-go-tags transf-env) (copy-hash-table (ctx-go-tags transf-env))
	  (ctx-local-functions transf-env) (copy-list (ctx-local-functions transf-env)))
    transf-env))
