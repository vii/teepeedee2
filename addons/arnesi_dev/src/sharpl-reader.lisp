;; -*- lisp -*-

(in-package :arnesi)

;;;; * A reader macro for simple lambdas

;;;; Often we have to create small (in the sense of textually short)
;;;; lambdas. This read macro, bound to #L by default, allows us to
;;;; eliminate the 'boilerplate' LAMBDA and concentrate on the body of
;;;; the lambda.

(defmacro sharpl-expander (package body min-args &environment env)
  (let* ((form body)
         (lambda-args (loop
                         for i upfrom 1 upto (max (or min-args 0)
                                                  (highest-bang-var form env))
                         collect (make-sharpl-arg package i))))
    `(lambda ,lambda-args
       , (when lambda-args
           `(declare (ignorable ,@lambda-args)))
       ,form)))

(defun sharpL-reader (stream subchar min-args)
  "Reader macro for simple lambdas.

This read macro reads exactly one form and serves to eliminate
the 'boiler' plate text from such lambdas and write only the body
of the lambda itself. If the form contains any references to
variables named !1, !2, !3, !n etc. these are bound to the Nth
parameter of the lambda.

Examples:

#L(foo) ==> (lambda () (foo)).

#L(foo !1) ==> (lambda (!1) (foo !1))

#L(foo (bar !2) !1) ==> (lambda (!1 !2) (foo (bar !2) !1))

All arguments are declared ignorable. So if there is a reference
to an argument !X but not !(x-1) we still take X arguments, but x
- 1 is ignored. Examples:

#L(foo !2) ==> (lambda (!1 !2) (declare (ignore !1)) (foo !2))

We can specify exactly how many arguments to take by using the
read macro's prefix parameter. NB: this is only neccessary if the
lambda needs to accept N arguments but only uses N - 1. Example:

#2L(foo !1) ==> (lambda (!1 !2) (declare (ignore !2)) (foo !1))

When #l forms are nested, !X variables are bound to the innermost 
form. Example:

#l#l(+ !1 !2)

returns a function that takes no arguments and returns a function
that adds its two arguments."
  (declare (ignore subchar))
  (let ((body (read stream t nil t)))
    `(sharpl-expander ,*package* ,body ,min-args)))

(defun with-sharp-l-syntax ()
  "To be used with the curly reader from arnesi: {with-sharp-l-syntax #L(typep !1 'foo)}"
  (lambda (handler)
    (%enable-sharp-l-reader)
    `(progn ,@(funcall handler))))

(defmacro enable-sharp-l-syntax ()
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-l-reader)))

(defun %enable-sharp-l-reader ()
  "Bind SHARPL-READER to the macro character #L.

This function overrides (and forgets) and previous value of #L."
  (set-dispatch-macro-character #\# #\L 'sharpL-reader))

(defun enable-sharp-l ()
  "TODO: Obsolete, to be removed. Use the enable-sharp-l-syntax macro."
  ;; (warn "Use the enable-sharp-l-syntax macro instead of enable-sharp-l")
  (%enable-sharp-l-reader))

(defun find-var-references (input-form)
  (typecase input-form
    (cons 
      (append (find-var-references (car input-form))
	      (find-var-references (cdr input-form))))

    (free-variable-reference (list (slot-value input-form 'name)))
    (local-lexical-variable-reference (list (slot-value input-form 'name)))
      
    (form
     (loop for slot-name in (mapcar #'mopp:slot-definition-name 
				    (mopp::class-slots (class-of input-form)))
	   if (not (member slot-name '(parent target-progn enclosing-tagbody target-block)))
	   append (find-var-references (slot-value input-form slot-name))))

    (t nil)))

(defun highest-bang-var (form env)
  (let ((*warn-undefined* nil))
    (or
     (loop for var in (find-var-references (walk-form form nil (make-walk-env env)))
	   if (bang-var-p var)
	   maximize (bang-var-p var))
     0)))

(defun bang-var-p (form)
  (and (char= #\! (aref (symbol-name form) 0))
       (parse-integer (subseq (symbol-name form) 1) :junk-allowed t)))

(defun make-sharpl-arg (package number)
  (intern (format nil "!~D" number) package))

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

;; This code was heavily inspired by iterate, which has the following
;; copyright:

;;                     ITERATE, An Iteration Macro
;;
;;                 Copyright 1989 by Jonathan Amsterdam
;;         Adapted to ANSI Common Lisp in 2003 by Andreas Fuchs
;;
;; Permission to use, copy, modify, and distribute this software and its
;; documentation for any purpose and without fee is hereby granted,
;; provided that this copyright and permission notice appear in all
;; copies and supporting documentation, and that the name of M.I.T. not
;; be used in advertising or publicity pertaining to distribution of the
;; software without specific, written prior permission. M.I.T. makes no
;; representations about the suitability of this software for any
;; purpose.  It is provided "as is" without express or implied warranty.

;; M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;; M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;; ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;; WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;; SOFTWARE.
