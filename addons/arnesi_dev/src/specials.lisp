;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * def-special-environment

(defun check-required (name vars required)
  (dolist (var required)
    (assert (member var vars)
            (var)
            "Unrecognized symbol ~S in ~S." var name)))

(defmacro def-special-environment (name (&key accessor binder binder*)
                                  &rest vars)
  "Define two macros for dealing with groups or related special variables.

ACCESSOR is defined as a macro: (defmacro ACCESSOR (VARS &rest
BODY)).  Each element of VARS will be bound to the
current (dynamic) value of the special variable.

BINDER is defined as a macro for introducing (and binding new)
special variables. It is basically a readable LET form with the
prorpe declarations appended to the body. The first argument to
BINDER must be a form suitable as the first argument to LET.

ACCESSOR defaults to a new symbol in the same package as NAME
which is the concatenation of \"WITH-\" NAME. BINDER is built as
\"BIND-\" and BINDER* is BINDER \"*\"."
  (unless accessor
    (setf accessor (intern-concat (list '#:with- name) (symbol-package name))))
  (unless binder
    (setf binder   (intern-concat (list '#:bind- name) (symbol-package name))))
  (unless binder*
    (setf binder*  (intern-concat (list binder '#:*) (symbol-package binder))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (flet ()
       (defmacro ,binder (requested-vars &body body)
         (check-required ',name ',vars (mapcar #'car requested-vars))
         `(let ,requested-vars
            (declare (special ,@(mapcar #'car requested-vars)))
            ,@body))
       (defmacro ,binder* (requested-vars &body body)
         (check-required ',name ',vars (mapcar #'car requested-vars))
         `(let* ,requested-vars
            (declare (special ,@(mapcar #'car requested-vars)))
            ,@body))
       (defmacro ,accessor (requested-vars &body body)
         (check-required ',name ',vars requested-vars)
         `(locally (declare (special ,@requested-vars))
            ,@body))
       ',name)))
  
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
