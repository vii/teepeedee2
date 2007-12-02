;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Defining classes with DEFSTRUCT's syntax

(defmacro defclass-struct (name-and-options supers &rest slots)
  "DEFCLASS with a DEFSTRUCT api.

NAME-AND-OPTIONS:

  name-symbol |
  ( name-symbol [ (:conc-name conc-name ) ]
                [ (:predicate predicate-name ) ]
                class-option* )

SUPERS - a list of super classes passed directly to DEFCLASS.

SLOTS - a list of slot forms:

  name |
  ( name [ init-arg ] [ slot-options* ] )"
  (generate-defclass (first (ensure-list name-and-options))
                     (cdr (ensure-list name-and-options))
                     supers slots))

(defun generate-defclass (class-name options supers slots)
  (let ((conc-name nil)
        (predicate nil)
        (predicate-forms nil)
        (class-options '()))
    (loop
       for (option-name . args) in options
       do (case option-name
            (:conc-name
             (when conc-name
               (error "Can't specify the :CONC-NAME argument more than once."))
             (setf conc-name (first args)))
            (:predicate
             (when predicate
               (error "Can't specify the :PREDICATE argument more than once."))
             (setf predicate (if (eql t (first args))
                                 (intern (strcat class-name :-p) *package*)
                                 (first args))))
            (t
             (push (cons option-name args) class-options))))
    (setf slots
          (mapcar
           (lambda (slot-spec)
             (destructuring-bind (name
                                  &optional initform
                                  &rest options)
                 (ensure-list slot-spec)
               `(,name
                 :initform ,initform
                 ,@(when conc-name
                     `(:accessor ,(intern (strcat conc-name name)
                                          (symbol-package conc-name))))
                 :initarg ,(intern (symbol-name name) :keyword)
                 ,@options)))
           slots)
          predicate-forms
          (if predicate
              (with-unique-names (obj)
                `((defmethod ,predicate ((,obj ,class-name)) t)
                  (defmethod ,predicate ((,obj t)) nil)))
              nil))
    `(prog1
         (defclass ,class-name ,supers ,slots ,@(nreverse class-options))
       ,@predicate-forms)))

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
