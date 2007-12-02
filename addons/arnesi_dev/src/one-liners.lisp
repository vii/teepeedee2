;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Miscalaneous stuff

(defun intern-concat (string-designators &optional (package *package*))
  (intern (with-output-to-string (symbol-name)
            (dolist (designator string-designators)
              (write-string (etypecase designator
                              (symbol (symbol-name designator))
                              (string designator))
                            symbol-name)))
          package))

(defmacro with-unique-names ((&rest bindings) &body body)
  "Evaluate BODY with BINDINGS bound to fresh unique symbols.

Syntax: WITH-UNIQUE-NAMES ( [ var | (var x) ]* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar (lambda (binding)
                   (check-type binding (or cons symbol))
                   (destructuring-bind (var &optional (prefix (symbol-name var)))
                       (if (consp binding) binding (list binding))
                     (check-type var symbol)
                     `(,var (gensym ,(concatenate 'string prefix "-")))))
                 bindings)
     ,@body))

(defmacro rebinding (bindings &body body)
  "Bind each var in BINDINGS to a gensym, bind the gensym to
var's value via a let, return BODY's value wrapped in this let.

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical
environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (car (if (consp binding) binding (list binding)))
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let* ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                               ,,@body))))))

(defmacro rebind (bindings &body body)
  `(let ,(loop
            for symbol-name in bindings
            collect (list symbol-name symbol-name))
     ,@body))

(defmacro with-accessors* (accessor-names object &body body)
  "Just like WITH-ACCESSORS, but if the slot-entry is a symbol
  assume the variable and accessor name are the same."
  `(with-accessors ,(mapcar (lambda (name)
			      (if (consp name) 
				  name 
				  `(,name ,name)))
			    accessor-names)
       ,object
     ,@body))

(defmacro define-constant (name value doc-string &optional export-p)
  "DEFCONSTANT with extra EXPORT-P argument."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(when export-p
        `(export ',name ,(package-name (symbol-package name))))
     (defconstant ,name ,value ,doc-string)))


(defun register (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(defmacro extend (environment type name datum &rest other-datum)
  `(setf ,environment (register ,environment ,type ,name ,datum ,@other-datum)))

(defun lookup (environment type name &key (error-p nil) (default-value nil))
  (loop
     for (.type .name . data) in environment
     when (and (eql .type type) (eql .name name))
       return (values data t)
     finally
       (if error-p
           (error "Sorry, No value for ~S of type ~S in environment ~S found."
                  name type environment)
           (values default-value nil))))

(defun (setf lookup) (value environment type name &key (error-p nil))
  (loop
     for env-piece in environment
     when (and (eql (first env-piece)  type)
               (eql (second env-piece) name))
       do (setf (cddr env-piece) value) and
       return value
     finally
       (when error-p
         (error "Sorry, No value for ~S of type ~S in environment ~S found."
                name type environment))))

(defun remove-keywords (plist &rest keywords)
  "Creates a copy of PLIST without the listed KEYWORDS."
  (declare (optimize (speed 3)))
  (loop for cell = plist :then (cddr cell)
        for el = (car cell)
        while cell
        unless (member el keywords :test #'eq)
        collect el
        and collect (cadr cell)
        and do (assert (cdr cell) () "Not a proper plist")))

(define-modify-macro remf-keywords (&rest keywords) remove-keywords
  "Creates a copy of PLIST without the properties identified by KEYWORDS.")

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro defalias (function redefinition)
  `(eval-always
    (progn
      (setf (fdefinition ',redefinition) (function ,function))
      ',redefinition)))

(defmacro defvaralias (variable redefinition)
  `(eval-always
    (defvar ,redefinition ,variable)))

(defmacro defmacalias (macro redefinition)
  #-allegro
  (with-unique-names (args)
    `(eval-always
      (defmacro ,redefinition (&rest ,args)
        `(,',macro ,@,args))))
  #+allegro ;; with-unique-names is undefined in allegro, why? This is a quick fix.
  (let ((args (gensym)))
    `(eval-always
      (defmacro ,redefinition (&rest ,args)
        `(,',macro ,@,args)))))


(defmacalias lambda fun)

(defalias make-instance new)

(defun append1 (list x)
  (append list (list x)))

(defun last1 (l)
  (car (last l)))

(defun flatten1 (l)
  (reduce #'append l))

(defun singlep (list)
  (and (consp list) (not (cdr list))))

(defun class-name-of (obj)
  (class-name (class-of obj)))

(defun circularize (&rest items)
  (let ((items (copy-list items)))
    (nconc items items)))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; Copyright (c) 2002-2006, Edward Marco Baringer
;; Copyright (c) 2006,      Hoan Ton-That
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
;;  - Neither the name of Edward Marco Baringer, Hoan Ton-That, nor
;;    BESE, nor the names of its contributors may be used to endorse
;;    or promote products derived from this software without specific
;;    prior written permission.
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
