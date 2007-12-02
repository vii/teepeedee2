;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Various flow control operators

;;;; ** Anaphoric conditionals

(defmacro if-bind (var test &body then/else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  "Just like IF-BIND but the var is always IT."
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  "Just like when except VAR will be bound to the
  result of TEST in BODY."
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  "Just like when expect the symbol IT will be
  bound to the result of TEST in BODY."
  `(when-bind it ,test ,@body))

(defmacro cond-bind (var &body clauses)
  "Just like COND but VAR will be bound to the result of the
  condition in the clause when executing the body of the clause."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
           (progn ,@(if body body (list var)))
           (cond-bind ,var ,@others)))
      nil))

(defmacro acond (&rest clauses)
  "Just like cond-bind except the var is automatically IT."
  `(cond-bind it ,@clauses))

(defmacro aand (&rest forms)
  `(and-bind it ,@forms))

(defmacro and-bind (var &rest forms)
  (cond
    ((cdr forms)
     `(when-bind ,var ,(first forms)
        (and-bind ,var ,@(cdr forms))))
    (forms (first forms))
    (t 't)))

;;;; ** Multiple value anaphoric conditionals

(defmacro if2-bind (var test &body then/else)
  "Anaphoric IF control structure for multiple values.

VAR (a symbol) will be bound to the primary value of TEST.  If
TEST's second value is true then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    (with-unique-names (bool)
      `(multiple-value-bind (,var ,bool) ,test
	 (if ,bool ,then ,else)))))

(defmacro aif2 (test then &optional else)
  "Just like IF-BIND but the var is always IT.

Very useful with functions like GETHASH."
  `(if2-bind it ,test ,then ,else))

;;;; ** Looping

(defmacro while (test &body body)
  "Repeat BODY while TEST is true.

You may exit the loop with (RETURN-FROM WHILE)."
  `(block while
     (loop
	(if ,test
	    (progn ,@body)
	    (return-from while)))))

(defmacro awhile (test &body body)
  "Just like WHILE, but the result of TEST is bound to IT.

You may exit the loop with (RETURN-FROM AWHILE)."
  `(block awhile
     (loop
	(aif ,test
	     (progn ,@body)
	     (return-from awhile)))))

(defmacro until (test &body body)
  "Repeat BODY until TEST is false.

You may exit the loop with (RETURN-FROM UNTIL)."
  `(block until
     (loop
	(if (not ,test)
	    (progn ,@body)
	    (return-from until)))))

;;;; ** Whichever

(defmacro whichever (&rest possibilities)
  "Evaluates one (and only one) of its args, which one is chosen at random"
  `(ecase (random ,(length possibilities))
     ,@(loop for poss in possibilities
             for x from 0
             collect (list x poss))))

;;;; ** XOR - The missing conditional

(defmacro xor (&rest datums)
  "Evaluates the args one at a time. If more than one arg returns true
  evaluation stops and NIL is returned. If exactly one arg returns
  true that value is returned."
  (let ((state (gensym "XOR-state-"))
        (block-name (gensym "XOR-block-"))
        (arg-temp (gensym "XOR-arg-temp-")))
    `(let ((,state nil)
           (,arg-temp nil))
       (block ,block-name
         ,@(loop
              for arg in datums
              collect `(setf ,arg-temp ,arg)
              collect `(if ,arg-temp
                           ;; arg is T, this can change the state
                           (if ,state
                               ;; a second T value, return NIL
                               (return-from ,block-name nil)
                               ;; a first T, swap the state
                               (setf ,state ,arg-temp))))
         (return-from ,block-name ,state)))))

;;;; ** Switch

(defmacro switch ((obj &key (test #'eql)) &body clauses)
  "Evaluate the first clause whose car satisfies (funcall test
  car obj)."
  ;; NB: There is no need to do the find-if and the remove here, we
  ;; can just as well do them with in the expansion
  (let ((default-clause (find-if (lambda (c) (eq t (car c))) clauses)))
    (when default-clause
      (setf clauses (remove default-clause clauses :test #'equalp)))
    (let ((obj-sym (gensym))
          (test-sym (gensym)))
      `(let ((,obj-sym ,obj)
             (,test-sym ,test))
         (cond
           ,@(mapcar (lambda (clause)
                       (let ((keys (ensure-list (car clause)))
                             (form (cdr clause)))
                         `((or ,@(mapcar (lambda (key)
					   `(funcall ,test-sym ',key ,obj-sym))
					 keys))
			   ,@form)))
                     clauses)
           ,@(when default-clause
                   `((t ,@(cdr default-clause)))))))))

(defmacro eswitch ((obj &key (test #'eql)) &body body)
  "Like switch but signals an error if no clause succeeds."
  (rebinding (obj test)
    `(switch (,obj :test ,test)
       ,@body
       (t
        (error "Unmatched SWITCH. Testing against ~S with ~S."
               ,obj ,test)))))

(defmacro cswitch ((obj &key (test #'eql)) &body body)
  "Like SWITCH but signals a continuable error if no clause
  matches."
  (rebinding (obj test)
    `(switch (,obj :test ,test)
       ,@body
       (t
        (cerror "Unmatched SWITCH. Testing against ~S with ~S."
                ,obj ,test)))))

;;;; ** Eliminating Nesting

(defmacro with* (&body body)
  (cond
    ((cddr body)
     (append (first body) `((with* ,@(cdr body)))))
    ((cdr body)
     `(,@(first body) ,(second body)))
    (body (first body))
    (t nil)))

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
