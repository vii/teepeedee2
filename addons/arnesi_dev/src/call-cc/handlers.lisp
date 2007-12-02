;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; ** Handlres for common-lisp special operators

;;;; Variable References

(defmethod evaluate/cc ((var local-variable-reference) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (lookup lex-env :let (name var) :error-p t)))

(defmethod evaluate/cc ((var local-lexical-variable-reference) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (funcall (first (lookup lex-env :lexical-let (name var) :error-p t)))))

(defmethod evaluate/cc ((var free-variable-reference) lex-env dyn-env k)
  (declare (ignore lex-env))
  (multiple-value-bind (value foundp)
      (lookup dyn-env :let (name var))
    (if foundp
        (kontinue k value)
        (kontinue k (symbol-value (name var))))))

;;;; Constants

(defmethod evaluate/cc ((c constant-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env))
  (kontinue k (value c)))

;;;; BLOCK/RETURN-FROM

(defmethod evaluate/cc ((block block-form) lex-env dyn-env k)
  (evaluate-progn/cc (body block)
                     (register lex-env :block (name block) k)
                     dyn-env k))

(defmethod evaluate/cc ((return return-from-form) lex-env dyn-env k)
  (declare (ignore k))
  (evaluate/cc (result return)
               lex-env dyn-env
               (lookup lex-env :block (name (target-block return)) :error-p t)))

;;;; CATCH/THROW

(defmethod evaluate/cc ((catch catch-form) lex-env dyn-env k)
  (evaluate/cc (tag catch) lex-env dyn-env
               `(catch-tag-k ,catch ,lex-env ,dyn-env ,k)))

(defk catch-tag-k (catch lex-env dyn-env k)
    (tag)
  (evaluate-progn/cc (body catch) lex-env (register dyn-env :catch tag k) k))

(defmethod evaluate/cc ((throw throw-form) lex-env dyn-env k)
  (evaluate/cc (tag throw) lex-env dyn-env
               `(throw-tag-k ,throw ,lex-env ,dyn-env ,k)))

(defk throw-tag-k (throw lex-env dyn-env k)
    (tag)
  (evaluate/cc (value throw) lex-env dyn-env
               (lookup dyn-env :catch tag :error-p t)))

;;;; FLET/LABELS

(defmethod evaluate/cc ((flet flet-form) lex-env dyn-env k)
  (let ((new-env lex-env))
    (dolist* ((name . form) (binds flet))
      (setf new-env (register new-env :flet name (make-instance 'closure/cc
                                                                :code form
                                                                :env lex-env))))
    (evaluate-progn/cc (body flet) new-env dyn-env k)))

(defmethod evaluate/cc ((labels labels-form) lex-env dyn-env k)
  (let ((closures '()))
    (dolist* ((name . form) (binds labels))
      (let ((closure (make-instance 'closure/cc :code form)))
        (setf lex-env (register lex-env :flet name closure))
        (push closure closures)))
    (dolist (closure closures)
      (setf (env closure) lex-env))
    (evaluate-progn/cc (body labels) lex-env dyn-env k)))

;;;; LET/LET*

;; returns a dynamic environment that holds the special variables imported for let
;; these variables are captured from the caller normal lisp code and stored within
;; the continuation. The mixin might be a binding-form-mixin and implicit-progn-with-declare-mixin.
(defun import-specials (mixin dyn-env)
  (dolist (declaration (declares mixin))
    (let ((name (name declaration)))
      (if (and (typep declaration 'special-declaration-form)
               (or (not (typep mixin 'binding-form-mixin))
                   (not (find name (binds mixin) :key 'first)))
               (not (lookup dyn-env :let name)))
          (setf dyn-env (register dyn-env :let name (symbol-value name))))))
  dyn-env)

(defmethod evaluate/cc ((let let-form) lex-env dyn-env k)
  (evaluate-let/cc (binds let) nil (body let) lex-env (import-specials let dyn-env) k))

(defk k-for-evaluate-let/cc (var remaining-bindings evaluated-bindings body lex-env dyn-env k)
    (value)
  (evaluate-let/cc remaining-bindings
                   (cons (cons var value) evaluated-bindings)
                   body lex-env dyn-env k))

(defun evaluate-let/cc (remaining-bindings evaluated-bindings body lex-env dyn-env k)
  (if remaining-bindings
      (destructuring-bind (var . initial-value)
          (car remaining-bindings)
        (evaluate/cc
         initial-value
         lex-env dyn-env
         `(k-for-evaluate-let/cc
           ,var
           ,(cdr remaining-bindings)
           ,evaluated-bindings
           ,body
           ,lex-env ,dyn-env ,k)))
      (dolist* ((var . value) evaluated-bindings
                (evaluate-progn/cc body lex-env dyn-env k))
        (if (special-var-p var (parent (first body)))
            (setf dyn-env (register dyn-env :let var value))
            (setf lex-env (register lex-env :let var value))))))

(defun special-var-p (var declares-mixin)
  (or (find-if (lambda (declaration)
                 (and (typep declaration 'special-declaration-form)
                      (eq (name declaration) var)))
               (declares declares-mixin))
      (boundp var)
      ;; This is the only portable way to check if a symbol is
      ;; declared special, without being boundp, i.e. (defvar 'foo).
      ;; Maybe we should make it optional with a compile-time flag?
      #+nil(eval `((lambda ()
                (flet ((func ()
                         (symbol-value ',var)))
                  (let ((,var t))
                    (declare (ignorable ,var))
                    (ignore-errors (func)))))))))

(defmethod evaluate/cc ((let* let*-form) lex-env dyn-env k)
  (evaluate-let*/cc (binds let*) (body let*) lex-env (import-specials let* dyn-env) k))

(defk k-for-evaluate-let*/cc (var bindings body lex-env dyn-env k)
    (value)
  (if (special-var-p var (parent (first body)))
      (evaluate-let*/cc bindings body
                        lex-env
                        (register dyn-env :let var value)
                        k)
      (evaluate-let*/cc bindings body
                        (register lex-env :let var value)
                        dyn-env
                        k)))

(defun evaluate-let*/cc (bindings body lex-env dyn-env k)
  (if bindings
      (destructuring-bind (var . initial-value)
          (car bindings)
        (evaluate/cc initial-value lex-env dyn-env
                      `(k-for-evaluate-let*/cc ,var ,(cdr bindings) ,body ,lex-env ,dyn-env ,k)))
      (evaluate-progn/cc body lex-env dyn-env k)))

;;;; IF

(defk k-for-evaluate-if/cc (then else lex-env dyn-env k)
    (value)
  (if value
      (evaluate/cc then lex-env dyn-env k)
      (evaluate/cc else lex-env dyn-env k)))

(defmethod evaluate/cc ((if if-form) lex-env dyn-env k)
  (evaluate/cc (consequent if) lex-env dyn-env
                `(k-for-evaluate-if/cc ,(then if) ,(else if) ,lex-env ,dyn-env ,k)))

;;;; LOCALLY

(defmethod evaluate/cc ((locally locally-form) lex-env dyn-env k)
  (evaluate-progn/cc (body locally) lex-env dyn-env k))

;;;; MACROLET

(defmethod evaluate/cc ((macrolet macrolet-form) lex-env dyn-env k)
  ;; since the walker already performs macroexpansion there's nothing
  ;; left to do here.
  (evaluate-progn/cc (body macrolet) lex-env dyn-env k))

;;;; multiple-value-call

(defk k-for-m-v-c (remaining-arguments evaluated-arguments lex-env dyn-env k)
    (value other-values)
  (evaluate-m-v-c
   remaining-arguments (append evaluated-arguments (list value) other-values)
   lex-env dyn-env k))

(defun evaluate-m-v-c (remaining-arguments evaluated-arguments lex-env dyn-env k)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) lex-env dyn-env
                   `(k-for-m-v-c  ,(cdr remaining-arguments) ,evaluated-arguments ,lex-env ,dyn-env ,k))
      (destructuring-bind (function &rest arguments)
          evaluated-arguments
        (etypecase function
          (closure/cc (apply-lambda/cc function arguments dyn-env k))
          (function (apply #'kontinue k (multiple-value-list
                                         (multiple-value-call function (values-list arguments)))))))))

(defmethod evaluate/cc ((m-v-c multiple-value-call-form) lex-env dyn-env k)
  (evaluate-m-v-c (list* (func m-v-c) (arguments m-v-c)) '() lex-env dyn-env k))

;;;; PROGN

(defmethod evaluate/cc ((progn progn-form) lex-env dyn-env k)
  (evaluate-progn/cc (body progn) lex-env dyn-env k))

(defk k-for-evaluate-progn/cc (rest-of-body lex-env dyn-env k)
    ()
  (evaluate-progn/cc rest-of-body lex-env dyn-env k))

(defun evaluate-progn/cc (body lex-env dyn-env k)
  (cond
    ((cdr body)
      (evaluate/cc (first body) lex-env dyn-env
                    `(k-for-evaluate-progn/cc ,(cdr body) ,lex-env ,dyn-env ,k)))
    (body
     (evaluate/cc (first body) lex-env dyn-env k))
    (t
     (kontinue k nil))))

;;;; SETQ

(defk k-for-local-setq (var lex-env dyn-env k)
    (value)
  (setf (lookup lex-env :let var :error-p t) value)
  (kontinue k value))

(defk k-for-free-setq (var lex-env dyn-env k)
    (value)
  (setf (symbol-value var) value)
  (kontinue k value))

(defk k-for-local-lexical-setq (var lex-env dyn-env k)
    (value)
  (funcall (second (lookup lex-env :lexical-let var :error-p t)) value)
  (kontinue k value))

(defmethod evaluate/cc ((setq setq-form) lex-env dyn-env k)
  (macrolet ((if-found (&key in-env of-type kontinue-with)
               `(multiple-value-bind (value foundp)
                    (lookup ,in-env ,of-type (var setq))
                  (declare (ignore value))
                  (when foundp
                    (return-from evaluate/cc
                      (evaluate/cc (value setq) lex-env dyn-env
                                   `(,',kontinue-with ,(var setq) ,lex-env ,dyn-env ,k)))))))
    (if-found :in-env lex-env
              :of-type :let
              :kontinue-with k-for-local-setq)
    (if-found :in-env dyn-env
              :of-type :let
              :kontinue-with k-for-special-setq)
    (if-found :in-env lex-env
              :of-type :lexical-let
              :kontinue-with k-for-local-lexical-setq)
    (evaluate/cc (value setq)
                       lex-env dyn-env
                       `(k-for-free-setq ,(var setq) ,lex-env ,dyn-env ,k))))

;;;; SYMBOL-MACROLET

(defmethod evaluate/cc ((symbol-macrolet symbol-macrolet-form) lex-env dyn-env k)
  ;; like macrolet the walker has already done all the work needed for this.
  (evaluate-progn/cc (body symbol-macrolet) lex-env dyn-env k))

;;;; TAGBODY/GO

(defk tagbody-k (k)
    ()
  (kontinue k nil))

(defmethod evaluate/cc ((tagbody tagbody-form) lex-env dyn-env k)
  (evaluate-progn/cc (body tagbody)
                     (register lex-env :tag tagbody k) dyn-env
                     `(tagbody-k ,k)))

(defmethod evaluate/cc ((go-tag go-tag-form) lex-env dyn-env k)
  (declare (ignore go-tag lex-env dyn-env))
  (kontinue k nil))

(defmethod evaluate/cc ((go go-form) lex-env dyn-env k)
  (declare (ignore k))
  (evaluate-progn/cc (target-progn go) lex-env dyn-env
                     (lookup lex-env :tag (enclosing-tagbody go) :error-p t)))

;;;; THE

(defmethod evaluate/cc ((the the-form) lex-env dyn-env k)
  (evaluate/cc (value the) lex-env dyn-env k))

;;;; LOAD-TIME-VALUE

(defmethod evaluate/cc ((c load-time-value-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env))
  (kontinue k (value c)))

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
