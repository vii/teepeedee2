;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; ** Functions, Generic Functions, Methods and standard-combination

;;;; DEFUN/CC

(defmacro defun/cc (name arguments &body body)
  `(progn
     (setf (fdefinition/cc ',name 'defun/cc)
           (make-instance 'closure/cc
                          :code (walk-form '(lambda ,arguments
                                             (block ,name ,@body))
                                           nil nil)
                          :env nil))
     (defun ,name ,arguments
       (declare (ignore ,@(extract-argument-names arguments)))
       (error "Sorry, /CC function are not callable outside of with-call/cc."))))

;;;; DEFGENERIC/CC

(defmacro defgeneric/cc (name args &rest options)
  "Trivial wrapper around defgeneric designed to alert readers that these methods are cc methods."
  (assert (not (find :method options :key #'first)) () "TODO: defgeneric/cc does not walk the :method entries yet, use standalone defmethod/cc's")
  `(progn
     (defgeneric ,name ,args
       ,@options
       (:method-combination cc-standard))
     (setf (fdefinition/cc ',name 'defmethod/cc) t)))

;;;; DEFMETHOD/CC

; for emacs:  (setf (get 'defmethod/cc 'common-lisp-indent-function) 'lisp-indent-defmethod)

(defmacro defmethod/cc (name &rest args)
  (let ((qlist (list (if (and (symbolp (car args))
				  (not (null (car args))))
                         (pop args)
                         :primary))))
    (let ((arguments (car args))
	  (body (cdr args)))
      `(progn
	 (unless (eq 'defmethod/cc (second (multiple-value-list (fdefinition/cc ',name))))
           (setf (fdefinition/cc ',name 'defmethod/cc) t)
           (defgeneric/cc ,name ,(if arguments 
                                     (convert-to-generic-lambda-list arguments)
                                     '())))
	 (defmethod ,name ,@qlist ,arguments
           ,(when arguments 
	     `(declare (ignorable ,@(extract-argument-names arguments :allow-specializers t))))
	   ,@(when (stringp (first body))
              (list (pop body)))
	   (make-instance 'closure/cc
			  :code (walk-form '(lambda ,(clean-argument-list arguments)
					     (block ,name ,@body))
					   nil nil)
			  :env nil))))))

;;;; CC-STANDARD (standard-combination for cc methods)

(defun closure-with-nextmethod (closure next)
  (make-instance 'closure/cc 
		 :code (code closure)
		 :env (register (env closure) :next-method t next)))

(defun closure-with-befores (closure befores)
  (make-instance 'closure/cc 
		 :code (walk-form `(lambda (&rest args)
				     ,@(loop 
					  for before in befores
					  collect `(apply ,before args))
				     (apply ,closure args)))
		 :env nil))

(defun closure-with-afters (closure afters)
  (make-instance 'closure/cc 
		 :code (walk-form `(lambda (&rest args)
				     (prog1 
					 (apply ,closure args)
				       ,@(loop 
					    for after in afters
					    collect `(apply ,after args)))))
		 :env nil))

(define-method-combination cc-standard
    (&key (around-order :most-specific-first)
          (before-order :most-specific-first)
          (primary-order :most-specific-first)
          (after-order :most-specific-last))
  ((around (:around))
   (before (:before))
   (primary (:primary) :required t)
   (after (:after)))
  
  (labels ((effective-order (methods order)
             (ecase order
               (:most-specific-first methods)
               (:most-specific-last (reverse methods))))
	   (primary-wrap (methods &optional nextmethod)
	     (case (length methods)
	       (1 `(closure-with-nextmethod 
		    (call-method ,(first methods))
		    ,nextmethod))
	       (t `(closure-with-nextmethod 
		    (call-method ,(first methods))
		    ,(primary-wrap (cdr methods) nextmethod)))))
	   (call-methods (methods)
	     `(list ,@(loop 
			 for m in methods
			 collect `(call-method ,m)))))
    (let* (;; reorder the methods based on the -order arguments
           (around  (effective-order around around-order))
           (before  (effective-order before before-order))
           (primary (effective-order primary primary-order))
           (after   (effective-order after after-order))
           (form    (primary-wrap primary)))
      (when after 
	(setf form `(closure-with-afters ,form ,(call-methods after))))
      (when before 
	(setf form `(closure-with-befores ,form ,(call-methods before))))
      (when around
	(setf form (primary-wrap around form)))
      form)))

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
