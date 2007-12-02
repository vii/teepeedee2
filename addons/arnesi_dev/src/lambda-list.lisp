;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Lambda-lists

(defun extract-argument-names (lambda-list &key allow-specializers)
  "Returns a list of symbols representing the names of the
  variables bound by the lambda list LAMBDA-LIST."
  (mapcan (lambda (argument)
	    (let1 vars '()
	      (dolist (slot-name '(name supplied-p-parameter))
		(awhen (and (slot-exists-p argument slot-name)
			    (slot-boundp   argument slot-name)
			    (slot-value    argument slot-name))
		  (push it vars)))
	      (nreverse vars)))
	  (walk-lambda-list lambda-list nil '() :allow-specializers allow-specializers)))

(defun convert-to-generic-lambda-list (defmethod-lambda-list)
  (loop
     with generic-lambda-list = '()
     for arg in (walk-lambda-list defmethod-lambda-list
                                  nil nil
                                  :allow-specializers t)
     do (etypecase arg
          ((or required-function-argument-form
               specialized-function-argument-form)
           (push (name arg) generic-lambda-list))
          (keyword-function-argument-form
           (pushnew '&key generic-lambda-list)
           (if (keyword-name arg)
               (push (list (list (keyword-name arg)
                                 (name arg)))
                     generic-lambda-list)
               (push (list (name arg)) generic-lambda-list)))
          (rest-function-argument-form
           (push '&rest generic-lambda-list)
           (push (name arg) generic-lambda-list))
          (optional-function-argument-form
           (pushnew '&optional generic-lambda-list)
           (push (name arg) generic-lambda-list))
          (allow-other-keys-function-argument-form
           (unless (member '&key generic-lambda-list)
             (push '&key generic-lambda-list))
           (push '&allow-other-keys generic-lambda-list)))
     finally (return (nreverse generic-lambda-list))))

(defun clean-argument-list (lambda-list)
  (loop
     for head on lambda-list
     for argument = (car head)
     if (member argument '(&optional &key &rest &allow-other-keys))
       return (append cleaned head)
     else
       collect (if (listp argument)
                   (first argument)
                   argument)
       into cleaned
     finally (return cleaned)))

;; Copyright (c) 2002-2006, Edward Marco Baringer
;; Copyright (c)      2006, Hoan Ton-That
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
