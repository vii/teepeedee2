;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Higher order functions

(defun compose (f1 &rest functions)
  "Returns a function which applies the arguments in order.

 (funcall (compose #'list #'+) 1 2 3) ==> (6)"
  (case (length functions)
    (0 f1)
    (1 (lambda (&rest args)
         (funcall f1 (apply (car functions) args))))
    (2 (lambda (&rest args)
         (funcall f1
                  (funcall (first functions)
                           (apply (second functions) args)))))
    (3 (lambda (&rest args)
         (funcall f1
                  (funcall (first functions)
                           (funcall (second functions)
                                    (apply (third functions) args))))))
    (t
     (let ((funcs (nreverse (cons f1 functions))))
       (lambda (&rest args)
         (loop
            for f in funcs
            for r = (multiple-value-list (apply f args))
               then (multiple-value-list (apply f r))
            finally (return (values-list r))))))))

(defun conjoin (&rest predicates)
  (case (length predicates)
    (0 (constantly t))
    (1 (car predicates))
    (2 (lambda (&rest args)
         (and (apply (first predicates) args)
              (apply (second predicates) args))))
    (3 (lambda (&rest args)
         (and (apply (first predicates) args)
              (apply (second predicates) args)
              (apply (third predicates) args))))
    (t
     (lambda (&rest args)
       (loop
          for p in predicates
          for val = (apply p args)
          while val
          finally (return val))))))

(defun curry (function &rest initial-args)
  "Returns a function which will call FUNCTION passing it
  INITIAL-ARGS and then any other args.

 (funcall (curry #'list 1) 2) ==> (list 1 2)"
  (lambda (&rest args)
    (apply function (append initial-args args))))

(defun rcurry (function &rest initial-args)
  "Returns a function which will call FUNCTION passing it the
  passed args and then INITIAL-ARGS.

 (funcall (rcurry #'list 1) 2) ==> (list 2 1)"
  (lambda (&rest args)
    (apply function (append args initial-args))))

(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))

(defmacro lambda-rec (name args &body body)
  "Just like lambda except BODY can make recursive calls to the
  lambda by calling the function NAME."
  `(lambda ,args
     (labels ((,name ,args ,@body))
       (,name ,@args))))

;;;; ** Just for fun

(defun y (lambda)
  (funcall (lambda (f)
             (funcall (lambda (g)
                        (funcall g g))
                      (lambda (x)
                        (funcall f
                                 (lambda ()
                                   (funcall x x))))))
           lambda))

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
