;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Messing with the MOP

;;;; The code pre-dates Pascal Costanza's closer-mop package. If
;;;; you're looking for a compatability layer you should probably look
;;;; there instead.

(defmacro with-class-slots ((object class-name &key except) &body body)
  "Execute BODY as if in a with-slots form containig _all_ the
  slots of (find-clas CLASS-NAME). This macro, which is something
  of an ugly hack, inspects the class named by CLASS-NAME at
  macro expansion time. Should the class CLASS-NAME change form
  containing WITH-CLASS-SLOTS must be recompiled. Should the
  class CLASS-NAME not be available at macro expansion time
  WITH-CLASS-SLOTS will fail."
  (declare (ignore object class-name except body))
  (error "Not yet implemented."))

;;;; ** wrapping-standard method combination

(define-method-combination wrapping-standard
    (&key (around-order :most-specific-first)
          (before-order :most-specific-first)
          (primary-order :most-specific-first)
          (after-order :most-specific-last)
          (wrapping-order :most-specific-last)
          (wrap-around-order :most-specific-last))
  ((wrap-around (:wrap-around))
   (around (:around))
   (before (:before))
   (wrapping (:wrapping))
   (primary () :required t)
   (after (:after)))
  "Same semantics as standard method combination but allows
\"wrapping\" methods. Ordering of methods:

 (wrap-around
   (around
     (before)
     (wrapping
       (primary))
     (after)))

:warp-around, :around, :wrapping and :primary methods call the
next least/most specific method via call-next-method (as in
standard method combination).

The various WHATEVER-order keyword arguments set the order in
which the methods are called and be set to either
:most-specific-last or :most-specific-first."
  (labels ((effective-order (methods order)
             (ecase order
               (:most-specific-first methods)
               (:most-specific-last (reverse methods))))
           (call-methods (methods)
             (mapcar (lambda (meth) `(call-method ,meth))
                     methods)))
    (let* (;; reorder the methods based on the -order arguments
           (wrap-around (effective-order wrap-around wrap-around-order))
           (around (effective-order around around-order))
           (wrapping (effective-order wrapping wrapping-order))
           (before (effective-order before before-order))
           (primary (effective-order primary primary-order))
           (after (effective-order after after-order))
           ;; inital value of the effective call is a call its primary
           ;; method(s)
           (form (case (length primary)
                   (1 `(call-method ,(first primary)))
                   (t `(call-method ,(first primary) ,(rest primary))))))
      (when wrapping
        ;; wrap form in call to the wrapping methods
        (setf form `(call-method ,(first wrapping)
                                 (,@(rest wrapping) (make-method ,form)))))
      (when before
        ;; wrap FORM in calls to its before methods
        (setf form `(progn
                      ,@(call-methods before)
                      ,form)))
      (when after
        ;; wrap FORM in calls to its after methods
        (setf form `(multiple-value-prog1
                        ,form
                      ,@(call-methods after))))
      (when around
        ;; wrap FORM in calls to its around methods
        (setf form `(call-method ,(first around)
                                 (,@(rest around)
                                    (make-method ,form)))))
      (when wrap-around
        (setf form `(call-method ,(first wrap-around)
                                 (,@(rest wrap-around)
                                    (make-method ,form)))))
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
