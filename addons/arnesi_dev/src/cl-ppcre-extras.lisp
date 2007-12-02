;; -*- lisp -*-

(in-package :it.bese.arnesi)

(defpackage :it.bese.arnesi.cl-ppcre-extras
  (:use)
  (:nicknames :rx)
  (:export
   #:=~
   #:!~
   #:$1
   #:$2
   #:$3
   #:$4
   #:$5
   #:$6
   #:$7
   #:$8
   #:$9))

(defparameter rx::$_ nil
  "The current default target for regexp matching.")
(defparameter rx::$1 nil "The string matched by the first group in the last regexp match.")
(defparameter rx::$2 nil "The string matched by the second group in the last regexp match.")
(defparameter rx::$3 nil "The string matched by the third group in the last regexp match.")
(defparameter rx::$4 nil "The string matched by the fourth group in the last regexp match.")
(defparameter rx::$5 nil "The string matched by the fifth group in the last regexp match.")
(defparameter rx::$6 nil "The string matched by the sixth group in the last regexp match.")
(defparameter rx::$7 nil "The string matched by the seventh group in the last regexp match.")
(defparameter rx::$8 nil "The string matched by the eight group in the last regexp match.")
(defparameter rx::$9 nil "The string matched by the ninth group in the last regexp match.")

(defmacro rx::=~ (regexp &optional (target 'rx::$_) (then t) (else nil))
  "Equivalent to perl's if (TARGET =~ REGEXP) { THEN } else { ELSE }.

Attempt to match REGEXP agains TARGET, if the match succedes THEN
is evaluated with $1, .. $9 bound to the groups in
REGEXP. Otherwise ELSE is executed."
  (destructuring-bind (regexp &rest create-scanner-args) (if (listp regexp)
                                                             regexp
                                                             (list regexp))
    (destructuring-bind (trgt &key start end) (if (listp target)
                                                  target
                                                  (list target))
      (let ((match-start (gensym))
            (match-end (gensym))
            (register-starts (gensym))
            (register-ends (gensym))
            (num-registers (gensym))
            (target (gensym)))
        (flet ((gen-$-var (index)
                 `(if (< ,num-registers ,index)
                      nil
                      (let ((start (aref ,register-starts (1- ,index)))
                            (end (aref ,register-ends (1- ,index))))
                        (if (null start)
                            nil
                            (make-array (- end start) :displaced-to ,target :displaced-index-offset start))))))
          `(let ((,target ,trgt))
             (multiple-value-bind (,match-start ,match-end ,register-starts ,register-ends)
                 (cl-ppcre:scan (cl-ppcre:create-scanner ,regexp ,@create-scanner-args)
                       ,trgt ,@(when start `(:start ,start))
                             ,@(when end `(:end ,end)))
               (declare (ignore ,match-end))
               (if (not (null ,match-start))
                   (let* ((,num-registers (length ,register-starts)))
                     (setf rx::$1 ,(gen-$-var 1)
                           rx::$2 ,(gen-$-var 2)
                           rx::$3 ,(gen-$-var 3)
                           rx::$4 ,(gen-$-var 4)
                           rx::$5 ,(gen-$-var 5)
                           rx::$6 ,(gen-$-var 6)
                           rx::$7 ,(gen-$-var 7)
                           rx::$8 ,(gen-$-var 8)
                           rx::$9 ,(gen-$-var 9))
                     ,then)
                   ,else))))))))

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
