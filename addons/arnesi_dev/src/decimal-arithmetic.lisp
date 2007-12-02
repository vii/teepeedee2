;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Decimal Arithmetic

;;;; Converting to and from external representations

(defvar *precision* 2
  "Default precision.")

(defmacro with-precision (prec &body body)
  "Evalute BODY with *precision* bound to PREC."
  (let ((precision (gensym)))
    `(let ((,precision ,prec))
       (assert (integerp ,precision)
	       (,precision)
	       "Precision must be an integer, not ~S" ,precision)
       (let ((*precision* (10^ ,precision)))
	 (declare (special *precision*))
	 ,@body))))

(defun decimal-from-float (float
                           &optional (precision *precision*)
                                     (rounding-method #'round-half-up))
  "Convert FLOAT to an exact value with precision PRECISION using
  ROUNDING-METHOD to do any neccessary rounding."
  (funcall rounding-method float precision))

(defun float-from-decimal (decimal)
  "Convert the exact decimal value DECIMAL to a (not neccassily
  equal) floating point value."
  (float decimal))

;;;; Rounding functions

(defun round-down (number &optional (precision *precision*))
  "Round towards 0."
  (if (minusp number)
      (round-ceiling number precision)
      (round-floor   number precision)))

(defun round-half-up (number &optional (precision *precision*))
  "Round towards the nearest value allowed with the current
precision. If the current value is exactly halfway between two logal
values round away from 0."
  (multiple-value-bind (value discarded)
      (floor (* number precision))
    (if (<= 1/2 discarded)
	(/ (1+ value) precision)
        (/ value precision))))

(defun round-half-even (number &optional (precision *precision*))
  "Round towards the nearest value allowed with the current
precision. If the current value is exactly halfway between two legal
values round towards the nearest even value."
  (multiple-value-bind (value discarded)
      (floor (* number precision))
    (cond
     ((< discarded 1/2) ;; down
      (/ value precision))
     ((= discarded 1/2) ;; goto even
      (if (evenp value)
	  (/ value precision)
	  (/ (1+ value) precision)))
     (t ;; (>= discarded 1/2)
      (/ (1+ value) precision)))))
	
(defun round-ceiling (number &optional (precision *precision*))
  "Round towards positive infintity"
  (/ (ceiling (* number precision)) precision))

(defun round-floor (number &optional (precision *precision*))
  "Round towards negative infinity."
  (/ (floor (* number precision)) precision))

(defun round-half-down (number &optional (precision *precision*))
  "Round towards the nearest legal value. If the current value is
exactly half way between two legal values round towards 0."
  (multiple-value-bind (value discarded)
      (floor number)
    (if (< 1/2 discarded)
	(/ (1+ value) precision)
        (/ value precision))))

(defun round-up (number &optional (precision *precision*))
  "Round away from 0."
  (if (minusp number)
      (round-floor number precision)
      (round-ceiling number precision)))

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
