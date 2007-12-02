;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Messing with numbers

(defun parse-ieee-double (u64)
  "Given an IEEE 64 bit double representeted as an integer (ie a
  sequence of 64 bytes), return the coressponding double value"
  (* (expt -1 (ldb (byte 1 63) u64))
     (expt 2 (- (ldb (byte 11 52) u64) 1023))
     (1+ (float (loop for i from 51 downto 0
                      for n = 2 then (* 2 n)
                      for frac = (* (/ n) (ldb (byte 1 i) u64))
                      sum frac)))))

(defun radix-values (radix)
  (assert (<= 2 radix 35)
          (radix)
          "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
  (make-array radix
              :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :displaced-index-offset 0
              :element-type 
              #+lispworks 'base-char
              #-lispworks 'character))

(defun parse-float (float-string
                    &key (start 0) (end nil) (radix 10)
                         (junk-allowed t)
                         (type 'single-float)
                         (decimal-character #\.))
  (let ((radix-array (radix-values radix))
        (integer-part 0)
        (mantissa 0)
        (mantissa-size 1)
        (sign 1))
    (with-input-from-string (float-stream (string-upcase (string-trim '(#\Space #\Tab) float-string)) :start start :end end)
      (labels ((peek () (peek-char nil float-stream nil nil nil))
               (next () (read-char float-stream nil nil nil))
               (sign () ;; reads the (optional) sign of the number
                 (cond
                   ((char= (peek) #\+) (next) (setf sign 1))
                   ((char= (peek) #\-) (next) (setf sign -1)))
                 (integer-part))
               (integer-part ()
                 (cond
                   ((position (peek) radix-array)
                    ;; the next char is a valid char
                    (setf integer-part (+ (* integer-part radix)
                                          (position (next) radix-array)))
                    ;; again
                    (return-from integer-part (integer-part)))
                   ((null (peek))
                    ;; end of string
                    (done))
                   ((char= decimal-character (peek))
                    ;; the decimal seperator
                    (next)
                    (return-from integer-part (mantissa)))                   
                   ;; junk
                   (junk-allowed (done))
                   (t (bad-string))))
               (mantissa ()                 
                 (cond
                   ((position (peek) radix-array)
                    (setf mantissa (+ (* mantissa radix)
                                      (position (next) radix-array))
                          mantissa-size (* mantissa-size radix))
                    (return-from mantissa
                      (mantissa)))
                   ((or (null (peek)) junk-allowed)
                    ;; end of string
                    (done))
                   (t (bad-string))))
               (bad-string ()
                 (error "Unable to parse ~S." float-string))
               (done ()
                 (return-from parse-float
                   (coerce (* sign (+ integer-part (/ mantissa mantissa-size))) type))))
        (sign)))))
    
(define-modify-macro mulf (B)
  *
  "SETF NUM to the result of (* NUM B).")

(define-modify-macro divf (B)
  /
  "SETF NUM to the result of (/ NUM B).")

(define-modify-macro minf (other)
  (lambda (current other)
    (if (< other current)
        other
        current))
  "Sets the place to new-value if new-value is #'< the current value")

(define-modify-macro maxf (other)
  (lambda (current other)
    (if (> other current)
        other
        current))
  "Sets the place to new-value if new-value is #'> the current value")

(defun map-range (lambda min max &optional (step 1))
  (loop for i from min upto max by step
     collect (funcall lambda i)))

(defmacro do-range ((index &optional min max step return-value)
                    &body body)
  (assert (or min max)
          (min max)
          "Must specify at least MIN or MAX")
  `(loop
      for ,index ,@(when min `(from ,min))
                 ,@(when max `(upto ,max))
                 ,@(when step `(by ,step))
      do (progn ,@body)
      finally (return ,return-value)))

(defun 10^ (x)
  (expt 10 x))

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
