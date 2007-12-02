;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * vector/array utilities

(defun vector-push-extend* (vector &rest items)
  (let ((element-type (array-element-type vector)))
    (dolist (item items)
      (cond
        ((typep item element-type) ;; item can be put directly into the 
         (vector-push-extend item vector))
        ((typep item `(vector ,element-type)) ;; item should be a vector
         (loop
            for i across item
            do (vector-push-extend i vector)))
        (t
         (error "Bad type for item ~S." item))))
    vector))

(defun string-from-array (array &key (start 0) (end (1- (length array))))
  "Assuming ARRAY is an array of ASCII chars encoded as bytes return
the corresponding string. Respect the C convention of null terminating
strings. START and END specify the zero indexed offsets of a sub range
of ARRAY."
  ;; This is almost always the case
  (assert (<= 0 start (1- (length array)))
          (start)
          "START must be a valid offset of ARRAY.")
  (assert (<= 0 end (1- (length array)))
          (end)
          "END must be a valid offset of ARRAY.")
  (assert (<= start end)
          (start end)
          "START must be less than or equal to END.")
  (assert (every (lambda (element) (<= 0 element 255)) array)
	  (array)
	  "Some element of ~S was not > 0 and < 255" array)
  (let* ((working-array (make-array (1+ (- end start))
                                    :element-type (array-element-type array)
                                    :displaced-to array
                                    :displaced-index-offset start))
	  (length (if-bind pos (position 0 working-array)
		      pos
		      (length working-array))))
    (map-into (make-array length :element-type 'character)
	      #'code-char
	      working-array)))

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
