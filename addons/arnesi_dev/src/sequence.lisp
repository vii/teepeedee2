;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Manipulating sequences

(defun tail (seq &optional (how-many 1))
  "Returns the last HOW-MANY elements of the sequence SEQ. HOW-MANY is
  greater than (length SEQ) then all of SEQ is returned."
  (let ((seq-length (length seq)))
    (cond
      ((<= 0 how-many seq-length)
       (subseq seq (- seq-length how-many)))
      ((< seq-length how-many)
       (copy-seq seq))
      (t ; (< how-many 0)
       (head seq (- how-many))))))

(defun but-tail (seq &optional (how-many 1))
  "Returns SEQ with the last HOW-MANY elements removed."
  (let ((seq-length (length seq)))
    (cond
      ((<= 0 how-many seq-length)
       (subseq seq 0 (- seq-length how-many)))
      ((< seq-length how-many)
       (copy-seq seq))
      (t
       (but-head seq (- how-many))))))

(defun head (seq &optional (how-many 1))
  "Returns the first HOW-MANY elements of SEQ."
  (let ((seq-length (length seq)))
    (cond
      ((<= 0 how-many seq-length)
       (subseq seq 0 how-many))
      ((< seq-length how-many)
       (copy-seq seq))
      (t
       (tail seq (- how-many))))))

(defun but-head (seq &optional (how-many 1))
  "Returns SEQ with the first HOW-MANY elements removed."
  (let ((seq-length (length seq)))
    (cond ((<= 0 how-many (length seq))
           (subseq seq how-many))
          ((< seq-length how-many)
           (copy-seq seq))
          (t
           (but-tail seq (- how-many))))))

(defun starts-with (sequence prefix &key (test #'eql) (return-suffix nil))
  "Test whether the first elements of SEQUENCE are the same (as
  per TEST) as the elements of PREFIX.

If RETURN-SUFFIX is T the functions returns, as a second value, a
displaced array pointing to the sequence after PREFIX."
  (let ((length1 (length sequence))
        (length2 (length prefix)))
    (when (< length1 length2)
      (return-from starts-with (values nil nil)))
    (dotimes (index length2)
      (when (not (funcall test (elt sequence index) (elt prefix index)))
        (return-from starts-with (values nil nil))))
    ;; if we get here then we match
    (values t
            (if return-suffix
                (make-array (- (length sequence) (length prefix))
                            :element-type (array-element-type sequence)
                            :displaced-to sequence
                            :displaced-index-offset (length prefix)
                            :adjustable nil)
                nil))))

(defun ends-with (seq1 seq2 &key (test #'eql))
  "Test whether SEQ1 ends with SEQ2. In other words: return true if
  the last (length seq2) elements of seq1 are equal to seq2."
  (let ((length1 (length seq1))
        (length2 (length seq2)))
    (when (< length1 length2)
      ;; if seq1 is shorter than seq2 than seq1 can't end with seq2.
      (return-from ends-with nil))
    (loop
       for seq1-index from (- length1 length2) below length1
       for seq2-index from 0 below length2
       when (not (funcall test (elt seq1 seq1-index) (elt seq2 seq2-index)))
         do (return-from ends-with nil)
       finally (return t))))

(defun read-sequence* (sequence stream &key (start 0) end)
  "Like READ-SEQUENCE except the sequence is returned as well.

The second value returned is READ-SEQUENCE's primary value, the
primary value returned by READ-SEQUENCE* is the medified
sequence."
  (let ((pos (read-sequence sequence stream :start start :end end)))
    (values sequence pos)))

(defmacro deletef
    (item sequence &rest delete-args
     &environment e)
  "Delete ITEM from SEQUENCE, using cl:delete, and update SEQUENCE.

DELETE-ARGS are passed directly to cl:delete."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion sequence e)
    `(let* (,@(mapcar #'list vars vals)
            (,(car store-vars) ,reader-form))
       (setq ,(car store-vars) (delete ,item ,(car store-vars)
                                       ,@delete-args))
       ,writer-form)))


(defun copy-array (array)
  "Returns a fresh copy of ARRAY. The returned array will have
  the same dimensions and element-type, will not be displaced and
  will have the same fill-pointer as ARRAY.
  
See http://thread.gmane.org/gmane.lisp.allegro/13 for the
original implementation and discussion."
  (let ((dims (array-dimensions array))
        (fill-pointer (and (array-has-fill-pointer-p array)
                           (fill-pointer array))))
    (adjust-array
     (make-array dims :displaced-to array)
     dims
     :fill-pointer fill-pointer)))

(defun make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))

;;;; ** Levenshtein Distance

;;;; 1) Set n to be the length of s. Set m to be the length of t. If n
;;;;    = 0, return m and exit. If m = 0, return n and exit. Construct
;;;;    a matrix containing 0..m rows and 0..n columns.

;;;; 2) Initialize the first row to 0..n. Initialize the first column
;;;;    to 0..m.

;;;; 3) Examine each character of s (i from 1 to n).

;;;; 4) Examine each character of t (j from 1 to m).

;;;; 5) If s[i] equals t[j], the cost is 0. If s[i] doesn't equal
;;;;    t[j], the cost is 1.

;;;; 6) Set cell d[i,j] of the matrix equal to the minimum of: a. The
;;;;    cell immediately above plus 1: d[i-1,j] + 1. b. The cell
;;;;    immediately to the left plus 1: d[i,j-1] + 1. c. The cell
;;;;    diagonally above and to the left plus the cost: d[i-1,j-1] +
;;;;    cost.

;;;; 7) After the iteration steps (3, 4, 5, 6) are complete, the
;;;;    distance is found in cell d[n,m].

(defun levenshtein-distance (source target &key (test #'eql))
  (block nil
    (let ((source-length (length source))
	  (target-length (length target)))
      (when (zerop source-length)
	(return target-length))
      (when (zerop target-length)
	(return source-length))
      (let ((buffer (make-array (1+ target-length))))
	(dotimes (i (1+ target-length))
	  (setf (aref buffer i) i))
	;; we make a slight modification to the alogrithm described
	;; above. we don't create the entire array, just enough to
	;; keep the info we need, which is an array of size
	;; target-length + the "above" value and the "over". (this is
	;; similar to the optimizaiont for determining lcs).
	(loop
	   for i from 1 upto source-length
	   do (setf (aref buffer 0) i)
	   do (loop
		 with above-value = i
		 with over-value = (1- i)
		 for j from 1 upto target-length
		 for cost = (if (funcall test (elt source (1- i))
					      (elt target (1- j)))
				0 1)
		 do (let ((over-value* (aref buffer j)))
		      (setf (aref buffer j) (min (1+ above-value)
						 (1+ (aref buffer j))
						 (+ cost over-value))
			    above-value (aref buffer j)
			    over-value over-value*))))
	(return (aref buffer target-length))))))

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
