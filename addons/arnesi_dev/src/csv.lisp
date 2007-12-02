;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Reading and Writing files in Comma-Seperated-Values format

;;;; Generating CSV files from lisp data

(defun princ-csv (items csv-stream
                  &key (quote #\")
                       (separator #\,)
                       (ignore-nulls t)
                       (newline +CR-LF+)
                       (princ #'princ-to-string))
  "Write the list ITEMS to csv-stream."
  (flet ((write-word (word)
           (write-char quote csv-stream)
           (loop
              for char across (funcall princ word)
              if (char= quote char) do
                (progn
                  (write-char quote csv-stream)
                  (write-char quote csv-stream))
              else do
                (write-char char csv-stream))
           (write-char quote csv-stream)))
    (when items
      (write-word (car items))
      (dolist (i (cdr items))
        (write-char separator csv-stream)
        (if ignore-nulls
            (when (not (null i))
              (write-word i))
            (write-word i)))
      (write-sequence newline csv-stream))))

(defun princ-csv-to-string (items)
  (with-output-to-string (csv)
    (princ-csv items csv)))

;;;; Reading in CSV files

(defun parse-csv-string (line &key (separator #\,) (quote #\"))
  "Parse a csv line into a list of strings using seperator as the
  column seperator and quote as the string quoting character."
  (let ((items '())
        (offset 0)
        (current-word (make-array 20
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer 0))
        (state :read-word))
    (loop
       (when (= offset (length line))
         ;; all done
         (ecase state
           (:in-string
            (error "Unterminated string."))
           (:read-word
            (return-from parse-csv-string
              (nreverse (cons current-word items))))))
       (cond
         ((char= separator (aref line offset))
          (ecase state
            (:in-string
             (vector-push-extend (aref line offset) current-word))
            (:read-word
             (push current-word items)
             (setf current-word (make-array 20
                                            :element-type 'character
                                            :adjustable t
                                            :fill-pointer 0)))))
         ((char= quote (aref line offset))
          (ecase state
            (:in-string
             (let ((offset+1 (1+ offset)))
	       (cond
		 ((and (/= offset+1 (length line))
		       (char= quote (aref line offset+1)))
		  (vector-push-extend quote current-word)
		  (incf offset))
		 (t (setf state :read-word)))))
            (:read-word
             (setf state :in-string))))
         (t
          (vector-push-extend (aref line offset) current-word)))
       (incf offset))))

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
