;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * HTTP/HTML utilities

;;;; ** URIs/URLs
;;;; http://www.faqs.org/rfcs/rfc2396.html

(eval-always
  (defvar *uri-escaping-ok-table* (make-array 256
                                              :element-type 'boolean
                                              :initial-element nil))
  (loop
      ;; The list of characters which don't need to be escaped when writing URIs.
      ;; This list is inherently a heuristic, because different uri components may have
      ;; different escaping needs, but it should work fine for http.
      for ok-char across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,/" do
      (setf (aref *uri-escaping-ok-table* (char-code ok-char)) t))
  (setf *uri-escaping-ok-table* (coerce *uri-escaping-ok-table* '(simple-array boolean (256)))))

(defun escape-as-uri (string)
  "Escapes all non alphanumeric characters in STRING following
  the URI convention. Returns a fresh string."
  (with-output-to-string (escaped)
    (write-as-uri string escaped)))

(defun write-as-uri (string stream)
  (declare (type vector string)
           (type stream stream)
           (optimize (speed 3) (debug 0)))
  (loop
      for char-code :of-type (unsigned-byte 8) :across (the (vector (unsigned-byte 8))
                                                         (string-to-octets string :utf-8)) do
      (if (aref (the (simple-array boolean (256)) (load-time-value *uri-escaping-ok-table* t)) char-code)
          (write-char (code-char char-code) stream)
          (format stream "%~2,'0X" char-code))))

(define-condition uri-parse-error (error)
  ((what :initarg :what :reader uri-parse-error.what)))

(define-condition expected-digit-uri-parse-error (uri-parse-error) ())

(defun continue-as-is (c)
  (declare (ignore c))
  (awhen (find-restart 'continue-as-is)
    (invoke-restart it)))

(defun try-other-encoding (c encoding)
  (declare (ignore c))
  (awhen (find-restart 'try-other-encoding)
    (invoke-restart it encoding)))

(defun unescape-as-uri-non-strict (string)
  (handler-bind ((uri-parse-error #'continue-as-is)
                 (serious-condition #'(lambda (c)
                                        (try-other-encoding c :iso-8859-1)) ))
    (%unescape-as-uri string)))

(defun %unescape-as-uri (input)
  "URI unescape based on http://www.ietf.org/rfc/rfc2396.txt"
  (declare (type string input)
           (optimize (speed 3) (debug 0)))
  (let ((input-length (length input)))
    (when (zerop input-length)
      (return-from %unescape-as-uri ""))
    (let* ((input-index 0)
           (output (make-array input-length :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (declare (type fixnum input-length input-index))
      (labels ((read-next-char (must-exists-p)
                 (when (>= input-index input-length)
                   (if must-exists-p
                       (error 'uri-parse-error :what input)
                       (return-from %unescape-as-uri
                         (restart-case
                             (octets-to-string output :utf-8)
                           (try-other-encoding (encoding)
                             :report "Try converting uri using other encoding"
                             (octets-to-string output encoding))))))
                 (prog1 (aref input input-index)
                   (incf input-index)))
               (write-next-byte (byte)
                 (vector-push-extend byte output)
                 (values))
               (char-to-int (char)
                 (let ((result (digit-char-p char 16)))
                   (unless result
                     (error 'expected-digit-uri-parse-error :what char))
                   result))
               (parse ()
                 (let ((next-char (read-next-char nil)))
                   (case next-char
                     (#\% (char%))
                     (#\+ (char+))
                     (t (write-next-byte (char-code next-char))))
                   (parse)))
               (char% ()
                 (let ((restart-input-index input-index))
                   (restart-case
                    (write-next-byte (+ (ash (char-to-int (read-next-char t)) 4)
                                        (char-to-int (read-next-char t))))
                    (continue-as-is ()
                                    :report "Continue reading uri without attempting to convert the escaped-code to a char."
                                    (setf input-index restart-input-index)
                                    (write-next-byte #.(char-code #\%)))))
                 (values))
               (char+ ()
                 (write-next-byte #.(char-code #\Space))))
        (parse)))))

(declaim (inline unescape-as-uri))
(defun unescape-as-uri (string)
  (%unescape-as-uri string))

(declaim (inline nunescape-as-uri))
(defun nunescape-as-uri (string)
  (%unescape-as-uri string))



;;;; ** HTML

;;;; This so blatently wrong its not even funny, and while this is
;;;; exactly what I need I would do well to start using a "real" html
;;;; escaping library (there are a couple to choose from).

(defun make-html-entities ()
  (let ((ht (make-hash-table :test 'equalp)))
    (flet ((add-mapping (char escaped)
             (setf (gethash char ht) escaped
                   (gethash escaped ht) char)))
      (add-mapping #\< "&lt;")
      (add-mapping #\> "&gt;")
      (add-mapping #\& "&amp;")
      (add-mapping #\" "&quot;")
      (add-mapping #\space "&nbsp;")
      (add-mapping "a`" "&#224;")
      (add-mapping "a'" "&#225;")
      (add-mapping "e`" "&#232;")
      (add-mapping "e'" "&#233;")
      (add-mapping "i'" "&#236;")
      (add-mapping "i`" "&#237;")
      (add-mapping "o`" "&#242;")
      (add-mapping "o'" "&#243;")
      (add-mapping "u`" "&#249;")
      (add-mapping "u'" "&#250;"))
    ht))

(defparameter *html-entites* (make-html-entities))

(defun html-entity->char (entity &optional (default #\?))
  (let ((res (gethash entity *html-entites*)))
    (if res
        (if (stringp res)
            (char res 0)
            res)
        default)))

(defun write-as-html (string &key (stream t) (escape-whitespace nil))
  (loop
     for char across string
     do (cond
          ((char= char #\Space)
           (if escape-whitespace
               (princ "&nbsp;" stream)
               (write-char char stream)))
          ((gethash char *html-entites*)
           (princ (gethash char *html-entites*) stream))
          (t (write-char char stream)))))

(defun escape-as-html (string &key (escape-whitespace nil))
  (with-output-to-string (escaped)
    (write-as-html string
                   :stream escaped
                   :escape-whitespace escape-whitespace))) 

(define-condition html-escape-error (error)
  ((what :accessor html-escape-error.what :initarg :what)))

(define-condition unterminated-html-entity (html-escape-error)
  ())

(define-condition unknown-html-entity (html-escape-error)
  ())

(define-condition unknown-char-escape (warning)
  ((what :accessor html-escape-error.what :initarg :what)))

(defun unescape-as-html (string)
  (with-output-to-string (unescaped)
    (loop
       for offset upfrom 0 below (length string)
       for char = (aref string offset)
       if (char= #\& char)
         do (progn
              (aif (position #\; string :start offset)
                   (let ((escape-tag (subseq string offset (1+ it))))
                     (aif (gethash escape-tag *html-entites*)
                          (progn
                            (princ it unescaped)
                            (incf offset (1- (length escape-tag))))
                          (if (char= #\# (aref escape-tag 1))
                              ;; special code, ignore
                              (restart-case
                                  (warn 'unknown-char-escape :what escape-tag)
                                (continue-delete ()
                                  :report "Continue processing, delete this char."
                                  (incf offset (1- (length escape-tag)))))
                              (restart-case
                                  (error 'unknown-html-entity :what escape-tag)
                                (continue-as-is ()
                                  :report "Continue processing, leaving the string as is."
                                  (write-char #\& unescaped))
                                (continue-delete ()
                                  :report "Continue processing, delete this entity."
                                  (incf offset (1- (length escape-tag))))))))
                   (restart-case
                       (error 'unterminated-html-entity
                              :what (subseq string offset
                                            (min (+ offset 20)
                                                 (length string))))
                     (continue-as-is ()
                       :report "Continue processing, leave the string as is."
                       (write-char #\& unescaped)))))
       else do (write-char char unescaped))))

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
