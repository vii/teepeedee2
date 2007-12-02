;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-unicode.lisp --- Unicode encodings.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; This implementation is largely based on OpenMCL's.

(in-package #:babel-encodings)

(defconstant +repl+ #xfffd "Unicode replacement character code point.")
(defconstant +byte-order-mark-code+ #xfeff)
(defconstant +swapped-byte-order-mark-code+ #xfffe)
(defconstant +swapped-byte-order-mark-code-32+ #xfffe0000)

;;; Some convenience macros adding FIXNUM declarations.
(defmacro f-ash (integer count) `(the fixnum (ash ,integer ,count)))
(defmacro f-logior (&rest integers) `(the fixnum (logior ,@integers)))
(defmacro f-logand (&rest integers) `(the fixnum (logand ,@integers)))
(defmacro f-logxor (&rest integers) `(the fixnum (logxor ,@integers)))

;;;; UTF-8

(define-character-encoding :utf-8
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 4 bytes."
  :max-units-per-char 4
  :literal-char-code-limit #x80
  :bom-encoding #(#xef #xbb #xbf)
  :default-replacement #xfffd)

(define-octet-counter :utf-8 (accessor type)
  `(lambda (seq start end)
     (declare (type ,type seq) (fixnum start end))
     (loop with noctets = 0
           for i fixnum from start below end
           for code of-type code-point = (,accessor seq i) do
           (incf noctets
                 (cond ((< code #x80) 1)
                       ((< code #x800) 2)
                       ((< code #x10000) 3)
                       (t 4)))
           finally (return noctets))))

(define-code-point-counter :utf-8 (accessor type)
  `(lambda (seq start end)
     (declare (type ,type seq) (fixnum start end))
     (loop with nchars fixnum = 0
           with i fixnum = start
           while (< i end)
           for octet of-type ub8 = (,accessor seq i)
           for next-i fixnum = (+ i (cond ((< octet #x80) 1)
                                          ((< octet #xe0) 2)
                                          ((< octet #xf0) 3)
                                          (t 4)))
           do (if (> next-i end)
                  (progn
                    ;; So, if this error is suppressed, we return the
                    ;; length calculated so far and the decoder won't
                    ;; see an END-OF-INPUT-IN-CHARACTER error.
                    (decoding-error (vector octet) :utf-8 seq i
                                    nil 'end-of-input-in-character)
                    (return (values nchars i)))
                  (setq nchars (1+ nchars)
                        i next-i))
           finally (progn
                     (assert (= i end))
                     (return (values nchars i))))))

(define-encoder :utf-8 (src-accessor src-type dest-accessor dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
           for i fixnum from start below end
           for code of-type code-point = (,src-accessor src i) do
           (macrolet ((octet (offset)
                        `(,',dest-accessor dest (the fixnum (+ di ,offset)))))
             (cond
               ;; 1 octet
               ((< code #x80)
                (setf (octet 0) code)
                (incf di))
               ;; 2 octets
               ((< code #x800)
                (setf (octet 0) (logior #xc0 (f-ash code -6))
                      (octet 1) (logior #x80 (f-logand code #x3f)))
                (incf di 2))
               ;; 3 octets
               ((< code #x10000)
                (setf (octet 0) (logior #xe0 (f-ash code -12))
                      (octet 1) (logior #x80 (f-logand #x3f (f-ash code -6)))
                      (octet 2) (logior #x80 (f-logand code #x3f)))
                (incf di 3))
               ;; 4 octets
               (t
                (setf (octet 0) (logior #xf0 (f-logand #x07 (f-ash code -18)))
                      (octet 1) (logior #x80 (f-logand #x3f (f-ash code -12)))
                      (octet 2) (logior #x80 (f-logand #x3f (f-ash code -6)))
                      (octet 3) (logand #x3f code))
                (incf di 4))))
           ;; XXX: this return value is obviously wrong, but I'm
           ;; leaving this in until either STRING-TO-OCTETS or some
           ;; unit test catches it.
           finally (return (the fixnum (- d-start di))))))

(define-decoder :utf-8 (src-accessor src-type dest-accessor dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((u2 0) (u3 0) (u4 0))
       (declare (type ub8 u2 u3 u4))
       (loop for di fixnum from d-start
             for i fixnum from start below end
             for u1 of-type ub8 = (,src-accessor src i) do
             ;; Note: CONSUME-OCTET doesn't check if I is being
             ;; incremented past END.  We're assuming that END has
             ;; been calculated with the CODE-POINT-POINTER above that
             ;; checks this.
             (macrolet
                 ((consume-octet ()
                    `(,',src-accessor src (incf i)))
                  (handle-error (n)
                    `(decoding-error (vector ,@(subseq '(u1 u2 u3 u4) 0 n))
                                     :utf-8 src (- i ,n) +repl+)))
               (setf (,dest-accessor dest di)
                     (cond
                       ((< u1 #x80)     ; 1 octet
                        u1)
                       ((>= u1 #xc2)
                        (setq u2 (consume-octet))
                        (cond
                          ((< u1 #xe0)  ; 2 octets
                           (if (< (f-logxor u2 #x80) #x40)
                               (logior (f-ash (f-logand #x1f u1) 6)
                                       (f-logxor u2 #x80))
                               (handle-error 2)))
                          (t
                           (setq u3 (consume-octet))
                           (cond
                             ((< u1 #xf0) ; 3 octets
                              (if (and (< (f-logxor u2 #x80) #x40)
                                       (< (f-logxor u3 #x80) #x40)
                                       (or (>= u1 #xe1) (>= u2 #xa0)))
                                  (logior
                                   (f-ash (f-logand u1 #x0f) 12)
                                   (f-logior (f-ash (f-logand u2 #x3f) 6)
                                             (f-logand u3 #x3f)))
                                  (handle-error 3)))
                             (t         ; 4 octets
                              (setq u4 (consume-octet))
                              (if (and (< (f-logxor u2 #x80) #x40)
                                       (< (f-logxor u3 #x80) #x40)
                                       (< (f-logxor u4 #x80) #x40)
                                       (or (>= u1 #xf1) (>= u2 #x90)))
                                  (logior
                                   (f-logior (f-ash (f-logand u1 7) 18)
                                             (f-ash (f-logxor u2 #x80) 12))
                                   (f-logior (f-ash (f-logxor u3 #x80) 6)
                                             (f-logxor u4 #x80)))
                                  (handle-error 4))))))))))
             finally (return (the fixnum (- d-start di)))))))

;;;; UTF-16

;;; TODO: add a way to pass some info at compile-time telling us that,
;;; for example, the maximum code-point will always be < #x10000 in
;;; which case we could simply return (* 2 (- end start)).
(defmacro utf16-octet-counter (accessor type)
  `(lambda (seq start end)
     (declare (type ,type seq) (fixnum start end))
     (loop with noctets = 0
           for i fixnum from start below end
           for code of-type code-point = (,accessor seq i)
           do (incf noctets (if (< code #x10000) 2 4))
           finally (return (values noctets end)))))

(defmacro utf-16-combine-surrogate-pairs (u1 u2)
  `(the (unsigned-byte 21)
     (+ #x10000
        (the (unsigned-byte 20)
          (logior
           (the (unsigned-byte 20)
             (ash (the (unsigned-byte 10) (- ,u1 #xd800)) 10))
           (the (unsigned-byte 10)
             (- ,u2 #xdc00)))))))

(define-character-encoding :utf-16
    "A 16-bit, variable-length encoding in which characters with
code points less than #x10000 can be encoded in a single 16-bit
word and characters with larger codes can be encoded in a pair of
16-bit words.  The endianness of the encoded data is indicated by
the endianness of a byte-order-mark character (#\u+feff)
prepended to the data; in the absence of such a character on
input, the data is assumed to be in big-endian order.  Output is
written in native byte-order with a leading byte-order mark."
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness t            ; not necessarily true when decoding
  :literal-char-code-limit #x10000
  :use-bom #+babel::be :utf-16be #+babel::le :utf-16le
  :bom-encoding #+babel::be #(#xfe #xff) #+babel::le #(#xff #xfe)
  :nul-encoding #(0 0)
  :default-replacement #xfffd)

(define-octet-counter :utf-16 (accessor type)
  `(utf16-octet-counter ,accessor ,type))

(define-code-point-counter :utf-16 (accessor type)
  `(lambda (seq start end)
     (declare (type ,type seq) (fixnum start end))
     (let* ((swap (when (> end start)
                    (case (,accessor seq start 2)
                      (#.+byte-order-mark-code+ (incf start 2) nil)
                      (#.+swapped-byte-order-mark-code+ (incf start 2) t)
                      (t #+babel::le t)))))
       (loop with count fixnum = 0
             with i fixnum = start
             while (<= i (- end 2)) do
             (let* ((code (if swap
                              (,accessor seq i 2 :re)
                              (,accessor seq i 2)))
                    (next-i (+ i (if (or (< code #xd800) (>= code #xdc00))
                                     2
                                     4))))
               (declare (type (unsigned-byte 16) code) (fixnum next-i))
               (if (> next-i end)
                   (progn
                     (decoding-error
                      (vector (,accessor seq i) (,accessor seq (1+ i)))
                      :utf-16 seq i nil 'end-of-input-in-character)
                     (return (values count i)))
                   (setq i next-i
                         count (1+ count))))
             finally (progn
                       (assert (= i end))
                       (return (values count i)))))))

(define-encoder :utf-16 (src-accessor src-type dest-accessor dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
           for i fixnum from start below end
           for code of-type code-point = (,src-accessor src i)
           for high-bits fixnum = (- code #x10000) do
           (cond ((< high-bits 0)
                  (setf (,dest-accessor dest di 2) code)
                  (incf di 2))
                 (t
                  (setf (,dest-accessor dest di 2)
                        (logior #xd800 (f-ash high-bits -10)))
                  (setf (,dest-accessor dest (+ di 2) 2)
                        (logior #xdc00 (f-logand high-bits #x3ff)))
                  (incf di 4)))
           finally (return (the fixnum (- d-start di))))))

(define-decoder :utf-16 (src-accessor src-type dest-accessor dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((swap (when (> end start)
                   (case (,src-accessor src start 2)
                     (#.+byte-order-mark-code+ (incf start 2) nil)
                     (#.+swapped-byte-order-mark-code+ (incf start 2) t)
                     (t #+babel::le t)))))
       (loop with i fixnum = start
             for di fixnum from d-start
             until (= i end) do
             (let ((u1 (if swap
                           (,src-accessor src i 2 :re)
                           (,src-accessor src i 2))))
               (declare (type (unsigned-byte 16) u1))
               (incf i 2)
               (setf (,dest-accessor dest di)
                     (cond
                       ((or (< u1 #xd800) (>= u1 #xe000)) ; 2 octets
                        u1)
                       ((< u1 #xdc00) ; 4 octets
                        (let ((u2 (if swap
                                      (,src-accessor src i 2 :re)
                                      (,src-accessor src i 2))))
                          (declare (type (unsigned-byte 16) u2))
                          (incf i 2)
                          (if (and (>= u2 #xdc00) (< u2 #xe000))
                              (utf-16-combine-surrogate-pairs u1 u2)
                              (decoding-error
                               (vector (,src-accessor src (- i 4))
                                       (,src-accessor src (- i 3))
                                       (,src-accessor src (- i 2))
                                       (,src-accessor src (- i 1)))
                               :utf-16 src i +repl+))))
                       (t
                        (decoding-error (vector (,src-accessor src (- i 2))
                                                (,src-accessor src (- i 1)))
                                        :utf-16 src i +repl+)))))
             finally (return (the fixnum (- di d-start)))))))

;;;; UTF-32

(defmacro utf32-octet-counter (accessor type)
  (declare (ignore accessor type))
  `(lambda (seq start end)
     (declare (ignore seq))
     ;; XXX: the result can be bigger than a fixnum and we don't want
     ;; that to happen.  Possible solution: signal a warning (hmm,
     ;; make that an actual error) and truncate.
     (* 4 (the fixnum (- end start)))))

(define-character-encoding :utf-32
   "A 32-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 32-bit word.  The
endianness of the encoded data is indicated by the endianness of
a byte-order-mark character (#\u+feff) prepended to the data; in
the absence of such a character on input, input data is assumed
to be in big-endian order.  Output is written in native byte
order with a leading byte-order mark."
  :max-units-per-char 1
  :code-unit-size 32
  :native-endianness t ; not necessarily true when decoding
  :literal-char-code-limit #x110000
  :use-bom #+babel::le :utf-32le #+babel::be :utf-32be
  :bom-encoding
  #+babel::be #(#x00 #x00 #xfe #xff)
  #+babel::le #(#xff #xfe #x00 #x00)
  :nul-encoding #(0 0 0 0))

(define-octet-counter :utf-32 (accessor type)
  `(utf32-octet-counter ,accessor ,type))

(define-code-point-counter :utf-32 (accessor type)
  `(lambda (seq start end)
     (declare (type ,type seq) (fixnum start end))
     (multiple-value-bind (count rem)
         (floor (- end start) 4)
       ;; check for incomplete last character
       (unless (zerop rem)
         (let ((vector (make-array 4 :fill-pointer 0)))
           (dotimes (i rem)
             (vector-push (,accessor seq (+ i (- end rem))) vector))
           (decoding-error vector :utf-32 seq (- end rem) nil
                           'end-of-input-in-character)
           (decf end rem)))
       ;; check for bom
       (when (and (not (zerop count))
                  (case (,accessor seq 0 4)
                    ((#.+byte-order-mark-code+
                      #.+swapped-byte-order-mark-code-32+) t)))
         (decf count))
       (values count end))))

(define-encoder :utf-32 (src-accessor src-type dest-accessor dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop for i fixnum from start below end
           for di fixnum from d-start by 4 do
           (setf (,dest-accessor dest di 4)
                 (,src-accessor src i))
           finally (return (the fixnum (- di d-start))))))

(define-decoder :utf-32 (src-accessor src-type dest-accessor dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (when (and (not (zerop (- end start)))
                (case (,src-accessor src 0 4)
                  ((#.+byte-order-mark-code+
                    #.+swapped-byte-order-mark-code-32+) t)))
       (incf start 4))
     (loop for i from start below end by 4
           for di from d-start do
           (setf (,dest-accessor dest di)
                 (,src-accessor src i 4))
           finally (return (the fixnum (- di d-start))))))
