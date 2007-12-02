;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Debugging Utilties

;;;; (These were far more useful in the pre-slime days.)

(defmacro ppm1 (form)
  "(pprint (macroexpand-1 ',form)).

NB: C-RET is even shorter."
  `(pprint (macroexpand-1 ',form)))

(defmacro ppm (form)
  `(pprint (macroexpand ',form)))

;;;; A portable flexible APROPOS implementation

(defun apropos-list* (string &key (fbound nil fbound-supplied-p)
                                  (bound nil bound-supplied-p)
                                  (package nil package-supplied-p)
                                  (distance 0 distance-supplied-p))
  (let ((symbols '()))
    (do-all-symbols (sym)
      (block collect-symbol
        (when fbound-supplied-p
          (when (xor fbound (fboundp sym))
            (return-from collect-symbol)))
        (when bound-supplied-p
          (when (xor bound (boundp sym))
            (return-from collect-symbol)))
       (when package-supplied-p
         (unless (eql package (symbol-package sym))
           (return-from collect-symbol)))
       (when distance-supplied-p
         (unless (and
                  (<= (abs (- (length (symbol-name sym)) 
                              (length string)))
                      distance)
                  (<= (levenshtein-distance string (symbol-name sym))
                      distance))
           (return-from collect-symbol)))
       (when (not distance-supplied-p)
         ;; regular string= test
         (unless (search string (symbol-name sym) :test #'char-equal)
           (return-from collect-symbol)))
       ;; all the checks we wanted to perform passed.
       (push sym symbols)))
    symbols))

(defun apropos* (&rest apropos-args)
  (flet ((princ-length (sym)
           (if (keywordp sym)
               (+ 1 (length (symbol-name sym)))
               (+ (length (package-name (symbol-package sym)))
                  1
                  (length (symbol-name sym))))))
    (let* ((syms (apply #'apropos-list* apropos-args))
           (longest (apply #'max (mapcar #'princ-length syms))))
      (dolist (sym syms)
        (if (keywordp sym)
            (progn
              (princ ":" *debug-io*)
              (princ (symbol-name sym) *debug-io*))
            (progn
              (princ (package-name (symbol-package sym)) *debug-io*)
              (princ ":" *debug-io*)
              (princ (symbol-name sym) *debug-io*)))
        (princ (make-string (- longest (princ-length sym))
                            :initial-element #\Space)
               *debug-io*)
        (when (fboundp sym)
          (princ " [FUNC] " *debug-io*))
        (when (boundp sym)
          (princ " [VAR] " *debug-io*))
        (terpri *debug-io*))))
  (values))

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
