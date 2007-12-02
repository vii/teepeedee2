;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Programmatic interface to CL:TIME

(defclass timing-info ()
  ((real-time :accessor real-time :initarg :real-time
              :initform :not-available
              :documentation "Real time (also known as wall time)
              consumed. Expressed in milliseconds.")
   (user-time :accessor user-time :initarg :user-time
              :initform :not-available
              :documentation "User time. Expressed in milliseconds.")
   (system-time :accessor system-time :initarg :system-time
                :initform :not-available
                :documentation "System time. Expressed in milliseconds.")
   (gc-time :accessor gc-time :initarg :gc-time
            :initform :not-available
            :documentation "GC time. Expressed in milliseconds.")
   (page-faults :accessor page-faults :initarg :page-faults
                :initform :not-available
                :documentation "Number of page faults.")
   (bytes-consed :accessor bytes-consed :initarg :bytes-consed
                 :initform :not-available
                 :documentation "Number of bytes allocated."))
  (:documentation "Specificer for collect-timing info.

Every slot is either a number (with the exact meanining depending
on the slot) or the keyword :not-available in the case the lisp
doesn't provide this information."))

(defun pprint-milliseconds (milliseconds &optional stream)
  (cond
    ((< milliseconds 1000)
     (format stream "~D ms" milliseconds))
    ((= milliseconds 1000)
     (format stream "1.00 second"))
    ((< milliseconds (* 60 1000))
     (format stream "~,2F seconds" (/ milliseconds 1000)))
    ((= milliseconds (* 60 1000))
     (format stream "1.00 minute"))
    (t
     (format stream "~,2F minutes" (/ milliseconds (* 60 1000))))))

(defun pprint-bytes (num-bytes &optional stream)
  "Writes NUM-BYTES to stream, rounds num-bytes and appends a
suffix depending on the size of num-bytes."
  (cond
    ((< num-bytes (expt 2 10))
     (format stream "~D B" num-bytes))
    ((< num-bytes (expt 2 20))
     (format stream "~,2F KiB" (/ num-bytes (expt 2 10))))
    ((< num-bytes (expt 2 30))
     (format stream "~,2F MiB" (/ num-bytes (expt 2 20))))
    ((< num-bytes (expt 2 40))
     (format stream "~,2F GiB" (/ num-bytes (expt 2 30))))
    (t
     (format stream "~,2F TiB" (/ num-bytes (expt 2 40))))))

(defmethod print-object ((info timing-info) stream)
  (print-unreadable-object (info stream :type t :identity t)
    (format stream "~A/~A"
            (pprint-milliseconds (real-time info))
            (pprint-bytes (bytes-consed info)))))

(defun collect-timing (lambda)
  "Executes LAMBDA and returns a timing-info object specifying
  how long execution took and how much memory was used.

NB: Not all implementations provide all information. See the
various %collect-timing definitions for details."
  (%collect-timing lambda))

#+sbcl
(defun %collect-timing (fun)
  (declare (type function fun))
  "Implementation of collect-timing for SBCL.

This code is a cut 'n paste from sbcl/src/code/time.lisp. It uses
internal functions, all bets off."
  (let (old-run-utime
        new-run-utime
        old-run-stime
        new-run-stime
        old-real-time
        new-real-time
        old-page-faults
        new-page-faults
        real-time-overhead
        run-utime-overhead
        run-stime-overhead
        page-faults-overhead
        old-bytes-consed
        new-bytes-consed
        cons-overhead)
    ;; Calculate the overhead...
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    ;; Do it a second time to make sure everything is faulted in.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    (multiple-value-setq
        (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (sb-impl::time-get-sys-info))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time (get-internal-real-time))
    (setq old-real-time (get-internal-real-time))
    (setq new-real-time (get-internal-real-time))
    (setq real-time-overhead (- new-real-time old-real-time))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    (setq old-real-time (get-internal-real-time))
    (let ((start-gc-run-time sb-impl::*gc-run-time*))
      (progn
        ;; Execute the form and return its values.
        (funcall fun)
        (multiple-value-setq
            (new-run-utime new-run-stime new-page-faults new-bytes-consed)
          (sb-impl::time-get-sys-info))
        (setq new-real-time (- (get-internal-real-time) real-time-overhead))
        (let ((gc-run-time (max (- sb-impl::*gc-run-time* start-gc-run-time) 0)))
          (make-instance 'timing-info
                         :real-time (max (- new-real-time old-real-time) 0.0)
                         :user-time (max (/ (- new-run-utime old-run-utime) 1000.0) 0.0)
                         :system-time (max (/ (- new-run-stime old-run-stime) 1000.0) 0.0)
                         :gc-time (float gc-run-time)
                         :page-faults (max (- new-page-faults old-page-faults) 0)
                         :bytes-consed (max (- new-bytes-consed old-bytes-consed) 0)))))))

#+openmcl
(defun %collect-timing (lambda)
  "Implementation of collect-timing for OpenMCL.

We only report the MAJOR-PAGE-FAULTS, the number of
MINOR-PAGE-FAULTS is ignored."
  (let ((ccl:*report-time-function* #'list))
    (destructuring-bind (&key elapsed-time user-time system-time
                              gc-time bytes-allocated major-page-faults
                              &allow-other-keys)
        (time (funcall lambda))
      (make-instance 'timing-info
                     :real-time elapsed-time
                     :user-time user-time
                     :system-time system-time
                     :gc-time gc-time
                     :bytes-consed bytes-allocated
                     :page-faults major-page-faults))))

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
