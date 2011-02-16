(in-package #:tpd2.lib)

(declaim (ftype (function (t) simple-byte-vector) apply-byte-vector-cat))
(defun-speedy byte-vector-cat (&rest args)
  (apply-byte-vector-cat args))

(defun apply-byte-vector-cat (args)
  (let ((vecs (mapcar (lambda(x)(force-simple-byte-vector x)) args)))
    (let ((len (reduce '+ (mapcar (lambda(x)(length (the simple-byte-vector x))) vecs))))
      (let ((ret (make-byte-vector len)) (i 0))
        (loop for v in vecs do
              (locally
                  (declare (type simple-byte-vector ret v) (type (integer 0 #. most-positive-fixnum) i))
                (replace ret v :start1 i)
                (incf i (length v))))
        ret))))

#-ccl ; compacting gc makes this unreliable
(ignore-errors
  (let ((v (make-byte-vector 10)) q)
    (with-pointer-to-vector-data (p v)
      (setf q p))
    (with-pointer-to-vector-data (p0 v)
      (with-pointer-to-vector-data (p1 v)
        (when (and (cffi:pointer-eq p0 p1) (cffi:pointer-eq p0 q))
          (pushnew :tpd2-byte-vectors-do-not-move-arbitrarily *features*))))))

(defun max-nil-ok (&rest args)
  (let (one)
    (let ((result (loop for a in args when a do (setf one t) and maximizing a)))
      (when one result))))

(defun random-between (min max)
  (+ min (random (1+ (- max min)))))

(defun random-shuffle (sequence)
  (loop while (plusp (length sequence))
        collect
        (let ((i (random (length sequence))))
          (prog1
              (elt sequence i)
            (setf sequence (remove-if (lambda(x) (declare (ignore x)) t) sequence :start i :count 1))))))

(declaim (inline random-elt))
(defun random-elt (sequence)
  (let ((len (length sequence)))
    (unless (zerop len)
      (elt sequence (random len)))))


(defun read-safely (&rest args)
  (let ((*read-eval* nil))
    (apply 'read args)))
(defun read-safely-from-string (string)
  (with-input-from-string (*standard-input* (force-string string)) (read-safely)))

(defun report-error (err &key (stream *error-output*))
  (format stream "~&ERROR ~A, ~A:~%~A~&"
          (ignore-errors (princ-to-string err))
          (with-output-to-string (*standard-output*) (describe err))
          (trivial-backtrace:backtrace-string)))

(defun backtrace-description (err)
  (report-error err :stream nil))

(defmacro with-ignored-errors ((&optional (report-function ''backtrace-description) ) &body body)
  (with-unique-names (safe func)
    `(block ,safe
       (flet ((,func (e)
                (,report-function e)
                (return-from ,safe (values nil e))))
         (declare (dynamic-extent #',func))
         (handler-bind
             ((error #',func))
           ,@body)))))

(defun safely-load-system (&key (system 'teepeedee2))
  (let* ((out (make-string-output-stream))
         (in (make-string-input-stream ""))
         (both (make-two-way-stream in out)))
    (let ((*standard-output* out)
          (*error-output* out)
          (*trace-output* out)
          (*standard-input* in)
          (*query-io* both)
          (*debug-io* both)
          (*terminal-io* both))
      (multiple-value-call #'values
        (ignore-errors
          (handler-bind
              ((error
                (lambda(c)
                  (report-error c)
                 (loop for restart in '(asdf:accept continue)
                       for found = (find-restart restart)
                       do (when found
                            (format *error-output* "~&Using restart ~A~%" restart)
                            (invoke-restart found))))))
            (values (asdf:oos 'asdf:load-op system) nil)))
        (get-output-stream-string out)))))

