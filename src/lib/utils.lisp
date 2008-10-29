(in-package #:tpd2.lib)


(defun-speedy byte-vector-cat (&rest args)
  (apply-byte-vector-cat args))

(defun-speedy apply-byte-vector-cat (args)
  (let ((vecs (mapcar (lambda(x)(force-byte-vector x)) args)))
    (let ((len (reduce '+ (mapcar 'length vecs))))
      (let ((ret (make-byte-vector len)) (i 0))
	(loop for v in vecs do
	      (replace ret v :start1 i)
	      (incf i (length v)))
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



(defun random-shuffle (sequence)
  (loop while (not (zerop (length sequence)))
	collect
	(let ((i (random (length sequence))))
	  (prog1
	      (elt sequence i) 
	    (setf sequence (remove-if (lambda(x) (declare (ignore x)) t) sequence :start i :count 1))))))

(declaim (inline random-elt))
(defun random-elt (sequence)
  (declare (optimize speed))
  (when sequence
    (elt sequence (random (length sequence)))))



(defun read-safely (&rest args)
  (let ((*read-eval* nil))
    (apply 'read args)))
(defun read-safely-from-string (string)
  (with-input-from-string (*standard-input* (force-string string)) (read-safely)))

(defun backtrace-description (err)
  (format nil "ERROR ~A:~&~A" (with-output-to-string (*standard-output*) (describe err)) 
	  (hunchentoot:get-backtrace err)))
