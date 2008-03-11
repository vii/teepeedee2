(in-package #:tpd2.lib)

(def-if-unbound defun-consistent utf8-decode (vec)
  (map 'string 'code-char vec))

(def-if-unbound defun-consistent utf8-encode (string)
  (map 'byte-vector 'char-code string))

(defun-consistent byte-vector-to-simple-byte-vector (val)
  (declare (optimize speed (safety 0)))
  (declare (type (and byte-vector (not simple-byte-vector)) val))
  (let ((ret (make-byte-vector (length val))))
    (replace ret val)
    ret))

(declaim (ftype (function ((and byte-vector (not simple-byte-vector))) simple-byte-vector) byte-vector-to-simple-byte-vector-consistent-internal))

(defun-consistent force-string (val)
  (declare (optimize speed (safety 0)))
  (let ((str
	 (the string
	   (typecase val
	     (null "")
	     (symbol (symbol-name val))
	     (string val)
	     (simple-byte-vector (utf8-decode val))
	     (byte-vector (utf8-decode (byte-vector-to-simple-byte-vector val)))
	     (t  (let ((*print-pretty* nil)) (princ-to-string val)))))))
    (etypecase str
      (simple-string str)
      (string 
       (locally 
	   (declare (type (and string (not simple-string)) str))
	 (replace (make-string (length str)) str))))))

(declaim (ftype (function (t) simple-string) force-string-consistent-internal))


(defun-consistent force-byte-vector (val)
  (declare (optimize speed (safety 0)))
  (typecase val
    (null #.(make-byte-vector 0))
    (simple-string (utf8-encode val))
    (string (utf8-encode val))
    (character (utf8-encode (string val)))
    (byte-vector val)
    (sequence (map 'byte-vector 'identity val))
    (t (utf8-encode (force-string val)))))

(declaim (ftype (function (t) byte-vector) force-byte-vector-consistent-internal))

(defun-consistent force-simple-byte-vector (val)
  (declare (optimize speed (safety 0)))
  (let ((val (force-byte-vector val)))
    (etypecase val
      (simple-byte-vector val)
      (byte-vector 
       (byte-vector-to-simple-byte-vector val)))))

(declaim (ftype (function (t) simple-byte-vector) force-simple-byte-vector-consistent-internal))

(defun-consistent byte-vector-to-string (vec)
  (utf8-decode (force-simple-byte-vector vec)))

(defun byte-vector-cat (&rest args)
  (declare (optimize speed))
  (let ((vecs (mapcar (lambda(x)(force-byte-vector x)) args)))
    (let ((len (reduce '+ (mapcar 'length vecs))))
      (let ((ret (make-byte-vector len)) (i 0))
	(loop for v in vecs do
	      (replace ret v :start1 i)
	      (incf i (length v)))
	ret))))
(declaim (inline byte-vector-cat))


(let ((v (make-byte-vector 10)) q)
  (with-pointer-to-vector-data (p v)
    (setf q p))
  (with-pointer-to-vector-data (p0 v)
    (with-pointer-to-vector-data (p1 v)
      (when (and (cffi:pointer-eq p0 p1) (cffi:pointer-eq p0 q))
	(pushnew :tpd2-byte-vectors-do-not-move-arbitrarily *features*)))))



(defun random-shuffle (sequence)
  (loop while (not (zerop (length sequence)))
	collect
	(let ((i (random (length sequence))))
	  (prog1
	      (elt sequence i) 
	    (setf sequence (remove-if (lambda(x) (declare (ignore x)) t) sequence :start i :count 1))))))

(defun random-elt (sequence)
  (declare (optimize speed))
  (when sequence
    (elt sequence (random (length sequence)))))

(declaim (inline random-elt))

(defun read-safely (&rest args)
  (let ((*read-eval* nil))
    (apply 'read args)))
(defun read-safely-from-string (string)
  (with-input-from-string (*standard-input* (force-string string)) (read-safely)))
