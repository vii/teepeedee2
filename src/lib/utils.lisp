(in-package #:tpd2.lib)

(def-if-unbound defun-consistent utf8-decode (vec)
  (map 'string 'code-char vec))

(def-if-unbound defun-consistent utf8-encode (string)
  (map 'byte-vector 'char-code string))

(defun-consistent byte-vector-to-string (vec)
  (utf8-decode vec))


(defun-consistent force-string (val)
  (declare (optimize speed))
  (let ((str
	 (the string
	   (typecase val
	     (null "")
	     (symbol (symbol-name val))
	     (byte-vector (byte-vector-to-string val))
	     (string val)
	     (t  (let ((*print-pretty* nil)) (princ-to-string val)))))))
    (etypecase str
      (simple-string str)
      (string (replace (make-string (length str)) (the (and string (not simple-string)) str))))))

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
       (let ((ret (make-byte-vector (length val))))
	 (replace ret (the (and byte-vector (not simple-byte-vector)) val))
	 ret)))))

(declaim (ftype (function (t) simple-byte-vector) force-simple-byte-vector-consistent-internal))

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




(defun random-shuffle (sequence)
  (loop while (not (zerop (length sequence)))
	collect
	(let ((i (random (length sequence))))
	  (prog1
	      (elt sequence i) 
	    (setf sequence (remove-if (lambda(x) (declare (ignore x)) t) sequence :start i :count 1))))))

(defun random-elt (sequence)
  (when sequence
    (elt sequence (random (length sequence)))))
(defun read-safely (&rest args)
  (let ((*read-eval* nil))
    (apply 'read args)))
(defun read-safely-from-string (string)
  (with-input-from-string (*standard-input* (force-string string)) (read-safely)))
