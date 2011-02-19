(in-package #:tpd2.lib)

(defmacro with-pointer-to-vector-data ((ptr lisp-vector) &body body)
  (check-symbols ptr)
  (once-only (lisp-vector)
    (with-unique-names (tmp real-vector offset cffi-ptr)
      `(let ((,tmp))
         (multiple-value-bind
               (,real-vector ,offset)
             (array-displacement ,lisp-vector)
           (when ,real-vector
             (setf ,lisp-vector ,real-vector))
           (cffi:with-pointer-to-vector-data (,cffi-ptr ,lisp-vector)
             (let ((,ptr (cffi:inc-pointer ,cffi-ptr ,offset)))
               (setf ,tmp (multiple-value-list (locally ,@body)))))
           (values-list ,tmp))))))

(defun-speedy concatenate-simple-byte-vectors (args)
  (let ((len 0))
    (declare (type fixnum len))
    (loop for x in args do
          (incf len (the fixnum (length (the simple-byte-vector x)))))
    (let ((ret (make-byte-vector len)) (i 0))
      (declare (type fixnum i))
      (loop for x in args do
            (loop for c across (the simple-byte-vector x) do
                  (setf (aref ret i) c)
                  (incf i)))
      ret)))


(defun-speedy byte-vector (&rest args)
  (declare (dynamic-extent args))
  (let ((ret (make-byte-vector (length args))))
    (loop for i from 0
          for arg in args
          do (setf (aref ret i) arg))
    ret))

(define-constant +byte-to-digit-table+
  (make-array 256 :element-type '(integer -1 36)
              :initial-contents (loop for i from 0 below 256
                                      collect
                                      (labels ((c (x) (char-code x))
                                             (in-range (a b x offset)
                                               (let ((l (min (c a) (c b)))
                                                     (m (max (c a) (c b))))
                                                 (when
                                                     (and (>= x l)
                                                          (>= m x))
                                                   (+ (- x l) offset)))))
                                        (or (in-range #\a #\z i 10)
                                            (in-range #\A #\Z i 10)
                                            (in-range #\0 #\9 i 0)
                                            -1))))
  :test 'equalp)

(declaim-defun-consistent-ftype byte-to-digit ((unsigned-byte 8)) (integer -1 36))
(defun-consistent byte-to-digit (byte)
  (declare (type (unsigned-byte 8) byte))
  (aref +byte-to-digit-table+ byte))



(defun byte-vector-parse-integer (string &optional (base 10))
  (declare (optimize speed))
  (declare (type byte-vector string))
  (let ((i 0) (val 0) (sign 1))
    (flet ((cur ()
             (aref string i))
           (eat ()
             (incf i)))
      (declare (ftype (function () (unsigned-byte 8)) cur))
      (when (= (char-code #\-) (cur))
        (setf sign -1)
        (eat))
      (loop while (> (length string) i) do
            (setf val (+ (byte-to-digit (cur)) (* val base)))
            (eat))
      (* sign val))))


(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) byte-to-ascii-upper))
(defun-speedy byte-to-ascii-upper (x)
  (declare (type (unsigned-byte 8) x))
  (if (>= #.(char-code #\z) x #.(char-code #\a))
      (+ #.(- (char-code #\A) (char-code #\a)) x)
      x))

(defun-speedy eql-fold-ascii-case (a b)
  (declare (type (unsigned-byte 8) a b))
  (= (byte-to-ascii-upper a) (byte-to-ascii-upper b)))


(defun-speedy byte-vector=-fold-ascii-case (a b)
  (declare (type simple-byte-vector a b))
  (and (= (length a) (length b))
       (loop for i from 0 below (length a)
             always (eql-fold-ascii-case (aref a i) (aref b i)))))

(defun-speedy unsafe-length-byte-vector=-fold-ascii-case (a b)
  (declare (type simple-byte-vector a b)
           (optimize speed (safety 0)))
  (loop for i from 0 below (length a)
        always (= (aref b i) (byte-to-ascii-upper (aref a i)))))


(defmacro case-match-fold-ascii-case (keyform &rest clauses)
  ;;; could be improved not to duplicate the clauses code and to handle non-constant forms much better
  (flet ((give-up ()
           (return-from case-match-fold-ascii-case (generate-case-key keyform :test 'byte-vector=-fold-ascii-case :transform 'force-byte-vector :clauses clauses))))
    (let ((table (make-hash-table :test #'equalp)) otherwise)
      (loop for (val . body) in clauses
            for vals = (force-list val)
           do
            (unless (every #'constantp vals)
              (give-up))
            (loop for v in vals
                  for bv = (force-simple-byte-vector (map 'byte-vector 'byte-to-ascii-upper
                                                          (force-byte-vector (eval v))))
                 do
                  (case v
                    ((t otherwise)
                     (assert (not otherwise))
                     (setf otherwise body))
                    (t
                    (push (cons bv body) (gethash (length bv) table))))))
     (with-unique-names (key)
       `(let ((,key (force-byte-vector ,keyform)))
          (case (length ,key)
            ,@(loop for len being the hash-keys of table using (hash-value vb)
                    do (when otherwise (give-up))
                    collect
                    `(,len
                      ,(generate-case-key key
                                          :test 'unsafe-length-byte-vector=-fold-ascii-case
                                          :clauses vb)))
            ,@(when otherwise
                    `((otherwise ,otherwise)))))))))

(defun copy-byte-vector (a)
  (let ((b (make-byte-vector (length a))))
    (replace b a)
    b))
