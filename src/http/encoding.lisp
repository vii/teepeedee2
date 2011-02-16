(in-package #:tpd2.http)

(defun url-encoding-decode (encoded)
  (declare (type simple-byte-vector encoded)
           (optimize speed))
  (match-replace-all
      encoded
    ((progn "%" (val (unsigned-byte :length 2 :base 16)))
     (byte-vector val))
    ("+" " ")))

(defun percent-hexpair-encode (plain)
  (declare (type byte-vector plain))
  (match-replace-all
   plain
   ( (c (and (not (or (- #\A #\Z) (- #\a #\z) (- #\0 #\9) #\- #\_ )) (char)))
    (labels ((digit (digit)
               (if (>= digit 10)
                   (+ digit (- (char-code #\A) 10))
                   (+ digit (char-code #\0)))))
      (multiple-value-bind (hi lo)
          (floor (aref (the byte-vector c) 0) 16)
        (byte-vector (char-code #\%) (digit hi) (digit lo)))))))
