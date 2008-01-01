(in-package #:tpd2.http)

(defun percent-hexpair-decode (encoded)
  (match-replace-all 
   ("%" (word '(:string 2))) 
   (vector (byte-vector-parse-integer word 16))
   encoded))

(defun url-encoding-decode (encoded)
  (match-replace-all "+" (force-byte-vector " ") (percent-hexpair-decode encoded)))

(defun percent-hexpair-encode (plain)
  (match-replace-all 
   ((c '(:char-range (not (or (- #\A #\Z) (- #\a #\z) (- #\0 #\9) #\- #\_ )))))
   (labels ((digit (digit)
	      (if (>= digit 10)
		  (+ digit (- (char-code #\A) 10))
		  (+ digit (char-code #\0)))))
     (multiple-value-bind (hi lo)
	 (floor c 16)
       (vector (char-code #\%) (digit hi) (digit lo))))
   plain))
											      