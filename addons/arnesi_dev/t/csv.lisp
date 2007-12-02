;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.csv :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.csv)

(test csv.1
  (is (equal '("1" "2" "3")
	     (arnesi:parse-csv-string "1,2,3")))
  (is (equal '("1" "2" "3")
	     (arnesi:parse-csv-string "1;2;3" :separator #\;)))
  (is (equal '("1" "2;" "3")
	     (arnesi:parse-csv-string "1;'2;';3" :separator #\; :quote #\'))))

(test csv.2
  ;; this corresponds to the quoting used in princ-csv
  (is (equal '("1" "2'" "3")
	     (arnesi:parse-csv-string "1;'2''';3" :separator #\; :quote #\')))
  (is (equal '("1" "2'" "3")
	     (arnesi:parse-csv-string "1;'2''';'3'" :separator #\; :quote #\'))))

