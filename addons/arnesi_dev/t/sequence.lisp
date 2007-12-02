;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.sequence :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.sequence)

(test levenshtein-distance 
  (is (= 4 (levenshtein-distance "aaaa" "")))
  (is (= 4 (levenshtein-distance "" "aaaa")))
  (is (= 0 (levenshtein-distance "" "")))
  (is (= 0 (levenshtein-distance "a" "a")))
  (is (= 2 (levenshtein-distance "aa" "cc")))
  (is (= 1 (levenshtein-distance "a" "aa")))
  (is (= 1 (levenshtein-distance "ab" "aa")))
  (is (= 1 (levenshtein-distance "test" "tent"))))


