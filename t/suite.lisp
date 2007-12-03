(in-package #:teepeedee2.system)

(defpackage #:teepeedee2.test
  (:nicknames #:tpd2.test)
  (:use #:common-lisp #:tpd2 #:5am))

(in-package #:tpd2.test)

(def-suite :tpd2)

