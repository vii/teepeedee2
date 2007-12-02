;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

(defpackage :it.bese.arnesi.test
  (:use :common-lisp
        :it.bese.arnesi
        :it.bese.FiveAM))

(unless (5am:get-test :it.bese)
  (5am:def-suite :it.bese))

(5am:def-suite :it.bese.arnesi :in :it.bese)
