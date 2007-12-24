
(defpackage #:cl-cont-test
  (:nicknames #:cont-test)
  (:use :cl :cont :rtest)
  (:export #:test-cont))

(in-package :cont-test)

(defun test-cont ()
  "Call this function to run all unit tests defined in 'cont-test'
package."
  (do-tests))

(defun do-pending ()
  "An alias for 'continue-testing'."
  (continue-testing))

