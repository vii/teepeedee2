;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :it.bese.arnesi.log :in :it.bese.arnesi))

(in-suite :it.bese.arnesi.log)

#|
(defparameter a-handler (make-instance 'collecting-log-handler))

(deflogger log-a ()
  :appender a-handler
  :level +dribble+)

(deflogger log-b (log-a))

(deflogger log-c (log-a))

(deflogger log-d (log-c))

(test log1
  (log-a.dribble "FOO")
  (is (string= "FOO" (car (slot-value (car (appenders (get-logger 'log-a))) 'messages))))

  (setf (log.level (get-logger 'log-a)) +warn+)
  (is (= +warn+ (log.level (get-logger 'log-d))))

  (setf (log.level (get-logger 'log-d)) +dribble+)
  (is (= +dribble+ (log.level (get-logger 'log-d))))
  (is (= +warn+ (log.level (get-logger 'log-b))))
  (is (= +warn+ (log.level (get-logger 'log-c))))

  (is (enabled-p (get-logger 'log-d) +warn+))
  (is (enabled-p (get-logger 'log-a) +warn+))
  (is (not (enabled-p (get-logger 'log-a) +dribble+))))

|#