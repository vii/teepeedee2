(in-package #:tpd2.game)

(defstruct robot)

(my-defun robot 'player-controller-name ()
  "RALPH")

(defstruct (robot-bully (:include robot)))

(my-defun robot-bully 'player-controller-name ()
  "NELSON")


(defmethod inform ((stream stream) game-state message &rest args)
  (format stream "MESSAGE: ~A~&~{~A ~} (state ~A)~&" message args game-state))
(defmethod player-controller-name ((controller stream))
  "Operator")
