(in-package #:tpd2.game)

(defstruct robot)

(my-defun robot 'player-controller-name ()
  "[Ralph]")

(defstruct (robot-bully (:include robot)))

(my-defun robot-bully 'player-controller-name ()
  "[Nelson]")

(defvar *bots* (list (make-robot) (make-robot-bully)))

(defmethod inform ((stream stream) game-state message &rest args)
  (format stream "MESSAGE: ~A~&~{~A ~} (state ~A)~&" message args game-state))
(defmethod player-controller-name ((controller stream))
  "Operator")

(defgeneric player-controller-message (controller sender message))
(defmethod player-controller-message (controller sender message))
