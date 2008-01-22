(in-package #:tpd2.game)

(defstruct robot)

(my-defun robot 'player-controller-name ()
  "<Ralph>")

(defstruct (robot-bully (:include robot)))

(my-defun robot-bully 'player-controller-name ()
  "<Nelson>")

(defstruct (robot-sensible (:include robot)))

(my-defun robot-sensible 'player-controller-name ()
  "<Martin>")


(defvar *bots* (list (make-robot) (make-robot-bully) (make-robot-sensible)))

(defmethod inform ((stream stream) game-state message &rest args)
  (format stream "MESSAGE: ~A~&~{~A ~} (state ~A)~&" message args game-state))

(defmethod move ((stream stream) player-state move-type choices &rest args)
  (format stream "Details  ~{~A ~}~&Game state ~A~&Your state ~A~&Your choices for ~A are ~A:~&" args (player-game player-state)
	  player-state
	  move-type choices)
  (loop do
	(handler-case (return-from move (validate-choice choices (read-safely stream)))
	  (error (e) (format t "Sorry that move is not allowed; ~A~&" e)))))

(defmethod player-controller-name ((controller stream))
  "Operator")
(defgeneric player-controller-message (controller sender message))
(defmethod player-controller-message (controller sender message))
