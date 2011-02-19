(in-package #:tpd2.game)

(defstruct robot)
(defstruct (robot-bully (:include robot)))
(defstruct (robot-sensible (:include robot)))

(my-defun robot 'player-controller-name-to-ml ()
  (<span :class "robot" "Ralph"))

(my-defun robot-bully 'player-controller-name-to-ml ()
  (<span :class "robot" "Nelson"))

(my-defun robot-sensible 'player-controller-name-to-ml ()
  (<span :class "robot" "Martin"))

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

