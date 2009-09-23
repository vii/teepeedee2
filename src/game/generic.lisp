(in-package #:tpd2.game)

(defgeneric game-drop-player (game player))
(defgeneric play (game))
(defgeneric game-finished (game &rest args))
(defgeneric game-name (game))
(defgeneric move (controller player-state move-type choices &rest args))
(defmethod move (controller player-state move-type choices &rest args)
  (declare (ignore args controller player-state move-type))
  (random-choice choices))
(defgeneric move-continuation (k controller player-state move-type choices &rest args))
(defmethod move-continuation (k controller player-state move-type choices &rest args)
  (funcall k (apply 'move controller player-state move-type choices args)))
(defgeneric choices-list (choice))
(defgeneric choices-list-form (first &rest rest))

(defmethod choices-list ((choices list))
  (apply 'choices-list-form (first choices) (rest choices)))

(defmethod choices-list ((choices (eql :boolean)))
  (list t nil))

(defmethod choices-list-form ((first (eql :integer)) &rest args)
  (destructuring-bind
	(min-inclusive max-inclusive)
      args
    (loop for i from min-inclusive upto max-inclusive
	  collect i)))

(defmethod choices-list-form ((first (eql :one)) &rest args)
  args)
(defgeneric inform (listener game-state message &rest args))
(defmethod inform (listener game-state message &rest args)
  (declare (ignore args listener game-state message)))

(defgeneric player-controller-name-to-ml (controller))

(defgeneric player-controller-var (controller var)
  (:method (controller var)
    (declare (ignorable controller var))))
(defgeneric (setf player-controller-var) (new-value controller var)
  (:method (new-value controller var)
    (declare (ignorable controller var))
    new-value))

(defgeneric game-players-ready (game)
  (:method (game)
    (declare (ignorable game))))