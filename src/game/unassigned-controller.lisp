(in-package #:tpd2.game)

(defstruct move-state
  cc
  move-type
  player-state
  choices
  args)

(my-defun move-state game-state ()
  (player-game (my player-state)))

(defstruct unassigned-controller
  move-states
  game
  timeout
  game-generator)

(my-defun unassigned-controller 'player-controller-name-to-ml ()
;  (strcat "Unassigned (" (timeout-remaining (my timeout)) " seconds left)")
  (<span :class "unassigned-player" 
	 (html-action-link "Unassigned" 
	   (my assign-robot))))

(my-defun unassigned-controller player-state ()
  (find me (game-players (my game)) :key 'player-controller))

(my-defun unassigned-controller del ()
  (deletef me (game-generator-unassigned-controllers-waiting (my game-generator))))

(my-defun unassigned-controller assign (other)
  (when (my game)
    (setf (player-controller (my player-state)) other)
    (my del)
    (timeout-cancel (my timeout))
    (game-announce (my game) :new-player other)
    (setf (my game) nil)
    (loop for i in (reverse (my move-states)) do
	  (move-state-continue i other))))

(my-defun unassigned-controller assign-robot ()
   (my assign (random-elt *bots*)))

(my-defun move-state continue (controller)
   (apply 'move-continuation (my cc) controller (my player-state) (my move-type) (my choices) (my args)))

(defmethod move-continuation (k (controller unassigned-controller) player-state move-type choices &rest args)
  (push (make-move-state :cc k :move-type move-type :player-state player-state :choices choices :args args)
	(unassigned-controller-move-states controller)))

(my-defun game-generator join-or-start (controller)
  (acond 
   ((pop (my unassigned-controllers-waiting))
    (let ((game (unassigned-controller-game it)))
      (unassigned-controller-assign it controller)
      game))
   (t
    (let ((uc (make-unassigned-controller :game-generator me)))
      (let ((game (funcall (my make-game) (list uc controller))))
	(setf (unassigned-controller-game uc) game)

	(setf (unassigned-controller-timeout uc) 
	      (make-timeout :delay 5 
			    :func (lambda()(unassigned-controller-assign-robot uc))))
	(appendf (my unassigned-controllers-waiting) (list uc))
	(play game)
	game)))))


(my-defun unassigned-controller 'inform (game-state (message (eql :game-over)) &rest args)
  (my del))
