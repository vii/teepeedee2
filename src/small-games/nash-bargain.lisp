(in-package #:tpd2.game.nash-bargain)

(defvar *max-pot* 20)
(defvar *min-pot* 1)
(defvar *max-penalty* 5)
(defvar *min-penalty* 0)

(defun random-between (min max)
  (+ min (random (1+ (- max min)))))

(defgame nash-bargain ()
  ((pot (random-between *min-pot* *max-pot*))
   (penalty (random-between *min-penalty* *max-penalty*)))
  (defplayer ()
      ((coins)
       (demand 0)))
  (:game-name "Nash bargaining game"))

(defun max-nil-ok (&rest args)
  (let (one)
    (let ((result (loop for a in args when a do (setf one t) and maximizing a)))
      (when one result))))

(my-defun nash-bargain finished :before (&rest args)
	  (declare (ignorable args))
	  (loop for p in (my players)
		do 
		(setf (player-controller-var p 'coins) (its coins p))))

(my-defun nash-bargain 'play ()
  (with-game
    (my new-state)
    (loop for p in (my players)
	  do (setf (its coins p) 
		   (setf (player-controller-var p 'coins)
			 (max-nil-ok (1+ (random *max-penalty*)) (player-controller-var p 'coins)))))
    (loop for p in (my players)
	  do (setf (its demand p) (my secret-move :select-demand p `(:integer 0 ,(my pot)))))
    (let ((total-demand 
	   (loop for p in (my players) 
		 for demand = (its demand p) 
		 do
		 (my announce :demand :player p :amount demand)
		 summing demand)))
      (flet ((give-coins (p c)
	       (incf (its coins p) c)
	       (my announce :profit :player p :amount c)
	       (unless (plusp (its coins p))
		 (my announce :bankrupt :player p))
	       ))
       (cond ((>= (my pot) total-demand)
	      (loop for p in (my players) do
		    (give-coins p (its demand p)))
	      (my finished :result :sharing))
	     (t
	      (loop for p in (my players) do
		    (give-coins p (- (my penalty))))
	      (my finished :result :penalty)))))))

(my-defun nash-bargain 'object-to-ml ()
  (flet ((coins (c) (format nil "~R coin~:P" c)))
    (with-ml-output
      (call-next-method)
      (<h3 "Pot: " (my pot) ", penalty: " (my penalty) ".")
      (<p "The pot has " (coins (my pot)) "; you can demand zero or more of them. "
	  "If the total demanded by the players is less than or equal to " (coins (my pot)) ", then each player receives his or her demand. "
	      "Otherwise, if the players are too greedy, they lose " (coins (my penalty)) ". ")
      (<p
	"Your real demand is secret, but you can talk to the other player.")
      (<p "Read more at "
	  (<a :href "http://en.wikipedia.org/wiki/Nash_bargaining_game" "wikipedia") "."))))

(my-defun nash-bargain 'object-to-ml :around ()
  (<div :class "nash-bargain"
	(call-next-method)
	(let ((coins (webapp-frame-var 'coins)))
	  (cond ((not coins) (values))
		((minusp coins)
		 (<h3 :class "bankrupt" "You are bankrupt."))
		(t
		 (<h3 (format nil "You have ~D coin~:P." coins)))))))

(my-defun nash-bargain drop-player :before (p)
	  (unless (my game-over)
	    (decf (its coins p) (my penalty))))

(my-defun nash-bargain-player 'object-to-ml ()
  (<div :class "nash-bargain-player"
	(call-next-method)
	(let ((coins (or (my coins) (my 'player-controller-var 'coins))))
	  (when coins
	    (<p (format nil "~D coin~:P" coins))))))

(my-defun nash-bargain title-ml ()
  (<div :class "nash-bargain-title"
	(call-next-method)))