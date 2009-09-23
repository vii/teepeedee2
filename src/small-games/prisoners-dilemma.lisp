(in-package #:tpd2.game.prisoners-dilemma)

(defvar *max-share* 10)
(defvar *min-share* 1)
(defvar *max-grab* 25)
(defvar *min-grab* 1)
(defvar *max-penalty* 5)
(defvar *min-penalty* 0)
(defvar *max-big-penalty* 25)
(defvar *min-big-penalty* 0)

(defgame prisoners-dilemma (coin-game)
  ((share (random-between *min-share* *max-share*))
   (grab (random-between *min-grab* *max-grab*))
   (penalty (random-between *min-penalty* *max-penalty*))
   (big-penalty (random-between *min-big-penalty* *max-big-penalty*)))
  (defplayer ()
      ((agree)))
  (:game-name "Prisoner's Dilemma")
  (:game-description
   (with-ml-output
     "If the players cooperate, they all receive an equal number of
     coins. However, if one player decides to betray the others, then
     he or she receives more coins, and the others are fined. But if
     multiple players cheat, then all players are fined." (<br)

     "This version of the Prisoner's Dilemma varies the punishments and rewards. Read more at "
     (<a :href "http://en.wikipedia.org/wiki/Prisoner's_dilemma" "Wikipedia") "."
     )))

(my-defun prisoners-dilemma 'object-to-ml ()
  (flet ((coins (c) (format nil "~R coin~:P" c)))
    (with-ml-output
      (call-next-method)
      (<h3 "Reward: " (my share) ", temptation: " (my grab) ", sucker's punishment: " (my penalty) ", penalty: " (my big-penalty) ".")
      (<p "If all players co-operate, they each receive " (coins (my share)) ". "
	  "If one player does not co-operate, then he or she can take " (coins (my grab)) " and the other players lose " (coins (my penalty)) ". "
	  "But if more than one player decides not to co-operate, they all lose " (coins (my big-penalty)) "."))))

(my-defun prisoners-dilemma 'play ()
  (with-its-type (p prisoners-dilemma-player)
    (with-game
      (setf (my grab) (max (my grab) (1+ (my share)))
	    (my big-penalty) (max (my penalty) (my big-penalty)))
      (my new-state)
      (with-join-spawn/cc ()
	(loop for p in (my players)
	      do (let-current-values (p) 
		   (spawn/cc () 
			     (setf (its agree p) (my secret-move :cooperate p :boolean))))))
      (let ((traitors
	     (loop for p in (my players)
		   for agree = (its agree p)
		   count (when (not agree)
			   (my announce :betrayal :player p)
			   t))))
	(cond ((zerop traitors)
	       (loop for p in (my players)
		     do (its give-coins p (my share)))
	       (my finished :result :sharing))
	      ((= 1 traitors)
	       (let (traitor)
		(loop for p in (my players)
		      do 
		      (cond
		        ((its agree p) 
			 (its give-coins p (- (my penalty))))
			(t
			 (setf traitor p)
			 (its give-coins p (my grab)))))
	       (my finished :winner traitor)))
	      (t
	       (loop for p in (my players)
		     do (its give-coins p (- (my big-penalty))))
	       (my finished :result :penalty)))))))

    