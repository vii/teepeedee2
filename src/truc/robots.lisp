(in-package #:tpd2.game.truc)

(defmethod move ((controller robot) (player-state truc-player) (move-type (eql :select-card)) choices &rest args)
  (declare(ignore args))
  (let ((played-card-values (its chosen-card-values (its game player-state)))
	(my-best-card (first (its cards player-state)))
	(my-worst-card (first (its cards player-state))))
    (let ((highest-played-card (when played-card-values (reduce 'max played-card-values :initial-value 0)))
	  (board-beater))
      (labels ((val (c)
		 (its truc-ranking (make-card-from-number c)))
	       (card-better (a b)
		 (> (val a) (val b))))
	(loop for card in (its cards player-state)
	      when (card-better card my-best-card)
	      do (setf my-best-card card)
	      when (card-better my-worst-card card)
	      do (setf my-worst-card card)
	      when (and highest-played-card (> highest-played-card (val card))
			(or (not board-beater) (card-better board-beater card)))
	      do (setf board-beater card))
	(cond ((and highest-played-card (>= highest-played-card (val my-best-card)))
	       my-worst-card)
	      (board-beater
	       board-beater)
	      (t my-best-card))))))


(defconstant +best-starts+ #(0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 0 2 2 0 2 2 2 2 0 2 2 2
  0 0 2 2 0 2 2 2 0 0 0 2 0 2 2 2 2 0 0 0 0 2 2 2 2 2 0 0 0 2 2 2 2 2 2 2
  2 0 1 1 1 1 1 1 2 1 2 1 1 2 2 2 2 1 1 0 1 1 2 2 2 1 1 1 0 1 1 2 2 1 2 1
  1 0 1 2 2 1 2 2 1 1 0 1 2 1 2 2 2 2 1 0 0 2 2 2 2 2 2 2 2 1 2 1 1 2 2 2
  2 2 0 1 1 1 1 1 2 1 1 2 1 1 2 2 2 1 1 1 0 1 1 2 2 2 1 1 1 0 1 1 2 2 1 2
  1 1 0 1 2 2 1 2 2 1 1 0 0 2 2 0 2 2 2 2 2 1 1 0 1 1 2 2 2 1 1 2 1 1 2 2
  0 0 2 0 1 1 1 1 2 1 1 1 2 1 1 2 2 1 1 1 1 0 1 1 2 2 2 1 1 1 0 1 2 2 2 1
  2 1 1 0 0 2 2 2 0 0 2 2 2 1 1 1 0 1 1 2 2 1 1 1 0 1 1 2 2 1 1 1 2 1 1 2
  0 0 0 2 0 1 1 1 0 1 1 1 1 2 1 1 2 1 1 1 1 1 0 1 2 2 2 2 1 1 1 0 0 2 2 2
  0 0 0 2 2 1 2 1 1 0 1 2 2 2 1 1 1 0 1 1 2 1 1 1 1 0 1 1 0 1 1 1 1 2 1 1
  0 0 0 0 2 0 1 1 0 1 1 1 1 1 0 1 2 2 1 1 1 1 1 0 0 2 2 2 2 0 0 0 2 1 2 2
  1 1 0 1 2 2 1 2 1 1 0 1 2 2 2 1 1 1 0 1 2 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1
  0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 2 2 2 2 2 0 0 2 1 2 2 2 2 1 0 2 2 1 2
  2 1 1 0 2 2 2 1 2 1 1 0 2 2 2 2 1 1 1 0 2 2 1 1 1 1 1 0 0 1 1 1 1 1 0 0
  0 0 0 0 0 0 0 0)
  "Given the hand described by the index, 0 means play the highest card, 1 the middle card and 2 the lowest card first")

(defmethod move ((controller robot-sensible) (player-state truc-player) (move-type (eql :select-card)) choices &rest args)
  (declare (ignore args))
  (cond ((not (its chosen-card-values (its game player-state)))
	 (let ((cards (its cards player-state)))
	   (elt (sort (copy-list cards) '> :key (lambda(c) (its truc-ranking (make-card-from-number c))))
		(aref +best-starts+
		      (loop for i from 0 
			    for card in cards
			    sum (* (expt 8 i) (its truc-ranking (make-card-from-number card))))))))
	(t (call-next-method))))

(defmethod move ((controller robot-bully) (player-state truc-player) (move-type (eql :accept-new-stake)) choices &rest args)
  (declare(ignore args))
  t)

(defmethod move ((controller robot-bully) (player-state truc-player) (move-type (eql :select-new-stake)) choices &rest args)
  (declare(ignore args))
  (max (reduce 'min (choices-list choices))
       (- +truc-winning-stack+ (its stack player-state))))

(defmethod move ((controller robot-bully) (player-state truc-player) (move-type (eql :reject-cards))  choices &rest args)
  (declare(ignore args))
  nil)
