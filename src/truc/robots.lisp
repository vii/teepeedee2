(in-package #:tpd2.game.truc)

(defmethod move ((controller robot) (player-state truc-player) (move-type (eql :select-card)) choices &rest args)
  (declare(ignore args choices))
  (let ((played-card-values (its chosen-card-values (its game player-state)))
	(my-best-card (first (its cards player-state)))
	(my-worst-card (first (its cards player-state))))
    (let ((highest-played-card (when played-card-values (reduce 'max played-card-values :initial-value 0)))
	  (board-beater))
      (labels ((val (c)
		 (card-number-truc-ranking c))
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


(define-constant +best-starts+ #(0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 0 2 2 0 2 2 2 2 0 2 2 2
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
  :documentation "Given the hand described by the index, 0 means play the highest card, 1 the middle card and 2 the lowest card first"
  :test 'equalp)


(define-constant +three-card-win-probabilities+ 
  #(0 1/1218 1/522 11/3654 5/1218 19/3654 23/3654 3/406 1/1218 79/3654 44/1827
    71/1827 122/1827 197/1827 296/1827 419/1827 1/522 44/1827 101/3654 82/1827
    5/63 8/63 49/261 478/1827 11/3654 71/1827 82/1827 143/1827 64/609 323/1827
    478/1827 73/203 5/1218 122/1827 5/63 64/609 340/1827 275/1218 91/261
    892/1827 19/3654 197/1827 8/63 323/1827 275/1218 95/261 1549/3654 373/609
    23/3654 296/1827 49/261 478/1827 91/261 1549/3654 1150/1827 2609/3654 3/406
    419/1827 478/1827 73/203 892/1827 373/609 2609/3654 1 1/1218 79/3654
    44/1827 71/1827 122/1827 197/1827 296/1827 419/1827 79/3654 11/261 19/203
    61/609 227/1827 101/609 137/609 19/63 44/1827 19/203 71/522 115/609 17/87
    7/29 88/261 121/261 71/1827 61/609 115/609 415/1827 527/1827 77/261 31/87
    298/609 122/1827 227/1827 17/87 527/1827 640/1827 107/261 761/1827 49/87
    197/1827 101/609 7/29 77/261 107/261 929/1827 149/261 132/203 296/1827
    137/609 88/261 31/87 761/1827 149/261 146/203 1441/1827 419/1827 19/63
    121/261 298/609 49/87 132/203 1441/1827 1 1/522 44/1827 101/3654 82/1827
    5/63 8/63 49/261 478/1827 44/1827 19/203 71/522 115/609 17/87 7/29 88/261
    121/261 101/3654 71/522 46/261 478/1827 502/1827 590/1827 106/261 958/1827
    82/1827 115/609 478/1827 55/174 752/1827 776/1827 880/1827 55/87 5/63 17/87
    502/1827 752/1827 844/1827 334/609 114/203 170/261 8/63 7/29 590/1827
    776/1827 334/609 1129/1827 20/29 428/609 49/261 88/261 106/261 880/1827
    114/203 20/29 482/609 1558/1827 478/1827 121/261 958/1827 55/87 170/261
    428/609 1558/1827 1 11/3654 71/1827 82/1827 143/1827 64/609 323/1827
    478/1827 73/203 71/1827 61/609 115/609 415/1827 527/1827 77/261 31/87
    298/609 82/1827 115/609 478/1827 55/174 752/1827 776/1827 880/1827 55/87
    143/1827 415/1827 55/174 671/1827 857/1827 893/1827 1025/1827 179/261
    64/609 527/1827 752/1827 857/1827 275/522 389/609 401/609 1375/1827
    323/1827 77/261 776/1827 893/1827 389/609 1265/1827 7/9 1457/1827 478/1827
    31/87 880/1827 1025/1827 401/609 7/9 1546/1827 1651/1827 73/203 298/609
    55/87 179/261 1375/1827 1457/1827 1651/1827 1 5/1218 122/1827 5/63 64/609
    340/1827 275/1218 91/261 892/1827 122/1827 227/1827 17/87 527/1827 640/1827
    107/261 761/1827 49/87 5/63 17/87 502/1827 752/1827 844/1827 334/609
    114/203 170/261 64/609 527/1827 752/1827 857/1827 275/522 389/609 401/609
    1375/1827 340/1827 640/1827 844/1827 275/522 1060/1827 1244/1827 1292/1827
    1468/1827 275/1218 107/261 334/609 389/609 1244/1827 893/1218 218/261
    1574/1827 91/261 761/1827 114/203 401/609 1292/1827 218/261 538/609
    1720/1827 892/1827 49/87 170/261 1375/1827 1468/1827 1574/1827 1720/1827 1
    19/3654 197/1827 8/63 323/1827 275/1218 95/261 1549/3654 373/609 197/1827
    101/609 7/29 77/261 107/261 929/1827 149/261 132/203 8/63 7/29 590/1827
    776/1827 334/609 1129/1827 20/29 428/609 323/1827 77/261 776/1827 893/1827
    389/609 1265/1827 7/9 1457/1827 275/1218 107/261 334/609 389/609 1244/1827
    893/1218 218/261 1574/1827 95/261 929/1827 1129/1827 1265/1827 893/1218
    475/609 25/29 545/609 1549/3654 149/261 20/29 7/9 218/261 25/29 550/609
    1765/1827 373/609 132/203 428/609 1457/1827 1574/1827 545/609 1765/1827 1
    23/3654 296/1827 49/261 478/1827 91/261 1549/3654 1150/1827 2609/3654
    296/1827 137/609 88/261 31/87 761/1827 149/261 146/203 1441/1827 49/261
    88/261 106/261 880/1827 114/203 20/29 482/609 1558/1827 478/1827 31/87
    880/1827 1025/1827 401/609 7/9 1546/1827 1651/1827 91/261 761/1827 114/203
    401/609 1292/1827 218/261 538/609 1720/1827 1549/3654 149/261 20/29 7/9
    218/261 25/29 550/609 1765/1827 1150/1827 146/203 482/609 1546/1827 538/609
    550/609 1702/1827 3575/3654 2609/3654 1441/1827 1558/1827 1651/1827
    1720/1827 1765/1827 3575/3654 1 3/406 419/1827 478/1827 73/203 892/1827
    373/609 2609/3654 1 419/1827 19/63 121/261 298/609 49/87 132/203 1441/1827
    1 478/1827 121/261 958/1827 55/87 170/261 428/609 1558/1827 1 73/203
    298/609 55/87 179/261 1375/1827 1457/1827 1651/1827 1 892/1827 49/87
    170/261 1375/1827 1468/1827 1574/1827 1720/1827 1 373/609 132/203 428/609
    1457/1827 1574/1827 545/609 1765/1827 1 2609/3654 1441/1827 1558/1827
    1651/1827 1720/1827 1765/1827 3575/3654 1 1 1 1 1 1 1 1 1)
  :test 'equalp)

(my-defun truc-player win-probability ()
  "Rough and ready win probability not taking into account who starts or anything much"
  (case (length (my cards))
    (3 (aref +three-card-win-probabilities+ (my hand-number)))
    (t
     (let ((expected-wins
	    (loop for c in (my cards)
		  sum (/ (card-number-truc-ranking c) (length +truc-ranking+)))))
       (min 1 (/ (+ expected-wins (my wins)) 2))))))

(my-defun truc-player hand-number ()
  (loop for i from 0 
	for card in (my cards)
	sum (* (expt 8 i) (card-number-truc-ranking card))))

(defmethod move ((controller robot-sensible) (player-state truc-player) (move-type (eql :select-card)) choices &rest args)
  (declare (ignore args choices))
  (let ((cards (its cards player-state)))
    (cond ((and (not (its chosen-card-values (its game player-state))) (= (length cards) 3))
	  (elt 
	   (sort (copy-list cards) '> :key 'card-number-truc-ranking)
	   (aref +best-starts+
		 (its hand-number player-state))))
	(t 
	 (call-next-method)))))

(defmethod move ((controller robot-sensible) (player-state truc-player) (move-type (eql :accept-new-stake)) choices &rest args)
  (declare (ignore choices))
  (let ((new-stake (getf args :new-stake)))
    (> (* new-stake (- 1 (its win-probability player-state))) (its stake (its game player-state)))))

(defmethod move ((controller robot-sensible) (player-state truc-player) (move-type (eql :select-new-stake)) choices &rest args)
  (declare(ignore args))
  (let ((max-other-stack (loop for player in (its players (its game player-state))
			       unless (eq player player-state) 
			       maximize (its stack player)))
	(least (reduce 'min (choices-list choices)))
	(wp (its win-probability player-state)))
    (let ((most (min (- +truc-winning-stack+ (its stack player-state)) (reduce 'max (choices-list choices)))))
      (cond ((>= (+ max-other-stack (its stake (its game player-state))) +truc-winning-stack+)
	     most)
	    ((= most least)
	     most)
	    ((> wp 0)
	     (min (max (floor (/ (its stake (its game player-state)) wp))  least) most))
	    (t 
	     least)))))

(defmethod move ((controller robot-bully) (player-state truc-player) (move-type (eql :select-new-stake)) choices &rest args)
  (declare(ignore args))
  (min (reduce 'max (choices-list choices))
       (- +truc-winning-stack+ (its stack player-state))))


(defmethod move ((controller robot-bully) (player-state truc-player) (move-type (eql :accept-new-stake)) choices &rest args)
  (declare(ignore args choices))
  t)



(defmethod move ((controller robot-bully) (player-state truc-player) (move-type (eql :reject-cards))  choices &rest args)
  (declare(ignore args choices))
  nil)
