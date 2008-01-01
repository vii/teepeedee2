(in-package #:tpd2.game)

(defconstant +truc-ranking+ '(6 7 0 12 11 10 9 8))
(defconstant +truc-deck+ 
  (mapcar 'card-number 
	  (loop for s in +suits+ append (loop for i in +truc-ranking+ collect (make-card :suit s :value i)))))
(defconstant +truc-winning-stack+ 12)

(defgame truc ()
  (stake ordered-winners)
  (defplayer ()
      ((cards nil :secret t)
       (wins 0)
       (stack 0)
       chosen-card
       (folded nil))))


(my-defun card truc-ranking ()
  (position (my value) (reverse +truc-ranking+)))  

(my-defun truc 'play ()
  (with-game
    (loop until (loop for p in (my players) thereis 
		      (when (<= +truc-winning-stack+ (its stack p)) 
			(my announce :game-over :player p)
			t))
	  do (my play-rubber))))

(my-defun truc-player chosen-card-value ()
  (if (or (my folded) (not (my chosen-card)))
      -1
      (its truc-ranking (make-card-from-number (my chosen-card)))))

(my-defun truc chosen-card-values ()
  (mapcar (lambda(p) (its chosen-card-value p)) (my players)))  

(defrules truc determine-round-winner ()
  (let ((winning-card 
	 (apply 'max 
	       (my chosen-card-values))))
    (let ((winners (loop for p in (my players)
				    when (and (eql winning-card (its chosen-card-value p)) (not (its folded p)))
			 collect p)))
      (when (= 1 (length winners))
	(let ((winner (first winners)))
	  (push winner (my ordered-winners))
	  (incf (its wins winner))
	  (my announce :winner :player winner)
	  (deletef winner (my players))
	  (push winner (my players)))))))


(defrules truc deal ()
  (setf (my stake) 1)
  (setf (my ordered-winners) nil)
  (loop do
	(let ((shuffle (random-shuffle +truc-deck+)))
	  (loop for player in (my players) do
		(setf (its wins player) 0)
		(setf (its folded player) nil)
		(setf (its cards player) (subseq shuffle 0 3))
		(setf shuffle (subseq shuffle 3))))
	(setf (my players) (random-shuffle (my players)))
	(my announce :new-state)
	while
	(loop for player in (reverse (my players))
	      always (my move :reject-cards player :boolean))))

(defrules truc play-rubber ()
  (my deal)
  (block rounds
    (flet ((play-round ()
	     (loop for p in (my players)
		   do (setf (its chosen-card p) nil))
	     (loop for player in (my players)
		   unless (its folded player)
			  do
		   (when (> +truc-winning-stack+ (+ (its stack player) (my stake)))
			    (let ((new-stake 
				   (my move :select-new-stake player `(:integer ,(my stake) ,(1+ +truc-winning-stack+)))))
			      (when (> new-stake (my stake))
				(loop for p in (my players)
				      do (unless 
					     (or
					      (eql p player)
					      (its folded p)
					      (>= (+ (my stake) (its stack p)) +truc-winning-stack+)
					      (my move :accept-new-stake p :boolean :new-stake new-stake))
					   
					   (setf (its folded p) t)
					   (let ((active-players (filter (lambda(p)(not (its folded p))) (my players)))) 
					     (when (>= 1 (length active-players))
					       (return-from rounds)))))
				(setf (my stake) new-stake))))
		   (setf (its chosen-card player) 
			 (let ((card (my move :select-card player `(:one ,@(its cards player)))))
			   (deletef card (its cards player) :count 1)
			   card)))
	     (my determine-round-winner)))

	     (loop for round from 3 downto 1
		   until (let ((wins (mapcar (lambda(p)(its wins p)) (my players))))
			   (> (- (apply 'max wins) (apply 'min wins)) round))
		   do (play-round))))
    (my determine-rubber-winner))

(defrules truc determine-rubber-winner ()
  (let* ((active-players (filter (lambda(p)(not (its folded p))) (my players)))
	 (top-wins (apply 'max (mapcar (lambda(p)(its wins p)) active-players)))
	 (winners (remove-if-not (lambda(p) (eql (its wins p) top-wins)) active-players)))
    (let ((winner (first (if (> (length winners) 1)
			     (remove-if-not (lambda(p) (member p winners)) (my ordered-winners))
			     winners))))
      (unless winner
	(my announce :draw))
	       
      (when winner
	(incf (its stack winner) (my stake))
	(my announce :winner :chips (my stake) :player winner)))))


(my-defun truc-player 'object-to-ml ()
  (<div :class "truc-player"
	(output-raw-ml (call-next-method))
	(<p (my stack) (format nil " point~P." (my stack)))
	(cond 
	  ((my folded)
	   (<p :class "folded" "FOLDED"))
	  ((not (zerop (my wins)))
	   (<p :class "wins" (my wins) (format nil " win~P this round." (my wins)))))))

(my-defun truc 'object-to-ml ()
  (<div :class "truc"
	(<div :class "players"
	      (loop for p in (my players)
		    do (output-object-to-ml p)))
	(<h2 :class "stake" "Playing for "
	     (my stake) (format nil " point~P" (my stake)))
	(<div :class "table"
	      (loop for p in (my players)
		    when (and (not (its folded p)) (its chosen-card p))
		    do (<p (its name p) ": " (output-object-to-ml (make-card-from-number (its chosen-card p))))))))


(defmethod move ((game-state truc) (move-type (eql :select-card)) (controller robot) player-state choices &rest args)
  (declare(ignore args))
  (let ((played-card-values (remove-if (lambda(c)(eql -1 c)) (its chosen-card-values game-state)))
	(my-best-card (first (its cards player-state)))
	(my-worst-card (first (its cards player-state))))
    (labels ((val (c)
		  (its truc-ranking (make-card-from-number c)))
	     (card-better (a b)
	       (> (val a) (val b))))
      (loop for card in (its cards player-state)
	    when (card-better card my-best-card)
	    do (setf my-best-card card)
	    when (card-better my-worst-card card)
	    do (setf my-worst-card card))
      (cond ((and played-card-values (>= (apply 'max played-card-values) (val my-best-card)))
	     my-worst-card)
	    (t my-best-card)))))

(defmethod move ((game-state truc) (move-type (eql :accept-new-stake)) (controller robot-bully) player-state choices &rest args)
  (declare(ignore args))
  t)
(defmethod move ((game-state truc) (move-type (eql :select-new-stake)) (controller robot-bully) player-state choices &rest args)
  (declare(ignore args))
  (max (apply 'min (choices-list choices))
       (- +truc-winning-stack+ (its stack player-state))))

(defmethod move ((game-state truc) (move-type (eql :reject-cards)) (controller robot-bully) player-state choices &rest args)
  (declare(ignore args))
  nil)
