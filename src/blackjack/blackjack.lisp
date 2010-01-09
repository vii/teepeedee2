(in-package #:tpd2.game.blackjack)

(defparameter +one-deck+ 
   (loop for s in +suits+ append (loop for i below +cards-per-suit+ collect (make-card :suit s :value i))))

(defparameter +decks-per-shoe+ 2)
(defparameter +reshuffle-proportion+ 1/6)

(defstruct hand
  cards
  (stake 0))

(defgame blackjack (coin-game)
  (shoe)
  (defplayer ()
      ((hands nil)))
  (:game-name "Blackjack")
  (:advertised nil))

(my-defun blackjack reshuffle ()
  (my announce :shuffle)
  (setf (my shoe) (random-shuffle (loop repeat +decks-per-shoe+ append +one-deck+))))

(my-defun blackjack take-card ()
  (pop (my shoe)))

(my-defun blackjack take-cards (&optional (num 2))
  (loop repeat num collect (my take-card)))

(defun cards-value (cards)
  (loop for card in cards
	for c = (card-value card)
	summing (min 10 (1+ c))))

(my-defun hand value ()
  (cards-value (my cards)))

(my-defun hand bust ()
  (< 21
     (my value)))

(defun cards-best-value (cards)
  (let ((other 0) (aces 0))
    (loop for card in cards
	  for c = (card-value card)
	  do
	  (if (zerop c)
	      (incf aces)
	      (incf other (min 10 (1+ c)))))
    (loop for ac in
	  (let ((min aces))
	    (list* min (loop repeat aces collect (incf min 9))))
	  for val = (+ ac other)
	  when (>= 21 val)
	  maximizing val)))

(my-defun hand blackjack ()
  (= 21 (cards-best-value (my cards))))

(my-defun blackjack 'play ()
  (with-its-type (p blackjack-player)
    (with-game
      (loop 

      (when (> (* +reshuffle-proportion+ +decks-per-shoe+ +cards-per-suit+ (length +suits+)) (length (my shoe)))
	(my reshuffle)
	(my new-state))

      (with-join-spawn/cc ()
       (loop for p in (my players)
	     do
	     (let-current-values (p) 
	       (spawn/cc ()
			 (setf (its hands p) (list (make-hand :stake (my move :select-new-stake p `(:integer 0 ,(max 1 (its coins p)))))))))))

      (loop for p in (my players)
	    do (setf (its hands p) (delete-if (lambda (h) (zerop (hand-stake h))) (its hands p))))

      (loop for p in (my players)
	    do (loop for h in (its hands p) 
		     do (setf (hand-cards h) (my take-cards 2))))

      (loop for p in (my players)
	    do (loop for h in (its hands p) do
		     (loop 
			   while (my move :hit p :boolean)
			   do (push (my take-card) (hand-cards h))
			   (when (hand-blackjack h)
			     (my announce :blackjack :player p :hand h)
			     (return))
			   (when (hand-bust h)
			     (my announce :bust :player p :hand h)
			     (return)))))

      (let (hand)
	(loop while (> 17 (cards-value hand))
	      do (let ((c (my take-card)))
		   (debug-assert c (hand me (my shoe)))
		   (push c hand)
		   (my announce :dealer :card c)))
	(let ((val (cards-best-value hand)))
	  (loop for p in (my players)
		do (loop for h in (its hands p)
			 for pval = (cards-best-value (hand-cards h))
			 do (its give-coins p
				(cond ((or (zerop pval) (> val pval))
				       (- (hand-stake h)))
				      ((= pval val)
				       0)
				      ((= 21 pval)
				       (* 3/2 (hand-stake h)))
				      (t
				       (hand-stake h))))))))))))

(my-defun hand 'object-to-ml ()
  (<div :class "blackjack-hand"
	(loop for c in (my cards)
	      do (output-object-to-ml c))
	(<p :class "stake" (my stake))
	(when (my cards)
	  (<p :class "value" "Value " (cards-best-value (my cards)))
	  (cond ((my bust)
		 (<p :class "bust" "Bust."))
		((my blackjack)
		 (<p :class "blackjack" "Blackjack!"))))))

(my-defun blackjack-player 'object-to-ml ()
  (<div :class "blackjack-player"
	(call-next-method)
	(loop for h in (my hands)
	      do (output-object-to-ml h))))
