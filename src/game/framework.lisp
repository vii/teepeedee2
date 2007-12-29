(in-package #:tpd2.game)

(defgeneric move-continuation (k game-state move-type controller player-state choices &rest args))
(defmethod move-continuation (k game-state move-type controller player-state choices &rest args)
  (funcall k (apply 'move game-state move-type controller player-state choices args)))

(defmacro with-game (&body body)
  `(with-call/cc
     ,@body))

(defgeneric move (game-state move-type controller player-state choices &rest args))
(defmethod move (game-state move-type controller player-state choices &rest args)
  (declare (ignore args))
  (random-choice choices))

(defun validate-choice (choices choice)
  (acond ((and (not choice) (member nil (choices-list choices)))
	  nil)
	 ((and choice (find choice (choices-list choices) :test 'equalp))
	  it)
	 (t
	  (error "Forbidden move ~A: choose from ~A" choice choices))))

(defmethod move (game-state move-type (stream stream) player-state choices &rest args)
  (format stream "Details  ~{~A ~}~&Game state ~A~&Your state ~A~&Your private state ~A~&Your choices for ~A are ~A:~&" args game-state 
	  (game-vars player-state) 
	  (secret-game-vars player-state)  move-type choices)
  (loop do
	(handler-case (return-from move (validate-choice choices (read-safely stream)))
	  (error (e) (format t "Sorry that move is not allowed; ~A~&" e)))))


(defgeneric choices-list (choice))
(defgeneric choices-list-form (first &rest rest))

(defmethod choices-list ((choices list))
  (apply 'choices-list-form (first choices) (rest choices)))

(defmethod choices-list ((choices (eql :boolean)))
  (list t nil))

(defmethod choices-list-form ((first (eql :integer)) &rest args)
  (destructuring-bind
	(min-inclusive max-exclusive)
      args
    (loop for i from min-inclusive below max-exclusive
	  collect i)))

(defmethod choices-list-form ((first (eql :one)) &rest args)
  args)

(defun random-choice (choices)
  (random-elt (choices-list choices)))

(eval-always
  (defgeneric game-vars (obj) (:method-combination append))
  (defgeneric secret-game-vars (obj) (:method-combination append))
  (defgeneric play (game)))

(defmacro defgameclass (name-and-options &rest slots)
  (let ((name (force-first name-and-options)))
    (flet ((slot-to-cons (slot)
	     `(cons 
	       ,(or (getf (rest (force-rest slot)) :documentation) (symbol-name (force-first slot)))
	       (,(intern (strcat name '- (force-first slot))) ,name))))
      `(eval-always 
	 (defmystruct ,name-and-options
	     ,@(mapcar (lambda(s)
			 (destructuring-bind (name &optional initform &rest args)
			     (force-list s)
			   `(,name ,initform
				   ,@(let ((n (copy-list args))) 
					  (remf n :secret)
					  (remf n :documentation)
					  n)))) slots))
	 (defmethod game-vars append ((,name ,name))
	   (list
	    ,@(loop for slot in slots 
		    unless (getf (rest (force-rest slot)) :secret) 
		    collect (slot-to-cons slot))))
	 (defmethod secret-game-vars append ((,name ,name))
	   (list
	    ,@(loop for slot in slots 
		    when (getf (rest (force-rest slot)) :secret) 
		    collect (slot-to-cons slot))))
	 (find-class ',name)))))

(defgameclass game
    players
  other-listeners)

(defgameclass player
    controller)

(my-defun game listeners ()
  (append (mapcar 'player-controller (my players)) (my other-listeners)))

(defgeneric inform (listener game-state message &rest args))
(defmethod inform (listener game-state message &rest args)
  (declare (ignore args)))

(defmethod inform ((stream stream) game-state message &rest args)
  (format stream "MESSAGE: ~A~&~{~A ~} (state ~A)~&" message args game-state))

(my-defun game announce (message &rest args)
  (loop for l in (my listeners)
	do (apply 'inform l me message args)))

(defgeneric player-controller-name (controller))
(defmethod player-controller-name (controller)
  controller)

(defmethod player-controller-name ((controller (eql *terminal-io*)))
  "Operator")

(my-defun player name ()
  (player-controller-name (my controller)))

(eval-always (defvar *games* (make-hash-table :test 'equalp)))
  
(defmacro defgame (name superclasses slots defplayer &rest options)
  (let ((friendly-name (or (getf options :documentation) (symbol-name name))))
    (flet ((defgameclass-form (name superclasses options slots)
	     `(defgameclass (,name 
			   ,@(mapcar (lambda(c) `(:include ,c)) 
				     superclasses)
			   ,@options)
		  ,@slots)))
    (destructuring-bind
	  (defplayer-sym df-superclasses df-slots &rest df-options)
	defplayer
      (assert (eq 'defplayer defplayer-sym))
      `(eval-always
	 (setf (gethash ,friendly-name *games*) (lambda(controllers)
						  (,(intern (strcat 'make- name)) :players
						    (mapcar (lambda(c) (,(intern (strcat 'make- name '-player)) :controller c)) controllers))))
	 ,(defgameclass-form (intern (strcat name '-player)) 
			     (or df-superclasses (list 'player))
			     df-options
			     df-slots)
	 ,(defgameclass-form name
			     (or superclasses (list 'game))
			     options
			     slots))))))

(defgame truc ()
  (stake ordered-winners)
  (defplayer ()
      ((cards nil :secret t)
       (wins 0)
       (stack 0)
       chosen-card
       (folded nil))))

(defmacro defrules (game func lambda-list &body body)
  `(eval-always
     (with-call/cc
       (my-defun ,game ,func ,lambda-list ,@body))
     #+never (my-defun ,game ,func ,lambda-list ,@body)))


(defrules game secret-move (type player choices &rest args)
  (let ((ret (call/cc (lambda(cc)
			(apply 'move-continuation cc me type (player-controller player) player choices 
			       args)
			'waiting-for-move-from-call/cc))))
    (validate-choice choices ret)))

(defrules game move (type player choices &rest args)
  (let ((ret (apply 'game-secret-move me type player choices args)))
    (my announce type :choice ret :player player)
    ret))

(defconstant +suits+ '(:clubs :hearts :spades :diamonds))
(defconstant +cards-per-suit+ 13)

(eval-always
  (defstruct card 
    (suit :clubs :type  #.`(member ,@+suits+))
    (value 0 :type (integer 0 #.+cards-per-suit+))))

(my-defun card value-string ()
  (case (my value)
    (0 "Ace")
    (10 "Jack")
    (11 "Queen")
    (12 "King")
    (t (string-capitalize (format nil "~R" (1+ (my value)))))))

(my-defun card name ()
  (format nil "~A of ~A"
	  (my value-string)
	  (string-capitalize (symbol-name (my suit)))))

(my-defun card 'object-to-ml ()
  (<span
    :class "card"
    (<span :class (symbol-name (my suit))
	   (my value-string)
	   (output-raw-ml
	    "&")
	   (case (my suit)
	     (:diamonds "diams")
	     (t (string-downcase (symbol-name (my suit)))))
	   ";")))

(eval-always
  (my-defun card number ()
    (+ (* (position (my suit) +suits+) +cards-per-suit+) (my value))))

(defun make-card-from-number (number)
  (multiple-value-bind
	(s-n v)
      (floor number +cards-per-suit+)
    (make-card :suit (elt +suits+ s-n) :value v)))

(defconstant +truc-ranking+ '(6 7 0 12 11 10 9 8))
(defconstant +truc-deck+ 
  (mapcar 'card-number 
	  (loop for s in +suits+ append (loop for i in +truc-ranking+ collect (make-card :suit s :value i)))))
(defconstant +truc-winning-stack+ 12)

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
	(my announce :new-cards)
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
	(my announce :winner :player winner)))))

(defun launch-game (game players)
  (play (funcall (gethash game *games*) players)))

(defmystruct move-state
    cc
    game-state
  move-type
  player-state
  choices
  args)

(defvar *pending-moves* nil)
(defvar *announcements* nil)

(eval-always
  (use-package '#:tpd2.ml.html))

(defmethod move-continuation (k game-state move-type (controller (eql :web)) player-state choices &rest args)
  (appendf *pending-moves* 
	   (list (make-move-state :cc k
			      :game-state game-state
			      :move-type move-type
			      :player-state player-state
			      :choices choices
			      :args args))))

(defmethod inform ((controller (eql :web)) game-state message &rest args)
  (push (format nil "MESSAGE: ~A ~{~A ~}~&" message (mapcar (lambda(a)(if (player-p a) (player-name a) a)) args))
	*announcements*))

(my-defun truc-player 'object-to-ml ()
  (<div :class "truc-player"
	(<h3 (my name))
	(<p (my stack) " points.")
	(cond 
	  ((my folded)
	   (<p :class "folded" "FOLDED"))
	  ((not (zerop (my wins)))
	   (<p :class "wins" (my wins) (format nil " win~P this round." (my wins)))))))

(my-defun truc 'object-to-ml ()
  (<div :class "truc"
	(loop for p in (my players)
	      do (output-object-to-ml p))
	(<h3 :class "stake" "Playing for "
	     (my stake) (format nil " point~P" (my stake)))
	(<div :class "table"
	      (loop for p in (my players)
		    when (and (not (its folded p)) (its chosen-card p))
		    do (<p (its name p) ": " (output-object-to-ml (make-card-from-number (its chosen-card p))))))))

(defpage "/game" (choice new-game)
  (when choice
    (awhen (pop *pending-moves*)
      (funcall (its cc it) (with-input-from-string (*standard-input* (force-string choice)) (read-safely)))))
  (when new-game
    (launch-game (force-string new-game) (list :web (make-robot-bully))))

  (<html
    (<head
      (<title "Game"))
    (<body
      (<form :action "/game" :method "GET"
	     (<p
	       (<select :name "new-game"
			(loop for g being the hash-keys of *games* do
			      (<option g)))
	       (<input :type :submit)))
      
      (when *announcements*
	(<div
	  (<h1 "Messages")
	  (<pre
	    (progn
	      (loop for a in (reverse *announcements*) do
		    (output-escaped-ml a))
	      (setf *announcements* nil)))))
      (when *pending-moves*
	(<div
	  (loop for m in *pending-moves*
		do 
		(output-object-to-ml (its game-state m))
		(<form :action "/game" :method "POST" 
		       (<p "Your cards are: " (loop for card in (its cards (its player-state m)) do
						       (output-escaped-ml " ")
						       (output-object-to-ml (make-card-from-number card))) ".")
		       (<p (its move-type m) (format nil " ~{~A ~}" (its args m)) " from " (format nil "~A" (its choices m) )
			   (<input :type 'text :name "choice" :value (first (choices-list (its choices m))))
			   (<input :type :submit)))))))))
		

(defvar *continuation*)
#+never (defmethod move-continuation (cc game-state move-type (stream stream) player-state choices &rest args)
  (format stream "Details  ~{~A ~}~&Game state ~A~&Your state ~A~&Your private state ~A~&Your choices for ~A are ~A:~&" args game-state 
	  (game-vars player-state) 
	  (secret-game-vars player-state)  move-type choices)
  (setf *continuation* (lambda()
			 (funcall cc 
				  (block move
				    (loop do
					  (handler-case (return-from move (validate-choice choices (read-safely stream)))
					    (error (e) (format t "Sorry that move is not allowed; ~A~&" e))))))))

  (values))


(defstruct robot)

(my-defun robot 'player-controller-name ()
  "RALPH")

(defstruct (robot-bully (:include robot)))

(my-defun robot-bully 'player-controller-name ()
  "NELSON")

(defmethod move ((game-state truc) (move-type (eql :select-card)) (controller robot) player-state choices &rest args)
  (declare(ignore args))
  (let ((played-card-values (remove-if (lambda(c)(eql -1 c)) (its chosen-card-values game-state)))
	(my-best-card (first (its cards player-state)))
	(my-worst-card (first (its cards player-state))))
    (labels ((val (c)
		  (its truc-value (make-card-from-number c)))
	     (card-better (a b)
	       (> (val a) (val b))))
      (loop for card in (its cards player-state)
	    when (card-better card my-best-card)
	    do (setf my-best-card card)
	    when (card-better my-worst-card card)
	    do (setf my-worst-card card))
      (cond ((>= (apply 'max played-card-values) (val my-best-card))
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


#+never
(progn
  (let ((socket (tpd2.io:make-con-listen :port 8889)))
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http::http-serve))

  (sb-thread:make-thread (lambda() (tpd2.io:event-loop)) :name "EVENT-LOOP"))