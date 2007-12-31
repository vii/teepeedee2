(in-package #:tpd2.game)

#.(progn (use-package '#:tpd2.ml.html) nil)

(defvar *web-state*)
(setf *web-state* nil)

(defmystruct move-state
    cc
  game-state
  move-type
  player-state
  choices
  args)

(defmystruct queued-choice
    move-type
  choice)

(defstruct web-state
  (name "The player you are controlling")
  (announcements (tpd2.io:with-sendbuf ()))
  (waiting-for-input nil)
  (queued-choices nil)
  game-state)

(my-defun web-state 'player-controller-name ()
  (my name))

(my-defun web-state 'inform :before (game-state message &rest args)
  (declare (ignore args))
  (setf (my game-state) game-state))

(my-defun web-state add-announcement (a)
  (tpd2.io:sendbuf-add (my announcements) a))

(my-defun web-state 'inform (game-state (message (eql :select-card)) &rest args)
  (my add-announcement (<p (player-name (getf args :player)) " played " (output-object-to-ml (make-card-from-number (getf args :choice))) ".")))

(my-defun web-state 'inform (game-state (message (eql :reject-cards)) &rest args)
  (my add-announcement (<p (player-name (getf args :player)) 
			   (if (getf args :choice) " wants to change cards."
			       " is satisfied with the cards."))))

(my-defun web-state 'inform (game-state (message (eql :accept-new-stake)) &rest args)
  (my add-announcement (<p (player-name (getf args :player)) 
			   (if (getf args :choice) " saw the raise."
			       " folded."))))

(my-defun web-state 'inform (game-state (message (eql :select-new-stake)) &rest args)
  (let ((choice (getf args :choice)))
    (unless (eql choice (its stake game-state))
      (my add-announcement (<p (player-name (getf args :player)) " raised to " choice " chips.")))))

(my-defun web-state 'inform (game-state (message (eql :winner)) &rest args)
  (my add-announcement 
      (<p (player-name (getf args :player)) " won"
	  (awhen (getf args :chips)
	    (with-ml-output " " it " chips"))
	  ".")))

(my-defun web-state 'inform (game-state (message (eql :game-over)) &rest args)
  (my add-announcement (<h2 (player-name (getf args :player)) " won the game.")))

(my-defun web-state 'inform (game-state (message (eql :new-state)) &rest args)
  (setf (my queued-choices) nil))

(my-defun web-state 'inform (game-state message &rest args)
  (my add-announcement 
      (<p (format nil "~A ~{~A ~}"  message (mapcar (lambda(a)(if (player-p a) (player-name a) a)) args)))))

(defmethod move-continuation (k game-state move-type (controller web-state) player-state choices &rest args)
  (its add-move-state controller
       (make-move-state :cc k
			:game-state game-state
			:move-type move-type
			:player-state player-state
			:choices choices
			:args args)))

(my-defun web-state add-move-state (move-state)
  (appendf (my waiting-for-input)
	   (list move-state))
  (my try-to-move))

(my-defun web-state queue-choice (move-type choice)
  (appendf (my queued-choices)
	   (list (make-queued-choice :move-type move-type :choice choice)))
  (my try-to-move))

(defun make-link (page &rest args)
  (apply 'byte-vector-cat page 
	 "?bogus-value=bogus-value"
	 (loop for (param val) on args by #'cddr
	       collect "&"
	       collect param 
	       collect "="
	       collect (tpd2.http::percent-hexpair-encode val))))

(my-defun web-state try-to-move ()
  (loop for waiting in (my waiting-for-input) do
	(loop for qc in (my queued-choices)
	      do (when (eql (its move-type qc) (its move-type waiting))
		   (deletef qc (my queued-choices))
		   (deletef waiting (my waiting-for-input))
		   (funcall (its cc waiting) (its choice qc))
		   (return-from web-state-try-to-move)))))

(defpage "/game" (choice move-type new-game)
  (when new-game
    (setf *web-state* (make-web-state))
    (launch-game new-game (list *web-state* (make-robot-bully))))
  (when (and choice move-type *web-state*)
    (let ((web-state *web-state*))
      (web-state-queue-choice web-state (read-safely-from-string move-type)
			      (read-safely-from-string choice))))

  (<html
    (<head
      (<title "mopoko.com")
      (css-html-style 
	(<body :font-family "georgia, serif")
	((<h1 <h2 <h3 <h4 <h5 <h6) :letter-spacing "0.03em" :font-weight "lighter")
	(<h1 :font-size "400%" :text-align "right")))
    (<body
      (<h1 "mopoko" (<span :style (css-attrib :color "rgb(188,188,188)" :font-style "italic") ".com"))
      (<form :action "/game" :method "GET"
	     (<p
	       (<select :name "new-game"
			(loop for g being the hash-keys of *games* do
			      (<option g)))
	       (<input :type :submit)))
      (when *web-state*
	(output-object-to-ml *web-state*)))))

(defun keyword-to-friendly-string (keyword)
  (string-capitalize (string-downcase (force-string (match-replace-all "-" " " keyword))) :end 1))

(my-defun move-state 'object-to-ml ()
  (<form :action "/game" :method "POST" 
	 (let ((friendly-move-type (keyword-to-friendly-string (my move-type))))
	   (cond ((eq :boolean (my choices))
		  (<p 
		    (loop for (keyword value) on (my args) by #'cddr
			  do (with-ml-output "The " (string-downcase (keyword-to-friendly-string keyword)) " is " value ". "))

		    friendly-move-type
		    "? "

		    (<A :href (make-link "/game" :choice 't :move-type (format nil "~W" (my move-type))) "Yes")
		    " "
		    (<A :href (make-link "/game" :choice "NIL" :move-type (format nil "~W" (my move-type))) "No")))
		 ((eql :select-card (my move-type))
		(<p friendly-move-type "."))
		 (t
		  (<p (my move-type) (format nil " ~{~A ~}" (my args)) " from " (format nil "~A" (my choices) )
		      (<input :type :text :name "choice" :value (first (choices-list (my choices))))
		      (<input :type :submit)))))
	 (<p 		    
	   (<input :type :hidden :name "move-type" :value (format nil "~W" (my move-type))))))


(my-defun web-state 'object-to-ml ()
  (<div :class "game-state"
	(<h2 "Messages")
	(<div
	  (output-raw-ml
	   (my announcements))
	  (without-ml-output (setf (my announcements) (tpd2.io:with-sendbuf ()))))
	(when (my waiting-for-input)
	  (<div :class "moves"
	    (loop for m in (my waiting-for-input)
		  do 
		  (output-object-to-ml (its game-state m))
		  (<p "Your cards are: " (loop for card in (its cards (its player-state m)) do
					       (output-escaped-ml " ")
					       (<A :href (make-link "/game" :choice card :move-type ":select-card")
						   (output-object-to-ml (make-card-from-number card)))) ".")
		  
		  (output-object-to-ml m))))))

