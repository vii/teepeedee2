(in-package #:tpd2.game)

(defstruct queued-choice
  move-type
  choice)

(defmyclass (web-state (:include simple-channel))
    frame
  (announcements nil)
  (waiting-for-input nil)
  (queued-choices nil)
  game-state)

(my-defun web-state resigned ()
  (not (loop for p in (game-players (my game-state)) thereis (eql me (player-controller p)))))

(my-defun web-state 'inform :before (game-state message &rest args)
	  (declare (ignore args))
	  (setf (my game-state) game-state))

(my-defun web-state add-announcement (a)
  (appendf (my announcements) (list a))
  (my notify))

(my-defun web-state 'inform (game-state (message (eql :talk)) &rest args)
  (let ((sender (getf args :sender)) (msg (getf args :text)))
    (my add-announcement (<p :class "game-talk-message" (player-controller-name-to-ml sender) ": " (<Q msg)))))

(my-defun web-state 'inform (game-state (message (eql :new-player)) &rest args)
  (my add-announcement (<p :class "game-message" (player-controller-name-to-ml (first args)) " has joined the game.")))

(my-defun web-state 'inform (game-state (message (eql :resigned)) &rest args)
  (my add-announcement (<p :class "game-message" (player-controller-name-to-ml (first args)) " has resigned.")))

(my-defun web-state 'inform (game-state (message (eql :select-card)) &rest args)
  (my add-announcement (<p :class "game-message" (player-controller-name-to-ml (player-controller (getf args :player))) " played " (output-object-to-ml (make-card-from-number (getf args :choice))) ".")))

(my-defun web-state 'inform (game-state (message (eql :reject-cards)) &rest args)
  (my add-announcement (<p :class "game-message" (player-controller-name-to-ml (player-controller (getf args :player)))
			   (if (getf args :choice) " wants to change cards."
			       " is satisfied with the cards."))))

(my-defun web-state 'inform (game-state (message (eql :accept-new-stake)) &rest args)
  (my add-announcement (<p :class "game-message" (player-controller-name-to-ml (player-controller (getf args :player)))
			   (if (getf args :choice) " saw the raise."
			       " folded."))))

(my-defun web-state 'inform (game-state (message (eql :select-new-stake)) &rest args)
  (let ((choice (getf args :choice)))
    (unless (eql choice (its stake game-state))
      (my add-announcement (<p :class "game-message" (player-controller-name-to-ml (player-controller (getf args :player))) " raised to " choice " chips.")))))

(my-defun web-state 'inform (game-state (message (eql :winner)) &rest args)
  (my add-announcement 
      (<p :class "game-message" (player-controller-name-to-ml (player-controller (getf args :player))) " won"
	  (awhen (getf args :chips)
	    (with-ml-output " " it " chips"))
	  ".")))

(my-defun web-state 'inform (game-state (message (eql :game-over)) &rest args)
  (my add-announcement (<h2 :class "game-message" (player-controller-name-to-ml (player-controller (getf args :player))) " won the game.")))

(my-defun web-state 'inform (game-state (message (eql :new-state)) &rest args)
  (declare (ignore args))
  (my add-announcement (<p :class "game-message" "New game."))
  (setf (my queued-choices) nil))

(my-defun web-state 'inform (game-state message &rest args)
  (my add-announcement 
      (<p :class "game-message"
	  message
	  " "
	  (output-object-to-ml args))))

(defmethod move-continuation (k (controller web-state) player-state move-type choices &rest args)
  (its add-move-state controller
       (make-move-state :cc k
			:move-type move-type
			:player-state player-state
			:choices choices
			:args args)))


(defmethod move-continuation (k (controller web-state) 
			      player-state 
			      (move-type (eql :ready-to-play)) choices &rest args)
  (funcall k t))

(my-defun web-state add-move-state (move-state)
  (appendf (my waiting-for-input)
	   (list move-state))
  (my try-to-move)
  (my notify))

(my-defun move-state queue-choice (choice)
  (web-state-queue-choice (player-controller (my player-state)) (my move-type) choice))

(my-defun web-state queue-choice (move-type choice)
  (appendf (my queued-choices)
	   (list (make-queued-choice :move-type move-type :choice choice)))
  (my try-to-move)
  (values))

(my-defun web-state try-to-move ()
  (loop for waiting in (my waiting-for-input) do
	(loop for qc in (my queued-choices)
	      do (when (eql (its move-type qc) (its move-type waiting))
		   (deletef qc (my queued-choices))
		   (unless (eq 'invalid-choice (validate-choice (its choices waiting) (its choice qc)))
		     (deletef waiting (my waiting-for-input))
		     (funcall (its cc waiting) (its choice qc))
		     (my notify)
		     (return-from web-state-try-to-move))))))


(defun keyword-to-friendly-string (keyword)
  (string-capitalize (string-downcase (force-string (match-replace-all keyword "-" " "))) :end 1))

(my-defun move-state 'object-to-ml ()
  (<div :class "move-state"
	(let ((friendly-move-type (keyword-to-friendly-string (my move-type))))
	  (cond ((eq :boolean (my choices))
		 (<p 
		   (loop for (keyword value) on (my args) by #'cddr
			 do (with-ml-output "The " (string-downcase (keyword-to-friendly-string keyword)) " is " value ". "))
		   
		   friendly-move-type
		   "? "
		    
		   (output-raw-ml (html-action-link "Yes" (my queue-choice t) (values)))
		   " "
		   (output-raw-ml (html-action-link "No" (my queue-choice nil) (values)))))
		((eql :select-card (my move-type))
		 (<p friendly-move-type "."))
		(t
		 (<p friendly-move-type
		     (cond ((eql (force-first (my choices)) :integer)
			    (with-ml-output " from " (apply 'min (choices-list (my choices))) " to " 
					    (apply 'max (choices-list (my choices))) ". "))
			   (t
			    (with-ml-output (format nil " ~{~A ~}" (my args)) " from " (format nil "~A" (my choices) ))))
     
		     (output-raw-ml 
		      (html-action-form 		     
			  ""
			  ((choice (first (choices-list (my choices)))))
			(my queue-choice (read-safely-from-string choice))
			(values)))))))))



(my-defun game 'object-to-ml ()
  (<div :class "players"
	(loop for p in (my players)
	      for once = t then nil
	      unless once do (<div :class "separate")
	      do (output-object-to-ml p))
	(<div :style (css-attrib :clear "both" :float "none" :border "none"))))
	  

(my-defun game 'object-to-ml :around ()
	  (if (my game-over)
	      (<p "Game over.")
	      (call-next-method)))

(defun current-web-controller (controller)
  (and (web-state-p controller)
       (eql *webapp-frame* (web-state-frame controller))))

(my-defun web-state resign ()
  (without-ml-output
    (game-resign (my game-state) me)
    (my notify)))

(my-defun web-state 'player-controller-name-to-ml ()
  (<span :class "username" (frame-username (my frame))))

(my-defun web-state 'object-to-ml ()
  (<div :class "game-state" :id (my id)
	(output-raw-ml (call-next-method))
	(<div :class "game-header"
	      (<h2 :class "game-title"
		   (string-capitalize (force-string (game-name (my game-state)))))

	      (<p :class "close-game"		
		  (cond ((its game-over (my game-state))
			 (html-replace-link "Play again."
			   (web-game-start (game-generator (my game-state)))))
			(t
			 (html-action-link "Resign."
			   (my resign))))))

	(<div :class "messages-and-talk"
	      (<div :class tpd2.webapp::+html-class-scroll-to-bottom+
		    (output-object-to-ml (my announcements)))
	      (<div :class "talk"
		    (html-action-form "Talk " 
			(text)
		      (without-ml-output
			(game-talk (my game-state) me text)
			))))
	
	(when (my resigned)
	  (<p "Resigned."))

	(unless (my resigned)
	  (output-object-to-ml (my game-state))
	  
	  (when (my waiting-for-input)
	    (<div :class "moves"
		  (loop for m in (my waiting-for-input)
			do 
			(output-object-to-ml m)))))))

(my-defun player 'object-to-ml ()
  (<div :class "player"
	(<h3 (player-controller-name-to-ml (my controller))
	     (when (my waiting-for-input)
	       (<span :class "turn" "'s turn")))))

(defmethod player-controller-message ((controller web-state) sender message)
  (web-state-add-announcement controller
			      (<p :class "message" (<span :class "sender" (frame-username sender))
				  " " message)))

(defun webapp-play-bot (game-name bot)
  (let ((game-state
	 (make-web-state :frame (webapp-frame))))
    (launch-game game-name (list bot game-state))
    (webapp ()
      (webapp-display game-state))))

(defun web-game-start (game-generator)
  (let ((c (make-web-state :frame (webapp-frame))))
    (game-generator-join-or-start game-generator c)
    (webapp ()
      (webapp-display c))))

(defun webapp-page-head-css ()
  (css-html-style 
    ((".inherit" <input <a)
     :text-decoration "inherit" :color "inherit" :background-color "inherit" :font-size "inherit" :font-weight "inherit"
     :font-family "inherit" 
     :border "none" :padding "0 0 0 0" :margin "0 0 0 0")
    (<body :font-family "georgia, serif" :word-spacing "0.075em" :letter-spacing "0.025em" :margin-left "5%" :margin-right "5%")
    ((<h1 <h2 <h3 <h4 <h5 <h6) :letter-spacing "0.05em" :font-weight "normal" :margin "0 0 0 0" :padding "0 0 0 0")
    ((<span <div <h1 <h2 <h3 <h4 <h5 <h6 <p <a <input) :direction "ltr" :unicode-bidi "bidi-override")
    ("input[type=text]" 
     :display "inline"
     :border-bottom "thin dashed black" 
     :font-style "italic" )
    (".frame" 
     :color "rgb(188,188,188)")
    (".game-message" :font-style "italic")
    (".change-name" :font-size "75%" :text-align "right")
    (".messages-and-talk"
     :margin-top "2em" 
     :margin-left "5em"
     :text-align "right")
    (".robot" :font-style "italic")
    ('(strcat ".messages-and-talk > ." tpd2.webapp::+html-class-scroll-to-bottom+) 
      :overflow "auto"      
      :padding-right "0.5em"
      :height "10em" )
    (".game-header"  :float "left")
    (".close-game:before" :content "\"+ \"")
    (".players"        
     :float "right"
     :margin-top "2em"
     )
    (".game-header + .messages-and-talk + DIV"
     :clear "both")
    (".players > DIV"
     :padding "0.4em 0.4em 0.4em 0.4em"
     :float "left"
     :border-top "2px solid black"
     )
    ("h1.mopoko" :font-size "4em" :text-align "right" :color "rgb(188,188,188)" :margin-bottom "0.333em")
    (<h2 :font-size "2.5em")
    (".webapp-section > ul > li" :font-size "2em")
    (".separate" 
     :height "4em"
     :border-right "2px solid black")
    (".talk input[type=\"text\"]" :width "60%")
    (("input[type=submit]" <a "[onclick]") 
     :display "inline" 
     :text-decoration "none")
    ("[onclick],a,input[type=submit]" 
     :background-color "rgb(228,228,228)"
     :cursor "pointer")))

(defun webapp-page-head (title)
  (<head
    (<title "mopoko.com " (output-raw-ml title))
    (output-raw-ml 
     (<noscript
       (output-raw-ml 
	(<meta :http-equiv "refresh" :content (byte-vector-cat "1000;" (page-link))))))
    (output-raw-ml 
     (webapp-page-head-css)
     (js-library))))

(defun webapp-page-body-start (title)
  (declare (ignore title))
  (<div :class "header"	
	(<h1 :class "mopoko" 
	     (<A :href (page-link "/") 
		 :class "inherit" 
		 (<span :style (css-attrib :color "black") "mopoko") ".com" ))
	(output-object-to-ml (webapp-frame))))

(register-action-page)
(register-channel-page)

(defpage "/" ()
  (webapp ""
    (webapp-select-one ""
		       (loop for game-name being the hash-keys of *games* collect game-name)
		       :display (lambda(g) (output-raw-ml 
				       "Play " g))
		       :replace
		       (lambda(game-name)
			 (web-game-start (find-game-generator game-name))))))

#.(when (find-package :swank)
    (push :tpd2-has-swank *features*)
    nil)

(progn
  (let ((socket (tpd2.io:make-con-listen :port 8888)))
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http::http-serve))

  #+sbcl
  (with-preserve-specials (*trace-output* *standard-output* *error-output* *debug-io* 
				     #+tpd2-has-swank swank::*emacs-connection*)
    (sb-thread:make-thread 
     (lambda() 
       (with-specials-restored
	   (tpd2.io:event-loop)))
     :name "MOPOKO-EVENT-LOOP")))
