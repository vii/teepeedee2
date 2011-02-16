(in-package #:tpd2.game)

(defvar *web-state-move-timeout* 60)

(defstruct queued-choice
  move-type
  choice)

(defmyclass (web-state (:include simple-channel))
    frame
  (announcements nil)
  (waiting-for-input nil)
  (queued-choices nil)
  game-state
  timed-out
  (timeout (make-timeout)))

(defun make-web-state-from-frame ()
  (let ((frame (webapp-frame)))
    (check-type frame frame)
    (let ((w (make-web-state :frame frame)))
      (push (lambda (f) (declare (ignore f)) (channel-destroy w))
            (frame-destroy-hooks frame))
      w)))

(my-defun web-state 'inform :after (game-state (message (eql :new-state)) &rest args)
  (declare (ignore game-state args))
  (setf (my queued-choices) nil))

(my-defun web-state 'inform (game-state message &rest args)
  (declare (ignore game-state))
  (my add-announcement
      (<p :class "game-message"
          message
          " "
          (output-object-to-ml args))))

(my-defun web-state resigned ()
  (not (loop for p in (game-players (my game-state)) thereis (eql me (player-controller p)))))

(my-defun web-state 'inform :before (game-state message &rest args)
          (declare (ignore args message))

          (setf (my game-state) game-state

                (timeout-func (my timeout))
                (lambda ()
                  (unless (game-game-over (my game-state))
                    (setf (my timed-out) t)
                    (my resign :reason :timed-out)))))

(my-defun web-state add-announcement (a)
  (appendf (my announcements) (list a))
  (my notify))

(defmethod move-continuation (k (controller web-state) player-state move-type choices &rest args)
  (web-state-add-move-state controller
       (make-move-state :cc k
                        :move-type move-type
                        :player-state player-state
                        :choices choices
                        :args args)))


(defmethod move-continuation (k (controller web-state)
                              player-state
                              (move-type (eql :ready-to-play)) choices &rest args)
  (declare (ignore args player-state choices))
  (funcall k t))

(my-defun web-state timeout-reset ()
  (timeout-set (my timeout) *web-state-move-timeout*))

(my-defun web-state timeout-cancel ()
  (timeout-cancel (my timeout)))

(my-defun web-state add-move-state (move-state)
  (my timeout-reset)
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
              do
              (when (eql (queued-choice-move-type qc) (move-state-move-type waiting))
                (deletef qc (my queued-choices))
                (unless (eq 'invalid-choice (validate-choice (move-state-choices waiting) (queued-choice-choice qc)))
                  (deletef waiting (my waiting-for-input))
                  (if (my waiting-for-input)
                      (my timeout-reset)
                      (my timeout-cancel))
                  (funcall (move-state-cc waiting) (queued-choice-choice qc))
                  (my notify)
                  (return-from web-state-try-to-move))))))


(defun keyword-to-friendly-string (keyword)
  (string-capitalize (string-downcase (match-replace-all (force-string keyword) ("-" " "))) :end 1))

(defun friendly-string (object)
  (typecase object
    (symbol
     (keyword-to-friendly-string object))
    (t
     object)))

(my-defun move-state 'object-to-ml ()
  (<div :class "move-state"
        (let ((friendly-move-type (keyword-to-friendly-string (my move-type))))
          (cond ((eq :boolean (my choices))
                 (<p
                   (loop for (keyword value) on (my args) by #'cddr
                         do (with-ml-output "The " (string-downcase (keyword-to-friendly-string keyword)) " is " value ". "))

                   friendly-move-type
                   "? "

                   (html-action-link "Yes" (my queue-choice t) (values))
                   " "
                   (html-action-link "No" (my queue-choice nil) (values))))
                ((eql :select-card (my move-type))
                 (<p friendly-move-type "."))
                (t
                 (<p friendly-move-type
                     (cond ((eql (force-first (my choices)) :integer)
                            (with-ml-output " from " (reduce #'min (choices-list (my choices))) " to "
                                            (reduce #'max (choices-list (my choices))) ". "))
                           (t
                            (with-ml-output (format nil " ~{~A ~}" (my args)) " from ")))

                     (loop for c in (choices-list (my choices)) do
                           (let-current-values (c)
                             (with-ml-output " "
                                             (html-action-link (friendly-string c)
                                               (my queue-choice c)))))

                     (html-action-form
                         ""
                         (choice)
                       (my queue-choice (read-safely-from-string choice))
                       (values))))))))



(my-defun game 'object-to-ml ()
  (<div :class "players"
        (loop for p in (my players)
              for once = t then nil
              unless once do (<div :class "separate")
              do (with-ml-output
                   (player-state-to-ml p)))
        (<div :style (css-attrib :clear "both" :float "none" :border "none"))))


(my-defun game 'object-to-ml :around ()
          (if (my game-over)
              (<h2 "Game over." (my play-again-ml) "?")
              (call-next-method)))

(defun current-web-controller (controller)
  (and (web-state-p controller)
       (eql *webapp-frame* (web-state-frame controller))))

(my-defun web-state resign (&rest args)
  (without-ml-output
    (apply 'game-resign (my game-state) me args)
    (my notify)))

(my-defun web-state 'player-controller-name-to-ml ()
  (<span :class "username" (frame-username (my frame))))

(my-defun web-state 'player-controller-var ( var)
  (frame-var (my frame) var))

(my-defun web-state (setf 'player-controller-var) (new-value var)
  (setf (frame-var (my frame) var) new-value))

(defgeneric game-title-ml (game)
  (:method (game)
    (<h2 :class "game-title"
         (game-name game))))

(my-defun web-state 'object-to-ml ()
  (assert (my game-state) () "No game started; please use game-new-state")
  (<div :class "game-state"
        (<div :class "game-header"
              (game-title-ml (my game-state)))

        (call-next-method)

        (<div :class "talk"
              (html-action-form "Talk "
                  ((text nil :reset ""))
                (without-ml-output
                  (my timeout-reset)
                  (game-talk (my game-state) me text))))))

(my-defun game play-again-ml ()
  (html-replace-link "Play again"
    (web-game-start (my generator))))

(my-defun web-state play-again-ml ()
  (game-play-again-ml (my game-state)))

(my-defun web-state 'simple-channel-body-ml ()
  (<div :class "game-state"
        (<div :class "game-state-body"
              (<p :class "close-game"
                  (cond ((game-game-over (my game-state))
                         (my play-again-ml))
                        (t
                         (html-action-link "Resign"
                           (my resign))))))

        (<div :class "messages-and-talk"
              (<div :class tpd2.webapp:+html-class-scroll-to-bottom+
                    (output-object-to-ml (my announcements))))

        (cond
          ((my timed-out)
           (<p (load-time-value (format nil "Timed out; sorry, you took longer than ~R second~:P to respond."
                                        *web-state-move-timeout*))
               (my play-again-ml) "?"))
          ((my resigned)
           (<p "Resigned." (my play-again-ml)))
          (t
           (output-object-to-ml (my game-state))

           (when (my waiting-for-input)
             (<div :class "moves"
                   (loop for m in (my waiting-for-input)
                         do
                         (output-object-to-ml m))))))))

(my-defun player state-to-ml ()
  (<div :class "player"
        (<h3 me
             (when (my waiting-for-input)
               (<span :class "turn" "'s turn")))))

(my-defun player 'object-to-ml ()
  (player-controller-name-to-ml (my controller)))

(defun css ()
  (css-html-style
    ((".inherit" <input <a)
     :text-decoration "inherit" :color "inherit" :background-color "inherit" :font-size "inherit" :font-weight "inherit"
     :font-family "inherit"
     :border "none" :padding "0 0 0 0" :margin "0 0 0 0")
    (<body :font-family "georgia, serif" :word-spacing "0.075em" :letter-spacing "0.025em" :margin-left "5%" :margin-right "5%"
           :background-color "white")
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
     :margin-left "1em"
     :text-align "left")
    (".robot" :font-style "italic")
    ('(strcat ".messages-and-talk > ." tpd2.webapp:+html-class-scroll-to-bottom+)
      :overflow "auto"
      :padding-right "0.5em"
      :height "20em" )
    (".play-game-description,.about"
     :padding-left "3em"
     :padding-right "3em")

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
    (".webapp-section > ul > li" :padding-bottom "1em")
    (".webapp-section > ul > li a.-replace-link-" :font-size "2em")
    (".separate"
     :height "4em"
     :border-right "2px solid black")
    (".talk input[type=\"text\"]" :width "60%")
    (("input[type=submit]" <a "[onclick]")
     :display "inline"
     :text-decoration "none")
    (".HEARTS, .DIAMONDS" :color "red")
    (".HEARTS, .DIAMONDS, .CLUBS, .SPADES" :font-size "4em")
    (".close-game" :text-align "right")
    ("[onclick],a,input[type=submit]"
     :background-color "rgb(228,228,228)"
     :cursor "pointer")
    ("#-async-status-"
     :position "fixed"
     :bottom "0em"
     :padding "0.2em 0.2em 0.2em 0.2em"
     :margin "0 0 0 0"
     :right "0em"
     :text-align "right"
     :background-color "white"
     :font-size "70%"
     :x-border-radius "0.3em"
     :border "thin solid #cccccc";
     :z-index 2)

    ))

(defsite *site*
    :page-body-start (lambda(title)
                       (declare (ignore title))
                               `(<div :class "header"
                                      (<h1 :class "mopoko"
                                           (<A :href (page-link "/")
                                               :class "inherit"
                                               "mopoko " " prerelease" ))
                                      (<h4 :id ,tpd2.webapp:+html-id-async-status+ )
                                      (output-object-to-ml (webapp-frame))))
    :page-head (lambda(title)
                 `(with-ml-output
                    (<title "mopoko.com " (output-raw-ml ,title))
                    (css)
                    (webapp-default-page-head-contents)))
    :page-body-footer
    (lambda(title)
      (declare (ignore title))
      `(with-ml-output
         (webapp-default-page-footer))))

(with-compile-time-site (*site*)
  (defun web-add-game (game-generator name)
    (defpage-lambda (byte-vector-cat "/" name)
        (lambda ()
          (web-game-start game-generator))))

  (defun webapp-play-bot (game-name bot)
    (let ((game-state
           (make-web-state-from-frame)))
      (launch-game game-name (list bot game-state))
      (webapp ()
        (webapp-display game-state))))

  (defun web-game-start (game-generator)
    (let ((c (make-web-state-from-frame)))
      (game-generator-join-or-start game-generator c)
      (webapp ()
        (webapp-display c))))

  (defpage "/" ()
    (webapp ""
      (webapp-select-one ""
                         (loop for g being the hash-values of *games*
                               when (game-generator-advertised g)
                               collect g)
                         :display (lambda(g) (output-raw-ml
                                         "Play " (game-generator-name g)))
                         :describe (lambda (g)
                                     (let ((d (game-generator-description g)))
                                       (when d
                                         (<div :class "play-game-description"
                                          (output-raw-ml d)))))
                         :replace
                         (lambda(g)
                           (web-game-start g)))

      (html-collapser (<h3 "About mopoko.com")
        (<div :class "about"

              (<p (<a :href (page-link "/") "mopoko.com") " is a place to play
      games. Have fun!")

              (<p "When you choose to play a game, we wait a few seconds for
      someone else to join in. If nobody does, then a robot will join
      the game. Each robot has a different style of play.")

              (<p "Please " (<a :href "mailto:john@fremlin.org" "email") " me
      with your comments, advice and suggestions for a new game.")

              (<p "Thanks for visiting " (output-raw-ml "&mdash;") " John Fremlin " (<a :href "mailto:john@fremlin.org" "<john@fremlin.org>") ", 24 September 2009")

              (<p "PS. Look at the " (<a :href "http://www.github.com/vii/teepeedee2" "source code for this website") "."))))))
