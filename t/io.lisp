(in-package #:tpd2.test)

(def-suite io :in :tpd2)
(in-suite io)

(defprotocol echo-line (con)
  (io 'send con 
      (build-sendbuf
	(io 'recvline con)
	+newline+))
  (hangup con))


(defprotocol accept-echo-line (con)
  (let ((new (io 'accept con)))
    (launch-io 'echo-line new)
    (hangup con)))

(test echo-line
  (let ((port 18890) (address "127.0.0.1") (count 0)
	(lines (list 
		(with-output-to-string (stream)
		  (dotimes (x 1000) (format stream "x")))
		"Once more unto the breach, dear friends, once more;"
		"Or close the wall up with our English dead."
		"In peace there's nothing so becomes a man"
		"As modest stillness and humility;"
		"But when the blast of war blows in our ears,"
		"Then imitate the action of the tiger:"
		"Stiffen the sinews, summon up the blood,"
		"Disguise fair nature with hard-favour'd rage;" ;
		"Then lend the eye a terrible aspect;"
		"Let it pry through the portage of the head"
		"Like the brass cannon: let the brow o'erwhelm it"
		"As fearfully as doth a galled rock"
		"O'erhang and jutty his confounded base,"
		"Swill'd with the wild and wasteful ocean."
		"Now set the teeth and stretch the nostril wide;"
		"Hold hard the breath, and bend up every spirit  "
		"To his full height. On, on, you noblest English,"
		"Whose blood is fet from fathers of war-proof-"
		"Fathers that like so many Alexanders"
		"Have in these parts from morn till even fought,"
		"And sheath'd their swords for lack of argument."
		"Dishonour not your mothers; now attest"
		"That those whom you call'd fathers did beget you."
		"Be copy now to men of grosser blood,"
		"And teach them how to war. And you, good yeomen,"
		"Whose limbs were made in England, show us here"
		"The mettle of your pasture; let us swear"
		"That you are worth your breeding- which I doubt not;"
		"For there is none of you so mean and base"
		"That hath not noble lustre in your eyes."
		"I see you stand like greyhounds in the slips,"
		"Straining upon the start. The game's afoot:"
		"Follow your spirit; and upon this charge"
		"Cry 'God for Harry, England, and Saint George!'")))

    (defprotocol check-echo-line (con line)
      (io 'send con 
	  (build-sendbuf
	    line
	    +newline+))
      (let ((answer (byte-vector-to-string (io 'recvline con))))
	(is (string= answer line)))
      (hangup con)
      (incf count))

    (event-loop-reset)
    (dolist (line lines)
      (launch-io 'accept-echo-line (make-con-listen :address address :port port))
      (launch-io 'check-echo-line (make-con-connect :address address :port port) line)
      (event-loop))

    (is (= (length lines) count))))

