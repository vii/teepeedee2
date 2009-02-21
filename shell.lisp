(asdf:oos 'asdf:load-op 'swank)
(setf swank:*use-dedicated-output-stream* nil)
(setf swank:*globally-redirect-io* t)
(setf *print-circle* t)

(swank:create-server :dont-close t :style :spawn)

;(proclaim '(optimize debug (safety 2)))
;(proclaim '(optimize speed))
(proclaim '(optimize debug (safety 2)))

(asdf:oos 'asdf:load-op 'teepeedee2)

(in-package #:teepeedee2)

(tpd2.datastore:datastore-use-file "datastore.log")

(progn
  (let ((socket (tpd2.io:make-con-listen :port 8888)))
    (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http::http-serve))
  (tpd2.io:event-loop))

