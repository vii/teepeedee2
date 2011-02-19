(in-package #:tpd2.test)

(def-suite http :in :tpd2)
(in-suite http)

(test http-request-and-serve
  (let ((port 18282)
        (req-count 19)
        (address "127.0.0.1")
        (disp (find-or-make-dispatcher (force-byte-vector "*..*")))
        *default-site*)
    (with-site (:dispatcher disp)
      (labels ((build-body (n)
                 (with-sendbuf ()
                   (let ((v (make-byte-vector n)))
                     (loop for a below (length v) do (setf (aref v a) (random #x100)))
                   v)))
               (random-alphnum (n)
                 (let ((v (make-byte-vector n)))
                   (loop for a below (length v) do (setf (aref v a) (+ (char-code #\A) (random 26))))
                   v))
             (build-headers (n)
               (let ((s (with-sendbuf ())))
                 (loop for i below n do
                       (with-sendbuf-continue (s)
                         "X-"
                         (random-alphnum (1+ (random 100)))
                         ": "
                         (random-alphnum (1+ (random 400)))
                         +newline+
                         ))
                 s)))
        (defpage "/" (header body)
          (values
           (build-body (byte-vector-parse-integer body))
           (build-headers (byte-vector-parse-integer header))))

        (let ((socket (tpd2.io:make-con-listen :address address :port port)))
          (block event-loop
            (labels ((req-finished ()
                       (decf req-count)
                       (when (zerop req-count)
                         (return-from event-loop))))
              (unwind-protect
                   (progn
                     (tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http::http-serve)
                     (loop for i below req-count do
                           (let ((b (* (random 16) (random (* 16 1024))))
                                 (h (* (random 16) (random 16))))
                             (tpd2.http:launch-http-request
                              :timeout 60
                              :port port :address address
                              :hostname (tpd2.http:dispatcher-canonical-name disp)
                              :path (byte-vector-cat "/?BODY=" b "&HEADER=" h)
                              :extra-header-lines (build-headers (random 32))
                              :done (lambda(response &key response-code)
                                      (req-finished)
                                      (is (= 200 response-code)) (is (= (length response) b)))
                              :failure (lambda(&rest e)
                                         (req-finished)
                                         (fail (format nil "~A" e))))))
                     (event-loop))
                (tpd2.io:hangup socket)))))))))
