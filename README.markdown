teepeedee2 
================================

What's this?
--------

teepeedee2 is a webapplication framework for dynamic webpages. It's goal is to be fast 

Getting started
--------

Very rough notes!

      $ cd teepeedee2

Start sbcl 
      
      $ sbcl

or ClozureCL
   
      $ lx86cl -K utf-8

Now enter into the Lisp

      (asdf:oos 'asdf:load-op 'teepeedee2)

If some dependencies are missing, either download and install them by hand, or use the following

      (require 'asdf-install)
      (loop for x in '(:cffi
	       :iterate
	       :cl-irregsexp
	       :trivial-backtrace) (asdf-install:install x))

To start the server listing on port 8888, do the following

      (in-package #:tpd2)
      (loop for port in '(8888) do
            (let ((socket (tpd2.io:make-con-listen :port port)))
            	(tpd2.io:launch-io 'tpd2.io:accept-forever socket 'tpd2.http:http-serve)))

      (tpd2.io:event-loop)

Now go to http://localhost:8888/


Benchmarking
--------

	(in-package #:tpd2)
	(defsite *bench*)
	(with-site (*bench*)
	 (defpage "/test" (name) :create-frame nil
	    (tpd2.ml.html:<h1 "Hello " name)))
	(launch-io 'accept-forever (make-con-listen :port 3000) 'tpd2.http::http-serve)
	(event-loop)

Use apachebench

	$ ab -n 100000 -c10 http://127.0.0.1:3000/test?name=John



More info
--------

This webserver is not finished and I wrote it to learn and experiment
with Common Lisp. There is a lot to be tidied up, now I know the
language better. If you have any interest in using it, please get in
touch with me, John Fremlin <john@freml.in>, or
http://john.fremlin.org/contact.html

It is faster than all(?) other web application frameworks for serving
small dynamic webpages. Please let me know if you have a case where
another framework is faster!

You can see benchmarks comparing it to other web application platforms
like PHP and Rails in this presentation
http://tlug.jp/meetings/2008/11/serving-dynamic-webpages-in-less-then-a-millisecond_john-fremlin_handout.pdf

You can see an example of it running a blog on http://john.freml.in.

It is designed so that small fragments of JavaScript can be delivered
at low overhead to many clients.

However, it also includes general support libraries for quickly
generating XML/HTML (tpd2.ml2), for doing fast networking in a
continuation passing style, i.e. fast event driven userspace
threading (tpd2.io).


For an example application, loaded by default, see the src/game directory.

The addons/ directory contains a few of the other projects (but not
all) that teepeedee2 depends on. They are released under their own
licence.

The licence for most of tpd2 is the Lisp LGPL. However, I take no profit
from making this, and request that if you make an application with it,
please release the source so others can learn from it. Don't be
selfish!

It runs on SBCL and ClozureCL. It shouldn't be hard to port to other
modern Common Lisps.