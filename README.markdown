teepeedee2 
================================

What's this?
--------

teepeedee2 is a webapplication framework for dynamic webpages. It's
goal is to be fast. It only works on Linux.

Quickstart
--------

You need SBCL. On Ubuntu or Debian

    $ sudo apt-get install sbcl

Untar or git clone

      $ git clone git://github.com/vii/teepeedee2.git

      $ cd teepeedee2

Start sbcl and run the quickstart.lisp
      
      $ sbcl --load quickstart.lisp

This will download the following packages with asdf-install, and their
dependencies: iterate cffi cl-irregsexp trivial-backtrace

Note that it DISABLES the GPG signature checking! You will be running
UNVERIFIED CODE.

	      It will try to download the following packages with 
	      Install where?
	      1) System-wide install: 
	         System in /usr/lib/sbcl/site-systems/
	         Files in /usr/lib/sbcl/site/ 
	      2) Personal installation: 
	         System in /home/username/.sbcl/systems/
	         Files in /home/username/.sbcl/site/ 
	       --> 

Enter "2" and press enter.

Lots of downloading and compiling will occur.

Then visit http://localhost:8080/hello

You can now enter new pages at the SBCL REPL

    CL-USER> (in-package #:teepeedee2.quickstart)

    QUICKSTART> (defpage "/goodbye" ((name "Friend"))
    		(<div (<h1 "Bye bye " name) (<p "The universal time is " (get-universal-time))))


Benchmarking
--------

	(in-package #:tpd2)
	(defpage "/test" (name) :create-frame nil
	    (tpd2.ml.html:<h1 "Hello " name))

	(http-start-server 8080)

	(event-loop)

Use apachebench

	$ ab -n 100000 -c10 http://127.0.0.1:8080/test?name=John



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
