(in-package #:tpd2.blog)

(defstruct blog
  name
  dir
  entries
  site
  link-base-url
  static-base-url)

(my-defun blog read-in ()
  