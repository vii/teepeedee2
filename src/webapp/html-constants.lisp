(in-package #:tpd2.webapp)

(defconstant-string +channel-page-name+ "/*channel*")

(defconstant-string +action-page-name+ "/*action*")

(defconstant-string +action-form-class+ "-action-form-")
(defconstant-string +action-link-class+ "-action-link-")
(defconstant-string +replace-link-class+ "-replace-link-")

(defconstant-string +html-id-async-status+ "-async-status-")
(defconstant-string +html-class-scroll-to-bottom+ "-scroll-to-bottom-")
(defconstant-string +html-class-collapsed+ "-collapsed-")

(alexandria:define-constant +http-header-html-content-type+
    (byte-vector-cat "Content-Type: text/html;charset=utf-8" tpd2.io:+newline+)
  :test 'equalp)