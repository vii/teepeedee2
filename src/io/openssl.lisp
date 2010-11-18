(in-package #:tpd2.io)

;; FFI for OpenSSL library

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libssl
    (:unix  "libssl.so" "libssl.so.0.9.8")
    (t (:default "libssl3")))

  (cffi:use-foreign-library libssl)

  #+freebsd
  (progn
    (cffi:define-foreign-library libcrypto
      (:unix (:or "libcrypto.so" "/usr/local/lib/libcrypto.so"))
      (t (:default "libcrypto")))
    (cffi:use-foreign-library libcrypto)))

(defconstant +SSL_ERROR_NONE+ 0)
(defconstant +SSL_ERROR_WANT_READ+ 2)
(defconstant +SSL_ERROR_WANT_WRITE+ 3)
(defconstant +SSL_ERROR_ZERO_RETURN+ 6)
(defconstant +SSL_CTRL_MODE+ 33)
(defconstant +SSL_MODE_ENABLE_PARTIAL_WRITE+ 1)
(defconstant +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+ 2)
(defconstant +SSL_ST_CONNECT+ #x1000)

(cffi:defcfun ("SSL_connect" ssl-connect)
    :int
  (ssl :pointer))
(cffi:defcfun ("SSL_accept" ssl-accept)
    :int
  (ssl :pointer))
(cffi:defcfun ("SSL_write" ssl-write)
    :int
  (ssl :pointer)
  (buf :pointer)
  (num :int))
(cffi:defcfun ("SSL_read" ssl-read)
    :int
  (ssl :pointer)
  (buf :pointer)
  (num :int))

(cffi:defcfun ("SSL_set_read_ahead" ssl-set-read-ahead)
    :void
  (ssl :pointer)
  (yes :int))

(cffi:defcfun ("SSL_get_shutdown" ssl-get-shutdown)
    :int
  (ssl :pointer))

(cffi:defcfun ("SSL_pending" ssl-pending)
    :int
  (ssl :pointer))

(cffi:defcfun ("SSL_state" ssl-state)
    :int
  (ssl :pointer))

(cffi:defcfun ("SSL_free" ssl-free)
    :void
  (ssl :pointer))
(cffi:defcfun ("SSL_get_error" ssl-get-error)
    :int
  (ssl :pointer)
  (ret :int))

(cffi:defcfun ("SSL_load_error_strings" ssl-load-error-strings)
    :void)
(cffi:defcfun ("SSL_library_init" ssl-library-init)
    :int)
(cffi:defcfun ("SSL_CTX_new" ssl-ctx-new)
    :pointer
  (method :pointer))
(cffi:defcfun ("SSLv23_method" ssl-v23-method)
    :pointer)
(cffi:defcfun ("SSL_new" ssl-new)
    :pointer
  (ctx :pointer))
(cffi:defcfun ("SSL_set_fd" ssl-set-fd)
    :int
  (ssl :pointer)
  (fd :int))

(defvar *openssl-initialized* nil)
(defvar *ssl-ctx*)
(defun initialize-openssl ()
  (unless *openssl-initialized*
    (cffi:load-foreign-library 'libssl)
    (cffi:load-foreign-library 'libeay32)
    (ssl-library-init)
    (ssl-load-error-strings)
    (setf *ssl-ctx* (ssl-ctx-new (ssl-v23-method))
	  *openssl-initialized* t)))

(defun ssl-ctx-set-mode (context mode)
  (ssl-ctx-ctrl context +SSL_CTRL_MODE+ mode (cffi:null-pointer)))
