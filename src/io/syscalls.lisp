(in-package #:tpd2.io)

;;; A simple syscall is one which returns -1 on error and sticks the
;;; error in *errno* (of course, this is just the glibc interface to
;;; the syscall).

(defmacro with-foreign-object-and-slots ((slots var type-unquoted) &body body)
  `(cffi:with-foreign-object (,var ',type-unquoted)
    (cffi:with-foreign-slots (,slots ,var ,type-unquoted)
      ,@body)))

(defmacro syscall-error-number (symbol number description)
  `(defconstant ,symbol ,number
    ,description))

(syscall-error-number +EPERM+  1 "Operation not permitted")
(syscall-error-number +ENOENT+  2 "No such file or directory")
(syscall-error-number +ESRCH+  3 "No such process")
(syscall-error-number +EINTR+  4 "Interrupted system call")
(syscall-error-number +EIO+  5 "I/O error")
(syscall-error-number +ENXIO+  6 "No such device or address")
(syscall-error-number +E2BIG+  7 "Argument list too long")
(syscall-error-number +ENOEXEC+  8 "Exec format error")
(syscall-error-number +EBADF+  9 "Bad file number")
(syscall-error-number +ECHILD+ 10 "No child processes")
(syscall-error-number +EAGAIN+ 11 "Try again")
(syscall-error-number +ENOMEM+ 12 "Out of memory")
(syscall-error-number +EACCES+ 13 "Permission denied")
(syscall-error-number +EFAULT+ 14 "Bad address")
(syscall-error-number +ENOTBLK+ 15 "Block device required")
(syscall-error-number +EBUSY+ 16 "Device or resource busy")
(syscall-error-number +EEXIST+ 17 "File exists")
(syscall-error-number +EXDEV+ 18 "Cross-device link")
(syscall-error-number +ENODEV+ 19 "No such device")
(syscall-error-number +ENOTDIR+ 20 "Not a directory")
(syscall-error-number +EISDIR+ 21 "Is a directory")
(syscall-error-number +EINVAL+ 22 "Invalid argument")
(syscall-error-number +ENFILE+ 23 "File table overflow")
(syscall-error-number +EMFILE+ 24 "Too many open files")
(syscall-error-number +ENOTTY+ 25 "Not a typewriter")
(syscall-error-number +ETXTBSY+ 26 "Text file busy")
(syscall-error-number +EFBIG+ 27 "File too large")
(syscall-error-number +ENOSPC+ 28 "No space left on device")
(syscall-error-number +ESPIPE+ 29 "Illegal seek")
(syscall-error-number +EROFS+ 30 "Read-only file system")
(syscall-error-number +EMLINK+ 31 "Too many links")
(syscall-error-number +EPIPE+ 32 "Broken pipe")
(syscall-error-number +EDOM+ 33 "Math argument out of domain of func")
(syscall-error-number +ERANGE+ 34 "Math result not representable")

(cffi:defcstruct (sockaddr_in :size 16)
  (family :uint16)
  (port :uint16)
  (addr :uint32))

(eval-always
  (cffi:defcvar (("errno" +syscall-error-number+) :read-only t) :int))

(cffi:defcfun strerror :string
  (errno :int))

(define-condition syscall-failed (error)
  ((errno :initarg :errno :reader syscall-failed-errno)
   (syscall :initarg :syscall))
  (:report (lambda (condition stream)
	     (with-slots (errno syscall) condition
	       (format stream "~A failed: ~A (errno ~A)" syscall (strerror errno) errno)))))
(eval-always
  (defun syscall-name (name)
    (string-downcase (force-string name)))
  (defun direct-syscall-sym (name)
    (intern (strcat 'syscall-direct- name))))

(defmacro def-syscall (name &rest args)
  `(cffi:defcfun (,(syscall-name name)  ,(direct-syscall-sym name))
      :int
      ,@args))

(defmacro def-simple-syscall (name &rest args)
  (let ((direct-sym (direct-syscall-sym name))
	(syscall-name (syscall-name name))
	(arg-names (mapcar #'first args))
	(func (intern (strcat 'syscall- name))))
    `(progn
      (def-syscall ,name ,@args)
      (defun ,func ,arg-names
	(declare (optimize speed (safety 0)))
	(loop
	    (let ((val (,direct-sym ,@arg-names)))
	      (cond ((or (/= val -1) (= +syscall-error-number+ +EAGAIN+))
		     (return val))
		    ((= +syscall-error-number+ +EINTR+)
		     nil)
		    (t
		     (error 'syscall-failed :errno +syscall-error-number+ :syscall ,syscall-name))))))
      (declaim (inline ,func))
      ',func)))


(def-simple-syscall close
    (fd :int))

(defconstant +SIG_IGN+ (cffi:make-pointer 1))
(defconstant +SIG_DFL+ (cffi:make-pointer 0))
(defconstant +SIGPIPE+ 13)

(cffi:defcfun ("signal" syscall-signal)
      :pointer
  (signum :int) 
  (action :pointer)) 

(def-simple-syscall read
    (fd :int)
  (buf :pointer)
  (size :unsigned-long))

(def-simple-syscall recvfrom
    (fd :int)
  (buf :pointer)
  (size :unsigned-long)
  (flags :int)
  (address :pointer)
  (address-len :pointer))

(def-simple-syscall write
    (fd :int)
  (buf :pointer)
  (size :unsigned-long))

(def-simple-syscall writev
    (fd :int)
  (iov :pointer)
  (iovcnt :int))

(def-simple-syscall sendto
    (fd :int)
  (buf :pointer)
  (size :unsigned-long)
  (flags :int)
  (address :pointer)
  (address-len :pointer))

(def-simple-syscall sendfile
    (out_fd :int)
  (in_fd :int)
  (offset :pointer)
  (count :unsigned-long))

(def-simple-syscall accept
    (sockfd :int)
  (addr :pointer)
  (addrlen :pointer))

(def-simple-syscall fcntl
    (fd :int)
  (cmd :int)
  (arg :long))




(defconstant +O_NONBLOCK+ #x800)
(defconstant +F_GETFL+ 3)
(defconstant +F_SETFL+ 4)

(defun set-fd-nonblock (fd)
  (syscall-fcntl fd +F_SETFL+ (logior +O_NONBLOCK+ (syscall-fcntl fd +F_GETFL+ 0))))


#|

(defun grovel-from-c-defines (string)
  (dolist (line (cl-ppcre:split "\\n" string))
    (cl-ppcre:register-groups-bind (name val description)
	("^#define\\s+(\\S+)\\s+(\\S+)\\s*(?:/\\*\\s*(.*?)\\s*\\*/)?" line)
      (format t "(defconstant +~A+ ~A \"~A\")~&"
	      name val (or description name)))
    (cl-ppcre:register-groups-bind (name val description)
	("^\\s*(\\S+)\\s*=\\s*(\\S+?(?:,)?)\\s*(?:/\\*\\s*(.*?)\\s*\\*/)?" line)
      (format t "(defconstant +~A+ ~A \"~A\")~&"
	      name val (or description name)))))
|#


(defconstant +IPPROTO_IP+ 0 "Dummy protocol for TCP")
(defconstant +IPPROTO_ICMP+ 1 "Internet Control Message Protocol")
(defconstant +IPPROTO_IGMP+ 2 "Internet Group Management Protocol")
(defconstant +IPPROTO_IPIP+ 4 "IPIP tunnels (older KA9Q tunnels use 94)")
(defconstant +IPPROTO_TCP+ 6 "Transmission Control Protocol")
(defconstant +IPPROTO_EGP+ 8 "Exterior Gateway Protocol")
(defconstant +IPPROTO_PUP+ 12 "PUP protocol")
(defconstant +IPPROTO_UDP+ 17 "User Datagram Protocol")
(defconstant +IPPROTO_IDP+ 22 "XNS IDP protocol")
(defconstant +IPPROTO_DCCP+ 33 "Datagram Congestion Control Protocol")
(defconstant +IPPROTO_RSVP+ 46 "RSVP protocol")
(defconstant +IPPROTO_GRE+ 47 "Cisco GRE tunnels (rfc 1701,1702)")
(defconstant +IPPROTO_IPV6+ 41 "IPv6-in-IPv4 tunnelling")
(defconstant +IPPROTO_ESP+ 50 "Encapsulation Security Payload protocol")
(defconstant +IPPROTO_AH+ 51 "Authentication Header protocol")
(defconstant +IPPROTO_BEETPH+ 94 "IP option pseudo header for BEET")
(defconstant +IPPROTO_PIM+ 103 "Protocol Independent Multicast")
(defconstant +IPPROTO_COMP+ 108 "Compression Header protocol")
(defconstant +IPPROTO_SCTP+ 132 "Stream Control Transport Protocol")
(defconstant +IPPROTO_UDPLITE+ 136 "UDP-Lite (RFC 3828)")
(defconstant +IPPROTO_RAW+ 255 "Raw IP packets")

(defconstant +SOL_SOCKET+ 1 "SOL_SOCKET")
(defconstant +SOL_RAW+ 255 "SOL_RAW")
(defconstant +SOL_DECNET+ 261 "SOL_DECNET")
(defconstant +SOL_X25+ 262 "SOL_X25")
(defconstant +SOL_PACKET+ 263 "SOL_PACKET")
(defconstant +SOL_ATM+ 264 "ATM layer (cell level).")
(defconstant +SOL_AAL+ 265 "ATM Adaption Layer (packet level).")
(defconstant +SOL_IRDA+ 266 "SOL_IRDA")
(defconstant +SO_DEBUG+ 1 "SO_DEBUG")
(defconstant +SO_REUSEADDR+ 2 "SO_REUSEADDR")
(defconstant +SO_TYPE+ 3 "SO_TYPE")
(defconstant +SO_ERROR+ 4 "SO_ERROR")
(defconstant +SO_DONTROUTE+ 5 "SO_DONTROUTE")
(defconstant +SO_BROADCAST+ 6 "SO_BROADCAST")
(defconstant +SO_SNDBUF+ 7 "SO_SNDBUF")
(defconstant +SO_RCVBUF+ 8 "SO_RCVBUF")
(defconstant +SO_SNDBUFFORCE+ 32 "SO_SNDBUFFORCE")
(defconstant +SO_RCVBUFFORCE+ 33 "SO_RCVBUFFORCE")
(defconstant +SO_KEEPALIVE+ 9 "SO_KEEPALIVE")
(defconstant +SO_OOBINLINE+ 10 "SO_OOBINLINE")
(defconstant +SO_NO_CHECK+ 11 "SO_NO_CHECK")
(defconstant +SO_PRIORITY+ 12 "SO_PRIORITY")
(defconstant +SO_LINGER+ 13 "SO_LINGER")
(defconstant +SO_BSDCOMPAT+ 14 "SO_BSDCOMPAT")
(defconstant +SO_PASSCRED+ 16 "SO_PASSCRED")
(defconstant +SO_PEERCRED+ 17 "SO_PEERCRED")
(defconstant +SO_RCVLOWAT+ 18 "SO_RCVLOWAT")
(defconstant +SO_SNDLOWAT+ 19 "SO_SNDLOWAT")
(defconstant +SO_RCVTIMEO+ 20 "SO_RCVTIMEO")
(defconstant +SO_SNDTIMEO+ 21 "SO_SNDTIMEO")

(defconstant +TCP_NODELAY+ 1 "Turn off Nagle's algorithm.")
(defconstant +TCP_MAXSEG+ 2 "Limit MSS")
(defconstant +TCP_CORK+ 3 "Never send partially complete segments")
(defconstant +TCP_KEEPIDLE+ 4 "Start keeplives after this period")
(defconstant +TCP_KEEPINTVL+ 5 "Interval between keepalives")
(defconstant +TCP_KEEPCNT+ 6 "Number of keepalives before death")
(defconstant +TCP_SYNCNT+ 7 "Number of SYN retransmits")
(defconstant +TCP_LINGER2+ 8 "Life time of orphaned FIN-WAIT-2 state")
(defconstant +TCP_DEFER_ACCEPT+ 9 "Wake up listener only when data arrive")
(defconstant +TCP_WINDOW_CLAMP+ 10 "Bound advertised window")
(defconstant +TCP_INFO+ 11 "Information about this connection.")
(defconstant +TCP_QUICKACK+ 12 "Block/reenable quick acks")
(defconstant +TCP_CONGESTION+ 13 "Congestion control algorithm")
(defconstant +TCP_MD5SIG+ 14 "TCP MD5 Signature (RFC2385)")


(defconstant +SOCK_STREAM+ 1)
(defconstant +SOCK_DGRAM+ 2)
(defconstant +SOCK_RAW+ 3)
(defconstant +SOCK_RDM+ 4)
(defconstant +SOCK_SEQPACKET+ 5)
(defconstant +SOCK_PACKET+ 10)

(defconstant +AF_UNSPEC+ 0 "AF_UNSPEC")
(defconstant +AF_UNIX+ 1 "Unix domain sockets")
(defconstant +AF_LOCAL+ 1 "POSIX name for AF_UNIX")
(defconstant +AF_INET+ 2 "Internet IP Protocol")
(defconstant +AF_AX25+ 3 "Amateur Radio AX.25")
(defconstant +AF_IPX+ 4 "Novell IPX")
(defconstant +AF_APPLETALK+ 5 "AppleTalk DDP")
(defconstant +AF_NETROM+ 6 "Amateur Radio NET/ROM")
(defconstant +AF_BRIDGE+ 7 "Multiprotocol bridge")
(defconstant +AF_ATMPVC+ 8 "ATM PVCs")
(defconstant +AF_X25+ 9 "Reserved for X.25 project")
(defconstant +AF_INET6+ 10 "IP version 6")
(defconstant +AF_ROSE+ 11 "Amateur Radio X.25 PLP")
(defconstant +AF_DECnet+ 12 "Reserved for DECnet project")
(defconstant +AF_NETBEUI+ 13 "Reserved for 802.2LLC project")
(defconstant +AF_SECURITY+ 14 "Security callback pseudo AF")
(defconstant +AF_KEY+ 15 "PF_KEY key management API")
(defconstant +AF_NETLINK+ 16 "AF_NETLINK")
(defconstant +AF_ROUTE+ +AF_NETLINK+ "Alias to emulate 4.4BSD")
(defconstant +AF_PACKET+ 17 "Packet family")
(defconstant +AF_ASH+ 18 "Ash")
(defconstant +AF_ECONET+ 19 "Acorn Econet")
(defconstant +AF_ATMSVC+ 20 "ATM SVCs")
(defconstant +AF_SNA+ 22 "Linux SNA Project (nutters!)")
(defconstant +AF_IRDA+ 23 "IRDA sockets")
(defconstant +AF_PPPOX+ 24 "PPPoX sockets")
(defconstant +AF_WANPIPE+ 25 "Wanpipe API Sockets")
(defconstant +AF_LLC+ 26 "Linux LLC")
(defconstant +AF_TIPC+ 30 "TIPC sockets")
(defconstant +AF_BLUETOOTH+ 31 "Bluetooth sockets")
(defconstant +AF_MAX+ 32 "For now..")


(defun socket-cork (fd)
  (declare (ignore fd)))
(defun socket-uncork (fd)
  (declare (ignore fd)))

#+really-do-some-sort-of-socket-corking
(progn
  (defun socket-cork (fd)
    (setsockopt-int fd +IPPROTO_TCP+ +TCP_CORK+ 1))
  
  (defun socket-uncork (fd)
    (setsockopt-int fd +IPPROTO_TCP+ +TCP_CORK+ 0)))



(def-simple-syscall socket
    (domain :int)
  (type :int)
  (protocol :int))

(def-simple-syscall bind
    (sockfd :int)
  (addr :pointer)
  (addrlen :int))

(def-simple-syscall connect
    (sockfd :int)
  (addr :pointer)
  (addrlen :int))


(def-simple-syscall listen
    (sockfd :int)
  (backlog :int))

(cffi:defcfun htons
    :uint16
  (port :uint16))

(cffi:defcfun inet_pton
    :int
  (af :int)
  (src :string)
  (dst :pointer))

(cffi:defcfun inet_ntop
    :string
  (af :int)
  (src :pointer)
  (dst :pointer)
  (cnt :int))

(def-simple-syscall setsockopt
    (fd :int)
  (level :int)
  (optname :int)
  (optval :pointer)
  (optlen :int))

(defun setsockopt-int (fd level optname value)
  (cffi:with-foreign-object (on :int)
    (setf (cffi:mem-ref on :int) value)
    (syscall-setsockopt fd level optname
			on (cffi:foreign-type-size :int))))
  

#+i-want-really-slow-functions-for-some-reason
(defun sockaddr-address-string (sa)
  (cffi:with-foreign-pointer-as-string (str 100 str-size)
    (unless (inet_ntop (cffi:foreign-slot-value sa 'sockaddr_in 'family)
		       (cffi:foreign-slot-pointer sa 'sockaddr_in 'addr)
		       str
		       str-size)
      (error "Cannot convert address: ~A" (strerror +syscall-error-number+)))))

#+mopoko-old-sockaddr-address-string
(defun sockaddr-address-string (sa)
  (declare (optimize speed))
  (let ((addr (cffi:foreign-slot-value sa 'sockaddr_in 'addr)))
    #.`(strcat ,@(loop for i from 0 below 4 unless (= i 0) collect "." collect `(ldb (byte 8 (* 8 ,i)) addr)))))


(let ((octet-to-string (make-array 256 :element-type 'simple-string :initial-contents (loop for i from 0 below 256 collect (princ-to-string i)))))
  (defun sockaddr-address-string (sa)
    (declare (optimize speed (safety 0)))
    (let ((addr (cffi:foreign-slot-value sa 'sockaddr_in 'addr)))
      #.`(strcat ,@(loop for i below 4 unless (= i 0) collect "." collect `(the simple-string (aref octet-to-string (ldb (byte 8 (* 8 ,i)) addr) )))))))

(defun new-socket-helper (&key 
			  port 
			  address 
			  socket-family
			  socket-type
			  action)
  (let ((fd (syscall-socket socket-family socket-type 0)))
    (signal-protect 
	(let ((network-port (htons port)))
	  (setsockopt-int fd +SOL_SOCKET+ +SO_REUSEADDR+ 1)
	  (with-foreign-object-and-slots ((addr port family) sa sockaddr_in)
	    (setf family socket-family)
	    (cffi:with-foreign-string (src address)
	      (when (<= (inet_pton socket-family src 
				   (cffi:foreign-slot-pointer sa 'sockaddr_in 'addr)) 0)
		(error "Internet address is not valid: ~A" address)))
	    (setf port network-port)
	    (funcall action fd sa (cffi:foreign-type-size 'sockaddr_in))) 
	  (set-fd-nonblock fd)
	  fd)
      (syscall-close fd))))

(defun make-listen-socket (&rest args)
  (apply 'new-socket-helper :action 
	 (lambda(fd sa len)
	   (syscall-bind fd sa len)
	   (syscall-listen fd 1024))
	 args))

(defun make-bind-socket (&rest args)
  (apply 'new-socket-helper :action 
	 (lambda(fd sa len)
	   (syscall-bind fd sa len))
	 args))

(defun make-connect-socket (&rest args)
  (apply 'new-socket-helper 
	 :action 'syscall-connect
	 args))

(defconstant +max-iovecs+ 1024)

(cffi:defcstruct iovec
    (base :pointer)
  (len :uint32))

(cffi:defcstruct timeval
  (sec :uint32)
  (usec :uint32))

(def-simple-syscall gettimeofday 
    (tv :pointer) 
  (tz :pointer))

(defconstant +unix-epoch-to-universal-time-offset+ 2208988800)

(defstruct precise-time
  sec
  usec)

(defun get-precise-time ()
  (with-foreign-object-and-slots ((sec usec) tv timeval)
    (syscall-gettimeofday tv (cffi:null-pointer))
    (make-precise-time :sec (+ sec +unix-epoch-to-universal-time-offset+) :usec usec)))

(my-defun precise-time 'print-object (stream)
  (if *print-readably*
      (call-next-method)
      (format stream "~D.~6,'0D" (my sec) (my usec))))

(my-defun precise-time after (old-time)
  (check-type old-time precise-time)
  (let ((usec (- (my usec) (its usec old-time))))
    (let ((one-over (if (> 0 usec) 1 0)))
      (make-precise-time :sec (- (my sec) (its sec old-time) one-over) :usec (+ usec (* 1000000 one-over))))))


(def-simple-syscall epoll_create
    (size :int))

(def-simple-syscall epoll_wait
    (epfd :int)
  (events :pointer)
  (maxevents :int)
  (timeout :int))

(def-simple-syscall epoll_ctl
    (epfd :int)
  (op :int)
  (fd :int)
  (event :pointer))

(defconstant +POLLIN+ #x001)
(defconstant +POLLPRI+ #x002)
(defconstant +POLLOUT+ #x004)
(defconstant +POLLRDNORM+ #x040)
(defconstant +POLLRDBAND+ #x080)
(defconstant +POLLWRNORM+ #x100)
(defconstant +POLLWRBAND+ #x200)
(defconstant +POLLRDHUP+ #x2000)
(defconstant +POLLMSG+ #x400)
(defconstant +POLLERR+ #x008)
(defconstant +POLLHUP+ #x010)
(defconstant +EPOLLONESHOT+ #.(ash 1 30))
(defconstant +EPOLLET+ #.(ash 1 31))

(defconstant +EPOLL_CTL_ADD+ 1)
(defconstant +EPOLL_CTL_DEL+ 2)
(defconstant +EPOLL_CTL_MOD+ 3)

(cffi:defcunion epoll-data
  (ptr :pointer)
  (fd :int)
  (u32 :uint32)
  (u64 :uint64))

(cffi:defcstruct epoll-event
  (events :uint32)
  (data epoll-data))
