;;;; -*- Mode: Lisp -*-
;;;; $Id: udp-client.lisp 900 2011-06-30 16:30:07Z binghe $
;;;; UDP Client Support for LispWorks

(in-package :comm+)

(defun initialize-dynamic-sockaddr (hostname service protocol)
  #+(or lispworks4 lispworks5 lispworks6.0)
  (let ((server-addr (fli:allocate-dynamic-foreign-object
                      :type '(:struct sockaddr_in))))
    (values (initialize-sockaddr_in 
             server-addr 
             *socket_af_inet*
             hostname
             service protocol)
            *socket_af_inet*
            server-addr
            (fli:pointer-element-size server-addr)))
  #-(or lispworks4 lispworks5 lispworks6.0)
  (progn
    (when (stringp hostname)
      (let ((resolved-hostname (comm:get-host-entry hostname :fields '(:address))))
        (unless resolved-hostname
          (return-from initialize-dynamic-sockaddr :unknown-host))
        (setq hostname resolved-hostname)))
    (if (or (null hostname)
            (integerp hostname)
            (comm:ipv6-address-p hostname))
        (let ((server-addr (fli:allocate-dynamic-foreign-object
                            :type '(:struct lw-sockaddr))))
          (multiple-value-bind (error family)
              (initialize-sockaddr_in 
               server-addr 
               hostname
               service protocol)
            (values error family
                    server-addr
                    (if (eql family *socket_af_inet*)
                        (fli:size-of '(:struct sockaddr_in))
                      (fli:size-of '(:struct sockaddr_in6))))))
      :bad-host)))

(defun open-udp-socket (&key errorp local-address (local-port #+mswindows 0 #-mswindows nil)
                             (address-family *socket_af_inet*)
                             read-timeout reuse-address)
  "Open a unconnected UDP socket.
   For binding on address ANY(*), just not set LOCAL-ADDRESS (NIL),
   for binding on random free unused port, set LOCAL-PORT to 0."

  ;; Note: move (ensure-sockets) here to make sure delivered applications
  ;; correctly have networking support initialized.
  ;;
  ;; Following words was from Martin Simmons, forwarded by Camille Troillard:

  ;; Calling comm::ensure-sockets at load time looks like a bug in Lispworks-udp
  ;; (it is too early and also unnecessary).

  ;; The LispWorks comm package calls comm::ensure-sockets when it is needed, so I
  ;; think open-udp-socket should probably do it too.  Calling it more than once is
  ;; safe and it will be very fast after the first time.
  #+mswindows (ensure-sockets)

  (let ((socket-fd (socket address-family *socket_sock_dgram* *socket_pf_unspec*)))
    (if socket-fd
      (progn
        (when read-timeout
          (setf (socket-receive-timeout socket-fd) read-timeout))
        (when reuse-address
          (setf (socket-reuse-address socket-fd) reuse-address))
        (if local-port
          (progn ;; bind to local address/port if specified.
            (fli:with-dynamic-foreign-objects ()
              (multiple-value-bind (error local-address-family client-addr client-addr-length)
                  (initialize-dynamic-sockaddr local-address local-port "udp")
                (if (or error (not (eql address-family local-address-family)))
                  (error "cannot resolve hostname ~S, service ~S: ~A"
                         local-address local-port (or error "address family mismatch"))
                  (if (bind socket-fd
                            (fli:copy-pointer client-addr :type '(:struct sockaddr))
                            client-addr-length)
                    ;; success, return socket fd
                    (make-inet-datagram socket-fd)
                    (progn ;; fail, close socket and return nil
                      (close-socket socket-fd)
                      (when errorp
                        (raise-socket-error "cannot bind to local"))))))))
          (make-inet-datagram socket-fd)))
      (when errorp
        (raise-socket-error "cannot create socket")))))

(defmacro with-udp-socket ((socket &rest options) &body body)
  `(let ((,socket (open-udp-socket ,@options)))
     (unwind-protect
         (progn ,@body)
       (close-datagram ,socket))))

(defvar *inet-message-send-buffer*
  (make-array +max-udp-message-size+
              :element-type '(unsigned-byte 8)
              :allocation :static))

(defvar *inet-message-send-lock* (mp:make-lock))

(defmethod send-message ((socket inet-datagram) buffer &key (length (length buffer)) host service)
  "Send message to a socket, using sendto()/send()"
  (declare (type sequence buffer))
  (flet ((send-it (client-addr client-addr-length)
           (mp:with-lock (*inet-message-send-lock*)
             (let ((socket-fd (socket-datagram-socket socket))
                   (message *inet-message-send-buffer*))
               (replace message buffer :end2 length)
               (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
                 (if client-addr
                     (%sendto socket-fd ptr (min length +max-udp-message-size+) 0
                              (fli:copy-pointer client-addr :type '(:struct sockaddr))
                              client-addr-length)
                   (%send socket-fd ptr (min length +max-udp-message-size+) 0)))))))
    (if (and host service)
      (fli:with-dynamic-foreign-objects ()
        (multiple-value-bind (error address-family client-addr client-addr-length)
            (initialize-dynamic-sockaddr host service "udp")
          (declare (ignore address-family))
          (if error
              (error "cannot resolve hostname ~S, service ~S: ~A"
                     host service error)
            (send-it client-addr client-addr-length))))
      (send-it nil nil))))

(defvar *inet-message-receive-buffer*
  (make-array +max-udp-message-size+
              :element-type '(unsigned-byte 8)
              :allocation :static))

(defvar *inet-message-receive-lock* (mp:make-lock))

(defmethod receive-message ((socket inet-datagram) &key buffer (length (length buffer))
                            read-timeout (max-buffer-size +max-udp-message-size+))
  "Receive message from socket, read-timeout is a float number in seconds.

   This function will return 4 values:
   1. receive buffer
   2. number of receive bytes
   3. remote address
   4. remote port"
  (declare (type socket-datagram socket)
           (type sequence buffer))
  (let ((message *inet-message-receive-buffer*)
        (socket-fd (socket-datagram-socket socket)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_in))
                                       (len :int
					    #-(or lispworks3 lispworks4 lispworks5.0)
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_in))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        ;; setup new read timeout
        (when read-timeout
          (setf (socket-receive-timeout socket-fd) read-timeout))
        (mp:with-lock (*inet-message-receive-lock*)
          (let ((n (%recvfrom socket-fd ptr max-buffer-size 0
                              (fli:copy-pointer client-addr :type '(:struct sockaddr))
                              len)))
            (if (plusp n)
              (values (if buffer
                        (replace buffer message
                                 :end1 (min length max-buffer-size)
                                 :end2 (min n max-buffer-size))
                        (subseq message 0 (min n max-buffer-size)))
                      (min n max-buffer-size)
                      (ntohl (fli:foreign-slot-value
                              (fli:foreign-slot-value client-addr
                                                      'sin_addr
                                                      :object-type '(:struct sockaddr_in)
                                                      :type '(:struct in_addr)
                                                      :copy-foreign-object nil)
                              's_addr
                              :object-type '(:struct in_addr)))
                      (ntohs (fli:foreign-slot-value client-addr
                                                     'sin_port
                                                     :object-type '(:struct sockaddr_in)
                                                     :type '(:unsigned :short))))
              (values nil n 0 0))))))))

(defun connect-to-udp-server (hostname service &key errorp
                                       local-address local-port read-timeout)
  "Something like CONNECT-TO-TCP-SERVER"
  (let ((socket (open-udp-socket :errorp errorp
                                 :local-address local-address
                                 :local-port local-port
                                 :read-timeout read-timeout)))
    (if socket
      (let ((socket-fd (socket-datagram-socket socket)))
        (fli:with-dynamic-foreign-objects ()
          ;; connect to remote address/port
          (multiple-value-bind (error address-family server-addr server-addr-length)
              (initialize-dynamic-sockaddr hostname service "udp")
            (declare (ignore address-family))
            (if error
              (error "cannot resolve hostname ~S, service ~S: ~A"
                     hostname service error)
              (if (connect socket-fd
                           (fli:copy-pointer server-addr :type '(:struct sockaddr))
                           server-addr-length)
                ;; success, return socket fd
                socket
                ;; fail, close socket and return nil
                (progn
                  (close-datagram socket)
                  (when errorp (raise-socket-error "cannot connect"))))))))
      (when errorp
        (error (raise-socket-error "cannot create socket"))))))

(defmacro with-connected-udp-socket ((socket &rest options) &body body)
  `(let ((,socket (connect-to-udp-server ,@options)))
     (unwind-protect
         (progn ,@body)
       (close-datagram ,socket))))

(defun open-udp-stream (hostname service &key (direction :io)
                                 (element-type 'base-char)
                                 errorp read-timeout
                                 local-address local-port)
  "Something like OPEN-TCP-STREAM"
  (let* ((socket (connect-to-udp-server hostname service
                                        :errorp errorp
                                        :local-address local-address
                                        :local-port local-port))
         (socket-fd (socket-datagram-socket socket)))
    (make-instance 'socket-stream
                   :socket socket-fd
                   :direction direction
                   :element-type element-type
                   :read-timeout read-timeout)))

(defmacro with-udp-stream ((stream &rest options) &body body)
  `(let ((,stream (open-udp-stream ,@options)))
     (unwind-protect
         (progn ,@body)
       (close ,stream))))
