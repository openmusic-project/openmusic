;;;; -*- Mode: Lisp -*-
;;;; $Id: unix.lisp 902 2011-07-29 07:14:02Z binghe $
;;;; UNIX Domain Socket support for LispWorks

;;; NOTE: OPEN-UNIX-STREAM (and only it) is from http://www.bew.org.uk/Lisp/
;;; Original license of UNIX Domain Socket support for LispWorks:

;;; Copyright 2001, Barry Wilkes <bew@bcs.org.uk>
;;; uk.org.bew.comm-ext, an extension to the network interface for LispWorks/Linux
;;; 
;;; uk.org.bew.comm-ext is licensed under the terms of the Lisp Lesser GNU
;;; Public License (http://opensource.franz.com/preamble.html), known as
;;; the LLGPL.  The LLGPL consists of a preamble (see above URL) and the
;;; LGPL.  Where these conflict, the preamble takes precedence. 
;;; uk.org.bew.comm-ext is referenced in the preamble as the "LIBRARY."

(in-package :comm+)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-unix-path-length+ #+darwin 104 #-darwin 108))

;; (fli:size-of '(:struct sockaddr_un)) = #+darwin 106 #-darwin 110
(fli:define-c-struct sockaddr_un
  #+darwin
  (sun_len    (:unsigned :byte))
  (sun_family (:unsigned #+darwin :byte #-darwin :short))
  (sun_path   (:c-array (:unsigned :byte) #.+max-unix-path-length+)))

(defun initialize-sockaddr_un (unaddr family pathname)
  (declare (type (satisfies fli:pointerp) unaddr)
           (type integer family)
           (type (or string pathname) pathname))
  (let ((path (namestring (translate-logical-pathname pathname))))
    (fli:fill-foreign-object unaddr :byte 0)
    (let* ((code (ef:encode-lisp-string path :utf-8))
           (len (length code)))
      (fli:with-foreign-slots (#+darwin sun_len sun_family) unaddr
	#+darwin
        (setf sun_len (+ len 2))
        (setf sun_family family))
      (fli:replace-foreign-array (fli:foreign-slot-pointer unaddr 'sun_path)
                                 code
                                 :start1 0 :end1 (min len +max-unix-path-length+))
      (setf (fli:foreign-aref (fli:foreign-slot-pointer unaddr 'sun_path)
                              (min len (1- +max-unix-path-length+)))
            0)
      (values))))

(defun get-socket-peer-pathname (socket-fd)
  "Get the connect pathname of a unix domain socket, only useful on stream socket"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((sock-addr (:struct sockaddr_un))
                                     (len :int
                                          #-(or lispworks3 lispworks4 lispworks5.0)
                                          :initial-element
                                          (fli:size-of '(:struct sockaddr_un))))
    (fli:fill-foreign-object sock-addr :byte 0)
    (let ((return-code
           (getpeername socket-fd (fli:copy-pointer sock-addr :type '(:struct sockaddr)) len)))
      (when (zerop return-code)
        (fli:convert-from-foreign-string (fli:foreign-slot-pointer sock-addr 'sun_path)
                                         :external-format :utf-8
                                         :null-terminated-p t
                                         :allow-null t)))))

(defun get-socket-pathname (socket-fd)
  "Get the bind pathname of a unix domain socket, only useful on stream socket"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((sock-addr (:struct sockaddr_un))
                                     (len :int
                                          #-(or lispworks3 lispworks4 lispworks5.0)
                                          :initial-element
                                          (fli:size-of '(:struct sockaddr_un))))
    (fli:fill-foreign-object sock-addr :byte 0)
    (let ((return-code
           (getsockname socket-fd (fli:copy-pointer sock-addr :type '(:struct sockaddr)) len)))
      (when (zerop return-code)
        (fli:convert-from-foreign-string (fli:foreign-slot-pointer sock-addr 'sun_path)
                                         :external-format :utf-8
                                         :null-terminated-p t
                                         :allow-null t)))))

(defun open-unix-socket (&key (protocol :datagram)
                              errorp path read-timeout)
  (let ((socket-fd (socket *socket_af_unix*
			   (ecase protocol
                             (:stream *socket_sock_stream*)
                             (:datagram *socket_sock_dgram*))
			   *socket_pf_unspec*)))
    (if socket-fd
      (progn
        (when read-timeout
          (setf (socket-receive-timeout socket-fd) read-timeout))
        (if path
          (progn
            (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_un)))
              (initialize-sockaddr_un client-addr *socket_af_unix* path)
              (ignore-errors
                (delete-file path))
              (if (bind socket-fd
                        (fli:copy-pointer client-addr :type '(:struct sockaddr))
                        (fli:pointer-element-size client-addr))
                ;; success, return socket fd
                (ecase protocol
                  (:stream socket-fd)
                  (:datagram (make-unix-datagram socket-fd path)))
                (progn ;; fail, close socket and return nil
                  (close-socket socket-fd)
                  (when errorp
                    (error 'socket-error
                           :format-string "cannot bind local pathname"))))))
          (ecase protocol
            (:stream socket-fd)
            (:datagram (make-unix-datagram socket-fd)))))
      (when errorp (error 'socket-error
                          :format-string "cannot create socket")))))

(defmacro with-unix-socket ((socket &rest options) &body body)
  `(let ((,socket (open-unix-socket ,@options)))
     (unwind-protect
         (progn ,@body)
       (close-datagram ,socket))))

(defun connect-to-unix-path (pathname &key (protocol :datagram)
                                      errorp read-timeout)
  "Something like CONNECT-TO-TCP-SERVER"
  (declare (type (or pathname string) pathname))
  (let ((socket (open-unix-socket :protocol protocol
                                  :errorp errorp
                                  :read-timeout read-timeout)))
    (if socket
      (let ((socket-fd (ecase protocol
                         (:stream socket)
                         (:datagram (socket-datagram-socket socket)))))
        (fli:with-dynamic-foreign-objects ((server-addr (:struct sockaddr_un)))
          (initialize-sockaddr_un server-addr *socket_af_unix*
                                  (truename pathname))
          (if (connect socket-fd
                       (fli:copy-pointer server-addr :type '(:struct sockaddr))
                       (fli:pointer-element-size server-addr))
            ;; success
            (ecase protocol
              (:stream socket-fd)
              (:datagram socket))
            ;; fail
            (progn
              (ecase protocol
                (:stream (close-socket socket-fd))
                (:datagram (close-datagram socket)))
              (when errorp
                (error 'socket-error
                       :format-string "Cannot connect: ~S."
                       (lw:get-unix-error (lw:errno-value))))))))
        (when errorp
          (error 'socket-error
                 :format-string "Cannot create socket: ~S."
                 (lw:get-unix-error (lw:errno-value)))))))

(defmacro with-connected-unix-socket ((socket &rest options) &body body)
  `(let ((,socket (connect-to-unix-path ,@options)))
     (unwind-protect
         (progn ,@body)
       (close-datagram ,socket))))

(defun open-unix-stream (pathname &key (direction :io)
                                  (element-type 'base-char)
                                  errorp read-timeout)
  "Open a UNIX domain socket stream"
  (declare (type (or pathname string) pathname))
  (let ((socket-fd (connect-to-unix-path pathname
                                         :protocol :stream
                                         :errorp errorp)))
    (if socket-fd
      (make-instance 'comm:socket-stream
                     :socket socket-fd
                     :element-type element-type
                     :direction direction
                     :read-timeout read-timeout))))

(defmacro with-unix-stream ((stream &rest options) &body body)
  `(let ((,stream (open-unix-stream ,@options)))
     (unwind-protect
         (progn ,@body)
       (close ,stream))))

(defvar *unix-message-send-buffer*
  (make-array +max-udp-message-size+
              :element-type '(unsigned-byte 8)
              :allocation :static))

(defvar *unix-message-send-lock* (mp:make-lock))

(defmethod send-message ((socket unix-datagram) buffer &key (length (length buffer)) path)
  "Send message to a socket, using send()/sendto()"
  (declare (type sequence buffer))
  (let ((message *unix-message-send-buffer*)
        (socket-fd (socket-datagram-socket socket)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_un))
                                       (len :int
					    #-(or lispworks3 lispworks4 lispworks5.0)
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_un))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        (mp:with-lock (*unix-message-send-lock*)
          (replace message buffer :end2 length)
          (if path
            (progn
              (initialize-sockaddr_un client-addr *socket_af_unix* path)
              (%sendto socket-fd ptr (min length +max-udp-message-size+) 0
                       (fli:copy-pointer client-addr :type '(:struct sockaddr))
                       (fli:dereference len)))
            (%send socket-fd ptr (min length +max-udp-message-size+) 0)))))))

(defvar *unix-message-receive-buffer*
  (make-array +max-udp-message-size+
              :element-type '(unsigned-byte 8)
              :allocation :static))

(defvar *unix-message-receive-lock* (mp:make-lock))

(defmethod receive-message ((socket unix-datagram) &key buffer (length (length buffer))
                            read-timeout (max-buffer-size +max-udp-message-size+))
  "Receive message from socket, read-timeout is a float number in seconds.

   This function will return 4 values:
   1. receive buffer
   2. number of receive bytes
   3. peer pathname"
  (declare (type socket-datagram socket)
           (type sequence buffer))
  (let ((message *unix-message-receive-buffer*)
        (socket-fd (socket-datagram-socket socket)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_un))
                                       (len :int
					    #-(or lispworks3 lispworks4 lispworks5.0)
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_un))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        ;; setup new read timeout
        (when read-timeout
          (setf (socket-receive-timeout socket-fd) read-timeout))
        (mp:with-lock (*unix-message-receive-lock*)
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
                        (fli:convert-from-foreign-string (fli:foreign-slot-pointer client-addr 'sun_path)
                                                         :external-format :utf-8
                                                         :null-terminated-p t
                                                         :allow-null t))
                (values nil n ""))))))))
