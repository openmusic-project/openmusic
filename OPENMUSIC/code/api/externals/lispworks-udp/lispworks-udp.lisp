;;;; -*- Mode: Lisp -*-
;;;; $Id: lispworks-udp.lisp 900 2011-06-30 16:30:07Z binghe $

(in-package :comm+)

(defconstant +max-udp-message-size+ 65536)

;;;; Below is something we have to define (others already in COMM package)
;;;; binghe: my design goal is to use what COMM already have, and no C code.

(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")

(defconstant *sockopt_ipproto_udp* 17)

(defconstant *sockopt_so_rcvtimeo*
  #-linux #x1006
  #+linux 20
  "Socket receive timeout")

(fli:define-c-struct timeval
  (tv-sec :long)
  (tv-usec :long))

;;; ssize_t
;;; recvfrom(int socket, void *restrict buffer, size_t length, int flags,
;;;          struct sockaddr *restrict address, socklen_t *restrict address_len);
(fli:define-foreign-function (%recvfrom "recvfrom" :source)
    ((socket :int)
     (buffer (:pointer (:unsigned :byte)))
     (length :int)
     (flags :int)
     (address (:pointer (:struct sockaddr)))
     (address-len (:pointer :int)))
  :result-type :int)

;;; ssize_t
;;; sendto(int socket, const void *buffer, size_t length, int flags,
;;;        const struct sockaddr *dest_addr, socklen_t dest_len);
(fli:define-foreign-function (%sendto "sendto" :source)
    ((socket :int)
     (buffer (:pointer (:unsigned :byte)))
     (length :int)
     (flags :int)
     (address (:pointer (:struct sockaddr)))
     (address-len :int))
  :result-type :int)

#-mswindows
(defmethod (setf socket-receive-timeout) (seconds (socket-fd integer))
  "Set socket option: RCVTIMEO, argument seconds can be a float number"
  (declare (type number seconds))
  (multiple-value-bind (sec usec) (truncate seconds)
    (fli:with-dynamic-foreign-objects ((timeout (:struct timeval)))
      (fli:with-foreign-slots (tv-sec tv-usec) timeout
        (setf tv-sec sec
              tv-usec (truncate (* 1000000 usec)))
        (if (zerop (setsockopt socket-fd
                               *sockopt_sol_socket*
                               *sockopt_so_rcvtimeo*
                               (fli:copy-pointer timeout
                                                 :type '(:pointer :void))
                               (fli:size-of '(:struct timeval))))
            seconds)))))

#+mswindows
(defmethod (setf socket-receive-timeout) (seconds (socket-fd integer))
  "Set socket option: RCVTIMEO, argument seconds can be a float number.
   On mswindows, you must bind the socket before use this function."
  (declare (type number seconds))
  (fli:with-dynamic-foreign-objects ((timeout :int))
    (setf (fli:dereference timeout)
          (truncate (* 1000 seconds)))
    (if (zerop (setsockopt socket-fd
                           *sockopt_sol_socket*
                           *sockopt_so_rcvtimeo*
                           (fli:copy-pointer timeout
                                             :type '(:pointer :char))
                           (fli:size-of :int)))
        seconds)))

#-mswindows
(defmethod socket-receive-timeout ((socket-fd integer))
  "Get socket option: RCVTIMEO, return value is a float number"
  (fli:with-dynamic-foreign-objects ((timeout (:struct timeval))
                                     (len :int))
    (getsockopt socket-fd
                *sockopt_sol_socket*
                *sockopt_so_rcvtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (fli:with-foreign-slots (tv-sec tv-usec) timeout
      (float (+ tv-sec (/ tv-usec 1000000))))))

#+mswindows
(defmethod socket-receive-timeout ((socket-fd integer))
  "Get socket option: RCVTIMEO, return value is a float number"
  (fli:with-dynamic-foreign-objects ((timeout :int)
                                     (len :int))
    (getsockopt socket-fd
                *sockopt_sol_socket*
                *sockopt_so_rcvtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (float (/ (fli:dereference timeout) 1000))))

(defun get-last-error ()
  #+mswindows
  (wsa-get-last-error)
  #-mswindows
  (errno-value))

(defun raise-socket-error (reason &rest args)
  (declare (type string reason))
  (error 'socket-error
         :format-string (format nil "ERROR ~D: ~A" (get-last-error) reason)
         :format-arguments args))
