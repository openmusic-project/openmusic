;;;; -*- Mode: Lisp -*-
;;;; $Id: icmp.lisp 666 2008-12-04 17:18:16Z binghe $
;;;; ICMP support for LispWorks

(in-package :comm+)

(defconstant *socket_sock_raw* 3)
(defconstant *sockopt_ipproto_icmp* 1 "control message protocol")
(defconstant *sockopt_ipproto_raw* 255 "raw IP packet")

(defconstant *sockopt_ip_hdrincl*
  #-linux 2 #+linux 3
  "int; header is included with data")

(defclass socket-raw (socket-datagram)
  ((socket :type integer
           :reader socket-raw-socket
           :initarg :socket)))

(defgeneric socket-header-include (socket))
(defgeneric (setf socket-header-include) (flag socket))

(defmethod socket-header-include ((socket socket-raw))
  (socket-header-include (socket-raw-socket socket)))

(defmethod socket-header-include ((socket-fd integer))
  "Get socket option: IP_HDRINCL, return value is 0 or 1"
  (fli:with-dynamic-foreign-objects ((flag :int)
                                     (len :int))
    (let ((reply (getsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_hdrincl*
                             (fli:copy-pointer flag :type '(:pointer :void))
                             len)))
      (when (zerop reply)
        (values (fli:dereference flag) t)))))

(defmethod (setf socket-header-include) (flag (socket socket-raw))
  (setf (socket-header-include (socket-raw-socket socket)) flag))

(defmethod (setf socket-header-include) (flag (socket-fd integer))
  (setf (socket-header-include socket-fd) (if flag 1 0)))

(defmethod (setf socket-header-include) ((flag integer) (socket-fd integer))
  "Set socket option: IP_HDRINCL, flag can be 0 or 1"
  (declare (type (integer 0 1) flag))
  (fli:with-dynamic-foreign-objects ((%flag :int))
    (setf (fli:dereference %flag) flag)
    (let ((reply (setsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_hdrincl*
                             (fli:copy-pointer %flag :type '(:pointer :void))
                             (fli:size-of :int))))
      (when (zerop reply)
        (values flag t)))))

(defun open-icmp-socket ()
  (let ((socket-fd (socket *socket_af_inet* *socket_sock_raw* *sockopt_ipproto_icmp*)))
    socket-fd))
