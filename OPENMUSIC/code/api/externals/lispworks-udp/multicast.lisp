;;;; -*- Mode: Lisp -*-
;;;; $Id: multicast.lisp 654 2008-11-21 16:36:01Z binghe $

;;; UDP Multicat support for LispWorks

(in-package :comm+)

(defconstant *sockopt_ipproto_ip* 0 "Dummy protocol")

(defconstant *sockopt_ip_multicast_if*
  #-linux  9 #+linux 32
  "specify default interface for outgoing multicasts")

(defconstant *sockopt_ip_multicast_ttl*
  #-linux 10 #+linux 33
  "specify TTL for outgoing multicasts")

(defconstant *sockopt_ip_multicast_loop*
  #-linux 11 #+linux 34
  "enable or disable loopback of outgoing multicasts")

(defconstant *sockopt_ip_add_membership*
  #-linux 12 #+linux 35
  "join a multicast group")

(defconstant *sockopt_ip_drop_membership*
  #-linux 13 #+linux 36
  "leave a multicast group")

;; (fli:size-of '(:struct ip_mreq)) = 8
(fli:define-c-struct ip_mreq
  (imr_multiaddr (:struct in_addr))
  (imr_interface (:struct in_addr)))

(defgeneric mcast-join (socket address &key))

(defmethod mcast-join ((socket inet-datagram) address &key (interface 0) errorp)
  (mcast-join (socket-datagram-socket socket) address
              :interface interface :errorp errorp))
  
(defmethod mcast-join ((socket-fd integer) address &key (interface 0) errorp)
  "Join the multicast group (address) on a interface"
  (declare (type (or string integer) address interface))
  (fli:with-dynamic-foreign-objects ((mreq (:struct ip_mreq))
                                     (sock-addr (:struct sockaddr_in)))

    ;; 1. set multiaddr
    (initialize-sockaddr_in sock-addr *socket_af_inet* address 0 "udp")
    (setf (fli:foreign-slot-value
           (fli:foreign-slot-value mreq 'imr_multiaddr
                                   :object-type '(:struct ip_mreq)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr))

          (fli:foreign-slot-value
           (fli:foreign-slot-value sock-addr 'sin_addr
                                   :object-type '(:struct sockaddr_in)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr)))

    ;; 2. set interface
    (initialize-sockaddr_in sock-addr *socket_af_inet* interface 0 "udp")
    (setf (fli:foreign-slot-value
           (fli:foreign-slot-value mreq 'imr_interface
                                   :object-type '(:struct ip_mreq)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr))

          (fli:foreign-slot-value
           (fli:foreign-slot-value sock-addr 'sin_addr
                                   :object-type '(:struct sockaddr_in)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr)))

    ;; 3. call setsockopt()
    (let ((reply (setsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_add_membership*
                             (fli:copy-pointer mreq :type '(:pointer :char))
                             (fli:size-of '(:struct ip_mreq)))))
      (or (zerop reply)
          (if errorp
              (raise-socket-error "cannot join to multicast group: ~A" address)
            (values nil (get-last-error)))))))

(defgeneric mcast-leave (socket address &key))

(defmethod mcast-leave ((socket inet-datagram) address &key errorp)
  (mcast-leave (socket-datagram-socket socket) address :errorp errorp))

(defmethod mcast-leave ((socket-fd integer) address &key errorp)
  "Leave the multicast group (address)"
  (declare (type (or string integer) address))
  (fli:with-dynamic-foreign-objects ((mreq (:struct ip_mreq))
                                     (sock-addr (:struct sockaddr_in)))
    ;; 1. set multiaddr
    (initialize-sockaddr_in sock-addr *socket_af_inet* address 0 "udp")
    (setf (fli:foreign-slot-value
           (fli:foreign-slot-value mreq 'imr_multiaddr
                                   :object-type '(:struct ip_mreq)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr))

          (fli:foreign-slot-value
           (fli:foreign-slot-value sock-addr 'sin_addr
                                   :object-type '(:struct sockaddr_in)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr)))

    ;; 2. set interface to zero (INADDR_ANY)
    (setf (fli:foreign-slot-value
           (fli:foreign-slot-value mreq 'imr_interface
                                   :object-type '(:struct ip_mreq)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr))
          0)

    ;; 3. call setsockopt()
    (let ((reply (setsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_drop_membership*
                             (fli:copy-pointer mreq :type '(:pointer :char))
                             (fli:size-of '(:struct ip_mreq)))))
      (or (zerop reply)
          (if errorp
              (raise-socket-error "cannot leave the multicast group: ~A" address)
            (values nil (get-last-error)))))))

(defgeneric mcast-interface (socket))

(defmethod mcast-interface ((socket inet-datagram))
  (mcast-interface (socket-datagram-socket)))

(defmethod mcast-interface ((socket-fd integer))
  (fli:with-dynamic-foreign-objects ((in-addr (:struct in_addr))
                                     (len :int))
    (let ((reply (getsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_if*
                             (fli:copy-pointer in-addr :type '(:pointer :char))
                             len)))
      (when (zerop reply)
        (values (ntohl (fli:foreign-slot-value in-addr 's_addr
                                               :object-type '(:struct in_addr)))
                t)))))

(defgeneric mcast-loop (socket))

(defmethod mcast-loop ((socket inet-datagram))
  (mcast-loop (socket-datagram-socket socket)))

(defmethod mcast-loop ((socket-fd integer))
  (fli:with-dynamic-foreign-objects ((flag (:unsigned :char))
                                     (len :int))
    (let ((reply (getsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_loop*
                             (fli:copy-pointer flag :type '(:pointer :char))
                             len)))
      (when (zerop reply)
        (values (fli:dereference flag) t)))))

(defgeneric mcast-ttl (socket))

(defmethod mcast-ttl ((socket inet-datagram))
  (mcast-ttl (socket-datagram-socket socket)))

(defmethod mcast-ttl ((socket-fd integer))
  (fli:with-dynamic-foreign-objects ((ttl (:unsigned :char))
                                     (len :int))
    (let ((reply (getsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_ttl*
                             (fli:copy-pointer ttl :type '(:pointer :char))
                             len)))
      (when (zerop reply)
        (values (fli:dereference ttl) t)))))

(defgeneric (setf mcast-ttl) (ttl socket))

(defmethod (setf mcast-ttl) (ttl (socket inet-datagram))
  (setf (mcast-ttl (socket-datagram-socket socket)) ttl))

(defmethod (setf mcast-ttl) ((ttl integer) (socket-fd integer))
  (declare (type (integer 0) ttl))
  (fli:with-dynamic-foreign-objects ((%ttl (:unsigned :char)))
    (setf (fli:dereference %ttl) ttl)
    (let ((reply (setsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_ttl*
                             (fli:copy-pointer %ttl :type '(:pointer :char))
                             (fli:size-of '(:unsigned :char)))))
      (when (zerop reply)
        (values ttl t)))))

(defgeneric (setf mcast-interface) (interface socket))

(defmethod (setf mcast-interface) (interface (socket inet-datagram))
  (setf (mcast-interface (socket-datagram-socket socket)) interface))

(defmethod (setf mcast-interface) ((interface string) (socket-fd integer))
  (setf (mcast-interface socket-fd)
        (get-host-entry interface :fields '(:address))))

(defmethod (setf mcast-interface) ((interface integer) (socket-fd integer))
  (declare (type (or string integer) interface))
  (fli:with-dynamic-foreign-objects ((in-addr (:struct in_addr)))
    (setf (fli:foreign-slot-value in-addr 's_addr
                                  :object-type '(:struct in_addr))
          (htonl interface))
    (let ((reply (setsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_if*
                             (fli:copy-pointer in-addr :type '(:pointer :char))
                             (fli:size-of '(:struct in_addr)))))
      (when (zerop reply)
        (values interface t)))))

(defgeneric (setf mcast-loop) (flag socket))

(defmethod (setf mcast-loop) (flag (socket inet-datagram))
  (setf (mcast-loop (socket-datagram-socket socket))
        (if flag 1 0)))

(defmethod (setf mcast-loop) (flag (socket-fd integer))
  (setf (mcast-loop socket-fd)
        (if flag 1 0)))

(defmethod (setf mcast-loop) ((flag integer) (socket-fd integer))
  (declare (type (integer 0 1) flag))
  (fli:with-dynamic-foreign-objects ((%flag (:unsigned :char)))
    (setf (fli:dereference %flag) flag)
    (let ((reply (setsockopt socket-fd
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_loop*
                             (fli:copy-pointer %flag :type '(:pointer :char))
                             (fli:size-of '(:unsigned :char)))))
      (when (zerop reply)
        (values flag t)))))
