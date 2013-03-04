;;;; -*- Mode: Lisp -*-
;;;; $Id: interface.lisp 573 2008-10-09 09:52:35Z binghe $

(in-package :comm+)

(defconstant +IFF_UP+          #x1    "interface is up")
(defconstant +IFF_BROADCAST+   #x2    "broadcast address valid")
(defconstant +IFF_MULTICAST+   #x8000 "supports multicast")
(defconstant +IFF_LOOPBACK+    #x8    "is a loopback net")
(defconstant +IFF_POINTOPOINT+ #x10   "interface is point-to-point link")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +IFNAMSIZ+ 16)

  ;; (fli:size-of '(:struct ifreq)) = 32
  (fli:define-c-struct ifreq
    (ifr_name (:ef-mb-string
               :limit +IFNAMSIZ+
               :external-format :ascii
               :null-terminated-p t))
    (ifr_ifru (:union
               (ifru_addr      (:struct sockaddr))
               (ifru_dstaddr   (:struct sockaddr))
               (ifru_broadaddr (:struct sockaddr))
               (ifru_flags     :short)
               (ifru_metric    :int)
               (ifru_data      (:pointer :void)))))

  ;; (fli:size-of '(:struct ifconf)) = 8
  (fli:define-c-struct ifconf
    (ifc_len :int)
    (ifc_ifcu (:union
               (ifcu_buf (:pointer :void))
               (ifcu_req (:pointer (:struct ifreq)))))))

;;; #define SIOCGIFCONF     _IOWR('i', 36, struct ifconf)   /* get ifnet list */
;;; #define _IOWR(g,n,t)    _IOC(IOC_INOUT, (g), (n), sizeof(t))
;;; #define _IOC(inout,group,num,len) \
;;;         (inout | ((len & IOCPARM_MASK) << 16) | ((group) << 8) | (num))
;;; #define IOCPARM_MASK    0x1fff          /* parameter length, at most 13 bits */
;;; #define IOC_INOUT       (IOC_IN|IOC_OUT)
;;; #define IOC_OUT         (unsigned long)0x40000000
;;; #define IOC_IN          (unsigned long)0x80000000

(eval-when (:compile-toplevel)
  (defconstant +IOC_IN+       #x80000000)
  (defconstant +IOC_OUT+      #x40000000)
  (defconstant +IOC_INOUT+    (logior +IOC_IN+ +IOC_OUT+))
  (defconstant +IOCPARM_MASK+ #x1fff)
  (defun _IOWR (g n type)
    (_IOC +IOC_INOUT+ g n (fli:size-of type)))
  (defun _IOC (inout group num len)
    (logior inout
            (ash (logand len +IOCPARM_MASK+) 16)
            (ash group 8)
            num)))

(defconstant +SIOCGIFCONF+
  #+linux #x8912
  #-linux #.(_IOWR (char-code #\i) 36 '(:struct ifconf))
  "_IOWR('i', 36, struct ifconf), get ifnet list")

(defun list-all-interfaces ()
  (with-udp-socket (socket :errorp t)
    (let ((socket-fd (socket-datagram-socket socket))
          (len (* 100 (fli:size-of '(:struct ifreq)))))
      (fli:with-dynamic-foreign-objects ((ifc     (:struct ifconf))
                                         (ifr     (:pointer (:struct ifreq)))
                                         (ifrcopy (:struct ifreq))
                                         (sinptr  (:pointer (:struct sockaddr_in)))
                                         (buf     (:pointer :byte)))
        (setf (fli:dereference buf) (fli:allocate-dynamic-foreign-object :type :byte
                                                                         :nelems len)
              (fli:foreign-slot-value ifc 'ifc_len) len
              (fli:foreign-slot-value
               (fli:foreign-slot-pointer ifc 'ifc_ifcu)
               'ifcu_buf)
              buf)
        (ioctl socket-fd +SIOCGIFCONF+ ifc)
        (fli:foreign-slot-value ifc 'ifc_len)))))
