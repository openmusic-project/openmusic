;;;; -*- Mode: Lisp -*-
;;;; $Id: defsys.lisp 659 2008-11-22 12:45:13Z binghe $
;;;; System Definition for LispWorks UDP

(in-package :cl-user)

;;; Load COMM package
(require "comm")

#+(and lispworks4 win32)
(pushnew :mswindows *features*)

(defsystem lispworks-udp
  (:optimize ((safety 3) (debug 3)))
  :members (package
            rtt
            lispworks-udp
            class
	    #-mswindows
            wait-for-input
	    condition
            multicast
            udp-client
            udp-server
            rtt-client
	    #-mswindows
            unix
	    #-mswindows
            unix-server
            #-mswindows
            icmp)
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))
