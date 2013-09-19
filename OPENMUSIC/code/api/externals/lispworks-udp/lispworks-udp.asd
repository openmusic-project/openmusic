;;;; -*- Mode: Lisp -*-
;;;; $Id: lispworks-udp.asd 659 2008-11-22 12:45:13Z binghe $
;;;; System Definition for LispWorks UDP

(in-package :asdf)

;;; Load COMM package
(require "comm")

#+(and lispworks4 win32)
(pushnew :mswindows *features*)

(defsystem lispworks-udp
  :description "UDP support for LispWorks"
  :license "MIT"
  :version "4.1"
  :author "Chun Tian (binghe) <binghe.lisp@gmail.com>"
  :serial t
  :components ((:file "package")
               (:file "rtt")
               (:file "lispworks-udp")
               (:file "class")
               #-mswindows
               (:file "wait-for-input")
	       (:file "condition")
               (:file "multicast")
               (:file "udp-client")
               (:file "udp-server")
               (:file "rtt-client")
               #-mswindows
               (:file "unix")
               #-mswindows
               (:file "unix-server")
               #-mswindows
               (:file "icmp")))
