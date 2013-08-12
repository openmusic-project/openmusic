;; -*- mode: lisp -*-
;;
;; Examples of how to send OSC messages. ..
;; 
;; Copyright (C) 2004 FoAM vzw
;;
;; Authors
;;  - nik gaffney <nik@f0.am>
;;
;;;;; ;; ;     ;    ;;    ;             ;

;;;;;::::: :   : : ;     ;; ;       ; ;    ;         ;     ;
;;
;; Commentry
;;
;;  These examples are currently sbcl specific, but should be easily ported to
;;  work with trivial-sockets, acl-compat or something similar. They should be
;;  able to explain enough to get you started. ..
;;
;;  eg. listen on port 6667 for incoming msgs 
;;
;;   (osc-listen 6667) 
;;
;;  eg. listen on port 6667 and send to 10.0.89:6668
;;     note the ip# is formatted as a vector
;;
;;   (osc-reflector 6667 #(10 0 0 89) 6668)
;;
;;;;;:::;;: ;     ; ;::: ;     ;; ;;     ;        ;;      ;

(require :sb-bsd-sockets)
(use-package :osc)
(use-package :sb-bsd-sockets)


(defun osc-listen (port) 
  "a basic test function which attempts to decode an osc message a given port."
  (let ((s (make-udp-socket))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (socket-bind s #(0 0 0 0) port)
    (format t "listening on localhost port ~A~%~%" port)
    (unwind-protect 
	(loop do
	      (socket-receive s buffer nil)
	      (format t "receiveded -=> ~S~%" (osc:decode-bundle buffer)))
      (when s (socket-close s))))) 


(defun osc-reflector (listen-port send-ip send-port) 
  "reflector.. . listens on a given port and sends out on another
   note ip#s need to be in the format #(127 0 0 1) for now.. ."
  (let ((in (make-udp-socket))
        (out (make-udp-socket))
	(buffer (make-sequence '(vector (unsigned-byte 8)) 512)))
    (socket-bind in #(0 0 0 0) listen-port)
    (socket-connect out send-ip send-port)
    (let ((stream 
	   (socket-make-stream
	    out :input t :output t 
	    :element-type '(unsigned-byte 8) :buffering :full)))
      (unwind-protect 
	  (loop do 
		(socket-receive in buffer nil)
		(let ((oscuff (osc:decode-bundle buffer)))
		  (format t "glonked -=> message with ~S~% arg(s)" (length oscuff))
		  (write-stream-t1 stream oscuff)))
	(when in (socket-close in)) 
 	(when out (socket-close out)))))) 


(defun make-udp-socket()
  (make-instance 'inet-socket :type :datagram :protocol :udp))

(defun write-stream-t1 (stream osc-message) 
  "writes a given message to a stream. keep in mind that when using a buffered 
   stream any funtion writing to the stream should  call (finish-output stream)
   after it sends the mesages,. ."
  (write-sequence 
   (osc:encode-message "/bzzp" "got" "it" )
   stream)
  (finish-output stream))

(defmacro osc-write-to-stream (stream &body args)
  `(progn (write-sequence (osc:encode-message ,@args) ,stream)
	  (finish-output ,stream)))

;end
