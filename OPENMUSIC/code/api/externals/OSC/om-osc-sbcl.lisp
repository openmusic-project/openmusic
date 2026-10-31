(in-package :cl-user)

(setf *stdout* #.*standard-output*)

(require :sb-bsd-sockets)

(load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "cl-osc")) :name "osc.asd"))
(asdf:operate 'asdf:load-op 'osc)

(push :osc *features*)

(in-package :om-api)

(export '(
         om-send-osc-bundle
         om-send-osc-message
         om-start-osc-server
         om-stop-osc-server
         om-decode-msg-or-bundle
          ) :om-api)


;;; OSC OVER UDP (SB-BSD-SOCKETS)

(defconstant +max-osc-datagram+ 65507)

(defun %osc-resolve-host (host)
  "IPv4 address vector for HOST; NIL/empty/\"0.0.0.0\" means all interfaces."
  (if (or (null host) (string= host "") (string= host "0.0.0.0"))
      #(0 0 0 0)
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))

(defun write-osc-msg (message datagram)
  (let ((buf (apply #'osc:encode-message message)))
    (sb-bsd-sockets:socket-send datagram buf (length buf))))

;; encode-bundle returns a (vector t); socket-send's etypecase requires ub8.
(defun write-osc-bundle (bundle datagram &optional (time-tag :now))
  (let ((buf (coerce (osc:encode-bundle bundle time-tag)
                     '(simple-array (unsigned-byte 8) (*)))))
    (sb-bsd-sockets:socket-send datagram buf (length buf))))

(defun open-osc-out-stream (host port)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (sb-bsd-sockets:socket-connect sock (%osc-resolve-host host) port)
    sock))

(defun close-osc-stream (datagram)
  (sb-bsd-sockets:socket-close datagram))

;;;================
;;; OM API
;;;================

;;; SEND

(defun om-send-osc-message (port host message)
  (let ((outs (open-osc-out-stream host port)))
    (write-osc-msg message outs)
    (close-osc-stream outs)))

(defun om-send-osc-bundle (port host bundle)
  (let ((outs (open-osc-out-stream host port)))
    (write-osc-bundle bundle outs)
    (close-osc-stream outs)))

;;; RECEIVE

(defun om-decode-msg-or-bundle (msg)
  (osc::decode-message-or-bundle msg))

(defstruct osc-server thread socket (stop-flag nil))

;; The receive thread has no notifier here (LW pops one up), so every error
;; it survives has to reach the log instead of the background debugger.
(defun %osc-log (fmt &rest args)
  (format *error-output* "~&;;; ~?~%" fmt args)
  (finish-output *error-output*))

(defun %osc-server-loop (server function)
  (let ((sock (osc-server-socket server))
        (buf  (make-array +max-osc-datagram+ :element-type '(unsigned-byte 8))))
    (loop until (osc-server-stop-flag server) do
      (block one-packet
        (multiple-value-bind (data len address port)
            (handler-case (sb-bsd-sockets:socket-receive sock buf nil)
              (serious-condition (c)
                (when (osc-server-stop-flag server) (return-from %osc-server-loop))
                (%osc-log "OSC receive error [~A]: ~A" (type-of c) c)
                (sleep 0.01)
                (return-from one-packet)))
          ;; A torn-down socket can return -1 read back as an unsigned length,
          ;; so only a length that fits the buffer is a real datagram. The
          ;; wakeup packet of om-stop-osc-server leaves through the flag test.
          (when (or (osc-server-stop-flag server)
                    (null data) (null len) (not (<= 1 len (length buf))))
            (return-from one-packet))
          (handler-case
              (let ((reply (funcall function (subseq data 0 len))))
                (when reply
                  (let ((out (coerce reply '(simple-array (unsigned-byte 8) (*)))))
                    (sb-bsd-sockets:socket-send sock out (length out)
                                                :address (list address port)))))
            ;; a malformed packet or a bug in the user patch must not kill the server
            (serious-condition (c)
              (%osc-log "OSC receive callback error [~A]: ~A" (type-of c) c))))))))

(defparameter *osc-wakeup-datagram*
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))

(defun %osc-wakeup (sock)
  "Unblock the receive thread with a loopback datagram: the portable stand-in
for the vanilla's 1s :read-timeout, without a platform-dependent SO_RCVTIMEO."
  (multiple-value-bind (address port) (sb-bsd-sockets:socket-name sock)
    (let ((dest (if (equalp address #(0 0 0 0)) #(127 0 0 1) address))
          (out (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
      (unwind-protect
           (progn (sb-bsd-sockets:socket-connect out dest port)
                  (sb-bsd-sockets:socket-send out *osc-wakeup-datagram* 1))
        (sb-bsd-sockets:socket-close out)))))

(defun om-start-osc-server (port host function &optional name)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (%osc-resolve-host host) port)
    (let ((server (make-osc-server :socket sock)))
      (setf (osc-server-thread server)
            (bt:make-thread (lambda () (%osc-server-loop server function))
                            :name (or name (format nil "OSC receiver on ~S ~S" host port))))
      server)))

(defun om-stop-osc-server (server)
  (when server
    (setf (osc-server-stop-flag server) t)
    ;; wake, join, THEN close -- the vanilla kills the process before closing
    ;; the socket (lispworks-udp/udp-server.lisp: stop-udp-server); closing under a blocked recvfrom is
    ;; what left the thread in the background debugger.
    (handler-case (%osc-wakeup (osc-server-socket server))
      (serious-condition (c) (%osc-log "OSC stop wakeup failed [~A]: ~A" (type-of c) c)))
    (let ((thread (osc-server-thread server)))
      (loop repeat 200 while (bt:thread-alive-p thread) do (sleep 0.01))
      (when (bt:thread-alive-p thread)
        (%osc-log "OSC server thread ~S still alive after 2s; closing socket anyway"
                  (bt:thread-name thread))))
    (sb-bsd-sockets:socket-close (osc-server-socket server))
    t))
