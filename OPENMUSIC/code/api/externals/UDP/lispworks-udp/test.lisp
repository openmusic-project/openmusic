;;;; -*- Mode: Lisp -*-
;;;; $Id: test.lisp 658 2008-11-21 18:27:24Z binghe $

(in-package :cl-user)

;;; UDP Echo Test: use macros
(defun udp-echo-test-3 (&optional (port 10000))
  (let* ((fn #'(lambda (data)
                 (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                      (format nil "receive from ~A:~A -> ~A"
                              (comm+:ip-address-string comm+:*client-address*)
                              comm+:*client-port*
                              (map 'string #'code-char data)))))
         (server-process (comm+:start-udp-server :function fn :service port)))
    (unwind-protect
        (comm+:with-udp-stream (stream "localhost" port :read-timeout 1 :errorp t)
          (let ((data "Hello, world!"))
            (format stream "~A" data)
            (terpri stream) ;; = "~%" or #\Newline
            (force-output stream)
            (format t "STREAM: Send message: ~A~%" data)
            (let ((echo (read-line stream nil nil)))
              (format t "STREAM: Recv message: ~A~%" echo))))
      (comm+:stop-udp-server server-process))))

(defun udp-echo-test-4 (&optional (port 10000))
  (let* ((server-process (comm+:start-udp-server :function #'reverse :service port)))
    (unwind-protect
        (comm+:with-udp-socket (socket :read-timeout 10)
          (let ((data #(1 2 3 4 5 6 7 8 9 10)))
            (comm+:send-message socket data :host "localhost" :service port)
            (format t "SOCKET: Send message: ~A~%" data)
            (let ((echo (multiple-value-list (comm+:receive-message socket
                                                                   :max-buffer-size 8))))
              (format t "SOCKET: Recv message: ~A~%" echo))))
      (comm+:stop-udp-server server-process))))

(defun udp-echo-test-5 (&optional (port 10000))
  "Limit MAX-BUFFER-SIZE, less than send bytes."
  (let* ((echo-fn #'(lambda (data)
                      data))
         (server-process (comm+:start-udp-server :function echo-fn :service port)))
    (unwind-protect
        (comm+:with-connected-udp-socket (socket "localhost" port :read-timeout 1 :errorp t)
          (let ((data #(1 2 3 4 5 6 7 8 9 10)))
            (princ (comm+:send-message socket data :host nil :service nil))
            (format t "SOCKET: Send message: ~A~%" data)
            (let ((echo (multiple-value-list (comm+:receive-message socket :max-buffer-size 8))))
              (format t "SOCKET: Recv message: ~A~%" echo))))
      (comm+:stop-udp-server server-process))))

(defun loop-test (&optional (port 3500))
  (labels ((echo-fn (data) data))
    (loop for i from 1 to 10
          do (let ((server (comm+:start-udp-server :function #'echo-fn :service port :loop-time 0.3)))
               (comm+:with-udp-socket (socket :read-timeout 1)
                 (let ((data #(1 2 3 4 5 6 7 8 9 10)))
                   (comm+:send-message socket data (length data) "localhost" port)
                   (format t "SOCKET: Send message: ~A~%" data)
                   (let ((echo (multiple-value-list (comm+:receive-message socket))))
                     (format t "SOCKET: Recv message: ~A~%" echo))))
               (princ (comm+:stop-udp-server server :wait t))))))

(defun rtt-test-1 (&optional (port 10000))
  "RTT test"
  (let ((server-process (comm+:start-udp-server :function #'reverse :service port)))
    (unwind-protect
        (comm+:with-connected-udp-socket (socket "localhost" port :errorp t)
          (comm+:sync-message socket "xxxxABCDEFGH"
                             :max-receive-length 8
                             :encode-function #'(lambda (x)
                                                  (values (map 'vector #'char-code x) 0))
                             :decode-function #'(lambda (x)
                                                  (values (map 'string #'code-char x) 0))))
      (comm+:stop-udp-server server-process))))

(defun rtt-test-2 (&optional (port 10000))
  "RTT test, no server"
  (comm+:with-udp-socket (socket)
    (handler-case
        (comm+:sync-message socket "xxxxABCDEFGH"
                            :host "localhost"
                            :service port
                            :max-receive-length 8
                            :encode-function #'(lambda (x)
                                                 (values (map 'vector #'char-code x) 0))
                            :decode-function #'(lambda (x)
                                                 (values (map 'string #'code-char x) 0)))
      (error (c)
        (format t "Got a condition (~A): ~A~%"
                (type-of c) c)))))

#-mswindows
(defun wait-test-1 (&optional (port 10000))
  (let* ((server-process (comm+:start-udp-server :function #'reverse :service port)))
    (unwind-protect
        (comm+:with-udp-socket (socket)
          (let ((data #(1 2 3 4 5 6 7 8 9 10)))
            (comm+:send-message socket data :host "localhost" :service port)
            (format t "SOCKET: Send message: ~A~%" data)

            ;;; wait the socket until it's available
            (comm+:wait-for-input socket)

            (let ((echo (multiple-value-list (comm+:receive-message socket
                                                                    :max-buffer-size 8))))
              (format t "SOCKET: Recv message: ~A~%" echo))))
      (comm+:stop-udp-server server-process))))

#-mswindows
(defun mcast-test-1 (&optional (host "224.0.0.1") (port 10000))
  "Send one get three"
  (let* ((echo-fn #'(lambda (data) data))
         (server-processes (mapcar #'(lambda (x) (comm+:start-udp-server :function echo-fn :service port
                                                                        :announce t
                                                                        :address host
                                                                        :multicast t))
                                   '(nil nil nil))))
    (unwind-protect
        (comm+:with-connected-udp-socket (socket host port :read-timeout 1 :errorp t)
          (let ((data #(1 2 3 4 5 6 7 8 9 10)))
            (format t "~A~%" (comm+:send-message socket data))
            (format t "SOCKET: Send message: ~A~%" data)
            (dotimes (i 3)
              (let ((echo (multiple-value-list (comm+:receive-message socket :max-buffer-size 8))))
                (format t "SOCKET: Recv message: ~A~%" echo)))))
      (mapcar #'comm+:stop-udp-server server-processes))))
