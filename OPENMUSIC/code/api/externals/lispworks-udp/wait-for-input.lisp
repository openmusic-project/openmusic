;;;; -*- Mode: Lisp -*-
;;;; $Id: wait-for-input.lisp 643 2008-11-20 05:21:49Z binghe $

;;;; WAIT-FOR-INPUT from USOCKET Project

(in-package :comm+)

#-mswindows
(defun check-for-multiprocessing-started (&optional errorp)
  (unless mp:*current-process*
    (funcall (if errorp 'error 'warn)
             "You must start multiprocessing on Lispworks by calling~
              ~%~3t(~s)~
              ~%for ~s function properly."
             'mp:initialize-multiprocessing
             'wait-for-input)))

#-mswindows
(check-for-multiprocessing-started)

(defgeneric wait-for-input (socket-or-sockets &key timeout ready-only))

(defstruct (wait-list (:constructor %make-wait-list))
  %wait     ;; implementation specific
  waiters ;; the list of all usockets
  map  ;; maps implementation sockets to usockets
  )

;; Implementation specific:
;;
;;  %setup-wait-list
;;  %add-waiter
;;  %remove-waiter

(defun make-wait-list (waiters)
  (let ((wl (%make-wait-list)))
    (setf (wait-list-map wl) (make-hash-table))
    (%setup-wait-list wl)
    (dolist (x waiters)
      (add-waiter wl x))
    wl))

(defun add-waiter (wait-list input)
  (setf (gethash (socket-datagram-socket input) (wait-list-map wait-list)) input
        (wait-list input) wait-list)
  (pushnew input (wait-list-waiters wait-list))
  (%add-waiter wait-list input))

(defun remove-waiter (wait-list input)
  (%remove-waiter wait-list input)
  (setf (wait-list-waiters wait-list)
        (remove input (wait-list-waiters wait-list))
        (wait-list input) nil)
  (remhash (socket-datagram-socket input) (wait-list-map wait-list)))

(defun remove-all-waiters (wait-list)
  (dolist (waiter (wait-list-waiters wait-list))
    (%remove-waiter wait-list waiter))
  (setf (wait-list-waiters wait-list) nil)
  (clrhash (wait-list-map wait-list)))

(defmethod wait-for-input ((socket inet-datagram) &key timeout ready-only)
  (multiple-value-bind (socks to)
      (wait-for-input (list socket) :timeout timeout :ready-only ready-only)
    (values (if ready-only (car socks) socket) to)))

(defmethod wait-for-input ((sockets list) &key timeout ready-only)
  (let ((wl (make-wait-list sockets)))
    (multiple-value-bind (socks to)
	(wait-for-input wl :timeout timeout :ready-only ready-only)
      (values (if ready-only socks sockets) to))))

(defmethod wait-for-input ((sockets wait-list) &key timeout ready-only)
  (let* ((start (get-internal-real-time))
         (sockets-ready 0))
    (dolist (x (wait-list-waiters sockets))
      (when (setf (state x)
                  (if (and (typep x 'inet-datagram)
                           (socket-listen (socket-datagram-socket x)))
                      :read nil))
        (incf sockets-ready)))
    ;; the internal routine is responsibe for
    ;; making sure the wait doesn't block on socket-streams of
    ;; which theready- socket isn't ready, but there's space left in the
    ;; buffer
    (wait-for-input-internal sockets
                             :timeout (if (zerop sockets-ready) timeout 0))
    (let ((to-result (when timeout
                       (let ((elapsed (/ (- (get-internal-real-time) start)
                                         internal-time-units-per-second)))
                         (when (< elapsed timeout)
                           (- timeout elapsed))))))
      (values (if ready-only
                  (remove-if #'null (wait-list-waiters sockets) :key #'state)
                  sockets)
              to-result))))

#-mswindows
(progn

  (defun %setup-wait-list (wait-list)
    (declare (ignore wait-list)))

  (defun %add-waiter (wait-list waiter)
    (declare (ignore wait-list waiter)))

  (defun %remove-waiter (wait-list waiter)
    (declare (ignore wait-list waiter)))

  (defmethod wait-for-input-internal (wait-list &key timeout)
    (dolist (x (wait-list-waiters wait-list))
      (mp:notice-fd (socket-datagram-socket x)))
    (labels ((wait-function (socks)
	       (let (rv)
		 (dolist (x socks rv)
		   (when (socket-listen (socket-datagram-socket x))
		     (setf (state x) :read
			   rv t))))))
      (if timeout
	  (mp:process-wait-with-timeout "Waiting for a socket to become active"
					(truncate timeout)
					#'wait-function
					(wait-list-waiters wait-list))
	  (mp:process-wait "Waiting for a socket to become active"
			   #'wait-function
			   (wait-list-waiters wait-list))))
    (dolist (x (wait-list-waiters wait-list))
      (mp:unnotice-fd (socket-datagram-socket x)))
    wait-list))
