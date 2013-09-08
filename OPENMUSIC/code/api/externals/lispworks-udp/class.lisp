;;;; -*- Mode: Lisp -*-
;;;; $Id: class.lisp 665 2008-12-04 17:17:41Z binghe $

(in-package :comm+)

(defclass socket-datagram ()
  ((open-p :type boolean
           :accessor socket-open-p
           :initform t)
   (socket :type integer
           :reader socket-datagram-socket
           :initarg :socket)
   ;;; following slots are taken from USOCKET project
   (wait-list
    :initform nil
    :accessor wait-list
    :documentation "WAIT-LIST the object is associated with.")
   (state
    :initform nil
    :accessor state
    :documentation "Per-socket return value for the `wait-for-input' function.

The value stored in this slot can be any of
 NIL          - not ready
 :READ        - ready to read
 :READ-WRITE  - ready to read and write
 :WRITE       - ready to write

The last two remain unused in the current version."))
  (:documentation "datagram socket class"))

(defmethod initialize-instance :after ((instance socket-datagram)
                                       &rest initargs &key &allow-other-keys)
  (hcl:flag-special-free-action instance))

(defclass inet-datagram (socket-datagram rtt-info-mixin)
  ())

(defclass unix-datagram (socket-datagram)
  ((local-pathname :type (or null string pathname)
                   :accessor unix-datagram-pathname
                   :initarg :pathname)))

(defun make-inet-datagram (socket-fd)
  (make-instance 'inet-datagram :socket socket-fd))

(defun make-unix-datagram (socket-fd &optional pathname)
  (make-instance 'unix-datagram
                 :socket socket-fd
                 :pathname pathname))

(defmethod close-datagram ((socket socket-datagram))
  (close-socket (socket-datagram-socket socket)))

(defmethod close-datagram :after ((socket socket-datagram))
  (setf (socket-open-p socket) nil))

(defmethod close-datagram :after ((socket unix-datagram))
  (ignore-errors
    (delete-file (unix-datagram-pathname socket))))

;; Register a special free action for closing datagram usocket when being GCed
(defun socket-special-free-action (object)
  (when (and (typep object 'socket-datagram)
             (socket-open-p object))
    (close-datagram object)))

(eval-when (:load-toplevel :execute)
  (hcl:add-special-free-action 'socket-special-free-action))

(defgeneric socket-receive-timeout (socket))
(defgeneric (setf socket-receive-timeout) (seconds socket))

(defmethod socket-receive-timeout ((socket socket-datagram))
  (socket-receive-timeout (socket-datagram-socket socket)))

(defmethod (setf socket-receive-timeout) (seconds (socket socket-datagram))
  (setf (socket-receive-timeout (socket-datagram-socket socket))
        seconds))

(defgeneric send-message (socket buffer &key))

(defgeneric receive-message (socket &key))

(defgeneric socket-reuse-address (socket))
(defgeneric (setf socket-reuse-address) (flag socket))

(defmethod socket-reuse-address ((socket-fd integer))
  "Get socket option: REUSEADDR, return value is 0 or 1"
  (fli:with-dynamic-foreign-objects ((flag :int)
                                     (len :int))
    (let ((reply (getsockopt socket-fd
                             *sockopt_sol_socket*
                             *sockopt_so_reuseaddr*
                             (fli:copy-pointer flag :type '(:pointer :void))
                             len)))
      (when (zerop reply)
        (values (fli:dereference flag) t)))))

(defmethod socket-reuse-address ((socket inet-datagram))
  (socket-reuse-address (socket-datagram-socket socket)))

(defmethod (setf socket-reuse-address) (flag (socket-fd integer))
  "Set socket option: REUSEADDR, argument flag can be a boolean"
  (setf (socket-reuse-address socket-fd) (if flag 1 0)))

(defmethod (setf socket-reuse-address) ((flag integer) (socket-fd integer))
  "Set socket option: REUSEADDR, argument flag can be 0 or 1"
  (declare (type (integer 0 1) flag))
  (fli:with-dynamic-foreign-objects ((%flag :int))
    (setf (fli:dereference %flag) flag)
    (let ((reply (setsockopt socket-fd
                             *sockopt_sol_socket*
                             *sockopt_so_reuseaddr*
                             (fli:copy-pointer %flag :type '(:pointer :void))
                             (fli:size-of :int))))
      (when (zerop reply)
        (values flag t)))))

(defmethod (setf socket-reuse-address) (flag (socket inet-datagram))
  (setf (socket-reuse-address (socket-datagram-socket socket))
        flag))
