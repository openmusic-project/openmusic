;;;; -*- Mode: Lisp -*-
;;;; $Id: condition.lisp 645 2008-11-20 09:15:03Z binghe $
;;;; Condition for LispWorks-UDP

(in-package :comm+)

(define-condition socket-warning (warning)
  ((socket :type socket-datagram
           :reader socket-warning-socket
           :initarg :socket)))

(define-condition rtt-timeout-warning (socket-warning)
  ((old-rto :type short-float
            :reader old-rto-of
            :initarg :old-rto)
   (new-rto :type short-float
            :reader new-rto-of
            :initarg :new-rto))
  (:report (lambda (condition stream)
             (format stream "Receive timeout (~0,1Fs), next: ~0,1Fs.~%"
                     (old-rto-of condition)
                     (new-rto-of condition))))
  (:documentation "RTT timeout warning"))

(define-condition rtt-seq-mismatch-warning (socket-warning)
  ((send-seq :type integer
             :reader send-seq-of
             :initarg :send-seq)
   (recv-seq :type integer
             :reader recv-seq-of
             :initarg :recv-seq))
  (:report (lambda (condition stream)
             (format stream "Sequence number mismatch (~D -> ~D), try read again.~%"
                     (send-seq-of condition)
                     (recv-seq-of condition))))
  (:documentation "RTT sequence mismatch warning"))

(define-condition datagram-socket-error (socket-error)
  ((socket :type socket-datagram
           :reader socket-error-socket
           :initarg :socket)))

(define-condition rtt-timeout-error (datagram-socket-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Max retransmit times (~D) reached, give up.~%"
                     *rtt-maxnrexmt*)))
  (:documentation "RTT timeout error"))
