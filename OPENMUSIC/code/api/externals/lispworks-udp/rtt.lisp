;;;; -*- Mode: Lisp -*-
;;;; $Id: rtt.lisp 641 2008-11-20 03:52:57Z binghe $

(in-package :comm+)

;;; UNIX Network Programming v1 - Networking APIs: Sockets and XTI
;;;  Chapter 20: Advance UDP Sockets
;;;   Adding Reliability to a UDP Application

(defclass rtt-info-mixin ()
  ((rtt    :type short-float
           :documentation "most recent measured RTT, seconds")
   (srtt   :type short-float
           :documentation "smoothed RTT estimator, seconds")
   (rttvar :type short-float
           :documentation "smoothed mean deviation, seconds")
   (rto    :type short-float
           :documentation "current RTO to use, seconds")
   (nrexmt :type fixnum
           :documentation "#times retransmitted: 0, 1, 2, ...")
   (base   :type integer
           :documentation "#sec since 1/1/1970 at start, but we use Lisp time here"))
  (:documentation "RTT Info Class"))

(defvar *rtt-rxtmin*  2.0s0 "min retransmit timeout value, seconds")
(defvar *rtt-rxtmax* 60.0s0 "max retransmit timeout value, seconds")
(defvar *rtt-maxnrexmt* 3 "max #times to retransmit")

(defmethod rtt-rtocalc ((instance rtt-info-mixin))
  "Calculate the RTO value based on current estimators:
        smoothed RTT plus four times the deviation."
  (with-slots (srtt rttvar) instance
    (+ srtt (* 4.0s0 rttvar))))

(defun rtt-minmax (rto)
  "rtt-minmax makes certain that the RTO is between the upper and lower limits."
  (declare (type short-float rto))
  (cond ((< rto *rtt-rxtmin*) *rtt-rxtmin*)
        ((> rto *rtt-rxtmax*) *rtt-rxtmax*)
        (t rto)))

(defmethod initialize-instance :after ((instance rtt-info-mixin) &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (rtt-init instance))

(defmethod rtt-init ((instance rtt-info-mixin))
  (with-slots (base rtt srtt rttvar rto) instance
    (setf base   (get-internal-real-time)
          rtt    0.0s0
          srtt   0.0s0
          rttvar 0.75s0
          rto    (rtt-minmax (rtt-rtocalc instance)))))

(defmethod rtt-ts ((instance rtt-info-mixin))
  (* (- (get-internal-real-time) (slot-value instance 'base))
     #.(/ 1000 internal-time-units-per-second)))

(defmethod rtt-start ((instance rtt-info-mixin))
  "return value can be used as: alarm(rtt_start(&foo))"
  (round (slot-value instance 'rto)))

(defmethod rtt-stop ((instance rtt-info-mixin) (ms number))
  (with-slots (rtt srtt rttvar rto) instance
    (setf rtt (/ ms 1000.0s0))
    (let ((delta (- rtt srtt)))
      (incf srtt (/ delta 8.0s0))
      (incf rttvar (/ (- (abs delta) rttvar) 4.0s0)))
    (setf rto (rtt-minmax (rtt-rtocalc instance)))))

(defmethod rtt-timeout ((instance rtt-info-mixin))
  (with-slots (rto nrexmt) instance
    (setf rto (* rto 2.0s0))
    (< (incf nrexmt) *rtt-maxnrexmt*)))

(defmethod rtt-newpack ((instance rtt-info-mixin))
  (setf (slot-value instance 'nrexmt) 0))
