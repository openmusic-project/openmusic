
;;; Author: Robert Strandh
;;; Copyright (c) 2002 by Robert Strandh (strandh@labri.fr)

;;; Object sequence library.  The purpose is to divide a sequence of
;;; objects into subsequences such that a cost function is optimized.

;;; The mutator (i.e. the abstract data type consisting of the
;;; user-visible data types) maintains two obseq positions, one with
;;; the greatest position below which no modification has been made
;;; (obseq-lowpos), and the other with the smallest position above
;;; which no modification has been made (obseq-highpos) (since the
;;; last call to obseq-recompute).
;;; 
;;; Whenever an operation is made to position pos, these values are
;;; updated so that lowpos becomes the value of (min lowpos (1- pos))
;;; and highpos becomes the value of (max highpos (1+ pos)).
;;; 
;;; When obseq-recompute is invoked, all costs strictly below lowpos
;;; and strictly above highpos are correct and need not be recomputed.
;;; The gap between the two needs to be closed.  It does so by
;;; applying the cost computation incrementally, in each step
;;; incrementing lowpos and decrementing highpos.  When the two
;;; overlap sufficiently, obseq-recompute decides how to break the
;;; entire sequence into subsequences.

(in-package :om)



;;; Main entry point called by client code.  It closes the gap between
;;; obseq-lowpos and obseq-highpos with sufficient overlap to
;;; determine a globally-optimal division of the sequence into
;;; subsequences.
(defgeneric obseq-recompute (obseq cost-method))

;;; Given an object sequence, this function returns (and with setf,
;;; sets) an object position such that all positions below it are
;;; completely computed.
(defgeneric obseq-lowpos (obseq))
(defgeneric (setf object-lowpos) (lowpos obseq))

;;; Given an object sequence, this function returns (and with setf,
;;; sets) an object position such that all positions above it are
;;; completely computed.
(defgeneric obseq-highpos (obseq))
(defgeneric (setf object-highpos) (highpos obseq))

;;; The base class for all object sequences
(defclass obseq ()
  ((lowpos :accessor obseq-lowpos)
   (highpos :accessor obseq-highpos)))

;;; Given an objseq-pos, return the corresponding object sequence. 
(defgeneric obseq (obseq-pos))

;;; Return (and with setf, set) the best total cost, from the start of
;;; the sequence up to, but not including, the obseq-pos.  An object
;;; of type total-cost is return by this function.  This function must
;;; return valid objects for positions from the start up to and
;;; including lowpos.  For the first element of the sequence, this
;;; function returns a total cost of "zero", i.e., a total-cost
;;; instance that is always smaller than or equal to all other
;;; instances (in the sense of cost-less). 
(defgeneric obseq-pos-best-total-cost-backward (obseq-pos))


;;; Return (and with setf, set) the best total cost, from, but not
;;; including, the obseq-pos to the end of the sequence.  An object of
;;; type total-cost is return by this function.  This function must
;;; return valid objects for positions from the end down to and
;;; including highpos.  For the last element of the sequence, this
;;; function returns a total cost of "zero", i.e., a total-cost
;;; instance that is always smaller than or equal to all other
;;; instances (in the sense of cost-less).
(defgeneric obseq-pos-best-total-cost-forward (obseq-pos))

;;; Return (and with setf, set) the previous obseq-pos in the sequence
;;; which is first in the subsequence giving the best total backward
;;; cost (in the sense of obseq-pos-best-total-cost-backward).  This
;;; function must return valid objects for positions from the start up
;;; to and including lowpos.  When given the first position in the
;;; sequence, this function returns that same fist position.
(defgeneric obseq-pos-best-total-cost-prev (obseq-pos))

;;; Return (and with setf, set) the next obseq-pos in the sequence
;;; which is first in the subsequence giving the best total forward
;;; cost (in the sense of obseq-pos-best-total-cost-forward).  This
;;; function must return valid objects for positions from the end down
;;; to and including highpos.  When given the last position in the
;;; sequence, this function returns the same last position.  
(defgeneric obseq-pos-best-total-cost-next (obseq-pos))

;;; The base class for a position of an object in an obseq.  An
;;; instance of this class can belong to only one object sequence, and
;;; the object sequence itself must be possible to determine from this
;;; instance.

;;; Inserting and deleting objects from the sequence must not
;;; influence the correspondence between the obseq-pos and the object
;;; itself contained in the sequence.
(defclass obseq-pos ()
  ((obseq :initarg :obseq :reader obseq)
   (best-total-cost-backward :accessor obseq-pos-best-total-cost-backward)
   (best-total-cost-forward :accessor obseq-pos-best-total-cost-forward)
   (best-total-cost-prev :accessor obseq-pos-best-total-cost-prev)
   (best-total-cost-next :accessor obseq-pos-best-total-cost-next)
   (cut-p :initform nil :accessor obseq-pos-cut-p)))

;;; Given an obseq, return an obseq-pos corresponding to the first
;;; element of the object sequence.
(defgeneric obseq-first-pos (obseq))

;;; Return a true value if and only if the obseq-pos is the first
;;; element of the corresponding object sequence.
(defgeneric obseq-first-pos-p (obseq-pos))

(defmethod obseq-first-pos-p ((pos obseq-pos))
  (eql pos (obseq-first-pos (obseq pos))))  

;;; Given an obseq, return an obseq-pos corresponding to one less than
;;; the first element of the object sequence.
(defgeneric obseq-first-sentinel (obseq))

;;; Return a true value if and only if the obseq-pos is the first
;;; sentinel of the corresponding object sequence.
(defgeneric obseq-first-sentinel-p (obseq-pos))

(defmethod obseq-first-sentinel-p ((pos obseq-pos))
  (eql pos (obseq-first-sentinel (obseq pos))))  

;;; Given an obseq, return an obseq-pos corresponding to the last
;;; element of the object sequence.
(defgeneric obseq-last-pos (obseq))

;;; Return a true value if and only if the obseq-pos is the last
;;; element of the corresponding object sequence.
(defgeneric obseq-last-pos-p (obseq-pos))

(defmethod obseq-last-pos-p ((pos obseq-pos))
  (eql pos (obseq-last-pos (obseq pos))))

;;; Given an obseq, return an obseq-pos corresponding to one greater
;;; than the last element of the object sequence.
(defgeneric obseq-last-sentinel (obseq))

;;; Return a true value if and only if the obseq-pos is the last
;;; sentinel of the corresponding object sequence.
(defgeneric obseq-last-sentinel-p (obseq-pos))

(defmethod obseq-last-sentinel-p ((pos obseq-pos))
  (eql pos (obseq-last-sentinel (obseq pos))))

;;; Given an obseq-pos, return an obseq-pos corresponding to the
;;; previous element of the object sequence of obseq-pos.  It is an
;;; error to call this function with an object-pos for which
;;; obseq-first-sentinel-p returns true.
(defgeneric obseq-prev-pos (obseq-pos))

(defmethod obseq-prev-pos :before ((pos obseq-pos))
  (assert (not (obseq-first-sentinel-p pos))))

;;; Given an obseq-pos, return an obseq-pos corresponding to the next
;;; element of the object sequence of obseq-pos.  It is an error to
;;; call this function with an object-pos for which
;;; obseq-last-sentinel-p returns true.
(defgeneric obseq-next-pos (obseq-pos))

(defmethod obseq-next-pos :before ((pos obseq-pos))
  (assert (not (obseq-last-sentinel-p pos))))

;;; Return true if and only of obseq-pos1 is before obseq-pos2 in the
;;; sequence.  This operation should be implemented very efficiently,
;;; and should preferably have constant-time execution.
(defgeneric obseq-pos-less (obseq-pos1 obseq-pos2))

(defgeneric obseq-pos-less-or-equal (obseq-pos1 obseq-pos2))

(defmethod obseq-pos-less-or-equal ((p1 obseq-pos) (p2 obseq-pos)) 
  (or (obseq-pos-less p1 p2)
      (not (obseq-pos-less p2 p1))))

;;;; ---------------------------------------------------------------------
;;;; Cost functions

;;; There are three types of cost: individual cost, sequence cost, and
;;; total cost.  The individual cost reflects the cost of an
;;; individual element.  The sequence cost reflects the cost of a
;;; sequence of individual elements.  The total cost reflects the cost
;;; of a sequence of sequences.

;;; The base class for all cost methods.  A cost method contains
;;; everything that is required to compute individual costs, sequence
;;; costs, and total costs.
(defclass cost-method ()
  ())

;;; The base class for the cost of an individual element of an object
;;; sequence.
(defclass elem-cost ()
  ())

;;; The base class for the cost of a sequence of elements of an object
;;; sequence.
(defclass seq-cost ()
  ())

;;; The base class for the cost of a sequence of object sequences. 
(defclass total-cost ()
  ())

;;; Given the cost of a sequence and the cost of an element, compute
;;; the cost of a new sequence which is like the the old sequence with
;;; the element at the end of it.  An object of type seq-cost is
;;; returned by this function.  If seq-cost is nil, compute the cost of
;;; a sequence containing only the one element.
(defgeneric combine-seq-and-elem-cost (cost-method seq-cost elem-cost))

;;; Given the cost of a sequence of object sequences and the cost of
;;; an object sequence, compute the cost of a new sequence of
;;; sequences, which is like the old one with the sequence at the end
;;; of it.  An object of type total-cost is returned by this function.
;;; If total-cost is nil, the cost of a sequence of sequences
;;; containing only the one sequence is returned.
(defgeneric combine-total-and-seq-cost (cost-method total-cost seq-cost))

;;; Given a cost method and a sequence cost, this function returns a
;;; true value if and only if the result of adding more elements to
;;; the sequence will be guaranteed to increase the cost.  We use this
;;; function to stop increasing the size of the overlap when the
;;; overlap, as a sequence, has a higher cost than the best cut within
;;; the overlap.  When this is the case, there must be a best cut
;;; within the overlap.  If not, the entire overlap must be contained
;;; WITHIN an unbroken subsequence.  But that unbroken sequence must
;;; then have a higher cost than the overlap.  Since a lower cost can
;;; be found inside the overlap, the overlap is already big enough. 
(defgeneric seq-cost-cannot-decrease (cost-method seq-cost))

;;; Compare cost1 and cost2 according to the cost method given, and
;;; return true if and only if cost1 is considered smaller than cost2.
;;; The ordering is total so that if neither (cost-less cost-method
;;; cost1 cost2) nor (cost-less cost-method cost2 cost1) is true, then
;;; cost1 and cost2 are the same.  Client code must define two methods
;;; on this function, one to compare two sequence costs and one to
;;; compare two total costs.
(defgeneric cost-less (cost-method cost1 cost2))

;;; Compute the max of two cost objects
(defgeneric cost-max (cost-method cost1 cost2))

(defmethod cost-max (cost-method cost1 cost2)
  (if (cost-less cost-method cost1 cost2) cost2 cost1))

;;; Compute the min of two cost objects
(defgeneric cost-min (cost-method cost1 cost2))

(defmethod cost-min (cost-method cost1 cost2)
  (if (cost-less cost-method cost1 cost2) cost1 cost2))

;;;; ---------------------------------------------------------------------
;;;; Functions for storing and retrieving costs in positions

;;; Return the element cost of the obseq position given as argument.
;;; An object of type elem-cost is returned by this function.
(defgeneric obseq-pos-elem-cost (obseq-pos))



;;; Example of application, an array of numbers.  The sequence is to
;;; be divided into subsequences so that the absolute difference
;;; between a constant and the sum of the numbers of a subsequence is
;;; as small as possible.  The sequence is optimally divided when the
;;; max of the absolute differences is as minimal (minimax). 

(defclass number-seq (obseq)
  (;; an array of objects of type number-pos
   (numbers)))

(defclass number-pos (obseq-pos)
  ((val :initarg :val :reader number-val)
   (pos :initarg :pos)))

(defmethod print-object ((obj number-pos) stream)
  (with-slots (val pos) obj
    (print-unreadable-object (obj stream :identity t :type t)
      (format stream "~S @ ~S" val pos))))

(defun make-number-pos (val obseq pos)
  (make-instance 'number-pos :val val :obseq obseq :pos pos))

(defun make-number-seq (array)
  (let ((newarray (make-array (+ (length array) 2)))
	(result (make-instance 'number-seq)))
    ;; copy contents of argument to new array
    (loop for i from 0 below (length array) do
	  (setf (aref newarray (1+ i))
		(make-number-pos (aref array i) result (1+ i))))
    ;; initialize sentinels
    (setf (aref newarray 0)
	  (make-number-pos 0 result 0)
	  (aref newarray (1- (length newarray)))
	  (make-number-pos 0 result (1- (length newarray))))
    (setf (slot-value result 'numbers) newarray
	  (obseq-lowpos result) (aref newarray 1)
	  (obseq-highpos result) (aref newarray (- (length newarray) 2)))
    (setf (obseq-pos-best-total-cost-backward (obseq-lowpos result))
	  (make-number-total-cost 0))
    (setf (obseq-pos-best-total-cost-prev (obseq-lowpos result))
	  (obseq-lowpos result))
    (setf (obseq-pos-best-total-cost-forward (obseq-highpos result))
	  (make-number-total-cost 0))
    (setf (obseq-pos-best-total-cost-next (obseq-highpos result))
	  (obseq-highpos result))
    result))

(defmethod obseq-first-pos ((seq number-seq))
  (with-slots (numbers) seq
    (aref numbers 1)))

(defmethod obseq-first-sentinel ((seq number-seq))
  (with-slots (numbers) seq
    (aref numbers 0)))

(defmethod obseq-last-pos ((seq number-seq))
  (with-slots (numbers) seq
    (aref numbers (- (length numbers) 2))))

(defmethod obseq-last-sentinel ((seq number-seq))
  (with-slots (numbers) seq
    (aref numbers (1- (length numbers)))))

(defmethod obseq-prev-pos ((pos number-pos))
  (aref (slot-value (obseq pos) 'numbers)
	(1- (slot-value pos 'pos))))

(defmethod obseq-next-pos ((pos number-pos))
  (aref (slot-value (obseq pos) 'numbers)
	(1+ (slot-value pos 'pos))))

(defmethod obseq-pos-less ((pos1 number-pos) (pos2 number-pos))
  (< (slot-value pos1 'pos) (slot-value pos2 'pos)))

(defclass number-cost-method (cost-method)
  ((best-sum :initarg :best-sum :accessor best-sum)))

(defclass number-elem-cost (elem-cost)
  ((cost :initarg :cost)))

(defclass number-seq-cost (seq-cost)
  ((cost :initarg :cost)))

(defclass number-total-cost (total-cost)
  ((cost :initarg :cost)))

(defmethod print-object ((obj number-total-cost) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (format stream "~S" (slot-value obj 'cost))))

(defun make-number-total-cost (cost)
  (make-instance 'number-total-cost :cost cost))

(defmethod combine-seq-and-elem-cost ((method number-cost-method)
				      (seq-cost number-seq-cost)
				      (elem-cost number-elem-cost))
  (make-instance 'number-seq-cost
		 :cost (+ (slot-value seq-cost 'cost)
			  (slot-value elem-cost 'cost))))

(defmethod combine-seq-and-elem-cost ((method number-cost-method)
				      (seq-cost (eql nil))
				      (elem-cost number-elem-cost))
  (make-instance 'number-seq-cost
		 :cost (slot-value elem-cost 'cost)))

(defmethod combine-total-and-seq-cost ((method number-cost-method)
				       (total-cost number-total-cost)
				       (seq-cost number-seq-cost))
  (make-instance 'number-total-cost
		 :cost (max (slot-value total-cost 'cost)
			    (abs (- (slot-value method 'best-sum)
				    (slot-value seq-cost 'cost))))))

(defmethod seq-cost-cannot-decrease ((method number-cost-method)
				     (seq-cost number-seq-cost))
  (>= (slot-value seq-cost 'cost)
      (slot-value method 'best-sum)))

(defmethod combine-total-and-seq-cost ((method number-cost-method)
				       (total-cost (eql nil))
				       (seq-cost number-seq-cost))
  (make-instance 'number-total-cost
		 :cost (abs (- (slot-value method 'best-sum)
			       (slot-value seq-cost 'cost)))))

(defmethod cost-less ((method number-cost-method)
		      (c1 number-seq-cost)
		      (c2 number-seq-cost))
  (< (abs (- (slot-value method 'best-sum)
	     (slot-value c1 'cost)))
     (abs (- (slot-value method 'best-sum)
	     (slot-value c2 'cost)))))

(defmethod cost-less ((method number-cost-method)
		      (c1 number-total-cost)
		      (c2 number-total-cost))
  (< (slot-value c1 'cost) (slot-value c2 'cost)))

(defmethod obseq-pos-elem-cost ((pos number-pos))
  (make-instance 'number-elem-cost
		 :cost (number-val pos)))

(defparameter *method* (make-instance 'number-cost-method :best-sum 9))



;;; Algorithm

;;; FIXME lowpos should be allowed to have the value of length and
;;; highpos the value -1

;;; FIXME with the current protocol, the algorithm is always 
;;; quadratic.  Add protocol operations to avoid that. 

(defun may-advance-right (obseq)
  (obseq-pos-less (obseq-lowpos obseq) (obseq-last-sentinel obseq)))

(defun advance-right (obseq method)
  (assert (may-advance-right obseq))
  (let* ((lowpos (obseq-lowpos obseq))
	 (prevpos lowpos)
	 (firstpos (obseq-first-pos obseq)))
    (setf lowpos (obseq-next-pos lowpos)
	  (obseq-lowpos obseq) lowpos)
    (let* ((elem-cost (obseq-pos-elem-cost prevpos))
	   (seq-cost (combine-seq-and-elem-cost method nil elem-cost))
	   (total-cost (combine-total-and-seq-cost
			method
			(obseq-pos-best-total-cost-backward prevpos)
			seq-cost))
	   (best-prev-pos prevpos)
	   (best-total-cost total-cost))
      (loop while (obseq-pos-less firstpos prevpos) do
	    (setf prevpos (obseq-prev-pos prevpos)
		  elem-cost (obseq-pos-elem-cost prevpos)
		  seq-cost (combine-seq-and-elem-cost method seq-cost elem-cost)
		  total-cost (combine-total-and-seq-cost
			      method
			      (obseq-pos-best-total-cost-backward prevpos)
			      seq-cost))
	    (when (cost-less method total-cost best-total-cost)
	      (setf best-total-cost total-cost
		    best-prev-pos prevpos)))
      (setf (obseq-pos-best-total-cost-backward lowpos) best-total-cost
	    (obseq-pos-best-total-cost-prev lowpos) best-prev-pos))))

(defun may-advance-left (obseq)
  (obseq-pos-less (obseq-first-sentinel obseq) (obseq-highpos obseq)))

(defun advance-left (obseq method)
  (assert (may-advance-left obseq))
  (let* ((highpos (obseq-highpos obseq))
	 (nextpos highpos)
	 (lastpos (obseq-last-pos obseq)))
    (setf highpos (obseq-prev-pos highpos)
	  (obseq-highpos obseq) highpos)
    (let* ((elem-cost (obseq-pos-elem-cost nextpos))
	   (seq-cost (combine-seq-and-elem-cost method nil elem-cost))
	   (total-cost (combine-total-and-seq-cost
			method
			(obseq-pos-best-total-cost-forward nextpos)
			seq-cost))
	   (best-next-pos nextpos)
	   (best-total-cost total-cost))
      (loop while (obseq-pos-less nextpos lastpos) do
	    (setf nextpos (obseq-next-pos nextpos)
		  elem-cost (obseq-pos-elem-cost nextpos)
		  seq-cost (combine-seq-and-elem-cost method seq-cost elem-cost)
		  total-cost (combine-total-and-seq-cost
			      method
			      (obseq-pos-best-total-cost-forward nextpos)
			      seq-cost))
	    (when (cost-less method total-cost best-total-cost)
	      (setf best-total-cost total-cost
		    best-next-pos nextpos)))
      (setf (obseq-pos-best-total-cost-forward highpos) best-total-cost
	    (obseq-pos-best-total-cost-next highpos) best-next-pos))))

(defun close-the-gap (obseq cost-method)
  (loop while (and (may-advance-right obseq)
		   (obseq-pos-less-or-equal (obseq-lowpos obseq) (obseq-highpos obseq))) do
		   (advance-right obseq cost-method)
		   (when (and (may-advance-left obseq)
			      (obseq-pos-less-or-equal (obseq-lowpos obseq) (obseq-highpos obseq)))
		     (advance-left obseq cost-method)))
  #|
(format *error-output* "lowpos: ~a~%highpos: ~a~2%"
	  (obseq-lowpos obseq) (obseq-highpos obseq))
|#
)

;;; create enough overlap that we know the best cut will be contained
;;; somewhere within it.
(defun create-some-overlap (obseq cost-method)
  (let ((best-cut-in-overlap (obseq-highpos obseq))
	(best-cost-in-overlap
	 (cost-max cost-method
		   (obseq-pos-best-total-cost-forward (obseq-highpos obseq))
		   (obseq-pos-best-total-cost-backward (obseq-lowpos obseq)))))
    (flet ((move-left () 
	      (advance-left obseq cost-method)
	      (let ((new-cost
		     (cost-max cost-method
			       (obseq-pos-best-total-cost-forward (obseq-highpos obseq))
			       (obseq-pos-best-total-cost-backward (obseq-next-pos (obseq-highpos obseq))))))
		(when (cost-less cost-method new-cost best-cost-in-overlap)
		  (setf best-cost-in-overlap new-cost
			best-cut-in-overlap (obseq-highpos obseq)))))
	   (move-right ()
	      (advance-right obseq cost-method)
	      (let ((new-cost
		     (cost-max cost-method
			       (obseq-pos-best-total-cost-forward (obseq-prev-pos (obseq-lowpos obseq)))
			       (obseq-pos-best-total-cost-backward (obseq-lowpos obseq)))))
		(when (cost-less cost-method new-cost best-cost-in-overlap)
		  (setf best-cost-in-overlap new-cost
			best-cut-in-overlap (obseq-prev-pos (obseq-lowpos obseq)))))))
      ;; this will not be true for the empty sequence.  Must handle that better
      ;(assert (or (may-advance-left obseq) (may-advance-right obseq)))
      (when (may-advance-left obseq)
	(move-left))
      (when (may-advance-right obseq)
	(move-right))
      ;; now the gap has one element in it, namely the one after highpos
      #|
(format *error-output* "lowpos: ~a~%highpos: ~a~2%"
	      (obseq-lowpos obseq) (obseq-highpos obseq))
|#
      (let ((overlap-sequence-cost
	     (combine-seq-and-elem-cost
	      cost-method nil (obseq-pos-elem-cost (obseq-next-pos (obseq-highpos obseq))))))
	(flet ((left ()
		 (move-left)
		 (setf overlap-sequence-cost
		       (combine-seq-and-elem-cost
			cost-method
			overlap-sequence-cost
			(obseq-pos-elem-cost (obseq-next-pos (obseq-highpos obseq))))))
	       (right ()
		 (move-right)
		 (setf overlap-sequence-cost
		       (combine-seq-and-elem-cost
			cost-method
			overlap-sequence-cost
			(obseq-pos-elem-cost (obseq-prev-pos (obseq-lowpos obseq)))))))
	  ;; increase overlap until we are sure its cost increases
	  (loop while (and (or (may-advance-left obseq) (may-advance-right obseq))
			   (not (seq-cost-cannot-decrease cost-method overlap-sequence-cost)))
		do (if (may-advance-left obseq)
		       (left)
		       (right)))
	  ;; increase overlap until its cost as a sequence is greater than
	  ;; the cost of the best cut within the overlap
	  (loop while (and (or (may-advance-left obseq) (may-advance-right obseq))
			   (cost-less cost-method
				      (combine-total-and-seq-cost cost-method nil overlap-sequence-cost)
				      best-cost-in-overlap))
		do (if (may-advance-left obseq)
		       (left)
		       (right))))))
    (values best-cut-in-overlap best-cost-in-overlap)))

(defmethod obseq-recompute (obseq cost-method)
  (when (obseq-pos-less-or-equal (obseq-lowpos obseq) (obseq-highpos obseq))
    (close-the-gap obseq cost-method)
    (multiple-value-bind (best-cut best-cost) (create-some-overlap obseq cost-method)
      #|
(format *error-output* "best cost: ~a~%best cut: ~a~2%"
	      best-cost best-cut)
|#
      (loop for p = best-cut then (obseq-pos-best-total-cost-prev p)
	    while (obseq-pos-less (obseq-first-pos obseq) p) do
	    (setf (obseq-pos-cut-p p) t))
      (loop for p = best-cut then (obseq-pos-best-total-cost-next p)
	    while (obseq-pos-less p (obseq-last-pos obseq)) do
	    (setf (obseq-pos-cut-p p) t)))))

(defmethod map-over-obseq-subsequences (function (seq obseq))
  (loop with list = '()
	for p = (obseq-first-pos seq) then (obseq-next-pos p)
	until (obseq-last-sentinel-p p)
	if (obseq-pos-cut-p p)
	do (progn
	     (funcall function (nreverse list))
	     (setf list (list p)))
	else do (push p list)
	finally (funcall function (nreverse list))))
	
(defparameter *numbers* (make-number-seq #(4 3 5 2 4 3 5 5 6 4 5 6 7 8 6 7 5 6 4 3 2 2 4 2 4 1 4 4 3 1 2 3 4 2 5 3 6 4 5 3 2 3 1 2 3 4 5 6)))

(defun print-numbers (seq)
  (map-over-obseq-subsequences
   #'(lambda (l) (mapcar #'(lambda (p) (slot-value p 'val)) l))
   seq))


;===========================
;Interface to OM
;===========================
(defun make-numbers-list (seq)
  (let (rep)
    (map-over-obseq-subsequences
     #'(lambda (l) (push (mapcar #'(lambda (p) (slot-value p 'val)) l) rep))
     seq)
    (reverse rep)))


(defun make-page-break (list linesize)
  (let ((numbers (make-number-seq (make-array (length list) :initial-contents list)))
        (metodo (make-instance 'number-cost-method :best-sum linesize)))
    (obseq-recompute numbers metodo)
    (make-numbers-list numbers)
    ))

;(make-page-break '(4 3 5 2 4 3 5 5 6 4 5 6 7 8 6 7 5 6 4 3 2 2 4 2 4 1 4 4 3 1 2 3 4 2 5 3 6 4 5 3 2 3 1 2 3 4 5 6) 9)
    


