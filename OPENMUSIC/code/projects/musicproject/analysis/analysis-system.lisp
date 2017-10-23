;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson
;=========================================================================

;;;===========================
;;; ANALYSIS FRAMEWORK FOR OM SCORE EDITORS
;;;===========================

(in-package :om)

;(defclass! analyse-object ()
;    ((analysis :accessor analysis :initform nil :documentation "analysis system(s)")))

;;; REDEFINITION
;(defclass* score-element (container analyse-object) () (:icon 138))

;;;============================
;;; EXPORTED SYMBOLS
;;;============================

;;; exporting slots from om::abstract-analysis and om::segment
(export '(analysis-segments analysis-object))
(export '(segment-data color tb te))

;;;============================
;;; ANALYSE ABSTRACT CLASS 
;;;============================

(defclass! ABSTRACT-ANALYSIS (named-object)
  ((analysis-segments :accessor analysis-segments :initarg :analysis-segments :initform nil :documentation "analysis segments")
   (analysis-object :accessor analysis-object :initform nil :documentation "object to be analyzed")
   (selected-segments :accessor selected-segments :initform nil :documentation "a list of selected segments")
   ))

;;; THIS ANALYSIS CAN COMPUTE SEGMENTS AUTOMATICALLY
(defmethod compute-segments-p ((self abstract-analysis)) nil)

;;; THIS ANALYSIS CAN ANALYSE SEGMENTS AUTOMATICALLY
(defmethod analyse-segments-p ((self abstract-analysis)) nil)

;;; THIS ANALYSIS CAN COMPUTE AND ANALYSE SEGMENTS AT A TIME
(defmethod compute+analyse-segments-p ((self abstract-analysis)) nil)

(defmethod compute-segments ((self abstract-analysis) (object analyse-object))
  (let ((seg-list (compute-analysis-segments self object)))
    (when seg-list
      (setf (analysis-segments self) nil)
      (loop for seg in seg-list
            do (add-in-analysis self seg)))))

(defmethod analyse-segments ((self abstract-analysis) (object analyse-object))
  (loop for seg in (analysis-segments self) do
     (analyse-one-segment self seg object)))

;;; can be redefned / optimized by analysis classes
(defmethod compute-and-analyse-segments ((self abstract-analysis) (object analyse-object))
  (compute-segments self object)
  (analyse-segments self object))
  
(defmethod reset-object-analysis ((self abstract-analysis)) 
  (setf (analysis-segments self) nil)
  (analysis-init self (analysis-object self)))

;;; Abstract method (to be implemented by ABSTRACT-ANALYSIS subclasses)
(defmethod compute-analysis-segments ((self abstract-analysis) object) nil)

;;; Abstract method (to be implemented by ABSTRACT-ANALYSIS subclasses)
(defmethod analyse-one-segment ((self abstract-analysis) segment object) nil)

;;; determines the type of segment added by default (can stay NIL)
(defmethod default-segment-class ((self abstract-analysis)) nil)

;;; called after attach analysis to a score object
(defmethod analysis-init ((self abstract-analysis) object) 
  (loop for seg in (analysis-segments self) do 
        (setf (container-analysis seg) self)
        (segment-init seg)
        (analysis-init-segment self seg)))

(defmethod analysis-init-segment ((analysis t) segment) nil)

;;; called after the score object has been changed
(defmethod analysis-update ((self abstract-analysis) object) 
  (setf (analysis-segments self)
        (remove nil
                (mapcar #'(lambda (seg) (segment-update seg object))
                        (analysis-segments self)))))


(defmethod get-obj-dur ((self abstract-analysis))
  (if (not (null (analysis-object self)))
      (get-obj-dur (analysis-object self))
    (progn (analysis-init self nil)
      (loop for s in (analysis-segments self) maximize 
            (segment-end s)))))

;;;============================
;;; EVENT CALLBACKS
;;;============================

;;; Does something when an existing segment is clicked
(defmethod handle-segment-click ((self abstract-analysis) segment panel pos) 
  ;;; (print (list self pos))
  nil)

;;; Does something when an existing segment is double-clicked
(defmethod handle-segment-doubleclick ((self abstract-analysis) segment panel pos) 
  ;;; (print (list self pos))
  nil)


;;; Gets add-click event and adds a type of segment depending on the analysis type
;;; The segments handles its instantiation with adequate parameters depending on POS
;;; (this method can also be compeltely redefined by the analysis)
; (defmethod analysis-add-click ((self abstract-analysis) panel pos) nil)
(defmethod analysis-add-click ((self abstract-analysis) panel pos)
  (let* ((segment-type (default-segment-class self)))
    (when segment-type
      (let ((new-seg (make-instance segment-type)))
        (when (segment-handle-add-click new-seg self panel pos)
          (add-in-analysis self new-seg)
        )))))


;;; Gets add-key event and adds a type of segment depending on the analysis type
;;; The segments handles its instantiation with adequate parameters depending on the current selection in PANEL
;;; (this method can also be compeltely redefined by the analysis)
(defmethod analysis-add-key ((self abstract-analysis) panel) 
  (let* ((segment-type (default-segment-class self)))
    (when segment-type
      (let ((new-seg (make-instance segment-type)))
        (when (segment-handle-add-key new-seg self panel)
          (add-in-analysis self new-seg)
          )))))


(defmethod analysis-key-event ((self abstract-analysis) panel char)
  (case char
    (:om-key-delete 
     (delete-selected-segments self)
     (update-panel panel))
    (#\c (color-segment-selection self)
         (update-panel panel))
    (#\s (analysis-add-key self panel))
    (otherwise nil)))
  


;;;============================
;;; SEGMENT ABSTRACT CLASS 
;;;============================

; TODO: at this moment segment-data is not being used, is it necessary?
;  I think we can remove it since each type of segment stores the data
;  in a class attribute, it would suffice with a segment-tostring method
; ==> A common slot for storing the data allows external/abstract functions to access it with (segment-data my-segment)
; without preliminary knowledge of the segment type. this is the same principle as the methods of abstract class:
; this just ensures you there is something here that you can access and call other specific methods on it.
; IN OTHER WORDS, IN THE CURRENT DESIGN: SEGMENT-DATA is an open slot which is used and filled by the different analyses and is passed to them in order to perform interactions (e.g. draw-segment-data, etc.) 
(defclass! segment ()
  ((segment-data :accessor segment-data :initarg :segment-data :initform nil :documentation "analysis data related to this segment")
   (color :accessor color :initarg :color :initform :black)
   (selected :accessor selected :initform nil)
   (tb :accessor tb :initarg :tb :initform 0) (te :accessor te :initarg :te :initform 0)
   (container-analysis :accessor container-analysis :initform nil)))
 


;(defmethod omng-copy ((self segment))
;  `(let ((seg ,(call-next-method)))
     ;(setf (container-analysis seg) ,(container-analysis self))
     ;(segment-init seg)
;     seg))

(defmethod segment-p ((self segment)) t)
(defmethod segment-p ((self t)) nil)

;;; Abstract methods (to be implemented by SEGMENT subclasses)
(defmethod draw-segment ((self segment) view) nil)
(defmethod segment-begin ((self segment)) (or (tb self) 0))
(defmethod segment-end ((self segment)) (or (te self) 0))

;;; called at init
(defmethod segment-init ((self segment)) 
  (setf (tb self) (segment-begin self)
        (te self) (segment-end self))
  nil)

;;; called when the object changed
;;; if NIL: the segments will be removed
(defmethod segment-update ((self segment) object) 
  (setf (tb self) (segment-begin self)
        (te self) (segment-end self))
  self)

(defmethod segment-clicked-p ((self segment) panel pos) 
  (when (and (segment-begin self) (segment-end self))
  (let* ((x (om-point-h pos))
         (p1 (time-to-pixels panel (segment-begin self) ))
         (p2 (time-to-pixels panel (segment-end self))))
    (and (>= x p1) (<= x p2)))))

;;; Abstract method (to be implemented by ABSTRACT-ANALYSIS subclasses)
; TODO: should they belong to segment? are they really necessary?
; ==> Different analyses would display the same type of segment differently
; because the have different things stored in segment data. 
; This would not make much sense if segments are systematically subclassed as you seemed to suggest but
; the initial design here was to provide a limited number of segments which (in most cases) would not require subclassing anymore
(defmethod draw-segment-data ((self abstract-analysis) segment view) 
  (let ((str (segment-data-tostring self segment)))
    (when str
      (om-with-font *om-default-font1*
      (om-draw-string (time-to-pixels view (segment-begin segment))
                      (- (h view) 60) str)))))

(defmethod segment-data-tostring ((self t) segment) 
  (if (segment-data segment) 
      (format nil "~D" (segment-data segment))
    "segment"))

(defmethod segment-handle-add-click ((self segment) analysis panel pos) 
  (om-beep-msg "ADD CLICK IS NOT VALID IN THIS ANALYSIS SEGMENTS"))

(defmethod segment-handle-add-key ((self segment) analysis panel) 
  (om-beep-msg "ADD KEY IS NOT VALID IN THIS ANALYSIS SEGMENTS"))

;;; deletes selected segments
(defmethod delete-selected-segments ((self abstract-analysis))
  (mapcar #'(lambda (seg) (delete-from-analysis self seg))
          (selected-segments self))
  (setf (selected-segments self) nil)
  )

(defmethod delete-from-analysis ((self abstract-analysis) segment)
  (setf (analysis-segments self)
        (remove segment (analysis-segments self))))

;;; adds a new segment
(defmethod add-in-analysis ((self abstract-analysis) (new-seg segment))
  (setf (container-analysis new-seg) self)
  (segment-init new-seg)
  (setf (analysis-segments self)
        (sort (append (analysis-segments self) 
                      (list new-seg)) '< :key 'segment-begin))
  (analysis-init-segment self new-seg)
  (analysis-segments self))

(defmethod color-segment-selection ((self abstract-analysis))
  (when (selected-segments self)
    (let ((c (om-choose-color-dialog :color (color (car (selected-segments self))))))
      (when c 
        (loop for seg in (selected-segments self) do (setf (color seg) c)))
      )))

(defmethod next-segment ((self segment))
   (let ((pos (and (container-analysis self) 
                   (position self (analysis-segments (container-analysis self))))))
     (when pos (nth (1+ pos) (analysis-segments (container-analysis self))))))

(defmethod previous-segment ((self segment))
   (let ((pos (and (container-analysis self)
                   (position self (analysis-segments (container-analysis self))))))
     (when (and pos (> pos 0)) (nth (1- pos) (analysis-segments (container-analysis self))))))


;;;============================
;;; OM OBJECTS INTERFACE
;;;============================

(defmethod compatible-analysis-p ((analyse abstract-analysis) (object analyse-object)) t)
(defmethod compatible-analysis-p ((analyse t) (object t)) nil)

(defmethod initialize-instance :after ((self analyse-object) &rest args) 
  (setf (analysis self) (list! (analysis self)))
  self)


(defmethod omng-save ((self score-element) &optional values?)
  (if (analysis self) 
    `(let ((object ,(call-next-method)))
       (set-object-analysis object ,(omng-save (analysis self)))
       object)
    (call-next-method)
    ))

(defmethod copy-container ((self score-element) &optional (pere ()))
  (let ((object (call-next-method)))
    (set-object-analysis object (eval (omng-copy (analysis object))))
    object))


(defmethod set-object-analysis ((self t) (analyse t)) nil)

(defmethod set-object-analysis ((self analyse-object) (analyse abstract-analysis))
  (if (compatible-analysis-p analyse self)
    (progn
      (setf (analysis self) (list analyse))
      (setf (analysis-object analyse) self)
      (analysis-init analyse self)
      self)
    (om-beep-msg (format nil "~A analysis does not apply to ~A objects!" (type-of analyse) (type-of self)))))

(defmethod set-object-analysis ((self analyse-object) (analyse list))
  (let ((an-list (remove-if-not 
                 #'(lambda (item) (and (or (compatible-analysis-p item self) 
                                           (om-beep-msg (format nil "~A analysis does not apply to ~A objects!" (type-of item) (type-of self))))
                                       (subtypep (type-of item) 'abstract-analysis)))
                 analyse)))
    (setf (analysis self) an-list)
    (mapcar #'(lambda (an) 
                (setf (analysis-object an) self)
                (analysis-init an self)
                ) an-list)
    self))

(defmethod add-object-analysis ((self analyse-object) (analysis abstract-analysis))
   (if (compatible-analysis-p analysis self)
     (progn 
       (setf (analysis self)
             (cons analysis (analysis self)))
       (setf (analysis-object analysis) self)
       (analysis-init analysis self)
       self)
     (om-beep-msg (format nil "~A does not apply to ~A objects!" (type-of analyse) (type-of self)))))

(defmethod remove-object-analysis ((self analyse-object) analysis)
  (setf (analysis self) (remove analysis (analysis self))))

(defmethod run-analysis ((self analyse-object))
  (when (analysis self)
    (loop for an in (list! (analysis self)) do
          (object-analysis an self))
    t))




