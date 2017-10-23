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

(in-package :om)


;;;=====================================
;;; ROUTE
;;;=====================================

(defclass RouteBox (OMBoxcall) 
  ((numouts :initform 1 :accessor numouts)))

(defmethod do-delete-one-input-extra ((self RouteBox))
   (setf (numouts self) (- (numouts self) 1)) t)

(defmethod do-add-one-input-extra ((self RouteBox))
   (setf (numouts self) (+ (numouts self) 1)) t)

(defmethod omNG-copy ((self RouteBox))
  `(let* ((copy ,(call-next-method)))
     (setf (numouts copy) ,(numouts self))
     copy))

(defmethod omNG-save ((self RouteBox) &optional (values? nil))
  `(let* ((routebox ,(call-next-method)))
     (setf (numouts routebox) ,(numouts self))
     routebox))

;;; NOTIFY ONLY THE ROUTED OUTPUT
(defmethod OMR-Notify ((self RouteBox))
  ;(print (list "NOTIFIED BOX" (name self)))
  ;(box-color self *notify-color*)
  (unless (push-tag self)
    (setf (push-tag self) t)
    (let ((listeners (remove-if-not 'active (listeners self))))
      (omNG-box-value self)
      (setf (state-lock self) t)
      (if (and (active self) listeners)
          (let ;((routed-output (position-if-not 'null (value self) :from-end t)))
              ((routed-outputs (loop for i = 0 then (+ i 1) 
                                     for v in (value self) 
                                     when v collect i)
                               ))
            (setf listeners (loop for lst in listeners
                                  when (remove nil 
                                                (mapcar #'(lambda (in) 
                                                            (and (connected? in) 
                                                                 (equal (first (connected? in)) self)
                                                                 (find (second (connected? in)) routed-outputs)))
                                                        (inputs lst)))
                                   collect lst))
            (mapcar 'omr-notify listeners))
        ;(omNG-box-value self)
        )
      (setf (state-lock self) nil)
      ))
  ;(box-color self *inactive-color*)
  )



;;;=====================================
;;; ROUTE GENERAL

(defun test-match (data test) 
  (if (functionp test) 
      (funcall test data)
    (equal test data)))

(defmethod! route (message &rest tests)
  :numouts 1
  :icon 161
  (values-list (copy-list (cons message 
                                (mapcar 
                                 #'(lambda (route-item) 
                                     (when (test-match message route-item) message))
                                 tests))))
  )

(defmethod get-boxcallclass-fun ((self (eql 'route))) 'RouteBox)


