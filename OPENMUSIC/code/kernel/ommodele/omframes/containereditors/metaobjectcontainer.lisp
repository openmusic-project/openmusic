;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;General Container and editor for the OM MetaObjects.
;This file define the abstract classes metaobj-editor et metaobj-panel
;Last Modifications :
;18/10/97  first date
;DocFile

(in-package :om)


;----------------------------------
;PANEL
;----------------------------------
;;;(defclass metaobj-panel (scroller OMContainerFrame om-view-d-d) ()
;;;   (:default-initargs 
;;;     :field-size (om-make-point 50 50)
;;;     :drag-allow-move-p t
;;;     :drag-auto-scroll-p t
;;;     :drag-accepted-flavor-list (list :|OMVW| :|OMSC| :|hfs | #$flavorTypePromiseHFS :|PICT|))
;;;   (:documentation "General class for scrollers in meta-object's editor window.
;;;This class inherites from om-view-d-d, so all scrollers allow  drag and drop, which is the first way for edition.#enddoc#
;;;#seealso# (om-view-d-d nonrelationpanel) #seealso#"))


(defclass metaobj-panel (om-scroller OMContainerFrame om-view-drop) ()    ;;;  
   (:default-initargs :scrollbars t)
   (:documentation "General class for scrollers in meta-object's editor window.
This class inherites from om-view-d-d, so all scrollers allow  drag and drop, which is the first way for edition.#enddoc#
#seealso# (om-view-d-d nonrelationpanel) #seealso#"))

(defmethod metaobj-panel-p ((self t)) nil)
(defmethod metaobj-panel-p ((self metaobj-panel)) t)

(defclass metaobj-editor (EditorView)  ()
   (:documentation "xx"))

;The editor frame of the object asociated with the scroller is set to NIL.
(defmethod close-editor-after ((self metaobj-editor))
  (when (object (panel self))
    (setf (Editorframe (object (panel self))) nil)))


(defmethod panel-position ((self metaobj-editor)) (om-make-point 0 0))

;;;(defmethod panel-size ((self metaobj-editor)) 
;;;  (om-make-point (- (w self) 15) (- (h self) 15)))

(defmethod panel-size ((self metaobj-editor)) 
   (om-make-point (w self) (h self)))


(defmethod update-subviews ((self metaobj-editor))
   (om-set-view-size (panel self) (panel-size self))
   (set-field-size (panel self)))

(defmethod get-editor-panel-class ((self metaobj-editor))  'metaobj-panel)

(defmethod set-panel-color ((self metaobj-panel)) 
  (om-set-bg-color self *om-white-color*))

;;; scrollbars params = '(scrollbars retain-scrollbars)
(defmethod metaobj-scrollbars-params ((self t))  (list t nil))


(defmethod initialize-instance ((self metaobj-editor) &rest rest)
  (declare (ignore rest))
  (call-next-method)
  (let* ((ed-view (om-make-view (get-editor-panel-class self)
                                                 :object (object self)
                                                 :scrollbars (first (metaobj-scrollbars-params self))
                                                 :retain-scrollbars (second (metaobj-scrollbars-params self))
                                                 :position (panel-position self) 
                                                 :size  (panel-size self)))
         )
    (setf (panel self) ed-view)
    (om-add-subviews self ed-view)
    (set-panel-color ed-view)
    self))



;----------------------------------
;PANEL
;----------------------------------

(defmethod delta-scroll ((self metaobj-panel))
   "Return a point with the scroll positions"
   (om-make-point (om-h-scroll-position self) (om-v-scroll-position self)))


(defmethod pos-panel2editor ((self metaobj-panel) point)
   (om-subtract-points (om-add-points point (om-view-position self)) (delta-scroll self)))
;----------------------------------


(defmethod window ((self metaobj-panel)) 
   "Return the view window of 'self'."  
   (window (editor self)))

(defmethod editor ((self metaobj-panel)) 
   "Return the editor containing 'self'."
   (om-view-container self))

(defmethod panel  ((self metaobj-panel)) self)

(defmethod direct-window-p ((self metaobj-panel))
   "TRUE if 'self' is the only panel in the window."
   (not (ref (editor self))))

(defmethod (setf object) ((metaobj t) (self metaobj-panel))
   (call-next-method)
   (when (editor self)
     (setf (object (editor self)) metaobj)))

(defmethod omG-rename ((self metaobj-panel) new-name)
   (om-set-window-title (window self) new-name)
   (omNG-rename self new-name))

;--------------- EVENTS

(defmethod view-position-changed ((self metaobj-panel) (view om-view))
   (set-field-size self))

;;; pb pour les maquettes!!!
(defmethod om-remove-subviews ((self metaobj-panel) &rest subviews)
   (declare (ignore subviews))
   (call-next-method)
   (set-field-size self))

(defmethod set-field-size ((self  t)) t)

(defmethod set-field-size ((self metaobj-panel))
  (let ((max-h 200) (max-v 150) bottom);((max-h 50) (max-v 150) bottom) 
    (mapcar 
     #'(lambda (view)
	 (let ((bottom (om-add-points (om-view-position view) (om-view-size view))))
	   (setf max-h (max max-h  (om-point-h bottom)))
	   (setf max-v (max max-v  (om-point-v bottom)))))
     (om-subviews self))
    (setf bottom (om-make-point (+ max-h 15) (+ max-v 15)))
    (ignore-errors 
      (when (not (om-points-equal-p bottom (om-field-size self)))
	(om-set-field-size self bottom)))
    (om-point-v bottom)))
          

(defmethod handle-key-event ((self metaobj-panel) char) nil)

;;; en attendant de savoir bien effacer le hilite...
(defmethod om-view-drag-hilite-p ((self metaobj-panel)) nil)

(defmethod om-drag-enter-view ((view metaobj-panel))
   "Called whenever a drag enters a drag & drop within 'self'."
   (declare (special *OM-drag&drop-handler*))
   (unless (equal (container-view *OM-drag&drop-handler*) view)
     (call-next-method)))
