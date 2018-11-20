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
; Maquette Markers
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;-------------------Markers

(defmethod delete-general ((self MaquettePanel))
   "Remove boxes markers lines or connecctions from 'self'."
   (let ((markers (selected-mark-lines self)))
     (if markers
       (progn
         ;(draw-mark-lines self nil)
         (loop for item in markers do
               (setf (tempojlist (car item))
                     (remove (second item) (tempojlist (car item)) :test 'equal :key 'second)))
         (setf (selected-mark-lines self) nil)
         ;(draw-mark-lines self)
         (om-invalidate-view self)
         )
       (call-next-method))))

(defmethod update-after-mark  ((self MaquettePanel)) 
  (when (get-maquette-markers self)
    (loop for item in (get-maquette-markers self) do
          (loop for box in (tempojlist (object item)) do
                (let ((frame (car (frames (second box)))))
                  (cond
                   ((= 1 (first box))
                    (om-set-view-size frame (om-make-point (- (x item) (x frame)) (h frame)))
                    (om-invalidate-view frame))
                   ((= 0 (first box))
                    (update-movement frame (om-make-point (x item) (y frame))))))
                ))
    (om-invalidate-view self)))
                 
;;; modifie jb
(defmethod draw-mark-lines ((self MaquettePanel) &optional (mode t))
   (om-with-focused-view self
     (loop for item in (get-maquette-markers self) do
           (loop for frame in (tempojlist (object item)) do
                 (let* ((source (car (frames (second frame))))
                        (x1 (if (zerop (first frame)) (x source) (x+w source)))
                        (y1 (+ (y source) (h source)))
                        (x2 (x item))
                        (y2 (y item))
                        (size 2)
                        modepat)
                   (if (null mode) 
                       (om-with-line-size 2 (om-erase-line x1 y1 x2 y2))
                     (om-with-line-size 
                         (if (member (list (object item) (second frame)) (selected-mark-lines self) :test 'equal) 2 1)
                       (om-with-line '(2 4) (om-draw-line x1 y1 x2 y2)))
                     )
                   )))))


(defmethod get-maquette-markers ((self MaquettePanel))
   "Return a list of the markers in 'self'."
   (let* (rep)
     (mapc #'(lambda (item)
               (if (markerframe-p item)
                 (push item rep))) (om-subviews self))
     rep))

(defmethod click-in-marker-line ((self MaquettePanel) where)
   "T if where is on a marker connection."
   (let ((markers (get-maquette-markers self))
         rep)
     (loop for marker in  markers
           while (not rep) do
           (loop for box in (tempojlist (object marker))
                 while (not rep) do
                 (let* ((tempobj (second box))
                        (tempframe (car (frames tempobj)))
                        (begin (= 0 (car box))))
                   (when ;(and (>= x0 (- (x marker) 2)) (<= x0 (+ (x marker) 2)) (>= y0 (y+h tempframe)))
                       (om-point-in-line-p where (om-view-position marker) 
                                           (om-make-point (if begin (x tempframe) (x+w tempframe)) (y+h tempframe)) 4)
                       (setf rep (list (object marker) tempobj))))))
     (when rep
       (draw-mark-lines self nil)
       (cond 
        ((om-shift-key-p)
         (if (member rep (selected-mark-lines self) :test 'equal)
           (setf (selected-mark-lines self) (remove rep (selected-mark-lines self) :test 'equal))
           (push rep (selected-mark-lines self))))
        (t (setf (selected-mark-lines self) (list rep))))
       (draw-mark-lines self)
       )
     rep))


;========================================================
;========================================================
;========================================================
;========================================================
;Marker Class. It is not so good I think that we can make better
;========================================================
;========================================================
;========================================================
;========================================================



(defclass temp-marker (omboxcall)
   ((tempojlist :initform nil :accessor tempojlist)
    (offset :initform nil :initarg :offset :accessor offset))
   (:documentation "This is the class for red flag boxes into a maquette.
#seealso# (OMMaquette temporalbox markerframe)#seealso#
#tempojlist# This slot store a list of pairs (begin/end , temporal box)  where the second of each element are
boxes attached to the flag, the car of each element say if the box is attached at its begin or its end.#tempojlist#
#offset# Contains the x position of the box (in ms).#offset#"))


;------Predicats
(defmethod marker-p ((self temp-marker)) t)
(defmethod marker-p ((self t)) nil)


(defmethod (setf frame-position) ((pos integer) (self temp-marker))
  (setf (frame-position self) (om-correct-point pos)))
;-------Info
(defmethod get-frame-class ((self temp-marker)) 'markerframe)
(defmethod numouts ((self temp-marker)) 0)
(defmethod get-frame-name ((self temp-marker)) nil)
(defmethod do-add-one-input ((self temp-marker)) (om-beep))
(defmethod do-add-all-inputs ((self temp-marker)) nil)
(defmethod get-icon-box-class ((self temp-marker)) 'bandera)

;-------Constructor
(defun omNG-make-new-marker (posi name offset)
   (let ((rep (make-instance 'temp-marker 
                :name name
                :reference nil 
                :icon 143
                :offset offset
                :inputs nil)))
     (setf (frame-position rep) (borne-position posi))
     rep))

(defmethod make-frame-from-callobj ((self temp-marker))
   "Cons a simple frame for a temp-marker instance."
   (let* ((module (om-make-view 'markerframe 
                                 :position (frame-position self) 
                                 :size  (om-make-point 8 10)
                                 :object self)))
     (om-add-subviews module (setf (iconView module)
                                (om-make-view 'bandera
                                              :iconID (icon self)
                                              :help-spec (name self)
                                              :size (om-make-point 8 10)
                                              :position (om-make-point 0 0))))
     (setf (name module) (name self))
     (setf (frames self) (list module))
     module))

;-----------Edition

(defmethod OpenEditorframe ((self temp-marker)) nil)

(defmethod change-offset ((self temp-marker) offset)
   "Move 'self' to the new position 'offset'. If 'self' has a simple frame active the graphic update is made too."
   (if (and (integerp offset) (not (minusp offset)))
     (if (car (frames self))
       (let* ((container (om-view-container (car (frames self))))
              (sys-etat (get-system-etat container))
              (new-pos (point2pixel container  (om-make-big-point offset 0) sys-etat)))
         (OMGMoveObject (car (frames self)) new-pos)
         (setf (offset self) offset))
       (setf (offset self) offset))
     (om-beep-msg "incorrect offset value")))

(defmethod add-mark ((self TemporalBox) (marker temp-marker) typesource)
   "Push the pair ('typesource' 'self') in the tempojlist slot of 'marker' (connect)."
   (push (list typesource self) (tempojlist marker)))
  
(defmethod del-mark ((self TemporalBox) (marker temp-marker) typesource )
   "Remove the pair ('typesource' 'self') from the tempojlist slot of 'marker' (unconnect)."
   (setf (tempojlist marker) (remove (list typesource self) (tempojlist marker) :test 'equal)))

(defmethod rename-marker ((self temp-marker) newname)
  (setf (name self) newname)
  (when (car (frames self)) (om-view-set-help (iconview (car (frames self))) newname)))

;------------TOOLS
(defun copy-markers (listsource listtarget)
   (loop for  marker in listsource
         for i = 0 then (incf i) do
         (when (equal (class-name (class-of marker)) 'temp-marker)
           (loop for item in (tempojlist marker) do
                 (push (list (first item)
                             (nth (position (second item) listsource :test 'equal) listtarget))
                       (tempojlist (nth i listtarget)))))))


(defun mk-markers-list (list)
   (let (rep)
     (loop for marker in list
           for i from 0 to (length list) do
           (when (equal (class-name (class-of marker)) 'temp-marker)
             (loop for item in (tempojlist marker) do
                   (push (list i (first item) (position (second item) list :test 'equal))
                         rep))))
     rep))

(defun remk-markers (list boxes)
   (loop for item in list do
         (let ((box (nth (first item) boxes)))
           (when (and box (third item))
             (push (list (second item)
                         (nth (third item) boxes))
                   (tempojlist box))))
         ))
      
;-------------------------------------------
;Simple frame
;-------------------------------------------

;------------simple frame class
(defclass markerframe (boxframe) ())

(defmethod markerframe-p ((self markerframe)) t)
(defmethod markerframe-p ((self t)) nil)

(defmethod om-view-click-handler ((self markerframe) where) nil)
 
(defmethod change-colorframe ((self markerframe) newcolor) nil)

(defmethod size-offset-frame-update ((self markerframe) container)
  (declare (ignore container))
  (om-beep-msg "Error"))

(defmethod init-size&position ((self t) container)
  (declare (ignore container)))


;------------Icon class
(defclass bandera (icon-box) ())

(defmethod om-drag-start ((self bandera))
  (unless (or (om-command-key-p) (om-shift-key-p))
    (call-next-method)))

(defmethod om-view-cursor :around ((self bandera))
   *om-arrow-cursor*)

(defmethod om-draw-contents ((self bandera))
  (call-next-method)
  (if (selected-p self) 
      (om-with-focused-view self 
        (om-with-fg-color self *om-dark-gray-color*
          (om-fill-rect 1 0 7 6)))))

(defmethod om-draw-contents ((self markerframe)) nil)


(defmethod om-view-click-handler ((self bandera) where)
  (let ((frame (om-view-container self)))
    (mapc #'(lambda (control) 
              (omG-unselect control)) (get-actives (om-view-container frame)))
    (OMG-select frame)
    (when (and (show-con? (om-view-container frame)) (om-shift-key-p))
      (drag-out-line self where))
    t))
  
(defmethod om-get-menu-context ((self bandera))
  (list (om-new-leafmenu "Change Position" 
                         #'(lambda () (change-offset-dialog (om-view-container self))))
        (om-new-leafmenu "Get Info" 
                         #'(lambda () (show-info-window (get-obj-for-info (om-view-container self)) 1)))))

(defmethod om-view-doubleclick-handler ((icon bandera) where)
  (let* ((frame (om-view-container icon)))
    (change-offset-dialog frame)))

(defmethod change-offset-dialog ((frame markerframe))
  (let* ((marker (object frame))

         (dialog (om-make-window 'om-dialog
                                :size (om-make-point 180 80)
                                :window-title (name marker)
                                :position (om-add-points (om-view-position (window frame)) (om-mouse-position (om-view-container frame)))))
         (offsettext (om-make-dialog-item 'om-editable-text 
                                         (om-make-point 20 25)
                                         (om-make-point 60 20)
                                         (format () "~D" (offset marker))
                                         :font *om-default-font2*)))
    
    (om-add-subviews dialog 
                     (om-make-dialog-item 'om-static-text (om-make-point 15 3) (om-make-point 60 20) (om-str :offset)
                                          :font *om-default-font2b*)
                     offsettext
                    
                     (om-make-dialog-item 'om-button (om-make-point (- (w dialog) 80) 5) (om-make-point 70 15) (om-str :cancel)
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog ()))
                                          ;:focus nil
                                          ;:default-button nil
                                          )
                     (om-make-dialog-item 'om-button (om-make-point (- (w dialog) 80) 30) (om-make-point 70 15) (om-str :ok)
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (let* ((newoffset (read-from-string (om-dialog-item-text offsettext)))
                                                              (dialogrep (if (and (integerp newoffset) (plusp newoffset))
                                                                             newoffset (om-beep-msg (om-str :offset-error)))))
                                                         (om-return-from-modal-dialog dialog newoffset)
                                                         ))
                                          ;:focus t
                                          :default-button t
                                          ))
  (let ((rep (om-modal-dialog dialog)))
    (when rep (change-offset marker rep)))
  ))

(defmethod drag-out-line ((self bandera) where)
   (let* ((panel (panel (om-view-window self)))
          (initpoint (om-convert-coordinates where self panel))
          (rx (om-point-h initpoint))
          (ry (om-point-v initpoint)))
     (om-init-motion-click self where
                               :motion-draw 'draw-connection-drag :draw-pane panel :display-mode 5
                               :release-action 'release-marker-connection)
     ))

(defmethod release-marker-connection ((self bandera) initpos pos) 
  (let* ((panel (panel (om-view-window self)))
         (initpoint (om-convert-coordinates pos self panel ))
         (rx (om-point-h initpoint))
         (ry (om-point-v initpoint))
         ctrl)
    (setf ctrl (om-find-view-containing-point panel (om-make-point rx ry)))
    (connect-box (om-view-container self) ctrl)
    ))

(defmethod init-size&position ((self markerframe) container)
   (let ((sysetat (get-system-etat container)))
     (om-set-view-position self (om-make-point (om-point-h (point2pixel container (om-make-big-point (offset (object self)) 0)
                                                               sysetat)) (- (h container) 10)))))

(defmethod OMGMoveObject ((self markerframe) new-position)
   "Move 'self' to 'new-position'"
   (if (< (om-point-h new-position) 0)
     (om-beep-msg "Object must start after 0"))
   (let ((cantmove? (can-move-marker? self new-position)))
     (if cantmove?
       (progn
         (om-beep-msg "The selected temporal object does not allow this movement")
         (omg-select (car (frames cantmove?))))
       (progn
         ;(mapc #'(lambda (conection)
         ;          (draw-connection conection nil)) (connections self))
         ;(draw-mark-lines (om-view-container self) nil)
         (setf new-position (om-make-point (om-point-h new-position) (- (h (om-view-container self)) 10)))
         (om-set-view-position self new-position)
         (setf (frame-position (object self)) (borne-position new-position))
         (setf (offset (object self)) (om-point-h (pixel2point (om-view-container self) (om-make-point (x self) 0))))
         (update-after-mark (om-view-container self))
         ;;(draw-mark-lines (om-view-container self))
         ;; ***
         ;(om-invalidate-view (om-view-container self))
         ))))

(defmethod can-move-marker? ((self markerframe) new-position)
   (let* ((marker (object self))
          (attached (tempojlist marker))
          (mark-list (get-maquette-markers (om-view-container self)))
          (offset (om-point-h (pixel2point (om-view-container self) new-position)))
          rep)
     (loop for item in attached 
           while (not rep) do
           (let* ((temobj (second item))
                  (startmarker (is-marketed temobj 0 mark-list))
                  (finmarker (is-marketed temobj 1 mark-list)))
             (cond
              ((and startmarker (not (equal startmarker marker)))
               (if (<= offset (offset startmarker))
                 (setf rep temobj)))
              ((and (equal startmarker marker) finmarker)
               (if (>= offset (offset finmarker))
                 (setf rep temobj)))
              ((and finmarker (not (equal finmarker marker)))
               (if (>= offset (offset finmarker))
                 (setf rep temobj)))
              ((and (equal finmarker marker) startmarker)
               (if (<= offset (offset startmarker))
                 (setf rep temobj)))
              ((equal finmarker marker)
               (unless (allow-strech-p (soft-get-mus-ob temobj) 0)
                 (setf rep temobj))
               (if (<= (om-point-h new-position) (x (car (frames temobj))))
                 (setf rep temobj))))))
     rep))





