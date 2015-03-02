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
;This file define the abstract class for metaobjects's editor having  connections 
;i.e. patches maquettes, etc.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;----------------------------
;relationEditor
;----------------------------
(defclass relationEditor (nonrelationeditor)  ()
   (:documentation "This is the general class for window containing a relationPanel, so windows for Patches, 
maquettes and hierarchical class editors.#enddoc#
#seealso# (relationPanel) #seealso#"))

(defmethod get-editor-panel-class ((self relationEditor))  'relationpanel)


;-------------Initialization
(defun open-new-RelationFrame (object name elements &optional ref pos size)
   (let* ((position (or pos (get-win-position object)))
          (siz (or size (get-win-size object)))
          (newwindow nil))
     (setf newwindow 
           (make-editor-window (get-editor-class object)
                               object name ref 
                               :winpos position
                               :winsize siz
                               :winshow nil))
     (om-with-delayed-redraw (panel newwindow)
       (om-with-delayed-update (panel newwindow)
         (mapc #'(lambda (elem)
                   (let ((newframe (make-frame-from-callobj elem)))
                     (om-add-subviews (panel newwindow) newframe)
                     (add-subview-extra newframe)
                     )) elements)
         (mapc #'(lambda (elem)
                   (update-graphic-connections elem elements)) (get-subframes (panel newwindow)))
         (add-window-buttons (panel newwindow))
         ))
     ;;; new jb
     (set-field-size (panel newwindow))
     ;(om-window-resized newwindow (om-view-size newwindow))
     ;(om-invalidate-view (panel newwindow))
     ;(om-select-window newwindow)
     newwindow))

;----------------------------------------------------
;MCL Events
;----------------------------------------------------

(defmethod close-editor-before ((self relationEditor))  
   (call-next-method)
   (mapc #'(lambda (frame) (close-frame frame)) (get-subframes (panel self))))

(defmethod close-editor-after ((self relationEditor)) 
   (call-next-method)
   (om-view-close-event self))

;;;(defmethod close-window ((self relationEditor))
(defmethod om-view-close-event ((self relationEditor))
   (compile-patch (object (panel self)))
   nil)

;----------------------------------------------------
;Methods redefinitions 
;----------------------------------------------------
(defmethod set-clipboard ((self relationEditor) val)
   (let* ((container (panel self)))
     (setf (scroll-scrap-pa container) val)))

(defmethod get-clipboard ((self relationEditor))
   (let* ((container (panel self)))
     (scroll-scrap-pa container)))  

(defmethod editor-paste ((self relationEditor)) 
   ; (print (eval (first (get-clipboard self)))))
   "Paste from the clipboard to 'self'."
   (let ((container (panel self)))
     (if (text-view self)
       (om-paste-command (text-view self))
       (let ((val (get-clipboard self)))
       (when val
         (let ((connections (second val))
               (new-boxes (eval (first val))))
           (loop for item in (get-actives container) do
                 (omG-unselect item))
           (om-with-delayed-update container
             (mapcar #'(lambda (object)
                         (setf (name object) (mk-unique-name container (name object)))
                         (setf (frame-position object) (paste-position object self))
                         (let ((new-frame (make-frame-from-callobj object)))
                           (omG-add-element container new-frame)
                           (omG-select new-frame))) new-boxes)
             )
             (remake-draggeds-connections container container new-boxes connections)
             (progn
               ; for multiple paste : make another copy in the clipboard
               (setf copies (mapcar #'(lambda (box) (omNG-copy box)) new-boxes))
               (set-clipboard self (list `(list ,.copies) (save-connections container container new-boxes))))
             )
         (om-invalidate-view container)
         ))) 
     t))

(defmethod editor-copy ((self relationEditor))
   "Copy in the clipboard the selected icons in 'self'."
   (let ((container (panel self)))
     (if (text-view self)
       (om-copy-command (text-view self))
       (let* ((subframes (mapcar #'(lambda (frame)
                                     (object frame)) (get-actives container)))
              (cannotcopy nil) copies)
         (loop for item in subframes
               while (not cannotcopy) do
               (setf cannotcopy (no-allow-copy-p item)))
         (if cannotcopy
           (om-beep-msg (string+ "Sorry I can't copy " cannotcopy))
           (progn
             (setf copies (mapcar #'(lambda (frame)
                                      (omNG-copy frame)) subframes))
             (set-clipboard self (list `(list ,.copies) (save-connections container container subframes))))))) t))

(defmethod do-undo ((self relationeditor)) 
  (let ((type (car (undo self))))
    (cond ((equal type 'remove)
           (let (framelist)
             (om-with-delayed-update (panel self)
               (mapc #'(lambda (elem)
                         (let ((newframe (make-frame-from-callobj elem)))
                           (push newframe framelist)
                           (om-add-subviews (panel self) newframe)
                           (add-subview-extra newframe)
                           )
                         ) (cdr (undo self)))
               )
             (setf (undo self) (append (list 'add) framelist)) 
           ))
          ((equal type 'add)
           (let (boxlist)
             (om-with-delayed-update (panel self)
               (mapc #'(lambda (frame)
                         (push (object frame) boxlist)
                         (omg-remove-element (panel self) frame)
                         ) (cdr (undo self)))
               )
               (setf (undo self) (append (list 'remove) boxlist)) 
           ))
          (t (setf (undo self) nil))))
  )

(defmethod editor-clear ((self relationEditor))
   "Clear the selected icons in 'self'."
   (let* ((container (panel self))
          (subframes (get-actives container)))
     (when subframes
       (delete-general container))))

(defmethod editor-cut ((self relationEditor))
   "Cut the selected icons in 'self'."
   (let* ((container (panel self))
          (subframes (get-actives container)))
     (if (text-view (editor self))
       (om-cut-command (text-view (editor self)))
       (when subframes
         (editor-copy self)
         (delete-general container)))))

(defmethod editor-save ((self relationEditor))
   "Relation windows save the object associated to the scroller, not the selected icons."
   (let ((thepatch (object (panel self))))
     (set-win-size thepatch (om-view-size (om-view-window self)))
     (set-win-position thepatch (om-view-position (om-view-window self)))
     (omNG-save thepatch nil)))


;; a faire...
(defmethod editor-save-as ((self relationEditor))
   "Relation windows save the object associated to the scroller, not the selected icons."
;   (let ((thepatch (omng-copy (object (panel self)))))
;;     (omng-add-element ???
;     (omNG-save thepatch nil))
)

(defmethod alias-editor ((self relationEditor))
   "Make aliases of the selected icons."
   (let* ((container (panel self))
          (subframes (get-actives container))
          object)
     (when subframes
       (mapc #'omG-unselect subframes)
       (mapcar #'(lambda (oldframe)
                   (setf object (omNG-make-alias (object oldframe)))
                   (when object
                     (let ((new-frame (make-frame-from-callobj object)))
                       (omG-add-element container new-frame)
                       (omG-select new-frame)))) subframes))))

;----------------------------------------------------
;Other Methods 
;----------------------------------------------------

(defmethod window-last-saved ((self t))  nil)
(defmethod window-last-saved ((self relationEditor))
   "Close the window 'self' and load the last save version of the patch associated to the scroller."
   (update-last-saved (object (panel self))))


;;; todo : find a good (relative!) paste position when fpx/fpy are out of the window...
(defmethod paste-position ((self t) view)
  (let ((fpx (om-point-h (frame-position self)))
        (fpy (om-point-v (frame-position self))))
    
    (when (or (> fpx (- (w view) 20)) (> fpy (- (h view) 20)))
      (setf fpx (- fpx (w view)) ; (om-point-h (om-mouse-position view))
            fpy (- fpy (h view)) ; (om-point-v (om-mouse-position view))
            ))
    (om-add-points (om-make-point 
                    (max 0 fpx)
                    (max 0 fpy))
                   (om-make-point 20 20))
    ))
    
      


;----------------------------
;relationPanel
;----------------------------

(omg-defclass relationPanel (nonrelationpanel) 
   ((scroll-scrap-pa :initform nil :allocation :class :accessor scroll-scrap-pa))
   (:documentation "This is the general class of editors containing objects plus a relation beetwen them.
Patches, maquettes and hierarchical class editors are sub-classes of this class.#enddoc#
#seealso# (patchPanel maquettePanel ClasstreePanel) #seealso#
#scroll-scrap-pa# Used for Copy and paste. #scroll-scrap-pa#"))

;------------------------------------------------------
;Other Methods
;------------------------------------------------------
(defmethod add-window-buttons ((self relationPanel))
   "Add extra buttons at the top-button in 'self'." nil)

(defmethod omG-add-element ((self relationPanel) frame)
   "Add a boxframe to the scroller, this method call the 'omNG-add-element' method with the objects referenced by 'self' and 'frame'."
   (omNG-add-element (object self) (object frame))
   (om-add-subviews self frame)
   (add-subview-extra frame)
   ;; dans om-add-subviews
   ;;(set-field-size self)
   )

(defmethod omg-remove-element ((self relationPanel) frame)
   "Remove a boxframe from the scroller, this method call the 'omng-remove-element' method with the objects referenced by 'self' and 'frame'."
   (omng-remove-element (object self) (object frame))
   (close-frame frame)
   (om-remove-subviews self frame))


(defmethod get-subframes ((self relationPanel))
   "Return a list with the boxes (boxframe's instances) subviews of 'self'."
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (boxframe-p icon)
                 (push icon rep))) (om-subviews self))
     rep))

(defmethod get-actives ((self relationPanel) &optional class)
   "Return a list with the selected boxes (boxframe's instances) subviews of 'self'."
   (declare (ignore scroll))
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (and (subtypep (type-of icon)(or class 'omboxframe))
                        (active-mode icon))
                 (push icon rep))) (om-subviews self))
     (reverse rep)))

(defmethod get-connections ((self relationPanel))
   "Get a list with all connections in 'self'."
  (let* (rep)
    (mapc #'(lambda (box)
              (setf rep (list+ rep (connections box)))) (get-subframes self))
    rep))

(defmethod get-actives-connections ((self relationPanel))
   "Get a list with the selected connections in 'self'."
   (let* (rep)
     (mapc #'(lambda (connec)
               (when (selected? connec)
                 (setf rep (list+ rep (list connec))))) (get-connections self))
     rep))


(defmethod click-in-connection ((self relationPanel) where)
   "Called by 'view-click-event-handler' this method verify if you click on a connection. If this is the case
this method set the select flag of the connection to T and return a list with the selected connections."
   (let ((controls (get-subframes self))
         rep)
     (loop for box in  controls 
           while (not rep) do
           (let* ((connection (connections box)))
             (loop for oneconnection in connection 
                   while (not rep) do
                       (let* ((where-click (point-in-connection oneconnection self where)))
                     (cond
                      ((numberp where-click)
                       (draw-connection oneconnection nil)
                       (unless (selected? oneconnection)
                         (unless (om-shift-key-p)
                           (mapc #'(lambda (control) 
                                     (deactivate-connect control)) 
                                 (remove oneconnection (get-connections self) :test 'equal)))
                         (setf (selected? oneconnection) t))
                       (cond 
                        ((om-shift-key-p)
                         (if (member (nth where-click (points oneconnection)) (point-sel oneconnection))
                           (setf (point-sel oneconnection) 
                                 (remove (nth where-click (points oneconnection))
                                         (point-sel oneconnection)))
                           (progn
                             (push (nth where-click (points oneconnection)) (point-sel oneconnection))
                             (scroll-points oneconnection))))
                        (t (unless (member (nth where-click (points oneconnection)) (point-sel oneconnection))
                             (setf (point-sel oneconnection)  (list (nth where-click (points oneconnection)))))
                           (scroll-points oneconnection)))
                       (draw-connection oneconnection t)
                       (setf rep t))
                      (where-click
                       (select-connection oneconnection)
                       (unless (om-shift-key-p)
                         (mapc #'(lambda (control) 
                                   (deactivate-connect control)) 
                               (remove oneconnection (get-connections self) :test 'equal)))
                       (setf rep t))
                      (t nil)))))) rep))


(defmethod make-move-after ((self relationPanel) dragged)
  (redraw-after self dragged)
  )

(defmethod redraw-after ((self relationPanel) dragged)
  "This method is called after moving one or more boxes, it redraw the connections involving in the moving operation."
  (let* ((frames (get-subframes self))
	 (rest-frame (set-difference frames dragged :test 'equal)))
;;; TEST
    (om-with-delayed-redraw 
	(mapc #'(lambda (oneobject)
		  (redraw-connections oneobject)
		  (mapc #'(lambda (source)
			    (when (is-connected? (object oneobject) (object source))
			      (redraw-connections source))) rest-frame)
		  (om-invalidate-view oneobject)
		  ) dragged)
      ;;(om-invalidate-view self)
      t)
    ))



(defmethod make-delete-before ((self relationPanel) deleted cible)
   "This method is called after deleting one or more boxes, 
it redraw the connections involving in the deleying operation."
   (if (move-and-not-action self cible)
     (real-make-delete-before  self deleted)))

;Sometimes you can not redraw connections, for exemple when you copy two editors from a patch
;to one maquette...
(defmethod move-and-not-action ((self t) (cible t)) t)

;;; deletes the connections with downstream boxes
(defmethod delete-connections-with-other-boxes (deletedboxframe otherframes patchpanel)
  (let ((boxes (get-elements (object patchpanel))))
    (mapc #'(lambda (conection) (draw-connection conection nil)) (connections deletedboxframe))
    (mapc #'(lambda (source)
              (when (is-connected? (object deletedboxframe) (object source))
                (mapc #'(lambda (conection) (draw-connection conection nil)) (connections source))
                (unconnected (object deletedboxframe) (object source))
                (update-graphic-connections source boxes))) otherframes)))
  
(defmethod real-make-delete-before ((self relationPanel) deleteds)
   (let* ((frames (get-subframes self))
          (rest-frame (set-difference frames deleteds :test 'equal)))
     (mapc #'(lambda (del) (delete-connections-with-other-boxes del rest-frame self)) deleteds)
     t))


;;;;------------------------------------------------------
;;;; Events
;;;;------------------------------------------------------
(defmethod priority-connecctions ((self relationPanel)) nil)

(defmethod om-draw-contents  ((self relationPanel))
 (if (priority-connecctions self)
       (progn
          (call-next-method)
          (mapc #'(lambda (frame)  (box-draw-connections frame t)) (get-subframes self))
          )
     (progn
       (mapc #'(lambda (frame) (box-draw-connections frame t)) (get-subframes self))
        (call-next-method))))

;;;;Click on the scroller not in a subview.
(defmethod control-actives ((view relationPanel) where)
  (when (and (editor view) (text-view (editor view)))
    (exit-from-dialog (text-view (editor view)) (om-dialog-item-text (text-view (editor view)))))
  (if (click-in-connection view where)
      (mapc #'(lambda (control) 
                (omG-unselect control)) (get-actives  view))
    
    (let* ((float (om-subtract-points (om-mouse-position view) where)))
      (unless (om-shift-key-p)
        (mapc #'(lambda (control) 
                  (deactivate-connect control)) (get-connections view)))
      (cond 
       ((om-command-key-p) 
        (make-undefined-box view where))
       (t (call-next-method))))))

(defmethod do-select-items-in-rect ((self relationPanel) rect) 
 (let (user-rect scratch-rect-i scratch-rect-n i-rect n-rect)
    (when rect
      (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
       (dolist (item (get-subframes self))
              (setf i-rect (om-pts-to-rect (om-view-position item) 
                                           (om-add-points (om-view-position item) (om-view-size item))))
              (setf scratch-rect-i (om-sect-rect user-rect i-rect))
              (unless (om-rect-empty scratch-rect-i) 
                (omG-select item))))))




(defmethod align-frame-boxes ((self relationPanel))
  (loop for icon in (get-subframes self) do
        (align-one-boxframe icon)))
  
