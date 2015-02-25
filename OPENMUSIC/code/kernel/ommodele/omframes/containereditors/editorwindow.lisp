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
;Window, editor and panel abstract classes are defined in this file.
;Last Modifications :
;18/10/97  first date
;DocFile

;;; Notes :

;;; - ccl::window-close-nicely definition commentée
;;; - drag-receive-dropped-flavor definition commentée

(in-package :om)


;------------------------------------------------------------

;==============
;=== WINDOW ===
;==============

;;; obj est rajoute pour associer un objet a une fenetre
;;; pour les pb de menubar en multiplateforme
(omg-defclass EditorWindow (om-window) 
   ((editor :initform nil  :accessor editor)
    (obj :initform nil  :initarg :obj :accessor obj)))

(defmethod omG-make-new-icon-window ((self EditorWindow) &optional type)
  "Called from New menu item type say if it create a patch a maquettte or a folder."
  (editor-make-new-icon-window (editor self) type))

(defmethod om-minimum-size ((window editorwindow))
  (editor-minimum-size (editor window)))

(defmethod editor-minimum-size ((self t))
  #-linux (om-make-point 200 120)
  #+linux (om-make-point 200 150)
  )

(defmethod do-undo ((self t)) nil)

(defmethod undo ((self EditorWindow))  
   (do-undo (editor self)))

(defmethod list-presentation? ((self EditorWindow))
  (editor-list-presentation? (editor self)))

(defmethod omG-change-presentation ((self EditorWindow) presentation)
   "Show icons in 'self' by name by type or normal."
   (om-without-interrupts 
     (editor-change-presentation (editor self) presentation)
     (om-window-resized self (om-view-size self))
     ;; test :
     (om-invalidate-view self)
     ))

(defmethod omG-align-presentation ((self EditorWindow))
    (editor-align-presentation (editor self))
    ;; test :
    (om-invalidate-view self))


;;; regarder MCL peut etre a appeler dans chaque methode de events...
;;;(defmethod window-event :after ((self EditorWindow))
;;;(defmethod om-window-event :after ((self EditorWindow))

;;; C'ETAIT POUR LES TOOL TIPS
;;;(defmethod  om-view-mouse-moved-handler :after ((self EditorWindow) position)
;;;   (declare (ignore position))
;;;   (editor-event-after (editor self)))

(defmethod alias-window ((self EditorWindow))
   "Make aliases of the selected icons." 
   (alias-editor (editor self)))


(defmethod window-save ((self EditorWindow))
   (editor-save (editor self)))

(defmethod window-save-as ((self EditorWindow))
   (editor-save-as (editor self)))

(defmethod copy ((self EditorWindow))
   (or (editor-copy (editor self)) nil))

(defmethod paste ((self EditorWindow)) 
   (unless (editor-paste (editor self)) nil))

(defmethod duplicate-window ((self t))
   "Copy and paste the selected icons." nil)

(defmethod duplicate-window ((self EditorWindow))
   "Copy and paste the selected icons."
   (when (copy self) (paste self)))

(defmethod cut ((self EditorWindow)) (editor-cut (editor self)))

(defmethod clear ((self EditorWindow)) (editor-clear (editor self)))

(defmethod select-all ((self EditorWindow)) (editor-select-all (editor self)))

(defmethod get-clipboard ((self EditorWindow)) nil)

(defmethod get-editor-class ((self EditorWindow)) nil)

(defmethod window ((self EditorWindow)) self)

(defmethod panel ((self EditorWindow))
   (panel (editor self)))

(defmethod get-win-ed-size ((self t)) (om-make-point 335 275))

(defmethod get-win-ed-pos ((self t)) (om-make-point 10 40))


(defmethod om-print-window ((self editorwindow))
  (do-print-editor (editor self)))


(defmethod om-window-class-menubar ((self EditorWindow))
     (get-menubar (editor self)))


 
(defmethod make-editor-window ((class t) object name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
   (declare (ignore retain-scroll))
   (let* ((sizewin (or (and (om-point-p winsize) winsize)
                       (get-win-ed-size object)))
          (poswin (or (and (om-point-p winpos) winpos)
                      (get-win-ed-pos object)))
          (win (om-make-window 'EditorWindow
                               :window-title name
                               :position poswin 
                               :close close-p
                               :resizable resize
                               :maximize resize
                               :window-show nil
                               :toolbox (member :toolbox wintype) 
                               :size sizewin
                               :obj (editor-object-from-value object)))
          (editor (om-make-view class
                                :ref ref
                                :owner win
                                :object (editor-object-from-value object)
                                :position (om-make-point 0 0)
                               :size (om-interior-size win)
                                ))
          )
      (setf (editor win) editor)
      (om-add-menu-to-win win)  
      (when winshow (om-select-window win))
      (om-set-view-size editor (om-interior-size win))
      win))



(defmethod om-window-resized ((self EditorWindow) size)
  (declare (ignore size))
  (when (editor self)
    (call-next-method)
    (om-set-view-size (editor self) (om-interior-size self)))
)


(defmethod om-set-view-size ((self EditorWindow) size) 
   (declare (ignore size))
   (call-next-method)  
   (when (editor self)
     (om-without-interrupts 
       (om-set-view-size (editor self) (om-interior-size self))
       )))

(defmethod om-set-interior-size ((self EditorWindow) size) 
   (declare (ignore size))
   (call-next-method)  
 ; (om-without-interrupts 
 ;   (om-set-view-size (editor self) (om-interior-size self))
 ;)
   )

(defmethod om-view-key-handler ((self EditorWindow) char)
  (unless (call-next-method)
    (when char 
      (handle-key-event (editor self) char)
      )
    t))

(defmethod handle-key-event ((self t) char) nil)

(defmethod om-window-check-before-close ((self EditorWindow)) 
  (when (editor self) (editor-close? (editor self))))

(defmethod editor-close? ((self t)) t)

(defmethod om-window-close-event :before ((self EditorWindow))
  (when (editor self)
    (close-editor-before (editor self))))

(defmethod om-window-close-event :after ((self EditorWindow)) 
   (when (editor self)
       (loop for ed in (attached-editors (editor self)) do
                 (om-close-window ed))
       (close-editorFrame (editor self))
       (close-editor-after (editor self))
       ))

(defmethod om-view-key-handler :around ((self EditorWindow) char)
   (if (and char (editor self) (key-event-around (editor self) char))
     (call-next-method))
   t)

;;;(defmethod window-null-event-handler ((self EditorWindow) position)
;;;   (call-next-method)
;;;   (editor-null-event-handler (editor self)))
(defmethod om-window-mouse-moved-handler ((self EditorWindow) position)
   (call-next-method)
   (when (editor self) (editor-null-event-handler (editor self))))

(defmethod workspace-window-p ((self EditorWindow))
   (equal *current-workSpace* (and (editor self) (object (editor self) ))))

(defmethod workspace-window-p ((self t)) nil)

(defmethod get-info-window ((self EditorWindow))
  (let ((info-objs (get-info-window-components (editor self)))
        (i -1))
    (loop for item in (remove nil info-objs) do
          (show-info-window item (incf i 1)))))


;=============
;EDITOR
;=============

(defclass EditorView (OMContainerFrame select-object) ; (OMSimpleFrame) 
   ((mini-editor-p :initform nil :initarg :mini-editor-p :accessor mini-editor-p)
    (panel :initform nil :initarg :panel :accessor panel)
    (text-view :initform nil :accessor text-view)
    (ref :initform nil  :initarg :ref :accessor ref)
    (attached-editors :initform nil :accessor attached-editors)
    (undo :initform nil :accessor undo)))


(defmethod EditorView-p ((self EditorView)) t)
(defmethod EditorView-p ((self t)) nil)

(defmethod InternalEditor-p ((self EditorView)) 
   (EditorView-p (ref self)))

(defmethod window ((self EditorView)) (om-view-window self))

(defmethod editor ((self EditorView)) self)

(defmethod get-info-window-components ((self EditorView))
  (mapcar 'get-obj-for-info (get-actives (panel self))))

(defmethod close-editor-before ((self t)) nil)

(defmethod close-editor-before ((self EditorView))  
  (set-win-position (ref self) (om-view-position (om-view-window self)))
  (set-win-size (ref self) (om-view-size (om-view-window self)))
  (call-next-method))

(defmethod close-editor-after ((self EditorView))
  nil)



(defmethod editor-copy ((self EditorView))
   "Copy in the clipboard the selected icons in 'self'."
   t)

(defmethod editor-paste ((self EditorView))
   "Paste from the clipboard to 'self'."
   t)


(defmethod editor-list-presentation? ((self EditorView)) nil)

(defmethod editor-change-presentation ((self EditorView) presentation)
   "Show icons in 'self' by name by type or normal." 
   nil)

(defmethod editor-align-presentation ((self EditorView))
   "aligns boxes in editor" 
   nil)


(defmethod editor-make-new-icon-window ((self EditorView) &optional type)
   "Called from New menu item type say if it create a patch a maquettte or a folder."
   nil)

(defmethod editor-cut ((self EditorView))
   "Cut the selected icons in 'self'."
   nil)

(defmethod editor-clear ((self EditorView))
   "Clear the selected icons in 'self'."
   nil)

(defmethod editor-select-all ((self EditorView))
   "Select all icons in 'self'."
   nil)

(defmethod editor-save ((self EditorView))
   "Save the object associated to the selected icons in 'self'."
   nil)

(defmethod editor-save-as ((self EditorView))
   "Save copies of the object associated to the selected icons in 'self'."
   nil)

(defmethod do-print-editor ((self EditorView))
  (om-print-one-page self))

;===============

(defmethod om-view-click-handler ((self EditorView) where)
   (declare (ignore where))
   (if (and (mini-editor-p self) (not (selected-p self)))
     (progn (setf (selected-p self) t) (om-invalidate-view self t))
     (call-next-method)))

(defmethod om-draw-contents ((self EditorView))
   (call-next-method))

(defmethod get-pos-in-object ((self EditorView) where)
  (om-add-points (om-view-position self) where))

(defmethod om-set-view-size ((self editorView) size)
   (declare (ignore size))
   (call-next-method)
  (update-subviews self)
)

(defmethod handle-key-event ((self EditorView) char)
   (if (text-view self)
       (om-view-key-handler (text-view self) char)
     (handle-key-event (panel self) char)))


(defmethod editor-null-event-handler ((self EditorView)) nil)

(defmethod key-event-around ((self EditorView) char) t)

(defmethod editor-object-from-value ((self t)) self)

(defmethod update-editor-after-eval ((self EditorView) val)
  (setf (object self) (editor-object-from-value val))
  (om-invalidate-view self t))


(defmethod close-editorFrame ((self EditorView))
  (cond ((null (ref self)) nil)
        ((EditorView-p (ref self))
         (setf (attached-editors (ref self)) 
               (remove self (attached-editors (ref self)) :test 'equal))
          (om-invalidate-view (ref self) t))
         ((slot-initform-p (ref self))
          (change-initform-ed (ref self) (object self)))
         ((ominstance-p (ref self))
          (setf (Editorframe (ref self)) nil))
         ((boxtempobj-p (ref self))
          (setf (Editorframe (ref self)) nil)
          (when (frames (ref self))
            (om-invalidate-view (car (frames (ref self))))))
         ((is-boxpatch-p (ref self))
          (setf (Editorframe (ref self)) nil)
          (when (frames (ref self))
            (om-invalidate-view (car (frames (ref self))))))
         (t (special-close-editor-frame (ref self) self))))

(defmethod special-close-editor-frame (ref ed) nil)


(defmethod update-subviews ((self EditorView)) nil)

(defmethod close-attached-editors ((self EditorView))
   (loop for ed in (attached-editors self) do
         (om-close-window ed)))

(defmethod update-panel ((self EditorView) &optional (updateref nil))
  (when updateref
    (report-modifications self))
  (om-invalidate-view self))

;(defmethod lock-after-modif (boxframe)
;  (when (lock-button boxframe) (remove-lock-button boxframe))
;  (add-lock-button boxframe))


(defmethod lock-after-modif (boxframe)
  (unless (lock-button boxframe)
    (add-lock-button boxframe)))

(defmethod report-modifications ((self EditorView))
  ;(print (list "report moif" (ref self) (frames (ref self))))
  (cond ((null (ref self)) nil)
        ((and (or (is-boxpatch-p (ref self)) (boxtempobj-p (ref self))) (car (frames (ref self))))
         (lock-after-modif (car (frames (ref self))))
         (when (showpict (ref self))
           (if (boxtempobj-p (ref self))
             (update-miniview (car (frames (ref self))) (car (value (ref self))))
             (update-miniview (iconview (car (frames (ref self)))) (value (ref self)))
             )))
        ((EditorView-p (ref self))
         (change-in-int-editor (panel (ref self)) (panel self) (object self) (object self)))
        ;;; new sheet
        ((ref self)
         (change-in-int-editor (ref self) (panel self) (object self) (object self))
         )))

(defmethod change-in-int-editor ((self t) (internal t) newobj lastobj) t)

;;; when some editor parameters are modified externally (e.g. conext menu of the box)

(defmethod update-editor-controls ((self editorview)) nil)

;(defmethod (setf selected-p) (selected-p (self EditorView))
;   (setf (slot-value self 'selected-p) selected-p))


;;; play-editor-mixin method
(defmethod get-obj-to-play ((self editorview)) (object self))







