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
;This file implements the class for icon+name frames.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defvar *size-little-icon-finder* 16 "Default icon size when the window is in mode name or list.")
(defvar *size-big-icon-finder* 32 "Default icon size when the window is in mode icon.")
(defvar *x-offset-icon-finder* 20 "Delta x position in pixels.") 

;===========================================
;ICON FINDER CLASS
;===========================================
(omg-defclass icon-finder (OMCompoundFrame om-view-drop)
   ((nameView :initform nil :initarg :nameView :accessor nameView)
    (iconView :initform nil :initarg :iconView :accessor iconView)
    (triangle :initform nil :accessor triangle)
    (container-p :initform nil :accessor container-p)
    (newviews :initform nil :accessor newviews)
    (change-name-p :initform nil :accessor change-name-p)
    (bx :initform 0 :accessor bx)
    (by :initform 0 :accessor by)
    (fil :initform 0 :accessor fil)
    (col :initform 0 :accessor col))
   (:documentation "Icon-finder is the class used to visulisate the most meta objects as a simple frame.
patches, folders, maquettes in the workspace are icon-finder instances.
in difference of OMContainerFrame this class is not graphic independent, 
so this class inherite from view. #enddoc#
#seealso# (class-icon-finder folder-icon-finder genfun-icon-finder maquette-icon-finder patch-icon-finder) #seealso#
#nameView# The name subview of the icon-finder, it is a icon-finder-name instance.#nameView#
#iconView# The icon subview of the icon-finder, it is a icon-finder-icon instance.#iconView#
#triangle# Some icon-finder as folders, classes or package have a triangle to open and close, 
this slot store a subview containing it subview if exists.#triangle#
#container-p# TRUE if the icon can be abierto.#container-p#
#newviews# Additional subviews for some icon-finders (slots for exemple) are stored in this slot.#newviews#
#change-name-p# TRUE if you can change the name of the icon's name.#change-name-p#
#col# The COL index of the icon in list mode.#col#
#fil# The ROW index of the icon in list mode.#fil#
#by# The h-position in pixels of the icon in normal mode.#by#
#bx# The v-position in pixels of the icon in normal mode.#bx#"))

(defmethod icon-finder-p ((self icon-finder)) t)
(defmethod icon-finder-p ((self t)) nil)


;------------CONSTRUCT
(defun make-icon-from-object (object bx by x y)
   (let ((icon (make-icon-finder object bx by x y)))
     (make-extra-info icon object)
     (setf (object icon) object)
     (add-extra-subviews icon)
     (setf (name icon) (name object))
     (push icon (frames object))
     icon))

(defmethod get-finder-iconID ((self t)) (icon self))

(defun make-icon-finder (object bx by x y)
  (let* ((class (get-class-icon object))
         (name (get-name-icon object))
         (namefont (get-view-font-icon object))
         icon-finder)
    (setf icon-finder (om-make-view class
                                    :object object
                                    :nameView (om-make-dialog-item (get-class-icon-name object) 
                                                                   (om-make-point 0 32) 
                                                                   (om-make-point (get-name-size name namefont) 16)
                                                                   (string-downcase name) 
                                                                   :value name
                                                                   :font namefont
                                                                   )
                                    :iconView (om-make-view (get-class-icon-icon object)
                                                            :position (om-make-point 0 32) 
                                                            :size (om-make-point 32 32)
                                                            :help-spec (string-until-cr (get-documentation object))
                                                            :iconID (get-finder-iconID object)
                                                            )))
    (om-add-subviews icon-finder (iconView icon-finder))
    (om-add-subviews icon-finder (nameView icon-finder))
    (setf (fil icon-finder) x (col icon-finder) y (bx icon-finder) bx (by icon-finder) by)  
    icon-finder))

(defmethod get-finder-iconID ((self t)) (icon self))

;(defmethod om-view-click-handler ((self patch-icon-frame) pos) (om-inspect self))

(defmethod add-icon-finder ((self icon-finder) container)
  ;; (unless (equal (om-get-bg-color (nameview self)) *om-transparent-color*)
  ;;  (om-set-part-color (nameview self) :body (om-get-bg-color container)))
  (om-add-subviews container self)
  (set-size self container)
  (if (big-icon-p (editor container))
      (change-icon-position self (bx self) (by self))
      (progn
	(add-extras-infos self)
	(flet ((match (x) (equal (col x) (col self))))
	  (let* ((controls (get-subframes container))
		 (pos (position-if #'match controls))
		 final-list)
	    (when pos
	      (setf final-list (subseq controls (+ pos 1)))
	      (change-icon-position self (fil self) (col self))
	      (mapc #'(lambda (icon)
			(change-icon-position icon (fil icon) (+ (col icon) 1)))
		    final-list)
	      )))
	(when (container-p self)
	  (add-triangle self container))
	;; new jb
	(om-invalidate-view container t)
	;;
	)))


(defun add-triangle (self container)
  (let ((triangle (om-make-view 'triangle-icon
                                :position (om-make-point (- (x self) 20) (+ (y self) 5))
                                :size (om-make-point 11 11)
                                :icon-finder self
                                :iconID 164)))
    (setf (open? triangle) nil)
    (om-add-subviews container triangle)
    (setf (triangle self) triangle)))


;PUT the position and the size to the icon parts
(defmethod change-icon-position ((self icon-finder) x y)
   (if (big-icon-p (editor (om-view-container self)))
     (progn
       (setf (bx self) x)
       (setf (by self) y)
       (om-set-view-position self (om-make-point x y))
       (set-icon-pos (object self) (om-make-point x y))
       (set-field-size (om-view-container self)))
     (progn
       (setf (fil self) x)
       (setf (col self) y)
       (om-set-view-position self (om-make-point (+ 15 (* *x-offset-icon-finder* x))
                                           (* (+ 5 *size-little-icon-finder*) y)))
       (when (triangle self)
         (om-set-view-position (triangle self) (om-make-point (- (x self) 20)
                                                        (+ (y self) 5))))
       (change-sub-views-position self))))

(defmethod change-sub-views-position ((self icon-finder))
   (let ((x (x self))
         (y (y self)))
     (mapcar #'(lambda (new-view)
                 (om-set-view-position new-view (om-make-point (incf x 75) y))
                 (om-add-subviews (panel (om-view-container  self)) new-view)) (newviews self))))


(defmethod set-size ((self icon-finder) container)
   (let (x y sizex sizey)
     (if (big-icon-p (editor container))
       (progn
         (om-set-view-size (iconView self) (om-make-point *size-big-icon-finder* *size-big-icon-finder*))
         (setf x (bx self))
         (setf y (by self))
         (setf sizey (+ 16 *size-big-icon-finder*))
         (setf sizex (max *size-big-icon-finder* (get-length-name (nameView self))))
          )
       (progn
         (om-set-view-size (iconView self) (om-make-point *size-little-icon-finder* *size-little-icon-finder*))
         (setf x (+ 15 (* *x-offset-icon-finder* (fil self))))
         (setf y (* (+ 5 *size-little-icon-finder*) (col self)))
         (setf sizex (+ 5 *size-little-icon-finder* (get-length-name (nameView self))))
         (setf sizey *size-little-icon-finder*)))
     (om-set-view-size self (om-make-point sizex sizey))
     (om-set-view-position self (om-make-point x y))
     (om-set-view-position (iconView self) (get-position-icon (iconView self)))
     (om-set-view-position (nameView self) (get-position-name (nameView self)))))


;DELETE
(defmethod delete-sub-icons ((self icon-finder) container)
  (let* ((controls (get-subframes container))
         (pos (position self controls :test 'equal)))
    (when pos
      (let ((final-list (subseq controls (+ pos 1)))
            (continue t) (i 0))
    (loop while (and final-list continue) do
          (if (> (fil (car final-list)) (fil self))
              (progn
                 (om-remove-subviews container (car final-list))
                 (remove-triangle (pop final-list) container)
                 (incf i))
            (setf continue nil)))
    (mapc #'(lambda (icon)
              (change-icon-position icon (fil icon) (- (col icon) i))) final-list)))
    ))

(defmethod remove-triangle ((self icon-finder) container)
   (when (not (big-icon-p (editor container)))
     (let ((views-list (om-subviews container)))
       (when (and (triangle self) (member (triangle self) views-list :test 'equal)) 
         (om-remove-subviews container (triangle self)))
       (mapc #'(lambda (item)
                 (when (member item views-list :test 'equal)
                   (om-remove-subviews container item))) (newviews self)))))

(defmethod delete-icon-finder ((self icon-finder) container &optional (update t))
   (if (big-icon-p (editor container))
     (om-remove-subviews container self)
     (progn
       (remove-triangle self container)
       (when (and (triangle self) (open? (triangle self)))
         (delete-sub-icons self container))
       (when update (up-from container (col self)))
       (om-remove-subviews container self)))
     ;;; new
     (om-invalidate-view container t)
     (set-field-size container)
     ;;;
   )

(defmethod omg-add-element-in-icon ((self icon-finder) new-obj)
   (if (or (big-icon-p (editor (om-view-container self)))
           (not (open? (triangle self))))
     (omNG-add-element (object self) new-obj)
     (let* ((newframe (make-icon-from-object  new-obj 22 22 1 1))
            (frames (get-subicons-finder self (om-view-container self))))
       (setf (fil newframe) (+ 1 (fil self)))
       (if frames
         (setf (col newframe) (findcol (om-view-container self) frames newframe))
         (setf (col newframe) (+ 1 (col self))))
       (add-icon-finder newframe (om-view-container self))
       (omNG-add-element (object self) new-obj) t)))

(defmethod get-subicons-finder ((self icon-finder) container)
   (let* ((controls (get-subframes container))
         (pos (position self controls :test 'equal))
         (final-list (subseq controls (+ pos 1)))
         (continue t) rep)
     (loop while (and final-list continue) do
          (if (> (fil (car final-list)) (fil self))
            (push (pop final-list) rep)
            (setf continue nil)))
     (reverse rep)))

(defmethod get-parent ((self icon-finder) container)
  (let* ((controls (get-subframes container))
         (pos (position self controls :test 'equal))
         (final-list (reverse (subseq controls 0 pos)))
         rep)
    (when final-list
      (loop while (not rep) do
          (if (< (fil (car final-list)) (fil self))
              (setf rep (pop final-list)) (pop final-list))))
     rep))
    
;----------------------------------INTERFACE

(defmethod omG-select :before ((self icon-finder))
   "The nameview of 'self' is also selected."
  (when (not (active-mode self)) 
    (setf (selected-p (nameView self)) t)
    ))

(defmethod omG-unselect :before ((self icon-finder))
   "The nameview of 'self' is also unselected."
   (when (active-mode self)
       (setf (selected-p (nameView self)) nil)
       ))
  

(defmethod open-icon-win ((self icon-finder) where)
   "Called when you double-click on 'self'."
   (declare (ignore where))
   (OpenObjectEditor (object self))
   (when (and (not (big-icon-p (editor (om-view-container self))))
              (triangle self)
              (open? (triangle self)))
     (setf (iconID (triangle self)) 164)
     (setf (open? (triangle self)) nil)
     ;;;(om-draw-contents (triangle self))
     (om-invalidate-view (triangle self))
     (delete-sub-icons self (om-view-container self))))
      

(defmethod OMGMoveObject ((self icon-finder) new-position)
   "Set the self's position to new-position."
   (when (big-icon-p (editor (om-view-container self)))
     (setf new-position (borne-position new-position))
     (change-icon-position self (om-point-h new-position) (om-point-v new-position))
     (set-field-size (om-view-container self))
     (om-highlight-view self nil)
   ))

 
;;; debug jean
(defmethod omG-change-container ((self icon-finder) container position)
   "Move 'self' from its view-container to 'container' at 'position'."
   (if (find-if #'(lambda (item) (string-equal (name (object item)) (name (object self)))) (get-subframes container))
     (om-beep-msg (string+ "An item named " (name self) " already exists in " (name (object (editor container))) "!"))
     (progn
       (let ((oldcontainer (om-view-container self))
             (oldfolder nil)
             (newitem (make-icon-finder (object self) (om-point-h position) (om-point-v position) 
                                        (fil self) (col self))))
         ;; aqui
         (loop for fr in (get-subframes oldcontainer) 
               while (not oldfolder)
               do (loop for elt in (get-elements (object fr))
                        do (when (equal (object self) elt)
                             (setf oldfolder (object fr)))))
         (unless oldfolder (setf oldfolder (object oldcontainer)))
         ;;  
         (delete-icon-finder self oldcontainer)
         (unless (big-icon-p (editor container))
           (let* ((frames (get-subframes container))
                  (newcol (findcol container frames self)))
             (setf (col self) newcol)
             (setf (fil self) 1)))
         (add-icon-finder newitem container)
         (omNG-change-container (object newitem) oldfolder (object container)) 
         t))))
  

(defmethod show-in-packtree ((self t) (pack OMFolder)) t)

(defmethod show-in-packtree ((self t) (pack OMPackage)) nil)
(defmethod show-in-packtree ((self OMPackage) (pack OMPackage)) t)

(defmethod omG-change-to-little-icon ((dragged icon-finder) target position)
   "Change of container with presentation in name or type mode."
   (declare (ignore position))
   (if (find-if #'(lambda (item) (string-equal (name item) (name dragged))) (get-elements (object target)))
     (om-beep-msg (string+ "The name " (name dragged) " already exists !"))
     (if (EditorFrame (object target))
       (omG-change-container dragged (EditorFrame (object target)) (om-make-point 20 20))
       (let ((oldcontainer (om-view-container dragged))
             (real-oldcontainer (get-real-container (object dragged)))
             (newitem (make-icon-finder (object dragged) (bx dragged) (by dragged) (fil dragged) (col dragged))))
         (delete-icon-finder dragged oldcontainer)
         (if (print (or (big-icon-p (editor target))
                 (not (open? (triangle target)))
                 (not (show-in-packtree (object dragged) (object target)))))
           (omNG-change-container (object newitem) real-oldcontainer (object target))
           (let* ((frames (get-subicons-finder target (om-view-container target))))
             (setf (fil newitem) (+ 1 (fil target)))
             (if frames
               (setf (col newitem) (findcol (om-view-container target) frames newitem))
               (setf (col newitem) (+ 1 (col target))))
             (add-icon-finder newitem (om-view-container target))
             (omNG-change-container (object newitem) real-oldcontainer (object target)) 
             ))))))

(defmethod editor ((self icon-finder))
   (editor (panel self)))

(defmethod panel ((self icon-finder))
   (om-view-container self))
  

(defmethod omG-rename ((self icon-finder) new-name)
   "Rename 'self' with 'new-name'."
   (let ((icnx (+ (x self) (round (w self) 2))))
     ;;; icnx must stay at the same pos
     (om-set-dialog-item-text (nameView self) new-name)
     (om-set-view-size (nameView self) (om-make-point (get-name-size new-name (om-get-font (nameview self))) 16))
     (om-add-subviews self (nameView self))
     (set-size self (om-view-container self))
     (om-set-view-position (nameView self) (get-position-name (nameView self)))
     (setf (name self) new-name)  
     (when (big-icon-p (editor (om-view-container self)))
       (om-set-view-position self (om-make-point (- icnx (round (w self) 2)) (y self))))
     (omG-rename (object self) new-name)))


(defmethod omG-change-icon ((self icon-finder) new-icon)
   "Set the icon ID of 'self' to 'new-icon'."
   (setf (iconID (iconview self)) new-icon)
   (om-invalidate-view self t))

(defmethod get-object-insp-name ((self icon-finder)) 
   (get-object-insp-name (object self)))



;==============================================
;ICONVIEW
;==============================================

(defclass icon-finder-icon (om-view-drag icon-view om-view-drop) ())

(defmethod icon-finder ((self icon-finder-icon)) (om-view-container self))

(defmethod view-frame ((self icon-finder-icon)) 
  (icon-finder self))


(defmethod get-position-icon ((self icon-finder-icon))
   (let* ((icon-finder (icon-finder self))
          (sizename (get-length-name (nameView icon-finder))))
     (if (big-icon-p (editor (om-view-container icon-finder)))
       (om-make-point (if (> sizename *size-big-icon-finder*) (round (-  sizename *size-big-icon-finder*) 2)
                       0) 0)
       (om-make-point 0 0))))

(defmethod om-view-click-handler ((self icon-finder-icon) where)
  (toggle-icon-active-mode (icon-finder self)))

(defmethod om-view-doubleclick-handler ((self icon-finder-icon) where)
  (open-icon-win (icon-finder self) where))


;===========NAMEVIEW===========
(defclass icon-finder-name-edit (om-editable-text)  
  ((icon-finder :initform nil :initarg :icon-finder :accessor icon-finder)))


(defclass icon-finder-name (om-static-text-drag) ())

(defmethod receiver-view-p ((self icon-finder-icon)) t)
(defmethod receiver-view-p ((self icon-finder-name)) t)
(defmethod receiver-view-p ((self t)) nil)
  
(defmethod icon-finder ((self icon-finder-name))
   (om-view-container self))

(defmethod view-frame ((self icon-finder-name)) 
  (icon-finder self))


(defmethod get-length-name ((self icon-finder-name))
  (get-name-size (om-dialog-item-text self) (om-get-font self)))


(defmethod get-position-name ((self icon-finder-name))
   (let* ((sizename (get-length-name self)))
     (if (big-icon-p (editor (om-view-container (om-view-container self))))
       (om-make-point (if (>= sizename *size-big-icon-finder*) 0
                       (round (- *size-big-icon-finder* sizename) 2)) *size-big-icon-finder*)
       (om-make-point (+ 5 *size-little-icon-finder*) 0))))
         
   
(defmethod om-view-click-handler ((self icon-finder-name) where)
   (declare (ignore where))
   (toggle-icon-active-mode (icon-finder self))
   (let* ((panel (om-view-container (icon-finder self)))
          (container (editor panel)))
     (when (text-view container)
         (exit-from-dialog (text-view container) (om-dialog-item-text (text-view container)))))
   t)

;;;text-view on the panel
(defmethod om-view-doubleclick-handler ((self icon-finder-name) where)
   (declare (ignore where))
   (if (change-name-p (icon-finder self))
    (let* ((panel (om-view-container (icon-finder self)))
           (container (editor panel)))
      (when (text-view container)
          (exit-from-dialog (text-view container) (om-dialog-item-text (text-view container))))
      (setf (text-view container)
        (om-make-dialog-item 'icon-finder-name-edit
                             (om-add-points (om-view-position (icon-finder self)) (om-view-position self))
                             (om-make-point (get-name-size (om-dialog-item-text self) (om-get-font self)) 20)
                             (om-dialog-item-text self)
                             :font *om-default-font1*
                             :icon-finder (icon-finder self)
                             :allow-returns nil
                             :focus t
                             :container panel
                             )
        )
      (om-remove-subviews (icon-finder self) self)
      )))



(defmethod om-view-key-handler ((self icon-finder-name-edit) char)
  (let ((size-name (get-name-size (om-dialog-item-text self) (om-get-font self))))
    (om-set-view-size self (om-make-point (om-point-h (om-make-point size-name 10)) (om-height self)))
   (if (big-icon-p (editor (om-view-container self)))
       (om-set-view-position self 
                             (om-make-point
                              (- (+ (x (icon-finder self)) 
                                   (round (w (icon-finder self)) 2))
                                 (round (om-point-h (om-make-point size-name 10)) 2))
                              (om-point-v (om-view-position self))))
     t)))


(defmethod om-dialog-item-action ((self icon-finder-name-edit))
  (exit-from-dialog self (om-dialog-item-text self)))

(defvar *forbidden-chars* '(#\/ #\\ #\> #\< #\| #\? #\: #\* #\"))
(defun forbidden-name (str)
  (loop for c in *forbidden-chars* do
        (when (find c str) (return t))))

;;;text-view on the pajnel
(defmethod exit-from-dialog  ((self icon-finder-name-edit) new-text-item)
   (let ((oldname (name (object (icon-finder self))))
         (panel (om-view-container (icon-finder self))))
     (cond 
      ((or (equal new-text-item "")
           (string-equal new-text-item (name (object (icon-finder self)))))
       (om-add-subviews (icon-finder self) (nameView (icon-finder self))))
      ((match-name panel new-text-item)
       (dialog-message (string+ "The name " new-text-item " is already used. Please choose another one"))
       (om-add-subviews (icon-finder self) (nameView (icon-finder self))))
      ((forbidden-name new-text-item)
       (dialog-message (string+ "The name " new-text-item " contains frobidden characters (< > / ; ...). Please choose another one."))
       (om-add-subviews (icon-finder self) (nameView (icon-finder self))))
      (t
       (unless (omG-rename (icon-finder self) new-text-item)
           (setf (name (object (icon-finder self))) oldname 
                 (name (icon-finder self)) oldname)
           (om-add-subviews (icon-finder self) (nameView (icon-finder self)))
           )))
     (setf (text-view (editor panel)) nil)
     (om-remove-subviews panel self)
     (om-invalidate-view (icon-finder self))
     (mapc #'(lambda (control) 
               (omG-unselect control)) (get-actives panel))
     ;(om-invalidate-view (om-view-container (icon-finder self))) 
     ))

;;;text-view on the panel
(defmethod exit-from-dialog  ((self t) new-text-item)
  (declare (ignore new-text-item))
  (setf (text-view (editor (om-view-container self))) nil)
  (om-remove-subviews (panel (editor (om-view-container self))) self))


  
;= FOR LITTLE ICONS =

(omg-defclass triangle-icon (icon-view)  
  ((open? :initform nil :accessor open?)
   (icon-finder :initform nil :initarg :icon-finder :accessor icon-finder)))


(defmethod om-view-click-handler ((item triangle-icon) where)
   (declare (ignore where))
   (open/close-triangle item (not (open? item))))


(defmethod open/close-triangle ((item triangle-icon) option)
  (unless (equal option (open? item))
    (om-with-cursor *om-wait-cursor*
    (let* ((icon (icon-finder item))
           (x1 (fil icon))
           (y1 (col icon))
           (by (by icon))
           (bx (bx icon))
           (container (om-view-container item))
             (object (object icon)))
      (setf (iconID item) (if (open? item) 164 165))
      ;;; new test : pour charger l'icon sur mac..
      (get&corrige-icon (iconID item))
      (setf (open? item) (not (open? item)))
      (if (open? item)
        (let ((i 0) rep)
          (when (EditorFrame (object icon))
            (om-close-window (window (EditorFrame (object icon))))) 
          (mapc #'(lambda (elem)
                            (pushr (make-icon-from-object  elem bx by 1 1) rep))
              (get-browser-elements object))
          (om-with-delayed-redraw container
          (mapc #'(lambda (elem)
                            (incf i)
                            (setf (fil elem) (+ 1 x1))
                            (setf (col elem) (+ i y1))
                            (add-icon-finder elem container)
			    ) (sort-subframes container rep))
          )
          )
        (delete-sub-icons icon container))
      (om-invalidate-view item)
      ;;; new for update scrollers...
      (set-field-size container)
      (om-invalidate-view container)
      t))))


;-----------------------------------------------------

;Methods to redefine

(defmethod get-class-icon-icon ((self t)) 'icon-finder-icon)
(defmethod get-class-icon-name ((self t)) 'icon-finder-name)
(defmethod get-name-icon ((self t)) "vide")
(defmethod get-view-font-icon ((self t)) *om-default-font1*)
(defmethod open-icon-win ((self t) where) (declare (ignore where)) nil)
(defmethod make-extra-info ((self icon-finder) (obj t)))
(defmethod add-extras-infos ((self icon-finder)) nil)
(defmethod add-extra-subviews ((self t)) nil)

;==========
;for D&D
(defmethod make-drag-region ((self icon-finder) region x0 y0 view)
  (let* ((reg1 (om-new-region))
         (reg2 (om-new-region))
         (name (nameView self))
         (icon (iconView self))
         (x (- (x self) x0 (x view)))
         (y (- (y self) y0 (y view))))
    (om-set-rect-region reg1 (+ x (x icon))  (+ y  (y icon)) (+ x (x+w icon)) (+ y  (y+h icon)))
    (om-set-rect-region reg2  (+ x (x name))  (+ y  (y name)) (+ x (x+w name)) (+ y  (y+h name)))
    (setf region (om-union-region reg1 reg2))
    (om-dispose-region reg1)
    (om-dispose-region reg2))
   region)


;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view icon-finder)) nil)
;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view icon-finder)) nil)

(defmethod get-drag-object ((self icon-finder-icon)) (icon-finder self))
(defmethod get-drag-object ((self icon-finder-name)) (icon-finder self))
(defmethod get-drag-object ((self icon-finder)) self)

(defmethod om-drag-selection-p ((self icon-finder-icon) mouse-position)
  (om-drag-selection-p (get-drag-object self) mouse-position))

(defmethod om-drag-selection-p ((self icon-finder-name) mouse-position)
  (om-drag-selection-p (get-drag-object self) mouse-position))

(defmethod om-drag-selection-p ((self icon-finder) mouse-position)
  (declare (ignore mouse-position)) (not (om-control-key-p)))

(defmethod get-pos-in-object ((self icon-finder-name) where)
  (om-add-points (om-view-position self) where))

(defmethod get-pos-in-object ((self icon-finder-icon) where)
  (om-add-points (om-view-position self) where))

(defmethod get-pos-in-object ((self icon-finder) where)
  (om-add-points (om-view-position self) where))

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view icon-finder-name))
   (om-highlight-view (icon-finder view) t))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view icon-finder-name)) 
  (om-highlight-view (icon-finder view) nil))

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view icon-finder-icon))
  (om-highlight-view (icon-finder view) t))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view icon-finder-icon)) 
  (om-highlight-view (icon-finder view) nil))


;============================
;============================
;;; WS ICON FINDER CLASSES
;============================
;============================
;INstances
;============================
(omg-defclass instance-icon-frame (icon-finder) ()
   (:documentation "The class of simple frames for OMInstance meta objects.#enddoc#
#seealso# (OMInstance) #seealso#"))

(defmethod initialize-instance :after ((self instance-icon-frame) &key controls)
   (declare (ignore controls))
   (setf (change-name-p self) t))

(defmethod OMGMoveObject :after ((self instance-icon-frame) new-position)
  (when (big-icon-p (editor (om-view-container  self)))
    (set-icon-pos (object self) (borne-position new-position))))


;============================
;Maquette
;============================
(omg-defclass maquette-icon-frame (icon-finder) ()
   (:documentation "The class of simple frames for OMMaquette meta objects.#enddoc#
#seealso# (OMMaquette) #seealso#"))

(defmethod initialize-instance :after ((self  maquette-icon-frame) &key controls)
   (declare (ignore controls))
   (setf (change-name-p self) t))

(defmethod OMGMoveObject :after ((self  maquette-icon-frame) new-position)
  (when (big-icon-p (editor (om-view-container self)))
    (set-icon-pos (object self) (borne-position new-position))))

;(defmethod om-draw-contents :after ((self maquette-icon-frame))
;   (when (< (omversion (object self)) 5)
;    (om-with-focused-view (iconview self)
;      (om-with-fg-color (iconview self) *om-purple-color*
;        (om-fill-rect 0 0 (w (iconview self)) (h (iconview self)))))))

;(defmethod get-finder-iconID ((self ommaquette)) 126)

;============================
;PATCH
;============================
(omg-defclass patch-icon-frame (icon-finder) ()
   (:documentation "The class of simple frames for OMPatch meta objects.#enddoc#
#seealso# (OMPatch) #seealso#"))

(defmethod initialize-instance :after ((self patch-icon-frame) &key controls)
   (declare (ignore controls))
   (setf (change-name-p self) t))

(defmethod OMGMoveObject :after ((self patch-icon-frame) new-position)
   (when (big-icon-p (editor (om-view-container self)))
     (set-icon-pos (object self) (borne-position new-position))))

;(defmethod om-draw-contents :after ((self patch-icon-frame))
;  (when (< (omversion (object self)) 5)
;    (om-with-focused-view (iconview self)
;      (om-with-fg-color (iconview self) *om-purple-color*
;        (om-fill-rect 0 0 (w (iconview self)) (h (iconview self)))))))

(defmethod get-class-icon-icon ((self ompatch)) 'patch-finder-icon)
;(defmethod get-finder-iconID ((self ompatch)) 125)

(defclass patch-finder-icon (icon-finder-icon) ())

;(defmethod om-draw-contents :after ((self patch-finder-icon))
;  (when (lisp-exp-p (object (om-view-container self)))
;    (om-with-focused-view self
;      (om-with-fg-color self *om-white-color*
;        (om-with-font *om-default-font1*
;          (if (big-icon-p (editor (om-view-container self)))
;            (om-draw-string 8 20 "lisp")
;            (om-draw-string 1 12 "lisp")
;            ))))))

(defmethod om-draw-contents :before ((self patch-finder-icon))
  (when (< (omversion (object (om-view-container self))) 5)
    (om-with-focused-view self
      (om-with-fg-color self *om-red-color*
        (om-fill-rect 0 0 (w self) (h self))
        ))))


;============================
;FOLDER
;============================

(omg-defclass folder-icon-frame (icon-finder) ()
   (:documentation "The class of simple frames for OMFolder meta objects.#enddoc#
#seealso# (OMFolder) #seealso#"))

(defmethod initialize-instance :after ((self folder-icon-frame) &key controls)
   (declare (ignore controls))
   (setf (container-p self) t)
   (setf (change-name-p self) t))


(defmethod OMGMoveObject :after ((self folder-icon-frame) new-position)
   (when (big-icon-p (editor (om-view-container  self)))
     (set-icon-pos (object self) (borne-position new-position))))


;============================
;TYPES
;============================
(omg-defclass type-icon-frame (icon-finder) ()
   (:documentation "The class of simple frames for OMBasicType meta objects.#enddoc#
#seealso# (OMBasicType) #seealso#"))

(defmethod show-big-doc ((self type-icon-frame)) nil)







