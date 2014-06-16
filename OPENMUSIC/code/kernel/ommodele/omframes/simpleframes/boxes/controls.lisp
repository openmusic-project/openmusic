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
;This file defines classes for dialog-item used to show and edit values.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


;==============
;   TTYBOX 
;==============
;used to show and edit values i. e. inputs default values, etc.

(defclass ttybox (om-static-text) ; (om-static-text-drag) 
   ((open-state :initform t  :initarg :open-state :accessor open-state)))

(defmethod om-view-drag-hilite-p ((self om-static-text-drag)) t)

(defmethod om-component-border ((self ttybox)) nil)  ;;; :line)

;------------EVENTS

  ;;;(defmethod om-view-click-handler ((self ttybox) where)
;;;   (declare (ignore where))
;;;   (cond
;;;    ((om-command-key-p) (om-set-help (not (help-on?))))
;;;    (t (toggle-icon-active-mode (om-view-container self)))))
(defmethod om-view-click-handler ((self ttybox) where)
   (declare (ignore where))
   (toggle-icon-active-mode (om-view-container self)))

;;;(defmethod om-view-doubleclick-handler ((self ttybox) where)
;;;   (declare (ignore where))
;;;   (cond
;;;    ((om-command-key-p) (om-set-help (not (help-on?))))
;;;    (t (open-ttybox self))
;;;    ))
(defmethod om-view-doubleclick-handler ((self ttybox) where)
   (declare (ignore where))
   (open-ttybox self))
    

(defmethod set-value ((self ttybox) value)
   (om-set-dialog-item-text self (format () "~S" value))
   (setf (value (object (om-view-container self))) value))


;------------EDITION
(defmethod open-ttybox-class ((self ttybox)) 'change-text-enter-view)

(defmethod initial-text-ttybox ((self ttybox))
   (cond
    ((equal (class-name (class-of (object (om-view-container self)))) 'OMBoxTypeCall)
     (thestring (object (om-view-container self))))
    (t (format () "~S" (value (object (om-view-container self)))))))

(defmethod text-enter-multiline-p ((self t)) t)

;;;(text-view container) sur le panel
(defmethod open-ttybox ((self ttybox))
  (let* ((thetext (initial-text-ttybox self))
	 (panel (om-view-container (om-view-container self)))
	 (container (editor panel)))
    (when (text-view container)
      (om-remove-subviews panel (text-view container))
      (setf (text-view container) nil))
    (setf (text-view container) 
	  (om-make-dialog-item (open-ttybox-class self)
			       (om-add-points (om-subtract-points 
					       (om-view-position self)
					       #-win32 (om-make-point 2 2) #+win32 (om-make-point 0 0)) 
					      (om-view-position (om-view-container self)))
			       (om-view-size self)
			       thetext
			       :allow-returns (text-enter-multiline-p self)
			       :focus t
			       :object self
			       :container panel
			       :font *om-default-font1*))
    ))


(defclass change-text-enter-view (edit-text-enter-view) ())


(defmethod om-dialog-item-action ((self change-text-enter-view))
 (exit-from-dialog self (om-dialog-item-text self)))
     

;;;remove text-view from panel
(defmethod exit-from-dialog ((self change-text-enter-view) newtext)
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (setf (text-view (editor (om-view-container self))) nil)
                            (om-remove-subviews (panel (editor (om-view-container self))) self)
                            (om-beep)
                            (om-abort))))
     (let ((*package* (find-package :om))
           (newval (read-from-string newtext))
           (newsize (good-text-box-size newtext *om-default-font1*))
           (box (object (om-view-container (object self)))))
       (setf *provisoire-flag* nil)
       (om-set-dialog-item-text (object self) newtext)
       (setf (value box) newval)
       (setf (thestring box) newtext)
       (reinit-size (om-view-container (object self)))
       (setf (text-view (editor (om-view-container self))) nil)
       (om-remove-subviews (panel (editor (om-view-container self))) self))))

;----------D&D
(defmethod get-drag-object ((self ttybox)) (om-view-container self))

(defmethod get-pos-in-object ((self ttybox) where)
  (om-add-points (om-view-position self) where))


;================
;  UNDEF-TTYBOX
;================
;a text box for an undefined function

(defclass undef-ttybox (ttybox om-static-text-drag) ())

;select
(defmethod (setf selected-p) (selected-p (self undef-ttybox))
   (if selected-p
     (progn
       (om-set-bg-color self *undefbox-color*)
       (om-set-fg-color self *om-white-color*))
     (progn
       (om-set-fg-color self *om-black-color*)
        (om-set-bg-color self *undefbox-color*)))
   ;;;(om-invalidate-view self)
   )


;-----------EDITION

(defclass new-fun-enter-view (edit-text-enter-view) ())   

(defmethod open-ttybox-class ((self undef-ttybox)) 'new-fun-enter-view)
(defmethod initial-text-ttybox ((self undef-ttybox)) "??")
(defmethod text-enter-multiline-p ((self undef-ttybox)) nil)

;;;self on panel
(defvar *spec-new-boxes-types* nil)
(defmethod get-new-box-from-type ((type t) position container) nil)

(defun decode-input-arguments (text)
  (let ((args nil)
        (texte text))
    (multiple-value-bind (symb pos) (read-from-string texte)
      (setf texte (subseq texte pos))
      (loop while (> (length texte) 0) do
            (setf temp (multiple-value-list (read-from-string texte pos)))
            (setf pos (cadr temp))
            (push (car temp) args)
            (setf texte (subseq texte pos))))
    (reverse args)))

; (decode-input-arguments "om+ 5 67")
 

(defmethod exit-from-dialog ((self new-fun-enter-view) str)
  (handler-bind ((error #'(lambda (c) ;(declare (ignore c)) 
                            (when (om-view-container self)
			      (setf (text-view (editor (om-view-container self))) nil)
			      (om-remove-subviews (panel (editor (om-view-container self))) self))
                            (om-beep)
                            (print c)
                            (om-abort))))
    (let* ((box (om-view-container (object self)))
           (pos (om-view-position box))
           (scroller (om-view-container box))
           (theeditor (editor (om-view-container box))))
      (add-box-in-patch-panel str scroller pos)
      (omG-remove-element scroller box)
      (setf (text-view theeditor) nil)
      (om-remove-subviews (panel theeditor) self)
      (om-invalidate-view (panel theeditor)))))


      
(defun add-box-in-patch-panel (str scroller pos)      
  (let ((*package* (find-package :om))
        (funname (read-from-string str))
        (args (decode-input-arguments str))
        (text (cadr (multiple-value-list (string-until-char str " "))))   
        newbox)
    (cond
       ((or (listp funname) (numberp funname) (stringp funname))
        (setf newbox (omNG-make-new-boxcall (get-basic-type 'list) pos (mk-unique-name scroller "list")))
        (setf (value newbox) funname)
        (setf (thestring newbox) str)
        (setf (frame-size newbox) nil)) ; (get-good-size newtext *om-default-font2*)))
       ((special-form-p funname)
        (om-beep-msg  (string+ "Special Lisp form " str)))
       
       ((equal funname 'patch)
        (setf newbox (omNG-make-new-boxcall 
                      (make-instance 'OMPatchAbs 
                        :name (mk-unique-name scroller "mypatch") :icon 210) 
                      pos 
                      (mk-unique-name scroller "mypatch"))))
       ((equal funname 'lisp)
        (setf newbox (omNG-make-new-boxcall 
                      (make-instance 'OMLispPatchAbs 
                        :name (mk-unique-name scroller "lispfunction")
                        :icon 123)
                      pos 
                      (mk-unique-name scroller "lispfunction"))))
       ((member funname *spec-new-boxes-types*)
        (setf newbox (get-new-box-from-type funname pos scroller)))
       ((equal funname 'maquette)
        (setf newbox (omNG-make-new-boxcall 
                      (make-instance 'OMMaqAbs 
                                     :name (mk-unique-name scroller "mymaquette") :icon 265) 
                      pos 
                      (mk-unique-name scroller "mymaquette"))))
       ((equal funname 'comment)
        (setf newbox (omNG-make-new-boxcall funname pos "comment"))
        (setf (reference newbox) (or text "Type your comments here")))
       ((and (find-class funname nil) (not (equal funname 'list)) (omclass-p (class-of (find-class funname nil))))
        (cond ((om-shift-key-p)
               (setf newbox  (omNG-make-new-boxcall-slots (find-class funname nil) pos (mk-unique-name scroller "slots"))))
              (t (let ((boxname (or text (mk-unique-name scroller (string funname)))))
                   (setf newbox (omNG-make-new-boxcall (find-class funname) pos boxname))
                   (if text (setf (show-name newbox) t))))
              ))
       ((not (fboundp funname))
        (if (equal funname '??)
            (om-beep)
          (om-beep-msg  (string+ "function " str " does not exist!"))))
       ((OMGenfun-p (fdefinition funname))
	(setf newbox (omNG-make-new-boxcall (fdefinition funname) pos 
                                            (mk-unique-name scroller (string funname))))
        (when args (add-args-to-box newbox args))
        )
       
       (t (setf newbox (omNG-make-new-lispboxcall funname pos 
                                                  (mk-unique-name scroller (string funname))))
          (when args (add-args-to-box newbox args))))
      (when (and newbox (box-allowed-p newbox scroller))
        (when (and (allow-rename newbox) (car args))
          (set-patch-box-name newbox text))
        (omG-add-element scroller (make-frame-from-callobj newbox)))
      ))

(defmethod add-args-to-box (box args)
  (let ((main-args (first-n args (length (inputs box))))
        (other-args (nthcdr (length (inputs box)) args)))
    (mapcar #'(lambda (input val) (setf (value input) val)) (inputs box) main-args)
    (loop while other-args do
      (let ((arg (pop other-args)))
        (cond ((do-add-one-input box)
               (setf (value (last-elem (inputs box))) arg))
              ((and (symbolp arg) (string-equal "KEYWORD" (package-name (symbol-package arg))))
               (if (do-add-one-keyword box arg)
                   (setf (def-value (last-elem (inputs box))) (pop other-args))
                 (pop other-args)))
              (t (om-beep)))
        ))))



(defmethod box-allowed-p (box panel) t)

;(mapc #'(lambda (a b) (+ a b)) '(1 1 1) '(2 2))

;==================
; INITFORM-TTYBOX
;==================
;Initform for slots


(defclass initform-ttybox (ttybox om-static-text-drag OMAtomicFrame) ())

;------------Events
;;;(defmethod om-view-click-handler ((self initform-ttybox) where)
;;;   (declare (ignore where))
;;;   (cond
;;;    ((om-command-key-p) (om-set-help (not (help-on?))))
;;;    ))
(defmethod om-view-click-handler ((self initform-ttybox) where)
   (declare (ignore where)))


;;;(defmethod om-view-doubleclick-handler ((self initform-ttybox) where)
;;;   (declare (ignore where))
;;;   (cond
;;;    ((om-command-key-p) (om-set-help (not (help-on?))))
;;;    (t (open-ttybox self))))
(defmethod om-view-doubleclick-handler ((self initform-ttybox) where)
   (declare (ignore where))
   (open-ttybox self))

(defmethod (setf selected-p) (selected-p (self initform-ttybox))
  (declare (ignore selected-p)) nil)


;------------Edition
;; new : text-view container = panel
(defmethod open-ttybox ((self initform-ttybox))
  (let ((container (editor (om-view-container self))))
     (if (protected-p (object container))
       (om-beep-msg "protected object")
       (progn
         (when (text-view container)
           (om-remove-subviews (panel container) (text-view container))
           (setf (text-view container) nil))
          (setf (text-view container) (om-make-dialog-item 'new-slot-initform-view
                                                           ;(pos-panel2editor (om-view-container self) 
                                                            (om-view-position self)
                                                            (om-view-size self)
                                                           (om-dialog-item-text self)
                                                           :allow-returns nil
                                                           :object self
                                                           :container (panel container)
                                                           :font *om-default-font1*))))))

(defclass new-slot-initform-view (edit-text-enter-view) ())

;;;new : panel au lieu de editor
(defmethod exit-from-dialog ((self new-slot-initform-view) newtext)
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (when (om-view-container self)
			      (setf (text-view (editor (om-view-container self))) nil)
			      (om-remove-subviews (panel (editor (om-view-container self))) self))
                            (om-beep)
                            (om-abort))))
     (let ((*package* (find-package :om))
           (slot (object (object (object self)))))
       (if (check-type-p (thetype slot) (read-from-string newtext))
         (let ((newval (read-from-string newtext)))
           (omG-change-initform slot newval)
           (om-set-dialog-item-text (object self) newtext)
           (om-set-view-size (object self) (om-make-point (min (get-name-size newtext) 500) (h (object self)))))
         (om-beep-msg "type mismatch"))
       (setf (text-view (editor (om-view-container self))) nil)
       (om-remove-subviews (panel (editor (om-view-container self))) self))))

;------------D&D
(defmethod om-drag-selection-p ((self initform-ttybox) mouse-position)
  (declare (ignore mouse-position))
  nil)

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((self initform-ttybox))
  (om-set-bg-color self *om-black-color*)
  (om-set-fg-color self *om-white-color*)
  (om-draw-contents self))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((self initform-ttybox)) 
  (om-set-fg-color self *om-black-color*)
  (om-set-bg-color self *om-white-color*)
  (om-draw-contents self))


;==============
; NUMBOX
;==============
;; Box for integers

(defclass numbox (ttybox)
  ((value   :initform 0     :initarg :value   :accessor value)
   (enable   :initform t     :initarg :enable   :accessor enable)
   (min-val :initform 0     :initarg :min-val :accessor min-val)
   (max-val :initform 30000 :initarg :max-val :accessor max-val)
   (afterfun :initform nil :initarg :afterfun :accessor afterfun)))


(defmethod om-component-border ((self numbox)) *om-gray-color*)

(defmethod enable-numbox ((self numbox) t-or-nil)
  (setf (enable self) t-or-nil)
  (om-set-fg-color self (if (enable self) *om-black-color* *om-gray-color*)))

;=========================
;events

(defvar *numbox-last-click* nil)
(defmethod om-view-click-handler  ((self numbox) where)
  (when (enable self)
    (setf *numbox-last-click* (om-make-point (om-point-h where) (* -1 (om-point-v where))))
    (om-init-motion-functions self 'numbox-motion 'numbox-release)))


(defmethod numbox-motion ((self numbox) pos)
  (let* ((y (om-point-v pos))
         (x (om-point-h pos))
         (first-v (* -1 y)))
    (when (value self)
     (set-value self  (max (min-val self) (min (max-val self)
                                            (+ (value self)
                                               (* (map-mouse-increment self) (- first-v (om-point-v *numbox-last-click*)))))))
     (setf *numbox-last-click* (om-make-point x first-v))
     (om-redraw-view self)
     (item-action-while-drag self))))


(defmethod numbox-release ((view numbox) pos) 
  (item-action-after-drag view)
  (om-invalidate-view view))



;;; pour eviter l'heritage de ttybox...
(defmethod om-view-doubleclick-handler ((self numbox) where)
   nil)

(defmethod map-mouse-increment ((view numbox))
  (cond ((om-shift-key-p) 10) 
        ((om-command-key-p) 100)
        (t 1)))  

(defmethod item-action-after-drag ((self numbox))
  (when (afterfun self)
    (funcall (afterfun self) self)))


(defmethod item-action-while-drag ((self numbox))
   (when (om-dialog-item-action-function self)
       (funcall 'om-dialog-item-action self)
    ))



(defmethod om-drag-selection-p ((self numbox) mouse-position)
  (declare (ignore mouse-position)) nil)

(defmethod set-value ((self numbox) value)
   (om-set-dialog-item-text self (format () " ~S" value))
   (setf (value self) value)
   (om-invalidate-view self t))



;;; NUMBOX STYLE POTENTIOMETRE ======================================================

(defclass graphic-numbox (om-item-view)
  ((pict :accessor pict :initarg :pict :initform nil)
   (pict-size :accessor pict-size :initarg :pict-size :initform (om-make-point 5 5))
   (nbpict :accessor nbpict :initarg :nbpict :initform 0)
   (pict-part :accessor pict-part :initarg :pict-part :initform 0)
   (value   :initform 0     :initarg :value   :accessor value)
   (min-val :initform 0     :initarg :min-val :accessor min-val)
   (max-val :initform 30000 :initarg :max-val :accessor max-val)
   (di-action :initform nil :initarg :di-action :accessor di-action)
   (afterfun :initform nil :initarg :afterfun :accessor afterfun)))

(defmethod initialize-instance ((self graphic-numbox) &rest initargs)
  (call-next-method)
  (setf (pict-part self) (round (* (- (nbpict self) 1) (/ (- (value self) (min-val self)) (- (max-val self) (min-val self)))))))



(defmethod om-draw-contents ((self graphic-numbox))
  (when (pict self)
    (om-draw-picture self (pict self) 
                          :size (om-view-size self)
                          :srctopleft (om-make-point 0 (* (pict-part self) (om-point-v (pict-size self))))
                          :srcsize (pict-size self) 
                          )))


(defvar *gnumbox-last-click* nil)
(defmethod om-view-click-handler  ((self graphic-numbox) where)
  (setf *gnumbox-last-click* (om-make-point (om-point-h where) (* -1 (om-point-v where))))
  (om-init-motion-functions self 'g-numbox-motion 'g-numbox-release))


(defmethod g-numbox-motion ((self graphic-numbox) pos)
  (let* ((y (om-point-v pos))
         (x (om-point-h pos))
         (first-v (* -1 y)))
    (set-value self  (max (min-val self) (min (max-val self)
                                              (+ (value self)
                                                 (* (map-mouse-increment self) (- first-v (om-point-v *gnumbox-last-click*)))))))
    (setf *gnumbox-last-click* (om-make-point x first-v))
    (om-redraw-view self)
    (item-action-while-drag self)))


(defmethod g-numbox-release ((view graphic-numbox) pos) 
  (item-action-after-drag view)
  (om-invalidate-view view))




;;; pour eviter l'heritage de ttybox...
(defmethod om-view-doubleclick-handler ((self graphic-numbox) where)
   nil)

(defmethod map-mouse-increment ((view graphic-numbox))
  (cond ((om-command-key-p) 100) 
        ((om-shift-key-p) 10) 
        (t 1))) 

(defmethod item-action-after-drag ((self graphic-numbox))
  (when (afterfun self)
    (funcall (afterfun self) self)))

(defmethod item-action-while-drag ((self graphic-numbox))
   (when (di-action self)
       (funcall (di-action self) self)
       ))


(defmethod set-value ((self graphic-numbox) value)
  (setf (pict-part self) (floor (* (- (nbpict self) 1) (/ (- value (min-val self)) (- (max-val self) (min-val self))))))
  (setf (value self) value)
  (om-invalidate-view self t))


;;; NUMBOX + EDIT ======================================================

(defclass edit-numbox (numbox)  
  ((edit :accessor edit :initarg :edit :initform nil)))
(defclass edit-numbox-edit (edit-text-enter-view)  ())
(defmethod open-ttybox-class ((self edit-numbox)) 'edit-numbox-edit)

(defmethod om-view-doubleclick-handler ((self edit-numbox) where)
  (when (enable self)
    (open-ttybox self)))

(defmethod open-ttybox ((self edit-numbox))
   (let ((thetext (format nil "~S" (value self)))
         (container (om-view-container self)))
     (om-add-subviews container
                      (setf (edit self) (om-make-dialog-item (open-ttybox-class self)
                                           (om-view-position self)
                                           (om-view-size self)
                                           thetext
                                           :allow-returns nil
                                           :focus t
                                           :object self
                                           :container (om-view-container self)
                                           :font *controls-font*))
                      )
     ))

(defmethod om-dialog-item-action ((self edit-numbox-edit))
  (exit-from-dialog self (om-dialog-item-text self)))
     

;;;remove text-view from panel
(defmethod exit-from-dialog ((self edit-numbox-edit) newtext)
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                             (om-remove-subviews (om-view-container self) self)
                             (om-beep)
                             (om-abort))))
     (let ((*package* (find-package :om))
           (newval (read-from-string newtext)))
       (set-value (object self) newval)
       (when (afterfun (object self))
         (funcall (afterfun (object self)) (object self)))
       (om-remove-subviews (om-view-container self) self)
       (setf (edit (object self)) nil))))

;;;======================================
;;; CHOOSE COLOR VIEW
(defclass om-color-view (om-view)
  ((color :accessor color :initarg :color :initform (om-make-color 0 0 0))
   (after-fun :accessor after-fun :initform nil :initarg :after-fun)))

(defmethod om-draw-contents ((self om-color-view))
  (om-with-focused-view self  
    (om-with-fg-color self (color self)
      (om-fill-rect 0 0 (om-width self) (om-height self)))))

(defmethod om-view-click-handler ((self om-color-view) pos)
  (declare (ignore pos))
  (let ((color (om-choose-color-dialog :color (color self))))
    (when color (setf (color self) color)
    ;(om-set-bg-color self color)
      (om-invalidate-view self)
      (when (after-fun self) (funcall (after-fun self) self)))))


