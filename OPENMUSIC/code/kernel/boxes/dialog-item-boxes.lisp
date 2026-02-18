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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================


(in-package :om)

;============================
;Dialog item boxes
;============================

;la class pour le dialog-item boxes
(defclass OMDIebox (OMBoxEditCall) ())

(defmethod omNG-box-value ((self OMDIebox) &optional (numout 0))
   "Eval a factory."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                        (om-report-condition c))
                                               :size (om-make-point 300 200))
                               (om-abort)))))

     (om-without-interrupts  
 
     (cond
        ((or (equal (allow-lock self) "l")  (equal (allow-lock self) "o") 
             (and (equal (allow-lock self) "x") (value self))
             (and (equal (allow-lock self) "&") (ev-once-p self)))
             (rep-editor (value self) numout))
        (t (let* ((args  (mapcar #'(lambda (input) (omNG-box-value input)) (inputs self)))
                  rep)
             (setf rep (set-dialog-item-params (value self) self args))
             (if (null rep)
                  (progn
                    (om-beep-msg (string+ "I can not construct a " (string (type-of (value self))) " with these parameters"))
                    (om-abort))
                (progn
                  (when (om-view-container (value self))
                           (update-for-subviews-changes (om-view-container (value self)) t))
                  (setf (value self) rep)
                  (rep-editor (value self) numout)
                  )))))
     )))

(defmethod gen-code-call ((self OMDIebox) &optional args)
   `(apply 'set-dialog-item-params (list ,(value self) ,self (list ,.(decode self)))))


(defmethod make-frame-from-callobj ((self OMDIebox))
   "Make a simple frame for the editor factory 'self'."
   (let ((name (string-downcase (name self)))
         (defsize (get-boxsize self))
         (numouts (numouts self))
         (numins (length (inputs self)))
         (index 0) 
         (module (om-make-view (get-frame-class self)
                               :position (frame-position self)
                               :object self)))
    
     ;#-linux
     (unless (frame-size self) 
              (setf (frame-size self) (om-make-point 
                                       (apply #'max (list (om-point-h defsize) (* 8 numouts) (* 8 numins))) 
                                       (om-point-v defsize))))
     #|
     #+linux(setf (frame-size self) (om-make-point 
                                       (apply #'max (list (om-point-h defsize) (* 8 numouts) (* 8 numins))) 
                                       (+ (om-point-v defsize) 10)))
     |#
     (setf (inputframes module) (mapcar #'(lambda (input)
                                            
                                            (setf index (+ index 1))
                                            (om-make-view (get-input-class-frame self)
                                                            :object input
                                                            :help-spec (string+ "<" (string-downcase (name input))
                                                                                "> " (doc-string input))
                                                            :size (om-make-point 8 8)
                                                            :position (om-make-point 
                                                                       (- (* index (round (om-point-h (frame-size self)) (+ numins 1))) 4) 
                                                                       1)
                                                            ))
                                        (inputs self)))
     
     (loop for input-f in (inputframes module) do (om-add-subviews module input-f))
     
     (make-outputs-from-names self (value self) module)
     
     (setf (iconview module) (value self))                
     (om-add-subviews module (iconview module))

     (setf (frames self) (list module))
     (setf (name module) name)
     (add-box-resize module)
     
     (om-set-view-size module (frame-size self))
     (update-di-size (value self) module)
     
     (when (allow-lock self)
       (add-lock-button module (allow-lock self)))
     
     module))

(defmethod get-frame-class ((self OMDIebox)) 'DIEditorframe)


;=======================
;the frame
;=======================

(defclass DIEditorframe ( omboxframe OMSimpleFrame om-transparent-view om-view-drag ) ())

(defmethod dieditorframe-p ((self t)) nil)
(defmethod dieditorframe-p ((self DIEditorframe)) self)

(defmethod om-show-tooltip ((self input-funboxframe) &optional (remove nil) (short nil)) 
  "prevenrting tooltips for interface boxes"
  (unless (dieditorframe-p (om-view-container self))
  (call-next-method)))


(defmethod show-fun-code ((self DIEditorframe))
  (edit-definition (class-name (reference (object self)))))

(defmethod show-big-doc ((self DIEditorframe))
  (om-show-reference-doc (class-name (reference (object self)))))

(defmethod allow-new-size ((self DIEditorframe) new-pos) 
   (om-make-point (max 20 (om-point-h new-pos )) (max 20 (om-point-v new-pos))))

(defmethod add-lock-button ((self DIEditorframe) &optional (mode "x"))
   "Add a lock button, if the box referenced by 'self' allow it."
   (when (allow-lock-button (object self))
     (setf (lock-button self) (make-lock-button self mode))
     (om-set-view-position (lock-button self) (om-make-point 0 8))
     (om-add-subviews self (lock-button self))
     (om-invalidate-view self)
     (setf (allow-lock (object self)) mode)))

(defmethod remove-lock-button ((self DIEditorframe))
   "Do not set value to nil."
   (om-remove-subviews self (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil))

(defmethod centre-icon ((self DIEditorframe))
   (om-set-view-size 
    (iconview self) 
    (om-subtract-points (om-view-size self) (om-make-point 0 17))))

(defmethod make-drag-region ((self DIEditorframe) region x0 y0 view)
  (declare (ignore view))
  (let* ((x (- (x self) x0))
         (y (- (y self) y0)))
    (om-set-rect-region region x y  (+ x (w self)) (- (+ y (h self)) 16)))
   region)

(defmethod omG-select ((self DIEditorframe))
   (when (not (active-mode self))
     (setf (active-mode self) t)
     (om-invalidate-view self)
     ))

(defmethod omG-unselect ((self DIEditorframe))
   (when (active-mode self)
     (setf (active-mode self) nil)
     (om-invalidate-view self)
     ))


(defmethod om-draw-contents ((self dieditorframe))
  (call-next-method)
  (when (active-mode self)
    (om-with-focused-view self
      (om-with-fg-color self *om-gray-color*
      (om-draw-rect 1 8 (- (w self) 3) (- (h self) 17) :pensize 2)))))

(defmethod change-boxframe-size ((view DIEditorframe) new-size)
   (when (setf new-size (allow-new-size view new-size))
     (om-set-view-size view new-size)
     (make-move-after (om-view-container view) (list view))
     (update-di-size (value (object view)) view)
     (om-invalidate-view view)
     (om-invalidate-view (om-view-container view))
     #+linux(update-for-subviews-changes (om-view-container view) t)
     ))

(defmethod reinit-size ((self DIEditorframe)) 
   (setf (frame-size (object self)) (get-boxsize (object self)))
   (change-boxframe-size self (frame-size (object self)))
   (update-di-size (value (object self)) self)
   (om-invalidate-view self))

(defmethod allow-new-size ((self DIEditorframe) new-pos) 
   (om-make-point (max 30 (om-point-h new-pos )) (max 40 (om-point-v new-pos ))))

(defmethod add-subview-extra ((self DIEditorframe))
  (update-di-size (value (object self)) self)
  #+linux(update-for-subviews-changes (om-view-container self) t))

(defmethod om-view-doubleclick-handler ((self DIEditorframe) pos) nil)

;==================
; The object
;==================

(defclass! d-i-box (select-object) 
     ((di-data :accessor di-data :initform nil)))

(defmethod get-type-of-ed-box ((self d-i-box))  'OMDIebox)

(defmethod default-obj-box-size ((self d-i-box)) (om-make-point 130 44))

(defmethod get-slot-in-out-names ((self d-i-box))
   (values '("text") 
           '("untitled")
           '("dialog-item text (string)")
           '(nil)))

(defmethod set-dialog-item-params ((self d-i-box)  box args)
  (om-set-dialog-item-text self (format nil "~D" (car args)))
  self)

(defmethod update-di-size ((self d-i-box) container)
  (om-set-view-position self (om-make-point 10 18))
  (om-set-view-size self (om-make-point (- (om-width container) 20) (max 20 (- (om-height container) 36)))))

(defmethod omng-copy ((self d-i-box))
  (let ((newitem (eval (omng-save self))))
    (om-set-dialog-item-action-function newitem (om-dialog-item-action-function  self))
    newitem))

(defmethod spec-obj-icon-size ((self d-i-box)) '(nil nil))


;==================
;SPECIFIC BOXES
;==================

;==================
; SIMPLE TEXT
;==================

;editable static-text
(defclass! text-box (om-editable-text d-i-box)  ()
     (:icon 290)
     (:documentation "
The TEXT-BOX allows to display data or values as text in OM patches.
It can also be used to get user text inputs.

Use 'm' to show/hide the box interface.

This box contains a single line of text: no carriage return or newline characters allowed.

Connect something to the input to display it in the TEXT-BOX.
Evaluate or connect the output to get the current contents of the box.

[Warning: TEXT-BOX must be locked ('b') in order to prevent the contents to be overwriten by the box input data.]
"))
;; compat
(defclass! editable-text-box (text-box) ()) 



(defmethod get-super-default-value ((type (eql 'text-box)))
  (om-make-dialog-item 'text-box (om-make-point 8 8) (om-make-point 40 20) " " 
                       :font *om-default-font1* 
                       ))

(defmethod default-obj-box-size ((self text-box)) 
  #-linux(om-make-point 130 45)
  #+linux(om-make-point 130 60)
  )

(defmethod omng-save ((self text-box) &optional (values? nil))
  `(om-make-dialog-item 'text-box (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self)) ,(om-dialog-item-text self)
                        :font ,(om-save-font (om-get-font self))))
                       

(defmethod rep-editor ((self text-box) num)
  (let* ((rep nil)
        (noerror (ignore-errors (setf rep (read-from-string (om-dialog-item-text self))) t)))
    (if noerror rep (om-dialog-item-text self))))


 
(defmethod update-di-size ((self text-box) container)
  (om-set-view-position self 
                        #+win32(om-make-point 12 12) 
                        #+macosx(om-make-point 12 10)
                        #+linux(om-make-point 12 20)
                        )
  (om-set-view-size self (om-subtract-points (om-view-size container) 
                                             #+win32(om-make-point 28 24) 
                                             #+macosx(om-make-point 28 20)
                                             #+linux(om-make-point 28 40)
                                             )))


;==================
; MULTI-LIGNE TEXT
;==================
(defclass! text-view (om-text-edit-view d-i-box)  ()
    (:icon 291)
    (:documentation "
The TEXT-VIEW allows to display data or values as text in OM patches.
It can also be used to get user text inputs.

Use 'm' to show/hide the box interface.

This box can contains multiple lines of text.

Connect something to the input to display it in the TEXT-VIEW. If the input is a list, then each list item is displayed in a nxw line.
Evaluate or connect the output to get the current contents of the box.

[Warning: TEXT-VIEW must be locked ('b') in order to prevent the contents to be overwriten by the box input data.]
"
))

;; compat
(defclass! text-edit-view-box (text-view) ()) 

(defmethod omng-save ((self text-view) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'text-view 
                                   (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled")))
     (om-set-dialog-item-text rep ,(om-dialog-item-text self))
     rep))

(defmethod get-super-default-value ((type (eql 'text-view)))
  (om-make-dialog-item 'text-view (om-make-point 1 1) 
                       #-linux(om-make-point 50 20) 
                       #+linux(om-make-point 50 25) 
                       "untitled"))

(defmethod update-di-size ((self text-view) container)
  (om-set-view-position self 
                        #+win32(om-make-point 12 12) 
                        #+macosx(om-make-point 12 10)
                        #+linux(om-make-point 12 15)
                        )
  (om-set-view-size self (om-subtract-points (om-view-size container) 
                                             #+win32(om-make-point 28 24) 
                                             #+macosx(om-make-point 28 20)
                                             #+linux(om-make-point 28 30)
                                             )))


(defmethod rep-editor ((self text-view) num) 
   (om-dialog-item-text self))

(defun list-to-lines (strlist)
  (let ((bigstr ""))
    (loop for str in strlist do
          (setf bigstr (string+ bigstr (or (and str (format nil "~D" str)) "") (format nil "~%"))))
    bigstr))

(defmethod set-dialog-item-params ((self text-view) box args)
  (let ((str (if (consp (car args)) 
                 (list-to-lines (car args))
               (format nil "~D" (car args)))))
    (om-set-dialog-item-text self str)
    self))



;==================
;BUTTON
;==================

(defclass! button (om-button d-i-box) () 
   (:icon 292)
   (:documentation 
"The BUTTON box provides an interface to trigger evaluations in a patch.

Use 'm' to show/hide the box interface.

Connect a text to the first input and evaluate the box ('v') to display it in the BUTTON.
Pushing the button will automatically evaluate anything connected to the second input, or funcall it if it is a function.

"
))
 
;; compat
(defclass! button-box (button) ()) 

(defmethod get-super-default-value ((type (eql 'button)))
  (om-make-dialog-item 'button (om-make-point 0 4) (om-make-point 50 24) "untitled"))



(defmethod omng-save ((self button) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'button (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) ,(om-dialog-item-text self))))
     rep))

(defmethod rep-editor ((self button) num)
  (nth num
       (list (om-dialog-item-text self)
             (di-data self))))


(defmethod make-outputs-from-names (self (value button) module) (call-next-method))

(defmethod get-slot-in-out-names ((self button))
   (values '("text" "action") 
           '("click me" t)
           '("button text" "a function or box in lambda mode")
           '(nil nil)))


;;; by default button does not set a function but evaluates the patch
(defmethod set-action ((self button) fun box) 
  (om-set-dialog-item-action-function 
   self 
   #'(lambda (x) 
       (when fun
         (let ((panel (get-patchpanel (editor (om-view-window self)))))
           (om-eval-enqueue 
            `(progn
               (setf (di-data ,self) 
                     (if (functionp ,fun) 
                         (funcall ,fun) ;;; call it
                       (omng-box-value (second (inputs ,box))) ;; reevaluate
                       ))
               (setf *cur-eval-panel* ,panel)
               #+om-reactive(self-notify ,box nil)
               (clear-ev-once ,panel)
               (setf (di-data ,self) nil)
               )))))
   ))


(defmethod set-dialog-item-params ((self button) box args)
  (om-set-dialog-item-text self (format nil "~D" (car args)))
  (set-action self (cadr args) box)
  (om-invalidate-view (om-view-container self))
  self)


(defmethod (setf value) :after ((value button) (self omdiebox)) 
  (set-action value (omng-box-value (second (inputs self))) self))


(defmethod update-di-size ((self button) container)
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) #+mswindows 12 #-mswindows 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24))
  )


;==================
;CHECK-BOX
;==================

(defclass! check-box (om-check-box d-i-box)  ()
    (:icon 293)
    (:documentation 
"The CHECK-BOX box provides an interface to select/unselect something, e.g. to give a choice in a patch execution.

Use 'm' to show/hide the box interface.

Connect text to the input and evaluate the box ('v') to display it in the CHECK-BOX.
The box output will return T (true) if it is checked, and NIL (false) if not.

"
))
;; compat
(defclass! check-box-box (check-box) ()) 

(defmethod omng-save ((self check-box) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'check-box (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) ,(om-dialog-item-text self))))
     (om-set-check-box rep ,(om-checked-p self))
     rep))


(defmethod get-super-default-value ((type (eql 'check-box)))
  (om-make-dialog-item 'check-box (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled"))


(defmethod rep-editor ((self check-box) num)
   (om-checked-p self))

(defmethod update-di-size ((self check-box) container)
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 12)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))



;==================
; RADIO BUTTON
;==================

(defclass! radio-button (om-radio-button d-i-box)  ()
           (:icon 294))
;; compat
(defclass! radio-button-box (radio-button) ()) 

(defmethod update-di-size ((self radio-button) container)
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 12)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))

(defmethod get-super-default-value ((type (eql 'radio-button)))
  (om-make-dialog-item 'radio-button (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled"))

;single-item-list
(defclass! single-item-list (om-single-item-list d-i-box)  ()
  (:icon 295)
  (:documentation 
"The SINGLE-ITEM-LIST box provides an interface to give a choice from a list in a patch execution.

Use 'm' to show/hide the box interface.

Connect a list of choice to the input and evaluate the box ('v') to display them in the box.

The box output will return the selected item. One (and only one) item can be selected.

[Warning: SINGLE-ITEM-LIST should be locked ('b') in order to prevent the contents and selection to be overwriten by the box input data.]
"
))

(defun process-item-list (itemlist)
  (loop for item in itemlist collect (cond ((pathnamep item) (namestring item))
                                           ((stringp item) item)
                                           (t (format nil "~a" item))
                                           ;(t item)
                                           )))



;==================
; LIST (SINGLE SELECTION)
;==================

(defclass! single-item-list-box (single-item-list) ()) 

(defmethod get-slot-in-out-names ((self single-item-list))
   (values '("items") 
           '(("uno" "dos" "tres"))
           '("list of choices" )
           '(nil)))

(defmethod omng-save ((self single-item-list) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'single-item-list (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled"
                                   :range ',(om-get-item-list self))))
     (setf (di-data rep) ',(di-data self))
     (om-set-selected-item-index rep ,(om-get-selected-item-index self))
     rep))

(defmethod get-super-default-value ((type (eql 'single-item-list)))
  (om-make-dialog-item 'single-item-list (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled" :range '("uno" "dos" "tres")))

(defmethod update-di-size ((self single-item-list) container)
  (om-set-view-position self (om-make-point 12 8))
  (om-set-view-size self (om-subtract-points (om-view-size container) (om-make-point 24 16))))


(defmethod set-dialog-item-params ((self single-item-list) box args)
  (setf (di-data self) (car args))
  (om-set-item-list self (process-item-list (car args)))
  self)

(defmethod rep-editor ((self single-item-list) num)
  (let ((i (om-get-selected-item-index self)))
    (nth i (di-data self))))


;==================
; LIST (MULTI-SELECTION)
;==================

(defclass! multi-item-list (om-multi-item-list d-i-box)  ()
   (:icon 296)
   (:documentation 
"The MULTI-ITEM-LIST box provides an interface to give a (possibly multiple) choice from a list in a patch execution.

Use 'm' to show/hide the box interface.

Connect a list of choices to the input and evaluate the box ('v') to display them in the box.
The box output will return the list of selected items, or NIL if no item is selected.

[Warning: MULTI-ITEM-LIST should be locked ('b') in order to prevent the contents and selection to be overwriten by the box input data.]
"
))

;; compat
(defclass! multi-item-list-box (multi-item-list) ()) 

(defmethod omng-save ((self multi-item-list) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'multi-item-list (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled"
                                   :range ',(om-get-item-list self))))
     (om-set-selected-item-index rep ',(om-get-selected-item-index self))
     (setf (di-data rep) ',(di-data self))
     rep))

(defmethod get-slot-in-out-names ((self multi-item-list))
   (values '("items") 
           '(("uno" "dos" "tres" "cuatro"))
           '("list of choices" )
           '(nil)))

(defmethod get-super-default-value ((type (eql 'multi-item-list)))
  (om-make-dialog-item 'multi-item-list (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled" :range '("uno" "dos" "tres" "cuatro")))

(defmethod update-di-size ((self multi-item-list) container)
  (om-set-view-position self (om-make-point 12 8))
  (om-set-view-size self (om-subtract-points (om-view-size container) (om-make-point 24 16))))

(defmethod set-dialog-item-params ((self multi-item-list) box args)
  (setf (di-data self) (car args))
  (om-set-item-list self (process-item-list (car args)))
  self)

(defmethod rep-editor ((self multi-item-list) num) 
  (let ((indices (om-get-selected-item-index self)))
    (loop for i in indices collect
          (nth i (di-data self))
          )))
     


;==================
; POP-UP CHOICE MENU
;==================

(defclass! pop-up-menu (om-pop-up-dialog-item d-i-box)  ()
   (:icon 297)
   (:documentation 
"The POP-UP-MENU box provides an interface to select an item from a list, and possibly trigger an evaluation with it.

Use 'm' to show/hide the box interface.

Connect a list of choices to the input and evaluate the box ('v') to display them in the box.
- The 1st box output will return the selected item index.
- The 2nd box output will return the selected item text.

Note : it is possible to connect a directory pathname to the first input, in which case the list of files in this directory will be displayed in the menu.


Connect a lambda function or patch to the second input. This function must accept one single argument (supposed to be an item index)
Any selection in the menu will automatically call this function or patch passing it the selected item index as argument.

[Warning: POP-UP-MENU should be locked ('b') in order to prevent the contents and selection to be overwriten by the box input data.]
"
))

;; compat
(defclass! pop-up-box (pop-up-menu) ()) 

(defmethod get-slot-in-out-names ((self pop-up-menu))
   (values '("items" "action") 
           '(("yes" "no") nil)
           '("list of choices" "a function or box in lambda mode")
           '(nil nil)))

#+linux
(defmethod default-obj-box-size ((self pop-up-menu)) (om-make-point 130 65))


(defmethod omng-save ((self pop-up-menu) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'pop-up-menu (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled"
                                   :range ',(om-get-item-list self))))
     (oa::om-set-selected-item-index rep ',(om-get-selected-item-index self))
     rep))

(defmethod get-super-default-value ((type (eql 'pop-up-menu)))
  (om-make-dialog-item 'pop-up-menu 
                       #-linux(om-make-point 1 4) 
                       #+linux(om-make-point 1 2) 
                       (om-make-point 50 20) 
                       "untitled" :range '("yes" "no")))

(defmethod update-di-size ((self pop-up-menu) container)
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))

(defmethod set-dialog-item-params  ((self pop-up-menu) box args)
  (let* ((boxframe (om-view-container self))
        (newpop (om-make-dialog-item 'pop-up-menu 
                                     #-linux(om-make-point 1 4) 
                                     #+linux(om-make-point 1 2) 
                                     (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
                                     "untitled" 
                                     :range (if (and (pathnamep (car args)) (directoryp (car args)))
                                                (om-directory (car args))
                                              (car args))
                                                
                                     )))
    (when (om-view-container self)
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newpop)
      (om-set-dialog-item-action-function newpop #'(lambda (x) 
                                                     (let ((fun (omNG-box-value (second (inputs box)))))
                                                       (when fun
                                                         (funcall fun (om-get-selected-item-index x))))))
      (update-di-size newpop boxframe))
    newpop))

(defmethod rep-editor ((self pop-up-menu) num) 
  (cond
   ((= num 0) (om-get-selected-item-index self))
   ((= num 1) (om-get-selected-item self))
   (t nil)))

(defmethod (setf value) :after ((value pop-up-menu) (self omdiebox)) 
  (om-set-dialog-item-action-function value #'(lambda (x) 
                                                     (let ((fun (omNG-box-value (second (inputs self)))))
                                                       (when fun
                                                         (funcall fun (om-get-selected-item-index x)))))))



;==================
;SLIDER
;==================

(defclass! slider (om-slider d-i-box)  ()
   (:icon 298)
   (:documentation 
"The SLIDER box provides an interface to select an value in a given range, and possibly trigger an evaluation with it.

Use 'm' to show/hide the box interface.


- The 1st box input/output allows to set/get the direction of the slider (horizontal/vertical).
Note: the direction may also depend on the actual shape of the box.

- The 2nd and 3rd box input/output allows to set/get the value range and increment of the slider.

- The 4th box input allows to set the initial value of the slider. The output allows to get this value or the one corresponding to the current slider position.

- Connect a lambda function or patch to the 5th input. This function must accept one single argument (supposed to be an integer value).
Any change of the slider value will automatically call this function or patch passing it the current slider value as argument.
Evaluating the 5th output will also call and get the result of the function with the current slider value.

[Warning: Once initialized, SLIDER should be locked ('b') in order to prevent the contents and selection to be overwriten by the box input data.]
"
))
;; compat
(defclass! slider-box (slider) ()) 

(defmethod omng-save ((self slider) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'slider (om-make-point 1 1) (om-make-point ,(om-width self) ,(om-height self)) "untitled"
                                   :direction ,(om-get-slider-orientation self)
                                   :range ',(om-get-slider-range self)
                                   :increment 1
                                   :value ,(om-slider-value self))))
     rep))


(defmethod get-slot-in-out-names ((self slider))
   (values '("direction" "range" "increment" "value" "action") 
           '(:horizontal '(0 127) 1 60 nil)
           '("vertical or horizontal" "min and max values" "step" "slider value" "a patch in mode lambda")
           '(((0 (("horizontal" :horizontal) ("vertical" :vertical)))) nil nil nil nil)))

(defmethod get-super-default-value ((type (eql 'slider)))
  (om-make-dialog-item 'slider (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled" :range '(0 127) :increment 1 :value 60))


(defmethod update-di-size ((self slider) container)
   (if (equal (om-get-slider-orientation self) :horizontal)
       (progn
         (om-set-view-position self (om-make-point 8 (- (round (h container) 2) 12)))
         (om-set-view-size self (om-make-point (- (w container) 16) 24)))
     (progn
         (om-set-view-position self (om-make-point (- (round (w container) 2) 12) 8))
         (om-set-view-size self (om-make-point 24 (- (h container) 16))))))

(defmethod set-function ((self slider) fun box) 
  (om-set-dialog-item-action-function self #'(lambda (x) 
                                                    (when fun
                                                      (funcall fun (om-slider-value x)))
                                                    #+om-reactive (self-notify box)
                                                    )))

(defmethod set-dialog-item-params ((self slider) box args)
  (let* ((boxframe (om-view-container self))
         (newslider (om-make-dialog-item 
                    'slider 
                    (if (equal (car args) :horizontal)
                        (om-make-point 8 (if boxframe (- (round (h boxframe) 2) 12) 20))
                      (om-make-point (if boxframe (- (round (w boxframe) 2) 12) 20) 8))
                    (if (equal (car args) :horizontal)
                        (om-make-point (if boxframe (- (w boxframe) 16) 60) 24)
                      (om-make-point 24 (if boxframe (- (h boxframe) 16) 60)))
                    "untitled"
                    :direction (car args) :range (second args) 
                    :increment (third args) :value (fourth args))))
    (when boxframe
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newslider)
      (set-function newslider (fifth args) box)
      (update-di-size newslider boxframe)
      )
    newslider))

(defmethod rep-editor ((self slider) num)
  (cond
   ((= num 0) (om-get-slider-orientation self))
   ((= num 1) (om-get-slider-range self))
   ((= num 2) (om-slider-increment self))
   ((= num 3) (om-slider-value self))
   (t  (om-dialog-item-action self))))

(defmethod (setf value) :after ((value slider) (self omdiebox)) 
  (set-function value (omNG-box-value (fifth (inputs self))) self))



