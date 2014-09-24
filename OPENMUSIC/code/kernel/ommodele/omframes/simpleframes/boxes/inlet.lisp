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
;Classes for box's inputs are defined in this file.
;Last Modifications :
;18/10/97 first date.
;DocFile



(in-package :om)

;Non graphic class, this class could be an OMBasicObject
;But i find that is simpler that
(defclass input-funbox () 
   ((doc-string :initform "" :initarg :doc-string  :accessor doc-string)
    (value :initform nil :initarg :value :accessor value)
    (name :initform nil :initarg :name :accessor name)
    (connected? :initform nil :accessor connected? :initarg :connected?)
    (box-ref :initform nil :initarg :box-ref :accessor box-ref)))


;-------Value and code generation
(defmethod get-icon-input ((self input-funbox)) 185)

(defmethod get-input-value ((self input-funbox)) (value self))


(defmethod omNG-box-value ((self input-funbox) &optional numout)
   (declare (ignore numout))
   (if (connected? self)
     (omNG-box-value (first (connected? self)) (second (connected? self)))
     (value self)))
   
(defmethod gen-code ((self input-funbox) numout)
  (declare (ignore numout))
  (let ((theinputconnect (connected? self))
        (value (value self)))
    (if theinputconnect
      (gen-code (first theinputconnect) (second theinputconnect))
      (gen-code value 0))))

(defmethod popup-input-menu ((self input-funbox) container)
   (let ((menulist (thepopup self))
           menu itemlist default)
     (loop for item in menulist 
            for i = 0 then (+ i 1) do
               (let ((title (car item))
                       (arg (second item)))
                 (if (equal (eval arg) (value self)) (setf default i))
                   (push (om-new-leafmenu title #'(lambda ()
                                                    (setf (value self) (eval arg))))
                               itemlist)))
     (setf menu (om-create-menu 'pair-pop-up-menu (reverse itemlist)))
     (when default (om-set-menu-default-item menu default))
     (om-open-pop-up-menu menu container)
     ))

   
;-----------------
;FRAMES
;-----------------

;This is the little bubble icon for inputs
;this class could be a simpleframe but i find that is sampler that
(omg-defclass input-funboxframe (icon-view) 
  ((object :initform nil :initarg :object :accessor object)))


;---------Inits
(defmethod input? ((self input-funboxframe)) t)
(defmethod input? ((self t)) nil)

(defmethod initialize-instance :after ((self input-funboxframe) &key controls)
  (declare (ignore controls))
  (setf (iconID self) (frame-icon-input self))
  (get&corrige-icon (iconID self)))

(defmethod frame-icon-input ((self input-funboxframe))
  (get-icon-input (object self)))

;-------Events
;Show only

;;; pour montrer ou pas les valeurs des inputs
(defvar *show-input-vals* t)

;;; new : textenterview container = panel 
(defmethod om-view-mouse-enter-handler ((self input-funboxframe))
  ;(oa::print-point (om-view-position (om-view-container self)))
  ;(print (x self))
   (unless (or (and (connected? (object self)) (not (keyword-input-p (object self)))) (not *show-input-vals*)) 
     (om-without-interrupts
      (when (and self (om-view-container self))
        (let* ((thetext (format () "~S" (value (object self))))
              (panel (om-view-container (om-view-container self)))
              (container (editor panel)))
          (when (text-view container)
            (exit-from-dialog (text-view container) (om-dialog-item-text (text-view container))))
          (setf (text-view container)
             (om-make-dialog-item 'text-enter-view
                                  (om-add-points (om-view-position (om-view-container self)) (om-make-point (- (x self) 4) -16))
                                  (om-make-point (get-name-size thetext) 20)
                                  thetext
                                  :container panel
                                  :font *om-default-font1*))
          ;(set-field-size panel)
          )))))

;;;new : text-view is on the panel
(defmethod om-view-mouse-leave-handler ((self input-funboxframe))
   (om-without-interrupts 
    (let* ((container (editor (om-view-container (om-view-container self)))))
      (when (and container (text-view container) (equal (class-name (class-of (text-view container))) 'text-enter-view))
        (om-remove-subviews (panel container) (text-view container))
        (setf (text-view container) nil)))
    )
   )


(defclass input-text-enter-view (edit-text-enter-view) ())

;;;new : text-view is on the panel
(defmethod om-view-click-handler ((self input-funboxframe) where)
  (declare (ignore where))
  
  ;;; (print (list (om-command-key-p) (om-shift-key-p) (om-option-key-p) (om-control-key-p)))
  
  (let* ((panel (om-view-container (om-view-container self)))
         (container (editor panel)))
     
     (cond
      ((om-command-key-p) (when (connected? (object self))
                        (disconnect-box (om-view-container self) self)))
      
      ((and (om-shift-key-p) (keyword-input-p (object self)) (val-menu (object self)))
       (popup-keyword-val-menu (object self) panel))
      
      ((and (om-shift-key-p) (not (maquette-p (object container)))) 
            (let* ((new-obj (omNG-make-new-boxcall (car *Basic-Lisp-Types*)
                                                   (om-make-point (+ (x (om-view-container self)) (x self))
                                                                  (- (y (om-view-container self)) 30))
                                                   (mk-unique-name panel "aux")))
                   new-frame)
              (setf (value new-obj) (get-input-value (object self)))
              (setf (thestring new-obj) (format () "~S" (value new-obj)))
              (setf (frame-size new-obj) (om-make-point (get-name-size (thestring new-obj)) 28))
              (setf new-frame (make-frame-from-callobj new-obj))
              (omG-add-element (om-view-container (om-view-container self)) new-frame)
              (connect-box (car (outframes new-frame)) self)
     
              (open-ttybox (iconview new-frame))))

      ((menu-input-p (object self))
       (popup-input-menu (object self) panel))
      
      ((and (keyword-input-p (object self)) (keyword-menu (object self)))
       (popup-keyword-input-menu (object self) panel)
       (om-view-set-help self (string+ "<" (string-downcase (name (object self))) "> ")))

      (t (unless (and (connected? (object self)) (not (keyword-input-p (object self))))
            (when (text-view container)
              (om-remove-subviews panel (text-view container))
              (setf (text-view container) nil))
            (let ((thetext (format () "~S" (value (object self)))))
              (setf (text-view container) (om-make-dialog-item 'input-text-enter-view
                                                               (om-add-points (om-view-position (om-view-container self))
                                                                                                (om-make-point (x self) -26))
                                                               (om-make-point (get-name-size thetext) 16)
                                                               thetext
                                                               :allow-returns nil
                                                               :di-selected-p t
                                                               :focus t
                                                               :object self
                                                               :container panel
                                                               :font *om-default-font1*))))))))
  
;-------

(omg-defclass temp-funboxframe (input-funboxframe) ())

(defmethod frame-icon-input ((self temp-funboxframe)) 227)



;------------------------------------------------------
;frame for class inputs, change the icon
;------------------------------------------------------


(omg-defclass input-classframe (icon-view) 
  ((object :initform nil :initarg :object :accessor object)))

(defmethod initialize-instance :after ((self input-classframe) &key controls)
   (declare (ignore controls))
   (setf (iconID self) 184))

;--------------------------------------------------------------------
;Text showed when mouse in input (non edit)
;--------------------------------------------------------------------
(omg-defclass text-enter-view (om-static-text) ())

;(defmethod om-draw-contents ((self text-enter-view))
;   (call-next-method)
;(om-draw-view-outline self))

;--------------------------------------------------------------------
; Edit input value
;--------------------------------------------------------------------

(omg-defclass edit-text-enter-view (om-editable-text) 
  ((object :initform nil :initarg :object :accessor object)))


(defmethod om-view-key-handler ((self edit-text-enter-view) char)
  ; (om-set-view-size self (om-make-point (max (w self) (+ 10 (get-name-size (om-dialog-item-text self)))) (h self)))
  nil)

;;; ENTER or RETURN if neqwline not accepted
(defmethod om-dialog-item-action ((self edit-text-enter-view))
  (exit-from-dialog self (om-dialog-item-text self)))


(defmethod copy ((self text-enter-view)) nil)
(defmethod paste ((self text-enter-view)) nil)

;;;new : text-view is on the panel
(defmethod exit-from-dialog ((self edit-text-enter-view) newtext)
  (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (setf (text-view (editor (om-view-container self))) nil)
                            (om-remove-subviews (panel (editor (om-view-container self))) self)
                            (om-beep)
                            (om-abort))))
     (let ((*package* (find-package :om))
           (fun-input (object (object self))))
       (if (keyword-input-p fun-input)
           (progn
             (set-new-keyword fun-input newtext)
             (om-view-set-help (object self) (string+ "<" (string-downcase (name (object (object self)))) "> ")))
         (setf (value fun-input) (read-from-string newtext)))
       (setf (text-view (editor (om-view-container self))) nil)
       (om-remove-subviews (panel (editor (om-view-container self))) self))))

;-------------------------------------------

(defclass input-funmenu (input-funbox) 
  ((thepopup :initform nil :initarg :thepopup :accessor thepopup)))

(defmethod menu-input-p ((self input-funmenu)) t)
(defmethod menu-input-p ((self t)) nil) 

(omg-defclass in-pop-up-menu (om-pop-up-menu)
  ((in-value :initform 0 :initarg :in-value :accessor in-value)
   (items :initform nil :initarg :items :accessor items)
   (originallist :initform nil :initarg :originallist :accessor originallist)))

(defun change-val (pu val)
  (setf (in-value pu) (eval val)))


;-------------------------------------------

(defclass input-keyword (input-funbox) 
  ((def-value :initform nil :initarg :def-value :accessor def-value)
   (val-menu :initform nil :initarg :val-menu :accessor val-menu)))

(defmethod get-icon-input ((self input-keyword)) 227)

(defmethod get-input-value ((self input-keyword)) (def-value self))


(defmethod keyword-input-p ((self input-keyword)) t)
(defmethod keyword-input-p ((self t)) nil) 

(defmethod keyword-menu ((self input-keyword)) t)
(defmethod keyword-menu ((self t)) nil) 


(defmethod popup-keyword-input-menu ((self input-keyword) container)
   (let* ((default nil)
          (menu (om-create-menu 'pair-pop-up-menu 
                                (loop for item in (get-keywords-from-box (box-ref self)) 
                                      for i = 0 then (+ i 1)
                                      collect (let ((currentkey item))
                                                (if (equal (value self) currentkey) (setf default i))
                                                (om-new-leafmenu (string-downcase currentkey)
                                                                 #'(lambda () (set-new-keyword self currentkey))
                                                                 nil
                                                                 #'(lambda () (or (equal (value self) currentkey)
                                                                                  (not (member currentkey (inputs (box-ref self)) :test 'equal :key 'value))))
                                                                 ))))))
     (when default (om-set-menu-default-item menu default))
     (om-open-pop-up-menu menu container)
     ))

(defmethod popup-keyword-val-menu ((self input-keyword) container)
   (let* ((default nil)
          (menu (om-create-menu 'pair-pop-up-menu 
                               (loop for item in (val-menu self) 
                                     for i = 0 then (+ i 1)
                                     collect (let ((currvalue (cadr item)))
                                               (if (equal (def-value self) currvalue) (setf default i))
                                               (om-new-leafmenu (car item)
                                                                #'(lambda () (setf (def-value self) currvalue))))))))
     
     (when default (om-set-menu-default-item menu default))
     (om-open-pop-up-menu menu container)
     ))


(defmethod set-new-keyword ((self input-keyword) (keyword string))
  (set-new-keyword self (read-from-string keyword)))

(defmethod set-new-keyword ((self input-keyword) keyword)
  (let ((keywords (get-keywords-from-box (box-ref self)))
        (err nil) (pos nil))
    (setf err (not (member (symbol-name keyword) keywords :test 'equal :key 'symbol-name)))
    (if err (om-beep-msg (format () "~D is not a good keyword. Allowed keywords are: ~S"  keyword keywords))
      (progn
        (setf pos (position keyword (inputs (box-ref self)) :test 'equal :key 'value))
        (setf err (and pos (not (= pos (position self (inputs (box-ref self)))))))
        (if err (om-beep-msg (format () "~D is already used by the input ~D in this box." keyword (incf pos))))))
    (unless err
      (setf (value self) keyword)
      (setf (name self) (string-downcase keyword))
      (let ((ref (reference (box-ref self))))
        (cond ((and (symbolp ref) (fboundp ref) (omgenfun-p (fdefinition ref)))
               (let* ((thefun (fdefinition ref))
                      (arg-list (ordered-arg-list (fdefinition ref)))
                      (argpos (position keyword arg-list))
                      (initval (nth argpos (inputs-default thefun)))
                      (doc (nth argpos (inputs-doc thefun)))
                      (menu (cadr (find argpos (inputs-menus thefun) :key 'car :test '=))))
                 (setf (def-value self) initval)
                 (setf (doc-string self) doc)
                 (setf (val-menu self) menu)
                 ))
              ((omclass-p ref)
               (let* ((theslot (find (symbol-name keyword) (get-all-slots-of-class (class-name ref)) :test 'string-equal :key 'name)))
                 (setf (def-value self) (valued-val (theinitform theslot)))
                 (setf (doc-string self) (doc theslot))
                 ))
              ))
      )
    ))



     
(defmethod eval-keyword-in ((self input-keyword))
   (declare (ignore numout))
   (list (value self) 
         (if (connected? self)
           (omNG-box-value (first (connected? self)) (second (connected? self)))
           (eval-not-connected self))))
   
(defmethod gen-code-keyword-in ((self input-keyword))
   (declare (ignore numout))
   (let ((theinput (connected? self))
         (value (value self)))
     (list value
           (if theinput
             (gen-code (first theinput) (second theinput))
             (code-not-connected self)))))

(defmethod eval-not-connected ((self input-keyword))
  ;(om-beep-msg (format nil "WARNING keyword ~D is not connected, NIL is used as default value." (string (value self))))
  (def-value self))
   
(defmethod code-not-connected ((self input-keyword))
   ;(om-beep-msg (format nil "WARNING keyword ~D is not connected, NIL is used as default value." (string (value self))))
  (def-value self))


;==============================
;Subclass this class to edit static-text

(defclass click-and-edit-text (om-static-text) 
   ((mini-editor :initform nil :accessor mini-editor)))

;------------Events
(defmethod om-view-click-handler ((self click-and-edit-text) where)
   (declare (ignore where))
   (open-mini-editor self))

(defmethod do-exit-function ((self click-and-edit-text) container newtext)
   (when newtext
     (om-set-dialog-item-text self newtext)
     (om-add-subviews container self)))

;------------Edition
(defmethod open-mini-editor ((self click-and-edit-text))
   (let ((container (om-view-container self)))
     (setf (mini-editor self) (om-make-dialog-item 'edit-click-and-edit 
                                                   (om-view-position self) (om-view-size self)
                                                   (om-dialog-item-text self)
                                                   :allow-returns t
                                                   :object self
                                                   :container container
                                                   :font *om-default-font2*))
     (om-remove-subviews container self)))

(omg-defclass edit-click-and-edit (edit-text-enter-view) 
   ((object :initform nil :initarg :object :accessor object)))

;;;new : text-view is on the panel
(defmethod exit-from-dialog ((self edit-click-and-edit) newtext)
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (setf (text-view (editor (om-view-container self))) nil)
                            (om-remove-subviews (panel (editor (om-view-container self))) self)
                            (om-beep)
                            (om-abort))))
     (let ((*package* (find-package :om))
           (original (object self)))
       (do-exit-function original container newtext) 
       (setf (mini-editor original) nil)
       (om-remove-subviews (om-view-container self) self))))
 



 




