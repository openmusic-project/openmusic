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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;Author: Karim Haddad
;DocFile
;OM-Select object.
;Last Modifications :
;01/09/24 first date.
;DocFile

(in-package :om)


;--------------------------------------------------
;OMSELECT
;NEW Box as a method but with a dialog interface
;
;--------------------------------------------------
(defclass omselect (OMBoxcall)
   ((docu :initform "" :accessor docu)
    (defval :initform nil :accessor defval)
    (in-symbol :initform nil :accessor in-symbol)
    (indice :initform t :initarg :indice :accessor indice)
    (keyref :initform nil :initarg :keyref :accessor keyref)
    (id :initform nil :initarg :id :accessor id)
    (value :initform nil :initarg :value :accessor value)
    (choice :initform nil :initarg :choice :accessor choice))
   (:documentation "Output boxes in a patch are instance of this class. #enddoc#
#seealso# (omboxcall ompatch outFrame) #seealso#
#indice# Used in order to sort all outputs in a patch. #indice#"))


(defmethod Class-has-editor-p  ((self omselect)) t )
(defmethod get-editor-class ((self omselect)) 'omselectEditor)

;;;;;wrap it in order to go in Genfunction menu
(defmethod get-boxcallclass-fun ((self (eql 'om-select))) 'omselect)

(defmethod* om-select  ((self list)) 
  :numouts 1 
  :initvals '('(1 2)) 
  :indoc '("a list")
  :icon 235
  :doc "Repeats <n> times the evaluation of <self> and collects the <n> results into a list.

Ex. (repeat-n (+ 1 1) 4) ==> (2 2 2 2)" 
  self)
;;;;;;;;;;;;

(defmethod OpenEditorframe ((self omselect)) (or (editorframe self) (set-select-dialog self)))
(defmethod allow-lock ((self omselect)) t)
(defmethod allow-lock-button ((self omselect)) t)
(defmethod get-frame-name ((self omselect)) (or (frame-name self) (string-downcase (name self))))
(defmethod get-frame-class ((self omselect)) 'selectFrame)

(defmethod numouts ((self omselect)) 1)
(defmethod show-info-window ((self  omselect) &optional (i 0)) (om-beep-msg "No info for out boxes"))
;(defmethod get-icon-box-class ((self omselect)) 'inout-icon-box)
(defmethod omselect-p ((self omselect)) t)
(defmethod omselect-p ((self t )) nil )

;make-new-editorcall
(defun make-new-select (name indice posi key-p &optional (icon 235) (class 'omselect))
  (let* ((theselect (make-instance class
                                 :name "om-select"
                                 :keyref (string (gensym))
                                 :icon icon
                                 :reference nil
                                 :indice indice
                                 :value nil
                                 )))
    (setf (frame-position theselect) posi)
    (setf (inputs theselect) (list (make-instance 'input-funbox
                                                :name (string name)
                                                :value nil 
                                                :box-ref theselect
                                                :doc-string nil
                                                )))
    theselect))
     

(defmethod gen-code ((self omselect) numout)
   (declare (ignore numout))
   (let ((theinput (connected? (car (inputs self)))))
     (if theinput
       (gen-code (first theinput) (second theinput)) 'nil)))



(defun om-load-boxselect (name indice position docu inputs &optional fname val fsize keyref id value choice)
  (let ((newbox (make-new-select name indice (om-correct-point position) nil)))
    (setf (docu newbox) docu)
    (when val
      (setf (defval newbox) (put-quote val)))
    (setf (keyref newbox) keyref);get key from patch
    (setf (id newbox) id)
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    ;(setf (value newbox) value)
    ;(setf (choice newbox) choice)
    (setf (value newbox) (mapcar #'(lambda (val) val) value))
    (setf (choice newbox) (mapcar #'(lambda (val) val) choice))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    ;(push_in_db newbox);put newbox in *send-db*
    newbox))

(defmethod omNG-save ((self OMSelect) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-boxselect ,(name self) ,(indice self)  ,(om-save-point (frame-position self))  ,(docu self)
                      ',inputs ,(frame-name self) ,(omng-save (eval (defval self)) t)
                      ,(om-save-point (frame-size self)) ,(keyref self) ,(id self) 
                      ,(omng-save (value self) t)
                      ,(omng-save (choice self) t)
                    ;  ,(value self) ,(choice self)
                      )))


(defmethod filt-box-value ((self omselect))
  (when (listp (value self))
  (remove nil
    (loop for val in (value self)
          for ch in (choice self)
            collect (if ch val)))))

(defmethod omNG-box-value ((self omselect) &optional (numout 1)) 
  (declare (ignore numout))
  (setf (value self) (omng-box-value (car (inputs self))));necessary!
  (filt-box-value self)
  )

(defmethod current-box-value ((self omselect) &optional (numout nil)) 
  (declare (ignore numout))
  ;(value self)
 (filt-box-value self)
)
#|
  (if numout (nth numout (value self))
    (value self)))
|#

(defmethod omNG-add-element ((self OMPatch) (elem omselect))
   "When you add a new output to the patch 'self' you must update all ompatchboxes attached to 'self'."
   (setf (defval elem) (mypathname self))
   ;(setf (gethash (keyref elem) *send-db*) elem)
   (setf (mycontainer elem) self)
   (push elem (boxes self))
   (loop for item in (attached-objs self) do ;don't know if necessary???
         (update-from-reference item))
   )



(defmethod omng-remove-element ((self OMPatch) (box omselect))
  "When you remove an output from the patch 'self' you must update all ompatchboxes attached to 'self'."
  (call-next-method)
  (loop for item in (attached-objs self) do
        (update-from-reference item))
  ;(setf *send-to-delete* nil)
  )



(defmethod do-add-one-input ((self omselect))  nil)
(defmethod do-delete-one-input ((self omselect)) nil)
(defmethod do-add-all-inputs ((self omselect)) nil)

(defmethod do-add-one-keyword ((self omselect) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self omselect)) nil)

;-------------FRAME
(defclass selectframe (boxframe) ()
   (:documentation "Simple frame for OMOut boxes. #enddoc#
#seealso# (OMIN) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))


(defmethod eval-box ((self selectframe)) 
  (omng-box-value (object self))
  (let ((v (current-box-value (object self) nil)))
    ;(format *om-stream* "OM => ~S~%" v)))
    ;(if (atom v) 
    ;    (format *om-stream* "OM => Only lists!~%")
    (if (consp v) 
        (if (> (length v) 1)
            (format *om-stream* "OM => ~{~S ~}~%" (list v))
          (format *om-stream* "OM => ~S~%" (car v))) ;faire message d'erreur quand c'est un atom
      (format *om-stream* "OM => ~S~%" v))
    ;)
  ))


;peut-etre pas besoin
(defmethod omg-remove-element ((self methodpanel) (box selectframe))
   "Remove only for the generic function definition."
   (if (equal (win-mod (editor self)) :abs)
     (call-next-method)
     (om-beep-msg "You can not modify the outputs of a function already defined")))


(defmethod omg-remove-element ((self patchPanel) (box selectframe))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (sends (find-class-boxes boxes 'selectframe)))
     #|
     (when (indice (object box))
       (loop for item in sends do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))
     |#
     ))

(defmethod omG-rename ((self selectframe) new-name)
  (setf (name (object self)) new-name)
  (call-next-method))

;(pushr 'om-select *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'om-select)) position container)
  (if (add-send-enabled container 'om-select)
    (let* ((boxes (get-subframes container)) 
           (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'selectframe))))
           (newsend (make-new-select (mk-unique-name container "om-select") i position t))
           )
      newsend)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

(defmethod show-big-doc ((self selectframe)) nil)


;;;;;from patchcontainer

(defmethod add-select-enabled ((self patchpanel) type) t)

(defmethod add-select ((self patchpanel) position)
   (when (add-select-enabled self 'om-select)
     (let* ((boxes (get-subframes self)) 
            (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'selectframe))))
            (pos (or position (om-make-point (+ 5 (* i 50)) 240)))
            (newsend (make-new-select (mk-unique-name self "om-select") i pos t))
            )
       (omG-add-element self (make-frame-from-callobj newselect))
       (set-field-size self)
       )))


(defmethod omNG-copy ((self omselect))
  `(let ((copy (make-instance ',(class-name (class-of self))
                 :name ,(name self)
                 :icon ,(copy-icon (icon self))
                 :reference nil
                 :indice ,(indice self))))
     (setf (frame-position copy) ,(om-copy-point (frame-position self)))
     (setf (frame-size copy) ,(om-copy-point (frame-size self)))
    ; (setf (frame-name copy) ,(frame-name self))
    ; (setf (docu copy) ,(docu self))
    ; (setf (defval copy) (put-quote ,(clone (defval self))))
     (setf (inputs copy) (list (make-instance 'input-funbox
                                               ; :name ,(string name)
                                                :value nil 
                                                :box-ref copy
                                                :doc-string nil
                                                )))
     copy))



;ici en shift drag (comme slot) faire les receives...

(defmethod OMGMoveObject ((self selectframe) new-position)
   "If shift-key is down when drag self it do not move but it create and slot box."
   (call-next-method))

; all section from in-out-boxes starting from Dialogs (for renaming+ info panel)
(defmethod allow-rename ((self omselect)) nil)


#|
;;;ATTENTION ici omboxframe!
(defmethod OMGMoveObject ((self omboxframe) new-position)
   "Move 'self' to 'new-position'."
   (setf new-position (borne-position new-position))
   (om-set-view-position self new-position)
   (setf (frame-position (object self)) new-position)
   (om-highlight-view self nil))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;DIALOG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass omselectEditor (editorView) 
  ((check-boxes :accessor check-boxes :initform nil)))

(defmethod handle-key-event ((self omselectEditor) char) nil);faire juste pour le help/doc?

(defmethod close-editor-after ((self omselectEditor))
  (setf (editorFrame (object self)) nil))

(defmethod close-editor-before ((self omselectEditor))
  (call-next-method))


(defmethod make-editor-window ((class (eql 'omselectEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow nil) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize winsize :winpos winpos :resize t ;nil ;ICI!!
                                                      :close-p t :winshow t
                                                      )))
    win))

(defmethod update-subviews ((Self omselectEditor))
   (om-set-view-size (panel self) (om-make-point (w self) (h self))))

(defmethod get-panel-class ((Self omselectEditor)) 'omselectPanel)

;=== MAIN PANEL ===
(defclass omselectPanel (om-scroller) ()
  ;;;(:default-initargs :scrollbars :h :retain-scrollbars t)
   )

(defmethod editor ((self omselectPanel)) 
  (om-view-container self))

(defmethod get-object ((Self omselectPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self omselectPanel))
  (report-modifications (om-view-container self)))
                                   
;;;;

(defmethod metaobj-scrollbars-params ((self omselectEditor))  '(:v t))


(defmethod initialize-instance :after ((self omselectEditor) &rest l)
  (declare (ignore l))
  (setf (panel self) (om-make-view (get-panel-class self) 
                                   :owner self
                                   :position (om-make-point 0 0) 
                                   :bg-color *om-light-gray-color* ;(om-make-color 0.5 0.5 0.5)
                                   :scrollbars (first (metaobj-scrollbars-params self)) ;t
                                   :field-size  (om-make-point 450 5400)
                                   ;;;:size (om-make-point (w self) (- (h self) 15)))
                                   :size (om-make-point (w self) (h self))))
  ;;add check-boxes
  (let ((checks (checkbox-list self (value (object self)))))
    (when checks
      (apply 'om-add-subviews (cons (panel self)  checks)))
  ))


;;;;;;

(defun set-select-dialog (theinput) 
  (let* ((dialog (make-editor-window 'omselectEditor theinput (or (frame-name theinput) (name theinput)) nil
                                     :winpos :centered
                                     :winsize (om-make-point 410 #+linux 450 #+(or macosx win32) 435)
                                     :resize t
                                     :retain-scroll t
                                     ))
         (theindex (indice theinput)))

    (let ((v (eval (defval theinput))))
      (when (omclass-p (class-of (class-of v)))
        (setf (instance-p defvalitem) t)
        )
      )

    (om-set-bg-color (editor dialog) *controls-color+*)
    (om-set-bg-color dialog *controls-color+*)
    (om-add-subviews (editor dialog))
    (loop for i in (flat (check-boxes (editor dialog)))
          for test in (choice theinput)
          do (if test (om-set-check-box i t)))
    dialog))



;add sunbviews according to list
(defmethod checkbox-list ((self omselecteditor) (list t)) nil) 
(defmethod checkbox-list ((self omselecteditor) (list list)) 
  (let* ((x0 20)
         (x1 65)
         (obj (object self))
         (checkboxes 
          (loop for i in list
                for n from 50 by 25 to (+ 50 (* 25 (length list)))
                for indx from 0 to (length list) 
                ;for state in (choice obj)
                collect (list (om::om-make-dialog-item 'om::om-check-box
                                                       (om::om-make-point x0 n) 
                                                       (om::om-make-point 20 20)
                                                       (format nil "~D" indx)
                                                       ;:checked-p state
                                                       :di-action #'(lambda (c)
                                                                      (setf (choice (object self)) 
                                                                            (check-filt-list self))
                                                                      ))))))
    (setf (check-boxes self) checkboxes)
    (flat
     (x-append
      (om-make-dialog-item 'om-static-text 
                           (om-make-point 10 25) 
                           (om-make-point 80 20) 
                           "choice"
                           :font *controls-font*)
      (om-make-dialog-item 'om-static-text 
                           (om-make-point 60 25) 
                           (om-make-point 80 20) 
                           "item"
                           :font *controls-font*)
      (loop for i in list
            for n from 50 by 25 to (+ 50 (* 25 (length list)))
            for indx from 0 to (length list) 
            collect 
              (om-make-dialog-item 'om-static-text 
                                   (om-make-point x1 n) 
                                   (om-make-point 1080 20) 
                                   (format nil "~D" indx))
              )
      checkboxes))))

;(checkbox-list '(1 2 3 4 5))

(defmethod check-filt-list ((self omselecteditor))
  (let* ((boxes (flat (check-boxes self)))
         (obj (object self)))
    (loop for i in boxes
          collect (om-checked-p i))
    ))

