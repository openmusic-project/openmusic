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

;changer peut-etre le nom en gate
;+ l'icone
;ajouter en interface genfunc!
;ajouter en plus un boutton choose all et delete all...
;--------------------------------------------------
;OMCHOOSE
;NEW Box as a method but with a dialog interface
;
;--------------------------------------------------
(defclass omchoose (OMBoxcall)
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


(defmethod Class-has-editor-p  ((self omchoose)) t )
(defmethod get-editor-class ((self omchoose)) 'omchooseEditor)

;;;;;wrap it in order to go in Genfunction menu
(defmethod get-boxcallclass-fun ((self (eql 'om-choose))) 'omchoose)


;(fmakunbound 'om-choose)

(defmethod* om-choose  ((l1? t) (l2? t) &rest lst?) 
  :numouts 1 
  :initvals '(nil nil nil) 
  :indoc '("anything" "additional elements")
  :icon 235
  :doc "Repeats <n> times the evaluation of <self> and collects the <n> results into a list.

Ex. (repeat-n (+ 1 1) 4) ==> (2 2 2 2)" 
 (apply 'list l1? l2? (mapcar #'append lst?)))
;;;;;;;;;;;;

(defmethod OpenEditorframe ((self omchoose)) (or (editorframe self) (set-choose-dialog self)))
(defmethod allow-lock ((self omchoose)) t)
(defmethod allow-lock-button ((self omchoose)) t)
(defmethod get-frame-name ((self omchoose)) (or (frame-name self) (string-downcase (name self))))
(defmethod get-frame-class ((self omchoose)) 'chooseFrame)

(defmethod numouts ((self omchoose)) 1)
(defmethod show-info-window ((self  omchoose) &optional (i 0)) (om-beep-msg "No info for out boxes"))
;(defmethod get-icon-box-class ((self omchoose)) 'inout-icon-box)
(defmethod omchoose-p ((self omchoose)) t)
(defmethod omchoose-p ((self t )) nil )

;make-new-editorcall
(defun make-new-choose (name indice posi key-p &optional (icon 235) (class 'omchoose))
  (let* ((thechoose (make-instance class
                                 :name "om-choose"
                                 :keyref (string (gensym))
                                 :icon icon
                                 :reference nil ;class 
                                 :indice indice
                                 :value nil
                                 )))
    (setf (frame-position thechoose) posi)
    (setf (inputs thechoose) (list (make-instance 'input-funbox
                                                :name (string name)
                                                :value nil 
                                                :box-ref thechoose
                                                :doc-string nil
                                                )))
    thechoose))
     

(defmethod gen-code ((self omchoose) numout)
   (declare (ignore numout)) 
   (let ((theinput (connected? (car (inputs self)))))
     (if theinput
       (gen-code (first theinput) (second theinput)) 'nil)))


(defmethod gen-code ((self omchoose) numout)
   (declare (ignore numout))
   (let ((theinput (loop for i in (inputs self)
                           collect (connected? i))))
     (print (list "gen" theinput))
     (if theinput
       (loop for i in theinput
               do (gen-code (first i) (second i))) 'nil)))

(defun om-load-boxchoose (name indice position docu inputs &optional fname val fsize keyref id value choice)
  (let ((newbox (make-new-choose name indice (om-correct-point position) nil)))
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

(defmethod omNG-save ((self OMChoose) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-boxchoose ,(name self) ,(indice self)  ,(om-save-point (frame-position self))  ,(docu self)
                      ',inputs ,(frame-name self) ,(omng-save (eval (defval self)) t)
                      ,(om-save-point (frame-size self)) ,(keyref self) ,(id self) 
                      ,(omng-save (value self) t)
                      ,(omng-save (choice self) t)
                    ;  ,(value self) ,(choice self)
                      )))


;;;;;;;;;;;;;;;;;;;;
;;box val

(defun unaire-p (lst)
  (if (and (atom (car lst)) (= 1 (length lst)))
      (car lst)))

(defmethod filt-box-value ((self omchoose)) 
  (let ((pos 
         (remove nil
         (loop for i in (choice self)
                   for n from 0 to (length (choice self))
                     collect (if i n)))))
    (if (unaire-p pos)
        (nth (car pos) (value self))
      (posn-match (value self) pos))))

(defmethod omNG-box-value ((self omchoose) &optional (numout 0))
  (declare (ignore numout))
  (setf (value self)
        (loop for i in (inputs self)
              collect (omng-box-value i)))
  (filt-box-value self))
  

(defmethod current-box-value ((self omchoose) &optional (numout nil))
  (declare (ignore numout))
  (value self) 
 (filt-box-value self)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmethod omNG-add-element ((self OMPatch) (elem omchoose))
   "When you add a new output to the patch 'self' you must update all ompatchboxes attached to 'self'."
   (setf (defval elem) (mypathname self))
   ;(setf (gethash (keyref elem) *send-db*) elem)
   (setf (mycontainer elem) self)
   (push elem (boxes self))
   (loop for item in (attached-objs self) do ;don't know if necessary???
         (update-from-reference item))
   )



(defmethod omng-remove-element ((self OMPatch) (box omchoose))
  "When you remove an output from the patch 'self' you must update all ompatchboxes attached to 'self'."
  (call-next-method)
  (loop for item in (attached-objs self) do
        (update-from-reference item)))

(defmethod do-add-one-input ((self omchoose))
  (setf (inputs self) (list+ (inputs self)
                             (list 
                              (make-instance 'input-funbox
                                             :name "" 
                                             :value t
                                             :doc-string "doc"
                                             )))) 
  (do-add-one-input-extra self))

(defmethod do-delete-one-input ((self omchoose))
   "Remove an optional input from the box 'self'."
   (let* ((current-imp (length (inputs self))))
     (when (> current-imp 1)
       (setf (inputs self) (butlast (inputs self)))
       (do-delete-one-input-extra self))))

(defmethod do-add-all-inputs ((self omchoose)) t)

(defmethod do-add-one-keyword ((self omchoose) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self omchoose)) nil)

;-------------FRAME
(defclass chooseframe (boxframe) ()
   (:documentation "Simple frame for OMOut boxes. #enddoc#
#seealso# (OMIN) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))


(defmethod eval-box ((self chooseframe)) 
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
(defmethod omg-remove-element ((self methodpanel) (box chooseframe))
   "Remove only for the generic function definition."
   (if (equal (win-mod (editor self)) :abs)
     (call-next-method)
     (om-beep-msg "You can not modify the outputs of a function already defined")))


(defmethod omg-remove-element ((self patchPanel) (box chooseframe))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (sends (find-class-boxes boxes 'chooseframe)))
     ))

(defmethod omG-rename ((self chooseframe) new-name)
  (setf (name (object self)) new-name)
  (call-next-method))

;(pushr 'om-choose *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'om-choose)) position container)
  (if (add-send-enabled container 'om-choose)
    (let* ((boxes (get-subframes container)) 
           (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'chooseframe))))
           (newsend (make-new-choose (mk-unique-name container "om-choose") i position t))
           )
      newsend)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

(defmethod show-big-doc ((self chooseframe)) nil)


;;;;;from patchcontainer

(defmethod add-choose-enabled ((self patchpanel) type) t)

(defmethod add-choose ((self patchpanel) position)
   (when (add-choose-enabled self 'om-choose)
     (let* ((boxes (get-subframes self)) 
            (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'chooseframe))))
            (pos (or position (om-make-point (+ 5 (* i 50)) 240)))
            (newsend (make-new-choose (mk-unique-name self "om-choose") i pos t))
            )
       (omG-add-element self (make-frame-from-callobj newchoose))
       (set-field-size self)
       )))


(defmethod omNG-copy ((self omchoose))
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

(defmethod OMGMoveObject ((self chooseframe) new-position)
   "If shift-key is down when drag self it do not move but it create and slot box."
   (call-next-method))

; all section from in-out-boxes starting from Dialogs (for renaming+ info panel)
(defmethod allow-rename ((self omchoose)) nil)


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

(defclass omchooseEditor (editorView) 
  ((check-boxes :accessor check-boxes :initform nil)))

(defmethod handle-key-event ((self omchooseEditor) char) nil);faire juste pour le help/doc?

(defmethod close-editor-after ((self omchooseEditor))
  (setf (editorFrame (object self)) nil))

(defmethod close-editor-before ((self omchooseEditor))
  (call-next-method))


(defmethod make-editor-window ((class (eql 'omchooseEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow nil) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize winsize :winpos winpos :resize t ;nil ;ICI!!
                                                      :close-p t :winshow t
                                                      )))
    win))

(defmethod update-subviews ((Self omchooseEditor))
   (om-set-view-size (panel self) (om-make-point (w self) (h self))))

(defmethod get-panel-class ((Self omchooseEditor)) 'omchoosePanel)

;=== MAIN PANEL ===
(defclass omchoosePanel (om-scroller) ()
  ;;;(:default-initargs :scrollbars :h :retain-scrollbars t)
   )

(defmethod editor ((self omchoosePanel)) 
  (om-view-container self))

(defmethod get-object ((Self omchoosePanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self omchoosePanel))
  (report-modifications (om-view-container self)))
                                   
;;;;

(defmethod metaobj-scrollbars-params ((self omchooseEditor))  '(:v t))


(defmethod initialize-instance :after ((self omchooseEditor) &rest l)
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

(defun set-choose-dialog (theinput) 
  (let* ((dialog (make-editor-window 'omchooseEditor theinput (or (frame-name theinput) (name theinput)) nil
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
(defmethod checkbox-list ((self omchooseeditor) (list t)) nil) 
(defmethod checkbox-list ((self omchooseeditor) (list list)) 
  (let* ((x0 20)
         (x1 65)
         (obj (object self))
         (checkboxes 
          (loop for i in list
                for n from 50 by 25 to (+ 50 (* 25 (length list)))
                for indx from 0 to (length list) 
                ;for state in (choice obj)
                collect (list (om-make-dialog-item 'om::om-check-box
                                                   (om::om-make-point x0 n) 
                                                   (om::om-make-point 20 20)
                                                   ""
                                                   ;(format nil "~D" indx)
                                                   ;:checked-p state
                                                   :di-action #'(lambda (c)
                                                                  (setf (choice (object self)) 
                                                                        (check-filt-list self))
                                                                  ))))))
    (setf (check-boxes self) checkboxes)
    (flat
     (x-append
      (om-make-dialog-item 'om::om-check-box
                               (om::om-make-point 20 4) 
                               (om::om-make-point 20 18)
                               ""
                             ;  :checked-p nil
                               :di-action 
                               #'(lambda (c)
                                   (let* ((chkboxes (flat (check-boxes self)))
                                          (lgt (length chkboxes)))
                                          (if (om-checked-p c)
                                       (loop for i in chkboxes
                                             do (progn
                                                  (setf (choice (object self)) (repeat-n t lgt))
                                                  (om-set-check-box i t)))
                                     (loop for i in chkboxes
                                           do (progn
                                                (setf (choice (object self)) (repeat-n nil lgt))
                                                (om-set-check-box i nil)))))))
                               
                                   
      (om-make-dialog-item 'om-static-text 
                           (om-make-point 60 4) 
                           (om-make-point 80 20) 
                           "set/reset all"
                           :font *controls-font*)
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

(defmethod check-filt-list ((self omchooseeditor))
  (let* ((boxes (flat (check-boxes self)))
         (obj (object self)))
    (loop for i in boxes
          collect (om-checked-p i))
    ))

