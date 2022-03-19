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
;Objects + Frames for send and receives.
;Last Modifications :
;28/02/22 first date.
;DocFile

(in-package :om)

;--------------------------------------------------
;DATABASE HASH
;--------------------------------------------------

(defparameter *send-db* (make-hash-table :test #'equal))
(defparameter *receive-db* (make-hash-table :test #'equal))

;--------------------------------------------------
;OMSEND
;--------------------------------------------------
(defclass OMsend (OMBoxcall)
   ((docu :initform "" :accessor docu)
    (defval :initform nil :accessor defval)
    (in-symbol :initform nil :accessor in-symbol)
    (indice :initform t :initarg :indice :accessor indice)
    (keyname :initform nil :initarg :keyname :accessor keyname)
    (value :initform nil :initarg :value :accessor value))
   (:documentation "Output boxes in a patch are instance of this class. #enddoc#
#seealso# (omboxcall ompatch outFrame) #seealso#
#indice# Used in order to sort all outputs in a patch. #indice#"))


(defmethod OpenEditorframe ((self OMSend)) (or (editorframe self) (set-send-dialog self)))
(defmethod allow-lock ((self OMsend)))
(defmethod allow-lock-button ((self OMsend)) nil)
(defmethod get-frame-name ((self OMsend)) (or (frame-name self) (name self)))
(defmethod get-frame-class ((self OMsend)) 'SendFrame)
(defmethod numouts ((self OMsend)) 0)
(defmethod show-info-window ((self OMsend) &optional (i 0)) (om-beep-msg "No info for out boxes"))
(defmethod get-icon-box-class ((self OMsend)) 'inout-icon-box)


(defun make-new-send (name indice posi key-p &optional (icon 856) (class 'OMsend))
  (let* ((thesend (make-instance class
                                 :name "send"
                                 :keyname (string (gensym))
                                 :icon icon
                                 :reference nil
                                 :indice indice
                                 :value nil
                                 )))
    (when key-p ;so not to duplicate hash entry while reloading
    (push_in_db thesend))
    (setf (frame-position thesend) posi)
    (setf (inputs thesend) (list (make-instance 'input-funbox
                                                :name (string name)
                                                :value nil 
                                                :box-ref thesend
                                                :doc-string nil)))
    thesend))
     

(defmethod gen-code ((self OMsend) numout)
   (declare (ignore numout))
   (let ((theinput (connected? (car (inputs self)))))
     (if theinput
       (gen-code (first theinput) (second theinput)) 'nil)))

(defmethod omNG-box-value ((self OMsend) &optional (numout 0))
  (declare (ignore numout))
    (let* ((key (keyname self)) 
           (receives (gethash key *receive-db*)))
           (when receives
               (loop for i in receives
                       do (setf (value i) (car (mapcar 'omng-box-value (inputs self)))))))
   (setf (value self) (mapcar 'omng-box-value (inputs self))))


(defmethod current-box-value ((self OMsend) &optional (numout nil))
  (if numout (nth numout (value self))
    (value self)))


(defmethod omNG-add-element ((self OMPatch) (elem OMsend))
   "When you add a new output to the patch 'self' you must update all ompatchboxes attached to 'self'."
   (setf (defval elem) (mypathname self))
   (setf (gethash (keyname elem) *send-db*) elem)
   (setf (mycontainer elem) self)
   (push elem (boxes self))
   (loop for item in (attached-objs self) do ;don't know if necessary???
         (update-from-reference item))
   )


(defvar *send-to-delete* nil)


(defmethod omng-remove-element ((self OMPatch) (box OMSend))
  "When you remove an output from the patch 'self' you must update all ompatchboxes attached to 'self'."
  (call-next-method)
  (setf *send-to-delete* (indice box))
  (let* ((key (keyname box))
        (receives (gethash key *receive-db*))
        ;(patchpanels (loop for i in receives
        ;                    collect (om-view-container (car (frames i)))))
        )
    (when receives
      (loop for i in receives
          do
          (progn
            (setf (indice i) nil)
            (setf (icon i) 190) ;skull (needs refreshing)
            ;(omng-remove-element (object (om-view-container (car (frames i)))) i);(needs refreshing)
            ;(om-invalidate-view (car (frames i)) t)
           ; (om-invalidate-view (om-view-container (car (frames i))) t)
          ;  (load-patch (object (om-view-container (car (frames i))))) ;marche pas.
            ))
      ))
  (pop_db box)
  (loop for item in (attached-objs self) do
        (update-from-reference item))
  (setf *send-to-delete* nil))



(defmethod do-add-one-input ((self OMsend))  nil)
(defmethod do-delete-one-input ((self OMsend)) nil)
(defmethod do-add-all-inputs ((self OMsend)) nil)

(defmethod do-add-one-keyword ((self OMsend) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMsend)) nil)

;-------------FRAME
(defclass sendFrame (boxframe) ()
   (:documentation "Simple frame for OMOut boxes. #enddoc#
#seealso# (OMIN) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))

;peut-etre pas besoin
(defmethod omg-remove-element ((self methodpanel) (box sendFrame))
   "Remove only for the generic function definition."
   (if (equal (win-mod (editor self)) :abs)
     (call-next-method)
     (om-beep-msg "You can not modify the outputs of a function already defined")))


(defmethod omg-remove-element ((self patchPanel) (box sendFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (sends (find-class-boxes boxes 'sendFrame)))
     (when (indice (object box))
       (loop for item in sends do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))


(defmethod omG-rename ((self sendFrame) new-name)
  "rename send and all receives"
  (setf (name (object self)) new-name) 
  (let* ((key (keyname (object self)))
         (receives (gethash key *receive-db*))
         (panels (loop for i in receives
                       collect (mycontainer i))))
    (when receives
      (loop for i in receives 
            for p in panels
            do (progn (openeditorframe p)
                 (omg-rename (car (frames i)) new-name))))
  (call-next-method)))


(pushr 'send *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'send)) position container)
  (if (add-send-enabled container 'send)
    (let* ((boxes (get-subframes container)) 
           (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'sendFrame))))
           (newsend (make-new-send (mk-unique-name container "send") i position t))
           )
      newsend)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

(defmethod show-big-doc ((self sendFrame)) nil)


;==================================
; special icon-view avec numero dessus
;==================================

(defclass send-icon-box (icon-box) ())

(defmethod om-draw-contents ((self send-icon-box))
  (call-next-method)
  (let ((container (om-view-container self)))
    (unless (tempoutframe-p container)
      (draw-send-box self (object container))
      )))

(defmethod draw-send-box ((container t) object)
  (om-with-focused-view container
    (om-with-font *om-default-font1b*
                  (om-draw-string (- (round (w container) 2) 7) 12 (format () "~2D" (indice object))))
    ))

;;;;;from patchcontainer

(defmethod add-send-enabled ((self patchpanel) type) t)

(defmethod add-send ((self patchpanel) position)
   (when (add-send-enabled self 'send)
     (let* ((boxes (get-subframes self)) 
            (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'sendFrame))))
            (pos (or position (om-make-point (+ 5 (* i 50)) 240)))
            (newsend (make-new-send (mk-unique-name self "send") i pos t))
            )
       (omG-add-element self (make-frame-from-callobj newsend))
       (set-field-size self)
       )))



;un copy de send donnant un receive:

(defmethod omNG-replicate ((self OMsend) posi)
  `(let ((copy (make-new-receive "receive" ,(indice self) ,(om-copy-point (frame-position self)) 
                                 857 
                                 ',(class-name (class-of (make-instance 'omreceive))))))
     (setf (frame-name copy) ,(frame-name self))
     (setf (frame-size copy) ,(om-copy-point (frame-size self)))
     (setf (frame-position copy) ,(borne-position posi))
     (setf (defval copy) ,(defval self))
     (setf (keyref copy) ,(keyname self))
     copy))


;ici en shift drag (comme slot) faire les receives...

(defmethod OMGMoveObject ((self sendFrame) new-position)
   "If shift-key is down when drag self it do not move but it create and slot box."
   (if (or (om-shift-key-p) (shift-key-p *OM-drag&drop-handler*))
     (let* ((target (om-view-container self)) newobj)
     
       (when target
         (setf newobj (eval (omng-replicate (object self) (borne-position new-position))))
         (omG-add-element target (make-frame-from-callobj newobj))))
     (call-next-method)))

; all section from in-out-boxes starting from Dialogs (for renaming+ info panel)
(defmethod allow-rename ((self OMSend)) t)


;--------------------------------------------------
;OMRECEIVE
;--------------------------------------------------
(defclass OMReceive (OMBoxcall)
   ((docu :initform "" :accessor docu)
    (defval :initform nil :accessor defval)
    (in-symbol :initform nil :accessor in-symbol)
    (indice :initform t :initarg :indice :accessor indice)
    (keyref :initform nil :initarg :keyref :accessor keyref)
    (value :initform nil :initarg :value :accessor value)
    )
   (:documentation "Input boxes in a patch are instance of this class. #enddoc#
#seealso# (omboxcall ompatch inFrame) #seealso#
#docu# Store the documentation of the input. #docu#
#defval# Store the default value of the input. #defval#
#in-symbol# A symbol used for code generation. #in-symbol#
#indice# Used in order to sort all inputs in a patch. #indice#"))


(defmethod OpenEditorframe ((self OMReceive)) (or (editorframe self) (set-send-dialog self)))
(defmethod numouts ((self OMReceive)) 1)
(defmethod get-frame-class ((self OMReceive)) 'receiveFrame)
(defmethod get-frame-name ((self OMReceive)) (or (frame-name self) (name self)))
(defmethod allow-lock-button ((self OMReceive)) nil)
(defmethod get-documentation ((self OMReceive)) "Input ~D~%" (indice self) (docu self))
(defmethod get-object-insp-name ((self OMReceive)) "Normal input")
(defmethod get-icon-box-class ((self OMReceive)) 'inout-icon-box)

(defun make-new-receive (name indice posi &optional (icon 857) (class 'OMReceive))
   (let* ((theinput (make-instance class
                      :name name
                      :icon icon
                      :reference nil
                      :indice indice)))
     (setf (frame-position theinput) posi)
     theinput))


(defmethod omNG-add-element ((self OMPatch) (elem OMReceive))
   "When you add a new input to the patch 'self' you must update all ompatchboxes attached to 'self'."
  (push elem (boxes self))
  (setf (mycontainer elem) self)
  (push_in_db elem (keyref elem))
  ;(unless *loaading-stack* ;;; do not update if patch is being loaded: inputs are already OK
    (loop for item in (attached-objs self) do
          (update-from-reference item))
    ;)
  )

(defvar *receive-to-erase* nil)

(defmethod omng-remove-element ((self OMPatch) (box OMReceive))
   "When you remove an input from the patch 'self' you must update all ompatchboxes attached to 'self'."
   (call-next-method)
   (setf *receive-to-erase* (indice box))
   (pop_db box) 
   (loop for item in (attached-objs self) do
         (update-from-reference item))
   (setf *receive-to-erase* nil))

#|
(defmethod gen-code ((self OMReceive) numout)
   (declare (ignore numout))
   (if *compiling-macro-boite* (value ,self) (in-symbol ,self)))
|#


;ICI:
(defmethod gen-code ((self OMReceive) numout)
  (declare (ignore numout))
  ; (setf (value ,self)
  ;       (car (omng-box-value (gethash (keyref ,self) *send-db*))))
   `(value ,self))


(defmethod omNG-box-value ((self OMReceive) &optional (numout nil))
   (let ((thesend (gethash (keyref self) *send-db*))) 
     (when thesend
       (setf (value self)
       (car (omng-box-value thesend))))))
     
     
 

;;; does not store values (?)
;(defmethod current-box-value ((self OMReceive) &optional (numout nil)))
(defmethod current-box-value ((self OMReceive) &optional (numout nil))
    (list (value self)))

(defmethod get-default-input-val ((self OMReceive)) nil) 
(defmethod do-add-one-input ((self OMReceive))  nil)
(defmethod do-add-all-inputs ((self OMReceive)) nil)
(defmethod do-delete-one-input ((self OMReceive)) nil)

(defmethod do-add-one-keyword ((self OMReceive) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMReceive)) nil)

;-------FRAME
(defclass ReceiveFrame (boxframe) ()
   (:documentation "Simple frame for OMReceive boxes. #enddoc#
#seealso# (OMReceive) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))

(defmethod omg-remove-element ((self patchPanel) (box ReceiveFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'ReceiveFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))

(defmethod omG-rename ((self ReceiveFrame) new-name)
  (setf (name (object self)) new-name)
  (call-next-method))



(defmethod OMGMoveObject ((self omboxframe) new-position)
   "Move 'self' to 'new-position'."
   (setf new-position (borne-position new-position))
   (om-set-view-position self new-position)
   (setf (frame-position (object self)) new-position)
   (om-highlight-view self nil))

(pushr 'receive *spec-new-boxes-types*)
(defmethod get-new-box-from-type ((type (eql 'receive)) position container)
  (if (add-input-enabled container 'receive)
  (let* ((boxes (get-subframes container))
         (i (length (list+ (find-class-boxes boxes 'selfInFrame) (find-class-boxes boxes 'ReceiveFrame)))))
    (make-new-receive (mk-unique-name container "receive") i position))
  (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

(defmethod show-big-doc ((self receiveframe)) nil)


; (for renaming+ info panel)
(defmethod allow-rename ((self OMReceive)) t)




;==================================
; special icon-view avec numero dessus
;==================================

(defclass sendreceive-icon-box (icon-box) ())

(defmethod om-draw-contents ((self sendreceive-icon-box))
  (call-next-method)
  (let ((container (om-view-container self)))
    (unless (tempoutframe-p container)
      (draw-send-receive-box self (object container))
      )))

(defmethod draw-send-receive-box ((container t) object)
  (om-with-focused-view container
    (om-with-font *om-default-font1b*
                  (om-draw-string (- (round (w container) 2) 7) 12 (format () "~2D" (indice object))))
    ))



;==================================
; Primitive Hash table tools
;==================================
;;SEND DB
(defmethod push_in_db ((self omsend) &optional (key nil))
  (let ((genkey (keyname self)))
    (setf (gethash genkey *send-db*) self)))

(defmethod push_in_db ((self omreceive) &optional (key nil))
  (unless (nth-value 1 (gethash key *receive-db*))
    (setf (gethash key *receive-db*) ()))
  (unless (member self (gethash key *receive-db*))
  (push self (gethash key *receive-db*))))

(defmethod pop_db ((self omsend))
  (let ((key (keyname self)))
    (remhash key *send-db*)
    (remhash key *receive-db*)
    ))

(defmethod pop_db ((self omreceive))
  (let ((key (keyref self)))
    (setf (gethash key *receive-db*) (remove self (gethash key *receive-db*)))))

;(remhash key *send-db*)))



(defun print-send-db()
  (loop for key being the hash-keys of *send-db*
        using (hash-value value)
        do (print (list key value))))

(defun print-receive-db()
  (loop for key being the hash-keys of *receive-db*
        using (hash-value value)
        do (print (list key value))))


;(print-send-db)
;(print-receive-db)



;==================================
; SEND/RECEIVE PANEL
;==================================
;opens a dialog enabling activation (opening) 
;of related send/receives patch locations

#|
(defparameter *wrkspc-elems* (pathname 
                              (string+ 
                               (namestring 
                                (mypathname  *current-workspace*)) "elements/")))
|#

(defun get-elem-folder (current-workspace)
  (pathname 
   (string+ 
    (namestring 
     (mypathname current-workspace)) "elements/")))

(defun set-send-dialog (theinput)
  (let* ((path (if (defval theinput) (namestring (relative-pathname (defval theinput) (get-elem-folder *current-workspace*)))))
         (dialog (make-editor-window 'inputEditor theinput (or (frame-name theinput) (name theinput)) nil
                                     :winpos :centered
                                     :winsize (om-make-point 410 #+linux 150 #+(or macosx win32) 135)
                                     :resize t))
         (defvalitem (make-instance 'om-text-edit-view :object theinput))
         (defval-scroller (om-make-dialog-item 'om-text-edit-view ;'defval-editbox
                                               (om-make-point 3 50) (om-make-point 400 35) 
                                               (format () "~A" path) 
                                               :item defvalitem
                                               :scrollbars :v
                                               :font *om-default-font2*
                                               ;:allow-returns t
                                               :help-spec "double click to edit initform <command> to see"
                                               ))
         (theindex (indice theinput)))
    (let ((v (eval (defval theinput))))
      (when (omclass-p (class-of (class-of v)))
        (setf (instance-p defvalitem) t)
        )
      )

    (om-set-bg-color (editor dialog) *azulito*)
    (om-set-bg-color dialog *azulito*)
    
    (om-add-subviews (editor dialog) defval-scroller 
                     (om-make-dialog-item 'om-static-text (om-make-point 3 30) (om-make-point 80 25) 
                                          "Path to send:" 
                                          :bg-color *azulito*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 2) (om-make-point 200 22)
                                          (format () "~D: ~D" theindex (or (frame-name theinput) (name theinput))) 
                                          
                                          :font *om-default-font3b* 
                                          :bg-color *azulito*)
                     
                     (om-make-dialog-item 'om-button (om-make-point 5 90) (om-make-point 80 18) "Open"
                                          :di-action (om-dialog-item-act item
                                                       (if (equal (type-of theinput) 'omreceive) 
                                                           (let* ((key (keyref theinput)) 
                                                                  (thesend (gethash key *send-db*)))
                                                             (if thesend
                                                                 (progn 
                                                                 ;  (when (defval theinput)
                                                                 ;   (load (defval theinput))
                                                                 ;   )
                                                                   (openobjecteditor (mycontainer thesend))
                                                                   )
                                                               (progn 
                                                                 ;(load (defval theinput))
                                                               (om-beep-msg "no send to this receive or not loaded!")
                                                               )
                                                               ))
                                                         
                                                         (let* ((key (keyname theinput)) 
                                                                (receives (gethash key *receive-db*))
                                                                (patches (loop for i in receives
                                                                               collect (mycontainer i))))
                                                           (when patches
                                                             (loop for i in patches
                                                                   for r in receives
                                                                   do (progn
                                                                        (setf (value r) (car (omng-box-value theinput))) 
                                                                        (openobjecteditor i)
                                                                        )
                                                                      )))
                                                         )
                                                       (om-close-window (editor dialog))
                                                       )))
    dialog))

