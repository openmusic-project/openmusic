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
;This file implements the meta-object TemporalBox
;Temporal boxes are boxes in a maquette.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


;=================================================================
;Temporal Objects in Maquette and class boxes in the hierchical tree
; can be seen as OMBOXCALL
;=================================================================

;This class exists only to hidde some slots of TemporalBox.
(defclass GraphHideTemporalBox (object-with-persistant-params) 
   ((pictu :initform (make-new-om-pict) :accessor pictu)
    (numouts :initform 0  :accessor numouts)
    ;;; (edition-params :initform (get-default-score-params t) :accessor edition-params)
    (ed-pictu-list :initform nil :accessor ed-pictu-list)
    (showpict :initform nil :accessor showpict)
    (minieditor? :initform nil :accessor minieditor?))
   (:documentation "This is the class of the temporal boxes into a maquette with slots that user do not see."))

;-----------------------------------------------------------------
;TEMPORAL OBJECTS IN MAQUETTE
;-----------------------------------------------------------------


(defclass* TemporalBox (GraphHideTemporalBox OMBoxcall) 
   ((offset :initform 0 :initarg :offset :type number :accessor offset :documentation "offset of the box in its container maquette (ms)")
    (extend :initform 2000 :initarg :extend :type number :accessor extend :documentation "extend (duration) of the box (ms)")
    (colorframe :initform  (eval *def-tempbox-color*) :initarg :colorframe  :accessor colorframe :documentation "the color of the box")
    (value :initform nil :initarg :value :accessor value :documentation "value of the box")
    (posy :initform 0 :initarg :posy :type number :accessor posy :documentation "position of the box in the vertical axis")
    (strech-fact :initform 1 :initarg :strech-fact :type number :accessor strech-fact :documentation "a time-stretch factor")
    (sizey :initform 10  :initarg :sizey :type number :accessor sizey :documentation "vertical extent")
    (free-store :initform 0 :initarg :free-store :accessor free-store :documentation "a freely usable slot")
    (reference :initform nil :initarg :reference :accessor reference :documentation "reference object")
    (inputs :initform nil  :accessor inputs)
    (mute :accessor mute :initform nil)
    (lock :accessor lock :initform nil))
   (:icon 238)
   (:documentation "A box representing an object inside a maquette.

TemporalBoxes can refer eitheir to objects, to patches producing such objects, or to internal maquettes. (This reference corresponds to the slot <reference>.) 

They have accessible graphical and temporal properties (slots <offest>, <extend>, <colorframe>, <posy>, <stretch-fact>, <sizey>). 
<stretch-fact> corresponds to a possible stretching resulting from user interaction in the Maquette editor. Therefore the real duration of the box corresponds to <extend> * <stretch-fact>.

A TemporalBox is supposed to yield a musical result to integrate in a temporal context (a maquette). This result is stored AS A LIST in the slot <value>."))


;;This class is kept for compatibility with old maquettes
(defclass* OMBOXTEMPOBJ (TemporalBox) ()
  (:icon 238))



(defmethod maq-changeparams ((self temporalbox) types vals)
  (mapc #'(lambda (ty va)
            (set-edit-param self ty va))
        (list! types) (list! vals))
  (when (maquette-p (reference self))
    (maq-changeparams (reference self) types vals))
  )


;--------------INITS
(defmethod boxtempobj-p ((self TemporalBox)) t)
(defmethod boxtempobj-p ((self t)) nil)
(defmethod has-mini-pict? ((self TemporalBox)) t)

(defmethod have-persistant-patch? ((box temporalbox))
  (and (reference box) (patch-p (reference box)) (mypathname (reference box))))

(defmethod allow-rename ((self TemporalBox)) t)

(defmethod get-input-class-frame ((self TemporalBox)) 'input-tempobj-frame)
(defmethod get-out-class ((self TemporalBox)) 'outtempobj)
(defmethod get-frame-class ((self TemporalBox)) 'tempobjframe)
(defmethod get-documentation ((self TemporalBox)) (string+ "This is a temporal box, its reference is a " 
                                                              (get-object-insp-name (reference self)) ". Double-click to edit it."))
(defmethod get-object-insp-name ((self TemporalBox)) "Temporal Box")

(defmethod is-marketed ((self TemporalBox) typesource marklist)
   (let (rep markitem)
     (loop for item in marklist
           while (not rep) do
           (setf markitem (cond ((markerframe-p item) (object item))
                                ((marker-p item) item)
                                (t nil)))
           (when (position (list typesource self) (tempojlist markitem) :test 'equal)
             (setf rep markitem))) rep))


(defmethod paste-position ((self TemporalBox) view) 
   (setf (slot-value self 'posy)  (round (* (posy self) 1.2))) nil)


(defmethod do-add-one-input ((self TemporalBox)) (om-beep))
(defmethod do-add-all-inputs ((self TemporalBox)) nil)
(defmethod do-delete-one-input ((self TemporalBox)) (om-beep))

(defmethod do-add-one-keyword ((self TemporalBox) &optional (input-key nil)) (om-beep))
(defmethod do-delete-one-keyword ((self TemporalBox)) (om-beep))


(defmethod make-frame-from-callobj ((self TemporalBox))
  (let* ((numouts (numouts self))
          (numins (length (inputs self)))
          outsname  module)
     (setq module
           (om-make-view (get-frame-class self)
             :position (om-make-point 0 0)
             :help-spec ""
             :size  (om-make-point 0 0)
             :object self))
     (unless (zerop numouts)
       (if (ominstance-p (reference self))
         (setf outsname (list "self"))
         (setf outsname (loop for item in (sort (find-class-boxes (boxes (reference self)) 'OMout) '< :key 'indice) 
                              collect (get-frame-name item))))
       (loop for i from 0 to (- numouts 1) do
             (let ((thenewout (om-make-view (get-out-class self)
                                :position (om-make-point 0 0)
                                :size (om-make-point 8 8)
                                :help-spec (nth i outsname)
                                :index i)))
               (setf (outframes module) (list+ (outframes module) (list thenewout)))
               (om-add-subviews module thenewout))))
     (loop for input in (inputs self)
           for i from 0 to (- numins 1) do
           (let ((newinput (om-make-view (get-input-class-frame self)
                             :object input
                             :help-spec (string+ "<" (string-downcase (name input))
                                                 "> " (doc-string input))
                             :size (om-make-point 8 8)
                             :position (om-make-point 0 0))))
             (setf (inputframes module) (list+ (inputframes module) (list newinput)))
             (om-add-subviews module newinput)))
     (setf (iconview module) (pictu self))
     (setf (name module) (name self))
     (setf (frames self) (list module))
     (when (allow-lock self)
       (add-lock-button module (allow-lock self)))
     (add-box-resize module)
     module))

#|
(defmethod OpenEditorframe ((self TemporalBox)) 
   (cond
    ((ominstance-p (reference self))
     (or (editorframe self)
         (editor (make-editor-window (get-editor-class (get-mus-ob self))
                                     (get-mus-ob self) (name self) self))))
    ((patch-temp-p (reference self))
     (cond
      ((om-option-key-p) 
       (if (Class-has-editor-p (get-mus-ob self))
         (or (editorframe self)
             (editor (make-editor-window (get-editor-class (get-mus-ob self))
                                         (get-mus-ob self) (name self) self)))
         (when (editorframe self) (om-close-window (window (editorframe self))))))
      (t (OpenObjectEditor (reference self)) (editorframe self))))
    (t (OpenObjectEditor (reference self)) (editorframe self))))
|#
;==== Correction affichage de l'editeur dans les maquettes =====

(defmethod set-win-position ((self TemporalBox) newpos) 
  (set-edit-param self 'winpos newpos))

(defmethod set-win-size ((self TemporalBox) newsize)
  (set-edit-param self 'winsize newsize))

(defmethod OpenEditorframe ((self TemporalBox))
  (cond
   ((ominstance-p (get-mus-ob self)) 
    (or (editorframe self)
        (editor (make-editor-window 'InstanceEditor (get-mus-ob self) (name self) self))))
   ((ominstance-p (reference self)) 
    (if (global-p (reference self))
        (openobjecteditor (reference self))
      (if (and (get-mus-ob self) (good-val-p? (get-mus-ob self)) (Class-has-editor-p (get-mus-ob self)))
          (or (editorframe self)
              (editor (make-editor-window (get-editor-class (get-mus-ob self))
                                          (get-mus-ob self) (name self) self
                                          :winpos (get-edit-param self 'winpos)
                                          :winsize (get-edit-param self 'winsize)
                                          ))
              (om-beep))
        )))
   ((maquette-p (reference self))
    (cond
     ;; ???
     ((om-command-key-p) 
      (if (Class-has-editor-p (get-mus-ob self))
        (or (editorframe self)
            (editor (make-editor-window  (get-editor-class (get-mus-ob self))
                                        (get-mus-ob self) (name self) self)))
        (when (editorframe self) (om-close-window (window (editorframe self))))))
     (t (OpenObjectEditor (reference self)) (editorframe self))))
   ((patch-p (reference self)) 
    (cond
     ((om-command-key-p)
      (if (Class-has-editor-p (get-mus-ob self))
        (or (editorframe self)
            (editor (make-editor-window (get-editor-class (get-mus-ob self))
                                        (get-mus-ob self) (name self) self)))
        (when (editorframe self) (om-close-window (window (editorframe self))))))
     (t (OpenObjectEditor (reference self)) (editorframe self))))
   
   (t (OpenObjectEditor (reference self)) (editorframe self))))
  
(defmethod box-has-pict-editors ((self TemporalBox))  t)

;What is a temporal object ?
;For us a temporal object is all object that
;implements these methods.

(defmethod allowed-in-maq-p ((self t)) nil)
 
(defmethod allowed-in-maq-p ((self cons)) nil)
;  (let ((rep t))
;    (loop for obj in self do (when (not (allowed-in-maq-p obj)) (return (setf rep nil))))
;    rep))

(defmethod get-obj-dur ((self list))
  (loop for obj in self maximize (+ (or (offset obj) 0) (get-obj-dur obj))))

(defmethod get-obj-dur ((self t)) 0)

(defmethod allow-strech-p ((self t) (factor number)) t)

(defmethod offset ((self t)) nil)


(defun make-temporal-box (reference &key name offset posy extend sizey strech-fact value free-store)
  (let ((rep (omNG-make-tempobj reference (om-make-point (or offset 0) (or posy 10)) (or name ""))))
    (when sizey (setf (slot-value rep 'sizey) sizey))
    (when extend (setf (slot-value rep 'extend) extend))
    (when strech-fact (setf (slot-value rep 'strech-fact) strech-fact))
    (when value (setf (slot-value rep 'value) value))
    (when free-store (setf (slot-value rep 'free-store) free-store))
    rep))

;-------from Patch-------
(defmethod omNG-make-tempobj ((self OMPatch) posi name)
   (let* ((newobj (make-instance *def-metaclass-box-tempo* 
                   :name name
                   :reference self
                   :extend 2000
                   :posy (om-point-v posi)
                   :offset (om-point-h posi)
                   :icon nil
                   :colorframe *patch-box-color*
                    :sizey 20)))
    (push newobj (attached-objs self))
    (update-from-reference newobj)
    newobj))


;-------from Maquette-------
(defmethod omNG-make-tempobj ((self OMMaquette) posi name)
   (let ((newobj (make-instance *def-metaclass-box-tempo* 
                   :name name
                   :reference self
                   :extend (get-obj-dur self)
                   :posy (om-point-v posi)
                   :offset (om-point-h posi)
                   :icon nil
                   :colorframe *maquette-box-color*
                   :sizey 20)))
     (push newobj (attached-objs self))
     ;; ***
     (update-from-reference newobj)
     newobj))

;-------from BoxInstance-------
(defmethod omNG-make-tempobj ((self OMBoxInstance) posi name)
  (omNG-make-tempobj (clone (reference self)) posi name))

;-------from OMInstance-------
(defmethod omNG-make-tempobj ((self OMInstance) posi name)
   (let* ((instance (instance self))
          (duration 1) obj)
     (when (allowed-in-maq-p instance) 
       (setf duration (get-obj-dur instance)))
     (setf obj (make-instance *def-metaclass-box-tempo*
                 :name name
                 :reference self
                 :extend duration
                 :posy (om-point-v posi)
                 :offset (om-point-h posi)
                 :icon nil
                 :colorframe (if (global-p self) *global-box-color* *obj-box-color*)
                 :sizey 20))
     (setf (value obj) (list instance))
     ; **
     (setf (edition-params obj) (edition-params self))
     ; ***
     (setf (numouts obj) 1)
     
     obj))


;----------from Class---------
(defmethod omNG-make-tempobj ((self OMClass) posi name)
   (let* ((instance (get-super-default-value (class-name self)))
          (reference (omNG-make-new-instance instance name)))
     (omNG-make-tempobj reference posi name)))

;----------from Slot---------
(defmethod omNG-make-tempobj ((self OMSlot) posi name)
  (let* ((instance (get-super-default-value (thetype self)))
        (reference (omNG-make-new-instance instance name)))
    (omNG-make-tempobj reference posi name)))

;----------from Anything---------
(defmethod omNG-make-tempobj ((self t) posi name)
   (omNG-make-tempobj (omNG-make-new-instance self name) posi name))

;----------from Dead---------
(defmethod omNG-make-tempobj ((self (eql 'dead)) posi name)
   (let* ((newobj (omNG-make-tempobj (make-instance 'OMPatch :name name) posi name)))
     (setf newobj (dead-reference newobj))
     newobj))



(defmethod get-maq-mus-obj ((self OMMaquette)) 
  (if (and (eval-func self) (value self))
    (value self)
    (cons-maquette-object self (boxes self))))

(defmethod get-mus-ob ((self t)) nil)
(defmethod get-mus-ob ((self TemporalBox))
   (cond
    ((Maquette-p (reference self)) (get-maq-mus-obj (reference self)))
    (t (nth 0 (value self)))))

(defmethod get-obj-to-draw ((self t)) nil)
(defmethod get-obj-to-draw ((self TemporalBox))
   (cond
    ((Maquette-p (reference self)) (reference self))
    ((patch-p (reference self))
     (let ((box (find-the-box-view-of-patch (reference self))))
       (if box 
         ;(value box) 
         (eval (gen-code box 0))
         (get-mus-ob self))))
    (t (get-mus-ob self))))




;==============Value=================

(defmethod get-tempbox-instance-for-boxvalue ((self TemporalBox)) (car (value self)))


;;; redefinie dans maq-in-out
(defmethod eval-maquette-box ((self temporalbox) num-out)
  (setf (slot-value self 'strech-fact)  1)
  (setf (value self) (list (cons-maquette-object (reference self) (boxes (reference self)))))
  )


(defmethod omNG-box-value ((self TemporalBox) &optional (num-out 0)) 
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                        (om-report-condition c))
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (setf num-out (incf num-out))
     ;(print (list (allow-lock self) (ev-once-p self)))
     (cond
      ((null (reference self)) nil)
      ; ***
      ((ominstance-p (reference self)) (clone 
                                        ;(car (setf (value self) (list 
                                        ;(get-tempbox-instance-for-boxvalue self)
                                        (car (value self))
                                        ))
                                        ;)))
      ; ***
      ((Maquette-p (reference self))
       (eval-maquette-box self num-out))
      ((not (patch-p (reference self))) (get-tempbox-instance-for-boxvalue self)) ; pas la peine ?  ;;(instance (reference self)))
      ((and (equal (allow-lock self) "x") (value self)) (nth num-out (value self)))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth num-out (value self)))
      ((equal (allow-lock self) "o") (reference self))
      (t (let* ((args  (mapcar #'(lambda (input)
                                   (if (connected? input)
                                     (let ((rep (omNG-box-value (first (connected? input)) (second (connected? input)))))
                                       (when (car (frames (first (connected? input))))
                                         (update-after-evaluation (car (frames (first (connected? input))))))
                                       rep)
                                     (value input))) (inputs self)))
                rep)
           ;(unless (= 1 (strech-fact self)) (setf (extend self) (round (* (strech-fact self) (extend self)))))
           ;;;(setf args (concatenate 'list (list self) args))
           (unless (compiled? (reference self))
             (compile-patch (reference self)))

           (when (patch-has-temp-in-p (reference self))
             (setf args (concatenate 'list (list self) args)))
           (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args)))
           (unless (patch-has-temp-out-p (reference self))
             (setf rep (concatenate 'list (list nil) rep)))
           
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t))
           (setf (value self) rep)
           ;;;
           (setf (edition-params self) (default-edition-params (car (value self))))
           ;;;
           (when (and (editorFrame self) (EditorView-p (editorframe self)))
             (if (equal (type-of (object (editorFrame self))) (type-of (get-mus-ob self)))
                 (update-editor-after-eval (editorFrame self) (get-mus-ob self))
               (om-close-window (window (editorFrame self)))))
           (unless (is-marketed self 1 
                                ;(get-maquette-markers (om-view-container (car (frames self))))
                                ; ***
                                (get-maquette-markers (mycontainer self))
                                )
             (setf (slot-value self 'strech-fact)  1))

           (when (car (frames self))
             (update-after-evaluation (car (frames self)))
             (make-move-after (om-view-container (car (frames self))) (list (car (frames self))))
             (when (showpict self)
               (update-miniview (car (frames self)) (get-mus-ob self))))
           (nth num-out rep))))))





(defmethod dead-reference ((self TemporalBox))
  (let* ((new-patch (make-instance 'OMPatchAbs :name (name self) :icon 210))
         (tempobj (omNG-make-tempobj new-patch (om-make-point (offset self) (posy self)) (name self)))
         (comment (omNG-make-new-boxcall 'comment (om-make-point 150 180) "comment"))
         )
    (add-temp-boxes new-patch)
    (setf (reference comment) "The reference of this box has been deleted.")
    (setf (frame-size comment) (om-make-point 200 50))
    (setf (textstyle comment) (om-make-font "arial" 16))
    (omNG-add-element new-patch comment)
    
    (setf (slot-value tempobj 'extend) (extend self))
    (setf (slot-value tempobj 'sizey)  (sizey self))
    
    (setf (show-name tempobj) (show-name self))
    (setf (pictu tempobj) (copy-picture (pictu self)))
       
    
    (when (frames self)
      (let ((container (om-view-container (car (frames self)))))
        (real-make-delete-before container (frames self))
        (omg-remove-element container (car (frames self)))
        
        (omG-add-element container
                         (make-frame-from-callobj tempobj))
        
        ))
    tempobj))

(defmethod get-input-class ((self t)) 'omin)
(defmethod get-output-class ((self t)) 'omout)

;provisoire voir update-from-reference pour patches
(defmethod update-from-reference ((self TemporalBox) &optional (udt? t))
  (declare (ignore udt?))
  (let* ((new-inputs (mapcar #'(lambda (input) 
                                  (make-instance 'input-funbox
                                    :name (get-frame-name input)
                                    :value (eval (defval input))
                                    :box-ref self
                                    :doc-string (docu input))) 
                             (sort (find-class-boxes (boxes (reference self)) (get-input-class (reference self))) '< :key 'indice))))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))
               (setf (value newin) (value oldin))) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs)
     (setf (numouts self) (length (find-class-boxes (boxes (reference self)) (get-output-class (reference self)))))
     (when (frames self)
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self)))
       )))


(defmethod remove-extra ((self OMPatch) (box TemporalBox))
  (let ((refer (reference box)))
    (when (and (pictu box) (thepict (pictu box)))
      (setf (thepict (pictu box)) nil)
      (setf (name (pictu box)) nil))
    (when refer
      (when (and (editorFrame refer) (not (have-persistant-patch? box))) 
        (om-close-window (window (editorFrame refer))))
      (when (patch-p refer)
        (setf (attached-objs refer)
              (remove box (attached-objs refer) :test 'equal))
        (unless (have-persistant-patch? box)
          (mapc #'(lambda (item) (remove-extra refer item)) (boxes refer))
          (when (patch-p refer)   ; ***
            (setf (boxes refer) nil))
          t)))))



;Set slot redefinition
(defmethod (setf reference) (reference (self TemporalBox))
   (let ((temptempbox (omNG-make-tempobj (or reference (make-instance 'OMTemporalPatch :name "tempobj")) (om-make-point 0 0) "ayay")))
     (setf (slot-value self 'reference) (reference temptempbox))
     (reference self)))


;Set slot redefinition
(defmethod (setf offset) (offset (self TemporalBox))
    (when (and (listp offset) offset (integerp (first offset)) (integerp (second offset)) (integerp (third offset)))
       (setf offset (read-info-metric? self offset)))
   (cond
    ((and (integerp offset) (not (minusp offset)))
     (if (car (frames self))
       (let* ((container (om-view-container (car (frames self))))
              (sys-etat (get-system-etat container))
              (new-pos (point2pixel container (om-make-big-point offset (posy self)) sys-etat)))
         (OMGMoveObject (car (frames self)) new-pos)))
     (setf (slot-value self 'offset) offset))
    (t (om-beep-msg "Incorrect offset value"))))


(defmethod (setf strech-fact) (strech-fact (self TemporalBox))
   (cond
    ((and (numberp strech-fact) (not (minusp strech-fact)))
     (if (car (frames self))
       (let* ((view (car (frames self)))
              (win (om-view-window view))
              (container (panel (om-view-container view)))
              (mark-list (get-maquette-markers container))
                (new-size  (om-make-point (norme2pixel container 'x (round (* strech-fact (extend self)))) (h view))))
         (unless (is-marketed (object view) 1 mark-list)
           (om-set-view-size view new-size)
           ;(draw-mark-lines (om-view-container view) nil)
           (update-after-mark (om-view-container view))
           ;(draw-mark-lines (om-view-container view))
           (when (showpict (object view))
             (update-miniview view (soft-get-mus-ob (object view))))
           (when (patch-temp-p (reference (object view)))
             (setf (mode view) 'changed))
           (make-move-after (om-view-container view) (list view))
           (setf (slot-value self 'strech-fact) strech-fact)
           (om-invalidate-view view t))))
     (setf (slot-value self 'strech-fact) strech-fact))
    (t (om-beep-msg "Incorrect stretch-fact value"))))

(defmethod (setf extend) (extend (self TemporalBox))
   (cond
    ((and (integerp extend) (not (minusp extend)))
     (if (car (frames self))
       (let* ((view (car (frames self)))
              (container (panel view))
              (mark-list (get-maquette-markers container))
              (new-size  (om-make-point (norme2pixel container 'x extend) (h view))))
         (unless (is-marketed (object view) 1 mark-list)
           (om-set-view-size view new-size)
           ;(draw-mark-lines (om-view-container view) nil)
           (update-after-mark (om-view-container view))
           ;(draw-mark-lines (om-view-container view))
           (when (showpict (object view))
             (update-miniview view (soft-get-mus-ob (object view))))
           (when (patch-temp-p (reference (object view)))
             (setf (mode view) 'changed))
           (make-move-after (om-view-container view) (list view))
           (om-invalidate-view view t))))
     (setf (slot-value self 'extend) extend))
    (t (om-beep-msg "Incorrect extend value"))))

(defmethod (setf posy) (posy (self TemporalBox))
   (cond
    ((integerp posy)
     (if (car (frames self))
       (let* ((container (om-view-container (car (frames self))))
              (sys-etat (get-system-etat container))
              (new-pos (point2pixel container  (om-make-big-point (slot-value self 'offset) posy) sys-etat)))
         (OMGMoveObject (car (frames self)) new-pos)))
     (setf (slot-value self 'posy) posy))
    (t (om-beep-msg "Incorrect posy value"))))

(defmethod (setf sizey) (sizey (self TemporalBox))
   (cond
    ((integerp sizey) 
     (if (car (frames self))
       (let* ((view (car (frames self)))
              (container (panel view))
              (new-size  (om-make-point (w view) (norme2pixel container 'y sizey))))
         (om-set-view-size view new-size)
         ;(draw-mark-lines (om-view-container view) nil)
         (update-after-mark (om-view-container view))
         ;(draw-mark-lines (om-view-container view))
         (when (showpict (object view))
           (update-miniview view (soft-get-mus-ob (object view))))
         (make-move-after (om-view-container view) (list view))
         (om-invalidate-view view t)))
     (setf (slot-value self 'sizey) sizey))
    (t (om-beep-msg "Incorrect sizey value"))))


;recursif for maq in maq
; not used
;(defmethod set-port-to-box ((self temporalbox) port)
;   (rplacd (assoc 'outport (edition-params self)) port))
;(defmethod set-port-to-box ((self t) port) t)


(defmethod soft-get-mus-ob ((self t)) nil)
(defmethod soft-get-mus-ob ((self TemporalBox))
   (cond
    ((Maquette-p (reference self)) (reference self))
    (t (nth 0 (value self)))))













   












