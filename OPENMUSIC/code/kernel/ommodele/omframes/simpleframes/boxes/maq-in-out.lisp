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
; Inputs/Outputs in the MAquette
;DocFile


(in-package :om)

;;;=====================================
;;; IN / OUT dans la maquette
;;;=====================================

(defclass maqinoutframe () ())

(omg-defclass maqinFrame (maqinoutframe inFrame) ())
(omg-defclass maqoutFrame (maqinoutframe outFrame) ())

(defmethod om-view-click-handler ((self tempobjframe) pos)
  ;(print (car (frames (first (connected? (object (car (inputframes self))))))))
  ;(print (value (object self)))
  ;(setf (extend (object self)) 5000)
  (unselect-eval-func (editor (om-view-container self)))
  (call-next-method))

;;; appele dans give-editor-list-range...
(defmethod tempframe-p ((self maqinoutframe)) t)

(defclass maqinout ()
  ((posy :accessor posy :initarg :posy :initform 0)
   (offset :accessor offset :initarg :offset :initform 0)))

(defmethod extend ((self maqinout)) 1)
(defmethod sizey ((self maqinout)) 1)
(defmethod strech-fact ((self maqinout)) 1)
(defmethod mute ((self maqinout)) t)

(defclass maq-omin (omin maqinout) ())
(defclass maq-omout (omout maqinout) ())
(defmethod get-frame-class ((self maq-omin)) 'maqinframe)
(defmethod get-frame-class ((self maq-omout)) 'maqoutframe)

(defmethod get-input-class ((self ommaquette)) 'maq-omin)
(defmethod get-output-class ((self ommaquette)) 'maq-omout)


(defmethod omNG-save ((self maq-OMIn) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-maq-boxin ,(name self) ,(indice self)  ,(om-save-point (om-make-point (offset self) (posy self))) ,(docu self) 
                  ,(frame-name self) ,(omng-save (eval (defval self)) t) ,(om-save-point (frame-size self))))

(defmethod omNG-save ((self maq-OMout) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-maq-boxout ,(name self) ,(indice self)  ,(om-save-point (om-make-point (offset self) (posy self)))
 ',inputs ,(frame-name self) ,(om-save-point (frame-size self)))))

(defmethod omNG-copy ((self maq-OMIn))
  `(let* ((copy ,(call-next-method)))
     (setf (offset copy) ,(offset self))
     (setf (posy copy) ,(posy self))
     copy))

(defmethod omNG-copy ((self maq-OMOut))
  `(let* ((copy ,(call-next-method)))
     (setf (offset copy) ,(offset self))
     (setf (posy copy) ,(posy self))
     copy))

(defun om-load-maq-boxin (name indice position docu &optional fname val fsize)
  (let* ((pos (om-correct-point position))
        (newbox (make-new-patch-input name indice pos 245 'maq-omin)))
    (setf (docu newbox) docu)
    (setf (offset newbox) (om-point-h pos))
    (setf (posy newbox) (om-point-v pos))
    (when val
      (setf (defval newbox) (put-quote val)))
    (setf (frame-name newbox) fname)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

;;; pour un map-inout position = offset / posy et pas frame-position
(defun om-load-maq-boxout (name indice position inputs &optional fname fsize)
  (let* ((pos (om-correct-point position))
        (newbox (make-new-output name indice pos 245 'maq-omout)))
    (setf (frame-name newbox) fname)
    (setf (offset newbox) (om-point-h pos))
    (setf (posy newbox) (om-point-v pos))
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))


(defmethod add-input ((self maquettepanel) pos)
  (let* ((boxes (get-subframes self))
         (i (length (find-class-boxes boxes 'maqinFrame)))
         (in (make-new-patch-input (mk-unique-name self "input")
                                   i pos 245 'maq-omin))
         (maqpos (get-offset/posy-from-pixel self pos))
         frame)
    (setf (posy in) (om-point-v maqpos))
    (setf (offset in) (om-point-h maqpos))
    (setf frame (make-frame-from-callobj in))
    (omG-add-element self frame)
    ))


(defmethod add-output ((self maquettepanel) pos)
  (let* ((boxes (get-subframes self)) 
         (i (length (find-class-boxes boxes 'maqoutFrame)))
         (out (make-new-output (mk-unique-name self "output")
                               i pos 245 'maq-omout))
         (maqpos (get-offset/posy-from-pixel self pos))
         frame)
    (setf (posy out) (om-point-v maqpos))
    (setf (offset out) (om-point-h maqpos))
    (setf frame (make-frame-from-callobj out))
    (omG-add-element self frame)
    ))


(defmethod add-subview-extra ((self maqinoutframe))
  (call-next-method)
  (om-set-bg-color (nameview self) (om-get-bg-color (om-view-container self))))

(defmethod non-connected ((self maqinoutframe) list) t)
(defmethod update-after-evaluation ((self maqinoutframe)) nil)

(defmethod eval+redraw ((self maqinoutframe)) nil)

(defmethod is-marketed ((self maqinout) typesource marklist) nil)
 
(defmethod init-size&position ((self maqinoutframe) container)
  (om-set-view-position self (get-offset/posy-in-pixel self container)))


(defmethod OMGMoveObject ((self maqinoutframe) new-position)
  (when (< (om-point-h new-position) 0)
    (setf new-position (om-make-point 0 (om-point-v new-position))))
   (let* ((container (om-view-container self)))
     (MoveTempObject self new-position)
     (omng-MoveObject (object self) new-position)))
    
(defmethod MoveTempObject ((self maqinoutframe) new-position)
   (let ((maqpos))
     (mapc #'(lambda (conection)
               (draw-connection conection nil)) (connections self))
     (om-set-view-position self new-position)
     (setf maqpos (get-offset/posy-from-pixel (om-view-container self) new-position))
     (setf (slot-value (object self) 'offset) (om-point-h maqpos))
     (setf (slot-value (object self) 'posy)  (om-point-v maqpos))
     (om-set-view-position self (setf (frame-position (object self)) (get-offset/posy-in-pixel self (om-view-container self))))
     ))

(defmethod get-relative-position ((self maqinoutframe)) 
   (om-make-big-point (slot-value (object self) 'offset) (posy (object self))))


(defmethod omg-remove-element ((self MaquettePanel) (box maqInFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'maqInFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))

(defmethod omg-remove-element ((self MaquettePanel) (box maqoutFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'maqoutFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))


(defmethod draw-in-inout-box ((container maqinframe) object)
  (om-with-focused-view container
    (om-with-font *om-default-font2b*
                  (om-draw-string (- (round (w container) 2) 5)  10 (format () "~D" (indice object))))
    ))

(defmethod draw-in-inout-box ((container maqoutframe) object)
  (om-with-focused-view container
    (om-with-font *om-default-font2b*
                  (om-draw-string (- (round (w container) 2) 5)  42 (format () "~D" (indice object))))
    ))


(defmethod class-of-connection ((self maqoutframe)) 'maq-connection)

(defmethod get-connection-lines ((self maqoutframe) i)
  (let* ((container (om-view-container self))
         (ctrl (nth i (inputframes self)))
         (connection (connected? (object ctrl)))
         (boxsource (first connection))
         possource sizesource
         (x-self (x self))
         (y-self (y self))
         (in-xs (x ctrl))
         x1 y1 xi yi)
     (if (car (frames boxsource))
         (progn
            (setf possource (om-view-position (car (frames boxsource))))
            (setf sizesource (om-view-size (car (frames boxsource))))
            (setq x1 (round (+ (om-point-h possource)  
                                              (- (* (+ (second connection) 1) (/ (om-point-h sizesource) (+ (numouts boxsource) 1))) 2))))
            (setq y1 (- (y+h (car (frames boxsource))) 2))
            (setq xi (+ x-self in-xs 4))
            (setq yi  y-self)
            (normalize-points container (get-line-connection x1 y1 xi yi)))
       nil)
     ))


(defmethod omNG-box-value ((self maq-OMIn) &optional (numout 0))
   (declare (ignore numout))
   (or (value self) (eval (defval self))))

(defmethod clear-ev-once ((self OMMaquette))
  "After one evaluation this methods set the ev-once flag of all boxes in ev-once mode to nil."
  (mapc #'(lambda (box) (clear-ev-once box)) (boxes self)))


;; !!! redefinition
(defmethod eval-maquette-box ((self temporalbox) num-out)
  (cond ((and (equal (allow-lock self) "x") (value self)) (nth num-out (value self)))
        ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth num-out (value self)))
        (t 
         (let* ((args  (mapcar #'(lambda (input)
                                   (if (connected? input)
                                     (let ((rep (omNG-box-value (first (connected? input)) (second (connected? input)))))
                                       (when (car (frames (first (connected? input))))
                                         (update-after-evaluation (car (frames (first (connected? input))))))
                                       rep)
                                     (value input))) (inputs self)))
                rep
                (out-boxes (sort (find-class-boxes (boxes (reference self)) 'maq-OMout)  '< :key 'indice))
                (in-boxes (sort (find-class-boxes (boxes (reference self)) 'maq-OMin)  '< :key 'indice))
                )

           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t))
           
           ; mettre les valeurs des inputs du temporalbox dans les inputs de la maquette 
           (mapcar #'(lambda (input arg)
                       (setf (value input) arg)) in-boxes args)
           
           ;eval la maquette 
           ;en prenant value des inputs 
           (loop for item in (get-boxes-to-value (boxes (reference self))) do
                 (omng-box-value item)
                 )
           (clear-ev-once (reference self))
           
           ;chercher ce qui est connecte a (nth (numout - 1) out-boxes) ou maq obj si numout = 0
           (setf rep (loop for out in out-boxes 
                           collect (if (connected? (car (inputs out)))
                                     (progn
                                       (setf con-obj (car (connected? (car (inputs out)))))
                                       (setf num (cadr (connected? (car (inputs out)))))
                                       ;(print (list (value con-obj) num))
                                       (if (listp (value con-obj))
                                         (nth (+ num 1) (value con-obj))
                                         (value con-obj))
                                       )
                                     (value (car (inputs out))))
                           ))

           ; enlever les valeurs des inputs
           (mapcar #'(lambda (input)
                       (setf (value input) nil)) in-boxes)
           
           (setf (value self) (append (list (cons-maquette-object (reference self) (boxes (reference self)))) rep))
           (when (and (editorFrame self) (EditorView-p (editorframe self)))
             (update-editor-after-eval (editorFrame self) (get-mus-ob self)))
           
           ;(unless (is-marketed self 1 (get-maquette-markers (om-view-container (car (frames self)))))
           ; bug dans maquette inclue ds une autre
           (unless (is-marketed self 1 (get-maquette-markers (mycontainer self)))
             (setf (slot-value self 'strech-fact)  1))
           (when (car (frames self))
             (update-after-evaluation (car (frames self)))
             (make-move-after (om-view-container (car (frames self))) (list (car (frames self))))
             (when (showpict self)
               (update-miniview (car (frames self)) (soft-get-mus-ob self))))
           
           (nth num-out (value self))
           
           )
         )
        ))


;;; +++++++++
(defmethod get-maquette-markers ((self OMMaquette))
   "Return a list of the markers in 'self'."
   (let* (rep)
     (mapc #'(lambda (item)
               (if (marker-p item)
                 (push item rep))) (boxes self))
     rep))

(defmethod non-connected ((self temporalbox) list)
   (let ((rep t))
     (loop for item in list 
           while list do
           (when (is-connected? self item)
             (setf rep nil)
             (setf list nil))) 
     rep))

(defmethod non-connected ((self maqinout) list) t)

(defun get-boxes-to-value (boxlist)
   (let (rep)
     (mapc #'(lambda (box)
               (when (and (not (marker-p box)) (non-connected box boxlist))
                 (push box rep))) boxlist) 
     rep))
;;; +++++++++

;==========================================
; Self input in a maquette 


(defclass! maquette-data () 
  ((maquette :accessor maquette :initform nil)
   (duration :accessor duration :initarg :duration :initform nil)
   (boxes :accessor boxes :initarg :boxes :initform nil)
   (boxvalues :accessor boxvalues :initarg :boxvalues :initform nil)))

(defclass maq-tempin  (selfTempIn) ())
(pushr 'maq-tempin *spec-new-boxes-types*)
(defclass maq-selfInFrame (selfInFrame) ())
(defmethod get-frame-class ((self maq-TempIn)) 'maq-selfInFrame)


(defmethod get-new-box-from-type ((type (eql 'maq-tempin)) position container)
  (let* ((patch (object container))
         (self-boxes (patch-has-temp-in-p patch)))
    (if self-boxes (om-beep-msg "This patch has a temporal input")
        (let* ((newbox (make-maq-temp-input "self" position))
              (boxes (get-subframes container))
              (inputs (find-class-boxes boxes 'InFrame)))
          (loop for item in inputs do
                (setf (indice (object item)) (+ (indice (object item)) 1))
                (om-invalidate-view item t))
          (setf (defval newbox) (make-instance 'maquette-data))
          newbox))))


(defun make-maq-temp-input (name posi)
  (let ((rep (make-instance 'maq-TempIn
                :name name
                :icon 178
                :reference 'maquette-data
                :indice 0)))
     (setf (frame-position rep) posi)
     (setf (defval rep) (make-instance 'maquette-data))
     rep))

(defmethod omNG-copy ((self maq-tempin))
  `(let* ((thein (make-instance ',(class-name (class-of self))
                   :name ,(name self)
                   :icon ,(copy-icon (icon self))
                   :reference 'maquette-data
                   :indice 0)))
          (setf (frame-position thein) ,(om-copy-point (frame-position self)))
          (setf (frame-size thein) ,(om-copy-point (frame-size self)))
          (setf (defval thein) (make-instance 'maquette-data))
     thein))


(defmethod gen-code ((self maq-tempin) numout)
   (if (= numout 0)
     `(maquette ,(in-symbol self))
     (call-next-method)))

(defmethod omNG-box-value ((self maq-tempin) &optional (numout 0))
   (if (= numout 0) 
     (maquette (defval self))
     (rep-editor (defval self) numout)))


(defmethod omNG-save ((self maq-TempIn) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-boxmaqselfin  ,(name self)  ,(om-save-point (frame-position self)) ,(om-save-point (frame-size self))))

(defun om-load-boxmaqselfin (name  position  &optional fsize)
  (let ((newbox (make-maq-temp-input name (om-correct-point position))))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    (setf (defval newbox) (make-instance 'maquette-data))
    newbox))


