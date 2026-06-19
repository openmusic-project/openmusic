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

;Author: Paulo Henrique Raposo

(in-package :om)

;;; ============================================================
;;; omdrag-drop: add click-on-dragged-box slot for cross-pane drops
;;; ============================================================

(defvar *OM-drag&drop-handler* "An object that handles the drag and drop of views at the lisp level.")

(defclass omdrag-drop ()
  ((dragged-view :accessor dragged-view :initform nil)
   (true-dragged-view :accessor true-dragged-view :initform nil)
   (target-view :accessor target-view :initform nil)
   (true-target-view :accessor true-target-view :initform nil)
   (container-view :accessor container-view :initform nil)
   (dragged-list-objs :accessor dragged-list-objs :initform nil)
   (opt-key-p :accessor opt-key-p :initform nil)
   (shift-key-p :accessor shift-key-p :initform nil)
   (initial-mouse-pos :initform nil :accessor initial-mouse-pos)
   ;; Click in box-LOCAL pixels; preserved before om-drag-receive overwrites
   ;; initial-mouse-pos with target-pane coords.
   (click-on-dragged-box :initform nil :accessor click-on-dragged-box)
   (drop-mouse-pos :initform nil :accessor drop-mouse-pos)
   (drag-flavor :initform nil :accessor drag-flavor)))

(defmethod om-drag-start ((view om-view-drag))
  (let* ((theview (get-drag-object view))
         (container (and theview (om-view-container theview))))
    (when (and theview container)
      (if (om-shift-key-p)
          (setf (shift-key-p *OM-drag&drop-handler*) t)
          (setf (shift-key-p *OM-drag&drop-handler*) nil))
      (setf (dragged-view *OM-drag&drop-handler*) theview
            (initial-mouse-pos *OM-drag&drop-handler*) (om-mouse-position theview)
            (click-on-dragged-box *OM-drag&drop-handler*) (om-mouse-position theview)
            (dragged-list-objs *OM-drag&drop-handler*) (get-actives container)
            (container-view *OM-drag&drop-handler*) container
            (true-dragged-view *OM-drag&drop-handler*) view
            (drag-flavor *OM-drag&drop-handler*) :omvw)
      t)))

(defmethod perform-drop-list ((dd t) (dragged t) (target t) pos) nil)

;;; ============================================================
;;; perform-drop: helper + 7 patchPanel targets (visual->logical + ctx)
;;; ============================================================

(defmethod make-func-from-obj ((self patchPanel) obj pos &optional name)
  (let* ((zoom    (oa::om-zoom-of self))
         (log-pos (if (= zoom 1.0) pos (oa::om-zoom-unscale-point pos zoom)))
         (theobj  (object obj)))
    (omG-add-element self
                     (oa::with-zoom-context-of self
                       (make-frame-from-callobj
                        (omNG-make-new-boxcall theobj log-pos
                                               (or name (mk-unique-name self (name theobj)))))))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged lispfun-icon-frame)
                         (target patchPanel) position)
  (let* ((zoom    (oa::om-zoom-of target))
         (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom)))
         (theobj  (object dragged)))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj
                        (omNG-make-new-lispboxcall (funname theobj) log-pos
                                                   (mk-unique-name target (name theobj))))))) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame)
                         (target patchPanel) position)
  (declare (ignore position))
  (let* ((zoom    (oa::om-zoom-of target))
         (vis-pos (om-mouse-position target))
         (log-pos (if (= zoom 1.0) vis-pos (oa::om-zoom-unscale-point vis-pos zoom)))
         (theobj  (object dragged))
         (new-name (name theobj)) newobj)
    (if (om-shift-key-p)
        (setf newobj (omNG-make-new-boxcall-slots theobj log-pos (mk-unique-name target new-name)))
        (setf newobj (omNG-make-new-boxcall theobj log-pos (mk-unique-name target new-name))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj newobj)))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classboxframe)
                         (target patchPanel) position)
  (let* ((zoom    (oa::om-zoom-of target))
         (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom)))
         (theobj  (find-class (reference (object dragged))))
         (new-name (name theobj)) newobj)
    (if (om-shift-key-p)
        (setf newobj (omNG-make-new-boxcall-slots theobj log-pos (mk-unique-name target new-name)))
        (setf newobj (omNG-make-new-boxcall theobj log-pos (mk-unique-name target new-name))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj newobj)))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged slot-icon-frame)
                         (target patchPanel) position)
  (let* ((zoom    (oa::om-zoom-of target))
         (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom)))
         (typeslot (string (thetype (object dragged))))
         (basic? (find-if #'(lambda (x) (string-equal (class-name x) typeslot))
                          *Basic-Lisp-Types*)))
    (unless basic?
      (setf basic? (find-class (thetype (object dragged)))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj
                        (omNG-make-new-boxcall basic? log-pos
                                               (mk-unique-name target (name basic?))))))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged omboxframe)
                         (target patchPanel) position)
  (let* ((zoom    (oa::om-zoom-of target))
         (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom))))
    (omg-remove-element (om-view-container dragged) dragged)
    (let ((new (oa::with-zoom-context-of target
                 (make-frame-from-callobj (object dragged)))))
      (omG-add-element target new)
      (setf (frames (object new)) (list new))
      (let ((oa::*om-zoom-drag-visual-pos* position))
        (om-set-view-position new log-pos))
      (setf (frame-position (object new)) (borne-position log-pos))
      t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged icon-method)
                         (target patchPanel) position)
  (let* ((zoom    (oa::om-zoom-of target))
         (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj
                        (omNG-make-new-boxcall (fdefinition (method-name (object dragged))) log-pos
                                               (mk-unique-name target (name dragged))))))) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged tempobjframe)
                         (target patchPanel) position)
  (let* ((zoom         (oa::om-zoom-of target))
         (new-position (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom))))
    (cond
      ((or (om-maquette-abs-p (reference (object dragged)))
           (abspatch-p (reference (object dragged))))
       (omG-add-element target
                        (oa::with-zoom-context-of target
                          (make-frame-from-callobj
                           (omNG-make-new-boxcall (clone (reference (object dragged)))
                                                  new-position
                                                  (mk-unique-name target (name (reference (object dragged))))))))
       t)
      ((patch-p (reference (object dragged)))
       (omG-add-element target
                        (oa::with-zoom-context-of target
                          (make-frame-from-callobj
                           (omNG-make-new-boxcall (reference (object dragged))
                                                  new-position
                                                  (mk-unique-name target (name (reference (object dragged))))))))
       t)
      ((ominstance-p (reference (object dragged)))
       (let ((newbox (omNG-make-new-boxcall
                      (omNG-make-new-instance (clone (car (value (object dragged))))
                                              (mk-unique-name target (name dragged)))
                      new-position (mk-unique-name target (name dragged)))))
         (setf (edition-params (reference newbox))
               (eval (copy-edition-params (object dragged))))
         (omG-add-element target
                          (oa::with-zoom-context-of target
                            (make-frame-from-callobj newbox)))
         t))
      (t nil))))

;;; ============================================================
;;; perform-drop: 2 classTreePanel targets
;;; ============================================================

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classboxFrame)
                         (target classTreePanel) position)
  (if (protected-p (find-class (reference (object dragged))))
      (om-beep-msg "This class is protected, try with an alias")
      (let* ((zoom    (oa::om-zoom-of target))
             (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom))))
        (omg-remove-element (om-view-container dragged) dragged)
        (omG-add-element target dragged)
        (let ((oa::*om-zoom-drag-visual-pos* position))
          (om-set-view-position dragged log-pos))
        (setf (frame-position (object dragged)) (borne-position log-pos))
        t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged aliasBoxframe)
                         (target classTreePanel) position)
  (if (and (OMBoxClass? (reference (object dragged)))
           (not (protected-p (object dragged))))
      (let* ((zoom    (oa::om-zoom-of target))
             (log-pos (if (= zoom 1.0) position (oa::om-zoom-unscale-point position zoom))))
        (omg-remove-element (om-view-container dragged) dragged)
        (omG-add-element target dragged)
        (let ((oa::*om-zoom-drag-visual-pos* position))
          (om-set-view-position dragged log-pos))
        (setf (frame-position (object dragged)) (borne-position log-pos))
        t)))

;;; ============================================================
;;; perform-drop: 10 MaquettePanel targets (ctx via macro)
;;; ============================================================

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos (name (object dragged))))
         new-frame)
    (setf new-frame (oa::with-zoom-context-of target
                      (make-frame-from-callobj new-call)))
    (omG-add-element target new-frame)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquette-icon-frame)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos (name (object dragged))))
         new-frame)
    (setf new-frame (oa::with-zoom-context-of target
                      (make-frame-from-callobj new-call)))
    (omG-add-element target new-frame)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged omboxframe)
                         (target MaquettePanel) position)
  (if (allowed-in-maq-p (value (object dragged)))
      (let* ((omins (omNG-make-new-instance (clone (value (object dragged))) (name (object dragged))))
             (maqpos (get-offset/posy-from-pixel target position))
             (new-call (omNG-make-tempobj omins maqpos (name (object dragged)))))
        (setf (edition-params new-call)
              (eval (copy-value-params (value (object dragged)) (object dragged))))
        (omG-add-element target
                         (oa::with-zoom-context-of target
                           (make-frame-from-callobj new-call)))
        t)
      (om-beep-msg "I can not put this into the maquette")))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe)
                         (target MaquettePanel) position)
  (when (allowed-in-maq-p (car (value (object dragged))))
    (let* ((omins (omNG-make-new-instance (clone (car (value (object dragged)))) (name (object dragged))))
           (maqpos (get-offset/posy-from-pixel target position))
           (new-call (omNG-make-tempobj omins maqpos (name (object dragged)))))
      (setf (edition-params new-call)
            (eval (copy-value-params (car (value (object dragged))) (object dragged))))
      (omG-add-element target
                       (oa::with-zoom-context-of target
                         (make-frame-from-callobj new-call)))
      t))
  (if (allowed-in-maq-p (value (object dragged)))
      (let* ((maqpos (get-offset/posy-from-pixel target position))
             new-call)
        (if (global-p (reference (object dragged)))
            (setf new-call (omNG-make-tempobj (reference (object dragged)) maqpos (name (object dragged))))
            (setf new-call (omNG-make-tempobj
                            (omNG-make-new-instance (clone (value (object dragged)))
                                                    (name (object dragged)))
                            maqpos (name (object dragged)))))
        (setf (edition-params new-call)
              (eval (copy-value-params (value (object dragged)) (object dragged))))
        (omG-add-element target
                         (oa::with-zoom-context-of target
                           (make-frame-from-callobj new-call)))
        t)
      (unless (allowed-in-maq-p (car (value (object dragged))))
        (om-beep-msg "I can not put this into the maquette"))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos (mk-unique-name target "tempobj")))
         new-frame)
    (when new-call
      (setf new-frame (oa::with-zoom-context-of target
                        (make-frame-from-callobj new-call)))
      (omG-add-element target new-frame) t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos (name (object dragged))))
         new-frame)
    (when new-call
      (setf new-frame (oa::with-zoom-context-of target
                        (make-frame-from-callobj new-call)))
      (omG-add-element target new-frame) t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxframe)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (reference (object dragged)) maqpos
                                      (name (reference (object dragged))))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj new-call)))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteframe)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (reference (object dragged)) maqpos
                                      (name (reference (object dragged))))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj new-call)))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxabsframe)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (eval (omng-copy (reference (object dragged)))) maqpos
                                      (name (reference (object dragged))))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj new-call)))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteabsframe)
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (eval (omng-copy (reference (object dragged)))) maqpos
                                      (name (reference (object dragged))))))
    (omG-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj new-call)))
    t))

;;; ============================================================
;;; perform-drop: 5 methodEditor-input-button targets (drop on zoom bar)
;;; ============================================================

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame)
                         (target methodEditor-input-button) position)
  (declare (ignore position))
  (funcall (action target) dragged))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame)
                         (target methodEditor-input-button) position)
  (declare (ignore position))
  (funcall (action target) dragged))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instBoxframe)
                         (target methodEditor-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (class-of (value (object dragged)))
                             (target-panel target)
                             (value (object dragged))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged boxEditorFrame)
                         (target methodEditor-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (class-of (value (object dragged)))
                             (target-panel target)
                             (value (object dragged))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classBoxframe)
                         (target methodEditor-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (find-class (reference (object dragged)) nil)
                             (target-panel target)))

;;; ============================================================
;;; perform-duplicate-list: OMPatch + OMMethod variants
;;; ============================================================

(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMPatch)
                                   (target-frame t) subframes copies pos0)
  (declare (ignore pos0))
  (unless (subtypep (type-of (container-view D&DHandler)) 'maquettepanel)
    (let* ((zoom      (oa::om-zoom-of target-frame))
           (delta-vis (om-subtract-points (drop-mouse-pos D&DHandler)
                                          (initial-mouse-pos D&DHandler)))
           boxes rep)
      (copy-connections subframes copies)
      (loop for item in (get-actives target-frame) do (omG-unselect item))
      (om-with-delayed-update target-frame
        (mapcar #'(lambda (object)
                    (let* ((old-logical (frame-position object))
                           (old-visual  (if (= zoom 1.0) old-logical
                                            (oa::om-zoom-scale-point old-logical zoom)))
                           (new-visual  (om-add-points old-visual delta-vis))
                           (new-logical (if (= zoom 1.0) new-visual
                                            (oa::om-zoom-unscale-point new-visual zoom))))
                      (setf (frame-position object) (borne-position new-logical))
                      (let ((new-frame (oa::with-zoom-context-of target-frame
                                         (make-frame-from-callobj object))))
                        (push new-frame rep)
                        (omG-add-element target-frame new-frame)
                        (let ((oa::*om-zoom-drag-visual-pos* new-visual))
                          (om-set-view-position new-frame new-logical))
                        (omG-select new-frame))))
                copies)
        (setf boxes (get-elements targetobj))
        (mapc #'(lambda (box) (update-graphic-connections box boxes)) rep))
      (om-invalidate-view target-frame)
      t)))

(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMMethod)
                                   (target-frame t) subframes copies pos0)
  (declare (ignore pos0))
  (let* ((zoom      (oa::om-zoom-of target-frame))
         (delta-vis (om-subtract-points (drop-mouse-pos D&DHandler)
                                        (initial-mouse-pos D&DHandler)))
         boxes rep)
    (copy-connections subframes copies)
    (loop for item in (get-actives target-frame) do (omG-unselect item))
    (mapcar #'(lambda (object)
                (let* ((old-logical (frame-position object))
                       (old-visual  (if (= zoom 1.0) old-logical
                                        (oa::om-zoom-scale-point old-logical zoom)))
                       (new-visual  (om-add-points old-visual delta-vis))
                       (new-logical (if (= zoom 1.0) new-visual
                                        (oa::om-zoom-unscale-point new-visual zoom))))
                  (setf (frame-position object) (borne-position new-logical))
                  (let ((new-frame (oa::with-zoom-context-of target-frame
                                     (make-frame-from-callobj object))))
                    (push new-frame rep)
                    (omG-add-element target-frame new-frame)
                    (let ((oa::*om-zoom-drag-visual-pos* new-visual))
                      (om-set-view-position new-frame new-logical))
                    (omG-select new-frame))))
            copies)
    (setf boxes (get-elements targetobj))
    (mapc #'(lambda (box) (update-graphic-connections box boxes)) rep)
    t))

;;; ============================================================
;;; perform-move/slots/special-move: drag in-place zoom-aware
;;; perform-change-view: cross-pane drop with separate zooms
;;; ============================================================

(defmethod perform-make-slots-view ((D&DHandler omdrag-drop))
  (mapc #'(lambda (oneobject)
            (let* ((zoom        (oa::om-zoom-effective oneobject))
                   (delta-vis   (om-subtract-points (drop-mouse-pos D&DHandler)
                                                    (initial-mouse-pos D&DHandler)))
                   (visual-curr (om-view-position oneobject))
                   (new-visual  (om-add-points visual-curr delta-vis))
                   (new-logical (if (= zoom 1.0) new-visual
                                    (oa::om-zoom-unscale-point new-visual zoom)))
                   (oa::*om-zoom-drag-visual-pos* new-visual))
              (OMGMoveObject oneobject new-logical)))
        (dragged-list-objs D&DHandler))
  t)

(defmethod perform-move-view ((D&DHandler omdrag-drop))
  (mapc #'(lambda (oneobject)
            (let* ((zoom        (oa::om-zoom-effective oneobject))
                   (delta-vis   (om-subtract-points (drop-mouse-pos D&DHandler)
                                                    (initial-mouse-pos D&DHandler)))
                   (visual-curr (om-view-position oneobject))
                   (new-visual  (om-add-points visual-curr delta-vis))
                   (new-logical (if (= zoom 1.0) new-visual
                                    (oa::om-zoom-unscale-point new-visual zoom)))
                   (oa::*om-zoom-drag-visual-pos* new-visual))
              (OMGMoveObject oneobject new-logical)))
        (dragged-list-objs D&DHandler))
  t)

(defmethod perform-special-move-view ((D&DHandler omdrag-drop))
  (mapc #'(lambda (oneobject)
            (let* ((zoom        (oa::om-zoom-effective oneobject))
                   (delta-vis   (om-subtract-points (drop-mouse-pos D&DHandler)
                                                    (initial-mouse-pos D&DHandler)))
                   (visual-curr (get-position oneobject))
                   (new-visual  (om-add-points visual-curr delta-vis))
                   (new-logical (if (= zoom 1.0) new-visual
                                    (oa::om-zoom-unscale-point new-visual zoom)))
                   (oa::*om-zoom-drag-visual-pos* new-visual))
              (OMGMoveObject oneobject new-logical)))
        (dragged-list-objs D&DHandler))
  t)

;; Cross-pane drop: landing position computed in LOGICAL via each pane's
;; own zoom. Does NOT use initial-mouse-pos because om-convert-coordinates
;; is unreliable cross-window (ignores screen offset between windows).
(defmethod perform-change-view ((D&DHandler omdrag-drop))
  (let* ((target-frame (get-drag-object (target-view D&DHandler)))
         (pos0 (get-position (get-drag-object (dragged-view D&DHandler))))
         (list-objs (mapcar #'(lambda (frame) (object frame)) (dragged-list-objs D&DHandler)))
         (connectlist (save-connections target-frame (container-view D&DHandler) list-objs))
         (some-item-used nil)
         (correctmove t)
         (source-zoom (or (and (container-view D&DHandler)
                               (oa::om-zoom-effective (container-view D&DHandler)))
                          1.0))
         (target-zoom (or (and target-frame (oa::om-zoom-effective target-frame)) 1.0))
         (primary-V (get-position (dragged-view D&DHandler)))
         (primary-L (if (= source-zoom 1.0) primary-V
                        (oa::om-zoom-unscale-point primary-V source-zoom)))
         (click-V (click-on-dragged-box D&DHandler))
         (click-L (if (or (null click-V) (= source-zoom 1.0))
                      (or click-V (om-make-point 0 0))
                      (oa::om-zoom-unscale-point click-V source-zoom)))
         (drop-V (drop-mouse-pos D&DHandler))
         (drop-L (if (= target-zoom 1.0) drop-V
                     (oa::om-zoom-unscale-point drop-V target-zoom))))
    (loop for item in (dragged-list-objs D&DHandler)
          while correctmove do
            (setf correctmove (drop-allow-p D&DHandler (object item) (object target-frame))))
    (flet ((frame-target-V (dragged-frame)
             (let* ((this-V (get-position dragged-frame))
                    (this-L (if (= source-zoom 1.0) this-V
                                (oa::om-zoom-unscale-point this-V source-zoom)))
                    (rel-L  (om-subtract-points this-L primary-L))
                    (new-L  (om-add-points (om-subtract-points drop-L click-L) rel-L)))
               (if (= target-zoom 1.0) new-L (oa::om-zoom-scale-point new-L target-zoom)))))
      (if correctmove
          (progn
            (make-delete-before (container-view D&DHandler) (dragged-list-objs D&DHandler) target-frame)
            (if (allow-drag-list (object target-frame))
                (setf some-item-used
                      (perform-drop-list D&DHandler (dragged-list-objs D&DHandler) target-frame
                                         (frame-target-V (car (dragged-list-objs D&DHandler)))))
                (progn
                  (loop for i in list-objs
                        do (when (typep i 'omtextfilebox)
                             (let* ((subframes (list i))
                                    (copies (mapcar #'(lambda (frame) (eval (omNG-copy frame)))
                                                    subframes)))
                               (setf some-item-used
                                     (perform-duplicate-list D&DHandler (object target-frame)
                                                             target-frame subframes copies pos0)))))
                  (mapc #'(lambda (dragged-frame)
                            (setf some-item-used
                                  (perform-drop D&DHandler dragged-frame target-frame
                                                (frame-target-V dragged-frame))))
                        (dragged-list-objs D&DHandler))))
            (remake-draggeds-connections target-frame (container-view D&DHandler) list-objs connectlist)
            (when (patchpanel-p target-frame) (modify-patch target-frame)))
          (om-beep)))
    (om-invalidate-view target-frame)
    some-item-used))

;;; ============================================================
;;; Additional perform-drop methods (patchPanel + typed-input-button)
;;; and perform-duplicate-list for OMMaquette
;;; ============================================================

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame)
                         (target patchPanel) position)
  (make-func-from-obj target dragged position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged genfun-icon-frame)
                         (target patchPanel) position)
  (declare (ignore position))
  (make-func-from-obj target dragged (om-mouse-position target)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame)
                         (target patchPanel) position)
  (make-func-from-obj target dragged position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame)
                         (target patchPanel) position)
  (make-func-from-obj target dragged position (name (object dragged))) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classBoxframe)
                         (target typed-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (find-class (reference (object dragged)) nil)
                             (om-view-container target)))

(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMMaquette)
                                   (target-frame t) subframes copies pos0)
  (declare (ignore pos0))
  (let ((rep ()))
    (copy-connections subframes copies)
    (loop for item in (get-actives target-frame) do (omG-unselect item))
    (let ((symbolic-drop-pos (get-offset/posy-from-pixel target-frame (drop-mouse-pos D&DHandler)))
          (symbolic-clic-start (pixel2point
                                (container-view D&DHandler)
                                (initial-mouse-pos D&DHandler))))
      (om-with-delayed-update target-frame
        (mapcar #'(lambda (object)
                    (let* ((symbolic-initpos (om-make-point (slot-value object 'offset) (posy object)))
                           (symbolic-delta (om-subtract-points symbolic-initpos symbolic-clic-start))
                           (symbolic-new-pos (om-add-points symbolic-drop-pos symbolic-delta)))
                      (setf (slot-value object 'posy)  (om-point-v symbolic-new-pos))
                      (setf (offset object) (om-point-h symbolic-new-pos))
                      (let ((new-frame (oa::with-zoom-context-of target-frame
                                         (make-frame-from-callobj object))))
                        (push new-frame rep)
                        (omG-add-element target-frame new-frame)
                        (omG-select new-frame))))
                copies)
        (let ((boxes (get-elements targetobj)))
          (mapc #'(lambda (box) (update-graphic-connections box boxes)) rep)))
      t)))
