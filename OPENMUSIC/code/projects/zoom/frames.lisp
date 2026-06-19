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
;;; omboxframe: centre-icon (iconview + nameview font) zoom-aware
;;; ============================================================

(defmethod centre-icon ((self omboxframe))
  (let* ((zoom     (oa::om-zoom-effective self))
         (off      (oa::om-zoom-scale-int 10 zoom))
         (iconview (iconview self))
         (nameview (nameView self))
         (obj      (object self)))
    (when iconview
      (let* ((logical-iconsize (and obj (icon-sizes (icon obj) (def-icon-size obj))))
             (iv-w (if logical-iconsize
                       (max 1 (round (* (first logical-iconsize) zoom)))
                       (om-point-h (om-view-size iconview))))
             (iv-h (if logical-iconsize
                       (max 1 (round (* (second logical-iconsize) zoom)))
                       (om-point-v (om-view-size iconview)))))
        (when logical-iconsize
          (om-set-view-size iconview (om-make-point iv-w iv-h)))
        (om-set-view-position iconview
                              (om-make-point (- (round (w self) 2) (round iv-w 2))
                                             off))))
    (when nameview
      (oa::om-zoom-capture-logical-font nameview)
      (let* ((logical-font (or (oa::om-zoom-logical-font nameview) *ombox-font*))
             (scaled-font  (if (= zoom 1.0)
                               logical-font
                               (oa::om-zoom-scale-font logical-font zoom)))
             (name (and obj (or (frame-name obj) (get-frame-name obj))))
             (name-w (if name (round (get-name-size name scaled-font)) 0))
             (name-h (if name (+ 3 (max 1 (om-string-h scaled-font))) 0)))
        (om-set-font nameview scaled-font)
        (when (and (> name-w 0) (> name-h 0))
          (om-set-view-size nameview (om-make-point name-w name-h)))
        (when iconview
          (let ((icon-bottom (+ (om-point-v (om-view-position iconview))
                                (om-point-v (om-view-size iconview)))))
            (om-set-view-position nameview
                                  (om-make-point (- (round (w self) 2)
                                                    (round (om-point-h (om-view-size nameview)) 2))
                                                 (- icon-bottom 1)))))))))

;;; ============================================================
;;; omboxframe: make-lock-button + ev-onceboxframe: add-lock-button
;;; ============================================================

(defmethod make-lock-button ((self omboxframe) mode)
  (let* ((zoom (or oa::*make-frame-zoom-context* (oa::om-zoom-effective self) 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (size-v (if scale-p (oa::om-zoom-scale-int-min 10 1 zoom) 10)))
    (om-make-view 'lock-button
                  :IconID (get-icon-lock mode)
                  :size (om-make-point size-v size-v)
                  :position (om-make-point 0 0)
                  :action #'(lambda (item)
                              (let* ((modes (allowed-lock-modes (object self)))
                                     (mpos (position (mode item) modes :test 'string-equal))
                                     (newmode (when mpos (nth (mod (1+ mpos) (length modes)) modes))))
                                (setf (mode item) newmode (iconID item) (get-icon-lock newmode))
                                (setf (allow-lock (object self)) newmode)
                                (om-invalidate-view self))))))

(defmethod add-lock-button ((self ev-onceboxframe) &optional (mode "&"))
  "Allow only lock button in mode ev-once."
  (declare (ignore icon))
  (when (allow-lock-button (object self))
    (let* ((zoom    (or oa::*make-frame-zoom-context* (oa::om-zoom-effective self) 1.0))
           (scale-p (and (numberp zoom) (/= zoom 1.0)))
           (size-v  (if scale-p (oa::om-zoom-scale-int-min 10 1 zoom) 10))
           (off-y   (if scale-p (oa::om-zoom-scale-int 8 zoom) 8)))
      (setf (lock-button self) (om-make-view 'lock-button
                                             :IconID (get-icon-lock mode)
                                             :size (om-make-point size-v size-v)
                                             :position (om-make-point (x (iconview self)) off-y)
                                             :owner (iconview self)
                                             :action #'(lambda (item)
                                                         (declare (ignore item)) (om-beep)))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) "&")))

;;; ============================================================
;;; boxTypeFrame: centre-icon + draw-after-box zoom-aware
;;; ============================================================

(defmethod centre-icon ((self boxTypeFrame))
  (let* ((zoom (oa::om-zoom-effective self))
         (mx   (oa::om-zoom-scale-int 2 zoom))
         (my   (oa::om-zoom-scale-int 11 zoom))
         (iv   (iconview self)))
    (when iv
      (om-set-view-size iv (om-make-point (- (w self) mx) (- (h self) my)))
      (let ((scaled (if (= zoom 1.0)
                        *ombox-font*
                        (oa::om-zoom-scale-font *ombox-font* zoom))))
        (handler-bind ((error
                        #'(lambda (c)
                            (oa::om-zoom-log-error :centre-icon-boxtype-font c))))
          (om-set-font iv scaled))))))

(defmethod draw-after-box ((self boxtypeframe))
  (let* ((zoom   (oa::om-zoom-effective self))
         (deltay (round (* (if (zerop (numouts (object self))) 1 9) zoom)))
         (off1   (oa::om-zoom-scale-int 1 zoom))
         (pen    (oa::om-zoom-scale-int-min 1 1 zoom)))
    (om-with-focused-view self
      (om-draw-rect 0 0 (- (w self) off1) (- (h self) off1 deltay) :pensize pen))
    t))

;;; ============================================================
;;; aliasBoxframe: draw-after-box zoom-aware
;;; ============================================================

(defmethod draw-after-box ((self aliasBoxframe))
  (let* ((zoom (oa::om-zoom-effective self))
         (view (iconView self))
         (off2 (oa::om-zoom-scale-int 2 zoom))
         (off4 (oa::om-zoom-scale-int 4 zoom))
         (pen  (max 1 (* 1.5 zoom))))
    (om-with-focused-view self
      (om-with-dashline
        (om-with-line-size pen
          (om-with-fg-color view *om-gray-color*
            (om-draw-rect off2 off2 (- (w self) off4) (- (h self) off4))))))))

;;; ============================================================
;;; instBoxframe: draw-before-box zoom-aware
;;; ============================================================

(defmethod draw-before-box ((self instBoxframe))
  (call-next-method)
  (let* ((zoom (oa::om-zoom-effective self))
         (off  (oa::om-zoom-scale-int 11 zoom)))
    (om-draw-picture self (if (mypathname (reference (object self)))
                              *glass-pict-per* *glass-pict*)
                     :size (om-subtract-points (om-view-size self) (om-make-point 0 off)))))

;;; ============================================================
;;; commentboxframe: centre-icon (font scaling) zoom-aware
;;; ============================================================

(defmethod centre-icon ((self commentboxframe))
  (let* ((zoom (oa::om-zoom-effective self))
         (m    (oa::om-zoom-scale-int 6 zoom))
         (iv   (iconview self))
         (obj  (object self)))
    (when iv
      (om-set-view-size iv (om-subtract-points (om-view-size self) (om-make-point m m)))
      (when (and obj (slot-exists-p obj 'textstyle) (textstyle obj))
        (let ((scaled (if (= zoom 1.0)
                          (textstyle obj)
                          (oa::om-zoom-scale-font (textstyle obj) zoom))))
          (handler-bind ((error
                          #'(lambda (c)
                              (oa::om-zoom-log-error :centre-icon-commentbox-font c))))
            (om-set-font iv scaled)))))))

;;; ============================================================
;;; boxEditorFrame: centre-icon + make-drag-region zoom-aware
;;; ============================================================

(defmethod centre-icon ((self boxEditorFrame))
  (let* ((zoom (oa::om-zoom-effective self))
         (mini? (minieditor? (object self)))
         (mx    (if mini? (oa::om-zoom-scale-int 4 zoom) 0))
         (my    (if mini? (oa::om-zoom-scale-int 21 zoom) (oa::om-zoom-scale-int 17 zoom)))
         (pos-x (if mini? (oa::om-zoom-scale-int 2 zoom) 0))
         (pos-y (oa::om-zoom-scale-int 8 zoom))
         (iv    (iconview self)))
    (when iv
      (om-set-view-size iv (om-subtract-points (om-view-size self) (om-make-point mx my)))
      (om-set-view-position iv (om-make-point pos-x pos-y)))))

(defmethod make-drag-region ((self boxEditorFrame) region x0 y0 view)
  (declare (ignore view))
  (let* ((zoom (oa::om-zoom-effective self))
         (off  (oa::om-zoom-scale-int 16 zoom))
         (x    (- (x self) x0))
         (y    (- (y self) y0)))
    (om-set-rect-region region x y (+ x (w self)) (- (+ y (h self)) off)))
  region)

;;; ============================================================
;;; input-funboxframe: mouse enter/leave handlers (tooltip + mag-in-out)
;;; ============================================================

(defmethod om-view-mouse-enter-handler ((self input-funboxframe))
  (let ((zoom (oa::om-zoom-effective self)))
    (when *mag-in-out* (setf (iconid self) 1550))
    (unless (or (and (connected? (object self)) (not (keyword-input-p (object self))))
                (not *show-input-vals*))
      (om-without-interrupts
        (when (and self (om-view-container self))
          (let* ((box       (om-view-container self))
                 (thetext   (format () "~S" (value (object self))))
                 (panel     (om-view-container box))
                 (container (editor panel))
                 (font      (if (= zoom 1.0) *om-default-font1*
                                (oa::om-zoom-scale-font *om-default-font1* zoom)))
                 (raw-w     (get-name-size thetext font))
                 (tt-w      (if (and (numberp raw-w) (>= raw-w 20))
                                raw-w
                                (max 20 (round (* (+ 2 (length thetext)) 8 zoom)))))
                 (tt-h      (oa::om-zoom-scale-int 20 zoom))
                 (box-pos   (om-view-position box))
                 (input-x   (x self))
                 (input-y   (y self))
                 (tt-x      (- (+ (om-point-x box-pos) input-x) 4))
                 (tt-y      (- (+ (om-point-y box-pos) input-y) tt-h 2)))
            (when (text-view container)
              (exit-from-dialog (text-view container) (om-dialog-item-text (text-view container))))
            (let ((prev-lock (oa::locked panel)))
              (setf (oa::locked panel) t)
              (unwind-protect
                  (let ((tv (om-make-dialog-item 'text-enter-view
                                                 (om-make-point tt-x tt-y)
                                                 (om-make-point tt-w tt-h)
                                                 thetext
                                                 :container panel
                                                 :font font)))
                    (om-set-font tv font)
                    (setf (text-view container) tv))
                (setf (oa::locked panel) prev-lock)))))))
    (when *mag-in-out*
      (let* ((big  (oa::om-zoom-scale-int 12 zoom))
             (off2 (oa::om-zoom-scale-int 2 zoom))
             (off5 (oa::om-zoom-scale-int 5 zoom))
             (pos  (om-view-position self))
             (ypos (om-point-y pos))
             (xpos (om-point-x pos)))
        (om-set-view-size self (om-make-point big big))
        (om-set-view-position self (om-make-point (- xpos off2) (- ypos off5)))))
    #+win32(oa::om-redraw-pinboard-object self)))

(defmethod om-view-mouse-leave-handler ((self input-funboxframe))
  (when *mag-in-out*
    (let* ((zoom  (oa::om-zoom-effective self))
           (small (oa::om-zoom-scale-int 8 zoom))
           (off2  (oa::om-zoom-scale-int 2 zoom))
           (off1  (oa::om-zoom-scale-int 1 zoom)))
      (setf (iconid self) (frame-icon-input self))
      (om-set-view-size self (om-make-point small small))
      (om-set-view-position self (om-make-point (+ (om-point-x (om-view-position self)) off2)
                                                off1))))
  (om-without-interrupts
    (let* ((container (editor (om-view-container (om-view-container self)))))
      (when (and container (text-view container) (equal (class-name (class-of (text-view container))) 'text-enter-view))
        (let* ((p         (panel container))
               (prev-lock (and p (oa::locked p))))
          (when p (setf (oa::locked p) t))
          (unwind-protect
              (progn
                (om-remove-subviews p (text-view container))
                (setf (text-view container) nil))
            (when p (setf (oa::locked p) prev-lock)))))))
  #+win32(oa::om-redraw-pinboard-object self))

;;; ============================================================
;;; inlet.lisp: input-text-enter-view + click handler zoom-aware
;;; ============================================================

(defclass input-text-enter-view (edit-text-enter-view) ())

(defmethod om-view-click-handler ((self input-funboxframe) where)
  (declare (ignore where))
  (let* ((panel (om-view-container (om-view-container self)))
         (container (editor panel)))
    (cond
      ((and (om-command-key-p) (om-option-key-p))
       (when *target-out*
         (setf *target-in* self)
         (when (equal (om-view-container (om-view-container *target-out*)) panel)
           (connect-box *target-out* *target-in*)
           (setf *target-in* nil))))
      ((om-command-key-p)
       (when (connected? (object self))
         (disconnect-box (om-view-container self) self)))
      ((and (om-shift-key-p) (keyword-input-p (object self)) (val-menu (object self)))
       (popup-keyword-val-menu (object self) panel))
      ((and (om-shift-key-p) (not (maquette-p (object container))))
       ;; ZOOM-COORD: unscale visual click point so the new OMBox stores logical coords.
       (let* ((zoom    (if (typep panel 'oa::om-scroller) (oa::om-zoom-of panel) 1.0))
              (vis-pos (om-make-point (+ (x (om-view-container self)) (x self))
                                      (- (y (om-view-container self)) 30)))
              (pos     (if (= zoom 1.0) vis-pos (oa::om-zoom-unscale-point vis-pos zoom)))
              (new-obj (omNG-make-new-boxcall (car *Basic-Lisp-Types*)
                                              pos
                                              (mk-unique-name panel "aux")))
              (target  (om-view-container (om-view-container self)))
              new-frame)
         (setf (value new-obj) (get-input-value (object self)))
         (setf (thestring new-obj) (format () "~S" (value new-obj)))
         (setf (frame-size new-obj) (om-make-point (get-name-size (thestring new-obj)) 28))
         ;; ZOOM-CTX: propagate target zoom so the new frame inherits scale.
         (setf new-frame (oa::with-zoom-context-of target
                           (make-frame-from-callobj new-obj)))
         (omG-add-element target new-frame)
         (connect-box (car (outframes new-frame)) self)
         (open-ttybox (iconview new-frame))
         #+linux (oa::om-redisplay-element target)))
      ((menu-input-p (object self))
       (popup-input-menu (object self) panel))
      ((and (keyword-input-p (object self)) (keyword-menu (object self)))
       (popup-keyword-input-menu (object self) panel)
       (om-view-set-help self (string+ "<" (string-downcase (name (object self))) "> ")))
      (t (unless (and (connected? (object self)) (not (keyword-input-p (object self))))
           (when (text-view container)
             (om-remove-subviews panel (text-view container))
             (setf (text-view container) nil))
           ;; ZOOM-SCALE: font + offset + height scale with panel zoom.
           (let* ((thetext (format () "~S" (value (object self))))
                  (zoom    (if (typep panel 'oa::om-scroller) (oa::om-zoom-of panel) 1.0))
                  (font    (if (= zoom 1.0) *om-default-font1*
                               (oa::om-zoom-scale-font *om-default-font1* zoom)))
                  (off-y   (round (* -26 zoom)))
                  (tth     (round (* 16  zoom))))
             (setf (text-view container)
                   (om-make-dialog-item 'input-text-enter-view
                                        (om-add-points (om-view-position (om-view-container self))
                                                       (om-make-point (x self) off-y))
                                        (om-make-point (get-name-size thetext font) tth)
                                        thetext
                                        :allow-returns nil
                                        :di-selected-p t
                                        :focus t
                                        :object self
                                        :container panel
                                        :font font))))))))

;;; ============================================================
;;; omboxframe geometry: position + size under zoom
;;; ============================================================

(defmethod om-set-view-position ((self omboxframe) pos)
  (let* ((zoom (oa::om-zoom-effective self))
         (zoom (if (numberp zoom) zoom 1.0))
         (capi-pos (or oa::*om-zoom-drag-visual-pos*
                       (if (= zoom 1.0) pos (oa::om-zoom-scale-point pos zoom)))))
    (when (object self)
      (setf (frame-position (object self)) pos))
    (call-next-method self capi-pos)))

(defmethod om-set-view-size ((self omboxframe) size)
  (let* ((zoom (oa::om-zoom-effective self))
         (zoom (if (numberp zoom) zoom 1.0))
         (visual (if (= zoom 1.0) size (oa::om-zoom-scale-point size zoom)))
         (w (om-point-x visual))
         (h (om-point-y visual))
         (outs (copy-list (outframes self)))
         (ins  (copy-list (inputframes self)))
         (outputs (sort outs '< :key 'index))
         (numouts (length outputs))
         (numins  (length ins))
         (io-size    (oa::om-zoom-scale-int-min 8 1 zoom))
         (io-offset  (oa::om-zoom-scale-int 4 zoom))
         (top-off    (oa::om-zoom-scale-int-min 1 0 zoom))
         (bottom-off (oa::om-zoom-scale-int 9 zoom))
         (lock-size  (oa::om-zoom-scale-int-min 10 1 zoom))
         (resize-marg (oa::om-zoom-scale-int 10 zoom))
         (i 0))
    (loop for out in outputs do
          (setf i (+ i 1))
          (om-set-view-position out (om-make-point (- (* i (round w (+ numouts 1))) io-offset)
                                                   (- h bottom-off)))
          (om-set-view-size out (om-make-point io-size io-size)))
    (setf i 0)
    (loop for in in ins do
          (setf i (+ i 1))
          (om-set-view-position in (om-make-point (- (* i (round w (+ numins 1))) io-offset)
                                                  top-off))
          (om-set-view-size in (om-make-point io-size io-size)))
    (when (and (object self) (not oa::*om-zoom-internal-resize*))
      (setf (frame-size (object self)) size))
    (when (lock-button self)
      (om-set-view-size (lock-button self) (om-make-point lock-size lock-size)))
    (when (resize-box self)
      (om-set-view-position (resize-box self)
                            (om-make-point (- w resize-marg) (- h resize-marg)))
      (om-set-view-size (resize-box self) (om-make-point lock-size lock-size)))
    (call-next-method self visual))
  (centre-icon self))

;;; ============================================================
;;; omboxframe + tempobjframe: zoom protocol specializations
;;; ============================================================

(defmethod oa::om-zoom-redraw-connections ((frame omboxframe))
  (redraw-connections frame))

(defmethod oa::om-zoom-relayout-frame ((frame omboxframe))
  (oa::om-zoom-capture-logical-geom frame)
  (let* ((obj (object frame))
         (logical-pos  (or (and obj (frame-position obj))
                           (oa::om-zoom-logical-pos frame)))
         (logical-size (or (and obj (frame-size obj))
                           (oa::om-zoom-logical-size frame))))
    (when logical-pos
      (om-set-view-position frame logical-pos))
    (if logical-size
        (let ((oa::*om-zoom-internal-resize* t))
          (om-set-view-size frame logical-size))
        (centre-icon frame))
    (let ((iv (iconview frame)))
      (when (and iv (typep iv 'general-miniview) obj
                 (and (slot-exists-p iv 'minipict)
                      (handler-case (minipict iv)
                        (error (c) (oa::om-zoom-log-error :relayout-minipict c) nil))))
        (handler-case (update-miniview iv (value obj))
          (error (c) (oa::om-zoom-log-error :relayout-refresh c)))))))

(defmethod oa::om-zoom-relayout-frame ((self tempobjframe))
  ;; Drop cached miniview after inherited layout so it rebuilds at new zoom.
  (call-next-method)
  (when (and (slot-exists-p self 'minipict) (slot-boundp self 'minipict) (minipict self))
    (handler-bind ((error
                    #'(lambda (c)
                        (oa::om-zoom-log-error :tempobj-minipict c))))
      (om-kill-picture (minipict self))
      (setf (minipict self) nil))))

;;; ============================================================
;;; patchPanel: input-model wiring + touch/keyboard dispatch
;;; ============================================================

(defmethod initialize-instance :after ((self patchPanel) &rest initargs)
  (declare (ignore initargs))
  (let ((existing (capi:output-pane-input-model self)))
    (setf (capi:output-pane-input-model self)
          (append (oa::om-zoom-input-entries) existing))))

(defmethod oa::om-zoom-touch-update ((pane patchPanel) scale anchor-x anchor-y)
  (let ((new-zoom (* (oa::om-zoom-of pane) scale)))
    (oa::om-zoom-update pane new-zoom anchor-x anchor-y)))

(defmethod oa::om-zoom-resolve-touch-target ((pane patchPanel) x y)
  (values pane x y))

(defmethod handle-key-event :around ((self patchPanel) char)
  (cond
    ((and (om-command-key-p) (oa::om-zoom-applies-p self)
          (or (equal char #\=) (equal char #\+)))
     (oa::om-zoom-step-anchored self (* (oa::om-zoom-of self) 1.1))
     t)
    ((and (om-command-key-p) (oa::om-zoom-applies-p self) (equal char #\-))
     (oa::om-zoom-step-anchored self (/ (oa::om-zoom-of self) 1.1))
     t)
    ((and (om-command-key-p) (oa::om-zoom-applies-p self) (equal char #\0))
     (oa::om-zoom-reset-and-recenter self)
     t)
    (t
     (let ((oa::*om-zoom-unscale-mouse-pos-p* t))
       (call-next-method)))))

(defmethod om-view-doubleclick-handler :around ((self patchPanel) where)
  (declare (ignore where))
  (let ((oa::*om-zoom-unscale-mouse-pos-p* t))
    (call-next-method)))

;;; ============================================================
;;; Editor lifecycle: restore saved zoom on open
;;; ============================================================

(defmethod OpenEditorframe :around ((self OMPatch))
  (let ((result (call-next-method)))
    (when (and result (oa::om-zoom-applies-p result))
      (let ((zoom (oa::get-win-zoom self)))
        (when (and (numberp zoom) (/= zoom 1.0))
          (let ((oa::*om-zoom-restoring-p* t))
            (handler-bind ((error
                            #'(lambda (c)
                                (oa::om-zoom-log-error :restore-ompatch c))))
              (oa::om-zoom-update result zoom 0 0))))))
    result))

(defmethod OpenEditorframe :around ((self OMPatchAbs))
  (let ((result (call-next-method)))
    (when (and result (oa::om-zoom-applies-p result))
      (let ((zoom (oa::get-win-zoom self)))
        (when (and (numberp zoom) (/= zoom 1.0))
          (let ((oa::*om-zoom-restoring-p* t))
            (handler-bind ((error
                            #'(lambda (c)
                                (oa::om-zoom-log-error :restore-ompatchabs c))))
              (oa::om-zoom-update result zoom 0 0))))))
    result))

;;; ============================================================
;;; Editor bar: classes + build + lifecycle
;;; ============================================================

(defclass om-patch-editor-bar (om-view) ())

(defclass generic-function-zoom-bar (om-patch-editor-bar om-view-drop) ())

(defclass om-zoom-toolbar-view (om-view) ())

(defparameter +om-zoom-bar-h+ 26)

(defun om-zoom-editor-applies-p (editor)
  "T iff EDITOR should host a zoom bar (excludes maquette editors)."
  (and editor
       (not (and (slot-exists-p editor 'object)
                 (slot-boundp editor 'object)
                 (handler-case (maquette-p (object editor))
                   (error (c) (oa::om-zoom-log-error :editor-applies-p c) nil))))))

(defmethod panel-position :around ((self patchEditor))
  (if (om-zoom-editor-applies-p self)
      (om-make-point 0 +om-zoom-bar-h+)
      (call-next-method)))

(defmethod panel-size :around ((self patchEditor))
  (if (om-zoom-editor-applies-p self)
      (om-make-point (w self) (- (h self) +om-zoom-bar-h+))
      (call-next-method)))

(defgeneric om-zoom-editor-bar-build (editor)
  (:documentation "Build and install the zoom bar on EDITOR; return the bar view."))

(defmethod om-zoom-editor-bar-build ((editor patchEditor))
  (let* ((panel (panel editor))
         (zoom  (if (typep panel 'oa::om-scroller) (oa::om-zoom-of panel) 1.0))
         (pct   (round (* zoom 100)))
         (bar (om-make-view 'om-patch-editor-bar
                            :position (om-make-point 0 0)
                            :size (om-make-point (w editor) +om-zoom-bar-h+)
                            :bg-color *controls-color*))
         (out-enabled (add-output-enabled panel 'out))
         (in-enabled  (add-input-enabled  panel 'in))
         (out-btn (om-make-view
                   'om-icon-button
                   :position (om-make-point 5 1)
                   :size (om-make-point 24 24)
                   :icon1 (if out-enabled "out" "out-disable")
                   :icon2 (if out-enabled "out-pushed" nil)
                   :action (when out-enabled
                             #'(lambda (item)
                                 (declare (ignore item))
                                 (modify-patch panel)
                                 (add-output panel nil)))))
         (in-btn (om-make-view
                  'om-icon-button
                  :position (om-make-point 30 1)
                  :size (om-make-point 24 24)
                  :icon1 (if in-enabled "in" "in-disable")
                  :icon2 (if in-enabled "in-pushed" nil)
                  :action (when in-enabled
                            #'(lambda (item)
                                (declare (ignore item))
                                (modify-patch panel)
                                (add-input panel nil)))))
         (zoom-bg (om-make-dialog-item
                   'om-static-text
                   (om-make-point 60 1)
                   (om-make-point 95 24)
                   ""
                   :font *om-default-font1*
                   :bg-color *controls-color*))
         (zoom-label (om-make-dialog-item
                      'om-static-text
                      (om-make-point 65 4)
                      (om-make-point 35 18)
                      "Zoom"
                      :font *om-default-font1*
                      :bg-color *controls-color*))
         (numbox (om-make-view
                  'oa::om-zoom-pop-up
                  :position (om-make-point 105 4)
                  :size (om-make-point 60 18)
                  :value pct
                  :presets '(50 100 150 200 300 400)
                  :bg-color *om-white-color*
                  :di-action #'(lambda (widget new-pct)
                                 (declare (ignore widget))
                                 (om-zoom-numbox-apply panel (/ new-pct 100.0))))))
    (om-add-subviews bar out-btn in-btn zoom-bg zoom-label numbox)
    (om-add-subviews editor bar)
    (when panel
      (setf (oa::om-pane-property panel :om-zoom-numbox) numbox))
    (setf (oa::om-pane-property editor :om-zoom-editor-bar) bar)
    bar))

(defmethod initialize-instance :after ((self patchEditor) &rest initargs)
  (declare (ignore initargs))
  (when (om-zoom-editor-applies-p self)
    (handler-bind ((error
                    #'(lambda (c)
                        (oa::om-zoom-log-error :bar-build c))))
      (om-zoom-editor-bar-build self))))

(defun om-zoom-numbox-apply (pane new-zoom)
  "Apply NEW-ZOOM from the numbox widget anchored at content / viewport center."
  (oa::om-zoom-step-anchored pane new-zoom))

(defmethod add-window-buttons :around ((self patchPanel))
  (unless (oa::om-zoom-applies-p self)
    (call-next-method)))

(defmethod update-subviews :after ((self patchEditor))
  (let ((bar (oa::om-pane-property self :om-zoom-editor-bar)))
    (when bar
      (handler-bind ((error
                      #'(lambda (c)
                          (oa::om-zoom-log-error :bar-resize c))))
        (om-set-view-size bar (om-make-point (w self) +om-zoom-bar-h+))))))

(defmethod om-zoom-editor-bar-build :after ((editor LoopEditor))
  "Add the loop toolbar (iterators + accumulators) into the editor's zoom bar."
  (let* ((panel        (panel editor))
         (bar          (oa::om-pane-property editor :om-zoom-editor-bar))
         (iterators    '((om-icon-button "loop-for"    "For i from A to B"   forloop    "for")
                         (om-icon-button "loop-while"  "While A"             whileloop  "while")
                         (om-icon-button "loop-list"   "For element in list" listloop   "inlist")
                         (om-icon-button "loop-onlist" "For element on list" onlistloop "onlist")))
         (accumulators '((button-icon    175           "Incremental counter"  counter     "count")
                         (button-icon    172           "Sum accumulator"      sum         "sum")
                         (button-icon    174           "Minimize"             minim       "min")
                         (button-icon    173           "Maximize"             maxi        "max")
                         (button-icon    206           "Collect"              listing     "collect")
                         (button-icon    171           "General accumulator"  accumulator "accum"))))
    (flet ((mk (spec)
             (om-loop-make-button (first spec) panel (second spec) (third spec)
                                  (fourth spec) (fifth spec))))
      (when (and bar panel)
        (let ((y 1))
          (let ((x 185))
            (dolist (spec iterators)
              (let ((b (mk spec)))
                (om-set-view-position b (om-make-point x y))
                (incf x 25)
                (om-add-subviews bar b))))
          (let ((x 309))
            (dolist (spec accumulators)
              (let ((b (mk spec)))
                (om-set-view-position b (om-make-point x y))
                (incf x 25)
                (om-add-subviews bar b)))))))))

;;; ============================================================
;;; Drag-and-drop: unscale destination before OMGMoveObject
;;; ============================================================


;;; ============================================================
;;; miniview: ancestor zoom scopes icon size, font and label offsets
;;; ============================================================

(defmethod om-draw-contents ((self miniview))
  (let* ((box  (object (om-view-container self)))
         (zoom (oa::om-zoom-find-ancestor-zoom self)))
    (om-with-focused-view self
      (if (showpict box)
          (progn
            (om-with-fg-color self *om-white-color*
              (om-fill-rect 0 0 (w self) (h self)))
            (draw-mini-view self (value box)))
        (let* ((icon (icon (reference box)))
               (sizeicn (icon-sizes icon (def-icon-size box)))
               (xi (if (and (numberp zoom) (/= zoom 1.0))
                       (max 1 (round (* (car sizeicn) zoom)))
                       (car sizeicn)))
               (yi (if (and (numberp zoom) (/= zoom 1.0))
                       (max 1 (round (* (cadr sizeicn) zoom)))
                       (cadr sizeicn)))
               (iconhdlr (second (get&corrige-icon icon)))
               (pic (get-fonde-pict (value box)))
               posi)
          (if pic
              (om-draw-picture self pic
                               :size (om-make-point (w self) (h self)))
              (om-with-fg-color self (or (get-fonde-color (value box))
                                         *om-light-gray-color*)
                (om-fill-rect 0 0 (w self) (h self))))
          (when iconhdlr
            (setf posi (om-make-point (- (round (w self) 2) (round xi 2))
                                      (- (round (h self) 2) (round yi 2))))
            (om-draw-picture self iconhdlr :pos posi :size (om-make-point xi yi)))))
      (when (show-name box)
        (let ((sname-font (if (and (numberp zoom) (/= zoom 1.0))
                              (oa::om-zoom-scale-font *om-default-font1* zoom)
                              *om-default-font1*)))
          (om-with-font sname-font
            (om-draw-string (max 1 (round (* 4 (or zoom 1.0))))
                            (- (h self) (max 1 (round (* 5 (or zoom 1.0)))))
                            (name box)))))
      (when (play-state box)
        (om-with-focused-view self
          (om-with-fg-color self *om-green2-color*
            (om-fill-polygon '((15 5) (20 8) (15 11))))))
      (let ((line (if (selected-p self) 2 1)))
        (om-with-fg-color self (om-make-color 0 0 0)
          (om-draw-rect 0 0 (1- (w self)) (1- (h self)) :pensize line))))))

;;; ============================================================
;;; Patch I/O frames: io-size, font and arrow offsets scale with zoom;
;;; frame creation propagates *make-frame-zoom-context*.
;;; ============================================================

(defmethod make-basic-output ((self t) module)
  (let* ((zoom    (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (io-size (if scale-p (max 1 (round (* 8 zoom))) 8))
         (off-x   (if scale-p (round (* 4 zoom)) 4))
         (off-y   (if scale-p (round (* 9 zoom)) 9))
         (thenewout (om-make-view (get-out-class self)
                      :position (om-make-point (- (round (w module) 2) off-x)
                                               (- (h module) off-y))
                      :size (om-make-point io-size io-size)
                      :help-spec "Basic Type input"
                      :index 0)))
    (push thenewout (outframes module))
    (om-add-subviews module thenewout)))

(defmethod omG-change-type ((self TypedInFrame) thetype)
  (if (keys (object self))
      (om-beep-msg (string+ "You can not type a " (string (keys (object self))) " input."))
      (let* ((obj (object self))
             (container (om-view-container self))
             (newin (make-new-typed-input (name obj) thetype (indice obj) (frame-position obj)))
             (frame (let ((oa::*make-frame-zoom-context*
                           (and (typep container 'om-scroller) (oa::om-zoom-of container))))
                      (make-frame-from-callobj newin))))
        (when (active-mode self)
          (omG-unselect self))
        (setf (indice (object self)) nil)
        (omg-remove-element container self)
        (real-make-delete-before container (list self))
        (omG-add-element container frame))))

(defmethod draw-typed-special ((self typedinframe))
  (let* ((zoom (oa::om-zoom-effective self))
         (offx (round (* 4 zoom)))
         (offy (round (* 8 zoom))))
    (om-with-focused-view self
      (om-with-font (oa::om-current-default-font1 self)
        (om-draw-string (- (round (w self) 2) offx) offy
                        (format () "~D" (indice (object self))))))))

(defmethod OMGMoveObject ((self TypedInFrame) new-position)
  (if (om-shift-key-p)
      (let* ((target (om-view-container self)) newobj)
        (when (and target (find-class (reference (object self)) nil)
                   (omclass-p (find-class (reference (object self)) nil)))
          (setf newobj (omNG-make-new-boxcall-slots
                        (find-class (reference (object self)))
                        (borne-position new-position) (mk-unique-name target "slot")))
          (omG-add-element target
                           (let ((oa::*make-frame-zoom-context*
                                  (and (typep target 'om-scroller) (oa::om-zoom-of target))))
                             (make-frame-from-callobj newobj)))))
      (call-next-method)))

(defmethod move-frame-delta ((self TypedInFrame) dir)
  (let* ((pixnum (if (om-shift-key-p) 10 1))
         (pos    (or (and (object self) (frame-position (object self)))
                     (om-view-position self)))
         (new-position
          (case dir
            (0 (om-subtract-points pos (om-make-point 0 pixnum)))
            (1 (om-add-points      pos (om-make-point 0 pixnum)))
            (2 (om-add-points      pos (om-make-point pixnum 0)))
            (3 (om-subtract-points pos (om-make-point pixnum 0))))))
    (mapc #'(lambda (conection)
              (draw-connection conection nil)) (connections self))
    (om-set-view-position self new-position)))

(defmethod draw-in-inout-box ((container t) object)
  (let* ((zoom (oa::om-zoom-effective container))
         (offx (round (* 7 zoom)))
         (offy (round (* 12 zoom))))
    (om-with-focused-view container
      (om-with-font (oa::om-current-default-font1b container)
        (om-draw-string (- (round (w container) 2) offx) offy
                        (format () "~2D" (indice object)))))))
