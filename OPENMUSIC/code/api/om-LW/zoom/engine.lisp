;=========================================================================
; OM API
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;Author: Paulo Henrique Raposo

(in-package :oa)

;;; ---------- Scroll-position tracking (via capi-object-property) ----------

(defun tracked-scroll-x (pane)
  "Pane-local mirror of the horizontal scroll slug."
  (or (capi:capi-object-property pane :om-tracked-scroll-x) 0))

(defun (setf tracked-scroll-x) (value pane)
  (setf (capi:capi-object-property pane :om-tracked-scroll-x) value))

(defun tracked-scroll-y (pane)
  "Pane-local mirror of the vertical scroll slug."
  (or (capi:capi-object-property pane :om-tracked-scroll-y) 0))

(defun (setf tracked-scroll-y) (value pane)
  (setf (capi:capi-object-property pane :om-tracked-scroll-y) value))

(defmethod om-set-h-scroll-position :after ((self om-scroller) pos)
  (when (numberp pos) (setf (tracked-scroll-x self) pos)))

(defmethod om-set-v-scroll-position :after ((self om-scroller) pos)
  (when (numberp pos) (setf (tracked-scroll-y self) pos)))

(defmethod om-set-scroll-position :after ((self om-scroller) pos)
  (setf (tracked-scroll-x self) (om-point-h pos)
        (tracked-scroll-y self) (om-point-v pos)))

(defmethod om-move-scroll-position :after ((self om-scroller) pos)
  (setf (tracked-scroll-x self) (om-point-h pos)
        (tracked-scroll-y self) (om-point-v pos)))

(defmethod scroll-update :around ((self om-scroller) dimension operation pos-list &rest options)
  (declare (ignore options))
  (cond
    ((and *om-zoom-in-progress-p*
          (member dimension '(:horizontal :vertical))
          (numberp pos-list))
     nil)
    (t
     (when (eq operation :move)
       (cond
         ((and (consp pos-list)
               (numberp (first pos-list)) (numberp (second pos-list)))
          (unless *om-zoom-in-progress-p*
            (setf (tracked-scroll-x self) (first pos-list)
                  (tracked-scroll-y self) (second pos-list))))
         ((and (eq dimension :horizontal) (numberp pos-list))
          (setf (tracked-scroll-x self) pos-list))
         ((and (eq dimension :vertical) (numberp pos-list))
          (setf (tracked-scroll-y self) pos-list))))
     (call-next-method))))

;;; ---------- Content / geometry helpers ----------

(defun om-zoom-content-frames (pane)
  "All subviews of PANE eligible as content for bbox computation."
  (om-subviews pane))

(defun om-zoom-enclosing-bbox (frames)
  "Visual bbox (min-x min-y max-x max-y) of FRAMES, or NIL when empty."
  (when frames
    (let (min-x min-y max-x max-y)
      (dolist (f frames)
        (let ((p (handler-case (om-view-position f)
                   (error (c) (om-zoom-log-error :bbox-pos c) nil)))
              (s (handler-case (om-view-size f)
                   (error (c) (om-zoom-log-error :bbox-size c) nil))))
          (when (and p s)
            (let ((l (om-point-h p)) (top (om-point-v p))
                  (r (+ (om-point-h p) (om-point-h s)))
                  (b (+ (om-point-v p) (om-point-v s))))
              (setf min-x (if min-x (min min-x l) l)
                    min-y (if min-y (min min-y top) top)
                    max-x (if max-x (max max-x r) r)
                    max-y (if max-y (max max-y b) b))))))
      (when (and min-x min-y max-x max-y)
        (list min-x min-y max-x max-y)))))

;;; ---------- Engine: om-zoom-update ----------

(defun om-zoom-update (pane new-zoom &optional anchor-x anchor-y)
  "Apply NEW-ZOOM (clamped) to PANE anchored at viewport-local (ANCHOR-X, ANCHOR-Y).
Returns the clamped factor when it changed, NIL otherwise."
  (let* ((old-zoom (om-zoom-of pane))
         (clamped  (om-clamp-zoom new-zoom)))
    (unless (= clamped old-zoom)
      (let* ((ratio  (if (zerop old-zoom) 1 (/ clamped old-zoom)))
             (old-sx (tracked-scroll-x pane))
             (old-sy (tracked-scroll-y pane))
             (vx     (or anchor-x 0))
             (vy     (or anchor-y 0))
             (new-sx (max 0 (round (+ (* vx (- ratio 1)) (* old-sx ratio)))))
             (new-sy (max 0 (round (+ (* vy (- ratio 1)) (* old-sy ratio))))))
        (setf (om-zoom-of pane) clamped)
        (setf (tracked-scroll-x pane) new-sx)
        (setf (tracked-scroll-y pane) new-sy)
        (om-zoom-sync-display pane clamped)
        (let ((*om-zoom-in-progress-p* t))
          (unwind-protect
              (capi:with-atomic-redisplay (pane)
                (dolist (frame (om-subviews pane))
                  (om-zoom-relayout-frame frame))
                (dolist (frame (om-subviews pane))
                  (om-zoom-redraw-connections frame))
                (multiple-value-bind (vw vh) (om-pane-visible-size pane)
                  (let ((content-h 0)
                        (content-v 0))
                    (dolist (sub (om-subviews pane))
                      (let ((p (handler-case (om-view-position sub)
                                 (error (c) (om-zoom-log-error :update-pos c) nil)))
                            (s (handler-case (om-view-size sub)
                                 (error (c) (om-zoom-log-error :update-size c) nil))))
                        (when (and p s)
                          (let ((right  (+ (om-point-h p) (om-point-h s)))
                                (bottom (+ (om-point-v p) (om-point-v s))))
                            (when (> right  content-h) (setf content-h right))
                            (when (> bottom content-v) (setf content-v bottom))))))
                    (let* ((target-h (max (or vw 0) content-h (+ new-sx (or vw 0))))
                           (target-v (max (or vh 0) content-v (+ new-sy (or vh 0))))
                           (sx-valid (max 0 (min new-sx (max 0 (- target-h (or vw 0))))))
                           (sy-valid (max 0 (min new-sy (max 0 (- target-v (or vh 0)))))))
                      (capi:set-horizontal-scroll-parameters pane :min-range 0 :max-range target-h)
                      (capi:set-vertical-scroll-parameters   pane :min-range 0 :max-range target-v)
                      (setf (tracked-scroll-x pane) sx-valid)
                      (setf (tracked-scroll-y pane) sy-valid)
                      (om-set-h-scroll-position pane sx-valid)
                      (om-set-v-scroll-position pane sy-valid)
                      (capi:scroll pane :pan :move (list sx-valid sy-valid))))))
            (om-invalidate-view pane t)))
        clamped))))

(defun om-set-zoom   (pane factor) (om-zoom-update pane factor))
(defun om-reset-zoom (pane)        (om-zoom-update pane 1.0))
(defun om-get-zoom   (pane)        (om-zoom-of pane))

;;; ---------- Anchored zoom step + reset-and-recenter ----------

(defparameter *om-zoom-selected-frames-fn* nil
  "Optional hook (function of PANE) returning user-selected frames+connections for
   anchor priority; bound by upper layers that know how to introspect selection.")

(defun om-zoom-step-anchored (pane new-zoom)
  "Zoom PANE to NEW-ZOOM anchored at selection bbox center if any, else content
   bbox center, with viewport center as final fallback."
  (multiple-value-bind (vw vh) (om-pane-visible-size pane)
    (let* ((vw (or vw 0))
           (vh (or vh 0))
           (sx (tracked-scroll-x pane))
           (sy (tracked-scroll-y pane))
           (frames (or (and *om-zoom-selected-frames-fn*
                            (funcall *om-zoom-selected-frames-fn* pane))
                       (om-zoom-content-frames pane)))
           (bbox (om-zoom-enclosing-bbox frames))
           (ax 0) (ay 0))
      (cond
        (bbox
         (setf ax (round (- (/ (+ (first bbox) (third bbox)) 2) sx))
               ay (round (- (/ (+ (second bbox) (fourth bbox)) 2) sy))))
        (t
         (setf ax (round vw 2)
               ay (round vh 2))))
      (om-zoom-update pane new-zoom ax ay))))

(defun om-zoom-reset-and-recenter (pane)
  "Reset PANE zoom to 1.0 and scroll so content bbox center lands at viewport center."
  (multiple-value-bind (vw vh) (om-pane-visible-size pane)
    (let* ((vw (or vw 0))
           (vh (or vh 0))
           (current (om-zoom-of pane))
           (frames  (om-zoom-content-frames pane))
           (bbox    (om-zoom-enclosing-bbox frames)))
      (cond
        ((null bbox)
         (setf (tracked-scroll-x pane) 0
               (tracked-scroll-y pane) 0)
         (om-zoom-update pane 1.0 0 0))
        (t
         (let* ((cx-vis (/ (+ (first bbox) (third bbox)) 2))
                (cy-vis (/ (+ (second bbox) (fourth bbox)) 2))
                (cx-log (if (zerop current) cx-vis (/ cx-vis current)))
                (cy-log (if (zerop current) cy-vis (/ cy-vis current)))
                (new-sx (max 0 (round (- cx-log (/ vw 2)))))
                (new-sy (max 0 (round (- cy-log (/ vh 2))))))
           (setf (tracked-scroll-x pane) new-sx
                 (tracked-scroll-y pane) new-sy)
           (om-zoom-update pane 1.0 0 0)))))))

;;; ---------- Keyboard shortcuts ----------

(defun om-zoom-shortcut-in (pane x y gspec)
  (declare (ignore x y gspec))
  (when (om-zoom-applies-p pane)
    (om-zoom-step-anchored pane (* (om-zoom-of pane) 1.1))))

(defun om-zoom-shortcut-out (pane x y gspec)
  (declare (ignore x y gspec))
  (when (om-zoom-applies-p pane)
    (om-zoom-step-anchored pane (/ (om-zoom-of pane) 1.1))))

(defun om-zoom-shortcut-reset (pane x y gspec)
  (declare (ignore x y gspec))
  (when (om-zoom-applies-p pane)
    (om-zoom-reset-and-recenter pane)))

(defun om-zoom-viewport-logical (pane)
  "Return four LOGICAL values (sx-log sy-log vw-log vh-log) describing PANE's visible viewport pre-zoom.
PANE must be the om-scroller; uses om-zoom-of (not effective)."
  (let ((zoom (om-zoom-of pane))
        (sx   (om-h-scroll-position pane))
        (sy   (om-v-scroll-position pane)))
    (let ((k (if (= zoom 0) 1.0 (/ 1.0 zoom))))
      (multiple-value-bind (vw vh) (om-pane-visible-size pane)
        (values (round (* sx k))
                (round (* sy k))
                (max 1 (round (* (or vw (vw pane)) k)))
                (max 1 (round (* (or vh (vh pane)) k))))))))

(defun om-zoom-input-entries ()
  "Input-model entries for pinch + Ctrl=/Ctrl+/Ctrl-/Ctrl0. Prepend to existing input-model."
  `(((:gesture-spec #\= #+macosx :hyper #-macosx :control) om-zoom-shortcut-in)
    ((:gesture-spec #\+ #+macosx :hyper #-macosx :control) om-zoom-shortcut-in)
    ((:gesture-spec #\- #+macosx :hyper #-macosx :control) om-zoom-shortcut-out)
    ((:gesture-spec #\0 #+macosx :hyper #-macosx :control) om-zoom-shortcut-reset)
    ((:touch :zoom) om-zoom-touch-handler)))

;;; ---------- Exports ----------

;;; ---------- Zoom percent popup widget ----------

(defclass om-zoom-pop-up (om-view)
  ((zoom-value     :initarg :value     :initform 100  :accessor zoom-value)
   (zoom-presets   :initarg :presets   :initform '(50 100 150 200 300 400) :accessor zoom-presets)
   (zoom-action    :initarg :di-action :initform nil  :accessor zoom-action)
   (zoom-text-font :initarg :font      :initform nil  :accessor zoom-text-font)
   (zoom-fg-color  :initarg :fg-color  :initform nil  :accessor zoom-fg-color)))

(defmethod om-draw-contents ((self om-zoom-pop-up))
  (let* ((w (om-width self))
         (h (om-height self))
         (font (or (zoom-text-font self) (om-current-default-font1 self)))
         (txt (format nil "~D%" (zoom-value self))))
    (om-with-focused-view self
      (om-draw-rect 0 0 (- w 1) (- h 1))
      (om-with-font font
        (multiple-value-bind (x1 y1 x2 y2)
            (gp::get-string-extent self txt
                                   (if (gp::font-p font)
                                       font
                                       (gp::find-best-font self font)))
          (declare (ignore x1 x2))
          (om-draw-string 4 (round (- h y1 y2) 2) txt)))
      (let* ((ax (- w 10))
             (ay (- (round h 2) 1)))
        (om-draw-line ax ay (+ ax 3) (+ ay 3))
        (om-draw-line (+ ax 3) (+ ay 3) (+ ax 6) ay)))))

(defmethod om-view-click-handler ((self om-zoom-pop-up) where)
  (declare (ignore where))
  (let* ((widget self)
         (items (loop for pct in (zoom-presets self)
                      collect
                      (let ((captured pct))
                        (make-instance 'capi:menu-item
                                       :title (format nil "~D%" pct)
                                       :data captured
                                       :callback
                                       #'(lambda (data interface)
                                           (declare (ignore data interface))
                                           (when (zoom-action widget)
                                             (funcall (zoom-action widget) widget captured)))))))
         (themenu (make-instance 'capi:menu :title "" :items items)))
    (capi::display-popup-menu themenu :owner self :x 0 :y (om-height self))
    t))

(defmethod om-set-zoom-pop-up-value ((self om-zoom-pop-up) new-pct)
  (setf (zoom-value self) new-pct)
  (om-invalidate-view self))

(defun om-zoom-sync-numbox (pane new-zoom)
  "Refresh PANE's percent popup widget (held under :om-zoom-numbox property)."
  (let ((widget (om-pane-property pane :om-zoom-numbox)))
    (when widget
      (om-set-zoom-pop-up-value widget (round (* new-zoom 100))))))

;;; ---------- Coord routing: mouse position unscale ----------

(defun om-zoom-mouse-pos-maybe-unscale (view raw)
  "Return RAW divided by VIEW's effective zoom when
*om-zoom-unscale-mouse-pos-p* is bound T; return RAW otherwise."
  (if (and *om-zoom-unscale-mouse-pos-p* view)
      (let ((zoom (om-zoom-effective view)))
        (if (or (null zoom) (= zoom 1.0))
            raw
            (om-zoom-unscale-point raw zoom)))
      raw))

(defmethod om-mouse-position :around ((view om-graphic-object))
  (om-zoom-mouse-pos-maybe-unscale view (call-next-method)))

(defmethod om-mouse-position :around ((view om-item-view))
  (om-zoom-mouse-pos-maybe-unscale view (call-next-method)))

;;; ---------- Override: om-set-view-size standard-dialog-item ----------
;;; Run hint-table + static-layout updates via apply-in-pane-process so
;;; CAPI gadgets carry the correct external-min/max-width even when the
;;; panel is not yet realized at reopen time.

(defmethod om-set-view-size ((self om-standard-dialog-item) size-point)
  (setf (vw self) (om-point-h size-point) (vh self) (om-point-v size-point))
  ;;; Original `when interface-visible-p` skipped hint-table + static-layout updates
  ;;; while panel was not yet realized (window-show happens after make-frame-from-callobj).
  ;;; That left CAPI gadgets (button, check-box, slider...) with stale external-min/max-width
  ;;; from `om-make-dialog-item` -- so a re-opened patch with zoom != 1.0 rendered them
  ;;; at the saved visual size, not at the size for the current saved zoom.
  ;;; Replaced with `apply-in-pane-process` which the LW manual states runs immediately
  ;;; for not-yet-displayed panes, so hints persist for when CAPI realizes the panel.
  (capi:apply-in-pane-process
   self
   (lambda ()
     (set-hint-table self
                     (list :default-width (om-point-h size-point)
                           :defalut-height (om-point-v size-point)
                           :external-min-width (om-point-h size-point)
                           :external-min-height (om-point-v size-point)
                           :external-max-width (om-point-h size-point)
                           :external-max-height (om-point-v size-point)))
     (setf (static-layout-child-size self)
           (values (om-point-h size-point) (om-point-v size-point)))
     (setf (static-layout-child-position self)
           (values (vx self) (vy self)))
     (di-after-settings self))))

;;; ---------- om-zoom-pop-up accessors ----------

(defmethod om-zoom-pop-up-value ((self om-zoom-pop-up))
  (zoom-value self))

(defmethod om-zoom-pop-up-presets ((self om-zoom-pop-up))
  (zoom-presets self))

(defmethod om-set-zoom-pop-up-presets ((self om-zoom-pop-up) preset-list)
  (setf (zoom-presets self) preset-list))

;;; ---------- om-zoom-musical-font-pop-up ----------

(defclass om-zoom-musical-font-pop-up (om-view)
  ((zoom-value     :initarg :value     :initform 24   :accessor zoom-value)
   (zoom-presets   :initarg :presets   :initform nil  :accessor zoom-presets)
   (zoom-action    :initarg :di-action :initform nil  :accessor zoom-action)
   (zoom-text-font :initarg :font      :initform nil  :accessor zoom-text-font)
   (zoom-fg-color  :initarg :fg-color  :initform nil  :accessor zoom-fg-color))
  (:documentation
   "Like om-zoom-pop-up but displays font sizes (no '%' suffix)."))

(defmethod om-draw-contents ((self om-zoom-musical-font-pop-up))
  (let* ((w (om-width self))
         (h (om-height self))
         (font (or (zoom-text-font self) (om-current-default-font1 self)))
         (txt (format nil "~D" (zoom-value self))))
    (om-with-focused-view self
      (om-draw-rect 0 0 (- w 1) (- h 1))
      (om-with-font font
        (multiple-value-bind (x1 y1 x2 y2)
            (gp::get-string-extent self txt
                                   (if (gp::font-p font)
                                       font
                                       (gp::find-best-font self font)))
          (declare (ignore x1 x2))
          (om-draw-string 4 (round (- h y1 y2) 2) txt)))
      (let* ((ax (- w 10))
             (ay (- (round h 2) 1)))
        (om-draw-line ax ay (+ ax 3) (+ ay 3))
        (om-draw-line (+ ax 3) (+ ay 3) (+ ax 6) ay)))))

(defmethod om-view-click-handler ((self om-zoom-musical-font-pop-up) where)
  (declare (ignore where))
  (let* ((widget self)
         (items (loop for size in (zoom-presets self)
                      collect
                      (let ((captured size))
                        (make-instance 'capi:menu-item
                                       :title (format nil "~D" size)
                                       :data captured
                                       :callback
                                       #'(lambda (data interface)
                                           (declare (ignore data interface))
                                           (when (zoom-action widget)
                                             (funcall (zoom-action widget) widget captured)))))))
         (themenu (make-instance 'capi:menu :title "" :items items)))
    (capi::display-popup-menu themenu :owner self :x 0 :y (om-height self))
    t))

(defmethod om-zoom-musical-font-pop-up-value ((self om-zoom-musical-font-pop-up))
  (zoom-value self))

(defmethod om-set-zoom-musical-font-pop-up-value ((self om-zoom-musical-font-pop-up) new-size)
  (setf (zoom-value self) new-size)
  (om-invalidate-view self))

(defmethod om-zoom-musical-font-pop-up-presets ((self om-zoom-musical-font-pop-up))
  (zoom-presets self))

(defmethod om-set-zoom-musical-font-pop-up-presets ((self om-zoom-musical-font-pop-up) preset-list)
  (setf (zoom-presets self) preset-list))

;;; ---------- om-scroller specializations: pinch zoom + tracked-scroll on layout ----------

(defmethod om-zoom-touch-update ((pane om-scroller) scale anchor-x anchor-y)
  (om-zoom-update pane (* (om-zoom-of pane) scale) anchor-x anchor-y))

(defmethod om-zoom-resolve-touch-target ((pane om-scroller) x y)
  (values pane x y))

(defmethod update-for-subviews-changes :after ((self om-scroller) &optional (recursive nil))
  (declare (ignore recursive))
  (when (initialized-p self)
    (capi::apply-in-pane-process
     self
     (lambda ()
       (let ((x (capi::get-horizontal-scroll-parameters self :slug-position))
             (y (capi::get-vertical-scroll-parameters self :slug-position)))
         (when (numberp x) (setf (tracked-scroll-x self) x))
         (when (numberp y) (setf (tracked-scroll-y self) y)))))))

(export '(tracked-scroll-x
          tracked-scroll-y
          om-zoom-content-frames
          om-zoom-enclosing-bbox
          om-zoom-viewport-logical
          om-zoom-update
          om-set-zoom
          om-reset-zoom
          om-get-zoom
          om-zoom-step-anchored
          om-zoom-reset-and-recenter
          om-zoom-shortcut-in
          om-zoom-shortcut-out
          om-zoom-shortcut-reset
          om-zoom-input-entries
          om-zoom-pop-up
          zoom-value
          zoom-presets
          zoom-action
          om-zoom-pop-up-value
          om-set-zoom-pop-up-value
          om-zoom-pop-up-presets
          om-set-zoom-pop-up-presets
          om-zoom-musical-font-pop-up
          om-zoom-musical-font-pop-up-value
          om-set-zoom-musical-font-pop-up-value
          om-zoom-musical-font-pop-up-presets
          om-set-zoom-musical-font-pop-up-presets
          om-zoom-sync-numbox
          om-zoom-mouse-pos-maybe-unscale)
        :oa)

;;; ---------- dialog-items: create-callback hint + pop-up select ----------

(defmethod om-create-callback ((self om-standard-dialog-item))
  (set-hint-table self (list :default-x (vx self) :default-y (vy self)
                             :default-width (vw self) :defalut-height (vh self))))

(defmethod om-select-item-index ((self om-pop-up-dialog-item) index)
  (setf (choice-selection self) index))
