(in-package :alloy-desktop)

(defparameter *wheel-step* 40
  "Pixels one wheel notch scrolls a clip-view.")

(defun scroll-clip-by-wheel (clip event &key (step *wheel-step*))
  "Scroll CLIP one wheel notch of STEP px per EVENT unit, remapping the axes as alloy's clip-view wheel handler does."
  ;; Mirrors alloy's own clip-view wheel handler (alloy layouts/clip-view.lisp:
  ;; handle for (scroll clip-view)) axis for axis -- including its limit-dependent
  ;; remap of a vertical wheel onto whichever axis is free -- but travels STEP px
  ;; instead of 10% of the viewport, so the notch is the same everywhere and a small
  ;; pane does not crawl. The clip stays the sole owner of the offset: its (setf
  ;; offset) clamps and notifies 'value, which is what moves the scrollbar and its
  ;; thumb (alloy layouts/clip-view.lisp: clamp-offset and the (setf offset) :around
  ;; for (point clip-view); alloy components/scroll.lisp: value and range of
  ;; y-scrollbar; alloy components/base.lisp: initialize-instance :after and
  ;; value-changed of value-component).
  (let* ((limit (alloy::limit clip))
         (dx (case limit
               (:x 0)
               (:y (if (zerop (alloy:dx event)) (alloy:dy event) (alloy:dx event)))
               (t  (alloy:dx event))))
         (dy (case limit
               (:x (if (zerop (alloy:dy event)) (alloy:dx event) (- (alloy:dy event))))
               (:y 0)
               (t  (- (alloy:dy event))))))
    (alloy:with-unit-parent clip
      ;; one axis per write: alloy's clip-view DROPS an offset whose two axes both
      ;; change (alloy layouts/clip-view.lisp: the (setf offset) :around for (point
      ;; clip-view) guards with AND instead of OR)
      (unless (zerop dx)
        (let ((o (alloy:offset clip)))
          (setf (alloy:offset clip)
                (alloy:px-point (+ (alloy:pxx o) (* step dx)) (alloy:pxy o)))))
      (unless (zerop dy)
        (let ((o (alloy:offset clip)))
          (setf (alloy:offset clip)
                (alloy:px-point (alloy:pxx o) (+ (alloy:pxy o) (* step dy)))))))
    t))
