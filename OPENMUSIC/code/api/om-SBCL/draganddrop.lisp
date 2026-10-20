(in-package :om-api)

(export '(om-drag-view
          om-drop-view
          om-drag-reference-view
          om-drag-container-view
          om-drag-start
          om-drag-receive
          om-drag-enter-view
          om-drag-leave-view
          om-draw-contents-for-drag
          om-import-files-in-app
          om-drag-string-in-app
          om-drop-callback)
        :om-api)

(defclass om-drag-view (alld:drag-view)
  ((cursor-pos :initform (om-make-point 0 0) :accessor om-drag-view-cursor-pos)))

(defclass om-drop-view (alld:drop-view) ())

(defgeneric om-drag-view-p (self)
  (:method ((self t)) nil)
  (:method ((self om-drag-view)) t))

(defgeneric om-drop-view-p (self)
  (:method ((self t)) nil)
  (:method ((self om-drop-view)) t))

(defgeneric om-drag-start            (self))
(defgeneric om-drag-reference-view   (self))
(defgeneric om-drag-container-view   (self))
(defgeneric om-draw-contents-for-drag (self))
(defgeneric om-drag-enter-view       (self))
(defgeneric om-drag-leave-view       (self))
(defgeneric om-drag-receive          (target dragged-view position &optional effect))
(defgeneric om-import-files-in-app   (self file-list))
(defgeneric om-drag-string-in-app    (self str))
(defgeneric om-drop-callback         (self drop-object stage))

(defmethod om-drag-start            ((self t))        nil)
(defmethod om-drag-start            ((self om-drag-view)) t)
(defmethod om-drag-reference-view   ((self t))        self)
(defmethod om-drag-container-view   ((self t))        (om-view-container (om-drag-reference-view self)))
(defmethod om-draw-contents-for-drag ((self t))       (om-draw-contents self))
(defmethod om-drag-enter-view       ((self t))        nil)
(defmethod om-drag-leave-view       ((self t))        nil)
(defmethod om-drag-receive          ((view t) (dragged-view t) position &optional (effect nil))
  (declare (ignore position effect)) nil)
(defmethod om-import-files-in-app   ((self t) file-list) (declare (ignore file-list)) nil)
;;; TODO: Win32 OLE IDropTarget + CF_UNICODETEXT to dispatch string drops from
;;; external apps (kernel patchpanel/soundpanel callsites); GLFW exposes only files.
(defmethod om-drag-string-in-app    ((self t) str)    (declare (ignore str)) nil)
(defmethod om-drop-callback ((self t) drop-object stage)
  (flet ((allow-effect ()
           (let ((effect (alld:negotiate-drop-effect
                          drop-object '(:move :copy))))
             (setf (capi::drop-object-drop-effect drop-object) effect)
             (unless (eq effect :none) effect))))
    (case stage
      (:formats
       (capi::set-drop-object-supported-formats
        drop-object '(:string :value :om-object :filename-list)))
      ((:enter :leave :drag) (allow-effect))
      (:drop
       (let* ((x (capi::drop-object-pane-x drop-object))
              (y (capi::drop-object-pane-y drop-object))
              (hit (capi::pinboard-object-at-position self x y))
              (drop-view (if hit (om-get-real-view hit) self)))
         (cond
           ((and (capi::drop-object-provides-format drop-object :filename-list)
                 (om-import-files-in-app
                  self (capi::drop-object-get-object
                        drop-object self :filename-list)))
            (allow-effect))
           ((and (capi::drop-object-provides-format drop-object :string)
                 (om-drag-string-in-app
                  self (capi::drop-object-get-object drop-object self :string)))
            (allow-effect))
            (t
             (let ((dragged-view
                     (capi::drop-object-get-object drop-object self :om-object)))
               (when dragged-view
                 (let ((effect (allow-effect)))
                   (when effect
                 (let ((grab (om-drag-view-cursor-pos dragged-view)))
                   (unless (om-drag-receive
                            drop-view dragged-view
                            (om-make-point (- x (om-point-h grab))
                                           (- y (om-point-v grab)))
                            effect)
                     (setf (capi::drop-object-drop-effect drop-object)
                           :none))))))))))))))

(defun %drop-location-to-om-point (pane location)
  (if (typep pane 'om-look-window)
      (om-make-point (round (alloy:pxx location))
                     (round (- (alloy:pxh (alloy:bounds pane))
                                   (alloy:pxy location))))
      (%alloy-location-to-om-point location pane)))

(defun %current-drag-proposed-effect ()
  ;; CAPI normally derives the proposal from the native drag session.  Alloy's
  ;; pointer events carry no modifiers, so use the keyboard state maintained by
  ;; actions.lisp; Option is OM's established copy modifier.
  (and (om-option-key-p) :copy))

(defun %make-capi-drop-object (pane location payload
                               &key
                                 (source-effects '(:move :copy))
                                 (proposed-effect
                                   (%current-drag-proposed-effect))
                                 (origin :internal))
  (let ((point (%drop-location-to-om-point pane location)))
    (make-instance 'capi::drop-object
                   :pane pane :payload payload
                   :source-effects source-effects
                   :proposed-effect proposed-effect
                   :origin origin
                   :x (om-point-h point) :y (om-point-v point))))

(defun %update-capi-drop-object (drop-object pane location)
  (let ((point (%drop-location-to-om-point pane location)))
    (setf (capi::drop-object-pane-x drop-object) (om-point-h point)
          (capi::drop-object-pane-y drop-object) (om-point-v point))
    (when (eq :internal (alld:drop-object-origin drop-object))
      (setf (alld:drop-object-proposed-effect drop-object)
            (%current-drag-proposed-effect))))
  drop-object)

(defun %current-drag-payload ()
  (when alld:*drag-source* (list :om-object alld:*drag-source*)))

;;; Drag cursor: vanilla gets it from the native session (capi:drag-pane-object,
;;; om-LW/draganddrop.lisp: internal-drag-start) -- while the gesture lives the pointer says the
;;; object is being carried. Here the API owns the pointer, so every window the
;;; gesture visits takes the cursor and gets its own back at cleanup
;;; (alloy-desktop services/drag-and-drop.lisp: take-drag-cursor). The move path
;;; rewrites it on every event, since the hover leave step (actions.lisp:
;;; %leave-hover-view) also writes the cursor mid-gesture.

(defvar *last-pinboard-under-mouse* nil)

(defvar *om-drag-cursor* nil
  "Pointer shown while an OM drag is live; NIL falls back to the API hand cursor.")

(defun %drag-cursor ()
  (or *om-drag-cursor* *om-hand-cursor* :pointing-hand))

(defun %om-view-owner (view)
  "VIEW's owner in the OM view graph. A widget's internal representation -- a list
row from alloy:represent-with (dialog-items.lisp: the alloy::represent-for methods
of om-single-item-list and om-multi-item-list) -- is entered through alloy and never
through om-add-subviews, so its vcontainer is NIL and the OM chain
would stop on it; the layout parent carries it to the widget that owns it."
  (or (om-view-container view)
      (unless (typep view 'om-abstract-window)
        (loop repeat 16
              for el = (alld:layout-owner view) then (alld:layout-owner el)
              while el
              when (typep el 'om-graphic-object) return el))))

(defun %drop-view-chain (window location)
  "The om-drop-views under LOCATION, deepest first, then WINDOW. Vanilla dispatches
files on the pane whose callback fired (om-LW/draganddrop.lisp: the :filename-list
branch of om-drop-callback for t); here the
deepest view is a kernel box, which is a drop-view but imports no file, so every
ancestor gets its turn."
  (append (loop for view = (alld:element-at-location
                            window location
                            :test (lambda (el) (typep el 'om-graphic-object)))
                  then (%om-view-owner view)
                while view
                when (om-drop-view-p view) collect view)
          (list window)))

(defun %drop-view-at-location (window location)
  "Deepest om-drop-view at LOCATION, or the nearest drop-view ancestor: the vanilla
drop target falls back on the pane when the hit object is not one
\(om-LW/draganddrop.lisp: the dropview choice in om-drop-callback for t)."
  (find-if #'om-drop-view-p (%drop-view-chain window location)))

;;; The drag session (alloy-desktop services/drag-and-drop.lisp: drag-move-epilogue,
;;; drag-up-epilogue, drag-cleanup) asks the OM drag source for targets, routes and
;;; the drop protocol; these methods are the OM answers.

;; Cross-window routing: vanilla has it for free, the native drag session posts the
;; drop to the pane under the pointer whatever window owns it
;; (om-LW/draganddrop.lisp: internal-drag-start).
(defmethod alld:drag-candidate-windows ((source om-drag-view) window)
  (declare (ignore window))
  (loop for window in (om-get-all-windows 'om-look-window)
        when (and (not (typep window 'alld:popup-window))
                  (%glfw:pointer window)
                  (om-window-visible-p window))
          collect window))

(defmethod alld:drag-target-at ((source om-drag-view) window location)
  (%drop-view-at-location window location))

(defmethod alld:drag-cursor ((source om-drag-view))
  (%drag-cursor))

(defmethod alld:drag-enter ((source om-drag-view) target location)
  (om-drag-enter-view target)
  (setf alld:*drag-drop-object*
        (%make-capi-drop-object target location (%current-drag-payload)))
  (om-drop-callback target alld:*drag-drop-object* :formats)
  (om-drop-callback target alld:*drag-drop-object* :enter))

(defmethod alld:drag-leave ((source om-drag-view) target)
  (when alld:*drag-drop-object*
    (om-drop-callback target alld:*drag-drop-object* :leave))
  (om-drag-leave-view target))

(defmethod alld:drag-leave-final ((source om-drag-view) target)
  (om-drag-leave-view target))

(defmethod alld:drag-over ((source om-drag-view) target location)
  (%update-capi-drop-object alld:*drag-drop-object* target location)
  (om-drop-callback target alld:*drag-drop-object* :drag))

(defmethod alld:drag-effect ((source om-drag-view))
  (capi::drop-object-drop-effect alld:*drag-drop-object*))

(defmethod alld:drag-commit ((source om-drag-view) target location)
  (let ((drop-object
          (or (and alld:*drag-drop-object*
                   (%update-capi-drop-object alld:*drag-drop-object* target location))
              (%make-capi-drop-object target location (%current-drag-payload)))))
    (om-drop-callback target drop-object :formats)
    ;; The destination commits first.  Only its successful callback
    ;; can make the kernel mutate/move the OM source.
    (om-drop-callback target drop-object :drop)))

;; The grab offset is WHERE on the view the mouse pressed; record it at click
;; time (the drag only starts on the first motion, by which point the cursor has
;; already left that point — recording then makes the ghost lag the cursor).
(defmethod om-view-click-handler :before ((self om-drag-view) pos)
  (unless alld:*drag-active-p*
    (setf (om-drag-view-cursor-pos self) pos)))

(defmethod om-click-motion-handler :before ((self om-drag-view) pos)
  (declare (ignore pos))
  ;; *click-motion-action* mutual exclusion: no drag while a transient-drawing
  ;; click-motion animation is running (vanilla internal-drag-start guard).
  (unless (or alld:*drag-active-p* *click-motion-action*)
    (when (om-drag-start self)
      (setf alld:*drag-source*   self
            alld:*drag-active-p* t)
      (alld:take-drag-cursor (om-view-window self) (%drag-cursor)))))

;;; Guaranteed per-event epilogues: alloy:decline is a restart UNWIND, which
;;; skips :after methods — drag tracking and the drop must run on EVERY
;;; move/up, so they live in :around unwind-protect cleanups.

(defmethod alloy:handle :around ((event alloy:pointer-move) (window om-look-window))
  (unwind-protect (call-next-method)
    (let ((location (alloy:location event)))
      (%zoom-popup-update-hover window location)
      (alld:drag-move-epilogue window location))))

(defmethod alloy:handle :around ((event alloy:pointer-up) (window om-look-window))
  (unwind-protect (call-next-method)
    ;; CAPI binds the whole drag gesture to button 1 -- press, motion and the
    ;; release that commits it (capi-drag-and-drop-ug-2.htm:63-71); any other
    ;; button leaves the drag armed instead of dropping.
    (when (eql (alloy:kind event) :left)
      (alld:drag-up-epilogue window (alloy:location event)))))

(defun %drag-view-window (view)
  "VIEW's window for alld:drag-involves-window-p. om-view-window reads
alloy:layout-tree, whose parent slot LEAVE makunbounds: a view deallocated
mid-drag answers unbound-slot, and that means \"not this window\"."
  (handler-case (om-view-window view) (unbound-slot () nil)))

;; These specializers/qualifiers are deliberately distinct from the existing
;; action, focus, and teardown authorities.  They only retire an active gesture.
(defmethod alloy:handle :before ((event alloy:key-down) (window om-look-window))
  (declare (ignore window))
  (when (and alld:*drag-active-p* (eq :escape (alloy:key event)))
    (alld:drag-cleanup)))

(defmethod %glfw:window-focused :before ((window om-look-window) focused)
  (when (and (not focused) alld:*drag-active-p*
             (alld:drag-involves-window-p window #'%drag-view-window))
    (alld:drag-cleanup)))

(defmethod alloy:deallocate :around ((window om-look-window))
  (when (and alld:*drag-active-p* (alld:drag-involves-window-p window #'%drag-view-window))
    (alld:drag-cleanup))
  (call-next-method))

;; Explicit OM closes call WINDOWING:CLOSE directly and intentionally bypass
;; ALLOY:DEALLOCATE (alloy-desktop/window/close.lisp: close-window for window).
;; Cover that independent destruction path too.
(defmethod om-close-window :before ((window om-look-window))
  (when (and alld:*drag-active-p* (alld:drag-involves-window-p window #'%drag-view-window))
    (alld:drag-cleanup)))

(defun %deliver-external-file-drop (target location paths)
  "Offer one GLFW filename-list to TARGET as a non-mutable copy source."
  (let ((drop-object
          (%make-capi-drop-object
           target location (list :filename-list paths)
           :source-effects '(:copy)
           :proposed-effect :copy
           :origin :external)))
    ;; GLFW exposes precisely this external format.  The semantic import method
    ;; remains the final acceptance gate for each target in the OM owner chain.
    (capi::set-drop-object-supported-formats drop-object '(:filename-list))
    (let ((effect (alld:negotiate-drop-effect drop-object '(:copy))))
      (setf (capi::drop-object-drop-effect drop-object) effect)
      (and (eq effect :copy)
           (capi::drop-object-provides-format drop-object :filename-list)
           ;; om-import-files-in-app is the KERNEL's import hook: its failure is
           ;; recorded as :none and reported, then RE-SIGNALLED to the kernel's handler
           (handler-bind ((error
                            (lambda (condition)
                              (setf (capi::drop-object-drop-effect drop-object) :none)
                              (format *error-output*
                                      "~&[om-api] external file drop rejected by ~A: ~A~%"
                                      (type-of target) condition))))
             (not (null (om-import-files-in-app
                         target
                         (capi::drop-object-get-object
                          drop-object target :filename-list)))))))))

;; the drop event reaches this through alloy-desktop window/assembly.lisp:
;; alloy:handle for (drop-event window), which declines a drop answered NIL
(defmethod alld:files-dropped ((window om-look-window) paths position)
  (and paths
       (loop for target in (%drop-view-chain window position)
             ;; GLFW already identifies this as the one external format
             ;; it supports.  Dispatch the semantic import hook directly
             ;; instead of re-entering project CAPI callbacks that query
             ;; global pointer state and wrap errors in ABORT.  Internal
             ;; OM drags still use the full staged callback protocol.
             thereis (%deliver-external-file-drop target position paths))))

;;; drag preview overlay (vanilla build-d&d-image, om-LW/draganddrop.lisp: build-d&d-image for om-drag-view):
;;; drawn just before the buffer swap (an alloy:render :after runs post-swap, so its
;;; pixels never reach the front buffer), with an explicit pixel-ortho.
(defmethod %glfw:swap-buffers :before ((window om-look-window))
  ;; the OS captures the mouse to the press window during a drag: the ghost is
  ;; only meaningful on the drag-source's window, never on other OM windows
  (when (and alld:*drag-active-p* alld:*drag-source*
             (eq window (om-view-window alld:*drag-source*)))
    (destructuring-bind (fw fh) (%glfw:framebuffer-size window)
      (let* ((source alld:*drag-source*)
             (container (om-drag-container-view source))
             ;; vanilla hotspot (om-LW/draganddrop.lisp: build-d&d-image for om-drag-view): the grab point in
             ;; CONTAINER coords — om-draw-contents-for-drag draws at container coords
             (posi   (om-subtract-points
                      (om-convert-coordinates (om-drag-view-cursor-pos source)
                                              source container)
                      (om-scroll-position container)))
             (cx     (round (alloy:pxx (alld:cursor-location (alld:pointer-state)))))
             (cy     (round (- fh (alloy:pxy (alld:cursor-location (alld:pointer-state))))))
             (saved-transform (copy-seq (simple:transform-matrix window)))
             (saved-identity  (copy-seq (simple:identity-matrix window)))
             (*current-renderer*    window)
             ;; the vanilla port shift (om-LW/draganddrop.lisp: the
             ;; set-graphics-port-coordinates of build-d&d-image) cancels the
             ;; scroll POSI subtracts: the contents are drawn in container coords
             (*pox*                 (- cx (om-point-h posi) (om-h-scroll-position container)))
             (*poy*                 (- cy (om-point-v posi) (om-v-scroll-position container)))
             (*current-view*        container)
             (*current-fg-color*    colors:black)
             (*current-bg-color*    (om-get-bg-color source))
             (*current-font*        (or (om-get-font source) *current-font*))
             (*current-view-height* fh))
        (unwind-protect
             (progn
               (simple:orthographic-matrix fw fh (simple:transform-matrix window))
               (setf (simple:identity-matrix window)
                     (copy-seq (simple:transform-matrix window)))
               (om-draw-contents-for-drag source))
          (replace (simple:transform-matrix window) saved-transform)
          (setf (simple:identity-matrix window) saved-identity))))))

