(in-package :oa)

;;; In-place completion window: a single-item-list with a vertical scrollbar that
;;; auto-sizes to the matches (capped to a visible window, then scrolls), drawn
;;; tooltip-like — no chrome, a drop shadow — on its OWN undecorated satellite
;;; surface, so it is never clipped by the owner and never takes the input focus.

(defparameter *completion-max-visible-rows* 12)
(defparameter *completion-min-width-px*     120)
(defparameter *completion-max-width-px*     380)
(defparameter *completion-frame-pad*        7
  "Inset (px) between the list and the popup edge, leaving room for the drop shadow.")
(defparameter *completion-bg-color* (om-make-color 0.96 0.96 0.82)
  "Pale cream background for the completion list (LispWorks completion look).")
(defparameter *completion-border-color* (om-make-color 0.5 0.5 0.5)
  "Thin gray frame around the completion popup.")

(defvar *active-completion-popup* nil)

(defclass om-completion-window (om-popup-window) ()
  (:documentation
   "Native surface hosting the in-place completion list. It is a non-focus window
\(capi-capi-ref-369.htm:106): it appears in front of the owner without ever
taking the input focus, so typing keeps reaching the text pane, and being a
top-level window it can never be clipped by the owner."))

(defclass om-completion-popup (alloy:popup
                               alloy:fixed-layout
                               alloy:renderable
                               om-filter-host)
  ((owner-window :initarg :owner-window :initform nil :accessor cp-owner-window)
   (surface      :initarg :surface      :initform nil :accessor cp-surface)
   (scroll       :initarg :scroll       :initform nil :accessor cp-scroll)
   (bar          :initarg :bar          :initform nil :accessor cp-bar)
   (list         :initarg :list         :initform nil :accessor cp-list)
   (clip         :initarg :clip         :initform nil :accessor cp-clip)
   (border       :initarg :border       :initform nil :accessor cp-border)
   ;; the pane being completed, and the text span the completion replaces
   (pane         :initarg :pane         :initform nil :accessor cp-pane)
   (start        :initarg :start        :initform 0   :accessor cp-start)
   (end          :initarg :end          :initform 0   :accessor cp-end)
   ;; thunk recomputing the candidates for the pane's CURRENT text
   (refresh-fn   :initarg :refresh-fn   :initform nil :accessor cp-refresh-fn)
   ;; all candidates, and the subset the filter lets through (what the list shows)
   (all-names    :initarg :all-names    :initform nil :accessor cp-all-names)
   (names        :initarg :names        :initform nil :accessor cp-names)
   ;; the filter is on when its widgets exist, and captures characters while
   ;; enabled (capi-capi-ref-369.htm:154-156)
   (filter-enabled :initform t :accessor cp-filter-enabled)
   ;; screen rectangle of the surface, y-down, and the caret it hangs from
   (screen-left  :initarg :screen-left  :initform 0 :accessor cp-screen-left)
   (caret-below  :initarg :caret-below  :initform 0 :accessor cp-caret-below)
   (caret-above  :initarg :caret-above  :initform 0 :accessor cp-caret-above)
   ;; omcolor overriding the default fill; NIL keeps *completion-bg-color*
   (bg-color     :initarg :bg-color     :initform nil :accessor cp-bg-color)))

(defmethod alld:filter-window ((popup om-completion-popup)) (cp-surface popup))
(defmethod alld:filter-mode-place ((popup om-completion-popup)) :west)

(defclass om-completion-list (om-single-item-list)
  ((cl-names        :initarg :cl-names        :initform nil :accessor cl-names)
   (cl-insert-fn    :initarg :cl-insert-fn    :initform nil :accessor cl-insert-fn)
   (cl-return-focus :initarg :cl-return-focus :initform nil :accessor cl-return-focus)
   (cl-bg-color     :initarg :bg-color        :initform nil :accessor cl-bg-color)))

(defun %completion-fill (color)
  (%omcolor->chroma (or color *completion-bg-color*)))

(defun %completion-label-fill (label)
  "The owning list's override, or the default cream."
  (let ((parent (alloy:layout-parent label)))
    (%completion-fill (and (typep parent 'om-completion-list) (cl-bg-color parent)))))

;; Frame + cream fill live on the POPUP (outside the clip-view), so the border shows
;; on all four sides; the list's own border would be clipped to the viewport. Border is
;; a filled rect behind a 1px-inset fill (outlined rectangles get hidden-p).
;; Shape z is RELATIVE to the renderer z (alloy presentations/protocol.lisp: the incf
;; of simple:z-index in alloy:render for (renderer renderable), inside pushed
;; transforms), so one elevated base here covers rows + the stock scrollbar.
(defmethod alloy:render :around ((renderer alld:window) (popup om-completion-popup))
  (simple:with-pushed-transforms (renderer)
    (incf (simple:z-index renderer) 2000)
    (call-next-method)))

(presentations:define-realization (alld:window om-completion-popup)
  ((:shadow simple:rectangle)
   (alloy:margins 6 8 3 3)
   :feather-radius (alloy:un 1)
   :z-index -3)
  ((:border-bg simple:rectangle)
   (alloy:margins 3 3 3 3)
   :z-index -2)
  ((:background simple:rectangle)
   (alloy:margins 4 4 4 4)
   :z-index -1))

(presentations:define-update (alld:window om-completion-popup)
  (:shadow    :hidden-p nil :pattern (chroma:rgb 0.0 0.0 0.0 0.45))
  (:border-bg :hidden-p nil :pattern (%omcolor->chroma *completion-border-color*))
  (:background :hidden-p nil :pattern (%completion-fill (cp-bg-color alloy:renderable))))

;; The list itself is transparent; the popup paints the frame + fill.
(presentations:define-realization (alld:window om-completion-list)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 2)))

(presentations:define-update (alld:window om-completion-list)
  (:background :hidden-p t)
  (:border :hidden-p t))

;; Completion rows: cream when unselected (matches the popup fill), blue when selected.
(defclass om-completion-label (om-list-label) ())

(defmethod alloy::represent-for ((component om-completion-list) data &rest initargs)
  (apply #'alloy:represent-with 'om-completion-label data initargs))

(presentations:define-realization (alld:window om-completion-label)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:label simple:text)
   (alloy:margins 6 1)
   alloy:text
   :font (%om-font-resolve alloy:renderer (or (om-font alloy:renderable) (%om-native-font :dialog-default)))
   :size (%om-font-draw-unit alloy:renderer (or (om-font alloy:renderable) (%om-native-font :dialog-default)))
   :valign :middle
   :z-index 1))

(presentations:define-update (alld:window om-completion-label)
  (:background
   :pattern (if (alld:list-label-selected-p alloy:renderable)
                :notice-background
                (%completion-label-fill alloy:renderable)))
  (:label
   :pattern (if (alld:list-label-selected-p alloy:renderable)
                :notice-foreground
                :foreground)
   :text alloy:text))

(defun %completion-close ()
  "Retire the in-place window. A GLFW surface must not be destroyed from inside
an input callback, so it is hidden here and reaped by the event loop, the way
alloy-desktop/menu/menu.lisp: popup-retire-frame does it for menus."
  (let ((p *active-completion-popup*))
    (when p
      (setf *active-completion-popup* nil)
      (let ((surface (cp-surface p))
            (owner   (cp-owner-window p)))
        (when surface
          ;; the close path may run twice (key + focus loss), so the popup can already
          ;; be out of the surface; hiding a surface GLFW already reaped is glfw-error
          (handler-case (alloy:leave p surface)
            (alloy:hierarchy-error (c)
              (format *error-output* "[completion-close] ~A~%" (princ-to-string c))))
          (handler-case (alld:hide-window surface)
            (%glfw:glfw-error (c)
              (format *error-output* "[completion-close] ~A~%" (princ-to-string c))))
          (setf (%glfw:should-close-p surface) t)
          (setf (cp-surface p) nil))
        (when owner (alloy:mark-for-render owner))))))

;; The active completion must never outlive its surface: once the surface is gone,
;; the next press in any window would retire it through a dead native handle.
(defmethod alloy:deallocate :before ((surface om-completion-window))
  (let ((popup *active-completion-popup*))
    (when (and popup (eq surface (cp-surface popup)))
      (%completion-close))))

(defun %completion-list-move (self delta)
  "Move the committed selection by DELTA rows, scrolling it into view."
  (let* ((n   (alloy:element-count self))
         (cur (alld:committed-index self))
         (new (cond ((null cur) (if (plusp delta) 0 (1- n)))
                    (t (max 0 (min (1- n) (+ cur delta)))))))
    (when (and (plusp n) (not (eql new cur)))
      (setf (alld:committed-index self) new)
      (alld:mark-list-children-for-render self)
      (let ((row (alloy:index-element new self))
            (cv  (alld:find-ancestor-clip-view self)))
        (when (and row cv) (alloy:ensure-visible row cv))))))

(defun %completion-list-accept (self)
  ;; idempotent: a click's row-activate and the ENTER key-down can both reach here
  (when *active-completion-popup*
    (let ((idx (alld:committed-index self)))
      (when (and idx (cl-insert-fn self))
        (funcall (cl-insert-fn self) idx)))
    (%completion-close)))

(defun %completion-list-cancel (self)
  (let ((pane (cl-return-focus self)))
    (%completion-close)
    (when pane (%om-focus-pane nil pane))))

;; Navigation is driven by the focus action-events the focused list receives
;; (alloy actions.lisp: the define-action-event forms from focus-next to exit), so the selection highlight follows the arrows and the
;; default focus-list child navigation is pre-empted (no call-next-method).
(defmethod alloy:handle ((event alloy:focus-down) (self om-completion-list))
  (%completion-list-move self 1) t)
(defmethod alloy:handle ((event alloy:focus-up) (self om-completion-list))
  (%completion-list-move self -1) t)
;; RET/TAB/ESC double-dispatch (an action-event then a raw key-down): consume the
;; action-events as no-ops and do the work on the key-down, so neither reaches the pane
;; after the popup closes and refocuses it (otherwise ENTER would EVAL / insert a newline).
(defmethod alloy:handle ((event alloy:activate) (self om-completion-list)) t)
(defmethod alloy:handle ((event alloy:focus-next) (self om-completion-list)) t)
(defmethod alloy:handle ((event alloy:exit) (self om-completion-list)) t)

(defmethod alloy:handle ((event alloy:key-down) (self om-completion-list))
  (case (alloy:key event)
    ((:enter :return :kp-enter :tab) (%completion-list-accept self) t)
    (:escape (%completion-list-cancel self) t)
    (t (call-next-method))))

;; The focused list receives every window pointer-event via the focus-tree (ui.lisp
;; routes there first), so it must ROUTE instead of consuming: inside the clip it
;; consumes (row hover would move selection and auto-scroll), over the scrollbar it
;; forwards (hover drives the fork's grow/arrows animation), inside the popup frame
;; it consumes, elsewhere it declines to the layout tree.
(defun %completion-pointer-inside-p (self event)
  (let ((cv (alld:find-ancestor-clip-view self)))
    (and cv (alloy:contained-p (alloy:location event) cv))))

(defmethod alloy:handle ((event alloy:pointer-move) (self om-completion-list))
  (let* ((popup *active-completion-popup*)
         (bar   (and popup (cp-bar popup))))
    (cond ((%completion-pointer-inside-p self event)
           (unless (eql (alloy:focus self) :strong)
             (%om-focus-pane nil self))
           t)
          ((and bar (alloy:contained-p (alloy:location event) bar))
           (alloy:handle event bar))
          ((and popup (alloy:contained-p (alloy:location event) popup))
           t)
          (t (alloy:decline)))))

;; Alloy's focus-driven activate picks an unrelated row on clicks, so hit-test the
;; event location. Single click = select (highlight); double click = accept (= ENTER).
(defun %completion-row-index-at (self pt)
  (let ((hit nil))
    (alloy:with-unit-parent self
      (alloy:do-elements (row self)
        (let ((g (alloy:global-location row))
              (b (alloy:bounds row)))
          (when (and (<= (alloy:pxx g) (alloy:pxx pt) (+ (alloy:pxx g) (alloy:pxw b)))
                     (<= (alloy:pxy g) (alloy:pxy pt) (+ (alloy:pxy g) (alloy:pxh b))))
            (setf hit row)))))
    (and hit (alloy:element-index hit self))))

(defmethod alloy:handle ((event alloy:pointer-down) (self om-completion-list))
  (if (%completion-pointer-inside-p self event) t (alloy:decline)))

(defmethod alloy:handle ((event alloy:pointer-up) (self om-completion-list))
  (if (%completion-pointer-inside-p self event)
      (let ((idx (%completion-row-index-at self (alloy:location event))))
        (when idx
          (setf (alld:committed-index self) idx)
          (alld:mark-list-children-for-render self))
        t)
      (alloy:decline)))

(defmethod alloy:handle ((event alloy:pointer-double-click) (self om-completion-list))
  (if (%completion-pointer-inside-p self event)
      (let ((idx (%completion-row-index-at self (alloy:location event))))
        (when idx
          (setf (alld:committed-index self) idx)
          (%completion-list-accept self))
        t)
      (alloy:decline)))

;; Hovering the bar makes it the chain-focused element; completion keys must
;; still reach the list (slider ignores :enter and the bubbling would cycle).
(defmethod alloy:handle :around ((event alloy:key-down) (sb alloy:scrollbar))
  (let ((popup *active-completion-popup*))
    (if (and popup (eql sb (cp-bar popup)) (cp-list popup)
             (member (alloy:key event) '(:enter :return :kp-enter :tab :escape)))
        (alloy:handle event (cp-list popup))
        (call-next-method))))

;; ESC emits an exit action-event before the raw key-down; consumed as
;; a no-op here, else (exit foc) demotes the chain and the raw ESC never lands.
(defmethod alloy:handle :around ((event alloy:exit) (chain alloy:focus-list))
  (let ((popup *active-completion-popup*))
    (if (and popup (eql chain (cp-scroll popup)))
        t
        (call-next-method))))

;; Focus changes on completion widgets must not scroll the viewport (component's
;; (setf focus) :after calls ensure-visible on every truthy transition); the
;; arrows' explicit ensure-visible in %completion-list-move stays outside the flag.
(defvar *completion-suppress-ensure* nil)

(defmethod (setf alloy:focus) :around (value (self om-completion-list))
  (declare (ignorable value))
  (let ((*completion-suppress-ensure* t)) (call-next-method)))

(defmethod (setf alloy:focus) :around (value (self om-completion-label))
  (declare (ignorable value))
  (let ((*completion-suppress-ensure* t)) (call-next-method)))

(defmethod alloy:ensure-visible :around ((e alloy:layout-element) (c alloy:clip-view))
  (if *completion-suppress-ensure* e (call-next-method)))

;;; The in-place window lives on its own surface, so a press anywhere in the
;;; owner dismisses it (capi-dialogs-ug-6.htm:130). Presses on the satellite
;;; surfaces — the completion window itself and any menu it drops — must not.
(defmethod alloy:handle :before ((event alloy:pointer-down) (window om-look-window))
  (when (and *active-completion-popup*
             (not (typep window 'alld:popup-window)))
    (%completion-close)))

(defun %completion-list-width-px (names)
  (let ((w (loop for nm in names maximize (ceiling (om-string-size nm (%om-native-font :dialog-default))))))
    (max *completion-min-width-px*
         (min *completion-max-width-px* (+ w 18)))))

(defun %completion-row-metrics (popup width)
  "(values CONTENT-H ROW-H) of POPUP's list at WIDTH. Rows are font/margin/
base-scale sized (alloy un), so both come from the measured suggest-size."
  (let* ((list (cp-list popup))
         (n    (alloy:element-count list))
         (content-h (alloy:pxh (alloy:suggest-size (alloy:px-size width 20) list))))
    (values content-h (if (plusp n) (/ content-h n) content-h))))

(defun %completion-chrome-height (popup)
  "Px the filter takes from the list: the filter field above plus the mode strip
below. Zero while the filter is off (capi-capi-ref-369.htm:132)."
  (if (alld:filter-bar popup)
      (+ alld:*filter-field-height* alld:*filter-bar-height*)
      0))

(defun %completion-wanted-height (popup width)
  "Height the popup asks for: frame padding, the filter chrome, and the visible
slice of the list capped at *completion-max-visible-rows*."
  (multiple-value-bind (content-h row-h) (%completion-row-metrics popup width)
    (let ((pad (* 2 *completion-frame-pad*))
          (chrome (%completion-chrome-height popup)))
      (values (+ pad chrome
                 (max row-h (min content-h (* *completion-max-visible-rows* row-h))))
              content-h row-h))))

(defun %completion-place (popup width height)
  "Screen rectangle (values LEFT TOP HEIGHT) for the in-place window, y-down.
CAPI hangs it below the caret and flips it above when the bottom does not fit
\(capi-capi-ref-369.htm:118); whatever is left is clamped to the work area."
  (let* ((left   (cp-screen-left popup))
         (below  (cp-caret-below popup))
         (above  (cp-caret-above popup))
         ;; Monitor may have been unplugged since the lookup
         (work   (let ((m (alld::%popup-monitor-for-screen-point left below)))
                   (and m (handler-case (%glfw:work-area m)
                            (alld::%glfw-unavailable () nil)))))
         (top    below)
         (h      height))
    (when work
      (destructuring-bind (wx wy ww wh) work
        (declare (ignore wx ww))
        (let ((room-below (- (+ wy wh) below))
              (room-above (- above wy)))
          (cond ((<= h room-below) (setf top below))
                ((<= h room-above) (setf top (- above h)))
                ((>= room-below room-above)
                 (setf top below h (max 1 room-below)))
                (t (setf h (max 1 room-above) top (- above h)))))))
    (multiple-value-bind (l tp) (alld:popup-fit-screen-rect left top width h)
      (values l tp h))))

(defun %completion-layout (popup width height)
  "Lay the frame out inside a surface of WIDTH x HEIGHT: filter field on top,
list in the middle, filter mode strip at the bottom, all inside the padding."
  (let* ((pad   *completion-frame-pad*)
         (inner-w (- width (* 2 pad)))
         (field (alld:filter-field popup))
         (bar   (alld:filter-bar popup))
         (fh    (if field alld:*filter-field-height* 0))
         (bh    (if bar alld:*filter-bar-height* 0))
         (clip-h (- height (* 2 pad) fh bh)))
    (setf (alloy:bounds popup) (alloy:px-extent 0 0 width height))
    (when field
      (setf (alloy:bounds field)
            (alloy:px-extent pad (- height pad fh) inner-w fh)))
    (setf (alloy:bounds (cp-border popup))
          (alloy:px-extent pad (+ pad bh) inner-w clip-h))
    (when bar
      (setf (alloy:bounds bar) (alloy:px-extent pad pad inner-w bh)))
    (alloy:refit (cp-border popup))
    (when bar (alloy:refit bar))
    clip-h))

(defun %completion-resize (popup)
  "Re-measure the list, resize the surface and re-lay the frame out. The surface
keeps its width so typing never makes the window jitter sideways."
  (let ((surface (cp-surface popup)))
    (when surface
      (alloy:with-unit-parent surface
        (let* ((*current-renderer* surface)
               (width (alloy:pxw (alloy:bounds popup))))
          (multiple-value-bind (want content-h row-h)
              (%completion-wanted-height popup (- width (* 2 *completion-frame-pad*)))
            (declare (ignore row-h))
            (multiple-value-bind (left top height) (%completion-place popup width want)
              (let ((size (%glfw:size surface)))
                (unless (and (= (first size) width) (= (second size) (round height)))
                  (setf (%glfw:size surface) (list (round width) (round height)))))
              (setf (%glfw:location surface) (list (round left) (round top)))
              (setf (slot-value surface 'alloy:bounds)
                    (alloy:px-extent 0 0 width height))
              (alloy:suggest-size (alloy:px-extent 0 0 width height) surface)
              (let ((m (simple:transform-matrix surface)))
                (simple:orthographic-matrix width height m)
                (setf (simple:identity-matrix surface) (copy-seq m)))
              (let ((clip-h (%completion-layout popup width height)))
                (declare (ignorable clip-h))
                (%completion-sync-scrollbar popup content-h)))))
        (setf (alloy:offset (cp-clip popup))
              (alloy:px-point 0 most-negative-single-float))
        (alloy:mark-for-render surface)))))

(defun %completion-sync-scrollbar (popup content-h)
  "Show the vertical scrollbar exactly while the content overflows the viewport."
  (let* ((border (cp-border popup))
         (clip   (cp-clip popup))
         (bar    (cp-bar popup))
         (need   (> content-h (+ (alloy:pxh (alloy:bounds clip)) 0.5))))
    (cond ((and need (null bar))
           (let ((new (alloy:represent-with 'alloy:y-scrollbar clip)))
             (alloy:enter new border :place :east :size (alloy:px alld:*scrollbar-track-px*))
             (alloy:enter new (cp-scroll popup))
             (setf (cp-bar popup) new)
             (alloy:refit border)))
          ((and (not need) bar)
           ;; a refill may already have taken the bar out of one of the two parents
           (handler-case
               (progn (alloy:leave bar border)
                      (alloy:leave bar (cp-scroll popup)))
             (alloy:hierarchy-error (c)
               (format *error-output* "[completion scrollbar] ~A~%" (princ-to-string c))))
           (setf (cp-bar popup) nil)
           (alloy:refit border)))))

(defun %completion-visible-names (popup)
  "The candidates the filter lets through (capi-dialogs-ug-6.htm:95)."
  (remove-if-not (lambda (name) (alld:filter-pass-p popup name))
                 (cp-all-names popup)))

(defun %completion-refill (popup)
  "Push the currently visible candidates into the list and re-fit the window."
  (let* ((list  (cp-list popup))
         (names (%completion-visible-names popup)))
    (cond ((null names) (%completion-close))
          (t
           (setf (cp-names popup) names
                 (cl-names list) names)
           (om-set-item-list list names)
           (setf (alld:committed-index list) 0)
           (alld:mark-list-children-for-render list)
           (%completion-resize popup)
           (%completion-update-matches popup)))))

(defun %completion-update-matches (popup)
  (let ((label (alld:filter-matches popup)))
    (when label
      (om-set-dialog-item-text
       label (format nil "~D" (length (cp-names popup)))))))

(defmethod alld:filter-apply ((popup om-completion-popup))
  (%completion-refill popup))

(defun %completion-recompute (popup)
  "CAPI recomputes the candidates on every change to the completed text
\(capi-capi-ref-488.htm:183, capi-dialogs-ug-6.htm:126). NIL and :destroy close
the window, exactly as the kernel's in-place function asks (kernel controls.lisp: the :in-place-completion-function of open-ttybox)."
  (let ((fn (cp-refresh-fn popup)))
    (when fn
      (multiple-value-bind (names start end) (funcall fn)
        (cond ((or (null names) (eq names :destroy) (stringp names))
               (%completion-close))
              ((and (consp names) (every #'stringp names))
               (setf (cp-all-names popup) names
                     (cp-start popup) (or start (cp-start popup))
                     (cp-end popup)   (or end (cp-end popup)))
               (let ((list (cp-list popup)))
                 (setf (cl-insert-fn list)
                       (%completion-insert-fn popup)))
               (%completion-refill popup))
              (t nil))))))

(defun %completion-insert-fn (popup)
  (lambda (idx)
    (%insert-completion (cp-pane popup) (cp-start popup) (cp-end popup)
                        (nth idx (cp-names popup)))))

(defun %open-completion-popup (pane window names start end &key bg-color refresh-fn)
  "Raise the in-place completion window at the symbol start. NAMES is the full
match list, BG-COLOR an omcolor overriding the default cream fill, REFRESH-FN a
thunk recomputing (values NAMES START END) for the pane's current text.
The window is a non-focus satellite surface: it never takes the input focus and,
being top-level, is never clipped by WINDOW (capi-capi-ref-369.htm:106)."
  (%completion-close)
  ;; om-string-size resolves units (to-px/un) which need *unit-parent* bound.
  (alloy:with-unit-parent window
    (multiple-value-bind (ax ay caret-h) (%completion-anchor pane start)
      (let* ((*current-renderer* window)
             (winh (alloy:pxh (alloy:bounds window)))
             (ax   (max 0 (or ax 40)))
             (ay   (or ay (- winh 60)))
             (pad  *completion-frame-pad*)
             (lw   (%completion-list-width-px names))
             (pw   (+ lw (* 2 pad)))
             (popup (make-instance 'om-completion-popup
                                   :owner-window window
                                   :bg-color bg-color
                                   :pane pane :start start :end end
                                   :refresh-fn refresh-fn
                                   :all-names names :names names)))
        (multiple-value-bind (sx sy) (alld:popup-owner-point-to-screen window ax ay)
          (setf (cp-screen-left popup) sx
                (cp-caret-below popup) sy
                (cp-caret-above popup) (- sy (or caret-h 0))))
        (let ((surface (alld:make-popup-window window
                                            (cp-screen-left popup)
                                            (cp-caret-below popup)
                                            pw 40
                                            :class 'om-completion-window
                                            :resizable-p t)))
          (setf (cp-surface popup) surface)
          (%completion-build popup pw lw)
          (%completion-resize popup)
          (setf *active-completion-popup* popup)
          (alloy:render surface surface)
          (alld:show-window-without-focus surface)
          (alloy:mark-for-render window)
          popup)))))

(defun %completion-build (popup width list-width)
  "Populate POPUP's surface: the frame, the clipped list and its focus list.
The focus list hangs off the SURFACE's tree, never the owner's, so the pane
keeps the strong focus while the window is up."
  (let* ((surface (cp-surface popup))
         (names   (cp-names popup))
         (pane    (cp-pane popup))
         (list    (om-make-dialog-item 'om-completion-list
                                       (om-make-point 0 0)
                                       (om-make-point list-width 20) ""
                                       :focus nil
                                       :range names))
         (border  (make-instance 'alloy:border-layout))
         (foc     (make-instance 'alloy:focus-list
                                 :focus-parent (alld:window-focus-root surface)))
         (clip    (make-instance 'alloy:clip-view :limit :x)))
    (setf (cl-names list)        names
          (cl-bg-color list)     (cp-bg-color popup)
          (cl-return-focus list) pane)
    (setf (cp-list popup) list
          (cp-clip popup) clip
          (cp-border popup) border
          (cp-scroll popup) foc)
    (setf (cl-insert-fn list) (%completion-insert-fn popup))
    (alloy:enter popup surface :x 0 :y 0 :w width :h 40)
    (alloy:enter clip border :place :center)
    (alloy:enter list clip)
    (alloy:enter list foc)
    (alloy:enter border popup :x *completion-frame-pad* :y *completion-frame-pad*
                              :w list-width :h 10)
    (setf (alld:committed-index list) 0)
    (alld:mark-list-children-for-render list)
    ;; notice-focus signals element-not-contained when LIST is not in the chain yet
    (handler-case (%om-focus-pane surface list)
      (alloy:hierarchy-error (c)
        (format *error-output* "[completion focus] ~A~%" (princ-to-string c))))
    popup))

(defun %show-completion-popup (pane &key (package oa::*om-default-package*) (min-start 0))
  (let* ((text (or (alloy:value pane) ""))
         (pos  (alloy:pos (alloy:cursor pane))))
    (multiple-value-bind (start end) (%symbol-bounds-at-point text pos :min-start min-start)
      (when start
        (let ((names (oa::om-completions (subseq text start end) :package package)))
          (cond ((null names) nil)
                ((null (rest names))
                 (%insert-completion pane start end (first names))
                 t)
                (t
                 (let ((window (om-view-window pane)))
                   (when window
                     (%open-completion-popup
                      pane window names start end
                      :refresh-fn (lambda () (%symbol-completions-at-point
                                              pane package min-start)))
                     t)))))))))

(defun %symbol-completions-at-point (pane package min-start)
  "(values NAMES START END) for the symbol the caret sits in, or NIL when there
is none — which closes the in-place window (capi-capi-ref-488.htm:183)."
  (let* ((text (or (alloy:value pane) ""))
         (pos  (alloy:pos (alloy:cursor pane))))
    (multiple-value-bind (start end) (%symbol-bounds-at-point text pos :min-start min-start)
      (if (null start)
          nil
          (values (oa::om-completions (subseq text start end) :package package)
                  start end)))))

;;; --- the filter, added and removed by Control+Return (capi-capi-ref-369.htm:154)

(defun %completion-filter-on-p (popup)
  (and popup (alld:filter-bar popup) t))

(defun %completion-filter-capturing-p (popup)
  "While the filter is visible AND enabled it captures characters and Backspace;
disabled, they go back to the input pane (capi-dialogs-ug-6.htm:97,110-112)."
  (and (%completion-filter-on-p popup) (cp-filter-enabled popup)))

(defun %completion-add-filter (popup)
  "Add the filter: a text field above the list and the mode strip in the footer.
Its text is reset and it is always enabled (capi-capi-ref-369.htm:154)."
  (let* ((surface (cp-surface popup))
         (focus   (cp-scroll popup))
         (field   (%filter-make-field popup))
         (mode    (%filter-make-mode-button popup))
         (clear   (%filter-make-clear-button popup))
         (matches (om-make-dialog-item 'om-static-text (om-make-point 0 0)
                                       (om-make-point 40 20) "0"
                                       :font (%om-native-font :dialog-default)
                                       :bg-color *om-light-gray-color*
                                       :fg-color *om-black-color*))
         (bar     (make-instance 'alloy:border-layout)))
    (when surface
      (setf (alld:filter-text popup) "")
      (setf (alloy:cell-margins bar) (alloy:margins 1))
      (alloy:enter mode bar :place :west
                   :size (alloy:un (%filter-mode-width (om-dialog-item-text mode))))
      (alloy:enter matches bar :place :east :size (alloy:un 40))
      (alloy:enter clear bar :place :center)
      (alloy:enter field popup :x 0 :y 0 :w 10 :h alld:*filter-field-height*)
      (alloy:enter bar popup :x 0 :y 0 :w 10 :h alld:*filter-bar-height*)
      (dolist (widget (list field mode clear)) (alloy:enter widget focus))
      (setf (alld:filter-field popup) field
            (alld:filter-mode-menu popup) mode
            (alld:filter-clear popup) clear
            (alld:filter-matches popup) matches
            (alld:filter-bar popup) bar
            (cp-filter-enabled popup) t)
      (%completion-refill popup))))

(defun %completion-remove-filter (popup)
  "Remove the filter and give the list back the space it occupied."
  (let ((field (alld:filter-field popup))
        (bar   (alld:filter-bar popup))
        (focus (cp-scroll popup)))
    ;; the widgets may already have left with their bar when the popup was refilled
    (handler-case
        (progn
          (dolist (widget (list field (alld:filter-mode-menu popup) (alld:filter-clear popup)))
            (when (and widget (eq (alloy:focus-parent widget) focus))
              (alloy:leave widget focus)))
          (when field (alloy:leave field popup))
          (when bar (alloy:leave bar popup)))
      (alloy:hierarchy-error (c)
        (format *error-output* "[completion filter remove] ~A~%" (princ-to-string c))))
    (setf (alld:filter-field popup) nil
          (alld:filter-mode-menu popup) nil
          (alld:filter-clear popup) nil
          (alld:filter-matches popup) nil
          (alld:filter-bar popup) nil
          (alld:filter-text popup) "")
    (%completion-refill popup)))

(defun %completion-toggle-filter (popup)
  "Control+Return destroys a filter that is on and adds one when none is present
\(capi-capi-ref-369.htm:154, capi-dialogs-ug-6.htm:87-90)."
  (if (%completion-filter-on-p popup)
      (%completion-remove-filter popup)
      (%completion-add-filter popup)))

(defun %completion-toggle-enable-filter (popup)
  "Control+Shift+Return; no effect while the filter is off (capi-capi-ref-281.htm:51)."
  (when (%completion-filter-on-p popup)
    (setf (cp-filter-enabled popup) (not (cp-filter-enabled popup)))))

(defun %completion-filter-set-text (popup text)
  (let ((field (alld:filter-field popup)))
    (when field
      (om-set-dialog-item-text field text)
      (setf (alld:filter-text popup) text)
      (%completion-refill popup))))

(defun %completion-filter-insert (popup string)
  (%completion-filter-set-text
   popup (concatenate 'string (alld:filter-text popup) string)))

(defun %completion-filter-backspace (popup)
  (let ((text (alld:filter-text popup)))
    (when (plusp (length text))
      (%completion-filter-set-text popup (subseq text 0 (1- (length text)))))))

;;; --- gestures the focused input pane hands to the non-focus window
;;; (capi-dialogs-ug-6.htm:77-108). PageUp/PageDown are deliberately absent: the
;;; user measured that they do nothing in the original OM.

(defun %completion-route-key (pane event)
  "T when the in-place window consumed EVENT on PANE's behalf."
  (let ((popup *active-completion-popup*))
    (when (and popup (eq pane (cp-pane popup)) (cp-list popup))
      (let* ((key   (alloy:key event))
             (mods  (alloy:modifiers event))
             (ctrl  (find :control mods))
             (shift (find :shift mods))
             (list  (cp-list popup))
             (enter (member key '(:enter :return :kp-enter))))
        (cond
          ((and ctrl shift enter) (%completion-toggle-enable-filter popup) t)
          ((and ctrl enter)       (%completion-toggle-filter popup) t)
          ((and ctrl shift (alld:filter-chord-axis event))
           (%filter-toggle popup (alld:filter-chord-axis event)) t)
          ((eq key :up)     (%completion-list-move list -1) t)
          ((eq key :down)   (%completion-list-move list 1) t)
          ((eq key :escape) (%completion-list-cancel list) t)
          ((or enter (eq key :tab)) (%completion-list-accept list) t)
          ((and (eq key :backspace) (%completion-filter-capturing-p popup))
           (%completion-filter-backspace popup) t)
          (t nil))))))

(defun %completion-after-key (pane event)
  "Recompute after a gesture the pane handled itself: CAPI updates the list on
any change to the text or the cursor (capi-dialogs-ug-6.htm:126)."
  (let ((popup *active-completion-popup*))
    (when (and popup (eq pane (cp-pane popup))
               (member (alloy:key event)
                       '(:backspace :delete :left :right :home :end)))
      (%completion-recompute popup))))

(defmethod alloy:handle :around ((event alloy:text-event) (pane alloy:text-input-component))
  (let ((popup *active-completion-popup*))
    (cond ((and popup (eq pane (cp-pane popup))
                (%completion-filter-capturing-p popup))
           (%completion-filter-insert popup (alloy:text event))
           t)
          (t
           (multiple-value-prog1 (call-next-method)
             (let ((live *active-completion-popup*))
               (when (and live (eq pane (cp-pane live)))
                 (%completion-recompute live))))))))

