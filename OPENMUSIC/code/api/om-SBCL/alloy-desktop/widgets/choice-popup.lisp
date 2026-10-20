(in-package :alloy-desktop)

;;; Dropdown of a CHOICE control (zoom presets, option-pane items), on the same
;;; native-surface popup chain the menus use: CAPI drops an option-pane on a native
;;; list that the owner window never clips (capi-choice-ug-7.htm:28).

;;; A widget dropdown is a LIST: CAPI caps it at ten visible rows and scrolls past
;;; that. Menus keep their own behaviour, so the whole
;;; mechanism lives in this subclass and never in popup-frame.

(defparameter *choice-popup-max-visible-items* 10
  "Rows a widget dropdown shows before it starts scrolling.")

;;; Rows stay DIRECT children of the frame: popup-frame is the layout and the
;;; focus chain over one shared element vector (alloy focus-tree.lisp: defclass focus-list,
;;; alloy layouts/fixed.lisp: defclass fixed-layout), so scrolling moves their bounds -- draw and hit-test then
;;; read the same geometry -- and the native surface does the clipping.
(defclass choice-popup-frame (popup-frame)
  ((rows       :initform #() :accessor choice-popup-rows)
   (row-height :initform *popup-menu-item-height* :accessor choice-popup-row-height)
   (row-width  :initform 0   :accessor choice-popup-row-width)
   (visible    :initform 0   :accessor choice-popup-visible)
   (top        :initform 0   :accessor choice-popup-top)
   (bar        :initform nil :accessor choice-popup-bar)))

(defclass choice-popup-item (popup-menu-item) ())

(defun choice-popup-max-top (frame)
  (max 0 (- (length (choice-popup-rows frame)) (choice-popup-visible frame))))

(defun choice-popup-scrolling-p (frame)
  (plusp (choice-popup-max-top frame)))

(defun choice-popup-place-rows (frame)
  "Lay the rows out for the current TOP row; the ones outside the visible band land
outside the surface, where the renderer's visibility test drops them."
  (let* ((ih  (choice-popup-row-height frame))
         (h   (* (choice-popup-visible frame) ih))
         (w   (choice-popup-row-width frame))
         (top (choice-popup-top frame)))
    (alloy:with-unit-parent frame
      (loop for item across (choice-popup-rows frame)
            for i from 0
            do (setf (alloy:bounds item)
                     (alloy:px-extent 0 (- h (* (1+ (- i top)) ih)) w ih))))))

(defun choice-popup-scroll-to (frame new-top)
  (let ((top (max 0 (min (choice-popup-max-top frame) (round new-top)))))
    (unless (= top (choice-popup-top frame))
      (setf (choice-popup-top frame) top)
      (choice-popup-place-rows frame)
      (alloy:mark-for-render frame)
      ;; the thumb offset comes from alloy:slider-unit, so the BAR must be marked
      ;; too, not only the rows (same reason as view/scroll.lisp: mark-scrollbars)
      (let ((bar (choice-popup-bar frame)))
        (when bar (alloy:mark-for-render bar)))
      (let ((surface (popup-frame-surface frame)))
        (when surface (alloy:mark-for-render surface))))
    top))

;;; The dropdown's scroller is a real alloy:scrollbar, so it inherits the whole
;;; scrollbar look and behaviour (widgets/chrome.lisp: the define-realization and
;;; define-update of (window alloy:scrollbar)): hover growth, arrows, auto-repeat. Like
;;; scrollbar (view/scroll.lisp: defclass scrollbar) it maps the bar's value onto its host's own
;;; state -- here the first visible row, inverted because alloy's axis grows UP.
(defclass choice-popup-scrollbar (alloy:scrollbar) ()
  (:default-initargs :orientation :vertical :step 1))

(defun choice-popup-bar-frame (sb) (alloy:data sb))

(defmethod alloy:value ((sb choice-popup-scrollbar))
  (let ((frame (choice-popup-bar-frame sb)))
    (- (choice-popup-max-top frame) (choice-popup-top frame))))

(defmethod (setf alloy:value) (v (sb choice-popup-scrollbar))
  (let ((frame (choice-popup-bar-frame sb)))
    (choice-popup-scroll-to frame (- (choice-popup-max-top frame) v)))
  v)

(defmethod alloy:range ((sb choice-popup-scrollbar))
  (cons 0 (choice-popup-max-top (choice-popup-bar-frame sb))))

(defmethod alloy:scroll-handle-size ((sb choice-popup-scrollbar))
  (let ((frame (choice-popup-bar-frame sb)))
    (max 0.01 (min 1.0 (/ (choice-popup-visible frame)
                          (max 1 (length (choice-popup-rows frame))))))))

;;; z-index orders the shapes of the WHOLE tree (alloy presentations/protocol.lisp: alloy:render for (renderer renderable)), and
;;; the popup paints its background at 1000 and its rows up to 1020 (menu/menu.lisp: the define-realization of (window popup-frame) and of (window popup-menu-item)),
;;; while a scrollbar's shapes default to 0. update-shape-arguments is a PROGN
;;; combination (alloy presentations/protocol.lisp: defgeneric update-shape-arguments),
;;; so this only adds the key to the shared update.
(presentations:define-update (window choice-popup-scrollbar)
  (:background :z-index 1030)
  (:handle     :z-index 1031)
  (:arrow-min  :z-index 1032)
  (:arrow-max  :z-index 1032))

;;; alloy's slider consumes the arrows whatever its orientation (alloy components/slider.lisp: handle for (key-down slider)),
;;; which are the keys that walk the rows.
(defmethod alloy:handle ((event alloy:key-down) (sb choice-popup-scrollbar))
  (alloy:decline))

;;; activate would take the :strong focus (alloy focus-tree.lisp: activate for focus-element), leaving the frame
;;; :weak -- and a :weak chain stops routing keys at all (alloy focus-tree.lisp: handle for (input-event focus-chain)).
(defmethod alloy:activate ((sb choice-popup-scrollbar)) sb)

;;; The bar shares the frame's single element vector, so it is in the focus chain as
;;; well;
;;; (menu/menu.lisp: alloy:focus-next :around for popup-frame) only knows how to step
;;; over separators and disabled rows.
(defmethod alloy:focus-next :around ((chain choice-popup-frame))
  (let ((element (call-next-method)))
    (if (typep element 'choice-popup-scrollbar) (call-next-method) element)))

(defmethod alloy:focus-prev :around ((chain choice-popup-frame))
  (let ((element (call-next-method)))
    (if (typep element 'choice-popup-scrollbar) (call-next-method) element)))

(defun choice-popup-add-bar (frame width height)
  "Give FRAME its vertical scroller, in a lane inside the popup-frame ring."
  (let ((bar   (make-instance 'choice-popup-scrollbar :data frame))
        (inset *popup-menu-border-width*))
    (setf (choice-popup-bar frame) bar)
    (alloy:enter bar frame
                 :x (- width inset *scrollbar-track-px*)
                 :y inset
                 :w *scrollbar-track-px*
                 :h (max 1 (- height (* 2 inset))))
    bar))

;;; A trackpad sends fractional deltas, so the wheel moves by row per notch sign.
(defmethod alloy:handle ((event alloy:scroll) (frame choice-popup-frame))
  (if (choice-popup-scrolling-p frame)
      (let ((delta (cond ((plusp (alloy:dy event)) -1)
                         ((minusp (alloy:dy event)) 1)
                         (t 0))))
        (choice-popup-scroll-to frame (+ (choice-popup-top frame) delta))
        t)
      (alloy:decline)))

;;; Focus must bring its row into view. popup-frame's own :around (menu/menu.lisp: alloy:ensure-visible :around for (alloy:layout-element popup-frame))
;;; exists only to block fixed-layout's z-order rotatef, which returning ELEMENT keeps.
(defmethod alloy:ensure-visible :around ((element alloy:layout-element)
                                         (layout choice-popup-frame))
  (let ((i   (position element (choice-popup-rows layout)))
        (top (choice-popup-top layout))
        (vis (choice-popup-visible layout)))
    (when i
      (cond ((< i top) (choice-popup-scroll-to layout i))
            ((>= i (+ top vis)) (choice-popup-scroll-to layout (1+ (- i vis)))))))
  element)

;;; choice-popup-frame inherits the popup-frame ring (menu/menu.lisp: the define-realization of (window popup-frame)); the row
;;; background must clear the ring, like the menu row does.
(presentations:define-realization (window choice-popup-item)
  ((:background simple:rectangle)
   (alloy:margins 4 1 4 1)
   :z-index 1000)
  ((:label simple:text)
   (alloy:margins 6 1) alloy:text
   ;; a row of a popup list is a menu line, and the OS dresses menu lines with
   ;; its menu font, not with the toolkit's control font
   :font (font-entry-font (role-font-entry alloy:renderer :menu))
   :size (font-entry-size-unit (role-font-entry alloy:renderer :menu))
   :halign :start :valign :middle
   :z-index 1020))

(presentations:define-update (window choice-popup-item)
  (:background
   :hidden-p nil
   :pattern (if (alloy:focus alloy:renderable) :notice-background :notice-foreground))
  (:label
   :text alloy:text
   :pattern (if (alloy:focus alloy:renderable) :notice-foreground :foreground)))

(defun popup-open-choice-list (widget rows &key selected-label (min-width 60))
  "Drop ROWS -- (LABEL . THUNK) -- under WIDGET on the shared popup chain. One
native surface, so the owner window never clips it, dropping downwards and
flipping up only at the work-area edge. SELECTED-LABEL gets the row tick. The
surface is sized for at most *choice-popup-max-visible-items* rows; past that the
list scrolls and the rows give a lane to the scrollbar."
  (let ((window (element-window widget)))
    (when (and window rows (not (popup-consume-opener-suppression-p widget)))
      (popup-close-stack window)
      (let* ((ih      *popup-menu-item-height*)
             (count   (length rows))
             (visible (min count *choice-popup-max-visible-items*))
             (height  (* visible ih))
             (width   (max (alloy:pxw (alloy:bounds widget)) min-width))
             (row-w   (if (< visible count)
                          (- width *popup-menu-border-width* *scrollbar-track-px*)
                          width)))
        (multiple-value-bind (ax ay) (popup-drop-anchor window widget height)
          (multiple-value-bind (left top) (popup-fit-screen-rect ax ay width height)
            (let* ((frame (make-instance 'choice-popup-frame
                                         :owner-window window :depth 0
                                         :opener        widget
                                         ;; captured BEFORE the frame takes :strong,
                                         ;; restored by popup-retire-frame
                                         :return-focus  (strong-focus-pane window)
                                         :screen-left   left
                                         :screen-top    top
                                         :screen-width  width
                                         :screen-height height))
                   (surface (popup-attach-surface frame window width height))
                   (items   '())
                   (focus-row nil))
              (setf (choice-popup-row-height frame) ih
                    (choice-popup-row-width frame)  row-w
                    (choice-popup-visible frame)    visible
                    (choice-popup-top frame)        0)
              (dolist (row rows)
                (let* ((label (car row))
                       (thunk (cdr row))
                       (selected (and selected-label (equal label selected-label)))
                       (item (make-instance 'choice-popup-item
                                            :value      label
                                            :enabled-p  t
                                            :selected-p (and selected t)
                                            :on-activate
                                            (lambda ()
                                              (popup-close-stack window)
                                              (funcall thunk)))))
                  (alloy:enter item frame :x 0 :y 0 :w row-w :h ih)
                  (push item items)
                  (when (or (null focus-row) selected) (setf focus-row item))))
              (setf (choice-popup-rows frame) (coerce (nreverse items) 'vector))
              (choice-popup-place-rows frame)
              (when (< visible count)
                (choice-popup-add-bar frame width height))
              ;; the tick'd row must be on screen, so this runs AFTER the rows exist
              (setf (alloy:focused frame) focus-row)
              (popup-present-frame frame surface))))))))
