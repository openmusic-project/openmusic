(in-package :alloy-desktop)

;;; The help balloon: one popup at a time, in its owner window, sized from the
;;; text the host measured and wrapped.

(defclass tooltip-popup (alloy:popup alloy:renderable)
  ((text :initarg :text :initform "" :accessor tooltip-text)))

(defmethod alloy:focus ((self tooltip-popup)) nil)

(defvar *active-tooltip* nil)
(defparameter *tooltip-max-width-px* 200)
(defparameter *tooltip-padding-h-px* 8)
(defparameter *tooltip-padding-v-px* 4)
(defparameter *tooltip-shadow-offset-px* 3)
(defparameter *tooltip-shadow-blur-px* 4)

(defparameter *tooltip-background*
  #+win32 (chroma:rgb 0.96 0.96 0.96 1.0)
  #-win32 (chroma:rgb 0.9 0.9 0.9 1.0)
  "Fill of the balloon: the light gray of the OS help balloon.")

(defparameter *tooltip-line-height-ratio* 1.25f0
  "Line height over font size when the balloon font has no atlas data yet.")

(presentations:define-realization (window tooltip-popup)
  ((:shadow simple:rectangle)
   (alloy:margins 5 6 3 3)
   :feather-radius (alloy:un 1)
   :z-index 1990)
  ((:background simple:rectangle)
   (alloy:margins 5 5 5 5)
   :z-index 2010)
  ((:label simple:text)
   (alloy:margins 13 9 13 9)
   (tooltip-text alloy:renderable)
   ;; a help balloon is OS chrome: Windows draws it in the status font
   ;; (NONCLIENTMETRICS lfStatusFont), the role the provider aliases to :tooltip
   :font (font-entry-font (role-font-entry alloy:renderer :tooltip))
   :size (font-entry-size-unit (role-font-entry alloy:renderer :tooltip))
   :halign :start
   :valign :top
   :wrap nil
   :z-index 2020))

(presentations:define-update (window tooltip-popup)
  (:shadow :hidden-p nil :pattern (chroma:rgb 0.0 0.0 0.0 0.5))
  (:background :hidden-p nil :pattern *tooltip-background*)
  (:label :text (tooltip-text alloy:renderable)))

(defun tooltip-line-height-px (renderer)
  "Whole px between two lines of the balloon drawn under RENDERER."
  (let* ((rend renderer)
         (obj  (and rend (font-entry-font (role-font-entry rend :tooltip))))
         (data (and obj
                    ;; msdf font DATA has no initform (alloy renderers/opengl/msdf.lisp:
                    ;; the data slot of defclass font): it is unbound until the atlas is
                    ;; loaded, and then the px fallback below applies
                    (handler-case
                        (org.shirakumo.alloy.renderers.opengl.msdf::data obj)
                      (unbound-slot (c)
                        (format *error-output* "[tooltip-line-h] ~A~%" (princ-to-string c))
                        nil)))))
    (if data
        ;; the balloon stacks its lines on the pitch every other vertical metric
        ;; walks (graphics/font-metrics.lisp: line-advance), never the baked one
        (ceiling (line-advance obj (role-draw-px rend (native-role :tooltip))))
        (ceiling (* (role-points-px :tooltip) *tooltip-line-height-ratio*)))))

(defun close-active-tooltip ()
  (when *active-tooltip*
    (let ((tt *active-tooltip*))
      (setf *active-tooltip* nil)
      (let ((parent (alloy:layout-parent tt)))
        (when parent (alloy:leave tt parent))))))

(defun open-tooltip (text owner-window x y wrap-lines measure-width)
  "Open the help balloon with its top-left at window point (X,Y) (Y-down).
WRAP-LINES breaks TEXT into the balloon's lines and MEASURE-WIDTH answers the px
width of one line; both are the host's text measurement, called under OWNER-WINDOW's
unit context with *current-renderer* bound to it."
  (close-active-tooltip)
  ;; the MEASUREMENT resolves units too, and a host may open the balloon from
  ;; outside the event pipeline, where *unit-parent* is unbound
  (alloy:with-unit-parent owner-window
    (let* ((*current-renderer* owner-window)
           (lines    (funcall wrap-lines text))
           (joined   (format nil "~{~A~^~%~}" lines))
           (line-h   (tooltip-line-height-px owner-window))
           (max-line (loop for l in lines maximize (funcall measure-width l)))
           (extra    10)
           (width    (+ (* 2 *tooltip-padding-h-px*) max-line extra))
           (height   (+ (* 2 *tooltip-padding-v-px*) (* line-h (length lines)) extra))
           (popup    (make-instance 'tooltip-popup :text joined)))
      ;; Y-down top-left -> Alloy Y-up bottom-left
      (alloy:enter popup owner-window
                   :x (max 0 x)
                   :y (max 0 (- (alloy:pxh (alloy:bounds owner-window)) y height))
                   :w width :h height)
      (alloy:mark-for-render owner-window)
      (setf *active-tooltip* popup)
      popup)))
