(in-package :oa)

(export '(om-set-help
          om-help-on?
          om-show-tooltip
          om-hide-tooltip)
        :om-api)

(defvar *helpon* t)

(defun om-set-help (mode)
  (setf *helpon* mode))

(defun om-help-on? () *helpon*)

;;; The balloon itself -- popup, look, geometry, the one-at-a-time rule -- is
;;; alloy-desktop/services/tooltip.lisp (open-tooltip, close-active-tooltip); OM
;;; resolves the help text and measures it.

(defun %split-by-space (s)
  (let ((words nil)
        (start 0))
    (loop for i from 0 below (length s)
          when (char= (char s i) #\Space)
            do (push (subseq s start i) words)
               (setf start (1+ i)))
    (when (< start (length s))
      (push (subseq s start) words))
    (nreverse words)))

(defun %tooltip-wrap-text (text font)
  "TEXT broken for the balloon the way vanilla breaks it: text-wrap-pix at the
balloon width (om-LW/tooltips.lisp: om-show-tooltip, which wraps at 200), so a
tooltip breaks where every other OM label breaks. The font is the one the balloon
is PAINTED in -- Windows draws a help balloon in the status font, the role the
provider aliases to :tooltip -- while vanilla measures in the OM default; copying
that mismatch would break the lines at a width the balloon does not have."
  (text-wrap-pix text font alld:*tooltip-max-width-px*))

(defmethod om-show-tooltip ((self t) &optional (remove nil) (short nil))
  (declare (ignore remove short))
  nil)

(defmethod om-show-tooltip ((self om-graphic-object) &optional (remove nil) (short nil))
  (declare (ignorable remove))
  (when *helpon*
    (let ((spec (om-get-help-spec self))
          (window (om-view-window self)))
      (when window
        (if (and spec (stringp spec) (plusp (length spec)))
            (let* ((text (if short (string-downcase (or (first (%split-by-space spec)) spec)) spec))
                   (cur  (om-mouse-position window))
                   ;; the SAME font the balloon is drawn with: vanilla wraps in the OM
                   ;; default while Windows paints the balloon in the status font
                   ;; (om-LW/tooltips.lisp: om-show-tooltip), and copying that mismatch
                   ;; would break every line at the wrong width
                   (font (%om-native-font :tooltip)))
              (alld:open-tooltip text window
                                 (om-point-h cur)
                                 (- (om-point-v cur) 20)
                                 (lambda (tx) (%tooltip-wrap-text tx font))
                                 (lambda (l) (om-string-size l font)))
              #+darwin (sleep (if remove 0.05 0.1)))
            (alld:close-active-tooltip)))))
  nil)

;; standard dialog items show no OM tooltip (vanilla om-LW/tooltips.lisp:
;; om-show-tooltip and om-hide-tooltip for om-standard-dialog-item)
(defmethod om-show-tooltip ((self om-standard-dialog-item) &optional remove short)
  (declare (ignore remove short))
  nil)

(defmethod om-hide-tooltip ((self t))
  (alld:close-active-tooltip)
  nil)

(defmethod om-hide-tooltip ((self om-standard-dialog-item)) nil)
