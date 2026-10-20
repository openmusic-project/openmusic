(in-package :alloy-desktop)

;;; ---- Text rendering: the msdf renderer hooks and the mnemonic underline ----

(declaim (type single-float
               *mnemonic-underline-lift*
               *mnemonic-underline-bias*))

(defparameter *mnemonic-underline-lift* 0.0
  "Pixels added to the mnemonic underline position (Y-UP alloy: positive moves it UP).")

(defparameter *mnemonic-underline-bias* 100.0
  "Bias added to MSDF distance for underline vertices (saturates opacity to 1.0 in text-shader.glsl, avoids anti-alias degrade).")

(defun underline-regions (flat-markup text-length)
  "Return ((start . end) ...) ascending char-index ranges with :underline style active."
  (let* ((entries (sort (copy-list flat-markup) #'< :key #'car))
         (regions nil)
         (current-start nil))
    (dolist (entry entries)
      (let* ((idx (car entry))
             (styles (cdr entry))
             (has-underline (some (lambda (s)
                                    (or (eq s :underline)
                                        (and (consp s) (eq (car s) :underline))))
                                  styles)))
        (cond ((and has-underline (null current-start))
               (setf current-start (max 0 (min idx text-length))))
              ((and (not has-underline) current-start)
               (let ((e (max 0 (min idx text-length))))
                 (when (< current-start e)
                   (push (cons current-start e) regions)))
               (setf current-start nil)))))
    (when current-start
      (when (< current-start text-length)
        (push (cons current-start text-length) regions)))
    (nreverse regions)))

(declaim (ftype (function (t) (values single-float single-float single-float single-float))
                inside-glyph-uv))
(defun inside-glyph-uv (font-data)
  "Return (values u v du dv) as single-floats for a solidly-inside pixel of one glyph."
  (let* ((chars   (3b-bmfont::chars font-data))
         (scale-w (float (3b-bmfont::scale-w font-data) 0f0))
         (scale-h (float (3b-bmfont::scale-h font-data) 0f0))
         (g (or (gethash #\M chars)
                (gethash #\W chars)
                (gethash #\m chars)
                (loop for v being the hash-values of chars
                      when (and (plusp (3b-bmfont:glyph-width v))
                                (plusp (3b-bmfont:glyph-height v)))
                      return v))))
    (if g
        (values (float (/ (+ (3b-bmfont:glyph-x g)
                             (* 0.5 (3b-bmfont:glyph-width g)))
                          scale-w)
                       0f0)
                (float (/ (+ (3b-bmfont:glyph-y g)
                             (* 0.5 (3b-bmfont:glyph-height g)))
                          scale-h)
                       0f0)
                (/ 1.0 scale-w)
                (/ 1.0 scale-h))
        (values 0.0 0.0 0.0 0.0))))

(defun snap-underline-rows (location array text-length)
  "Put the mnemonic underline quads of ARRAY (after the TEXT-LENGTH glyph quads, six vertices of fifteen floats each) on the device pixel grid and return LOCATION: a 1 px quad straddling two rows is resolved by the window MSAA into two grey rows. The owner settles the widget's own fractional row once the shape is placed."
  (let ((delta (- (fround (alloy:pxy location)) (alloy:pxy location))))
    (unless (zerop delta)
      (loop for i from (1+ (* 90 text-length)) below (length array) by 15
            do (incf (aref array i) delta))))
  location)

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:msdf-line-height
    ((font org.shirakumo.alloy.renderers.opengl.msdf:font) pixel-size)
  "The native line box of FONT, the pitch every vertical metric of this library walks."
  (line-advance font pixel-size))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:msdf-pen-advance
    ((font org.shirakumo.alloy.renderers.opengl.msdf:font) string start end pixel-size)
  "Pen travel on the grid the glyph pass draws on, START and END clamped to STRING."
  (pen-advance (org.shirakumo.alloy.renderers.opengl.msdf::data font)
               string start end pixel-size))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:text-decoration-count
    ((font org.shirakumo.alloy.renderers.opengl.msdf:font) markup text-length)
  "One quad per mnemonic underline region of MARKUP."
  (length (underline-regions markup text-length)))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:decorate-text
    ((font org.shirakumo.alloy.renderers.opengl.msdf:font) text markup pens scale emit)
  "The Windows keyboard cue: one device-pixel row under the advance box of each underline region."
  (declare (type simple-string text)
           (type (simple-array single-float (*)) pens)
           (type single-float scale)
           (type function emit))
  (let* ((text-length (length text))
         ;; the baseline above the pen in the quads' local space: the metric the string
         ;; draw anchors on, so the cue cannot drift from the glyphs it underlines
         ;; the TTF underline-position is NOT used: the Windows cue is the device row right
         ;; below the baseline (LW manual capi/capi-common-elements-ug-1.htm:93-95)
         (baseline-rise (baseline-rise font scale))
         (metrics (font-underline-metrics font))
         ;; one EM in the local space of the emitted quads: the glyph scale is
         ;; SCALE/em, so one EM back is SCALE itself
         (under-thick-local (max 1.0
                                 (if metrics
                                     (* (aref metrics 1) scale (/ 1.0 (aref metrics 2)))
                                     1.0)))
         (color colors:black)
         (outline-color colors:black)
         (bias *mnemonic-underline-bias*))
    (declare (type single-float baseline-rise under-thick-local))
    (flet ((blankp (k)
             (member (char text k) '(#\Space #\Linefeed)))
           (pen-x (k) (aref pens (* 3 k)))
           (pen-y (k) (aref pens (+ 1 (* 3 k))))
           (glyph-x+ (k) (aref pens (+ 2 (* 3 k)))))
      (multiple-value-bind (in-u in-v du dv)
          (inside-glyph-uv (org.shirakumo.alloy.renderers.opengl.msdf::data font))
        (let ((u-l in-u)
              (u-r (+ in-u du))
              (v-t in-v)
              (v-b (+ in-v dv)))
          (dolist (region (underline-regions markup text-length))
            (let* ((start (car region))
                   (end (cdr region))
                   ;; a blank character carries no ink, so it never sets the baseline
                   (pen-y-min (loop with m = nil
                                    for k from start below end
                                    unless (blankp k)
                                      do (setf m (if m (min m (pen-y k)) (pen-y k)))
                                    finally (return m)))
                   ;; the ADVANCE box Windows underlines: first pen to the next one, and
                   ;; the glyph quad only where the region ends the string
                   (x-min (pen-x start))
                   (x-max (cond ((< end text-length) (pen-x end))
                                ((blankp (1- end)) (pen-x (1- end)))
                                (t (glyph-x+ (1- end))))))
              (when pen-y-min
                (let* ((x0 (fround x-min))
                       (x1 (fround x-max))
                       ;; the row below the baseline, on the device grid: a 1 px quad on a
                       ;; fractional row is split in two grey rows by the window MSAA
                       (yb (ffloor (+ (the single-float pen-y-min) baseline-rise)))
                       (under-y-top (+ (- yb 1.0) *mnemonic-underline-lift*))
                       (under-y-bot (- under-y-top under-thick-local)))
                  (when (< x0 x1)
                    (funcall emit x0 under-y-top u-l (- 1 v-t) color 0.0 outline-color bias)
                    (funcall emit x0 under-y-bot u-l (- 1 v-b) color 0.0 outline-color bias)
                    (funcall emit x1 under-y-top u-r (- 1 v-t) color 0.0 outline-color bias)
                    (funcall emit x1 under-y-top u-r (- 1 v-t) color 0.0 outline-color bias)
                    (funcall emit x0 under-y-bot u-l (- 1 v-b) color 0.0 outline-color bias)
                    (funcall emit x1 under-y-bot u-r (- 1 v-b) color 0.0 outline-color bias)))))))))))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:decorated-text-origin
    ((font org.shirakumo.alloy.renderers.opengl.msdf:font) location array text-length)
  "LOCATION, once the mnemonic underline quads of ARRAY sit on whole device rows."
  (snap-underline-rows location array text-length))
