(in-package :alloy-desktop)

;;; ---- Text measurement ------------------------------------------------------
;;; Two walks run side by side over a string: ADV sums the face's own advances
;;; (atlas units, kerning included), PEN sums the whole-pixel grid the toolkit's
;;; layout walks (glyph-advance-px over the q6 table).  With no native table the
;;; two coincide.
;;; The toolkit's answers follow a MEASURED rule: when the whole-pixel walk
;;; overshoots the face's own sum, the sum wins ("OpenMusic" is 65.168 at Verdana
;;; 12 where the walk is 68; "Output Files:" is 78.117 in a box of 80 where the
;;; walk totals 80), and when it falls short the walk stands ("iiii" is 12.0,
;;; 4 x 3, where the face's own sum is 13.17).  The per-extent residual rule of
;;; apply-extent-residual is likewise measured, not specified: over 1352 strings it
;;; reproduces the toolkit's extents as well as a fitted model, role by role.
;;; A newline starts a new line: the
;;; answer for a multi-line string is its widest line, the box gp:get-string-extent
;;; measures.  A space advances by the atlas space size, never through the glyph
;;; table (alloy renderers/opengl/msdf.lisp: the space handling in map-glyphs).

(defun string-advance (font px string)
  "Advance width in device px of the widest line of STRING in the realized msdf FONT drawn at PX."
  (org.shirakumo.alloy.renderers.opengl.msdf::partial-allocate font)
  (let* ((data     (org.shirakumo.alloy.renderers.opengl.msdf::data font))
         (scale    (/ (float px 0f0) (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data)))
         (chars    (3b-bmfont::chars data))
         (kernings (3b-bmfont::kernings data))
         (k        (cons nil nil))
         (table    (q6-advances font px))
         (space-em (float (3b-bmfont::space-size data) 0f0))
         (space-px (glyph-advance-px table #\Space (* scale space-em)))
         (adv      0f0)
         (pen      0f0)
         (widest   0f0)
         (prev     nil))
    (flet ((line-width ()
             ;; the toolkit answers the whole-pixel walk only while it
             ;; stays under the face's own sum
             (let ((exact (* scale adv)))
               (if (and table (< pen exact)) pen exact))))
      (loop for ch across (or string "")
            do (cond ((eql ch #\Newline)
                      (setf widest (max widest (line-width))
                            adv 0f0 pen 0f0 prev nil))
                     ((eql ch #\Space)
                      (incf adv space-em)
                      (incf pen space-px)
                      (setf prev ch))
                     (t
                      (let ((gl (gethash ch chars)))
                        (when gl
                          (when prev
                            (setf (car k) prev (cdr k) ch)
                            (incf adv (the single-float (gethash k kernings 0f0))))
                          (incf adv (float (3b-bmfont:glyph-xadvance gl) 0f0)))
                        (incf pen (glyph-advance-px
                                   table ch
                                   (if gl
                                       (* scale (float (3b-bmfont:glyph-xadvance gl) 0f0))
                                       0f0))))
                      (setf prev ch))))
      (max widest (line-width)))))

(defun apply-extent-residual (extents string exact-total space-advance)
  "Fold the excess of the whole-pixel walk in EXTENTS over EXACT-TOTAL back into EXTENTS in place, spaces first down to 2 px each and then 1 px per trailing glyph, so the last extent lands on EXACT-TOTAL."
  (let ((n (length extents)))
    (when (plusp n)
      (let* ((spaces   (count #\Space string :end n))
             (excess   (max 0f0 (- (aref extents (1- n)) exact-total)))
             (capacity (* spaces (max 0f0 (- space-advance 2f0))))
             (given    (min excess capacity))
             (per      (if (plusp spaces) (/ given spaces) 0f0))
             (left     (max 0f0 (- excess given)))
             (trimmed  (max 0 (ceiling (- left 1e-4))))
             (from     (max 0 (- n 1 trimmed)))
             (seen     0))
        (dotimes (i n)
          (when (eql (char string i) #\Space) (incf seen))
          (decf (aref extents i) (* seen per))
          (when (and (plusp trimmed) (<= from i) (< i (1- n)))
            (decf (aref extents i) (float (1+ (- i from)) 0f0))))
        (when (plusp excess)
          (setf (aref extents (1- n)) exact-total)))))
  extents)

(defun char-extents (font px string)
  "Ending x in device px of every character of STRING in the realized msdf FONT drawn at PX, as a fresh vector of single-floats (the gp:compute-char-extents answer, LW-lib/8-1-0-0/manual/html-w/capi/capi-gp-ref-13.htm:60)."
  (org.shirakumo.alloy.renderers.opengl.msdf::partial-allocate font)
  (let* ((data     (org.shirakumo.alloy.renderers.opengl.msdf::data font))
         (scale    (/ (float px 0f0) (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data)))
         (chars    (3b-bmfont::chars data))
         (kernings (3b-bmfont::kernings data))
         (k        (cons nil nil))
         (table    (q6-advances font px))
         (s        (or string ""))
         (n        (length s))
         (space-em (float (3b-bmfont::space-size data) 0f0))
         (space-px (glyph-advance-px table #\Space (* scale space-em)))
         (adv      0f0)
         (pen      0f0)
         (prev     nil)
         (out      (make-array n :element-type 'single-float)))
    ;; ADV walks the face's own advances (atlas units), PEN the grid the
    ;; toolkit's glyph pass walks; with no native table the two coincide
    (dotimes (i n)
      (let ((ch (char s i)))
        (if (eql ch #\Space)
            (progn (incf adv space-em)
                   (incf pen space-px))
            (let ((gl (gethash ch chars)))
              (when gl
                (when prev
                  (setf (car k) prev (cdr k) ch)
                  (incf adv (the single-float (gethash k kernings 0f0))))
                (incf adv (float (3b-bmfont:glyph-xadvance gl) 0f0)))
              (incf pen (glyph-advance-px
                         table ch
                         (if gl
                             (* scale (float (3b-bmfont:glyph-xadvance gl) 0f0))
                             0f0)))))
        (setf prev ch)
        (setf (aref out i) (if table pen (* scale adv)))))
    (when table
      (apply-extent-residual out s (* scale adv) space-px))
    out))

(defun ascender-px (font px)
  "Px from the top of a line box of the realized msdf FONT drawn at PX down to its baseline: the atlas base plus the padding the bake subtracts from it (sdf bmfont.lisp: to-bmfont)."
  (org.shirakumo.alloy.renderers.opengl.msdf::partial-allocate font)
  (let* ((data  (org.shirakumo.alloy.renderers.opengl.msdf::data font))
         (scale (/ (float px 0f0) (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data))))
    (* scale (+ (float (3b-bmfont:base data) 0f0)
                (float (3b-bmfont:padding-down data) 0f0)))))
