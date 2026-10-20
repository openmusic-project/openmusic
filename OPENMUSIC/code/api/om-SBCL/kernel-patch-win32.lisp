(in-package :om)

;;; Preferences and About: geometry polish
;;;
;;; The nine preference panes and the About splash are laid out in absolute OM units by
;;; kernel graphics/default-prefs.lisp (make-new-pref-scroll for :general and :appearance),
;;; kernel graphics/dialogs.lisp (make-splash-view), projects midi/tools/midi-preferences.lisp
;;; (make-new-pref-scroll for :midi), projects musicproject/editor/musicpreferences.lisp
;;; (make-new-pref-scroll for :score and :conversion) and projects
;;; omsounds/general/sound-preferences.lisp (make-new-pref-scroll for :audio).  A number of
;;; those items overlap a neighbour, reach past their page, or carry a box too short for the
;;; text they draw.  Those sources are read-only, so the corrected geometry lives here, as
;;; data, applied to the pane the kernel has just built through the public accessors.
;;;
;;; This file is loaded on win32 only.  The geometry those sources write holds on the
;;; other systems: it is the dialog font of Windows that draws wider than they allow for,
;;; and correcting for it anywhere else would move widgets that already fit.

(defparameter *om-pref-polish-table*
  '((:general
     ((om-static-text "Reset paths:" 620 300)                              :w 120)
     ((om-static-text "(Catch Lisp erros and display a simple message window)" 20 90)
                                                                           :w 360)
     ((om-check-box " Handle Error Messages" 20 70)                        :h 18)
     ((om-check-box " Enable Evaluation Process" 20 120)                   :h 18)
     ((om-check-box " Tooltips" 20 170)                                    :h 18)
     ((om-check-box " Keep Listener in Front" 20 200)                      :h 18)
     ((om-check-box " Enable Listener input" 20 230)                       :h 18)
     ((om-check-box " Enable reactivity" 20 260)                           :h 18))
    (:appearance
     ((om-static-text "Default Presentation" 40 75)                        :w 130)
     ((om-button "Color" 240 310)                                          :x 245)
     ((om-static-text "patch" 582 217)                                     :y 224 :w 64)
     ((om-static-text "maquette" 640 217)                                  :y 224 :w 64)
     ((om-static-text "instance" 702 217)                                  :y 224 :w 64))
    (:score
     ((tonaloptions-view nil 590 70)                                       :w 160)
     ((om-color-view nil 160 267)                                          :w 60))
    (:fluid
     ((om-icon-button nil 210 125)                                         :x 157)
     ((om-icon-button nil 210 165)                                         :x 157)
     ((om-static-text nil 250 127)                                  :x 197 :y 130))
    (:conversion
     ((om-static-text "(Maximum interval for grouping notes in a same chord)" 20 85)
                                                                           :y 92))
    (:midi
     ((om-static-text "Applies to all new score boxes" 20 55)              :y 58)
     ((om-static-text "Force score player:" 20 77)                         :y 84)
     ((om-static-text "Changes the score player of existing boxes" 20 102) :y 109)
     ((om-static-text "In case of emergency:" 20 246)                      :y 250)
     ((om-button "Restart" 200 246)                                        :y 250)
     ((om-static-text "With 4 channels  semi-tones remain on ch.1, 1/8th tones shift to ch.2, 1/4th tones to ch.3 and 3/8th tones to ch.4" 400 194)
                                                                           :y 210)
     ((om-static-text "Auto microtone bend:" 400 234)                      :y 264 :w 140)
     ((om-check-box "" 600 234)                                            :y 264)
     ((om-static-text "Applies 1/8th tone pitchbend to channels 1-4 during playback" 400 259)
                                                                           :y 283 :w 380)
     ((om-static-text "[works only with objects using a global MIDI port]" 400 275)
                                                                           :y 301))
    (:audio
     ((om-pop-up-dialog-item nil 190 50)                                   :w 200)
     ((om-pop-up-dialog-item nil 190 80)                                   :w 200)
     ((om-pop-up-dialog-item nil 190 110)                                  :w 200)
     ((om-pop-up-dialog-item nil 190 140)                                  :w 200)
     ((om-static-text "Synthesis / Processing outputs" 400 20)             :w 290)
     ((om-static-text "Level (dB)" 600 200)                                :w 70)
     ((om-editable-text nil 680 200)                                       :h 24)
     ((om-static-text "(Redirects outputs to the OM Listener)" 400 250)    :y 253))
    (:externals
     ((om-clickable-static-text nil 20 60)                          :w 520 :h 24)
     ((om-icon-button nil 270 60)                                          :x 544)
     ((om-button "Options" 300 60)                                         :x 574)
     ((om-clickable-static-text nil 20 125)                         :w 520 :h 24)
     ((om-icon-button nil 270 125)                                         :x 544)
     ((om-button "Options" 300 125)                                        :x 574)
     ((om-clickable-static-text nil 20 190)                         :w 520 :h 24)
     ((om-icon-button nil 270 190)                                         :x 544))
    (:about
     ((om-static-text nil 390 20)                                          :h 465)
     ((om-static-text nil 58 435)                                          :w 320))
    ("OpenMusic - Workspaces"
     (:window                                                              :w 520)
     ((om-view nil 20 20)                                                  :w 480)
     ((om-static-text nil 15 10)                                    :y 16 :w 450)
     ((om-static-text nil 25 30)                                    :y 42 :w 435)
     ((om-radio-button " Open previous workspace" 20 100)                  :y 106)
     ((om-static-text nil 40 124)             :y 130 :w 430 :fit :ellipsis-middle)
     ((om-radio-button " Open a workspace" 20 155)                         :y 161)
     ((om-static-text nil 40 179)                                   :y 185 :w 400)
     ((om-radio-button " Create a new workspace" 20 205)                   :y 216)
     ((om-button "Quit" 220 280)                                   :x 320 :y 286)
     ((om-button "OK" 320 280)                                     :x 420 :y 286)))
  "Corrected geometry of the preference panes, of the About splash and of the dialogs
the kernel lays out in absolute units, by key: the key make-new-pref-scroll dispatches
on, :about, or the title of a dialog.

Every entry is (KEY . GEOMETRY).  KEY is (CLASS TEXT X Y): the class of the widget, the
string om-dialog-item-text answers -- NIL for a widget that carries none, or whose text
is built at run time -- and the position the source gives it, which is what tells apart
the siblings that share a class and a text; X and Y are whole units, and a source that
places a widget on a fraction of a unit is matched on the rounded value and keeps its own
coordinate.  The KEY :WINDOW names the window that carries the page.  GEOMETRY is a plist
of ABSOLUTE OM units, any of :X :Y :W :H, plus the optional :FIT :ELLIPSIS-MIDDLE, which
shortens a path too long for its box; whatever it leaves out stays as the source set it.
The table corrects the SOURCE's own geometry -- a box too small for its text, a widget
past its page, a label crowding the next one.  It does not place the controls a label
names: those go where that label is, and putting one here would be overwritten.")

(defparameter *om-pref-column-merges*
  '((:appearance (171 215) (561 589)))
  "Control columns that must read as one, by module key: each list gives the x its
columns come to rest on, and the whole set is pulled onto the rightmost of them.
A column is placed after the longest label that answers to it, so one group whose labels
differ in length comes to rest on two x -- side by side those read as two columns where
the source wrote one.")

(defun %om-pref-descendants (view)
  "The user-added subviews under VIEW, depth first, VIEW itself excluded."
  (loop for child in (om-subviews view)
        append (cons child (%om-pref-descendants child))))

;;; The pane is polished as the source built it, before it is realized: it reaches
;;; make-new-pref-scroll's caller with no geometry of its own yet (kernel
;;; graphics/preferences.lisp: make-preference-win builds every pane before the tab
;;; layout and the window exist).  So the coordinates read here are the ones the source
;;; wrote into the view, which om-set-view-position and om-set-view-size keep in sync
;;; and which the placement uses once the pane is realized.

;;; A text field keeps its own inset: it stores the position shifted and gives it back
;;; unshifted (om-SBCL dialog-items.lisp: om-set-view-position and the om-view-position
;;; :around, both for om-editable-text).  Reading the slots raw would place a field a
;;; couple of units off the column it belongs to, so the inset is undone on the way in
;;; exactly as the accessor undoes it.

(defun %om-pref-inset (view)
  (if (typep view 'om-editable-text)
      (oa::om-get-text-edit-pos (om-make-point 0 0))
      (om-make-point 0 0)))

(defun %om-pref-x (view) (- (oa::vx view) (om-point-h (%om-pref-inset view))))
(defun %om-pref-y (view) (- (oa::vy view) (om-point-v (%om-pref-inset view))))

(defun %om-pref-font (item)
  (or (om-get-font item) *om-default-font1*))

(defun %om-pref-lines (item)
  "The lines ITEM draws at its own width, by the break the drawing uses
(alloy-desktop/widgets/controls.lisp: widget-label for static-text, through alld:text-wrap-pix)."
  (oa::text-wrap-pix (or (om-dialog-item-text item) "")
                     (%om-pref-font item)
                     (om-point-h (om-view-size item))))

(defun %om-pref-text-rows (item)
  "Rows ITEM draws at its own width, down to the last one that carries text: a text
ending in a newline yields a trailing empty line, and that line paints nothing."
  (let ((rows 0))
    (loop for line in (%om-pref-lines item)
          for index from 1
          unless (string= "" (string-trim " " line))
          do (setf rows index))
    rows))

(defun %om-pref-clears-below-p (item height)
  "True when giving ITEM HEIGHT still leaves 3 px between it and every sibling that
starts below its current box and shares its columns."
  (let* ((siz (om-view-size item))
         (x0 (%om-pref-x item))
         (x1 (+ x0 (om-point-h siz)))
         (top (%om-pref-y item))
         (bottom (+ top (om-point-v siz)))
         (container (om-view-container item)))
    (every (lambda (other)
             (or (eq other item)
                 (let ((osiz (om-view-size other)))
                   (or (<= (+ (%om-pref-x other) (om-point-h osiz)) x0)
                       (>= (%om-pref-x other) x1)
                       (< (%om-pref-y other) bottom)
                       (>= (%om-pref-y other) (+ top height 3))))))
           (and container (om-subviews container)))))

(defun %om-pref-fit-text-height (item)
  "Grow ITEM's box to the height of the text it draws, when that text does not fit and
the growth does not crowd a sibling below it."
  (let* ((rows (%om-pref-text-rows item))
         (lineh (om-string-h (or (om-get-font item) *om-default-font1*)))
         (needed (+ (* rows lineh) 2))
         (siz (om-view-size item))
         (height (om-point-v siz)))
    (when (and (plusp rows)
               (> (* rows lineh) (- height 1))
               (> needed height)
               (%om-pref-clears-below-p item needed))
      (om-set-view-size item (om-make-point (om-point-h siz) needed)))))

(defun %om-pref-fit-caption-width (item)
  "Grow ITEM's box to hold the caption it draws, indicator included: a check box and a
radio button paint their own caption, and a box narrower than that caption clips it.
The indicator margin is a DEVICE quantity, so ITEM has to be realized before it can be
read: the panes of the preference window are laid out before they reach one, and the
widths their sources give hold every caption they carry."
  (let ((text (or (om-dialog-item-text item) ""))
        (left (alld:indicator-label-px item)))
    (when (and left (string/= "" (string-trim " " text)))
      (let ((needed (+ left
                       (ceiling (om-string-size
                                 text
                                 (or (om-get-font item)
                                     (oa::%om-native-font :dialog-default))))
                       2))
            (siz (om-view-size item)))
        (when (> needed (om-point-h siz))
          (om-set-view-size item (om-make-point needed (om-point-v siz))))))))

;;; A path names its directory by the components it ends with, so a path too long for
;;; its box is shortened from the middle and never cut inside a component: the kernel
;;; keeps the whole path, only what is DISPLAYED is abbreviated.

(defun %om-pref-path-parts (text)
  "TEXT cut after every separator, each part carrying the separator that ends it."
  (let ((parts '()) (start 0))
    (dotimes (i (length text))
      (when (member (char text i) '(#\\ #\/))
        (push (subseq text start (1+ i)) parts)
        (setf start (1+ i))))
    (when (< start (length text)) (push (subseq text start) parts))
    (nreverse parts)))

(defun %om-pref-ellipsize-path (text font width)
  "TEXT shortened to WIDTH by dropping whole components after the first, the ones it
ends with kept.  Answers TEXT itself when it already fits, and the first component
plus the last when nothing shorter does."
  (let ((parts (%om-pref-path-parts text)))
    (if (or (<= (om-string-size text font) width) (< (length parts) 3))
        text
        (let* ((head (first parts))
               (sep (string (char head (1- (length head)))))
               (n (length parts)))
          (loop for tail downfrom (- n 2) above 0
                for candidate = (format nil "~A...~A~{~A~}" head sep (subseq parts (- n tail)))
                when (<= (om-string-size candidate font) width)
                  return candidate
                finally (return (format nil "~A...~A~A" head sep (car (last parts)))))))))

(defun %om-pref-fit-path-text (item)
  "Shorten what ITEM displays to the width of its box."
  (let ((text (om-dialog-item-text item)))
    (when (stringp text)
      (om-set-dialog-item-text
       item
       (%om-pref-ellipsize-path text (%om-pref-font item)
                                (- (om-point-h (om-view-size item)) 2))))))

;;; A preference control is an option of the user's, and it belongs to the label that
;;; names it: it sits right after that label's text, and on the line of that text.

(defparameter *om-pref-label-gap* 8
  "Clear space between the end of a label and the control it names, in OM units.")

(defparameter *om-pref-bindable-classes*
  '(om-editable-text om-check-box om-pop-up-dialog-item om-color-view)
  "Classes of control a preference label names.  A button and a heading name nothing,
and a check box that carries its own caption already holds its text.")

(defun %om-pref-label-end (label)
  "Right edge of the TEXT LABEL draws -- its widest line -- not of its box."
  (let ((font (%om-pref-font label)))
    (+ (%om-pref-x label)
       (ceiling (reduce #'max
                        (mapcar (lambda (line) (om-string-size line font))
                                (%om-pref-lines label))
                        :initial-value 0)))))

(defun %om-pref-label-line-center (label)
  "Middle of the text LABEL draws.  One line: the middle of its glyphs, the drawing
putting that line's baseline one line height below the top of the item and the glyphs
standing one ascent above it.  More than one: the middle of the whole block, because a
control that answers a wrapped label belongs to the label, not to its first line."
  (let ((font (%om-pref-font label))
        (rows (%om-pref-text-rows label)))
    (if (> rows 1)
        (+ (%om-pref-y label) (/ (* rows (om-string-h font)) 2))
        (- (+ (%om-pref-y label) (om-string-h font))
           (/ (oa::%om-font-baseline-px font) 2)))))

(defun %om-pref-bindable-p (view)
  (and (member (class-name (class-of view)) *om-pref-bindable-classes*)
       (or (not (typep view 'om-check-box))
           (string= "" (string-trim " " (or (om-dialog-item-text view) ""))))))

(defun %om-pref-same-row-p (a b)
  "True when A and B share at least half of the shorter one's height."
  (let ((ay (%om-pref-y a)) (ah (om-point-v (om-view-size a)))
        (by (%om-pref-y b)) (bh (om-point-v (om-view-size b))))
    (>= (- (min (+ ay ah) (+ by bh)) (max ay by))
        (* 1/2 (min ah bh)))))

(defun %om-pref-label-of (control siblings)
  "The static text CONTROL belongs to: the one whose text ends nearest to its left on
the same row, with nothing standing between the two.  NIL when it names nothing.
A label ENDS before the control it names, which is what keeps a paragraph or a heading
that merely spans the row from being read as one."
  (let ((clear nil) (clear-end nil) (over nil) (over-end nil))
    (dolist (view siblings)
      (when (and (typep view 'om-static-text)
                 (< (%om-pref-x view) (%om-pref-x control))
                 (%om-pref-same-row-p view control))
        (let ((end (%om-pref-label-end view)))
          (cond ((<= end (%om-pref-x control))
                 (when (or (null clear-end) (> end clear-end))
                   (setf clear view clear-end end)))
                ;; a label the source let run a hair into its own control still names
                ;; it -- that crowding is one of the things this pass exists to undo
                ((<= end (+ (%om-pref-x control) *om-pref-label-gap*))
                 (when (or (null over-end) (< end over-end))
                   (setf over view over-end end)))))))
    (flet ((reaches-p (label end)
             (and label
                  (notany (lambda (view)
                            (and (not (eq view control))
                                 (not (eq view label))
                                 (%om-pref-same-row-p view control)
                                 (< end (%om-pref-x view) (%om-pref-x control))))
                          siblings)
                  label)))
      (or (reaches-p clear clear-end) (reaches-p over over-end)))))

(defun %om-pref-followers (control siblings)
  "The controls that sit behind CONTROL on its row with no label between them: they
belong to the same option and keep their distance when CONTROL moves."
  (remove-if-not
   (lambda (view)
     (and (not (eq view control))
          (%om-pref-bindable-p view)
          (> (%om-pref-x view) (%om-pref-x control))
          (%om-pref-same-row-p view control)
          (notany (lambda (other)
                    (and (typep other 'om-static-text)
                         (%om-pref-same-row-p other control)
                         (< (%om-pref-x control) (%om-pref-x other) (%om-pref-x view))))
                  siblings)))
   siblings))

(defun %om-pref-columns (pairs)
  "PAIRS -- (control . label) -- grouped into the columns their controls already form.
Controls within one gap of each other are one column, which is what reads as a column:
the sources place a column by hand and a couple of units apart.  A column is then placed
as one, so it stays a column."
  (let ((sorted (sort (copy-list pairs) #'< :key (lambda (p) (%om-pref-x (car p)))))
        (columns '()))
    (dolist (pair sorted (nreverse columns))
      (let ((column (first columns)))
        (if (and column
                 (<= (- (%om-pref-x (car pair)) (%om-pref-x (car (first column))))
                     *om-pref-label-gap*))
            (setf (first columns) (append column (list pair)))
            (push (list pair) columns))))))

(defun %om-pref-pairs (siblings)
  "(control . label) for every control of SIBLINGS that a label names."
  (loop for view in siblings
        when (%om-pref-bindable-p view)
        append (let ((label (%om-pref-label-of view siblings)))
                 (when label (list (cons view label))))))

(defun %om-pref-column-targets (columns)
  "One x per column: the end of its longest label plus a gap."
  (mapcar (lambda (column)
            (+ *om-pref-label-gap*
               (reduce #'max
                       (mapcar (lambda (p) (%om-pref-label-end (cdr p))) column))))
          columns))

(defun %om-pref-level-targets (moves)
  "MOVES with every x that lands within a gap of a further one pulled onto it: two
groups that come to rest a couple of units apart are one column to the eye, and they
have to be one column on the screen."
  (let ((xs (mapcar #'second moves)))
    (mapcar (lambda (move)
              (list (first move)
                    (reduce #'max
                            (remove-if-not (lambda (x)
                                             (<= (abs (- (second move) x))
                                                 *om-pref-label-gap*))
                                           xs))
                    (third move)))
            moves)))

(defun %om-pref-bind-controls (container)
  "Put every labelled control of CONTAINER one gap after its label and on the line of
its text, keeping a column a column: the whole column lands after the longest label it
answers to.  A control that follows a labelled one on its row travels with it and sits
on the same line."
  (let* ((siblings (om-subviews container))
         (columns (%om-pref-columns (%om-pref-pairs siblings)))
         (moves (%om-pref-level-targets
                 (loop for column in columns
                       for x in (%om-pref-column-targets columns)
                       append (mapcar (lambda (p)
                                        (list (car p) x
                                              (%om-pref-label-line-center (cdr p))))
                                      column))))
         (moved (mapcar #'first moves))
         (chained '()))
    (dolist (move moves)
      (destructuring-bind (control x center) move
        (let ((delta (- x (%om-pref-x control))))
          (dolist (view (%om-pref-followers control siblings))
            (unless (or (member view moved) (assoc view chained))
              (push (list view (+ (%om-pref-x view) delta) center) chained))))))
    ;; the top rounds UP: a native control realizes at its own line box and never
    ;; taller than the source asked, so rounding down would leave it a pixel high
    (dolist (move (append moves chained))
      (destructuring-bind (control x center) move
        (om-set-view-position
         control
         (om-make-point x (ceiling (- center (/ (om-point-v (om-view-size control)) 2)))))))))

(defun %om-pref-bind-all (root)
  (%om-pref-bind-controls root)
  (dolist (child (om-subviews root))
    (when (om-subviews child) (%om-pref-bind-all child))))

(defun %om-pref-merge-columns (root columns)
  "Pull every control of ROOT resting on one of COLUMNS onto the rightmost of them.
Runs after the binding, whose placement it is correcting."
  (let ((target (reduce #'max columns)))
    (dolist (view (%om-pref-descendants root))
      (when (and (%om-pref-bindable-p view)
                 (member (round (%om-pref-x view)) columns))
        (om-set-view-position view (om-make-point target (%om-pref-y view)))))))

(defun %om-pref-find-widget (views key &optional geometry)
  "The widget of VIEWS that KEY -- (CLASS TEXT X Y) -- names, either where the source
put it or where GEOMETRY moves it to: a pass over a pane already polished has to find
the same widgets and leave them as they are, so applying the table twice is applying
it once."
  (destructuring-bind (class text x y) key
    (let ((tx (or (getf geometry :x) x))
          (ty (or (getf geometry :y) y)))
      (find-if (lambda (view)
                 (and (eq class (class-name (class-of view)))
                      (let ((vx (round (%om-pref-x view)))
                            (vy (round (%om-pref-y view))))
                        (or (and (= x vx) (= y vy))
                            (and (= tx vx) (= ty vy))))
                      (or (null text)
                          (equal text (om-dialog-item-text view)))))
               views))))

(defun %om-pref-apply-geometry (item geometry)
  (let ((siz (om-view-size item)))
    (om-set-view-size item
                      (om-make-point (or (getf geometry :w) (om-point-h siz))
                                     (or (getf geometry :h) (om-point-v siz)))))
  (om-set-view-position item
                        (om-make-point (or (getf geometry :x) (%om-pref-x item))
                                       (or (getf geometry :y) (%om-pref-y item))))
  (when (eq :ellipsis-middle (getf geometry :fit))
    (%om-pref-fit-path-text item))
  item)

(defun %om-pref-resize-window (window geometry)
  "Give WINDOW the size GEOMETRY asks for, and centre it again: the kernel centred it
on the screen at the size it built it with."
  (let* ((siz (om-view-size window))
         (w (or (getf geometry :w) (om-point-h siz)))
         (h (or (getf geometry :h) (om-point-v siz))))
    (unless (and (= w (om-point-h siz)) (= h (om-point-v siz)))
      (om-set-view-size window (om-make-point w h))
      (multiple-value-bind (screen-w screen-h) (alld:screen-size)
        (om-set-view-position window
                              (om-make-point (round (- screen-w w) 2)
                                             (round (- screen-h h) 2)))))))

(defun %om-polish-pref-pane (pane key)
  "Correct the geometry of the widgets PANE carries, in the order the corrections depend
on each other: first the boxes too small for the text they draw, then the per-widget
entries *om-pref-polish-table* holds for KEY, then every control against the label that
names it, and last the columns *om-pref-column-merges* asks to read as one.
The labels are settled before the controls are placed, because a control belongs to the
label as it will finally be drawn: a label the table widens or moves would otherwise
leave its control behind.  The table names its widgets by the coordinates of the source,
so it is resolved before anything moves.
The binding then runs a second time, which is not a repetition: a control that a
neighbour was standing in front of only comes into reach of its own label once that
neighbour has moved.  Answers PANE."
  (let* ((table (cdr (assoc key *om-pref-polish-table* :test #'equal)))
         (window (find :window table :key #'car))
         (views (%om-pref-descendants pane))
         (entries (loop for entry in table
                        when (consp (car entry))
                        append (let ((item (%om-pref-find-widget views (car entry) (cdr entry))))
                                 (when item (list (cons item (cdr entry))))))))
    (when (and window (typep pane 'om-dialog))
      (%om-pref-resize-window pane (cdr window)))
    (dolist (view views)
      (cond ((typep view 'om-static-text) (%om-pref-fit-text-height view))
            ((typep view '(or om-check-box om-radio-button))
             (%om-pref-fit-caption-width view))))
    (dolist (entry entries)
      (%om-pref-apply-geometry (car entry) (cdr entry)))
    (%om-pref-bind-all pane)
    (%om-pref-bind-all pane)
    (dolist (columns (cdr (assoc key *om-pref-column-merges* :test #'equal)))
      (%om-pref-merge-columns pane columns)))
  pane)

;;; One method per module key, as the panel-class methods above: the specializer of
;;; make-new-pref-scroll is an EQL on that key (kernel graphics/default-prefs.lisp).
(dolist (%pref-key '(:general :appearance :userlibs :score :conversion :midi :fluid
                     :audio :externals))
  (eval `(defmethod make-new-pref-scroll :around ((num (eql ,%pref-key)) modulepref)
           (declare (ignorable modulepref))
           (%om-polish-pref-pane (call-next-method) ,%pref-key))))

;;; A dialog reaches the screen through om-show-window, which the modal path calls once
;;; the caller has finished building it (om-SBCL window.lisp: %run-modal-dialog), so that
;;; is where a dialog whose title the table names is polished: complete, and not yet seen.
(defmethod om-show-window :before ((self om-dialog))
  (let ((title (om-window-title self)))
    (when (assoc title *om-pref-polish-table* :test #'equal)
      (%om-polish-pref-pane self title))))

(defmethod %om-polish-about :after ((view om-view))
  (%om-polish-pref-pane view :about))

