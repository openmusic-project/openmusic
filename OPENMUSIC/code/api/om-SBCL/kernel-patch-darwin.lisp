(in-package :om)

;;; Score-editor control strip, darwin only: the tuning (Approx) button that the
;;; #+macosx branch of scoreeditors.lisp (initialize-instance :after for
;;; omcontrols-view) oversizes is refitted here, as data through the public accessors.

(defparameter *om-edo-button-label-margin* 6
  "Inset between the widest tuning symbol and each side of the button box (dialog-items.lisp: om-button label margins).")

(defparameter *om-edo-button-column* 380
  "The x of the Staff pop-up column, the l2 of scoreeditors.lisp (initialize-instance :after for omcontrols-view).")

(defparameter *om-edo-button-height* 22
  "Height of every other control of the strip's second row.")

(defun %om-edo-labels ()
  "Every tuning symbol the button can carry (omicron-dialog.lisp: give-symbol-of-approx over *scales-list*), fallback included; the label is rewritten in place, so the box holds the widest."
  (cons "12 EDO_#" (mapcar (lambda (scale) (car (last scale))) *scales-list*)))

(defun %om-edo-button-p (view)
  "True when VIEW is the strip's tuning button, told apart by its label."
  (and (typep view 'om-button)
       (member (om-dialog-item-text view) (%om-edo-labels) :test #'equal)
       t))

(defun %om-edo-button-width (font)
  "Width that holds the widest tuning symbol drawn in FONT, its label inset included."
  (+ (ceiling (reduce #'max
                      (mapcar (lambda (label) (om-string-size label font))
                              (%om-edo-labels))))
     (* 2 *om-edo-button-label-margin*)))

(defun %om-fit-edo-button (strip)
  "Give the tuning button of STRIP the row the strip's own second-row controls stand on
and the width the widest tuning symbol needs.  Answers STRIP."
  (let ((button (find-if #'%om-edo-button-p (om-subviews strip))))
    (when button
      (om-set-view-size
       button
       (om-make-point (%om-edo-button-width (or (om-get-font button) *om-default-font1*))
                      *om-edo-button-height*))
      (om-set-view-position
       button
       (om-make-point *om-edo-button-column* *second-row-y*))))
  strip)

(when (find-class 'omcontrols-view nil)     ; musicproject is an optional project

  ;; :around, not :after -- scoreeditors.lisp defines the :after that builds the strip
  ;; on this same specializer, and the subclasses add their controls in :afters of their own
  (defmethod initialize-instance :around ((self omcontrols-view) &rest initargs)
    (declare (ignore initargs))
    (multiple-value-prog1 (call-next-method)
      (%om-fit-edo-button self))))
