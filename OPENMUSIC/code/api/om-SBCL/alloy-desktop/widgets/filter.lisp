(in-package :alloy-desktop)

;;; CAPI filtering-layout (capi-capi-ref-156.htm): a text field plus a dropdown of
;;; three INDEPENDENT axes — regexp/plain, exclude/include, case — that combine
;;; freely into eight states. Alloy has no equivalent, so the parts are dialog
;;; items wired to one filter state, shared by every host that needs a filter.
;;; This file holds the state, the decision and the strip's measures; the host
;;; builds the widgets and answers filter-apply.

(defparameter *filter-bar-height* 26)
(defparameter *filter-field-height* 22)
(defparameter *filter-clear-width* 26)
(defparameter *filter-matches-width* 92)
(defparameter *filter-field-width* 120)
(defparameter *filter-clear-color* (chroma:rgb 0.80 0.13 0.13))
(defparameter *filter-clear-hover* (chroma:rgb 0.88 0.88 0.88))
(defparameter *filter-text-color*  (chroma:rgb 0.08 0.08 0.08))

(defclass filter-host ()
  ((filter-text      :accessor filter-text      :initform "")
   (filter-regexp-p  :accessor filter-regexp-p  :initform nil)
   (filter-exclude-p :accessor filter-exclude-p :initform nil)
   (filter-case-p    :accessor filter-case-p    :initform nil)
   (filter-field     :accessor filter-field     :initform nil)
   (filter-mode-menu :accessor filter-mode-menu :initform nil)
   (filter-clear     :accessor filter-clear     :initform nil)
   (filter-matches   :accessor filter-matches   :initform nil)
   (filter-bar       :accessor filter-bar       :initform nil))
  (:documentation "State and widget handles of one CAPI filtering-layout."))

(defgeneric filter-apply (host)
  (:documentation "Re-run HOST's filter over its own data and refresh its view."))

(defgeneric filter-window (host)
  (:method ((host window)) host)
  (:method ((host t)) (element-window host))
  (:documentation "Native window anchoring HOST's filter dropdown."))

(defgeneric filter-mode-place (host)
  (:method ((host t)) :west)
  (:documentation "Border-layout cell the mode button occupies in HOST's bar."))

(defun filter-hit-p (host text)
  "Does TEXT match HOST's needle under the regexp and case axes?"
  (let ((needle (filter-text host)))
    (if (filter-regexp-p host)
        (let ((scanner (handler-case
                           (cl-ppcre:create-scanner
                            needle
                            :case-insensitive-mode (not (filter-case-p host)))
                         (error (condition)
                           (format *error-output* "~&[om filter] ~S is not a regexp: ~A~%"
                                   needle (princ-to-string condition))
                           nil))))
          (and scanner (cl-ppcre:scan scanner text) t))
        (and (search needle text
                     :test (if (filter-case-p host) #'char= #'char-equal))
             t))))

(defun filter-active-p (host)
  (plusp (length (filter-text host))))

(defun filter-pass-p (host text)
  "The full decision for one candidate: no needle passes everything, otherwise
the regexp/case hit, negated when the exclude axis is on."
  (or (not (filter-active-p host))
      (let ((hit (filter-hit-p host text)))
        (if (filter-exclude-p host) (not hit) hit))))

(defun filter-mode-label (host)
  "CAPI's :medium filter label — Filter/Exclude plus the R and C flags
(capi-capi-ref-156.htm:97)."
  (let ((flags (concatenate 'string
                            (if (filter-regexp-p host) "R" "")
                            (if (filter-case-p host) "C" ""))))
    (concatenate 'string
                 (if (filter-exclude-p host) "Exclude" "Filter")
                 (if (plusp (length flags)) ":" "")
                 flags)))

(defparameter *filter-mode-keys* '((:r . :regexp) (:e . :exclude) (:c . :case))
  "CAPI's filter chords: R regexp/plain, E exclude/include, C case sensitivity.")

(defparameter *filter-mode-items*
  '(("Regexp Filter" . :regexp) ("Exclude Matches" . :exclude) ("Case Sensitive" . :case))
  "The three checkable rows of CAPI's filter menu.")

(defun filter-axis-on-p (host axis)
  (ecase axis
    (:regexp  (filter-regexp-p host))
    (:exclude (filter-exclude-p host))
    (:case    (filter-case-p host))))

(defun filter-chord-axis (event)
  "The axis a Control chord selects, or NIL (capi-capi-ref-282.htm:75)."
  (cdr (assoc (alloy:key event) *filter-mode-keys*)))
