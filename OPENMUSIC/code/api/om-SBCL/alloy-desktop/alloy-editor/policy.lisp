(in-package :alloy-editor)

;;; The :CLASSIC family: a pane that answers keys and gestures the way the toolkit's
;;; own text inputs do (alloy components/text-input.lisp, the handle of key-down for a
;;; text-input-component, an input-box and an input-line), with a history that keeps
;;; every write as a step of its own.  What a table can say is said here, as data a
;;; host may replace -- the field's table is derived from the document's once, when
;;; this file is loaded, so a host replacing the one derives the other again; the rest
;;; is a switch on the family where each behaviour lives.

(defparameter *classic-keymap*
  '((:right () :forward-character)
    (:left () :backward-character)
    (:right (:shift) :forward-character-extending)
    (:left (:shift) :backward-character-extending)
    (:right (:control) :forward-token)
    (:left (:control) :backward-token)
    (:right (:control :shift) :forward-token-extending)
    (:left (:control :shift) :backward-token-extending)
    (:down () :next-line)
    (:up () :previous-line)
    (:down (:shift) :next-line-extending)
    (:up (:shift) :previous-line-extending)
    (:home () :beginning-of-line)
    (:end () :end-of-line)
    (:home (:shift) :beginning-of-line-extending)
    (:end (:shift) :end-of-line-extending)
    ;; Control changes nothing about Home and End: they stay on the line
    (:home (:control) :beginning-of-line)
    (:end (:control) :end-of-line)
    ;; nor about the two deleting keys: one character, or what is marked
    (:backspace () :delete-previous-character)
    (:backspace (:control) :delete-previous-character)
    (:delete () :delete-next-character)
    (:delete (:control) :delete-next-character)
    ;; nor about Insert, Shift and Control included
    (:insert () :toggle-overwrite)
    (:insert (:shift) :toggle-overwrite)
    (:insert (:control) :toggle-overwrite)
    (:a (:control) :select-all)
    (:b (:control) :previous-character)
    (:b (:control :alt) :backward-token)
    (:e (:control) :end-of-line)
    (:n (:control) :next-line)
    (:c (:control) :copy)
    (:x (:control) :cut)
    (:v (:control) :paste)
    (:enter () :new-line)
    (:return () :new-line)
    (:enter (:control) :leave)
    (:return (:control) :leave)
    (:escape () :leave))
  "The keys of a :CLASSIC pane that holds a document.  Nothing here undoes or redoes:
the host calls those commands from its own menu.  A key this table binds nothing to
is taken by the pane while it holds the strong focus and does nothing -- Tab, the page
keys and Control with F or P among them.  A mode's own keys are read before this table
and the mode's run before the family's, so under :LISP Tab indents and a double click
on a parenthesis takes the form.")

(defparameter *classic-field-keymap*
  (let ((own '((:kp-enter () :new-line)
               (:enter (:control) :new-line)
               (:return (:control) :new-line))))
    (append own
            (remove-if (lambda (entry)
                         (find-if (lambda (mine)
                                    (and (eq (first mine) (first entry))
                                         (equal (second mine) (second entry))))
                                  own))
                       *classic-keymap*)))
  "The keys of a :CLASSIC pane held to one line: every Return key hands the text over.
The rest is *CLASSIC-KEYMAP* as it stood when this file was loaded.")

(defparameter *classic-run-delimiters* '(#\Space #\Tab #\Return #\Linefeed)
  "Characters a :CLASSIC double click stops at: the run it takes is everything else.")

(defparameter *classic-wheel-step* 40
  "Device pixels one wheel notch takes a :CLASSIC view's text.")

(defparameter *classic-drag-step* 12
  "Device pixels a :CLASSIC view's text travels for each pointer move a selection drag makes outside it.")

(defparameter *classic-follow-margin* 2
  "Device pixels a :CLASSIC view keeps between the caret's row and the edge of what it shows.")

(defparameter *classic-history-size* 200
  "Steps of the history a :CLASSIC view keeps; the oldest goes when a write passes it.")
