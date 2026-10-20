(in-package :alloy-desktop)

;;; Layout of a self-sizing prompt window: native CAPI prompts size themselves to
;;; the message and ignore a requested size, so a host that builds the prompt
;;; window itself measures the text first and keeps its requested size as the
;;; MINIMUM. The measurement belongs to the host and enters as three functions.

(defparameter *dialog-margin* 15
  "Gutter between a self-sizing dialog's border and its text, in px.")

(defparameter *dialog-text-pad* 8
  "Slack around the measured text block: a label shape insets itself by the base
scale on each side and the laid-out glyph extent runs a little past the summed
advances a width measurer returns.")

(defparameter *dialog-max-width-ratio* 2/3
  "Widest a self-sizing dialog grows before its text wraps, as a fraction of the work area.")

(defun dialog-work-area-size ()
  "(values W H) of the work area a centered dialog lands on; the video mode when GLFW
answers no monitor -- the fallback the screen-size helper already uses."
  (let* ((monitor (handler-case (%glfw:primary-monitor) (%glfw-unavailable () nil)))
         (work    (and monitor (handler-case (%glfw:work-area monitor)
                                 (%glfw-unavailable () nil)))))
    (if (and (consp work) (= 4 (length work)))
        (values (third work) (fourth work))
        (screen-size))))

(defun dialog-hard-lines (text)
  "TEXT split on its newlines."
  (let ((s (if (stringp text) text (princ-to-string text)))
        (lines '())
        (start 0))
    (loop for pos = (position #\Newline s :start start)
          do (push (subseq s start (or pos (length s))) lines)
             (when (null pos) (return))
             (setf start (1+ pos)))
    (nreverse lines)))

(defun dialog-fit-size (text measure-width line-height wrapped-rows min-w min-h chrome-h)
  "Window size showing TEXT whole. MEASURE-WIDTH (line) answers a line's width in px,
LINE-HEIGHT () the height of one row and WRAPPED-ROWS (line width) the rows a line
takes wrapped at WIDTH. MIN-W/MIN-H is the floor (the requested size, widened to what
the buttons need) and CHROME-H the height spent outside the text. Past
*dialog-max-width-ratio* of the work area the text wraps instead of widening, and the
height stays inside the work area the way the window frame is kept there.
Returns (values W H TEXT-W TEXT-H WRAP-P)."
  (multiple-value-bind (work-w work-h) (dialog-work-area-size)
    (let* ((lines   (dialog-hard-lines text))
           (widest  (reduce #'max lines :key measure-width :initial-value 0))
           (max-w   (max min-w (round (* *dialog-max-width-ratio* work-w))))
           (w       (min max-w (max min-w (+ widest *dialog-text-pad* (* 2 *dialog-margin*)))))
           (text-w  (- w (* 2 *dialog-margin*)))
           (wrap-w  (- text-w *dialog-text-pad*))
           (wrap-p  (> widest wrap-w))
           (rows    (if wrap-p
                        (reduce #'+ lines :key (lambda (l) (funcall wrapped-rows l wrap-w))
                                          :initial-value 0)
                        (length lines)))
           ;; a rounded line height understates the pitch by up to 1/2 px
           (block-h (+ *dialog-text-pad*
                       (ceiling (* (max 1 rows) (+ (funcall line-height) 1/2)))))
           (h       (min (- work-h 60) (max min-h (+ block-h chrome-h)))))
      (values w h text-w (- h chrome-h) wrap-p))))

;;; Composition of the four simple modal prompts over a host protocol. The host owns
;;; every toolkit object, the font and the colours (read when a method runs, never
;;; stored); the library owns the sizing, the placement of the items, the button
;;; layout and the mapping of a pressed button to the returned value. The host is an
;;; explicit argument and no window position or owner enters the protocol.

(defclass prompt-host ()
  ()
  (:documentation "Base class of the objects that supply the toolkit side of a prompt."))

(defgeneric prompt-measure-width (host line)
  (:documentation "Width in px of LINE in the prompt font."))

(defgeneric prompt-line-height (host)
  (:documentation "Height in px of one row of text in the prompt font."))

(defgeneric prompt-wrapped-rows (host line width)
  (:documentation "Rows LINE takes in the prompt font when wrapped at WIDTH px."))

(defgeneric prompt-make-window (host title width height)
  (:documentation "A new modal prompt window with TITLE and a client area of WIDTH by HEIGHT."))

(defgeneric prompt-make-static-text (host x y width height text wrap-p)
  (:documentation "A read-only text item at X Y of WIDTH by HEIGHT; WRAP-P when TEXT wraps."))

(defgeneric prompt-make-editable-text (host x y width height text)
  (:documentation "A one-line editable text item at X Y of WIDTH by HEIGHT holding TEXT."))

(defgeneric prompt-make-button (host x y width height label default-p action)
  (:documentation "A push button at X Y of WIDTH by HEIGHT. DEFAULT-P marks the default
button; ACTION is a function of no arguments the button calls when pressed."))

(defgeneric prompt-add-items (host window items)
  (:documentation "Put ITEMS, a list, in WINDOW in order."))

(defgeneric prompt-run-modal (host window &optional focus)
  (:documentation "Run WINDOW modally and return the value it exits with, NIL when closed.
FOCUS, when given, is the item that receives the input focus once the window is shown."))

(defgeneric prompt-item-text (host item)
  (:documentation "The text an editable text ITEM holds now."))

(defgeneric prompt-exit-modal (host window value)
  (:documentation "End the modal run of WINDOW so that it returns VALUE."))

(defun %prompt-fit (host text minimum-width minimum-height button-width chrome-h)
  (dialog-fit-size text
                   (lambda (l) (prompt-measure-width host l))
                   (lambda () (prompt-line-height host))
                   (lambda (l w) (prompt-wrapped-rows host l w))
                   (max minimum-width (+ button-width (* 2 *dialog-margin*)))
                   minimum-height chrome-h))

(defun %prompt-button (host window x y label default-p value)
  (prompt-make-button host x y 80 24 label default-p
                      (lambda () (prompt-exit-modal host window value))))

(defun message-dialog (host message &key (title "Warning") (minimum-width 335)
                                         (minimum-height 100))
  "Show MESSAGE with an OK button and return NIL. The window is at least
MINIMUM-WIDTH by MINIMUM-HEIGHT and grows to show the whole text. Runs on the UI thread."
  (call-on-ui-thread
   (lambda ()
     (multiple-value-bind (w h text-w text-h wrap-p)
         (%prompt-fit host message minimum-width minimum-height 80 55)
       (let ((win (prompt-make-window host title w h)))
         (prompt-add-items
          host win
          (list (prompt-make-static-text host *dialog-margin* 15 text-w text-h message wrap-p)
                (%prompt-button host win (round (- (/ w 2) 40)) (- h 34) "OK" t nil)))
         (prompt-run-modal host win))))))

(defun yes-or-no-dialog (host message &key (title "Confirm") (minimum-width 300)
                                           (minimum-height 150) default-button)
  "Ask MESSAGE with No and Yes buttons and return T for Yes, NIL for No or a closed
window. No is the default button unless DEFAULT-BUTTON is :YES. Runs on the UI thread."
  (call-on-ui-thread
   (lambda ()
     (multiple-value-bind (w h text-w text-h wrap-p)
         (%prompt-fit host message minimum-width minimum-height 170 55)
       (let* ((x0  (round (- (/ w 2) 85)))
              (by  (- h 34))
              (win (prompt-make-window host title w h)))
         (prompt-add-items
          host win
          (list (prompt-make-static-text host *dialog-margin* 15 text-w text-h message wrap-p)
                (%prompt-button host win x0 by "No" (not (equal default-button :yes)) nil)
                (%prompt-button host win (+ x0 90) by "Yes" (equal default-button :yes) t)))
         (prompt-run-modal host win))))))

(defun yes-no-cancel-dialog (host message &key (title "Confirm") (minimum-width 300)
                                               (minimum-height 150) default-button)
  "Ask MESSAGE with Cancel, No and Yes buttons and return :CANCEL, NIL or T. A closed
window returns NIL. Yes is the default button unless DEFAULT-BUTTON is :NO, and Cancel
never is. Runs on the UI thread."
  (call-on-ui-thread
   (lambda ()
     (multiple-value-bind (w h text-w text-h wrap-p)
         (%prompt-fit host message minimum-width minimum-height 260 55)
       (let* ((x0  (round (- (/ w 2) 130)))
              (by  (- h 34))
              (win (prompt-make-window host title w h)))
         (prompt-add-items
          host win
          (list (prompt-make-static-text host *dialog-margin* 15 text-w text-h message wrap-p)
                (%prompt-button host win x0 by "Cancel" nil :cancel)
                (%prompt-button host win (+ x0 90) by "No" (equal default-button :no) nil)
                (%prompt-button host win (+ x0 180) by "Yes" (not (equal default-button :no)) t)))
         (prompt-run-modal host win))))))

(defun prompt-for-string (host prompt &key (initial-string "") (title "Get string")
                                           (minimum-width 365) (minimum-height 100))
  "Ask PROMPT with an editable field holding INITIAL-STRING and Cancel and OK buttons.
Returns the field text for OK and NIL for Cancel or a closed window. Runs on the UI thread."
  (call-on-ui-thread
   (lambda ()
     (multiple-value-bind (w h text-w text-h wrap-p)
         (%prompt-fit host prompt minimum-width minimum-height 170 78)
       (let* ((x0    (round (- (/ w 2) 85)))
              (by    (- h 34))
              (win   (prompt-make-window host title w h))
              (input (prompt-make-editable-text host *dialog-margin* (+ text-h 18) text-w 22
                                                (or initial-string ""))))
         (prompt-add-items
          host win
          (list (prompt-make-static-text host *dialog-margin* 12 text-w text-h prompt wrap-p)
                input
                (prompt-make-button host x0 by 80 24 "Cancel" nil
                                    (lambda () (prompt-exit-modal host win nil)))
                (prompt-make-button host (+ x0 90) by 80 24 "OK" t
                                    (lambda ()
                                      (prompt-exit-modal host win (prompt-item-text host input))))))
         (prompt-run-modal host win input))))))
