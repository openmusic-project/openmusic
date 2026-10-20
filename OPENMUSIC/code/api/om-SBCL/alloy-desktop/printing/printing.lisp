(in-package :alloy-desktop)

;;; Printing: the printer settings a session owns and the page geometry they give,
;;; the single print slot, the native Print and Page Setup dialogs with the
;;; modality a desktop window expects, and the jobs -- a vector document drawn on
;;; the printer's own device (printing/gdi-port.lisp) in a thread of its own with a
;;; progress window, and a raster job of ready-made pages.  The native dialogs, the
;;; device contexts and the page transport are PAPER-PRINT's; a consumer names only
;;; this package.  A host's policy reaches the jobs through the hook generics
;;; below, each with a neutral default.

(defclass printing-service (desktop-service)
  ((settings :initform nil :accessor printer-settings
             :documentation "Printer settings this service owns, NIL until something needed
them.  PAPER-GEOMETRY resolves the default printer itself when handed NIL, so the
page size stays answerable without them; the native dialogs do not (see
ENSURE-PRINTER-SETTINGS).")
   (page-size :initform nil :accessor %page-size
              :documentation "PAGE-SIZE cache; NIL asks the printer again on the next call.")
   (page-dpi :initform nil :accessor page-dpi
             :documentation "Printer resolution behind the cached page size, NIL while that
is unmeasured.  The page size is expressed in logical units and drops it, but a
print job has to record at the printer's own resolution, not at the screen's.")
   (job-active-p :initform nil :reader print-job-active-p
                 :documentation "True from the moment a print command is accepted until its
job has ended.  A slot of the service, not a binding: the job runs on a thread of
its own, so the command that starts it and the thread that finishes it are not the
same thread.")
   (lock :initform (bordeaux-threads:make-lock "alloy-desktop-printing")
         :reader %printing-lock)
   (warning-prefix :initarg :warning-prefix :initform "Printing"
                   :reader printing-warning-prefix
                   :documentation "How the warnings of this service name the printing: a
failed operation is logged as \"<prefix> failed during ...\"."))
  (:documentation "The printer state of a session and the entry points that print."))

(defgeneric select-printer (service &key owner settings)
  (:documentation "Run the native Print dialog over OWNER; answer the accepted printer
settings, or NIL on Cancel."))

(defgeneric page-setup (service &key owner name)
  (:documentation "Run the native Page Setup dialog owned by OWNER as the print
operation NAME; answer true when the user accepted it."))

(defvar *default-printing-service* nil)

(defun default-printing-service ()
  (or *default-printing-service*
      (setf *default-printing-service* (make-instance 'printing-service))))

(defparameter *print-fallback-dpi* 600
  "Resolution a print job records at when the backend reports none.
GTK answers in PostScript points whenever the printer exposes no resolution
(paper-print/linux.lisp: paper-geometry), which names a page size, not a device
to aim at.  600 is what the Windows backend itself assumes for a host with no
reachable printer (paper-print/win32.lisp: +fallback-dots-per-inch+).")

(defparameter *print-output-release-seconds* 5
  "How long the spooler is given to let go of a destination file.")

;;; ------------------------------------------------------------- host hooks

(defgeneric printing-fallback-scale (service)
  (:documentation "The display scale the logical page unit uses when no window can be
asked for its own (LOGICAL-SCREEN-DPI). The default is the last display content
scale recorded (NOTE-CONTENT-SCALE).")
  (:method (service) (declare (ignore service)) *last-content-scale*))

(defgeneric activate-print-window (service window)
  (:documentation "Put WINDOW, the window a print command was given on, back in
front: after a native dialog, before the progress window opens over it, and when
the job ends.  WINDOW may be NIL or already closed.")
  (:method (service window)
    (declare (ignore service))
    (when (and window (window-registered-p window))
      (unless (window-visible-p window) (show-window window))
      (raise-window window))))

(defgeneric choose-print-output-file (service name)
  (:documentation "Ask where a job printing to a document writer must write,
prefilled with NAME; answer a pathname or NIL for Cancel (see PRINT-OUTPUT-FILE).")
  (:method (service name)
    (declare (ignore service))
    (choose-new-file (make-instance 'file-dialog-service)
                     :title "Print to File" :default name
                     :filter (types->filter '("PDF file" "*.pdf")))))

(defgeneric note-printing-window (service window)
  (:documentation "WINDOW is the window a job draws from its own thread, from the
moment the job starts; NIL when it has ended.  A host whose frame loop repaints
from the same tree the job reads uses it to stop repainting that window.")
  (:method (service window) (declare (ignore service window)) nil))

(defun print-failure (service operation detail)
  "Log a failed OPERATION with its DETAIL; answer NIL.
A failed CAPI job simply does nothing visible (om-LW/print.lisp: om-print-document,
its (when printer ...)): parity is not to invent a dialog, but the reason has to be
recoverable, so it goes to the log with its context."
  (warn "~A failed during ~A: ~A" (printing-warning-prefix service) operation
        (princ-to-string detail))
  nil)

;;; ----------------------------------------------------------- logical units

#+darwin
(defconstant +darwin-print-logical-dpi+ 120
  "Logical units per inch of a darwin print job: 96 dpi typography at a 125% display
scale, the unit LOGICAL-SCREEN-DPI answers on the Windows host darwin prints like.")

(defun logical-screen-dpi (service)
  "The logical resolution of the user's coordinate space.

CAPI:GET-PAGE-AREA called without :DPI answers the printable area in exactly this
unit (LispWorks manual capi/capi-capi-ref-172.htm:56,60), and every page size a
pagination consumes is expressed in it.  It is the toolkit's typography DPI at 100%
display scaling times the display scale alloy applies once (the base scale of the
front window, or PRINTING-FALLBACK-SCALE) -- 120 on a 125% Windows display.
On darwin it is +DARWIN-PRINT-LOGICAL-DPI+."
  #+darwin (declare (ignore service))
  #+darwin +darwin-print-logical-dpi+
  #-darwin
  (let* ((window (front-window))
         (tree   (and window (alloy:layout-tree window)))
         (ui     (and tree (alloy:ui tree)))
         (scale  (if ui (alloy:base-scale ui) (printing-fallback-scale service))))
    (* (typography-dpi) (rational scale))))

(defun a4-page-size (logical-dpi)
  "A4 less the 25 mm margins of the vanilla Page Setup contract, in logical units,
as (values WIDTH HEIGHT).  A pagination sizes every page from the paper size, so a
host whose printer cannot be reached still has to lay out."
  (values (round (* (- 210 50) logical-dpi 10) 254)
          (round (* (- 297 50) logical-dpi 10) 254)))

;;; ---------------------------------------------------------- the print slot

(defun %claim-print-job (service)
  "Take the single print slot, or answer NIL when a job already holds it."
  (bordeaux-threads:with-lock-held ((%printing-lock service))
    (unless (slot-value service 'job-active-p)
      (setf (slot-value service 'job-active-p) t))))

(defun %release-print-job (service)
  (bordeaux-threads:with-lock-held ((%printing-lock service))
    (setf (slot-value service 'job-active-p) nil))
  nil)

(defun %paper-call-sync (function)
  "Call FUNCTION in the platform thread that owns the native printing API.

On Windows that is the calling thread: what comes through here is the part of the
command that runs on the thread owning the windows (START-PRINT-TASK), so there is
nothing to marshal.  The vector job itself does not come through here at all -- it
owns its own thread and its own device context (%SPAWN-PRINT-JOB).  On Linux GTK3
is already initialized on the UI thread (for example by os-fonts) while the print
command runs in an operation thread, so every PAPER-PRINT entry point is marshalled
back to that same UI thread; this avoids re-initializing or touching GTK from a
second thread.

The native Print dialog does not come through here: a dialog is a window, and a
window belongs to the thread that owns the application's windows, so SELECT-PRINTER
marshals it with CALL-ON-UI-THREAD instead."
  #+win32 (funcall function)
  #+(or linux darwin) (call-on-ui-thread function))

(defun start-print-task (service name function)
  "Run FUNCTION as the single print operation the application may have in flight.

On Windows FUNCTION is the part of the command that belongs on the thread owning
the windows: the two native dialogs, the page count, and the progress window.  A
menu action already runs on that thread, and a dialog created anywhere else opens
behind the document, because Win32 grants the foreground to the thread owning the
active window.  FUNCTION answers :HANDED-OVER when it has handed the job to a
worker thread, which then owns the print slot and frees it when it ends; the
answer is then T.  Any other outcome -- a finished job, an accepted or cancelled
dialog, a failure -- frees the slot here, and FUNCTION's answer is the answer.

On Linux the whole operation runs on a worker named NAME: that backend marshals
every PAPER-PRINT entry point back to the UI/GTK thread through %PAPER-CALL-SYNC,
which would deadlock if the command ran on it."
  #+(or win32 darwin)
  (if (not (%claim-print-job service))
      (progn (warn "~A: a print operation is already active."
                   (printing-warning-prefix service))
             nil)
      (let ((result nil))
        (unwind-protect
             (setf result
                   (handler-case (funcall function)
                     (error (condition) (print-failure service name condition))))
          (unless (eq result :handed-over) (%release-print-job service)))
        (or (eq result :handed-over) result)))
  #+linux
  (if (not (%claim-print-job service))
      (progn (warn "~A: a print operation is already active."
                   (printing-warning-prefix service))
             nil)
      (bordeaux-threads:make-thread
       (lambda ()
         (unwind-protect
              (handler-case (funcall function)
                (error (condition) (print-failure service name condition)))
           (%release-print-job service)))
       :name name)))

;;; ------------------------------------------------- settings and geometry

(defun close-printer-settings (service)
  "Release the settings SERVICE owns.  A quit action, so it answers T.

A job still drawing when the application quits is not stopped here: its thread
holds the device context and ends with the process, and the spooler keeps whatever
pages it has already been handed -- which is what a printer does when the program
that fed it goes away."
  (let ((settings (printer-settings service)))
    (setf (printer-settings service) nil
          (%page-size service) nil
          (page-dpi service) nil)
    (when settings
      (%paper-call-sync (lambda () (paper-print:close-settings settings)))))
  t)

(defun ensure-printer-settings (service)
  "The settings SERVICE owns, created from the current printer if none.

Both native dialogs must be handed real settings.  Given none, Page Setup reads the
zeroed margin rectangle it was passed as the margins the caller asked for, and the
Print dialog stores a zeroed one; the printable page then grows to the whole sheet
and a pagination spreads over the part the printer cannot reach.  Answers NIL when
no printer can be resolved at all, the only case in which a dialog is opened
without them."
  (or (printer-settings service)
      (setf (printer-settings service)
            (handler-case
                (%paper-call-sync #'paper-print:default-printer-settings)
              (paper-print:paper-print-error (condition)
                (print-failure service :default-printer condition))))))

(defun adopt-printer-settings (service settings)
  "Keep SETTINGS, release whatever they replace, and invalidate the page cache."
  (unless (eq settings (printer-settings service))
    (let ((old (printer-settings service)))
      (setf (printer-settings service) settings)
      (when old
        (%paper-call-sync (lambda () (paper-print:close-settings old))))))
  (setf (%page-size service) nil
        (page-dpi service) nil)
  settings)

(defun %logical-page-extent (service device-width device-height metadata)
  "DEVICE-WIDTH x DEVICE-HEIGHT printer pixels in logical units, as (values W H)."
  (let ((dpi (logical-screen-dpi service)))
    (values (max 1 (round (* device-width dpi) (getf metadata :dpi-x)))
            (max 1 (round (* device-height dpi) (getf metadata :dpi-y))))))

(defun %measure-page-size (service)
  "Printable page in logical units and the printer resolution it came from, as
(values WIDTH HEIGHT DPI).  PAPER-GEOMETRY answers a missing or refused printer with
its own deterministic fallback, so only a caller defect can signal here.

The size is the printable area less the margins, as CAPI's page area is
(om-LW/print.lisp: om-score-paper-size through capi:get-page-area): it is what a
pagination counts pages from.  WHERE that page then lands on the sheet is a
separate question, and the answer is not the margin but the printable origin --
which is why a vector page is drawn at the top left of what the device can reach
(printing/gdi-port.lisp: call-with-printer-port)."
  (handler-case
      (multiple-value-bind (width height metadata)
          (%paper-call-sync
           (lambda () (paper-print:paper-geometry (printer-settings service))))
        (multiple-value-bind (logical-width logical-height)
            (%logical-page-extent service width height metadata)
          (values logical-width logical-height
                  (min (getf metadata :dpi-x 0) (getf metadata :dpi-y 0)))))
    (error (condition)
      (print-failure service :paper-geometry condition)
      (multiple-value-bind (width height) (a4-page-size (logical-screen-dpi service))
        (values width height nil)))))

(defun page-size (service &key (make-size #'cons))
  "The printable page of SERVICE's printer in logical units, as the value MAKE-SIZE
answers for the width and the height; cached until the settings change."
  (or (%page-size service)
      (multiple-value-bind (width height dpi) (%measure-page-size service)
        (setf (page-dpi service) dpi)
        (setf (%page-size service) (funcall make-size width height)))))

(defun max-record-side ()
  "Longest side an offscreen record may have, or NIL when GL cannot be asked.
The record is one texture (graphics/image.lisp: begin-record), so a page recorded
for printing can never be wider or taller than the live GL limit."
  (let ((window (front-window)))
    (when window
      (call-on-ui-thread
       (lambda ()
         (call-with-gl-context
          window
          (lambda () (gl:get-integer :max-texture-size))))))))

(defun print-page-scale (service page-width page-height)
  "Device pixels per logical unit a PAGE-WIDTH x PAGE-HEIGHT page of a raster job
records at.

Vanilla has no such number: it records the page as a vector metafile and lets the
printer port draw it at device resolution (om-LW/print.lisp: om-print-document).
A recorder produces pixels, so the resolution has to be chosen, and the choice is
the printer's own -- the page is in logical screen units, and this is what takes it
to the printer's dots.  A backend that answers in points names no device resolution
to aim at, and then *PRINT-FALLBACK-DPI* is the target.  Never below 1: a printer
coarser than the screen still gets the screen's detail."
  (let* ((logical (logical-screen-dpi service))
         (reported (or (page-dpi service) 0))
         (target (if (> reported logical) reported *print-fallback-dpi*))
         (wanted (max 1 (/ (rational target) logical)))
         (side (max 1 (round (max page-width page-height))))
         (limit (max-record-side)))
    (if (and limit (plusp limit) (> (* wanted side) limit))
        (max 1 (floor limit side))
        wanted)))

;;; ------------------------------------------------- dialogs and modality

(defun %dialog-owner (window)
  #+(or linux darwin) (declare (ignore window))
  #+win32
  (let ((hwnd (and window (%window-hwnd window))))
    (and hwnd (not (cffi:null-pointer-p hwnd)) hwnd))
  #+(or linux darwin) nil)

(defun %call-with-owner-disabled (service window function)
  "Run FUNCTION with the platform's native dialog modality.

On Windows PrintDlgW cannot use the GLFW window as hwndOwner, so WINDOW is disabled
explicitly for the duration of the native dialog.  On Linux PAPER-PRINT owns the
GtkPrintUnixDialog loop and deliberately does not cross a GLFWwindow into GTK, so no
foreign window manipulation is required here.

WINDOW is also put back in front afterwards (ACTIVATE-PRINT-WINDOW): a disabled
window cannot be given the activation a destroyed dialog releases, so the system
hands the foreground to some other window of the thread and merely re-enabling
does not take it back (Microsoft, EnableWindow, Remarks).  The window the command
was given on is the one that must have it."
  #+(or linux darwin) (declare (ignore service window))
  #+win32
  (let ((hwnd (%dialog-owner window)))
    (when hwnd (%enable-window hwnd 0))
    (unwind-protect (funcall function)
      (when hwnd
        (%enable-window hwnd 1)
        (activate-print-window service window))))
  #+(or linux darwin) (funcall function))

(defmethod select-printer ((service printing-service) &key owner settings)
  "Run the native Print dialog; answer the accepted settings, now SERVICE's, or NIL
on Cancel.  SETTINGS preset the dialog; by default the ones SERVICE owns.

The dialog ignores OWNER as its hwndOwner (paper-print/win32.lisp: %print-dialog-dc)
and OWNER is disabled around it instead.  The dialog is created on the thread that
owns the windows.  Win32 keeps one input queue and one active window PER THREAD and
grants the foreground to the thread owning the active window, so a dialog created
by any other thread cannot take activation from OWNER: it opens BEHIND it, whatever
hwndOwner it was given."
  (let ((result nil)
        (accepted nil))
    (%call-with-owner-disabled
     service owner
     (lambda ()
       (multiple-value-setq (result accepted)
         (call-on-ui-thread
          (lambda ()
            (paper-print:select-printer :settings (or settings (ensure-printer-settings service))
                                        :owner (%dialog-owner owner)))))
       (when accepted (adopt-printer-settings service result))))))

(defmethod page-setup ((service printing-service) &key owner (name "Page Setup"))
  "Run the native Page Setup dialog as the print operation NAME, owned by OWNER;
accepted settings become SERVICE's."
  (start-print-task
   service name
   (lambda ()
     (multiple-value-bind (settings accepted)
         ;; GTK is process-global and was initialized on the UI thread; Page
         ;; Setup stays on that same thread.  Windows runs it in place.
         (%paper-call-sync
          (lambda ()
            (paper-print:page-setup :owner (%dialog-owner owner)
                                    :settings (ensure-printer-settings service))))
       (when accepted (adopt-printer-settings service settings))
       accepted))))

(defun %prompting-port-p (port)
  "True when PORT is a port monitor that opens a destination dialog of its own.

On PORTPROMPT: -- the port of Microsoft Print to PDF and of the XPS Document Writer
-- and on the plain FILE: port, StartDoc asks the user for the output file itself
and does not return until that dialog is dismissed (Microsoft, StartDoc, Remarks).
That dialog is owned by no window of this application and need not even be created
by this process, so nothing here can raise it above the application's windows or
close it.  Naming the destination in DOCINFO.lpszOutput instead means it is never
opened."
  (let ((port (and (stringp port) (string-upcase port))))
    (and port
         (or (eql 0 (search "PORTPROMPT" port))
             (eql 0 (search "FILE:" port))))))

(defun %file-name-candidate (title)
  "TITLE as a file name a Save dialog can prefill: every character Win32 forbids
in a name becomes a dash."
  (substitute-if #\- (lambda (character) (find character "\\/:*?\"<>|"))
                 (if (stringp title) title "")))

(defun print-output-file (service settings title)
  "Where a job on SETTINGS must write, as (values OUTPUT CANCELLED-P).

OUTPUT is NIL for a queue that spools by itself, the normal case and the only one
vanilla ever has (om-LW/print.lisp: om-print-document hands CAPI a printer and
nothing else).  For a prompting port it is the file the user picked, asked for
through CHOOSE-PRINT-OUTPUT-FILE so that it is owned, placed and cancellable like
every other dialog of the application; CANCELLED-P is true only when the user
dismissed that dialog, and the job must then not start at all.

The offered type is PDF: both prompting queues in Windows are document writers and
the extension only prefills the dialog, which the user may still override."
  (let ((port (handler-case
                  (%paper-call-sync
                   (lambda () (paper-print:settings-port-name settings)))
                (error (condition)
                  ;; an unreadable port must not make the document unprintable:
                  ;; the job goes out as if the queue spooled by itself, and the
                  ;; reason is recoverable from the log
                  (print-failure service :printer-port condition)))))
    (if (%prompting-port-p port)
        (let ((answer (choose-print-output-file service (%file-name-candidate title))))
          (if answer
              (values (if (pathname-type answer)
                          answer
                          (make-pathname :type "pdf" :defaults answer))
                      nil)
              (values nil t)))
        (values nil nil))))

;;; ----------------------------------------------------------- raster pages

(defun opaque-raster-page (width height data origin
                           &key (logical-width width) (logical-height height))
  "RGBA page whose alpha is forced opaque.

The GDI backend flattens straight alpha against white (paper-print/win32.lisp:
%draw-page).  The pages here are already composited -- a window framebuffer or a
white-cleared record framebuffer -- so a residual alpha below 255 would wash the ink
out.  DATA is mutated in place: it belongs to this page alone.

LOGICAL-WIDTH and LOGICAL-HEIGHT are the size the page means, which is the size in
pixels only for a page captured from the screen: a recorded page carries more
pixels than logical units, and the backend must scale the sheet from the units
(paper-print: make-raster-page)."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (locally (declare (type (simple-array (unsigned-byte 8) (*)) data)
                    (optimize (speed 3)))
    (loop for index of-type fixnum from 3 below (length data) by 4
          do (setf (aref data index) 255)))
  (paper-print:make-raster-page width height data
                                :origin origin
                                :logical-width logical-width
                                :logical-height logical-height))

(defun capture-raster-page (window x y width height)
  "The WIDTH x HEIGHT pixels of WINDOW's visible frame whose lower-left corner is
at the alloy px point X Y (rows count upwards), as one raster page.  A page captured
from the screen has no logical size of its own: its pixels ARE its units."
  (call-on-ui-thread
   (lambda ()
     (call-with-gl-context
      window
      (lambda ()
        ;; the production loop renders and swaps, so the visible frame is the
        ;; front buffer -- except on Linux/GLFW, where it reads back cleared
        #+linux (gl:read-buffer :back)
        #-linux (gl:read-buffer :front)
        (gl:pixel-store :pack-alignment 1)
        (opaque-raster-page
         width height
         (gl:read-pixels x y width height :rgba :unsigned-byte)
         :bottom-left))))))

;;; ------------------------------------------------------------ raster jobs

(defun run-print-job (service window title page-count-function page-function)
  "Open the native Print dialog over WINDOW, then spool the raster pages it
accepted: PAGE-FUNCTION answers the paper-print raster page of a page number.

PAGE-COUNT-FUNCTION runs after acceptance, as in vanilla, so the count is computed
from the printer the user actually chose (om-LW/print.lisp: om-print-document asks
CAPI:GET-PAGE-AREA of the returned printer).  Answers T once the job printed."
  (let ((settings (select-printer service :owner window)))
    ;; Cancel is a normal result and shows nothing, as in vanilla
    (when settings
      (multiple-value-bind (output cancelled)
          (print-output-file service settings title)
        (unless cancelled
          (let ((page-count (funcall page-count-function)))
            (multiple-value-bind (status detail)
                (%paper-call-sync
                 (lambda ()
                   (paper-print:print-image-pages page-count
                                                  page-function
                                                  :title title
                                                  :settings settings
                                                  :show-dialog nil
                                                  :output output)))
              (case status
                (:printed t)
                (:failed (print-failure service :print-job detail))
                (t nil)))))))))

(defun print-one-page (service window title page &key (name "Print"))
  "Print the ready-made raster PAGE as the print operation NAME, after the native
Print dialog opened over WINDOW.  Capture the page BEFORE calling this (vanilla
does, om-LW/print.lisp: om-print-one-page), so it is the one the user was looking
at and no dialog can occlude it."
  (start-print-task
   service name
   (lambda ()
     (run-print-job service window title
                    (constantly 1)
                    (lambda (page-number)
                      (declare (ignore page-number))
                      page)))))

;;; ------------------------------------------------------------ vector jobs

#+(or win32 darwin)
(progn

  (defun %discard-empty-print-output (service output)
    "Remove OUTPUT when the job left it empty.

A job that prints to a file names its destination in DOCINFO.lpszOutput
(PRINT-OUTPUT-FILE), and StartDoc creates that file before the first page is
drawn.  A job that then ends without writing -- cancelled, or failed -- leaves it
behind at zero bytes, and the user sees a PDF that no reader can open.  Nothing is
said about it: vanilla's cancelled job shows nothing either (om-LW/print.lisp:
om-print-document).

The file belongs to the SPOOLER, not to this process, and it is still open there
when the job's own device context is already gone -- so the destination is tried
until it can be opened, for at most *PRINT-OUTPUT-RELEASE-SECONDS*.  This runs on
the print thread, which has nothing left to do (%SPAWN-PRINT-JOB), so the wait
costs the interface nothing.

Only an empty file is removed; anything the spooler did write is the user's."
    (when (and output (probe-file output))
      (let ((deadline (+ (get-internal-real-time)
                         (* *print-output-release-seconds*
                            internal-time-units-per-second)))
            (last nil))
        (loop
          (handler-case
              (return
                (when (zerop (with-open-file (stream output
                                                     :element-type '(unsigned-byte 8))
                               (file-length stream)))
                  (delete-file output)
                  t))
            (error (condition) (setf last condition)))
          (when (> (get-internal-real-time) deadline)
            ;; the reason has to be recoverable, but a destination the spooler
            ;; never released is not a failed print
            (return (print-failure service :print-output last)))
          (sleep 0.1)))))

  (defun %finish-print-job (service window progress status detail)
    "End a print job, on the thread that owns the windows.
Runs for every outcome -- printed, cancelled or failed -- and is the only place the
job's effects on the interface are undone."
    (note-printing-window service nil)
    (%set-other-windows-enabled (progress-window-native progress) t)
    (close-progress-window progress)
    ;; the progress window held the activation, and destroying it releases the
    ;; foreground to whichever window the system picks next.  The document's
    ;; window takes it back.
    (activate-print-window service window)
    (when (eq status :failed) (print-failure service :print-job detail))
    (%release-print-job service))

  (defun %spawn-print-job (service name window title settings output page-count
                           progress page-function)
    "Draw the document on the printer, on a thread of its own named NAME.

The whole GDI job lives here -- the device context, StartDoc, every page and EndDoc
-- because a printer DC belongs to the thread that created it and cannot be driven
from another.  Nothing in this thread touches the interface directly: the progress
text and the epilogue are posted to the thread that owns the windows
(core/ui-thread.lisp: post-to-ui-thread), and the host is told which window the
job reads (NOTE-PRINTING-WINDOW).

Vanilla has no such thread because CAPI keeps pumping inside its own job
(om-LW/print.lisp: om-print-document).  Here EndDoc is one call that returns only
when the spooler has taken the whole document; left on the thread that owns the
windows it stops the application answering, and Windows then replaces every one of
its windows with a ghost."
    (bordeaux-threads:make-thread
     (lambda ()
       (let ((status nil)
             (detail nil))
         (unwind-protect
              (handler-case
                  (multiple-value-setq (status detail)
                    (paper-print:print-pages
                     page-count
                     (lambda (hdc page-number origin-x origin-y
                              printable-width printable-height dpi-x dpi-y)
                       (declare (ignore printable-width printable-height))
                       (post-to-ui-thread
                        (lambda ()
                          (set-progress-message
                           progress
                           (format nil "Printing page ~D..." page-number))))
                       (funcall page-function hdc page-number page-count
                                origin-x origin-y dpi-x dpi-y))
                     :title title
                     :settings settings
                     :output output
                     :abort-hook (lambda () (progress-cancelled-p progress))
                     #+darwin :on-main-thread #+darwin #'call-on-ui-thread))
                (error (condition)
                  (setf status :failed detail condition)))
           ;; the interface is restored FIRST: the destination may still be open
           ;; in the spooler, and waiting for it here would hold the progress
           ;; window on the screen for as long as it takes
           (post-to-ui-thread
            (lambda () (%finish-print-job service window progress status detail)))
           (unless (eq status :printed)
             (%discard-empty-print-output service output)))))
     :name name))

  (defun %run-vector-print-job (service window title page-count-function page-function
                                name)
    "Open the native Print dialog, then start the job that draws the document.

This half runs on the thread that owns the windows: the two native dialogs, the
page count, and the progress window, which is raised over WINDOW.  Every other
window is then disabled, exactly as a modal dialog would disable them, so the only
input the job can take is its own Cancel -- and the drawing is handed to a worker,
so the event loop keeps running and that button keeps answering.

Answers :HANDED-OVER once the worker owns the job, which is what tells
START-PRINT-TASK the print slot has been handed over."
    (let ((settings (select-printer service :owner window)))
      ;; Cancel is a normal result and shows nothing, as in vanilla
      (when settings
        (multiple-value-bind (output cancelled) (print-output-file service settings title)
          (unless cancelled
            (let ((page-count (funcall page-count-function)))
              ;; the document's own window in front FIRST, so the progress window
              ;; -- which is raised as it is shown -- lands directly on top of the
              ;; window being printed and not on top of whatever the native
              ;; dialogs left in front
              (activate-print-window service window)
              (let ((progress (open-progress-window service window "Preparing to print...")))
                (%set-other-windows-enabled (progress-window-native progress) nil)
                (note-printing-window service window)
                (%spawn-print-job service name window title settings output page-count
                                  progress page-function)
                :handed-over)))))))

  (defun print-document (service window title page-count-function page-function
                         &key (name "Print"))
    "Print a document the caller draws page by page on the printer's own device, as
the print operation NAME, after the native Print dialog opened over WINDOW.

PAGE-COUNT-FUNCTION runs on the thread that owns the windows once the dialog was
accepted and answers the number of pages.  PAGE-FUNCTION then runs on the job's own
thread, once per page in ascending order, with
  (device page-number page-count origin-x origin-y dpi-x dpi-y)
and draws that page -- through CALL-WITH-PRINTER-PORT, which turns the port drawing
primitives into device drawing.  Meanwhile a progress window says which page is
being printed and offers Cancel; every other window is disabled; the event loop
keeps running.  Answers T when the job was handed to its thread."
    (start-print-task
     service name
     (lambda ()
       (%run-vector-print-job service window title page-count-function page-function
                              name)))))
