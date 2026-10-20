(in-package :om-api)

(export '(om-page-setup
          om-print-window
          om-print-one-page
          om-print-view
          om-compute-page-number
          om-print-document
          om-score-paper-size
          ) :om-api)

;;;=========================================================================
;;; PRINT UTILITY
;;;
;;; The seven exports keep the vanilla names, arities and return types
;;; (om-LW/print.lisp).  Vanilla drives CAPI's printer port directly; here the
;;; printer settings, the native dialogs, the jobs and the printer port belong to
;;; the library (alloy-desktop/printing/printing.lisp, printing/gdi-port.lisp),
;;; and this file carries only what is OpenMusic's: the page each object draws,
;;; the window it is printed from, the fonts and the look of the dialogs.
;;;=========================================================================

(defvar *score-thickness* nil)
(setf *score-thickness* 0.4)

(defun om-draw-score-line (x1 y1 x2 y2)
  ;; vanilla draws into *CURSTREAM* with an explicit :thickness
  ;; (om-LW/print.lisp: om-draw-score-line); here the port is already selected
  ;; and the pen width is the dynamic *CURRENT-PEN-SIZE* om-with-line-size binds
  (om-with-line-size *score-thickness*
    (om-draw-line x1 y1 x2 y2)))

;;;------------------------------------------------------------ the service

(defclass om-printing-service (alld:printing-service) ()
  (:documentation "The session's printing, with OpenMusic's answers to the library's
printing hooks."))

(defvar *om-printing-service*
  (make-instance 'om-printing-service :warning-prefix "OM printing")
  "The printer settings, page geometry and print slot of the session.")

(defmethod alld:activate-print-window ((service om-printing-service) window)
  ;; om-select-window, not a bare raise: a kernel window answers it too
  ;; (kernel palette.lisp: om-select-window for EditorWindow re-opens its palettes)
  (when (and window (om-window-open-p window))
    (om-select-window window)))

(defmethod alld:choose-print-output-file ((service om-printing-service) name)
  ;; the ordinary OM Save dialog, so the destination is owned, placed and
  ;; cancellable like every other one
  (om-choose-new-file-dialog :prompt "Print to File"
                             :name name
                             :types '("PDF file" "*.pdf")))

(defmethod alld:note-printing-window ((service om-printing-service) window)
  ;; the frame loop does not repaint the window a job draws from its own thread
  ;; (window.lisp: %window-frozen-p)
  (setf *printing-window* window))

;;;--------------------------------------------------------------- page units

(defun %om-logical-screen-dpi ()
  "The logical resolution of the user's coordinate space (alloy-desktop
printing/printing.lisp: logical-screen-dpi): 120 on a 125% Windows display."
  (alld:logical-screen-dpi *om-printing-service*))

(defun %print-title (object)
  "Document name for the spooler; it crosses an FFI string boundary."
  (let ((name (om-get-view-name object)))
    (if (stringp name) name "OpenMusic document")))

;;; om-print-view has no method here, exactly as in vanilla (om-LW/print.lisp
;;; exports it and stops there): the score editor is what implements it
;;; (projects .../scoreeditors.lisp: om-print-view for scorePanel and for
;;; scoreeditor).  A default answering NIL would turn "no applicable method"
;;; into a silently blank page.
(defgeneric om-print-view (self page-size page-number page-count))

(defmethod om-compute-page-number ((view om-graphic-object) page-size)
  (declare (ignore page-size))
  1)

(defgeneric %call-with-print-page (view page-number function)
  (:documentation
   "Run FUNCTION with whatever VIEW needs in place to draw its PAGE-NUMBER at the
resolution the running print job records at.

A view that draws the page itself needs nothing: the recorder is already sampling
at that resolution and the drawing lands on it.  The score is the exception --
om-print-view blits a page PICTURE the editor made at screen resolution (projects
.../scoreeditors.lisp: om-print-view for scorePanel), and a picture cannot gain
detail by being stretched, so the score swaps in a picture of its own for the
page being printed.")
  (:method (view page-number function)
    (declare (ignore view page-number))
    (funcall function)))

;;;--------------------------------------------------------- native backend

(defun %close-paper-printer-settings ()
  "Release the session's printer settings.  A quit action, so it answers T."
  (alld:close-printer-settings *om-printing-service*))

(pushnew '%close-paper-printer-settings *om-quit-actions*)

(defun %view-raster-page (view)
  "The pixels VIEW currently shows on screen, as one raster page.

Vanilla captures the live pane and crops 15 pixels off the right edge and off
the bottom (om-LW/print.lisp: om-print-one-page, whose GP:MAKE-IMAGE-FROM-PORT
rectangle starts at the pane's top-left).  Alloy and GL count rows upwards, so
the kept top rows begin 15 pixels above the view's own origin -- reading from
the origin would crop the wrong end."
  (let ((window (om-view-window view)))
    (unless window
      (error "om-print-one-page: ~S is not in a window." view))
    (let ((location (alloy:global-location view)))
      (alld:capture-raster-page window
                                (round (alloy:pxx location))
                                (+ 15 (round (alloy:pxy location)))
                                (max 1 (- (round (om-width view)) 15))
                                (max 1 (- (round (om-height view)) 15))))))

(defun %record-raster-page (size draw-function)
  "Run DRAW-FUNCTION into a SIZE-wide white offscreen page and hand back the
raster.  This is the same recorder the score already draws its pages with
(icons-picts.lisp: %om-record-pict-call), which hops to the UI thread itself.

The recorder samples at *OM-RECORD-PICT-SCALE*, so the raster has more pixels
than SIZE has units; SIZE stays the page's logical size and the backend scales
the sheet from it."
  (let ((picture (%om-record-pict-call nil size draw-function)))
    (unless picture
      (error "om-print-document: the page recorder produced no picture."))
    (unwind-protect
         (let ((image (alld:picture-image picture)))
           (alld:opaque-raster-page (alld:picture-width picture) (alld:picture-height picture)
                                    (org.shirakumo.fraf.imagine:data image)
                                    (org.shirakumo.fraf.imagine:origin image)
                                    :logical-width (max 1 (round (om-point-h size)))
                                    :logical-height (max 1 (round (om-point-v size)))))
      (om-kill-picture picture))))

;;; ------------------------------------------------------------ printer port
;;; Vanilla selects the printer port into *CURSTREAM* and the ordinary drawing
;;; primitives then land on paper (om-LW/print.lisp: om-print-document).  Here
;;; the library's printer port takes the primitives (alloy-desktop
;;; printing/gdi-port.lisp: call-with-printer-port); what is OpenMusic's is the
;;; focused-view offset, the fonts it ships and the page each view draws.

#+(or win32 darwin)
(progn

  (defmethod %port-call-with-focused-view ((port alld:printer-port) view thunk)
    ;; on a printer port there is no renderer to focus and none is wanted: vanilla
    ;; only rebinds the port and the child offset here (om-LW/graphics.lisp:
    ;; om-with-focused-view).  The screen method would either drop the drawing, for
    ;; a view whose window cannot be found, or make a GL context current for a page
    ;; that is not going to the screen at all.
    (let ((*curstream* (or view *curstream*)))
      (if (or (null view) (null *current-view*) (eq view *current-view*))
          (funcall thunk)
          (multiple-value-bind (tx ty) (%view-window-xy view)
            (multiple-value-bind (cx cy) (%view-window-xy *current-view*)
              (let ((*pox* (+ *pox* (- tx cx)))
                    (*poy* (+ *poy* (- ty cy))))
                (funcall thunk)))))))

  #+win32
  (defvar *bundled-fonts-registered-p* nil
    "True once the bundled faces have been made resolvable by name to GDI.
A private registration lasts for the process, so it is done once.")

  #+win32
  (defun %register-bundled-fonts ()
    "Let GDI resolve the faces OpenMusic ships -- the four music fonts and the
bundled text fallback -- by family name.  Without it CreateFontIndirectW
substitutes an installed face and the music glyphs come out as letters."
    (unless *bundled-fonts-registered-p*
      (setf *bundled-fonts-registered-p* t)
      (dolist (entry alld:*font-atlases*)
        (destructuring-bind (name family slant weight kind . files) entry
          (declare (ignore name family slant weight))
          (when (eq kind :bundled)
            (let ((path (alld:find-font-source kind files)))
              (if (null path)
                  (warn "OM printing: no source file for a bundled font (~{~A~^, ~})."
                        files)
                  (dolist (refused (alld:register-private-fonts (list path)))
                    (warn "OM printing: GDI refused the font ~A." refused)))))))))

  (defun %print-font-description ()
    "The printer port's text face: the current OM font, or the port's default."
    (let ((font *current-font*))
      (if font
          (values (omfd-family font) (om-font-px-size font)
                  (eq (omfd-weight font) :bold) (eq (omfd-slant font) :italic))
          (values nil nil nil nil))))

  (defun %draw-print-page (self hdc page-size page-number page-count
                           origin-x origin-y dpi-x dpi-y)
    "Draw one page of SELF straight onto the printer device context.

*PRINTER-PORT* is bound only here, around the drawing of the page and nothing
else: a frame the job pumps between pages must still go to the screen.  The
view's own font is the port's default, as the screen pass makes it (graphics.lisp:
%call-with-focused-view); a port with no font draws every glyph in a text face."
    (let ((logical (%om-logical-screen-dpi)))
      #+win32 (%register-bundled-fonts)
      (alld:call-with-printer-port
       hdc origin-x origin-y dpi-x dpi-y logical
       (lambda (port)
         (let ((*printer-port* port)
               (*default-printer-port* port)
               (*curstream* port)
               (*current-font* (om-get-font self)))
           (%call-with-print-page
            self page-number
            (lambda ()
              (om-print-view self page-size page-number page-count)))))
       :font-function #'%print-font-description)))

  ;;; the window the user sees while a document spools: vanilla's (LispWorks,
  ;;; its print progress window), in the OM dialog's own look
  (defmethod alld:make-progress-surface ((service om-printing-service) owner x y title
                                         message cancel-function)
    (declare (ignore owner))
    (let*((window (om-make-window 'om-dialog
                                   :position (om-make-point x y)
                                   :size (om-make-point 320 120)
                                   :resizable nil :maximize nil
                                   :minimize nil
                                   :window-title title
                                   :bg-color *om-window-def-color*))
           (text (om-make-dialog-item 'om-static-text (om-make-point 20 22)
                                      (om-make-point 280 38) message
                                      :font *om-default-font2*)))
      (om-add-subviews window text
                       (om-make-dialog-item 'om-button (om-make-point 120 72)
                                            (om-make-point 80 24) "Cancel"
                                            :di-action
                                            (om-dialog-item-act item
                                              (declare (ignore item))
                                              (funcall cancel-function))))
      (om-show-window window)
      (values window (lambda (string) (om-set-dialog-item-text text string))))))

(defun om-page-setup ()
  (alld:page-setup *om-printing-service* :owner (om-front-window) :name "OM Page Setup"))

(defmethod om-score-paper-size ()
  (alld:page-size *om-printing-service* :make-size #'om-make-point))

(defun om-print-one-page (self)
  ;; vanilla captures the pane BEFORE opening the dialog, so the picture is
  ;; the one the user was looking at and no dialog can occlude it
  (let ((page (%view-raster-page self))
        (window (om-view-window self))
        (title (%print-title self)))
    (alld:print-one-page *om-printing-service* window title page :name "OM Print")))

#+(or win32 darwin)
(defmethod om-print-document ((self om-graphic-object))
  (let ((window (om-view-window self))
        (title (%print-title self))
        (page-size nil))
    (alld:print-document
     *om-printing-service* window title
     (lambda ()
       (setf page-size (om-score-paper-size))
       (om-compute-page-number self page-size))
     (lambda (hdc page-number page-count origin-x origin-y dpi-x dpi-y)
       (%draw-print-page self hdc page-size page-number page-count
                         origin-x origin-y dpi-x dpi-y))
     :name "OM Print")))

#+linux
(defmethod om-print-document ((self om-graphic-object))
  (let ((service *om-printing-service*)
        (window (om-view-window self))
        (title (%print-title self))
        (page-size nil)
        (page-count 1)
        (scale 1))
    (alld:start-print-task
     service "OM Print"
     (lambda ()
       (alld:run-print-job
        service window title
        (lambda ()
          ;; the accepted settings invalidated the cache, so this re-reads the
          ;; chosen printer -- and it is the SAME value the score laid its pages
          ;; out from (projects .../pagination.lisp: make-score-pages-picts
          ;; sizes them from score-paper-size)
          (setf page-size (om-score-paper-size))
          (setf scale (alld:print-page-scale service (om-point-h page-size)
                                             (om-point-v page-size)))
          ;; the page COUNT is a layout question, answered in logical units; only
          ;; the page being printed is recorded at the printer's resolution
          (setf page-count
                (alld:call-on-ui-thread (lambda () (om-compute-page-number self page-size)))))
        (lambda (page-number)
          ;; one hop for the whole page: the record scale is a dynamic binding
          ;; and dynamic bindings do not cross into the UI thread, so it is bound
          ;; THERE, around both the page's own preparation and the recording
          (alld:call-on-ui-thread
           (lambda ()
             (let ((*om-record-pict-scale* scale))
               (%call-with-print-page
                self page-number
                (lambda ()
                  (%record-raster-page
                   page-size
                   (lambda ()
                     ;; vanilla binds the printer port around the page drawing
                     ;; (om-LW/print.lisp: om-print-document); here the port being
                     ;; drawn into is the recorder's *CURSTREAM*
                     (let ((*default-printer-port* *curstream*))
                       (om-print-view self page-size page-number page-count)))))))))))))))

(defmethod om-print-window ((self om-abstract-window))
  (om-print-document self))
