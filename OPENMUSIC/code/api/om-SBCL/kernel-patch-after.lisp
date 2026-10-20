(in-package :om)

;; Scope initarg leniency to the OM metaclass (LW doesn't check; kernel saves rely on it).
(defmethod make-instance :around ((class omstandardclass) &rest initargs)
  (if (getf initargs :allow-other-keys)
      (call-next-method)
      (apply #'call-next-method class :allow-other-keys t initargs)))

;;; LispWorks LOOP accepts a zero numeric step and lets NUMMAX terminate the
;;; series. SBCL rejects a floating zero in BY before the loop can run. Keep the
;;; vanilla contract explicitly; non-zero paths remain identical to the source.
(defmethod arithm-ser ((begin number) (end number) (step number)
                       &optional (nummax most-positive-fixnum))
  "Return the vanilla arithmetic series, including its zero-step behaviour."
  (cond ((plusp step)
         (loop for value from begin to end by step
               for counter from 1 to nummax
               collect value))
        ((zerop step)
         (when (>= begin end)
           (loop repeat nummax
                 for value = begin then (+ value step)
                 collect value)))
        (t
         (loop for value from begin downto end by (abs step)
               for counter from 1 to nummax
               collect value))))

;;; Speed colouring in traject-editor.lisp reduces the segment-speed list
;;; without an initial value and divides by its range. Empty/single-point curves
;;; therefore call MAX with no arguments, while constant-speed curves divide by
;;; zero. Returning NIL deliberately activates 3D-CURVE's existing per-vertex
;;; fallback; a zero range maps every vertex to the Min colour.
(defun filter-and-normalize-speeds-list (speeds)
  "Normalize trajectory speeds without failing on degenerate curves."
  (when speeds
    (let* ((max-speed (reduce #'max speeds))
           (min-speed (reduce #'min speeds))
           (range-speed (- max-speed min-speed)))
      (if (zerop range-speed)
          (make-list (length speeds) :initial-element 0.0)
          (om/ (om- speeds min-speed) range-speed)))))

(defun %trajectory-speed-mode-error (editor)
  "Return a user-facing reason when EDITOR cannot display Speed colours."
  (let* ((object (get-current-object editor))
         (points (format-3d-points object))
         (times (times object)))
    (cond ((< (length points) 2)
           "Speed mode requires at least two points.")
          (t
           (let ((speeds
                   (loop for first on points
                         for index from 0
                         while (second first)
                         collect
                         (let ((duration
                                 (- (or (nth (1+ index) times) 0)
                                    (or (nth index times) 0))))
                           (if (zerop duration)
                               -1
                               (/ (3d-points-distance
                                   (first first) (second first))
                                  duration))))))
             (when (= (reduce #'max speeds) (reduce #'min speeds))
               "Speed mode requires varying segment speeds."))))))

(defmethod param-color-mode :around
    ((self traject-editor) &optional (set-value nil set-value-supplied-p))
  "Reject an undefined Speed colour range before it reaches continuous redraw."
  (let ((reason (and set-value-supplied-p
                     (eql set-value 2)
                     (%trajectory-speed-mode-error self))))
    (if (null reason)
        (call-next-method)
        (let ((current (get-edit-param self 'color-mode)))
          (let ((popup (find-if
                        (lambda (item)
                          (typep item 'oa::om-pop-up-dialog-item))
                        (curve-buttons (ctrlp self)))))
            (when popup
              (oa::om-set-selected-item-index popup current)))
          (om-message-dialog reason)
          current))))

(defun %pathname-host-lw (pathname)
  "Windows drive letter (LispWorks keeps it in the host slot); NIL when absent."
  (let ((dev (pathname-device pathname)))
    (and (stringp dev) (plusp (length dev)) dev)))

(defun om-save-pathname (pathname)
  "Serialize PATHNAME as a reader form, drive in the host slot (LW convention)."
  (when pathname
    `(om-make-pathname
      :directory ',(pathname-directory pathname)
      :device nil
      :host ,(%pathname-host-lw pathname)
      :name ,(pathname-name pathname)
      :type ,(pathname-type pathname))))

(defun om-save-pathname-relative (path &optional from)
  (when path
    (let ((path2 (or from (relative-path-reference))))
      (if path2
          (let* ((relpath (path-in-dir-p path path2))
                 (dir (if (consp relpath)
                          (append (list :relative) (remove nil relpath))
                          (pathname-directory path))))
            `(om-make-pathname
              :directory ',dir
              :device nil
              :host ,(and relpath (%pathname-host-lw path))
              :name ,(pathname-name path)
              :type ,(pathname-type path)))
          (om-save-pathname path)))))

;;; Optional-project symbols
;;;
;;; build/build-om.lisp (load-om-projects) wraps EACH project load in a handler-case
;;; that swallows a serious-condition and moves on, and the two vanilla trees do not
;;; load fluid/omsounds at all (the load-om-projects calls for omsounds and fluid are
;;; commented out in both upstream build-om.lisp trees this port tracks).
;;; So a package-qualified token like cl-fluid::getsptr or juce::getAudioSourcePos is
;;; a READ-time reference to a package that may not exist, and this file is loaded
;;; with no handler around it (build-om.lisp: the compile&load of kernel-patch-after):
;;; one skipped project would take down every patch in here.  Resolve at runtime
;;; instead -- the pattern the fork already uses in editors/om-table-editor.lisp
;;; (%table-openmusic-slot).

(defun %optional-fdefinition (package-name symbol-name)
  "Function named PACKAGE-NAME::SYMBOL-NAME, or NIL if the project never loaded."
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol (string-upcase symbol-name) pkg))))
    (and sym (fboundp sym) (fdefinition sym))))

(defun %optional-symbol-value (package-name symbol-name &optional default)
  "Value of PACKAGE-NAME::SYMBOL-NAME, or DEFAULT if unbound / project not loaded."
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol (string-upcase symbol-name) pkg))))
    (if (and sym (boundp sym)) (symbol-value sym) default)))

;;; Player timing

(defun %native-sound-player-time-ms (player)
  "Return the JUCE source position for a single sound editor, or NIL.

The scheduler clock is appropriate for scores and multi-engine playback.  A
sound editor has a more accurate source: the sample position of the audio source
that is actually feeding the device."
  ;; same optional-project rule as the header above, applied to the ONE thing that is
  ;; optional here: with omsounds skipped there is no SOUNDEDITOR class -- and with the
  ;; class present its generics are present too, so they are called, not intercepted
  (let ((editor (caller player))
        (class  (find-class 'soundeditor nil)))
    (when (and class
               (typep editor class)
               (eq (get-player-engine editor) :om-audio))
      (let* ((sound  (get-obj-to-play editor))
             (source (and sound (player-data sound)))
             (rate   (and sound (om-sound-sample-rate sound)))
             (source-pos (%optional-fdefinition "JUCE" "getAudioSourcePos")))
        (when (and source source-pos (numberp rate) (plusp rate))
          (round (* 1000 (/ (funcall source-pos source) rate))))))))

(om-with-redefinitions
  (defun clock-time ()
    "Return the monotonic player clock in milliseconds."
    (round (get-internal-real-time)
           (/ internal-time-units-per-second 1000)))

  (defun get-player-time (player)
    "Return musical time in milliseconds, using the active audio source when possible."
    (cond ((equal (state player) :play)
           (or (%native-sound-player-time-ms player)
               (+ (player-offset player)
                  (start-time player)
                  (- (clock-time) (ref-clock-time player)))))
          ((equal (state player) :pause)
           (+ (player-offset player) (start-time player)))
          (t 0)))

  (defmethod start-recording ((self scoreeditor))
    (om-print "start MIDI recording" "OM ::")
    (if (midi-recorder-editor *midi-recorder*)
        (om-beep-msg
         (format nil "ERROR: MIDI recording is already ON in ~A"
                 (om-window-title
                  (om-view-window
                   (midi-recorder-editor *midi-recorder*)))))
        (progn
          (setf (recording self) t)
          (setf (midi-recorder-editor *midi-recorder*) self)
          (when (midi-recorder-process *midi-recorder*)
            (midi-in-stop (midi-recorder-process *midi-recorder*)))
          (setf (midi-recorder-memory *midi-recorder*) nil)
          (setf (midi-recorder-t0 *midi-recorder*) (clock-time))
          (setf (midi-recorder-process *midi-recorder*)
                (midi-in-start
                 *def-midi-in*
                 (lambda (msg time)
                   (declare (ignore time))
                   (cond
                     ((equal (state (player self)) :play)
                      (setf (om-midi:midi-evt-date msg)
                            (get-player-time (player self)))
                      (push msg (midi-recorder-memory *midi-recorder*)))
                     ((not (timed-recording self))
                      (setf (om-midi:midi-evt-date msg)
                            (- (clock-time)
                               (midi-recorder-t0 *midi-recorder*)))
                      (push msg (midi-recorder-memory *midi-recorder*)))
                     (t nil)))
                 1
                 *def-midi-out*))))))

;;; The kernel has explicit persistence for CHECK-BOX but not RADIO-BUTTON
;;; (kernel dialog-item-boxes.lisp: omng-save for check-box, with no equivalent for
;;; radio-button). Under LispWorks the generic OM saver can
;;; reconstruct the CAPI-backed radio. On SBCL that fallback descends into the
;;; Alloy component slots and produces an invalid constructor (missing required
;;; layout/data arguments). Keep the kernel source untouched and provide the
;;; irreducible backend-specific persistence method at the official after-load
;;; boundary. Its representation mirrors the kernel CHECK-BOX method.
(defmethod omng-save ((self radio-button) &optional (values? nil))
  (declare (ignore values?))
  `(let ((rep (om-make-dialog-item
               'radio-button
               (om-make-point 1 1)
               (om-make-point ,(om-width self) ,(om-height self))
               ,(om-dialog-item-text self))))
     (om-set-check-box rep ,(om-checked-p self))
     rep))

;;; SINGLE-ITEM-LIST gained a second "sel" input; a patch saved with only the
;;; first input reloads through this method (projects sound.lisp: correct-box-inputs
;;; for sound), which rebuilds the input list from the class's current shape.
(defmethod correct-box-inputs ((class (eql 'single-item-list)) inputs)
  (loop for input in (get-inputs-from-inst (make-instance class))
        for i = 0 then (+ i 1) collect
        (if (and (nth i inputs) (string-equal (name input) (name (nth i inputs))))
            (nth i inputs) input)))

;;; Function boxes are OMAtomicFrame pinboard objects, so their controls are
;;; flattened into the patch panel.  The cleanup of NEW-FUN-ENTER-VIEW updates
;;; the new frame after those controls were entered and raises the frame above
;;; its own resize handle.  Reapply the authoritative OM subtree order only
;;; after that complete transaction: body first, controls last/topmost.
(defun %restack-new-frame-subtree (frame)
  (let ((panel (and frame (om-view-container frame))))
    (when (typep panel 'org.shirakumo.alloy:vector-container)
      (labels ((walk (view)
                 (when (and (typep view 'org.shirakumo.alloy:layout-element)
                            (slot-boundp view 'org.shirakumo.alloy::layout-parent)
                            (eq panel (org.shirakumo.alloy:layout-parent view)))
                   (org.shirakumo.alloy:update
                    view panel
                    :index (1- (length (org.shirakumo.alloy:elements panel)))))
                 (when (typep view 'oa::om-graphic-object)
                   (dolist (child (oa::vsubviews view))
                     (walk child)))))
        (walk frame)))))

(defmethod exit-from-dialog :around ((self new-fun-enter-view) str)
  (let ((new-box (call-next-method)))
    (when new-box
      (%restack-new-frame-subtree (first (frames new-box))))
    new-box))

;;; Pane rect of the three interface-box classes (kernel dialog-item-boxes.lisp: the
;;; button, check-box and radio-button classes),
;;; in the vanilla's colours; every other dialog item keeps the NIL default.
(defmethod oa::%di-frame-color ((self button))
  (om-make-color (/ 253 255.0) (/ 253 255.0) (/ 253 255.0)))
(defmethod oa::%di-frame-border-color ((self button))
  (om-make-color (/ 208 255.0) (/ 208 255.0) (/ 208 255.0)))

(defmethod oa::%di-frame-color ((self check-box)) *om-white-color*)

(defmethod oa::%di-frame-color ((self radio-button)) *om-white-color*)

;;; Unlike native dialog radios, a radio used as a patch interface box may be
;;; cleared so the visual program can represent an explicit false value.
(defmethod alld:radio-may-uncheck-p ((self oa:om-radio-button))
  (loop for view = (oa:om-view-container self) then (oa:om-view-container view)
        while view
        thereis (dieditorframe-p view)))

;;; Single-grace head/stem/flag in *heads-font* (omheads), as openmusic-8.0
;;; graces.lisp (draw-grace-notes for s-grap-grace-notes) and the group-grace path
;;; do; the working-tree grace-font (=*micron-font*) drew them in the accidental font.
(defmethod draw-grace-notes ((self s-grap-grace-notes) x y zoom minx maxx miny maxy slot size linear? staff chnote)
  (om-with-fg-color nil *grace-color*
    (let* ((dir (not-stem-dir (stemdir (grc self))))
           (thenotes (inside self))
           (off (cond
                 ((and (= (offset (reference self)) 0)
                       (= (offset (reference (parent self))) 0))
                  (round (* 25 zoom)))
                 ((and (= (offset (reference self)) 0) (first-of-meas-p (reference self)))
                  (round (* 35 zoom)))
                 (t 0))))
      (loop for item in thenotes do
            (draw-head-grace item (+ x off) y zoom minx maxx miny maxy slot size linear? staff chnote))
      (collect-rectangles self)
      (om-with-font (om-make-music-font *heads-font* (round size *grace-factor*))
                    (draw-chord-grace-stem self x y zoom (beams-num self) dir (round size *grace-factor*))))))

(defmethod draw-head-grace ((self t) x y zoom minx maxx miny maxy slot size linear? staff chnote)
  (declare (ignore minx maxx miny maxy linear?))
  (let* ((new-size (/ size *grace-factor*))
         (realpos (round (+ x 5 (* zoom (x self)))))
         (altpos (if (alteration self)
                     (round (- (+ realpos (* (- (alteration self) 1) (/ new-size 4)))
                               (* (/ new-size 3.5) (delta-head self))))
                     realpos))
         (str (headchar self))
         (headsizex (get-name-size str (om-make-font *music-font* size)))
         (note-color *grace-color*)
         (altstr (string (alt-char self))))
    (om-with-fg-color nil (if chnote (nth (chan (reference self)) *16-color-list*) note-color)
      (om-with-font (om-make-music-font *heads-font* new-size)
                    (om-draw-string realpos (+ y (y self)) str))
      (when (alteration self)
        (om-with-font (grace-font new-size)
                      (om-draw-string altpos (+ y (y self)) altstr)))
      (setf (rectangle self) (list altpos (+ y (- (y self) (round new-size 8)))
                                   (+ realpos (round new-size 3)) (+ y (round new-size 8) (y self)))))
    (draw-auxiliar-grace-lines self x y size (- realpos 5) headsizex)))

;;; Page-mode bigchord count in *signs-font* (omsign, has digits), as the score-mode
;;; twin in projects scoretools.lisp (draw-object-ryth for grap-ryth-chord); the
;;; original omheads (get-font-to-draw 8) has no digits.
(defmethod page-draw-object-ryth ((self grap-ryth-chord) score pagenum x y zoom slot size staff chnote)
  (om-with-fg-color nil (mus-color (reference self))
    (let* ((dir (stemdir self))
           (thenotes (copy-list (inside self))))
      (loop for item in thenotes do
            (page-draw-object-ryth item score pagenum x y zoom slot size staff chnote))
      (collect-rectangles self)
      (when (bigchord self)
        (om-with-font (om-make-font *signs-font* (round size 1.6))
                      (om-draw-string (+ (car (rectangle self)) (round size 5))
                                      (+ (round size 7) (fourth (rectangle self)))
                                      (format nil "~D" (bigchord self)))))
      (when (figure-? self)
        (when (stem self)
          (draw-chord-beams self x y zoom (beams-num self) dir size)))
      (draw-extras self (om-get-current-port) size staff zoom)
      (when *om-tonalite*
        (draw-chiffrage self x y zoom size)))
    (page-draw-action-boxes self score)))

;;; kernel editorwindow.lisp (the om-window-close-event :around for EditorWindow) closes
;;; every editor with (setf (Editorframe (object (panel self))) nil), but `object' is a
;;; slot of OMFrame (kernel metaclasses.lisp: the object slot of the defclass OMFrame)
;;; and these panels lack it in their CPL.  The value is the metaobject OpenObjectEditor
;;; registered (kernel modele.lisp: OpenObjectEditor for OMBasicObject): the editor's
;;; REF, or (object editor) when the editor opens with no ref (projects om-choose.lisp:
;;; set-choose-dialog).
(defun %panel-editor-registrant (panel)
  (let ((win (om-view-window panel)))
    (when win
      (let ((ed (editor win)))
        (when ed (or (ref ed) (object ed)))))))

;;; One method per panel class: om-view/om-scroller precede OMFrame in the CPL of the
;;; panels that DO own the object slot (patchPanel/scorePanel, kernel metaclasses.lisp:
;;; the defclass OMContainerFrame), so
;;; a bridge-class method would shadow them. EVAL fixes each specializer per loaded class.
(dolist (%panel-class '(soundPanel bpfpanel arraypanel pictpanel omchoosePanel
                        controllerPanel audiocontrollerPanel fluidcontrollerPanel
                        fluidchannelcontrollerPanel midipanel cerclePanel trillpanel trackpanel
                        sdif-desc-panel sdif-data-panel sdif-fstreamdesc-panel
                        sdif-mstreamdesc-panel))
  (eval `(defmethod object ((self ,%panel-class)) (%panel-editor-registrant self))))

;;; A non-resizable CAPI editor has no usable maximize action. On Win32, keeping
;;; GLFW's maximize title button gives the native frame a 200 px tracking minimum,
;;; widening a two-channel AUDIO-MIX-CONSOLE whose vanilla content is 160 px.
;;; Remove that button, then restore the object's own editor width while preserving
;;; the already-computed menu/chrome height.
(defmethod make-editor-window :around
    ((class (eql 'audiocontrollerEditor)) object name ref
     &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let ((window (call-next-method)))
    (when window
      (let* ((editor (editor window))
             (wanted (get-win-ed-size object))
             (chrome-height (max 0 (- (h window) (h editor))))
             (width (om-point-h wanted))
             (height (+ (om-point-v wanted) chrome-height)))
        (oa::%apply-native-window-style window :maximize nil)
        (om-set-view-size window (om-make-point width height))
        (oa::set-not-resizable window width height)))
    window))

;;; Same close method, other argument: `panel' of the window delegates to the editor
;;; (kernel editorwindow.lisp: panel for EditorWindow) and the EditorView `panel' slot
;;; starts NIL (the panel slot of the defclass EditorView).
;;; Six editor classes never fill it -- sdifeditor, StreamEditor, inputEditor, 3DEditor,
;;; sheetEditor, boardEditor -- so the line evaluates (object NIL), then (setf (Editorframe
;;; NIL) NIL). The kernel writes this very expression guarded elsewhere
;;; (kernel metaobjectcontainer.lisp: close-editor-after for metaobj-editor), and that
;;; call is the last form of the :around, after close-editorFrame already cleared the
;;; box (kernel editorwindow.lisp: close-editorFrame for EditorView).

(defmethod object ((self null))
  "No panel means no registrant -- answer NIL instead of signalling."
  nil)

(defmethod (setf editorframe) (val (self null))
  "Setting the editor frame of no object is a no-op, not a signal."
  val)

;;; kernel graphics/icons/picture.lisp (omNG-save for internepicture) registers the
;;; picture resource with :host (pathname-device
;;; restoredfolder): under LispWorks a Windows drive letter lives in the host slot,
;;; but on SBCL a string host names a LOGICAL host and signals, aborting the whole
;;; patch save. Same body, host taken from the source pathname, drive left in :device.
(om-with-redefinitions
  (defmethod omNG-save ((self internepicture) &optional (values? nil))
    (declare (ignore values?))
    (let* ((path (and (pict-pathname self) (om-save-pathname-relative (pict-pathname self))))
           (pict-data (if (equal (storemode self) :internal)
                          `(om-create-picture ,(omng-save (om-picture-values (thepict self))))
                        (if (pict-pathname self)
                            `(om-get-picture-file (restore-pict-path (restore-path ,path)))
                          (if (name self)
                              (if (stringp (name self))
                                  `(om-get-picture ,(name self) ',(source self))
                                `(om-get-picture ',(name self)))
                            nil)
                          )))
           (name (name self))
           (src (source self)))

      (unless (stringp name)
        (multiple-value-bind (sss nnn) (string-until-char (string name) "-")
          (setf src (intern sss :om)
                name nnn)))

      (unless (equal (storemode self) :internal)
        (let* ((restoredfolder (restore-pict-folder src))
               (pfile (or (pict-pathname self)
                          (and restoredfolder
                               (make-pathname :directory (pathname-directory restoredfolder)
                                              :host (pathname-host restoredfolder)
                                              :device (pathname-device restoredfolder)
                                              :name name)))))
          (when pfile (register-resource :picture pfile))))

      `(make-instance ',(type-of self)
                      :name ,name
                      :source ',src
                      :pict-pathname (restore-pict-path (restore-path ,path))
                      :thepict ,pict-data
                      :storemode ,(storemode self)
                      :draw-params ',(draw-params self)
                      :extraobjs ,(omng-save (extraobjs self))
                      :folder ,(om-save-pathname (folder self))))))

;;; relative-pathname (kernel tools/lisptools.lisp: relative-pathname) appends the tail of PATH's directory
;;; after the ".." chain. SBCL parses "~/x" as (:ABSOLUTE :HOME "x"), so a resource
;;; under the home lands :HOME in a non-first slot and cl:make-pathname refuses it,
;;; aborting the patch save (kernel save.lisp: write-resources). Same body, run on
;;; directory lists whose :HOME was first resolved to the real home components.
(defun %expand-home-directory (dir)
  "DIR with any :HOME component replaced by the components of the user home."
  (let ((home (pathname-directory (user-homedir-pathname))))
    (if (and (member :home dir) (consp home) (eq (car home) :absolute))
        (loop for item in dir
              append (if (eq item :home) (copy-list (cdr home)) (list item)))
        dir)))

(om-with-redefinitions
  (defun relative-pathname (path refpath)
    (let* ((pathdir (%expand-home-directory (pathname-directory path)))
           (refdir (%expand-home-directory (pathname-directory refpath)))
           (dirlist '(:relative))
           (refrest (cdr refdir))
           (dirrest (cdr pathdir)))
      (loop for path-dir in (cdr pathdir)
            for ref-dir in (cdr refdir)
            while (string-equal path-dir ref-dir) do
            (setf refrest (cdr refrest)
                  dirrest (cdr dirrest)))
      (loop for item in refrest do
            (setf dirlist (append dirlist (list ".."))))
      (loop for item in dirrest do
            (setf dirlist (append dirlist (list item))))
      (make-pathname :device (pathname-device refpath) :host (pathname-host refpath)
                     :directory dirlist :name (pathname-name path) :type (pathname-type path))
      )))

;;; projects soundeditor.lisp (update-cursor for full-preview) places the preview cursor at
;;; (round (* (/ time (cadr (bounds-x panel))) (w self))). Closing a sound editor with no
;;; sound loaded reaches it with time 0 over a zero span -- om-window-close-event :around
;;; (kernel editorwindow.lisp, for EditorWindow) -> close-editor-before :around
;;; (kernel new-player.lisp, for play-editor-mixin) -> general-stop ->
;;; stop-editor-callback (kernel new-player.lisp: stop-editor-callback for
;;; play-editor-mixin), which passes
;;; (start-position view) = 0 while the panel bounds are still (0 0) -- so the division is
;;; (/ 0 0). Vanilla 8.0 carries the same unguarded body and the same zero, but its sibling
;;; on this very class already guards this exact divisor (projects soundeditor.lisp:
;;; om-draw-contents for full-preview). Same body with that guard: a zero span pins
;;; the cursor at the
;;; start, every other span is computed as before.
(defmethod update-cursor ((self full-preview) time &optional y1 y2)
  (let* ((y (or y1 0))
         (h (if y2 (- y2 y1) (h self)))
         (dur (cadr (bounds-x (panel (om-view-container self)))))
         (pixel (if (zerop dur) 0 (round (* (/ time dur) (w self))))))
    (om-update-transient-drawing self :x pixel :y y :w 4 :h h)
    ))

;;; The vanilla transient cursor crosses into CAPI through APPLY-IN-PANE-PROCESS.
;;; Callbacks are coalesced and the newest native player position is applied on
;;; the one GLFW/UI thread, so cursor rendering cannot contend with pointer
;;; events and stale callbacks cannot accumulate behind an expensive frame.
(defvar *sound-cursor-update-lock*
  (bordeaux-threads:make-lock "sound cursor UI updates"))
(defvar *sound-cursor-update-pending* (make-hash-table :test #'eq))

(defun %apply-sound-editor-cursor (editor time)
  (mapc (lambda (view) (update-cursor view time))
        (cursor-panes editor)))

(defun %queue-sound-editor-cursor (editor fallback-time)
  (let ((enqueue nil))
    (bordeaux-threads:with-lock-held (*sound-cursor-update-lock*)
      (unless (gethash editor *sound-cursor-update-pending*)
        (setf (gethash editor *sound-cursor-update-pending*) t
              enqueue t)))
    (when enqueue
      (alld:post-to-ui-thread
       (lambda ()
         (unwind-protect
              (when (member (state (player editor)) '(:play :pause))
                (%apply-sound-editor-cursor
                 editor
                 (if (eq (state (player editor)) :play)
                     (get-player-time (player editor))
                     fallback-time)))
           (bordeaux-threads:with-lock-held (*sound-cursor-update-lock*)
             (remhash editor *sound-cursor-update-pending*))))))))

(defmethod play-editor-callback ((self soundeditor) time)
  (if (alld:on-ui-thread-p)
      (%apply-sound-editor-cursor self (get-player-time (player self)))
      (%queue-sound-editor-cursor self time)))

;;; projects sound-tools.lisp (handle-new-file-exists) and projects sdif-write.lisp
;;; (save-sdif-file for sdif-buffer, write-sdif-file for list) call (gc-all): under
;;; LispWorks that is the environment function om-gc itself delegates to
;;; (om-LW/system.lisp: om-gc).

(defun gc-all () (om-gc))

;;; Sound editor file drops and malformed metadata

;;; LispWorks' MAKE-ARRAY accepts :ALLOCATION :STATIC; CL:MAKE-ARRAY on SBCL
;;; does not.  omsounds uses that implementation extension in both display-array
;;; builders.  Keep kernel/projects pristine and replace the two project methods
;;; only after the project has loaded.
(defmethod build-display-array-dynamic ((self sound))
  (let* ((ratio 128)
         (size (om-sound-n-samples self))
         (format (om-sound-format self))
         (channels (om-sound-n-channels self))
         (array-width (ceiling size ratio)))
    (setf (display-ratio self) ratio
          (display-builder self)
          (om-run-process
           "DisplayArrayBuilder"
           (lambda (snd)
             (setf (display-array snd)
                   (make-array (list channels array-width)
                               :element-type 'single-float
                               :initial-element 0.0))
             (fli:with-dynamic-lisp-array-pointer
                 (ptr (display-array snd) :type :float)
               (om-fill-sound-display-array
                format (namestring (filename snd)) ptr
                channels array-width ratio))
             (sound-get-best-pict snd)
             (setf (display-builder self) nil)
             (om-print (format nil "~A Loaded..." (filename self))))
           self))))

(defmethod build-display-array ((self sound))
  (let ((winsize 128)
        (format (om-sound-format self))
        (channels (om-sound-n-channels self)))
    (when (and format channels)
      (let ((array-width (ceiling (om-sound-n-samples self) winsize)))
        (setf (display-ratio self) winsize)
        (funcall
         (lambda (snd)
           (setf (display-array snd)
                 (make-array (list channels array-width)
                             :element-type 'single-float
                             :initial-element 0.0))
           (fli:with-dynamic-lisp-array-pointer
               (ptr (display-array snd) :type :float)
             (om-fill-sound-display-array
              format (namestring (filename snd)) ptr
              channels array-width winsize))
           (sound-get-best-pict snd)
           (om-print (format nil "~A Loaded..." (filename self))))
         self)))))

;;; libsndfile/audio backends can report a file object with a zero sample rate.
;;; Zero is true in Common Lisp, so soundeditor.lisp's render guard accepts it
;;; and immediately computes (/ number-of-samples 0).  At the public metadata
;;; boundary, an invalid/non-positive rate means "unavailable", exactly as NIL
;;; does for a sound whose header could not be loaded.
(defmethod om-sound-sample-rate :around ((self internalsound))
  (let ((rate (call-next-method)))
    (and (numberp rate) (plusp rate) rate)))

(defmethod om-import-files-in-app ((self soundpanel) files)
  "Replace the sound edited by SELF with the single audio file dropped on it."
  (when (= 1 (length files))
    (let* ((path (pathname (first files)))
           (new-sound (and (probe-file path) (load-sound-file path)))
           (valid (and new-sound
                       (fill-sound-info new-sound)
                       (plusp (or (om-sound-n-samples new-sound) 0))
                       (plusp (or (om-sound-n-channels new-sound) 0))
                       (plusp (or (om-sound-sample-rate new-sound) 0)))))
      (if valid
          (let* ((sound-editor (editor self))
                 (box (ref sound-editor)))
            ;; Keep the box value and its open editor transactional: no visible
            ;; state changes until the replacement file has valid metadata.
            ;; OPENEDITORFRAME normally builds both the display array and the
            ;; preview picture before constructing an editor.  A drop replaces
            ;; the object in an already-open editor, so it must perform that
            ;; same preparation explicitly.
            (build-display-array new-sound)
            (setf (value box) new-sound)
            (when (frames box)
              (om-invalidate-view (first (frames box))))
            (update-editor-after-eval sound-editor new-sound)
            t)
          (progn
            (om-beep-msg
             (format nil "File '~A' is not a valid audio file."
                     (namestring path)))
            nil)))))

;;; FluidSynth queried before any synth exists
;;;
;;; fluid-make-presets (projects fluid/tools/fluid-send.lisp: fluid-make-presets for number) indexes
;;; cl-fluid::*fl-synths* and hands the result to the CFFI entry point:
;;;   (cl-fluid::fluid_synth_get_sfont (cl-fluid::getsptr (nth port *fl-synths*)) 0)
;;; With no synth instantiated -- fluid autoload off, or no FluidSynth library --
;;; (nth port nil) is NIL and (getsptr NIL) is NIL by the catch-all method
;;; (projects cl-fluidsynth.lisp: getsptr for t).  LispWorks' FLI accepts NIL where a foreign pointer is
;;; expected; SBCL's sb-alien signals TYPE-ERROR ("The value nil is not of type
;;; sb-sys:system-area-pointer"), which aborts the whole patch load -- reached from
;;; get-super-default-value of FL-PGM/FL-MICROTUNE while a saved patch is being read
;;; (projects fluid-interface-boxes.lisp: get-slot-in-out-names, get-super-default-value
;;; and set-dialog-item-params for fl-pgm), e.g. resources/tutorials/Fluid/01-fluid.omp.
;;; The function already answers "nothing to list" for the neighbouring case (a synth
;;; whose soundfont slot is a null pointer, projects fluid-send.lisp: the null-pointer
;;; guard in fluid-make-presets); this extends the same
;;; answer -- the same om-beep-msg with the same string -- to "there is no synth on this
;;; port", which is the state SBCL cannot pass down to the foreign call.
;;;
;;; fluid-make-presets is a defmethod! (projects fluid-send.lisp: fluid-make-presets), so its generic carries
;;; :method-class OMMethod (kernel ommethod.lisp: the defmethod* macro, via kernel
;;; tools/lisptools.lisp: *def-metaclass-method*) and this :around
;;; is instantiated as an OMMethod with name NIL and the inherited :protected-p T
;;; (kernel metaclasses.lisp: the :default-initargs :protected-p t of the defclass
;;; OMMethod) -- which is exactly what excludes it from every function-UI
;;; enumerator (kernel ompackage.lisp: direct-methods and get-methods for OMPackage;
;;; kernel omgenfun.lisp: update-methods for omgenericfunction -- all filter on
;;; protected-p).
;;; The kernel itself does this: kernel reactive-behaviours.lisp (the omNG-box-value
;;; :around for OMReactiveBox) puts a plain cl:defmethod
;;; :around on omNG-box-value, a defgeneric* generic with the same method class.
(defmethod fluid-make-presets :around ((port number))
  (let ((getsptr (%optional-fdefinition "CL-FLUIDSYNTH" "getsptr"))
        (synths  (%optional-symbol-value "CL-FLUIDSYNTH" "*fl-synths*")))
    (if (and getsptr (funcall getsptr (nth port synths)))
        (call-next-method)
        (om-beep-msg "no soundfont loaded in this synth"))))

;;; LW's FLI reads NIL where a foreign pointer is declared as a pointer to address 0
;;; (fli/fli-type-ref-29.htm:54); sb-alien signals TYPE-ERROR.  Two families of entry
;;; points receive NIL from the projects themselves: projects sdiffile.lisp (GetNVTList
;;; for string), and every CL-FLUIDSYNTH C wrapper whose handle is (getsptr NIL) for a
;;; port without a synth (projects cl-fluidsynth.lisp: getsptr for t) -- reached, for
;;; one, by opening the console (projects fluid-console.lisp: window for
;;; FluidConsoleEditor plays every channel ctrl).  With a NULL handle the C side
;;; answers FLUID_FAILED, which is what the vanilla gets.

(defun %ffi-accept-null-pointer (sym &key (positions :all))
  "Give the foreign entry point SYM the FLI reading of NIL: a pointer to address 0,
in every argument (:ALL) or in the first one only (:FIRST)."
  (let ((fn (fdefinition sym)))
    (setf (fdefinition sym)
          (ecase positions
            (:all (lambda (&rest args)
                    (apply fn (substitute (cffi:null-pointer) nil args))))
            (:first (lambda (&rest args)
                      (apply fn (if (and args (null (first args)))
                                    (cons (cffi:null-pointer) (rest args))
                                    args))))))))

(dolist (%sdif-entry '(sdif::SdifCreateHashTableIterator sdif::SdifFClose))
  (%ffi-accept-null-pointer %sdif-entry))

;;; The swig wrappers are the underscored functions of the package (projects
;;; fluidsynth_ffi.lisp); the handle is always their first parameter, and later
;;; parameters may be :boolean or :string, where NIL keeps its CFFI meaning.
(let ((fluid (find-package "CL-FLUIDSYNTH")))
  (when fluid
    (do-symbols (sym fluid)
      (when (and (eq (symbol-package sym) fluid)
                 (find #\_ (symbol-name sym))
                 (fboundp sym)
                 (not (macro-function sym))
                 (not (typep (fdefinition sym) 'generic-function)))
        (%ffi-accept-null-pointer sym :positions :first)))))

;;; A console keeps the track count it was SAVED with (projects fluid-console.lisp:
;;; omNG-save for fluid-synth-console), while the number of synths comes from the
;;; preferences, so the shipped tutorial -- 8 tracks against the default single synth --
;;; builds a ctrl for ports 1..7 that have none.  getsptr answers NIL for anything that
;;; is not a synth (projects cl-fluidsynth.lisp: getsptr for t, the author's
;;; ";necessaire"), but its sibling synthptr is only the slot reader of fl-synth: the
;;; guard of oct-tun-off (projects fluid-tuning.lisp: oct-tun-off for (number list))
;;; tests that the synth LIST is non-empty and never the element, so NIL reaches
;;; synthptr and there is no applicable method.  Reading NIL the way getsptr does sends
;;; the null handle down the same route: %ffi-accept-null-pointer above turns it into a
;;; pointer to address 0 and the C side answers FLUID_FAILED, which is what every
;;; getsptr path already receives.  Reported once per session -- oct-tun-off asks once
;;; per channel of every silent port.

(defvar *fluid-no-synth-reported-p* nil)

(defmethod cl-fluid::synthptr ((self t))
  (unless *fluid-no-synth-reported-p*
    (setf *fluid-no-synth-reported-p* t)
    (format *error-output*
            "~&[fluid] a port of this console has no synth: the console has more tracks than the preferences load synths~%")
    (finish-output *error-output*))
  nil)

;;; The vanilla registers the DLL through LispWorks' FLI only (projects sdif-api.lisp: load-sdif-library);
;;; this file provides the CFFI-based SBCL loader (no FLI).
(defun sdif::load-sdif-library ()
  (let ((libpath (om-lib-pathname sdif::*sdif-pathname*)))
    (if (probe-file libpath)
        (progn
          (print (concatenate 'string "Loading SDIF library: " (namestring libpath)))
          (setf sdif::*sdif-library*
                (handler-case
                    (progn (cffi:load-foreign-library (namestring libpath)) t)
                  (error (c)
                    (om-message-dialog
                     (format nil "Could not load SDIF foreign-library.~%~A~%~A"
                             (namestring libpath) (princ-to-string c)))
                    nil))))
        (om-message-dialog
         (format nil "SDIF library not found: ~A" (namestring libpath))))
    (setf sdif::*sdif-initialized-p* nil)))

;;; APPROX of the objects the :fluidsynth engine plays: the caller-less branch of
;;; prepare-to-play reads (approx object) (projects fluid-player.lisp: the prepare-to-play for (eql :fluidsynth)).

(defun %note-editor-approx (note)
  "Editor approx reaching NOTE: its own edition param, its chord's, or its slot."
  (when (typep note 'note)
    (let* ((parent (parent note))
           (own (get-approx-from-edparam note))
           (up  (and (typep parent 'chord) (get-approx-from-edparam parent))))
      (cond ((numberp own) own)
            ((numberp up) up)
            (t (approx note))))))

;;; ARP-CHORD holds the notes of the chord it arpeggiates (projects
;;; score-playback.lisp: chord-obj-to-play for chord), and those notes reach the box
;;; through their parent.
(defmethod approx ((self arp-chord))
  "Approx of an ARP-CHORD: the editor value carried by the notes it arpeggiates."
  (let ((a (loop for n in (notes self)
                 for v = (%note-editor-approx n)
                 when (numberp v) return v)))
    (if (numberp a) a *global-midi-approx*)))

;;; A MIDIFILE box carries the score edition params, whose approx default is this
;;; very variable (projects midifile.lisp: default-edition-params for midifile;
;;; projects edition.lisp: get-default-score-params for t); no editor writes it.
(defmethod approx ((self midifile))
  "Approx of a MIDIFILE: the global MIDI approximation preference."
  *global-midi-approx*)

;;; The fluid consoles tune per channel controller (projects fluid-console.lisp: the
;;; tuning slot of the defclass* Fluid-Ctrl), not
;;; through the player approx: their box edition params declare no approx at all.
(defmethod approx ((self fluid-settings-ctrl)) nil)
(defmethod approx ((self fluid-ch-settings-ctrl)) nil)
(defmethod approx ((self fluid-chan-settings-ctrl)) nil)


;;; The kernel body (projects sdif-struct.lisp: sort-nvts for sdif-buffer) has a stray LOOP `do` before the first
;;; `when`, malformed under SBCL's strict LOOP; projects/ is read-only and DEV syncs
;;; restore the broken source, so the corrected method lives at this after-load boundary.

(defmethod sort-nvts ((self sdif-buffer))
  (let ((sorted-tables
         (loop for item in (list! (nvts self)) ;do
               when (subtypep (type-of item) 'sdifnvt)
               collect item into nvts
               when (subtypep (type-of item) 'sdifsid)
               collect item into sids
               finally (return (list nvts sids)))))
    (setf (nvts self) (car sorted-tables)
          (sids self) (cadr sorted-tables))
    ))

#+win32
(defmethod load-om-lib :around ((self OMLib) &optional (winmessage t))
  "Load a legacy CR-only library init file with the matching source format."
  (declare (ignore winmessage))
  (let ((external-format
          (alloy-desktop:detect-cr-only-external-format (lib-pathname self))))
    (if external-format
        (let ((sb-ext:*default-source-external-format* external-format))
          (call-next-method))
      (call-next-method))))

#+win32
(defmethod load-om-lib-without-menu :around ((self OMLib))
  "Load a legacy CR-only library init file without changing other platforms."
  (let ((external-format
          (alloy-desktop:detect-cr-only-external-format (lib-pathname self))))
    (if external-format
        (let ((sb-ext:*default-source-external-format* external-format))
          (call-next-method))
      (call-next-method))))

;;; SBCL bakes get-icon-lock's derived return type (145/166-168/nil) into same-file
;;; callers; recompiled here so libraries may redefine it with new modes (LW parity).
(defmethod org.shirakumo.alloy:render :around
    ((renderer oa::om-look-window) (self lock-button))
  "Keep a flattened lock icon above its interface-box control."
  (org.shirakumo.alloy.renderers.simple:with-pushed-transforms (renderer)
    (incf (org.shirakumo.alloy.renderers.simple:z-index renderer)
          alld:*dialog-item-z-elevation*)
    (call-next-method)))

(defmethod make-lock-button ((self omboxframe) mode)
  (om-make-view 'lock-button
                :IconID (get-icon-lock mode)
                :size (om-make-point 10 10)
                :position (om-make-point 0 0) ;;; (x (iconview self)) 8)
                :action #'(lambda (item)
                            (let* ((modes (allowed-lock-modes (object self)))
                                   (mpos (position (mode item) modes :test 'string-equal))
                                   (newmode (when mpos (nth (mod (1+ mpos) (length modes)) modes))))
                              (setf (mode item) newmode (iconID item) (get-icon-lock newmode))
                              (setf (allow-lock (object self)) newmode)
                              (om-invalidate-view self)
                              ))))

(defmethod add-lock-button ((self ev-onceboxframe) &optional (mode "&"))
  "Allow only lock button in mode ev-once."
  (when (allow-lock-button (object self))
    (setf (lock-button self)  (om-make-view 'lock-button
                                            :IconID (get-icon-lock mode)
                                            :size (om-make-point 10 10)
                                            :position (om-make-point (x (iconview self)) 8)
                                            :owner (iconview self)    ;;; self
                                            :action #'(lambda (item)
                                                        (declare (ignore item)) (om-beep))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) "&")))

(defmethod add-lock-button ((self maquetteframe) &optional (mode "x"))
  "Not lambda mode for maquette boxes."
  (when (and (allow-lock-button (object self))
             (find mode (allowed-lock-modes (object self)) :test 'string-equal))
    (setf (lock-button self)  (om-make-view 'lock-button
                                            :IconID (get-icon-lock mode)
                                            :size (om-make-point 10 10)
                                            :position (om-make-point 0 0) ;;;(om-make-point (x (iconview self)) 8)
                                            :owner (iconview self)   ;;; self
                                            :action #'(lambda (item)
                                                        (let* ((modes (allowed-lock-modes (object self)))
                                                               (mpos (position (mode item) modes :test 'string-equal))
                                                               (newmode (nth (mod (1+ mpos) (length modes)) modes)))
                                                          (setf (mode item) newmode
                                                                (iconID item) (get-icon-lock newmode))
                                                          (setf (allow-lock (object self)) newmode)
                                                          ))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) mode)))


;;; OM file serialization must always run under the LW print environment,
;;; whatever the ambient bindings of the calling thread.
(defmacro with-lw-save-syntax (&body body)
  `(let ((*print-pretty* nil) (*print-case* :downcase)
         (*print-right-margin* nil) (*print-miser-width* nil)
         (*print-readably* nil) (*print-circle* nil)
         (*print-level* nil) (*print-length* nil) (*print-lines* nil)
         (*print-base* 10) (*print-radix* nil))
     ,@body))

(defmethod header-comment-from-obj :around ((self t))
  (with-lw-save-syntax (call-next-method)))

(defmethod omNG-save :around ((self OMPersistantObject) &optional (values? nil))
  (declare (ignore values?))
  (with-lw-save-syntax (call-next-method)))

(defmethod save-instance :around (value &optional name)
  (declare (ignore value name))
  (with-lw-save-syntax (call-next-method)))

;;; verbatim from kernel tools/saveheader.lisp (set-finder-comment) wrapped in the LW print
;;; environment (the "; ~S" resources line is built with the ambient printer)
(defun set-finder-comment (pathname self)
  "Sets the finder comment of a file"
  (with-lw-save-syntax
    (when (om-persistant-p pathname)
      (let (line (linelist nil) folderp rsrc)
        (when (setf folderp (directoryp pathname))
          (setq pathname (om-make-pathname :directory pathname :name ".finderinfo")))
        (when (probe-file pathname)
          (setf rsrc (get-resources pathname))
          (with-open-file (file pathname :direction :input)
            ; skip comments
            (loop while (and (not (equal line :eof))
                             (is-a-comment line))
                  do (setf line (read-line file nil :eof)))
            (loop while (not (equal line :eof)) do
                  (unless (equal line "") (setf linelist (append linelist (list line))))
                  (setf line (read-line file nil :eof))))
          (delete-file-protection pathname))
        (with-open-file (file pathname :direction :output :if-exists :supersede)
          (write-header self file)
          (when rsrc
            (write-line "; External resources " file)
            (write-line (format nil "; ~S" (omng-save rsrc)) file))
          (setf line (pop linelist))
          (loop while (and line (not (equal line :eof))) do
                (write-line line file)
                (setf line (pop linelist))
                ))))))


;;; Verbatim from kernel browsercontainer.lisp (editor-make-new-icon-window for
;;; PackageEditor), with the 'c branch's LET declaring STRING/ICONID/DOC as its 'f
;;; sibling already does: undeclared, they hit
;;; CL:STRING, which LispWorks tolerates and SBCL's package lock refuses.
(defmethod editor-make-new-icon-window ((self PackageEditor) &optional type)
  (if (not (= (current-scroll self) 1))
      (om-message-dialog "Open an unprotected package to add new elements !")
      (let* ((scrollframe (nth 0 (scroll-list self)))
             (iconframe (car (get-actives scrollframe)))
             (container-object (object iconframe))
             new-object new-frame)
        (declare (ignorable new-frame))
        (if (not (add-subpackages-p container-object))
            (dialog-message (string+ "You can create "
                                     (cond ((equal type 'p) "subpackages")
                                           ((equal type 'p) "classes")
                                           ((equal type 'p) "generic functions")
                                           (t "new elements"))
                                     " only in the USER package or in its subpackages."))
            (progn
              (setf (current-scroll self) 0)
              (remove-scrolls self)
              (UpDateScrollers self)
              (cond ((equal type 'p)
                     (setf new-object (make-instance 'OMPackage :name (unique-name-from-list "New Package" (subpackages container-object)) :icon 22))
                     (omng-add-element (object iconframe) new-object)
                     (unless (triangle iconframe)
                       (add-triangle iconframe scrollframe))
                     (when (open? (triangle iconframe))
                       (open/close-triangle (triangle iconframe) nil))
                     (open/close-triangle (triangle iconframe) t))
                    ((equal type 'c)
                     (let ((dial (get-classname-dialog)) string iconID doc)
                       (when dial
                         (setf string (first dial))
                         (setf iconID (third dial))
                         (setf doc (second dial))
                         (when (string-equal doc "")
                           (setf doc "No Documentation"))
                         (when (and string (not (string-equal "" string)))
                           (if (exist-class-p string)
                               (progn
                                 (dialog-message (string+ "The class " string " is already defined!"))
                                 (editor-make-new-icon-window self type))
                               (progn
                                 (setf new-object (eval `(defclass* ,(interne string) () ()
                                                           (:icon ,(car (list! iconID))) (:documentation ,doc))))
                                 (when (listp iconID)
                                   (icon-for-user-package new-object (second iconID)))
                                 (setf (create-info new-object) (list (om-get-date) (om-get-date)))
                                 (omNG-add-element container-object new-object))))))
                     (open-icon-scroller iconframe))
                    ((equal type 'f)
                     (let ((diag (get-funname-dialog)) string iconid doc)
                       (if diag
                           (progn
                             (setf string (first diag))
                             (setf doc (second diag))
                             (setf iconid (third diag))
                             (when (string-equal doc "")
                               (setf doc "No Documentation"))
                             (when (and string (not (string-equal "" string)))
                               (if (fboundp (interne string))
                                   (progn (dialog-message (string+ "A function " string " already exists."))
                                          (editor-make-new-icon-window self type))
                                   (make-new-genfunwin self string iconid doc container-object))))
                           (open-icon-scroller iconframe))))))))))

;;; TEXT-BOX and TEXT-VIEW (kernel dialog-item-boxes.lisp: the text-box and text-view
;;; classes) are interface boxes: a
;;; text field only while the user is editing it. Out of edition the keystroke belongs
;;; to the patch panel, which moves the selected boxes -- but the focus chain keeps
;;; routing input at a pane whose own focus already fell to NIL (alloy
;;; focus-tree.lisp: the handle for (input-event focus-chain)), so the box would answer for keys it does not own.
;;; Releasing the key needs DECLINE: handle answers T by default (alloy events.lisp:
;;; the handle :around for (event handler)). It must be a PRIMARY method -- the
;;; restart is established by the generic
;;; :around, which runs after any :around of ours.

(defun %text-di-box-key-to-kernel (self event)
  "Hand EVENT's key to the patch, the same way the window handler does
(actions.lisp: the alloy:handle for (key-down om-look-window)). Declining is not
enough here: the focus tree answers T even
when the box refused, so the OR in that handler would never reach the kernel."
  (let ((wkey (oa::%om-key-for-kernel (org.shirakumo.alloy:key event))))
    (or (and (not (eq wkey :none))
             (oa:om-char-spec-callback (oa:om-view-window self) 0 0 wkey))
        (org.shirakumo.alloy:decline))))

(defmethod org.shirakumo.alloy:handle
    ((event org.shirakumo.alloy:key-down) (self text-box))
  (if (alld:pane-editing-p self)
      (call-next-method)
      (%text-di-box-key-to-kernel self event)))

(defmethod org.shirakumo.alloy:handle
    ((event org.shirakumo.alloy:key-down) (self text-view))
  (if (alld:pane-editing-p self)
      (call-next-method)
      (%text-di-box-key-to-kernel self event)))

;;; SLIDER (kernel dialog-item-boxes.lisp: the slider class) keeps :strong whether or not the user is on
;;; it, so its focus tells the two states apart for nothing. What does is the panel
;;; selection: with boxes selected the arrows belong to the patch, which moves them.
;;; GET-ACTIVES answers NIL for anything that is not a panel (kernel
;;; nonrelationcontainer.lisp: get-actives for t), so the walk is safe outside a
;;; patch. The specializer is the BRIDGE
;;; class, so the guarantee holds for every slider box: the kernel's SLIDER and the
;;; fluid FL-GAIN / FL-PAN alike, which are om-sliders without being d-i-boxes
;;; (projects fluid/tools/fluid-interface-boxes.lisp: the fl-gain and fl-pan classes).

(defun %di-box-in-selection-p (self)
  "True when a panel above SELF holds selected boxes."
  (loop for view = (oa:om-view-container self) then (oa:om-view-container view)
        while view
        thereis (and (get-actives view) t)))

(defmethod org.shirakumo.alloy:handle
    ((event org.shirakumo.alloy:key-down) (self oa::om-standard-dialog-item))
  "Return box deletion keys to the patch when an interface box is selected."
  (if (and (%di-box-in-selection-p self)
           (member (org.shirakumo.alloy:key event) '(:backspace :delete)))
      (%text-di-box-key-to-kernel self event)
      (call-next-method)))

(defmethod org.shirakumo.alloy:handle
    ((event org.shirakumo.alloy:key-down) (self om-pop-up-dialog-item))
  "Return box deletion keys before Alloy's combo-set keyboard handler consumes them."
  (if (and (%di-box-in-selection-p self)
           (member (org.shirakumo.alloy:key event) '(:backspace :delete)))
      (%text-di-box-key-to-kernel self event)
      (call-next-method)))

(defmethod org.shirakumo.alloy:handle
    ((event org.shirakumo.alloy:key-down) (self om-slider))
  (if (%di-box-in-selection-p self)
      (%text-di-box-key-to-kernel self event)
      (call-next-method)))

;;; TAB in a patch goes to handle-key-event (patchPanel) and nowhere else (kernel
;;; patchcontainer.lisp: handle-key-event for patchPanel, its otherwise branch beeps): the
;;; window consumes Alloy's focus-next/focus-prev, and the raw key-down still reaches the
;;; kernel, or the edited text field that completes on it. scorePanel inherits patchPanel
;;; (projects scoreeditors.lisp: the scorePanel class) but is a score editor, which keeps
;;; its Tab.

(defmethod org.shirakumo.alloy:handle :around
    ((event org.shirakumo.alloy:focus-event) (window EditorWindow))
  "Tab and Shift-Tab never walk the widgets of a patch window."
  (let ((panel (and (editor window) (panel (editor window)))))
    (if (and (typep event '(or org.shirakumo.alloy:focus-next org.shirakumo.alloy:focus-prev))
             (typep panel 'patchPanel)
             (not (typep panel 'scorePanel)))
        t
        (call-next-method))))

;;; POP-UP-MENU (kernel dialog-item-boxes.lisp: the pop-up-menu class) travels with its box while the box is
;;; dragged, so the release lands on it. Alloy activates on the RELEASE without
;;; looking at where the press was (alloy component.lisp: the handle for (pointer-up
;;; component)), and the fork's activate
;;; opens a chooser window (dialog-items.lisp: the alloy:activate :around for
;;; om-pop-up-dialog-item): the gesture that moved the
;;; box also opened the menu. CAPI opens an option-pane on the PRESS, so a drag
;;; release never opens it there. The specializer is the BRIDGE class, so the
;;; guarantee holds for every popup box, the fluid FL-PGM / FL-MICROTUNE included
;;; (projects fluid/tools/fluid-interface-boxes.lisp: the fl-microtune and fl-pgm classes).

(defmethod org.shirakumo.alloy:handle
    ((event org.shirakumo.alloy:pointer-up) (self om-pop-up-dialog-item))
  (if alld:*drag-active-p*
      (org.shirakumo.alloy:decline)
      (call-next-method)))

;;; kernel infowindow.lisp defines cut/copy/paste twice. The first trio (cut, copy
;;; and paste for info-window) reads CP-ITEM, a slot of the defclass info-window.
;;; The second pair (the later cut and copy for info-window) redefines them on that
;;; same class but reads NAME-INFO/OFFSET-INFO/POSY-INFO/DUR-INFO/SIZEY-INFO, which
;;; are slots of the subclass INFO+OFFSET-WINDOW -- so on a plain info window they
;;; signal NO-APPLICABLE-METHOD. Only paste escaped, having been specialised on the
;;; subclass (paste for info+offset-window). Same reading as the author's second
;;; version, over the fields each class actually has.

(defun %info-window-edited-field (self)
  "The window's text field currently being edited, or NIL."
  (let ((fields (if (typep self 'info+offset-window)
                    (list (name-info self) (offset-info self) (posy-info self)
                          (dur-info self) (sizey-info self) (cp-item self))
                    (list (cp-item self)))))
    (and oa::*edited-self*
         (find oa::*edited-self* fields :test #'equal))))

(defmethod cut ((self info-window))
  (let ((field (%info-window-edited-field self)))
    (if field (om-cut-command field) (om-beep))))

(defmethod copy ((self info-window))
  (let ((field (%info-window-edited-field self)))
    (if field (om-copy-command field) (om-beep))))

(defmethod paste ((self info-window))
  (let ((field (%info-window-edited-field self)))
    (if field (om-paste-command field) (om-beep))))

;;; the kernel's own paste is specialised here (kernel infowindow.lisp: paste for
;;; info+offset-window) and would otherwise win
(defmethod paste ((self info+offset-window))
  (let ((field (%info-window-edited-field self)))
    (if field (om-paste-command field) (om-beep))))

;;; Measured selection matrix: a click somewhere else INSIDE the window
;;; undoes the text selection in the patch editors only -- another box or the
;;; panel background. Dialogs and text editors keep it (the API default is NIL).
;;; Every OM editor window is this one class (kernel editorwindow.lisp: make-editor-window calls om-make-window
;;; 'EditorWindow), so score/maquette/instance editors answer T as well; they carry
;;; no text-input widget of their own, which makes the answer inert there.
(defmethod alld:click-clears-selection-p ((window EditorWindow)) t)

;;; The kernel rebuilds the comment box frame on every Set/Edit (kernel
;;; boxframe.lisp: reinit-size for commentboxframe), so the comment editor's frame
;;; must follow the rebuild instead of keeping the detached one.
(defmethod redraw-frame :around ((self commentboxframe))
  (let ((frame (call-next-method)))
    (when (and frame (eq self om-edit::*comment-frame*))
      (setf om-edit::*comment-frame* frame))
    frame))

;;; The text view of the comment frame the editor is currently aimed at.
;;; OA is the home package of the editor symbols (editors/export.lisp).
(defun oa::comment-live-panel ()
  (when (typep om-edit::*comment-frame* 'commentboxframe)
    (iconview om-edit::*comment-frame*)))

;;; *pref-window* (kernel preferences.lisp: omG-make-preference-dialog) and
;;; *finder-window* (kernel omfinder.lisp: omG-make-finder-dialog)
;;; are convenience pointers to the live singleton, but only startup clears them
;;; (kernel preferences.lisp: init-preferences): the last closed dialog would stay a GC root for
;;; its entire subtree.
(defmethod om-window-close-event :after ((self ompref-window))
  (when (eq self *pref-window*) (setf *pref-window* nil)))

(defmethod om-window-close-event :after ((self omfinder-window))
  (when (eq self *finder-window*) (setf *finder-window* nil)))

;;; One failing module must not destroy every preference the user has.
;;;
;;; The kernel applies the saved modules in a single unguarded loop
;;; (kernel preferences.lisp: restore-preferences), and init-OM-session wraps the whole call in a
;;; handler-bind that shows a dialog, DELETES the preferences file and drops
;;; *saved-pref* (kernel omworkspace.lisp: the catch :load-prefs of init-OM-session).
;;; So whichever module signals first
;;; takes with it every module after it AND the file on disk -- nine modules'
;;; settings lost because one of them failed.
;;;
;;; In LispWorks the setups reached from put-preferences (audio device, MIDI
;;; port, fluidsynth) sit on stable native bindings, so the vanilla never pays
;;; for the all-or-nothing loop.  Under SBCL+CFFI they are the least stable part
;;; of the boot, so, to keep what the vanilla gets for free (the user's
;;; preferences survive a session), the fork needs a mechanism the vanilla does
;;; not have.
;;;
;;; Nothing is swallowed: a module that fails is reported in full through the
;;; listener, and anything that escapes a module -- a broken *saved-pref*
;;; structure, a failure inside find-pref-module -- still reaches the kernel's
;;; dialog and its file deletion, untouched.  The per-module report is a log
;;; rather than a dialog because a modal per module would stack up at boot.
(defmethod restore-preferences ()
  (when *saved-pref*
    (setf *restore-defaults* nil)
    (loop for item in *saved-pref* do
          (let ((modulepref (find-pref-module (car item))))
            (when modulepref
              (handler-case
                  (progn
                    (setf (cadr modulepref)
                          (corrige-preference-list (car item)
                                                   (eval (second item))
                                                   (or (third item) 0)))
                    (put-preferences (car item)))
                (error (e)
                  (om-print (format nil "PREFERENCES: module ~S was not restored: ~A"
                                    (car item) (princ-to-string e))))))))
    (setf *restore-defaults* nil)))

;;; In Emacs mode the listener input is the SLIME REPL, always there: the General
;;; panel shows "Enable Listener input" checked and disabled, and the saved
;;; preference, shared with the GUI executable, is left as it is. MODULEPREF is
;;; specialized (a module is (key plist)) so the method composes with the
;;; per-page :around of kernel-patch-win32.lisp: make-new-pref-scroll.
(defmethod make-new-pref-scroll :around ((num (eql :general)) (modulepref cons))
  (declare (ignore modulepref))
  (let ((pane (call-next-method)))
    (when om-lisp:*emacs-mode*
      (let ((item (find-if (lambda (v) (and (typep v 'om-check-box)
                                            (string= (om-dialog-item-text v) " Enable Listener input")))
                           (om-subviews pane))))
        (om-set-check-box item t)
        (om-enable-dialog-item item nil)))
    pane))

;;; The GP raster antialiases by analytic coverage, so the vanilla bow-tie lobe --
;;; a triangle whose apex sits on the centre line and whose base is the envelope
;;; (projects soundeditor.lisp: om-draw-waveform for soundPanel) -- resolves to a
;;; vertical ramp, dark at the envelope
;;; and pale at the centre.  GL offers 4 MSAA samples at a fixed subpixel phase and
;;; drops that sliver, so the fork paints the same ramp with stacked opaque quads
;;; whose alpha IS the coverage the triangle has at each depth.

(defparameter *sound-wave-bands* 16
  "Steps used to paint the waveform coverage ramp.  One draw call per step per
lobe, so the ramp is finer than the eye resolves without costing draw calls.")

(defun waveform-ramp-colors (color n)
  "COLOR at the N alpha levels of the ramp, centre-most first."
  (let ((v (make-array n))
        (r (om-color-r color))
        (g (om-color-g color))
        (b (om-color-b color)))
    (dotimes (k n v)
      (setf (svref v k) (om-make-color-alpha r g b (/ (+ k 0.5) n))))))

(defun draw-waveform-bands (xs vs npts yc colors)
  "Paint one channel's coverage ramp: each band is ONE triangle strip per lobe.
XS/VS hold the per-column pixel abscissa and half-amplitude."
  (declare (type (simple-array single-float (*)) xs vs))
  (let ((n (length colors))
        (buf (make-array (* 4 npts) :element-type 'single-float)))
    (dotimes (k n)
      (let ((in  (/ (float k) n))
            (out (/ (float (1+ k)) n)))
        (dolist (sign '(1.0 -1.0))
          (dotimes (j npts)
            (let ((o (* 4 j))
                  (x (aref xs j))
                  (v (* sign (aref vs j))))
              (setf (aref buf o)       x
                    (aref buf (+ o 1)) (+ yc (* v in))
                    (aref buf (+ o 2)) x
                    (aref buf (+ o 3)) (+ yc (* v out)))))
          (om-with-fg-color nil (svref colors k)
            (alld:fill-triangle-strip buf)))))))

(defmethod om-draw-waveform ((self soundPanel))
  (multiple-value-bind (data smplevel)
      (om-get-display-slice self)
    (when (and (> (car (array-dimensions data)) 0) (> (cadr (array-dimensions data)) 0))
      (let* ((thesound (object (om-view-container self)))
           (window-v-size (* 0.99 (om-point-v (om-view-size self))))
           (system-etat (get-system-etat self))
           (xmin (car (rangex self)))
           (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
           (xmax (cadr (rangex self)))
           (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
           (xtime (- xmax xmin))
           (nbpts (cadr (array-dimensions data)))
           (timestep (/ xtime (coerce nbpts 'single-float)))
           (nch (om-sound-n-channels thesound))
           (channels-h (round window-v-size nch))
           (offset-y (round channels-h 2))
           (fillp (and (not smplevel) (not *sound-fill*)))
           (xs (and fillp (make-array nbpts :element-type 'single-float)))
           (vs (and fillp (make-array nbpts :element-type 'single-float)))
           (rampcolors (and fillp (waveform-ramp-colors *sound-wave-color*
                                                        *sound-wave-bands*)))
           pixpoint
           pixtime
           pixprev pixtprev)
      (when data
        (om-with-fg-color nil *sound-wave-color*
          (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 0.5) ;;;=*om-steel-blue-color* with 50% transparency
            (dotimes (i nch)
              (om-draw-line pixmin (- (+ (* i channels-h) offset-y) 10) pixmax (- (+ (* i channels-h) offset-y) 10))))
          (dotimes (c nch)
            (setq pixprev (round (* offset-y (* 0.99 (aref data c 0)))))
            (setq pixtprev (om-point-h (point2pixel self (om-make-point (round xmin) 0) system-etat)))
            (when fillp
              (setf (aref xs 0) (float pixtprev 0f0)
                    (aref vs 0) (float pixprev 0f0)))
            (loop for i from 1 to (1- (cadr (array-dimensions data))) do
                  (let ((val (aref data c i)))
                    (setf pixtime (om-point-h (point2pixel self (om-make-point (+ xmin (* i timestep)) 0) system-etat)))
                    (setf pixpoint (* offset-y (* 0.99 val))) ; scaled 0-1 --> 0 -->256/2
                    (if (not smplevel) ;(= nbpts nbpix)
                        (if *sound-fill*
                            (om-draw-polygon `(,(om-make-point pixtprev (- (+ offset-y (* c channels-h) pixprev) 9))
                                               ,(om-make-point pixtime (- (+ offset-y (* c channels-h) pixpoint) 9))
                                               ,(om-make-point pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 9))
                                               ,(om-make-point pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 9))))
                          (setf (aref xs i) (float pixtime 0f0)
                                (aref vs i) (float pixpoint 0f0)))
                      (om-draw-line pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 10) pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 10)))
                    (setq pixprev pixpoint pixtprev pixtime)))
            (when fillp
              (draw-waveform-bands xs vs nbpts
                                   (- (+ offset-y (* c channels-h)) 9)
                                   rampcolors)))))))))

;;; Free-on-GC for the om-sound-data audio buffer
;;;
;;; Vanilla reaches this through hcl::add-special-free-action, which hands the
;;; dying object to om-cleanup; sb-ext:finalize runs with the object already gone,
;;; so the buffer has to be captured BY VALUE while the instance is being built --
;;; it arrives as the :buffer initarg (projects sound.lisp: the buffer slot of
;;; om-sound-data, get-om-sound-data for string), and nothing in code/
;;; ever assigns the slot afterwards or shares it between two instances.
;;;
;;; internalsound's player-data and pict-sound are deliberately NOT covered: clone
;;; copies both pointers into the copy (projects sound.lisp: copy-sound-file), so one finalizer per
;;; object would free the same resource twice.

(when (find-class 'om-sound-data nil)          ; omsounds is an optional project

  ;; :around, not :after -- projects sound.lisp (the initialize-instance :after for
  ;; om-sound-data) already defines an :after on this exact specializer (it infers
  ;; smpl-type), and a second one would replace it.
  (defmethod initialize-instance :around ((self om-sound-data) &rest initargs)
    (declare (ignore initargs))
    (multiple-value-prog1 (call-next-method)
      (let ((ptr (buffer self)))
        (when ptr
          (alld:finalize-on-gc self (lambda () (om-free-pointer ptr)))))))

  (defmethod om-cleanup :after ((self om-sound-data))
    "The primary already freed the buffer; drop the finalizer so it cannot free it again."
    (alld:cancel-finalize self)))

;;; Free-on-GC for internalsound's SHARED resources: player-data and pict-sound
;;;
;;; copy-sound-file copies BOTH pointers into the clone (projects sound.lisp: copy-sound-file), so one
;;; finalizer per object would free the same resource twice -- the latent double free
;;; the vanilla carries, since hcl::add-special-free-action hands the dying object to
;;; om-cleanup (projects sound.lisp: om-cleanup for internalsound), which frees
;;; unconditionally. Reference counting per
;;; resource is code the reference does NOT have and the fork DOES need.
;;;
;;; sb-ext:finalize runs with the owner already gone, so the finalizer closes over the
;;; HOLDER -- a 2-cell list naming what the owner holds right now -- never over the
;;; owner.


;; toplevel, outside the WHEN: a defvar nested in a form does not proclaim the
;; special at compile time, and every reference below would compile as undefined
(defvar *sound-resource-refs* (make-hash-table :test #'equal)
  "Resource key -> number of live internalsound owners.")

(defvar *sound-resource-lock* (bordeaux-threads:make-lock "om-sound-refcount")
  "The table is written from the eval thread, the UI thread and the finalizer thread.")

(defvar *sound-resource-holders* (make-hash-table :test #'eq :weakness :key)
  "internalsound -> its holder. Weak on the KEY, and a holder never names its owner,
so nothing here keeps a sound alive.")

(when (find-class 'internalsound nil)          ; omsounds is an optional project

  (defun %sound-resource-key (value)
    "The refcount key of VALUE, or NIL when VALUE is not a shared resource: the slots
legitimately hold NIL and :error too (projects sound.lisp: sound-get-best-pict for sound)."
    (cond ((and (cffi:pointerp value) (not (cffi:null-pointer-p value)))
           (cons :ptr (cffi:pointer-address value)))
          ((alld:picture-p value) value)))

  (defun %sound-on-ui (thunk)
    "Every free runs on the UI thread, as the GL free already does
(alloy-desktop/graphics/image.lisp: free-image-texture); with no UI thread there is no tick to wait for."
    (if (or (null (alld:ui-thread)) (alld:on-ui-thread-p))
        (funcall thunk)
        (alld:post-to-ui-thread thunk)))

  (defun %sound-free-resource (value)
    "Free VALUE for good. Reached only once the key is out of the registry, so the
om-kill-picture :around below lets a picture through to the real free."
    (cond ((cffi:pointerp value) (juce::freeAudioSource value))
          ((alld:picture-p value) (om-kill-picture value))))

  (defun %sound-registered-p (value)
    "True while some internalsound still owns VALUE."
    (let ((key (%sound-resource-key value)))
      (and key
           (bordeaux-threads:with-lock-held (*sound-resource-lock*)
             (nth-value 1 (gethash key *sound-resource-refs*)))
           t)))

  (defun %sound-acquire (value)
    "Count one more owner for VALUE; return what the holder should remember."
    (let ((key (%sound-resource-key value)))
      (when key
        (bordeaux-threads:with-lock-held (*sound-resource-lock*)
          (incf (gethash key *sound-resource-refs* 0)))
        value)))

  (defun %sound-release (value)
    "Drop one owner and free VALUE when the last one lets go. A value this registry
never counted is left alone."
    (let ((key (%sound-resource-key value)))
      (when (and key
                 (bordeaux-threads:with-lock-held (*sound-resource-lock*)
                   (let ((count (gethash key *sound-resource-refs*)))
                     (cond ((null count) nil)
                           ((> count 1)
                            (setf (gethash key *sound-resource-refs*) (1- count))
                            nil)
                           (t (remhash key *sound-resource-refs*) t)))))
        (%sound-on-ui (lambda () (%sound-free-resource value))))))

  (defun %sound-holder (self)
    "SELF's holder: (player-data pict-sound) as they stand right now."
    (or (gethash self *sound-resource-holders*)
        (setf (gethash self *sound-resource-holders*) (list nil nil))))

  (defun %sound-hold (self index value)
    "Point holder cell INDEX at VALUE: acquire the new resource, release the old."
    (let* ((holder (%sound-holder self))
           (old (nth index holder)))
      (unless (equal (%sound-resource-key old) (%sound-resource-key value))
        (setf (nth index holder) (%sound-acquire value))
        (%sound-release old))))

  (defun %sound-drop-holder (holder)
    "Let go of everything the holder still names."
    (loop for cell on holder
          do (%sound-release (car cell))
             (setf (car cell) nil)))

  ;; :around, not :after: an :after on a specializer the kernel also uses
  ;; REPLACES the kernel's. Nothing currently defines initialize-instance on
  ;; internalsound; :around keeps this correct if something ever does.
  (defmethod initialize-instance :around ((self internalsound) &rest initargs)
    (declare (ignore initargs))
    (multiple-value-prog1 (call-next-method)
      (let ((holder (%sound-holder self)))
        (%sound-hold self 0 (player-data self))
        (%sound-hold self 1 (pict-sound self))
        (alld:finalize-on-gc self (lambda () (%sound-drop-holder holder))))))

  (defmethod (setf player-data) :around (value (self internalsound))
    (multiple-value-prog1 (call-next-method)
      (%sound-hold self 0 value)))

  (defmethod (setf pict-sound) :around (value (self internalsound))
    (multiple-value-prog1 (call-next-method)
      (%sound-hold self 1 value)))

  (defmethod om-cleanup :around ((self internalsound))
    "No CALL-NEXT-METHOD on purpose: the kernel primary
(projects sound.lisp: om-cleanup for internalsound) frees both
pointers unconditionally, double-freeing whatever copy-sound-file shared with a clone
that is still alive. Release by refcount instead."
    (%sound-drop-holder (%sound-holder self))
    (alld:cancel-finalize self))

  (defun %sound-release-all ()
    "Exit sweep: let go of every resource a still-live internalsound holds -- the
sounds a patch still references at quit, which the GC path never reaches."
    ;; on the UI thread, where the frees run (alld:call-on-ui-thread inlines there
    ;; and otherwise blocks until done): a quit from the listener thread leaves no
    ;; UI drain to wait for
    (alld:call-on-ui-thread
     (lambda ()
       ;; holders collected before the loop: %sound-release takes this same
       ;; non-reentrant lock
       (dolist (holder (bordeaux-threads:with-lock-held (*sound-resource-lock*)
                         (loop for holder being the hash-values of *sound-resource-holders*
                               collect holder)))
         (%sound-drop-holder holder)))))

  ;; last? t pushes, and om-exit-funcall walks the reverse: the sweep runs after
  ;; close-players (kernel/scheduler-player/players.lisp: close-players), so the
  ;; audio device is down before its sources go
  (om-add-exit-cleanup-func '%sound-release-all t)

  (defmethod om-kill-picture :around ((image t))
    "A picture a sound OWNS is freed only when its last owner lets go: sound-update-pict
and sound-get-best-pict (projects sound.lisp: sound-update-pict and
sound-get-best-pict) kill the old picture and immediately assign
another, while a clone made by copy-sound-file may still be showing it."
    (if (%sound-registered-p image)
        t
        (call-next-method))))

;;; Persistent GPU geometry for the SBCL 3D editors.
;;;
;;; LispWorks compiles OM-3D-OBJECTs into display lists.  The core-profile
;;; backend cannot use display lists, so immutable vertex data lives in VBOs
;;; owned by the current GLFW context.  Camera and light matrices remain
;;; dynamic uniforms; geometry is rebuilt only when an OM curve really changes.

(defstruct (%3d-cache-entry (:constructor %make-3d-cache-entry))
  records batches)

(defvar *3d-gpu-cache*
  (make-hash-table :test #'eq :weakness :key))

;; The pane owns the top-level cached scene. Replacing its G-OBJECT therefore
;; invalidates the previous VBOs on the next draw in the correct GL context.
;; Nested objects keep their identity so heterogeneous lists cannot evict one
;; another while the same panel is drawing them.
(defvar *3d-cache-owner* nil)
(defvar *3d-cache-top-level-object* nil)

(defmethod om-draw-contents :around ((self 3dpanel))
  (let ((*3d-cache-owner* self)
        (*3d-cache-top-level-object* (oa::om-get-gl-object self)))
    (call-next-method)))

(defun %3d-cache-key (object)
  (if (and *3d-cache-owner* (eq object *3d-cache-top-level-object*))
      *3d-cache-owner*
      object))

(defun %3d-curve-record (curve)
  (list curve
        (oa::om-get-gl-points curve)
        (copy-tree (get-vertices-colors curve))
        (copy-tree (selected-points curve))
        (lines curve)
        (or (line-width curve) 1.0)))

(defun %3d-cache-valid-p (entry curves)
  (let ((records (%3d-cache-entry-records entry)))
    (and (= (length records) (length curves))
         (loop for record in records
               for curve in curves
               always (and (eq (first record) curve)
                           (eq (second record) (oa::om-get-gl-points curve))
                           (equal (third record) (get-vertices-colors curve))
                           (equal (fourth record) (selected-points curve))
                           (eql (fifth record) (lines curve))
                           (= (sixth record) (or (line-width curve) 1.0)))))))

(defun %delete-3d-cache-entry (entry)
  (dolist (descriptor (%3d-cache-entry-batches entry))
    (immediate-gl:delete-static-batch (third descriptor))))

(defun %emit-static-3d-lines (curve fallback-rgb)
  (when (lines curve)
    (let* ((vertices (oa::om-get-gl-points curve))
           (colors (get-vertices-colors curve))
           (count (length vertices)))
      (loop for index below (1- count)
            for rgb-a = (or (nth index colors) fallback-rgb)
            for rgb-b = (or (nth (1+ index) colors) fallback-rgb)
            do (destructuring-bind (red green blue) rgb-a
                 (opengl:gl-color4-f red green blue 0.8))
               (opengl:gl-vertex4-dv (aref vertices index))
               (destructuring-bind (red green blue) rgb-b
                 (opengl:gl-color4-f red green blue 0.8))
               (opengl:gl-vertex4-dv (aref vertices (1+ index)))))))

(defun %emit-static-3d-points (curve fallback-rgb selected-rgb)
  (let ((vertices (oa::om-get-gl-points curve))
        (colors (get-vertices-colors curve))
        (selection (selected-points curve)))
    (loop for index below (length vertices)
          for selected = (or (eq t selection) (find index selection))
          for rgb = (if selected selected-rgb
                        (or (nth index colors) fallback-rgb))
          do (destructuring-bind (red green blue) rgb
               (opengl:gl-color4-f red green blue (if selected 1.0 0.7)))
             (opengl:gl-vertex4-dv (aref vertices index)))))

(defun %compile-3d-curve-group (curves width)
  (let ((fallback-rgb (om-color-to-single-float-list *om-light-gray-color*))
        (selected-rgb (om-color-to-single-float-list *om-dark-red-color*))
        (descriptors nil))
    (opengl:gl-line-width (float width))
    (when (some #'lines curves)
      (dolist (batch
               (immediate-gl:compile-static-batches
                (lambda ()
                  (opengl:gl-begin opengl:*gl-lines*)
                  (dolist (curve curves)
                    (%emit-static-3d-lines curve fallback-rgb))
                  (opengl:gl-end))))
        (push (list :lines width batch) descriptors)))
    (opengl:gl-point-size (* 3.0 width))
    (dolist (batch
             (immediate-gl:compile-static-batches
              (lambda ()
                (opengl:gl-begin opengl:*gl-points*)
                (dolist (curve curves)
                  (%emit-static-3d-points curve fallback-rgb selected-rgb))
                (opengl:gl-end))))
      (push (list :points width batch) descriptors))
    (nreverse descriptors)))

(defun %build-3d-cache-entry (curves)
  (let ((groups nil)
        (batches nil))
    (dolist (curve curves)
      (let* ((width (or (line-width curve) 1.0))
             (group (assoc width groups :test #'=)))
        (if group
            (push curve (cdr group))
            (push (list width curve) groups))))
    (unwind-protect
         (progn
           (opengl:gl-enable opengl:*gl-light0*)
           (dolist (group (nreverse groups))
             (setf batches
                   (nconc batches
                          (%compile-3d-curve-group (nreverse (cdr group))
                                                   (car group)))))
           (%make-3d-cache-entry
            :records (mapcar #'%3d-curve-record curves)
            :batches batches))
      (restore-om-gl-colors-and-attributes))))

(defun %draw-3d-static-curves (key curves)
  (let* ((context immediate-gl:*context*)
         (contexts (or (gethash key *3d-gpu-cache*)
                       (setf (gethash key *3d-gpu-cache*)
                             (make-hash-table :test #'eq))))
         (entry (gethash context contexts)))
    (unless (and entry (%3d-cache-valid-p entry curves))
      (when entry (%delete-3d-cache-entry entry))
      (setf entry (%build-3d-cache-entry curves)
            (gethash context contexts) entry))
    (unwind-protect
         (progn
           (opengl:gl-enable opengl:*gl-light0*)
           (dolist (descriptor (%3d-cache-entry-batches entry))
             (let ((kind (first descriptor))
                   (width (second descriptor)))
               (opengl:gl-line-width (float width))
               (when (eq kind :points)
                 (opengl:gl-point-size (* 3.0 width)))
               (immediate-gl:draw-static-batch (third descriptor)))))
      (restore-om-gl-colors-and-attributes))))

(defun %forget-3d-gpu-context (window)
  (maphash (lambda (key contexts)
             (declare (ignore key))
             (remhash window contexts))
           *3d-gpu-cache*))

(alld:register-gl-context-cleanup
 '%forget-3d-gpu-context
 :forget #'%forget-3d-gpu-context)

(defmethod om-draw-contents ((self 3d-curve))
  (%draw-3d-static-curves (%3d-cache-key self) (list self)))

(defmethod oa::draw ((self oa::om-3D-object-list))
  (let ((objects (oa::om-get-3D-objects self)))
    (if (every (lambda (object) (typep object '3d-curve)) objects)
        (%draw-3d-static-curves (%3d-cache-key self) objects)
        (mapc #'oa::draw objects))))

;;; A 3DC-LIB miniview can contain thousands of short curves. LispWorks GP
;;; submits those inexpensive immediate primitives efficiently; mapping every
;;; segment and point to a separate Alloy shape causes tens of thousands of GL
;;; uploads/draws whenever any patch box moves. Preserve the vanilla geometry
;;; and colour order, but submit consecutive same-colour curves as batches.
(defmethod draw-obj-in-rect ((self 3dc-lib) x x1 y y1 edparams view)
  "Draw a 3DC-LIB miniview with vanilla geometry and batched GPU submission."
  (declare (ignore edparams))
  (let ((ranges (get-miniview-bpf-range (bpf-list self)))
        (run-color nil)
        (run-lines nil)
        (run-rectangles nil)
        (run-active-p nil))
    (labels ((color-components (color)
               (list (oa::om-color-r color) (oa::om-color-g color)
                     (oa::om-color-b color) (oa::om-color-alpha color)))
             (flush-run ()
               (when run-active-p
                 (oa:om-with-fg-color view run-color
                   (alld:draw-line-segments (nreverse run-lines))
                   (alld:fill-rectangles (nreverse run-rectangles)))
                 (setf run-lines nil run-rectangles nil)))
             (add-curve (curve)
               (let* ((points (point-list curve))
                      (pixels
                        (mapcar
                         (lambda (point)
                           (point-to-pixel-with-sizes
                            ranges point (- x1 x) (- y1 y)))
                         points)))
                 (loop for (first second) on pixels
                       while second
                       do (push (list (+ x (oa:om-point-h first))
                                      (+ y (oa:om-point-v first))
                                      (+ x (oa:om-point-h second))
                                      (+ y (oa:om-point-v second)))
                                run-lines))
                 (when (< (length pixels) 50)
                   (dolist (point pixels)
                     (push (list (+ x (oa:om-point-h point) -1)
                                 (+ y (oa:om-point-v point) -1)
                                 3 3)
                           run-rectangles))))))
      (oa:om-with-focused-view view
        (dolist (curve (bpf-list self))
          (let ((color (bpfcolor curve)))
            (unless (and run-active-p
                         (equalp (color-components run-color)
                                 (color-components color)))
              (flush-run)
              (setf run-color color run-active-p t))
            (add-curve curve)))
        (flush-run)))))

;;; Vanilla draw-bpc-in-rect (projects bpc.lisp: draw-bpc-in-rect) walks the list with NTH and draws one
;;; line per segment; same geometry, colour and order here, batched.
(defun draw-bpc-in-rect (bpf x x1 y y1 ranges)
  "Draw BPF in the rect with vanilla geometry and batched GPU submission."
  (let* ((points (point-list bpf))
         (pl (length points))
         (draw-points (< pl 50))
         (width (- x1 x))
         (height (- y1 y)))
    (if (= 1 pl)
        (let ((pix-point (point-to-pixel-with-sizes ranges (car points) width height)))
          (om-fill-rect (+ x (- (oa:om-point-h pix-point) 1))
                        (+ y (- (oa:om-point-v pix-point) 1))
                        3 3))
        (let ((segments nil)
              (squares nil)
              (previous nil))
          (dolist (point points)
            (let* ((pixel (point-to-pixel-with-sizes ranges point width height))
                   (px (+ x (oa:om-point-h pixel)))
                   (py (+ y (oa:om-point-v pixel))))
              (when previous
                (push (list (first previous) (second previous) px py) segments))
              (when draw-points
                (push (list (- px 1) (- py 1) 3 3) squares))
              (setf previous (list px py))))
          (alld:draw-line-segments (nreverse segments))
          (alld:fill-rectangles (nreverse squares))))))

;;; Vanilla do-space-objects (projects scoretools.lisp: do-space-objects) reads
;;; the current and the next onset packet with three NTH over the packet list,
;;; inside a loop over that same list -- O(n^2) in packets. It is the single
;;; most expensive step of a score zoom: every Ctrl+= / Ctrl+- rebuilds the whole
;;; graphic tree (projects scoreeditors.lisp: change-editor-size -> update-panel),
;;; so on a dense score its quadratic walk dominates the step.
;;; Same contract in one pass over the packets: NEXTDUR is still the next
;;; packet's onset minus this packet's own, and 2000 when there is no next
;;; packet. COUNT still threads through SPACE-PACKET and the value returned is
;;; still NIL, as the vanilla LOOP returns.
(defun do-space-objects (list size)
  "Space LIST's objects with vanilla geometry, in one pass over the onset packets."
  (let ((count 0))
    (loop for (packet next) on (group-space-objects list)
          do (let ((nextdur (if (caar next)
                                (- (caar next) (caar packet))
                                2000)))
               (setf count (space-packet packet count nextdur size))))))

;;; Vanilla draw-object for grap-chord-seq (projects scoretools.lisp: draw-object
;;; for grap-chord-seq) reads each chord and its successor with (nth i (inside self))
;;; and (nth (+ i 1) (inside self)) -- O(n^2) in chords, and paid on EVERY repaint
;;; because the culling test needs both chords: what the visible range spares is the
;;; drawing, never the walk. Same geometry, same order, same culling condition, in
;;; one pass; the rest of the body is the vanilla's.
(defmethod draw-object ((self grap-chord-seq) view x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
  "Draw a CHORD-SEQ with vanilla geometry, walking the chords once."
  (let* ((namedir (if (= 0 *staff-name-dir*) 6 0))
         endxms previous)
    (om-with-font (get-font-to-draw 0)
      (loop for (cur-chord next-chord) on (inside self)
            do (when (or (null next-chord)
                         (and (x next-chord) (x cur-chord)
                              (>= (round (* zoom (x next-chord))) minx)
                              (<= (round (* zoom (x cur-chord))) maxx)))
                 (draw-object cur-chord view x y zoom minx maxx miny maxy slot size nil staff grille-p chnote)
                 (when (and *om-tonalite* (not *draw-mini-pict*))
                   (draw-modulation self cur-chord previous x y zoom size view)))
               (setf previous cur-chord)))
    (when (and grille-p (> (ms2pixel grille-p (/ size 4) zoom) 1))
      (setf endxms (* (ceiling (pixel2ms (- maxx x) (/ size 4) zoom) 1000) 1000))
      (om-with-fg-color view *om-gray-color*
        (om-with-line '(2 2)
          (loop for time from 0 to endxms by grille-p do
                (let ((posx (time-to-pixels view time)))
                  (om-with-fg-color view *om-black-color*
                    (om-with-font (get-font-to-draw 6)
                      (om-draw-string posx 10 (number-to-string time))))
                  (om-draw-line posx miny posx maxy))))))
    (when (and (name (reference self)) (stringp (name (reference self))))
      (om-with-font (get-font-to-draw 6)
                    (om-draw-string (+ (- x 12) (om-h-scroll-position view))
                                    (+ y (line2pixel (+ namedir (posy (last-elem (staff-list staff))))
                                                     t (/ size 4))
                                       (/ size -8))
                                    (name (reference self)))))
    (collect-rectangles self)
    (draw-extras self view size staff zoom)))

;;; Vanilla draw-aligned-measures (projects scoretools.lisp: draw-aligned-measures
;;; for grap-poly) is the only drawing method of that file that receives no visible
;;; range and runs no visibility test: it emits one bar number and one barline per
;;; aligned group for the WHOLE score on every repaint, however few measures are on
;;; screen.  On a dense POLY that is two orders of magnitude more bar numbers and
;;; barlines than the panel can show.
;;; Same lambda-list and same geometry: the rectangle alignment loop still runs for
;;; every group, because the rest of the repaint reads those rectangles.  Only the
;;; DRAWING is skipped, for a barline outside the band the panel already clips to
;;; (projects scoreeditors.lisp: draw-view-contents for polypanel, the
;;; om-with-clip-rect), widened by four staff sizes so a bar number ending inside
;;; the band is never cut.

(defvar *score-visible-x* nil
  "(minx . maxx) of the score panel's visible band, in the coordinate space the
grap-measure rectangles live in, or NIL for \"draw everything\".")

;;; The band is the minx/maxx the caller already holds; an :around carries it in
;;; rather than a copy of the vanilla body.  Only an editor panel binds it: a
;;; miniview and the page-mode pass draw the same POLY through this generic with
;;; ranges of their own (projects musminiboxes.lisp: draw-object-mini-obj;
;;; projects pagination.lisp: make-pages-form-obj), and neither is what the
;;; measurement covers.
(defmethod draw-object :around ((self grap-poly) view x y zoom minx maxx miny maxy
                                slot size linear? staff grille-p chnote)
  (let ((*score-visible-x* (when (typep view 'scorePanel) (cons minx maxx))))
    (call-next-method)))

(defmethod draw-aligned-measures ((self grap-poly) list staff size positions)
  "Align the measure rectangles of every group, draw only the visible barlines."
  (let ((minx (car *score-visible-x*))
        (maxx (cdr *score-visible-x*))
        (margin (* 4 size)))
    (loop for group in list do
          (let* ((measures (loop for item in group collect
                                 (nth (first item) (inside (nth (second item) (inside self))))))
                 (leftmost (loop for item in measures minimize (first (rectangle item))))
                 subgroups)
            (loop for item in measures do
                  (setf (nth 0 (rectangle item)) leftmost))
            (setf subgroups (make-aligned-groups group))
            (loop for bar in subgroups do
                  (let* ((firstm (car bar))
                         (lastm (car (last bar)))
                         (realmes (nth (first firstm) (inside (nth (second firstm) (inside self)))))
                         (ys (round (or (nth (second firstm) positions) 0)))
                         (ysf (round (or (nth (second lastm) positions) 0)))
                         (bx (car (rectangle realmes))))
                    (when (or (null *score-visible-x*)
                              (and (>= bx (- minx margin)) (<= bx (+ maxx margin))))
                      (if (= *on-all-staves* 0)
                          (om-with-font (get-font-to-draw 5)
                                        (om-draw-string (- bx (round size 4))
                                                        (- ys (round size 4))
                                                        (format () "~D" (+ (position realmes (inside (parent realmes)) :test 'equal) 1))))
                        (loop for p in positions
                              do (om-with-font (get-font-to-draw 5)
                                               (om-draw-string (+ 15 (- bx (round size 4)))
                                                               (- p (round size 4))
                                                               (format () "~D" (+ (position realmes (inside (parent realmes)) :test 'equal) 1))))))
                      (om-draw-line bx ys bx
                                    (+ ysf (get-system-size (nth (second lastm) staff) size))))))))))

;;; Coalescing the zoom rebuild.
;;;
;;; change-editor-size (projects scoreeditors.lisp: change-editor-size for
;;; scorePanel) rebuilds the whole graphic tree through update-panel, and the
;;; rebuild is what a zoom step costs.  A held Ctrl+- or a pinch delivers its steps
;;; in ONE poll-events pass, so N steps pay N full rebuilds and a single draw: the
;;; intermediate trees are built and thrown away without ever being seen.
;;; Same lambda-list, same state changes, same return value; the rebuild is marked
;;; instead of run, and the mark is honoured by the panel's own draw BEFORE it
;;; draws, so the tree the frame shows is always the tree of the size it carries.
;;; The panels are held weakly: a panel whose window closed before the flush must
;;; not be kept alive by the mark.

(defvar *score-panels-awaiting-rebuild*
  (make-hash-table :test 'eq :weakness :key)
  "Score panels whose graphic tree a deferred change-editor-size still owes.")

(defvar *score-page-bakers*
  (make-hash-table :test 'eq :weakness :key)
  "Score panels' per-page bakers: the closure that draws one page as one picture.
The page pictures are rasters here, not the metafiles vanilla records (om-LW/
icons-picts.lisp: om-record-pict), so their resolution is fixed when they are
baked.  Keeping the bake reachable per page is what lets a page be produced again
at another resolution -- on demand for the screen, and at the printer's dots for
a print job -- without holding every page of the score at that resolution.")

(defmethod change-editor-size ((self scorePanel) newsize)
  (unless (equal newsize (staff-size self))
    (setf (staff-size self) newsize)
    (om-set-font self (om-make-music-font *heads-font* newsize))
    (set-edit-param (editor self) 'fontsize newsize)
    (setf (gethash self *score-panels-awaiting-rebuild*) t)
    (om-invalidate-view self)
    t))

(defmethod om-draw-contents :around ((self scorePanel))
  (when (remhash self *score-panels-awaiting-rebuild*)
    (update-panel self)
    #+macosx (update-slot-edit self))
  ;; The bake belongs here and not inside the page drawing: begin-record takes over
  ;; the framebuffer and the matrices (alloy-desktop/graphics/image.lisp: begin-record).
  #+darwin
  (let ((bakers (gethash self *score-page-bakers*))
        (picts (score-picts-list self))
        (x0 (om-h-scroll-position self))
        (y0 (om-v-scroll-position self)))
    (when (and (score-page-mode self) bakers (= (length picts) (length bakers)))
      ;; the page rects widened by one page, so a scroll step shows what it reaches
      (loop for rect in (get-rectangle-only-pages self)
            for page from 0
            for pw = (- (om-rect-right rect) (om-rect-left rect))
            for ph = (- (om-rect-bottom rect) (om-rect-top rect))
            for nearp = (and (< (om-rect-left rect) (+ x0 (w self) pw))
                             (> (om-rect-right rect) (- x0 pw))
                             (< (om-rect-top rect) (+ y0 (h self) ph))
                             (> (om-rect-bottom rect) (- y0 ph)))
            do (cond ((and nearp (null (nth page picts)))
                      ;; the raster costs (paper x factor) squared, so the factor
                      ;; a shown page is baked at is capped
                      (setf (nth page picts)
                            (let ((oa::*om-record-pict-scale*
                                    (min 2 (or (score-scale self) 1))))
                              (funcall (nth page bakers)))))
                     ((and (not nearp) (nth page picts))
                      (om-kill-picture (nth page picts))
                      (setf (nth page picts) nil))))))
  (call-next-method))

;;; The page picts bake at the resolution the page is blitted at (projects
;;; pagination.lisp: make-score-pages-picts), so a new page scale owes them again.

#+darwin
(defmethod change-editor-scale ((self scorePanel) newscale)
  "Take SELF's page scale to NEWSCALE clamped to the allowed range, the picts owed again."
  ;; the pop-up, a typed percentage, the pinch and the zoom keys all end here,
  ;; so the range is held here and nowhere else
  (let ((newscale (max 0.5 (min 4 newscale))))
    (unless (equal newscale (score-scale self))
      (score-scale self newscale)
      ;; the bake needs the rebuilt tree: a page baked from a tree that has already
      ;; been drawn does not give the same raster
      (setf (gethash self *score-panels-awaiting-rebuild*) t)
      (om-invalidate-view self))))

;;; The presets the pop-up offers, which are also the bounds %score-page-set-scale
;;; reads for the pinch and the zoom keys (its first and its last).

#+darwin
(setf *mus-page-factors*
      '(("50%" 0.5) ("75%" 0.75) ("100%" 1) ("125%" 1.25) ("150%" 1.50)
        ("175%" 1.75) ("200%" 2) ("300%" 3) ("400%" 4)))

;;; The vanilla drawing (projects pagination.lisp: make-score-pages-picts) moved into
;;; a per-page closure; the bindings wrap the om-record-pict call, not the drawing.
;;; The CALLER owns the resolution: it binds *om-record-pict-scale* around the call.

(defun %map-score-rectangles (function object)
  "Call FUNCTION on OBJECT and on every graphic object drawn under it.
Extras carry a rectangle of their own and are not graphic containers (projects
extraobjs.lisp: the grap-extra-objet class), so they end the walk."
  (typecase object
    (grap-container
     (funcall function object)
     (dolist (item (extras object)) (%map-score-rectangles function item))
     (dolist (item (inside object)) (%map-score-rectangles function item)))
    (simple-graph-container
     (funcall function object)
     (dolist (item (extras object)) (%map-score-rectangles function item)))
    (grap-extra-objet
     (funcall function object))))

;;; Drawing a page rewrites the tree's rectangles (projects rendering.lisp:
;;; draw-object for grap-ryth-chord), which the measured page drawing reads back
;;; to place a system (projects pagination.lisp: draw-page-one-line).  Restored,
;;; not recomputed: set-graph-rectangles is not idempotent.

(defun %score-layout-rectangles (grapobj)
  "GRAPOBJ's rectangles as the layout left them, in walk order."
  (let ((rectangles '()))
    (%map-score-rectangles
     (lambda (item) (push (copy-list (rectangle item)) rectangles))
     grapobj)
    (nreverse rectangles)))

(defun %score-restore-rectangles (grapobj rectangles)
  "Put RECTANGLES back into GRAPOBJ, so a page draws from the layout again.
The numbers go into the list the slot already holds, never the kept list itself:
the drawing writes through the list it is handed."
  (let ((rest rectangles))
    (%map-score-rectangles
     (lambda (item)
       (let ((saved (pop rest))
             (current (rectangle item)))
         (if (and (consp current) (consp (cdddr current)))
             (setf (first current) (first saved)
                   (second current) (second saved)
                   (third current) (third saved)
                   (fourth current) (fourth saved))
             (setf (rectangle item) (copy-list saved)))))
     grapobj)))

(defun %score-page-baker (score grapobj fdoc page x y zoom slot size staff chnote layout)
  "A closure that draws SCORE's PAGE, at the scale in force when it is called.
Its body is the per-page drawing of projects pagination.lisp:
make-score-pages-picts.  LAYOUT is the tree's rectangles as the layout left them,
which the drawing overwrites and which every call therefore puts back first.

With a printer port in force the drawing goes to the printer and the closure
answers NIL; otherwise it is recorded and the closure answers the picture.  The
page is the same either way: one body, so the paper cannot drift from the screen.

The margins the layout used (projects edition.lisp: score-left-margin) depend on
*OLD-PRINT-MODE*, so the closure draws under the value in force when it was made."
  (let ((print-mode *old-print-mode*))
    (lambda ()
      (%score-restore-rectangles grapobj layout)
      (let* ((*old-print-mode* print-mode)
             (*internal-score-fonts* (init-fonts-to-draw size))
             (page-size (score-paper-size score)))
        (flet ((draw ()
                 (om-with-line-size 0.3
                   (om-with-fg-color nil *om-white-color*
                     (om-fill-rect 0 0 (om-point-h page-size) (om-point-v page-size))
                     (draw-score-page (list! staff)  (howmany-lines fdoc page) score x y (score-widht score size) size (and (= page 0) (page-tempo score)) nil))
                   (loop for line from 0 to (- (howmany-lines fdoc page) 1) do
                         (draw-page-one-line grapobj score page line
                                             (+ x (get-key-space score)) (initial-y-position score staff size y)
                                             (- (score-widht  score size) (get-key-space score)) (get-delta-line staff size score )
                                             zoom  slot size  staff  chnote 0)))))
          ;; the font om-record-pict is handed is the RECORDING's default font,
          ;; the one every glyph drawn without an om-with-font of its own comes
          ;; out in; a port that is given none draws the music in a text face
          (if oa::*printer-port*
              (om-with-font (get-font-to-draw 0) (draw))
              (om-record-pict (get-font-to-draw 0) page-size (draw))))))))

(defun %score-page-bakers (score grapobj x y zoom slot size staff chnote)
  "Give SCORE one baker per page of its current layout, and answer them."
  (let ((fdoc (score-fdoc score))
        ;; the tree is still the layout's here: make-pages-form-obj calls this
        ;; before any page has been drawn from it
        (layout (%score-layout-rectangles grapobj)))
    (setf (gethash score *score-page-bakers*)
          (loop for page from 0 to (- (howmany-pages fdoc) 1)
                collect (%score-page-baker score grapobj fdoc page x y zoom slot
                                           size staff chnote layout)))))

#+darwin
(defmethod make-score-pages-picts ((score scorePanel) self  grapobj x y zoom  slot size  staff chnote)
  "Give SCORE a baker per page and an empty pict list; a page bakes when it is shown."
  ;; the raster costs pages x (paper x factor) squared, so the unshown pages are not
  ;; baked; past the cap on the factor the page is magnified from the texture
  (loop for pict in (score-picts-list score) do
        (when pict (om-kill-picture pict)))
  ;; the writer only writes when its optional value is true (projects
  ;; edition.lisp: score-picts-list), so emptying the slot goes through the slot
  (setf (picts-list (edition-values score))
        (make-list (howmany-pages (score-fdoc score))))
  (%score-page-bakers score grapobj x y zoom slot size staff chnote)
  (score-picts-list score))

#-darwin
(defmethod make-score-pages-picts ((score scorePanel) self  grapobj x y zoom  slot size  staff chnote)
  "Bake every page of SCORE, as vanilla does, and keep the bakers alongside them.
The pictures are the vanilla ones, at screen resolution; the bakers are how a
print job gets the same page again at the printer's resolution."
  (loop for pict in (score-picts-list score) do
        (when pict (om-kill-picture pict)))
  (score-picts-list
   score
   (loop for baker in (%score-page-bakers score grapobj x y zoom slot size staff chnote)
         collect (funcall baker))))

;;; Printing a page of the score.  On a printer port the baker draws the page
;;; onto the device and FUNCTION is not run at all: what it would run is
;;; om-print-view, whose whole body is a blit of the page PICTURE (projects
;;; .../scoreeditors.lisp: om-print-view for scorePanel), and a picture is the
;;; one thing a vector document must not be handed.
;;; Without a printer port the page is baked again at the scale the recorder is
;;; running at and the editor's own picture is put back as soon as the page has
;;; been drawn, so only ONE page is ever held at that resolution.

(defmethod oa::%call-with-print-page ((self scorePanel) page-number function)
  (let* ((bakers (gethash self *score-page-bakers*))
         (baker (nth (1- page-number) bakers))
         (picts (score-picts-list self))
         (page nil))
    (cond ((and baker oa::*printer-port*)
           (funcall baker))
          ((and baker picts (<= page-number (length picts)))
           (let ((kept (nth (1- page-number) picts)))
             (unwind-protect
                  (progn (setf page (funcall baker))
                         (setf (nth (1- page-number) picts) page)
                         (funcall function))
               (setf (nth (1- page-number) picts) kept)
               (when page (om-kill-picture page)))))
          (t (funcall function)))))

(defmethod oa::%call-with-print-page ((self scoreeditor) page-number function)
  (oa::%call-with-print-page (panel self) page-number function))

;;; PAGE mode: Ctrl+= / Ctrl++ / Ctrl+- / Ctrl+0 and the pinch drive the page scale
;;; (projects scoreeditors.lisp: change-editor-scale), shown as an exact percentage;
;;; NORMAL mode keeps the kernel's font-size routing.

(defun %score-page-factor (percent)
  "The kernel's own page factor for PERCENT, or PERCENT/100 for a percentage that
is not one of its presets (projects scoreeditors.lisp: *mus-page-factors*)."
  (or (cadr (find percent *mus-page-factors*
                  :key (lambda (item) (round (* 100 (cadr item))))))
      (/ percent 100.0)))

(defun %score-page-scale-widget (panel)
  "PANEL's page-scale pop-up: in PAGE mode the mini-editor slot holds it
(projects scoreeditors.lisp: set-mini-ed-page)."
  (let ((widget (slotedit (ctr-view (editor panel)))))
    (and (typep widget 'om-zoom-pop-up) widget)))

(defun %score-page-origin (panel)
  "The page area's top-left, the one distance that does NOT scale with the page
factor (projects pagination.lisp: the sepw / seph of get-rectangle-pages)."
  (let ((rect (car (get-rectangle-only-pages panel))))
    (if rect
        (cons (om-rect-left rect) (om-rect-top rect))
        (cons 0 0))))

(defun %score-page-pivot (panel)
  "The pointer's offset inside the page area, while the pointer is over PANEL;
NIL anchors nothing and leaves the scroll where it is."
  (let* ((origin (%score-page-origin panel))
         (sx (om-h-scroll-position panel))
         (sy (om-v-scroll-position panel))
         (point (om-mouse-position panel))
         (px (om-point-h point))
         (py (om-point-v point)))
    (when (and (<= sx px) (< px (+ sx (w panel)))
               (<= sy py) (< py (+ sy (h panel))))
      (cons (- px (car origin)) (- py (cdr origin))))))

(defun %score-page-keep-pivot (panel pivot ratio)
  "Scroll PANEL so PIVOT keeps the screen offset it had before the scale changed."
  (when pivot
    (om-set-h-scroll-position
     panel (max 0 (round (+ (om-h-scroll-position panel)
                            (* (car pivot) (- ratio 1))))))
    (om-set-v-scroll-position
     panel (max 0 (round (+ (om-v-scroll-position panel)
                            (* (cdr pivot) (- ratio 1))))))))

(defun %score-page-set-scale (panel newscale)
  "Take PANEL's page scale to NEWSCALE, clamped to the extreme presets, anchored on
the pointer, with the page-scale widget kept in step."
  (let* ((low (cadr (first *mus-page-factors*)))
         (high (cadr (car (last *mus-page-factors*))))
         (new (max low (min high newscale)))
         (old (or (score-scale panel) 1)))
    (unless (= new old)
      (let ((pivot (%score-page-pivot panel)))
        (change-editor-scale panel new)
        (%score-page-keep-pivot panel pivot (/ new old)))
      (let ((widget (%score-page-scale-widget panel)))
        (when widget (om-set-zoom-pop-up-value widget (round (* 100 new))))))))

(defun zoom-font-scale (pane scale)
  "Pinch: in PAGE mode it scales the PAGE; elsewhere it steps the font size by at
least one, so a ratio that rounds back to the same size cannot stall the gesture."
  (if (score-page-mode pane)
      (%score-page-set-scale pane (* (or (score-scale pane) 1) scale))
      (let* ((current  (or (staff-size pane) 24))
             (rounded  (round (* current scale)))
             (stepped  (cond ((and (> scale 1) (<= rounded current)) (1+ current))
                             ((and (< scale 1) (>= rounded current)) (1- current))
                             (t rounded)))
             (new-size (max 8 (min 72 stepped))))
        (unless (= new-size current)
          (change-editor-size pane new-size)
          (let ((w (find-score-font-widget pane)))
            (when w (om-set-zoom-pop-up-value w new-size)))))))

(defun zoom-font-step (pane delta &optional resetp)
  "Ctrl+= / Ctrl++ / Ctrl+- / Ctrl+0: the same split as zoom-font-scale, stepping
the page scale by the tenth the kernel's own zoom keys use (projects
scoreeditors.lisp: zoom-font-step and the om-command-key-p branch of
handle-key-event for scorePanel)."
  (if (score-page-mode pane)
      (let ((current (or (score-scale pane) 1)))
        (%score-page-set-scale pane (cond (resetp        1)
                                          ((plusp delta) (* current 1.1))
                                          (t             (/ current 1.1)))))
      (let* ((cur (or (staff-size pane) 24))
             (new (cond (resetp                24)
                        ((plusp delta)         (min 72 (+ cur delta)))
                        (t                     (max 8  (+ cur delta))))))
        (unless (= new cur)
          (change-editor-size pane new)
          (let ((w (find-score-font-widget pane)))
            (when w (om-set-zoom-pop-up-value w new)))))))

(defmethod set-mini-ed-page ((self scoreeditor) page?)
  "Give the control view its PAGE-mode scale pop-up -- the exact-percentage widget
of projects scoreeditors.lisp: build-zoom-font-widget, in the slot the kernel reads
-- or its NORMAL-mode slot numbox, which is the vanilla's."
  (let ((control-view (ctr-view self)))
    (when (slotedit control-view)
      (setf (slotedit control-view) nil))
    (let* ((panel (panel self))
           (minied
            (if page?
                (om-make-view 'om-zoom-pop-up
                              :position #+linux (om-make-point 96 35)
                                        #-linux (om-make-point 92 26)
                              :size (om-make-point 70 18)
                              :value (round (* 100 (or (score-scale panel) 1)))
                              :presets (loop for item in *mus-page-factors*
                                             collect (round (* 100 (cadr item))))
                              :format-string "~D%"
                              :font *om-default-font1*
                              :bg-color *om-white-color*
                              :di-action #'(lambda (widget percent)
                                             (change-editor-scale
                                              panel (%score-page-factor percent))
                                             ;; a typed percentage outside the range
                                             ;; is clamped, so the widget re-reads it
                                             (om-set-zoom-pop-up-value
                                              widget (round (* 100 (or (score-scale panel) 1))))))
                (om-make-dialog-item 'edit-numbox (om-make-point 96 3)
                                     (om-make-point 50 18) " "
                                     :value nil
                                     :font *om-default-font1*
                                     :bg-color *om-white-color*
                                     :help-spec ""))))
      (setf (slotedit control-view) minied)
      (om-add-subviews control-view minied))))

;;; The page walk is defined only for containers (projects pagination.lisp:
;;; get-page-line-elements for grap-container); a NOTE page carries the note itself.
(defmethod get-page-line-elements ((self grap-note) fdoc pagenum linenum numvoice)
  (declare (ignore fdoc pagenum linenum numvoice))
  (list self))

(defmethod om-window-close-event :around ((self score-inspector-window))
  "Bury the score inspector through the window's own reference (the inspector
slot filled by projects inspectorpalette-editor.lisp: show-inspector for
scorepanel), not through *scoreinspector*, which is NIL while the palette is
alive (projects inspectorpalette-editor.lisp: update-inspector); CAPI never
reaches the kernel's method (om-LW/window.lisp: om-destroy-callback for om-abstract-window)."
  (let ((inspector (or *scoreinspector* (inspector self))))
    (when inspector
      (setf (winpos inspector) (om-view-position self)
            (win inspector) nil
            (show inspector) nil
            (current-editor inspector) nil))
    (setf *scoreinspector* nil)))

(defmethod open-ttybox :after ((self ttybox))
  "The inline edit field is white on every platform (kernel controls.lisp: open-ttybox
passes a grey :bg-color on Linux only)."
  (let ((field (text-view (editor (om-view-container (om-view-container self))))))
    (when field (om-set-bg-color field *om-white-color*))))

;;; The two loops that emit the consecutive runs of om-draw-line of a score
;;; repaint: the staff lines of draw-staff (projects scoretools.lisp: draw-staff for
;;; OMStaff) and the ledger lines of draw-auxiliar-lines (projects scoretools.lisp:
;;; draw-auxiliar-lines).  Each loop draws one line at a time under a single pen
;;; state, so the whole run can go to the GPU as one submission.
;;; Same geometry, same colour and the same place in the drawing ORDER -- a run is
;;; replaced by one submission where it already stood, so the ledger lines still
;;; land on top of the note head the caller drew just before them -- through the
;;; batch the fork already uses for the BPF and the 3D curves (om-SBCL alloy-desktop/graphics/drawing.lisp:
;;; draw-line-segments), which falls back to one line at a time whenever the
;;; pen is dashed.

(defmethod draw-staff ((self OMStaff) x top xsize fontsize deltay &optional stempo name)
  "Draw the staff lines in one submission, with vanilla geometry, on darwin an eighth of the staff space thick."
  ;; oa::*om-record-pict-scale* is the raster's samples per drawing unit, so under
  ;; one of its pixels a band rounds away: that is the floor wherever it is the higher
  (let* ((linespace (/ fontsize 4))
         (mytop (line2pixel (posy self) top linespace))
         #+darwin (thickness (max 1 (/ 1 oa::*om-record-pict-scale*)
                                  (round linespace 8))))
    (draw-tempo 4 stempo (+ x 15) (- (+ deltay mytop) (round fontsize 2)) fontsize name)
    #+darwin
    (alld:fill-rectangles
     (loop for i from 0 to (- (numlines self) 1)
           collect (let ((ly (round (+ deltay (* i (round linespace)) mytop))))
                     (list x (- ly (floor thickness 2)) xsize thickness))))
    #-darwin
    (alld:draw-line-segments
     (loop for i from 0 to (- (numlines self) 1)
           collect (let ((ly (+ deltay (* i (round linespace)) mytop)))
                     (list x ly (+ x xsize) ly))))
    (when (key-obj self)
      (om-with-font (get-font-to-draw 2)
                    (draw-key (key-obj self) (+ x linespace)
                              (+ deltay (* (- (numlines self) (keyline (key-obj self))) linespace) mytop) linespace)))
    (list (+ deltay mytop)
          (round (+ deltay (* 4 linespace) mytop)))))

;;; The staff of the score panel and of the page-mode raster (projects scoretools.lisp:
;;; draw-one-staff for OMStaff); only its strokes change, the rest is the vanilla body.
#+darwin
(defmethod draw-one-staff ((self OMStaff) x y xsize fontsize select &optional (limits nil))
  "Vanilla geometry, the staff lines filled as bands an eighth of the staff space, never under a raster pixel."
  ;; oa::*om-record-pict-scale* is the raster's samples per drawing unit, so under
  ;; one of its pixels a band rounds away: that is the floor wherever it is the higher
  (let* ((linespace (/ fontsize (- (numlines self) 1)))
         (thickness (max 1 (/ 1 oa::*om-record-pict-scale*)
                         (round linespace 8))))
    (loop for i from 0 to (- (numlines self) 1) do
          (om-fill-rect x (- (round (+ y (* i linespace))) (floor thickness 2))
                        xsize thickness))
    (when (key-obj self)
      (om-with-font (get-font-to-draw 2)
                    (draw-key (key-obj self) (+ x linespace)
                              (round (+ y (* (- (numlines self) (keyline (key-obj self))) linespace))) linespace)))
    (when limits
      (om-with-fg-color nil (if select *select-color* (om-make-color 0.92 0.92 0.92))
        (om-fill-rect (+ x linespace) (round y) xsize (* fontsize (staff-h self)))))))

(defun draw-auxiliar-lines (self x y size realpos headsizex)
  "Draw the ledger lines of SELF in one submission, with vanilla geometry."
  (declare (ignorable x))
  (when (auxlines self)
    (om-with-fg-color nil *system-color*
      (let ((dir (car (auxlines self)))
            (topy (+ (- y (round size 8)) (second (auxlines self))))
            (limy (+ (- y (round size 8)) (third (auxlines self))))
            (segments nil))
        ;; FLOOR (not ROUND) so the step is never larger than the auxlines gap
        ;; (size/4): with ROUND, the sizes where (round size 4) > size/4 lose every
        ;; ledger line.  Vanilla's own correction, kept verbatim.
        (if (equal dir 'dw)
            (progn
              (setf topy (+ topy (floor size 4)))
              (loop while (<= topy limy) do
                    (push (list (- realpos (round size 8)) topy (+ headsizex realpos) topy)
                          segments)
                    (setf topy (+ topy (floor size 4)))))
            (progn
              (setf topy (- topy (floor size 4)))
              (loop while (>= topy limy) do
                    (push (list (- realpos (round size 8)) topy (+ headsizex realpos) topy)
                          segments)
                    (setf topy (- topy (floor size 4))))))
        (alld:draw-line-segments (nreverse segments))
        nil))))

;;; The About splash credits the vanilla release (kernel graphics/dialogs.lisp: make-splash-view);
;;; the SBCL port adds its own credit under it and moves the dedication down to keep the gap.
(defun %om-about-text-p (item prefix)
  (and (typep item 'om-static-text)
       (let ((text (om-dialog-item-text item)))
         (and (stringp text)
              (>= (length text) (length prefix))
              (string= prefix text :end2 (length prefix))))))

(defgeneric %om-polish-about (view)
  (:documentation "Adjust the populated About splash VIEW before it is shown."))

(defmethod %om-polish-about ((view om-view))
  (let* ((items (om-subviews view))
         (release (find-if (lambda (item) (%om-about-text-p item "Release:")) items))
         (dedication (find-if (lambda (item) (%om-about-text-p item "Dedicated")) items)))
    (when release
      (om-set-dialog-item-text
       release (format nil "~A~%SBCL version: Paulo Raposo" (om-dialog-item-text release))))
    (when (and release dedication)
      (om-set-view-position
       dedication
       (om-make-point (oa::vx dedication)
                      (+ (oa::vy release) (* 3 (om-string-h (om-get-font release))) 8))))))

(defmethod om-add-subviews :after ((self about-window) &rest subviews)
  "The splash view reaches the About window already populated (kernel graphics/dialogs.lisp: show-kero-pict)."
  (declare (ignorable self))
  (dolist (view subviews)
    (%om-polish-about view)))
