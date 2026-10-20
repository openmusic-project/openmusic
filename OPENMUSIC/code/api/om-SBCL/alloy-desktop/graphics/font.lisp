(in-package :alloy-desktop)

;;; ---- Atlas registry ----------------------------------------------------------
;;; Fonts resolve through pre-baked MSDF atlases, so the render path never calls
;;; font discovery (CoreText faults hard on macOS).

(defvar *font-atlases*
  '(#+win32 ("Segoe UI"             "Segoe UI"             :roman :regular :system "segoeui.ttf")
    #+win32 ("Segoe UI Bold"        "Segoe UI"             :roman :bold    :system "segoeuib.ttf")
    #+win32 ("Microsoft Sans Serif" "Microsoft Sans Serif" :roman :regular :system "micross.ttf")
    ;; substitute file: the system font's own file is variable, and baking it
    ;; yields its default optical size, a display design
    #+darwin ("AppleSystemUI"       ".AppleSystemUIFont"   :roman :regular :system "HelveticaNeue.ttc")
    #+darwin ("Helvetica"           "Helvetica"            :roman :regular :system "Helvetica.ttc")
    #-(or win32 darwin) ("DejaVu Sans"     "DejaVu Sans"     :roman :regular :system "DejaVuSans.ttf")
    #-(or win32 darwin) ("DejaVu Sans Bold" "DejaVu Sans"    :roman :bold    :system "DejaVuSans-Bold.ttf")
    #-(or win32 darwin) ("Liberation Sans" "Liberation Sans" :roman :regular :system "LiberationSans-Regular.ttf")
    #-(or win32 darwin) ("Liberation Sans Bold" "Liberation Sans" :roman :bold :system "LiberationSans-Bold.ttf")
    #-(or win32 darwin) ("Noto Sans"       "Noto Sans"       :roman :regular :system "NotoSans-Regular.ttf")
    #-(or win32 darwin) ("Noto Sans Bold"  "Noto Sans"       :roman :bold    :system "NotoSans-Bold.ttf"))
  "The atlases this application bakes, as (ATLAS-NAME FAMILY SLANT WEIGHT
SOURCE-KIND . FILES).  SOURCE-KIND :bundled looks FILES up under
*bundled-font-directory*; :system looks them up in the OS font dirs.  Baked once
into the renderer cache dir; the render path resolves by atlas name and never
calls font discovery.
The default holds the platform's generic UI faces -- on Windows the face of the
non-client metrics and the one the stock dialog font resolves to, on macOS the
system UI face and Helvetica, elsewhere sans faces that practically every
distribution installs -- so the native font roles and the widgets that take them
resolve to a baked face with no application setup.  An application adds its own
entries to this list, or replaces it with the faces it is validated with; a
replacement list should name a face for each native role it uses.")

(defvar *bundled-font-directory* nil
  "Directory holding the application's own font files, in per-OS subdirectories
\(win/mac/linux) or at its root; NIL when the application bundles none.")

(defvar *fallback-families*
  '(#+win32 "Segoe UI" #+win32 "Microsoft Sans Serif"
    #+darwin ".AppleSystemUIFont" #+darwin "Helvetica"
    #-(or win32 darwin) "DejaVu Sans" #-(or win32 darwin) "Liberation Sans"
    #-(or win32 darwin) "Noto Sans")
  "Text families tried in order when a requested face has no baked atlas.  The
default lists the families of the default *font-atlases*, in the same order;
they are system faces, so none is guaranteed to be installed.  A face that is
not installed is skipped, and when none of the list is baked the fallback is
any font the renderer holds.  An application that needs a face that always
bakes ends the list with a face it bundles.")

(defvar *atlas-em-size* 64
  "MSDF bake em-size for the atlases: 2x the renderer default (32).
The bake size does NOT change the rendered size -- every scale divides the
requested draw size by the atlas em (alloy renderers/opengl/msdf.lisp:
msdf-em-size), which is the bake size itself -- so this only decides how many
atlas texels cover one device pixel.")

(defvar *atlas-spread* 8)

(defvar *native-role-fallback* nil
  "(FAMILY POINTS WEIGHT SLANT) a native role takes when the OS cannot answer
for it.  The record built from it is marked as a fallback.")

(defun os-subdir () #+win32 "win" #+darwin "mac" #-(or win32 darwin) "linux")

(defun find-font-source (kind files)
  "Existing TTF pathname for the source, or NIL. :bundled = *bundled-font-directory*
per-OS subdir, then its root; :system = first match across the OS font dirs
\(alloy-desktop/graphics/font-metrics.lisp: find-system-font-file)."
  (ecase kind
    (:bundled
     (when *bundled-font-directory*
       (loop for dir in (list (merge-pathnames
                               (make-pathname :directory (list :relative (os-subdir)))
                               *bundled-font-directory*)
                              *bundled-font-directory*)
             thereis (loop for f in files
                           for p = (merge-pathnames f dir)
                           thereis (and (probe-file p) p)))))
    (:system
     (find-system-font-file files))))

(defun atlas-file-state (path)
  "State of one baked-atlas file: :ok, :corrupt (present but zero-length) or :missing."
  (let ((file (probe-file path)))
    (cond ((null file) :missing)
          ((zerop (with-open-file (s file :element-type '(unsigned-byte 8))
                    (file-length s)))
           :corrupt)
          (t :ok))))

(defun atlas-pair-state (json png)
  "State of the JSON+PNG pair the MSDF renderer needs: :complete (both usable),
:corrupt (a present file is zero-length), :partial (one side missing) or
:missing (neither exists). Anything but :complete must be re-baked."
  (let ((j (atlas-file-state json))
        (p (atlas-file-state png)))
    (cond ((or (eq j :corrupt) (eq p :corrupt))     :corrupt)
          ((and (eq j :ok) (eq p :ok))              :complete)
          ((and (eq j :missing) (eq p :missing))    :missing)
          (t                                        :partial))))

(defun registered-font-source (name)
  "(values TTF 0) for NAME from *font-atlases*, or NIL. An atlas is named after
its source: either the entry name (\"Verdana\", \"VERDANAB\") or the file it was
baked from, which is how the same face appears as CONSOLA / COUR / TIMES; a family
name answers with its roman regular entry."
  (let ((known (or (loop for (atlas fam sl wt kind . files) in *font-atlases*
                         when (or (string-equal atlas name)
                                  (member name files
                                          :key #'pathname-name :test #'string-equal))
                           return (find-font-source kind files))
                   (loop for (atlas fam sl wt kind . files) in *font-atlases*
                         when (and (string-equal fam name) (eql sl :roman) (eql wt :regular))
                           return (find-font-source kind files)))))
    (when known (values known 0))))

(defun registered-atlas-face (atlas-name)
  "(values FAMILY BOLD-P ITALIC-P) of ATLAS-NAME's entry in *font-atlases*, or NIL."
  (loop for (atlas family slant weight) in *font-atlases*
        when (string-equal atlas atlas-name)
          return (values family (eql weight :bold) (eql slant :italic))))

;;; ---- Per-renderer font cache ----------------------------------------------
;;; CAPI converts a font description into a realized font when the pane is created
;;; and keeps it (capi/capi-capi-ref-452.htm:138), and LispWorks' own font cache is
;;; what makes a per-draw font lookup free there.  An MSDF renderer resolves per
;;; draw and has no such cache, so the resolution is memoized here.
;;;
;;; Keyed by RENDERER first -- an MSDF font instance belongs to one renderer's own
;;; fontcache (alloy renderers/opengl/msdf.lisp: the fontcache slot of the defclass
;;; renderer), so a table shared between windows would hand a window a font
;;; allocated in another GL context.  The renderer key is weak: a closed window
;;; takes its entries with it.
;;;
;;; Inside, the key is the host's font description compared by CONTENT (EQUALP),
;;; not by identity: a host that rebuilds equal descriptions on every redraw would
;;; otherwise miss on every frame.  The descriptions must be immutable, so no entry
;;; can serve a stale font.

(defstruct (font-entry (:conc-name font-entry-))
  "What a text draw would otherwise re-derive on every call, for one
\(renderer x font description): the resolved MSDF font, the alloy unit carrying
the draw size, and the baseline descent together with the px size it was computed
at.  The first two depend only on the description and on the renderer's atlases;
the descent also follows alloy:base-scale, so it is re-derived whenever that px
size changes.
GLYPHS is the host's memo of glyph shapes, keyed by string, valid for the draw
size in GLYPHS-PX and dropped whenever that size changes -- the same invalidation
the descent uses."
  font size-unit (descent-px nil) (descent 0) (glyphs-px nil) (glyphs nil))

(defvar *font-entry-cache* (make-hash-table :test 'eq :weakness :key)
  "renderer -> (EQUALP hash: font description -> font-entry).")

(defvar *font-entry-recent* nil
  "The most recent lookup, as #(renderer description entry).  A text draw asks
several times per call with the same pair -- the font, the draw size and the
baseline descent -- and hashing a description's CONTENT is not free, so the
repeat is answered by EQ.  It is written as ONE object so that a reader on
another thread sees either the whole previous triple or the whole new one, never
a mixture.")

(defun forget-font-cache (renderer)
  "Drop RENDERER's memoized font resolutions -- its set of atlases has changed."
  (setf *font-entry-recent* nil)
  (remhash renderer *font-entry-cache*))

(defun cached-font-entry (renderer key make-entry)
  "The font-entry memoized for KEY under RENDERER, built by calling MAKE-ENTRY
the first time; NIL when either RENDERER or KEY is missing."
  (when (and renderer key)
    (let ((recent *font-entry-recent*))
      (if (and recent (eq renderer (svref recent 0)) (eq key (svref recent 1)))
          (svref recent 2)
          (let* ((per-renderer (or (gethash renderer *font-entry-cache*)
                                   (setf (gethash renderer *font-entry-cache*)
                                         (make-hash-table :test 'equalp))))
                 (entry (or (gethash key per-renderer)
                            (setf (gethash key per-renderer)
                                  (funcall make-entry)))))
            (setf *font-entry-recent* (vector renderer key entry))
            entry)))))

(defun ensure-font-atlases (renderer)
  "Bake any atlas of *FONT-ATLASES* whose JSON+PNG pair is not :complete into
the renderer cache dir and register it. A missing/unbakeable font is logged and
skipped -- never signals out, so a font problem cannot crash the software."
  (let ((dir (org.shirakumo.alloy.renderers.opengl.msdf:fontcache-default-directory)))
    (dolist (entry *font-atlases*)
      (destructuring-bind (name family slant weight kind . files) entry
        (declare (ignore family slant weight))
        (let ((json (make-pathname :name name :type "json" :defaults dir))
              (png  (make-pathname :name name :type "png"  :defaults dir)))
          (handler-case
              (let ((state (atlas-pair-state json png)))
                (unless (eq state :complete)
                  (let ((src (find-font-source kind files)))
                    (if (null src)
                        (format *error-output* "[fonts] skip ~A: source font not installed~%" name)
                        (progn
                          (ensure-directories-exist json)
                          (sdf-bmfont:create-bmfont src json
                                                    :size *atlas-em-size* :spread *atlas-spread*
                                                    :mode :msdf+a :type :json)
                          (setf (gethash name (org.shirakumo.alloy.renderers.opengl.msdf::font-name-cache renderer)) json
                                (gethash name (org.shirakumo.alloy.renderers.opengl.msdf:fontcache renderer))
                                (make-instance 'org.shirakumo.alloy.renderers.opengl.msdf::font
                                               :family name :file json :renderer renderer))
                          (format *error-output* "[fonts] baked ~A (was ~A)~%" name state))))))
            (error (c)
              (format *error-output* "[fonts] FAILED ~A: ~A~%" name (princ-to-string c))))
          (force-output *error-output*))))))

(defun fallback-font (renderer)
  "Last-resort fallback (never font-discovery): first TEXT family in
*FALLBACK-FAMILIES* with a cached atlas; else any cached font; else NIL. The MSDF
fallback-chain then covers glyphs the chosen base font lacks."
  (let ((cache (org.shirakumo.alloy.renderers.opengl.msdf::font-name-cache renderer)))
    (or (loop for fam in *fallback-families*
              when (gethash fam cache)
                return (simple:request-font renderer fam))
        (loop for font being the hash-values
                of (org.shirakumo.alloy.renderers.opengl.msdf:fontcache renderer)
              return font))))

(defun variant-atlas-name (family slant weight)
  "Canonical atlas-name for FAMILY+SLANT+WEIGHT from *FONT-ATLASES*, or NIL."
  (loop for (name fam sl wt) in *font-atlases*
        when (and (string-equal fam family)
                  (eql sl (or slant :roman))
                  (eql wt (or weight :regular)))
          return name))

;;; ---- Foreign atlases in the shared cache directory -------------------------
;;; The renderer registers EVERY .json it finds in its cache directory (alloy
;;; renderers/opengl/msdf.lisp: the initialize-instance :after for renderer), and
;;; that directory is shared with anything else on the machine that bakes an MSDF
;;; atlas.  An atlas the application did not bake is not one of its faces: it would
;;; be parsed into the fallback chain and its page uploaded to the GL for nothing.
;;; So the foreign entries are taken out of both renderer caches before anything
;;; reads them, and kept aside -- a family that is actually asked for comes back.

(defvar *parked-atlases* (make-hash-table :test 'eq :weakness :key)
  "renderer -> (EQUALP hash: atlas name -> its .json). The foreign atlases taken
out of that renderer's caches; a closed window takes its entry with it.")

(defun registered-atlas-name-p (name)
  "T when NAME is an atlas the application owns: an entry of *FONT-ATLASES*, by
entry name or by the basename of the file it is baked from."
  (loop for (entry fam sl wt kind . files) in *font-atlases*
        thereis (or (string-equal entry name)
                    (and (member name files
                                 :key #'pathname-name :test #'string-equal)
                         t))))

(defun park-foreign-atlases (renderer)
  "Take every foreign atlas out of RENDERER's font caches, keeping its
name -> file pair aside for UNPARK-ATLAS.  Runs on every renderer, before
anything can read an atlas.  Answers how many were parked."
  (let ((parked (or (gethash renderer *parked-atlases*)
                    (setf (gethash renderer *parked-atlases*)
                          (make-hash-table :test 'equalp))))
        (names (org.shirakumo.alloy.renderers.opengl.msdf::font-name-cache renderer))
        (fonts (org.shirakumo.alloy.renderers.opengl.msdf:fontcache renderer))
        (foreign '()))
    (loop for name being the hash-keys of names
          unless (registered-atlas-name-p name)
            do (push name foreign))
    (dolist (name foreign)
      (setf (gethash name parked) (gethash name names))
      (remhash name names)
      (remhash name fonts))
    (length foreign)))

(defun unpark-atlas (renderer name)
  "Put the parked atlas NAME back into RENDERER's name cache and answer its file,
or NIL when nothing of that name is parked.  This is what makes an explicit
request for a family the application does not bake resolve to its own atlas
instead of the fallback -- without asking the OS font database, which macOS
cannot answer."
  (let* ((parked (gethash renderer *parked-atlases*))
         (file (and parked (gethash name parked))))
    (when file
      (setf (gethash name (org.shirakumo.alloy.renderers.opengl.msdf::font-name-cache renderer))
            file)
      (remhash name parked)
      file)))

(defun request-font-variant (renderer family slant weight)
  "Resolve FAMILY+SLANT/WEIGHT to a cached MSDF atlas WITHOUT font-discovery:
variant atlas-name -> regular family atlas -> any cached font. Never calls
find-font (CoreText faults on macOS). A foreign atlas of that name comes back
out of the park here: being asked for is what admits it."
  (let* ((cache (org.shirakumo.alloy.renderers.opengl.msdf::font-name-cache renderer))
         (name  (variant-atlas-name family slant weight)))
    (flet ((available (key)
             (and key (or (gethash key cache) (unpark-atlas renderer key)))))
      (cond ((available name)   (simple:request-font renderer name))
            ((available family) (simple:request-font renderer family))
            (t                  (fallback-font renderer))))))

(defun resolve-font (renderer family slant weight)
  "REQUEST-FONT-VARIANT, degrading to the fallback font when the request signals:
the error is reported and the draw goes on with a cached face."
  (handler-case
      (request-font-variant renderer family slant weight)
    (error (c)
      (warn "[font-resolve ~S] ~A" family (princ-to-string c))
      (fallback-font renderer))))

;;; ---- Native OS font roles -------------------------------------------------
;;; CAPI dresses the chrome it does not own with the fonts the operating system
;;; names for each role, and a widget created with no :font of its own takes the
;;; toolkit default (capi/capi-capi-ref-452.htm:138, the :font initarg of
;;; simple-pane: "nil is converted to the default font").  Two distinct native
;;; roles answer on Windows: the non-client font of NONCLIENTMETRICS dresses the
;;; menu bar, the popup items, the tooltip and the caption, while the GDI stock
;;; DEFAULT_GUI_FONT dresses every CAPI dialog item.  They are different faces
;;; and different sizes, so one variable cannot serve both.

(defstruct (native-role (:conc-name native-role-))
  "One OS font role: what the OS answered, which face that resolves to among the
baked atlases, and how much of that is measured.
POINTS is the provider's point size, exact and never rounded -- 7.8 and 8 differ
by a third of a pixel per line, which accumulates down a multi-line layout.
LOGICAL-DPI is diagnostic only: the point-to-pixel conversion goes through
typography-dpi and the display scale enters once, in apply-content-scale.
PIXEL-METRICS are the native UI metrics in device pixels, read for the geometry
that consumes them.  GENERATION is the provider generation the read belongs to,
which is what invalidates the record when the OS settings change.  HOST-FONT is
the host's own font object for the role, memoized on the record, NIL here."
  role family family-candidates points weight slant
  logical-dpi pixel-metrics source confidence generation host-font)

(defmacro with-native-font-provider ((role) &body body)
  "Run BODY, which reads the native font provider on behalf of ROLE.  A provider
failure is reported with its context and answers NIL, so the caller falls back to
a value it MARKS as a fallback -- never to a plausible one with no trace."
  (let ((tag (gensym "PROVIDER")))
    `(block ,tag
       (handler-bind ((error (lambda (condition)
                               (format *error-output*
                                       "[native-font ~S] provider failed: ~A: ~A~%"
                                       ,role (type-of condition) (princ-to-string condition))
                               (force-output *error-output*)
                               (return-from ,tag nil))))
         ,@body))))

(defun resolve-native-role-family (family)
  "(values FAMILY CONFIDENCE CANDIDATES) for FAMILY: the first candidate
with a baked atlas, whether that face is still one the OS named, and the list
tried.  The atlas resolves families by name and never calls font discovery, so a
role whose face is not baked has to degrade -- visibly."
  (let* ((os-named (list family))
         (candidates (append os-named *fallback-families*))
         (hit (loop for family in candidates
                    when (variant-atlas-name family :roman :regular)
                      return family)))
    (values (or hit (car (last *fallback-families*)))
            (if (and hit (member hit os-named :test #'string-equal)) :measured :fallback)
            candidates)))

(defun native-role-spec (role)
  "(values FAMILY POINTS WEIGHT SLANT SOURCE CONFIDENCE) the OS reports for ROLE.
The provider owns every platform-specific role, including :DIALOG-DEFAULT."
  (let ((spec (os-fonts:font-for-role role)))
    (when spec
      (values (os-fonts:font-spec-family spec) (os-fonts:font-spec-size spec)
              (os-fonts:font-spec-weight spec) (os-fonts:font-spec-slant spec)
              (os-fonts:font-spec-source spec) :measured))))

(defun build-native-role (role family points weight slant source confidence generation)
  (multiple-value-bind (resolved family-confidence candidates)
      (resolve-native-role-family family)
    (make-native-role
     :role role :family resolved :family-candidates candidates
     :points points :weight (or weight :regular) :slant (or slant :roman)
     :logical-dpi (with-native-font-provider (role) (os-fonts:logical-dpi))
     :pixel-metrics (with-native-font-provider (role) (os-fonts:metrics))
     :source source
     :confidence (if (and (eq confidence :measured)
                          (eq family-confidence :measured))
                     :measured :fallback)
     :generation generation)))

(defun read-native-role (role)
  "Read ROLE from the OS and normalize it.  When the OS cannot answer,
*native-role-fallback* stands in and the record says so, so a silent fallback can
never be mistaken for a measured native result."
  (let ((generation (with-native-font-provider (role)
                      (os-fonts:generation (os-fonts:default-backend)))))
    (multiple-value-bind (family points weight slant source confidence)
        (with-native-font-provider (role) (native-role-spec role))
      (if (and family (realp points) (plusp points))
          (build-native-role role family points weight slant source confidence generation)
          (progn
            (format *error-output*
                    "[native-font ~S] no native answer; falling back to the configured role font~%"
                    role)
            (force-output *error-output*)
            (destructuring-bind (&optional family points weight slant) *native-role-fallback*
              (build-native-role role family points weight slant
                                 :unavailable :fallback generation)))))))

(defvar *native-roles* (make-hash-table :test 'eq)
  "ROLE -> (PROVIDER-GENERATION . native-role).  Never filled at load time:
the process is DPI-unaware until GLFW initializes, so native METRICS read before
the first window exists are the virtualized ones.")

(defun native-role (role)
  "The native-role record for ROLE, read from the OS at most once per provider
generation.  A changed OS setting bumps that generation, which is what expires
the entry (os-fonts protocol.lisp: refresh)."
  (let ((generation (with-native-font-provider (role)
                      (os-fonts:generation (os-fonts:default-backend))))
        (entry (gethash role *native-roles*)))
    (if (and entry (eql (car entry) generation))
        (cdr entry)
        (cdr (setf (gethash role *native-roles*)
                   (cons generation (read-native-role role)))))))

(defun invalidate-native-fonts ()
  "Drop every cached native role, so the next request re-reads the OS."
  (clrhash *native-roles*))

(defun refresh-native-fonts ()
  "Re-read the native font settings now; true when they changed.  One re-read is
~290 ms of GDI calls on Windows, so it belongs where the display configuration is
known to have changed, never in a frame loop."
  (multiple-value-bind (state changed)
      (with-native-font-provider (:refresh) (os-fonts:refresh))
    (declare (ignore state))
    changed))

;;; ---- Native role fonts as drawn fonts ---------------------------------------
;;; A role is realized like any font: at a WHOLE device pixel, the integer lfHeight
;;; GDI builds from the point size (see resolve-font for the face).

(defun points-draw-px (renderer points)
  "Whole device px a font of POINTS is drawn at under RENDERER's unit context, never below 1."
  (max 1 (round (alloy:with-unit-parent renderer
                  (alloy:to-px (alloy:un (/ (* points (typography-dpi)) 72)))))))

(defun role-draw-px (renderer record)
  "Whole device px the native-role RECORD is drawn at under RENDERER's unit context, never below 1."
  (points-draw-px renderer (native-role-points record)))

(defun role-font-entry (renderer role)
  "The font-entry of the native ROLE under RENDERER, memoized with the other entries under the key ROLE."
  (flet ((make-entry ()
           (let ((r (native-role role)))
             (make-font-entry :font (resolve-font renderer (native-role-family r)
                                                  (native-role-slant r) (native-role-weight r))
                              :size-unit (alloy:px (role-draw-px renderer r))))))
    (declare (dynamic-extent #'make-entry))
    (cached-font-entry renderer role #'make-entry)))

(defun role-points-px (role)
  "Unrounded px of the native ROLE's point size at the typography DPI."
  (/ (* (native-role-points (native-role role)) (typography-dpi)) 72))
