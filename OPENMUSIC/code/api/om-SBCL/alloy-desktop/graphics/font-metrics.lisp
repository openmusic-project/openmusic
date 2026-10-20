(in-package :alloy-desktop)

;;; ---- Configuration ---------------------------------------------------------

(defvar *font-source-hook* nil
  "Function (NAME) -> (values FILE COLLECTION-INDEX), or NIL.  Tried before any
discovery when a font name is resolved to the TTF it was baked from; a NIL answer
falls through to the generic discovery.  When the hook is NIL the atlas registry
answers (alloy-desktop/graphics/font.lisp: registered-font-source).")

(defvar *font-face-hook* nil
  "Function (ATLAS-NAME) -> (values FAMILY BOLD-P ITALIC-P), or NIL.  Names the
face the OS is asked for when the native whole-pixel advances of an atlas are read;
an atlas it does not answer for keeps its own advances.  When the hook is NIL the
atlas registry answers (alloy-desktop/graphics/font.lisp: registered-atlas-face).")

(defvar *baked-metrics-names* nil
  "Atlas names that keep the advances and the line box they were baked with: faces
with no native counterpart to be corrected towards.")

;;; ---- Font source discovery -------------------------------------------------

(defun os-font-dirs ()
  "The directories this platform installs fonts in, user fonts included."
  (let ((home (user-homedir-pathname)))
    #+win32 (list #p"C:/Windows/Fonts/"
                  (merge-pathnames "AppData/Local/Microsoft/Windows/Fonts/" home))
    #+darwin (list (merge-pathnames "Library/Fonts/" home)   ; user fonts
                   #p"/Library/Fonts/"                        ; computer fonts (all users)
                   #p"/System/Library/Fonts/Supplemental/"    ; bundled extras: Arial/Verdana/Times/Courier
                   #p"/System/Library/Fonts/")                ; system fonts
    #-(or win32 darwin) (list #p"/usr/share/fonts/" #p"/usr/local/share/fonts/"
                              (merge-pathnames ".local/share/fonts/" home)
                              (merge-pathnames ".fonts/" home))))

(defun find-system-font-file (files)
  "Existing pathname of the first of FILES found across the OS font dirs (one level
+ recursive subdirs), or NIL."
  (loop for dir in (os-font-dirs)
        thereis (loop for f in files
                      for direct = (merge-pathnames f dir)
                      thereis (or (and (probe-file direct) direct)
                                  (first (directory
                                          (merge-pathnames
                                           (make-pathname :directory '(:relative :wild-inferiors)
                                                          :name (pathname-name f)
                                                          :type (pathname-type f))
                                           dir)))))))

;;; ---- Exact glyph advances --------------------------------------------------
;;; The MSDF bake stores ONE INTEGER advance per glyph and rounds it UP: at a
;;; 64 px em, Verdana's 'O' is 50.375 font units and the atlas carries 51, its
;;; 'i' is 17.5625 and the atlas carries 18 (verdana.ttf: the ceiling, for every
;;; glyph).  Every reader of the atlas inherits that bias -- the glyph pen of
;;; map-glyphs, 3b-bmfont:measure-glyphs behind the caret and the selection, the
;;; string measure -- so a string measures and draws about 1% wider than the
;;; same face under GDI and a label that fits its box natively wraps here
;;; ("Output Files:" is 78.12 px in a box of 80).
;;; The toolkit advances the pen by the face's own hmtx value, so the library
;;; restores it, ONCE, in the atlas data itself: every consumer is corrected
;;; without knowing about it, and measuring can never disagree with drawing.

(defvar *font-source-cache* (make-hash-table :test 'equal)
  "Atlas name -> (TTF collection-index branch), or :NONE. Resolved once per name,
misses included, so the OS font dirs are walked -- and a miss reported -- once.")

(defvar *font-dir-scan* nil
  "Family -> (file collection-index regular-p) for every face installed on this
machine; NIL until a name needs the last resort that builds it.")

(defun ttf-face-index (file family)
  "Index of the face named FAMILY inside FILE: 0 for a single-face file, the
matching member of a TrueType collection, NIL when FILE carries no such face or
cannot be read as TrueType (a CFF/OpenType outline is not readable here). A
Regular subfamily wins a tie between members of the same family."
  (handler-case
      (zpb-ttf:with-font-loader (probe file)
        (let ((count (zpb-ttf:collection-font-count probe)))
          (if (null count)
              (when (string-equal (zpb-ttf:family-name probe) family) 0)
              (loop with any = nil
                    for i from 0 below count
                    do (zpb-ttf:with-font-loader (loader file :collection-index i)
                         (when (string-equal (zpb-ttf:family-name loader) family)
                           (when (string-equal (zpb-ttf:subfamily-name loader) "Regular")
                             (return i))
                           (unless any (setf any i))))
                    finally (return any)))))
    (error (c)
      (format *error-output* "[font-source ~A] ~A~%" file (princ-to-string c))
      (force-output *error-output*)
      nil)))

(defun font-file-candidates (family)
  "File names a face called FAMILY plausibly carries in the OS font dirs: the
family with and without its spaces, in the container types this reader opens."
  (loop for stem in (remove-duplicates
                     (list (string-downcase family) family
                           (string-downcase (remove #\Space family))
                           (remove #\Space family))
                     :test #'string= :from-end t)
        append (loop for type in '("ttf" "ttc" "otf")
                     collect (concatenate 'string stem "." type))))

(defun font-database-file (family)
  "File the OS font database names for FAMILY, or NIL. macOS answers NIL by
construction: font-discovery's CoreText backend is unmaintained upstream and
faults, so the database is never queried at runtime there."
  #+darwin (progn family nil)
  #-darwin
  (handler-case
      (let ((font (org.shirakumo.font-discovery:find-font :family family)))
        (when font (org.shirakumo.font-discovery:file font)))
    (error (c)
      (format *error-output* "[font-source ~A] font database: ~A~%"
              family (princ-to-string c))
      (force-output *error-output*)
      nil)))

(defun scan-os-font-dirs ()
  "Index every readable TrueType face installed on this machine by family name.
The last resort of the font source resolution, and the only one macOS has, so it
is built at most once per session and only when a face is actually asked for."
  (let ((table (make-hash-table :test 'equalp)))
    (flet ((note (family file index regular)
             (let ((hit (gethash family table)))
               (when (or (null hit) (and regular (not (third hit))))
                 (setf (gethash family table) (list file index regular))))))
      (dolist (dir (os-font-dirs) table)
        (dolist (file (append (directory (make-pathname :name :wild :type "ttf" :defaults dir))
                              (directory (make-pathname :name :wild :type "ttc" :defaults dir))))
          (handler-case
              (zpb-ttf:with-font-loader (probe file)
                (let ((count (zpb-ttf:collection-font-count probe)))
                  (if (null count)
                      (note (zpb-ttf:family-name probe) file 0
                            (string-equal (zpb-ttf:subfamily-name probe) "Regular"))
                      (dotimes (i count)
                        (zpb-ttf:with-font-loader (loader file :collection-index i)
                          (note (zpb-ttf:family-name loader) file i
                                (string-equal (zpb-ttf:subfamily-name loader) "Regular")))))))
            (error (c)
              (format *error-output* "[font-scan ~A] ~A~%" file (princ-to-string c))
              (force-output *error-output*))))))))

(defun resolve-font-source (name)
  "(values TTF COLLECTION-INDEX BRANCH) the atlas NAME was baked from, or NIL.
Tried in cost order: *font-source-hook*, the family's own file name in the OS font
dirs, the OS font database, then the full index of installed faces."
  (multiple-value-bind (known index)
      (if *font-source-hook*
          (funcall *font-source-hook* name)
          (registered-font-source name))
    (when known
      (return-from resolve-font-source (values known index :hook))))
  (let* ((guess (find-system-font-file (font-file-candidates name)))
         (index (and guess (ttf-face-index guess name))))
    (when index
      (return-from resolve-font-source (values guess index :file-name))))
  (let* ((named (font-database-file name))
         (file  (and named (probe-file named)))
         (index (and file (ttf-face-index file name))))
    (when index
      (return-from resolve-font-source (values file index :font-database))))
  (let ((hit (gethash name (or *font-dir-scan*
                               (setf *font-dir-scan* (scan-os-font-dirs))))))
    (when hit (values (first hit) (second hit) :font-scan))))

(defun font-source (name)
  "(values TTF COLLECTION-INDEX BRANCH) the atlas NAME was baked from, or NIL when
no face of that name is installed. Memoized, misses included, so the resolution --
and the single warning a miss earns -- happens once per name."
  (multiple-value-bind (hit found) (gethash name *font-source-cache*)
    (if found
        (when (consp hit) (values (first hit) (second hit) (third hit)))
        (multiple-value-bind (src index branch) (resolve-font-source name)
          (setf (gethash name *font-source-cache*)
                (if src (list src index branch) :none))
          (unless src
            (warn "[font-source] no TTF source for atlas ~A: keeping the baked advances and line box"
                  name))
          (values src index branch)))))

(defun family-font-source (family)
  "(values TTF COLLECTION-INDEX) for FAMILY's regular source: *font-source-hook*
first, then the generic resolution under the family name."
  (let ((known (multiple-value-list
                (if *font-source-hook*
                    (funcall *font-source-hook* family)
                    (registered-font-source family)))))
    (if (first known)
        (values (first known) (second known))
        (font-source family))))

(defun apply-exact-advances (data ttf &optional (index 0))
  "Rewrite DATA's glyph advances (and its space size) with TTF's own hmtx values,
expressed in the atlas em so every existing scale factor still applies. INDEX
selects the face inside a TrueType collection."
  (let ((em (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data)))
    (zpb-ttf:with-font-loader (loader ttf :collection-index index)
      (let ((upem (float (zpb-ttf:units/em loader) 0f0)))
        (when (plusp upem)
          (flet ((advance (ch)
                   (when (and (characterp ch) (zpb-ttf:glyph-exists-p ch loader))
                     (/ (* (float (zpb-ttf:advance-width (zpb-ttf:find-glyph ch loader)) 0f0) em)
                        upem))))
            (maphash (lambda (ch glyph)
                       (let ((a (advance ch)))
                         (when a (setf (3b-bmfont:glyph-xadvance glyph) a))))
                     (3b-bmfont::chars data))
            ;; a space carries no outline, so map-glyphs advances it by space-size
            ;; and never through the glyph table (alloy renderers/opengl/msdf.lisp: map-glyphs)
            (let ((a (advance #\Space)))
              (when a (setf (3b-bmfont::space-size data) a)))))))))

(defvar *exact-advances-applied*
  (make-hash-table :test 'eq :weakness :key)
  "The bmfont data objects whose advances were already rewritten.
One parsed atlas is shared by every font over that file (alloy
renderers/opengl/msdf.lisp: *bmfont-data-cache*), so the second window's font
receives a table that is already corrected; without this the correction would
re-open and re-walk the TTF once per window.")

(defmethod (setf org.shirakumo.alloy.renderers.opengl.msdf::data)
    :after (data (font org.shirakumo.alloy.renderers.opengl.msdf::font))
  "Correct the baked advances the moment an atlas is read -- the single point both
partial-allocate and allocate go through (alloy renderers/opengl/msdf.lisp:
partial-allocate, and alloy:allocate for font). Each parsed atlas is corrected
exactly once, however many fonts come to share it. A face whose TTF cannot be
located keeps the baked advances; the miss is reported once, by the resolution
itself.
Faces named in *baked-metrics-names* keep the baked advances: the correction
reproduces the OS's own text advances, and a face without a native target has
nothing to be corrected towards."
  (let ((name (pathname-name (org.shirakumo.alloy.renderers.opengl.msdf:file font))))
    (unless (or (gethash data *exact-advances-applied*)
                (member name *baked-metrics-names* :test #'string-equal))
      (setf (gethash data *exact-advances-applied*) t)
      (multiple-value-bind (ttf index) (font-source name)
        (when ttf
          (handler-case (apply-exact-advances data ttf index)
            (error (c)
              (warn "[exact-advances ~A] ~A" ttf (princ-to-string c)))))))))

;;; ---- Native whole-pixel advances -------------------------------------------
;;; The toolkit LAYS TEXT OUT with lfQuality = CLEARTYPE_NATURAL_QUALITY and
;;; walks the WHOLE-PIXEL widths GDI answers at that quality, so its layout pen
;;; lands on integers the outline never produces: Verdana advances 'm' by 13 px
;;; at ppem 12 while the face's own hmtx value is 11.67.  That table is what
;;; compute-char-extents walks, and therefore what decides every line break; it
;;; is read ONCE per (face x ppem), never per frame, per update or per draw.
;;;
;;; It is NOT what the toolkit DRAWS on, and not what get-string-extent answers.
;;; Both of those are the face's own advance sum: drawn on the whole-pixel grid
;;; the ink overshoots the toolkit's by up to a dozen pixels per label, while the
;;; face's own advances land within a pixel or two.  So the pen that draws, the
;;; caret, the selection and the hit test all stay on the face's own advances,
;;; and only the WRAP reads the table.
;;;
;;; The scratch DC MUST have a COLOUR bitmap selected into it: on the 1x1
;;; MONOCHROME surface CreateCompatibleDC(NULL) hands back, ClearType is off and
;;; lfQuality is silently ignored, so every quality answers the same hinted
;;; widths and the table comes out wrong with no error anywhere.

(defun read-native-advance-block (family ppem bold italic block)
  "The 256 whole-pixel advances the OS answers for the characters of BLOCK
\(codes BLOCK*256 .. BLOCK*256+255) of FAMILY at PPEM, or NIL when this platform
has no such table.
lfQuality, ClearType and the GDI hinting are a Windows policy; CoreText and
FreeType apply their own, so on macOS and Linux the face's own advances are kept."
  (os-fonts:native-advance-block family ppem bold italic block))

(defstruct (q6-advances (:conc-name q6-))
  "One (face x ppem)'s whole-pixel advances, tabulated per 256-character block.
LATIN0 is block 0 -- every character a Latin interface writes -- kept in its own
slot so the glyph pass reads a single aref.  BLOCKS holds the rest, read on
first demand and memoized, so no character is ever measured twice."
  latin0 (blocks (make-hash-table :test 'eql)) family ppem bold italic)

(defun q6-advance-slow (table code)
  "TABLE's advance for CODE outside block 0, reading that block on first demand."
  (let ((hit (gethash (ash code -8) (q6-blocks table))))
    (unless hit
      (setf hit (or (read-native-advance-block (q6-family table) (q6-ppem table)
                                               (q6-bold table) (q6-italic table)
                                               (ash code -8))
                    :none))
      (setf (gethash (ash code -8) (q6-blocks table)) hit))
    (unless (eq hit :none)
      (let ((v (aref (the (simple-array (signed-byte 32) (256)) hit) (logand code 255))))
        (unless (minusp v) v)))))

(declaim (inline q6-advance))
(defun q6-advance (table code)
  "TABLE's whole-pixel advance for character CODE, or NIL when the OS reports
none -- the caller then keeps the face's own advance for that one glyph."
  (declare (type fixnum code))
  (if (< code 256)
      (let ((v (aref (the (simple-array (signed-byte 32) (256)) (q6-latin0 table)) code)))
        (if (minusp v) nil v))
      (q6-advance-slow table code)))

(defvar *q6-face-tables* (make-hash-table :test 'equal)
  "(family ppem bold italic) -> q6-advances, or :NONE.  ONE OS read per face
and size for the life of the image: nothing here runs per frame, per update or
per draw call.")

(defun q6-face-table (family ppem bold italic)
  "The whole-pixel advance table for FAMILY at PPEM, read once and memoized;
NIL when this platform, or this machine's font set, cannot answer for it."
  (let ((hit (gethash (list family ppem bold italic) *q6-face-tables*)))
    (unless hit
      (setf hit (or (let ((block0 (read-native-advance-block family ppem bold italic 0)))
                      (when block0
                        (make-q6-advances :latin0 block0 :family family :ppem ppem
                                          :bold bold :italic italic)))
                    :none))
      (setf (gethash (list family ppem bold italic) *q6-face-tables*) hit))
    (unless (eq hit :none) hit)))

(defvar *font-q6-advances* (make-hash-table :test 'eq :weakness :key)
  "Realized MSDF font -> (ppem . q6-advances|NIL).  The atlas name, the family
behind it and the OS read are resolved once per font and draw size; a closed
window's renderer takes its fonts, and these entries, with it.")

(defun resolve-q6-advances (font ppem)
  "The native advance table behind FONT at PPEM, or NIL for a face with no native
reference: the atlases named in *baked-metrics-names*, and any atlas
*font-face-hook* -- or the atlas registry, when it is NIL -- does not answer for."
  (let ((name (pathname-name (org.shirakumo.alloy.renderers.opengl.msdf:file font))))
    (unless (member name *baked-metrics-names* :test #'string-equal)
      (multiple-value-bind (family bold italic)
          (if *font-face-hook*
              (funcall *font-face-hook* name)
              (registered-atlas-face name))
        (when family
          (q6-face-table family ppem bold italic))))))

(defun q6-advances (font px)
  "FONT's whole-pixel advance table at PX device pixels, or NIL when the face's
own advances are kept: off Windows, for a baked-metrics atlas, and for a draw
size that is not a whole ppem -- the OS realizes integer pixel sizes only, so a
fractional one has no table of its own."
  (when (and (typep font 'org.shirakumo.alloy.renderers.opengl.msdf::font)
             (realp px) (= px (round px)))
    (let* ((ppem (round px))
           (hit  (gethash font *font-q6-advances*)))
      (unless (and hit (eql (car hit) ppem))
        (setf hit (cons ppem (resolve-q6-advances font ppem)))
        (setf (gethash font *font-q6-advances*) hit))
      (cdr hit))))

(declaim (ftype (function (t t single-float) single-float) glyph-advance-px))
(defun glyph-advance-px (table char fallback)
  "CHAR's advance in device px on the grid the toolkit walks: TABLE's whole
pixel where it has one, FALLBACK -- the face's own scaled advance -- otherwise."
  (let ((v (and table (q6-advance table (char-code char)))))
    (if v (float v 0f0) fallback)))

(defun pen-advance (data string start end px)
  "Pen travel from START to END of STRING in device px, on the SAME grid the
glyph pass draws on (alloy renderers/opengl/msdf.lisp: map-glyphs): the face's own advances,
scaled by the draw size.  The caret, the selection rectangles and the click hit
test all read this, so none of them can land on a grid the ink is not on.
START and END are CLAMPED to STRING: the callers derive them from a line-break
vector that a pending re-layout can leave one edit behind, and an index past the
text has to answer the end of the text rather than signal."
  (let* ((scale (/ (float px 0f0) (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data)))
         (from  (max 0 (min start (length string))))
         (to    (max from (min end (length string)))))
    (* scale (float (3b-bmfont:measure-glyphs data string :start from :end to) 0f0))))

;;; ---- Native line box -------------------------------------------------------
;;; The line box the toolkit answers with (gp:get-string-extent): the face's own
;;; VDMX record for that ppem, and OS/2 usWinAscent / usWinDescent rounded at the
;;; ppem for a face carrying no VDMX.  Faces named in *baked-metrics-names* keep
;;; the baked box.

(defconstant +line-box-ppem+ 256
  "Ppem values the per-face line box is tabulated for.  A draw size past it is
answered from the OS/2 fields on the spot, which is the branch that has no table.")

(defstruct (face-line-box (:conc-name flb-))
  "One face's native line box, read once and tabulated per ppem.  ASCENTS and
DESCENTS carry the whole device pixels for every ppem below +line-box-ppem+,
filled from OS/2 and then overwritten wherever VDMX has a record; WIN-ASCENT,
WIN-DESCENT and UPEM keep the OS/2 branch computable past the table."
  ascents descents win-ascent win-descent upem)

(defvar *face-line-boxes* (make-hash-table :test 'equal)
  "(TTF collection-index) -> face-line-box, or :NONE.  One read per face, ever.
The index belongs in the key: the members of a TrueType collection can share a
glyph table and still carry different OS/2 metrics.")

(defvar *font-line-boxes* (make-hash-table :test 'eq :weakness :key)
  "Realized MSDF font -> its face-line-box, or :NONE.  The atlas name and the
TTF lookup behind it are resolved once per font; a closed window's renderer takes
its fonts, and these entries, with it.")

(defun read-os2-win-metrics (loader)
  "(values usWinAscent usWinDescent) of LOADER's OS/2 table, or NIL when the face
carries none.  The two fields sit at byte 74 and 76 of the table in every version
of OS/2, so no version dispatch is needed to reach them."
  (when (zpb-ttf::table-exists-p "OS/2" loader)
    (let ((stream (zpb-ttf::input-stream loader)))
      (file-position stream (+ (zpb-ttf::table-position "OS/2" loader) 74))
      (values (zpb-ttf::read-uint16 stream)
              (zpb-ttf::read-uint16 stream)))))

(defun read-vdmx (loader ascents descents)
  "Overwrite ASCENTS and DESCENTS with LOADER's VDMX records, for the aspect group
a square-pixel display selects: the first ratio whose xRatio is 0 (any device) or
1 with the 1:1 y range inside it.  Does nothing when the face has no VDMX, which
is exactly the case the OS/2 fill already answers."
  (when (zpb-ttf::table-exists-p "VDMX" loader)
    (let* ((stream (zpb-ttf::input-stream loader))
           (base (zpb-ttf::table-position "VDMX" loader)))
      (file-position stream (+ base 4))
      (let ((n-ratios (zpb-ttf::read-uint16 stream))
            (pick nil))
        (loop for r from 0 below n-ratios
              until pick
              do (file-position stream (+ base 6 (* 4 r)))
                 (zpb-ttf::read-uint8 stream)   ; bCharSet
                 (let ((x-ratio (zpb-ttf::read-uint8 stream))
                       (y-start (zpb-ttf::read-uint8 stream))
                       (y-end   (zpb-ttf::read-uint8 stream)))
                   (when (or (zerop x-ratio)
                             (and (= x-ratio 1) (<= y-start 1 y-end)))
                     (setf pick r))))
        (when pick
          (file-position stream (+ base 6 (* 4 n-ratios) (* 2 pick)))
          (let ((group (+ base (zpb-ttf::read-uint16 stream))))
            (file-position stream group)
            (let ((records (zpb-ttf::read-uint16 stream)))
              (zpb-ttf::read-uint8 stream)      ; startsz
              (zpb-ttf::read-uint8 stream)      ; endsz
              (loop repeat records
                    for ppem  = (zpb-ttf::read-uint16 stream)
                    for y-max = (zpb-ttf::read-int16 stream)
                    for y-min = (zpb-ttf::read-int16 stream)
                    when (< ppem +line-box-ppem+)
                      do (setf (aref ascents ppem)  (max 0 y-max)
                               (aref descents ppem) (max 0 (- y-min)))))))))))

(defun compute-face-line-box (ttf &optional (index 0))
  "TTF's native line box, or NIL when the face carries no usable OS/2 table.
INDEX selects the face inside a TrueType collection."
  (zpb-ttf:with-font-loader (loader ttf :collection-index index)
    (multiple-value-bind (win-ascent win-descent) (read-os2-win-metrics loader)
      (let ((upem (zpb-ttf:units/em loader)))
        (when (and win-ascent (plusp upem))
          (let ((ascents  (make-array +line-box-ppem+ :element-type '(unsigned-byte 16)))
                (descents (make-array +line-box-ppem+ :element-type '(unsigned-byte 16))))
            (dotimes (ppem +line-box-ppem+)
              (setf (aref ascents ppem)  (round (* win-ascent ppem) upem)
                    (aref descents ppem) (round (* win-descent ppem) upem)))
            (read-vdmx loader ascents descents)
            (make-face-line-box :ascents ascents :descents descents
                                :win-ascent win-ascent :win-descent win-descent
                                :upem upem)))))))

(defun resolve-face-line-box (font)
  "The native line box behind the realized FONT, read from the TTF its atlas was
baked from, or NIL for a baked-metrics atlas and for a face whose source cannot be
read.  A TTF that is gone or truncated is reported with its name and degrades to
the baked box; a malformed font is a bug and propagates."
  (let ((name (pathname-name (org.shirakumo.alloy.renderers.opengl.msdf:file font))))
    (unless (member name *baked-metrics-names* :test #'string-equal)
      (multiple-value-bind (ttf index) (font-source name)
        (when ttf
          (let* ((key (list ttf index))
                 (hit (gethash key *face-line-boxes*)))
            (unless hit
              (setf hit (or (handler-case (compute-face-line-box ttf index)
                              ((or file-error end-of-file) (c)
                                (format *error-output* "[line-box ~A] ~A~%"
                                        ttf (princ-to-string c))
                                (force-output *error-output*)
                                nil))
                            :none))
              (setf (gethash key *face-line-boxes*) hit))
            (unless (eq hit :none) hit)))))))

(defun font-line-box (font)
  "FONT's native line box, or NIL when the face keeps the baked one."
  (let ((hit (gethash font *font-line-boxes*)))
    (unless hit
      (setf hit (or (resolve-face-line-box font) :none))
      (setf (gethash font *font-line-boxes*) hit))
    (unless (eq hit :none) hit)))

(defun line-box (box px)
  "(values ASCENT DESCENT) of BOX at PX device pixels, in whole device pixels --
the gp:get-font-ascent / -descent pair."
  (let ((ppem (max 0 (round px))))
    (if (< ppem +line-box-ppem+)
        (values (aref (flb-ascents box) ppem) (aref (flb-descents box) ppem))
        (values (round (* (flb-win-ascent box) ppem) (flb-upem box))
                (round (* (flb-win-descent box) ppem) (flb-upem box))))))

(declaim (ftype (function (t t) single-float) line-advance))
(defun line-advance (font px)
  "Baseline-to-baseline distance of FONT at PX device pixels: the native line box
of the face, and the baked line height for the faces that keep it.  Every vertical
metric goes through here, so measuring can never disagree with the pitch the glyph
pass actually walks."
  (let ((box (font-line-box font)))
    (if box
        (multiple-value-bind (ascent descent) (line-box box px)
          (float (+ ascent descent) 0f0))
        (let ((data (org.shirakumo.alloy.renderers.opengl.msdf::data font)))
          (* (/ (float px 0f0) (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data))
             (float (3b-bmfont:line-height data) 0f0))))))

(declaim (ftype (function (t t) single-float) baseline-rise))
(defun baseline-rise (font px)
  "Px from the pen of a line up to its baseline, for FONT at PX device pixels --
what separates a shape anchored on its own line box from a baseline the caller
places itself.  3b-bmfont's base is the ascender MINUS the padding the bake adds
below it (sdf bmfont.lisp: to-bmfont), so the padding is added back."
  (let* ((data (org.shirakumo.alloy.renderers.opengl.msdf::data font))
         (g (/ (float px 0f0) (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data))))
    (- (line-advance font px)
       (* g (+ (float (3b-bmfont:base data) 0f0)
               (float (3b-bmfont:padding-down data) 0f0))))))

;;; ---- Underline metrics -----------------------------------------------------

(defvar *underline-metrics-cache* (make-hash-table :test 'equal)
  "Family -> #(underline-position underline-thickness units-em), or NIL.")

(declaim (ftype (function (t) (or null (simple-array single-float (3))))
                font-underline-metrics))
(defun font-underline-metrics (msdf-font)
  "Return #(underline-position underline-thickness units-em) in TTF units for MSDF-FONT, or NIL."
  ;; simple:family is a reader of simple:font (alloy renderers/simple/defaults.lisp:
  ;; the family slot of the defclass font): the
  ;; type test is the applicability test, so no condition has to be intercepted.
  (let ((family (when (typep msdf-font 'simple:font) (simple:family msdf-font))))
    (when family
      (multiple-value-bind (cached present-p) (gethash family *underline-metrics-cache*)
        (if present-p
            cached
            (let ((metrics
                    (handler-case
                        (multiple-value-bind (path index) (family-font-source family)
                          (when path
                            (zpb-ttf:with-font-loader (loader path :collection-index index)
                              (make-array 3 :element-type 'single-float
                                            :initial-contents
                                            (list (float (zpb-ttf:underline-position loader) 0f0)
                                                  (float (zpb-ttf:underline-thickness loader) 0f0)
                                                  (float (zpb-ttf:units/em loader) 0f0))))))
                      ;; the TTF the source points at may be gone (file-error) or
                      ;; truncated (end-of-file); a malformed font is a bug and propagates
                      ((or file-error end-of-file) (c)
                        (format *error-output* "[underline-metrics ~A] ~A~%" family (princ-to-string c))
                        nil))))
              (setf (gethash family *underline-metrics-cache*) metrics)
              metrics))))))
