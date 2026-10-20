(in-package :alloy-desktop)

;;; The Windows printer port.  CAPI selects a printer port into the graphics port
;;; and the ordinary drawing primitives then land on paper; here the primitives
;;; dispatch on *CURRENT-PORT* (graphics/drawing.lisp) and, while a page is drawn
;;; inside CALL-WITH-PRINTER-PORT, arrive at the PRINTER-PORT methods below, where
;;; the same drawing is expressed in GDI.  A page therefore reaches the document as
;;; text, lines and shapes rather than as a picture of them.

(cffi:defcstruct gdi-point
  (x :int32) (y :int32))

(cffi:defcstruct gdi-rect
  (left :int32) (top :int32) (right :int32) (bottom :int32))

(cffi:defcstruct gdi-logbrush
  (style :uint32) (color :uint32) (hatch :pointer))

(cffi:defcstruct gdi-logfont
  (height :int32) (width :int32) (escapement :int32) (orientation :int32)
  (weight :int32)
  (italic :uint8) (underline :uint8) (strike-out :uint8) (char-set :uint8)
  (out-precision :uint8) (clip-precision :uint8) (quality :uint8)
  (pitch-and-family :uint8)
  (face-name :uint16 :count 32))

(cffi:defcstruct gdi-bitmapinfoheader
  (size :uint32) (width :int32) (height :int32) (planes :uint16)
  (bit-count :uint16) (compression :uint32) (image-size :uint32)
  (x-pels-per-meter :int32) (y-pels-per-meter :int32)
  (colors-used :uint32) (colors-important :uint32))

(cffi:defcfun ("CreateFontIndirectW" %gdi-create-font) :pointer (logfont :pointer))
(cffi:defcfun ("SelectObject" %gdi-select) :pointer (hdc :pointer) (object :pointer))
(cffi:defcfun ("DeleteObject" %gdi-delete-object) :int32 (object :pointer))
(cffi:defcfun ("GetStockObject" %gdi-stock-object) :pointer (index :int32))
(cffi:defcfun ("ExtTextOutW" %gdi-text-out) :int32
  (hdc :pointer) (x :int32) (y :int32) (options :uint32) (rectangle :pointer)
  (string :pointer) (count :uint32) (widths :pointer))
(cffi:defcfun ("SetTextColor" %gdi-set-text-color) :uint32
  (hdc :pointer) (color :uint32))
(cffi:defcfun ("SetBkMode" %gdi-set-background-mode) :int32
  (hdc :pointer) (mode :int32))
(cffi:defcfun ("SetTextAlign" %gdi-set-text-align) :uint32
  (hdc :pointer) (align :uint32))
(cffi:defcfun ("CreatePen" %gdi-create-pen) :pointer
  (style :int32) (width :int32) (color :uint32))
(cffi:defcfun ("ExtCreatePen" %gdi-ext-create-pen) :pointer
  (style :uint32) (width :uint32) (brush :pointer) (count :uint32)
  (style-array :pointer))
(cffi:defcfun ("CreateSolidBrush" %gdi-create-brush) :pointer (color :uint32))
(cffi:defcfun ("MoveToEx" %gdi-move-to) :int32
  (hdc :pointer) (x :int32) (y :int32) (previous :pointer))
(cffi:defcfun ("LineTo" %gdi-line-to) :int32 (hdc :pointer) (x :int32) (y :int32))
(cffi:defcfun ("Rectangle" %gdi-rectangle) :int32
  (hdc :pointer) (left :int32) (top :int32) (right :int32) (bottom :int32))
(cffi:defcfun ("FillRect" %gdi-fill-rect) :int32
  (hdc :pointer) (rectangle :pointer) (brush :pointer))
(cffi:defcfun ("Ellipse" %gdi-ellipse) :int32
  (hdc :pointer) (left :int32) (top :int32) (right :int32) (bottom :int32))
(cffi:defcfun ("Arc" %gdi-arc) :int32
  (hdc :pointer) (left :int32) (top :int32) (right :int32) (bottom :int32)
  (x-start :int32) (y-start :int32) (x-end :int32) (y-end :int32))
(cffi:defcfun ("Polygon" %gdi-polygon) :int32
  (hdc :pointer) (points :pointer) (count :int32))
(cffi:defcfun ("AddFontResourceExW" %gdi-add-font-resource) :int32
  (name :pointer) (flags :uint32) (reserved :pointer))
(cffi:defcfun ("SetStretchBltMode" %gdi-set-stretch-mode) :int32
  (hdc :pointer) (mode :int32))
(cffi:defcfun ("SetBrushOrgEx" %gdi-set-brush-origin) :int32
  (hdc :pointer) (x :int32) (y :int32) (previous :pointer))
(cffi:defcfun ("StretchDIBits" %gdi-stretch-dibits) :int32
  (hdc :pointer) (destination-x :int32) (destination-y :int32)
  (destination-width :int32) (destination-height :int32)
  (source-x :int32) (source-y :int32)
  (source-width :int32) (source-height :int32)
  (bits :pointer) (bitmap-info :pointer) (usage :uint32)
  (raster-operation :uint32))

(defconstant +gdi-transparent+ 1)
(defconstant +gdi-align-baseline-left+ 24)
(defconstant +gdi-pen-solid+ 0)
(defconstant +gdi-pen-geometric-dash+ #x00010001)
(defconstant +gdi-pen-endcap-flat+ #x00000200)
(defconstant +gdi-pen-join-miter+ #x00002000)
(defconstant +gdi-brush-solid+ 0)
(defconstant +gdi-stock-white-brush+ 0)
(defconstant +gdi-stock-null-brush+ 5)
(defconstant +gdi-stock-black-pen+ 7)
(defconstant +gdi-stock-null-pen+ 8)
(defconstant +gdi-stock-system-font+ 13)
(defconstant +gdi-font-private+ #x10)
(defconstant +gdi-halftone+ 4)
(defconstant +gdi-dib-rgb-colors+ 0)
(defconstant +gdi-source-copy+ #x00CC0020)

(defstruct (printer-port (:conc-name %pp-) (:constructor %make-printer-port))
  (hdc (cffi:null-pointer))
  (origin-x 0)
  (origin-y 0)
  (scale-x 1)
  (scale-y 1)
  (font-function nil)
  (pens (make-hash-table :test 'equal))
  (brushes (make-hash-table :test 'equal))
  (fonts (make-hash-table :test 'equal))
  (selected-pen nil)
  (selected-brush nil)
  (selected-font nil))

;;; ------------------------------------------------------- device coordinates

(defun %pp-x (port x)
  (round (+ (%pp-origin-x port) (* (%pp-scale-x port) (+ x *pox*)))))

(defun %pp-y (port y)
  ;; no flip: the port and GDI both count rows downwards from the top left
  (round (+ (%pp-origin-y port) (* (%pp-scale-y port) (+ y *poy*)))))

(defun %pp-width (port n) (max 1 (round (* (%pp-scale-x port) n))))

(defun %pp-height (port n) (max 1 (round (* (%pp-scale-y port) n))))

(defun %pp-colorref (color)
  "COLOR as a Win32 COLORREF.
The drawing state carries a chroma color, or NIL for the default, which degrades to
black exactly as the screen path does."
  (let ((c (or color colors:black)))
    (flet ((component (value) (max 0 (min 255 (round (* 255 value))))))
      (logior (component (chroma:r c))
              (ash (component (chroma:g c)) 8)
              (ash (component (chroma:b c)) 16)))))

;;; -------------------------------------------------------------- GDI objects

(defun %pp-pen (port)
  "The GDI pen for the current line state, created once per distinct state.
The width is the pen size taken to device units and not through the screen path's
PEN-WIDTH: that multiplier compensates alloy's thin-line rendering, while CAPI hands
the printer port the thickness it was given."
  (let* ((width (%pp-width port *current-pen-size*))
         (color (%pp-colorref *current-fg-color*))
         (dashed (and (or *current-dash-pattern*
                          (eq *current-line-style* :dashed))
                      t))
         (key (list width color dashed)))
    (or (gethash key (%pp-pens port))
        (setf (gethash key (%pp-pens port))
              (if dashed
                  ;; geometric, not cosmetic: a cosmetic pen is one device dot
                  ;; wide, which at printer resolution is invisible
                  (cffi:with-foreign-object (brush '(:struct gdi-logbrush))
                    (setf (cffi:foreign-slot-value brush '(:struct gdi-logbrush) 'style)
                          +gdi-brush-solid+
                          (cffi:foreign-slot-value brush '(:struct gdi-logbrush) 'color)
                          color
                          (cffi:foreign-slot-value brush '(:struct gdi-logbrush) 'hatch)
                          (cffi:null-pointer))
                    (%gdi-ext-create-pen (logior +gdi-pen-geometric-dash+
                                                 +gdi-pen-endcap-flat+
                                                 +gdi-pen-join-miter+)
                                         width brush 0 (cffi:null-pointer)))
                  (%gdi-create-pen +gdi-pen-solid+ width color))))))

(defun %pp-brush (port color)
  (let ((key (%pp-colorref color)))
    (or (gethash key (%pp-brushes port))
        (setf (gethash key (%pp-brushes port)) (%gdi-create-brush key)))))

(defun %pp-font (port)
  "The GDI font for the current font, created once per distinct font.
The port's FONT-FUNCTION answers (values FAMILY SIZE BOLD-P ITALIC-P) with SIZE in
port units; with no function, or a NIL family, the text face is Arial at 12.  A
bundled face is named by family like any other, which a private registration makes
possible (REGISTER-PRIVATE-FONTS)."
  (multiple-value-bind (family size bold-p italic-p)
      (let ((function (%pp-font-function port)))
        (and function (funcall function)))
    (let* ((family (or family "Arial"))
           (height (max 1 (round (* (%pp-scale-y port) (or size 12)))))
           (weight (if bold-p 700 400))
           (italic (if italic-p 1 0))
           (key (list family height weight italic)))
      (or (gethash key (%pp-fonts port))
          (setf (gethash key (%pp-fonts port))
                (cffi:with-foreign-object (logfont '(:struct gdi-logfont))
                  (dotimes (index (cffi:foreign-type-size '(:struct gdi-logfont)))
                    (setf (cffi:mem-aref logfont :uint8 index) 0))
                  ;; negative: the number is the character height, not the cell
                  (setf (cffi:foreign-slot-value logfont '(:struct gdi-logfont) 'height)
                        (- height)
                        (cffi:foreign-slot-value logfont '(:struct gdi-logfont) 'weight)
                        weight
                        (cffi:foreign-slot-value logfont '(:struct gdi-logfont) 'italic)
                        italic)
                  (let ((name (cffi:foreign-slot-pointer logfont '(:struct gdi-logfont)
                                                         'face-name)))
                    (dotimes (index 32)
                      (setf (cffi:mem-aref name :uint16 index)
                            (if (< index (length family))
                                (char-code (char family index))
                                0))))
                  (%gdi-create-font logfont)))))))

(defun %pp-select-pen (port)
  (let ((pen (%pp-pen port)))
    (unless (eq pen (%pp-selected-pen port))
      (%gdi-select (%pp-hdc port) pen)
      (setf (%pp-selected-pen port) pen))
    pen))

(defun %pp-select-null-pen (port)
  (let ((pen (%gdi-stock-object +gdi-stock-null-pen+)))
    (unless (eq pen (%pp-selected-pen port))
      (%gdi-select (%pp-hdc port) pen)
      (setf (%pp-selected-pen port) pen))
    pen))

(defun %pp-select-brush (port color)
  (let ((brush (%pp-brush port color)))
    (unless (eq brush (%pp-selected-brush port))
      (%gdi-select (%pp-hdc port) brush)
      (setf (%pp-selected-brush port) brush))
    brush))

(defun %pp-select-null-brush (port)
  (let ((brush (%gdi-stock-object +gdi-stock-null-brush+)))
    (unless (eq brush (%pp-selected-brush port))
      (%gdi-select (%pp-hdc port) brush)
      (setf (%pp-selected-brush port) brush))
    brush))

(defun %pp-select-font (port)
  (let ((font (%pp-font port)))
    (unless (eq font (%pp-selected-font port))
      (%gdi-select (%pp-hdc port) font)
      (setf (%pp-selected-font port) font))
    font))

;;; ------------------------------------------------------------ private fonts

(defun register-private-fonts (paths)
  "Let GDI resolve the faces in the font files PATHS by family name, for this
process only; answer the paths GDI refused.  Without it CreateFontIndirectW
substitutes an installed face, and a face the application ships (music symbols,
say) comes out as letters of another.  A private registration lasts for the
process, so a caller registers each file once."
  (loop for path in paths
        when (cffi:with-foreign-string
                 (wide (substitute #\\ #\/ (namestring path)) :encoding :utf-16le)
               (zerop (%gdi-add-font-resource wide +gdi-font-private+ (cffi:null-pointer))))
          collect path))

;;; --------------------------------------------------------------- primitives

(defmethod port-draw-line ((port printer-port) x1 y1 x2 y2)
  (%pp-select-pen port)
  (%gdi-move-to (%pp-hdc port) (%pp-x port x1) (%pp-y port y1)
                (cffi:null-pointer))
  (%gdi-line-to (%pp-hdc port) (%pp-x port x2) (%pp-y port y2))
  nil)

(defmethod port-draw-line-segments ((port printer-port) segments)
  ;; a printer device has no batch of its own, and the GL batch draws through
  ;; the renderer this page is not going to
  (dolist (segment segments) (apply #'port-draw-line port segment)))

(defmethod port-draw-text ((port printer-port) x y string font size entry)
  "Draw STRING with its BASELINE at Y, which is what the argument means
(om-LW/graphics.lisp: om-draw-string hands Y straight to gp:draw-string)."
  (declare (ignore font size entry))
  (let* ((hdc (%pp-hdc port))
         (text (if (find #\Tab string) (substitute #\Space #\Tab string) string)))
    (%pp-select-font port)
    (%gdi-set-text-color hdc (%pp-colorref *current-fg-color*))
    (%gdi-set-background-mode hdc +gdi-transparent+)
    (%gdi-set-text-align hdc +gdi-align-baseline-left+)
    (cffi:with-foreign-string ((wide bytes) text :encoding :utf-16le)
      ;; the count is in UTF-16 code units, which is what the buffer holds less
      ;; its terminator -- not the number of characters
      (%gdi-text-out hdc (%pp-x port x) (%pp-y port y) 0 (cffi:null-pointer)
                     wide (max 0 (1- (floor bytes 2))) (cffi:null-pointer))))
  nil)

(defmethod port-draw-rect ((port printer-port) x y w h pensize)
  (multiple-value-bind (left top wi he) (normalize-rectangle x y w h)
    (let ((*current-pen-size* pensize))
      (%pp-select-pen port)
      (%pp-select-null-brush port)
      (%gdi-rectangle (%pp-hdc port)
                      (%pp-x port left) (%pp-y port top)
                      (%pp-x port (+ left wi)) (%pp-y port (+ top he)))))
  nil)

(defmethod port-fill-rect ((port printer-port) x y w h)
  (multiple-value-bind (left top wi he) (normalize-rectangle x y w h)
    (cffi:with-foreign-object (rectangle '(:struct gdi-rect))
      (setf (cffi:foreign-slot-value rectangle '(:struct gdi-rect) 'left)
            (%pp-x port left)
            (cffi:foreign-slot-value rectangle '(:struct gdi-rect) 'top)
            (%pp-y port top)
            (cffi:foreign-slot-value rectangle '(:struct gdi-rect) 'right)
            (%pp-x port (+ left wi))
            (cffi:foreign-slot-value rectangle '(:struct gdi-rect) 'bottom)
            (%pp-y port (+ top he)))
      (%gdi-fill-rect (%pp-hdc port) rectangle
                      (%pp-brush port *current-fg-color*))))
  nil)

(defmethod port-draw-ellipse ((port printer-port) x y rx ry filled)
  "The ellipse centred on (X Y) with radii RX and RY (om-LW/graphics.lisp:
om-draw-ellipse).  A filled one carries no outline, as :FILLED T means there."
  (if filled
      (progn (%pp-select-brush port *current-fg-color*)
             (%pp-select-null-pen port))
      (progn (%pp-select-pen port)
             (%pp-select-null-brush port)))
  (%gdi-ellipse (%pp-hdc port)
                (%pp-x port (- x rx)) (%pp-y port (- y ry))
                (%pp-x port (+ x rx)) (%pp-y port (+ y ry)))
  nil)

(defmethod port-draw-ellipse-arc ((port printer-port) x y width height start-angle sweep-angle)
  "The arc inscribed in the port box (X Y WIDTH HEIGHT).

The angles run counter-clockwise from the positive x axis AS DRAWN, so a growing
angle rises on the page and the row number falls -- which is why the radial points
take the sine downwards.  GDI draws from the start radial to the end one in that
same direction, so a negative sweep is the same arc with its two radials exchanged."
  (let* ((rx (/ (max 1 width) 2))
         (ry (/ (max 1 height) 2))
         (cx (+ x rx))
         (cy (+ y ry))
         (a0 (float start-angle 0d0))
         (a1 (+ a0 (float sweep-angle 0d0)))
         (from (min a0 a1))
         (to (max a0 a1)))
    (%pp-select-pen port)
    (%gdi-arc (%pp-hdc port)
              (%pp-x port (- cx rx)) (%pp-y port (- cy ry))
              (%pp-x port (+ cx rx)) (%pp-y port (+ cy ry))
              (%pp-x port (+ cx (* rx (cos from))))
              (%pp-y port (- cy (* ry (sin from))))
              (%pp-x port (+ cx (* rx (cos to))))
              (%pp-y port (- cy (* ry (sin to))))))
  nil)

(defmethod port-draw-polygon ((port printer-port) points)
  ;; POINTS are (x y) lists: the caller normalized them
  (let ((count (length points)))
    (when (plusp count)
      (%pp-select-pen port)
      (%pp-select-null-brush port)
      (cffi:with-foreign-object (array '(:struct gdi-point) count)
        (loop for (px py) in points
              for index from 0
              for slot = (cffi:mem-aptr array '(:struct gdi-point) index)
              do (setf (cffi:foreign-slot-value slot '(:struct gdi-point) 'x)
                       (%pp-x port px)
                       (cffi:foreign-slot-value slot '(:struct gdi-point) 'y)
                       (%pp-y port py)))
        ;; CAPI draws the polygon closed and unfilled, which with a null brush is
        ;; exactly what Polygon does
        (%gdi-polygon (%pp-hdc port) array count))))
  nil)

(defun %pict-to-device-bits (data width height bottom-up alpha bits)
  "Copy DATA into BITS as the top-down 32-bit BGRX bitmap GDI is handed.
The device carries no alpha channel, so the picture is flattened against white,
exactly as the raster backend does (paper-print/win32.lisp: %page-to-device-bits)."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum width height)
           (optimize (speed 3) (safety 1)))
  (let ((factor (float alpha 1f0)))
    (dotimes (row height)
      (let ((source (the fixnum (* (if bottom-up (- height row 1) row) width 4)))
            (destination (the fixnum (* row width 4))))
        (dotimes (column width)
          (declare (ignorable column))
          (let ((a (max 0 (min 255 (round (* (aref data (+ source 3)) factor))))))
            (flet ((flatten (channel)
                     (truncate (+ (* channel a) (* 255 (- 255 a)) 127) 255)))
              (setf (cffi:mem-aref bits :uint8 destination)
                    (flatten (aref data (+ source 2)))
                    (cffi:mem-aref bits :uint8 (+ destination 1))
                    (flatten (aref data (+ source 1)))
                    (cffi:mem-aref bits :uint8 (+ destination 2))
                    (flatten (aref data source))
                    (cffi:mem-aref bits :uint8 (+ destination 3)) 0)))
          (incf source 4)
          (incf destination 4))))))

(defmethod port-draw-image ((port printer-port) pict x y w h src-x src-y
                            src-width src-height alpha highlight)
  "Blit PICT onto the printer device.
The HIGHLIGHT tint drawn on screen is a selection affordance and has no place in a
document, so it is not reproduced here."
  (declare (ignore highlight))
  (let* ((image (picture-image pict))
         (data (org.shirakumo.fraf.imagine:data image))
         (pw (picture-width pict))
         (ph (picture-height pict))
         (bottom-up (eq (org.shirakumo.fraf.imagine:origin image) :bottom-left)))
    (cffi:with-foreign-object (bits :uint8 (* pw ph 4))
      (%pict-to-device-bits data pw ph bottom-up (or alpha 1.0) bits)
      (cffi:with-foreign-object (info '(:struct gdi-bitmapinfoheader))
        (dotimes (index (cffi:foreign-type-size '(:struct gdi-bitmapinfoheader)))
          (setf (cffi:mem-aref info :uint8 index) 0))
        (setf (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'size)
              (cffi:foreign-type-size '(:struct gdi-bitmapinfoheader))
              (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'width) pw
              ;; negative height: the rows above are in top-down order
              (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'height)
              (- ph)
              (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'planes) 1
              (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'bit-count) 32
              (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'compression) 0
              (cffi:foreign-slot-value info '(:struct gdi-bitmapinfoheader) 'image-size)
              (* pw ph 4))
        (%gdi-set-stretch-mode (%pp-hdc port) +gdi-halftone+)
        (%gdi-set-brush-origin (%pp-hdc port) 0 0 (cffi:null-pointer))
        (%gdi-stretch-dibits (%pp-hdc port)
                             (%pp-x port x) (%pp-y port y)
                             (%pp-width port w) (%pp-height port h)
                             (round src-x)
                             (round src-y)
                             (if src-width (round src-width) pw)
                             (if src-height (round src-height) ph)
                             bits info +gdi-dib-rgb-colors+ +gdi-source-copy+))))
  nil)

;;; ------------------------------------------------------------ port lifetime

(defun %close-printer-port (port)
  "Put the device's stock objects back and delete everything the port created.
Selecting the stock objects first is what makes the deletions safe: GDI refuses
DeleteObject on an object still selected into a device context."
  (let ((hdc (%pp-hdc port)))
    (%gdi-select hdc (%gdi-stock-object +gdi-stock-black-pen+))
    (%gdi-select hdc (%gdi-stock-object +gdi-stock-white-brush+))
    (%gdi-select hdc (%gdi-stock-object +gdi-stock-system-font+))
    (flet ((discard (table)
             (maphash (lambda (key object)
                        (declare (ignore key))
                        (%gdi-delete-object object))
                      table)
             (clrhash table)))
      (discard (%pp-pens port))
      (discard (%pp-brushes port))
      (discard (%pp-fonts port)))
    (setf (%pp-selected-pen port) nil
          (%pp-selected-brush port) nil
          (%pp-selected-font port) nil))
  nil)

(defun call-with-printer-port (hdc origin-x origin-y dpi-x dpi-y logical-dpi function
                               &key font-function)
  "Call FUNCTION with a printer port on the device context HDC and answer its values.

The page's port units map to device pixels at DPI-X / LOGICAL-DPI and DPI-Y /
LOGICAL-DPI from the device point ORIGIN-X ORIGIN-Y -- the top left of what the
device can reach, which is where a page is drawn.  FONT-FUNCTION answers the text
face the port draws with (%PP-FONT).  *CURRENT-PORT* is bound to the port only
around FUNCTION, so a frame drawn between pages still goes to the screen, and the
drawing state is bound to its defaults as a fresh graphics port carries them, so a
page never inherits the colour or pen of whatever drew last.  Every GDI object the
port creates is deleted when FUNCTION returns."
  (let ((port (%make-printer-port :hdc hdc :origin-x origin-x :origin-y origin-y
                                  :scale-x (/ dpi-x logical-dpi)
                                  :scale-y (/ dpi-y logical-dpi)
                                  :font-function font-function)))
    (unwind-protect
         (let ((*current-port* port)
               (*pox* 0)
               (*poy* 0)
               (*current-fg-color* nil)
               (*current-pen-size* 1)
               (*current-line-style* :solid)
               (*current-dash-pattern* nil))
           (funcall function port))
      (%close-printer-port port))))
