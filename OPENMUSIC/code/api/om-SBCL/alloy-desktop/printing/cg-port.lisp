(in-package :alloy-desktop)

(defconstant +cf-string-encoding-utf8+ #x08000100)

(defstruct (printer-port (:conc-name %cp-) (:constructor %make-printer-port))
  context origin-x origin-y scale-x scale-y font-function fonts)

(defun %cp-double (value) (coerce value 'double-float))

(defun %cp-x (port x)
  (%cp-double (+ (%cp-origin-x port) (* (%cp-scale-x port) (+ x *pox*)))))

(defun %cp-y (port y)
  (%cp-double (+ (%cp-origin-y port) (* (%cp-scale-y port) (+ y *poy*)))))

(defun %cp-width (port value)
  (%cp-double (* (%cp-scale-x port) value)))

(defun %cp-components (color)
  "COLOR as red, green and blue doubles; NIL is black."
  (let ((c (or color colors:black)))
    (values (%cp-double (chroma:r c))
            (%cp-double (chroma:g c))
            (%cp-double (chroma:b c)))))

(defun %cp-set-stroke (port)
  (let ((context (%cp-context port)))
    (multiple-value-bind (red green blue) (%cp-components *current-fg-color*)
      (cffi:foreign-funcall "CGContextSetRGBStrokeColor" :pointer context
                            :double red :double green :double blue :double 1d0 :void))
    (cffi:foreign-funcall "CGContextSetLineWidth" :pointer context
                          :double (%cp-width port *current-pen-size*) :void)
    (if (eq *current-line-style* :dashed)
        (let ((pattern (or *current-dash-pattern* '(4 4))))
          (cffi:with-foreign-object (lengths :double (length pattern))
            (loop for value in pattern
                  for index from 0
                  do (setf (cffi:mem-aref lengths :double index)
                           (%cp-width port value)))
            (cffi:foreign-funcall "CGContextSetLineDash" :pointer context
                                  :double 0d0 :pointer lengths
                                  :long (length pattern) :void)))
        (cffi:foreign-funcall "CGContextSetLineDash" :pointer context
                              :double 0d0 :pointer (cffi:null-pointer)
                              :long 0 :void))))

(defun %cp-set-fill (port)
  (multiple-value-bind (red green blue) (%cp-components *current-fg-color*)
    (cffi:foreign-funcall "CGContextSetRGBFillColor" :pointer (%cp-context port)
                          :double red :double green :double blue :double 1d0 :void)))

(defun %cp-move-to (port x y)
  (cffi:foreign-funcall "CGContextMoveToPoint" :pointer (%cp-context port)
                        :double (%cp-x port x) :double (%cp-y port y) :void))

(defun %cp-line-to (port x y)
  (cffi:foreign-funcall "CGContextAddLineToPoint" :pointer (%cp-context port)
                        :double (%cp-x port x) :double (%cp-y port y) :void))

(defun %cp-begin-path (port)
  (cffi:foreign-funcall "CGContextBeginPath" :pointer (%cp-context port) :void))

(defun %cp-stroke (port)
  (cffi:foreign-funcall "CGContextStrokePath" :pointer (%cp-context port) :void))

(defun %cp-fill (port)
  (cffi:foreign-funcall "CGContextFillPath" :pointer (%cp-context port) :void))

(defun %cp-close-path (port)
  (cffi:foreign-funcall "CGContextClosePath" :pointer (%cp-context port) :void))

(defun %cp-save (port)
  (cffi:foreign-funcall "CGContextSaveGState" :pointer (%cp-context port) :void))

(defun %cp-restore (port)
  (cffi:foreign-funcall "CGContextRestoreGState" :pointer (%cp-context port) :void))

(defun %cp-translate (port x y)
  (cffi:foreign-funcall "CGContextTranslateCTM" :pointer (%cp-context port)
                        :double x :double y :void))

(defun %cp-scale (port x y)
  (cffi:foreign-funcall "CGContextScaleCTM" :pointer (%cp-context port)
                        :double x :double y :void))

(defun %cp-rectangle-path (port left top width height)
  (%cp-begin-path port)
  (%cp-move-to port left top)
  (%cp-line-to port (+ left width) top)
  (%cp-line-to port (+ left width) (+ top height))
  (%cp-line-to port left (+ top height))
  (%cp-close-path port))

;;; ------------------------------------------------------------------- fonts

(defun %cp-cfstring (string)
  (cffi:foreign-funcall "CFStringCreateWithCString" :pointer (cffi:null-pointer)
                        :string string :unsigned-int +cf-string-encoding-utf8+
                        :pointer))

(defun %cp-font-file (family)
  (loop for (nil atlas-family nil nil kind . files) in *font-atlases*
        when (string-equal family atlas-family)
          do (let ((file (find-font-source kind files)))
               (when file (return file)))))

(defun %cp-make-font (family size bold italic)
  (let* ((family (or family "Helvetica"))
         (file (%cp-font-file family))
         (points (%cp-double (max 1 size)))
         (font nil))
    (when file
      (let ((provider (cffi:foreign-funcall "CGDataProviderCreateWithFilename"
                                            :string (uiop:native-namestring file)
                                            :pointer)))
        (unless (cffi:null-pointer-p provider)
          (let ((graphics-font (cffi:foreign-funcall "CGFontCreateWithDataProvider"
                                                     :pointer provider :pointer)))
            (cffi:foreign-funcall "CFRelease" :pointer provider :void)
            (unless (cffi:null-pointer-p graphics-font)
              (setf font (cffi:foreign-funcall "CTFontCreateWithGraphicsFont"
                                               :pointer graphics-font :double points
                                               :pointer (cffi:null-pointer)
                                               :pointer (cffi:null-pointer) :pointer))
              (cffi:foreign-funcall "CFRelease" :pointer graphics-font :void))))))
    (unless font
      (let ((name (%cp-cfstring family)))
        (setf font (cffi:foreign-funcall "CTFontCreateWithName" :pointer name
                                         :double points :pointer (cffi:null-pointer)
                                         :pointer))
        (cffi:foreign-funcall "CFRelease" :pointer name :void)))
    (when (and font (or bold italic))
      (let* ((traits (logior (if bold 2 0) (if italic 1 0)))
             (styled (cffi:foreign-funcall "CTFontCreateCopyWithSymbolicTraits"
                                           :pointer font :double 0d0
                                           :pointer (cffi:null-pointer)
                                           :uint32 traits :uint32 (logior 2 1)
                                           :pointer)))
        (unless (cffi:null-pointer-p styled)
          (cffi:foreign-funcall "CFRelease" :pointer font :void)
          (setf font styled))))
    font))

(defun %cp-font (port)
  "The CTFont the port draws text with, from FONT-FUNCTION, cached per face."
  (multiple-value-bind (family size bold italic)
      (if (%cp-font-function port)
          (funcall (%cp-font-function port))
          (values "Helvetica" 12 nil nil))
    (let* ((size (* (%cp-scale-y port) (or size 12)))
           (key (list (or family "Helvetica") size (and bold t) (and italic t))))
      (or (gethash key (%cp-fonts port))
          (setf (gethash key (%cp-fonts port))
                (%cp-make-font family size bold italic))))))

;;; ------------------------------------------------------------------ drawing

(defmethod port-draw-line ((port printer-port) x1 y1 x2 y2)
  (%cp-set-stroke port)
  (%cp-begin-path port)
  (%cp-move-to port x1 y1)
  (%cp-line-to port x2 y2)
  (%cp-stroke port)
  nil)

(defmethod port-draw-line-segments ((port printer-port) segments)
  (dolist (segment segments) (apply #'port-draw-line port segment)))

(defmethod port-draw-text ((port printer-port) x y string font size entry)
  "Draw STRING with its baseline at Y."
  (declare (ignore font size entry))
  (let ((context (%cp-context port))
        (text (if (find #\Tab string) (substitute #\Space #\Tab string) string))
        (ct-font (%cp-font port)))
    (when (and ct-font (plusp (length text)))
      (%cp-set-fill port)
      (let ((count (length text)))
        (cffi:with-foreign-objects ((characters :uint16 count)
                                    (glyphs :uint16 count)
                                    (positions :double (* 2 count))
                                    (advances :double (* 2 count)))
          (dotimes (index count)
            (setf (cffi:mem-aref characters :uint16 index)
                  (char-code (char text index))))
          (cffi:foreign-funcall "CTFontGetGlyphsForCharacters" :pointer ct-font
                                :pointer characters :pointer glyphs :long count :char)
          (cffi:foreign-funcall "CTFontGetAdvancesForGlyphs" :pointer ct-font
                                :uint32 0 :pointer glyphs :pointer advances
                                :long count :double)
          (let ((pen 0d0))
            (dotimes (index count)
              (setf (cffi:mem-aref positions :double (* 2 index)) pen
                    (cffi:mem-aref positions :double (1+ (* 2 index))) 0d0)
              (incf pen (cffi:mem-aref advances :double (* 2 index)))))
          (%cp-save port)
          (%cp-translate port (%cp-x port x) (%cp-y port y))
          (%cp-scale port 1d0 -1d0)
          (cffi:foreign-funcall "CTFontDrawGlyphs" :pointer ct-font :pointer glyphs
                                :pointer positions :long count :pointer context :void)
          (%cp-restore port)))))
  nil)

(defmethod port-draw-rect ((port printer-port) x y w h pensize)
  (multiple-value-bind (left top width height) (normalize-rectangle x y w h)
    (let ((*current-pen-size* pensize))
      (%cp-set-stroke port)
      (%cp-rectangle-path port left top width height)
      (%cp-stroke port)))
  nil)

(defmethod port-fill-rect ((port printer-port) x y w h)
  (multiple-value-bind (left top width height) (normalize-rectangle x y w h)
    (%cp-set-fill port)
    (%cp-rectangle-path port left top width height)
    (%cp-fill port))
  nil)

(defmethod port-draw-ellipse ((port printer-port) x y rx ry filled)
  "The ellipse centred on (X Y) with radii RX and RY."
  (let ((radius-x (max (%cp-width port rx) 0.001d0))
        (radius-y (max (%cp-double (* (%cp-scale-y port) ry)) 0.001d0)))
    (if filled (%cp-set-fill port) (%cp-set-stroke port))
    (%cp-save port)
    (%cp-translate port (%cp-x port x) (%cp-y port y))
    (%cp-scale port radius-x radius-y)
    (%cp-begin-path port)
    (cffi:foreign-funcall "CGContextAddArc" :pointer (%cp-context port)
                          :double 0d0 :double 0d0 :double 1d0
                          :double 0d0 :double (%cp-double (* 2 pi))
                          :int 0 :void)
    (%cp-restore port)
    (if filled (%cp-fill port) (%cp-stroke port)))
  nil)

(defmethod port-draw-ellipse-arc ((port printer-port) x y width height start-angle sweep-angle)
  "The arc inscribed in the port box (X Y WIDTH HEIGHT)."
  (let* ((rx (/ (max 1 width) 2))
         (ry (/ (max 1 height) 2))
         (cx (+ x rx))
         (cy (+ y ry))
         (a0 (%cp-double start-angle))
         (a1 (+ a0 (%cp-double sweep-angle))))
    (%cp-set-stroke port)
    (%cp-save port)
    (%cp-translate port (%cp-x port cx) (%cp-y port cy))
    (%cp-scale port (max (%cp-width port rx) 0.001d0)
               (max (%cp-double (* (%cp-scale-y port) ry)) 0.001d0))
    (%cp-begin-path port)
    (cffi:foreign-funcall "CGContextAddArc" :pointer (%cp-context port)
                          :double 0d0 :double 0d0 :double 1d0
                          :double (- a0) :double (- a1)
                          :int (if (< a1 a0) 0 1) :void)
    (%cp-restore port)
    (%cp-stroke port))
  nil)

(defmethod port-draw-polygon ((port printer-port) points)
  (when points
    (%cp-set-stroke port)
    (%cp-begin-path port)
    (destructuring-bind ((first-x first-y) . rest) points
      (%cp-move-to port first-x first-y)
      (loop for (px py) in rest do (%cp-line-to port px py)))
    (%cp-close-path port)
    (%cp-stroke port))
  nil)

(defmethod port-draw-image ((port printer-port) pict x y w h src-x src-y
                            src-width src-height alpha highlight)
  (declare (ignore highlight))
  (let* ((image (picture-image pict))
         (width (picture-width pict))
         (height (picture-height pict))
         (page (paper-print:pdf-page-from-rgba
                (org.shirakumo.fraf.imagine:data image) width height
                (eq :top-left (org.shirakumo.fraf.imagine:origin image))))
         (page-ref (and page (paper-print:pdf-page-reference page)))
         (target-width (or w width))
         (target-height (or h height))
         (source-width (or src-width width))
         (source-height (or src-height height)))
    (when page-ref
      (%cp-save port)
      (%cp-rectangle-path port x y target-width target-height)
      (cffi:foreign-funcall "CGContextClip" :pointer (%cp-context port) :void)
      (cffi:foreign-funcall "CGContextSetAlpha" :pointer (%cp-context port)
                            :double (%cp-double (or alpha 1)) :void)
      (%cp-translate port (%cp-x port x) (%cp-y port y))
      (%cp-scale port (%cp-double (* (%cp-scale-x port) (/ target-width source-width)))
                 (%cp-double (* (%cp-scale-y port) (/ target-height source-height))))
      (%cp-translate port (%cp-double (- (or src-x 0)))
                     (%cp-double (- height (or src-y 0))))
      (%cp-scale port 1d0 -1d0)
      (cffi:foreign-funcall "CGContextDrawPDFPage" :pointer (%cp-context port)
                            :pointer page-ref :void)
      (%cp-restore port)))
  nil)

;;; ------------------------------------------------------------------- the port

(defun %close-printer-port (port)
  "Release every font the port made."
  (maphash (lambda (key font)
             (declare (ignore key))
             (unless (cffi:null-pointer-p font)
               (cffi:foreign-funcall "CFRelease" :pointer font :void)))
           (%cp-fonts port))
  (clrhash (%cp-fonts port))
  nil)

(defun call-with-printer-port (device origin-x origin-y dpi-x dpi-y logical-dpi function
                               &key font-function)
  "Call FUNCTION with a printer port drawing into the PDF context DEVICE."
  (let ((port (%make-printer-port :context device
                                  :origin-x origin-x :origin-y origin-y
                                  :scale-x (/ dpi-x logical-dpi)
                                  :scale-y (/ dpi-y logical-dpi)
                                  :font-function font-function
                                  :fonts (make-hash-table :test 'equal))))
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
