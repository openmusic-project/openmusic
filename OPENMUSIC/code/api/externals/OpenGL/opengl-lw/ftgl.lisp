;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/ftgl.lisp,v 1.2.1.1 2014/05/27 20:56:56 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

;; An FTGL interface.

(cl:defpackage "FTGL"
  (:export
   #:ft-error
   #:ft-glyph-slot
   #:ft-encoding
   #:FTGL_RENDER_FRONT
   #:FTGL_RENDER_BACK
   #:FTGL_RENDER_SIDE
   #:FTGL_RENDER_ALL
   #:ftgl-double
   #:ftgl-float
   #:ftglglyph
   #:ftgl-create-custom-glyph
   #:ftgl-destroy-glyph
   #:ftgl-render-glyph
   #:ftgl-get-glyph-advance
   #:ftgl-get-glyph-bbox
   #:ftgl-get-glyph-error
   #:ftgl-create-bitmap-glyph
   #:ftgl-create-extrude-glyph
   #:ftgl-create-outline-glyph
   #:ftgl-create-pixmap-glyph
   #:ftgl-create-polygon-glyph
   #:ftgl-create-texture-glyph
   #:ftglfont
   #:ftgl-create-custom-font
   #:ftgl-destroy-font
   #:ftgl-attach-file
   #:ftgl-attach-data
   #:ftgl-set-font-char-map
   #:ftgl-get-font-char-map-count
   #:ftgl-get-font-char-map-list
   #:ftgl-set-font-face-size
   #:ftgl-get-font-face-size
   #:ftgl-set-font-depth
   #:ftgl-set-font-outset
   #:ftgl-set-font-display-list
   #:ftgl-get-font-ascender
   #:ftgl-get-font-descender
   #:ftgl-get-font-line-height
   #:ftgl-get-font-bbox
   #:ftgl-get-font-advance
   #:ftgl-render-font
   #:ftgl-get-font-error
   #:ftgl-create-bitmap-font
   #:ftgl-create-buffer-font
   #:ftgl-create-extrude-font
   #:ftgl-create-outline-font
   #:ftgl-create-pixmap-font
   #:ftgl-create-polygon-font
   #:ftgl-create-texture-font
   #:ftgllayout
   #:ftgl-destroy-layout
   #:ftgl-get-layout-bbox
   #:ftgl-render-layout
   #:ftgl-get-layout-error
   #:ftgl-create-simple-layout
   #:ftgl-set-layout-font
   #:ftgl-get-layout-font
   #:ftgl-set-layout-line-length
   #:ftgl-get-layout-line-length
   #:ftgl-set-layout-alignment
   #:ftgl-get-layout-alignement
   #:ftgl-set-layout-line-spacing
   #:ftgl-get-layout-line-spacing
   )
)

(in-package "FTGL")

;; Misc FreeType definitions.
(fli:define-c-typedef (ft-error (:foreign-name "FT_Error")) :int)
(fli:define-c-struct (ft-glyph-slot-rec- (:foreign-name "FT_GlyphSlotRec_")
                                         (:forward-reference t)))
(fli:define-c-typedef (ft-glyph-slot (:foreign-name "FT_GlyphSlot"))
  (:pointer (:struct ft-glyph-slot-rec-)))
(fli:define-c-enum (ft-encoding- (:foreign-name "FT_Encoding_"))
                   (ft-encoding-none 0)
                   (ft-encoding-ms-symbol 1937337698)
                   (ft-encoding-unicode 1970170211)
                   (ft-encoding-sjis 1936353651)
                   (ft-encoding-gb2312 1734484000)
                   (ft-encoding-big5 1651074869)
                   (ft-encoding-wansung 2002873971)
                   (ft-encoding-johab 1785686113)
                   (ft-encoding-ms-sjis 1936353651)
                   (ft-encoding-ms-gb2312 1734484000)
                   (ft-encoding-ms-big5 1651074869)
                   (ft-encoding-ms-wansung 2002873971)
                   (ft-encoding-ms-johab 1785686113)
                   (ft-encoding-adobe-standard 1094995778)
                   (ft-encoding-adobe-expert 1094992453)
                   (ft-encoding-adobe-custom 1094992451)
                   (ft-encoding-adobe-latin-1 1818326065)
                   (ft-encoding-old-latin-2 1818326066)
                   (ft-encoding-apple-roman 1634889070))
(fli:define-c-typedef (ft-encoding (:foreign-name "FT_Encoding"))
  (:enum ft-encoding-))



(fli:define-c-typedef ftgl-string-in (:reference-pass :ef-mb-string))



(defconstant FTGL_RENDER_FRONT #x0001)
(defconstant FTGL_RENDER_BACK  #x0002)
(defconstant FTGL_RENDER_SIDE  #x0004)
(defconstant FTGL_RENDER_ALL   #xffff)



(fli:define-c-typedef (ftgl-double (:foreign-name "FTGL_DOUBLE")) :double)
(fli:define-c-typedef (ftgl-float (:foreign-name "FTGL_FLOAT")) :float)
(fli:define-c-struct (-ftglglyph
                      (:foreign-name "_FTGLGlyph")
                      (:forward-reference t)))
(fli:define-c-typedef (ftglglyph (:foreign-name "FTGLglyph"))
                      (:struct -ftglglyph))
(fli:define-foreign-function (ftgl-create-custom-glyph
                              "ftglCreateCustomGlyph"
                              :source)
                             ((base (:pointer ftglglyph))
                              (data (:pointer :void))
                              (render-callback
                               (:pointer
                                (:function
                                 ((:pointer ftglglyph)
                                  (:pointer :void)
                                  ftgl-double
                                  ftgl-double
                                  :int
                                  (:pointer ftgl-double)
                                  (:pointer ftgl-double))
                                 :void)))
                              (destroy-callback
                               (:pointer
                                (:function
                                 ((:pointer ftglglyph) (:pointer :void))
                                 :void))))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-destroy-glyph "ftglDestroyGlyph" :source)
                             ((glyph (:pointer ftglglyph)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-render-glyph "ftglRenderGlyph" :source)
                             ((glyph (:pointer ftglglyph))
                              (penx ftgl-double)
                              (peny ftgl-double)
                              (render-mode :int)
                              (advancex (:pointer ftgl-double))
                              (advancey (:pointer ftgl-double)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-glyph-advance
                              "ftglGetGlyphAdvance"
                              :source)
                             ((glyph (:pointer ftglglyph)))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-glyph-bbox "ftglGetGlyphBBox" :source)
                             ((glyph (:pointer ftglglyph))
                              (bounds (:c-array :float 6)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-glyph-error
                              "ftglGetGlyphError"
                              :source)
                             ((glyph (:pointer ftglglyph)))
                             :result-type
                             ft-error
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-bitmap-glyph
                              "ftglCreateBitmapGlyph"
                              :source)
                             ((glyph ft-glyph-slot))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-extrude-glyph
                              "ftglCreateExtrudeGlyph"
                              :source)
                             ((glyph ft-glyph-slot)
                              (depth :float)
                              (front-outset :float)
                              (back-outset :float)
                              (use-display-list :int))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-outline-glyph
                              "ftglCreateOutlineGlyph"
                              :source)
                             ((glyph ft-glyph-slot)
                              (outset :float)
                              (use-display-list :int))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-pixmap-glyph
                              "ftglCreatePixmapGlyph"
                              :source)
                             ((glyph ft-glyph-slot))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-polygon-glyph
                              "ftglCreatePolygonGlyph"
                              :source)
                             ((glyph ft-glyph-slot)
                              (outset :float)
                              (use-display-list :int))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-texture-glyph
                              "ftglCreateTextureGlyph"
                              :source)
                             ((glyph ft-glyph-slot)
                              (id :int)
                              (x-offset :int)
                              (y-offset :int)
                              (width :int)
                              (height :int))
                             :result-type
                             (:pointer ftglglyph)
                             :language
                             :ansi-c)
(fli:define-c-struct (-ftglfont
                      (:foreign-name "_FTGLFont")
                      (:forward-reference t)))
(fli:define-c-typedef (ftglfont (:foreign-name "FTGLfont"))
                      (:struct -ftglfont))
(fli:define-foreign-function (ftgl-create-custom-font
                              "ftglCreateCustomFont"
                              :source)
                             ((font-file-path ftgl-string-in)
                              (data (:pointer :void))
                              (makeglyph-callback
                               (:pointer
                                (:function
                                 (ft-glyph-slot (:pointer :void))
                                 (:pointer ftglglyph)))))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-destroy-font "ftglDestroyFont" :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-attach-file "ftglAttachFile" :source)
                             ((font (:pointer ftglfont))
                              (path ftgl-string-in))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-attach-data "ftglAttachData" :source)
                             ((font (:pointer ftglfont))
                              (data (:pointer (:const (:unsigned :char))))
                              (size :size-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-font-char-map
                              "ftglSetFontCharMap"
                              :source)
                             ((font (:pointer ftglfont))
                              (encoding ft-encoding))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-char-map-count
                              "ftglGetFontCharMapCount"
                              :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             (:unsigned :int)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-char-map-list
                              "ftglGetFontCharMapList"
                              :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             (:pointer ft-encoding)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-font-face-size
                              "ftglSetFontFaceSize"
                              :source)
                             ((font (:pointer ftglfont))
                              (size (:unsigned :int))
                              (res (:unsigned :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-face-size
                              "ftglGetFontFaceSize"
                              :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             (:unsigned :int)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-font-depth "ftglSetFontDepth" :source)
                             ((font (:pointer ftglfont)) (depth :float))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-font-outset
                              "ftglSetFontOutset"
                              :source)
                             ((font (:pointer ftglfont))
                              (front :float)
                              (back :float))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-font-display-list
                              "ftglSetFontDisplayList"
                              :source)
                             ((font (:pointer ftglfont)) (use-list :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-ascender
                              "ftglGetFontAscender"
                              :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-descender
                              "ftglGetFontDescender"
                              :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-line-height
                              "ftglGetFontLineHeight"
                              :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-bbox "ftglGetFontBBox" :source)
                             ((font (:pointer ftglfont))
                              (string ftgl-string-in)
                              (len :int)
                              (bounds (:c-array :float 6)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-advance
                              "ftglGetFontAdvance"
                              :source)
                             ((font (:pointer ftglfont))
                              (string ftgl-string-in))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-render-font "ftglRenderFont" :source)
                             ((font (:pointer ftglfont))
                              (string ftgl-string-in)
                              (mode :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-font-error "ftglGetFontError" :source)
                             ((font (:pointer ftglfont)))
                             :result-type
                             ft-error
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-bitmap-font
                              "ftglCreateBitmapFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-buffer-font
                              "ftglCreateBufferFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-extrude-font
                              "ftglCreateExtrudeFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-outline-font
                              "ftglCreateOutlineFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-pixmap-font
                              "ftglCreatePixmapFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-polygon-font
                              "ftglCreatePolygonFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-texture-font
                              "ftglCreateTextureFont"
                              :source)
                             ((file ftgl-string-in))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-c-struct (-ftgllayout
                      (:foreign-name "_FTGLlayout")
                      (:forward-reference t)))
(fli:define-c-typedef (ftgllayout (:foreign-name "FTGLlayout"))
                      (:struct -ftgllayout))
(fli:define-foreign-function (ftgl-destroy-layout
                              "ftglDestroyLayout"
                              :source)
                             ((layout (:pointer ftgllayout)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-layout-bbox
                              "ftglGetLayoutBBox"
                              :source)
                             ((layout (:pointer ftgllayout))
                              (string ftgl-string-in)
                              (bounds (:c-array :float 6)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-render-layout "ftglRenderLayout" :source)
                             ((layout (:pointer ftgllayout))
                              (string ftgl-string-in)
                              (mode :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-layout-error
                              "ftglGetLayoutError"
                              :source)
                             ((layout (:pointer ftgllayout)))
                             :result-type
                             ft-error
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-create-simple-layout
                              "ftglCreateSimpleLayout"
                              :source)
                             nil
                             :result-type
                             (:pointer ftgllayout)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-layout-font
                              "ftglSetLayoutFont"
                              :source)
                             ((arg-1 (:pointer ftgllayout))
                              (arg-2 (:pointer ftglfont)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-layout-font
                              "ftglGetLayoutFont"
                              :source)
                             ((arg-1 (:pointer ftgllayout)))
                             :result-type
                             (:pointer ftglfont)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-layout-line-length
                              "ftglSetLayoutLineLength"
                              :source)
                             ((arg-1 (:pointer ftgllayout))
                              (arg-2 (:const :float)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-layout-line-length
                              "ftglGetLayoutLineLength"
                              :source)
                             ((arg-1 (:pointer ftgllayout)))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-layout-alignment
                              "ftglSetLayoutAlignment"
                              :source)
                             ((arg-1 (:pointer ftgllayout))
                              (arg-2 (:const :int)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-layout-alignement
                              "ftglGetLayoutAlignement"
                              :source)
                             ((arg-1 (:pointer ftgllayout)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-set-layout-line-spacing
                              "ftglSetLayoutLineSpacing"
                              :source)
                             ((arg-1 (:pointer ftgllayout))
                              (arg-2 (:const :float)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftgl-get-layout-line-spacing
                              "ftglGetLayoutLineSpacing"
                              :source)
                             ((arg-1 (:pointer ftgllayout)))
                             :result-type
                             :float
                             :language
                             :ansi-c)


(push :ftgl *features*)

(fli:register-module "-lftgl")

