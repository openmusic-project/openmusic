;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/win32.lisp,v 1.8.3.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "WIN32")

;;; From WINGDI.H

;;; /* pixel types */
(defconstant *PFD-TYPE-RGBA*        0)
(defconstant *PFD-TYPE-COLORINDEX*  1)

;;; /* layer types */
(defconstant *PFD-MAIN-PLANE*       0)
(defconstant *PFD-OVERLAY-PLANE*    1)
(defconstant *PFD-UNDERLAY-PLANE*   -1)

;;; /* PIXELFORMATDESCRIPTOR flags */
(defconstant *PFD-DOUBLEBUFFER*            #x00000001)
(defconstant *PFD-STEREO*                  #x00000002)
(defconstant *PFD-DRAW-TO-WINDOW*          #x00000004)
(defconstant *PFD-DRAW-TO-BITMAP*          #x00000008)
(defconstant *PFD-SUPPORT-GDI*             #x00000010)
(defconstant *PFD-SUPPORT-OPENGL*          #x00000020)
(defconstant *PFD-GENERIC-FORMAT*          #x00000040)
(defconstant *PFD-NEED-PALETTE*            #x00000080)
(defconstant *PFD-NEED-SYSTEM-PALETTE*     #x00000100)
(defconstant *PFD-SWAP-EXCHANGE*           #x00000200)
(defconstant *PFD-SWAP-COPY*               #x00000400)

;;; /* PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only */
(defconstant *PFD-DOUBLEBUFFER-DONTCARE*   #x40000000)
(defconstant *PFD-STEREO-DONTCARE*         #x80000000)

(fli:define-c-struct (tag-pixel-format-descriptor (:foreign-name "tagPIXELFORMATDESCRIPTOR"))
  (nSize  WORD) 
  (nVersion  WORD) 
  (dwFlags DWORD) 
  (iPixelType  BYTE) 
  (cColorBits  BYTE) 
  (cRedBits  BYTE) 
  (cRedShift  BYTE) 
  (cGreenBits  BYTE) 
  (cGreenShift  BYTE) 
  (cBlueBits  BYTE) 
  (cBlueShift  BYTE) 
  (cAlphaBits  BYTE) 
  (cAlphaShift  BYTE) 
  (cAccumBits  BYTE) 
  (cAccumRedBits  BYTE) 
  (cAccumGreenBits  BYTE) 
  (cAccumBlueBits  BYTE) 
  (cAccumAlphaBits  BYTE) 
  (cDepthBits  BYTE) 
  (cStencilBits  BYTE) 
  (cAuxBuffers  BYTE) 
  (iLayerType  BYTE) 
  (bReserved  BYTE) 
  (dwLayerMask DWORD) 
  (dwVisibleMask DWORD) 
  (dwDamageMask DWORD))

(fli:define-c-typedef (pixel-format-descriptor (:foreign-name "PIXELFORMATDESCRIPTOR"))
  (:struct tag-pixel-format-descriptor))

(fli:define-c-typedef (hglrc (:foreign-name "HGLRC"))
  HANDLE)



(fli:define-foreign-function (get-pixel-format "GetPixelFormat") 
    ((hdc HDC))
  :result-type :int
  :documentation "Obtains the index of the specified device context's currently selected pixel format."
  :language :ansi-c)

(fli:define-foreign-function (choose-pixel-format "ChoosePixelFormat") 
    ((hdc HDC)
     (pfd (:ptr pixel-format-descriptor)))
  :result-type :int
  :documentation "Attempts to find the pixel format supported by a device context that is the best match to a given pixel format specification."
  :language :ansi-c)

(fli:define-foreign-function (set-pixel-format "SetPixelFormat") 
    ((hdc HDC)
     (iPixelFormat :int)
     (pfd (:ptr pixel-format-descriptor)))
  :result-type BOOL
  :documentation "Sets the specified device context's pixel format to the format specified by the iPixelFormat index"
  :language :ansi-c)

(fli:define-foreign-function (%describe-pixel-format "DescribePixelFormat") 
    ((hdc HDC)
     (iPixelFormat :int)
     (nBytes :int)
     (pfd (:ptr pixel-format-descriptor)))
  :result-type :int
  :documentation "Obtains information about the pixel format identified by iPixelFormat of the device associated with hdc. 
  The function sets the members of the PIXELFORMATDESCRIPTOR structure pointed to by ppfd with that pixel format information."
  :language :ansi-c)

(defun describe-pixel-format (hdc ipfd pfd)
  (%describe-pixel-format hdc ipfd (fli:size-of 'pixel-format-descriptor) pfd))


;;; ------------------------------------------------------------
;;;

(fli:define-foreign-function (wgl-Create-Context "wglCreateContext")
    ((hdc HDC))
  :documentation "Creates a new OpenGL rendering context, which is suitable for drawing on the device referenced by hdc.
   The rendering context has the same pixel format as the device context."
  :result-type HGLRC
  :language :ansi-c)

(fli:define-foreign-function (wgl-make-current "wglMakeCurrent")
    ((hdc HDC)
     (hglrc HGLRC))
  :documentation "Makes a specified OpenGL rendering context the calling thread's current rendering context. 
  All subsequent OpenGL calls made by the thread are drawn on the device identified by hdc. 
  You can also use wglMakeCurrent to make the calling thread's current rendering context not current."
  :result-type BOOL
  :language :ansi-c)

(fli:define-foreign-function (wgl-delete-context "wglDeleteContext")
    ((hglrc HGLRC))
  :result-type BOOL
  :language :ansi-c)

(fli:define-foreign-function (wgl-get-current-context "wglGetCurrentContext")
    ()
  :result-type HGLRC
  :language :ansi-c)

(fli:define-foreign-function (wgl-get-current-dc "wglGetCurrentDC")
    ()
  :result-type HDC
  :language :ansi-c)



(fli:define-foreign-function (swap-buffers "SwapBuffers")
    ((hdc HDC))
  :result-type BOOL
  :language :ansi-c)


;;; ----------------------------------------------------------------------
;;; Support for text within double buffered OpenGL panes
;;; ----------------------------------------------------------------------

(defconstant TMPF_TRUETYPE       #x04)


(defconstant *WGL-FONT-LINES* 0)
(defconstant *WGL-FONT-POLYGONS* 1)

;;; For extruded 3D fonts 

(fli:define-c-struct _POINTFLOAT
  (x :float)
  (y :float))
(fli:define-c-typedef POINTFLOAT (:struct _POINTFLOAT))

(fli:define-c-struct _GLYPHMETRICSFLOAT
  (gmfBlackBoxX      :float) 
  (gmfBlackBoxY      :float) 
  (gmfptGlyphOrigin POINTFLOAT)
  (gmfCellIncX      :float)
  (gmfCellIncY      :float))

(fli:define-c-typedef GLYPHMETRICSFLOAT
  (:struct _GLYPHMETRICSFLOAT))

(fli:define-foreign-function ( win32::wgl-use-font-outlines
                               "wglUseFontOutlines" :dbcs)
    ((hdc HDC)
     (first DWORD)
     (count DWORD)
     (list-base DWORD)
     (deviation :float)
     (extrusion :float)
     (format :int)
     (lpgmf :pointer)) ;;LPGLYPHMETRICSFLOAT
  :result-type BOOL
  :language :ansi-c)


;;; Convert a font to a bitmap.

(fli:define-foreign-function (win32::wgl-use-font-bitmaps
                              "wglUseFontBitmaps" :dbcs)
    ((hdc HDC)
     (first DWORD)
     (count DWORD)
     (list-base DWORD))
  :result-type BOOL)

;;; (WGL-USE-FONT OPENGL-PANE &key FONT OUTLINEP DEVIATION
;;;                                EXTRUSION GLYPH-METRIC-POINTER
;;;                                START COUNT ERRORP LIST-BASE)
;;; Create a display-list for a given font.
;;; Arguments :
;;;  OPENGL-PANE : CAPI:OPENGL-PANE
;;;  FONT : capi font specification. If no font is specified then the
;;;         font for the OPENGL-PANE will be used.
;;;  OUTLINEP : BOOLEAN - When NULL - A display list containing bitmap representation
;;;                                   of each font character will be generated.
;;;                       Otherwise - A display list containing 3d character representation
;;;                                   for each font character will be generated. (Note that
;;;                                   the font must be TrueType in this case)
;;;  DEVIATION : FLOAT - chordal deviation from the original outlines
;;;  EXTRUSION : FLOAT - how much the font is extruded in the negative z direction. 
;;;  GLYPH-METRIC-POINTER : Foreign pointer to an array (length COUNT) of GLYPHMETRICSFLOAT
;;;                         Structures that recieve the glyph metrics. If NIL is passed in
;;;                         then no data will be stored.
;;;  START : INTEGER - First of the set of glyphs which will form the display-list
;;;  COUNT : INTEGER - Number of glyphs to be used in the creation of the display-list.
;;;  ERRORP : BOOLEAN - When not NULL, an error will be signaled when a glerror occurs.
;;;                     otherwise NIL will be returned from the WGL-USE-FONT
;;;  LIST-BASE : INTEGER - Specifies the start of the  display-list.
;;; RESULTS :
;;;   The list-base used for creating the font. (or NIL indicating an error)
;;; Note that once the font list (pointed to by the list-base) is nolonger
;;; required then it should be destroyed by calling OPENGL:GL-DELETE-LISTS

(defun wgl-use-font (opengl-pane &key
                                 (font (gp::get-port-font opengl-pane))
                                 outlinep
                                 (deviation 0.0)
                                 (extrusion 0.1)
                                 glyph-metric-pointer
                                 (start 32) (count 224)
                                 (errorp t)
                                 (list-base (opengl:gl-gen-lists count)))
  (let ((gpfont (gp:lookup-font opengl-pane font))
        (hdc (win32::wgl-get-current-dc)))
    (unwind-protect
        (progn
          (realize-tool gpfont)

          (when (and outlinep
                     (not (logtest TMPF_TRUETYPE
                                   (logical-font-textmetric-pitch gpfont))))
            (opengl:gl-delete-lists list-base count)
            (case errorp
              (:warn (warn "~s : Outline font must be a TrueType font."
                           'wgl-use-font))
              ((nil) nil)
              (otherwise (error "~s : Outline font must be a TrueType font."
                                'wgl-use-font)))
            (return-from wgl-use-font nil))

          (select-object hdc (drawing-tool-handle gpfont))
          ;; For some reason - OpenGL on NT sometimes fails to
          ;; create the font the first time around. Not sure for
          ;; the reason why. The simple fix is if the creation
          ;; of the font fails the first time - try calling 
          ;; wglUseFont... again to make sure that there really
          ;; is some form or error???
          (unless (if outlinep
                      (let ((format (if (eq outlinep :lines)
                                        *WGL-FONT-LINES*
                                      *WGL-FONT-POLYGONS*)))
                        (or
                         (win32::wgl-use-font-outlines hdc start count list-base
                                                       deviation extrusion
                                                       format
                                                       glyph-metric-pointer)
                         (win32::wgl-use-font-outlines hdc start count list-base
                                                       deviation extrusion
                                                       format
                                                       glyph-metric-pointer)))
                    (or (win32::wgl-use-font-bitmaps hdc start count list-base)
                        (win32::wgl-use-font-bitmaps hdc start count list-base)))
                          
            (unwind-protect
                (let ((windows-error-message (win32:get-last-error)))
                  (case errorp
                    (:warn (warn "~S failed : (getLastError = ~d) " 'wgl-use-font windows-error-message))
                    ((nil) nil)
                    (otherwise (error "~S failed : (getLastError = ~d)" 'wgl-use-font windows-error-message))))
              (opengl:gl-delete-lists list-base count)
              (return-from wgl-use-font nil)))
          list-base)
      (unrealize-tool gpfont))))

