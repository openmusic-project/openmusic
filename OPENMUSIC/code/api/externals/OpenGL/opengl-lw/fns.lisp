;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/fns.lisp,v 1.11.1.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")


(fli:define-foreign-function (gl-accum "glAccum" :source)
    ((op glenum) (value glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-alpha-func "glAlphaFunc" :source)
    ((func glenum) (ref glclampf))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-are-textures-resident "glAreTexturesResident" :source)
    ((n glsizei)
     (textures (gl-vector :unsigned-32))
     (residences (gl-vector :unsigned-8)))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-array-element "glArrayElement" :source)
    ((i glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-begin "glBegin" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-bind-texture "glBindTexture" :source)
    ((target glenum)
     (texture gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-bitmap "glBitmap" :source)
    ((width glsizei)
     (height glsizei)
     (xorig glfloat)
     (yorig glfloat)
     (xmove glfloat)
     (ymove glfloat)
     (bitmap (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-blend-color "glBlendColor" :source)
    ((red glclampf)
     (green glclampf)
     (blue glclampf)
     (alpha glclampf))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-blend-equation "glBlendEquation" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-blend-equation-separate
                              "glBlendEquationSeparate"
                              :source)
    ((mode-rgb glenum)
     (mode-alpha glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-blend-func "glBlendFunc" :source)
    ((sfactor glenum) (dfactor glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-call-list "glCallList" :source)
    ((list gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-call-lists "glCallLists" :source)
    ((n glsizei)
     (type glenum)
     (lists gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clear "glClear" :source)
    ((mask glbitfield))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clear-accum "glClearAccum" :source)
    ((red glfloat)
     (green glfloat)
     (blue glfloat)
     (alpha glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clear-color "glClearColor" :source)
    ((red glclampf)
     (green glclampf)
     (blue glclampf)
     (alpha glclampf))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clear-depth "glClearDepth" :source)
    ((depth glclampd))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clear-index "glClearIndex" :source)
    ((c glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clear-stencil "glClearStencil" :source)
    ((s glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-clip-plane "glClipPlane" :source)
    ((plane glenum)
     (equation (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-b "glColor3b" :source)
    ((red glbyte) (green glbyte) (blue glbyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-bv "glColor3bv" :source)
    ((v (gl-vector :signed-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-d "glColor3d" :source)
    ((red gldouble) (green gldouble) (blue gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-dv "glColor3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-f "glColor3f" :source)
    ((red glfloat) (green glfloat) (blue glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-fv "glColor3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-i "glColor3i" :source)
    ((red glint) (green glint) (blue glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-iv "glColor3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-s "glColor3s" :source)
    ((red glshort) (green glshort) (blue glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-sv "glColor3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-ub "glColor3ub" :source)
    ((red glubyte) (green glubyte) (blue glubyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-ubv "glColor3ubv" :source)
    ((v (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-ui "glColor3ui" :source)
    ((red gluint) (green gluint) (blue gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-uiv "glColor3uiv" :source)
    ((v (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-us "glColor3us" :source)
    ((red glushort) (green glushort) (blue glushort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color3-usv "glColor3usv" :source)
    ((v (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-b "glColor4b" :source)
    ((red glbyte)
     (green glbyte)
     (blue glbyte)
     (alpha glbyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-bv "glColor4bv" :source)
    ((v (gl-vector :signed-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-d "glColor4d" :source)
    ((red gldouble)
     (green gldouble)
     (blue gldouble)
     (alpha gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-dv "glColor4dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-f "glColor4f" :source)
    ((red glfloat)
     (green glfloat)
     (blue glfloat)
     (alpha glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-fv "glColor4fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-i "glColor4i" :source)
    ((red glint)
     (green glint)
     (blue glint)
     (alpha glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-iv "glColor4iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-s "glColor4s" :source)
    ((red glshort)
     (green glshort)
     (blue glshort)
     (alpha glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-sv "glColor4sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-ub "glColor4ub" :source)
    ((red glubyte)
     (green glubyte)
     (blue glubyte)
     (alpha glubyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-ubv "glColor4ubv" :source)
    ((v (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-ui "glColor4ui" :source)
    ((red gluint)
     (green gluint)
     (blue gluint)
     (alpha gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-uiv "glColor4uiv" :source)
    ((v (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-us "glColor4us" :source)
    ((red glushort)
     (green glushort)
     (blue glushort)
     (alpha glushort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color4-usv "glColor4usv" :source)
    ((v (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-mask "glColorMask" :source)
    ((red glboolean)
     (green glboolean)
     (blue glboolean)
     (alpha glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-material "glColorMaterial" :source)
    ((face glenum) (mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-pointer "glColorPointer" :source)
    ((size glint)
     (type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-sub-table "glColorSubTable" :source)
    ((target glenum)
     (start glsizei)
     (count glsizei)
     (format glenum)
     (type glenum)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-table "glColorTable" :source)
    ((target glenum)
     (internalformat glenum)
     (width glsizei)
     (format glenum)
     (type glenum)
     (table gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-table-parameterfv
                              "glColorTableParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-color-table-parameteriv
                              "glColorTableParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-convolution-filter1-d
                              "glConvolutionFilter1D"
                              :source)
    ((target glenum)
     (internalformat glenum)
     (width glsizei)
     (format glenum)
     (type glenum)
     (image gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-convolution-filter2-d
                              "glConvolutionFilter2D"
                              :source)
    ((target glenum)
     (internalformat glenum)
     (width glsizei)
     (height glsizei)
     (format glenum)
     (type glenum)
     (image gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-convolution-parameterf
                              "glConvolutionParameterf"
                              :source)
    ((target glenum) (pname glenum) (params glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-convolution-parameterfv
                              "glConvolutionParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-convolution-parameteri
                              "glConvolutionParameteri"
                              :source)
    ((target glenum) (pname glenum) (params glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-convolution-parameteriv
                              "glConvolutionParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-color-sub-table
                              "glCopyColorSubTable"
                              :source)
    ((target glenum)
     (start glsizei)
     (x glint)
     (y glint)
     (width glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-color-table
                              "glCopyColorTable"
                              :source)
    ((target glenum)
     (internalformat glenum)
     (x glint)
     (y glint)
     (width glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-convolution-filter1-d
                              "glCopyConvolutionFilter1D"
                              :source)
    ((target glenum)
     (internalformat glenum)
     (x glint)
     (y glint)
     (width glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-convolution-filter2-d
                              "glCopyConvolutionFilter2D"
                              :source)
    ((target glenum)
     (internalformat glenum)
     (x glint)
     (y glint)
     (width glsizei)
     (height glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-pixels "glCopyPixels" :source)
    ((x glint)
     (y glint)
     (width glsizei)
     (height glsizei)
     (type glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-tex-image1-d "glCopyTexImage1D" :source)
    ((target glenum)
     (level glint)
     (internalformat glenum)
     (x glint)
     (y glint)
     (width glsizei)
     (border glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-tex-image2-d "glCopyTexImage2D" :source)
    ((target glenum)
     (level glint)
     (internalformat glenum)
     (x glint)
     (y glint)
     (width glsizei)
     (height glsizei)
     (border glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-tex-sub-image1-d "glCopyTexSubImage1D" :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (x glint)
     (y glint)
     (width glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-tex-sub-image2-d "glCopyTexSubImage2D" :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (yoffset glint)
     (x glint)
     (y glint)
     (width glsizei)
     (height glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-copy-tex-sub-image3-d "glCopyTexSubImage3D" :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (yoffset glint)
     (zoffset glint)
     (x glint)
     (y glint)
     (width glsizei)
     (height glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-cull-face "glCullFace" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-delete-lists "glDeleteLists" :source)
    ((list gluint) (range glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-delete-textures "glDeleteTextures" :source)
    ((n glsizei) (textures (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-depth-func "glDepthFunc" :source)
    ((func glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-depth-mask "glDepthMask" :source)
    ((flag glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-depth-range "glDepthRange" :source)
    ((z-near glclampd) (z-far glclampd))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-disable "glDisable" :source)
    ((cap glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-disable-client-state
                              "glDisableClientState"
                              :source)
    ((array glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-draw-arrays "glDrawArrays" :source)
    ((mode glenum)
     (first glint)
     (count glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-draw-buffer "glDrawBuffer" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-draw-elements "glDrawElements" :source)
    ((mode glenum)
     (count glsizei)
     (type glenum)
     (indices gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-draw-pixels "glDrawPixels" :source)
    ((width glsizei)
     (height glsizei)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-draw-range-elements "glDrawRangeElements" :source)
    ((mode glenum)
     (start gluint)
     (end gluint)
     (count glsizei)
     (type glenum)
     (indices gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-edge-flag "glEdgeFlag" :source)
    ((flag glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-edge-flag-pointer "glEdgeFlagPointer" :source)
    ((stride GLsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-edge-flagv "glEdgeFlagv" :source)
    ((flag (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-enable "glEnable" :source)
    ((cap glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-enable-client-state
                              "glEnableClientState"
                              :source)
    ((array glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-end "glEnd" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-end-list "glEndList" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord1-d "glEvalCoord1d" :source)
    ((u gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord1-dv "glEvalCoord1dv" :source)
    ((u (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord1-f "glEvalCoord1f" :source)
    ((u glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord1-fv "glEvalCoord1fv" :source)
    ((u (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord2-d "glEvalCoord2d" :source)
    ((u gldouble) (v gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord2-dv "glEvalCoord2dv" :source)
    ((u (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord2-f "glEvalCoord2f" :source)
    ((u glfloat) (v glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-coord2-fv "glEvalCoord2fv" :source)
    ((u (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-mesh1 "glEvalMesh1" :source)
    ((mode glenum) (i1 glint) (i2 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-mesh2 "glEvalMesh2" :source)
    ((mode glenum)
     (i1 glint)
     (i2 glint)
     (j1 glint)
     (j2 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-point1 "glEvalPoint1" :source)
    ((i glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-eval-point2 "glEvalPoint2" :source)
    ((i glint) (j glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-feedback-buffer "glFeedbackBuffer" :source)
    ((size glsizei)
     (type glenum)
     (buffer (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-finish "glFinish" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-flush "glFlush" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fogf "glFogf" :source)
    ((pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fogfv "glFogfv" :source)
    ((pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fogi "glFogi" :source)
    ((pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fogiv "glFogiv" :source)
    ((pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-front-face "glFrontFace" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-frustum "glFrustum" :source)
    ((left gldouble)
     (right gldouble)
     (bottom gldouble)
     (top gldouble)
     (z-near gldouble)
     (z-far gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-gen-lists "glGenLists" :source)
    ((range glsizei))
  :result-type gluint
  :language :ansi-c)

(fli:define-foreign-function (gl-gen-textures "glGenTextures" :source)
    ((n glsizei)
     (textures (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-booleanv "glGetBooleanv" :source)
    ((pname glenum)
     (params (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-clip-plane "glGetClipPlane" :source)
    ((plane glenum)
     (equation (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-color-table "glGetColorTable" :source)
    ((target glenum)
     (format glenum)
     (type glenum)
     (table gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-color-table-parameterfv
                              "glGetColorTableParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-color-table-parameteriv
                              "glGetColorTableParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-convolution-filter
                              "glGetConvolutionFilter"
                              :source)
    ((target glenum)
     (format glenum)
     (type glenum)
     (image gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-convolution-parameterfv
                              "glGetConvolutionParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-convolution-parameteriv
                              "glGetConvolutionParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-doublev "glGetDoublev" :source)
    ((pname glenum) (params (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-error "glGetError" :source)
    nil
  :result-type glenum
  :language :ansi-c)

(fli:define-foreign-function (gl-get-floatv "glGetFloatv" :source)
    ((pname glenum) (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-histogram "glGetHistogram" :source)
    ((target glenum)
     (reset glboolean)
     (format glenum)
     (type glenum)
     (values gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-histogram-parameterfv
                              "glGetHistogramParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-histogram-parameteriv
                              "glGetHistogramParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-integerv "glGetIntegerv" :source)
    ((pname glenum) (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-lightfv "glGetLightfv" :source)
    ((light glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-lightiv "glGetLightiv" :source)
    ((light glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-mapdv "glGetMapdv" :source)
    ((target glenum)
     (query glenum)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-mapfv "glGetMapfv" :source)
    ((target glenum)
     (query glenum)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-mapiv "glGetMapiv" :source)
    ((target glenum)
     (query glenum)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-materialfv "glGetMaterialfv" :source)
    ((face glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-materialiv "glGetMaterialiv" :source)
    ((face glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-minmax "glGetMinmax" :source)
    ((target glenum)
     (reset glboolean)
     (format glenum)
     (type glenum)
     (values gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-minmax-parameterfv
                              "glGetMinmaxParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-minmax-parameteriv
                              "glGetMinmaxParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-pixel-mapfv "glGetPixelMapfv" :source)
    ((map glenum) (values (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-pixel-mapuiv "glGetPixelMapuiv" :source)
    ((map glenum) (values (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-pixel-mapusv "glGetPixelMapusv" :source)
    ((map glenum) (values (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-pointerv "glGetPointerv" :source)
    ((pname glenum)
     (:ignore #|params|# (:reference-return (:pointer GLvoid))))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-polygon-stipple
                              "glGetPolygonStipple"
                              :source)
    ((mask (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-separable-filter
                              "glGetSeparableFilter"
                              :source)
    ((target glenum)
     (format glenum)
     (type glenum)
     (row gl-vector)
     (column gl-vector)
     (span gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-string "glGetString" :source)
    ((name glenum))
  :result-type glstring-return
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-envfv "glGetTexEnvfv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-enviv "glGetTexEnviv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-gendv "glGetTexGendv" :source)
    ((coord glenum)
     (pname glenum)
     (params (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-genfv "glGetTexGenfv" :source)
    ((coord glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-geniv "glGetTexGeniv" :source)
    ((coord glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-image "glGetTexImage" :source)
    ((target glenum)
     (level glint)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-level-parameterfv
                              "glGetTexLevelParameterfv"
                              :source)
    ((target glenum)
     (level glint)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-level-parameteriv
                              "glGetTexLevelParameteriv"
                              :source)
    ((target glenum)
     (level glint)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-parameterfv
                              "glGetTexParameterfv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-tex-parameteriv
                              "glGetTexParameteriv"
                              :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-hint "glHint" :source)
    ((target glenum) (mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-histogram "glHistogram" :source)
    ((target glenum)
     (width glsizei)
     (internalformat glenum)
     (sink glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-index-mask "glIndexMask" :source)
    ((mask gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-index-pointer "glIndexPointer" :source)
    ((type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexd "glIndexd" :source)
    ((c gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexdv "glIndexdv" :source)
    ((c (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexf "glIndexf" :source)
    ((c glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexfv "glIndexfv" :source)
    ((c (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexi "glIndexi" :source)
    ((c glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexiv "glIndexiv" :source)
    ((c (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexs "glIndexs" :source)
    ((c glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexsv "glIndexsv" :source)
    ((c (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexub "glIndexub" :source)
    ((c glubyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-indexubv "glIndexubv" :source)
    ((c (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-init-names "glInitNames" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-interleaved-arrays
                              "glInterleavedArrays"
                              :source)
    ((format glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-is-enabled "glIsEnabled" :source)
    ((cap glenum))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-is-list "glIsList" :source)
    ((list gluint))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-is-texture "glIsTexture" :source)
    ((texture gluint))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-light-modelf "glLightModelf" :source)
    ((pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-light-modelfv "glLightModelfv" :source)
    ((pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-light-modeli "glLightModeli" :source)
    ((pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-light-modeliv "glLightModeliv" :source)
    ((pname glenum) (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-lightf "glLightf" :source)
    ((light glenum) (pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-lightfv "glLightfv" :source)
    ((light glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-lighti "glLighti" :source)
    ((light glenum) (pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-lightiv "glLightiv" :source)
    ((light glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-line-stipple "glLineStipple" :source)
    ((factor glint) (pattern glushort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-line-width "glLineWidth" :source)
    ((width glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-list-base "glListBase" :source)
    ((base gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-load-identity "glLoadIdentity" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-load-matrixd "glLoadMatrixd" :source)
    ((m (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-load-matrixf "glLoadMatrixf" :source)
    ((m (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-load-name "glLoadName" :source)
    ((name gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-logic-op "glLogicOp" :source)
    ((opcode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map1-d "glMap1d" :source)
    ((target glenum)
     (u1 gldouble)
     (u2 gldouble)
     (stride glint)
     (order glint)
     (points (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map1-f "glMap1f" :source)
    ((target glenum)
     (u1 glfloat)
     (u2 glfloat)
     (stride glint)
     (order glint)
     (points (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map2-d "glMap2d" :source)
    ((target glenum)
     (u1 gldouble)
     (u2 gldouble)
     (ustride glint)
     (uorder glint)
     (v1 gldouble)
     (v2 gldouble)
     (vstride glint)
     (vorder glint)
     (points (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map2-f "glMap2f" :source)
    ((target glenum)
     (u1 glfloat)
     (u2 glfloat)
     (ustride glint)
     (uorder glint)
     (v1 glfloat)
     (v2 glfloat)
     (vstride glint)
     (vorder glint)
     (points (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map-grid1-d "glMapGrid1d" :source)
    ((un glint) (u1 gldouble) (u2 gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map-grid1-f "glMapGrid1f" :source)
    ((un glint) (u1 glfloat) (u2 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map-grid2-d "glMapGrid2d" :source)
    ((un glint)
     (u1 gldouble)
     (u2 gldouble)
     (vn glint)
     (v1 gldouble)
     (v2 gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map-grid2-f "glMapGrid2f" :source)
    ((un glint)
     (u1 glfloat)
     (u2 glfloat)
     (vn glint)
     (v1 glfloat)
     (v2 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-materialf "glMaterialf" :source)
    ((face glenum) (pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-materialfv "glMaterialfv" :source)
    ((face glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-materiali "glMateriali" :source)
    ((face glenum) (pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-materialiv "glMaterialiv" :source)
    ((face glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-matrix-mode "glMatrixMode" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-minmax "glMinmax" :source)
    ((target glenum)
     (internalformat glenum)
     (sink glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-mult-matrixd "glMultMatrixd" :source)
    ((m (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-mult-matrixf "glMultMatrixf" :source)
    ((m (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-new-list "glNewList" :source)
    ((list gluint) (mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-b "glNormal3b" :source)
    ((nx glbyte) (ny glbyte) (nz glbyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-bv "glNormal3bv" :source)
    ((v (gl-vector :signed-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-d "glNormal3d" :source)
    ((nx gldouble) (ny gldouble) (nz gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-dv "glNormal3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-f "glNormal3f" :source)
    ((nx glfloat) (ny glfloat) (nz glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-fv "glNormal3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-i "glNormal3i" :source)
    ((nx glint) (ny glint) (nz glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-iv "glNormal3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-s "glNormal3s" :source)
    ((nx glshort) (ny glshort) (nz glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal3-sv "glNormal3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-normal-pointer "glNormalPointer" :source)
    ((type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-ortho "glOrtho" :source)
    ((left gldouble)
     (right gldouble)
     (bottom gldouble)
     (top gldouble)
     (z-near gldouble)
     (z-far gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pass-through "glPassThrough" :source)
    ((token glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-mapfv "glPixelMapfv" :source)
    ((map glenum)
     (mapsize glint)
     (values (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-mapuiv "glPixelMapuiv" :source)
    ((map glenum)
     (mapsize glint)
     (values (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-mapusv "glPixelMapusv" :source)
    ((map glenum)
     (mapsize glint)
     (values (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-storef "glPixelStoref" :source)
    ((pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-storei "glPixelStorei" :source)
    ((pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-transferf "glPixelTransferf" :source)
    ((pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-transferi "glPixelTransferi" :source)
    ((pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pixel-zoom "glPixelZoom" :source)
    ((xfactor glfloat) (yfactor glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-point-size "glPointSize" :source)
    ((size glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-polygon-mode "glPolygonMode" :source)
    ((face glenum) (mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-polygon-offset "glPolygonOffset" :source)
    ((factor glfloat) (units glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-polygon-stipple "glPolygonStipple" :source)
    ((mask (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pop-attrib "glPopAttrib" :source)
    ()
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pop-client-attrib "glPopClientAttrib" :source)
    ()
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pop-matrix "glPopMatrix" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-pop-name "glPopName" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-prioritize-textures
                              "glPrioritizeTextures"
                              :source)
    ((n GLsizei)
     (textures (gl-vector :unsigned-32))
     (priorities (gl-vector :single-float)) ; glclampf
     )
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-push-attrib "glPushAttrib" :source)
    ((mask glbitfield))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-push-client-attrib "glPushClientAttrib" :source)
    ((mask glbitfield))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-push-matrix "glPushMatrix" :source)
    nil
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-push-name "glPushName" :source)
    ((name gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-d "glRasterPos2d" :source)
    ((x gldouble) (y gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-dv "glRasterPos2dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-f "glRasterPos2f" :source)
    ((x glfloat) (y glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-fv "glRasterPos2fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-i "glRasterPos2i" :source)
    ((x glint) (y glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-iv "glRasterPos2iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-s "glRasterPos2s" :source)
    ((x glshort) (y glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos2-sv "glRasterPos2sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-d "glRasterPos3d" :source)
    ((x gldouble) (y gldouble) (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-dv "glRasterPos3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-f "glRasterPos3f" :source)
    ((x glfloat) (y glfloat) (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-fv "glRasterPos3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-i "glRasterPos3i" :source)
    ((x glint) (y glint) (z glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-iv "glRasterPos3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-s "glRasterPos3s" :source)
    ((x glshort) (y glshort) (z glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos3-sv "glRasterPos3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-d "glRasterPos4d" :source)
    ((x gldouble)
     (y gldouble)
     (z gldouble)
     (w gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-dv "glRasterPos4dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-f "glRasterPos4f" :source)
    ((x glfloat) (y glfloat) (z glfloat) (w glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-fv "glRasterPos4fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-i "glRasterPos4i" :source)
    ((x glint) (y glint) (z glint) (w glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-iv "glRasterPos4iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-s "glRasterPos4s" :source)
    ((x glshort) (y glshort) (z glshort) (w glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-raster-pos4-sv "glRasterPos4sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-read-buffer "glReadBuffer" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-read-pixels "glReadPixels" :source)
    ((x glint)
     (y glint)
     (width glsizei)
     (height glsizei)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rectd "glRectd" :source)
    ((x1 gldouble)
     (y1 gldouble)
     (x2 gldouble)
     (y2 gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rectdv "glRectdv" :source)
    ((v1 (gl-vector :double-float))
     (v2 (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rectf "glRectf" :source)
    ((x1 glfloat)
     (y1 glfloat)
     (x2 glfloat)
     (y2 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rectfv "glRectfv" :source)
    ((v1 (gl-vector :single-float))
     (v2 (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-recti "glRecti" :source)
    ((x1 glint) (y1 glint) (x2 glint) (y2 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rectiv "glRectiv" :source)
    ((v1 (gl-vector :signed-32))
     (v2 (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rects "glRects" :source)
    ((x1 glshort)
     (y1 glshort)
     (x2 glshort)
     (y2 glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rectsv "glRectsv" :source)
    ((v1 (gl-vector :signed-16))
     (v2 (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-render-mode "glRenderMode" :source)
    ((mode glenum))
  :result-type glint
  :language :ansi-c)

(fli:define-foreign-function (gl-reset-histogram
                              "glResetHistogram"
                              :source)
    ((target glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-reset-minmax "glResetMinmax" :source)
    ((target glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rotated "glRotated" :source)
    ((angle gldouble)
     (x gldouble)
     (y gldouble)
     (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-rotatef "glRotatef" :source)
    ((angle glfloat)
     (x glfloat)
     (y glfloat)
     (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-scaled "glScaled" :source)
    ((x gldouble) (y gldouble) (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-scalef "glScalef" :source)
    ((x glfloat) (y glfloat) (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-scissor "glScissor" :source)
    ((x glint)
     (y glint)
     (width glsizei)
     (height glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-select-buffer "glSelectBuffer" :source)
    ((size glsizei) (buffer (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-separable-filter2-d
                              "glSeparableFilter2D"
                              :source)
    ((target glenum)
     (internalformat glenum)
     (width glsizei)
     (height glsizei)
     (format glenum)
     (type glenum)
     (row gl-vector)
     (column gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-shade-model "glShadeModel" :source)
    ((mode glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-stencil-func "glStencilFunc" :source)
    ((func glenum) (ref glint) (mask gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-stencil-mask "glStencilMask" :source)
    ((mask gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-stencil-op "glStencilOp" :source)
    ((fail glenum) (zfail glenum) (zpass glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-d "glTexCoord1d" :source)
    ((s gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-dv "glTexCoord1dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-f "glTexCoord1f" :source)
    ((s glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-fv "glTexCoord1fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-i "glTexCoord1i" :source)
    ((s glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-iv "glTexCoord1iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-s "glTexCoord1s" :source)
    ((s glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord1-sv "glTexCoord1sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-d "glTexCoord2d" :source)
    ((s gldouble) (arg-t gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-dv "glTexCoord2dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-f "glTexCoord2f" :source)
    ((s glfloat) (arg-t glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-fv "glTexCoord2fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-i "glTexCoord2i" :source)
    ((s glint) (arg-t glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-iv "glTexCoord2iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-s "glTexCoord2s" :source)
    ((s glshort) (arg-t glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord2-sv "glTexCoord2sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-d "glTexCoord3d" :source)
    ((s gldouble) (arg-t gldouble) (r gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-dv "glTexCoord3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-f "glTexCoord3f" :source)
    ((s glfloat) (arg-t glfloat) (r glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-fv "glTexCoord3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-i "glTexCoord3i" :source)
    ((s glint) (arg-t glint) (r glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-iv "glTexCoord3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-s "glTexCoord3s" :source)
    ((s glshort) (arg-t glshort) (r glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord3-sv "glTexCoord3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-d "glTexCoord4d" :source)
    ((s gldouble)
     (arg-t gldouble)
     (r gldouble)
     (q gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-dv "glTexCoord4dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-f "glTexCoord4f" :source)
    ((s glfloat)
     (arg-t glfloat)
     (r glfloat)
     (q glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-fv "glTexCoord4fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-i "glTexCoord4i" :source)
    ((s glint) (arg-t glint) (r glint) (q glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-iv "glTexCoord4iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-s "glTexCoord4s" :source)
    ((s glshort)
     (arg-t glshort)
     (r glshort)
     (q glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord4-sv "glTexCoord4sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-coord-pointer "glTexCoordPointer" :source)
    ((size glint)
     (type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-envf "glTexEnvf" :source)
    ((target glenum) (pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-envfv "glTexEnvfv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-envi "glTexEnvi" :source)
    ((target glenum) (pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-enviv "glTexEnviv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-gend "glTexGend" :source)
    ((coord glenum) (pname glenum) (param gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-gendv "glTexGendv" :source)
    ((coord glenum)
     (pname glenum)
     (params (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-genf "glTexGenf" :source)
    ((coord glenum) (pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-genfv "glTexGenfv" :source)
    ((coord glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-geni "glTexGeni" :source)
    ((coord glenum) (pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-geniv "glTexGeniv" :source)
    ((coord glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-image1-d "glTexImage1D" :source)
    ((target glenum)
     (level glint)
     (internalformat glint)
     (width glsizei)
     (border glint)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-image2-d "glTexImage2D" :source)
    ((target glenum)
     (level glint)
     (internalformat glint)
     (width glsizei)
     (height glsizei)
     (border glint)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-image3-d "glTexImage3D" :source)
    ((target glenum)
     (level glint)
     (internalformat glint)
     (width glsizei)
     (height glsizei)
     (depth glsizei)
     (border glint)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-parameterf "glTexParameterf" :source)
    ((target glenum) (pname glenum) (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-parameterfv "glTexParameterfv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-parameteri "glTexParameteri" :source)
    ((target glenum) (pname glenum) (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-parameteriv "glTexParameteriv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-sub-image1-d
                              "glTexSubImage1D"
                              :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (width glsizei)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-sub-image2-d
                              "glTexSubImage2D"
                              :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (yoffset glint)
     (width glsizei)
     (height glsizei)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-tex-sub-image3-d
                              "glTexSubImage3D"
                              :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (yoffset glint)
     (zoffset glint)
     (width glsizei)
     (height glsizei)
     (depth glsizei)
     (format glenum)
     (type glenum)
     (pixels gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-translated "glTranslated" :source)
    ((x gldouble) (y gldouble) (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-translatef "glTranslatef" :source)
    ((x glfloat) (y glfloat) (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-d "glVertex2d" :source)
    ((x gldouble) (y gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-dv "glVertex2dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-f "glVertex2f" :source)
    ((x glfloat) (y glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-fv "glVertex2fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-i "glVertex2i" :source)
    ((x glint) (y glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-iv "glVertex2iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-s "glVertex2s" :source)
    ((x glshort) (y glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex2-sv "glVertex2sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-d "glVertex3d" :source)
    ((x gldouble) (y gldouble) (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-dv "glVertex3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-f "glVertex3f" :source)
    ((x glfloat) (y glfloat) (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-fv "glVertex3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-i "glVertex3i" :source)
    ((x glint) (y glint) (z glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-iv "glVertex3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-s "glVertex3s" :source)
    ((x glshort) (y glshort) (z glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex3-sv "glVertex3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-d "glVertex4d" :source)
    ((x gldouble)
     (y gldouble)
     (z gldouble)
     (w gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-dv "glVertex4dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-f "glVertex4f" :source)
    ((x glfloat) (y glfloat) (z glfloat) (w glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-fv "glVertex4fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-i "glVertex4i" :source)
    ((x glint) (y glint) (z glint) (w glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-iv "glVertex4iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-s "glVertex4s" :source)
    ((x glshort) (y glshort) (z glshort) (w glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex4-sv "glVertex4sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-pointer "glVertexPointer" :source)
    ((size glint)
     (type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-viewport "glViewport" :source)
    ((x glint)
     (y glint)
     (width glsizei)
     (height glsizei))
  :result-type :void
  :language :ansi-c)


(fli:define-foreign-function (gl-sample-coverage "glSampleCoverage" :source)
    ((value glclampf)
     (invert glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-load-transpose-matrixf "glLoadTransposeMatrixf" :source)
    ((m (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-load-transpose-matrixd "glLoadTransposeMatrixd" :source)
    ((m (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-mult-transpose-matrixf "glMultTransposeMatrixf" :source)
    ((m (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-mult-transpose-matrixd "glMultTransposeMatrixd" :source)
    ((m (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compressed-tex-image3-d "glCompressedTexImage3D" :source)
    ((target glenum)
     (level glint)
     (internalformat glenum)
     (width glsizei)
     (height glsizei)
     (depth glsizei)
     (border glint)
     (image-size glsizei)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compressed-tex-image2-d "glCompressedTexImage2D" :source)
    ((target glenum)
     (level glint)
     (internalformat glenum)
     (width glsizei)
     (height glsizei)
     (border glint)
     (image-size glsizei)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compressed-tex-image1-d "glCompressedTexImage1D" :source)
    ((target glenum)
     (level glint)
     (internalformat glenum)
     (width glsizei)
     (border glint)
     (image-size glsizei)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compressed-tex-sub-image3-d
                              "glCompressedTexSubImage3D"
                              :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (yoffset glint)
     (zoffset glint)
     (width glsizei)
     (height glsizei)
     (depth glsizei)
     (format glenum)
     (image-size glsizei)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compressed-tex-sub-image2-d
                              "glCompressedTexSubImage2D"
                              :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (yoffset glint)
     (width glsizei)
     (height glsizei)
     (format glenum)
     (image-size glsizei)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compressed-tex-sub-image1-d
                              "glCompressedTexSubImage1D"
                              :source)
    ((target glenum)
     (level glint)
     (xoffset glint)
     (width glsizei)
     (format glenum)
     (image-size glsizei)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-compressed-tex-image
                              "glGetCompressedTexImage"
                              :source)
    ((target glenum)
     (lod glint)
     (img gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-active-texture
                              "glActiveTexture"
                              :source)
    ((texture glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-client-active-texture
                              "glClientActiveTexture"
                              :source)
    ((texture glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-d "glMultiTexCoord1d" :source)
    ((target glenum)
     (s gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-dv "glMultiTexCoord1dv" :source)
    ((target glenum)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-f "glMultiTexCoord1f" :source)
    ((target glenum)
     (s glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-fv "glMultiTexCoord1fv" :source)
    ((target glenum)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-i "glMultiTexCoord1i" :source)
    ((target glenum)
     (s glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-iv "glMultiTexCoord1iv" :source)
    ((target glenum)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-s "glMultiTexCoord1s" :source)
    ((target glenum)
     (s glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord1-sv "glMultiTexCoord1sv" :source)
    ((target glenum)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-d "glMultiTexCoord2d" :source)
    ((target glenum)
     (s gldouble) (arg-t gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-dv "glMultiTexCoord2dv" :source)
    ((target glenum)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-f "glMultiTexCoord2f" :source)
    ((target glenum)
     (s glfloat) (arg-t glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-fv "glMultiTexCoord2fv" :source)
    ((target glenum)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-i "glMultiTexCoord2i" :source)
    ((target glenum)
     (s glint) (arg-t glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-iv "glMultiTexCoord2iv" :source)
    ((target glenum)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-s "glMultiTexCoord2s" :source)
    ((target glenum)
     (s glshort) (arg-t glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord2-sv "glMultiTexCoord2sv" :source)
    ((target glenum)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-d "glMultiTexCoord3d" :source)
    ((target glenum)
     (s gldouble) (arg-t gldouble) (r gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-dv "glMultiTexCoord3dv" :source)
    ((target glenum)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-f "glMultiTexCoord3f" :source)
    ((target glenum)
     (s glfloat) (arg-t glfloat) (r glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-fv "glMultiTexCoord3fv" :source)
    ((target glenum)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-i "glMultiTexCoord3i" :source)
    ((target glenum)
     (s glint) (arg-t glint) (r glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-iv "glMultiTexCoord3iv" :source)
    ((target glenum)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-s "glMultiTexCoord3s" :source)
    ((target glenum)
     (s glshort) (arg-t glshort) (r glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord3-sv "glMultiTexCoord3sv" :source)
    ((target glenum)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-d "glMultiTexCoord4d" :source)
    ((target glenum)
     (s gldouble)
     (arg-t gldouble)
     (r gldouble)
     (q gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-dv "glMultiTexCoord4dv" :source)
    ((target glenum)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-f "glMultiTexCoord4f" :source)
    ((target glenum)
     (s glfloat)
     (arg-t glfloat)
     (r glfloat)
     (q glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-fv "glMultiTexCoord4fv" :source)
    ((target glenum)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-i "glMultiTexCoord4i" :source)
    ((target glenum)
     (s glint) (arg-t glint) (r glint) (q glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-iv "glMultiTexCoord4iv" :source)
    ((target glenum)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-s "glMultiTexCoord4s" :source)
    ((target glenum)
     (s glshort)
     (arg-t glshort)
     (r glshort)
     (q glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-tex-coord4-sv "glMultiTexCoord4sv" :source)
    ((target glenum)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fog-coordf "glFogCoordf" :source)
    ((coord glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fog-coordfv "glFogCoordfv" :source)
    ((coord (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fog-coordd "glFogCoordd" :source)
    ((coord gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fog-coorddv "glFogCoorddv" :source)
    ((coord (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-fog-coord-pointer "glFogCoordPointer" :source)
    ((type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-b "glSecondaryColor3b" :source)
    ((red glbyte) (green glbyte) (blue glbyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-bv "glSecondaryColor3bv" :source)
    ((v (gl-vector :signed-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-d "glSecondaryColor3d" :source)
    ((red gldouble) (green gldouble) (blue gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-dv "glSecondaryColor3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-f "glSecondaryColor3f" :source)
    ((red glfloat) (green glfloat) (blue glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-fv "glSecondaryColor3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-i "glSecondaryColor3i" :source)
    ((red glint) (green glint) (blue glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-iv "glSecondaryColor3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-s "glSecondaryColor3s" :source)
    ((red glshort) (green glshort) (blue glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-sv "glSecondaryColor3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-ub "glSecondaryColor3ub" :source)
    ((red glubyte) (green glubyte) (blue glubyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-ubv "glSecondaryColor3ubv" :source)
    ((v (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-ui "glSecondaryColor3ui" :source)
    ((red gluint) (green gluint) (blue gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-uiv "glSecondaryColor3uiv" :source)
    ((v (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-us "glSecondaryColor3us" :source)
    ((red glushort) (green glushort) (blue glushort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color3-usv "glSecondaryColor3usv" :source)
    ((v (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-secondary-color-pointer "glSecondaryColorPointer" :source)
    ((size glint)
     (type glenum)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-point-parameterf "glPointParameterf" :source)
    ((pname glenum)
     (param glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-point-parameterfv "glPointParameterfv" :source)
    ((pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-point-parameteri "glPointParameteri" :source)
    ((pname glenum)
     (param glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-point-parameteriv "glPointParameteriv" :source)
    ((pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-blend-func-separate "glBlendFuncSeparate" :source)
    ((src-rgb glenum)
     (dst-rgb glenum)
     (src-alpha glenum)
     (dst-alpha glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-draw-arrays "glMultiDrawArrays" :source)
    ((mode GLenum)
     (first (gl-vector :signed-32))
     (count (gl-vector :signed-32)) ; glsizei
     (primcount glsizei))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-multi-draw-elements "glMultiDrawElements" :source)
    ((mode GLenum)
     (count (gl-vector :signed-32)) ; glsizei
     (type glenum)
     (indices gl-vector)
     (primcount glsizei))
  :result-type :void
  :language :ansi-c)




(fli:define-foreign-function (gl-window-pos2-d "glWindowPos2d" :source)
    ((x gldouble) (y gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-dv "glWindowPos2dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-f "glWindowPos2f" :source)
    ((x glfloat) (y glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-fv "glWindowPos2fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-i "glWindowPos2i" :source)
    ((x glint) (y glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-iv "glWindowPos2iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-s "glWindowPos2s" :source)
    ((x glshort) (y glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos2-sv "glWindowPos2sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-d "glWindowPos3d" :source)
    ((x gldouble) (y gldouble) (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-dv "glWindowPos3dv" :source)
    ((v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-f "glWindowPos3f" :source)
    ((x glfloat) (y glfloat) (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-fv "glWindowPos3fv" :source)
    ((v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-i "glWindowPos3i" :source)
    ((x glint) (y glint) (z glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-iv "glWindowPos3iv" :source)
    ((v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-s "glWindowPos3s" :source)
    ((x glshort) (y glshort) (z glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-window-pos3-sv "glWindowPos3sv" :source)
    ((v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-gen-queries "glGenQueries" :source)
    ((n GLsizei)
     (ids (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-delete-queries "glDeleteQueries" :source)
    ((n GLsizei)
     (ids (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-is-query "glIsQuery" :source)
    ((id gluint))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-begin-query "glBeginQuery" :source)
    ((target glenum)
     (id gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-end-query "glEndQuery" :source)
    ((target glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-queryiv "glGetQueryiv" :source)
    ((target glenum)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-query-objectiv "glGetQueryObjectiv" :source)
    ((id gluint)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-query-objectuiv "glGetQueryObjectuiv" :source)
    ((id gluint)
     (pname glenum)
     (params (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-bind-buffer "glBindBuffer" :source)
    ((target glenum)
     (buffer gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-delete-buffers "glDeleteBuffers" :source)
    ((n glsizei)
     (buffers (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-gen-buffers "glGenBuffers" :source)
    ((n glsizei)
     (buffers (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-is-buffer "glIsBuffer" :source)
    ((id gluint))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-buffer-data "glBufferData" :source)
    ((target glenum)
     (size glsizeiptr)
     (data gl-vector)
     (usage glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-buffer-sub-data "glBufferSubData" :source)
    ((target glenum)
     (offset glintptr)
     (size glsizeiptr)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-buffer-sub-data "glGetBufferSubData" :source)
    ((target glenum)
     (offset glintptr)
     (size glsizeiptr)
     (data gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-map-buffer "glMapBuffer" :source)
    ((target glenum)
     (access glenum))
  :result-type (:pointer glvoid)
  :language :ansi-c)

(fli:define-foreign-function (gl-unmap-buffer "glUnmapBuffer" :source)
    ((target glenum))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-get-buffer-parameteriv
                              "glGetBufferParameteriv"
                              :source)
    ((target glenum)
     (pname GLenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-buffer-pointerv
                              "glGetBufferPointerv"
                              :source)
    ((target glenum)
     (pname GLenum)
     (:ignore #|params|# (:reference-return (:pointer glvoid))))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-draw-buffers "glDrawBuffers" :source)
    ((n glsizei)
     (bufs gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib1-d "glVertexAttrib1d" :source)
    ((index gluint)
     (x gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib1-dv "glVertexAttrib1dv" :source)
    ((index gluint)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib1-f "glVertexAttrib1f" :source)
    ((index gluint)
     (x glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib1-fv "glVertexAttrib1fv" :source)
    ((index gluint)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib1-s "glVertexAttrib1s" :source)
    ((index gluint)
     (x glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib1-sv "glVertexAttrib1sv" :source)
    ((index gluint)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib2-d "glVertexAttrib2d" :source)
    ((index gluint)
     (x gldouble) (y gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib2-dv "glVertexAttrib2dv" :source)
    ((index gluint)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib2-f "glVertexAttrib2f" :source)
    ((index gluint)
     (x glfloat) (y glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib2-fv "glVertexAttrib2fv" :source)
    ((index gluint)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib2-s "glVertexAttrib2s" :source)
    ((index gluint)
     (x glshort) (y glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib2-sv "glVertexAttrib2sv" :source)
    ((index gluint)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib3-d "glVertexAttrib3d" :source)
    ((index gluint)
     (x gldouble) (y gldouble) (z gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib3-dv "glVertexAttrib3dv" :source)
    ((index gluint)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib3-f "glVertexAttrib3f" :source)
    ((index gluint)
     (x glfloat) (y glfloat) (z glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib3-fv "glVertexAttrib3fv" :source)
    ((index gluint)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib3-s "glVertexAttrib3s" :source)
    ((index gluint)
     (x glshort) (y glshort) (z glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib3-sv "glVertexAttrib3sv" :source)
    ((index gluint)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)




(fli:define-foreign-function (gl-vertex-attrib4-nbv "glVertexAttrib4Nbv" :source)
    ((index gluint)
     (v (gl-vector :signed-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-niv "glVertexAttrib4Niv" :source)
    ((index gluint)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-nsv "glVertexAttrib4Nsv" :source)
    ((index gluint)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-nub "glVertexAttrib4Nub" :source)
    ((index gluint)
     (x glubyte)
     (y glubyte)
     (z glubyte)
     (w glubyte))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-nubv "glVertexAttrib4Nubv" :source)
    ((index gluint)
     (v (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-nuiv "glVertexAttrib4Nuiv" :source)
    ((index gluint)
     (v (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-nusv "glVertexAttrib4Nusv" :source)
    ((index gluint)
     (v (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-bv "glVertexAttrib4bv" :source)
    ((index gluint)
     (v (gl-vector :signed-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-d "glVertexAttrib4d" :source)
    ((index gluint)
     (x gldouble)
     (y gldouble)
     (z gldouble)
     (w gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-dv "glVertexAttrib4dv" :source)
    ((index gluint)
     (v (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-f "glVertexAttrib4f" :source)
    ((index gluint)
     (x glfloat)
     (y glfloat)
     (z glfloat)
     (w glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-fv "glVertexAttrib4fv" :source)
    ((index gluint)
     (v (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-iv "glVertexAttrib4iv" :source)
    ((index gluint)
     (v (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-s "glVertexAttrib4s" :source)
    ((index gluint)
     (x glshort)
     (y glshort)
     (z glshort)
     (w glshort))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-sv "glVertexAttrib4sv" :source)
    ((index gluint)
     (v (gl-vector :signed-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-ubv "glVertexAttrib4ubv" :source)
    ((index gluint)
     (v (gl-vector :unsigned-8)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-uiv "glVertexAttrib4uiv" :source)
    ((index gluint)
     (v (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib4-usv "glVertexAttrib4usv" :source)
    ((index gluint)
     (v (gl-vector :unsigned-16)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-vertex-attrib-pointer
                              "glVertexAttribPointer"
                              :source)
    ((index gluint)
     (size glint)
     (type glenum)
     (normalized glboolean)
     (stride glsizei)
     (pointer gl-vector))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-enable-vertex-attrib-array
                              "glEnableVertexAttribArray"
                              :source)
    ((index gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-disable-vertex-attrib-array
                              "glDisableVertexAttribArray"
                              :source)
    ((index gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-vertex-attribdv
                              "glGetVertexAttribdv"
                              :source)
    ((index gluint)
     (pname glenum)
     (params (gl-vector :double-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-vertex-attribfv
                              "glGetVertexAttribfv"
                              :source)
    ((index gluint)
     (pname glenum)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-vertex-attribiv
                              "glGetVertexAttribiv"
                              :source)
    ((index gluint)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-vertex-attrib-pointerv
                              "glGetVertexAttribPointerv"
                              :source)
    ((index gluint)
     (pname glenum)
     (:ignore #|pointer|# (:reference-return (:pointer glvoid))))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-delete-shader "glDeleteShader" :source)
    ((shader gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-detach-shader "glDetachShader" :source)
    ((program gluint)
     (shader gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-create-shader "glCreateShader" :source)
    ((type glenum))
  :result-type gluint
  :language :ansi-c)

(fli:define-foreign-function (gl-shader-source "glShaderSource" :source)
    ((shader gluint)
     (count GLsizei)
     (string gl-vector)
     (length (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-compile-shader "glCompileShader" :source)
    ((shader gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-create-program "glCreateProgram" :source)
    ()
  :result-type gluint
  :language :ansi-c)

(fli:define-foreign-function (gl-attach-shader "glAttachShader" :source)
    ((program gluint)
     (shader gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-link-program "glLinkProgram" :source)
    ((program gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-use-program "glUseProgram" :source)
    ((program gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-delete-program "glDeleteProgram" :source)
    ((program gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-validate-program "glValidateProgram" :source)
    ((program gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform1-f "glUniform1f" :source)
    ((location glint)
     (v0 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform2-f "glUniform2f" :source)
    ((location glint)
     (v0 glfloat)
     (v1 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform3-f "glUniform3f" :source)
    ((location glint)
     (v0 glfloat)
     (v1 glfloat)
     (v2 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform4-f "glUniform4f" :source)
    ((location glint)
     (v0 glfloat)
     (v1 glfloat)
     (v2 glfloat)
     (v3 glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform1-i "glUniform1i" :source)
    ((location glint)
     (v0 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform2-i "glUniform2i" :source)
    ((location glint)
     (v0 glint)
     (v1 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform3-i "glUniform3i" :source)
    ((location glint)
     (v0 glint)
     (v1 glint)
     (v2 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform4-i "glUniform4i" :source)
    ((location glint)
     (v0 glint)
     (v1 glint)
     (v2 glint)
     (v3 glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform1-fv "glUniform1fv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform2-fv "glUniform2fv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform3-fv "glUniform3fv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform4-fv "glUniform4fv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform1-iv "glUniform1iv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform2-iv "glUniform2iv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform3-iv "glUniform3iv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform4-iv "glUniform4iv" :source)
    ((location glint)
     (count glsizei)
     (value (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix2-fv "glUniformMatrix2fv" :source)
    ((location glint)
     (count glsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix3-fv "glUniformMatrix3fv" :source)
    ((location glint)
     (count glsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix4-fv "glUniformMatrix4fv" :source)
    ((location glint)
     (count glsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-is-shader "glIsShader" :source)
    ((shader gluint))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-is-program "glIsProgram" :source)
    ((program gluint))
  :result-type glboolean
  :language :ansi-c)

(fli:define-foreign-function (gl-get-shaderiv "glGetShaderiv" :source)
    ((shader gluint)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-programiv "glGetProgramiv" :source)
    ((program gluint)
     (pname glenum)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-attached-shaders
                              "glGetAttachedShaders"
                              :source)
    ((program gluint)
     (max-count glsizei)
     (:ignore (:reference-return glsizei)) ; count
     (shaders (gl-vector :unsigned-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-shader-info-log
                              "glGetShaderInfoLog"
                              :source)
    ((shader gluint)
     (buf-size glsizei)
     (:ignore (:reference-return glsizei)) ; length
     (info-log (:pointer glchar)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-program-info-log
                              "glGetProgramInfoLog"
                              :source)
    ((program gluint)
     (buf-size glsizei)
     (:ignore (:reference-return glsizei)) ; length
     (info-log (:pointer glchar)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-uniform-location
                              "glGetUniformLocation"
                              :source)
    ((program gluint)
     (name (:pointer glchar)))
  :result-type glint
  :language :ansi-c)

(fli:define-foreign-function (gl-get-active-uniform
                              "glGetActiveUniform"
                              :source)
    ((program gluint)
     (index gluint)
     (buf-size glsizei)
     (:ignore (:reference-return glsizei)) ; length
     (:ignore (:reference-return glsizei)) ; size
     (:ignore (:reference-return glenum))  ; type
     (name (:pointer glchar)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-uniformfv "glGetUniformfv" :source)
    ((program gluint)
     (location glint)
     (params (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-uniformiv "glGetUniformiv" :source)
    ((program gluint)
     (location glint)
     (params (gl-vector :signed-32)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-shader-source "glGetShaderSource" :source)
    ((shader gluint)
     (buf-size glsizei)
     (:ignore (:reference-return glsizei)) ; length
     (source (:pointer glchar)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-bind-attrib-location
                              "glBindAttribLocation"
                              :source)
    ((program gluint)
     (index gluint)
     (name (:pointer glchar)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-active-attrib
                              "glGetActiveAttrib"
                              :source)
    ((program gluint)
     (index gluint)
     (buf-size glsizei)
     (:ignore (:reference-return glsizei)) ; length
     (:ignore (:reference-return glsizei)) ; size
     (:ignore (:reference-return glenum))  ; type
     (name (:pointer glchar)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-get-attrib-location
                              "glGetAttribLocation"
                              :source)
    ((program gluint)
     (name (:pointer glchar)))
  :result-type glint
  :language :ansi-c)

(fli:define-foreign-function (gl-stencil-func-separate
                              "glStencilFuncSeparate"
                              :source)
    ((face glenum)
     (func glenum)
     (ref glint)
     (mask gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-stencil-op-separate
                              "glStencilOpSeparate"
                              :source)
    ((face glenum)
     (fail glenum)
     (zfail glenum)
     (zpass glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-stencil-mask-separate
                              "glStencilMaskSeparate"
                              :source)
    ((face glenum)
     (mask gluint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix2x3-fv "glUniformMatrix2x3fv" :source)
    ((location glint)
     (count GLsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix3x2-fv "glUniformMatrix3x2fv" :source)
    ((location glint)
     (count GLsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix2x4-fv "glUniformMatrix2x4fv" :source)
    ((location glint)
     (count GLsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix4x2-fv "glUniformMatrix4x2fv" :source)
    ((location glint)
     (count GLsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix3x4-fv "glUniformMatrix3x4fv" :source)
    ((location glint)
     (count GLsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (gl-uniform-matrix4x3-fv "glUniformMatrix4x3fv" :source)
    ((location glint)
     (count GLsizei)
     (transpose glboolean)
     (value (gl-vector :single-float)))
  :result-type :void
  :language :ansi-c)

