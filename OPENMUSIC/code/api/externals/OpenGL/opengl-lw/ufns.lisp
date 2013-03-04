;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/ufns.lisp,v 1.8.3.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")


#| DATE           : 9Oct95 
 | USER           : ken 
 | PROCESSED FILE : /u/ken/OpenGL/opengl/glu.h
 |#


(fli:define-foreign-function (glu-error-string "gluErrorString" :source)
    ((error-code glenum))
  :result-type #+Win32 (w:LPSTR :pass-by :reference)
               #-Win32 (:reference :lisp-string-array)
  :language :ansi-c)

;;; glu-error-string - Win32 implmentation includes gluErrorUnicodeStringEXT which returns
;;;   a UNICODE error string. Only access this when running on NT - use glu-error-string-win
;;;   to dispatch to the correct OS function.

#+Win32
(fli:define-foreign-function (glu-error-unicode-string "gluErrorUnicodeStringEXT" :source)
    ((error-code glenum))
  :result-type (w:LPWSTR :pass-by :reference))

#+Win32
(defun glu-error-string-win (glenum)
  (if (string= (software-type) "Windows NT")
      (glu-error-unicode-string glenum)
    (glu-error-string glenum)))


(fli:define-foreign-function (glu-get-string "gluGetString" :source)
    ((name glenum))
  :result-type #+Win32 (w:LPSTR :pass-by :reference)
               #-Win32 (:reference :lisp-string-array)
  :language :ansi-c)

(fli:define-foreign-function (glu-ortho2-d "gluOrtho2D" :source)
    ((left gldouble)
     (right gldouble)
     (bottom gldouble)
     (top gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-perspective "gluPerspective" :source)
    ((fovy gldouble)
     (aspect gldouble)
     (z-near gldouble)
     (z-far gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-pick-matrix "gluPickMatrix" :source)
    ((x gldouble)
     (y gldouble)
     (width gldouble)
     (height gldouble)
     (viewport (gl-vector :signed-32 4)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-look-at "gluLookAt" :source)
    ((eyex gldouble)
     (eyey gldouble)
     (eyez gldouble)
     (centerx gldouble)
     (centery gldouble)
     (centerz gldouble)
     (upx gldouble)
     (upy gldouble)
     (upz gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-project "gluProject" :source)
    ((objx gldouble)
     (objy gldouble)
     (objz gldouble)
     (model-matrix (gl-vector :double-float 16))
     (proj-matrix (gl-vector :double-float 16))
     (viewport (gl-vector :signed-32 4))
     (:ignore #|winx|# (:reference-return gldouble))
     (:ignore #|winy|# (:reference-return gldouble))
     (:ignore #|winz|# (:reference-return gldouble)))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glu-un-project "gluUnProject" :source)
    ((winx gldouble)
     (winy gldouble)
     (winz gldouble)
     (model-matrix (gl-vector :double-float 16))
     (proj-matrix (gl-vector :double-float 16))
     (viewport (gl-vector :signed-32 4))
     (:ignore #|objx|# (:reference-return gldouble))
     (:ignore #|objt|# (:reference-return gldouble))
     (:ignore #|objz|# (:reference-return gldouble)))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glu-scale-image "gluScaleImage" :source)
    ((format glenum)
     (widthin glint)
     (heightin glint)
     (typein glenum)
     (datain gl-vector)
     (widthout glint)
     (heightout glint)
     (typeout glenum)
     (dataout gl-vector))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glu-build1-dmipmaps "gluBuild1DMipmaps" :source)
    ((target glenum)
     (components glint)
     (width glint)
     (format glenum)
     (type glenum)
     (data gl-vector))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glu-build2-dmipmaps "gluBuild2DMipmaps" :source)
    ((target glenum)
     (components glint)
     (width glint)
     (height glint)
     (format glenum)
     (type glenum)
     (data gl-vector))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-c-typedef (gluquadric-obj (:foreign-name "GLUquadricObj"))
  (:struct gluquadric))

(fli:define-foreign-function (glu-new-quadric "gluNewQuadric" :source)
    nil
  :result-type (:pointer gluquadric-obj)
  :language :ansi-c)

(fli:define-foreign-function (glu-delete-quadric "gluDeleteQuadric" :source)
    ((state (:pointer gluquadric-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-quadric-normals "gluQuadricNormals" :source)
    ((quad-object (:pointer gluquadric-obj))
     (normals glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-quadric-texture "gluQuadricTexture" :source)
    ((quad-object (:pointer gluquadric-obj))
     (texture-coords glboolean))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-quadric-orientation
                              "gluQuadricOrientation"
                              :source)
    ((quad-object (:pointer gluquadric-obj))
     (orientation glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-quadric-draw-style
                              "gluQuadricDrawStyle"
                              :source)
    ((quad-object (:pointer gluquadric-obj))
     (draw-style glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-cylinder "gluCylinder" :source)
    ((qobj (:pointer gluquadric-obj))
     (base-radius gldouble)
     (top-radius gldouble)
     (height gldouble)
     (slices glint)
     (stacks glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-disk "gluDisk" :source)
    ((qobj (:pointer gluquadric-obj))
     (inner-radius gldouble)
     (outer-radius gldouble)
     (slices glint)
     (loops glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-partial-disk "gluPartialDisk" :source)
    ((qobj (:pointer gluquadric-obj))
     (inner-radius gldouble)
     (outer-radius gldouble)
     (slices glint)
     (loops glint)
     (start-angle gldouble)
     (sweep-angle gldouble))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-sphere "gluSphere" :source)
    ((qobj (:pointer gluquadric-obj))
     (radius gldouble)
     (slices glint)
     (stacks glint))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-quadric-callback "gluQuadricCallback" :source)
    ((qobj (:pointer gluquadric-obj))
     (which glenum)
     (fn (:pointer (:function nil :void))))
  :result-type :void
  :language :ansi-c)

(fli:define-c-typedef (glutriangulator-obj
                       (:foreign-name "GLUtriangulatorObj"))
  (:struct GLUtesselator))

(fli:define-foreign-function (glu-new-tess "gluNewTess" :source)
    nil
  :result-type (:pointer glutriangulator-obj)
  :language :ansi-c)

(fli:define-foreign-function (glu-tess-callback "gluTessCallback" :source)
    ((tobj (:pointer glutriangulator-obj))
     (which glenum)
     (fn (:pointer (:function nil :void))))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-delete-tess "gluDeleteTess" :source)
    ((tobj (:pointer glutriangulator-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-begin-polygon "gluBeginPolygon" :source)
    ((tobj (:pointer glutriangulator-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-end-polygon "gluEndPolygon" :source)
    ((tobj (:pointer glutriangulator-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-next-contour "gluNextContour" :source)
    ((tobj (:pointer glutriangulator-obj))
     (type glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-tess-vertex "gluTessVertex" :source)
    ((tobj (:pointer glutriangulator-obj))
     (v (:c-array gldouble 3))
     (data (:pointer :void)))
  :result-type :void
  :language :ansi-c)

(fli:define-c-typedef (glunurbs-obj (:foreign-name "GLUnurbsObj"))
  (:struct glunurbs))

(fli:define-foreign-function (glu-new-nurbs-renderer
                              "gluNewNurbsRenderer"
                              :source)
    nil
  :result-type (:pointer glunurbs-obj)
  :language :ansi-c)

(fli:define-foreign-function (glu-delete-nurbs-renderer
                              "gluDeleteNurbsRenderer"
                              :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-begin-surface "gluBeginSurface" :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-begin-curve "gluBeginCurve" :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-end-curve "gluEndCurve" :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-end-surface "gluEndSurface" :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-begin-trim "gluBeginTrim" :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-end-trim "gluEndTrim" :source)
    ((nobj (:pointer glunurbs-obj)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-pwl-curve "gluPwlCurve" :source)
    ((nobj (:pointer glunurbs-obj))
     (count glint)
     (array (gl-vector :single-float))
     (stride glint)
     (type glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-nurbs-curve "gluNurbsCurve" :source)
    ((nobj (:pointer glunurbs-obj))
     (nknots glint)
     (knot (gl-vector :single-float))
     (stride glint)
     (ctlarray (gl-vector :single-float))
     (order glint)
     (type glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-nurbs-surface "gluNurbsSurface" :source)
    ((nobj (:pointer glunurbs-obj))
     (sknot-count glint)
     (sknot  (gl-vector :single-float))
     (tknot-count glint)
     (tknot (gl-vector :single-float))
     (s-stride glint)
     (t-stride glint)
     (ctlarray  (gl-vector :single-float))
     (sorder glint)
     (torder glint)
     (type glenum))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-load-sampling-matrices
                              "gluLoadSamplingMatrices"
                              :source)
    ((nobj (:pointer glunurbs-obj))
     (model-matrix (gl-vector :single-float 16))
     (proj-matrix  (gl-vector :single-float 16))
     (viewport (gl-vector :signed-32 4)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-nurbs-property "gluNurbsProperty" :source)
    ((nobj (:pointer glunurbs-obj))
     (property glenum)
     (value glfloat))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-get-nurbs-property
                              "gluGetNurbsProperty"
                              :source)
    ((nobj (:pointer glunurbs-obj))
     (property glenum)
     (value (:pointer glfloat)))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (glu-nurbs-callback "gluNurbsCallback" :source)
    ((nobj (:pointer glunurbs-obj))
     (which glenum)
     (fn (:pointer (:function nil :void))))
  :result-type :void
  :language :ansi-c)




