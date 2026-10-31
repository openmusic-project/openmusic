(in-package #:immediate-gl)

;;;; The names and values are those of the LispWorks OPENGL package, checked
;;;; one by one against opengl-lw/constants.lisp of the OpenMusic tree.

;; Primitive modes (GL 1.1 BeginMode).
(defconstant *gl-points*        #x0000)
(defconstant *gl-lines*         #x0001)
(defconstant *gl-line-loop*     #x0002)
(defconstant *gl-line-strip*    #x0003)
(defconstant *gl-triangles*     #x0004)
(defconstant *gl-quads*         #x0007)
(defconstant *gl-polygon*       #x0009)

;; Matrix modes.
(defconstant *gl-modelview*     #x1700)
(defconstant *gl-projection*    #x1701)
(defconstant *gl-texture*       #x1702)

;; Matrix query names, for GL-GET-DOUBLEV.
(defconstant *gl-modelview-matrix*  #x0BA6)
(defconstant *gl-projection-matrix* #x0BA7)

;; Capabilities.
(defconstant *gl-point-smooth*  #x0B10)
(defconstant *gl-point-size*    #x0B11)
(defconstant *gl-line-smooth*   #x0B20)
(defconstant *gl-cull-face*     #x0B44)
(defconstant *gl-lighting*      #x0B50)
(defconstant *gl-depth-test*    #x0B71)
(defconstant *gl-normalize*     #x0BA1)
(defconstant *gl-blend*         #x0BE2)
(defconstant *gl-color-material* #x0B57)
(defconstant *gl-light0*        #x4000)

;; Clear-buffer bits.
(defconstant *gl-depth-buffer-bit*   #x00000100)
(defconstant *gl-stencil-buffer-bit* #x00000400)
(defconstant *gl-color-buffer-bit*   #x00004000)

;; Depth functions.
(defconstant *gl-never*         #x0200)
(defconstant *gl-less*          #x0201)
(defconstant *gl-equal*         #x0202)
(defconstant *gl-lequal*        #x0203)
(defconstant *gl-greater*       #x0204)
(defconstant *gl-notequal*      #x0205)
(defconstant *gl-gequal*        #x0206)
(defconstant *gl-always*        #x0207)

;; Blending factors.
(defconstant *gl-zero*                #x0)
(defconstant *gl-one*                 #x1)
(defconstant *gl-src-alpha*           #x0302)
(defconstant *gl-one-minus-src-alpha* #x0303)
(defconstant *gl-dst-alpha*           #x0304)
(defconstant *gl-one-minus-dst-alpha* #x0305)

;; Hints.
(defconstant *gl-point-smooth-hint*   #x0C51)
(defconstant *gl-line-smooth-hint*    #x0C52)
(defconstant *gl-polygon-smooth-hint* #x0C53)
(defconstant *gl-dont-care*     #x1100)
(defconstant *gl-fastest*       #x1101)
(defconstant *gl-nicest*        #x1102)

;; Faces and polygon modes.
(defconstant *gl-front*          #x0404)
(defconstant *gl-back*           #x0405)
(defconstant *gl-front-and-back* #x0408)
(defconstant *gl-point*          #x1B00)
(defconstant *gl-line*           #x1B01)
(defconstant *gl-fill*           #x1B02)

;; Shading models.
(defconstant *gl-flat*          #x1D00)
(defconstant *gl-smooth*        #x1D01)

;; Light and material parameters.
(defconstant *gl-ambient*              #x1200)
(defconstant *gl-diffuse*              #x1201)
(defconstant *gl-specular*             #x1202)
(defconstant *gl-position*             #x1203)
(defconstant *gl-emission*             #x1600)
(defconstant *gl-shininess*            #x1601)
(defconstant *gl-ambient-and-diffuse*  #x1602)

;; Light model parameters.
(defconstant *gl-light-model-local-viewer* #x0B51)
(defconstant *gl-light-model-two-side*     #x0B52)
(defconstant *gl-light-model-ambient*      #x0B53)
