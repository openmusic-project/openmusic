;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/constants.lisp,v 1.5.1.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.


(in-package "OPENGL")


;; AccumOp 
(defconstant *GL-ACCUM*                            #x0100)
(defconstant *GL-LOAD*                             #x0101)
(defconstant *GL-RETURN*                           #x0102)
(defconstant *GL-MULT*                             #x0103)
(defconstant *GL-ADD*                              #x0104)

;; AlphaFunction 
(defconstant *GL-NEVER*                            #x0200)
(defconstant *GL-LESS*                             #x0201)
(defconstant *GL-EQUAL*                            #x0202)
(defconstant *GL-LEQUAL*                           #x0203)
(defconstant *GL-GREATER*                          #x0204)
(defconstant *GL-NOTEQUAL*                         #x0205)
(defconstant *GL-GEQUAL*                           #x0206)
(defconstant *GL-ALWAYS*                           #x0207)

;; AttribMask 
(defconstant *GL-CURRENT-BIT*                      #x00000001)
(defconstant *GL-POINT-BIT*                        #x00000002)
(defconstant *GL-LINE-BIT*                         #x00000004)
(defconstant *GL-POLYGON-BIT*                      #x00000008)
(defconstant *GL-POLYGON-STIPPLE-BIT*              #x00000010)
(defconstant *GL-PIXEL-MODE-BIT*                   #x00000020)
(defconstant *GL-LIGHTING-BIT*                     #x00000040)
(defconstant *GL-FOG-BIT*                          #x00000080)
(defconstant *GL-DEPTH-BUFFER-BIT*                 #x00000100)
(defconstant *GL-ACCUM-BUFFER-BIT*                 #x00000200)
(defconstant *GL-STENCIL-BUFFER-BIT*               #x00000400)
(defconstant *GL-VIEWPORT-BIT*                     #x00000800)
(defconstant *GL-TRANSFORM-BIT*                    #x00001000)
(defconstant *GL-ENABLE-BIT*                       #x00002000)
(defconstant *GL-COLOR-BUFFER-BIT*                 #x00004000)
(defconstant *GL-HINT-BIT*                         #x00008000)
(defconstant *GL-EVAL-BIT*                         #x00010000)
(defconstant *GL-LIST-BIT*                         #x00020000)
(defconstant *GL-TEXTURE-BIT*                      #x00040000)
(defconstant *GL-SCISSOR-BIT*                      #x00080000)
(defconstant *GL-ALL-ATTRIB-BITS*                  #x000fffff)

;; BeginMode 
(defconstant *GL-POINTS*                           #x0000)
(defconstant *GL-LINES*                            #x0001)
(defconstant *GL-LINE-LOOP*                        #x0002)
(defconstant *GL-LINE-STRIP*                       #x0003)
(defconstant *GL-TRIANGLES*                        #x0004)
(defconstant *GL-TRIANGLE-STRIP*                   #x0005)
(defconstant *GL-TRIANGLE-FAN*                     #x0006)
(defconstant *GL-QUADS*                            #x0007)
(defconstant *GL-QUAD-STRIP*                       #x0008)
(defconstant *GL-POLYGON*                          #x0009)

;; BlendEquationMode 
;;      GL-LOGIC-OP 
;;      GL-FUNC-ADD 
;;      GL-MIN 
;;      GL-MAX 
;;      GL-FUNC-SUBTRACT 
;;      GL-FUNC-REVERSE-SUBTRACT 

;; BlendingFactorDest 
(defconstant *GL-ZERO*                             #x0)
(defconstant *GL-ONE*                              #x1)
(defconstant *GL-SRC-COLOR*                        #x0300)
(defconstant *GL-ONE-MINUS-SRC-COLOR*              #x0301)
(defconstant *GL-SRC-ALPHA*                        #x0302)
(defconstant *GL-ONE-MINUS-SRC-ALPHA*              #x0303)
(defconstant *GL-DST-ALPHA*                        #x0304)
(defconstant *GL-ONE-MINUS-DST-ALPHA*              #x0305)
;;      GL-CONSTANT-COLOR 
;;      GL-ONE-MINUS-CONSTANT-COLOR 
;;      GL-CONSTANT-ALPHA 
;;      GL-ONE-MINUS-CONSTANT-ALPHA 

;; BlendingFactorSrc 
;;      GL-ZERO 
;;      GL-ONE 
(defconstant *GL-DST-COLOR*                        #x0306)
(defconstant *GL-ONE-MINUS-DST-COLOR*              #x0307)
(defconstant *GL-SRC-ALPHA-SATURATE*               #x0308)
;;      GL-SRC-ALPHA 
;;      GL-ONE-MINUS-SRC-ALPHA 
;;      GL-DST-ALPHA 
;;      GL-ONE-MINUS-DST-ALPHA 
;;      GL-CONSTANT-COLOR 
;;      GL-ONE-MINUS-CONSTANT-COLOR 
;;      GL-CONSTANT-ALPHA 
;;      GL-ONE-MINUS-CONSTANT-ALPHA 

;; Boolean 
(defconstant *GL-TRUE*                             #x1)
(defconstant *GL-FALSE*                            #x0)

;; ClearBufferMask 
;;      GL-COLOR-BUFFER-BIT 
;;      GL-ACCUM-BUFFER-BIT 
;;      GL-STENCIL-BUFFER-BIT 
;;      GL-DEPTH-BUFFER-BIT 

;; ClientArrayType 
;;      GL-VERTEX-ARRAY 
;;      GL-NORMAL-ARRAY 
;;      GL-COLOR-ARRAY 
;;      GL-INDEX-ARRAY 
;;      GL-TEXTURE-COORD-ARRAY 
;;      GL-EDGE-FLAG-ARRAY 

;; ClipPlaneName 
(defconstant *GL-CLIP-PLANE0*                      #x3000)
(defconstant *GL-CLIP-PLANE1*                      #x3001)
(defconstant *GL-CLIP-PLANE2*                      #x3002)
(defconstant *GL-CLIP-PLANE3*                      #x3003)
(defconstant *GL-CLIP-PLANE4*                      #x3004)
(defconstant *GL-CLIP-PLANE5*                      #x3005)

;; ColorMaterialFace 
;;      GL-FRONT 
;;      GL-BACK 
;;      GL-FRONT-AND-BACK 

;; ColorMaterialParameter 
;;      GL-AMBIENT 
;;      GL-DIFFUSE 
;;      GL-SPECULAR 
;;      GL-EMISSION 
;;      GL-AMBIENT-AND-DIFFUSE 

;; ColorPointerType 
;;      GL-BYTE 
;;      GL-UNSIGNED-BYTE 
;;      GL-SHORT 
;;      GL-UNSIGNED-SHORT 
;;      GL-INT 
;;      GL-UNSIGNED-INT 
;;      GL-FLOAT 
;;      GL-DOUBLE 

;; ColorTableParameterPName 
;;      GL-COLOR-TABLE-SCALE 
;;      GL-COLOR-TABLE-BIAS 

;; ColorTableTarget 
;;      GL-COLOR-TABLE 
;;      GL-POST-CONVOLUTION-COLOR-TABLE 
;;      GL-POST-COLOR-MATRIX-COLOR-TABLE 
;;      GL-PROXY-COLOR-TABLE 
;;      GL-PROXY-POST-CONVOLUTION-COLOR-TABLE 
;;      GL-PROXY-POST-COLOR-MATRIX-COLOR-TABLE 

;; ConvolutionBorderMode 
;;      GL-REDUCE 
;;      GL-IGNORE-BORDER 
;;      GL-CONSTANT-BORDER 

;; ConvolutionParameter 
;;      GL-CONVOLUTION-BORDER-MODE 
;;      GL-CONVOLUTION-FILTER-SCALE 
;;      GL-CONVOLUTION-FILTER-BIAS 

;; ConvolutionTarget 
;;      GL-CONVOLUTION-1D 
;;      GL-CONVOLUTION-2D 

;; CullFaceMode 
;;      GL-FRONT 
;;      GL-BACK 
;;      GL-FRONT-AND-BACK 

;; DataType 
(defconstant *GL-BYTE*                             #x1400)
(defconstant *GL-UNSIGNED-BYTE*                    #x1401)
(defconstant *GL-SHORT*                            #x1402)
(defconstant *GL-UNSIGNED-SHORT*                   #x1403)
(defconstant *GL-INT*                              #x1404)
(defconstant *GL-UNSIGNED-INT*                     #x1405)
(defconstant *GL-FLOAT*                            #x1406)
(defconstant *GL-2-BYTES*                          #x1407)
(defconstant *GL-3-BYTES*                          #x1408)
(defconstant *GL-4-BYTES*                          #x1409)
(defconstant *GL-DOUBLE*                           #x140A)

;; DepthFunction 
;;      GL-NEVER 
;;      GL-LESS 
;;      GL-EQUAL 
;;      GL-LEQUAL 
;;      GL-GREATER 
;;      GL-NOTEQUAL 
;;      GL-GEQUAL 
;;      GL-ALWAYS 

;; DrawBufferMode 
(defconstant *GL-NONE*                             #x0)
(defconstant *GL-FRONT-LEFT*                       #x0400)
(defconstant *GL-FRONT-RIGHT*                      #x0401)
(defconstant *GL-BACK-LEFT*                        #x0402)
(defconstant *GL-BACK-RIGHT*                       #x0403)
(defconstant *GL-FRONT*                            #x0404)
(defconstant *GL-BACK*                             #x0405)
(defconstant *GL-LEFT*                             #x0406)
(defconstant *GL-RIGHT*                            #x0407)
(defconstant *GL-FRONT-AND-BACK*                   #x0408)
(defconstant *GL-AUX0*                             #x0409)
(defconstant *GL-AUX1*                             #x040A)
(defconstant *GL-AUX2*                             #x040B)
(defconstant *GL-AUX3*                             #x040C)

;; Enable 
;;      GL-FOG 
;;      GL-LIGHTING 
;;      GL-TEXTURE-1D 
;;      GL-TEXTURE-2D 
;;      GL-LINE-STIPPLE 
;;      GL-POLYGON-STIPPLE 
;;      GL-CULL-FACE 
;;      GL-ALPHA-TEST 
;;      GL-BLEND 
;;      GL-INDEX-LOGIC-OP 
;;      GL-COLOR-LOGIC-OP 
;;      GL-DITHER 
;;      GL-STENCIL-TEST 
;;      GL-DEPTH-TEST 
;;      GL-CLIP-PLANE0 
;;      GL-CLIP-PLANE1 
;;      GL-CLIP-PLANE2 
;;      GL-CLIP-PLANE3 
;;      GL-CLIP-PLANE4 
;;      GL-CLIP-PLANE5 
;;      GL-LIGHT0 
;;      GL-LIGHT1 
;;      GL-LIGHT2 
;;      GL-LIGHT3 
;;      GL-LIGHT4 
;;      GL-LIGHT5 
;;      GL-LIGHT6 
;;      GL-LIGHT7 
;;      GL-TEXTURE-GEN-S 
;;      GL-TEXTURE-GEN-T 
;;      GL-TEXTURE-GEN-R 
;;      GL-TEXTURE-GEN-Q 
;;      GL-MAP1-VERTEX-3 
;;      GL-MAP1-VERTEX-4 
;;      GL-MAP1-COLOR-4 
;;      GL-MAP1-INDEX 
;;      GL-MAP1-NORMAL 
;;      GL-MAP1-TEXTURE-COORD-1 
;;      GL-MAP1-TEXTURE-COORD-2 
;;      GL-MAP1-TEXTURE-COORD-3 
;;      GL-MAP1-TEXTURE-COORD-4 
;;      GL-MAP2-VERTEX-3 
;;      GL-MAP2-VERTEX-4 
;;      GL-MAP2-COLOR-4 
;;      GL-MAP2-INDEX 
;;      GL-MAP2-NORMAL 
;;      GL-MAP2-TEXTURE-COORD-1 
;;      GL-MAP2-TEXTURE-COORD-2 
;;      GL-MAP2-TEXTURE-COORD-3 
;;      GL-MAP2-TEXTURE-COORD-4 
;;      GL-POINT-SMOOTH 
;;      GL-LINE-SMOOTH 
;;      GL-POLYGON-SMOOTH 
;;      GL-SCISSOR-TEST 
;;      GL-COLOR-MATERIAL 
;;      GL-NORMALIZE 
;;      GL-AUTO-NORMAL 
;;      GL-VERTEX-ARRAY 
;;      GL-NORMAL-ARRAY 
;;      GL-COLOR-ARRAY 
;;      GL-INDEX-ARRAY 
;;      GL-TEXTURE-COORD-ARRAY 
;;      GL-EDGE-FLAG-ARRAY 
;;      GL-POLYGON-OFFSET-POINT 
;;      GL-POLYGON-OFFSET-LINE 
;;      GL-POLYGON-OFFSET-FILL 
;;      GL-COLOR-TABLE 
;;      GL-POST-CONVOLUTION-COLOR-TABLE 
;;      GL-POST-COLOR-MATRIX-COLOR-TABLE 
;;      GL-CONVOLUTION-1D 
;;      GL-CONVOLUTION-2D 
;;      GL-SEPARABLE-2D 
;;      GL-HISTOGRAM 
;;      GL-MINMAX 
;;      GL-RESCALE-NORMAL 
;;      GL-TEXTURE-3D 

;; ErrorCode 
(defconstant *GL-NO-ERROR*                         #x0)
(defconstant *GL-INVALID-ENUM*                     #x0500)
(defconstant *GL-INVALID-VALUE*                    #x0501)
(defconstant *GL-INVALID-OPERATION*                #x0502)
(defconstant *GL-STACK-OVERFLOW*                   #x0503)
(defconstant *GL-STACK-UNDERFLOW*                  #x0504)
(defconstant *GL-OUT-OF-MEMORY*                    #x0505)
;;      GL-TABLE-TOO-LARGE 

;; FeedBackMode 
(defconstant *GL-2D*                               #x0600)
(defconstant *GL-3D*                               #x0601)
(defconstant *GL-3D-COLOR*                         #x0602)
(defconstant *GL-3D-COLOR-TEXTURE*                 #x0603)
(defconstant *GL-4D-COLOR-TEXTURE*                 #x0604)

;; FeedBackToken 
(defconstant *GL-PASS-THROUGH-TOKEN*               #x0700)
(defconstant *GL-POINT-TOKEN*                      #x0701)
(defconstant *GL-LINE-TOKEN*                       #x0702)
(defconstant *GL-POLYGON-TOKEN*                    #x0703)
(defconstant *GL-BITMAP-TOKEN*                     #x0704)
(defconstant *GL-DRAW-PIXEL-TOKEN*                 #x0705)
(defconstant *GL-COPY-PIXEL-TOKEN*                 #x0706)
(defconstant *GL-LINE-RESET-TOKEN*                 #x0707)

;; FogMode 
;;      GL-LINEAR 
(defconstant *GL-EXP*                              #x0800)
(defconstant *GL-EXP2*                             #x0801)

;; FogParameter 
;;      GL-FOG-COLOR 
;;      GL-FOG-DENSITY 
;;      GL-FOG-END 
;;      GL-FOG-INDEX 
;;      GL-FOG-MODE 
;;      GL-FOG-START 

;; FrontFaceDirection 
(defconstant *GL-CW*                               #x0900)
(defconstant *GL-CCW*                              #x0901)

;; GetColorTableParameterPName 
;;      GL-COLOR-TABLE-SCALE 
;;      GL-COLOR-TABLE-BIAS 
;;      GL-COLOR-TABLE-FORMAT 
;;      GL-COLOR-TABLE-WIDTH 
;;      GL-COLOR-TABLE-RED-SIZE 
;;      GL-COLOR-TABLE-GREEN-SIZE 
;;      GL-COLOR-TABLE-BLUE-SIZE 
;;      GL-COLOR-TABLE-ALPHA-SIZE 
;;      GL-COLOR-TABLE-LUMINANCE-SIZE 
;;      GL-COLOR-TABLE-INTENSITY-SIZE 

;; GetConvolutionParameterPName 
;;      GL-CONVOLUTION-BORDER-COLOR 
;;      GL-CONVOLUTION-BORDER-MODE 
;;      GL-CONVOLUTION-FILTER-SCALE 
;;      GL-CONVOLUTION-FILTER-BIAS 
;;      GL-CONVOLUTION-FORMAT 
;;      GL-CONVOLUTION-WIDTH 
;;      GL-CONVOLUTION-HEIGHT 
;;      GL-MAX-CONVOLUTION-WIDTH 
;;      GL-MAX-CONVOLUTION-HEIGHT 

;; GetHistogramParameterPName 
;;      GL-HISTOGRAM-WIDTH 
;;      GL-HISTOGRAM-FORMAT 
;;      GL-HISTOGRAM-RED-SIZE 
;;      GL-HISTOGRAM-GREEN-SIZE 
;;      GL-HISTOGRAM-BLUE-SIZE 
;;      GL-HISTOGRAM-ALPHA-SIZE 
;;      GL-HISTOGRAM-LUMINANCE-SIZE 
;;      GL-HISTOGRAM-SINK 

;; GetMapTarget 
(defconstant *GL-COEFF*                            #x0A00)
(defconstant *GL-ORDER*                            #x0A01)
(defconstant *GL-DOMAIN*                           #x0A02)

;; GetMinmaxParameterPName 
;;      GL-MINMAX-FORMAT 
;;      GL-MINMAX-SINK 

;; GetPixelMap 
;;      GL-PIXEL-MAP-I-TO-I 
;;      GL-PIXEL-MAP-S-TO-S 
;;      GL-PIXEL-MAP-I-TO-R 
;;      GL-PIXEL-MAP-I-TO-G 
;;      GL-PIXEL-MAP-I-TO-B 
;;      GL-PIXEL-MAP-I-TO-A 
;;      GL-PIXEL-MAP-R-TO-R 
;;      GL-PIXEL-MAP-G-TO-G 
;;      GL-PIXEL-MAP-B-TO-B 
;;      GL-PIXEL-MAP-A-TO-A 

;; GetPointerTarget 
;;      GL-VERTEX-ARRAY-POINTER 
;;      GL-NORMAL-ARRAY-POINTER 
;;      GL-COLOR-ARRAY-POINTER 
;;      GL-INDEX-ARRAY-POINTER 
;;      GL-TEXTURE-COORD-ARRAY-POINTER 
;;      GL-EDGE-FLAG-ARRAY-POINTER 

;; GetTarget 
(defconstant *GL-CURRENT-COLOR*                    #x0B00)
(defconstant *GL-CURRENT-INDEX*                    #x0B01)
(defconstant *GL-CURRENT-NORMAL*                   #x0B02)
(defconstant *GL-CURRENT-TEXTURE-COORDS*           #x0B03)
(defconstant *GL-CURRENT-RASTER-COLOR*             #x0B04)
(defconstant *GL-CURRENT-RASTER-INDEX*             #x0B05)
(defconstant *GL-CURRENT-RASTER-TEXTURE-COORDS*    #x0B06)
(defconstant *GL-CURRENT-RASTER-POSITION*          #x0B07)
(defconstant *GL-CURRENT-RASTER-POSITION-VALID*    #x0B08)
(defconstant *GL-CURRENT-RASTER-DISTANCE*          #x0B09)
(defconstant *GL-POINT-SMOOTH*                     #x0B10)
(defconstant *GL-POINT-SIZE*                       #x0B11)
(defconstant *GL-POINT-SIZE-RANGE*                 #x0B12)
(defconstant *GL-POINT-SIZE-GRANULARITY*           #x0B13)
(defconstant *GL-LINE-SMOOTH*                      #x0B20)
(defconstant *GL-LINE-WIDTH*                       #x0B21)
(defconstant *GL-LINE-WIDTH-RANGE*                 #x0B22)
(defconstant *GL-LINE-WIDTH-GRANULARITY*           #x0B23)
(defconstant *GL-LINE-STIPPLE*                     #x0B24)
(defconstant *GL-LINE-STIPPLE-PATTERN*             #x0B25)
(defconstant *GL-LINE-STIPPLE-REPEAT*              #x0B26)
;;      GL-SMOOTH-POINT-SIZE-RANGE 
;;      GL-SMOOTH-POINT-SIZE-GRANULARITY 
;;      GL-SMOOTH-LINE-WIDTH-RANGE 
;;      GL-SMOOTH-LINE-WIDTH-GRANULARITY 
;;      GL-ALIASED-POINT-SIZE-RANGE 
;;      GL-ALIASED-LINE-WIDTH-RANGE 
(defconstant *GL-LIST-MODE*                        #x0B30)
(defconstant *GL-MAX-LIST-NESTING*                 #x0B31)
(defconstant *GL-LIST-BASE*                        #x0B32)
(defconstant *GL-LIST-INDEX*                       #x0B33)
(defconstant *GL-POLYGON-MODE*                     #x0B40)
(defconstant *GL-POLYGON-SMOOTH*                   #x0B41)
(defconstant *GL-POLYGON-STIPPLE*                  #x0B42)
(defconstant *GL-EDGE-FLAG*                        #x0B43)
(defconstant *GL-CULL-FACE*                        #x0B44)
(defconstant *GL-CULL-FACE-MODE*                   #x0B45)
(defconstant *GL-FRONT-FACE*                       #x0B46)
(defconstant *GL-LIGHTING*                         #x0B50)
(defconstant *GL-LIGHT-MODEL-LOCAL-VIEWER*         #x0B51)
(defconstant *GL-LIGHT-MODEL-TWO-SIDE*             #x0B52)
(defconstant *GL-LIGHT-MODEL-AMBIENT*              #x0B53)
(defconstant *GL-SHADE-MODEL*                      #x0B54)
(defconstant *GL-COLOR-MATERIAL-FACE*              #x0B55)
(defconstant *GL-COLOR-MATERIAL-PARAMETER*         #x0B56)
(defconstant *GL-COLOR-MATERIAL*                   #x0B57)
(defconstant *GL-FOG*                              #x0B60)
(defconstant *GL-FOG-INDEX*                        #x0B61)
(defconstant *GL-FOG-DENSITY*                      #x0B62)
(defconstant *GL-FOG-START*                        #x0B63)
(defconstant *GL-FOG-END*                          #x0B64)
(defconstant *GL-FOG-MODE*                         #x0B65)
(defconstant *GL-FOG-COLOR*                        #x0B66)
(defconstant *GL-DEPTH-RANGE*                      #x0B70)
(defconstant *GL-DEPTH-TEST*                       #x0B71)
(defconstant *GL-DEPTH-WRITEMASK*                  #x0B72)
(defconstant *GL-DEPTH-CLEAR-VALUE*                #x0B73)
(defconstant *GL-DEPTH-FUNC*                       #x0B74)
(defconstant *GL-ACCUM-CLEAR-VALUE*                #x0B80)
(defconstant *GL-STENCIL-TEST*                     #x0B90)
(defconstant *GL-STENCIL-CLEAR-VALUE*              #x0B91)
(defconstant *GL-STENCIL-FUNC*                     #x0B92)
(defconstant *GL-STENCIL-VALUE-MASK*               #x0B93)
(defconstant *GL-STENCIL-FAIL*                     #x0B94)
(defconstant *GL-STENCIL-PASS-DEPTH-FAIL*          #x0B95)
(defconstant *GL-STENCIL-PASS-DEPTH-PASS*          #x0B96)
(defconstant *GL-STENCIL-REF*                      #x0B97)
(defconstant *GL-STENCIL-WRITEMASK*                #x0B98)
(defconstant *GL-MATRIX-MODE*                      #x0BA0)
(defconstant *GL-NORMALIZE*                        #x0BA1)
(defconstant *GL-VIEWPORT*                         #x0BA2)
(defconstant *GL-MODELVIEW-STACK-DEPTH*            #x0BA3)
(defconstant *GL-PROJECTION-STACK-DEPTH*           #x0BA4)
(defconstant *GL-TEXTURE-STACK-DEPTH*              #x0BA5)
(defconstant *GL-MODELVIEW-MATRIX*                 #x0BA6)
(defconstant *GL-PROJECTION-MATRIX*                #x0BA7)
(defconstant *GL-TEXTURE-MATRIX*                   #x0BA8)
(defconstant *GL-ATTRIB-STACK-DEPTH*               #x0BB0)
(defconstant *GL-CLIENT-ATTRIB-STACK-DEPTH*        #x0BB1)
(defconstant *GL-ALPHA-TEST*                       #x0BC0)
(defconstant *GL-ALPHA-TEST-FUNC*                  #x0BC1)
(defconstant *GL-ALPHA-TEST-REF*                   #x0BC2)
(defconstant *GL-DITHER*                           #x0BD0)
(defconstant *GL-BLEND-DST*                        #x0BE0)
(defconstant *GL-BLEND-SRC*                        #x0BE1)
(defconstant *GL-BLEND*                            #x0BE2)
(defconstant *GL-LOGIC-OP-MODE*                    #x0BF0)
(defconstant *GL-INDEX-LOGIC-OP*                   #x0BF1)
(defconstant *GL-COLOR-LOGIC-OP*                   #x0BF2)
(defconstant *GL-AUX-BUFFERS*                      #x0C00)
(defconstant *GL-DRAW-BUFFER*                      #x0C01)
(defconstant *GL-READ-BUFFER*                      #x0C02)
(defconstant *GL-SCISSOR-BOX*                      #x0C10)
(defconstant *GL-SCISSOR-TEST*                     #x0C11)
(defconstant *GL-INDEX-CLEAR-VALUE*                #x0C20)
(defconstant *GL-INDEX-WRITEMASK*                  #x0C21)
(defconstant *GL-COLOR-CLEAR-VALUE*                #x0C22)
(defconstant *GL-COLOR-WRITEMASK*                  #x0C23)
(defconstant *GL-INDEX-MODE*                       #x0C30)
(defconstant *GL-RGBA-MODE*                        #x0C31)
(defconstant *GL-DOUBLEBUFFER*                     #x0C32)
(defconstant *GL-STEREO*                           #x0C33)
(defconstant *GL-RENDER-MODE*                      #x0C40)
(defconstant *GL-PERSPECTIVE-CORRECTION-HINT*      #x0C50)
(defconstant *GL-POINT-SMOOTH-HINT*                #x0C51)
(defconstant *GL-LINE-SMOOTH-HINT*                 #x0C52)
(defconstant *GL-POLYGON-SMOOTH-HINT*              #x0C53)
(defconstant *GL-FOG-HINT*                         #x0C54)
(defconstant *GL-TEXTURE-GEN-S*                    #x0C60)
(defconstant *GL-TEXTURE-GEN-T*                    #x0C61)
(defconstant *GL-TEXTURE-GEN-R*                    #x0C62)
(defconstant *GL-TEXTURE-GEN-Q*                    #x0C63)
(defconstant *GL-PIXEL-MAP-I-TO-I*                 #x0C70)
(defconstant *GL-PIXEL-MAP-S-TO-S*                 #x0C71)
(defconstant *GL-PIXEL-MAP-I-TO-R*                 #x0C72)
(defconstant *GL-PIXEL-MAP-I-TO-G*                 #x0C73)
(defconstant *GL-PIXEL-MAP-I-TO-B*                 #x0C74)
(defconstant *GL-PIXEL-MAP-I-TO-A*                 #x0C75)
(defconstant *GL-PIXEL-MAP-R-TO-R*                 #x0C76)
(defconstant *GL-PIXEL-MAP-G-TO-G*                 #x0C77)
(defconstant *GL-PIXEL-MAP-B-TO-B*                 #x0C78)
(defconstant *GL-PIXEL-MAP-A-TO-A*                 #x0C79)
(defconstant *GL-PIXEL-MAP-I-TO-I-SIZE*            #x0CB0)
(defconstant *GL-PIXEL-MAP-S-TO-S-SIZE*            #x0CB1)
(defconstant *GL-PIXEL-MAP-I-TO-R-SIZE*            #x0CB2)
(defconstant *GL-PIXEL-MAP-I-TO-G-SIZE*            #x0CB3)
(defconstant *GL-PIXEL-MAP-I-TO-B-SIZE*            #x0CB4)
(defconstant *GL-PIXEL-MAP-I-TO-A-SIZE*            #x0CB5)
(defconstant *GL-PIXEL-MAP-R-TO-R-SIZE*            #x0CB6)
(defconstant *GL-PIXEL-MAP-G-TO-G-SIZE*            #x0CB7)
(defconstant *GL-PIXEL-MAP-B-TO-B-SIZE*            #x0CB8)
(defconstant *GL-PIXEL-MAP-A-TO-A-SIZE*            #x0CB9)
(defconstant *GL-UNPACK-SWAP-BYTES*                #x0CF0)
(defconstant *GL-UNPACK-LSB-FIRST*                 #x0CF1)
(defconstant *GL-UNPACK-ROW-LENGTH*                #x0CF2)
(defconstant *GL-UNPACK-SKIP-ROWS*                 #x0CF3)
(defconstant *GL-UNPACK-SKIP-PIXELS*               #x0CF4)
(defconstant *GL-UNPACK-ALIGNMENT*                 #x0CF5)
(defconstant *GL-PACK-SWAP-BYTES*                  #x0D00)
(defconstant *GL-PACK-LSB-FIRST*                   #x0D01)
(defconstant *GL-PACK-ROW-LENGTH*                  #x0D02)
(defconstant *GL-PACK-SKIP-ROWS*                   #x0D03)
(defconstant *GL-PACK-SKIP-PIXELS*                 #x0D04)
(defconstant *GL-PACK-ALIGNMENT*                   #x0D05)
(defconstant *GL-MAP-COLOR*                        #x0D10)
(defconstant *GL-MAP-STENCIL*                      #x0D11)
(defconstant *GL-INDEX-SHIFT*                      #x0D12)
(defconstant *GL-INDEX-OFFSET*                     #x0D13)
(defconstant *GL-RED-SCALE*                        #x0D14)
(defconstant *GL-RED-BIAS*                         #x0D15)
(defconstant *GL-ZOOM-X*                           #x0D16)
(defconstant *GL-ZOOM-Y*                           #x0D17)
(defconstant *GL-GREEN-SCALE*                      #x0D18)
(defconstant *GL-GREEN-BIAS*                       #x0D19)
(defconstant *GL-BLUE-SCALE*                       #x0D1A)
(defconstant *GL-BLUE-BIAS*                        #x0D1B)
(defconstant *GL-ALPHA-SCALE*                      #x0D1C)
(defconstant *GL-ALPHA-BIAS*                       #x0D1D)
(defconstant *GL-DEPTH-SCALE*                      #x0D1E)
(defconstant *GL-DEPTH-BIAS*                       #x0D1F)
(defconstant *GL-MAX-EVAL-ORDER*                   #x0D30)
(defconstant *GL-MAX-LIGHTS*                       #x0D31)
(defconstant *GL-MAX-CLIP-PLANES*                  #x0D32)
(defconstant *GL-MAX-TEXTURE-SIZE*                 #x0D33)
(defconstant *GL-MAX-PIXEL-MAP-TABLE*              #x0D34)
(defconstant *GL-MAX-ATTRIB-STACK-DEPTH*           #x0D35)
(defconstant *GL-MAX-MODELVIEW-STACK-DEPTH*        #x0D36)
(defconstant *GL-MAX-NAME-STACK-DEPTH*             #x0D37)
(defconstant *GL-MAX-PROJECTION-STACK-DEPTH*       #x0D38)
(defconstant *GL-MAX-TEXTURE-STACK-DEPTH*          #x0D39)
(defconstant *GL-MAX-VIEWPORT-DIMS*                #x0D3A)
(defconstant *GL-MAX-CLIENT-ATTRIB-STACK-DEPTH*    #x0D3B)
(defconstant *GL-SUBPIXEL-BITS*                    #x0D50)
(defconstant *GL-INDEX-BITS*                       #x0D51)
(defconstant *GL-RED-BITS*                         #x0D52)
(defconstant *GL-GREEN-BITS*                       #x0D53)
(defconstant *GL-BLUE-BITS*                        #x0D54)
(defconstant *GL-ALPHA-BITS*                       #x0D55)
(defconstant *GL-DEPTH-BITS*                       #x0D56)
(defconstant *GL-STENCIL-BITS*                     #x0D57)
(defconstant *GL-ACCUM-RED-BITS*                   #x0D58)
(defconstant *GL-ACCUM-GREEN-BITS*                 #x0D59)
(defconstant *GL-ACCUM-BLUE-BITS*                  #x0D5A)
(defconstant *GL-ACCUM-ALPHA-BITS*                 #x0D5B)
(defconstant *GL-NAME-STACK-DEPTH*                 #x0D70)
(defconstant *GL-AUTO-NORMAL*                      #x0D80)
(defconstant *GL-MAP1-COLOR-4*                     #x0D90)
(defconstant *GL-MAP1-INDEX*                       #x0D91)
(defconstant *GL-MAP1-NORMAL*                      #x0D92)
(defconstant *GL-MAP1-TEXTURE-COORD-1*             #x0D93)
(defconstant *GL-MAP1-TEXTURE-COORD-2*             #x0D94)
(defconstant *GL-MAP1-TEXTURE-COORD-3*             #x0D95)
(defconstant *GL-MAP1-TEXTURE-COORD-4*             #x0D96)
(defconstant *GL-MAP1-VERTEX-3*                    #x0D97)
(defconstant *GL-MAP1-VERTEX-4*                    #x0D98)
(defconstant *GL-MAP2-COLOR-4*                     #x0DB0)
(defconstant *GL-MAP2-INDEX*                       #x0DB1)
(defconstant *GL-MAP2-NORMAL*                      #x0DB2)
(defconstant *GL-MAP2-TEXTURE-COORD-1*             #x0DB3)
(defconstant *GL-MAP2-TEXTURE-COORD-2*             #x0DB4)
(defconstant *GL-MAP2-TEXTURE-COORD-3*             #x0DB5)
(defconstant *GL-MAP2-TEXTURE-COORD-4*             #x0DB6)
(defconstant *GL-MAP2-VERTEX-3*                    #x0DB7)
(defconstant *GL-MAP2-VERTEX-4*                    #x0DB8)
(defconstant *GL-MAP1-GRID-DOMAIN*                 #x0DD0)
(defconstant *GL-MAP1-GRID-SEGMENTS*               #x0DD1)
(defconstant *GL-MAP2-GRID-DOMAIN*                 #x0DD2)
(defconstant *GL-MAP2-GRID-SEGMENTS*               #x0DD3)
(defconstant *GL-TEXTURE-1D*                       #x0DE0)
(defconstant *GL-TEXTURE-2D*                       #x0DE1)
(defconstant *GL-FEEDBACK-BUFFER-POINTER*          #x0DF0)
(defconstant *GL-FEEDBACK-BUFFER-SIZE*             #x0DF1)
(defconstant *GL-FEEDBACK-BUFFER-TYPE*             #x0DF2)
(defconstant *GL-SELECTION-BUFFER-POINTER*         #x0DF3)
(defconstant *GL-SELECTION-BUFFER-SIZE*            #x0DF4)
;;      GL-TEXTURE-BINDING-1D 
;;      GL-TEXTURE-BINDING-2D 
;;      GL-TEXTURE-BINDING-3D 
;;      GL-VERTEX-ARRAY 
;;      GL-NORMAL-ARRAY 
;;      GL-COLOR-ARRAY 
;;      GL-INDEX-ARRAY 
;;      GL-TEXTURE-COORD-ARRAY 
;;      GL-EDGE-FLAG-ARRAY 
;;      GL-VERTEX-ARRAY-SIZE 
;;      GL-VERTEX-ARRAY-TYPE 
;;      GL-VERTEX-ARRAY-STRIDE 
;;      GL-NORMAL-ARRAY-TYPE 
;;      GL-NORMAL-ARRAY-STRIDE 
;;      GL-COLOR-ARRAY-SIZE 
;;      GL-COLOR-ARRAY-TYPE 
;;      GL-COLOR-ARRAY-STRIDE 
;;      GL-INDEX-ARRAY-TYPE 
;;      GL-INDEX-ARRAY-STRIDE 
;;      GL-TEXTURE-COORD-ARRAY-SIZE 
;;      GL-TEXTURE-COORD-ARRAY-TYPE 
;;      GL-TEXTURE-COORD-ARRAY-STRIDE 
;;      GL-EDGE-FLAG-ARRAY-STRIDE 
;;      GL-POLYGON-OFFSET-FACTOR 
;;      GL-POLYGON-OFFSET-UNITS 
;;      GL-COLOR-TABLE 
;;      GL-POST-CONVOLUTION-COLOR-TABLE 
;;      GL-POST-COLOR-MATRIX-COLOR-TABLE 
;;      GL-CONVOLUTION-1D 
;;      GL-CONVOLUTION-2D 
;;      GL-SEPARABLE-2D 
;;      GL-POST-CONVOLUTION-RED-SCALE 
;;      GL-POST-CONVOLUTION-GREEN-SCALE 
;;      GL-POST-CONVOLUTION-BLUE-SCALE 
;;      GL-POST-CONVOLUTION-ALPHA-SCALE 
;;      GL-POST-CONVOLUTION-RED-BIAS 
;;      GL-POST-CONVOLUTION-GREEN-BIAS 
;;      GL-POST-CONVOLUTION-BLUE-BIAS 
;;      GL-POST-CONVOLUTION-ALPHA-BIAS 
;;      GL-COLOR-MATRIX 
;;      GL-COLOR-MATRIX-STACK-DEPTH 
;;      GL-MAX-COLOR-MATRIX-STACK-DEPTH 
;;      GL-POST-COLOR-MATRIX-RED-SCALE 
;;      GL-POST-COLOR-MATRIX-GREEN-SCALE 
;;      GL-POST-COLOR-MATRIX-BLUE-SCALE 
;;      GL-POST-COLOR-MATRIX-ALPHA-SCALE 
;;      GL-POST-COLOR-MATRIX-RED-BIAS 
;;      GL-POST-COLOR-MATRIX-GREEN-BIAS 
;;      GL-POST-COLOR-MATRIX-BLUE-BIAS 
;;      GL-POST-COLOR-MATRIX-ALPHA-BIAS 
;;      GL-HISTOGRAM 
;;      GL-MINMAX 
;;      GL-MAX-ELEMENTS-VERTICES 
;;      GL-MAX-ELEMENTS-INDICES 
;;      GL-RESCALE-NORMAL 
;;      GL-LIGHT-MODEL-COLOR-CONTROL 
;;      GL-PACK-SKIP-IMAGES 
;;      GL-PACK-IMAGE-HEIGHT 
;;      GL-UNPACK-SKIP-IMAGES 
;;      GL-UNPACK-IMAGE-HEIGHT 
;;      GL-TEXTURE-3D 
;;      GL-MAX-3D-TEXTURE-SIZE 
;;      GL-BLEND-COLOR 
;;      GL-BLEND-EQUATION 

;; GetTextureParameter 
;;      GL-TEXTURE-MAG-FILTER 
;;      GL-TEXTURE-MIN-FILTER 
;;      GL-TEXTURE-WRAP-S 
;;      GL-TEXTURE-WRAP-T 
(defconstant *GL-TEXTURE-WIDTH*                    #x1000)
(defconstant *GL-TEXTURE-HEIGHT*                   #x1001)
(defconstant *GL-TEXTURE-INTERNAL-FORMAT*          #x1003)
(defconstant *GL-TEXTURE-BORDER-COLOR*             #x1004)
(defconstant *GL-TEXTURE-BORDER*                   #x1005)
;;      GL-TEXTURE-RED-SIZE 
;;      GL-TEXTURE-GREEN-SIZE 
;;      GL-TEXTURE-BLUE-SIZE 
;;      GL-TEXTURE-ALPHA-SIZE 
;;      GL-TEXTURE-LUMINANCE-SIZE 
;;      GL-TEXTURE-INTENSITY-SIZE 
;;      GL-TEXTURE-PRIORITY 
;;      GL-TEXTURE-RESIDENT 
;;      GL-TEXTURE-DEPTH 
;;      GL-TEXTURE-WRAP-R 
;;      GL-TEXTURE-MIN-LOD 
;;      GL-TEXTURE-MAX-LOD 
;;      GL-TEXTURE-BASE-LEVEL 
;;      GL-TEXTURE-MAX-LEVEL 

;; HintMode 
(defconstant *GL-DONT-CARE*                        #x1100)
(defconstant *GL-FASTEST*                          #x1101)
(defconstant *GL-NICEST*                           #x1102)

;; HintTarget 
;;      GL-PERSPECTIVE-CORRECTION-HINT 
;;      GL-POINT-SMOOTH-HINT 
;;      GL-LINE-SMOOTH-HINT 
;;      GL-POLYGON-SMOOTH-HINT 
;;      GL-FOG-HINT 

;; HistogramTarget 
;;      GL-HISTOGRAM 
;;      GL-PROXY-HISTOGRAM 

;; IndexPointerType 
;;      GL-SHORT 
;;      GL-INT 
;;      GL-FLOAT 
;;      GL-DOUBLE 

;; LightModelColorControl 
;;      GL-SINGLE-COLOR 
;;      GL-SEPARATE-SPECULAR-COLOR 

;; LightModelParameter 
;;      GL-LIGHT-MODEL-AMBIENT 
;;      GL-LIGHT-MODEL-LOCAL-VIEWER 
;;      GL-LIGHT-MODEL-TWO-SIDE 
;;      GL-LIGHT-MODEL-COLOR-CONTROL 

;; LightName 
(defconstant *GL-LIGHT0*                           #x4000)
(defconstant *GL-LIGHT1*                           #x4001)
(defconstant *GL-LIGHT2*                           #x4002)
(defconstant *GL-LIGHT3*                           #x4003)
(defconstant *GL-LIGHT4*                           #x4004)
(defconstant *GL-LIGHT5*                           #x4005)
(defconstant *GL-LIGHT6*                           #x4006)
(defconstant *GL-LIGHT7*                           #x4007)

;; LightParameter 
(defconstant *GL-AMBIENT*                          #x1200)
(defconstant *GL-DIFFUSE*                          #x1201)
(defconstant *GL-SPECULAR*                         #x1202)
(defconstant *GL-POSITION*                         #x1203)
(defconstant *GL-SPOT-DIRECTION*                   #x1204)
(defconstant *GL-SPOT-EXPONENT*                    #x1205)
(defconstant *GL-SPOT-CUTOFF*                      #x1206)
(defconstant *GL-CONSTANT-ATTENUATION*             #x1207)
(defconstant *GL-LINEAR-ATTENUATION*               #x1208)
(defconstant *GL-QUADRATIC-ATTENUATION*            #x1209)

;; InterleavedArrays 
;;      GL-V2F 
;;      GL-V3F 
;;      GL-C4UB-V2F 
;;      GL-C4UB-V3F 
;;      GL-C3F-V3F 
;;      GL-N3F-V3F 
;;      GL-C4F-N3F-V3F 
;;      GL-T2F-V3F 
;;      GL-T4F-V4F 
;;      GL-T2F-C4UB-V3F 
;;      GL-T2F-C3F-V3F 
;;      GL-T2F-N3F-V3F 
;;      GL-T2F-C4F-N3F-V3F 
;;      GL-T4F-C4F-N3F-V4F 

;; ListMode 
(defconstant *GL-COMPILE*                          #x1300)
(defconstant *GL-COMPILE-AND-EXECUTE*              #x1301)

;; ListNameType 
;;      GL-BYTE 
;;      GL-UNSIGNED-BYTE 
;;      GL-SHORT 
;;      GL-UNSIGNED-SHORT 
;;      GL-INT 
;;      GL-UNSIGNED-INT 
;;      GL-FLOAT 
;;      GL-2-BYTES 
;;      GL-3-BYTES 
;;      GL-4-BYTES 

;; LogicOp 
(defconstant *GL-CLEAR*                            #x1500)
(defconstant *GL-AND*                              #x1501)
(defconstant *GL-AND-REVERSE*                      #x1502)
(defconstant *GL-COPY*                             #x1503)
(defconstant *GL-AND-INVERTED*                     #x1504)
(defconstant *GL-NOOP*                             #x1505)
(defconstant *GL-XOR*                              #x1506)
(defconstant *GL-OR*                               #x1507)
(defconstant *GL-NOR*                              #x1508)
(defconstant *GL-EQUIV*                            #x1509)
(defconstant *GL-INVERT*                           #x150A)
(defconstant *GL-OR-REVERSE*                       #x150B)
(defconstant *GL-COPY-INVERTED*                    #x150C)
(defconstant *GL-OR-INVERTED*                      #x150D)
(defconstant *GL-NAND*                             #x150E)
(defconstant *GL-SET*                              #x150F)

;; MapTarget 
;;      GL-MAP1-COLOR-4 
;;      GL-MAP1-INDEX 
;;      GL-MAP1-NORMAL 
;;      GL-MAP1-TEXTURE-COORD-1 
;;      GL-MAP1-TEXTURE-COORD-2 
;;      GL-MAP1-TEXTURE-COORD-3 
;;      GL-MAP1-TEXTURE-COORD-4 
;;      GL-MAP1-VERTEX-3 
;;      GL-MAP1-VERTEX-4 
;;      GL-MAP2-COLOR-4 
;;      GL-MAP2-INDEX 
;;      GL-MAP2-NORMAL 
;;      GL-MAP2-TEXTURE-COORD-1 
;;      GL-MAP2-TEXTURE-COORD-2 
;;      GL-MAP2-TEXTURE-COORD-3 
;;      GL-MAP2-TEXTURE-COORD-4 
;;      GL-MAP2-VERTEX-3 
;;      GL-MAP2-VERTEX-4 

;; MaterialFace 
;;      GL-FRONT 
;;      GL-BACK 
;;      GL-FRONT-AND-BACK 

;; MaterialParameter 
(defconstant *GL-EMISSION*                         #x1600)
(defconstant *GL-SHININESS*                        #x1601)
(defconstant *GL-AMBIENT-AND-DIFFUSE*              #x1602)
(defconstant *GL-COLOR-INDEXES*                    #x1603)
;;      GL-AMBIENT 
;;      GL-DIFFUSE 
;;      GL-SPECULAR 

;; MatrixMode 
(defconstant *GL-MODELVIEW*                        #x1700)
(defconstant *GL-PROJECTION*                       #x1701)
(defconstant *GL-TEXTURE*                          #x1702)

;; MeshMode1 
;;      GL-POINT 
;;      GL-LINE 

;; MeshMode2 
;;      GL-POINT 
;;      GL-LINE 
;;      GL-FILL 

;; MinmaxTarget 
;;      GL-MINMAX 

;; NormalPointerType 
;;      GL-BYTE 
;;      GL-SHORT 
;;      GL-INT 
;;      GL-FLOAT 
;;      GL-DOUBLE 

;; PixelCopyType 
(defconstant *GL-COLOR*                            #x1800)
(defconstant *GL-DEPTH*                            #x1801)
(defconstant *GL-STENCIL*                          #x1802)

;; PixelFormat 
(defconstant *GL-COLOR-INDEX*                      #x1900)
(defconstant *GL-STENCIL-INDEX*                    #x1901)
(defconstant *GL-DEPTH-COMPONENT*                  #x1902)
(defconstant *GL-RED*                              #x1903)
(defconstant *GL-GREEN*                            #x1904)
(defconstant *GL-BLUE*                             #x1905)
(defconstant *GL-ALPHA*                            #x1906)
(defconstant *GL-RGB*                              #x1907)
(defconstant *GL-RGBA*                             #x1908)
(defconstant *GL-LUMINANCE*                        #x1909)
(defconstant *GL-LUMINANCE-ALPHA*                  #x190A)
;;      GL-ABGR 

;; PixelInternalFormat 
;;      GL-ALPHA4 
;;      GL-ALPHA8 
;;      GL-ALPHA12 
;;      GL-ALPHA16 
;;      GL-LUMINANCE4 
;;      GL-LUMINANCE8 
;;      GL-LUMINANCE12 
;;      GL-LUMINANCE16 
;;      GL-LUMINANCE4-ALPHA4 
;;      GL-LUMINANCE6-ALPHA2 
;;      GL-LUMINANCE8-ALPHA8 
;;      GL-LUMINANCE12-ALPHA4 
;;      GL-LUMINANCE12-ALPHA12 
;;      GL-LUMINANCE16-ALPHA16 
;;      GL-INTENSITY 
;;      GL-INTENSITY4 
;;      GL-INTENSITY8 
;;      GL-INTENSITY12 
;;      GL-INTENSITY16 
;;      GL-R3-G3-B2 
;;      GL-RGB4 
;;      GL-RGB5 
;;      GL-RGB8 
;;      GL-RGB10 
;;      GL-RGB12 
;;      GL-RGB16 
;;      GL-RGBA2 
;;      GL-RGBA4 
;;      GL-RGB5-A1 
;;      GL-RGBA8 
;;      GL-RGB10-A2 
;;      GL-RGBA12 
;;      GL-RGBA16 

;; PixelMap 
;;      GL-PIXEL-MAP-I-TO-I 
;;      GL-PIXEL-MAP-S-TO-S 
;;      GL-PIXEL-MAP-I-TO-R 
;;      GL-PIXEL-MAP-I-TO-G 
;;      GL-PIXEL-MAP-I-TO-B 
;;      GL-PIXEL-MAP-I-TO-A 
;;      GL-PIXEL-MAP-R-TO-R 
;;      GL-PIXEL-MAP-G-TO-G 
;;      GL-PIXEL-MAP-B-TO-B 
;;      GL-PIXEL-MAP-A-TO-A 

;; PixelStore 
;;      GL-UNPACK-SWAP-BYTES 
;;      GL-UNPACK-LSB-FIRST 
;;      GL-UNPACK-ROW-LENGTH 
;;      GL-UNPACK-SKIP-ROWS 
;;      GL-UNPACK-SKIP-PIXELS 
;;      GL-UNPACK-ALIGNMENT 
;;      GL-PACK-SWAP-BYTES 
;;      GL-PACK-LSB-FIRST 
;;      GL-PACK-ROW-LENGTH 
;;      GL-PACK-SKIP-ROWS 
;;      GL-PACK-SKIP-PIXELS 
;;      GL-PACK-ALIGNMENT 
;;      GL-PACK-SKIP-IMAGES 
;;      GL-PACK-IMAGE-HEIGHT 
;;      GL-UNPACK-SKIP-IMAGES 
;;      GL-UNPACK-IMAGE-HEIGHT 

;; PixelTransfer 
;;      GL-MAP-COLOR 
;;      GL-MAP-STENCIL 
;;      GL-INDEX-SHIFT 
;;      GL-INDEX-OFFSET 
;;      GL-RED-SCALE 
;;      GL-RED-BIAS 
;;      GL-GREEN-SCALE 
;;      GL-GREEN-BIAS 
;;      GL-BLUE-SCALE 
;;      GL-BLUE-BIAS 
;;      GL-ALPHA-SCALE 
;;      GL-ALPHA-BIAS 
;;      GL-DEPTH-SCALE 
;;      GL-DEPTH-BIAS 
;;      GL-POST-CONVOLUTION-RED-SCALE 
;;      GL-POST-CONVOLUTION-GREEN-SCALE 
;;      GL-POST-CONVOLUTION-BLUE-SCALE 
;;      GL-POST-CONVOLUTION-ALPHA-SCALE 
;;      GL-POST-CONVOLUTION-RED-BIAS 
;;      GL-POST-CONVOLUTION-GREEN-BIAS 
;;      GL-POST-CONVOLUTION-BLUE-BIAS 
;;      GL-POST-CONVOLUTION-ALPHA-BIAS 
;;      GL-POST-COLOR-MATRIX-RED-SCALE 
;;      GL-POST-COLOR-MATRIX-GREEN-SCALE 
;;      GL-POST-COLOR-MATRIX-BLUE-SCALE 
;;      GL-POST-COLOR-MATRIX-ALPHA-SCALE 
;;      GL-POST-COLOR-MATRIX-RED-BIAS 
;;      GL-POST-COLOR-MATRIX-GREEN-BIAS 
;;      GL-POST-COLOR-MATRIX-BLUE-BIAS 
;;      GL-POST-COLOR-MATRIX-ALPHA-BIAS 

;; PixelType 
(defconstant *GL-BITMAP*                           #x1A00)
;;      GL-BYTE 
;;      GL-UNSIGNED-BYTE 
;;      GL-SHORT 
;;      GL-UNSIGNED-SHORT 
;;      GL-INT 
;;      GL-UNSIGNED-INT 
;;      GL-FLOAT 
;;      GL-BGR 
;;      GL-BGRA 
;;      GL-UNSIGNED-BYTE-3-3-2 
;;      GL-UNSIGNED-SHORT-4-4-4-4 
;;      GL-UNSIGNED-SHORT-5-5-5-1 
;;      GL-UNSIGNED-INT-8-8-8-8 
;;      GL-UNSIGNED-INT-10-10-10-2 
;;      GL-UNSIGNED-SHORT-5-6-5 
;;      GL-UNSIGNED-BYTE-2-3-3-REV 
;;      GL-UNSIGNED-SHORT-5-6-5-REV 
;;      GL-UNSIGNED-SHORT-4-4-4-4-REV 
;;      GL-UNSIGNED-SHORT-1-5-5-5-REV 
;;      GL-UNSIGNED-INT-8-8-8-8-REV 
;;      GL-UNSIGNED-INT-2-10-10-10-REV 

;; PolygonMode 
(defconstant *GL-POINT*                            #x1B00)
(defconstant *GL-LINE*                             #x1B01)
(defconstant *GL-FILL*                             #x1B02)

;; ReadBufferMode 
;;      GL-FRONT-LEFT 
;;      GL-FRONT-RIGHT 
;;      GL-BACK-LEFT 
;;      GL-BACK-RIGHT 
;;      GL-FRONT 
;;      GL-BACK 
;;      GL-LEFT 
;;      GL-RIGHT 
;;      GL-AUX0 
;;      GL-AUX1 
;;      GL-AUX2 
;;      GL-AUX3 

;; RenderingMode 
(defconstant *GL-RENDER*                           #x1C00)
(defconstant *GL-FEEDBACK*                         #x1C01)
(defconstant *GL-SELECT*                           #x1C02)

;; SeparableTarget 
;;      GL-SEPARABLE-2D 

;; ShadingModel 
(defconstant *GL-FLAT*                             #x1D00)
(defconstant *GL-SMOOTH*                           #x1D01)

;; StencilFunction 
;;      GL-NEVER 
;;      GL-LESS 
;;      GL-EQUAL 
;;      GL-LEQUAL 
;;      GL-GREATER 
;;      GL-NOTEQUAL 
;;      GL-GEQUAL 
;;      GL-ALWAYS 

;; StencilOp 
;;      GL-ZERO 
(defconstant *GL-KEEP*                             #x1E00)
(defconstant *GL-REPLACE*                          #x1E01)
(defconstant *GL-INCR*                             #x1E02)
(defconstant *GL-DECR*                             #x1E03)
;;      GL-INVERT 

;; StringName 
(defconstant *GL-VENDOR*                           #x1F00)
(defconstant *GL-RENDERER*                         #x1F01)
(defconstant *GL-VERSION*                          #x1F02)
(defconstant *GL-EXTENSIONS*                       #x1F03)

;; TextureCoordName 
(defconstant *GL-S*                                #x2000)
(defconstant *GL-T*                                #x2001)
(defconstant *GL-R*                                #x2002)
(defconstant *GL-Q*                                #x2003)

;; TexCoordPointerType 
;;      GL-SHORT 
;;      GL-INT 
;;      GL-FLOAT 
;;      GL-DOUBLE 

;; TextureEnvMode 
(defconstant *GL-MODULATE*                         #x2100)
(defconstant *GL-DECAL*                            #x2101)
;;      GL-BLEND 
;;      GL-REPLACE 

;; TextureEnvParameter 
(defconstant *GL-TEXTURE-ENV-MODE*                 #x2200)
(defconstant *GL-TEXTURE-ENV-COLOR*                #x2201)

;; TextureEnvTarget 
(defconstant *GL-TEXTURE-ENV*                      #x2300)

;; TextureGenMode 
(defconstant *GL-EYE-LINEAR*                       #x2400)
(defconstant *GL-OBJECT-LINEAR*                    #x2401)
(defconstant *GL-SPHERE-MAP*                       #x2402)

;; TextureGenParameter 
(defconstant *GL-TEXTURE-GEN-MODE*                 #x2500)
(defconstant *GL-OBJECT-PLANE*                     #x2501)
(defconstant *GL-EYE-PLANE*                        #x2502)

;; TextureMagFilter 
(defconstant *GL-NEAREST*                          #x2600)
(defconstant *GL-LINEAR*                           #x2601)

;; TextureMinFilter 
;;      GL-NEAREST 
;;      GL-LINEAR 
(defconstant *GL-NEAREST-MIPMAP-NEAREST*           #x2700)
(defconstant *GL-LINEAR-MIPMAP-NEAREST*            #x2701)
(defconstant *GL-NEAREST-MIPMAP-LINEAR*            #x2702)
(defconstant *GL-LINEAR-MIPMAP-LINEAR*             #x2703)

;; TextureParameterName 
(defconstant *GL-TEXTURE-MAG-FILTER*               #x2800)
(defconstant *GL-TEXTURE-MIN-FILTER*               #x2801)
(defconstant *GL-TEXTURE-WRAP-S*                   #x2802)
(defconstant *GL-TEXTURE-WRAP-T*                   #x2803)
;;      GL-TEXTURE-BORDER-COLOR 
;;      GL-TEXTURE-PRIORITY 
;;      GL-TEXTURE-WRAP-R 
;;      GL-TEXTURE-MIN-LOD 
;;      GL-TEXTURE-MAX-LOD 
;;      GL-TEXTURE-BASE-LEVEL 
;;      GL-TEXTURE-MAX-LEVEL 

;; TextureTarget 
;;      GL-TEXTURE-1D 
;;      GL-TEXTURE-2D 
;;      GL-PROXY-TEXTURE-1D 
;;      GL-PROXY-TEXTURE-2D 
;;      GL-TEXTURE-3D 
;;      GL-PROXY-TEXTURE-3D 

;; TextureWrapMode 
(defconstant *GL-CLAMP*                            #x2900)
(defconstant *GL-REPEAT*                           #x2901)
;;      GL-CLAMP-TO-EDGE 

;; VertexPointerType 
;;      GL-SHORT 
;;      GL-INT 
;;      GL-FLOAT 
;;      GL-DOUBLE 

;; ClientAttribMask 
(defconstant *GL-CLIENT-PIXEL-STORE-BIT*           #x00000001)
(defconstant *GL-CLIENT-VERTEX-ARRAY-BIT*          #x00000002)
(defconstant *GL-CLIENT-ALL-ATTRIB-BITS*           #xffffffff)

;; polygon-offset 
(defconstant *GL-POLYGON-OFFSET-FACTOR*            #x8038)
(defconstant *GL-POLYGON-OFFSET-UNITS*             #x2A00)
(defconstant *GL-POLYGON-OFFSET-POINT*             #x2A01)
(defconstant *GL-POLYGON-OFFSET-LINE*              #x2A02)
(defconstant *GL-POLYGON-OFFSET-FILL*              #x8037)

;; texture 
(defconstant *GL-ALPHA4*                           #x803B)
(defconstant *GL-ALPHA8*                           #x803C)
(defconstant *GL-ALPHA12*                          #x803D)
(defconstant *GL-ALPHA16*                          #x803E)
(defconstant *GL-LUMINANCE4*                       #x803F)
(defconstant *GL-LUMINANCE8*                       #x8040)
(defconstant *GL-LUMINANCE12*                      #x8041)
(defconstant *GL-LUMINANCE16*                      #x8042)
(defconstant *GL-LUMINANCE4-ALPHA4*                #x8043)
(defconstant *GL-LUMINANCE6-ALPHA2*                #x8044)
(defconstant *GL-LUMINANCE8-ALPHA8*                #x8045)
(defconstant *GL-LUMINANCE12-ALPHA4*               #x8046)
(defconstant *GL-LUMINANCE12-ALPHA12*              #x8047)
(defconstant *GL-LUMINANCE16-ALPHA16*              #x8048)
(defconstant *GL-INTENSITY*                        #x8049)
(defconstant *GL-INTENSITY4*                       #x804A)
(defconstant *GL-INTENSITY8*                       #x804B)
(defconstant *GL-INTENSITY12*                      #x804C)
(defconstant *GL-INTENSITY16*                      #x804D)
(defconstant *GL-R3-G3-B2*                         #x2A10)
(defconstant *GL-RGB4*                             #x804F)
(defconstant *GL-RGB5*                             #x8050)
(defconstant *GL-RGB8*                             #x8051)
(defconstant *GL-RGB10*                            #x8052)
(defconstant *GL-RGB12*                            #x8053)
(defconstant *GL-RGB16*                            #x8054)
(defconstant *GL-RGBA2*                            #x8055)
(defconstant *GL-RGBA4*                            #x8056)
(defconstant *GL-RGB5-A1*                          #x8057)
(defconstant *GL-RGBA8*                            #x8058)
(defconstant *GL-RGB10-A2*                         #x8059)
(defconstant *GL-RGBA12*                           #x805A)
(defconstant *GL-RGBA16*                           #x805B)
(defconstant *GL-TEXTURE-RED-SIZE*                 #x805C)
(defconstant *GL-TEXTURE-GREEN-SIZE*               #x805D)
(defconstant *GL-TEXTURE-BLUE-SIZE*                #x805E)
(defconstant *GL-TEXTURE-ALPHA-SIZE*               #x805F)
(defconstant *GL-TEXTURE-LUMINANCE-SIZE*           #x8060)
(defconstant *GL-TEXTURE-INTENSITY-SIZE*           #x8061)
(defconstant *GL-PROXY-TEXTURE-1D*                 #x8063)
(defconstant *GL-PROXY-TEXTURE-2D*                 #x8064)

;; texture-object 
(defconstant *GL-TEXTURE-PRIORITY*                 #x8066)
(defconstant *GL-TEXTURE-RESIDENT*                 #x8067)
(defconstant *GL-TEXTURE-BINDING-1D*               #x8068)
(defconstant *GL-TEXTURE-BINDING-2D*               #x8069)
(defconstant *GL-TEXTURE-BINDING-3D*               #x806A)

;; vertex-array 
(defconstant *GL-VERTEX-ARRAY*                     #x8074)
(defconstant *GL-NORMAL-ARRAY*                     #x8075)
(defconstant *GL-COLOR-ARRAY*                      #x8076)
(defconstant *GL-INDEX-ARRAY*                      #x8077)
(defconstant *GL-TEXTURE-COORD-ARRAY*              #x8078)
(defconstant *GL-EDGE-FLAG-ARRAY*                  #x8079)
(defconstant *GL-VERTEX-ARRAY-SIZE*                #x807A)
(defconstant *GL-VERTEX-ARRAY-TYPE*                #x807B)
(defconstant *GL-VERTEX-ARRAY-STRIDE*              #x807C)
(defconstant *GL-NORMAL-ARRAY-TYPE*                #x807E)
(defconstant *GL-NORMAL-ARRAY-STRIDE*              #x807F)
(defconstant *GL-COLOR-ARRAY-SIZE*                 #x8081)
(defconstant *GL-COLOR-ARRAY-TYPE*                 #x8082)
(defconstant *GL-COLOR-ARRAY-STRIDE*               #x8083)
(defconstant *GL-INDEX-ARRAY-TYPE*                 #x8085)
(defconstant *GL-INDEX-ARRAY-STRIDE*               #x8086)
(defconstant *GL-TEXTURE-COORD-ARRAY-SIZE*         #x8088)
(defconstant *GL-TEXTURE-COORD-ARRAY-TYPE*         #x8089)
(defconstant *GL-TEXTURE-COORD-ARRAY-STRIDE*       #x808A)
(defconstant *GL-EDGE-FLAG-ARRAY-STRIDE*           #x808C)
(defconstant *GL-VERTEX-ARRAY-POINTER*             #x808E)
(defconstant *GL-NORMAL-ARRAY-POINTER*             #x808F)
(defconstant *GL-COLOR-ARRAY-POINTER*              #x8090)
(defconstant *GL-INDEX-ARRAY-POINTER*              #x8091)
(defconstant *GL-TEXTURE-COORD-ARRAY-POINTER*      #x8092)
(defconstant *GL-EDGE-FLAG-ARRAY-POINTER*          #x8093)
(defconstant *GL-V2F*                              #x2A20)
(defconstant *GL-V3F*                              #x2A21)
(defconstant *GL-C4UB-V2F*                         #x2A22)
(defconstant *GL-C4UB-V3F*                         #x2A23)
(defconstant *GL-C3F-V3F*                          #x2A24)
(defconstant *GL-N3F-V3F*                          #x2A25)
(defconstant *GL-C4F-N3F-V3F*                      #x2A26)
(defconstant *GL-T2F-V3F*                          #x2A27)
(defconstant *GL-T4F-V4F*                          #x2A28)
(defconstant *GL-T2F-C4UB-V3F*                     #x2A29)
(defconstant *GL-T2F-C3F-V3F*                      #x2A2A)
(defconstant *GL-T2F-N3F-V3F*                      #x2A2B)
(defconstant *GL-T2F-C4F-N3F-V3F*                  #x2A2C)
(defconstant *GL-T4F-C4F-N3F-V4F*                  #x2A2D)

;; bgra 
(defconstant *GL-BGR*                              #x80E0)
(defconstant *GL-BGRA*                             #x80E1)

;; blend-color 
(defconstant *GL-CONSTANT-COLOR*                   #x8001)
(defconstant *GL-ONE-MINUS-CONSTANT-COLOR*         #x8002)
(defconstant *GL-CONSTANT-ALPHA*                   #x8003)
(defconstant *GL-ONE-MINUS-CONSTANT-ALPHA*         #x8004)
(defconstant *GL-BLEND-COLOR*                      #x8005)

;; blend-minmax 
(defconstant *GL-FUNC-ADD*                         #x8006)
(defconstant *GL-MIN*                              #x8007)
(defconstant *GL-MAX*                              #x8008)
(defconstant *GL-BLEND-EQUATION*                   #x8009)

;; blend-equation-separate 
(defconstant *GL-BLEND-EQUATION-RGB*               #x8009)
(defconstant *GL-BLEND-EQUATION-ALPHA*             #x883D)

;; blend-subtract 
(defconstant *GL-FUNC-SUBTRACT*                    #x800A)
(defconstant *GL-FUNC-REVERSE-SUBTRACT*            #x800B)

;; color-matrix 
(defconstant *GL-COLOR-MATRIX*                     #x80B1)
(defconstant *GL-COLOR-MATRIX-STACK-DEPTH*         #x80B2)
(defconstant *GL-MAX-COLOR-MATRIX-STACK-DEPTH*     #x80B3)
(defconstant *GL-POST-COLOR-MATRIX-RED-SCALE*      #x80B4)
(defconstant *GL-POST-COLOR-MATRIX-GREEN-SCALE*    #x80B5)
(defconstant *GL-POST-COLOR-MATRIX-BLUE-SCALE*     #x80B6)
(defconstant *GL-POST-COLOR-MATRIX-ALPHA-SCALE*    #x80B7)
(defconstant *GL-POST-COLOR-MATRIX-RED-BIAS*       #x80B8)
(defconstant *GL-POST-COLOR-MATRIX-GREEN-BIAS*     #x80B9)
(defconstant *GL-POST-COLOR-MATRIX-BLUE-BIAS*      #x80BA)
(defconstant *GL-POST-COLOR-MATRIX-ALPHA-BIAS*     #x80BB)

;; color-table 
(defconstant *GL-COLOR-TABLE*                      #x80D0)
(defconstant *GL-POST-CONVOLUTION-COLOR-TABLE*     #x80D1)
(defconstant *GL-POST-COLOR-MATRIX-COLOR-TABLE*    #x80D2)
(defconstant *GL-PROXY-COLOR-TABLE*                #x80D3)
(defconstant *GL-PROXY-POST-CONVOLUTION-COLOR-TABLE* #x80D4)
(defconstant *GL-PROXY-POST-COLOR-MATRIX-COLOR-TABLE* #x80D5)
(defconstant *GL-COLOR-TABLE-SCALE*                #x80D6)
(defconstant *GL-COLOR-TABLE-BIAS*                 #x80D7)
(defconstant *GL-COLOR-TABLE-FORMAT*               #x80D8)
(defconstant *GL-COLOR-TABLE-WIDTH*                #x80D9)
(defconstant *GL-COLOR-TABLE-RED-SIZE*             #x80DA)
(defconstant *GL-COLOR-TABLE-GREEN-SIZE*           #x80DB)
(defconstant *GL-COLOR-TABLE-BLUE-SIZE*            #x80DC)
(defconstant *GL-COLOR-TABLE-ALPHA-SIZE*           #x80DD)
(defconstant *GL-COLOR-TABLE-LUMINANCE-SIZE*       #x80DE)
(defconstant *GL-COLOR-TABLE-INTENSITY-SIZE*       #x80DF)

;; convolution 
(defconstant *GL-CONVOLUTION-1D*                   #x8010)
(defconstant *GL-CONVOLUTION-2D*                   #x8011)
(defconstant *GL-SEPARABLE-2D*                     #x8012)
(defconstant *GL-CONVOLUTION-BORDER-MODE*          #x8013)
(defconstant *GL-CONVOLUTION-FILTER-SCALE*         #x8014)
(defconstant *GL-CONVOLUTION-FILTER-BIAS*          #x8015)
(defconstant *GL-REDUCE*                           #x8016)
(defconstant *GL-CONVOLUTION-FORMAT*               #x8017)
(defconstant *GL-CONVOLUTION-WIDTH*                #x8018)
(defconstant *GL-CONVOLUTION-HEIGHT*               #x8019)
(defconstant *GL-MAX-CONVOLUTION-WIDTH*            #x801A)
(defconstant *GL-MAX-CONVOLUTION-HEIGHT*           #x801B)
(defconstant *GL-POST-CONVOLUTION-RED-SCALE*       #x801C)
(defconstant *GL-POST-CONVOLUTION-GREEN-SCALE*     #x801D)
(defconstant *GL-POST-CONVOLUTION-BLUE-SCALE*      #x801E)
(defconstant *GL-POST-CONVOLUTION-ALPHA-SCALE*     #x801F)
(defconstant *GL-POST-CONVOLUTION-RED-BIAS*        #x8020)
(defconstant *GL-POST-CONVOLUTION-GREEN-BIAS*      #x8021)
(defconstant *GL-POST-CONVOLUTION-BLUE-BIAS*       #x8022)
(defconstant *GL-POST-CONVOLUTION-ALPHA-BIAS*      #x8023)
(defconstant *GL-CONSTANT-BORDER*                  #x8151)
(defconstant *GL-REPLICATE-BORDER*                 #x8153)
(defconstant *GL-CONVOLUTION-BORDER-COLOR*         #x8154)

;; draw-range-elements 
(defconstant *GL-MAX-ELEMENTS-VERTICES*            #x80E8)
(defconstant *GL-MAX-ELEMENTS-INDICES*             #x80E9)

;; histogram 
(defconstant *GL-HISTOGRAM*                        #x8024)
(defconstant *GL-PROXY-HISTOGRAM*                  #x8025)
(defconstant *GL-HISTOGRAM-WIDTH*                  #x8026)
(defconstant *GL-HISTOGRAM-FORMAT*                 #x8027)
(defconstant *GL-HISTOGRAM-RED-SIZE*               #x8028)
(defconstant *GL-HISTOGRAM-GREEN-SIZE*             #x8029)
(defconstant *GL-HISTOGRAM-BLUE-SIZE*              #x802A)
(defconstant *GL-HISTOGRAM-ALPHA-SIZE*             #x802B)
(defconstant *GL-HISTOGRAM-LUMINANCE-SIZE*         #x802C)
(defconstant *GL-HISTOGRAM-SINK*                   #x802D)
(defconstant *GL-MINMAX*                           #x802E)
(defconstant *GL-MINMAX-FORMAT*                    #x802F)
(defconstant *GL-MINMAX-SINK*                      #x8030)
(defconstant *GL-TABLE-TOO-LARGE*                  #x8031)

;; packed-pixels 
(defconstant *GL-UNSIGNED-BYTE-3-3-2*              #x8032)
(defconstant *GL-UNSIGNED-SHORT-4-4-4-4*           #x8033)
(defconstant *GL-UNSIGNED-SHORT-5-5-5-1*           #x8034)
(defconstant *GL-UNSIGNED-INT-8-8-8-8*             #x8035)
(defconstant *GL-UNSIGNED-INT-10-10-10-2*          #x8036)
(defconstant *GL-UNSIGNED-BYTE-2-3-3-REV*          #x8362)
(defconstant *GL-UNSIGNED-SHORT-5-6-5*             #x8363)
(defconstant *GL-UNSIGNED-SHORT-5-6-5-REV*         #x8364)
(defconstant *GL-UNSIGNED-SHORT-4-4-4-4-REV*       #x8365)
(defconstant *GL-UNSIGNED-SHORT-1-5-5-5-REV*       #x8366)
(defconstant *GL-UNSIGNED-INT-8-8-8-8-REV*         #x8367)
(defconstant *GL-UNSIGNED-INT-2-10-10-10-REV*      #x8368)

;; rescale-normal 
(defconstant *GL-RESCALE-NORMAL*                   #x803A)

;; separate-specular-color 
(defconstant *GL-LIGHT-MODEL-COLOR-CONTROL*        #x81F8)
(defconstant *GL-SINGLE-COLOR*                     #x81F9)
(defconstant *GL-SEPARATE-SPECULAR-COLOR*          #x81FA)

;; texture3D 
(defconstant *GL-PACK-SKIP-IMAGES*                 #x806B)
(defconstant *GL-PACK-IMAGE-HEIGHT*                #x806C)
(defconstant *GL-UNPACK-SKIP-IMAGES*               #x806D)
(defconstant *GL-UNPACK-IMAGE-HEIGHT*              #x806E)
(defconstant *GL-TEXTURE-3D*                       #x806F)
(defconstant *GL-PROXY-TEXTURE-3D*                 #x8070)
(defconstant *GL-TEXTURE-DEPTH*                    #x8071)
(defconstant *GL-TEXTURE-WRAP-R*                   #x8072)
(defconstant *GL-MAX-3D-TEXTURE-SIZE*              #x8073)

;; texture-edge-clamp 
(defconstant *GL-CLAMP-TO-EDGE*                    #x812F)
(defconstant *GL-CLAMP-TO-BORDER*                  #x812D)

;; texture-lod 
(defconstant *GL-TEXTURE-MIN-LOD*                  #x813A)
(defconstant *GL-TEXTURE-MAX-LOD*                  #x813B)
(defconstant *GL-TEXTURE-BASE-LEVEL*               #x813C)
(defconstant *GL-TEXTURE-MAX-LEVEL*                #x813D)

;; GetTarget1-2 
(defconstant *GL-SMOOTH-POINT-SIZE-RANGE*          #x0B12)
(defconstant *GL-SMOOTH-POINT-SIZE-GRANULARITY*    #x0B13)
(defconstant *GL-SMOOTH-LINE-WIDTH-RANGE*          #x0B22)
(defconstant *GL-SMOOTH-LINE-WIDTH-GRANULARITY*    #x0B23)
(defconstant *GL-ALIASED-POINT-SIZE-RANGE*         #x846D)
(defconstant *GL-ALIASED-LINE-WIDTH-RANGE*         #x846E)

(defconstant *GL-TEXTURE0*                         #x84C0)
(defconstant *GL-TEXTURE1*                         #x84C1)
(defconstant *GL-TEXTURE2*                         #x84C2)
(defconstant *GL-TEXTURE3*                         #x84C3)
(defconstant *GL-TEXTURE4*                         #x84C4)
(defconstant *GL-TEXTURE5*                         #x84C5)
(defconstant *GL-TEXTURE6*                         #x84C6)
(defconstant *GL-TEXTURE7*                         #x84C7)
(defconstant *GL-TEXTURE8*                         #x84C8)
(defconstant *GL-TEXTURE9*                         #x84C9)
(defconstant *GL-TEXTURE10*                        #x84CA)
(defconstant *GL-TEXTURE11*                        #x84CB)
(defconstant *GL-TEXTURE12*                        #x84CC)
(defconstant *GL-TEXTURE13*                        #x84CD)
(defconstant *GL-TEXTURE14*                        #x84CE)
(defconstant *GL-TEXTURE15*                        #x84CF)
(defconstant *GL-TEXTURE16*                        #x84D0)
(defconstant *GL-TEXTURE17*                        #x84D1)
(defconstant *GL-TEXTURE18*                        #x84D2)
(defconstant *GL-TEXTURE19*                        #x84D3)
(defconstant *GL-TEXTURE20*                        #x84D4)
(defconstant *GL-TEXTURE21*                        #x84D5)
(defconstant *GL-TEXTURE22*                        #x84D6)
(defconstant *GL-TEXTURE23*                        #x84D7)
(defconstant *GL-TEXTURE24*                        #x84D8)
(defconstant *GL-TEXTURE25*                        #x84D9)
(defconstant *GL-TEXTURE26*                        #x84DA)
(defconstant *GL-TEXTURE27*                        #x84DB)
(defconstant *GL-TEXTURE28*                        #x84DC)
(defconstant *GL-TEXTURE29*                        #x84DD)
(defconstant *GL-TEXTURE30*                        #x84DE)
(defconstant *GL-TEXTURE31*                        #x84DF)
(defconstant *GL-ACTIVE-TEXTURE*                   #x84E0)
(defconstant *GL-CLIENT-ACTIVE-TEXTURE*            #x84E1)
(defconstant *GL-MAX-TEXTURE-UNITS*                #x84E2)

(defconstant *GL-COMBINE*                          #x8570)
(defconstant *GL-COMBINE-RGB*                      #x8571)
(defconstant *GL-COMBINE-ALPHA*                    #x8572)
(defconstant *GL-RGB-SCALE*                        #x8573)
(defconstant *GL-ADD-SIGNED*                       #x8574)
(defconstant *GL-INTERPOLATE*                      #x8575)
(defconstant *GL-CONSTANT*                         #x8576)
(defconstant *GL-PRIMARY-COLOR*                    #x8577)
(defconstant *GL-PREVIOUS*                         #x8578)
(defconstant *GL-SUBTRACT*                         #x84E7)

(defconstant *GL-SRC0-RGB*                         #x8580)
(defconstant *GL-SRC1-RGB*                         #x8581)
(defconstant *GL-SRC2-RGB*                         #x8582)
(defconstant *GL-SRC3-RGB*                         #x8583)
(defconstant *GL-SRC4-RGB*                         #x8584)
(defconstant *GL-SRC5-RGB*                         #x8585)
(defconstant *GL-SRC6-RGB*                         #x8586)
(defconstant *GL-SRC7-RGB*                         #x8587)
(defconstant *GL-SRC0-ALPHA*                       #x8588)
(defconstant *GL-SRC1-ALPHA*                       #x8589)
(defconstant *GL-SRC2-ALPHA*                       #x858A)
(defconstant *GL-SRC3-ALPHA*                       #x858B)
(defconstant *GL-SRC4-ALPHA*                       #x858C)
(defconstant *GL-SRC5-ALPHA*                       #x858D)
(defconstant *GL-SRC6-ALPHA*                       #x858E)
(defconstant *GL-SRC7-ALPHA*                       #x858F)

;; Obsolete 
(defconstant *GL-SOURCE0-RGB*                      #x8580)
(defconstant *GL-SOURCE1-RGB*                      #x8581)
(defconstant *GL-SOURCE2-RGB*                      #x8582)
(defconstant *GL-SOURCE3-RGB*                      #x8583)
(defconstant *GL-SOURCE4-RGB*                      #x8584)
(defconstant *GL-SOURCE5-RGB*                      #x8585)
(defconstant *GL-SOURCE6-RGB*                      #x8586)
(defconstant *GL-SOURCE7-RGB*                      #x8587)
(defconstant *GL-SOURCE0-ALPHA*                    #x8588)
(defconstant *GL-SOURCE1-ALPHA*                    #x8589)
(defconstant *GL-SOURCE2-ALPHA*                    #x858A)
(defconstant *GL-SOURCE3-ALPHA*                    #x858B)
(defconstant *GL-SOURCE4-ALPHA*                    #x858C)
(defconstant *GL-SOURCE5-ALPHA*                    #x858D)
(defconstant *GL-SOURCE6-ALPHA*                    #x858E)
(defconstant *GL-SOURCE7-ALPHA*                    #x858F)

(defconstant *GL-OPERAND0-RGB*                     #x8590)
(defconstant *GL-OPERAND1-RGB*                     #x8591)
(defconstant *GL-OPERAND2-RGB*                     #x8592)
(defconstant *GL-OPERAND3-RGB*                     #x8593)
(defconstant *GL-OPERAND4-RGB*                     #x8594)
(defconstant *GL-OPERAND5-RGB*                     #x8595)
(defconstant *GL-OPERAND6-RGB*                     #x8596)
(defconstant *GL-OPERAND7-RGB*                     #x8597)
(defconstant *GL-OPERAND0-ALPHA*                   #x8598)
(defconstant *GL-OPERAND1-ALPHA*                   #x8599)
(defconstant *GL-OPERAND2-ALPHA*                   #x859A)
(defconstant *GL-OPERAND3-ALPHA*                   #x859B)
(defconstant *GL-OPERAND4-ALPHA*                   #x859C)
(defconstant *GL-OPERAND5-ALPHA*                   #x859D)
(defconstant *GL-OPERAND6-ALPHA*                   #x859E)
(defconstant *GL-OPERAND7-ALPHA*                   #x859F)

(defconstant *GL-DOT3-RGB*                         #x86AE)
(defconstant *GL-DOT3-RGBA*                        #x86AF)

(defconstant *GL-TRANSPOSE-MODELVIEW-MATRIX*       #x84E3)
(defconstant *GL-TRANSPOSE-PROJECTION-MATRIX*      #x84E4)
(defconstant *GL-TRANSPOSE-TEXTURE-MATRIX*         #x84E5)
(defconstant *GL-TRANSPOSE-COLOR-MATRIX*           #x84E6)

(defconstant *GL-NORMAL-MAP*                       #x8511)
(defconstant *GL-REFLECTION-MAP*                   #x8512)
(defconstant *GL-TEXTURE-CUBE-MAP*                 #x8513)
(defconstant *GL-TEXTURE-BINDING-CUBE-MAP*         #x8514)
(defconstant *GL-TEXTURE-CUBE-MAP-POSITIVE-X*      #x8515)
(defconstant *GL-TEXTURE-CUBE-MAP-NEGATIVE-X*      #x8516)
(defconstant *GL-TEXTURE-CUBE-MAP-POSITIVE-Y*      #x8517)
(defconstant *GL-TEXTURE-CUBE-MAP-NEGATIVE-Y*      #x8518)
(defconstant *GL-TEXTURE-CUBE-MAP-POSITIVE-Z*      #x8519)
(defconstant *GL-TEXTURE-CUBE-MAP-NEGATIVE-Z*      #x851A)
(defconstant *GL-PROXY-TEXTURE-CUBE-MAP*           #x851B)
(defconstant *GL-MAX-CUBE-MAP-TEXTURE-SIZE*        #x851C)

(defconstant *GL-COMPRESSED-ALPHA*                 #x84E9)
(defconstant *GL-COMPRESSED-LUMINANCE*             #x84EA)
(defconstant *GL-COMPRESSED-LUMINANCE-ALPHA*       #x84EB)
(defconstant *GL-COMPRESSED-INTENSITY*             #x84EC)
(defconstant *GL-COMPRESSED-RGB*                   #x84ED)
(defconstant *GL-COMPRESSED-RGBA*                  #x84EE)
(defconstant *GL-TEXTURE-COMPRESSION-HINT*         #x84EF)
(defconstant *GL-TEXTURE-COMPRESSED-IMAGE-SIZE*    #x86A0)
(defconstant *GL-TEXTURE-COMPRESSED*               #x86A1)
(defconstant *GL-NUM-COMPRESSED-TEXTURE-FORMATS*   #x86A2)
(defconstant *GL-COMPRESSED-TEXTURE-FORMATS*       #x86A3)

(defconstant *GL-MULTISAMPLE*                      #x809D)
(defconstant *GL-SAMPLE-ALPHA-TO-COVERAGE*         #x809E)
(defconstant *GL-SAMPLE-ALPHA-TO-ONE*              #x809F)
(defconstant *GL-SAMPLE-COVERAGE*                  #x80A0)
(defconstant *GL-SAMPLE-BUFFERS*                   #x80A8)
(defconstant *GL-SAMPLES*                          #x80A9)
(defconstant *GL-SAMPLE-COVERAGE-VALUE*            #x80AA)
(defconstant *GL-SAMPLE-COVERAGE-INVERT*           #x80AB)
(defconstant *GL-MULTISAMPLE-BIT*                  #x20000000)

(defconstant *GL-DEPTH-COMPONENT16*                #x81A5)
(defconstant *GL-DEPTH-COMPONENT24*                #x81A6)
(defconstant *GL-DEPTH-COMPONENT32*                #x81A7)
(defconstant *GL-TEXTURE-DEPTH-SIZE*               #x884A)
(defconstant *GL-DEPTH-TEXTURE-MODE*               #x884B)

(defconstant *GL-TEXTURE-COMPARE-MODE*             #x884C)
(defconstant *GL-TEXTURE-COMPARE-FUNC*             #x884D)
(defconstant *GL-COMPARE-R-TO-TEXTURE*             #x884E)

;; occlusion-query 
(defconstant *GL-QUERY-COUNTER-BITS*               #x8864)
(defconstant *GL-CURRENT-QUERY*                    #x8865)
(defconstant *GL-QUERY-RESULT*                     #x8866)
(defconstant *GL-QUERY-RESULT-AVAILABLE*           #x8867)
(defconstant *GL-SAMPLES-PASSED*                   #x8914)

(defconstant *GL-FOG-COORD-SRC*                    #x8450)
(defconstant *GL-FOG-COORD*                        #x8451)
(defconstant *GL-FRAGMENT-DEPTH*                   #x8452)
(defconstant *GL-CURRENT-FOG-COORD*                #x8453)
(defconstant *GL-FOG-COORD-ARRAY-TYPE*             #x8454)
(defconstant *GL-FOG-COORD-ARRAY-STRIDE*           #x8455)
(defconstant *GL-FOG-COORD-ARRAY-POINTER*          #x8456)
(defconstant *GL-FOG-COORD-ARRAY*                  #x8457)

;; Obsolete 
(defconstant *GL-FOG-COORDINATE-SOURCE*            #x8450)
(defconstant *GL-FOG-COORDINATE*                   #x8451)
(defconstant *GL-CURRENT-FOG-COORDINATE*           #x8453)
(defconstant *GL-FOG-COORDINATE-ARRAY-TYPE*        #x8454)
(defconstant *GL-FOG-COORDINATE-ARRAY-STRIDE*      #x8455)
(defconstant *GL-FOG-COORDINATE-ARRAY-POINTER*     #x8456)
(defconstant *GL-FOG-COORDINATE-ARRAY*             #x8457)

(defconstant *GL-COLOR-SUM*                        #x8458)
(defconstant *GL-CURRENT-SECONDARY-COLOR*          #x8459)
(defconstant *GL-SECONDARY-COLOR-ARRAY-SIZE*       #x845A)
(defconstant *GL-SECONDARY-COLOR-ARRAY-TYPE*       #x845B)
(defconstant *GL-SECONDARY-COLOR-ARRAY-STRIDE*     #x845C)
(defconstant *GL-SECONDARY-COLOR-ARRAY-POINTER*    #x845D)
(defconstant *GL-SECONDARY-COLOR-ARRAY*            #x845E)

(defconstant *GL-POINT-SIZE-MIN*                   #x8126)
(defconstant *GL-POINT-SIZE-MAX*                   #x8127)
(defconstant *GL-POINT-FADE-THRESHOLD-SIZE*        #x8128)
(defconstant *GL-POINT-DISTANCE-ATTENUATION*       #x8129)

(defconstant *GL-BLEND-DST-RGB*                    #x80C8)
(defconstant *GL-BLEND-SRC-RGB*                    #x80C9)
(defconstant *GL-BLEND-DST-ALPHA*                  #x80CA)
(defconstant *GL-BLEND-SRC-ALPHA*                  #x80CB)

(defconstant *GL-GENERATE-MIPMAP*                  #x8191)
(defconstant *GL-GENERATE-MIPMAP-HINT*             #x8192)

(defconstant *GL-INCR-WRAP*                        #x8507)
(defconstant *GL-DECR-WRAP*                        #x8508)

(defconstant *GL-MIRRORED-REPEAT*                  #x8370)

(defconstant *GL-MAX-TEXTURE-LOD-BIAS*             #x84FD)
(defconstant *GL-TEXTURE-FILTER-CONTROL*           #x8500)
(defconstant *GL-TEXTURE-LOD-BIAS*                 #x8501)

;; vertex-buffer-object 
(defconstant *GL-ARRAY-BUFFER*                     #x8892)
(defconstant *GL-ELEMENT-ARRAY-BUFFER*             #x8893)
(defconstant *GL-ARRAY-BUFFER-BINDING*             #x8894)
(defconstant *GL-ELEMENT-ARRAY-BUFFER-BINDING*     #x8895)
(defconstant *GL-VERTEX-ARRAY-BUFFER-BINDING*      #x8896)
(defconstant *GL-NORMAL-ARRAY-BUFFER-BINDING*      #x8897)
(defconstant *GL-COLOR-ARRAY-BUFFER-BINDING*       #x8898)
(defconstant *GL-INDEX-ARRAY-BUFFER-BINDING*       #x8899)
(defconstant *GL-TEXTURE-COORD-ARRAY-BUFFER-BINDING* #x889A)
(defconstant *GL-EDGE-FLAG-ARRAY-BUFFER-BINDING*   #x889B)
(defconstant *GL-SECONDARY-COLOR-ARRAY-BUFFER-BINDING* #x889C)
(defconstant *GL-FOG-COORD-ARRAY-BUFFER-BINDING*   #x889D)
(defconstant *GL-WEIGHT-ARRAY-BUFFER-BINDING*      #x889E)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-BUFFER-BINDING* #x889F)
(defconstant *GL-STREAM-DRAW*                      #x88E0)
(defconstant *GL-STREAM-READ*                      #x88E1)
(defconstant *GL-STREAM-COPY*                      #x88E2)
(defconstant *GL-STATIC-DRAW*                      #x88E4)
(defconstant *GL-STATIC-READ*                      #x88E5)
(defconstant *GL-STATIC-COPY*                      #x88E6)
(defconstant *GL-DYNAMIC-DRAW*                     #x88E8)
(defconstant *GL-DYNAMIC-READ*                     #x88E9)
(defconstant *GL-DYNAMIC-COPY*                     #x88EA)
(defconstant *GL-READ-ONLY*                        #x88B8)
(defconstant *GL-WRITE-ONLY*                       #x88B9)
(defconstant *GL-READ-WRITE*                       #x88BA)
(defconstant *GL-BUFFER-SIZE*                      #x8764)
(defconstant *GL-BUFFER-USAGE*                     #x8765)
(defconstant *GL-BUFFER-ACCESS*                    #x88BB)
(defconstant *GL-BUFFER-MAPPED*                    #x88BC)
(defconstant *GL-BUFFER-MAP-POINTER*               #x88BD)
;; Obsolete 
(defconstant *GL-FOG-COORDINATE-ARRAY-BUFFER-BINDING* #x889D)

;; OpenGL 2.0 
(defconstant *GL-CURRENT-PROGRAM*                  #x8B8D)
(defconstant *GL-SHADER-TYPE*                      #x8B4F)
(defconstant *GL-DELETE-STATUS*                    #x8B80)
(defconstant *GL-COMPILE-STATUS*                   #x8B81)
(defconstant *GL-LINK-STATUS*                      #x8B82)
(defconstant *GL-VALIDATE-STATUS*                  #x8B83)
(defconstant *GL-INFO-LOG-LENGTH*                  #x8B84)
(defconstant *GL-ATTACHED-SHADERS*                 #x8B85)
(defconstant *GL-ACTIVE-UNIFORMS*                  #x8B86)
(defconstant *GL-ACTIVE-UNIFORM-MAX-LENGTH*        #x8B87)
(defconstant *GL-SHADER-SOURCE-LENGTH*             #x8B88)
(defconstant *GL-FLOAT-VEC2*                       #x8B50)
(defconstant *GL-FLOAT-VEC3*                       #x8B51)
(defconstant *GL-FLOAT-VEC4*                       #x8B52)
(defconstant *GL-INT-VEC2*                         #x8B53)
(defconstant *GL-INT-VEC3*                         #x8B54)
(defconstant *GL-INT-VEC4*                         #x8B55)
(defconstant *GL-BOOL*                             #x8B56)
(defconstant *GL-BOOL-VEC2*                        #x8B57)
(defconstant *GL-BOOL-VEC3*                        #x8B58)
(defconstant *GL-BOOL-VEC4*                        #x8B59)
(defconstant *GL-FLOAT-MAT2*                       #x8B5A)
(defconstant *GL-FLOAT-MAT3*                       #x8B5B)
(defconstant *GL-FLOAT-MAT4*                       #x8B5C)
(defconstant *GL-SAMPLER-1D*                       #x8B5D)
(defconstant *GL-SAMPLER-2D*                       #x8B5E)
(defconstant *GL-SAMPLER-3D*                       #x8B5F)
(defconstant *GL-SAMPLER-CUBE*                     #x8B60)
(defconstant *GL-SAMPLER-1D-SHADOW*                #x8B61)
(defconstant *GL-SAMPLER-2D-SHADOW*                #x8B62)
(defconstant *GL-SHADING-LANGUAGE-VERSION*         #x8B8C)
(defconstant *GL-VERTEX-SHADER*                    #x8B31)
(defconstant *GL-MAX-VERTEX-UNIFORM-COMPONENTS*    #x8B4A)
(defconstant *GL-MAX-VARYING-FLOATS*               #x8B4B)
(defconstant *GL-MAX-VERTEX-TEXTURE-IMAGE-UNITS*   #x8B4C)
(defconstant *GL-MAX-COMBINED-TEXTURE-IMAGE-UNITS* #x8B4D)
(defconstant *GL-ACTIVE-ATTRIBUTES*                #x8B89)
(defconstant *GL-ACTIVE-ATTRIBUTE-MAX-LENGTH*      #x8B8A)
(defconstant *GL-FRAGMENT-SHADER*                  #x8B30)
(defconstant *GL-MAX-FRAGMENT-UNIFORM-COMPONENTS*  #x8B49)
(defconstant *GL-FRAGMENT-SHADER-DERIVATIVE-HINT*  #x8B8B)
(defconstant *GL-MAX-VERTEX-ATTRIBS*               #x8869)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-ENABLED*      #x8622)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-SIZE*         #x8623)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-STRIDE*       #x8624)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-TYPE*         #x8625)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-NORMALIZED*   #x886A)
(defconstant *GL-CURRENT-VERTEX-ATTRIB*            #x8626)
(defconstant *GL-VERTEX-ATTRIB-ARRAY-POINTER*      #x8645)
(defconstant *GL-VERTEX-PROGRAM-POINT-SIZE*        #x8642)
(defconstant *GL-VERTEX-PROGRAM-TWO-SIDE*          #x8643)
(defconstant *GL-MAX-TEXTURE-COORDS*               #x8871)
(defconstant *GL-MAX-TEXTURE-IMAGE-UNITS*          #x8872)
(defconstant *GL-MAX-DRAW-BUFFERS*                 #x8824)
(defconstant *GL-DRAW-BUFFER0*                     #x8825)
(defconstant *GL-DRAW-BUFFER1*                     #x8826)
(defconstant *GL-DRAW-BUFFER2*                     #x8827)
(defconstant *GL-DRAW-BUFFER3*                     #x8828)
(defconstant *GL-DRAW-BUFFER4*                     #x8829)
(defconstant *GL-DRAW-BUFFER5*                     #x882A)
(defconstant *GL-DRAW-BUFFER6*                     #x882B)
(defconstant *GL-DRAW-BUFFER7*                     #x882C)
(defconstant *GL-DRAW-BUFFER8*                     #x882D)
(defconstant *GL-DRAW-BUFFER9*                     #x882E)
(defconstant *GL-DRAW-BUFFER10*                    #x882F)
(defconstant *GL-DRAW-BUFFER11*                    #x8830)
(defconstant *GL-DRAW-BUFFER12*                    #x8831)
(defconstant *GL-DRAW-BUFFER13*                    #x8832)
(defconstant *GL-DRAW-BUFFER14*                    #x8833)
(defconstant *GL-DRAW-BUFFER15*                    #x8834)
(defconstant *GL-POINT-SPRITE*                     #x8861)
(defconstant *GL-COORD-REPLACE*                    #x8862)
(defconstant *GL-POINT-SPRITE-COORD-ORIGIN*        #x8CA0)
(defconstant *GL-LOWER-LEFT*                       #x8CA1)
(defconstant *GL-UPPER-LEFT*                       #x8CA2)
(defconstant *GL-STENCIL-BACK-FUNC*                #x8800)
(defconstant *GL-STENCIL-BACK-VALUE-MASK*          #x8CA4)
(defconstant *GL-STENCIL-BACK-REF*                 #x8CA3)
(defconstant *GL-STENCIL-BACK-FAIL*                #x8801)
(defconstant *GL-STENCIL-BACK-PASS-DEPTH-FAIL*     #x8802)
(defconstant *GL-STENCIL-BACK-PASS-DEPTH-PASS*     #x8803)
(defconstant *GL-STENCIL-BACK-WRITEMASK*           #x8CA5)

;; OpenGL 2.1 
(defconstant *GL-CURRENT-RASTER-SECONDARY-COLOR*   #x845F)
(defconstant *GL-PIXEL-PACK-BUFFER*                #x88EB)
(defconstant *GL-PIXEL-UNPACK-BUFFER*              #x88EC)
(defconstant *GL-PIXEL-PACK-BUFFER-BINDING*        #x88ED)
(defconstant *GL-PIXEL-UNPACK-BUFFER-BINDING*      #x88EF)
(defconstant *GL-FLOAT-MAT2x3*                     #x8B65)
(defconstant *GL-FLOAT-MAT2x4*                     #x8B66)
(defconstant *GL-FLOAT-MAT3x2*                     #x8B67)
(defconstant *GL-FLOAT-MAT3x4*                     #x8B68)
(defconstant *GL-FLOAT-MAT4x2*                     #x8B69)
(defconstant *GL-FLOAT-MAT4x3*                     #x8B6A)
(defconstant *GL-SRGB*                             #x8C40)
(defconstant *GL-SRGB8*                            #x8C41)
(defconstant *GL-SRGB-ALPHA*                       #x8C42)
(defconstant *GL-SRGB8-ALPHA8*                     #x8C43)
(defconstant *GL-SLUMINANCE-ALPHA*                 #x8C44)
(defconstant *GL-SLUMINANCE8-ALPHA8*               #x8C45)
(defconstant *GL-SLUMINANCE*                       #x8C46)
(defconstant *GL-SLUMINANCE8*                      #x8C47)
(defconstant *GL-COMPRESSED-SRGB*                  #x8C48)
(defconstant *GL-COMPRESSED-SRGB-ALPHA*            #x8C49)
(defconstant *GL-COMPRESSED-SLUMINANCE*            #x8C4A)
(defconstant *GL-COMPRESSED-SLUMINANCE-ALPHA*      #x8C4B)





;; For compatibility with OpenGL v1.0 
(defconstant *GL-LOGIC-OP*                         *GL-INDEX-LOGIC-OP*)
(defconstant *GL-TEXTURE-COMPONENTS*               *GL-TEXTURE-INTERNAL-FORMAT*)




(defconstant *GrayScale*		1)
(defconstant *StaticColor*		2)
(defconstant *PseudoColor*		3)
(defconstant *TrueColor*		4)
(defconstant *DirectColor*		5)
