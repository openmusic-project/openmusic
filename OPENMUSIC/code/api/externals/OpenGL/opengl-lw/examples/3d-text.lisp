;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/3d-text.lisp,v 1.6.1.2 2014/06/05 11:59:33 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "USER")

(defun set-up-gl-fonts (pane obj)
  #-mswindows (declare (ignore pane obj))
  #+mswindows
  (when (name obj)
    (unless (assoc :font (extra-display-lists obj))
      (push (list :font
                  (win32::wgl-use-font pane
                                       :start 0
                                       :count 256
                                       :outlinep t)
                  256)
            (extra-display-lists obj)))))

(defmacro with-3d-text-state-saved (&body body)
  `(opengl:with-matrix-pushed
     #+mswindows
     (opengl:gl-push-attrib opengl:*gl-all-attrib-bits*)
     ,@body
     #+mswindows
     (opengl:gl-pop-attrib)))

#+ftgl
(defvar *ftgl-font-file* "/usr/share/fonts/paratype-pt-sans/PTN57F.ttf")

(defun draw-3d-text (obj text)
  #+mswindows
  (let* ((base (second (assoc :font (extra-display-lists obj)))))
    ;; Set up for a string-drawing display list call.
    (opengl:gl-list-base base)
    ;; Draw a string using font display lists.
    (fli:with-foreign-string (ptr elts bytes
                                  :external-format win32:*multibyte-code-page-ef*
                                  :null-terminated-p nil)
        text
      (declare (ignore bytes))
      (opengl:gl-call-lists elts
                            opengl:*gl-unsigned-byte*
                            ptr)))
  #+ftgl
  (let ((font (ftgl:ftgl-create-extrude-font *ftgl-font-file*)))
    (unwind-protect
        (opengl:with-matrix-pushed
          (let ((scale (float 1/72 0d0)))
            (opengl:gl-scaled scale scale scale))
          (ftgl:ftgl-set-font-display-list font (if (use-display-list obj) 0 1))
          (ftgl:ftgl-set-font-face-size font 72 72)
          (ftgl:ftgl-set-font-depth font 5.0)
          (ftgl:ftgl-render-font font text ftgl:FTGL_RENDER_ALL))
      (ftgl:ftgl-destroy-font font))))

#+(or mswindows ftgl)
(defun draw-positioned-3d-text (obj text
                                    x-pos y-pos z-pos
                                    x-rotation y-rotation z-rotation
                                    scale)
  (with-3d-text-state-saved
    (opengl:gl-translated x-pos y-pos z-pos)
    (opengl:gl-scaled scale scale scale)
    (opengl:gl-rotated x-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated y-rotation 0.0d0 1.0d0 0.0d0)
    (opengl:gl-rotated z-rotation 0.0d0 0.0d0 1.0d0)
    ;; Draw the text.
    (draw-3d-text obj text)))

#+(or mswindows ftgl)
(defmethod draw :after ((obj geom-object))
  (let* ((text (name obj)))
    (when text
      (opengl:gl-color4-d 1d0 0.5d0 0.0d0 1.0d0)
      (if (listp text)
          (dolist (spec text)
            (apply 'draw-positioned-3d-text obj spec))
        (let* ((vertexes (vertexes obj))
               (vertex (aref vertexes 0)))
          (draw-positioned-3d-text obj text
                                   (opengl:gl-vector-aref vertex 0)
                                   (opengl:gl-vector-aref vertex 1)
                                   (opengl:gl-vector-aref vertex 2)
                                   -90d0 0d0 180d0
                                   0.5d0))))))

