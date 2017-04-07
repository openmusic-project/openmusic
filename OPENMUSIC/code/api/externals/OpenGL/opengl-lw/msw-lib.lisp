;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/msw-lib.lisp,v 1.11.10.1 2014/05/27 20:56:56 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")


(defmethod %make-context ((rep ww::r-output-pane) opengl-configuration)
  (if (set-win32-pixel-format rep opengl-configuration)
      (win32::wgl-create-context (ww::r-output-pane-hdc rep))
    (error "Can't make wgl context for ~s.~%Pixel-format not set" opengl-configuration)))

(defmethod %start-rendering ((rep ww::r-output-pane) context)
  (or (win32::wgl-make-current (ww::r-output-pane-hdc rep) context)
     (let ((e (win32:get-last-error))) (format t "~%OpenGL %start-rendering failed! last-error ~d (~x)" e e)
       nil)))

(defmethod %stop-rendering ((rep ww::r-output-pane))
  (win32::wgl-make-current (ww::r-output-pane-hdc rep) 0))

(defmethod %swap-buffers ((rep ww::r-output-pane) context)
  (declare (ignore context))
  (win32::SWAP-BUFFERS  (ww::r-output-pane-hdc rep)))

(defmethod %resize-opengl-context ((rep ww::r-output-pane)
                                   context width height)
  (declare (ignore context width height))
  )

(defmethod %free-opengl-resources ((rep ww::r-output-pane) context)
  (when context
    (win32::WGL-DELETE-CONTEXT context)))


;;; ------------------------------------------------------------
;;; 

(defun device-generic-p (rep)
  (pfd-dw-flag-p rep win32::*PFD-GENERIC-FORMAT*))

(defun device-native-p (rep)
  (pfd-dw-flag-p rep win32::*PFD-DRAW-TO-WINDOW*))

(defun device-supports-opengl-p (rep)
  (pfd-dw-flag-p rep win32::*PFD-SUPPORT-OPENGL*))

(defun device-is-rgba-p (rep)
  (pfd-slot-test rep 'win32::IPIXELTYPE win32::*PFD-TYPE-RGBA*))

(defun device-is-color-buffer-p (rep)
  (pfd-slot-test rep 'win32::ILAYERTYPE win32::*PFD-MAIN-PLANE*))

(defun device-color-bits (rep)
  (pfd-slot-value rep 'win32::CCOLORBITS))

(defun device-palette-needed-p (rep)
  (pfd-dw-flag-p rep win32::*PFD-NEED-PALETTE* ))

(defun pfd-slot-value (rep slot)
  (let* ((hdc (ww::r-output-pane-hdc rep))
         (ipfd (win32::GET-PIXEL-FORMAT hdc)))
    (fli:with-dynamic-foreign-objects ((pfd win32::pixel-format-descriptor))
      (win32::DESCRIBE-PIXEL-FORMAT hdc ipfd pfd)
      (fli:foreign-slot-value pfd slot))))

(defun pfd-slot-test (rep slot val)
  (= val
     (pfd-slot-value rep slot)))

(defun pfd-dw-flag-p (rep flag)
  (let* ((hdc (ww::r-output-pane-hdc rep))
         (ipfd (win32::GET-PIXEL-FORMAT hdc)))
    (fli:with-dynamic-foreign-objects ((pfd win32::pixel-format-descriptor))
      (win32::DESCRIBE-PIXEL-FORMAT hdc ipfd pfd)
      (not (zerop (logand flag (fli:foreign-slot-value pfd 'win32::DWFLAGS)))))))

(defun set-win32-pixel-format (rep configuration &key (errorp T))
  "Sets the device-context pixel-format-descriptor for rep which supports the 
   requested configuration. Returns T if it succeeds.
   Configuration is a plist with the following allowed
   indicators: 
      :double-buffer, :double-buffered, - synonyms, value T or NIL.
      :buffer-size - color buffer size for indexed colormap visuals
      :accum - accumulator buffer size (per channel), or NIL.
      :depth-buffer - value is a depth buffer size or NIL
      :stencil-size - stencil buffer size or NIL.
      :aux - aux buffer size or NIL."
  (let* ((hdc (ww::r-output-pane-hdc rep))
         (mode (if (or (getf configuration :rgb) (getf configuration :rgba))
                   win32::*pfd-type-rgba*
                 win32::*pfd-type-colorindex*))
         (depth (or (getf configuration :buffer-size)
                    (win32::R-DEVICE-BITSPIXEL (win32::dc-device (ww::r-output-pane-dc rep)))))
         (z-buffer (or (getf configuration :depth-buffer) 0))
         (accum (or (getf configuration :accum) 0))
         (stencil (or (getf configuration :stencil-size) 0))
         (aux (or (getf configuration :aux) 0))
         (double-buffer (or (getf configuration :double-buffer)(getf configuration :double-buffered)))
         (flags (logior win32::*pfd-draw-to-window* win32::*pfd-support-opengl*
                        (if double-buffer win32::*PFD-DOUBLEBUFFER* 0))))
    (fli:with-dynamic-foreign-objects ((pfd win32::pixel-format-descriptor))
      (setf (fli:foreign-slot-value pfd 'win32::nsize) (fli:size-of 'win32::pixel-format-descriptor)
            (fli:foreign-slot-value pfd 'win32::nversion) 1
            (fli:foreign-slot-value pfd 'win32::dwflags) flags
            (fli:foreign-slot-value pfd 'win32::iPixelType) mode
            (fli:foreign-slot-value pfd 'win32::cColorBits) depth
            (fli:foreign-slot-value pfd 'win32::cRedBits) 0
            (fli:foreign-slot-value pfd 'win32::cRedShift) 0
            (fli:foreign-slot-value pfd 'win32::cGreenBits) 0
            (fli:foreign-slot-value pfd 'win32::cGreenShift) 0
            (fli:foreign-slot-value pfd 'win32::cBlueBits) 0
            (fli:foreign-slot-value pfd 'win32::cBlueShift) 0
            (fli:foreign-slot-value pfd 'win32::cAlphaBits) 0
            (fli:foreign-slot-value pfd 'win32::cAlphaShift) 0
            (fli:foreign-slot-value pfd 'win32::cAccumBits) accum
            (fli:foreign-slot-value pfd 'win32::cAccumRedBits) 0
            (fli:foreign-slot-value pfd 'win32::cAccumGreenBits) 0
            (fli:foreign-slot-value pfd 'win32::cAccumBlueBits) 0
            (fli:foreign-slot-value pfd 'win32::cAccumAlphaBits) 0
            (fli:foreign-slot-value pfd 'win32::cDepthBits) z-buffer
            (fli:foreign-slot-value pfd 'win32::cStencilBits) stencil
            (fli:foreign-slot-value pfd 'win32::cAuxBuffers) aux
            (fli:foreign-slot-value pfd 'win32::iLayerType) win32::*pfd-main-plane*
            (fli:foreign-slot-value pfd 'win32::breserved) 0
            (fli:foreign-slot-value pfd 'win32::dwlayermask) 0
            (fli:foreign-slot-value pfd 'win32::dwVisiblemask) 0
            (fli:foreign-slot-value pfd 'win32::dwDamagemask) 0)
      (let ((ipfd (win32::choose-pixel-format hdc pfd)))
        (if (zerop ipfd)
            (progn 
              (when errorp
                (error "Unable to find pixel-format for this opengl configuration: ~s" configuration))
              0)
          (win32::set-pixel-format hdc ipfd pfd))))))

;; return a list giving a configuration that corresponds to the pfd
(defun get-win32-pixel-format (rep)
  (let* ((hdc (ww::r-output-pane-hdc rep))
         (ipfd (win32::GET-PIXEL-FORMAT hdc)))
    (fli:with-dynamic-foreign-objects ((pfd win32::pixel-format-descriptor))
      (win32::DESCRIBE-PIXEL-FORMAT hdc ipfd pfd)
      (list
      ; (fli:foreign-slot-value pfd 'win32::nversion) 1
       :double-buffer (not (zerop (logand (fli:foreign-slot-value pfd 'win32::dwflags)
                                          win32::*PFD-DOUBLEBUFFER*)))
       :rgba (eq 
              (fli:foreign-slot-value pfd 'win32::iPixelType) 
              win32::*pfd-type-rgba*)

       :buffer-size (fli:foreign-slot-value pfd 'win32::cColorBits)
      ; (fli:foreign-slot-value pfd 'win32::cRedBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cRedShift) 0
      ; (fli:foreign-slot-value pfd 'win32::cGreenBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cGreenShift) 0
      ; (fli:foreign-slot-value pfd 'win32::cBlueBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cBlueShift) 0
      ; (fli:foreign-slot-value pfd 'win32::cAlphaBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cAlphaShift) 0
       :accum (let ((accum (fli:foreign-slot-value pfd 'win32::cAccumBits)))
                (unless (zerop accum) accum))
      ; (fli:foreign-slot-value pfd 'win32::cAccumRedBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cAccumGreenBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cAccumBlueBits) 0
      ; (fli:foreign-slot-value pfd 'win32::cAccumAlphaBits) 0
       :depth-buffer (let ((z-buffer (fli:foreign-slot-value pfd 'win32::cDepthBits)))
                       (unless (zerop z-buffer) z-buffer))
       :stencil-size (let ((stencil (fli:foreign-slot-value pfd 'win32::cStencilBits)))
                       (unless (zerop stencil) stencil))
       :aux (let ((aux (fli:foreign-slot-value pfd 'win32::cAuxBuffers)))
              (unless (zerop aux) aux))
      ; (fli:foreign-slot-value pfd 'win32::iLayerType) 
      ; (fli:foreign-slot-value pfd 'win32::breserved)
      ; (fli:foreign-slot-value pfd 'win32::dwlayermask)
      ; (fli:foreign-slot-value pfd 'win32::dwVisiblemask)
      ; (fli:foreign-slot-value pfd 'win32::dwDamagemask)
       ))))



(defmethod %describe-configuration ((rep ww::r-output-pane) context &optional (stream *standard-output*) collectp)
  (declare (ignore context))
  (let ((results (get-win32-pixel-format rep)))
    (if collectp
        results
      (format stream
              "~&Color buffer size : ~d~%Uses ~:[Color Index~;RGBA~]~%Is ~:[single~;double~]-buffered~@[~%Accumulator buffer size (per channel) = ~d bits~]~@[~%Depth buffer size = ~d bits~]~@[~%Stencil buffer size = ~d bits~]~@[~%Has ~d aux buffers~]" 
              (getf results :buffer-size) 
              (getf results :rgba)
              (getf results :double-buffer)
              (getf results :accum)
              (getf results :depth-buffer)
              (getf results :stencil-size)
              (getf results :aux)))))
                                           

