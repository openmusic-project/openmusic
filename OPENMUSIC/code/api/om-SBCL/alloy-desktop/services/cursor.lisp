(in-package :alloy-desktop)

;;; An image cursor with a hotspot, realised as a GLFW cursor on first apply.

(defstruct (cursor-image (:predicate cursor-image-p))
  pixels width height xhot yhot (glfw nil))

(defmethod (setf windowing:icon) ((c cursor-image)
                                  (cursor org.shirakumo.alloy.renderers.glfw::cursor))
  (let ((native (or (cursor-image-glfw c)
                    (setf (cursor-image-glfw c)
                          (make-instance 'org.shirakumo.fraf.glfw:cursor
                                         :pixels (cursor-image-pixels c)
                                         :width  (cursor-image-width c)
                                         :height (cursor-image-height c)
                                         :xhot   (cursor-image-xhot c)
                                         :yhot   (cursor-image-yhot c))))))
    (setf (org.shirakumo.fraf.glfw:cursor
           (org.shirakumo.alloy.renderers.glfw:window cursor))
          native)
    (org.shirakumo.alloy.renderers.glfw::set-icon c cursor))
  c)

(defun load-cursor-image (file xhot yhot)
  "The cursor image decoded from FILE with hotspot XHOT YHOT, or NIL when FILE cannot be decoded."
  (let ((img (decode-image-file file)))
    (when img
      ;; GLFW cursor pixels are top-left RGBA; the fraf wrapper copies them through
      ;; a signed :char buffer, so pre-encode bytes >127 as signed complements.
      (org.shirakumo.fraf.imagine:adjust img :origin :top-left)
      (let* ((data   (org.shirakumo.fraf.imagine:data img))
             (signed (make-array (length data) :element-type '(signed-byte 8))))
        (loop for i below (length data)
              for b = (aref data i)
              do (setf (aref signed i) (if (> b 127) (- b 256) b)))
        (make-cursor-image :pixels signed
                           :width  (org.shirakumo.fraf.imagine:w img)
                           :height (org.shirakumo.fraf.imagine:h img)
                           :xhot   xhot
                           :yhot   yhot)))))
