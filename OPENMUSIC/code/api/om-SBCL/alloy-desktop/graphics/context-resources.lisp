(in-package :alloy-desktop)

(defstruct (gl-context-cleanup
            (:constructor %make-gl-context-cleanup (name release forget order)))
  name release forget order)

(defvar *gl-context-cleanups* nil
  "Registered owners of resources or bookkeeping associated with a GL context.")

(defun register-gl-context-cleanup (name &key release forget (order 50))
  "Register GL-context cleanup callbacks under NAME.

RELEASE, when non-NIL, is called with the retiring window while its native GL
context is alive and current. FORGET, when non-NIL, is called afterwards, and
is also called when the native context is already gone. ORDER determines
execution order within each phase. Registering NAME again replaces its previous
callbacks."
  (check-type name symbol)
  (check-type order real)
  (when release (check-type release function))
  (when forget (check-type forget function))
  (unless (or release forget)
    (error "At least one of RELEASE or FORGET must be supplied for ~S." name))
  (unregister-gl-context-cleanup name)
  (push (%make-gl-context-cleanup name release forget order)
        *gl-context-cleanups*)
  (setf *gl-context-cleanups*
        (stable-sort *gl-context-cleanups* #'< :key #'gl-context-cleanup-order))
  name)

(defun unregister-gl-context-cleanup (name)
  "Remove the GL-context cleanup registered under NAME."
  (setf *gl-context-cleanups*
        (delete name *gl-context-cleanups*
                :key #'gl-context-cleanup-name :test #'eq))
  name)

(defun gl-context-cleanup-names ()
  "Return registered GL-context cleanup names in execution order."
  (mapcar #'gl-context-cleanup-name *gl-context-cleanups*))

(defun deallocate-gl-resource (resource)
  "Delete one Alloy OpenGL resource using the context that is currently active.

Alloy's unmanaged OpenGL resource structs do not define DEALLOCATE methods, so
this function performs the corresponding GL deletion explicitly. Unknown
resource types fall back to ALLOY:DEALLOCATE."
  (typecase resource
    (org.shirakumo.alloy.renderers.opengl::program
     (gl:delete-program
      (org.shirakumo.alloy.renderers.opengl:gl-name resource)))
    (org.shirakumo.alloy.renderers.opengl::vbo
     (gl:delete-buffers
      (list (org.shirakumo.alloy.renderers.opengl:gl-name resource))))
    (org.shirakumo.alloy.renderers.opengl::vao
     (gl:delete-vertex-arrays
      (list (org.shirakumo.alloy.renderers.opengl:gl-name resource))))
    (org.shirakumo.alloy.renderers.opengl::fbo
     (gl:delete-framebuffers
      (list (org.shirakumo.alloy.renderers.opengl:gl-name resource)))
     (gl:delete-texture
      (org.shirakumo.alloy.renderers.opengl::fbo-color resource))
     (gl:delete-texture
      (org.shirakumo.alloy.renderers.opengl::fbo-depth resource)))
    (org.shirakumo.alloy.renderers.opengl::texture
     (gl:delete-texture
      (org.shirakumo.alloy.renderers.opengl:gl-name resource)))
    (t
     (alloy:deallocate resource)))
  resource)

(defun deallocate-renderer-gl-resources (renderer)
  "Delete and forget the OpenGL resources owned by RENDERER.

The Alloy GLFW renderer has a more-specific primary ALLOY:DEALLOCATE method
that destroys the GLFW window without calling the OpenGL renderer's method.
This cleanup must therefore run while RENDERER's context is still alive."
  (when (typep renderer 'org.shirakumo.alloy.renderers.opengl::renderer)
    (loop for resource being the hash-values
            of (org.shirakumo.alloy.renderers.opengl::resources renderer)
          do (deallocate-gl-resource resource))
    (clrhash (org.shirakumo.alloy.renderers.opengl::resources renderer)))
  renderer)

(defun %run-gl-context-cleanups (window accessor)
  (dolist (cleanup *gl-context-cleanups*)
    (let ((function (funcall accessor cleanup)))
      (when function
        (funcall function window)))))

(defun retire-gl-context-resources (window)
  "Release GL resources associated with WINDOW before its native context dies.

When WINDOW still has a native GLFW context, make it current, release the
renderer-owned OpenGL resources, and run every registered RELEASE callback.
Then run every registered FORGET callback. If WINDOW has already lost its
native context, skip all GL operations and run only FORGET callbacks."
  (when (and (typep window '%glfw:window)
             (%glfw:pointer window))
    (call-with-gl-context
     window
     (lambda ()
       (deallocate-renderer-gl-resources window)
       (%run-gl-context-cleanups window #'gl-context-cleanup-release))))
  (%run-gl-context-cleanups window #'gl-context-cleanup-forget)
  window)

(defvar *fonts-allocated-p* nil
  "True once the session's font atlases were uploaded by the first renderer.")

(defmethod alloy:allocate :after
    ((renderer org.shirakumo.alloy.renderers.opengl.msdf:renderer))
  ;; FIRST, before anything can read an atlas: the renderer registered every .json
  ;; it found in the shared cache directory, and the foreign ones must stay out.
  (park-foreign-atlases renderer)
  (unless *fonts-allocated-p*
    (ensure-font-atlases renderer)
    ;; The font setup may have registered new atlases in this renderer's caches.
    (forget-font-cache renderer)
    (loop for font being the hash-values
            of (org.shirakumo.alloy.renderers.opengl.msdf:fontcache renderer)
          ;; isolate each font: a failing allocate must not abort window creation
          do (handler-case
                 (unless (alloy:allocated-p font)
                   (alloy:allocate font))
               (error (c)
                 (format *error-output*
                         "[fonts] eager allocate failed for ~A: ~A~%"
                         font (princ-to-string c))
                 (force-output *error-output*))))
    (setf *fonts-allocated-p* t)))

(defmethod alloy:deallocate :after
    ((renderer org.shirakumo.alloy.renderers.opengl.msdf:renderer))
  ;; the memoized resolutions belong to this renderer's atlases, and the one-element
  ;; memo would otherwise hold the closed window until the next text draw anywhere
  (forget-font-cache renderer))

(defvar *image-cache* (make-hash-table :test 'equal)
  "Truename -> texture, shared by every renderer of the session.")

(defmethod simple:request-image :around
    ((renderer org.shirakumo.alloy.renderers.opengl.msdf:renderer)
     (path pathname)
     &key)
  (let ((key (or (probe-file path) path)))
    (or (gethash key *image-cache*)
        (setf (gethash key *image-cache*)
              (call-next-method)))))

(defun forget-session-graphics ()
  "Drop the session's shared textures and font uploads: the next renderer starts from disk."
  (clrhash *image-cache*)
  (setf *fonts-allocated-p* nil))
