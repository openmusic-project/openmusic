
(in-package :lsr)

#-macosx
(defun init-libsamplerate ()
  (define-foreign-library libsamplerate
    (:darwin "libsamplerate.dylib")
    (:unix (:or "libsamplerate.dll" "/usr/local/lib64/libsamplerate.so" "libsamplerate.so"))
    (t (:default "libsamplerate")))
  (handler-case 
      (use-foreign-library libsamplerate)
    (error () (progn 
                (print (format nil "could not load foreign-library libsamplerate"))
                nil)))
  )

#+macosx
(defun init-libsamplerate ()
  (let ((libpath (namestring (om::om-lib-pathname "libsamplerate.dylib"))))
    (if (probe-file libpath)
        (progn
          (print (format nil "Loading libsamplerate library: ~A" libpath))
          (handler-case 
              (fli:register-module "libsamplerate" 
                                   :real-name libpath
                                   :connection-style :immediate)
            (error () (format nil "could not load foreign-library ~A" libpath))))
      (print (format nil "could not find foreign-library ~A" libpath)))))

(oa:om-add-init-func 'init-libsamplerate)

;;; THE ONLY FUNCTION USED FROM OM:
(defun resample-audio-buffer (in-buffer in-size n-channels out-buffer out-size ratio method)
  (cffi:with-foreign-object (lsrdata '(:struct lsr::src_data))
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::data_in) in-buffer)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::input_frames) in-size)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::data_out) out-buffer)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::output_frames) out-size)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::src_ratio) ratio)
    (let ((res (lsr::src-simple lsrdata method n-channels)))
      (if (= res 0)
          (values T (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::output_frames_gen))
        (values NIL (lsr::src-strerror res))))))