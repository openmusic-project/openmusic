(require :cffi "../../FFI/CFFI/load-cffi")
(require :libsndfile "../../Audio/libsndfile/libsndfile")

(defun compile?-and-load (file)
  (compile-file-if-needed file)
  (load file))

(setf cl-jack-files '("cl-jack" "cl-jack-midi" "cl-jack-audio"))

(dolist (file cl-jack-files)
  (compile?-and-load file))
