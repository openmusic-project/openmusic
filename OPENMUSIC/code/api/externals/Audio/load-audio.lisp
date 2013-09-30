;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;;===========================================================================


(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "LibAudioStream")) :name "LibAudioStream"))

(compile&load (make-pathname :directory (pathname-directory *load-pathname*) :name "las-audio-player"))                                      


(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "LibAudioStream")) :name "faust-api"))
(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "libsndfile")) :name "libsndfile"))

(compile&load (make-pathname :directory  (pathname-directory *load-pathname*) :name "audio-api"))

(push :om-audio-api *features*)


