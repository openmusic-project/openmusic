;;===========================================================================
; OM AUDIO API 
;;===========================================================================
(in-package :cl-user)

(compile&load (decode-local-path "libsndfile/libsndfile"))
(compile&load (decode-local-path "libsndfile/libsndfile-api"))
(compile&load (decode-local-path "file-access"))
(compile&load (decode-local-path "libsamplerate/libsamplerate"))
(compile&load (decode-local-path "libsamplerate/libsamplerate-api"))
(compile&load (decode-local-path "omjuceaudiolib/omjuceaudiolib"))


(push :audio *features*)