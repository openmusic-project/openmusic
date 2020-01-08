;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;;; authors G. Assayag, C. Agon, J. Bresson
;;===========================================================================

;;===========================================================================
; OM AUDIO API 
;;===========================================================================

(in-package :cl-user)

(compile&load (decode-local-path "libsndfile/libsndfile"))
(compile&load (decode-local-path "libsndfile/libsndfile-api"))
(compile&load (decode-local-path "libsamplerate/libsamplerate"))
(compile&load (decode-local-path "libsamplerate/libsamplerate-api"))
(compile&load (decode-local-path "omaudiolib/omaudiolib"))
(compile&load (decode-local-path "file-access"))


(push :audio *features*)


;;; TODO CLEANUP !!!
#-macosx
(defun init-libsndfile ()
  (cffi:define-foreign-library libsndfile
    (:darwin "libsndfile.dylib")
    #+win32(:unix (:or "cygsndfile-1.dll" "libsndfile.so.1" "libsndfile.so"))
    (t (:default #+win32 "libsndfile-1" #-win32 "libsndfile")))
  (handler-case 
      (cffi:use-foreign-library libsndfile)
    (error () (progn 
                (print (format nil "could not load foreign-library libsndfile"))
                nil))))

#+macosx
(defun init-libsndfile ()
  (let ((libpath (namestring (om::om-lib-pathname "libsndfile.dylib"))))
    (if (probe-file libpath)
        (progn
          (print (format nil "Loading libsndfile library: ~A" libpath))
          (handler-case 
              (fli:register-module "libsndfile" 
                                   :real-name libpath
                                   :connection-style :immediate)
            (error () (format nil "could not load foreign-library ~A" libpath))))
      (print (format nil "could not find foreign-library ~A" libpath)))))

(oa:om-add-init-func 'init-libsndfile)

#-macosx
(defun init-libsamplerate ()
  (cffi:define-foreign-library libsamplerate
    (:darwin "libsamplerate.dylib")
    (:unix (:or "libsamplerate.dll" "libsamplerate.so"))
    (t (:default "libsamplerate")))
  (handler-case 
      (cffi:use-foreign-library libsamplerate)
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



(defparameter *omaudiolib-pathname*
  #+win32
  "/WINDOWS/system32/OMAudioLib.dll"
  #+(or darwin macos macosx)  
  "OM6/OPENMUSIC/resources/lib/mac/OMAudioLib.dylib"
  #+linux
  "OM6/OPENMUSIC/resources/lib/linux/OMAudioLib.so"
  )

(defvar omaudiolib-error-message
  (format nil
	  (concatenate
	   'string
	   "Something wrong calling functions from OMAudioLib.so~2%"
	   "Check README file coming with OMAudioLibs sources: ~2%"
	   "https://github.com/openmusic-project/omaudiolib")))

(defun load-juceaudiolib ()
  (let ((libpath (namestring (om::om-lib-pathname *omaudiolib-pathname*))))
    (if (probe-file libpath)
	(progn 
	  (print (concatenate 'string "Loading OM Audio library: " libpath))
	  (handler-case 
              (fli:register-module "OMAudio" 
				   :real-name libpath
				   :connection-style :immediate)
            (error (c) (format nil "could not load foreign-library ~A : " libpath c)))
	  ;; check if ffi-functions are accessible via ffi:
	  (when (fli::null-pointer-p
		 (fli:make-pointer :symbol-name "openAudioManager"
				   :errorp nil))
	    (om-api::om-message-dialog omaudiolib-error-message))
	  t)
	(print (concatenate 'string "OMAudioLib not found: " libpath)))))

(om::om-add-init-func 'load-juceaudiolib)



