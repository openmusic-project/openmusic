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
;===========================================================================

; SDIF package by J. Bresson

;;===========================================================================
;  SDIF MID-LEVEL FUNCTIONS
;;===========================================================================


(in-package :sdif)

(pushnew :sdif *features*)

;;;==============================
;;; CHARGEMENT
;;; !!! *sdif-pathname* is modified in OM
;;; 
(defvar *sdif-pathname* 
  #+win32 "/WINDOWS/system32/sdif.dll"
  #+(or darwin macos macosx) "libSDIF.dylib"
  #+linux "libsdif.so"
  )

(defvar *sdif-library* nil)
(defvar *sdif-initialized-p* nil)

;; (defun load-sdif-library ()
;;   (let ((libpath (om::om-lib-pathname sdif::*sdif-pathname*)))
;;     ;(om::om-message-dialog (format nil "~A" *sdif-pathname*))
;;     ;(om::om-message-dialog (format nil "~A" libpath))
;;     (if (probe-file libpath)
;;         (progn (print (concatenate 'string "Loading SDIF library: " (namestring libpath)))
;;           (setf *sdif-library*
;;                 #-linux (handler-case 
;;                            (progn
;;                              (fli:register-module "SDIF" 
;;                                                   :real-name (namestring libpath)
;;                                                   :connection-style :immediate)
;;                              t)
;;                           (error () (progn 
;;                                       (om::om-message-dialog (format nil "Could not load SDIF foreign-library.~%~A" (namestring libpath)))
;;                                       nil)))
                
;;                 #+linux (progn 
;;                           (define-foreign-library libsdif
;;                             ;; #+:LISPWORKS-64BIT (:unix (:or "/usr/local/lib64/libsdif.so" libpath "libsdif.so"))
;;                             #+:LISPWORKS-64BIT libpath
;;                             #+:LISPWORKS-32BIT (:unix (:or "/usr/lib/libsdif.so" libpath "libsdif.so"))
;;                             (t (:default "libsdif")))
;;                           (handler-case (progn
;;                                           (let ((lib (use-foreign-library libsdif)))
;;                                             (print (format nil "Loaded SDIF lib: ~A" (foreign-library-pathname lib))))
;;                                           t)
;;                             (error () (progn (print (format nil "Could not load foreign-library libsdif")) nil))))
;;                 ))
;;       (om::om-message-dialog (format nil "SDIF library not found: ~A" (namestring libpath))))
;;     (setf *sdif-initialized-p* nil)
;;     ))

(defun load-sdif-library ()
  ;;(om::om-message-dialog (format nil "~A" *sdif-pathname*))
  ;;(om::om-message-dialog (format nil "~A" libpath))
  (let ((libpath (om::om-lib-pathname sdif::*sdif-pathname*)))
    (if (probe-file libpath)
	(progn (print (concatenate 'string "Loading SDIF library: " (namestring libpath)))
	       (setf *sdif-library*
		     (handler-case 
			 (progn
			   (fli:register-module "SDIF" 
						:real-name (namestring libpath)
						:connection-style :immediate)
			   t)
		       (error () (progn 
				   (om::om-message-dialog (format nil "Could not load SDIF foreign-library.~%~A" (namestring libpath)))
				   nil)))))
	(om::om-message-dialog (format nil "SDIF library not found: ~A" (namestring libpath)))))
  (setf *sdif-initialized-p* nil))


(om::om-add-init-func 'load-sdif-library)

;;; INIT/CLOSE SDIF
(defun sdif-null-ptr-p (sdifptr)
   (cffi:null-pointer-p sdifptr))

(defun sdif-init (string)
  (unless *sdif-initialized-p*
    (when *sdif-library*
      (print "Initializing SDIF...")
      (sdif::SdifGenInit string))
    (setf *sdif-initialized-p* t)))

(defun sdif-init-cond ()
  (unless *sdif-initialized-p*
    (when *sdif-library*
      (print "Initializing SDIF...")
      (sdif::SdifGenInitCond "")
      (setf *sdif-initialized-p* t))
    ))

(defun sdif-kill ()
   (sdif::SdifGenKill)
   (setf *sdif-initialized-p* nil))

;;; SDIF FILES
(defun get-file-mode (symb)
   (cond ((equal symb :eUnknownFileMode) 0)
         ((equal symb :eWriteFile) 1)
         ((equal symb :eReadFile) 2)
         ((equal symb :eReadWriteFile) 3)
         ((equal symb :ePredefinedTypes) 4)
         (t 0)))

;;; mode = un entier ou une constante ou un symbole :eReadFile, :eWriteFile, ...
(defun sdif-open-file (filename mode)
  (sdif-init-cond)
  (let ((open-mode (if (integerp mode) mode (get-file-mode mode))))
    (sdif::SdifFOpen (namestring filename) open-mode)))

(defun sdif-close-file (file)
  (sdif::SdifFClose file))

(defun sdif-check-file (filename)
  (not (zerop (sdif::SdifCheckFileFormat (namestring filename)))))

(defun sdif-to-text (sdiff outfilename)
  (sdif::SdifToText sdiff (namestring outfilename)))

(defun sdif-get-pos (ptr)
  (let ((thelong (cffi::%foreign-alloc 8))
        rep)
    (sdif::SdiffGetPos ptr thelong)
    (setf rep (cffi::mem-ref thelong :unsigned-long))
    (cffi:foreign-free thelong)
    rep))

(defun sdif-set-pos (ptr thelong)
  (let ((longptr (cffi::%foreign-alloc 8)))
    (cffi::mem-set thelong longptr :unsigned-long)
    (sdif::SdiffSetPos ptr longptr)
    (cffi::foreign-free longptr)
    t))

(defun sdif-get-signature (sdiff)
   (let ((nbcharread (cffi::%foreign-alloc 4))
         rep)
     (cffi::mem-set 0 sdiff :long)
     (setf rep (sdif::SdifFGetSignature sdiff nbcharread))
     (cffi::foreign-free nbcharread)
     rep))

;;; 22/09/14 changed floor by (- ceiling), for datasize > align.
(defun sdif-calculate-padding (nr nc size)
  (let ((datasize (* nr nc size))
        (align 8))
    (cond ((zerop datasize) 0)
          ((< datasize align) (- align datasize))
          (t (mod (- (cadr (multiple-value-list (ceiling datasize align)))) align)))
    ))



;;;============================
;;; TESTS
;(fli:register-module "SDIF" :real-name "/Users/bresson/SRC/OM6/OPENMUSIC/OM 6.11.app/Connts/Frameworks/libSDIF.dylib" :connection-style :immediate)
;(fli:register-module "SDIF" :real-name "/Users/bresson/SRC/OM6/OPENMUSIC/resources/lib/mac/SDIF.framework/Versions/3.11/SDIF" :connection-style :immediate)
; (init-sdif-framework)
; (sdif::sdifprintversion)
;(Sdif-Init "")
;(Sdif-Kill)
;(setf filepath (namestring (capi::prompt-for-file nil))) 
;(setf filepath (namestring filepath))
;(setf sdiffile (Sdif-Open-file filepath :eReadFile))
;(sdif-close-file sdiffile)
;(sdif-check-file filepath)
;(sdif::sdifFreadgeneralheader sdiffile)
;(sdif::sdifFReadAllASCIIChunks sdiffile)  
;(sdif::sdifFReadFrameHeader sdiffile)
;(sdif::SdifFSkipFrameData sdiffile)
;(setf sign (sdif::SdifFCurrSignature sdiffile))
;(setf sign (sdif::SdifFCurrMatrixSignature sdiffile))
;(setf sign (sdif::SdifFCurrFrameSignature sdiffile))
;(sdif-Signature-to-string sign)
;(setf bytesread (ff::make-foreign-pointer :size 2 :type :int))
;(setf posptr (ff::make-foreign-pointer :size 8 :type :int))
;(sdif::SdifFGetSignature sdiffile bytesread)
;(sdif::sdiffcurrtime sdiffile)
;(sdif::sdiffcurrdatatype sdiffile)
;(sdif::sdiffcurrID sdiffile)
;(sdif::sdiffcurrnbmatrix sdiffile)
;(sdif::sdiffreadmatrixheader sdiffile)
;(sdif::sdiffcurrnbcol sdiffile)
;(sdif::sdiffcurrnbrow sdiffile)
;(sdif::sdiffskipmatrixdata sdiffile) 
;(sdif::sdiffgetsignature sdiffile bytesread) 
;(sdif::sdiffgetpos sdiffile posptr)
;(sys::memref-int (ff::foreign-pointer-address posptr) 0 0 :unsigned-long)

;(defun signature (sdiff ptr)
;   (print (sdif::sdifSignaturetostring (sdif::SdifFCurrSignature sdiff)))
;   (sdif::sdifFReadFrameHeader sdiff)
;   (sdif::sdifFSkipFrameData sdiff)
;   (sdif::SdifFGetSignature sdiffile ptr))
