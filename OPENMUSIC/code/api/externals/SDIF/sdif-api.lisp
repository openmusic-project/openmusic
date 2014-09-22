;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;Macintosh version (Digitool Macintosh Common Lisp - MCL)
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson and Augusto Agon
;;===========================================================================

;;===========================================================================
;DocFile
;  SDIF FUNCTIONS
;;===========================================================================

(in-package :om-api)


(export '(
          om-start-sdif       
          sdif-init sdif-init-cond
          sdif-kill
          sdif-open-file
          sdif-null-ptr-p
          sdif-close-file
          sdif-check-file
          sdif-to-text
          sdif-get-pos
          sdif-set-pos
          sdif-get-signature
          sdif-calculate-padding
          
          ) :om-api)

;;; INIT/CLOSE SDIF
(defun sdif-null-ptr-p (sdifptr)
   (cffi:null-pointer-p sdifptr))

(defun sdif-init (string)
  (unless *sdiflib*
    (when sdif::*sdif-framework*
      (print "Initializing SDIF...")
      (sdif::SdifGenInit string))
    (setf *sdiflib* t)))

(defun sdif-init-cond ()
  (unless *sdiflib*
    (when sdif::*sdif-framework*
      (print "Initializing SDIF...")
      (sdif::SdifGenInitCond ""))
    (setf *sdiflib* t)))

(defun sdif-kill ()
   (sdif::SdifGenKill)
   (setf *sdiflib* nil))


(defvar *sdiflib* nil)

(defun om-start-sdif ()
  #-linux (setf sdif::*sdif-pathname* (om-lib-pathname sdif::*sdif-pathname*))
  (sdif::init-sdif-framework)
  ;; (sdif-init "") ;; => LATER !!! :(
  (setf *sdiflib* nil)
  sdif::*sdif-framework*)
  

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
