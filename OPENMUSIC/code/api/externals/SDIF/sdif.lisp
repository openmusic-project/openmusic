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
; SDIF Library binding
;;===========================================================================


(in-package :cl-user)

(defpackage "SDIF-PACKAGE"
  (:nicknames "SDIF")
   (:use common-lisp cffi))

(in-package :sdif)

;;;==============================
;;; CHARGEMENT

(defvar *sdif-framework* nil)


(defvar *sdif-pathname* 
  #+win32
  "/WINDOWS/system32/sdif.dll"
  #+(or darwin macos macosx) 
  "/Library/Frameworks/SDIF.framework/SDIF"
  #+linux "libsdif.so"
  )


;;; !!! *sdif-pathname* is modified in OM

(defun init-sdif-framework ()
  (setf *sdif-framework*
	#-linux (if (probe-file *sdif-pathname*)
		    (progn
		      (print (concatenate 'string "Loading SDIF library: " (namestring *sdif-pathname*)))
		      (fli:register-module "SDIF" 
					   :real-name (namestring *sdif-pathname*)
					   :connection-style :immediate)
		      t))
	#+linux (progn 
		  (define-foreign-library libsdif
		    (:unix (:or "/usr/local/lib/libsdif.so" "libsdif.so" (om-lib-pathname *sdif-pathname*)))
		    (t (:default "libsdif") ))
		  (handler-case (progn
				  (let ((lib (use-foreign-library libsdif)))
				    (print (format nil "Loaded SDIF lib: ~A" (foreign-library-pathname lib))))
				  t)
		    (error () (progn (print (format nil "could not load foreign-library libsdif"))
				     nil))))))

; (oa::om-start-sdif)
; (load-foreign-library "/Library/Frameworks/SDIF.framework/Versions/3.11/SDIF")
; (init-sdif-framework)
; (sdif::sdifprintversion)

;(define-foreign-type sdif-name () ':pointer)
(defctype sdif-name :pointer)

(defconstant eUnknownFileMode 0)
(defconstant eWriteFile 1)
(defconstant eReadFile 2)
(defconstant eReadWriteFile 3)
(defconstant ePredefinedTypes 4)


;;;============================================
;;; SDIF GENERAL

(defcfun  ("SdifGenInit" SdifGenInitSTR) :void (PredefinedTypesFile sdif-name))

(defun SdifGenInit (str) 
  (with-foreign-string (cstr str)
    (sdif::SdifGenInitSTR cstr)))

(defcfun  ("SdifGenInitCond" SdifGenInitCondSTR) :void (PredefinedTypesFile sdif-name))

(defun SdifGenInitCond (str) 
  (with-foreign-string (cstr str)
    (sdif::SdifGenInitCondSTR cstr)))

(defcfun  ("SdifGenKill" SdifGenKill) :void)

(defcfun  ("SdifPrintVersion" SdifPrintVersion) :void )

(defcfun  ("SdifenableErrorOutput" SdifenableErrorOutput) :void )

(defcfun  ("SdifDisableErrorOutput" SdifDisableErrorOutput) :void )

(defcfun  ("SdifSizeofDataType" SdifSizeofDataType) :unsigned-int (SdifDataTypeET :unsigned-int))

;;;============================================
;;; SDIF FILE

(defcfun  ("SdifFOpen" SdifFOpenSTR) :pointer  (name sdif-name) (mode :unsigned-int))

(defun SdifFOpen (str mode) 
  (with-foreign-string (cstr str)
    (sdif::SdifFOpenSTR cstr mode)))

(defcfun  ("SdifFClose" SdifFClose) :void (sdifF :pointer))

(defcfun  ("SdifCheckFileFormat" SdifCheckFileFormatSTR) :int  (name sdif-name))

(defun SdifCheckFileFormat (str) 
  (with-foreign-string (cstr str)
    (sdif::SdifCheckFileFormatSTR cstr)))

(defcfun  ("SdifToText" SdifToTextSTR) :unsigned-int (sdifF :pointer) (textStreamName sdif-name))

(defun SdifToText (sdiff str) 
  (with-foreign-string (cstr str)
    (sdif::SdifToTextSTR sdiff cstr)))


(defcfun  ("SdifFGetPos" SdifFGetPos) :int  (pointer :pointer) (pos :pointer))

(defcfun  ("SdifFSetPos" SdifFSetPos) :int ( pointer :pointer) (pos :pointer))

;;;============================================
;;; READ SDIF

(defcfun  ("SdifFGetSignature" SdifFGetSignature) :int  (sdifF :pointer) (nbCharRead :pointer))

(defcfun  ("SdifFReadGeneralHeader" SdifFReadGeneralHeader) :unsigned-int (sdifF :pointer))

(defcfun  ("SdifFReadAllASCIIChunks" SdifFReadAllASCIIChunks) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFReadMatrixHeader" SdifFReadMatrixHeader) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFReadOneRow" SdifFReadOneRow) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFSkipOneRow" SdifFSkipOneRow) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFReadFrameHeader" SdifFReadFrameHeader) :unsigned-int  (sdifF :pointer))


(defcfun  ("SdifFSkipMatrix" SdifFSkipMatrix) :unsigned-int  (sdifF :pointer))

;;; new F dans le nom...
(defcfun  ("SdifFSkipMatrixData" SdifFSkipMatrixData) :unsigned-int  (sdifF :pointer))

;;; new F dans le nom...
(defcfun  ("SdifFSkipFrameData" SdifFSkipFrameData) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFReadPadding" SdifFReadPadding) :unsigned-int  (sdifF :pointer) (Padding :unsigned-int))

;;; new 
(defcfun  ("SdifFSkip" SdifFSkip) :unsigned-int  (sdifF :pointer) (bytes :unsigned-int ))

(defcfun  ("SdifFReadAndIgnore" SdifFReadAndIgnore) :unsigned-int  (sdifF :pointer) (bytes :unsigned-int ))


;;;============================================
;;; SDIF CURR
;;; remplace unsigned-fullword par address pour les renvois de signature.. a tester

(defcfun  ("SdifSignatureToString" SdifSignatureToStringSTR) :pointer (Signature :pointer))

(defun SdifSignatureToString (sdifsignature)
  (foreign-string-to-lisp (sdif::SdifSignatureToStringSTR sdifsignature)))

(defcfun ("SdifStringToSignature" SdifStringToSignatureSTR) :pointer (str :pointer))

(defun SdifStringToSignature (str) 
  (with-foreign-string (cstr str)
    (sdif::SdifStringToSignatureSTR cstr)))

(defcfun  ("SdifFCurrSignature" SdifFCurrSignature)  :pointer (SdifF :pointer))

(defcfun  ("SdifFCleanCurrSignature" SdifFCleanCurrSignature)  :pointer (SdifF :pointer))

(defcfun  ("SdifFCurrFrameSignature" SdifFCurrFrameSignature)  :pointer (SdifF :pointer))

(defcfun  ("SdifFCurrMatrixSignature" SdifFCurrMatrixSignature)  :pointer (SdifF :pointer))

(defcfun  ("SdifFCurrTime" SdifFCurrTime) :double (SdifF :pointer))

(defcfun  ("SdifFCurrID" SdifFCurrID) :unsigned-int  (SdifF :pointer))

;;; !!! en vrai ça retourne pas un int mais SdifDataTypeET ...
(defcfun  ("SdifFCurrDataType" SdifFCurrDataType)  :unsigned-int  (SdifF :pointer))

(defcfun  ("SdifFCurrNbMatrix" SdifFCurrNbMatrix)  :unsigned-int  (SdifF :pointer))

(defcfun  ("SdifFCurrNbCol" SdifFCurrNbCol) :unsigned-int  (SdifF :pointer))

(defcfun  ("SdifFCurrNbRow" SdifFCurrNbRow) :unsigned-int  (SdifF :pointer))

(defcfun  ("SdifFCurrOneRowCol" SdifFCurrOneRowCol) :double (SdifF :pointer) (numcol :unsigned-int))


;;;============================================
;;; CREATE SDIF

(defcfun  ("SdifFSetCurrFrameHeader" SdifFSetCurrFrameHeader) :pointer 
  (SdifF :pointer) (Signature :pointer) (Size :unsigned-int)
  (NbMatrix :unsigned-int) (NumID :unsigned-int) (Time :double))

(defcfun  ("SdifFSetCurrMatrixHeader" SdifFSetCurrMatrixHeader) :pointer (SdifF :pointer) (Signature :pointer) 
               (DataType :unsigned-int) 
               (NbRow :unsigned-int))

(defcfun  ("SdifFSetCurrOneRow" SdifFSetCurrOneRow) :pointer (SdifF :pointer) (values :pointer))

(defcfun  ("SdifFSetCurrOneRowCol" SdifFSetCurrOneRowCol) :pointer (SdifF :pointer) (numcol :unsigned-int) (value :double))

;;;============================================
;;; WRITE SDIF

(defcfun  ("SdifFWriteGeneralHeader" SdifFWriteGeneralHeader) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFWriteAllASCIIChunks" SdifFWriteAllASCIIChunks) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifFWriteFrameHeader" SdifFWriteFrameHeader) :unsigned-int  (sdifF :pointer))

(defcfun  ("SdifUpdateChunkSize" SdifUpdateChunkSize) :void (sdifF :pointer) (chunk-size :unsigned-int))

;;; new
(defcfun  ("SdifUpdateFrameHeader" SdifUpdateFrameHeader) :int
                       (sdifF :pointer)  (chunksize :unsigned-int) (nummat :int)) 

(defcfun  ("SdifFWriteMatrixHeader" SdifFWriteMatrixHeader) :unsigned-int  (SdifF :pointer))

(defcfun  ("SdifFWriteOneRow" SdifFWriteOneRow) :unsigned-int  (SdifF :pointer))

(defcfun  ("SdifFWriteMatrixData" SdifFWriteMatrixData) :unsigned-int  (SdifF :pointer) (data :pointer))

(defcfun  ("SdifFWriteMatrix" SdifFWriteMatrix) :unsigned-int (SdifF :pointer) (Signature :pointer) (DataType :unsigned-int)
               (NbRow :unsigned-int) (NbCol :unsigned-int) (data :pointer))

(defcfun  ("SdifFWritePadding" SdifFWritePadding) :unsigned-int  (sdifF :pointer) (padding :unsigned-int))

(defcfun  ("SdifPaddingCalculate" SdifPaddingCalculate) :unsigned-int  (sdifF :pointer) (nbbytes :unsigned-int))

;;;=================================
;;; ID TABLE

(defcfun  ("SdifStreamIDTablePutSID" SdifStreamIDTablePutSIDSTR) :pointer (SDITable :pointer) (NumID :unsigned-int) (Source sdif-name) (TreeWay sdif-name))

(defun SdifStreamIDTablePutSID (table id name tree)
  (with-foreign-string (strname name)
       (with-foreign-string (strtree tree)
         (SdifStreamIDTablePutSIDSTR table id strname strtree))))

(defcfun ("SdifFStreamIDTable" SdifFStreamIDTable) :pointer (file :pointer))

(defcfun ("SdifStreamIDTableGetNbData" SdifStreamIDTableGetNbData) :unsigned-int (SdifStreamIDTableT :pointer))

(defcfun ("SdifStreamIDTableGetSID" SdifStreamIDTableGetSID) :pointer (SdifStreamIDTableT :pointer) (NumID :unsigned-int))

(defcfun ("SdifStreamIDEntryGetSID" SdifStreamIDEntryGetSID) :unsigned-int (SdifStreamIDT :pointer))

(defcfun ("SdifStreamIDEntryGetSource" SdifStreamIDEntryGetSourceSTR) :pointer (SdifStreamIDT :pointer))

(defun SdifStreamIDEntryGetSource (sid)
  (foreign-string-to-lisp (SdifStreamIDEntryGetSourceSTR sid)))

(defcfun ("SdifStreamIDEntryGetTreeWay" SdifStreamIDEntryGetTreeWaySTR) :pointer (SdifStreamIDT :pointer))

(defun SdifStreamIDEntryGetTreeWay (sid)
  (foreign-string-to-lisp (SdifStreamIDEntryGetTreeWaySTR sid)))



;;;=================================
;;; NVT
(defcfun  ("SdifCreateNameValuesL" SdifCreateNameValuesL) :pointer  (hashsize :unsigned-int))

(defcfun  ("SdifKillNameValuesL" SdifKillNameValuesL) :void (NVL :pointer))

(defcfun  ("SdifFNameValueList" SdifFNameValueList) :pointer (sdifF :pointer))

(defcfun  ("SdifNameValuesLGet" SdifNameValuesLGetSTR) :pointer (NVL :pointer) (name :pointer))

(defun SdifNameValuesLGet (nvl name)
  (with-foreign-string (strname name)
    (sdif::SdifNameValuesLGetSTR nvl strname)))


(defcfun  ("SdifNameValuesLNewTable" SdifNameValuesLNewTable) :pointer (SdifNameValuesLT :pointer) (StreamID :unsigned-int))

(defcfun  ("SdifNameValuesLSetCurrNVT" SdifNameValuesLSetCurrNVT) :pointer  (NVL :pointer) (numNVT :unsigned-int))

(defcfun  ("SdifNameValuesLPutCurrNVT" SdifNameValuesLPutCurrNVTSTR)  :pointer  (SdifNameValuesLT :pointer) (name sdif-name) (value sdif-name))

(defun SdifNameValuesLPutCurrNVT (table name value)
   (with-foreign-string (strname name)
     (with-foreign-string  (strvalue value)
       (sdif::SdifNameValuesLPutCurrNVTSTR table strname strvalue))))


(defcfun  ("SdifNameValueTableGetStreamID" SdifNameValueTableGetStreamID) :int (NVT :pointer))

(defcfun  ("SdifNameValueTableGetNumTable" SdifNameValueTableGetNumTable) :int (NVT :pointer))


(defcfun  ("SdifNameValueTableList" SdifNameValueTableList) :pointer (NVL :pointer))

(defcfun  ("SdifNameValueTableGetHashTable" SdifNameValueTableGetHashTable) :pointer (NVT :pointer))

(defcfun  ("SdifNameValueGetName" SdifNameValueGetNameSTR) :pointer (NV :pointer))

(defun SdifNameValueGetName (nv)
  (foreign-string-to-lisp (sdif::SdifNameValueGetNameSTR nv)))

(defcfun  ("SdifNameValueGetValue" SdifNameValueGetValueSTR) :pointer (NV :pointer))

(defun SdifNameValueGetValue (nv)
  (foreign-string-to-lisp (sdif::SdifNameValueGetValueSTR nv)))





;;;=================================
;;;TYPES TABLE

(defcfun  ("SdifFGetMatrixTypesTable" SdifFGetMatrixTypesTable)  :pointer  (file :pointer))

(defcfun  ("SdifFGetFrameTypesTable" SdifFGetFrameTypesTable) :pointer  (file :pointer))

(defcfun  ("SdifCreateMatrixType" SdifCreateMatrixType) :pointer (Signature :unsigned-int) (SdifMatrixTypeT :pointer))

(defcfun  ("SdifPutMatrixType" SdifPutMatrixType) :void (mtable :pointer) (SdifMatrixTypeT :pointer))

(defcfun  ("SdifMatrixTypeInsertTailColumnDef" SdifMatrixTypeInsertTailColumnDefSTR) :pointer (matrix :pointer) (str sdif-name))

(defun SdifMatrixTypeInsertTailColumnDef (matrixtype name)
  (with-foreign-string (strname name)
    (sdif::SdifMatrixTypeInsertTailColumnDefSTR matrixtype strname)))

(defcfun  ("SdifCreateFrameType" SdifCreateFrameType) :pointer (Signature :unsigned-int) (SdifMatrixTypeT :pointer))

(defcfun  ("SdifPutFrameType" SdifPutFrameType) :void (ftable :pointer) (SdifFrameTypeT :pointer))

(defcfun  ("SdifFrameTypePutComponent" SdifFrameTypePutComponentSTR) :pointer (frame :pointer) (sign :unsigned-int) (str sdif-name))

(defun SdifFrameTypePutComponent (frametype sign name)
  (with-foreign-string (strname name)
    (sdif::SdifFrameTypePutComponentSTR frametype sign strname)))


(defcfun  ("SdifTestMatrixType" SdifTestMatrixType) :pointer (sdifF :pointer) (signature :pointer))

(defcfun  ("SdifTestFrameType" SdifTestFrameType) :pointer (sdifF :pointer) (signature :pointer))

(defcfun  ("SdifMatrixTypeGetColumnName" SdifMatrixTypeGetColumnNameSTR) :pointer (MatrixType :pointer) (num :int))

(defun SdifMatrixTypeGetColumnName (mtype num) 
  (foreign-string-to-lisp (sdif::SdifMatrixTypeGetColumnNameSTR mtype num)))

(defcfun  ("SdifMatrixTypeGetNbColumns" SdifMatrixTypeGetNbColumns) :int (MatrixType :pointer))

(defcfun  ("SdifFrameTypeGetNbComponents" SdifFrameTypeGetNbComponents) :int (FrameType :pointer))

(defcfun  ("SdifFrameTypeGetNthComponent" SdifFrameTypeGetNthComponent) :pointer (FrameType :pointer) (num :int))

(defcfun  ("SdifFrameTypeGetComponentSignature" SdifFrameTypeGetComponentSignature) :pointer (Component :pointer))

;;;=================================
;;; STRING

(defcfun ("SdifStringNew" SdifStringNew) :pointer)

(defcfun ("SdifStringFree" SdifStringFree) :void (str :pointer))

(defcfun ("SdifStringAppend" SdifStringAppendSTR) :int (str :pointer) (str1 :pointer))

(defun SdifStringAppend (ptr str)
  (with-foreign-string (strp str)
    (setf rep (sdif::SdifStringAppendSTR ptr strp))))

(defcfun ("SdifStringGetC" SdifStringGetC) :int (str :pointer))

(defcfun ("SdifFGetAllTypefromSdifString" SdifFGetAllTypefromSdifString) :unsigned-int (file :pointer) (str :pointer))
  


;;;=================================
;;; LIST

(defcfun ("SdifListGetNbData" SdifListGetNbData) :unsigned-int (list :pointer))

(defcfun ("SdifListInitLoop" SdifListInitLoop) :int (list :pointer))

(defcfun ("SdifListIsNext" SdifListIsNext) :int (list :pointer))

(defcfun ("SdifListGetNext" SdifListGetNext) :pointer (list :pointer))


;;;=================================
;;; HASH TABLE ITERATOR

(defcfun ("SdifHashTableGetNbData" SdifHashTableGetNbData) :unsigned-int (sdifhashtable :pointer))

(defcfun ("SdifCreateHashTableIterator" SdifCreateHashTableIterator) :pointer (sdifhashtable :pointer))

(defcfun ("SdifKillHashTableIterator" SdifKillHashTableIterator) :void (htiterator :pointer))

(defcfun ("SdifHashTableIteratorInitLoop" SdifHashTableIteratorInitLoop) :pointer (htiterator :pointer) (sdifhashtable :pointer))

(defcfun ("SdifHashTableIteratorIsNext" SdifHashTableIteratorIsNext) :int (htiterator :pointer))

(defcfun ("SdifHashTableIteratorGetNext" SdifHashTableIteratorGetNext) :pointer (htiterator :pointer))


















