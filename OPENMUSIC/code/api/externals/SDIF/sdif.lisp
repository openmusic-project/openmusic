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
   (:use common-lisp))

(in-package :sdif)

;;;==============================
;;; CHARGEMENT

(defvar *sdif-framework* nil)


(defvar *sdif-pathname* 
  #+win32
  "/WINDOWS/system32/sdif.dll"
  #+(or darwin macos macosx) 
  "/Library/Frameworks/SDIF.framework/SDIF"
  #+(or linux (and clisp unix (not macos)))
  ;; "/usr/lib/libsdif.so"
  "libsdif.so"
  )

;;; !!! *sdif-pathname* is modified in OM

(defun init-sdif-framework ()
  (setf *sdif-framework*
	#-linux (if (probe-file *sdif-pathname*)
		    (progn 
		      (print (concatenate 'string "Loading SDIF library: " (namestring *sdif-pathname*))) 
					;(cffi:load-foreign-library *sdif-pathname*)
		      (fli:register-module "SDIF" 
					   :real-name (namestring *sdif-pathname*)
					   :connection-style :immediate)
		      t))
	#+linux (progn 
		  (print (concatenate 'string "Loading SDIF library: " (namestring *sdif-pathname*))) 
		  (fli:register-module "SDIF" 
				       :real-name (namestring *sdif-pathname*)
				       :connection-style :immediate)
		  t)))

; (oa::om-start-sdif)
; (cffi:load-foreign-library "/Library/Frameworks/SDIF.framework/Versions/3.11/SDIF")
; (init-sdif-framework)
; (sdif::sdifprintversion)

#-cffi-new (cffi:define-foreign-type sdif-name () ':pointer)
#+cffi-new (cffi:defctype sdif-name :pointer)

(defconstant eUnknownFileMode 0)
(defconstant eWriteFile 1)
(defconstant eReadFile 2)
(defconstant eReadWriteFile 3)
(defconstant ePredefinedTypes 4)


;;;============================================
;;; SDIF GENERAL

(cffi:defcfun  ("SdifGenInit" SdifGenInitSTR) :void (PredefinedTypesFile sdif-name))

(defun SdifGenInit (str) 
  (cffi:with-foreign-string (cstr str)
    (sdif::SdifGenInitSTR cstr)))

(cffi:defcfun  ("SdifGenInitCond" SdifGenInitCondSTR) :void (PredefinedTypesFile sdif-name))

(defun SdifGenInitCond (str) 
  (cffi:with-foreign-string (cstr str)
    (sdif::SdifGenInitCondSTR cstr)))

(cffi:defcfun  ("SdifGenKill" SdifGenKill) :void)

(cffi:defcfun  ("SdifPrintVersion" SdifPrintVersion) :void )

(cffi:defcfun  ("SdifenableErrorOutput" SdifenableErrorOutput) :void )

(cffi:defcfun  ("SdifDisableErrorOutput" SdifDisableErrorOutput) :void )


;;;============================================
;;; SDIF FILE

(cffi:defcfun  ("SdifFOpen" SdifFOpenSTR) :pointer  (name sdif-name) (mode :unsigned-int))

(defun SdifFOpen (str mode) 
  (cffi:with-foreign-string (cstr str)
    (sdif::SdifFOpenSTR cstr mode)))

(cffi:defcfun  ("SdifFClose" SdifFClose) :void (sdifF :pointer))

(cffi:defcfun  ("SdifCheckFileFormat" SdifCheckFileFormatSTR) :int  (name sdif-name))

(defun SdifCheckFileFormat (str) 
  (cffi:with-foreign-string (cstr str)
    (sdif::SdifCheckFileFormatSTR cstr)))

(cffi:defcfun  ("SdifToText" SdifToTextSTR) :unsigned-int (sdifF :pointer) (textStreamName sdif-name))

(defun SdifToText (sdiff str) 
  (cffi:with-foreign-string (cstr str)
    (sdif::SdifToTextSTR sdiff cstr)))


(cffi:defcfun  ("SdifFGetPos" SdifFGetPos) :int  (pointer :pointer) (pos :pointer))

(cffi:defcfun  ("SdifFSetPos" SdifFSetPos) :int ( pointer :pointer) (pos :pointer))

;;;============================================
;;; READ SDIF

(cffi:defcfun  ("SdifFGetSignature" SdifFGetSignature) :int  (sdifF :pointer) (nbCharRead :pointer))

(cffi:defcfun  ("SdifFReadGeneralHeader" SdifFReadGeneralHeader) :unsigned-int (sdifF :pointer))

(cffi:defcfun  ("SdifFReadAllASCIIChunks" SdifFReadAllASCIIChunks) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFReadMatrixHeader" SdifFReadMatrixHeader) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFReadOneRow" SdifFReadOneRow) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFSkipOneRow" SdifFSkipOneRow) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFReadFrameHeader" SdifFReadFrameHeader) :unsigned-int  (sdifF :pointer))


(cffi:defcfun  ("SdifFSkipMatrix" SdifFSkipMatrix) :unsigned-int  (sdifF :pointer))

;;; new F dans le nom...
(cffi:defcfun  ("SdifFSkipMatrixData" SdifFSkipMatrixData) :unsigned-int  (sdifF :pointer))

;;; new F dans le nom...
(cffi:defcfun  ("SdifFSkipFrameData" SdifFSkipFrameData) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFReadPadding" SdifFReadPadding) :unsigned-int  (sdifF :pointer) (Padding :unsigned-int))

;;; new 
(cffi:defcfun  ("SdifFSkip" SdifFSkip) :unsigned-int  (sdifF :pointer) (bytes :unsigned-int ))

(cffi:defcfun  ("SdifFReadAndIgnore" SdifFReadAndIgnore) :unsigned-int  (sdifF :pointer) (bytes :unsigned-int ))


;;;============================================
;;; SDIF CURR
;;; remplace unsigned-fullword par address pour les renvois de signature.. a tester

(cffi:defcfun  ("SdifSignatureToString" SdifSignatureToStringSTR) :pointer (Signature :pointer))

(defun SdifSignatureToString (sdifsignature)
  (cffi::foreign-string-to-lisp (sdif::SdifSignatureToStringSTR sdifsignature)))

(cffi:defcfun ("SdifStringToSignature" SdifStringToSignatureSTR) :pointer (str :pointer))

(defun SdifStringToSignature (str) 
  (cffi:with-foreign-string (cstr str)
    (sdif::SdifStringToSignatureSTR cstr)))

(cffi:defcfun  ("SdifFCurrSignature" SdifFCurrSignature)  :pointer (SdifF :pointer))

(cffi:defcfun  ("SdifFCleanCurrSignature" SdifFCleanCurrSignature)  :pointer (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrFrameSignature" SdifFCurrFrameSignature)  :pointer (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrMatrixSignature" SdifFCurrMatrixSignature)  :pointer (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrTime" SdifFCurrTime) :double (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrID" SdifFCurrID) :unsigned-int  (SdifF :pointer))

;;; !!! en vrai ça retourne pas un int mais SdifDataTypeET ...
(cffi:defcfun  ("SdifFCurrDataType" SdifFCurrDataType)  :unsigned-int  (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrNbMatrix" SdifFCurrNbMatrix)  :unsigned-int  (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrNbCol" SdifFCurrNbCol) :unsigned-int  (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrNbRow" SdifFCurrNbRow) :unsigned-int  (SdifF :pointer))

(cffi:defcfun  ("SdifFCurrOneRowCol" SdifFCurrOneRowCol) :double (SdifF :pointer) (numcol :unsigned-int))


;;;============================================
;;; CREATE SDIF

(cffi:defcfun  ("SdifFSetCurrFrameHeader" SdifFSetCurrFrameHeader) :pointer 
  (SdifF :pointer) (Signature :pointer) (Size :unsigned-int)
  (NbMatrix :unsigned-int) (NumID :unsigned-int) (Time :double))

(cffi:defcfun  ("SdifFSetCurrMatrixHeader" SdifFSetCurrMatrixHeader) :pointer (SdifF :pointer) (Signature :pointer) 
               (DataType :unsigned-int) 
               (NbRow :unsigned-int))

(cffi:defcfun  ("SdifFSetCurrOneRow" SdifFSetCurrOneRow) :pointer (SdifF :pointer) (values :pointer))

(cffi:defcfun  ("SdifFSetCurrOneRowCol" SdifFSetCurrOneRowCol) :pointer (SdifF :pointer) (numcol :unsigned-int) (value :double))

;;;============================================
;;; WRITE SDIF

(cffi:defcfun  ("SdifFWriteGeneralHeader" SdifFWriteGeneralHeader) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFWriteAllASCIIChunks" SdifFWriteAllASCIIChunks) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifFWriteFrameHeader" SdifFWriteFrameHeader) :unsigned-int  (sdifF :pointer))

(cffi:defcfun  ("SdifUpdateChunkSize" SdifUpdateChunkSize) :void (sdifF :pointer) (chunk-size :unsigned-int))

;;; new
(cffi:defcfun  ("SdifUpdateFrameHeader" SdifUpdateFrameHeader) :int
                       (sdifF :pointer)  (chunksize :unsigned-int) (nummat :int)) 

(cffi:defcfun  ("SdifFWriteMatrixHeader" SdifFWriteMatrixHeader) :unsigned-int  (SdifF :pointer))

(cffi:defcfun  ("SdifFWriteOneRow" SdifFWriteOneRow) :unsigned-int  (SdifF :pointer))

(cffi:defcfun  ("SdifFWriteMatrixData" SdifFWriteMatrixData) :unsigned-int  (SdifF :pointer) (data :pointer))

(cffi:defcfun  ("SdifFWriteMatrix" SdifFWriteMatrix) :unsigned-int (SdifF :pointer) (Signature :pointer) (DataType :unsigned-int)
               (NbRow :unsigned-int) (NbCol :unsigned-int) (data :pointer))

(cffi:defcfun  ("SdifFWritePadding" SdifFWritePadding) :unsigned-int  (sdifF :pointer) (padding :unsigned-int))


;;;=================================
;;; ID TABLE

(cffi:defcfun  ("SdifStreamIDTablePutSID" SdifStreamIDTablePutSIDSTR) :pointer (SDITable :pointer) (NumID :unsigned-int) (Source sdif-name) (TreeWay sdif-name))

(defun SdifStreamIDTablePutSID (table id name tree)
  (cffi:with-foreign-string (strname name)
       (cffi:with-foreign-string  (strtree tree)
         (SdifStreamIDTablePutSIDSTR table id strname strtree))))

(cffi:defcfun ("SdifFStreamIDTable" SdifFStreamIDTable) :pointer (file :pointer))


;;;=================================
;;; NVT
(cffi:defcfun  ("SdifCreateNameValuesL" SdifCreateNameValuesL) :pointer  (hashsize :unsigned-int))

(cffi:defcfun  ("SdifKillNameValuesL" SdifKillNameValuesL) :void (NVL :pointer))

(cffi:defcfun  ("SdifFNameValueList" SdifFNameValueList) :pointer (sdifF :pointer))

(cffi:defcfun  ("SdifNameValuesLGet" SdifNameValuesLGetSTR) :pointer (NVL :pointer) (name :pointer))

(defun SdifNameValuesLGet (nvl name)
  (cffi:with-foreign-string (strname name)
    (sdif::SdifNameValuesLGetSTR nvl strname)))


(cffi:defcfun  ("SdifNameValuesLNewTable" SdifNameValuesLNewTable) :pointer (SdifNameValuesLT :pointer) (StreamID :unsigned-int))

(cffi:defcfun  ("SdifNameValuesLSetCurrNVT" SdifNameValuesLSetCurrNVT) :pointer  (NVL :pointer) (numNVT :unsigned-int))

(cffi:defcfun  ("SdifNameValuesLPutCurrNVT" SdifNameValuesLPutCurrNVTSTR)  :pointer  (SdifNameValuesLT :pointer) (name sdif-name) (value sdif-name))

(defun SdifNameValuesLPutCurrNVT (table name value)
   (cffi:with-foreign-string (strname name)
     (cffi:with-foreign-string  (strvalue value)
       (sdif::SdifNameValuesLPutCurrNVTSTR table strname strvalue))))


(cffi:defcfun  ("SdifNameValueTableGetStreamID" SdifNameValueTableGetStreamID) :int (NVT :pointer))

(cffi:defcfun  ("SdifNameValueTableGetNumTable" SdifNameValueTableGetNumTable) :int (NVT :pointer))


(cffi:defcfun  ("SdifNameValueTableList" SdifNameValueTableList) :pointer (NVL :pointer))

(cffi:defcfun  ("SdifNameValueTableGetHashTable" SdifNameValueTableGetHashTable) :pointer (NVT :pointer))

(cffi:defcfun  ("SdifNameValueGetName" SdifNameValueGetNameSTR) :pointer (NV :pointer))

(defun SdifNameValueGetName (nv)
  (cffi::foreign-string-to-lisp (sdif::SdifNameValueGetNameSTR nv)))

(cffi:defcfun  ("SdifNameValueGetValue" SdifNameValueGetValueSTR) :pointer (NV :pointer))

(defun SdifNameValueGetValue (nv)
  (cffi::foreign-string-to-lisp (sdif::SdifNameValueGetValueSTR nv)))





;;;=================================
;;;TYPES TABLE

(cffi:defcfun  ("SdifFGetMatrixTypesTable" SdifFGetMatrixTypesTable)  :pointer  (file :pointer))

(cffi:defcfun  ("SdifFGetFrameTypesTable" SdifFGetFrameTypesTable) :pointer  (file :pointer))

(cffi:defcfun  ("SdifCreateMatrixType" SdifCreateMatrixType) :pointer (Signature :unsigned-int) (SdifMatrixTypeT :pointer))

(cffi:defcfun  ("SdifPutMatrixType" SdifPutMatrixType) :void (mtable :pointer) (SdifMatrixTypeT :pointer))

(cffi:defcfun  ("SdifMatrixTypeInsertTailColumnDef" SdifMatrixTypeInsertTailColumnDefSTR) :pointer (matrix :pointer) (str sdif-name))

(defun SdifMatrixTypeInsertTailColumnDef (matrixtype name)
  (cffi:with-foreign-string (strname name)
    (sdif::SdifMatrixTypeInsertTailColumnDefSTR matrixtype strname)))

(cffi:defcfun  ("SdifCreateFrameType" SdifCreateFrameType) :pointer (Signature :unsigned-int) (SdifMatrixTypeT :pointer))

(cffi:defcfun  ("SdifPutFrameType" SdifPutFrameType) :void (ftable :pointer) (SdifFrameTypeT :pointer))

(cffi:defcfun  ("SdifFrameTypePutComponent" SdifFrameTypePutComponentSTR) :pointer (frame :pointer) (sign :unsigned-int) (str sdif-name))

(defun SdifFrameTypePutComponent (frametype sign name)
  (cffi:with-foreign-string (strname name)
    (sdif::SdifFrameTypePutComponentSTR frametype sign strname)))


(cffi:defcfun  ("SdifTestMatrixType" SdifTestMatrixType) :pointer (sdifF :pointer) (signature :pointer))

(cffi:defcfun  ("SdifTestFrameType" SdifTestFrameType) :pointer (sdifF :pointer) (signature :pointer))

(cffi:defcfun  ("SdifMatrixTypeGetColumnName" SdifMatrixTypeGetColumnNameSTR) :pointer (MatrixType :pointer) (num :int))

(defun SdifMatrixTypeGetColumnName (mtype num) 
  (cffi::foreign-string-to-lisp (sdif::SdifMatrixTypeGetColumnNameSTR mtype num)))

(cffi:defcfun  ("SdifMatrixTypeGetNbColumns" SdifMatrixTypeGetNbColumns) :int (MatrixType :pointer))

(cffi:defcfun  ("SdifFrameTypeGetNbComponents" SdifFrameTypeGetNbComponents) :int (FrameType :pointer))

(cffi:defcfun  ("SdifFrameTypeGetNthComponent" SdifFrameTypeGetNthComponent) :pointer (FrameType :pointer) (num :int))

(cffi:defcfun  ("SdifFrameTypeGetComponentSignature" SdifFrameTypeGetComponentSignature) :pointer (Component :pointer))

;;;=================================
;;; STRING

(cffi:defcfun ("SdifStringNew" SdifStringNew) :pointer)

(cffi:defcfun ("SdifStringFree" SdifStringFree) :void (str :pointer))

(cffi:defcfun ("SdifStringAppend" SdifStringAppendSTR) :int (str :pointer) (str1 :pointer))

(defun SdifStringAppend (ptr str)
  (cffi:with-foreign-string (strp str)
    (setf rep (sdif::SdifStringAppendSTR ptr strp))))

(cffi:defcfun ("SdifStringGetC" SdifStringGetC) :int (str :pointer))

(cffi:defcfun ("SdifFGetAllTypefromSdifString" SdifFGetAllTypefromSdifString) :unsigned-int (file :pointer) (str :pointer))
  


;;;=================================
;;; LIST

(cffi:defcfun ("SdifListGetNbData" SdifListGetNbData) :unsigned-int (list :pointer))

(cffi:defcfun ("SdifListInitLoop" SdifListInitLoop) :int (list :pointer))

(cffi:defcfun ("SdifListIsNext" SdifListIsNext) :int (list :pointer))

(cffi:defcfun ("SdifListGetNext" SdifListGetNext) :pointer (list :pointer))


;;;=================================
;;; HASH TABLE ITERATOR

(cffi:defcfun ("SdifHashTableGetNbData" SdifHashTableGetNbData) :unsigned-int (sdifhashtable :pointer))

(cffi:defcfun ("SdifCreateHashTableIterator" SdifCreateHashTableIterator) :pointer (sdifhashtable :pointer))

(cffi:defcfun ("SdifKillHashTableIterator" SdifKillHashTableIterator) :void (htiterator :pointer))

(cffi:defcfun ("SdifHashTableIteratorInitLoop" SdifHashTableIteratorInitLoop) :pointer (htiterator :pointer) (sdifhashtable :pointer))

(cffi:defcfun ("SdifHashTableIteratorIsNext" SdifHashTableIteratorIsNext) :int (htiterator :pointer))

(cffi:defcfun ("SdifHashTableIteratorGetNext" SdifHashTableIteratorGetNext) :pointer (htiterator :pointer))
