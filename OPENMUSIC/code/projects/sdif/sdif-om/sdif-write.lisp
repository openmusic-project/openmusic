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

(in-package :om)

;==============================================
;writing sdif file
;==============================================

(defmethod save-sdif ((self t) ptr) nil)

(defmethod save-sdif ((self list) ptr) (mapcar #'(lambda (elt) (save-sdif elt ptr)) self))


;;; MATRIX
;;; Attention entre OMArray et SDIF Matrix les ROWs-COLS siont inversés

(defun calc-pad (n)
  (if (zerop (mod n 8)) n
      (* 8 (+ (floor n 8) 1))))

(defmethod calcul-sdif-size ((self sdifmatrix))
  (+ (calc-pad (* 4 (numcols self) (numrows self))) 16))


(defmethod save-sdif ((matrix sdifmatrix) ptr)
  (let* ((datatype 4)
         (numrow (numrows matrix)) 
         (rowsize (* datatype numrow))
         (vals (om-make-pointer (* datatype numrow (numcols matrix)) :clear t)))
   (loop for i from 0 to (- (numcols matrix) 1) do
         (let ((sdifrow (first-n (get-array-col matrix i) numrow)))
            (loop for j from 0 to (- numrow 1)
                  for val in sdifrow do
                  (om-write-ptr vals (+ (* j datatype) (* i rowsize)) :float (coerce val 'single-float))
                  )
             ))
    (sdif::SdifFWriteMatrix ptr (sdif::SdifStringToSignature (signature matrix)) datatype
                            (numcols matrix) numrow vals)
    ))

;;; RAW MATRIX : data is already prepared

(defmethod save-sdif ((matrix raw-sdifmatrix) ptr)
  (let* ((datatype 4)
         (values (om-make-pointer (* datatype (num-fields matrix) (num-elts matrix)))))
    (loop for val in (data matrix) 
          for i = 0 then (+ i 1) do
          (om-write-ptr values (* i datatype) :float (coerce val 'single-float))
          )
    (sdif::SdifFWriteMatrix ptr (sdif::SdifStringToSignature (signature matrix)) datatype
                            (num-elts matrix) 
                            (num-fields matrix) values)
    ))


(defmethod calcul-sdif-size ((self raw-sdifmatrix))
   (+ (calc-pad (* 4 (num-elts self) (num-fields self))) 16))


;;; FRAME
(defmethod calcul-sdif-size ((self sdifframe))
   (let ((rep 0))
     (loop for item in (LMatrix self) do
           (setf rep (+ rep (calcul-sdif-size item))))
     (+ rep 16)))

(defmethod save-sdif ((self sdifframe) fileptr)
  (let ((framesize (calcul-sdif-size self)))
    (sdif::SdifFSetCurrFrameHeader fileptr (sdif::SdifStringToSignature (signature self)) framesize (length (Lmatrix self))
                                   (streamId self) (coerce (Ftime self) 'double-float))
    (sdif::SdifFWriteFrameHeader fileptr)
    (loop for item in (LMatrix self) do
          (save-sdif item fileptr)
          )
    ))



;;; STREAM
(defmethod save-sdif ((self sdifstream) fileptr)
   (save-sdif (LFrames self)))


;;; TYPES / NVT / IDS =
;;; A faire avant d'appeler SdifWriteAllASCIIChunks

;;; TYPES
(defmethod format-sdif-type-string ((self sdiftype))
   (let ((str "") (desc ""))
     (cond ((equal 'F (struct self)) 
               (loop for item in (description self) do
                       (if (and (listp item) (stringp (car item)) (stringp (cadr item)))
                           (setf desc (string+ desc (car item) " " (cadr item) "; "))
                         (om-beep-msg (format nil "WARNING: Ill-formed frame type could not be read: ~A" item))))
               (setf desc (subseq desc 0 (max 0 (- (length desc) 1))))
               (setf str (string+ str " 1FTD " (signature self) " {" desc "}")))
              ((equal 'M (struct self)) 
               (loop for item in (description self) do
                       (if (stringp item)
                           (setf desc (string+ desc item ", "))
                         (om-beep-msg (format nil "WARNING: Ill-formed frame type could not be read: ~A" item))))
               (setf desc (subseq desc 0 (max 0 (- (length desc) 2))))
               (setf str (string+ str " 1MTD " (signature self) " {" desc "}")))
              (t nil))
     str))

(defun write-sdif-types (thefile typesstr)
  (let ((sstr (sdif::SdifStringNew)))
    (sdif::SdifStringAppend sstr typesstr)
    (sdif::SdifStringGetC sstr)
    (sdif::SdifFGetAllTypefromSdifString thefile sstr)))
  
(defun write-types-table (thefile typeslist)
   (let ((str "{") (FList nil) (MList nil))
     (loop for typedef in typeslist do
             (when (sdiftypep typedef) 
                 (cond ((equal 'F (struct typedef)) (push typedef Flist))
                       ((equal 'M (struct typedef)) (push typedef Mlist)))))
     (loop for mdef in Mlist do
             (setf str (string+ str (format-sdif-type-string mdef))))
     (loop for fdef in Flist do
             (setf str (string+ str (format-sdif-type-string fdef))))
     (setf str (string+ str "}"))
     (write-sdif-types thefile str)))
 


;;; NVT
(defun write-1nvt-table (thefile namelist vallist)
   (let ((nvt (sdif::SdifFNameValueList thefile)))
     (sdif::SdifNameValuesLNewTable nvt #xffff)
     (loop for name in namelist
           for val in vallist do
           (sdif::SdifNameValuesLPutCurrNVT nvt name val))))

(defun write-nvt-tables (fileptr tablelist)
  (mapcar #'(lambda (table)
              (save-sdif table fileptr)) 
          tablelist))

(defmethod save-sdif ((self SDIFNVT) fileptr)
  (let ((nvtlist (sdif::SdifFNameValueList fileptr)))
    (sdif::SdifNameValuesLNewTable nvtlist (or (ID self) #xffff))
    (when (tablename self)
      (sdif::SdifNameValuesLPutCurrNVT nvtlist "TableName" (tablename self)))
    (loop for NV-pair in (nv-pairs self)
          do (sdif::SdifNameValuesLPutCurrNVT nvtlist (car nv-pair) (cadr nv-pair)))))



;;; IDS

(defun sdif-write-IDS (file id str tree)
   (let ((idstable (sdif::SdifFStreamIDTable file)))
     (sdif::SdifStreamIDTablePutSID idstable id str tree)))

(defmethod save-sdif ((self SDIFSID) fileptr)
  (sdif-write-IDS fileptr
                  (id self) (source self) 
                  (cond ((stringp (treeway self)) (treeway self))
                        ((listp (treeway self)) (reduce #'(lambda (a b) (format nil "~a/~a" a b)) (treeway self)))
                        (t ""))))

(defun write-SID-table (fileptr SID-list)
  (mapcar #'(lambda (table)
              (save-sdif table fileptr)) 
          SID-list))


(defmethod! save-sdif-file ((self sdif-buffer) &key out)
  :icon 639
   :indoc '("an SDIF-buffer" "output pathname")
   :initvals '(nil nil)
   :doc "Saves the contents of <self> as an SDIF file in <outpath>.
   <self> is an SDIF-Buffer object or some other object having the SAVE-SDIF-FILE method implemented.

If <outpath> is not specified, a pop-up dialog will open and allow to choose a destination pathname.
"
  (let* ((outfile (or (and out 
                           (handle-new-file-exists out))
                      (om-choose-new-file-dialog)))
         (dir (om-make-pathname :directory outfile)))
    (when outfile 
      (unless (probe-file dir)
        (om-create-directory dir))
      (let ((thefile (sdif::sdif-open-file (namestring outfile) :eWriteFile)))
        (sdif::SdifFWriteGeneralHeader thefile)
        (write-types-table thefile (list! (types self)))
        (write-nvt-tables thefile (cons (default-om-NVT) (list! (NVTs self))))
        (write-sid-table thefile (list! (SIDs self)))
           (sdif::SdifFWriteAllASCIIChunks thefile)
           (loop for item in (LFrames self) do
                 (save-sdif item thefile))
           (sdif::sdif-close-file thefile))
      outfile)))


(defmethod! write-sdif-file ((self list) &key outpath)
   :icon 639
   :indoc '("a list of SDIF-frames" "output pathname")
   :initvals '(nil nil)
   :doc "Saves the contents of <self> as an SDIF file in <outpath>. 
<self> is a list of SDIFframe object or some other object having the SAVE-SDIF-FILE method implemented

If <outpath> is not specified, a pop-up dialog will open and allow to choose a destination pathname.
"
   (let* ((outfile (or (and outpath 
                            (handle-new-file-exists outpath))
                       (om-choose-new-file-dialog)))
          (dir (om-make-pathname :directory outfile)))
       (when outfile 
         (unless (probe-file dir)
           (om-create-directory dir))
         (let ((thefile (sdif::sdif-open-file (namestring outfile) :eWriteFile)))
           (sdif::SdifFWriteGeneralHeader thefile)
           (sdif::SdifFWriteAllASCIIChunks thefile)
           (loop for item in self do
                 (save-sdif item thefile))
           (sdif::sdif-close-file thefile))
         outfile)))



;;;=========================================

;;; INTERFACE FILE-BOX
;;; interface file-box

(defmethod! sdif-write-frame ((self sdifframe) fstream)
  :icon 908
  :indoc '("an SDIFFrame to write" "an SDIF file pointer")
  :doc "Writes the SDIF frame <self> in <fstream>.

<fstream> is a file pointer represented by the 'streamFile' box in the File-Box editor." 
  (save-sdif self fstream))

(defmethod! sdif-write-frame ((self t) fstream)
  :icon 908
  (print "Error: only write frames in SDIF files!") 
  nil)

(defmethod! sdif-write-header (fstream &optional (types nil) (nvts nil) (sids nil))
  :icon 908
  :indoc '("an SDIF file pointer" "list of SDIFType" "list of SDIFNVT" "list of SDIFSID")
  :doc "Writes the header of the SDIF file in <fstream>.

This is a compulsory operation before to start writing SDIF frames in the file.
<fstream> is a file pointer represented by the 'streamFile' box in the File-Box editor.

<types>, <nvts> and <sids> are SDIF types, name/value tables to declare and write in the file header.
" 
  (sdif::SdifFWriteGeneralHeader fstream)
  (write-nvt-tables fstream (cons (default-om-NVT) (list! nvts)))
  (when types (write-types-table fstream (list! types)))
  (when sids (write-sid-table fstream sids))
  (sdif::SdifFWriteAllASCIIChunks fstream))


;;; REDEFINITION
(defmethod compile-patch ((self openfilepatch))
  "Code generates by Loop patches is generate by this method."
  (let* ((boxes (boxes self))
         (oldletlist *let-list*)
         (*let-list* nil)
         (oldlambdacontext *lambda-context*)
         (do-box (car (find-class-boxes boxes 'OMLoopDo)))
         (final-box (car (find-class-boxes boxes 'OMFinalDo)))
         (init-box (car (find-class-boxes boxes 'OMinitDo)))
         (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
         (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
         (loop-boxes (find-loop-boxes boxes))
         (loop-code (mapcan #'(lambda (theout)
                                (loop-gen-code theout 0)) loop-boxes))
         (acum-boxes (find-acum-boxes boxes))
         (acum-declaration (mapcar #'(lambda (acumm)
                                       (declare-closure acumm)) acum-boxes))
         (acum-inits (mapcar #'(lambda (acumm)
                                 (gen-code acumm -1)) acum-boxes))
         (stream-boxes (find-class-boxes boxes 'Streambox))
         (streams (loop for sb in stream-boxes collect (list 
                                                       (get-stream-type sb)
                                                       (car (decode sb))
                                                       (direction sb)
                                                       (if-ex sb)
                                                       (id sb))))
         (streamids (loop for s in streams collect (nth 4 s)))
         (stream-box (car stream-boxes))
         body init)
    (setf body (gen-code do-box 0))
    (setf init (gen-code init-box 0))
    (cond
     (loop-boxes
      (eval `(defun ,(intern (string (first (code self))) :om)  (,.symbols)
                  (let (rep ,.streamids)
                  ,.(remove nil (loop for item in streams collect 
                                      (when (nth 1 item)
                                        (cond ((and (find :sdif *features*) (equal (nth 0 item) 'sdif))
                                               `(setf ,(nth 4 item) (sdif::sdif-open-file (namestring (eval ,(nth 1 item))) 
                                                                                    (cond ((equal ,(nth 2 item) :input) :eReadFile)
                                                                                          ((equal ,(nth 2 item) :output) :eWriteFile)
                                                                                          (t :eReadWriteFile)))))
                                              ((equal (nth 0 item) 'text)
                                               `(setf ,(nth 4 item) (open (eval ,(nth 1 item)) 
                                                                   :if-does-not-exist :create 
                                                                   :direction ,(nth 2 item) :if-exists ,(nth 3 item)))))
                                        )))

                  (let (,.acum-declaration (iter-count 0)) 
                    ,.acum-inits
                    (let* ,*let-list* ,init)
                    (setf rep (loop ,.loop-code 
                                    do ,(loop-check-code)
                                    finally (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                                    collect  (gen-code final-box i)))) do
                                    (let* ,*let-list* ,body)))
                    )
                         
                         ,.(remove nil (loop for item in streams collect 
                                             (when (nth 1 item)
                                               (cond ((and (find :sdif *features*) (equal (nth 0 item) 'sdif))
                                                      `(sdif::sdif-close-file ,(nth 4 item)))
                                                     ((equal (nth 0 item) 'text)
                                                      `(close ,(nth 4 item))))
                                               )))
                         rep)
                  )))
     (t 
      (eval `(defun ,(intern (string (first (code self))) :om)  (,.symbols) 
               (let (rep ,.streamids)
                  ,.(remove nil (loop for item in streams collect 
                                      (when (nth 1 item)
                                        (cond ((and (find :sdif *features*) (equal (nth 0 item) 'sdif))
                                               `(setf ,(nth 4 item) (sdif::sdif-open-file (namestring (eval ,(nth 1 item))) 
                                                                                    (cond ((equal ,(nth 2 item) :input) :eReadFile)
                                                                                          ((equal ,(nth 2 item) :output) :eWriteFile)
                                                                                          (t :eReadWriteFile)))))
                                              ((equal (nth 0 item) 'text)
                                               `(setf ,(nth 4 item) (open (eval ,(nth 1 item)) 
                                                                   :if-does-not-exist :create 
                                                                   :direction ,(nth 2 item) :if-exists ,(nth 3 item)))))
                                        )))

                  (let (,.acum-declaration) ,.acum-inits
                       (let* ,*let-list* 
                         ,init
                         ,body
                         (setf rep (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                   collect  (gen-code final-box i))))))
                  
                  ,.(remove nil (loop for item in streams collect 
                                      (when (nth 1 item)
                                        (cond ((and (find :sdif *features*) (equal (nth 0 item) 'sdif))
                                                `(sdif::sdif-close-file ,(nth 4 item)))
                                                ((equal (nth 0 item) 'text)
                                                 `(close ,(nth 4 item)))))
                                        ))
                  rep)
                ))))
    ;(compile (intern (string (first (code self)))))
    ;(print (fdefinition (intern (string (first (code self))))))
    (setf *lambda-context* oldlambdacontext)
    (setf *let-list* oldletlist)))

;;; REDEFINITION
(defmacro with-open-loop-file (self &body body)
  `(let* ((pathname ,(car (decode self)))
          (pathname (or pathname (om-choose-file-dialog :prompt "Choose a File to Read/Write")))
          rep)
     (cond 
      ((equal (get-stream-type ,self) 'text)
       (let ((stream (open pathname :if-does-not-exist :create :direction ,(direction self) :if-exists ,(if-ex self))))
         (setf rep ,@body)
         (close stream)))

      ((and (find :sdif *features*) (equal (get-stream-type ,self) 'sdif))
       (let* ((dir (cond ((equal ,(direction self) :input) :eReadFile)
                         ((equal ,(direction self) :output) :eWriteFile)
                         (t :eReadWriteFile)))
              (stream (sdif::sdif-open-file (namestring pathname) dir)))
         (setf rep ,@body)
         (sdif::sdif-close-file stream)))
      (t nil))
      rep))

