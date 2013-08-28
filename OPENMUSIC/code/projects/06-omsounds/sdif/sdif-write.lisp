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
         (vals (om-make-pointer (* datatype numrow (numcols matrix)) t)))
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


;;; SDIF-Buffer
(defmethod! save-sdif-file ((self sdif-buffer) &key out options)
   :icon 639
   :indoc '("an SDIF-buffer" "format options" "output pathname")
   :initvals '(nil nil t)
   :doc "Saves the contents of <self> as an SDIF file in <outpath>.

<self> is an SDIF-Buffer object or some other object having the SAVE-SDIF-FILE method implemented.
<options> are specific options depending on <self>.

If <outpath> is not specified, a pop-up dialog will open and allow to choose a destination pathname.
"
   (declare (ignore options))
   (let* ((outfile (or (and out 
                            (handle-new-file-exists out))
                       (om-choose-new-file-dialog)))
          (dir (om-make-pathname :directory outfile)))
       (when outfile 
         (unless (probe-file dir)
           (om-create-directory dir))
         (let ((thefile (sdif-open-file (namestring outfile) :eWriteFile)))
           (sdif::SdifFWriteGeneralHeader thefile)
           (write-types-table thefile (list! (types self)))
           (write-nvt-tables thefile (cons (default-om-NVT) (list! (NVTs self))))
           (write-sid-table thefile (list! (SIDs self)))
           (sdif::SdifFWriteAllASCIIChunks thefile)
           (loop for item in (LFrames self) do
                 (save-sdif item thefile))
           (sdif-close-file thefile))
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

