(in-package :om)


;-------------------Class definition-----------------------------
(defclass! sdifFile (sdif-object)   
   ((filepathname  :initform nil :accessor filepathname)
    (loaded  :initform nil :accessor loaded)
    (framesdesc :initform nil :accessor framesdesc)
    (streamsdesc :initform nil :accessor streamsdesc))
   (:icon 638)
   (:documentation "
SDIFFILE represents an SDIF file stored somewhere in your hard drive.

SDIF is a generic format for the storage and transfer of sound description data between applications.
It is used in particular by softwares like SuperVP, pm2, AudioSculpt, Spear, etc. to store and export sound analyses.

See http://www.ircam.fr/sdif for more inforamtion about SDIF

Connect a pathname to an SDIF file, or the output of a function returning such a pathname, to initialize the SDIFFILE.
If not connected, the evaluation of the SDIFFILE box will open a file chooser dialog.

Lock the box ('b') to keep the current file.
"))



(defmethod* objfromobjs ((self string) (type sdifFile))
   (load-sdif-file self))

(defmethod* objfromobjs ((self pathname) (type sdifFile))
   (load-sdif-file self))

(defmethod objfromobjs ((self sdif-buffer) (type sdiffile))
   (objfromobjs (save-sdif-file self) type))

(defmethod objfromobjs ((self sdiffile) (type textfile))
   (objfromobjs (bin2ascii self) type))


(defmethod get-name ((self sdiffile))
  (or (get-filename (filepathname self)) ""))

  
(defmethod execption-save-p ((self sdifFile)) 'sdifFile)

(defmethod save-exepcion ((self sdifFile)) 
   (if (filepathname self)
       (and 
        (register-resource :sdif (filepathname self))
        ;`(om-load-if ,(om-save-pathname-relative (filepathname self)) 'load-sdif-file)
        ;`(load-sdif-file (search-file-for-load ,(om-save-pathname-relative (filepathname self)))))
        `(load-sdif-file ,(om-save-pathname-relative (filepathname self)))
        )
     `(make-instance 'sdifFile)))

(defmethod omNG-copy ((self sdiffile))
   (let ((copy (make-instance 'sdiffile)))
     (setf (slot-value copy 'filepathname) (filepathname self))
     (setf (slot-value copy 'framesdesc) (copy-list (framesdesc self)))
     (setf (slot-value copy 'streamsdesc) (copy-list (streamsdesc self)))
     copy))


(defmethod sdif-open ((self sdifFile))
  (when (filepathname self)
    (sdif-open (filepathname self))))
       
(defmethod sdif-open ((self pathname))
  (sdif-open (namestring self)))

(defmethod sdif-open ((self string))
  (let ((fileptr (sdif-open-file self :eReadFile)))
    (when (and fileptr (not (sdif-null-ptr-p fileptr)))
      fileptr)))

;;; useless
(defmethod sdif-close ((self t) ptr)
   (sdif-close-file ptr)
   t)


;-------------------Box-----------------------------

(defmethod get-type-of-ed-box ((self sdifFile))  'sdifFilebox)

(defclass sdifFilebox (OMBoxEditCall) ())

(defmethod get-frame-class ((self sdifFilebox)) 'boxsdifframe)

(defclass boxsdifframe (boxEditorFrame) ())

(defmethod om-get-menu-context ((object boxsdifframe))
  (list+ (boxframe-default-list object)
         ;(list (om-new-leafmenu "SDIF Player..."
         ;                       #'(lambda () (sdif-player-options))))
         ))
             
(defmethod draw-obj-in-rect ((self sdiffile) x x1 y y1 params view)
  (cond ((and (filepathname self) (loaded self))
         (om-draw-string (+ x 2) (+ y 12) "SDIF FILE contents:")
         (loop for str in (streamsdesc self) for pos = 24 then (+ pos 12) do
               (om-draw-string (+ x 6) (+ y pos) (format nil "~D:~A ~A" (fstreamdesc-id str) (fstreamdesc-fsig str)
                                                         (mapcar 'mstreamdesc-msig (fstreamdesc-matrices str))))
               ))
        ((filepathname self)
         (om-with-fg-color view *om-red-color*
           (om-draw-string 6 14 (namestring (filepathname self)))))
        ((not (filepathname self))
         (om-draw-string 6 14 "[no file loaded]"))
        ))






;------------------Load sdif file method---------------------------            
 
(defmethod* get-sdif () 
   :initvals nil
   :indoc nil
   :doc "load a sdif file"
   :icon 148
   (let ((name (om-choose-file-dialog :directory (def-load-directory)
                                      :prompt "Choose SDIF File" 
                                      :types '("SDIF Files" "*.sdif" "All Files" "*.*"))))
     (when name
       (setf *last-loaded-dir* (pathname-dir name))
       (load-sdif-file name)
       )))


(defun load-sdif-file (path)
  (let ((name (search-file-for-load path))
        (rep (make-instance 'sdifFile)))
    (setf (filepathname rep) name)
    (when name
    (if (probe-file name)
        (if (sdif-check-file (om-path2cmdpath name))
            (let ((fileptr (sdif-open-file (om-path2cmdpath name) :eReadFile)))
              (om-print (string+ "Loading SDIF file : " (om-namestring name)))
              (if (and fileptr (not (sdif-null-ptr-p fileptr)))
                  (init-description rep name fileptr)
                (om-beep-msg (format nil "Error at opening the file ~D" name))
                ))
          (om-beep-msg (format nil "File ~s is not an SDIF file !" name)))
      (om-beep-msg (format nil "File ~s does not exist !" name)))
    rep)))


(defmethod get-obj-from-file ((type (eql 'sdif)) filename)
  (load-sdif-file filename))

;;; TODO
(defmethod objfromobjs ((self sdiffile) (type sdif-buffer))
   nil)

;----------------FILL SLOTS

; modiJB
;19/12/03
; FRAMEDESC = (signature time ID pos (matrixDesc-list))
; MATRIXDESC = (signature nbRow nbCol datatype pos)

(defstruct fstreamdesc id fsig tmin tmax nf matrices)
(defstruct mstreamdesc msig fields rmax tmin tmax nf)

(defmethod init-description ((self sdifFile) name ptrfile)
   (let ((Framelist nil)
         (nextFrame nil)
         (streamlist nil))
     (sdif::SdifFReadGeneralHeader ptrfile)
     (sdif::SdifFReadAllASCIIChunks ptrfile)
     ;(setf nextFrame (next-frame-is-ok self ptrfile)) 
     (loop while (next-frame-is-ok ptrfile) do
           (let ((fdesc (description-frame ptrfile)))
             (record-in-streams self fdesc)
             (push fdesc framelist) 
             ;(setf nextFrame (nextframep self ptrfile))
             (sdif-get-signature ptrfile)
             ))
     (setf (framesdesc self) (reverse framelist))
     (setf (loaded self) t)
     (sdif-close-file ptrfile) t))


(defun record-in-streams (self fdesc)
  (let ((streamdesc (find fdesc (streamsdesc self) 
                          :test #'(lambda (frame stream) 
                                    (and (string-equal (car frame) (fstreamdesc-fsig stream))
                                         (= (third frame) (fstreamdesc-id stream)))))))
    (if streamdesc
        (let () ;;; EXISTING FRAME STREAM
          (setf (fstreamdesc-nf streamdesc) (1+ (fstreamdesc-nf streamdesc)))
          (when (< (cadr fdesc) (fstreamdesc-tmin streamdesc))
            (setf (fstreamdesc-tmin streamdesc) (cadr fdesc)))
          (when (> (cadr fdesc) (fstreamdesc-tmax streamdesc))
            (setf (fstreamdesc-tmax streamdesc) (cadr fdesc)))
          (loop for mdesc in (fifth fdesc) do
                (let ((mstreamdesc (find mdesc (fstreamdesc-matrices streamdesc) 
                                         :test #'(lambda (mat mstreamdesc) 
                                                   (string-equal (car mat) (mstreamdesc-msig mstreamdesc))))))
                  (if mstreamdesc 
                      (let ()  ;;; EXISTING MATRIX STREAM
                        (setf (mstreamdesc-nf mstreamdesc) (1+ (mstreamdesc-nf mstreamdesc)))
                        (when (< (cadr fdesc) (mstreamdesc-tmin mstreamdesc))
                          (setf (mstreamdesc-tmin mstreamdesc) (cadr fdesc)))
                        (when (> (cadr fdesc) (mstreamdesc-tmax mstreamdesc))
                          (setf (mstreamdesc-tmax mstreamdesc) (cadr fdesc)))
                        (when (> (cadr mdesc) (mstreamdesc-rmax mstreamdesc))
                          (setf (mstreamdesc-rmax mstreamdesc) (cadr mdesc)))
                        ;(when (> (caddr mdesc) (mstreamdesc-fields mstreamdesc))
                        ;  (setf (mstreamdesc-fields mstreamdesc) (caddr mdesc)))
                        )
                    ;;; NEW MATRIX STREAM
                    (pushr (make-mstreamdesc :msig (car mdesc) :fields (first-n (SDIFTypeDescription self (car mdesc) 'm) (caddr mdesc))
                                             :rmax (cadr mdesc) :tmin (cadr fdesc) :tmax (cadr fdesc)
                                             :nf 1 ) 
                           (fstreamdesc-matrices streamdesc)))
                  ))
          )
      ;;; NEW FRAME STREAM
      (pushr (make-fstreamdesc :fsig (car fdesc) :id (third fdesc)
                               :tmin (cadr fdesc) :tmax (cadr fdesc) :nf 1
                               :matrices  (loop for mdesc in (fifth fdesc) collect 
                                                (make-mstreamdesc :msig (car mdesc) :fields (first-n (SDIFTypeDescription self (car mdesc) 'm) (caddr mdesc))
                                                                  :rmax (cadr mdesc) :tmin (cadr fdesc) :tmax (cadr fdesc)
                                                                  :nf 1)))
             (streamsdesc self))
      )))



(defmethod description-frame (ptr)
   (let ((nummatrix 0) pos rep)
         ;(*read-default-float-format* 'double-float))
     (setf pos (- (sdif-get-pos ptr) 4))
     (sdif::SdifFReadFrameHeader ptr)
     (setf rep (list (sdif::SdifSignatureToString (sdif::SdifFCurrFrameSignature ptr))
                     (sdif::SdifFCurrTime ptr)
                     (sdif::SdifFCurrId ptr)
                     ; (coerce (sdif::SdifFCurrId ptr) 'single-float)
                     pos))
     (setf nummatrix (sdif::SdifFCurrNbMatrix ptr))
     (append rep (list (loop for i from 1 to nummatrix 
                             collect (description-matrix ptr))))))

(defun read-frame-header (ptr)
  (let ((pos (- (sdif-get-pos ptr) 4)))
    (sdif::SdifFReadFrameHeader ptr)
    (values (sdif::SdifSignatureToString (sdif::SdifFCurrFrameSignature ptr))
            (sdif::SdifFCurrTime ptr)
            (sdif::SdifFCurrId ptr)
            pos
            (sdif::SdifFCurrNbMatrix ptr))))

;(sdif::SdifSizeofDataType 0)

(defmethod description-matrix (ptr)
   (let (rep pos)
     (setf pos (sdif-get-pos ptr))
     (sdif::SdifFReadMatrixHeader ptr)
     (setf rep (list  (sdif::SdifSignatureToString (sdif::SdifFCurrMatrixSignature ptr)) 
                      (sdif::SdifFCurrNbRow ptr) 
                      (sdif::SdifFCurrNbCol ptr) 
                      (sdif::SdifSizeofDataType (sdif::SdifFCurrDataType ptr))
                      pos
                      ))   
     (sdif::SdifFSkipMatrixData ptr)
     rep))

(defun read-matrix-header (ptr)
  (let ((pos (- (sdif-get-pos ptr) 4)))
    (sdif::SdifFReadMatrixHeader ptr)
    (values (sdif::SdifSignatureToString (sdif::SdifFCurrMatrixSignature ptr))
            (sdif::SdifFCurrNbRow ptr) 
            (sdif::SdifFCurrNbCol ptr) 
            (sdif::SdifSizeofDataType (sdif::SdifFCurrDataType ptr))
            pos)))

(defmethod how-many-mat ((self sdifFile) i)
   (length (fifth (nth i (framesdesc self)))))

(defun good-signature-p (sign)
  (and sign (stringp sign)
       (not (string-equal "" sign))
       (string>= sign "0000")
       (string<= sign "zzzz")
       T))
       

(defun next-frame-is-ok (ptr)
  (good-signature-p (sdif::SdifSignatureToString (sdif::SdifFCurrSignature ptr))))

(defmethod firstframep ((self sdifFile) ptr)
  (good-signature-p (sdif::SdifSignatureToString (sdif::SdifFCurrSignature ptr))))

(defmethod nextframep ((self sdifFile) ptr)
   (let ((signature (sdif-get-signature ptr)))
     (good-signature-p (sdif::SdifSignatureToString (sdif::SdifFCurrSignature ptr)))))


;--------------------------------------
;MOVING INTO A FILE
(defmethod goto-sdif-frame ((self sdifFile) i)
   (if (< i (numframes self))
     (let ((ptrfile (sdif-open self)))
       (if (null ptrfile)
         (om-beep-msg (format nil "Error reading the sdif file ~D" (filepathname self)))
         (progn
           (sdif::SdifFReadGeneralHeader ptrfile)
           (sdif::SdifFReadAllASCIIChunks ptrfile)
           (loop for j from 1 to i do
                 (sdif::SdifFReadFrameHeader ptrfile)
                 (sdif::SdifFSkipFrameData ptrfile)
                 (sdif-get-signature ptrfile))
           ptrfile)))
     (om-beep-msg (format nil "the sdif file ~D has only ~D frames" (filepathname self) (numframes self)))))

(defmethod goto-sdif-frame-mat ((self sdifFile) i j)
   (let ((ptrfile (goto-sdif-frame self i)) matnum)
     (when ptrfile
       (setf matnum (how-many-mat self i))
       (if (< j matnum)
         (progn
           (sdif::SdifFReadFrameHeader ptrfile)
           (loop for k from 1 to j do
                 (sdif::SdifFReadMatrixHeader ptrfile)
                 (sdif::SdifFSkipMatrixData ptrfile) )
           ptrfile)
         (om-beep-msg (format nil "the ~D frame of the sdif file ~D has only ~D matrix" i (filepathname self) matnum))))))

(defmethod get-sdif-i-j-point ((self sdifFile) frame mat i j)
   (let ((ptrfile (goto-sdif-frame-mat self frame mat))
         col row rep)
     (when ptrfile
       (sdif::SdifFReadMatrixHeader ptrfile)
       (setf col (sdif::SdifFCurrNbCol ptrfile))
       (setf row (sdif::SdifFCurrNbRow ptrfile))
       (if (and (> col j) (> row i))
         (progn
           (loop for k from 0 to i do
                 (sdif::SdifFReadOneRow ptrfile))
           (loop for k from 1 to (+ j 1) do
                 (setf rep (sdif::SdifFCurrOneRowCol ptrfile k)))
           (sdif-close-file ptrfile)
           rep)
         (om-beep-msg (format nil "the point (~D, ~D) is out of range, the matrix dimension is (~D,~D)" i j row col))))))

(defmethod get-sdif-col ((self sdifFile) frame mat i)
   (let ((ptrfile (goto-sdif-frame-mat self frame mat))
         col row rep)
     (when ptrfile
       (sdif::SdifFReadMatrixHeader ptrfile)
       (setf col (sdif::SdifFCurrNbCol ptrfile))
       (setf row (sdif::SdifFCurrNbRow ptrfile))
       (if (> col i)
         (progn
           (setf rep (loop for k from 0 to (- row 1) 
                           collect (progn
                                     (sdif::SdifFReadOneRow ptrfile)
                                     (sdif::SdifFCurrOneRowCol ptrfile (+ i 1)))))
           (sdif-close-file ptrfile)
           rep)
         (om-beep-msg (format nil "Error the matrix dimension is (~D,~D)" row col))))))


(defmethod get-sdif-row ((self sdifFile) frame mat i)
   (let ((ptrfile (goto-sdif-frame-mat self frame mat))
         col row rep)
     (when ptrfile
       (sdif::SdifFReadMatrixHeader ptrfile)
       (setf col (sdif::SdifFCurrNbCol ptrfile))
       (setf row (sdif::SdifFCurrNbRow ptrfile))
       (if  (> row i)
         (progn
           (loop for k from 0 to i do
                 (sdif::SdifFReadOneRow ptrfile))
           (setf rep (loop for k from 1 to col collect
                           (sdif::SdifFCurrOneRowCol ptrfile k)))
           (sdif-close-file ptrfile)
           rep)
         (om-beep-msg (format nil "Error the matrix dimension is (~D,~D)" row col))))))


;=====================================================
;METHODS FOR EDITION
;=====================================================

(defmethod get-initval ((self sdiffile)) (make-instance (type-of self)))

(defmethod make-one-instance ((class sdiffile) &rest slots-vals) 
   (get-sdif))

(defmethod good-val-p? ((self sdifFile))
   (not (null (filepathname self))))


;; compat
(defmethod! bin2ascii ((self sdifFile) &optional targetname)
   :icon 639
   (let ((rep nil))
   (if (filepathname self)
       (let ((ptr (sdif-open self)))
         (unless targetname
            (setf targetname (make-pathname :device (pathname-device (filepathname self)) :directory (pathname-directory (filepathname self))
                                            :name (pathname-name (filepathname self)) :type "txt")))
         (if (and ptr targetname)
             (progn
                (sdif-to-text ptr (namestring targetname))
                (setf rep targetname))
           (om-message-dialog "Could not convert SDIF file to text..."))
         (when ptr (sdif-close-file ptr)))
     (om-message-dialog "SDIFFile box seems not to have attached SDIF file..."))
     rep))

(defmethod! SDIF->text ((self sdifFile) &optional targetname)
   :icon 639
   :indoc '("SDIF file" "text file pathname")
   :doc "Converts <self> to text-SDIF in targetname."
   (bin2ascii self targetname))

;;; compat
(defmethod! sdif2text ((self sdifFile) &optional targetname)
  (sdif->text self targetname))


(defmethod! numFrames ((self sdifFile) &optional type)
   :icon 639
   :indoc '("SDIF file" "SDIF type signature")
   :doc "Returns the number of SDIF frames in <self>.

If <type>, returns the number of frames of type <type> in <self>."
   (let ((frames (if type 
                     (remove-if-not #'(lambda (ftype) (string-equal ftype type)) (framesdesc self) :key 'car)
                   (framesdesc self))))
     (length frames)))


(defmethod! FrameInfo ((self sdifFile) i &optional type)
   :icon 639
   :numouts 5
   :indoc '("SDIF file" "frame number (int)" "SDIF type signature")
   :initvals '(nil 0 nil)
   :outdoc '("type signature" "time" "stream ID" "position in file" "number of matrix")
   :doc "Returns info about frame number <i> in <self>.

If <type>, returns info about frame number <i> of type <type> in <self>.

Frame info is formatted as multiple values : type signature, time, stream ID, position in file, number of matrix.
"
   (let* ((frames (if type 
                      (remove-if-not #'(lambda (ftype) (string-equal ftype type)) (framesdesc self) :key 'car)
                    (framesdesc self)))
          (desc (nth i frames)))
     (when desc
       (values (first desc) (second desc) (third desc) (fourth desc) (length (fifth desc))))))

(defmethod! numMatrix ((self sdifFile) i &optional type)
   :icon 639
   :indoc '("SDIF file" "frame number (int)" "SDIF type signature")
   :initvals '(nil 0 nil)
   :doc "Returns the number of matrix for the frame number <i> in <self>.

If <type>, returns the number of matrix for the frame number <i> of type <type> in <self>.
"
   (let ((frames (if type 
                     (remove-if-not #'(lambda (ftype) (string-equal ftype type)) (framesdesc self) :key 'car)
                   (framesdesc self))))
     (length (fifth (nth i frames)))))

(defmethod! MatrixInfo ((self sdifFile) i j &optional type)
   :icon 639
   :numouts 4
   :indoc '("SDIF file" "frame number (int)" "matrix number (int)" "SDIF type signature")
   :initvals '(nil 0 0 nil)
   :outdoc '("type signature" "number of rows (elements)" "number of columns (fields)" "data type" "position in file")
   :doc "Returns info about matrix number <j> in frame number <i> of <self>.

If <type>, returns info about frame number <i> of type <type> in <self>.

Matrix info is formatted as multiple values : type signature, number of rows (elements), number of columns (fields), data type, position in file.
"
    (let* ((frames (if type 
                      (remove-if-not #'(lambda (ftype) (string-equal ftype type)) (framesdesc self) :key 'car)
                     (framesdesc self)))
           (desc (nth j (fifth (nth i frames)))))
     (when desc
       (values (first desc) (second desc) (third desc) (fourth desc) (fifth desc)))))

(defmethod! SDIFStreams ((self sdiffile))
     :icon 639
     :indoc '("SDIF file")
     :initvals '(nil)
     :doc "Returns the list of SDIF streams (streamID frame-signature (list-of-matrix-signatures)) in <self>."
     (loop for fstream in (streamsdesc self) collect
           (list (fstreamdesc-id fstream) (fstreamdesc-fsig fstream)
                 (loop for mstream in (fstreamdesc-matrices fstream) collect (mstreamdesc-msig mstream)))))


(defmethod! SDIFInfo ((self sdifFile) &optional (print t))
   :icon 639
   :doc "Prints information about the SDIF data in <self>.
Returns an advanced stream description with every FrameType-MatrixType pair in the file.
"
   :indoc '("SDIF file")
   (when print 
     (format *om-stream* "----------------------------------------------------------~%")
     (format *om-stream*  "SDIF file description for ~D~%"  (namestring (filepathname self)))
     (format *om-stream* "----------------------------------------------------------~%"))
   (let ((streams nil)
         (rep-list nil))
     (loop for fr in (framesdesc self) do
           (let ((pos (position fr streams :test #'(lambda (frame1 frame2) (and (string-equal (car frame1) (car frame2))
                                                                         (= (third frame1) (third frame2))))
                                :key 'car)))
             (if pos (setf (nth pos streams) (append (nth pos streams) (list fr)))
               (setf streams (append streams (list (list fr)))))))
   
   (when print 
     (format *om-stream*  "NUMBER OF SDIF STREAMS: ~D~%"  (length streams)))
   (loop for st in streams do
      (let ((times (mapcar 'cadr st))
            (matrices (remove-duplicates (mapcar 'car (flat (mapcar 'fifth st) 1)) :test 'string-equal)))
         (when print 
           (format *om-stream*  "   STREAM ID ~D - ~D Frames type = ~A ~%"  (third (car st)) (length st) (car (car st)))
           (format *om-stream*  "      Tmin= ~D   -   Tmax= ~D~%"  (list-min times) (list-max times))
           (format *om-stream*  "      Matrices :  "))
         (loop for ma in matrices do 
               (pushr (list (third (car st)) (car (car st)) ma) rep-list)
               (when print (format *om-stream*  " ~D" ma))
               )
         (when print (format *om-stream*  "~%~%"))
         ))
   (when print 
     (format *om-stream* "----------------------------------------------------------~%")
     (format *om-stream*  "End file description~%")
     (format *om-stream* "----------------------------------------------------------~%"))
   rep-list
   ))



(defmethod! GetRow ((self sdifFile) fnum Mnum Rnum)
   :icon 639
   :indoc '("SDIF file" "frame number" "matrix number" "row number")
   :initvals '(nil 0 0 0)
   :doc "Returns row number <rnum> from matrix <mnum> of frame <fnum> in <self>.

Rows correspond to the set of description fields for one signle matrix component.
"
   (get-sdif-row self fnum Mnum Rnum))

(defmethod! GetCol ((self sdifFile) fnum Mnum Cnum)
   :icon 639
    :indoc '("SDIF file" "frame number" "matrix number" "column number")
   :initvals '(nil 0 0 0)
   :doc "Returns column number <cnum> from matrix <mnum> of frame <fnum> in <self>.

Columns correspond to the values of all matrix components for one given description field.
"
 (get-sdif-col self fnum mnum Cnum))

(defmethod! GetVal ((self sdifFile) fnum Mnum Rnum Cnum)
   :icon 639
    :indoc '("SDIF file" "frame number" "matrix number" "row number" "column number")
   :initvals '(nil 0 0 0 0)
   :doc "Returns the value of the cell <cnum>,<rnum> from matrix <mnum> of frame <fnum> in <self>.

Columns correspond to matrix components and rows to the different description fields.

"
   (get-sdif-i-j-point self fnum Mnum Rnum Cnum))



(defmethod! GetSDIFData ((self sdifFile) sID frameType matType Cnum rmin rmax tmin tmax)
   :icon 639
   :indoc '("sdif file" "stream number (int)" "frame type (string)" "matrix type (string)" "field number (int or list)" "min row" "max row" "min time (s)" "max time (s)")
   :outdoc '("matrix values" "times")
   :initvals '(nil 0 "" "" 0 nil nil nil nil)
   :doc "Extracts and returns a data array (<rmin>-<rmax>, <tmin>-<tmax>) from the <cnum> field of the <matType> matrix from the Stream <sid> of <frameType> frames from <self>.

<cnum> can be a single value or a list of values (for multiple dimentional descriptions)
Unspecified arguments mean all SDIF data (i.e. for instance any time, all rows, any types, etc.) will be considered and returned.

Use SDIFINFO for information about the Frame and Matrix types contained in a <self>.

Ex. (GETSDIFDATA <SDIFFile> 0 \"1MRK\" \"1TRC\" (0 1) nil nil 0.0 2.0)
means : Get all data from Stream number 1, frames of type 1MRK, matrices of type 1TRC, columns (i.e. fields) 0 and 1, all matrox rows, between 0.0s and 2.0s.

The second oulet returns all corresponding frame TIMES.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.

"
   :numouts 2
   (when (filepathname self) 
     (get-sdif-data (filepathname self) sID frameType matType Cnum rmin rmax tmin tmax)))

(defmethod! getsdifdata ((self pathname) sID frameType matType Cnum rmin rmax tmin tmax)
    (get-sdif-data self sID frameType matType Cnum rmin rmax tmin tmax))

(defmethod! getsdifdata ((self string) sID frameType matType Cnum rmin rmax tmin tmax)
    (get-sdif-data (pathname self) sID frameType matType Cnum rmin rmax tmin tmax))


(defmethod get-sdif-data ((self pathname) streamNum frameT matT colNum rmin rmax tmin tmax)
  (let ((error nil) (data nil) (onecol nil)
        val r1 r2
        (times nil))
    (if (or (and rmin rmax (> rmin rmax)) (and tmin tmax (> tmin tmax)))
      
        (setf error t)
      
      (let ((ptrfile (sdif-open self)))
        (if (null ptrfile) 
            (setf error t)
          (let (nextframe)
          ;(om-print "extracting SDIF data...")
            (sdif::SdifFReadGeneralHeader ptrfile)
            (sdif::SdifFReadAllASCIIChunks ptrfile)
            ;(setf nextFrame (next-frame-is-ok ptrfile))
            (loop ;for item in (framesdesc self)
             while (next-frame-is-ok ptrfile)
             while (not error) do
             (multiple-value-bind (sig time id pos nbmat)
                 (read-frame-header ptrfile)
               ;(print (list "frame" time))
               (if (and (or (not streamNum) (= streamNum id)) (string-equal frameT (string sig)) 
                        (or (not tmin) (>= time tmin)))
                   (if (and tmax (> time tmax))
                       (setf error t)
                     (loop for matnum from 0 to (1- nbmat) do
                           (multiple-value-bind (sig nrows ncols size pos)
                               (read-matrix-header ptrfile) 
                             ;(print (list "cols" ncols "rows" nrows))
                             (if (string-equal matT sig)
                                 (progn
                                   (if (and (numberp colNum) (<= ncols colNum))
                                       (progn 
                                         (om-beep-msg (format nil "Error the matrix has ~D fields" ncols))
                                         (setf error t))
                                     (progn
                                       (if (and rmin (> nrows rmin)) (setf r1 rmin) (setf r1 0))
                                       (if (and rmax (> nrows rmax)) (setf r2 rmax) (setf r2 (1- nrows)))
                                       (setf onecol nil)
                                       (loop for k from 0 to (- r1 1) do (sdif::SdifFSkipOneRow ptrfile))
                                       (loop for k from r1 to r2 do
                                             (sdif::SdifFReadOneRow ptrfile)
                                             (cond
                                              ((numberp colNum) (setf val (and (< colnum ncols) (sdif::SdifFCurrOneRowCol ptrfile (+ colNum 1)))))
                                              ((consp colNum) (setf val (loop for n in colNum collect (and (< n ncols) (sdif::SdifFCurrOneRowCol ptrfile (+ n 1))))))
                                              ((null colNum) (setf val (loop for n from 1 to ncols collect (sdif::SdifFCurrOneRowCol ptrfile n))))
                                              )
                                             (push val onecol)
                                             )
                                       (loop for k from (1+ r2) to (1- nrows) do (sdif::SdifFSkipOneRow ptrfile))
                                       (sdif::SdifFReadPadding ptrfile (sdif-calculate-padding nrows ncols size))
                                       (when onecol
                                         (push (reverse onecol) data)
                                         (push time times)
                                         )
                                       )
                                     )
                                   )
                               (sdif::SdifFSkipMatrixData ptrfile)
                               )
                             ))
                     )
                 (sdif::SdifFSkipFrameData ptrfile)
                 )
               ;(setf nextFrame (nextframep self ptrfile))
               (sdif-get-signature ptrfile)
               ))

             (sdif-close-file ptrfile)
            ))))
      
      (if (not data) (om-print (format nil "No data found with t1=~D t2=~D r1=~D r2=~D " tmin tmax rmin rmax)) 
        ;(om-print "... done")
        )
    (values (reverse data) (reverse times))
    ))



;;; seems to crash in recent versions
(defun sdif-read-headers (ptrfile framepos matpos)
  (sdif-set-pos ptrfile framepos)
  (sdif-get-signature ptrfile)
  (sdif::SdifFReadFrameHeader ptrfile)
  (sdif-set-pos ptrfile matpos)
  (sdif::SdifFReadMatrixHeader ptrfile))

;(defun sdif-read-headers (ptrfile framepos matnum)
;  (sdif-set-pos ptrfile framepos)
;  (sdif-get-signature ptrfile)
;  (sdif::SdifFReadFrameHeader ptrfile)
; (loop for k from 0 to (1- matnum) do
;       (sdif::SdifFReadMatrixHeader ptrfile)
;        (sdif::SdifFSkipMatrixData ptrfile))
;  (sdif::SdifFReadMatrixHeader ptrfile))



          

(defmethod! GetSDIFTimes ((self sdifFile) sID frameType matType tmin tmax)
   :icon 639
   :indoc '("SDIF file" "stream number (integer)" "frame type" "matrix type" "min time (s)" "max time (s)")
   :initvals '(nil 0 "" "" nil nil)
   :doc "Returns a list of times (s) between <tmin> and <tmax> for frames of type <frameType> from the stream <sID> in <self>, containing a matrix of type <matType>.

Unspecified arguments mean respectively that all streamms (for <sID>), frames (<frameType>), matrices (<matType>) and no time boundaries (<tmin> and <tmax>) are considered.

Use SDIFINFO for information about the Frame and Matrix types contained in a <self>.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.

"
   (get-matrix-times self sID frameType matType tmin tmax))

;;; compat 
(defmethod! Get-Times ((self sdifFile) sID frameType matType tmin tmax)
  (GetSDIFTimes self sID frameType matType tmin tmax))
            
(defmethod get-matrix-times ((self sdifFile) streamNum frameT matT tmin tmax)
  (let ((error nil) (repList nil) currtime)
    (if (and tmin tmax (> tmin tmax)) (setf error t))
    (loop for item in (framesdesc self)
          while (not error) do
          (setf currtime (second item))
          (if (and (= streamNum (third item)) (string-equal frameT (string (first item))) 
                   (or (not tmin) (>= currtime tmin)) (or (not tmax) (<= currtime tmax)))
            (if (stringp matT)
              (loop for mat in (fifth item) do
                    (if (string-equal matT (first mat))
                      (push currtime repList)
                      ))
              (push currtime repList))
            )
          finally (if repList (return (reverse repList)) 
                      (progn (om-beep-msg (format nil "No data found"))(return repList))
    ))))


(defmethod! GetSDIFStream ((self sdifFile) sID tmin tmax &optional frameType matType)
   :icon 639
   :indoc '("SDIF file" "stream number" "min time (s)" "max time (s)" "frame type (string)" "matrix type (string)" )
   :initvals '(nil 0 nil nil nil nil)
   :doc "Creates and returns an SDIFStream instance from SDIF data in stream <sid> of <self>.

<tmin> and <tmax> allow to bound the data to consider in the stream.
<frameType> and <matType> allow to select frames and/or matrices of a specific type.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
   (when (filepathname self)
     (get-sdif-stream (filepathname self) sID tmin tmax frameType matType )))


(defmethod! GetSDIFStream ((self pathname) sID tmin tmax &optional frameType matType)
  (get-sdif-stream self sID tmin tmax frameType matType))

(defmethod! GetSDIFStream ((self string) sID tmin tmax &optional frameType matType)
  (get-sdif-stream (pathname self) sID tmin tmax frameType matType))

            
(defmethod get-sdif-stream ((self pathname) streamNum tmin tmax frameT matT)
  (let ((error nil) (frameslist nil) (oneframe nil) (onemat nil)
        ptrfile data rep)
    (if  (or (null self) (and tmin tmax (> tmin tmax))) 
        (setf error t) 
      (setf ptrfile (sdif-open self)))
    (if (null ptrfile) (setf error t)
      (let (nextframe)
        (print "reading SDIF data...")
        (sdif::SdifFReadGeneralHeader ptrfile)
        (sdif::SdifFReadAllASCIIChunks ptrfile)
        ; (setf nextFrame (next-frame-is-ok ptrfile))
        (loop while (next-frame-is-ok ptrfile) 
              ;for item in (framesdesc self)
              while (not error) do
              (multiple-value-bind (sig time id pos nbmat)
                  (read-frame-header ptrfile) 
                   ;(sdif-get-signature ptrfile)
                (if (and (= streamNum id) (or (null frameT) (string-equal frameT (string sig))) 
                         (or (not tmin) (>= time tmin)))
                    (if (or (not tmax) (<= time tmax))
                        (progn
                          (setf oneframe (make-instance 'sdifframe 
                                                        :streamID streamNum
                                                        :signature (string sig)
                                                        :FTime time))
                              ;(sdif-set-pos ptrfile (fourth item))
                              
                              
                          (loop ;for mat in (fifth item) for mn = 0 then (+ mn 1) do
                                 for m from 0 to (1- nbmat) do
                                 (multiple-value-bind (sig nrows ncols size pos)
                                     (read-matrix-header ptrfile) 
                                  ;(sdif::SdifFReadMatrixHeader ptrfile)
                                       
                                  (if  (or (null matT) (string-equal matT sig))
                                      (progn
                                             
                                       ;(sdif-read-headers ptrfile (fourth item) mn) ; (first mat))
                                          
                                        (setf data (mat-trans 
                                                    (loop for i = 0 then (+ i 1) while (< i nrows) do
                                                          (sdif::sdiffreadonerow ptrfile)
                                                          collect (loop for j = 0 then (+ j 1) while (< j ncols) 
                                                                        collect (sdif::SdifFCurrOneRowCol ptrfile (+ j 1))))))
                                        
                                        (sdif::SdifFReadPadding ptrfile (sdif-calculate-padding nrows ncols size))
                                       
                                        (let ((mtype (sdif::sdiftestmatrixtype ptrfile (sdif::SdifStringToSignature sig))))
                                          (setf onemat (make-instance 'sdifmatrix :signature sig))
                                          (setf onemat (cons-array onemat 
                                                                   (list nil nrows sig)
                                                                   (loop for control in data
                                                                         for j = 0 then (+ j 1)
                                                                         append (list (intern (or (and (not (sdif-null-ptr-p mtype))
                                                                                                       (sdif::SdifMatrixTypeGetColumnName mtype (+ j 1)))
                                                                                                  (format nil "Field ~D" j))
                                                                                              )
                                                                                      control
                                                                                      ))
                                                                   ))
                                          (set-data onemat)
                                          (push onemat (LMatrix oneframe))
                                          )
                                        )
                                  (sdif::SdifFSkipMatrixData ptrfile)
                                  )))
                          (push oneframe frameslist)
                          )
                      (setf error t))
                  (sdif::SdifFSkipFrameData ptrfile)
                  )
                ;(setf nextFrame (nextframep self ptrfile))
                (sdif-get-signature ptrfile)
                ))
        (sdif-close-file ptrfile)))
    (if (not frameslist) (print (format nil "No data found")))
    (setf rep (make-instance 'sdifstream :id streamNum :LFrames (reverse frameslist)))
    rep))


;;; TYPE INSPECT TOOLS
;;; OM 6.0.5
(defun matrixinfo-from-signature (file msig)
  (let* ((sig (sdif::SdifStringToSignature msig))
         (mtype (sdif::SdifTestMatrixType file sig)))
  (if (sdif-null-ptr-p mtype)
      (progn 
        (print (string+ "Matrix Type " msig " not found."))
        NIL)
    (let ((mnumcol (sdif::SdifMatrixTypeGetNbColumns mtype)))
      (loop for i = 1 then (+ i 1) while (<= i mnumcol)
            collect (sdif::SdifMatrixTypeGetColumnName mtype i))
      )
    )))

(defun frameinfo-from-signature (file fsig)
   (let* ((sig (sdif::SdifStringToSignature fsig))
          (ftype (sdif::SdifTestFrameType file sig)))
    (if (sdif-null-ptr-p ftype)
      (progn 
        (print (string+ "Frame Type " fsig " not found."))
        NIL)
      (let ((fnumcomp (sdif::SdifFrameTypeGetNbComponents ftype)))
         (loop for i = 1 then (+ i 1) while (<= i fnumcomp) collect 
               (let ((fcomp (sdif::SdifFrameTypeGetNthComponent ftype i)))
                 (if (sdif-null-ptr-p fcomp) NIL
                   (let ((msig (sdif::SdifFrameTypeGetComponentSignature fcomp)))
                     (list (sdif::SdifSignaturetoString msig)
                           (matrixinfo-from-signature file (sdif::SdifSignatureToString msig))
                           ))
                   ))))
      )))
      
(defmethod! SDIFTypeDescription ((self sdifFile) (signature string) &optional (type 'm))
            :icon 639
            :indoc '("SDIF file" "SDIF type Signature" "Frame / Matrix")
            :initvals '(nil "1TYP" 'm)
            :menuins '((2 (("Matrix" 'm) ("Frame" 'f))))
            :doc "Returns a description of type <signature>.

This function must be connected to an SDIF file (<self>) containing this data type.
<type> (m/f) allows to specify if the type correspond to matrices (default) or to frames.

Matrix type description is a list of the different field (columns) names.
Frame type description is a list of lists containing the internal matrix signatures and their respective type descriptions.
"

  (let ((error nil) (data nil)
        (ptrfile (sdif-open self)))
    (if (null ptrfile) (setf error t)
        (progn
          (sdif::SdifFReadGeneralHeader ptrfile)
          (sdif::SdifFReadAllASCIIChunks ptrfile)
          (setf data 
                (if (equal type 'm)
                    (matrixinfo-from-signature ptrfile signature)
                  (frameinfo-from-signature ptrfile signature)))
          (sdif-close-file ptrfile)))
    data))

;;; NVT

(defmethod! GetNVTList ((self string))
            :icon 639
            :indoc '("SDIF file")
            :initvals '(nil)
            :doc "Returns the list of Name/Value tables in <self>.

Name/Value tables are formatted as SDIFNVT objects.
"
  (let ((error nil) (data nil)
        (ptrfile (sdif-open self)))
    (if (null ptrfile) (setf error t)
        (progn
          (sdif::SdifFReadGeneralHeader ptrfile)
          (sdif::SdifFReadAllASCIIChunks ptrfile)
          (let* ((nvtlist (sdif::SdifFNameValueList ptrfile))
                 (nvtl (sdif::SdifNameValueTableList nvtlist))
                 (numnvt (sdif::SdifListGetNbData nvtl))
                 (nvtiter (sdif::SdifCreateHashTableIterator nil)))
            
            (sdif::SdifListInitLoop nvtl)
            (loop for i = 0 then (+ i 1)
                  while (< i 2000)
                  while (let ((next (sdif::SdifListIsNext nvtl)))
                          (and next (> next 0)))
                  do (let* ((curnvt (sdif::SdifListGetNext nvtl))
                            (nvht (sdif::SdifNameValueTableGetHashTable curnvt))
                            (nvnum (sdif::SdifNameValueTableGetNumTable curnvt))
                            (nvstream (sdif::SdifNameValueTableGetStreamID curnvt))
                            (nvtdata (make-instance 'SDIFNVT :id nvstream)))
                       (setf (tnum nvtdata) nvnum)
                       (sdif::SdifHashTableIteratorInitLoop nvtiter nvht)
                       (setf (nv-pairs nvtdata)
                             (loop for i = 0 then (+ i 1)
                                   while (< i 2000)
                                   while (let ((nextit (sdif::SdifHashTableIteratorIsNext nvtiter)))
                                           (and nextit (> nextit 0)))
                                   collect 
                                   (let* ((nv (sdif::SdifHashTableIteratorGetNext nvtiter))
                                          (name (sdif::SdifNameValueGetName nv))
                                          (value (sdif::SdifNameValueGetValue nv)))
                                     (when (string-equal name "TableName")
                                       (setf (tablename nvtdata) value))
                                     (list name value))))
                       (pushr nvtdata data)
                       ))
            (sdif::SdifKillHashTableIterator nvtiter)
            )))
    (sdif-close-file ptrfile)
    data))

(defmethod! GetNVTList ((self sdiffile))
   (GetNVTList (filepathname self)))

(defmethod! GetNVTList ((self pathname))
   (GetNVTList (namestring self)))

(defmethod! find-in-nvtlist ((nvtlist list) (entry string) &optional table-num)
            :icon 639
            :indoc '("list of SDIFNVT" "A name entry in the NameValue table" "Table Number")
            :initvals '(nil "" nil)
            :doc "Finds value corresponding to name <entry> in the name/value tables <nvtlist>.

<table-num> allows to look specifically in table number <table-num> as internally assigned to SDIF NVTs.
"
    (if (and table-num (numberp table-num))
        (let ((nvt (find table-num nvtlist :key 'tnum)))
          (if nvt 
              (find-in-nvt nvt entry)
            (om-beep-msg (string+ "There is no table number " (integer-to-string table-num)))))
      (let ((rep nil))
        (loop for nvt in nvtlist while (not rep) do
              (setf rep (find-in-nvt nvt entry)))
        rep)))
        
  
(defmethod! find-in-nvt ((nvt SDIFNVT) (entry string))
            :icon 639
            :indoc '("a SDIFNVT object" "A name entry in the Name/Value table")
            :initvals '(nil "")
            :doc "Finds value corresponding to name <entry> in the name/value table <nvt>."
    (cadr (find entry (nv-pairs nvt) :test 'string-equal :key 'car)))


;;; SID

(defmethod! GetSIDTable ((self string))
            :icon 639
            :indoc '("SDIF file")
            :initvals '(nil)
            :doc "Returns the list of Stream ID descriptions in the SID table of <self>.

Each entry is a list of (ID source treeway).
See http://sdif.sourceforge.net/standard/sdif-standard.html#Stream%20IDs%20Table 
"
  (let ((data nil)
        (ptrfile (sdif-open self)))
    (unless (null ptrfile) 
      (sdif::SdifFReadGeneralHeader ptrfile)
      (sdif::SdifFReadAllASCIIChunks ptrfile)
      (let* ((idtable (sdif::SdifFStreamIDTable ptrfile))
             (nbitem (sdif::SdifStreamIDTableGetNbData idtable)))
        (setf data 
              (loop for i from 1 to nbitem collect
                    (let ((sid (sdif::SdifStreamIDTableGetSID idtable i)))
                      (make-instance 'sdifsid :id (sdif::SdifStreamIDEntryGetSID sid)
                            :source (sdif::SdifStreamIDEntryGetSource sid)
                            :treeway (sdif::SdifStreamIDEntryGetTreeWay sid)
                            ))
                    ))
        )
      (sdif-close-file ptrfile))
    data))

(defmethod! GetSIDTable ((self sdiffile))
   (GetSIDTable (filepathname self)))

(defmethod! GetSIDTable ((self pathname))
   (GetSIDTable (namestring self)))


(defun get-sid-triplets (SIDlist)
 (loop for item in (list! SIDlist) collect
       (x-append (id item) (source item) (treeway item))
       )
 )


(defmethod! find-sid ((SIDTable list) id)
           :icon 639
           :indoc '("a list of SDIFSID objects" "A streamID (number) or list of StreamIDs")
           :initvals '(nil nil)
           :doc "Returns source, treeway for StreamIDs from a list of StreamIDtable <sdifsid> objects."
  (let ((sid (find id SIDTable :key 'id :test '=)))
    (when sid (list (source sid) (treeway sid)))))

(defmethod! find-sid ((SIDTable list) (id list))
   (loop for item in id collect         
         (find-sid list item)))


