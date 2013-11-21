;;;================================================================================================================
;;; Sound Analysis Tools
;;; jb - 2005 
;;;================================================================================================================


(in-package :om)

;;;================================================================================================================
;;; FFT TOOLS
;;;================================================================================================================

;;; "sous echantilloner" une fft...
(defmethod! format-fft ((self SdifFile) streamNum f-scale f-min f-max f-step)
  :initvals '(nil 0 "lin" 0 22000 100) 
  (let ((fftp nil))
    (loop for item in (framedesc self) while (not fftp) do
          (when (string-equal (string (first item)) "IGBG")
            (setf fftp t)))
    (if (not fftp) (om-message-dialog "Sorry this SDIF file does not contain a SVP fft")
        (let* ((error nil) time (onemat nil)
               (out-path (om-CHOOSE-new-FILE-DIALOG))
               (outfile (sdif-open-file (om-path2cmdpath out-path) 1))
               val (newMat nil) (data nil) str sstr)
          (sdif::SdifFWriteGeneralHeader outfile)
          (write-1nvt-table outfile
                            (list "Author" )
                            (list (string+ "OM " *version-string*)))
          (setf str (format nil "{1MTD 1GB4 {frequency, amplitude} 1FTD XFFT {1GB4  fftata;}}"))
          (setf sstr (sdif::SdifStringNew)) 
          (sdif::SdifStringAppend sstr str)
          (sdif::SdifStringGetC sstr)
          (sdif::SdifFGetAllTypefromSdifString outfile sstr)
          (sdif::SdifFWriteAllASCIIChunks outfile)
          (loop for item in (framesdesc self)
                while (not error)
                for i = 0 then (+ i 1) do
                (setf onemat nil)
                (setf newmat nil)
                (setf time (second item))
                (if (and (= streamNum (third item)) (string-equal (string (first item)) "1GB4")) 
                  (loop for mat in (fifth item)
                        for j = 0 then (+ j 1) do
                        (if (string-equal (first mat) "1GB4")
                          (let ((ptrfile (goto-sdif-frame-mat self i j)) col row val r1 r2)
                            (when ptrfile
                              (progn
                                (sdif::SdifFReadMatrixHeader ptrfile)
                                (setf col (sdif::SdifFCurrNbCol ptrfile))
                                (setf row (sdif::SdifFCurrNbRow ptrfile))
                                (setf onemat (loop for k from 1 to row 
                                                   collect (progn
                                                             (sdif::SdifFReadOneRow ptrfile)
                                                             (loop for c from 1 to col collect
                                                                   (sdif::SdifFCurrOneRowCol ptrfile c)))))
                                (sdif-close-file ptrfile)
                                (loop for f from f-min to f-max by f-step do
                                      (setf val (find-amplitude f onemat))
                                      (push (list f val) newmat))
                                (print (format nil "writing frame t=~D ..." time))
                                (write-1GB4-frame outfile (reverse newMat) time)
                                ;(push (list time (reverse newmat)) data)
                                ))
                            )))))
          (sdif-close-file outfile)
          (om-namestring out-path)
          ;(reverse data)
          ))))


(defmethod! fftdata2sdif ((fftdata list) &optional timelist &key file)
  (let* ((error nil) time
         (out-path (or file (om-CHOOSE-new-FILE-DIALOG)))
         (outfile (sdif-open-file (om-path2cmdpath out-path) 1))
         val (data nil) str sstr)
    (sdif::SdifFWriteGeneralHeader outfile)
    (write-1nvt-table outfile
                    (list "Author" )
                    (list (string+ "OM " *version-string*)))
  (setf str (format nil "{1MTD 1GB1 {amplitude} 1FTD XFFT {1GB1  fftata;}}"))
  (setf sstr (sdif::SdifStringNew)) 
  (sdif::SdifStringAppend sstr str)
  (sdif::SdifStringGetC sstr)
  (sdif::SdifFGetAllTypefromSdifString outfile sstr)
  (sdif::SdifFWriteAllASCIIChunks outfile)
  (loop for framedata in fftdata
        while (not error)
        for i = 0 then (+ i 1) do
        (setf time (if timelist (nth i timeList) i))
        ;(print (format nil "writing frame t=~D ..." time))
        (write-1GB1-frame outfile framedata time)
        ;(push (list time framedata) data)
        )
  (sdif-close-file outfile)
  (om-namestring out-path)
))   


(defun find-amplitude (freq f-a-list)
  (let ((min-dist 44000) (val 0) dist)
    (loop for f-a in f-a-list do
          (setf dist (abs (- freq (first f-a))))
          (if (<= dist min-dist) (setf min-dist dist val (second f-a))))
    val))


(defun write-1GB1-frame (ptr points time)
   (om-without-interrupts  
    (let* ((datatype 4)
           (numcols 1)
           (numlines (length points))
           (framesize (+ 32 (calc-pad (* numcols datatype numlines))))
           (values (om-make-pointer (* numcols datatype numlines))))
      (sdif::SdifFSetCurrFrameHeader ptr (sdif::SdifStringToSignature "XFFT") framesize 1 0 (coerce time 'double-float))
      (sdif::SdifFWriteFrameHeader ptr)
      (loop for i from 0 to (- numlines 1)
            for val in points do
            (om-write-ptr values (* i datatype) :float (coerce val 'single-float)))
      (sdif::SdifFWriteMatrix ptr (sdif::SdifStringToSignature "1GB1") datatype numlines numcols values)
      ;(print (format nil "FRAME ~D -- ~D" time values))
)))


(defun write-1GB4-frame (ptr points time)
   (om-without-interrupts  
    (let* ((datatype 4)
           (numcols 2)
           ;(mat-points (mat-trans points))
           (numlines (length points))
           (framesize (+ 32 (calc-pad (* numcols datatype numlines))))
           (values (om-make-pointer (* numcols datatype  numlines))))
      (sdif::SdifFSetCurrFrameHeader ptr (sdif::SdifStringToSignature "XFFT") framesize 1  0 (coerce time 'double-float))
      (sdif::SdifFWriteFrameHeader ptr)
      (loop for i from 0 to (- numlines 1)
            for sdifrow in points do
            (loop for j from 0 to (- numcols 1)
                  for val in sdifrow do
                  (om-write-ptr values (+ (* j datatype) (* i numcols datatype)) :float (coerce val 'single-float))))
      (sdif::SdifFWriteMatrix ptr (sdif::SdifStringToSignature "1GB4") datatype numlines numcols values))))

;;;================================================================================================================
;;; F0 TOOLS
;;;================================================================================================================

(defmethod! sdif->bpf ((self sdiffile) &key (frametype "1FQ0") (matrixtype "1FQ0") (stream 0) (field 0) (tmin nil) (tmax nil))
    :icon 608
    :indoc '("SDIF file" "frame type (string)" "matrix type (string)" "stream ID (int)" "field number" "min time (s)" "max time (s)")
    :initvals '(nil "1FQ0" "1FQ0" 0 0 nil nil)
    :doc "Reads SDIF data and formats results as a BPF.

Default values are suited to read and convert 1FQ0 frame and matrix types, typically resulting from fundamental frequency analysis.
Other type of data can be extracted by setting the <stream>, <frame>, <matrix> and <field> arguments accordingly.

<tmin> and <tmax> allow to bound the extracted data in a time interval.
"
  (when (and stream frametype frametype field)
      (multiple-value-bind (y x) (getsdifdata self stream frametype matrixtype field nil nil tmin tmax)
        (simple-bpf-from-list x (flat y) 'bpf 3))))

(defmethod! f0->bpf ((self sdiffile))
  :icon 608
  (let ((streamid 0) streams rep)
    (loop for f in (framesdesc self) do
          (when (string-equal "1FQ0" (car f))
            (unless (member (nth 2 f) streams) (push (nth 2 f) streams))))
    (when streams
      (setf rep (remove nil 
                        (loop for streamid in streams collect
                              (multiple-value-bind (y x) (getsdifdata self streamid "1FQ0" "1FQ0" 0 nil nil nil nil)
                                (simple-bpf-from-list x (flat y) 'bpf 4))))))
    (if (= 1 (length rep)) (car rep) rep)))


;;;================================================================================================================
;;; MARKERS TOOLS
;;;================================================================================================================

(defmethod! sdif->markers ((self sdiffile) &key (frame "1MRK") (matrix nil) (stream 0) (tmin nil) (tmax))
    :icon 608
        :indoc '("SDIF file" "frame type (string)" "matrix type (string)" "stream ID (int)" "min time (s)" "max time (s)")
    :initvals '(nil "1MRK" nil 0 nil nil)
    :doc "Reads SDIF data and formats results as a list of time markers (in s).

Default values are suited to read 1MRK frames, typically resulting from markers or transient detection analyses.
Other more specific type of data can be extracted by setting the <stream>, <frame>, <matrix> arguments accordingly.

<tmin> and <tmax> allow to bound the extracted data in a time interval.
"
    (when (and stream frame)
      (get-times self stream frame matrix tmin tmax)))

(defmethod! get-mrk-onsets ((self sdiffile) &optional (streamid 0))
  :icon 608
  ;;; test si le type de la matrice est 1BEG 
  ;;; les marqueurs automatiques renvoient 1BEG, les marqueurs manuels revoient nil
  (if (first (nth 0 (fifth (nth 0 (framesdesc self))))) 
      (get-times self streamid "1MRK" "1BEG" nil nil)
    (get-times self streamid "1MRK" nil nil nil) ))


;;;================================================================================================================
;;; BPF TO SDIF
;;;================================================================================================================

(defmethod! bpf->sdif ((self bpf) ftype mtype &optional (scope 'time) (typedefs nil) (outfile "mybpf.sdif"))
  :icon 608
  :initvals '(nil "1FQ0" "1FQ0" 'time nil nil "mybpf.sdif")
  :indoc '("a BPF" "frame type (string)" "matrix type (string)" "x = time or elements" "custom types declaration" "output file")
  :menuins '((3 (("Time" 'time) ("Elements" 'elts))))
  :doc "Saves the contents of <self> (a BPF) as an SDIF file in <outfile>.

<ftype> and <mtype> allow to determine the SDIF type to enclose the data in (default = 1FQ0, i.e. fundamental frequency).
If these types are not standard, they must be declared and given as a list of SDIFType objects in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

<scope> allows to choose whether the x-dimension of the BPF should be considered as time (default) or as the elements in a single matrix.
"
  (let* ((error nil) time
         (out-path (cond ((stringp outfile) (outfile outfile))
                         ((pathnamep outfile) outfile)
                         (t (om-CHOOSE-new-FILE-DIALOG))))
         (file (sdif-open-file (om-path2cmdpath out-path) 1))
         (datatype 4))
    (sdif::SdifFWriteGeneralHeader file)
    (write-nvt-tables file (list (default-om-NVT)))
    (when typedefs (write-types-table file (list! typedefs)))
    (sdif::SdifFWriteAllASCIIChunks file)
    (if (equal scope 'time)
        (let* ((framesize (+ 32 (calc-pad datatype)))
               (valptr (om-make-pointer datatype)))
          (loop for time in (x-points self)
                for val in (y-points self)
                while (not error) do
                (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 1 0 (coerce time 'double-float))
                (sdif::SdifFWriteFrameHeader file)
                (om-write-ptr valptr 0 :float (coerce val 'single-float))
                (sdif::SdifFWriteMatrix file (sdif::SdifStringToSignature mtype) datatype 1 1 valptr)
                )
          (om-free-pointer valptr))
      (let* ((framesize (+ 32 (calc-pad (* datatype (length (point-list self))))))
             (valptr (om-make-pointer (* datatype (length (point-list self))))))
        (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 1 0 (coerce 0.0 'double-float))
        (sdif::SdifFWriteFrameHeader file)
        (loop for elt in (x-points self)
              for val in (y-points self)
              for i = 0 then (+ i 1)
              while (not error) do
              (om-write-ptr valptr (* i datatype) :float (coerce val 'single-float)))
        (sdif::SdifFWriteMatrix file (sdif::SdifStringToSignature mtype) datatype (length (point-list self)) 1 valptr)
        (om-free-pointer valptr))
      )
    (sdif-close-file file)
    (om-namestring out-path)
    ))

;;;================================================================================================================
;;; MARKERS TO SDIF
;;;================================================================================================================

(defmethod! markers->sdif ((self list) &optional (ftype "1MRK") (typedefs nil) (outfile "markers.sdif"))
  :icon 608
  :initvals '(nil "1MRK" nil "mybpf.sdif")
  :indoc '("onset list (s)" "SDIF frame type" "custom types declaration" "output file")
  :doc "Saves <self> (a list of onsets) as an SDIF file in <outfile>.

<ftype> allows to determine the SDIF frame type to use (default = 1MRK, the standard SDIF type for time markers).
If this type is not standard, it must be declared and given as an SDIFType object in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

"
  (let* ((error nil) time
         (out-path (cond ((stringp outfile) (outfile outfile))
                         ((pathnamep outfile) outfile)
                         (t (om-CHOOSE-new-FILE-DIALOG))))
         (file (sdif-open-file (om-path2cmdpath out-path) 1))
         (datatype 4))
    (sdif::SdifFWriteGeneralHeader file)
    (write-nvt-tables file (list (default-om-NVT)))
    (when typedefs (write-types-table file (list! typedefs)))
    (sdif::SdifFWriteAllASCIIChunks file)
    (let* ((framesize (+ 32 (calc-pad datatype))))
      (loop for time in self
            while (not error) do
            (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 0 0 (coerce time 'double-float))
            (sdif::SdifFWriteFrameHeader file)))
    (sdif-close-file file)
    (om-namestring out-path)
    ))



