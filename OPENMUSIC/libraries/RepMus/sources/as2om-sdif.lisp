;;
;;            Librairie RepMus
;;            SDIF partial tracking --> OM chord-seq
;;            Jean Bresson  © IRCAM 2005
           
(in-package :om)

;;;=============================================
;;; AUDIOSCULPT SDIF 
;;;=============================================

(defun mk-partial-set-from-sdif (sdiffile delta vmin vmax fmin fmax)
   (setf analyse (get-chordseq-data sdiffile))
   (let ((nbpartials (length analyse)))
     (setf analyse (mat-trans analyse))
     (setf (fourth analyse) (om-round (om-scale (fourth analyse) vmin vmax)))
     (setf analyse (list (second analyse) (third analyse) (first analyse) (fourth analyse)))
     (let ((partial-set
             (make-instance 'partial-set
                 :chord-delta delta
                 :partials
                 (apply #'mapcar
                           #'(lambda (onset dur freq amp)
                               (round-time (make-instance 'partial :ponset onset :outset (+ onset dur) :frequency freq
                                                      :amplitude amp)))
                           analyse))))
       (setf (partials partial-set)
             (loop for partial in (partials partial-set)
                   when (<= fmin (frequency partial) fmax)
                   collect partial))
       
       (round-time partial-set)
       (set-inter-onset partial-set)
       partial-set)))

(defmethod! AS->OM ((analyse sdiffile)
                    (vmin integer)
                    (vmax integer)
                    (delta integer)
                    (mmin integer)
                    (mmax integer)
                    (approx integer)
                    (npoly integer ))
  (partials->chords (mk-partial-set-from-sdif analyse (round delta 10) 
                                    vmin vmax (mc->f mmin) (mc->f mmax)) 
                    approx npoly))


(defmethod! AS->OM ((analyse sdifstream)
                    (vmin integer)
                    (vmax integer)
                    (delta integer)
                    (mmin integer)
                    (mmax integer)
                    (approx integer)
                    (npoly integer ))
  (partials->chords (mk-partial-set-from-sdif analyse (round delta 10) 
                                    vmin vmax (mc->f mmin) (mc->f mmax))
                    approx npoly))


;;;==============================
;;; PARTIALS -> OMSDIF (compatibilite SDIFEdit)
;;;==============================

(defun moyene-list (list)
  (let* ((timel (car list))
        (st (car timel))
        (args (cdr list)))
    (setf timel (cons st (loop for item in timel collect (- item st))))
    (cons timel
          (loop for item in args 
                collect (let ((newitem (/ (apply '+ item) (length item))))
                          (cons newitem (om- item newitem )))))))

(defun write-the-frame (fileptr point)
  (let* ((col-num 3)
         (only-data (moyene-list (list-modulo (cddr point) col-num))))
    (save-sdif-partials only-data fileptr (third point))))

(defun calc-pad (n)
  (if (zerop (mod n 8)) n
      (* 8 (+ (floor n 8) 1))))

(defun save-sdif-partials (points ptr starttime)
   (om-without-interrupts  
    (let* ((datatype 4)
           (numcols 3)
           (mat-points (mat-trans points))
           (numlines (length mat-points))
           (framesize (+ 32 (calc-pad (* numcols datatype  numlines))))
           (values (om-make-pointer (* numcols datatype  numlines))))     ;;;(#_newptr (* numcols datatype  numlines))))
      (sdif::SdifFSetCurrFrameHeader ptr (sdif-string-to-signature "EASF") framesize 1  0 (coerce starttime 'double-float))
      (sdif::SdifFWriteFrameHeader ptr)
      (loop for i from 0 to (- numlines 1)
            for sdifrow in mat-points do
            (loop for j from 0 to (- numcols 1)
                  for val in sdifrow do
                    (om-write-ptr values (+ (* j datatype) (* i numcols datatype)) :single-float (coerce val 'single-float))))   
      (sdif::SdifFWriteMatrix ptr (sdif-string-to-signature "EASM") datatype numlines  numcols values))))

(defun write-partial-headers (ptrfile)
   (let (str sstr)
     (sdif::SdifFWriteGeneralHeader ptrfile)
     (write-1nvt-table ptrfile
                               (list "Author" )
                               (list (format nil "OM ~D" *om-version*)))
     (setf str (format nil "{1MTD EASM   {Onset, Frequency, Amplitude} 1FTD EASF {EASM  PartialsSet;}}"))
     (setf sstr (sdif::SdifStringNew)) 
     (Sdif-String-Append sstr str)
     (sdif::SdifStringGetC sstr)
     (sdif::SdifFGetAllTypefromSdifString ptrfile sstr)
     (sdif::SdifFWriteAllASCIIChunks  ptrfile)
     ))

(defmethod! as-cs2sdifedit ((analyse list))
  :icon 639
   :doc "audiosculpt partials to sdifedit partials."
   (let* ((new-path (om-CHOOSE-new-FILE-DIALOG)))
     (when new-path
         (let ((sfile (sdif-open-file (om-path2cmdpath new-path) :eWriteFile)))
           (write-partial-headers sfile)
           (setf analyse (first analyse))
           (unless (string= (symbol-name (pop analyse)) "PARTIALS")
              (error "This is not a spectral analysis"))
           (loop for cur-point in (cdr analyse) do
                   (write-the-frame sfile cur-point))
           (sdif-close-file sfile)))
           new-path))


(defmethod! as-cs2sdifedit ((sdiffile sdiffile))
  :icon 639
  :doc "audiosculpt partials to sdifedit partials."
   (let ((new-path (om-CHOOSE-new-FILE-DIALOG)))
     (when new-path
         (let ((sfile (sdif-open-file (om-path2cmdpath new-path) :eWriteFile))
                        (analyse (get-chordseq-data sdiffile)))
           (write-partial-headers sfile)
           (loop for cur-point in analyse do
                   (write-the-frame sfile (list 'points 2 
                                                           (second cur-point) (first cur-point) (fourth cur-point)
                                                           (third cur-point) (first cur-point) (fourth cur-point))))
           (sdif-close-file sfile)))
     new-path))

(defmethod! as-cs2sdifedit ((sdiffile sdifstream))
  :icon 639
  :doc "audiosculpt partials to sdifedit partials."
   (let ((new-path (om-CHOOSE-new-FILE-DIALOG)))
     (when new-path
         (let ((sfile (sdif-open-file (om-path2cmdpath new-path) :eWriteFile))
                        (analyse (get-chordseq-data sdiffile)))
           (write-partial-headers sfile)
           (loop for cur-point in analyse do
                   (write-the-frame sfile (list 'points 2 
                                                           (second cur-point) (first cur-point) (fourth cur-point)
                                                           (third cur-point) (first cur-point) (fourth cur-point))))
           (sdif-close-file sfile)))
     new-path))


;;;===================================================
;;;
;;; ON SE SERT PLUS DE TOUT ÁA
;;;
;;;===================================================
;;;(defclass! EASM (SDIF-MTC)
;;;   ((Onset :initform 0 :initarg :Onset :type number :accessor Onset)
;;;    (Frequency :initform 440.0 :initarg :Frequency :type number :accessor Frequency)
;;;    (Amplitude :initform 0.8 :initarg :Amplitude :type number :accessor Amplitude)
;;;    )
;;;   (:icon 640)
;;;   (:documentation "Onset, Frequency, Amplitude."))
;;;===================================================
;;;
;;; ÁA C'ETAIT POUR PASSER DE PARTIAL ASCII A OM-SDIF
;;;====================================================
;;;
;;;(defmethod* as-cs2sdif ((filename t))
;;;  :icon 639
;;;  :doc "audiosculpt partials to sdif."
;;;  (let* ((new-path (om-CHOOSE-new-FILE-DIALOG))
;;;        (sfile (sdif-open-file (om-path2cmdpath new-path) :eWriteFile)) str sstr)
;;;    (sdif::SdifFWriteGeneralHeader sfile)
;;;    (write-1nvt-table sfile
;;;                       (list "Author" )
;;;                       (list (format nil "OM ~D" *om-version*)))
;;;    (setf str (format nil "{1MTD EASM   {Onset, Frequency, Amplitude} 1FTD EASF {EASM  PartialsSet;}}"))
;;;    (setf sstr (sdif::SdifStringNew)) 
;;;    (Sdif-String-Append sstr str)
;;;    (sdif::SdifStringGetC sstr)
;;;    (sdif::SdifFGetAllTypefromSdifString sfile sstr)
;;;    (sdif::SdifFWriteAllASCIIChunks  sfile)
;;;    (with-open-file (in filename)
;;;      (let ((info (read-line in)) np  )
;;;        (setf info (string+ info ")"))
;;;        (setf np (second (read-from-string info)))
;;;        (loop for i from 1 to np do
;;;              (let ((cur-point (read in)))
;;;                (write-the-frame sfile cur-point)))
;;;        (close-sdif-file sfile)))
;;;    new-path))
;;;
;;;
;;; ÁA C'EST POUR PASSER DE OM-SDIF A PARTIAL-ASCII
;;;==================================================
;;;(defmethod* om-sdif2partials ((self sdiffile))
;;;  :icon 639
;;;  (let ((frame-num (numFrames self))
;;;        rep)
;;;    (setf rep
;;;          (loop for i from 0 to (- frame-num 1) 
;;;                when (equal :EASF (FrameInfo self i)) collect (read-partial-as self i)))
;;;    (list (cons 'PARTIALS (cons frame-num rep)))))
;;;
;;;(defun read-partial-as (file fnum)
;;;  (let* ((mat-info (multiple-value-list (MatrixInfo file fnum 0)))
;;;         (numrows (second mat-info)))
;;;    (cons-partial-as (loop for i from 0 to (- numrows 1) collect (GetRow file fnum 0 i)))))
;;;
;;;(defun cons-partial-as (list)
;;;  (cons 'POINTS (cons (- (length list) 1)
;;;                      (loop for item in (cdr list)
;;;                            append (om+ (car list) item)))))

