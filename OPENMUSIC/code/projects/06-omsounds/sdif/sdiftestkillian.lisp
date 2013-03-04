; Storing partials as SDIF files
; partials must be initially lists following the format :
; (init-time ((freq-values) (onsets-for-freq-points))((amp-values)(onsets-for-amp-points)))
; jb - ks    ***    27/01/04

(in-package :om)

(defun write-sdif-ascii-chunks (fileptr authorname NVTstring)
  (let (str sstr)
    (write-1nvt-table fileptr
                      (list "Author" )
                      (list authorname))
    (setf str (format nil NVTstring))
    (setf sstr (sdif::SdifStringNew)) 
    (Sdif-String-Append sstr str)
    (sdif::SdifStringGetC sstr)
    (sdif::SdifFGetAllTypefromSdifString fileptr sstr)
    (sdif::SdifFWriteAllASCIIChunks fileptr)
  ))

(defun write-partial-frame (fileptr time fdata adata)
   (om-without-interrupts  
    (let* ((datatype 4)
           (numcols 2)
           (print (format nil "F : ~D" fdata))
           (fmat (mat-trans fdata))
           (amat (mat-trans adata))
           (f-numlines (length fmat))
           (a-numlines (length amat))
           (framesize (+ 32 (calc-pad (* numcols datatype f-numlines)) (calc-pad (* numcols datatype a-numlines))))
           (f-values (om-make-pointer (* numcols datatype f-numlines)))
           (a-values (om-make-pointer (* numcols datatype a-numlines))))
      (sdif::SdifFSetCurrFrameHeader fileptr (sdif-string-to-signature "XPBF") framesize 2 0 (coerce time 'double-float))
      (sdif::SdifFWriteFrameHeader fileptr)
      (loop for i from 0 to (- f-numlines 1)
            for fpair in fmat do
            (om-write-ptr f-values (* i 2 datatype) :single-float (coerce (first fpair) 'single-float) )
            (om-write-ptr f-values (* (+ (* 2 i) 1) datatype) :single-float (coerce (second fpair) 'single-float) ))
      (sdif::SdifFWriteMatrix fileptr (sdif-string-to-signature "XFBF") datatype f-numlines numcols f-values)
      (loop for i from 0 to (- a-numlines 1)
            for apair in amat do
            (om-write-ptr a-values (* i 2 datatype) :single-float (coerce (first apair) 'single-float) )
            (om-write-ptr a-values (* (+ (* 2 i) 1) datatype) :single-float (coerce (second apair) 'single-float) ))
      (sdif::SdifFWriteMatrix fileptr (sdif-string-to-signature "XABF") datatype a-numlines numcols a-values)
      (om-free-pointer f-values)
      (om-free-pointer a-values)
      
)))


(defmethod! partials2sdif ((partialdata list))
  (let* ((error nil) time
    (out-path (om-CHOOSE-new-FILE-DIALOG))
    (outfile (sdif-open-file (om-path2cmdpath out-path) 1))
    f-data a-data)
  (sdif::SdifFWriteGeneralHeader outfile)
    (write-sdif-ascii-chunks outfile 
                                      "OM-Jean" 
                                      "{1MTD XFBF {frequency, onset} 1MTD XABF {amplitude, onset} 1FTD XPBF {XFBF freq-bpf; XABF amp-bpf;}}")
    (loop for partial in partialdata
        while (not error)
          for i = 0 then (+ i 1) do
            (when partial
                (setf time (first partial))
                (setf f-data (second partial))
                (setf a-data (third partial))
                (write-partial-frame outfile time f-data a-data)))
  (sdif-close-file outfile)
  out-path
))


(defmethod! get-sdif-partial ((self sdifFile) nump)
  (let ((stop nil) (partial nil)
        ptrfile (cptpart 0) vals fmatrix amatrix 
        rows matdesc)
    (setf ptrfile (sdif-open self))
    (if (sdif-null-ptr-p ptrfile) (setf stop t)
        (progn
          (print "Finding XPBF SDIF data...")
          (sdif::SdifFReadGeneralHeader ptrfile)
          (sdif::SdifFReadAllASCIIChunks ptrfile)
          (loop for fdesc in (framesdesc self)
                while (not stop) do
                (if (string-equal (first fdesc) (string :XPBF))
                  (if (= cptpart nump)
                    ; we've got it so extract data in our list (t ((fvals)(onsets))((ampvals)(onsets)))
                    (progn
                      ; first matrix : freq/onsets
                      (setf matdesc (first (fifth fdesc)))
                      (sdif-read-headers ptrfile (fourth fdesc) (fifth matdesc))
                      (setf rows (second matdesc))
                      (loop for l from 0 to (- rows 1) do
                            (sdif::SdifFReadOneRow ptrfile)
                            (setf vals (list (sdif::SdifFCurrOneRowCol ptrfile 1) (sdif::SdifFCurrOneRowCol ptrfile 2)))
                            (push vals fmatrix)
                            )
                      (setf fmatrix (mat-trans (reverse fmatrix)))

                      ; second matrix : amp/onsets
                      (setf matdesc (second (fifth fdesc)))
                      (sdif-read-headers ptrfile (fourth fdesc) (fifth matdesc))
                      (setf rows (second matdesc))
                      (loop for l from 0 to (- rows 1) do
                            (sdif::SdifFReadOneRow ptrfile)
                            (setf vals (list (sdif::SdifFCurrOneRowCol ptrfile 1) (sdif::SdifFCurrOneRowCol ptrfile 2)))
                            (push vals amatrix)
                            )
                      (setf amatrix (mat-trans (reverse amatrix)))
                      
                      ; now all in partial format
                      (setf partial (list (second fdesc) fmatrix amatrix))

                      (setf stop t))
                    (setf cptpart (+ cptpart 1)))))))
    (if (not partial) 
      (if (= cptpart 0) (print "no XPBF partial found in this file...")
          (print "not so much partials in this file...")))

    partial))
 
           