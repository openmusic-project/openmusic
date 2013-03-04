; Chordseq to SDIF
; jb  23/02/05

(in-package :om)


(defun midivel->partialamp (vel)
  (/ vel 1270.0))

(defmethod! chordseq-to-datalist ((self chord-seq))
  (let* ((ind -1)
         (chord-data nil)
         (enddata nil)
         newendlist)
    ;;; recup les donnees
    (setf chord-data 
          (loop for chord in (inside self) 
                collect (list (offset->ms chord)
                              (loop for note in (inside chord) 
                                    do (pushr (list (+ (offset->ms chord) (dur note)) (incf ind)) enddata)
                                    collect (list ind (mc->f (midic note)) (midivel->partialamp (vel note)) 0)))))
    ;;; mettre tous les end simulatnes dans une meme frame
    (setf enddata (sort enddata '< :key 'car))
    (let* (tmptime)
      (setf newendlist (loop while enddata do
                             (setf tmptime (car (car enddata)))
                             collect (list tmptime (loop while (equal (car (car enddata)) tmptime) collect (second (pop enddata))))) 
            ))
    (sort (append chord-data newendlist) '< :key 'car)
    ))


(defun write-begin-frame (fileptr data)
  (om-without-interrupts  
   (let* ((time (/ (car data) 1000.0))
        (datatype 4)
        (numlines (length (second data)))
        (framesize (+ 32 (calc-pad (* 4 datatype numlines)) (calc-pad (* 1 datatype numlines))))
        (mrk-values (om-make-pointer (* 1 datatype numlines)))
        (trc-values (om-make-pointer (* 4 datatype numlines))))
    (sdif::SdifFSetCurrFrameHeader fileptr (sdif::SdifStringToSignature "1MRK") framesize 2 0 (coerce time 'double-float))
    (sdif::SdifFWriteFrameHeader fileptr)
    (loop for i from 0 to (- numlines 1)
          for note in (second data) do
            (om-write-ptr mrk-values (* i datatype) :float (coerce (car note) 'single-float))
            
            (om-write-ptr trc-values (* (* i 4) datatype) :float (coerce (car note) 'single-float) )
            (om-write-ptr trc-values (* (+ (* i 4) 1) datatype) :float (coerce (second note) 'single-float) )
            (om-write-ptr trc-values (* (+ (* i 4) 2) datatype) :float (coerce (third note) 'single-float) )
            (om-write-ptr trc-values (* (+ (* i 4) 3) datatype) :float (coerce (fourth note) 'single-float) )
            )
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1BEG") datatype numlines 1 mrk-values)
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1TRC") datatype numlines 4 trc-values)
      ))
)

(defun write-end-frame (fileptr data)
  (om-without-interrupts  
  (let* ((time (/ (car data) 1000.0))
        (datatype 4)
        (numlines (length (second data)))
        (framesize (+ 32 (calc-pad (* 1 datatype numlines))))
        (mrk-values (om-make-pointer (* 1 datatype numlines))))
    (sdif::SdifFSetCurrFrameHeader  fileptr (sdif::SdifStringToSignature "1MRK") framesize 1 0 (coerce time 'double-float))
    (sdif::SdifFWriteFrameHeader fileptr)
    (loop for i from 0 to (- numlines 1)
          for ind in (second data) do
          (om-write-ptr mrk-values (* i datatype) :float (coerce ind 'single-float))
          )
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1END") datatype numlines 1 mrk-values)
      )))

(defun write-mrk-frame (fileptr data) 
   (let (beg end)
     (loop for elt in (second data) do 
           (if (consp elt) 
             (pushr elt beg)
             (pushr elt end)))
  (let* ((time (/ (car data) 1000.0))
         (datatype 4)
         (numlinesb (length beg))
         (numlinese (length end))
         (framesize (+ 32 (calc-pad (* 4 datatype numlinesb)) (calc-pad (* 1 datatype (+ numlinesb numlinese)))))
         (beg-values (om-make-pointer (* 1 datatype numlinesb)))
         (trc-values (om-make-pointer (* 4 datatype numlinesb)))
         (end-values (om-make-pointer (* 1 datatype numlinese)))
         (nbmat 0))
    (when beg (setf nbmat (+ nbmat 2))) 
    (when end (setf nbmat (+ nbmat 1)))
    (sdif::SdifFSetCurrFrameHeader fileptr (sdif::SdifStringToSignature  "1MRK") framesize nbmat 0 (coerce time 'double-float))
    (sdif::SdifFWriteFrameHeader fileptr)
    (loop for i from 0 to (- numlinesb 1)
          for note in beg do
          (om-write-ptr beg-values (* i datatype) :float (coerce (car note) 'single-float))
          (om-write-ptr trc-values (* (* i 4) datatype) :float (coerce (car note) 'single-float) )
          (om-write-ptr trc-values (* (+ (* i 4) 1) datatype) :float (coerce (second note) 'single-float) )
          (om-write-ptr trc-values (* (+ (* i 4) 2) datatype) :float (coerce (third note) 'single-float) )
          (om-write-ptr trc-values (* (+ (* i 4) 3) datatype) :float (coerce (fourth note) 'single-float) )
          )
    (loop for i from 0 to (- numlinese 1)
          for note in end do
          (om-write-ptr end-values (* i datatype) :float (coerce note 'single-float))
          )
    (when beg
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1BEG") datatype numlinesb 1 beg-values)
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1TRC") datatype numlinesb 4 trc-values))
    (when end
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1END") datatype numlinese 1 end-values))
    (om-free-pointer trc-values)
    (om-free-pointer beg-values)
    (om-free-pointer end-values)
  )))
     
 

(defun collect-mrk-frames (datalist)
  (let ((last-time nil)
        (frameslist nil))
    (loop for data in datalist do
          (if (and last-time (= (car data) last-time))
            (setf (cadr (last-elem frameslist))
                  (append (cadr (last-elem frameslist)) (cadr data)))
            (pushr data frameslist))
          (setf last-time (car data)))
    frameslist
    ))

(defmethod! chord-seq->sdif ((self chord-seq) &optional (outpath "cseq.sdif"))
  :icon 264
  :indoc '("a CHORD-SEQ" "output pathname")
  :doc "Saves the contents of <self> (a CHORD-SEQ) as an SDIF file in <outpath>.

Data is stored as a sequence of 1MRK frames containing 1BEG and 1END matrices for begin and end times, and 1TRC matrices for chords values.
"
  (let* ((error nil) time outfile
         (out-path (cond ((pathnamep outpath) outpath)
                         ((stringp outpath) (outfile outpath))
                         (t (om-choose-new-file-dialog)))))
    (when out-path
      (setf outfile (sdif-open-file (om-path2cmdpath out-path) 1))
      (sdif::SdifFWriteGeneralHeader outfile)
      (write-nvt-tables outfile (list (default-om-NVT)))
      (write-sdif-types outfile  "{1FTD 1MRK {1TRC chord_seq_partials;}}")
      (sdif::SdifFWriteAllASCIIChunks outfile)
      (let ((datalist (chordseq-to-datalist self)))
        (setf datalist (collect-mrk-frames datalist)) 
        (loop for data in datalist
              while (not error) do
          ;(cond 
          ; ((consp (car (second data))) (write-begin-frame outfile data))
          ; ((integerp (car (second data))) (write-end-frame outfile data))
          ; (t nil))
          (write-mrk-frame outfile data)
          ))
  (sdif-close-file outfile)
  (probe-file out-path)
  )))

;;; compat
(defmethod! chordseq->sdif ((self chord-seq) &optional (outpath "cseq.sdif"))
  (chord-seq->sdif self outpath))


; get Chordseq from SDIF

(defmethod! chord-seq-raw-data ((self sdiffile) &optional (datatype 'all) (stream nil))
   :doc "Return a list (onsets pitches velocities) or (onset freq vel dur) from an sdif file (using 1TRC or 1MRK frames)"
   :menuins '((1 (("All" all) ("MRK (chord-seq)" mrk) ("TRC (partials)" trc))))
   :initvals '(nil all nil)
   :icon 639
   (let ((data nil) 
         (mrk-partials (make-hash-table)) (trc-partials (make-hash-table))
          mlist bmat emat pmat time
          (ptrfile (sdif-open self)))
     (sdif::SdifFReadGeneralHeader ptrfile)
     (sdif::SdifFReadAllASCIIChunks ptrfile)
     (loop for item in (framesdesc self) do
           (when (or (null stream) (member (nth 2 item) (list! stream) :test '=))
             (when (equal "1MRK" (car item))
                 (setf time (nth 1 item))
                 (sdif-set-pos ptrfile (nth 3 item))
                 (setf mlist (nth 4 item))
                 (loop for mat in mlist do
                         (cond ((equal "1BEG" (car mat)) (setf bmat mat))
                               ((equal "1END" (car mat)) (setf emat mat))
                               ((equal "1TRC" (car mat)) (setf pmat mat))
                               (t nil)))
                 (when bmat 
                     ;;; a begin matrix :
                     ;;; ajoute (onset (note1 note2 ...)) dans tmplist 
                     ;;; avec des valeurs de amp (0) freq (0) et dur (1000) arbitraires pour l'instant
                     (sdif-read-headers ptrfile (nth 3 item) (fifth bmat))
                     (loop for i = 0 then (+ i 1) while (< i (second bmat)) do
                             (sdif::SdifFReadOneRow ptrfile)
                             (sethash mrk-partials (floor (sdif::SdifFCurrOneRowCol ptrfile 1)) (list time 0 0 time))
                     ))
                 (when pmat 
                     ;;; a parameter matrix :
                     ;;; cherche les notes dans tmplist et set pitch et velocity
                     (sdif-read-headers ptrfile (nth 3 item) (fifth pmat))
                     (loop for i = 0 then (+ i 1) while (< i (second pmat)) do
                             (sdif::SdifFReadOneRow ptrfile)
                             (let* ((ind (floor (sdif::SdifFCurrOneRowCol  ptrfile 1))) 
                                    (freq (sdif::SdifFCurrOneRowCol ptrfile 2))
                                    (amp (sdif::SdifFCurrOneRowCol ptrfile 3)) 
                                    (par (gethash ind mrk-partials)))
                               (when par
                                 (setf (nth 1 par) freq)
                                 (setf (nth 2 par) amp))
                               ))
                     )
                 (when emat 
                     ;;; a end matrix :
                     ;;; find the notes, set duration and put int the final notes list 
                     (sdif-read-headers ptrfile (nth 3 item) (fifth emat))
                     (loop for i = 0 then (+ i 1) while (< i (second emat)) do
                             (sdif::SdifFReadOneRow ptrfile)
                             (let* ((ind (floor (sdif::SdifFCurrOneRowCol ptrfile 1)))
                                   (par (gethash ind mrk-partials)))
                               (when par
                                 (setf (nth 3 par) time))
                               )))
                 (setf bmat nil)
                 (setf emat nil)
                 (setf pmat nil)
                 )

             
             (when (or (equal "1TRC" (car item)) (equal "1HRM" (car item)))
               (setf time (nth 1 item))
               (sdif-set-pos ptrfile (nth 3 item))
               (setf mlist (nth 4 item))
               (loop for m in mlist do
                     (when (or (equal "1TRC" (car m)) (equal "1HRM" (car m)))
                       (sdif-read-headers ptrfile (nth 3 item) (fifth m))
                       (loop for i = 0 then (+ i 1) while (< i (second m)) do
                             (sdif::SdifFReadOneRow ptrfile)
                             (let* ((ind (floor (sdif::SdifFCurrOneRowCol ptrfile 1)))
                                    (freq (sdif::SdifFCurrOneRowCol ptrfile 2))
                                    (amp (sdif::SdifFCurrOneRowCol ptrfile 3))
                                    (phase (sdif::SdifFCurrOneRowCol ptrfile 4))
                                    (par (gethash ind trc-partials)))
                               (if par
                                 (progn 
                                   (setf (nth 0 par) (append (nth 0 par) (list time)))
                                   (setf (nth 1 par) (append (nth 1 par) (list freq)))
                                   (setf (nth 2 par) (append (nth 2 par) (list amp)))
                                   (setf (nth 3 par) (append (nth 3 par) (list phase)))
                                   )
                                 (sethash trc-partials ind (list (list time) (list freq) (list amp) (list phase))))
                               )
                             ))
                     )
               )


             ))
     
     (when (or (equal datatype 'mrk) (equal datatype 'all))
       (maphash #'(lambda (key val) (push val data)) mrk-partials))
        
     (when (or (equal datatype 'trc) (equal datatype 'all))
       (maphash #'(lambda (key val) (push val data)) trc-partials))

     (sdif-close self ptrfile)
     
     (sort (reverse data) '< :key #'(lambda (dataitem) (if (consp (car dataitem)) (caar dataitem) (car dataitem))))
     
     ))


(defmethod! GetSDIFChords ((self sdiffile))
   :indoc '("an SDIF file")
   :doc "Returns a list of chords data from an sdif file (using 1MRK / 1TRC frames).

Chords are formatted as a list of (pitch [Hz]  onset [s]  duration [s]  velocity [lin]).
"
   :icon 639
   (let ((rawdata (chord-seq-raw-data self 'all)))
     (mapcar #'(lambda (partial)
                 (if (consp (car partial))
                     ;;; trc
                     (let ((t1 (list-min (nth 0 partial))) 
                           (t2 (list-max (nth 0 partial))))
                       (list (om-mean (nth 1 partial)) 
                             t1 (- t2 t1)
                             (om-mean (nth 2 partial))))
                   ;;; mrk
                   (list (nth 1 partial) (nth 0 partial) (- (nth 3 partial) (nth 0 partial)) (nth 2 partial))
                   ))
             rawdata)
     ))
                   
;; compat
(defmethod! get-chordseq-data ((self sdiffile))
    (GetSDIFChords self))        


(defmethod! sdif->chord-seq ((self sdiffile))
   :indoc '("an SDIF file")
   :doc "Generates a CHORD-SEQ instance from the 1TRC or 1MRK frame data in <self>.

Internally calls and formats data from GetSDIFChords.
"
   :icon 639
   (let* ((rawdata (sort (GetSDIFChords self) '< :key 'cadr))
          (chords nil) (cseqdata nil))
     (loop for note in rawdata do
           ;;; note = (pitch onset dur vel)
           ;;; (car chords) = (onset (pitches) (durs) (vels)) 
           (if (and (car chords) 
                    (= (second note) (car (car chords))))
               ;;; add note to chord
               (setf (car chords)
                     (list (first (car chords))
                           (append (second (car chords)) (list (first note)))
                           (append (third (car chords)) (list (third note)))
                           (append (fourth (car chords)) (list (fourth note)))))
             ;;; else create new chord
             (push (list (second note) (list (first note)) (list (third note)) (list (fourth note)))
                   chords)))
     (setf cseqdata (mat-trans (sort chords '< :key 'car)))
     (make-instance 'chord-seq
                    :lonset (om-round (om* (first cseqdata) 1000))
                    :lmidic (om-round (f->mc (second cseqdata)))
                    :ldur (om-round (om* (third cseqdata) 1000))
                    :lvel (om-round (om-scale (fourth cseqdata) 50 127)))))




;;by JB 05/10/06

(defmethod load-audiosculpt-file (file)
  (format t "LOADING DATA FROM ~a~%" file)
  (with-open-file (in-stream file :direction :input)
    (read in-stream)))


(defmethod! as-to-datalist ((self list))
  (let* ((ind -1)
         (chorddata nil)
         (enddata nil)
         newendlist
         newbeglist)
    ;;; recup les donnees
    (loop for partial in (cddr self) do
          (when (nth 5 partial)
          (pushr (list (* (nth 2 partial) 1000.0) (incf ind) (nth 3 partial) (/ (db->lin (+ 90 (nth 4 partial)))) 32767.0) chorddata)
          (pushr (list (* (nth 5 partial) 1000.0) ind) enddata)
          ))
    ;;; mettre tous les end simulatnes dans une même frame
    (setf enddata (sort enddata '< :key 'car))
    (setf chorddata (sort chorddata '< :key 'car))
    (loop for end in enddata do
            (if (equal (car end) (car (last-elem newendlist)))
              (pushr (cadr end) (cadr (last-elem newendlist)))
              (pushr (list (car end) (list (cadr end))) newendlist)))
    (loop for beg in chorddata do
          (if (equal (car beg) (car (last-elem newbeglist)))
              (pushr (list (second beg) (third beg) (fourth beg) 0) (cadr (last-elem newbeglist)))
              (pushr (list (car beg) (list (list (second beg) (third beg) (fourth beg) 0))) newbeglist)))
    
    (sort (append newbeglist newendlist) '< :key 'car)
    ))

(defmethod! asfile->sdif (asfile &optional outpath)
  :icon 264
  (let* ((error nil) time
    (in-path (or asfile (om-choose-new-file-dialog)))
    (out-path (or outpath (om-choose-new-file-dialog)))
    (outfile (sdif-open-file (om-path2cmdpath out-path) 1)))
  (sdif::SdifFWriteGeneralHeader outfile)
  (write-1nvt-table outfile (list "Author" ) (list (string+ "OpenMusic " *version-string*)))
  (write-sdif-types outfile  "{1FTD 1MRK {1TRC chord_seq_partials;}}")
  (sdif::SdifFWriteAllASCIIChunks outfile)
  
  (let ((datalist (as-to-datalist (load-audiosculpt-file in-path))))
    (setf datalist (collect-mrk-frames datalist)) 
    (loop for data in datalist
          while (not error) do
          (write-mrk-frame outfile data)
          ))
  (sdif-close-file outfile)
  (om-namestring out-path)
))
