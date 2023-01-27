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

; Chordseq to SDIF

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
      (setf outfile (sdif::sdif-open-file (namestring out-path) 1))
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
  (sdif::sdif-close-file outfile)
  (probe-file out-path)
  )))



;;; compat
(defmethod! chordseq->sdif ((self chord-seq) &optional (outpath "cseq.sdif"))
  (chord-seq->sdif self outpath))

;;; merge frames for same kind of (single) matrices (adds data in the matrix)
;;; (from OM-Spat...)
(defun merge-frame-data (frames)
  (let ((newframes nil))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (ftime (car newframes)) (ftime fr))
                     (string-equal (signature (car newframes)) (signature fr)))
                (loop for matrix in (lmatrix fr) do
                      (let ((fmat (find (signature matrix) (lmatrix (car newframes)) :test 'string-equal :key 'signature)))
                        (if fmat 
                            (setf (data fmat) (append (data fmat) (data matrix))
                                  (num-elts fmat) (1+ (num-elts fmat)))
                          (setf (lmatrix fr) (append (lmatrix fr) (list matrix))))))
              (push (make-instance 'SDIFFrame :ftime (ftime fr) :signature (signature fr)
                                   :streamID 0 :lmatrix (lmatrix fr))
                    newframes))))
    (reverse newframes)))

(defun make-trc-frames (partials)
  (merge-frame-data 
   (sort 
    (loop for partial in partials 
          for i = 1 then (+ i 1) append
          (loop for p in (mat-trans partial) collect
                (make-instance 'SDIFFrame :ftime (car p) :streamid i 
                               :signature "1TRC"
                               :lmatrix (list (make-instance 'raw-SDIFMatrix :signature "1TRC"
                                                             :num-elts 1 :num-fields 4
                                                             :data (list i (cadr p) (caddr p) 0))))
                ))
    '< :key 'ftime)))


(defmethod! partials->sdif ((partials list) &optional (outpath "partials.sdif"))
  :icon 264
  :indoc '("a list of partials" "output pathname")
  :doc "Saves the contents of <partials> as an SDIF file in <outpath>.

Data is stored as a sequence of 1TRC frames containing 1TRC matrices.
"
  (let ((out-path (cond ((pathnamep outpath) outpath)
                         ((stringp outpath) (outfile outpath))
                         (t (om-choose-new-file-dialog)))))
    (when out-path
      (let ((outfile (sdif::sdif-open-file (namestring out-path) 1)))
        (sdif::SdifFWriteGeneralHeader outfile)
        (write-nvt-tables outfile (list (default-om-NVT)))
        (write-sdif-types outfile  "{1FTD 1TRC {1TRC sinusoidal tracks;}}")
        (sdif::SdifFWriteAllASCIIChunks outfile)
        (let ((frames (make-trc-frames partials)))
          (loop for frame in frames do
                (save-sdif frame outfile)))
        (sdif::sdif-close-file outfile)
        (probe-file out-path)
        ))))



;;;=========================================================
; get Chordseq from SDIF

(defmethod! chord-seq-raw-data ((self sdiffile) &optional (datatype 'all) (stream nil))
   :doc "Return a list (onsets pitches velocities) or (onset freq vel dur) from an sdif file (using 1TRC or 1MRK frames)"
   :menuins '((1 (("All" all) ("MRK (chord-seq)" mrk) ("TRC (partials)" trc))))
   :initvals '(nil all nil)
   :icon 639
   (let ((data nil) 
         (mrk-partials (make-hash-table)) (trc-partials (make-hash-table))
          bmat emat pmat
          (ptrfile (sdif-open self)))
     (when ptrfile 
       (let (nextframe) 
         (sdif::SdifFReadGeneralHeader ptrfile)
         (sdif::SdifFReadAllASCIIChunks ptrfile)
         ;(setf nextFrame (next-frame-is-ok ptrfile))
         (loop while (next-frame-is-ok ptrfile) do
           ; for item in (framesdesc self) do
               (multiple-value-bind (fsig time sid pos nbmat)
                   (read-frame-header ptrfile)
                 (if (and stream (not (find sid (list! stream) :test '=)))
                     (sdif::SdifFSkipFrameData ptrfile)
                   (cond 
                    ((equal "1MRK" fsig)
                     (loop for mn from 0 to (1- nbmat) do
                           (multiple-value-bind (msig nrows ncols size pos)
                               (read-matrix-header ptrfile) 
                             ;(print (list msig nrows ncols size pos))
                             ;(print (sdif::sdif-calculate-padding nrows ncols size))
                             (cond ((equal "1BEG" msig) 
                                    ;;; a begin matrix :
                                    ;;; ajoute (onset (note1 note2 ...)) dans tmplist 
                                    ;;; avec des valeurs de amp (0) freq (0) et dur (1000) arbitraires pour l'instant
                                    (setf bmat 
                                          (loop for i from 1 to nrows do
                                                (sdif::SdifFReadOneRow ptrfile)
                                                collect (list (floor (sdif::SdifFCurrOneRowCol ptrfile 1)) time))
                                          )
                                    
                                    (sdif::SdifFReadPadding ptrfile (sdif::sdif-calculate-padding nrows ncols size))
                                    
                                    )
                              
                                   ((equal "1TRC" msig) 
                                    ;;; a parameter matrix :
                                    ;;; cherche les notes dans tmplist et set pitch et velocity
                                    (setf pmat 
                                          (loop for i from 1 to nrows do
                                                (sdif::SdifFReadOneRow ptrfile)
                                                collect 
                                                (list (floor (sdif::SdifFCurrOneRowCol ptrfile 1))
                                                      (coerce (sdif::SdifFCurrOneRowCol ptrfile 2) 'single-float)
                                                      (coerce (sdif::SdifFCurrOneRowCol ptrfile 3) 'single-float))))
                                    (sdif::SdifFReadPadding ptrfile (sdif::sdif-calculate-padding nrows ncols size))
                                    )
                              
                                   ((equal "1END" msig)
                                    ;;; a end matrix :
                                    ;;; find the notes, set duration and put int the final notes list 
                                    (setf emat 
                                          (loop for i from 1 to nrows do
                                                (sdif::SdifFReadOneRow ptrfile)
                                                collect (list (floor (sdif::SdifFCurrOneRowCol ptrfile 1)) time))
                                          )
                                    (sdif::SdifFReadPadding ptrfile (sdif::sdif-calculate-padding nrows ncols size))
                                    )
                               
                                   (t (sdif::SdifFSkipMatrixData ptrfile)))
                             ))
                     
                     (loop for b in bmat do
                           (sethash mrk-partials (car b) (list (cadr b) 0 0 (cadr b))))
                     (loop for p in pmat do 
                       (let ((par (gethash (car p) mrk-partials)))
                         (when par
                           (setf (nth 1 par) (second p))
                           (setf (nth 2 par) (third p)))))
                     (loop for e in emat do
                       (let* ((par (gethash (car e) mrk-partials)))
                         (when par (setf (nth 3 par) (cadr e)))))
                     
                     (setf bmat nil emat nil pmat nil)
                     
                     )
                             
             
                    ((or (equal "1TRC" fsig) (equal "1HRM" fsig))
                     
                     (loop for mn from 0 to (1- nbmat) do
                           (multiple-value-bind (msig nrows ncols size pos)
                               (read-matrix-header ptrfile) 
                             ;(print (list msig nrows ncols size pos (sdif::sdif-calculate-padding nrows ncols size)))
                             (cond ((or (equal "1TRC" msig) (equal "1HRM" msig))
                                    (loop for i from 1 to nrows do
                                          (sdif::SdifFReadOneRow ptrfile)
                                          (let* ((ind (floor (sdif::SdifFCurrOneRowCol ptrfile 1)))
                                                 (freq (coerce (sdif::SdifFCurrOneRowCol ptrfile 2) 'single-float))
                                                 (amp (coerce (sdif::SdifFCurrOneRowCol ptrfile 3) 'single-float))
                                                 (phase (coerce (sdif::SdifFCurrOneRowCol ptrfile 4) 'single-float))
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
                                          )
                                    (sdif::SdifFReadPadding ptrfile (sdif::sdif-calculate-padding nrows ncols size))
                                    )
                               
                                   (t (sdif::sdiffskipmatrixdata ptrfile)))
                             )))

                    (t (sdif::SdifFSkipFrameData ptrfile)))
                   ))
               (sdif::sdif-get-signature ptrfile)
               ;(setf nextFrame (nextframep self ptrfile))
               )

         (sdif::sdif-close-file ptrfile)
         ))
     
     (when (or (equal datatype 'mrk) (equal datatype 'all))
       (maphash #'(lambda (key val) (push val data)) mrk-partials))
        
     (when (or (equal datatype 'trc) (equal datatype 'all))
       (maphash #'(lambda (key val) (push val data)) trc-partials))

     (sort (reverse data) '< :key #'(lambda (dataitem) (if (consp (car dataitem)) (caar dataitem) (car dataitem))))
     
     ))


(defmethod! GetSDIFChords ((self sdiffile) &key stream)
   :indoc '("SDIF file" "stream ID")
   :doc "Returns a list of chords data from an sdif file (using 1MRK / 1TRC frames).

Chords are formatted as a list of (pitch [Hz]  onset [s]  duration [s]  velocity [lin]).
"
   :icon 639
   (let ((rawdata (chord-seq-raw-data self 'all stream)))
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


(defmethod! sdif->chord-seq ((self sdiffile) &key stream)
   :indoc '("SDIF file" "stream ID")
   :doc "Generates a CHORD-SEQ instance from the 1TRC or 1MRK frame data in <self>.

Internally calls and formats data from GetSDIFChords.
"
   :icon 639
   (let* ((rawdata (sort (GetSDIFChords self :stream stream) '< :key 'cadr))
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
    ;;; mettre tous les end simulatnes dans une mme frame
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
    (outfile (sdif::sdif-open-file (namestring out-path) 1)))
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
  (sdif::sdif-close-file outfile)
  (om-namestring out-path)
))
