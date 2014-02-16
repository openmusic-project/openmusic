;;==================================
;;; AUDIO TOOLS tools (AU)
;;==================================




(in-package :om-audio)


(export '(
          om-sound-get-info
          om-get-sound-buffer
          om-save-sound-in-file
          
          resample-audio-buffer

          ) :om-audio)



;;==================================
;;; FILE I/O
;;==================================


(defun convert-filename-encoding (path)
  #+cocoa (external-format::decode-external-string (external-format::encode-lisp-string (namestring path) :utf-8) :latin-1)
  #-cocoa (namestring path))


;;; USE LIBSNDFILE
;;; READ
(defun om-sound-get-info (path)
  ;; RETURNS format n-channels sample-rate sample-size size skip
  (sf::sndfile-get-info (convert-filename-encoding path)))

(defun om-get-sound-buffer (path)
  ;; RETURNS buffer format n-channels sample-rate sample-size size skip
  (sf::sndfile-get-sound-buffer (convert-filename-encoding path)))

(defun om-save-sound-in-file (buffer filename size nch sr resolution format)
  (sf::sndfile-save-sound-in-file buffer filename size nch sr resolution format))

;;; USE LIBSampleRATE
(defun resample-audio-buffer (in-buffer in-size n-channels out-buffer out-size ratio method)
  (cffi:with-foreign-object (lsrdata '(:struct lsr::src_data))
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::data_in) in-buffer)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::input_frames) in-size)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::data_out) out-buffer)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::output_frames) out-size)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::src_ratio) ratio)
    
    (let ((res (lsr::src-simple lsrdata method n-channels)))
      (if (= res 0)
          (values T (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::output_frames_gen))
        (values NIL (lsr::src-strerror res)))
      )))



#|


;;==================================
;;; OM inbuilt audio tools (NOT USED ANYMORE)
;;==================================

(defun read-short (s)
  (let ((a (read-byte s))
        (b (read-byte s)))
    (logior (ash a 8) b)))

(defun read-24 (s)
  (let ((a (read-byte s))
        (b (read-byte s))
        (c (read-byte s)))
    (logior (ash a 16) (ash b 8) c)))

(defun read-long (s)
  (let ((a (read-byte s))
        (b (read-byte s))
        (c (read-byte s))
        (d (read-byte s)))
    (logior (ash a 24) (ash b 16) (ash c 8) d)))

(defun read-ostype (s)
  (let ((a (read-byte s))
        (b (read-byte s))
        (c (read-byte s))
        (d (read-byte s)))
    (coerce (list (code-char a) (code-char b) (code-char c) (code-char d)) 'string)))

;;;==================
;;; !!!!!!!!
(defun x80tod (ptr)
  (let* ((expon (logior (ash (logand (nth 0 ptr) #x7F) 8) (nth 1 ptr)))
         (hiMant (logior (ash (nth 2 ptr) 24) (ash (nth 3 ptr) 16) (ash (nth 4 ptr) 8) (nth 5 ptr)))
         (loMant (logior (ash (nth 6 ptr) 24) (ash (nth 7 ptr) 16) (ash (nth 8 ptr) 8) (nth 9 ptr)))
         (signe (logand (nth 0 ptr) #x80))
         rep)
    (if (and (= 0 expon) (= 0 hiMant) (= 0 loMant))
        (setf rep 0)
      (progn 
        (decf expon 16383)
        (setf rep (* (float himant) (expt 2 (decf expon 31))))
        (setf rep (+ rep (* (float himant) (expt 2 (decf expon 32)))))
        ))
    (unless (= 0 signe) (setf rep (- rep)))
    rep))

(defun read-extended (s)
  (let (10bytes rep)
    (setf 10bytes (loop for i from 1 to 10 collect (read-byte  s)))
    (setf rep (x80tod 10bytes))
    rep))



;;;=============
;;; AIFF
;;;=============

(defun aiff-file-p (pathname)
  (let ((in (open pathname :element-type 'unsigned-byte))
        (rep nil))
    (read-ostype in)
    (read-ostype in)
    (setf rep (read-ostype in))
    (close in)
    (cond ((string-equal rep "AIFF") formatAiff)
          ((string-equal rep "AIFC") formatAifc)
          (t nil))))

;;; read sample value (i bytes) in file
(defun read-aiff-sample (file i)
  (case i
    (1 (read-byte file)) 
    (2 (read-short file))
    (3 (read-24 file))
    (4 (read-long file))))

(defun aiff-stream-init (file)
  (file-position file 12))

(defun aiff-look-for-chunck (s ckname)
  (aiff-stream-init s)
  (loop while (and (not (oa::stream-eofp s)) (ignore-errors (not (string-equal ckname (read-ostype s))))) do
        (let ((sizeck (read-long s)))
          (loop for i from 1 to sizeck do
                (read-byte s))))
  (if (oa::stream-eofp s)
    (print (format nil "File has not a ~D chunck~%" ckname))
    (read-long s)))

(defmethod sound-get-info-aiff (filename)
  (let (nch sr ss size skip
            (in (open filename :element-type 'unsigned-byte))
        (format nil))
    (read-ostype in)
    (read-ostype in)
    (setf format (read-ostype in))
    (cond
     ((not (or (string-equal "AIFF" format) (string-equal "AIFC" format)))
      (print "Error: file is not an AIFF file")
      nil)
     (t
      (aiff-look-for-chunck in "COMM")
      (setf nch (read-short in))
      (setf size (read-long in))
      (setf ss (read-short in))
      (cond
       ((or (and (string-equal "AIFF" format)
                 (not (member ss '(8 16 24 32))))
            (and (string-equal "AIFC" format) (not (integerp ss))))
        (print (format nil "Error: cannot read ~A sounds in ~D bits" format ss))
        nil)
       (t 
        (setf sr (read-extended in))
        (aiff-look-for-chunck in "SSND")
        (read-long in)
        (read-long in)
        (setf skip (file-position in))
        ))))
    (close in)
    (values nch sr ss size skip)
    ))


;;;===========
;;; WAVE 
;;;===========

(defun read-wav-sample (file i)
  (let* ((base 256)
        (rep (read-byte file)) 
        (list (list rep)))
    (loop for k from 2 to i do
          (let ((byte (read-byte file)))
            (setf rep (+ rep (* base byte)))
            (setf base (* base 256))))
    rep))


(defun wave-file-p (pathname)
   (let ((str (make-sequence 'string 4))
         (rep t))
   (with-open-file (in pathname :direction :input)
       (file-position in 0)
       (read-sequence str in)
       (setf rep (equal str "RIFF"))
       (when rep 
           (file-position in 8)
           (read-sequence str in)
           (setf rep (equal str "WAVE"))
           )
       )
   (and rep formatWave)))


; (wave-file-p "/Users/bresson/Desktop/ML-MAQ-TESTS/audio-problematik/bonilla/dosigliss-2.wav")
(defmethod sound-get-info-wave (filename)
  (let (nch sr ss size skip
            (in (open filename :element-type 'unsigned-byte)))
         (file-position in 22)
         (setf nch (read-wav-sample in 2))
         (setf sr (read-wav-sample in 4))
         (file-position in 34)
         (setf ss (read-wav-sample in 2))
         (file-position in 40)
       (setf size (round (read-wav-sample in 4) (max 1 (* (/ ss 8) nch))))
       (setf skip 44)
       (close in)
       (values nch sr ss size skip)
       ))

(defun wave-data (seq i nbytes)
  (cond
   ((= nbytes 1) (aref seq i))
   ((= nbytes 2) (logior (ash (aref seq i) 8) (aref seq (+ i 1))))
   ((= nbytes 3) (logior (ash (aref seq i) 16) (ash (aref seq (+ i 1)) 8) (aref seq (+ i 2))))
   (t 0)))

;;;
(defun max-in-win (seq n numchan sizedata)
   (let* ((nbytes (round sizedata 8))
           (step (* numchan nbytes))
           (maxpos (- (length seq) 1)))
     (loop for i = (* n nbytes) then (+ i step) 
               while (< i maxpos)
            maximize (wave-data seq i nbytes))))


(defun read-wave-window (in winsize numchans samplesize)
     (let ((seq (make-array (* winsize numchans samplesize) :element-type 'unsigned-byte)))
       (read-sequence seq in :start 0 :end (* winsize numchans samplesize))
       seq
     ))

|#

;;;======================
;;; DRAW PICTURE (OLD METHODS)
;;;======================

#|

;; Dessin an lecture de bytes. Marche avec WAV int 16bit seulement pour l'instant
(defmethod om-cons-raw-pict ((self om-sound))
  (let* ((ss (sample-size self))
         (in (open (filename self) :element-type `(signed-byte ,(if (/= ss 0) ss 16))))
         (nsmp (number-of-samples self))
         (nch (number-of-channels self))
         (pict-w 4000)
         (pict-h 256)
         (smpstep (* (round nsmp pict-w) nch))
         (channels-h (round pict-h nch))
         (offset-y (round channels-h 2))
         (init-pos (- (file-length in) (* nch nsmp)))
         (indx 0)
         pixpoint pict datalist)

    (loop for i from 0 to (1- pict-w) do 
          (file-position in (+ init-pos (* i smpstep)))
          (loop for l from 0 to (1- nch) do
                (push (/ (read-byte in nil 250) (expt 2 (1- ss)) 1.0) datalist)))
    (setf datalist (reverse datalist))

    (setf pict 
          (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
            (loop for i from 0 to (- nch 1) do  
                  (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y)))
            (om-with-fg-color *curstream* *om-dark-gray-color*
              (loop for i from 0 to (1- pict-w) do
                    (loop for k from 0 to (1- nch) do
                          (setf pixpoint (round (* offset-y (nth indx datalist)))) ; scaled 0-1 --> 0 -->256/2
                          (gp::draw-line *curstream* i (+ offset-y (* k channels-h) (- pixpoint)) i  
                                         (+ offset-y (* k channels-h) pixpoint))
                          (incf indx))))))
    (close in)
    pict))

;;; SANS RIEN : LECTURE FICHIER
(defmethod cons-wave-pict ((self om-sound))
  (let* ((pixnum 8000)
         (sizedata (sample-size self))
         (numdat (number-of-samples  self))
         (numchan (number-of-channels  self))
         (step (floor numdat pixnum))
         (bytesizedata (round sizedata 8))
         (winsize (min step 20))
         
         (positivesize (round 128 numchan))
         (axes (loop for i from 1 to numchan
                     collect (+ positivesize (* 2 (- i 1) positivesize))))
         (channelstep (* (- numchan 1) bytesizedata))
         (deltax (round 256  numchan))
         firstpict win-seq 
         (last-pos (data-position self)))
    (om-with-cursor *om-wait-cursor*
      (setf firstpict 
            (om-record-pict *om-default-font2* (om-make-point pixnum 256) 
              (loop for i from 0 to (- numchan 1) do 
                    (om-draw-line 0 (+ (* i deltax) (round deltax 2)) pixnum (+ (* i deltax) (round deltax 2))))                            
              (let ((in (open (filename self) :element-type 'unsigned-byte)))
                (file-position in (data-position self))                             
                (loop for i from 0 to (- pixnum 1)
                      do (when (stream::stream-listen in)
                           (setf win-seq (read-wave-window in winsize numchan bytesizedata))
                           (setf last-pos (+ last-pos (* bytesizedata numchan step)))
                           (file-position in last-pos)
                           (loop for k from 1 to numchan do
                                 (let (new-point pixelpoint)
                                   (setf new-point (round (max-in-win win-seq (- k 1) numchan sizedata) (ash 1 (- sizedata 8))))
                                   ;(cond
                                   ; ((= sizedata 8) (setf new-point (max-in-win win-seq (- k 1) numchan sizedata )))
                                   ; ((= sizedata 16) (setf new-point (round (max-in-win win-seq (- k 1) numchan sizedata ) 256)))
                                   ; )
                                   (when (> new-point 128) (setf new-point (- new-point 256)))
                                   (setf pixelpoint (- 128 new-point))
                                   (om-draw-line i (+ (* (- k 1) deltax) (round pixelpoint numchan)) i  
                                                 (+ (* (- k 1) deltax) (round (+ pixelpoint (* 2 new-point)) numchan)))                                          
                                   ))))
                (close in))))
      firstpict)))
|#





