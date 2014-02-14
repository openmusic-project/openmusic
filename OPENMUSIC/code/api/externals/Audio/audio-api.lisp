;;==================================
;;; AUDIO TOOLS tools (AU)
;;==================================




(in-package :om-audio)


(export '(
          om-sound-get-info
          om-get-sound-buffer
          om-save-sound-in-file
          
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
  (sf::sndfile-save-sound-in-file filename size nch sr resolution format))

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

;;==================================
;;; OM inbuilt audio tools
;;==================================

(defconstant formatAiff 0)
(defconstant formatWave 1)
(defconstant formatAifc 2)
(defconstant formatWAVint 0)
(defconstant formatWAVfloat 1)
(defconstant formatAIFFint 2)
(defconstant formatAIFFfloat 3)


(defvar *supported-audio-formats* nil)

(setf *supported-audio-formats* (list formatWAVint formatWAVfloat formatAIFFint formatAIFFfloat))

(defun format-name (format)
  (case format
    (formatWAVint "WAVE(int)")
    (formatWAVfloat "WAVE(float)")
    (formatAIFFint "AIFF(int)")
    (formatAIFFfloat "AIFF(float)")))


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

;;; read sample value (i bytes) in file
(defun read-aiff-sample (file i)
  (case i
    (1 (read-byte file)) 
    (2 (read-short file))
    (3 (read-24 file))
    (4 (read-long file))))

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
      (setf (audio-format self) 'ERR)
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


;;; SG OLD 

(defun wave-data (seq i nbytes)
  (cond
   ((= nbytes 1) (aref seq i))
   ((= nbytes 2) (logior (ash (aref seq i) 8) (aref seq (+ i 1))))
   ((= nbytes 3) (logior (ash (aref seq i) 16) (ash (aref seq (+ i 1)) 8) (aref seq (+ i 2))))
   (t 0)))

;;; returns max sample value in a window of size size
;;; of an input buffer 
;;; (pas utilisee)
(defun max-in-window (self input-stream size chanstep)
  (loop for i from 1 to size 
        maximize 
        (let ((val (if (= (sample-size self) 8) (read-byte input-stream) (read-short input-stream))))
          (file-position input-stream (+ (file-position input-stream) chanstep))
          val)))

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


;;;======================================
;;; OM API
;;;======================================
(in-package :oa)


(defclass om-sound ()  
   (

    ;tracknum utilise par le systeme, par forcement celui de l'utilisateur
    (tracknum-sys :accessor tracknum-sys :initform -1)
    ;Savoir si ce son joue sur le player cache (pas de tracks) ou sur le visible (tracks system)
    (assoc-player :accessor assoc-player :initform nil)
    ;buffer du son actuel (pas forcement d'origine, evolue)
    
    ;pointeur LAS fixe (son d'origine au cas ou)
    (sndlasptr :accessor sndlasptr :initarg :sndlasptr :initform nil)
    ;;;pointeur LAS evolutif (son actuel suite à toutes les modifications)
    (sndlasptr-current :accessor sndlasptr-current :initarg :sndlasptr-current :initform nil)
    (sndlasptr-current-save :accessor sndlasptr-current-save :initarg :sndlasptr-current-save :initform nil)
    (current-is-original :accessor current-is-original :initarg :current-is-original :initform -1)
    ;;;Nombre de samples dans le pointeur courant
    (number-of-samples-current :accessor number-of-samples-current :initform nil)
    ;;;pointeur LAS envoyé à la lecture (dérivé de current)
    (sndlasptr-to-play :accessor sndlasptr-to-play :initform nil)
    ;;;Nombre de samples dans le pointeur courant
    (number-of-samples-to-play :accessor number-of-samples-to-play :initform nil)
    ;;;pointeur LAS servant de "presse papier"
    (snd-slice-to-paste :accessor snd-slice-to-paste :initarg :snd-slice-to-paste :initform nil)
    ;;;Undo/Redo pool
    (las-slicing-past-stack :accessor las-slicing-past-stack :initform (make-hash-table))
    (las-slicing-future-stack :accessor las-slicing-future-stack :initform (make-hash-table))
    ;;;If sound has been saved in temp file and re-opened, srate is now the las srate
    (las-using-srate :accessor las-using-srate :initform 0)))


;;; ???????

#+linux 
(defmethod number-of-samples-current ((self om-sound))
  (number-of-samples self))


(defmethod initialize-instance :after ((self om-sound) &rest initargs)
  (setf (assoc-player self) *audio-player-hidden*)
  self)


(defmethod om-sound-sndbuffer ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (sndbuffer self)))

(defmethod om-sound-tracknum-sys ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (tracknum-sys self)))

(defmethod om-sound-sndlasptr ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (sndlasptr self)))

(defmethod om-sound-sndlasptr-current ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (sndlasptr-current self)))

(defmethod om-sound-n-samples-current ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (number-of-samples-current self)))

(defmethod om-sound-sndlasptr-to-play ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (sndlasptr-to-play self)))

(defmethod om-sound-set-sndlasptr-to-play ((self om-sound) ptr)
    (setf (sndlasptr-to-play self) ptr))

(defmethod om-sound-n-samples-to-play ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (number-of-samples-to-play self)))

(defmethod om-sound-snd-slice-to-paste ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (snd-slice-to-paste self)))

(defmethod om-sound-update-sndlasptr-current ((self om-sound) pointer)
  (setf (sndlasptr-current self) pointer))

(defmethod om-sound-update-snd-slice-to-paste ((self om-sound) pointer)
  (setf (snd-slice-to-paste self) pointer))

(defmethod om-sound-update-las-infos ((self om-sound))
  (setf (number-of-samples-current self) (las-get-length-sound (sndlasptr-current self)))
  (setf (number-of-samples-to-play self) (las-get-length-sound (sndlasptr-to-play self))))




(defmethod om-sound-las-slicing-past-stack ((self om-sound))
  (las-slicing-past-stack self))

(defmethod om-sound-las-slicing-future-stack ((self om-sound))
  (las-slicing-future-stack self))

(defmethod om-sound-las-using-srate-? ((self om-sound))
  (if (= 0 (las-using-srate self))
      nil
    t))

(defmethod om-sound-las-using-srate ((self om-sound))
  (setf (las-using-srate self) 1))


(defun las-fill-sound-info (sound)
  (let ((las-infos (las-get-sound-infos (om-path2cmdpath (filename sound)))))
    (setf (sndlasptr sound) (car las-infos)
          (sndlasptr-current sound) (sndlasptr sound)
          (sndlasptr-current-save sound) (sndlasptr sound)
          (number-of-samples-current sound) (cadr las-infos)
          (sndlasptr-to-play sound) (sndlasptr sound)
          (number-of-samples-to-play sound) (cadr las-infos)
          (snd-slice-to-paste sound) nil)
    sound))
    



;;;===================================
;;; PICTURE A PARTIR DU SOUND FILE
;;;===================================

(defmacro om-sound-protect (sound &body body)
  `(if (equal (loaded ,sound) :error)
       (progn 
         (print (format nil "sound ~s is disabled because of previous errors" (namestring (filename ,sound))))
         nil)
     (or (ignore-errors ,@body)
         (progn
           (print (format nil "error in sound ~s" (namestring (filename ,sound))))
           (setf (loaded ,sound) :error)
           nil))))

;;; exported function
(defmethod om-sound-get-pict ((self t)) nil)

(defmethod om-sound-get-pict ((self om-sound))
  (when (and (not (equal :error (loaded self)))
             (or (loaded self) (ignore-errors (om-fill-sound-info self))))
    (om-sound-protect self 
      (om-cons-max-snd-pict (filename self) 5000))))

;;;CONS SND PICT WITH MAX DETECTION
(defun om-cons-max-snd-pict (sndpath nbpix) 
  (let* ((pict nil)) 
    (multiple-value-bind (data format format nch sr ss size skip)
        (om-get-sound-buffer self)
      
      (if (and (> size 0) (> nch 0))
          (let* ((pict-w nbpix) ; taille max de l'image en pixels
                 (pict-h 256)
                 (step (ceiling size pict-w))
                 (channels-h (round pict-h nch))   ; imag height = 256, channels-h = height of 1 channel
                 (offset-y (round channels-h 2))); draw from middle of each channels-h
            (if data
                (let ((tmpArray (make-array step :element-type 'single-float))
                      (smpArray (make-array `(,nch ,pict-w) :element-type 'single-float))
                      pixIndx
                      smpIndx
                      pixpoint)
                  (dotimes (n nch)
                    (setf smpIndx n
                          pixIndx 0)
                    (dotimes (i size)
                      (setf (aref tmpArray (mod i step)) (fli:dereference data :index smpIndx :type :float))
                      (incf smpIndx nch)
                      (when (= (mod i step) 0)
                        (setf (aref smpArray n pixIndx) (reduce #'max tmpArray))
                        (incf pixIndx))))
                  (setf pict 
                        (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
                          (dotimes (i nch)  
                            (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y)))
                          (om-with-fg-color *curstream* *om-gray-color*
                            (dotimes (c nch)
                              (dotimes (i pixIndx)
                                (setf pixpoint (round (* offset-y (aref smpArray c i))))
                                (gp::draw-line *curstream*
                                               i (+ offset-y (* c channels-h) pixpoint)
                                               i (+ offset-y (* c channels-h) (- pixpoint))))))))
                  (fli::free-foreign-object data)
                  pict)
              (setf pict 
                    (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
                      (dotimes (i nch)  
                        (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y))
                        (om-with-fg-color *curstream* (om-make-color 0.8 0.2 0.2) ;;;ICI EN ROUGE
                          (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y 2) pict-w (+ (* i channels-h) offset-y 2))
                          (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y -2) pict-w (+ (* i channels-h) offset-y -2))))
                      ))))
        nil
        ))))

(defun om-cons-snd-pict (sndpath)
  (let* ((pict nil)) 
    (multiple-value-bind (data format format nch sr ss size skip)
        (om-get-sound-buffer self)    
      ;(print (list sndpath size nch))
      (if (and (> size 0) (> nch 0))
          (let* ((pict-w (min #+win32 2000 #-win32 4000 size))  ; taille max de l'image en pixels
                 (pict-h 256)
                 (xstep (round size pict-w))
                 (channels-h (round pict-h nch))   ; imag height = 256, channels-h = height of 1 channel
                 (offset-y (round channels-h 2))); draw from middle of each channels-h
            (if data
                (let ((datalist (loop for pix from 0 to (- pict-w 1) collect
                                      (loop for chan from 0 to (- nch 1) collect 
                                            (fli::dereference data 
                                                              :index (+ (min (* (1- size) nch) (1+ (* pix xstep nch))) chan) 
                                                              ;;ICI LE 1+ EST JUSTE POUR EVITER UN CAS PARTICULIER. POUR ETRE PROPRE : DOWNSAMPLE OBLIGATOIRE : TODO
                                                              :type :float))))
                      pixpoint)
                  (setf pict 
                        (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
                          (loop for i from 0 to (- nch 1) do  
                                (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y)))
                          (om-with-fg-color *curstream* *om-gray-color*
                            (loop for sample in datalist
                                  for i = 0 then (+ i 1) do 
                                  (loop for val in sample 
                                        for c = 0 then (+ c 1) do
                                        (setf pixpoint (round (* offset-y val))) ; scaled 0-1 --> 0 -->256/2
                                ;(print (list i val pixpoint))
                                        (gp::draw-line *curstream* i (+ offset-y (* c channels-h) pixpoint)
                                                       i (+ offset-y (* c channels-h) (- pixpoint)))
                                        ))
                            )))
                  (fli::free-foreign-object data) 
                  pict)
              (setf pict 
                    (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
                      (loop for i from 0 to (- nch 1) do  
                            (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y))
                            (om-with-fg-color *curstream* (om-make-color 0.8 0.2 0.2) ;;;ICI EN ROUGE
                              (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y 2) pict-w (+ (* i channels-h) offset-y 2))
                              (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y -2) pict-w (+ (* i channels-h) offset-y -2))))
                      ))))
        nil
        ))))

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


;;; COCOA : read loop with LibAudioStream (plante sur windows)
#+cocoa
(defun las-cons-snd-pict (sndpath)
  (let* ((snd (las::makereadsound (namestring sndpath)))
         (pict nil)
         (pict-w 5000) ; taille max de l'image en pixels
         (pict-h 256)
         (numsamples (las::GetLengthSound snd))
         (numchannels (las::GetChannelsSound snd)))
    (unless (or (zerop numsamples) (zerop numchannels))
      (let* ((buffer-size (ceiling numsamples pict-w)) ; nb samples dans le buffer
             (pict-w (round numsamples buffer-size)) ;nb exact de pixels
             (channels-h (round 256 numchannels)) ; imag height = 256, channels-h = height of 1 channel
             (offset-y (round channels-h 2)) ; draw from middle of each channels
             (sndr (las::MakeRendererSound snd))
             (bytesread buffer-size)
             (buffer (om-make-pointer (* 4 buffer-size numchannels) t)))
        (mp::with-interrupts-blocked 
          (setf pict 
                (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
                  (loop for i from 0 to (- numchannels 1) do  
                        (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y)))
                  (las::ResetSound snd)
                  (om-with-fg-color *curstream* *om-dark-gray-color*
                    (loop for i from 0 to (- pict-w 1) 
                          while (= bytesread buffer-size) do 
                          (setf bytesread (las::ReadSound snd buffer buffer-size numchannels))
                          (loop for k from 0 to (- numchannels 1) do
                                (setf pixpoint (om-read-ptr buffer (* 4 k) :float))
                                (setf pixpoint (round (* offset-y pixpoint))) ; scaled 0-1 --> 0 -->256/2
                                (gp::draw-line *curstream* i (+ offset-y (* k channels-h) (- pixpoint)) i  
                                               (+ offset-y (* k channels-h) pixpoint))  
                                )
                          ))
                  ))
          )
        (om-free-pointer buffer)))
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