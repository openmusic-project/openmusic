;;==================================
;;; AUDIO API
;;==================================

(in-package :om-api)

;;==================================
;;; EXPORTED SYMBOLS (OM-API)
;;==================================

(export '(
          formatAiff 
          formatAifs 
          formatWave
          om-supported-audio-format
          om-format-name
     
          om-sound
          om-make-sound
          
          om-sound-file-name
          om-sound-n-samples
          om-sound-sample-rate
          om-sound-sample-size
          om-sound-n-channels
          om-sound-data-pos
          om-sound-format
          
          om-cons-snd-pict
          om-sound-get-pict
          om-read-sound-data      
          ) :om-api)


;;==================================
;;; AUDIO TOOLS tools (AU)
;;==================================

(cl:defpackage "Audio"
  (:nicknames "AU")
   (:use common-lisp))


(in-package :au)

;;==================================
;;; LIBSNDFILE tools
;;==================================

(defun sndfile-get-info (path)
  "Returns a matrix of sound data"
  (cffi:with-foreign-object (sfinfo 'sf::SF_INFO)
    (setf (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open (namestring path) sf::SFM_READ sfinfo))
           (frames (fli::dereference (cffi:foreign-slot-pointer sfinfo 'sf::SF_INFO 'sf::frames) :type :int))	  
	   (format (fli::dereference (cffi:foreign-slot-pointer sfinfo 'sf::SF_INFO 'sf::format) :type :int))
           (channels (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::channels))
	   (skip (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::seekable))
	   (sample-rate (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::samplerate)))
      (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
      (values format channels sample-rate 0 frames skip))))

(defun load-audio-data (path &optional (datatype :float))
  "Returns a matrix of sound data"
  (cffi:with-foreign-object (sfinfo 'sf::SF_INFO)
    (setf (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open (namestring path) sf::SFM_READ sfinfo))
           (frames-to-read-ptr (cffi:foreign-slot-pointer sfinfo 'sf::SF_INFO 'sf::frames))
	   (frames-to-read (fli::dereference frames-to-read-ptr :type :int :index #+powerpc 1 #-powerpc 0))
	   (channels (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::channels))
	   (buffer nil)
           (frames-read nil)
           (size -1))
      (setq size frames-to-read)
      (setq buffer (ignore-errors (fli:allocate-foreign-object :type datatype :nelems (* size channels) :fill 0)))
      (when buffer 
        (setq frames-read
              (case datatype
                (:float (sf::sf-readf-float sndfile-handle buffer frames-to-read))
                (:int (sf::sf-readf-int sndfile-handle buffer frames-to-read))
                (:short (sf::sf-readf-short sndfile-handle buffer frames-to-read))
                (othewise (print (concatenate 'string "Warning: unsupported datatype for reading audio data: " (string datatype)))))) 
        (sf::sf_close sndfile-handle) ; should return 0 on successful closure
        )
      (values buffer size channels))))

;;==================================
;;; OM tools
;;==================================

(defconstant formatAiff 0)
(defconstant formatWave 1)
(defconstant formatAifc 2)

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
      (setf *snd-error* (print "Error: file is not an AIFF file"))
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
        (setf *snd-error* (print (format nil "Error: cannot read ~A sounds in ~D bits" format ss)))
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

(defvar *supported-audio-formats* nil)
(setf *supported-audio-formats* (list au::formatAiff au::formatAifc au::formatWave))

(defun om-supported-audio-format (format)
  (member format *supported-audio-formats*))

(defun om-format-name (format)
  (cond 
    ((equal format au::formatAiff) "AIFF")
    ((equal format au::formatWave) "WAVE")
    ((equal format au::formatAifc) "AIFC")))

(defun default-sample-size (format)
  (cond 
    ((equal format au::formatAiff) 16)
    ((equal format au::formatWave) 16)
    ((equal format au::formatAifc) 32)))

;;;===================
;;; SOUND CLASS
;;;===================
(defclass om-sound ()  
   ((filename :accessor filename :initarg :filename :initform nil)
    (audio-format :accessor audio-format :initarg :audio-format :initform nil)
    (device :accessor device :initarg :device :initform nil)
    (number-of-samples :accessor number-of-samples :initarg :number-of-samples :initform nil)
    (sample-rate  :accessor sample-rate :initarg :sample-rate :initform nil)
    (number-of-channels :accessor number-of-channels :initarg :number-of-channels :initform nil)
    (sample-size :accessor sample-size :initarg :sample-size :initform nil)
    (data-position :accessor data-position :initarg :data-position :initform nil)
    (loaded :accessor loaded :initform nil)
    )
   )

(defmethod om-sound-file-name ((self om-sound))
   (filename self))

(defmethod om-sound-sample-rate ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (sample-rate self)))

(defmethod om-sound-sample-size ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (sample-size self)))

(defmethod om-sound-n-samples ((self om-sound))
  (if (or (loaded self) (om-fill-sound-info self))
    (number-of-samples self)
    0))

(defmethod om-sound-n-channels ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (number-of-channels self)))

(defmethod om-sound-data-pos ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (data-position self)))

(defmethod om-sound-format ((self om-sound))
  (when (or (loaded self) (om-fill-sound-info self))
    (audio-format self)))

(defun audio-file-type (pathname)
  (or
   (au::wave-file-p pathname)
   (au::aiff-file-p pathname)))

(defvar *snd-error* nil)
 
(defun om-make-sound (class filename)
 (make-instance class :filename filename))


(defun om-fill-sound-info (sound)
  (when (and sound (filename sound) (probe-file (filename sound)))
    (print (format nil "Loading sound file : ~s" (namestring (filename sound))))
    (multiple-value-bind (format nch sr ss size skip)
        (om-sound-get-info (filename sound))
      ;(print (list format nch sr ss size skip))
      (if (and format size nch (> size 0) (> nch 0))
        (progn 
          (setf (audio-format sound) format
                (number-of-samples sound) size
                (number-of-channels sound) nch
                (sample-size sound) ss
                (sample-rate sound) sr
                (data-position sound) skip)
          (setf (loaded sound) t)
          (unless (om-supported-audio-format format)
            (print (format nil "Warning : unsupported audio format ~A" format))
            (setf (loaded sound) :error)))
        (progn 
          (print (format nil "Error whie loading file ~s" (filename sound)))
          (setf (loaded sound) :error))))
    (loaded sound)))

(defun convert-filename-encoding (path)
  #+cocoa (external-format::decode-external-string (external-format::encode-lisp-string (namestring path) :utf-8) :latin-1)
  #-cocoa (namestring path))


; (setf ppp (capi::prompt-for-file ""))
; (mapcar 'code-char (length (convert-filename-encoding ppp)))
; (external-format::decode-external-string (external-format::encode-lisp-string (namestring ppp) :latin-1) :utf-8)
 
;;; a refaire bien

(defmethod om-sound-get-info (path)
  (let ((filename path))
  (multiple-value-bind (format nch sr ss size skip)
      (sound-get-info filename)
    #+libsndfile
    (multiple-value-bind (format2 nch2 sr2 ss2 size2 skip2)
        (au::sndfile-get-info (convert-filename-encoding filename))
      (when (> nch2 0) (setf nch nch2))
      (when (> sr2 0) (setf sr sr2))
      (when (> size2 0) (setf size size2)))
    (unless (and (numberp ss) (plusp ss)) (setf ss (default-sample-size format)))
    (values format nch sr ss size skip)
    )))

(defun sound-get-info (filename) 
  (let ((format (audio-file-type filename)))
    (when (om-supported-audio-format format)
      (multiple-value-bind (nch sr ss size skip)
          (cond 
           ((equal format au::formatWave) (au::sound-get-info-wave filename))
           ((or (equal format au::formatAiff) (equal format au::formatAifc)) (au::sound-get-info-aiff filename))
           (t nil))
        #+libaudiostream (let ((tmpptr (las::MakeReadSound (convert-filename-encoding filename))))
                           (setf size (las::GetLengthSound tmpptr))
                           (setf nch (las::GetChannelsSound tmpptr))
                           )
        (values format nch sr ss size skip)
      ))))

;(defparameter soundfile1 "/Users/bresson/Desktop/ngyengb/beyi-ngyengb.aiff")

;(defparameter soundfile1 "/Users/bresson/Desktop/aéo/testf.aif")
;(defparameter soundfile2 "C:\\Users\\Jean Bresson\\Desktop\\audio-problematik\\farinelli.aif")
;(defparameter soundfile3 "C:\\Users\\Jean Bresson\\Desktop\\accord.wav")
;(defparameter soundfile4 "C:\\Users\\Jean Bresson\\Desktop\\audio-problematik\\bonilla\\dosigliss-2.wav")

;; (au::sndfile-get-info soundfile1)
;; (sound-get-info soundfile1)

; 1, 2, 44100, 32, 249856, 44

;;; !!! c'est pas du tout efficace. 
;;; en refaire une avec le fichier deja ouvert.
(defmethod om-read-sound-data2 ((self om-sound) position nbytes)
   (let ((in (open (filename self) :element-type 'unsigned-byte))
         (data 0))
       (file-position in position)
       (setf data (cond 
                   ((equal (audio-format self) au::formatWave) (au::read-wav-sample in nbytes))
                   ((or (equal (audio-format self) au::formatAiff) (equal (audio-format self) au::formatAifc))
                    (au::read-aiff-sample in nbytes))
                   (t 0)))
       (close in)
       data))

(defmethod om-read-sound-data ((self om-sound) position &optional (datatype :short))
  (let ((filename (convert-filename-encoding (filename self))))
  (multiple-value-bind (data size nch) 
      (au::load-audio-data filename datatype)
    (let ((snddata (loop for chan from 0 to (- nch 1) collect 
                      (fli::dereference data 
                                        :index (+ position chan)
                                        :type datatype))))
      (fli::free-foreign-object data)
      snddata
      ))))
    

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
(defmethod om-sound-get-pict ((self om-sound))
  (when (and (not (equal :error (loaded self)))
             (or (loaded self) (ignore-errors (om-fill-sound-info self))))
    (om-sound-protect self 
      (om-cons-snd-pict (filename self))
      )
    ))

;(setf qqq (capi::prompt-for-file ""))
;(setf ppp #P"C:/Documents and Settings/Jean Bresson/test.aiff")
;(setf qqq #P"C:/Documents and Settings/Jean Bresson/Bureau/mlmaq-tests/Maquette Bolero et fichiers/Bolero extrait violons.AIFF")

; 0, 2, 44100, 0, 53474, 1

;(au::sndfile-get-info ppp)
;(au::load-audio-data ppp)
;(logior (ash 292780 32) 1143)

;;; USE LIBSNDFILE

(defun om-cons-snd-pict (sndpath)
  (let* ((pict nil)) 
    (multiple-value-bind (data size nch) 
        (ignore-errors
          (au::load-audio-data (convert-filename-encoding sndpath) :float))
      ;(print (list sndpath size nch))
      (if (and (> size 0) (> nch 0)) 
          (let* ((pict-w (min #+win32 2000 #-win32 4000 size))  ; taille max de l'image en pixels
                 (pict-h 256)
                 (xstep (round size pict-w))
                 (channels-h (round pict-h nch))   ; imag height = 256, channels-h = height of 1 channel
                 (offset-y (round channels-h 2))) ; draw from middle of each channels-h
            (if data 
                (let ((datalist (loop for pix from 0 to (- pict-w 1) collect
                                      (loop for chan from 0 to (- nch 1) collect 
                                            (fli::dereference data 
                                                              :index (+ (min (* (1- size) nch) (* pix xstep nch)) chan)
                                                              :type :float)
                                            )))                      
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
                  pict
                  )
              (setf pict 
                    (om-record-pict *om-default-font2* (om-make-point pict-w pict-h)
                      (loop for i from 0 to (- nch 1) do  
                            (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y) pict-w (+ (* i channels-h) offset-y))
                            (om-with-fg-color *curstream* (om-make-color 0.8 0.2 0.2)
                              (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y 2) pict-w (+ (* i channels-h) offset-y 2))
                              (gp::draw-line *curstream* 0 (+ (* i channels-h) offset-y -2) pict-w (+ (* i channels-h) offset-y -2)))
                      )
                      

                      ))))
        nil
        ))))


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












