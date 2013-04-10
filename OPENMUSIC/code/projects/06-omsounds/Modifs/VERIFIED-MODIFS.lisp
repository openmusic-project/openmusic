;;;================================================================================================================================================================
;;;                                                                       MODIFICATIONS VALIDEES
;;;================================================================================================================================================================
;;;================================================================================================================================================================
;;;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;;================================================================================================================================================================
;;;================================================================================================================================================================
;;;                                                                           audio-api.lisp
;;;================================================================================================================================================================
(in-package :oa)


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
          om-sound-snd-slice-to-paste
          om-sound-sndlasptr
          om-sound-sndlasptr-current
          
          om-cons-snd-pict
          om-sound-get-pict
          om-read-sound-data      
          ) :om-api)



;;;Redéfinition de la classe sound
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

    ;tracknum utilisé par le système, par forcément celui de l'utilisateur
    (tracknum-sys :accessor tracknum-sys :initform -1)

    ;Savoir si ce son joue sur le player caché (pas de tracks) ou sur le visible (tracks system)
    (assoc-player :accessor assoc-player :initform nil)

    ;buffer du son actuel (pas forcément d'origine, évolue)
    (sndbuffer :accessor sndbuffer :initarg :sndbuffer :initform nil)

    ;pointeur LAS fixe (son d'origine au cas où)
    (sndlasptr :accessor sndlasptr :initarg :sndlasptr :initform nil)

    ;;;pointeur LAS évolutif (son actuel suite à toutes les modifications)
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
    )
   )

(defmethod initialize-instance :after ((self om-sound) &rest initargs)
  (setf (oa::assoc-player self) *audio-player-hidden*)
  self)

;;;Méthode d'accès au pointeur de stream
(defmethod om-sound-sndbuffer ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (sndbuffer self)))



;;les 3 au sens LAS
(defmethod om-sound-sndlasptr ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (sndlasptr self)))

(defmethod om-sound-sndlasptr-current ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (sndlasptr-current self)))

(defmethod om-sound-snd-slice-to-paste ((self om-sound))
   (when (or (loaded self) (om-fill-sound-info self))
    (snd-slice-to-paste self)))


;;;Remplissage des slots de la classe sound
(defun om-fill-sound-info (sound)
  (when (and sound (filename sound) (probe-file (filename sound)))
    (print (format nil "Loading sound file : ~s" (namestring (filename sound))))
    (multiple-value-bind (format nch sr ss size skip)
        (sound-get-info (filename sound))
      (if (and format size nch (> size 0) (> nch 0))
        (progn 
          (setf (audio-format sound) format
                (number-of-samples sound) size
                (number-of-channels sound) nch
                (sample-size sound) ss
                (sample-rate sound) sr
                (data-position sound) skip
                (sndbuffer sound) (multiple-value-bind (data size nch) 
                                      (au::load-audio-data (oa::convert-filename-encoding (om-sound-file-name sound)) :float) 
                                    (let* ((sndbuffer data)) sndbuffer))
                (sndlasptr sound) (om::ptr (om::get-sound-data sound))
                (sndlasptr-current sound) (sndlasptr sound)
                (sndlasptr-current-save sound) (sndlasptr sound)
                (number-of-samples-current sound) (las::GetLengthSound (sndlasptr-current sound))
                (sndlasptr-to-play sound) (sndlasptr sound)
                (number-of-samples-to-play sound) (las::GetLengthSound (sndlasptr-to-play sound))
                (snd-slice-to-paste sound) nil)
          (setf (loaded sound) t)
          (unless (om-supported-audio-format format)
            (print (format nil "Warning : unsupported audio format ~A" format))
            (setf (loaded sound) :error)))
        (progn 
          (print (format nil "Error while loading file ~s" (filename sound)))
          (setf (loaded sound) :error))))
    (loaded sound)))


(defun om-update-sound-las-infos (sound)
  (let ()
    (setf (number-of-samples-current sound) (las::GetLengthSound (sndlasptr-current sound)))
    (setf (number-of-samples-to-play sound) (las::GetLengthSound (sndlasptr-to-play sound)))
    ))



;;;Obtention des infos audio (appel libsndfile)
(defun sound-get-info (filename) 
  (let ((format_save (audio-file-type filename)))
    (when (om-supported-audio-format format_save)
        (multiple-value-bind (format nch sr ss size skip)
            (au::sndfile-get-info (convert-filename-encoding filename))
            (values format nch sr ss size skip)))))



;;;Fonctions rendues inutiles
(defmethod om-sound-get-info (path))
(defmethod om-read-sound-data2 ((self om-sound) position nbytes))




(in-package :au)

;;;Adaptation appels libsndfile
(defun sndfile-get-info (path)
  "Returns a matrix of sound data"
  (cffi:with-foreign-object (sfinfo 'sf::SF_INFO)
    (setf (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open (namestring path) sf::SFM_READ sfinfo))
           (frames (fli::dereference (cffi:foreign-slot-pointer sfinfo 'sf::SF_INFO 'sf::frames) :type :int))	  
	   (format (fli::dereference (cffi:foreign-slot-pointer sfinfo 'sf::SF_INFO 'sf::format) :type :int))
           (format_list (map 'list #'digit-char-p (prin1-to-string (write-to-string format :base 16))))
           (channels (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::channels))
	   (skip (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::seekable))
	   (sample-rate (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::samplerate))
           (ss 0))
                 (when (and (= 1 (cadr format_list)) (< (cadddr (cddr format_list)) 6)) (setf format 0))
                 (when (and (= 1 (cadr format_list)) (>= (cadddr (cddr format_list)) 6)) (setf format 1))
                 (when (and (= 2 (cadr format_list)) (< (cadddr (cddr format_list)) 6)) (setf format 2))
                 (when (and (= 2 (cadr format_list)) (>= (cadddr (cddr format_list)) 6)) (setf format 3))
                 ;;;Detection format and Sample size : cf http://www.mega-nerd.com/libsndfile/api.html#open
                 (when (= 1 (cadddr (cddr format_list))) (setf ss 8))
                 (when (= 2 (cadddr (cddr format_list))) (setf ss 16))
                 (when (= 3 (cadddr (cddr format_list))) (setf ss 24))
                 (when (= 4 (cadddr (cddr format_list))) (setf ss 32))
                 (when (= 5 (cadddr (cddr format_list))) (setf ss 8))
                 (when (= 6 (cadddr (cddr format_list))) (setf ss 32))
      (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
      (values format channels sample-rate ss frames skip))))


;;;relisting des formats

(defconstant formatWAVint 0)
(defconstant formatWAVfloat 1)
(defconstant formatAIFFint 2)
(defconstant formatAIFFfloat 3)


(in-package :oa)

(defun om-format-name (format)
  (cond 
    ((equal format au::formatWAVint) "WAVE(int)")
    ((equal format au::formatWAVfloat) "WAVE(float)")
    ((equal format au::formatAIFFint) "AIFF(int)")
    ((equal format au::formatAIFFfloat) "AIFF(float)")))

(defvar *supported-audio-formats* nil)
(setf *supported-audio-formats* (list au::formatWAVint au::formatWAVfloat au::formatAIFFint au::formatAIFFfloat))

(defun default-sample-size (format)
  (cond 
    ((equal format au::formatWAVint) 16)
    ((equal format au::formatWAVfloat) 32)
    ((equal format au::formatAIFFint) 16)
    ((equal format au::formatAIFFfloat) 32)))





(in-package :om)

(defmethod update-buffer-with-current-las ((self sound))
  (let* ()
    (om::save-sound-in-file (oa::sndlasptr-current self) *tmp-draw-filename*)
    (fli:free-foreign-object (oa::sndbuffer self))
    (setf (oa::sndbuffer self) (multiple-value-bind (data size nch) 
                                   (au::load-audio-data (oa::convert-filename-encoding *tmp-draw-filename*) :float)
                                 (let* ((sndbuffer data)) sndbuffer)))
    (setf (pict-sound self) (oa::om-cons-snd-pict *tmp-draw-filename*))))

