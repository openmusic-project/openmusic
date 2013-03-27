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
                (assoc-player sound) *audio-player-hidden*
                (sndbuffer sound) (multiple-value-bind (data size nch) 
                                (au::load-audio-data (oa::convert-filename-encoding (om-sound-file-name sound)) :float) 
                              (let* ((sndbuffer data)) sndbuffer ))
                (sndlasptr sound) (om::ptr (om::get-sound-data sound))
                (sndlasptr-current sound) (sndlasptr sound)
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
(in-package :au)

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

;;;================================================================================================================================================================
;;;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;;================================================================================================================================================================
;;;================================================================================================================================================================
;;;                                                                            soundeditor.lisp
;;;================================================================================================================================================================

(in-package :om)


;;; Décalage des slots d'infos
(defmethod init-titlebar ((self soundeditor))
  (let* ((pathname (om-sound-file-name (object self)))
         (name (if pathname
                   (if (stringp (pathname-type pathname))
                       (string+ (pathname-name pathname) "." (pathname-type pathname))
                     (pathname-name pathname))
                 "No file attached"))) 
  (om-add-subviews (title-bar self)
                   (om-make-dialog-item 'om-static-text (om-make-point 10 4) 
                                        (om-make-point 
                                         (+ 10 (om-string-size (string+ (om-str :file) ": " name)
                                                               *om-default-font1b*))
                                         18)
                                        (string+ (om-str :file) ": " name)
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1b*
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 400 4) (om-make-point 120 18)
                                        (format nil "Format: ~D" (or (om-format-name (om-sound-format (object self))) "--"))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 600 4) (om-make-point 80 18)
                                        (format nil "SR: ~D" (if (om-sound-sample-rate (object self))
                                                                 (round (om-sound-sample-rate (object self)))
                                                               "--"))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 700 4) (om-make-point 80 18)
                                        (format nil "SS: ~D" (or (om-sound-sample-size (object self)) "--"))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                   )))






;;; DEBUG JEAN MOUVEMENT PLAYER INTERVAL IS UNBOUND
(defmethod play-selection-from-palette ((self cursor-play-view-mixin))
   (setf (loopplay? *general-player*) (loopplay? self))
   (let ((interval (get-play-interval self))) 
         (multiple-value-bind (obj start end) (get-selection-to-play self)
           (when obj 
             (when (and (scroll-to-0 self) (not (= 0 (om-h-scroll-position self)))
                        (or (< (car interval) (om-point-h (pixel2point self (om-scroll-position self))))
                            (> (car interval) (om-point-h (pixel2point self (om-make-point (+ (om-h-scroll-position self) (w self)) 0))))))
               (om-set-scroll-position self (point2pixel self (om-make-point (car interval) 0) (get-system-etat self)))
               (mapc #'(lambda (view) (om-invalidate-view view)) (attached-cursor-views self)))
             (let ((s (om-point-h (point2pixel self (om-make-point start 0) (get-system-etat self))))) ; (start-position self)))
               (setf (delta-cursor-pos *general-player*) start)
               (om-new-movable-cursor self s 0 4 (h self) 'om-cursor-line)
               (mapc #'(lambda (view) (om-new-movable-cursor view s 0 4 (h self) 'om-cursor-line))
                     (attached-cursor-views self))
               (apply 'PlayAny (cons (get-score-player self) obj))
               (start (draw-play-cursor self start end t))
               )))))



(defmethod om-draw-contents ((self soundPanel))
  (call-next-method)  
  (if (recording? self)
       (om-with-focused-view self  
         (om-with-fg-color self *om-red2-color*
           (om-with-font *om-default-font4b*
              (let ((halftext (round (om-string-size "Recording" *om-default-font4b*) 2)))
                (om-draw-string (- (round (w self) 2) halftext) (round (h self) 2) "Recording")))))
  (if (om-sound-file-name (object (om-view-container self)))
    (let* ((thesound (object (om-view-container self)))
         (dur (or (and (and (om-sound-sample-rate thesound) (om-sound-n-samples thesound))
                       (/ (om-sound-n-samples thesound) (om-sound-sample-rate thesound))) 0))
         (dur-ms (round (om-sound-n-samples thesound) (/ (om-sound-sample-rate thesound) 1000.0)))
         (total-width (om-point-h (om-field-size self)))
         (thepicture (and dur (pic-to-draw thesound)))
         (sr (om-sound-sample-rate thesound))
         (window-h-size (om-point-h (om-view-size self)))
         (window-v-size (om-point-v (om-view-size self)))
         (stream-ptr (oa::sndbuffer thesound))
         (system-etat (get-system-etat self))
         (xmin (car (rangex self)))
         (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
         (xmax (cadr (rangex self)))
         (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
         (xview (- xmax xmin))
         (pict-threshold (/ dur-ms 3)) 
         (step-1smp 1)
         (step-100us (/ sr 10000.0))
         (step-200us (/ sr 5000.0))
         (step-400us (/ sr 2500.0))
         (step-800us (/ sr 1250.0))
         (step-1ms (/ sr 1000.0))
         (step-2ms (/ sr 500.0))
         (step-5ms (/ sr 200.0))
         (step-10ms (/ sr 100.0))
         (step-20ms (/ sr 50.0))
         (step-50ms (/ sr 25.0))
         (step-100ms (/ sr 10.0)))
      ;(om-sound-slice self)
      (om-with-focused-view self
        (when (and thesound thepicture)
          (om-with-fg-color self *om-dark-gray-color*
            (when (or (>= xview 15000) (>= xview pict-threshold)) 
              (om-draw-picture self thepicture (om-make-point 0 0) (om-subtract-points (om-field-size self) (om-make-point 0 15))))
              (when (and (< xview 15000) (>= xview 8000) (< xview pict-threshold)) 
                    (om-draw-waveform self step-50ms))
              (when (and (< xview 8000) (>= xview 5500) (< xview pict-threshold)) 
                    (om-draw-waveform self step-20ms))
              (when (and (< xview 5500) (>= xview 3000) (< xview pict-threshold)) 
                    (om-draw-waveform self step-10ms))
              (when (and (< xview 3000) (>= xview 1800) (< xview pict-threshold)) 
                    (om-draw-waveform self step-5ms))
              (when (and (< xview 1800) (>= xview 1100) (< xview pict-threshold)) 
                    (om-draw-waveform self step-2ms))
              (when (and (< xview 1100) (>= xview 900) (< xview pict-threshold)) 
                    (om-draw-waveform self step-1ms))
              (when (and (< xview 900) (>= xview 600) (< xview pict-threshold)) 
                    (om-draw-waveform self step-800us))
              (when (and (< xview 600) (>= xview 300) (< xview pict-threshold)) 
                    (om-draw-waveform self step-400us))
              (when (and (< xview 300) (>= xview 75) (< xview pict-threshold)) 
                    (om-draw-waveform self step-200us))
              (when (and (< xview 75) (>= xview 30) (< xview pict-threshold)) 
                    (om-draw-waveform self step-100us))
              (when (and (< xview 30) (< xview pict-threshold)) 
                    (om-draw-waveform self step-1smp))
        (om-with-fg-color self *om-blue-color*
          (loop for item in (markers thesound) 
                for k = 0 then (+ k 1) do
                (om-with-line-size (if (member k (selection? self)) 2 1)
                  (om-draw-line (round (* total-width item) dur) 0 (round (* total-width item) dur) (h self))
                  (om-fill-rect (- (round (* total-width item) dur) 2) 0 5 5)))))
      (when (grille-p self)
        (draw-grille self))
      (draw-interval-cursor self)
      (unless thepicture
        (if (and (om-sound-file-name thesound) (zerop dur))
            (om-with-focused-view self
              (om-draw-string 30 30 (format nil "Error: file ~s is empty" (om-sound-file-name (object (editor self))))))
          (om-with-focused-view self 
            (om-draw-string (round (w self) 2) (round (h self) 2) "..."))
          )
        )))) (om-with-focused-view self(om-draw-string 10 40 (format nil "You have to load a file."))))))


(defmethod om-draw-waveform ((self soundPanel) smpstep)
  (let* ((thesound (object (om-view-container self)))
         (nch (om-sound-n-channels thesound))
         (sr (om-sound-sample-rate thesound))
         (window-v-size (om-point-v (om-view-size self)))
         (stream-ptr (oa::om-sound-sndbuffer thesound))
         (system-etat (get-system-etat self))
         (xmin (car (rangex self)))
         (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
         (xmax (cadr (rangex self)))
         (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
         (xtime (- xmax xmin))
         (basicstep smpstep)
         (timestep (/ (/ sr 1000.0) smpstep))
         (channels-h (round window-v-size nch))
         (offset-y (round channels-h 2))
         (datalist (loop for pt from 0 to (* timestep xtime) collect
                                      (loop for chan from 0 to (- nch 1) collect 
                                            (fli::dereference stream-ptr 
                                                              :index (+ (* xmin (round sr 1000) nch) (round (* pt basicstep nch)) chan)
                                                              :type :float)))))
    (loop for i from 0 to (- nch 1) do  
                                (om-draw-line pixmin (+ (* i channels-h) offset-y) pixmax (+ (* i channels-h) offset-y)))
    (setf sampleprev (car datalist))
    (loop for sample in (cdr datalist)
                                  for i = 0 then (+ i 1) do 
                                  (loop for val in sample 
                                        for c = 0 then (+ c 1) do
                                        (setf pixpoint (round (* offset-y val))) ; scaled 0-1 --> 0 -->256/2
                                        (setf pixtime (om-point-h (point2pixel self (om-make-point (+ xmin (* i (/ 1 timestep)) (/ 1 timestep)) 0) system-etat)))
                                        (setf pixtimeprev (om-point-h (point2pixel self (om-make-point (+ xmin (* i (/ 1 timestep))) 0) system-etat)))
                                        (om-draw-line  pixtimeprev (+ offset-y (* c channels-h) (round (* offset-y (nth c sampleprev))))
                                                       pixtime (+ offset-y (* c channels-h) pixpoint)) 
                                        ) (setf sampleprev sample))))

