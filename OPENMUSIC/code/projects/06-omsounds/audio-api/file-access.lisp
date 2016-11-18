;;==================================
;;; AUDIO FILE ACCESS TOOLS (R/W)
;;==================================


(in-package :om-audio)


(export '(
          om-sound-get-info
          om-get-sound-buffer
          om-get-sound-display-array
          om-get-sound-display-array-slice
          om-fill-sound-display-array
          om-save-sound-in-file
          resample-audio-buffer

          ) :om-audio)

(in-package :sf)

(defconstant formatAiff 0)
(defconstant formatWave 1)
(defconstant formatAifc 2)
(defconstant formatWAVint 0)
(defconstant formatWAVfloat 1)
(defconstant formatAIFFint 2)
(defconstant formatAIFFfloat 3)

(defun decode-format (sndfile-format) ; (print sndfile-format)
 (let* ((format_list (map 'list #'digit-char-p (prin1-to-string (write-to-string sndfile-format :base 16))))
        (ff (cond ((and (= 1 (cadr format_list)) (< (cadddr (cddr format_list)) 6)) 0)
                  ((and (= 1 (cadr format_list)) (>= (cadddr (cddr format_list)) 6)) 1)
                  ((and (= 2 (cadr format_list)) (< (cadddr (cddr format_list)) 6)) 2)
                  ((and (= 2 (cadr format_list)) (>= (cadddr (cddr format_list)) 6)) 3)
                  (t nil)))
        (ss (and ff 
                 (cond ((= 1 (cadddr (cddr format_list))) 8)
                       ((= 2 (cadddr (cddr format_list))) 16)
                       ((= 3 (cadddr (cddr format_list))) 24)
                       ((= 4 (cadddr (cddr format_list))) 32)
                       ((= 5 (cadddr (cddr format_list))) 8)
                       ((= 6 (cadddr (cddr format_list))) 32)
                       (t -1))))
        (name (case ff
                (0 "Wav(int)")
                (1 "Wav(float)")
                (2 "AIFF(int)")
                (3 "AIFF(float)")
                (otherwise nil)
                )))
   (values ff ss name)))
  


(defun sndfile-get-info (path)
  "Returns info about the soudn file (not the actual data)."
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames))
           (channels (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels))
           (sr (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate))
           (format (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format))
           (skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable)))
      ;(print (sf::sf_format_check sfinfo))
      (multiple-value-bind (ff ss nn)
          (decode-format format)
        ;;;Detection format and Sample size : cf http://www.mega-nerd.com/libsndfile/api.html#open 
        (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
        ;(print (list nn channels sr ss size skip))
        (values nn channels sr ss size skip)))))


(defun sndfile-get-sound-buffer (path &optional (datatype :float))
  "Returns a sound data buffer + info. The soudn buffer must be freed."
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames))
           (size (fli::dereference size-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels))
           (channels (fli::dereference channels-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate))
           (sr (fli::dereference sr-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::format))
           (format (fli::dereference format-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable))
           (buffer-size (* size channels))
           (buffer (fli:allocate-foreign-object :type datatype :nelems buffer-size :fill 0))
           (frames-read 
            (ignore-errors
              (case datatype
                (:double (sf::sf-readf-double sndfile-handle buffer buffer-size))
                (:float (sf::sf-readf-float sndfile-handle buffer buffer-size))
                (:int (sf::sf-readf-int sndfile-handle buffer buffer-size))
                (:short (sf::sf-readf-short sndfile-handle buffer buffer-size))
                (otherwise (print (concatenate 'string "Warning: unsupported datatype for reading audio data: " (string datatype))))))))
      (multiple-value-bind (ff ss nn)
          (decode-format format)
        (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
        (values buffer nn channels sr ss size skip)))))



;; WRITE 
(defun sndfile-save-sound-in-file (buffer filename size nch sr resolution format &optional (datatype :float))
  (let* ((res (case resolution
               (8 sf::sf_format_pcm_s8)
               (16 sf::sf_format_pcm_16)
               (24 sf::sf_format_pcm_24)
               (32 sf::sf_format_pcm_32)              
               (otherwise sf::sf_format_pcm_16)))
        (format (logior (case format 
                          (:aiff sf::sf_format_aiff)
                          (:wav sf::sf_format_wav)
                          (:ogg sf::sf_format_ogg)
                          (:flac sf::sf_format_flac)
                          (otherwise sf::sf_format_aiff))
                        res)))
        
    (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) sr)
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) nch)
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) format)

      (let ((sndfile-handle-out (sf::sf_open filename sf::SFM_WRITE sfinfo)))
            ;(datatype (fli::pointer-element-type buffer))  ;; not reliable all the time :(
        (case datatype
          (:double (sf::sf-write-double sndfile-handle-out buffer (* nch size)))
          (:float (sf::sf-write-float sndfile-handle-out buffer (* nch size)))
          (:int (sf::sf-write-int sndfile-handle-out buffer (* nch size)))
          (:short (sf::sf-write-short sndfile-handle-out buffer (* nch size)))
          (otherwise (print (concatenate 'string "Warning: unsupported datatype for writing audio data: " (string datatype)))))
        
        (sf::sf_close sndfile-handle-out)
        )))
  (probe-file filename))


(in-package :om-audio)

;======================
; FORMAT HANDLERS
;======================

(defvar *additional-audio-formats* nil)

(defun try-other-file-support (path ext)
  (let ((format-id (car (find ext *additional-audio-formats* :key 'cdr 
                              :test #'(lambda (ext list) (find ext list :test 'string-equal))))))
    (and format-id 
         (audio-file-get-info format-id path))))
   
;;; MUST RETURN (values format channels sr ss size skip)
(defmethod audio-file-get-info (type path) nil)

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
  (let* ((cool-path (convert-filename-encoding path))
         (sf-info  (multiple-value-list (sf::sndfile-get-info cool-path))))
    (if (car sf-info) (values-list sf-info)
      (try-other-file-support cool-path (pathname-type path)))))
 

(defun om-get-sound-buffer (path &optional (format :double))
  ;; RETURNS buffer format n-channels sample-rate sample-size size skip
  (sf::sndfile-get-sound-buffer (convert-filename-encoding path) format))


;;;Function used to get the display array from the file path (and choosed max window)
(defun om-get-sound-display-array (path &optional (window 128))
  ;;;Ouverture d'un descripteur libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* (;;;Remplissage du descripteur et affectation aux variables temporaires
           (sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames) :type :int :index #+powerpc 1 #-powerpc 0))
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           ;(sr (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) :type :int :index #+powerpc 1 #-powerpc 0))
           ;(format (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) :type :int :index #+powerpc 1 #-powerpc 0))
           ;(skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable))
           ;;;Variables liées au calcul de waveform
           (buffer-size (* window channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))   ;Fenêtrage du son
           (MaxArray (make-array (list channels (ceiling size window)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (ceiling size window)))
           (frames-read 0)
           maxi)
      (loop for indx from 0 do ;(print (list indx "/" (ceiling size window)))
            (setq frames-read (sf::sf-readf-float sndfile-handle buffer window))
            (dotimes (n channels)
              (dotimes (i window)
                (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
              (setf (aref MaxArray n (min indx indxmax)) maxi)
              (setq maxi 0.0))
            while (= frames-read window))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle)
      MaxArray)))


;;;Function used to FILL the display array of a sound (and choosed max window)
(defmethod om-fill-sound-display-array ((format t) path ptr channels size &optional (window 128))
  ;(print (list channels size window))
  ;;;Ouverture d'un descripteur libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* (;;;Remplissage du descripteur et affectation aux variables temporaires
           (sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames) :type :int :index #+powerpc 1 #-powerpc 0))
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           ;(sr (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) :type :int :index #+powerpc 1 #-powerpc 0))
           ;(format (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) :type :int :index #+powerpc 1 #-powerpc 0))
           ;(skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable))
           ;;;Variables liées au calcul de waveform
           (buffer-size (* window channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))   ;Fenêtrage du son
           ;(MaxArray (make-array (list channels (ceiling size window)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (ceiling size window)))
           (frames-read 0)
           maxi)
      (loop for indx from 0 do ;(print (list indx "/" (ceiling size window)))
            (setq frames-read (sf::sf-readf-float sndfile-handle buffer window))
            (dotimes (n channels)
              (dotimes (i window)
                (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
              ;(setf (aref MaxArray n (min indx indxmax)) maxi)
              (setf (fli:dereference ptr :index (+ (min indx indxmax) (* n (ceiling size window)))) maxi)
              (setq maxi 0.0))
            while (= frames-read window))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle))))


(defmethod om-get-sound-display-array-slice ((format t) path size nchannels start-time end-time)
  ;;;Ouverture d'un descripteur libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* (;;;Remplissage du descripteur et affectation aux variables temporaires
           (sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (sr (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ratio (* sr 0.001))
           (start-smp (floor (* start-time sr-ratio)))
           (end-smp (ceiling (* end-time sr-ratio)))
           (dur-smp (- end-smp start-smp))
           ;;; use nchannels !
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           (window (/ dur-smp size 1.0))
           (window-adaptive (round window))
           ;;;Variables liées au calcul de waveform
           (buffer-size (* (ceiling window) channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))   ;Fenêtrage du son
           (MaxArray (make-array (list channels (min size dur-smp)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (min size dur-smp)))
           (frames-read 0)
           (frames-count 0)
           (winsum 0)
           maxi throw-buffer)
      (when (> start-smp 0)
        (setq throw-buffer (fli::allocate-foreign-object :type :float :nelems (* start-smp channels)))
        (sf::sf-readf-float sndfile-handle throw-buffer start-smp)
        (fli:free-foreign-object throw-buffer))
      (if (> dur-smp size)
          (loop for indx from 0 do
                (setq winsum (+ winsum window-adaptive))
                (if (> indx 0) (setq window-adaptive (- (round (* (+ 2 indx) window)) (round winsum))))
                (setq frames-read (sf::sf-readf-float sndfile-handle buffer window-adaptive)
                      frames-count (+ frames-count frames-read))
                (dotimes (n channels)
                  (dotimes (i window-adaptive)
                    (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
                  (setf (aref MaxArray n (min indx indxmax)) maxi)
                  (setq maxi 0.0))
                while (and (< frames-count dur-smp) (= frames-read window-adaptive)))
        (loop for indx from 0 do
              (setq window-adaptive (max window-adaptive 1)
                    frames-read (sf::sf-readf-float sndfile-handle buffer window-adaptive)
                    frames-count (+ frames-count frames-read))
              (dotimes (n channels)
                (setf (aref MaxArray n (min indx indxmax)) (fli:dereference buffer :type :float :index n)))
              while (and (< frames-count size) (= frames-read window-adaptive))))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle)
      MaxArray)))


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
        (values NIL (lsr::src-strerror res))))))

