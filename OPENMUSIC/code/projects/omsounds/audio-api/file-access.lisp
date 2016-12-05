;;==================================
;;; AUDIO FILE ACCESS TOOLS (R/W)
;;==================================

(in-package :cl-user)

;;; genertal package for low-level audio features
(defpackage :audio-io
  (:use cl-user common-lisp))

(in-package :audio-io)

(export '(
          om-sound-get-info
          om-get-sound-buffer
          om-get-sound-display-array
          om-get-sound-display-array-slice
          om-fill-sound-display-array
          om-save-buffer-in-file
          ) :audio-io)

;;==================================
;;; FILE I/O
;;==================================
;;;Convert path
(defun convert-filename-encoding (path)
  #+cocoa (external-format::decode-external-string (external-format::encode-lisp-string (namestring path) :utf-8) :latin-1)
  #-cocoa (namestring path))

;;;Acquire sound infos
(defun om-get-sound-info (path)
  ;; RETURNS format n-channels sample-rate sample-size size skip
  (sf::sndfile-get-info (convert-filename-encoding path)))

;; RETURNS buffer format n-channels sample-rate sample-size size skip
(defun om-get-sound-buffer (path &optional (type :float) (interleaved nil))
  (when (probe-file path)
  (if interleaved 
      (sf::sndfile-get-sound-buffer (convert-filename-encoding path) type)
    (multiple-value-bind (buffer format n-channels sample-rate sample-size size skip)
        (sf::sndfile-get-2D-sound-buffer (convert-filename-encoding path) type)
          (if (= 1 n-channels)
            
              (let ((buffer2 (fli::allocate-foreign-object :type :pointer)))
                (setf (fli:dereference buffer2) buffer)
                (values buffer2 format n-channels sample-rate sample-size size skip))
            
            (unwind-protect 
                
                (let ((buffer2 (fli::allocate-foreign-object 
                                :type :pointer :nelems n-channels
                                :initial-contents (loop for c from 0 to (1- n-channels) collect 
                                                        (fli::allocate-foreign-object 
                                                         :type type 
                                                         :nelems size)))))
                  (dotimes (i size)
                    (dotimes (c n-channels)
                      ;;; apparently FLI:DEREFERENCE iS MUCH FASTER THAN CFFI:MEM-AREF
                      (setf (fli:dereference (fli:dereference buffer2 :type :pointer :index c) :type type :index i)
                            (fli:dereference buffer :type type :index (+ c (* n-channels i))))
                      ;(setf (cffi::mem-aref (cffi::mem-aref buffer2 :pointer c) type i)
                      ;      (cffi::mem-aref buffer type (+ c (* n-channels i))))
                      ))
                  (values buffer2 format n-channels sample-rate sample-size size skip))
              (cffi::foreign-free buffer)))))))

;;;Free a buffer
(defun om-free-sound-buffer (buffer nch)
  (when nch (dotimes (c nch) (fli::free-foreign-object (fli:dereference buffer :type :pointer :index c))))
  (fli::free-foreign-object buffer))

;;;Save a buffer in a file
(defun om-save-buffer-in-file (buffer filename size nch sr resolution format)
  (sf::sndfile-save-sound-in-file buffer filename size nch sr resolution format))




;;;============================================
;;; DISPLAY ARRAY
;;;============================================


;;;Function used to get the display array from the file path (and choosed max window)

#+libsndfile
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

#+libsndfile
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


#+libsndfile
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




