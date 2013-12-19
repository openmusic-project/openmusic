(in-package :om)

;======================================================
;SND Process boxes relying on Libsndfile
;======================================================
; D. Bouche 2013
;======================================================

(defclass om-sound-data (simple-container)
  ((buffer :accessor buffer :initform nil :initarg :buffer)
   (tracknum :accessor tracknum :initform 0 :initarg :tracknum)
   (size :accessor size :initform nil :initarg :size)
   (nch :accessor nch :initform nil :initarg :nch)
   (sr :accessor sr :initform nil :initarg :sr)))

(defmethod get-obj-dur ((self om-sound-data)) (round (size self) (/ (sr self) 1000.0)))

(defmethod extent->ms ((self om-sound-data)) (round (size self) (/ (sr self) 1000.0)))

(defmethod allowed-in-maq-p ((self om-sound-data)) t)

(defmethod Class-has-editor-p ((self om-sound-data)) nil)

(defmethod! get-om-sound-data ((self sound))
            :icon 221
            (and (om-sound-file-name self)
                 (let ((infos (get-buffer-and-infos-from-path (om-path2cmdpath (om-sound-file-name self)))))
                   (make-instance 'om-sound-data 
                                  :buffer (nth 0 infos)
                                  :tracknum 0
                                  :size (nth 1 infos)
                                  :nch (nth 2 infos)
                                  :sr (nth 3 infos)))))

(defmethod! get-om-sound-data ((self string))
            :icon 221
            (let ((infos (get-buffer-and-infos-from-path (om-path2cmdpath self))))
              (make-instance 'om-sound-data 
                             :buffer (nth 0 infos)
                             :tracknum 0
                             :size (nth 1 infos)
                             :nch (nth 2 infos)
                             :sr (nth 3 infos))))

(defmethod! get-om-sound-data ((self pathname))
            :icon 221
            (get-om-sound-data (namestring self)))

(defun get-buffer-and-infos-from-path (path)
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    (let* ((sndfile-handle-in (sf::sf_open path sf::SFM_READ sfinfo))
           (size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames))
           (size (fli::dereference size-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels))
           (nch (fli::dereference channels-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate))
           (sr (fli::dereference sr-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (buffer-size (* size nch))
           (buffer (fli:allocate-foreign-object :type :float :nelems buffer-size :fill 0)))
      (sf::sf-read-float sndfile-handle-in buffer buffer-size)
      (sf::sf_close sndfile-handle-in)
      (list buffer size nch sr))))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SAVE-SOUND///////////////
(defmethod om-save-sound-in-file ((self om-sound-data) filename &optional (format nil))
  (let ((sndformat (or format *def-snd-format*))
        (resolution (case *audio-res*
                      (8 sf::sf_format_pcm_s8)
                      (16 sf::sf_format_pcm_16)
                      (24 sf::sf_format_pcm_24)
                      (32 sf::sf_format_pcm_32)              
                      (otherwise sf::sf_format_pcm_16))))
    (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) (sr self))
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) (nch self))
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) (if (equal sndformat 'aiff)
                                                                                              (logior sf::sf_format_aiff resolution)
                                                                                            (logior sf::sf_format_wav resolution)))
      (let ((sndfile-handle-out (sf::sf_open filename sf::SFM_WRITE sfinfo)))
        (sf::sf-write-float sndfile-handle-out (buffer self) (* (nch self) (size self)))
        (sf::sf_close sndfile-handle-out)
        (fli:free-foreign-object (buffer self)))))
  (probe-file filename))

(defmethod! om-save-sound ((self om-sound-data) filename &optional (format 'aiff))
            :icon 107
            :initvals '(nil nil 'aiff)
            :indoc '("a om-sound-data buffer" "output file pathname" "audio format")
            :menuins '((2 (("AIFF" 'aiff) ("WAV" 'wav))))
            :doc "Saves a 'om-sound-data' buffer as an audio file.

'om-sound-data' buffers are generated with libsndfile."
            (let ((sndformat (or format *def-snd-format*))
                  (file (or filename (om-choose-new-file-dialog :directory (def-save-directory) 
                                                                :prompt (om-str "Save as...")
                                                                :types (cond ((equal format 'aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                             ((equal format 'wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                             (t nil)))))) 
              (when file
                (setf *last-saved-dir* (make-pathname :directory (pathname-directory file)))
                (om-save-sound-in-file self (om-path2cmdpath file) format))))

(defmethod! om-save-sound ((self sound) filename &optional (format 'aiff))
            (om-save-sound (get-om-sound-data self) filename format))


(defmethod* objfromobjs ((self om-sound-data) (type sound))
            (let ((snd (om-save-sound self nil)))
              (when snd (load-sound-file snd))))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SAVE-SOUND/////////////




;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-MIX///////////////
(defmethod! om-sound-mix ((s1 om-sound-data) (s2 om-sound-data) &optional (method 0))
            :icon 101
            :initvals '(nil nil 0)
            :menuins '((2 (("Sum" 0)
                           ("Sum / Average" 1)
                           ("Sum / Hard Limiting" 2))))
            :indoc '("an om-sound-data" "an om-sound-data" "a mixing method")
            :doc "Generates a mix of <s1> and <s2>."
            
            (if (and (= (nch s1) (nch s2)) (= (sr s1) (sr s2)))
            
                (let* ((nch (nch s1))
                       (size1 (* nch (size s1)))
                       (size2 (* nch (size s2)))
                       (final-size (max size1 size2))
                       (final-buffer (fli:allocate-foreign-object :type :float :nelems final-size :fill 0))
                       a b res)
                  
                  (cond ((= method 0) 
                         (dotimes (i final-size)
                           (setf (fli:dereference final-buffer :index i)
                                 (+ (if (< i size1) (fli:dereference (buffer s1) :index i) 0.0) 
                                    (if (< i size2) (fli:dereference (buffer s2) :index i) 0.0)))))
                        ((= method 1)
                         (dotimes (i final-size)
                           (setf (fli:dereference final-buffer :index i)
                                 (/ (+ (if (< i size1) (fli:dereference (buffer s1) :index i) 0.0) 
                                       (if (< i size2) (fli:dereference (buffer s2) :index i) 0.0)) 2))))
                        ((= method 2) 
                         (dotimes (i final-size)
                           (setf res (+ (if (< i size1) (fli:dereference (buffer s1) :index i) 0.0) 
                                        (if (< i size2) (fli:dereference (buffer s2) :index i) 0.0)))
                           (setf (fli:dereference final-buffer :index i)
                                 (cond ((< res -1) -1.0)
                                       ((> res 1) 1.0)
                                       (t res))))))

                  (fli:free-foreign-object (buffer s1))
                  (fli:free-foreign-object (buffer s2))

                  (make-instance 'om-sound-data 
                                 :buffer final-buffer
                                 :size (round final-size nch)
                                 :nch nch
                                 :sr (sr s1)))
              (progn
                (print "ERROR : TRYING TO MIX 2 SOUNDS WITH DIFFERENT NUMBER OF CHANNELS OR DIFFERENT SAMPLE-RATE. OUTPUT IS THE INPUT 1.")
                s1)))


(defmethod! om-sound-mix ((s1 sound) (s2 sound) &optional (method 0))
            (om-sound-mix (get-om-sound-data s1) (get-om-sound-data s2) method))

(defmethod! om-sound-mix ((s1 om-sound-data) (s2 sound) &optional (method 0))
            (om-sound-mix s1 (get-om-sound-data s2) method))

(defmethod! om-sound-mix ((s1 sound) (s2 om-sound-data) &optional (method 0))
            (om-sound-mix (get-om-sound-data s1) s2 method))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-MIX///////////////




;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-SILENCE///////////
(defmethod! om-sound-silence ((dur float) &optional (channels 1))
            :icon 105
            :initvals '(1.0)
            :indoc '("duration (float or interger)" "number of channels")
            :doc "Generates a silence of duration = <dur>.
<dur> is considered to be in seconds if a float number is given (e.g. 20.0) or in milliseconds if integer (e.g. 20)\."
            (make-instance 'om-sound-data 
                           :buffer (fli:allocate-foreign-object :type :float :nelems (round (* dur *audio-sr* (if (< channels 1) 1 channels))) :fill 0)
                           :size (round (* dur *audio-sr*))
                           :nch (if (< channels 1) 1 channels)
                           :sr *audio-sr*))

(defmethod! om-sound-silence ((dur integer) &optional (channels 1))
            (make-instance 'om-sound-data 
                           :buffer (fli:allocate-foreign-object :type :float :nelems (round (* dur *audio-sr* 0.001 (if (< channels 1) 1 channels))) :fill 0)
                           :size (round (* dur *audio-sr*))
                           :nch (if (< channels 1) 1 channels)
                           :sr *audio-sr*))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-SILENCE///////////




;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-SEQ///////////////
(defmethod! om-sound-seq ((s1 om-sound-data) (s2 om-sound-data) &optional (crossfade 0))
            :icon 100
            :initvals '(nil nil 0)
            :indoc '("a sound" "a sound" "cross-fading duration (ms)")
            "Concatenates <s1> and <s2>. 
<crossfade> (duration in milliseconds) determines a fade-in/fade out overlapping between the sounds."
            (if (and (= (nch s1) (nch s2)) (= (sr s1) (sr s2)))
                (let* ((nch (nch s1))
                       (sr (sr s1))
                       (size1 (* nch (size s1)))
                       (size2 (* nch (size s2)))
                       (smp-cross (round (* nch crossfade sr 0.001)))
                       (smp-cross-side (* nch (ceiling smp-cross 2)))
                       (factor1 (- (/ 1.0 (max 1 smp-cross))))
                       (factor2 (/ 1.0 (max 1 smp-cross)))
                       (final-size (- (+ size1 size2) smp-cross-side))
                       (final-buffer (fli:allocate-foreign-object :type :float :nelems final-size :fill 0)))
                  
                  (dotimes (i final-size)
                    (setf (fli:dereference final-buffer :index i)
                          (cond ((< i (- size1 smp-cross)) 
                                 (fli:dereference (buffer s1) :index i))
                                ((and (>= i (- size1 smp-cross)) (<= i size1)) 
                                 (+ (* (1+ (* factor1 (- i (- size1 smp-cross)))) (fli:dereference (buffer s1) :index i))
                                    (* factor2 (- i (- size1 smp-cross)) (fli:dereference (buffer s2) :index (+ smp-cross (- i size1))))))
                                ((> i size1) 
                                 (fli:dereference (buffer s2) :index (+ smp-cross (- i size1)))))))

                  (fli:free-foreign-object (buffer s1))
                  (fli:free-foreign-object (buffer s2))

                  (make-instance 'om-sound-data 
                                 :buffer final-buffer
                                 :size (round final-size nch)
                                 :nch nch
                                 :sr (sr s1)))
              (progn
                (print "ERROR : TRYING TO SEQUENCE 2 SOUNDS WITH DIFFERENT NUMBER OF CHANNELS OR DIFFERENT SAMPLE-RATE. OUTPUT IS THE INPUT 1.")
                s1)))

(defmethod! om-sound-seq ((s1 om-sound-data) (s2 sound) &optional (crossfade 0))
            (om-sound-seq s1 (get-om-sound-data s2) crossfade))

(defmethod! om-sound-seq ((s1 sound) (s2 om-sound-data) &optional (crossfade 0))
            (om-sound-seq (get-om-sound-data s1) s2 crossfade))

(defmethod! om-sound-seq ((s1 sound) (s2 sound) &optional (crossfade 0))
            (om-sound-seq (get-om-sound-data s1) (get-om-sound-data s2) crossfade))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-SEQ///////////////






;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-FADE//////////////
(defmethod! om-sound-fade ((s om-sound-data) in out)
            :icon 102
            :initvals '(nil 100 100)
            :indoc '("a om-sound-data" "fade in duration (ms)" "fade out duration (ms)")
            "Generates a fade-in and/or fade-out effect on <s>."

            (let* ((nch (nch s))
                   (sr (sr s))
                   (size (size s))
                   (size2 (* size nch))
                   (fade-in-frames (round (* in sr 0.001 nch)))
                   (fade-in-factor (/ 1.0 fade-in-frames))
                   (fade-out-frames (round (* out sr 0.001 nch)))
                   (fade-out-frames-start (- size2 (round (* out sr 0.001 nch))))
                   (fade-out-factor (- (/ 1.0 fade-out-frames))))
                  
              (dotimes (i size2)
                (setf (fli:dereference (buffer s) :index i)
                      (cond ((< i fade-in-frames) 
                             (* fade-in-factor i (fli:dereference (buffer s) :index i)))
                            ((> i fade-out-frames-start) 
                             (* (1+ (* fade-out-factor (- i (- size2 fade-out-frames)))) (fli:dereference (buffer s) :index i)))
                            (t (fli:dereference (buffer s) :index i)))))              
              s))

(defmethod! om-sound-fade ((s sound) in out)
            (om-sound-fade (get-om-sound-data s) in out))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-FADE//////////////





;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-LOOP//////////////
(defmethod! om-sound-loop ((s om-sound-data) n)
            :icon 103
            :initvals '(nil 3)
            :indoc '("a sound" "a number")
            "Generates a <n>-times repetition of <s>."
            (let* ((nch (nch s))
                   (size (size s))
                   (size2 (* nch size))
                   (final-buffer (fli:allocate-foreign-object :type :float :nelems (* n size2) :fill 0)))
              (dotimes (i (* n (nch s) (size s)))
                (setf (fli:dereference final-buffer :index i) (fli:dereference (buffer s) :index (mod i size2))))
              
              (fli:free-foreign-object (buffer s))
              
              (make-instance 'om-sound-data 
                             :buffer final-buffer
                             :size (* n size)
                             :nch nch
                             :sr (sr s))))

(defmethod! om-sound-loop ((s sound) n)
            (om-sound-loop (get-om-sound-data s) n))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-LOOP//////////////






;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-CUT///////////////
(defmethod! om-sound-cut ((s om-sound-data) beg end)
            :icon 104
            :initvals '(nil 0 1000)
            :indoc '("a sound" "begin time (ms)" "end time (ms)")
            "Cuts and returns an extract between <beg> and <end> in <s>."
            (let* ((nch (nch s))
                   (sr (sr s))
                   (size2 (* (size s) nch))
                   (beg-smp (round (* beg sr 0.001 nch)))
                   (end-smp (* end sr 0.001 nch))
                   (end-smp (round (if (> end-smp size2) size2 end-smp)))
                   (lengthfinal (- end-smp beg-smp))
                   (final-buffer (fli:allocate-foreign-object :type :float :nelems lengthfinal :fill 0)))

              (dotimes (i lengthfinal)
                (setf (fli:dereference final-buffer :index i) (fli:dereference (buffer s) :index (+ beg-smp i))))
              
              (fli:free-foreign-object (buffer s))
              
              (make-instance 'om-sound-data 
                             :buffer final-buffer
                             :size (round lengthfinal nch)
                             :nch nch
                             :sr sr)))

(defmethod! om-sound-cut ((s sound) beg end)
            (om-sound-cut (get-om-sound-data s) beg end))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-CUT///////////////






;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-VOL///////////////
(defmethod! om-sound-vol ((s om-sound-data) gain &optional (in 1) (out 1))
            :icon 106
            :initvals '(nil 1.0 100 100)
            :indoc '("a sound" "a gain value" "fade in duration (ms)" "fade out duration (ms)")
            "Adds gain effect (volume) on <s>. 

<gain> is a multiplicative factor to the sound sample values.
<in> and <out> determine fade-in / fade-out periods for the gain effect."
            (let* ((nch (nch s))
                   (sr (sr s))
                   (size (size s))
                   (size2 (* size nch))
                   (fade-in-frames (round (* in sr 0.001 nch)))
                   (fade-in-factor (/ (1- gain) fade-in-frames))
                   (fade-out-frames (round (* out sr 0.001 nch)))
                   (fade-out-factor (/ (- 1 gain) fade-out-frames))
                   (fade-out-frame-start (- size2 fade-out-frames))
                   (fade-out-factor (- (/ 1.0 fade-out-frames))))
                  
              (dotimes (i size2)
                (setf (fli:dereference (buffer s) :index i)
                      (* (cond ((< i fade-in-frames) (1+ (* fade-in-factor i)))
                               ((>= i fade-out-frame-start) (+ gain (* fade-out-factor (- i (- size2 fade-out-frames)))))
                               (t gain)) 
                         (fli:dereference (buffer s) :index i))))

              s))

(defmethod! om-sound-vol ((s sound) gain &optional (in 1) (out 1))
            (om-sound-vol (get-om-sound-data s) gain in out))
;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-VOL//////////////
