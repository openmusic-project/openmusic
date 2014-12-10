(in-package :om)

;======================================================
;SND Process boxes
; THIS FILE USES LISPWORKS-SPECIFIC TOOLS FOR MEMORY ALLOCATION AND RELEASE
;======================================================
; D. Bouche 2013
;======================================================
; List of available methods :
;   - sound-silence
;   - sound-mix         
;   - sound-fade
;   - sound-cut
;   - sound-vol
;   - sound-normalize
;   - sound-mono-to-stereo
;   - sound-stereo-to-mono
;   - sound-stereo-pan
;   - sound-resample
;   - sound-seq
;   - sound-loop
;   - sound-reverse
;   
;======================================================


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SAVE-SOUND///////////////


(defmethod! save-sound ((self om-sound-data) filename &optional (format :aiff))
            :icon 107
            :initvals '(nil nil 'aiff)
            :indoc '("a sound or om-sound-data buffer" "output file pathname" "audio format")
            :menuins '((2 (("AIFF" :aiff) ("WAV" :wav) ("FLAC" :flac) ("OGG Vorbis" :ogg))))
            :doc "Saves a <self> (om-sound-data buffer) as an audio file."
            (if (null (buffer self))
                (om-beep-msg "Error: null sound buffer")
              (let ((format (or format *def-snd-format*))
                  (file (or filename (om-choose-new-file-dialog :directory (def-save-directory) 
                                                                :prompt (om-str "Save as...")
                                                                :types (cond ((equal format :aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                             ((equal format :wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                             ((equal format :flac) (list (format nil (om-str :file-format) "FLAC") "*.flac"))
                                                                             ((equal format :ogg) (list (format nil (om-str :file-format) "OGG Vorbis") "*.ogg"))
                                                                             (t nil)))))) 
              (when file
                (setf *last-saved-dir* (make-pathname :directory (pathname-directory file)))
                (om-audio::om-save-sound-in-file (buffer self) (namestring file) (size self) (nch self) (sr self) *audio-res* (or format *def-snd-format*))
                ;(fli:free-foreign-object buffer)
                (probe-file (namestring file))
                ))))

(defmethod! save-sound ((self sound) filename &optional (format 'aiff))
  (save-sound (get-om-sound-data self) filename format))


(defmethod* objfromobjs ((self om-sound-data) (type sound))
  (let ((snd (save-sound self nil)))
    (when snd (load-sound-file snd))))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-RESAMPLE//////////



(defmethod! sound-resample ((s om-sound-data) sample-rate &optional (resample-method 0))
            :icon 114
            :initvals '(nil 44100 0)
            :menuins '((2 (("Best Quality" 0)
                           ("Medium Quality" 1)
                           ("Fastest" 2)
                           ("Zero-Order Hold" 3)
                           ("Linear" 4))))
            :indoc '("a sound or sound-data buffer" "new sample rate in Hz" "resampling method")
            "Resamples a sound <s>."
            (cond ((null (buffer s))
                   (om-beep-msg "Error: null sound buffer"))
                  ((and (= (mod sample-rate 1) 0) (> (/ sample-rate (sr s) 1.0) (/ 1.0 256)) (< (/ sample-rate (sr s) 1.0) 256))
                   (let* ((buffer (buffer s))
                          (size (size s))
                          (nch (nch s))
                          (sr (sr s))
                          (ratio (coerce (/ sample-rate sr) 'double-float))
                          (out-size (round (* ratio size)))
                          (final-buffer (om-make-pointer (* out-size nch) :type (smpl-type s) :clear t)))
                     
                     ;;; USE LIBSAMPLERATE
                     ;;; (resample-method values correspond to libsamplerate options)
                     (multiple-value-bind (success newsize-or-error)
                         (om-audio::resample-audio-buffer buffer size nch final-buffer out-size ratio resample-method)



                       (if success
                           (progn
                             ;(fli:free-foreign-object (buffer s))
                             (make-instance 'om-sound-data 
                                            :buffer final-buffer
                                            :size newsize-or-error
                                            :nch nch
                                            :sr sample-rate))
                         (progn
                           (om-beep-msg (format nil "Resample failed to resample and returned this error : ~A. Output is the original input." newsize-or-error ))
                           s)))))
                  (t
                   (om-beep-msg "The sample-rate you supplied is invalid. It must be an integer, and the output-sr/input-sr ratio must be inside [1/256, 256] range. Output is the original input.")
                   s)))


(defmethod! sound-resample ((s sound) sample-rate &optional (method 0))
            ;(declare (type fixnum method))
   (sound-resample (get-om-sound-data s) sample-rate method))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-NORMALIZE/////////
(defmethod! sound-normalize ((s om-sound-data) &optional (method 0))
            :icon 109
            :initvals '(nil 0)
            :menuins '((1 (("Peak" 0)
                           ("Peak RMS / Hard limiting" 1))))
            :indoc '("a sound" "a normalization method")
            "Normalizes a sound <s>.

<method> is a normalization method. Choose between Peak detection or Peak RMS detection."
              (if (null (buffer s))
                (om-beep-msg "Error: null sound buffer")

                (let* ((nch (nch s))
                       (size (size s))
                       (size2 (* size nch))
                       (peak 0.0)
                       (peak-rms 0.0)
                       (gain 0.0)
                       (x 0.0)
                       (tampon (list))
                       (indx 0)
                       (rms 0.0)
                       (tampon-size (* 100 nch))
                       (b2 (om-make-pointer size2 :type (smpl-type s) :clear t)))

              ;(declare (type fixnum nch size size2 indx tampon-size))
              ;(declare (type single-float peak peak-rms gain x rms))
              ;(declare (type list tampon))

                  (cond ((= method 0)
                         (progn 
                           (dotimes (i size2)
                             (setf x (abs (fli:dereference (buffer s) :index i)))
                             (if (> x peak) (setf peak x)))
                           (if (> peak 0)
                               (progn
                                 (setf gain (/ 1.0 peak))
                                 (dotimes (i size2)
                                   (setf (fli:dereference b2 :index i) (* gain (fli:dereference (buffer s) :index i))))))))
                        ((= method 1)
                         (progn
                           (setf indx 0)
                           (loop while (< indx size2) do
                                 (dotimes (i tampon-size)
                                   (when (< indx size2)
                                     (push (fli:dereference (buffer s) :index indx) tampon)
                                     (incf indx)))
                                 (when tampon
                                   (setf tampon (mapcar #'(lambda (x) (* x x)) tampon))
                                   (setf rms (sqrt (/ (reduce #'+ tampon) tampon-size)))
                                   (if (> rms peak-rms) (setf peak-rms rms))
                                   (setf tampon nil)))
                           (dotimes (i size2)
                             (setf x (/ (fli:dereference (buffer s) :index i) peak-rms))
                             (setf (fli:dereference b2 :index i) (cond ((< x -1) -1.0)
                                                                               ((> x 1) 1.0)
                                                                               (t x)))))))
                  (make-instance 'om-sound-data 
                                 :buffer b2
                                 :size size
                                 :nch nch
                                 :sr (sr s))
                  )))

(defmethod! sound-normalize ((s sound) &optional (method 0))
            ;(declare (type fixnum method))
            (sound-normalize (get-om-sound-data s) method))



;;; USE THIS AS DEFAULT NORMALIZER..
(defmethod general-normalize ((norm (eql :om)) inpath outpath val &optional resolution)
  (print "Warning: OM normlizer does not take into account the normalization value.")
  (let ((normalized (sound-normalize (get-om-sound-data inpath))))
    (om-audio::om-save-sound-in-file (buffer normalized) (namestring outpath) 
                                     (size normalized) (nch normalized) (sr normalized) 
                                     (or resolution *audio-res*) *def-snd-format*)
    outpath))

(defmethod get-def-normalize-value ((self (eql :om))) 0.0)
(defmethod get-module-name ((self (eql :om))) "OM internal")


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-SILENCE///////////
(defmethod! sound-silence ((dur float) &optional (channels 1) (sample-rate *audio-sr*))
            :icon 105
            :initvals (list 1.0 1 *audio-sr*)
            :indoc '("duration (float or interger)" "number of channels")
            :doc "Generates a silence of duration = <dur>.
<dur> is considered to be in seconds if a float number is given (e.g. 20.0) or in milliseconds if integer (e.g. 20)\."
            (let ((nsmpl (round (* dur sample-rate)))
                  (ch (if (< channels 1) 1 channels)))
              (make-instance 'om-sound-data 
                             :buffer (om-make-pointer (* nsmpl ch) :type *default-internal-sample-size* :clear t)
                             :size nsmpl
                             :nch ch
                             :sr sample-rate))
            )

(defmethod! sound-silence ((dur integer) &optional (channels 1) (sample-rate *audio-sr*))
   (sound-silence (* dur 0.001) channels sample-rate))



;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-FADE//////////////

(defmethod! sound-fade ((s om-sound-data) (in float) (out float))
            :icon 102
            :initvals '(nil 100 100)
            :indoc '("a om-sound-data" "fade in duration" "fade out duration")
            "Generates a fade-in and/or fade-out effect on <s>.

             <in> and <out> can be in seconds (floats, e.g. 0.3) or milliseconds (integer, e.g. 300)."
            (if (null (buffer s))
                (om-beep-msg "Error: null sound buffer")

            (let* ((nch (nch s))
                   (sr (sr s))
                   (size (size s))
                   (size2 (* size nch))
                   (fade-in-frames (round (* in sr nch)))
                   (fade-in-factor (/ 1.0 fade-in-frames))
                   (fade-out-frames (round (* out sr nch)))
                   (fade-out-frames-start (- size2 (round (* out sr nch))))
                   (fade-out-factor (- (/ 1.0 fade-out-frames)))
                   (b2 (om-make-pointer size2 :type (smpl-type s) :clear t)))

              ;(declare (type fixnum nch sr size size2 fade-in-frames fade-out-frames fade-out-frames-start))
              ;(declare (type single-float fade-in-factor fade-out-factor))
                  
              (dotimes (i size2)
                (setf (fli:dereference b2 :index i)
                      (cond ((< i fade-in-frames) 
                             (* fade-in-factor i (fli:dereference (buffer s) :index i)))
                            ((> i fade-out-frames-start) 
                             (* (1+ (* fade-out-factor (- i (- size2 fade-out-frames)))) (fli:dereference (buffer s) :index i)))
                            (t (fli:dereference (buffer s) :index i)))))         
              
              (make-instance 'om-sound-data 
                             :buffer b2
                             :size size
                             :nch nch
                             :sr sr)
              )))

(defmethod! sound-fade ((s om-sound-data) (in integer) (out integer))
    (sound-fade s (* in 0.001) (* out 0.001))) 

(defmethod! sound-fade ((s sound) in out)
    (sound-fade (get-om-sound-data s) in out))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-LOOP//////////////
(defmethod! sound-loop ((s om-sound-data) n)
            :icon 103
            :initvals '(nil 3)
            :indoc '("a sound" "a number")
            "Generates a <n>-times repetition of <s>."
            (if (null (buffer s))
                (om-beep-msg "Error: null sound buffer")
              (let* ((nch (nch s))
                   (size (size s))
                   (size2 (* nch size))
                   (final-buffer (om-make-pointer (* n size2) :type (smpl-type s) :clear t)))
              
              ;(declare (type fixnum nch size size2))

              (dotimes (i (* n (nch s) (size s)))
                (setf (fli:dereference final-buffer :index i) (fli:dereference (buffer s) :index (mod i size2))))
              
              ;(fli:free-foreign-object (buffer s))
              
              (make-instance 'om-sound-data 
                             :buffer final-buffer
                             :size (* n size)
                             :nch nch
                             :sr (sr s)))))

(defmethod! sound-loop ((s sound) n)
            ;(declare (type fixnum n))
            (sound-loop (get-om-sound-data s) n))



;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-CUT///////////////
(defmethod! sound-cut ((s om-sound-data) (beg float) (end float))
            :icon 104
            :initvals '(nil 0 1000)
            :indoc '("a sound" "begin time" "end time")
            "Cuts and returns an extract between <beg> and <end> in <s>.

            <beg> and <end> can be in seconds (floats, e.g. 0.3) or milliseconds (integer, e.g. 300)."
            (if (null (buffer s))
                (om-beep-msg "Error: null sound buffer")
                
              (let* ((nch (nch s))
                     (sr (sr s))
                     (size2 (* (size s) nch))
                     (beg-smp (round (* beg sr nch)))
                     (end-smp (* end sr nch))
                     (end-smp (round (if (> end-smp size2) size2 end-smp)))
                     (lengthfinal (- end-smp beg-smp))
                     (final-buffer (om-make-pointer lengthfinal :type (smpl-type s) :clear t)))

              ;(declare (type fixnum nch sr size2 beg-smp end-smp lengthfinal))
                
                (dotimes (i lengthfinal)
                  (setf (fli:dereference final-buffer :index i) (fli:dereference (buffer s) :index (+ beg-smp i))))
              
                ;(fli:free-foreign-object (buffer s))
              
                (make-instance 'om-sound-data 
                               :buffer final-buffer
                               :size (round lengthfinal nch)
                               :nch nch
                               :sr sr))))

(defmethod! sound-cut ((s om-sound-data) (beg integer) (end integer))
    (sound-cut s (* beg 0.001) (* end 0.001))) 

(defmethod! sound-cut ((s sound) beg end)
            ;(declare (type fixnum beg end))
            (sound-cut (get-om-sound-data s) beg end))



;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-VOL///////////////
(defmethod! sound-vol ((s om-sound-data) gain &optional (in 1) (out 1))
            :icon 106
            :initvals '(nil 1.0 1 1)
            :indoc '("a sound" "a gain value" "fade in duration" "fade out duration")
            "Adds gain effect (volume) on <s>. 

<gain> is a multiplicative factor to the sound sample values.
<in> and <out> determine fade-in / fade-out periods for the gain effect. They can be in seconds (floats, e.g. 0.3) or milliseconds (integer, e.g. 300)."
               (if (null (buffer s))
                (om-beep-msg "Error: null sound buffer")

                 (let* ((nch (nch s))
                   (sr (sr s))
                   (size (size s))
                   (size2 (* size nch))
                   (in (if (integerp in) (* in 0.001) in))
                   (out (if (integerp out) (* out 0.001) out))
                   (fade-in-frames (round (* in sr nch)))
                   (fade-in-factor (/ (1- gain) fade-in-frames))
                   (fade-out-frames (round (* out sr nch)))
                   (fade-out-factor (/ (- 1 gain) fade-out-frames))
                   (fade-out-frame-start (- size2 fade-out-frames))
                   (final-buffer (om-make-pointer size2 :type (smpl-type s) :clear t)))

              ;(declare (type fixnum nch sr size size2 fade-in-frames fade-out-frames fade-out-frames-start))
              ;(declare (type single-float fade-in-factor fade-out-factor))
                  
              (dotimes (i size2)
                (setf (fli:dereference final-buffer :index i)
                      (* (cond ((< i fade-in-frames) (1+ (* fade-in-factor i)))
                               ((>= i fade-out-frame-start) (+ gain (* fade-out-factor (- i (- size2 fade-out-frames)))))
                               (t gain)) 
                         (fli:dereference (buffer s) :index i))))

              (make-instance 'om-sound-data 
                               :buffer final-buffer
                               :size (round size2 nch)
                               :nch nch
                               :sr sr))))

(defmethod! sound-vol ((s sound) gain &optional (in 1) (out 1))
            ;(declare (type single-float gain))
            ;(declare (type fixnum in out))
   (sound-vol (get-om-sound-data s) gain in out))








;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-MONO-TO-STEREO///
(defmethod! sound-mono-to-stereo ((s om-sound-data) &optional (pan 0))
            :icon 111
            :initvals '(nil 0)
            :indoc '("a sound" "a panoramic value between -100 and 100")
            "Stereo-ize a mono sound with possible panoramic <s>.

<pan> is a panoramic value between -100 (Left channel) and 100 (Right channel)."
            
             (cond ((null (buffer s))
                    (om-beep-msg "Error: null sound buffer"))

                   ((= (nch s) 1)
                    (let* ((datatype (smpl-type s))
                           (final-buffer (om-make-pointer (* 2 (size s)) :type datatype :clear t))
                           (pan (/ pan 100.0))
                           (Lgain (if (<= pan 0) 1 (- 1 pan)))
                           (Rgain (if (>= pan 0) 1 (+ 1 pan)))
                           (x 0.0))
                      
                      ;(print (list datatype (buffer s) final-buffer))
                      (dotimes (i (size s))
                        (setf x (fli:dereference (buffer s)  :type datatype :index i))
                        (setf (fli:dereference final-buffer :type datatype :index (* 2 i)) (* Lgain x)
                              (fli:dereference final-buffer :type datatype :index (1+ (* 2 i))) (* Rgain x))
                        )
              
                      ;(fli:free-foreign-object (buffer s))
              
                      (make-instance 'om-sound-data 
                                     :buffer final-buffer
                                     :size (size s)
                                     :nch 2
                                     :sr (sr s))))
                   (t
                    (om-beep-msg (format nil "Error : trying to stereo-ize a sound with ~A channels. Needs 1. Output is the original input." (nch s)))
                    s))
             
             )

(defmethod! sound-mono-to-stereo ((s sound) &optional (pan 0))
            ;(declare (type fixnum pan))
            (sound-mono-to-stereo (get-om-sound-data s) pan))




;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-STEREO-TO-MONO///
(defmethod! sound-stereo-to-mono ((s om-sound-data))
            :icon 112
            :initvals '(nil)
            :indoc '("a sound")
            "Mono-ize a stereo sound."
            (cond ((null (buffer s))
                   (om-beep-msg "Error: null sound buffer"))
                  ((= (nch s) 2)
                   (let* ((final-buffer (om-make-pointer (size s) :type (smpl-type s) :clear t))    
                          (x 0.0))

                  ;(declare (type single-float x))

                     (dotimes (i (size s))
                       (setf x (/ (+ (fli:dereference (buffer s) :index (* 2 i)) (fli:dereference (buffer s) :index (1+ (* 2 i)))) 2.0))
                       (setf (fli:dereference final-buffer :index i) x))
              
                     ;(fli:free-foreign-object (buffer s))
              
                     (make-instance 'om-sound-data 
                                    :buffer final-buffer
                                    :size (size s)
                                    :nch 1
                                    :sr (sr s))))
                  (t
                   (om-beep-msg (format nil "Error : trying to mono-ize a sound with ~A channels. Needs 2. Output is the original input." (nch s)))
                   s)))

(defmethod! sound-stereo-to-mono ((s sound))
  (sound-stereo-to-mono (get-om-sound-data s)))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-PAN//////////////
(defmethod! sound-stereo-pan ((s om-sound-data) left right)
            :icon 113
            :initvals '(nil -100 100)
            :indoc '("a sound" "a left channel pan value" "a right channel pan value")
            "Pan a stereo sound.

<left> is a panoramic value for the left channel between -100 (full left) and 100 (full right).
<right> is a panoramic value for the right channel between -100 (full left) and 100 (full right)."

            (cond ((null (buffer s))
                   (om-beep-msg "Error: null sound buffer"))
                  ((= (nch s) 2)
                   (let* ((left (cond ((< left -100) 100) ((> left 100) -100) (t (- left))))
                          (right (cond ((< right -100) -100) ((> right 100) 100) (t right)))
                          (leftRgain (- 0.5 (* (/ 1.0 200) left)))
                          (leftLgain (+ 0.5 (* (/ 1.0 200) left)))
                          (rightRgain (+ 0.5 (* (/ 1.0 200) right)))
                          (rightLgain (- 0.5 (* (/ 1.0 200) right)))
                          (xl 0.0) (xr 0.0)
                          (b2 (om-make-pointer (* (nch s) (size s)) :type (smpl-type s) :clear t)))

                  ;(declare (type fixnum left right))
                  ;(declare (type single-float leftRgain leftLgain rightRgain rightLgain))

                     (dotimes (i (size s))
                       (setf xl (fli:dereference (buffer s) :index (* 2 i))
                             xr (fli:dereference (buffer s) :index (1+ (* 2 i))))
                       (setf (fli:dereference b2 :index (* 2 i)) (+ (* leftLgain xl) (* rightLgain xr))
                             (fli:dereference b2 :index (1+ (* 2 i))) (+ (* leftRgain xl) (* rightRgain xr))))
                     (make-instance 'om-sound-data 
                                    :buffer b2
                                    :size (size s)
                                    :nch (nch s)
                                    :sr (sr s))))
                  (t
                   (om-beep-msg (format nil "Error : trying to pan a sound with ~A channels. Needs 2. Output is the original input." (nch s)))
                   s)))

(defmethod! sound-stereo-pan ((s sound) left right)
 ;(declare (type fixnum left right))
 (sound-stereo-pan (get-om-sound-data s) left right))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-MIX///////////////
(defmethod! sound-mix ((s1 om-sound-data) (s2 om-sound-data) &optional (method 0))
            :icon 101
            :initvals '(nil nil 0)
            :menuins '((2 (("Sum" 0)
                           ("Sum / Average" 1)
                           ("Sum / Hard Limiting" 2))))
            :indoc '("an om-sound-data" "an om-sound-data" "a mixing method")
            :doc "Generates a mix of <s1> and <s2>."
            
            (cond ((or (null (buffer s1)) (null (buffer s2)))
                   (om-beep-msg "Error : buffer(s) not initialized."))
                  ((and (= (nch s1) (nch s2)) (= (sr s1) (sr s2)))
                   (let* ((nch (nch s1))
                       (size1 (* nch (size s1)))
                       (size2 (* nch (size s2)))
                       (final-size (max size1 size2))
                       (final-buffer (om-make-pointer final-size :type (smpl-type s1) :clear t))
                       (res 0.0))

                  ;(declare (type fixnum size1 size2 final-size))
                  ;(declare (type single-float res))
                  
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

                  ;(fli:free-foreign-object (buffer s1))
                  ;(fli:free-foreign-object (buffer s2))

                  (make-instance 'om-sound-data 
                                 :buffer final-buffer
                                 :size (round final-size nch)
                                 :nch nch
                                 :sr (sr s1)))
                   )
                  (t
                   (om-beep-msg "Error : trying to mix 2 sounds with different number of channels or different sample rate. Output is the input 1.")
                   s1)))


(defmethod! sound-mix ((s1 sound) (s2 sound) &optional (method 0))
            ;(declare (type fixnum method))
            (sound-mix (get-om-sound-data s1) (get-om-sound-data s2) method))

(defmethod! sound-mix ((s1 om-sound-data) (s2 sound) &optional (method 0))
            ;(declare (type fixnum method))
            (sound-mix s1 (get-om-sound-data s2) method))

(defmethod! sound-mix ((s1 sound) (s2 om-sound-data) &optional (method 0))
            ;(declare (type fixnum method))
            (sound-mix (get-om-sound-data s1) s2 method))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-SEQ///////////////
(defmethod! sound-seq ((s1 om-sound-data) (s2 om-sound-data) &optional (crossfade 0))
            :icon 100
            :initvals '(nil nil 0)
            :indoc '("a sound" "a sound" "cross-fading duration (ms)")
            "Concatenates <s1> and <s2>. 
<crossfade> (duration in seconds/flots or milliseconds/int) determines a fade-in/fade out overlapping between the sounds."
            (cond ((or (null (buffer s1)) (null (buffer s2)))
                   (om-beep-msg "Error : buffer(s) not initialized."))
                  ((and (= (nch s1) (nch s2)) (= (sr s1) (sr s2)))
                   (let* ((nch (nch s1))
                          (sr (sr s1))
                          (size1 (* nch (size s1)))
                          (size2 (* nch (size s2)))
                          (cf (if (integerp crossfade) (* crossfade 0.001) crossfade))
                          (smp-cross (round (* nch cf sr)))
                          (smp-cross-side (round (* nch (/ cf 2.0) sr)))
                          (factor1 (- (/ 1.0 (max 1 smp-cross))))
                          (factor2 (/ 1.0 (max 1 smp-cross)))
                          (final-size (- (+ size1 size2) smp-cross))
                          (final-buffer (om-make-pointer final-size :type (smpl-type s1) :clear t)))
                  ;(declare (type fixnum nch sr size1 size2 smp-cross-side smp-cross-side final-size))
                  ;(declare (type single-float factor1 factor2))

                     (dotimes (i final-size)
                       (setf (fli:dereference final-buffer :index i)
                             (cond ((< i (- size1 smp-cross)) 
                                    (fli:dereference (buffer s1) :index i))
                                   ((and (>= i (- size1 smp-cross)) (<= i size1)) 
                                    (+ (* (1+ (* factor1 (- i (- size1 smp-cross)))) (fli:dereference (buffer s1) :index i))
                                       (* factor2 (- i (- size1 smp-cross)) (fli:dereference (buffer s2) :index (+ smp-cross (- i size1))))))
                                   ((> i size1) 
                                    (fli:dereference (buffer s2) :index (+ smp-cross (- i size1)))))))
                                    
                     (make-instance 'om-sound-data 
                                    :buffer final-buffer
                                    :size (round final-size nch)
                                    :nch nch
                                    :sr (sr s1))))
                  (t
                   (om-beep-msg "Error : trying to sequence 2 sounds with different number of channels or different sample-rate. Output is input 1.")
                   s1)))


(defmethod! sound-seq ((s1 om-sound-data) (s2 sound) &optional (crossfade 0))
            ;(declare (type fixnum crossfade))
            (sound-seq s1 (get-om-sound-data s2) crossfade))

(defmethod! sound-seq ((s1 sound) (s2 om-sound-data) &optional (crossfade 0))
            ;(declare (type fixnum crossfade))
            (sound-seq (get-om-sound-data s1) s2 crossfade))

(defmethod! sound-seq ((s1 sound) (s2 sound) &optional (crossfade 0))
            ;(declare (type fixnum crossfade))
            (sound-seq (get-om-sound-data s1) (get-om-sound-data s2) crossfade))


;//////////////////////////////////////////////////////////////////////////////////////////////////OM-SOUND-REVERSE///////////////
(defmethod! sound-reverse ((s om-sound-data))
            :icon 115
            :initvals '(nil)
            :indoc '("a sound")
            "Reverse a sound."

            (if (null (buffer s))
                (om-beep-msg "Error: null sound buffer"))
            (let* ((size (* (nch s) (size s)))
                   (out-buffer (om-make-pointer size :type (smpl-type s) :clear t)))
              (dotimes (i size)
                (setf (fli:dereference out-buffer :index i) 
                      (fli:dereference (buffer s) :index (- size i 1))))

              (make-instance 'om-sound-data
                             :buffer out-buffer
                             :size (size s)
                             :nch (nch s)
                             :sr (sr s))))
                  

(defmethod! sound-reverse ((s sound))
            (sound-reverse (get-om-sound-data s)))



