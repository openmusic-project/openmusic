
(in-package :las)

;;;  (libaudiostream-framework)

(libversion)

;;;=======================
;;; SoundFiles
;;;=======================

(defparameter soundfile1 "/Users/bresson/Dropbox/workspaces/om-acmmm/in-files/guitar-extract.aif")
(defparameter soundfile2 "/Users/bresson/WORKSPACES/aaaa/in-files/africa.aiff")
(defparameter soundfile3 "/Users/bresson/Desktop/Bassclarinet1.aif")

(defparameter soundfile1 "C:\\Users\\Jean Bresson\\Desktop\\ccc.aif")
(defparameter soundfile2 "C:\\Users\\Jean Bresson\\Desktop\\audio-problematik\\farinelli.aif")
(defparameter soundfile3 "C:\\Users\\Jean Bresson\\Desktop\\accord.wav")

;;;=======================
;;; Multi-channel Player
;;;=======================

;; Input channels
;; Output channels
;; Number of channels
;; Sample rate
;; Buffer size
;; Disk buffer size
;; Disk buffer size for real-time stream

;;(setq player (OpenAudioPlayer 0 0 32 44100 1024 65536 65536 kPortAudioRenderer 1))

; (las::OpenAudioPlayer 0 2 32 44100 512 65536 26460000 2 1)

;; WARNING !! when using Jack, the Sample rate and Buffer size values much match the values currently used with Jack server
 
;(setq player (OpenAudioPlayer 2 2 32 48000 512 65536 26460000 kCoreAudioRenderer 1))

(CloseAudioPlayer player)


;; Start Audio engine execution
;;==============================

(StartAudioPlayer player)
(StopAudioPlayer player)


;; Stream creation using files
;;=============================

(setq s1 (MakeReadSound soundfile1))
(setq s2 (MakeReadSound soundfile2))
(setq s3 (MakeReadSound soundfile3))

(LoadChannel player s1 1 1.0 1.0 1.0)
(LoadChannel player s2 2 1.0 1.0 1.0)
(LoadChannel player s3 3 1.0 1.0 0.0)


(StartChannel player 1)
(StartChannel player 2)
(StartChannel player 3)

(GetLengthSound s2)
(GetChannelsSound s2)

(GetLengthSound s1)
(GetChannelsSound s1)

(setq s3 (MakeLoopSound (MakeRegionSound soundfile1 400000 450000) 50))

(GetLengthSound s3)
(GetChannelsSound s3)

(setq s4 (MakeWriteSound "out.aiff" 
                          (MakeLoopSound 
                           (MakeFadeSound
                           (MakeRegionSound soundfile1 400000 450000)
                           200 200)
                           50) 
                          (logior SF_FORMAT_AIFF SF_FORMAT_PCM_16)))

;; Real-time input (audio thru)
;;==============================

(setq s5 (MakeInputSound))


;; Record real-time input
;;========================

(setq s6 (MakeWriteSound "input.aiff" s5 
                         (logior SF_FORMAT_AIFF SF_FORMAT_PCM_16)))

(LoadChannel player s5 1 1.0 1.0 1.0)
(LoadChannel player s6 2 1.0 1.0 0.0)
(StartChannel player 2)
(StopChannel player 2)

;; When played, files based streams are read "asynchronously" (using an aditionnal feeder thread)
;; To access a stream "synchronously", it has to be wrapped by a "MakeRendererSound" construct. 
;;===============================================================================================


(setq s10 (MakeRendererSound (MakeReadSound soundfile1)))
(GetLengthSound s10)
(GetChannelsSound s10)

(defparameter buffer_size 512)
(defvar buffer (cffi::%foreign-alloc (* 4  buffer_size (GetChannelsSound s10))))

(ReadSound s10 buffer buffer_size (GetChannelsSound s10))

(setf ss (makestereosound s1))

(progn 
(let* ((sndformat (or format *def-snd-format*))
         (res (case *audio-res*
                (8 las::SF_FORMAT_PCM_S8)
                (16 las::SF_FORMAT_PCM_16)
                (24 las::SF_FORMAT_PCM_24)
                (32 las::SF_FORMAT_PCM_32)              
                (otherwise las::SF_FORMAT_PCM_16)))
         (setq sndr (las::MakeRendererSound (las::MakeWriteSound (om-path2cmdpath filename) snd 
                                                          (if (equal sndformat 'aiff)
                                                            (logior las::SF_FORMAT_AIFF res)
                                                            (logior las::SF_FORMAT_WAV res))))))
    (setf buffer-size 512)
    ;(setf buffer (om-make-pointer (* 4 buffer-size (las::GetChannelsSound sndr)) t))
    (setf buffer (om-make-pointer (* 4 buffer-size 2)  :clear t))
    (setf res 512)
    (las::ResetSound sndr)
    (loop while (= res 512) do
          ;(setf res (las::ReadSound sndr buffer buffer-size (las::GetChannelsSound sndr)))
          (setf res (las::ReadSound sndr buffer buffer-size 2))
          )
    (om-free-pointer buffer)
    ))

;; Load audio player channels
;;============================

(LoadChannel player ss 1 120 64 64)
(LoadChannel player s2 2 120 64 64)

(LoadChannel player s3 3 120 64 64)
(LoadChannel player s4 4 120 64 64)

(LoadChannel player s5 5 120 64 64)
(LoadChannel player s6 6 120 64 64)


;; Channels can be started/stopped/continued individually 
;;========================================================

;; Start channels
;;================

(StartChannel player 1)
(StartChannel player 2)
(StartChannel player 3)
(StartChannel player 4)
(StartChannel player 5)
(StartChannel player 6)

;; Stop channels
;;================

(StopChannel player 1)
(StopSound player 2)
(StopSound player 3)
(StopSound player 4)
(StopSound player 5)
(StopSound player 6)

;; Continue channels
;;====================

(ContSound player 1)
(ContSound player 2)
(ContSound player 3)
(ContSound player 4)
(ContSound player 5)
(ContSound player 6)


;;==================================================================================
;; NOTE : To start all channels simultanously in a perfectly synchronous manner, 
;; the Audio engine has to be stopped (using StopAudioPlayer), then channels set in 
;; play mode (using StartSound) and StartAudioPlayer will start all channels at 
;; the same time.
;; The same thing can be done to "resume" a set of channels: use 
;; StopAudioPlayer/StartAudioPlayer to stop/resume.
;;==================================================================================


;; Set channel volume (0 127)
;;============================

(SetVolSound player 1 80)
(SetVolSound player 1 50)

;; Set channel pan (0 127)
;;============================

(SetPanSound player 1 80)

;; Set audio player volume (0 127)
;;================================

(SetVolAudioPlayer player 80)

;; Set audio player pan  (0 127)
;;================================

(SetPanAudioPlayer player 64)

;; Streams not used anymore in channels MUST be deleted (typically before loading a new stream in a channel)
;;==========================================================================================================

(deletesound s6)


;; Stop and close
;;==================

(StopAudioPlayer player)

(CloseAudioPlayer player)



#|
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
             (buffer (om-make-pointer (* 4 buffer-size numchannels) :clear t)))
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
|#


