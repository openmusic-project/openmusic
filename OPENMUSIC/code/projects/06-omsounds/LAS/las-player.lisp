;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE
;;;===========================================

(in-package :om)

;;;==================================================================================================================================================================
;;;================================================================NEW AUDIO ARCHITECTURE============================================================================
;;;==================================================================================================================================================================

;The new audio architecture is made of two LAS players :
;         -One called *audio-player-visible*, which is used when the user wants a track system, with one sound on one track etc...
;         -One called *audio-player-hidden*, which is used when the user just wants to play sounds without going through a track system.
;The *audio-player-hidden* has a track system, but it's internally managed and it's impossible to apply effects or anything else on these tracks. They are "hidden".



(defvar *audio-player-visible* nil)
(defvar *audio-player-hidden* nil)

;Global player context state (to use in maquette, when the two players are sync)
(defvar *audio-context-state* "Idle")

;Two hash tables which contain informations (list sound state) on each track of each player
(defvar *audio-player-hidden-tracks-info* (make-hash-table))
(defvar *audio-player-visible-tracks-info* (make-hash-table))

;A hash table which contains pointers to numbers from 0 to (las-channels -1).
;It is used for the callback functions, which require pointers to numbers, and not numbers directly.
(defvar *channel-numbers-hash-table* (make-hash-table))

;Constants to use to create players.
(defconstant las-inchan 0)
(defconstant las-outchan 2)
(defconstant las-channels 32)
(defparameter las-srate 44100)
(defconstant las-buffsize 512)
(defconstant las-streambuffsize 65536)
(defconstant las-instreamduration (* las-srate 600))
(defconstant las-renderer #+macosx las::kCoreAudioRenderer #-macosx las::kPortAudioRenderer)
(defconstant las-thread 1)

;;; VARIABLE FROM OM AUDIO MIXER
(setf *audio-n-channels* las-channels)

;Define callbacks when channels stop
(cffi:defcallback channel-stop-callback-hidden :void ((chan :pointer))
  (let* ((status-list *audio-player-hidden-tracks-info*)
         (snd (car (gethash (cffi::mem-aref chan :int) status-list))))
    (las-stop snd)))

(cffi:defcallback channel-stop-callback-visible :void ((chan :pointer))
  (let* ((status-list *audio-player-visible-tracks-info*)
         (snd (car (gethash (cffi::mem-aref chan :int) status-list))))
    (las-stop snd (+ (cffi::mem-aref chan :int) 1))))

;===============================================================================================================================================================
;============================================================================ API ==============================================================================
;===============================================================================================================================================================

(defun las-load-library (&optional from-path)
  (setf las::*libaudiostream* nil)
  (setf las::*libaudiostream-pathname* (or from-path las::*libaudiostream-pathname*))
  (las::libaudiostream-framework))

(defun las-init-full-system ()
  (instanciate-players)
  (start-global-audio-context))

; (las-close-full-system)
; (las-init-full-system)


(defun las-close-full-system ()
  (destroy-global-audio-context))

(defun las-play-all-players ()
  (play-full-audio-context))
(defun las-pause-all-players ()
  (pause-full-audio-context))
(defun las-stop-all-players ()
  (stop-full-audio-context))
(defun las-cont-all-players ()
  (cont-full-audio-context))


(defun las-play (obj &optional from to track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-play object from to track))
    (om-smart-play obj from to track)))

(defun las-loop-play (obj &optional track)
  (om-smart-loop-play obj track))

(defun las-pause (obj &optional track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-pause object track))
    (om-smart-pause obj track)))

(defun las-stop (obj &optional track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-stop object (tracknum obj)))
    (om-smart-stop obj track)))


(defun las-play/stop (obj &optional track)
  (print "Play/Stop function TODO"))

(defun las-switch-sound-las-player (sound kind)
  (cond ((= kind 1) (setf (assoc-player sound) *audio-player-visible*))
        ((= kind 0) (setf (assoc-player sound) *audio-player-hidden*))
        (t (print "WARNING : LAS couldn't set your sound associated player info properly (wrong argument)"))))

(defun las-change-channel-pan-visible (channel pan)
  (change-channel-pan-visible (- channel 1) pan))

(defun las-change-channel-vol-visible (channel vol)
  (change-channel-vol-visible (- channel 1) vol))


(defun las-get-length-sound (pointer)
  (las::getlengthsound pointer))

(defun las-slice-sample-cut (pointer from to)
  (las::makecutsound pointer (max 0 from) (min (las::getlengthsound pointer) to)))

(defun las-slice-seq (pointer1 pointer2 crossfade)
  (las::makeseqsound pointer1 pointer2 crossfade))

(defun las-make-stereo-sound (pointer)
  (las::makestereosound pointer))

;===============================================================================================================================================================
;===========================================================SOUND UTILS======================================================================
;===============================================================================================================================================================

;;; all these slots still needed ?
(defclass las-player-sound ()  
   (
    (filename :accessor filename :initform nil :initarg :filename)
    (ref :accessor ref :initform nil :initarg :ref)
    
    (loaded :accessor loaded :initform nil :initarg :loaded)

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
    (las-using-srate :accessor las-using-srate :initform 0))
   )


(defmethod las-player-sound-p ((self las-player-sound)) t)
(defmethod las-player-sound-p ((self t)) nil)

(defmethod las-fill-sound-info ((self las-player-sound))
  (let* ((las-ptr (las::makereadsound (namestring (filename self))))
         (size (las::getlengthsound las-ptr)))
    (setf (sndlasptr self) las-ptr
          (sndlasptr-current self) las-ptr
          (sndlasptr-current-save self) las-ptr
          (sndlasptr-to-play self) las-ptr
          (number-of-samples-current self) size
          (number-of-samples-to-play self) size
          (snd-slice-to-paste self) nil
          (loaded self) t)))

(defmethod initialize-instance :after ((self las-player-sound) &rest initargs)
  (setf (assoc-player self) *audio-player-hidden*)
  self)


(defmethod las-sound-tracknum-sys ((self las-player-sound))
   (when (or (loaded self) (las-fill-sound-info self))
    (tracknum-sys self)))

(defmethod las-sound-sndlasptr ((self las-player-sound))
   (when (or (loaded self) (las-fill-sound-info self))
    (sndlasptr self)))

(defmethod las-sound-sndlasptr-current ((self las-player-sound))
   (when (or (loaded self) (las-fill-sound-info self))
    (sndlasptr-current self)))

(defmethod las-sound-n-samples-current ((self las-player-sound))
  (when (or (loaded self) (las-fill-sound-info self))
    (number-of-samples-current self)))

(defmethod las-sound-sndlasptr-to-play ((self las-player-sound))
  (when (or (loaded self) (las-fill-sound-info self))
    (sndlasptr-to-play self)))

(defmethod las-sound-set-sndlasptr-to-play ((self las-player-sound) ptr)
    (setf (sndlasptr-to-play self) ptr))

(defmethod las-sound-n-samples-to-play ((self las-player-sound))
  (when (or (loaded self) (las-fill-sound-info self))
    (number-of-samples-to-play self)))

(defmethod las-sound-snd-slice-to-paste ((self las-player-sound))
   (when (or (loaded self) (las-fill-sound-info self))
    (snd-slice-to-paste self)))

(defmethod las-sound-update-sndlasptr-current ((self las-player-sound) pointer)
  (setf (sndlasptr-current self) pointer))

(defmethod las-sound-update-snd-slice-to-paste ((self las-player-sound) pointer)
  (setf (snd-slice-to-paste self) pointer))

(defmethod las-sound-update-las-infos ((self las-player-sound))
  (setf (number-of-samples-current self) (las-get-length-sound (sndlasptr-current self)))
  (setf (number-of-samples-to-play self) (las-get-length-sound (sndlasptr-to-play self))))

(defmethod las-sound-las-slicing-past-stack ((self las-player-sound))
  (las-slicing-past-stack self))

(defmethod las-sound-las-slicing-future-stack ((self las-player-sound))
  (las-slicing-future-stack self))

(defmethod las-sound-las-using-srate-? ((self las-player-sound))
  (if (= 0 (las-using-srate self))
      nil
    t))

(defmethod las-sound-las-using-srate ((self las-player-sound))
  (setf (las-using-srate self) 1))


;===============================================================================================================================================================
;===========================================================GLOBAL AUDIO CONTEXT FUNCTIONS======================================================================
;===============================================================================================================================================================

;////////////////////////////////////////Players Management////////////////////////////////////////////////////
;/MAKE NEW PLAYER FUCNTION
;Returns a LAS player pointer
(defun make-new-player ()
  (las::OpenAudioPlayer las-inchan las-outchan las-channels las-srate las-buffsize las-streambuffsize las-instreamduration las-renderer las-thread))

(defun las-set-sample-rate (sr)
  (setf las-srate sr)
  (las-close-full-system)
  (las-init-full-system))
  
;/INSTANCIATE PLAYERS FUCNTION
;Bind both *audio-player-visible* and *audio-player-hidden* with LAS player pointers, init players infos
(defun instanciate-players ()
  (progn
    (setf *audio-player-visible* (make-new-player))
    (setf *audio-player-hidden* (make-new-player))
    (loop for i from 0 to (- las-channels 1) do
          (setf (gethash i *audio-player-hidden-tracks-info*) (list nil "Idle" 1.0 1.0 0.0))
          (setf (gethash i *audio-player-visible-tracks-info*) (list nil "Idle" 1.0 1.0 0.0))
          (setf (gethash i *channel-numbers-hash-table*) (cffi::foreign-alloc :int :initial-element i)))
    nil))


;/START AUDIO CONTEXT FUNCTION
;Start both *audio-player-visible* and *audio-player-hidden* players, affect callbacks to channels
(defun start-global-audio-context ()
  (if (and *audio-player-visible* *audio-player-hidden*)
      (progn
        (las::StartAudioPlayer *audio-player-visible*)
        (las::StartAudioPlayer *audio-player-hidden*)
        (loop for i from 0 to (- las-channels 1) do
              (las::SetStopCallbackChannel *audio-player-hidden* i (cffi:callback channel-stop-callback-hidden) (gethash i *channel-numbers-hash-table*))
              (las::SetStopCallbackChannel *audio-player-visible* i (cffi:callback channel-stop-callback-visible) (gethash i *channel-numbers-hash-table*)))
        "Audio is ready")
    (print "WARNING : Audio context can't be started because there is no instanciated player")))

;/DESTROY AUDIO CONTEXT FUNCTION
;Close and delete both *audio-player-visible* and *audio-player-hidden*, init players infos
(defun destroy-global-audio-context ()
  (when (and *audio-player-visible* *audio-player-hidden*)
    (las::CloseAudioPlayer *audio-player-visible*)
    (las::CloseAudioPlayer *audio-player-hidden*)
    (setf *audio-player-visible* nil)
    (setf *audio-player-hidden* nil)
    (loop for i from 0 to (- las-channels 1) do
          (setf (gethash i *audio-player-hidden-tracks-info*) (list nil "Idle"))
          (setf (gethash i *audio-player-visible-tracks-info*) (list nil "Idle")))))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////


;////////////////////////////////////////Players Tools/////////////////////////////////////////////////////////
;/PLAY FULL AUDIO CONTEXT FUNCTION
;Tool that plays both players, by playing all channels using loops (not really effective...)
(defun play-full-audio-context ()
  (play-full-player *audio-player-visible*)
  (play-full-player *audio-player-hidden*))

;/STOP FULL AUDIO CONTEXT FUNCTION
;Tool that stops both players, by stopping all channels using loops (not really effective...)
(defun stop-full-audio-context ()
  (stop-full-player *audio-player-visible*)
  (stop-full-player *audio-player-hidden*))

;/STOP FULL AUDIO CONTEXT FUNCTION
;Tool that pause both players, by pausing all channels using loops (not really effective...)
(defun pause-full-audio-context ()
  (pause-full-player *audio-player-visible*)
  (pause-full-player *audio-player-hidden*))

;/CONT FULL AUDIO CONTEXT FUNCTION
;Tool that continues both players, by stopping all channels using loops (not really effective...)
(defun cont-full-audio-context ()
  (cont-full-player *audio-player-visible*)
  (cont-full-player *audio-player-hidden*))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////


;////////////////////////////////////////Players Transport/////////////////////////////////////////////////////
;/PLAY GLOBAL AUDIO CONTEXT FUNCTION
;Chooses to do nothing if it's already playing, play if it's idle or continue if it's paused
(defun play-global-audio-context ()
  (cond ((string-equal *audio-context-state* "Idle") 
         (let () 
           (play-full-audio-context)
           (setf *audio-context-state* "Playing")))
        ((string-equal *audio-context-state* "Paused")
         (let ()
           (cont-full-audio-context)
           (setf *audio-context-state* "Playing")))
        (t nil)))

;/PAUSE GLOBAL AUDIO CONTEXT FUNCTION
;Chooses to do nothing if it's already paused or idle, pause if it's playing
(defun pause-global-audio-context ()
  (cond ((string-equal *audio-context-state* "Playing") 
         (let () 
           (stop-full-audio-context)
           (setf *audio-context-state* "Paused")))
        (t nil)))

;/STOP GLOBAL AUDIO CONTEXT
;Stops in every cases.
(defun stop-global-audio-context ()
  (let ()
    (stop-full-player *audio-player-visible*)
    (stop-full-player *audio-player-hidden*)
    (setf *audio-context-state* "Idle")))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////


;===============================================================================================================================================================
;===============================================================SINGLE PLAYER UTILITIES=========================================================================
;===============================================================================================================================================================

;////////////////////////////////////////Player Transport//////////////////////////////////////////////////////
;/PLAY FULL PLAYER FUNCTION
;Play all tracks of a player using a loop (not really effective...)
(defun play-full-player (player)
  (loop for i from 0 to (- las-channels 1) do
        (play-one-channel player i)))

;/CONT FULL PLAYER FUNCTION
;Cont all tracks of a player using a loop (not really effective...)
(defun cont-full-player (player)
  (loop for i from 0 to (- las-channels 1) do
        (cont-one-channel player i)))

;/STOP FULL PLAYER FUNCTION
;Stop all tracks of a player using a loop (not really effective...)
(defun stop-full-player (player)
  (loop for i from 0 to (- las-channels 1) do
        (stop-one-channel player i)))

;/PAUSE FULL PLAYER FUNCTION
;Pause all tracks of a player using a loop (not really effective...)
(defun pause-full-player (player)
  (loop for i from 0 to (- las-channels 1) do
        (pause-one-channel player i)))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////


;//////////////////////////////////////////Player Tools////////////////////////////////////////////////////////
;/EMPTY ONE PLAYER FUNCTION
;Tool that loads a single sample null sound to all tracks using a loop.
(defun empty-one-player (player)
  (let ((nullsnd (las::MakeNullSound 1)))
    (loop for i from 0 to (- las-channels 1) do
        (las::LoadChannel player nullsnd i 1.0 0.5 0.5))))

;/GET FREE CHANNEL FUNCTION
;Tool that find a free channel (which state is IDLE, no matter if a sound is loaded) starting from 0.
;Returns the first encountered free channel (as an int).
(defun get-free-channel (player)
  (let ((i 0)
        (status "init")
        (status-list (if (eq player *audio-player-hidden*)
                         *audio-player-hidden-tracks-info*
                       *audio-player-visible-tracks-info*)))
    (loop while (not (string-equal status "Idle")) do
          (progn
            (setf status (cadr (gethash i status-list)))
            (setf freetrack i)
            (incf i)
            ))
    freetrack))

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////


;////////////////////////////////////////Channel Transport/////////////////////////////////////////////////////
;/PLAY ONE CHANNEL FUNCTION
;Tool that starts one channel and binds the correct channel state to the appropriate status list.
(defun play-one-channel (player channel)
  (let ()
    (las::StartChannel player channel)
    (if (eq player *audio-player-hidden*)
        (setf (cadr (gethash channel *audio-player-hidden-tracks-info*)) "Playing")
      (setf (cadr (gethash channel *audio-player-visible-tracks-info*)) "Playing"))))

;/CONT ONE CHANNEL FUNCTION
;Tool that continues one channel and binds the correct channel state to the appropriate status list.
(defun cont-one-channel (player channel)
  (let ()
    (las::ContChannel player channel)
    (if (eq player *audio-player-hidden*)
        (setf (cadr (gethash channel *audio-player-hidden-tracks-info*)) "Playing")
      (setf (cadr (gethash channel *audio-player-visible-tracks-info*)) "Playing"))))

;/STOP ONE CHANNEL FUNCTION
;Tool that stops one channel and binds the correct channel state to the appropriate status list.
(defun stop-one-channel (player channel)
  (let ()
    (las::StopChannel player channel)
    (if (eq player *audio-player-hidden*)
        (setf (cadr (gethash channel *audio-player-hidden-tracks-info*)) "Idle")
      (setf (cadr (gethash channel *audio-player-visible-tracks-info*)) "Idle"))))

;/PAUSE ONE CHANNEL FUNCTION
;Tool that pauses one channel and binds the correct channel state to the appropriate status list.
(defun pause-one-channel (player channel)
  (let ()
    (las::StopChannel player channel)
    (if (eq player *audio-player-hidden*)
        (setf (cadr (gethash channel *audio-player-hidden-tracks-info*)) "Paused")
      (setf (cadr (gethash channel *audio-player-visible-tracks-info*)) "Paused"))))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////


;///////////////////////////////////////////Channel Tools//////////////////////////////////////////////////////
;/CHANGE CHANNEL VOL
;Tool that change the volume of a channel
(defun change-channel-vol-visible (channel vol)
  (las::SetVolChannel *audio-player-visible* channel vol)
  (setf (nth 2 (gethash channel *audio-player-visible-tracks-info*)) vol))


; L = 1.0
; R = 0.0
(defun pan2panpan (pan)
  ;(print pan)
  (if (>= pan 0.5) 
      ;; Left
      (list 1.0 (* 2.0 (- pan 0.5)))
    ;; Right
    (list (* pan 2.0) 0.0)))

;/CHANGE CHANNEL PAN
;Tool that change the pan of a channel
(defun change-channel-pan-visible (channel pan)
  (let* ((status-list *audio-player-visible-tracks-info*)
         (snd (car (gethash channel status-list)))
         (nchnls 1)
         (pan2 (pan2panpan pan)))
    (if snd
        (setf nchnls (las::GetChannelsSound (sndlasptr-current (player-data snd)))))
    (case nchnls 
      (1 (las::SetPanChannel *audio-player-visible* channel pan pan))
      (2 (las::SetPanChannel *audio-player-visible* channel (car pan2) (cadr pan2)))
      (otherwise nil))
    (setf (nth 3 (gethash channel *audio-player-visible-tracks-info*)) (car pan2))
    (setf (nth 4 (gethash channel *audio-player-visible-tracks-info*)) (cadr pan2))))

;/EMPTY ONE CHANNEL FUNCTION
;Tool that load a single sample null sound to a track.
(defun empty-one-channel (player channel)
  (las::LoadChannel player (las::MakeNullSound 1) channel 1.0 0.5 0.5)
  (if (eq player *audio-player-hidden*)
      (setf (car (gethash channel *audio-player-hidden-tracks-info*)) nil)
    (setf (car (gethash channel *audio-player-visible-tracks-info*)) nil)))

;/LOAD SOUND ON ONE CHANNEL FUNCTION
;Tool that loads a sound (his sndlasptr-to-play) to a track, and update the appropriate status list.
(defun load-sound-on-one-channel (player snd tracknum &optional (vol 1.0) (panLeft 1.0) (panRight 0.0))
  (let ((ptr (sndlasptr-to-play (om::player-data snd))) 
        (status-list nil)
        (vol 1.0)
        (panL 1.0)
        (panR 0.0))
    (if (eq player *audio-player-visible*)
        (setf status-list *audio-player-visible-tracks-info*)
      (setf status-list *audio-player-hidden-tracks-info*))
    (setf vol (nth 2 (gethash tracknum status-list)))
    (setf panL (nth 3 (gethash tracknum status-list)))
    (setf panR (nth 4 (gethash tracknum status-list)))
    (las::LoadChannel player ptr tracknum vol panL panR)
    (setf (car (gethash tracknum status-list)) snd)))

;/GET CHANNEL STATUS FUNCTION
;Tools that get the current status of a channel.
;Returns the status of the channel, as an int (2 : playing, 0 : paused or idle).
(defun get-channel-status (player channel)
  (let ((status -1)
        (infolist nil))
    (setf infolist 
          (cffi::with-foreign-object (chan-info 'las::TChannelInfo)
            (las::GetInfoChannel player channel chan-info)
            (cffi::with-foreign-slots ((las::fStatus las::fCurFrame) chan-info las::TChannelInfo)
              (list las::fStatus las::fCurFrame))))
    (setf status (car infolist))
    status))

;/GET CHANNEL CURRENT FRAME FUNCTION
;Tools that get the current frame of a channel.
;Returns the current frame of the channel, as an int.
(defun get-channel-curframe (player channel)
  (let ((curframe -1)
        (infolist nil))
    (setf infolist 
          (cffi::with-foreign-object (chan-info 'las::TChannelInfo)
            (las::GetInfoChannel player channel chan-info)
            (cffi::with-foreign-slots ((las::fStatus las::fCurFrame) chan-info las::TChannelInfo)
              (list las::fStatus las::fCurFrame))))
    (setf curframe (cadr infolist))
    curframe))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////

;===============================================================================================================================================================
;=================================================================SMART TRANSPORT SYSTEM========================================================================
;===============================================================================================================================================================


;;; INTERNAL

;/SMART PLAY STOP FUNCTION
;This function decides to play or stop a sound according to his current state.
(defun om-smart-play/stop (sound &optional track)
  (let* ((chan (if (and track (> 0 track)) 
                  track
                (tracknum-sys (om::player-data sound))))
        (status-list (if (and track (> 0 track))
                         *audio-player-visible-tracks-info*
                       *audio-player-hidden-tracks-info*))) 
    (if (eq sound (car (gethash chan status-list)))
        (if (string-equal "Playing" (cadr (gethash chan status-list)))
            (om-smart-stop sound track)
          (om-smart-play sound nil nil track))
     (om-smart-play sound nil nil track))))


;/SMART PLAY FUNCTION
;This function makes the choice to call the right play function (hidden or visible)
;It also checks if there's a selection to play, or if it has to play the song straight ahead.
(defun om-smart-play (sound &optional from to track)
  (if (and track (> track 0))
      (om-smart-play-visible sound (- track 1))
    (om-smart-play-hidden sound)))

;/SMART LOOP PLAY FUNCTION
;This function makes the choice to call the right play function (hidden or visible)
;It doesn't mdify the sndlasptr-to-play of the sound, so it loops on it.
(defun om-smart-loop-play (sound &optional track)
  (if (and track (> track 0))
        (om-smart-play-visible sound (- track 1))
    (om-smart-play-hidden sound)))

;/SMART PAUSE FUNCTION
;This function makes the choice to call the right pause function (hidden or visible)
(defun om-smart-pause (sound &optional track)
  (if (sndlasptr-current (om::player-data sound))
      (if (and track (> track 0))
          (om-smart-pause-visible sound (- track 1))
        (om-smart-pause-hidden sound))))


;/SMART STOP FUNCTION
;This function makes the choice to call the right stop function (hidden or visible)
(defun om-smart-stop (sound &optional track)
  (when (om::player-data sound)
    (let ((chan (if (and track (> 0 track)) track
                (tracknum-sys (om::player-data sound))))
        (status-list (if (and track (> 0 track))
                         *audio-player-visible-tracks-info*
                       *audio-player-hidden-tracks-info*)))
    (if (sndlasptr-current (om::player-data sound))
        (if (and track (> track 0))
            (om-smart-stop-visible sound (- track 1))
          (om-smart-stop-hidden sound))))))




;/PLAY FUNCTION FOR HIDDEN PLAYER
;This function works based on a little system that checks if the sound is already loaded :
;           -if yes it uses a basic transport system
;           -if not it assigns it to the first available track
;           -if yes but since it was idle its track was allocated to an other sound, it assigns it to the first available track
(defun om-smart-play-hidden (snd)
  (let* ((actual-track (tracknum-sys (om::player-data snd))))
    (if (/= actual-track -1)
        (if (eq snd (car (gethash actual-track *audio-player-hidden-tracks-info*)))
           (cond ((string-equal "Idle" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
                  (let () 
                    (load-sound-on-one-channel *audio-player-hidden* snd actual-track)
                    (play-one-channel *audio-player-hidden* actual-track)))
                 ((string-equal "Paused" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
                  (cont-one-channel *audio-player-hidden* actual-track))
                 ((string-equal "Playing" (cadr (gethash actual-track *audio-player-hidden-tracks-info*))) nil))
          (if (string-equal "Idle" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
              (let ()
                (load-sound-on-one-channel *audio-player-hidden* snd actual-track)
                (play-one-channel *audio-player-hidden* actual-track))
            (let ((chan (get-free-channel *audio-player-hidden*)))
              (setf (tracknum-sys (om::player-data snd)) chan)
              (load-sound-on-one-channel *audio-player-hidden* snd chan)
              (play-one-channel *audio-player-hidden* chan))
            ))
      (let* ((chan (get-free-channel *audio-player-hidden*)))
        (if (< chan las-channels)
            (progn
              (setf (tracknum-sys (om::player-data snd)) chan)
              (load-sound-on-one-channel *audio-player-hidden* snd chan)
              (play-one-channel *audio-player-hidden* chan))
            (om-message-dialog (format nil "Oops! It seems that you reached the system limit. Too many songs are playing at the same time.~%~%Note : You can play up to ~D songs with no track assignation at the same time." (- las-channels 1))))))))

;/PLAY FUNCTION FOR VISIBLE PLAYER
;This function works based on a little system that checks if the sound is already loaded :
;           -if yes it uses a basic transport system
;           -if not it tries to load it in the user selected track :
;                       -if the selected track is empty, it fills it with the sound
;                       -if the selected track is already filled but Idle, the system allows the replacement
;                       -if the selected track is already filled but Play or Paused, the system forbid the replacement and notice the user.
(defun om-smart-play-visible (snd &optional (tracknum 0))
  (let* ((actual-track tracknum))
    (if (eq snd (car (gethash actual-track *audio-player-visible-tracks-info*)))
        (cond ((string-equal "Idle" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
               (load-sound-on-one-channel *audio-player-visible* snd actual-track)
               (play-one-channel *audio-player-visible* actual-track))
              ((string-equal "Paused" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
               (cont-one-channel *audio-player-visible* actual-track))
              ((string-equal "Playing" (cadr (gethash actual-track *audio-player-visible-tracks-info*))) nil))
      (cond ((string-equal "Idle" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
             (let ()
               (load-sound-on-one-channel *audio-player-visible* snd actual-track)
               (play-one-channel *audio-player-visible* actual-track)))
            ((string-equal "Paused" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
             (print "WARNING : A sound seems to be paused on this channel. Stop it first or please select a new track"))
            ((string-equal "Playing" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
             (print "WARNING : A sound seems to be playing on this channel. Stop it first or please select a new track"))))))

;/PAUSE FUNCTION FOR HIDDEN PLAYER
;This function is a basic pause function which works only if the sound is playing. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-pause-hidden (snd)
  (let ((actual-track (tracknum-sys (om::player-data snd)))) 
    (if (eq snd (car (gethash actual-track *audio-player-hidden-tracks-info*)))
        (if (string-equal "Playing" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
            (pause-one-channel *audio-player-hidden* actual-track)))))

;/PAUSE FUNCTION FOR VISIBLE PLAYER
;This function is a basic pause function which works only if the sound is playing. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-pause-visible (snd &optional (tracknum 0))
  (if (eq snd (car (gethash tracknum *audio-player-visible-tracks-info*)))
      (if (string-equal "Playing" (cadr (gethash tracknum *audio-player-visible-tracks-info*)))
          (pause-one-channel *audio-player-visible* tracknum))))

;/STOP FUNCTION FOR HIDDEN PLAYER
;This function is a basic stop function. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-stop-hidden (snd &optional synth)
  (let ((actual-track (tracknum-sys (om::player-data snd))))
    (if (eq snd (car (gethash actual-track *audio-player-hidden-tracks-info*)))
        (stop-one-channel *audio-player-hidden* actual-track))))

;/STOP FUNCTION FOR VISIBLE PLAYER
;This function is a basic stop function. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-stop-visible (snd &optional (tracknum 0))
  (if (eq snd (car (gethash tracknum *audio-player-visible-tracks-info*)))
      (stop-one-channel *audio-player-visible* tracknum)))

;/USE ORIGINAL SOUND
;This functions switch between the orginal stream and the modified stream
(defun om-use-original-sound (snd)
  (if (or (= 0 (current-is-original (om::player-data snd))) (= -1 (current-is-original (om::player-data snd))))
      (let ()
        (setf (sndlasptr-current-save (om::player-data snd)) (sndlasptr-current (om::player-data snd)))
        (setf (sndlasptr-current (om::player-data snd)) (sndlasptr (om::player-data snd)))
        (setf (current-is-original (om::player-data snd)) 1))
    (let ()
      (setf (sndlasptr-current (om::player-data snd)) (sndlasptr-current-save (om::player-data snd)))
      (setf (current-is-original (om::player-data snd)) 0))))



;;;===========================================================================================================================================================
;;;===========================================================================================================================================================
;;;===========================================================================================================================================================
;;;===========================================================================================================================================================


(defmethod player-name ((self (eql :libaudiostream))) "LibAudioStream")
(defmethod player-desc ((self (eql :libaudiostream))) "internal OM Player")

(add-player-for-object 'sound :libaudiostream)

(defun libaudiostream-open ()
 (if (las-load-library (om-lib-pathname las::*libaudiostream-pathname*))
     (progn 
       (las-init-full-system)
       (enable-player :libaudiostream))
   (om-message-dialog (format nil (om-str :lib-error) "LibAudioStream"))))

(defun libaudiostream-close ()
  (las-close-full-system)
  (disable-player :libaudiostream))

(om-add-init-func 'libaudiostream-open)  
(om-add-exit-cleanup-func 'libaudiostream-close t)



(defmethod prepare-to-play ((engine (eql :libaudiostream)) (player omplayer) object at interval params)
  (when (loaded object)
  (let* ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at))
         (from (car newinterval))
         (to (cadr newinterval))
         newptr)
    (setf (player-data object)
          (make-instance 'las-player-sound :filename (om-sound-file-name object)))
    (if (and (or (null interval) newinterval) (las-sound-sndlasptr-current (player-data object)))
        (progn
          (setf newptr (if (> (om-sound-n-channels object) 1) 
                           (las-sound-sndlasptr-current (player-data object)) 
                         (las-make-stereo-sound (las-sound-sndlasptr-current (player-data object)))))
          (if (or from to)
              (let ((begin (if from (round (* from (/ las-srate 1000.0)))))
                    (end (if to (round (* to (/ las-srate 1000.0)))))
                    (max (las-sound-n-samples-current (player-data object))))
                (if (and begin (or (< begin 0) (not begin)))
                    (setf begin 0))
                (if (and end (or (> end max) (not end)))
                    (setf end max))
                (las-sound-set-sndlasptr-to-play (player-data object) (las-slice-sample-cut newptr begin end)))
            (las-sound-set-sndlasptr-to-play (player-data object) newptr))
          (las-sound-update-las-infos (player-data object))
          (call-next-method engine player object at newinterval params))))))


(defmethod player-start ((engine (eql :libaudiostream)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-pause-object engine (nth i play-list)))
    (las-pause-all-players)))

;;; CONTINUE
(defmethod player-continue ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-continue-object engine (nth i play-list)))
    (las-cont-all-players)))

;;; STOP
(defmethod player-stop ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-stop-object engine (nth i play-list)))
    (las-stop-all-players)))


;;; PLAY (NOW)
(defmethod player-play-object ((engine (eql :libaudiostream)) (object sound) &key interval params)
  (las-play object (car interval) (cadr interval) (tracknum object)))

(defmethod player-loop ((self (eql :libaudiostream)) player &optional play-list)
  (declare (ignore player))
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (let ((thesound (nth i play-list)))
              (las-stop thesound (tracknum thesound))
              (las-loop-play thesound (tracknum thesound))))))

;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-pause object (tracknum object)))

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; STOP ONLY ONE OBJECT
(defmethod player-stop-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-stop object (tracknum object)))

;(defclass las-player (omplayer) 
;  ((sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play))
;  ())

;;; TODO
;;; called when a box or editor attached to player is removed/closed
(defmethod player-cleanup ((player (eql :libaudiostream)) snd)
  (let* ((status-list (if (= (tracknum snd) 0)
                          *audio-player-hidden-tracks-info*
                        *audio-player-visible-tracks-info*))
         (chan (if (eq player *audio-player-hidden*)
                   (tracknum-sys (player-data snd))
                 (tracknum snd)))
         (loadedsnd (car (gethash chan status-list)))
         (status (cadr (gethash chan status-list))))
    (if (eq snd loadedsnd)
        (let () 
           (if (= (tracknum snd) 0)
               (las-stop snd)
             (las-stop snd (tracknum snd)))
          (setf (car (gethash chan status-list)) nil)))))


;;; creates the player-specific controls on the sound editor control panel
(defmethod make-player-specific-controls ((self (eql :libaudiostream)) control-view)
  (let* ((snd (object (editor control-view)))
         (track (tracknum snd)))
    (list 
     (om-make-dialog-item 'om-static-text (om-make-point 420 8)
                          (om-make-point 40 20) "Track"
                          :font *om-default-font1*)
     (om-make-dialog-item 'numBox
                          (om-make-point 480 8)
                          (om-make-point 60 18) (if (> track 0) (format () " ~D" track) "no track")
                          :min-val 0 :max-val 32
                          :font *om-default-font1*
                          :bg-color *om-white-color*
                          :fg-color (if (> track 0) *om-black-color* *om-gray-color*)
                          :value track
                          :afterfun #'(lambda (item)
                                        (if (/= (tracknum snd) (value item))
                                            (progn
                                              (general-stop (player (editor control-view)))
                                              (setf (tracknum snd) (value item))
                                              (om-set-fg-color item (if (> (value item) 0) *om-black-color* *om-gray-color*))
                                              (om-set-dialog-item-text item (if (> (value item) 0) (format () " ~D" (value item)) "no track"))
                                              (when (player-data snd)
                                                (if (> (value item) 0) 
                                                    (las-switch-sound-las-player (player-data snd) 1) 
                                                  (las-switch-sound-las-player (player-data snd) 0)))
                                              (report-modifications (editor control-view)))))))))


;;;===========================================

(defmethod player-change-channel-vol ((player (eql :libaudiostream)) channel value)
  (las-change-channel-vol-visible channel value))

(defmethod player-change-channel-pan ((player (eql :libaudiostream)) channel value) 
  (las-change-channel-pan-visible channel value))