;;;==================================================================================================================================================================
;;;================================================================NEW AUDIO ARCHITECTURE============================================================================
;;;==================================================================================================================================================================

;The new audio architecture is made of two LAS players :
;         -One called *audio-player-visible*, which is used when the user wants a track system, with one sound on one track etc...
;         -One called *audio-player-hidden*, which is used when the user just wants to play sounds without going through a track system.
;The *audio-player-hidden* has a track system, but it's internally managed and it's impossible to apply effects or anything else on these tracks. They are "hidden".

(in-package :oa)

(export '(
          las-play
          las-pause
          las-stop
          las-play/stop ;en cours
          las-switch-sound-las-player
          las-srate
          las-channels
          ) :om-api)

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
(defconstant las-srate 44100)
(defconstant las-buffsize 512)
(defconstant las-streambuffsize 65536)
(defconstant las-instreambuffsize 65536)
(defconstant las-renderer las::kCoreAudioRenderer)
(defconstant las-thread 1)


;Define callbacks when channels stop
(cffi:defcallback channel-stop-callback-hidden :void ((chan :pointer))
  (let* ((status-list *audio-player-hidden-tracks-info*)
         (snd (car (gethash (cffi::mem-aref chan :int) status-list))))
    (las-stop snd)))

(cffi:defcallback channel-stop-callback-visible :void ((chan :pointer))
  (let* ((status-list *audio-player-visible-tracks-info*)
         (snd (car (gethash (cffi::mem-aref chan :int) status-list))))
    (las-stop snd (cffi::mem-aref chan :int))))

;======================================================
;===         CREATE 32 EMPTY EFFECTS LISTS          ===
;======================================================
(defvar *effects-lists* nil)

(defun plug-faust-effect-list-on-channel (player effectlist channel &optional (fadein 100) (fadeout 100))
  (las::SetEffectListChannel player channel effectlist fadein fadeout))

(defun ResetEffectsLists (player)
  (setf *effects-lists* (make-hash-table))
  (loop for i from 0 to 31 do 
      (setf (gethash i *effects-lists*) (las::MakeAudioEffectList))
      (plug-faust-effect-list-on-channel player (gethash i *effects-lists*) i)))


;===============================================================================================================================================================
;============================================================================ API ==============================================================================
;===============================================================================================================================================================

(defun las-play (obj &optional from to track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-play object from to track))
    (om-smart-play obj from to track)))

(defun las-pause (obj &optional track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-pause object track))
    (om-smart-pause obj track)))

(defun las-stop (obj &optional track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-stop object track))
    (om-smart-stop obj track)))

(defun las-play/stop (obj &optional track)
  (if (listp obj)
      (loop for object in obj do
          (om-smart-play/stop object from to track))
    (om-smart-play/stop obj from to track)))

(defun las-switch-sound-las-player (sound kind)
  (cond ((= kind 1) (setf (assoc-player sound) *audio-player-visible*))
        ((= kind 0) (setf (assoc-player sound) *audio-player-hidden*))
        (t (print "LAS couldn't set your sound associated player info properly (wrong argument)"))))


;===============================================================================================================================================================
;===========================================================GLOBAL AUDIO CONTEXT FUNCTIONS======================================================================
;===============================================================================================================================================================

;////////////////////////////////////////Players Management////////////////////////////////////////////////////
;/MAKE NEW PLAYER FUCNTION
;Returns a LAS player pointer
(defun make-new-player ()
  (las::OpenAudioPlayer las-inchan las-outchan las-channels las-srate las-buffsize las-streambuffsize las-instreambuffsize las-renderer las-thread))

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
        (ResetEffectsLists *audio-player-visible*)
        (loop for i from 0 to (- las-channels 1) do
              (las::SetStopCallbackChannel *audio-player-hidden* i (cffi:callback channel-stop-callback-hidden) (gethash i *channel-numbers-hash-table*))
              (las::SetStopCallbackChannel *audio-player-visible* i (cffi:callback channel-stop-callback-visible) (gethash i *channel-numbers-hash-table*)))
        "Audio is ready")
    (print "Audio context can't be started because there is no instanciated player")))

;/DESTROY AUDIO CONTEXT FUNCTION
;Close and delete both *audio-player-visible* and *audio-player-hidden*, init players infos
(defun destroy-global-audio-context ()
  (let ()
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
    (while (not (string-equal status "Idle"))
      (setf status (cadr (gethash i status-list)))
      (setf freetrack i)
      (incf i)
      )
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
(defun change-channel-vol (player channel vol)
  (let ((status-list (if (eq player *audio-player-visible*)
                          *audio-player-visible-tracks-info*
                        *audio-player-hidden-tracks-info*)))
    (las::SetVolChannel player channel vol)
    (setf (nth 2 (gethash channel status-list)) vol)
    ))

;/CHANGE CHANNEL PAN
;Tool that change the pan of a channel
(defun change-channel-pan (player channel pan)
  (let* ((status-list (if (eq player *audio-player-visible*)
                          *audio-player-visible-tracks-info*
                        *audio-player-hidden-tracks-info*))
         (snd (car (gethash channel status-list)))
         (nchnls 1)
         (pan2 (pan2panpan pan)))
    (if snd
        (setf nchnls (las::GetChannelsSound (sndlasptr-current snd))
              ))
    (case nchnls 
      (1 (las::SetPanChannel player channel pan pan))
      (2 (las::SetPanChannel player channel (car pan2) (cadr pan2)))
      (otherwise nil))
    (setf (nth 3 (gethash channel status-list)) (car pan2))
    (setf (nth 4 (gethash channel status-list)) (cadr pan2))
    ))

;/EMPTY ONE CHANNEL FUNCTION
;Tool that load a single sample null sound to a track.
(defun empty-one-channel (player channel)
  (let ((nullsnd (las::MakeNullSound 1)))
    (las::LoadChannel player nullsnd channel 1.0 0.5 0.5)
    (if (eq player *audio-player-hidden*)
        (setf (car (gethash channel *audio-player-hidden-tracks-info*)) nil)
      (setf (car (gethash channel *audio-player-visible-tracks-info*)) nil))))

;/LOAD SOUND ON ONE CHANNEL FUNCTION
;Tool that loads a sound (his sndlasptr-to-play) to a track, and update the appropriate status list.
(defun load-sound-on-one-channel (player snd tracknum &optional (vol 1.0) (panLeft 1.0) (panRight 0.0))
  (let ((ptr (oa::sndlasptr-to-play snd))
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

;/SMART PLAY STOP FUNCTION
;This function decides to play or stop a sound according to his current state.
(defun om-smart-play/stop (sound &optional track)
  (let ((chan (if (and track (> 0 track)) 
                  track
                (tracknum-sys sound)))
        (status-list (if (and track (> 0 track))
                         *audio-player-visible-tracks-info*
                       *audio-player-hidden-tracks-info*)))
    (if (eq sound (car (gethash chan status-list)))
        (if (string-equal "Playing" (cadr (gethash chan status-list)))
            (om-smart-stop sound track)
          (om-smart-play sound track)
      (om-smart-play sound)))))

;/SEND TO TRACK FUNCTION
;This function stops the sound and changes the associated player.
;(defun om-send-to-track (snd)
;  (if (assoc-player snd)
;      (let () (om-smart-stop snd)
;        (if (eq (assoc-player snd) *audio-player-hidden*)
;            (setf (assoc-player snd) *audio-player-visible*)
;          (setf (assoc-player snd) *audio-player-hidden*)))))

;/SMART PLAY FUNCTION
;This function makes the choice to call the right play function (hidden or visible)
;It also checks if there's a selection to play, or if it has to play the song straight ahead.
(defun om-smart-play (sound &optional from to track)
  (if (sndlasptr-current sound)
        (let ()
          (if (or from to)
              (let ((begin (if from (round (* from (/ las-srate 1000.0)))))
                    (end (if to (round (* to (/ las-srate 1000.0)))))
                    (max (number-of-samples-current sound)))
                (if (or (< begin 0) (not begin))
                    (setf begin 0))
                (if (or (> end max) (not end))
                    (setf end max))
                (setf (sndlasptr-to-play sound) (las::MakeCutSound (sndlasptr-current sound) begin end)))
            (setf (sndlasptr-to-play sound) (sndlasptr-current sound)))
          (om-update-sound-las-infos sound)
          (if (and track (> track 0))
              (om-smart-play-visible sound track)
          (om-smart-play-hidden sound)))))

;/SMART PAUSE FUNCTION
;This function makes the choice to call the right pause function (hidden or visible)
(defun om-smart-pause (sound &optional track)
    (if (sndlasptr-current sound)
        (if (and track (> track 0))
            (om-smart-pause-visible sound track)
          (om-smart-pause-hidden sound))))


;/SMART STOP FUNCTION
;This function makes the choice to call the right stop function (hidden or visible)
(defun om-smart-stop (sound &optional track)
    (if (sndlasptr-current sound)
        (if (and track (> track 0))
            (om-smart-stop-visible sound track)
          (om-smart-stop-hidden sound))))

;/PLAY FUNCTION FOR HIDDEN PLAYER
;This function works based on a little system that checks if the sound is already loaded :
;           -if yes it uses a basic transport system
;           -if not it assigns it to the first available track
;           -if yes but since it was idle its track was allocated to an other sound, it assigns it to the first available track
(defun om-smart-play-hidden (snd)
  (let* ((actual-track (tracknum-sys snd))
         (player *audio-player-hidden*))
    (if (/= actual-track -1)
        (if (eq snd (car (gethash actual-track *audio-player-hidden-tracks-info*)))
           (cond ((string-equal "Idle" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
                  (let () 
                    (load-sound-on-one-channel player snd actual-track)
                    (play-one-channel player actual-track)))
                 ((string-equal "Paused" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
                  (cont-one-channel player actual-track))
                 ((string-equal "Playing" (cadr (gethash actual-track *audio-player-hidden-tracks-info*))) nil))
          (if (string-equal "Idle" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
              (let ()
                (load-sound-on-one-channel player snd actual-track)
                (play-one-channel player actual-track))
            (let ((chan (get-free-channel player)))
              (setf (tracknum-sys snd) chan)
              (load-sound-on-one-channel player snd chan)
              (play-one-channel player chan))
            ))
      (let* ((chan (get-free-channel player)))
        (setf (tracknum-sys snd) chan)
        (load-sound-on-one-channel player snd chan)
        (play-one-channel player chan)))))



;/PLAY FUNCTION FOR VISIBLE PLAYER
;This function works based on a little system that checks if the sound is already loaded :
;           -if yes it uses a basic transport system
;           -if not it tries to load it in the user selected track :
;                       -if the selected track is empty, it fills it with the sound
;                       -if the selected track is already filled but Idle, the system allows the replacement
;                       -if the selected track is already filled but Play or Paused, the system forbid the replacement and notice the user.
(defun om-smart-play-visible (snd &optional (tracknum 0))
  (let* ((actual-track tracknum)
         (player *audio-player-visible*)) 
    (if (eq snd (car (gethash actual-track *audio-player-visible-tracks-info*)))
        (cond ((string-equal "Idle" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
               (load-sound-on-one-channel player snd actual-track)
               (play-one-channel player actual-track))
              ((string-equal "Paused" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
               (cont-one-channel player actual-track))
              ((string-equal "Playing" (cadr (gethash actual-track *audio-player-visible-tracks-info*))) nil))
      (cond ((string-equal "Idle" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
             (let ()
               (load-sound-on-one-channel player snd actual-track)
               (play-one-channel player actual-track)))
            ((string-equal "Paused" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
             (print "A sound seems to be paused on this channel. Stop it first or please select a new track"))
            ((string-equal "Playing" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
             (print "A sound seems to be playing on this channel. Stop it first or please select a new track"))))))

;/PAUSE FUNCTION FOR HIDDEN PLAYER
;This function is a basic pause function which works only if the sound is playing. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-pause-hidden (snd)
  (let ((actual-track (tracknum-sys snd))
        (player *audio-player-hidden*)) 
    (if (eq snd (car (gethash actual-track *audio-player-hidden-tracks-info*)))
        (if (string-equal "Playing" (cadr (gethash actual-track *audio-player-hidden-tracks-info*)))
            (pause-one-channel player actual-track)))))

;/PAUSE FUNCTION FOR VISIBLE PLAYER
;This function is a basic pause function which works only if the sound is playing. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-pause-visible (snd &optional (tracknum 0))
  (let ((actual-track tracknum)
        (player *audio-player-visible*))
    (if (eq snd (car (gethash actual-track *audio-player-visible-tracks-info*)))
        (if (string-equal "Playing" (cadr (gethash actual-track *audio-player-visible-tracks-info*)))
            (pause-one-channel player actual-track)))))

;/STOP FUNCTION FOR HIDDEN PLAYER
;This function is a basic stop function. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-stop-hidden (snd)
  (let ((actual-track (tracknum-sys snd))
        (player *audio-player-hidden*)) 
    (if (eq snd (car (gethash actual-track *audio-player-hidden-tracks-info*)))
        (let ()
          (stop-one-channel player actual-track)
          (load-sound-on-one-channel player snd actual-track)))))

;/STOP FUNCTION FOR VISIBLE PLAYER
;This function is a basic stop function. It also check if the channel of the sound is well loaded with it to avoid issues.
(defun om-smart-stop-visible (snd &optional (tracknum 0))
  (let ((actual-track tracknum)
        (player *audio-player-visible*)) 
    (if (eq snd (car (gethash actual-track *audio-player-visible-tracks-info*)))
        (let ()
          (stop-one-channel player actual-track)
          (load-sound-on-one-channel player snd actual-track)))))

;/USE ORIGINAL SOUND
;This functions switch between the orginal stream and the modified stream
(defun om-use-original-sound (snd)
  ;(let ((snd (om::object (om-view-container sndpanel))))
    (if (or (= 0 (current-is-original snd)) (= -1 (current-is-original snd)))
        (let ()
          (setf (sndlasptr-current-save snd) (sndlasptr-current snd))
          (setf (sndlasptr-current snd) (sndlasptr snd))
          (setf (current-is-original snd) 1))
      (let ()
          (setf (sndlasptr-current snd) (sndlasptr-current-save snd))
          (setf (current-is-original snd) 0))))