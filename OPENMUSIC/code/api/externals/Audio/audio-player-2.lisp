;===============================================================================================================================================================
;=================================================================SMART TRANSPORT SYSTEM========================================================================
;===============================================================================================================================================================
(in-package :oa)


;;;////////////////////"API"////////////////////
(export '(
          las-play
          las-pause
          las-stop
          las-play/stop ;en cours
          las-switch-sound-las-player
          ) :om-api)


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
;;;/////////////////////////////////////////////

;;;;////////////////////////////////////////////////////////////////////////////////////////////////////

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
