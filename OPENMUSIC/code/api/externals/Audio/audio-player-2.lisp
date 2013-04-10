

(in-package :oa)

(defun new-audio-open ()
  (let ()
    (instanciate-players)
    (start-global-audio-context)))

(defun new-audio-close ()
  (destroy-global-audio-context))

(defun new-audio-reset ()
  (let ()
    (destroy-global-audio-context)
    (instanciate-players)
    (start-global-audio-context)))


(om-add-init-func 'new-audio-open)
(om-add-exit-cleanup-func 'new-audio-close t)


;===============================================================================================================================================================
;=================================================================SMART TRANSPORT SYSTEM========================================================================
;===============================================================================================================================================================

;/SMART PLAY STOP LIST FUNCTION
;This function take as argument a list of sound objects and call smart-play-stop for each one of these.
(defun om-smart-play-stop-list (list)
    (loop for snd in list do
          (om-smart-play-stop snd)))

;/SMART PLAY STOP FUNCTION
;This function decides to play or stop a sound according to his current state.
(defun om-smart-play-stop (snd &optional (sndpanel nil))
  (let* ((player (assoc-player snd))
         (chan (if (eq player *audio-player-hidden*)
                   (tracknum-sys snd)
                 (om::tracknum snd)))
         (status-list (if (eq player *audio-player-hidden*)
                          *audio-player-hidden-tracks-info*
                        *audio-player-visible-tracks-info*)))
    (if (eq snd (car (gethash chan status-list)))
        (if (string-equal "Playing" (cadr (gethash chan status-list)))
            (if sndpanel (om-smart-stop snd sndpanel) (om-smart-stop snd))
          (if sndpanel (om-smart-play snd sndpanel) (om-smart-play snd)))
      (if sndpanel (om-smart-play snd sndpanel) (om-smart-play snd)))))

;/SEND TO TRACK FUNCTION
;This function stops the sound and changes the associated player.
(defun om-send-to-track (sndpanel)
  (let ((snd (om::object (om-view-container sndpanel))))
    (if (assoc-player snd)
        (let () (om-smart-stop snd sndpanel)
          (if (eq (assoc-player snd) *audio-player-hidden*)
              (setf (assoc-player snd) *audio-player-visible*)
            (setf (assoc-player snd) *audio-player-hidden*))))))

;/SMART PLAY FUNCTION
;This function makes the choice to call the right play function (hidden or visible)
;It also checks if there's a selection to play, or if it has to play the song straight ahead.
(defun om-smart-play (sound &optional (interval nil))
  (let ((snd sound))
    (if sndpanel (setf snd (om::object (om-view-container sndpanel))))
    (if (sndlasptr-current snd)
        (let ()
          (if (and sndpanel (om::selection-to-play-? sndpanel))
              (let* ((interval (caddr (om::get-selection-to-play sndpanel)))
                     (begin-time (car interval))
                     (end-time (cadr interval))
                     (nch (number-of-channels snd))
                     (nsmp (number-of-samples snd))
                     (sr (sample-rate snd))
                     (srdiv (* (/ sr srate) 1.0))
                     (begin (round (* begin-time (/ srate 1000.0))))
                     (end (round (* end-time (/ srate 1000.0)))))
                (if (> end (las::GetLengthSound (sndlasptr-current snd)))
                    (setf end (las::GetLengthSound (sndlasptr-current snd))))
                (setf (sndlasptr-to-play snd) (las::MakeCutSound (sndlasptr-current snd) begin end))
                (om-update-sound-las-infos snd))
            (let ()
              (setf (sndlasptr-to-play snd) (sndlasptr-current snd))
              (om-update-sound-las-infos snd)))
          (if (eq (assoc-player snd) *audio-player-hidden*)
              (om-smart-play-hidden snd)
            (om-smart-play-visible snd))))))

;/SMART PAUSE FUNCTION
;This function makes the choice to call the right pause function (hidden or visible)
(defun om-smart-pause (sound &optional (sndpanel nil))
  (let ((snd sound))
    (if sndpanel (setf snd (om::object (om-view-container sndpanel))))
    (if (sndlasptr-current snd)
        (if (eq (assoc-player snd) *audio-player-hidden*)
            (om-smart-pause-hidden snd)
          (om-smart-pause-visible snd)))))


;/SMART STOP FUNCTION
;This function makes the choice to call the right stop function (hidden or visible)
;It also reset any selection in the sound to play.
(defun om-smart-stop (snd &optional (sndpanel nil))
  (let ((sound snd))
    (if sndpanel (setf sound (om::object (om-view-container sndpanel))))
    (if (sndlasptr-current sound)
        (if (eq (assoc-player sound) *audio-player-hidden*)
            (let () 
              (if sndpanel 
                  (let () 
                    (setf (om::cursor-pos sndpanel) 0)
                    (setf (om::cursor-interval sndpanel) (list 0 0))
                    (om::om-invalidate-view sndpanel)))
              (om-smart-stop-hidden sound))
          (let () 
            (if sndpanel 
                  (let () 
                    (setf (om::cursor-pos sndpanel) 0)
                    (setf (om::cursor-interval sndpanel) (list 0 0))
                    (om::om-invalidate-view sndpanel)))
            (om-smart-stop-visible sound))))))

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
(defun om-smart-play-visible (snd)
  (let* ((actual-track (om::tracknum snd))
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
(defun om-smart-pause-visible (snd)
  (let ((actual-track (om::tracknum snd))
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
(defun om-smart-stop-visible (snd)
  (let ((actual-track (om::tracknum snd))
        (player *audio-player-visible*)) 
    (if (eq snd (car (gethash actual-track *audio-player-visible-tracks-info*)))
        (let ()
          (stop-one-channel player actual-track)
          (load-sound-on-one-channel player snd actual-track)))))

;/USE ORIGINAL SOUND
;This functions switch between the orginal stream and the modified stream
(defun om-use-original-sound (sndpanel)
  (let ((snd (om::object (om-view-container sndpanel))))
    (if (or (= 0 (current-is-original snd)) (= -1 (current-is-original snd)))
        (let ()
          (setf (sndlasptr-current-save snd) (sndlasptr-current snd))
          (setf (sndlasptr-current snd) (sndlasptr snd))
          (setf (current-is-original snd) 1))
      (let ()
          (setf (sndlasptr-current snd) (sndlasptr-current-save snd))
          (setf (current-is-original snd) 0)))))
