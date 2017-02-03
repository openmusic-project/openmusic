(in-package :om)

; (defparameter *refnum* (ms::midiopen "Common Lisp"))
; (ms::MidiClose *refnum*)

(enable-player :midi-player)

;;; NEW MIDI PLAYER (NOT YET AVAILABLE)                 
(defmethod player-name ((player (eql :midi-player))) "OM MIDI player")   ;;; A short name
(defmethod player-desc ((player (eql :midi-player))) "Default MIDI system")   ;;; a description
(defmethod player-special-action ((player (eql :midi-player))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midi-player))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midi-player))) :midi)   ;;; communication protocol (:midi / :udp)


(defmethod prepare-to-play ((engine (eql :midi-player)) (player omplayer) object at interval params)
  ;(print (format nil "~s" params))
  
  (let ((approx (if (find :approx params)
                    (nth (1+ (position :approx params)) params)
                  (if (caller player) (get-edit-param (caller player) 'approx))))
        (port (if (find :port params)
                    (nth (1+ (position :port params)) params)
                (if (caller player) (get-edit-param (caller player) 'outport)))))
    ;(print params)
    ;(print port)
    (if (equal port :default) (setf port *def-midi-out*))
    (mapcar #'(lambda (evt) 
              ;  (call-next-method engine player evt (+ (or (car interval) 0) (om-midi::midi-evt-date evt)) interval params)
                (schedule-task player 
                               #'(lambda () 
                                   ;(print evt)
                                   (player-play-object engine evt :interval interval))
                               (+ (or (car interval) 0) (om-midi::midi-evt-date evt))
                               nil)
                )
            
            
            (append (and (and *midi-microplay* approx (find approx '(4 8) :test '=))
			 ;; (let ((chan-offset (lchan object)))
			 ;;   (microplay-events approx at (get-obj-dur object) port chan-offset))
			 (microplay-events at (get-obj-dur object) port)
			 )
                    (remove nil (flat (PrepareToPlay :midi object at :interval interval :approx approx :port port)))
                    )
            )
    (sort-events player)
    ))

(defmethod player-start ((engine (eql :midi-player)) &optional play-list)
  (midi-start))

(defparameter *key-ons* (make-hash-table :test #'equal))

(defmethod player-stop ((engine (eql :midi-player)) &optional play-list)
  (maphash #'(lambda (k ch)
	       (mapc #'(lambda (note)
			 (midi-send-evt
			  (om-midi:make-midi-evt :type :keyOff
						 :chan (car (last note))
						 :date 0 :ref 0 :port (car note)
						 :fields (list (cadr note) 0))))
		     ch)
	       (remhash k *key-ons*))
	   *key-ons*)
  (midi-stop)
  (if *midi-microplay* (microplay-reset nil engine)))

;; (defmethod player-loop ((self (eql :midi-player)) player &optional play-list) (call-next-method))

;;; PLAY (NOW) 
(defmethod player-play-object ((engine (eql :midi-player)) (object om-midi::midi-evt) &key interval params)
  (declare (ignore interval params))
  ;;(print (format nil "~A : play ~A - ~A" engine object interval))
  ;;(print object)
  (let* ((chan (om-midi::midi-evt-chan object))
	 (key-index (1- chan))
	 (port (om-midi::midi-evt-port object)))
    (when (and *midi-port-modulo-channel* (>= chan 16))
      (incf port (+ (floor key-index 16)))
      (setf (om-midi::midi-evt-port object) port
	    (om-midi::midi-evt-chan object) (mod chan 16)))
    (cond ((or (equal (om-midi::midi-evt-type object) :keyOff)
	       (and (equal (om-midi::midi-evt-type object) :keyOn) (= 0 (cadr (om-midi::midi-evt-fields object)))))
	   (setf (gethash key-index *key-ons*)
		 (delete (list port (car (om-midi::midi-evt-fields object)) chan)
			 (gethash key-index *key-ons*)
			 :test 'equal)))
	  ((equal (om-midi::midi-evt-type object) :keyOn)
	   (pushnew (list port (car (om-midi::midi-evt-fields object)) chan)
		    (gethash key-index *key-ons*) :test 'equal)))
    (midi-send-evt object)))

;;;==============================
;;; MICROTONALITE
;;;==============================

;;; use msb / LSB ?
(defun make-pitchwheel-event (date chan port val)
  (om-midi::make-midi-evt :type :PitchBend
                        :date date
                        :port port 
                        :chan chan 
                        :ref 0
                        :fields val))

;;; variable set by the MIDI preferences
(defvar *midi-microplay* nil)


; (om+ 8192 '(0 1024 2048 3072))
; (* 8192 2)
(defun microplay-reset (port player)
  (let ((send-fun (or (om-midi::send-midi-event-function player)
                      'midi-send-evt))
        (p (or port *def-midi-out*)))    
  (funcall send-fun (make-pitchwheel-event 0 1 p 8192))
  (funcall send-fun (make-pitchwheel-event 0 2 p 8192))
  (funcall send-fun (make-pitchwheel-event 0 3 p 8192))
  (funcall send-fun (make-pitchwheel-event 0 4 p 8192))
  ))

; (microplay-set 0 :portmidi)
; (microplay-reset 0 :portmidi)

(defun microplay-set (port player)
  (let ((send-fun (or (om-midi::send-midi-event-function player)
                      'midi-send-evt))
        (p (or port *def-midi-out*)))    
  (funcall send-fun (make-pitchwheel-event 0 1 p 8192))
  (funcall send-fun (make-pitchwheel-event 0 2 p 9216))
  (funcall send-fun (make-pitchwheel-event 0 3 p 10240))
  (funcall send-fun (make-pitchwheel-event 0 4 p 11264))
  ))


;; jb: temp. remove 
;; this is not working the way it is specified in the preferences (i.e. shift channels 1-2-3-4 to 0,1/8,2/8,3/8)
#|
(defun microplay-events (approx at dur port chan-offset)
  (let ((port (or port *def-midi-out*))
	(offset (car (remove-duplicates (flat chan-offset)))))
    (append (loop
	       repeat (/ approx 2)
	       for bend from 8192 by (/ 8192 approx)
	       for chan from offset
	       collect (make-pitchwheel-event at chan port bend))
	    (loop
	       repeat (/ approx 2)
	       for chan from offset
	       collect (make-pitchwheel-event (+ at dur) chan port 8192)))))
|#

(defun microplay-events (at dur port)
  (let ((port (or port *def-midi-out*)))
    (list (make-pitchwheel-event at 1 port 8192) 
          (make-pitchwheel-event at 2 port 9216) 
          (make-pitchwheel-event at 3 port 10240) 
          (make-pitchwheel-event at 4 port 11264) 
          (make-pitchwheel-event (+ at dur) 1 port 8192) 
          (make-pitchwheel-event (+ at dur) 2 port 8192) 
          (make-pitchwheel-event (+ at dur) 3 port 8192) 
          (make-pitchwheel-event (+ at dur) 4 port 8192))))

