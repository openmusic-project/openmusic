;;===========================================================================
;;FluidSynth API for Common Lisp/CFFI
;;
;;This program is free software; you can redistribute it and/or modify
;;it under the terms of the GNU Lesser General Public License as published by
;;the Free Software Foundation; either version 2.1 of the License, or
;;(at your option) any later version.
;;  
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU Lesser General Public License for more details.
;;  
;;Authors: Anders Vinjar, Karim Haddad

(in-package :cl-fluidsynth)

;; (oa::om-cmd-line "opera /usr/share/doc/fluidsynth-devel-1.1.6/html/index.html &")

(defmacro flassq (item list) `(cdr (assoc ,item ,list :test #'eq)))

(defvar *fl-synths* nil)
(defvar *fluidsynth* nil)
(defvar *fluidsynth-settings* nil)
(defvar *fluidplayer* nil)
(defvar *fluid-midi-player* nil)
(defvar *fluidadriver* nil)
(defvar *soundfont-dir* (or (probe-file "/usr/share/soundfonts/")
			    (probe-file "/usr/share/sounds/sf2/")))

(defvar *soundfont* (concatenate 'string (namestring om::*om-resources-folder*) "online/in-files/merlin.sf2"))

;  (namestring
;   (make-pathname :directory (if *soundfont-dir* (pathname-directory *soundfont-dir*))
;                  :name "FluidR3_GM.sf2")))



(defvar *fluid-midi-driver-settings* nil)


(defparameter *fluidsynths-loaded-p* nil)

(define-condition not-a-soundfont (error)
  ((soundfont :initarg :soundfont-name :reader soundfont-name)
   (synth :initarg :synth :reader fluidsynth-synth))
  (:report (lambda (condition stream)
	     (format stream "could not load soundfont ~S into running synth ~A."
		     (soundfont-name condition)
		     (fluidsynth-synth condition)))))

(defun prompt-for-soundfont ()
  (format t "Enter name of soundfont to load: ")
  #+capi (list (namestring (oa::om-choose-file-dialog :prompt "Enter name of soundfont to load: "
						      :types '("sf2" "*.sf2" "All" "*.*"))))
  #-capi (multiple-value-list (eval (read))))

(defun fluid-load-new-soundfont-aux (synth soundfont)
  (let ((status (fluid_synth_sfload synth soundfont 1)))
    (if (= status FLUID_FAILED)
	(error 'not-a-soundfont :soundfont-name soundfont :synth synth)
	status)))

(defun fluid-load-new-soundfont (&optional (synth *fluidsynth*) (soundfont *soundfont*))
  (restart-case
      (fluid-load-new-soundfont-aux synth soundfont)
    (prompt (new-soundfont)
      :report "Provide soundfont-file to load."
      :interactive prompt-for-soundfont
      (setq soundfont new-soundfont))
    (reload ()
      :report "Retry loading soundfont"
      (fluid-load-new-soundfont synth soundfont)
      t)))




(defun fluid-synth-setup ()
  (unless *fluidsynth*
    (progn
      (setf *fluidsynth-settings* (new_fluid_settings))
      #+linux(fluid_settings_setstr *fluidsynth-settings* "audio.driver"
			     #+cl-jack "jack"
			     #+(and linux (not cl-jack)) "alsa"
			     #+(and cocoa (not cl-jack)) "coreaudio"
                             #+cocoa "coreaudio")
      #+darwin(fluid_settings_setstr *fluidsynth-settings* "audio.driver"
			     #+cl-jack "jack"
                             #+cocoa "coreaudio")
      #+cl-jack (progn
		  (fluid_settings_setint *fluidsynth-settings* "audio.jack.autoconnect" 1)
		  (fluid_settings_setstr *fluidsynth-settings* "audio.jack.id" "OM_fluidsynth"))
      #+(and (or linux darwin) (not cl-jack)) (fluid_settings_setstr *fluidsynth-settings* "audio.alsa.device" "default") ;plughw:0, hw:0,0...
      (setf *fluidsynth* (new_fluid_synth *fluidsynth-settings*))
      (setf *fluidplayer* (new_fluid_player *fluidsynth*))
      (setf *fluidadriver* (new_fluid_audio_driver *fluidsynth-settings* *fluidsynth*))
      (fluid-load-new-soundfont *fluidsynth* *soundfont*)
      (fluid_synth_set_gain *fluidsynth* 0.6)

      ;(fluid_synth_set_chorus_on *fluidsynth* choruson)
     ;(fluid_synth_set_chorus *fluidsynth* 1 0.0d0 0.5d0 3.9d0) ;voir les valeurs

      ;(fluid_synth_set_reverb_on *fluidsynth* revon)
      ;(fluid_synth_set_reverb *fluidsynth* 2.0d0 0.0d0 0.5d0 2.9d0)

      (let ((running? (fluid_player_get_status *fluidplayer*)))
	(or (and (= running? (cffi:foreign-enum-value 'fluid_player_status :FLUID_PLAYER_READY)) running?)
	    (warn "fluid-synth-setup: could not start fluidplayer"))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Load an create N fluid synths 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun setup-fluid-synths (indx)
  (progn
      (setf *fluidsynth-settings* (new_fluid_settings))
      #+linux(fluid_settings_setstr *fluidsynth-settings* "audio.driver"
			     #+cl-jack "jack"
			     #+(and linux (not cl-jack)) "alsa"
			     #+(and cocoa (not cl-jack)) "coreaudio"
                             #+cocoa "coreaudio")
      #+darwin(fluid_settings_setstr *fluidsynth-settings* "audio.driver"
			     #+cl-jack "jack"
                             #+cocoa "coreaudio")
      #+cl-jack (progn
		  (fluid_settings_setint *fluidsynth-settings* "audio.jack.autoconnect" 1)
		  (fluid_settings_setstr *fluidsynth-settings* "audio.jack.id" (format nil "OM_fluidsynth_~A" indx)))
      #+(and (or linux darwin) (not cl-jack)) (fluid_settings_setstr *fluidsynth-settings* "audio.alsa.device" "default") ;plughw:0, hw:0,0...
      (setf (flassq indx *fl-synths*) (new_fluid_synth *fluidsynth-settings*))
      (setf *fluidplayer* (new_fluid_player (flassq indx *fl-synths*)))
      (setf *fluidadriver* (new_fluid_audio_driver *fluidsynth-settings* (flassq indx *fl-synths*)))
      (fluid-load-new-soundfont (flassq indx *fl-synths*) *soundfont*)
      (fluid_synth_set_gain (flassq indx *fl-synths*) 0.6)

      ;(let ((running? (fluid_player_get_status *fluidplayer*)))
	;(or (and (= running? (cffi:foreign-enum-value 'fluid_player_status :FLUID_PLAYER_READY)) running?)
	 ;   (warn "fluid-synth-setup: could not start fluidplayer")))
      )
    )




(defun load-all-fl-synths (num)
  "<num> stands for n created synths by port" 
  (setf *fluidsynths-loaded-p* 't)
  (setf *fl-synths*
      (let ((variables
             (let ((vars
                    (loop for i from 1 to num
                          collect (read-from-string 
                                   (format nil "*OM_fluidsynth_~D*" i)))))
               (mapcar 'eval
                       (loop for i from 1 to num
                             for par in vars
                             collect `(defparameter ,par ,i))))))
        (loop for i from 1 to num
              for v in variables
              collect (cons i v))))
  (loop for i from 1 to num
        do (setup-fluid-synths i))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun fluid-synth-cleanup ()
  (progn
    (delete_fluid_audio_driver *fluidadriver*)
    (delete_fluid_player *fluidplayer*)
    (delete_fluid_synth *fluidsynth*)
    (delete_fluid_settings *fluidsynth-settings*)
    (setf *fluidsynth* nil)))

(defun fluid-synth-restart ()
  (progn
    (fluid-synth-cleanup)
    (fluid-synth-setup)))

;;(fluid_player_get_status *fluidplayer*)

(defun fluid-midi-setup ()
  (unless *fluid-midi-driver-settings*
    (progn
      (setf *fluid-midi-driver-settings* (new_fluid_settings))
      #+cl-jack (progn (fluid_settings_setstr *fluid-midi-driver-settings* "midi.driver" "jack")
		       (fluid_settings_setstr *fluid-midi-driver-settings* "midi.jack.id" "OM_fluidsynth")))))


(defun cl-fluid-setup-fluidsynth ()

  (fluid-synth-setup)
  (fluid-midi-setup)


  (defcallback cl-fluid-handle-midi-event :int
      ((data (:pointer :void))
       (event (:pointer fluid_midi_event_t)))
    (declare (ignore data))    
    (fluid_synth_handle_midi_event *fluidsynth* event)
    ;;(print (format nil "event type: ~X" (fluid_midi_event_get_type event)))
    1)

  (unless *fluid-midi-player*
    (setf *fluid-midi-player* (new_fluid_midi_driver
			       *fluid-midi-driver-settings*
			       (callback cl-fluid-handle-midi-event)
			       (null-pointer))))

  #+cl-jack (cl-jack::jack-connect cl-jack::*CLJackClient*
				   (cl-jack::jack-port-name cl-jack::*jack-midi-output-port*)
				   "OM_fluidsynth:midi")
  )
