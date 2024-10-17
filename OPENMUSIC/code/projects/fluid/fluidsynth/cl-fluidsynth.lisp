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

(defun fluidsynthlib-p ()
  #+linux *fluidsynth-loaded*
  #+(or macosx win32) *fluidsynth-library*
  #+win32 t)

(defmacro flassq (item list) `(cdr (assoc ,item ,list :test #'eq)))

(defclass fl-synth ()
  ((index :accessor index :initarg :index :initform nil)
   (synthname :accessor synthname :initarg :synthname :initform nil)
   (settings :accessor settings :initarg :settings :initform nil)
   (synthptr :accessor synthptr :initarg :synthptr :initform nil)
   (audioptr :accessor audioptr :initarg :audioptr :initform nil)
   (sf2path :accessor sf2path :initarg :sf2path :initform nil)
   (sf2stack :accessor sf2stack :initarg :sf2stack :initform '())
   ))

;necessaire
(defmethod getsptr ((self t)) nil)
(defmethod getsptr ((self fl-synth)) (synthptr self))

(defvar *fl-synths* nil)
(defvar *fluidsynth* nil)
(defvar *fluidsynth-settings* nil)
(defvar *fluidplayer* nil)
(defvar *fluid-midi-player* nil)
(defvar *fluidadriver* nil)
(defvar *soundfont-dir* (or (probe-file "/usr/share/soundfonts/")
			    (probe-file "/usr/share/sounds/sf2/")))

;(defvar *soundfont* (concatenate 'string (namestring om::*om-resources-folder*) "online/in-files/merlin.sf2"))
(defvar *soundfont* nil) ;(namestring om::*fluid-sf2*))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Load an create N fluid synths 


(defmethod create-fl-synt (indx)
  (let* ((synth (make-instance 'fl-synth :index indx))
         (name (format nil "OM_fluidsynth_~A" indx))
         ;(fluidsettings (new_fluid_settings))
         )
    
    (progn
      (setf *fluidsynth-settings* (new_fluid_settings))
      #+linux(if (equal om::*jack-alsa* "alsa")
                 (progn 
                   ;(fluid_settings_setnum *fluidsynth-settings* "synth.sample-rate" (coerce om::*audio-sr* 'double-float))
                   (fluid_settings_setstr *fluidsynth-settings* "audio.alsa.device" "default");om::*audio-out-device*)
                   )
               (progn  
                 (fluid_settings_setnum *fluidsynth-settings* "synth.sample-rate" (coerce om::*audio-sr* 'double-float))
                 (fluid_settings_setstr *fluidsynth-settings* "audio.driver" "jack")
                 (fluid_settings_setstr *fluidsynth-settings* "audio.jack.id" (format nil "OM_fluidsynth_~A" indx))
                 (fluid_settings_setint *fluidsynth-settings* "audio.jack.autoconnect" 1)
                 ;(fluid_settings_setstr *fluidsynth-settings* "audio.jack.id" (format nil "OM_fluidsynth_~A" indx))
                 ))
    ;  #+darwin(fluid_settings_setstr *fluidsynth-settings* "audio.driver"
    ;                                 #+cl-jack "jack"
    ;                                 #+cocoa "coreaudio")
      #+cocoa(fluid_settings_setstr *fluidsynth-settings* "audio.driver" "coreaudio"))
    
    (setf (synthname synth) name)
    (setf (settings synth) *fluidsynth-settings*)
    (setf (synthptr synth) (new_fluid_synth *fluidsynth-settings*))
    (setf (audioptr synth) (new_fluid_audio_driver (settings synth) (synthptr synth)))
    synth))


(defun add-n-fsynths (n)
  (loop for i from 1 to n
          do (push (create-fl-synt i) *fl-synths*))
  (setf *fl-synths* (reverse *fl-synths*))
  (loop for i in *fl-synths*
  do (fluid_synth_set_gain (synthptr i) 0.0));to be on the safe side
  (loop for i in *fl-synths*
  do (fluid_synth_set_gain (synthptr i) 0.25))
  )

;(add-n-fsynths 1)

;needed because cl-fluid package is loaded AFTER midi package
(defun om::load-all-fsynths (num)
  (cl-fluid::add-n-fsynths num))

(defun name-fsynths (synths)
  (when synths
    (loop for i in synths
          for n from 0 to (1- (length synths))
          collect (list n i))))

;;;

(defun delete-all-audio-drivers ()
  (when cl-fluidsynth::*fl-synths*
  (loop for i from 1 to (length *fl-synths*)
          do (delete_fluid_audio_driver 
              (audioptr (nth (1- i)  *fl-synths*))))
  (setf *fl-synths* nil)))

;(delete-all-audio-drivers)

(defun load-sf-to-all (path)
(when *fl-synths*
  (loop for i from 1 to (length *fl-synths*)
          do 
          (let ((synth (nth (1- i)  *fl-synths*)))
            (setf (sf2path synth) path)
            (push (sf2path synth) (sf2stack synth))
             (fluid_synth_sfload
              (synthptr synth) path 1)))))

(defun om::load-sf-to-all ()
  (let ((newpath (namestring om::*fluid-sf2*))); avoir pour avoir plusieurs sf2 pour chaque port
  (cl-fluid::load-sf-to-all newpath)))

;(load-sf-to-all "/home/karim/Work/DEV/deploy/openmusic/OPENMUSIC/resources/online/in-files/merlin.sf2")

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
;used below
(defun fluid-midi-setup ()
  (unless *fluid-midi-driver-settings*
    (progn
      (setf *fluid-midi-driver-settings* (new_fluid_settings))
      #+cl-jack (progn (fluid_settings_setstr *fluid-midi-driver-settings* "midi.driver" "jack")
		       (fluid_settings_setstr *fluid-midi-driver-settings* "midi.jack.id" "OM_fluidsynth")))))

;apparently not used
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


;;;;;;autoload fluid from prefs
(in-package :om)

(defun autoload-fluid-synths ()
  (let* ((modulepref (om::find-pref-module :fluid om::*saved-pref*))
         (auto (om::get-pref modulepref :fluid-autoload))
         (nsynths (om::get-pref modulepref :n-fsynth))
         (playlist (om::get-pref modulepref :sf2-setup-list))
         (paths (if playlist 
                    (loop for i in (cdr playlist)
                          collect (namestring (eval i))))))
    (if
        (and 
         (cl-fluid::fluidsynthlib-p)
         (not cl-fluid::*fl-synths*)
         auto
         )
        (progn
          (load-all-fsynths nsynths)
          (if paths
              (load-all-sf paths)
            (load-sf-to-all))
          (setf *sf2-setup-list* playlist); avoir quand on quite et on ne fait rien au niveau prefs...
          ))
    ))

; a finir:
(defun load-all-sf (paths)
  (when cl-fluid::*fl-synths*
    (let* ((pt paths)
           (der (last-elem pt)))
      (loop for i from 1 to (length cl-fluid::*fl-synths*)
            do 
              (let ((synth (nth (1- i)  cl-fluid::*fl-synths*)))
                (if pt
                    (progn
                      (setf (cl-fluid::sf2path synth) (car pt))
                      (push (cl-fluid::sf2path synth) (cl-fluid::sf2stack synth))
                      (cl-fluid::fluid_synth_sfload
                       (cl-fluid::synthptr synth) (car pt) 1)
                      (pop pt))
                  (progn
                    (setf (cl-fluid::sf2path synth) der)
                    (push (cl-fluid::sf2path synth) (cl-fluid::sf2stack synth))
                    (cl-fluid::fluid_synth_sfload
                     (cl-fluid::synthptr synth) der 1)))
                )))))
