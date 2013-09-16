;;; ===========================================================================
;;; CL setting up an external fluidsynth proc.
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or
;;; (at your option) any later version.
;;;   
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;   
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program; if not, write to the Free Software 
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;; 
;;; Author: Anders Vinjar

(in-package :cl-fluidsynth)

;;; setup external fluidsynth to connect midi from jack to
;;; TODO: wrap and load libfluidsynth.so to get this inside
;;;

(defparameter *fluidsynth-pid* nil)
(defparameter *fluidynth-io* nil)
(defparameter *fluid-synth-cmd* nil)

(defparameter *fluid-soundfont* "/usr/share/soundfonts/default.sf2")

(defvar *fluidsynth-cmd* (format nil "fluidsynth -j -m jack -g 2.0 -o midi.jack.id='OM_fluid' ~A" *fluid-soundfont*))


(defun fluidsynth-launch ()
  (unless *fluidsynth-pid*
    (when (and (streamp *fluidynth-io*) (open-stream-p *fluidynth-io*))
      (close *fluidynth-io*))
    (setf *fluidsynth-pid* nil)
    (multiple-value-bind (io err pid)
	(system:run-shell-command *fluidsynth-cmd*
				  :wait nil
				  :input :stream
				  :output :stream
				  :error-output nil)
      (declare (ignore err))
      (setf *fluidsynth-pid* pid)
      (setf *fluidynth-io* io)
      (format *standard-output* "started fluidsynth: pid: ~A" pid)
      (list pid io))))


#+cl-jack (defun fluidsynth-start-and-connect ()
	    (unless *fluidsynth-pid*
	      (fluidsynth-launch))
	    (mp:process-run-function "waiting-to-start-fluidsynth" nil
				     #'(lambda ()
					 (mp:process-wait "getting fluidsynth running first"
							  #'(lambda ()
							      (and cl-jack::*ClJackClient*
								   (cl-jack::jack-port-name cl-jack::*jack-midi-output-port*)
								   *fluidsynth-pid*)))
					 (sleep 1.0)
					 (cl-jack::jack-connect cl-jack::*ClJackClient*
								(cl-jack::jack-port-name cl-jack::*jack-midi-output-port*)
								"fluidsynth:midi"))))

(defun fluidsynth-quit ()
  (when (and (open-stream-p *fluidynth-io*) *fluidsynth-pid*)
    (format *fluidynth-io* "quit~%")
    (format *standard-output* "~&stopped fluidsynth: pid: ~A~%" *fluidsynth-pid*)
    (setf *fluidsynth-pid* nil)
    (when (open-stream-p *fluidynth-io*)
      (close *fluidynth-io*))))


;; (om::fluidsynth-start-and-connect)
;; (om::om-add-exit-cleanup-func 'fluidsynth-quit)
