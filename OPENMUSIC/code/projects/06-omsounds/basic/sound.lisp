;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: C. Agon, G. Assayag, J. Bresson

(in-package :om)


(defclass internalsound (om-sound)   
  ((sound-offset :accessor sound-offset)
   ;(soundpointer :initform nil :accessor soundpointer)
   (pict-sound :initform nil :accessor pict-sound)
   (pict-spectre :initform nil :accessor pict-spectre)
   (pict-spectre? :initform nil :accessor pict-spectre?)
   (time-started :initarg :time-started :initform nil :accessor time-started)
   (selec  :initform nil :accessor selec)))

(defclass* sound (simple-score-element internalsound) 
  ((tracknum :accessor tracknum :initarg :tracknum :initform 1 :documentation "a track index for multichannel mixing")
   (markers :accessor markers :initarg :markers :initform nil :documentation "a list of markers (s)")
   (vol :accessor vol :initform 100)  
   (pan :accessor pan :initform 0))  
   (:icon 287)
   (:documentation "Sound file object.

A sound is initialized with a pathname corresponding to an audio file on the disk.
Connect a pathname to the first input (<self>) to initialize the instance. 
If it is unlocked and unconnected, evaluating the box will open a file chooser dialog and allow the selection of a sound file to load.

The other inputs/outputs correspond to :
- <tracknum> = a track number on which the sound will be dispatch at playback. Tracknum can also be changed from the sound editor.
- <markers> = a list of markers (time in seconds). The markers can also be added/moved/removed from the sound editor.

NOTE: These inputs can be connected and will be evaluated even if something is connected to he <self> input of the box.

Press 'space' to play/stop the sound file.
"))

(defmethod initialize-instance :after ((self sound) &rest args)
  (setf (Qvalue self) 1000)
  ;;;(setf (soundpointer self) (get-sound-data self))
  )

(defmethod extent ((self sound))
  (setf (extent self) (sound-dur-ms self))
  (call-next-method))

;(defmethod extent ((self sound))
;  (unless (slot-value self 'extent) 
;    (setf (extent self) (sound-dur-ms self)))
;  (call-next-method))

(defmethod sound-name ((self sound))
   (pathname-name (om-sound-file-name self)))

(defmethod get-name ((self sound))
  (or (get-filename (om-sound-file-name self)) ""))
    
(defmethod sound-path ((self sound) )
   (om-sound-file-name self))

(defmethod real-dur ((self sound)) 
  (round (extent->ms self)))

(defmethod real-duration ((self sound) time) 
  (values time (+ time (round (extent->ms self)))))

(defmethod! set-channel ((self sound) chan)
  (setf (tracknum self) chan))
 
(defmethod copy-container ((self sound) &optional (pere ()))
   (let ((snd (if (om-sound-file-name self) 
                  (let ((copy (load-sound-file (om-sound-file-name self)))
                        (slots  (class-instance-slots (find-class 'simple-container))))
                    (setf (slot-value copy 'parent) pere)
                    (loop for slot in slots
                        when (not (eq (slot-definition-name slot) 'parent))
                        do (setf (slot-value  copy  (slot-definition-name slot))
                             (copy-container (slot-value self  (slot-definition-name slot)) copy)))
                    copy)
                (make-instance 'sound))))
     (setf (tracknum snd) (tracknum self))
     (setf (markers snd) (markers self))
     snd))

;;; copie : meme ptrs (pour le maquette play)
(defun copy-sound-file (sound)
  (let ((thesound (make-instance (type-of sound)
                    :filename (om-sound-file-name sound)
                    :audio-format (om-sound-format sound)
                    :number-of-samples (om-sound-n-samples sound)
                    :sample-rate (om-sound-sample-rate  sound)
                    :number-of-channels (om-sound-n-channels sound)
                    :data-position (om-sound-data-pos sound)
                    )))
    (if thesound
      (progn
        (setf (pict-sound thesound) (pict-sound sound))
        (setf (tracknum thesound) (tracknum sound))
        (setf (markers thesound) (markers sound))
        (setf (vol thesound) (vol sound))
        (setf (pan thesound) (pan sound))
        (setf (extent thesound) nil)
        )
      (om-message-dialog (format nil "Cannot copy the file : ~S" (namestring name)))
      )
    thesound))

;;; copy for play in maq
(defmethod maq-copy-container ((self sound)  &optional (pere ())) 
  (let ((copy (copy-sound-file self))
        (slots  (class-instance-slots (find-class 'simple-container))))
    (setf (slot-value copy 'parent) pere)
    (loop for slot in slots
          when (not (eq (slot-definition-name slot) 'parent))
          do (setf (slot-value  copy  (slot-definition-name slot))
                   (copy-container (slot-value self (slot-definition-name slot)) copy)))
    
    copy))

;;; maquette interface
(defmethod allowed-in-maq-p ((self sound))  (good-val-p? self))
(defmethod get-obj-dur ((self sound)) (extent self))
(defmethod allow-strech-p ((self sound) (factor number)) nil)


(defmethod execption-save-p ((self sound)) 'sound)

(defmethod save-exepcion ((self sound)) 
  (and (om-sound-file-name self)
        (register-resource :sound (om-sound-file-name self))
        `(let ((thesound (load-sound ,(om-save-pathname-relative (om-sound-file-name self))
                                     ,(tracknum self))))
           (when thesound
             (setf (markers thesound) ',(markers self)))
           thesound)))

;;;========
;;; LOAD
;;;========   
 
(defun load-sound-file (name &optional track)
  (let ((sound nil))
  ;;; (om-print (string+ "Loading sound file : " (om-namestring name)))
  (if (probe-file name)
      (progn 
        (setf sound (om-make-sound 'sound name))
        (setf (extent sound) nil))
    ;;; (om-supported-audio-format (om-sound-format thesound)))
    (progn 
      (setf sound (make-instance 'sound))
      (om-message-dialog (format nil (om-str :file-not-found) (namestring name))))
    )
  sound))

(defmethod* get-sound () 
   :initvals nil
   :indoc nil
   :doc "load a Aiff file"
   :icon 148
  (let ((name (om-choose-file-dialog 
               :directory (def-load-directory)
               :prompt (om-str :choose-snd) :types (list (om-str :all-files) "*.*" 
                                                         (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif" 
                                                         (format nil (om-str :file-format) "WAV") "*.wav")))
        (rep nil))
    (when name
      (setf *last-loaded-dir* (pathname-dir name))
      (setf rep (load-sound-file name)))
    (unless rep (om-abort))
    rep))

; (get-sound)

(defmethod get-obj-from-file ((type (eql 'aiff)) filename)
  (load-sound-file filename))
(defmethod get-obj-from-file ((type (eql 'aif)) filename)
  (load-sound-file filename))
(defmethod get-obj-from-file ((type (eql 'wav)) filename)
  (load-sound-file filename))

(defmethod* objfromobjs ((self null) (type sound))
   (make-instance 'sound))

(defmethod* objfromobjs ((self string) (type sound))
   (load-sound-file self))

(defmethod* objfromobjs ((self pathname) (type sound))
   (load-sound-file self))

(defun load-sound (name &optional track)
  (let ((snd (om-load-if name 'load-sound-file)))
    (unless snd (setf snd (make-instance 'sound :filename name)))
    (when (and snd track) (setf (tracknum snd) track))
    snd))

;======================
; EDITOR
;======================

(defmethod Class-has-editor-p ((self sound)) t)

(defmethod get-initval ((self sound)) (make-instance 'sound))

(defmethod default-obj-box-size ((self sound)) (om-make-point 80 50))

(defmethod get-editor-class ((self sound)) 'soundEditor)

;;; boxeditcall evaluation
(defmethod cons-new-object ((self sound) args objs)
  (let ((rep (call-next-method)))
    (when rep
      (setf (tracknum rep) (if (integerp (nth 1 args)) (nth 1 args) 1))
      (when (consp (nth 2 args)) (setf (markers rep) (nth 2 args))))
    rep))

;;; default value at box evaluation
(defmethod make-one-instance ((self sound) &rest slots-vals) 
  (let ((snd (get-sound)))
    (when (and snd (car slots-vals)) 
      (setf (tracknum snd) (car slots-vals))
      (setf (markers snd) (cadr slots-vals)))
    snd))

(defmethod object-remove-extra ((self sound) box)
  (box-stop-player box (player box)))

(defmethod box-stop-player (box player) nil)
  


;============
; OM METHODS
;============

;;; OPTIMISER AVEC L'OUVERTURE DU FICHIER !!!
(defmethod* sound-points-old ((self sound) (num integer) &optional channel)
  :initvals '(nil 1000 1)
  :indoc '("sound file" "num of points")
  :doc "load the sound points"
  :icon 221
   (let ((sizedata (om-sound-sample-size self))
           (numdat (om-sound-n-samples  self))
           (numchan (om-sound-n-channels  self))
           (position 0)
           (old-pos 0)
           (rep nil))
     (setf channel (if channel channel 1))
     (setf position (+ (om-sound-data-pos self) (* (round sizedata 8) (- channel 1))))
     (setf old-pos position)
     (if (or (> channel numchan) (> num numdat)) 
       (om-message-dialog "Bad input values")
       (loop for i from 0 to numdat by (round numdat num) do
                 (let (new-point)
                   ;(cond
                   ; ((= sizedata 8) (setf new-point (om-read-sound-data self position 1)))
                   ; ((= sizedata 16) (setf new-point (round (om-read-sound-data self position 2) 256))))
                   (setf new-point (round (om-read-sound-data self position (round sizedata 8)) (ash 1 (- sizedata 8))))
                   (when (> new-point 128)
                       (setf new-point (- new-point 256)))
                   (push new-point rep)
                   )  
                 (setf position (+ old-pos (* numchan (* (/ sizedata 8) i))))
                 ))
     (reverse rep)))


(defmethod* sound-points ((self sound) (num integer) &optional channel)
  :initvals '(nil 1000 1)
  :indoc '("a sound object" "number of points" "channel number")
  :doc "Reurns <num> sampled points from the audio waveform of channel <channel> in <self>."
  :icon 221
  (let ((numdat (om-sound-n-samples  self))
        (numchan (om-sound-n-channels  self)))
     (setf channel (if channel channel 1))
     (if (or (> channel numchan) (> num numdat)) 
         (om-message-dialog "Bad input values")
       (loop for i from 0 to numdat by (round numdat num) collect
             (nth (1- channel) (om-read-sound-data self i :float))))
     ))

(defmethod! sound-dur ((sound pathname))
  :icon 221
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in seconds."
  (let ((thesound (om-make-sound 'sound sound)))
    (sound-dur thesound)))

(defmethod! sound-dur ((sound string))
  (when (probe-file (pathname sound))
    (sound-dur (pathname sound))))

(defmethod! sound-dur ((sound sound))
   (if (and sound (oa::number-of-samples-current sound) oa::srate
            (> oa::srate 0))
       (float (/ (oa::number-of-samples-current sound) oa::srate))
     0))

;(defmethod! sound-dur ((sound sound))
;   (if (and sound (om-sound-n-samples sound) (om-sound-sample-rate sound)
;            (> (om-sound-sample-rate sound) 0))
;       (float (/ (om-sound-n-samples sound) (om-sound-sample-rate sound)))
;     0))


(defmethod! sound-dur-ms ((sound t))
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in milliseconds."
  :icon 221
  (round (* 1000 (sound-dur sound))))



;=======
; BOX 
;=======
(defmethod good-val-p? ((self sound)) t)

(defmethod valid-sound-p ((self sound))
  (and (om-sound-file-name self) 
       (ignore-errors (probe-file (om-sound-file-name self)))
       (> (get-obj-dur self) 0)))

(defmethod get-type-of-ed-box ((self sound)) 'OMaiffFilebox)

(defclass OMaiffFilebox (OMBoxEditCall) ())

(defmethod gen-code-call ((self OMaiffFilebox))
   (if (connected? (first (inputs self)))
       `(let ((snd (objFromObjs ,(gen-code (first (inputs self)) (second (inputs self))) ,(value self))))
                (when ,(cadr (decode self)) (setf (tracknum snd) ,(cadr (decode self))))
                (when ,(caddr (decode self)) (setf (markers snd) ,(caddr (decode self))))
                snd)
     `(apply 'make-one-instance (list ,(value self) ,.(cdr (decode self))))))

(defmethod numouts ((self OMaiffFilebox)) 3)

(defmethod correct-box-inputs ((class (eql 'sound)) inputs)
  (loop for input in (get-inputs-from-inst (make-instance class))
        for i = 0 then (+ i 1) collect
        (if (and (nth i inputs) (string-equal (name input) (name (nth i inputs))))
            (nth i inputs) input)))

(defmethod get-frame-class ((self OMaiffFilebox)) 'boxsoundframe)

(omg-defclass boxsoundframe (boxEditorFrame) ())

(defmethod om-get-menu-context ((self boxsoundframe))
  (append 
   (boxframe-default-list self)
   (player-menu-item (object self))
   (object-specific-menu (value (object self)))))

(defmethod object-specific-menu ((self sound))
  (list (om-new-leafmenu "Open with external editor..."
                         #'(lambda () (om-cmd-line (string+ "open " (namestring (sound-path self))))))))

;=======
; PICT
;=======

(defmethod sound-get-pict ((self sound)) 
  (unless (equal (pict-sound self) :error)
    (or (pict-sound self)
        (progn
          (setf (pict-sound self) (or (om-sound-get-pict self) :error))
          (unless (equal (pict-sound self) :error)
              (pict-sound self)
            )))))
  
(defmethod pic-to-draw ((self sound)) 
  (if (and (pict-spectre self) (pict-spectre? self))
    (thepict (pict-spectre self))
    (sound-get-pict self)))

(defmethod draw-mini-view ((self t) (val sound))
  (draw-obj-in-rect val 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self sound) x x1 y y1 edparams view) 
  (let ((picture (pic-to-draw self)))
    (om-with-focused-view view 
      (if picture
          (let ((dur (/ (om-sound-n-samples self) (om-sound-sample-rate self)))
                (pos x) (w (- x1 x)) (h (h view)))
            (om-with-fg-color view *om-dark-gray-color*
              (om-draw-picture view picture (om-make-point x y) (om-make-point (- x1 x) (- y1 y))))
            (om-with-fg-color view *om-steel-blue-color*
              (om-with-line '(2 2)
                (loop for item in (markers self) do
                      (setf pos (+ x (round (* w item) dur)))
                      (om-draw-line pos y pos y1)))))
        (let ((path (om-sound-file-name self)))
          (when path
            (om-with-font *om-default-font1*
                          (om-draw-string 5 14 (string+ (pathname-name path) (if (stringp (pathname-type path)) (string+ "." (pathname-type path)) "")
                                                        ":"))
                          (om-draw-string 5 28 (if (probe-file path) (om-str :file-error)
                                                 (string+ (format nil (om-str :file-not-found) (pathname-name path)) "...")))))
          ))
      ;(when (and (om-sound-n-samples self) (zerop (om-sound-n-samples self)))
      ;  (om-draw-string 15 15 (om-str "Error")))
      )))


(defmethod update-miniview ((self t) (type sound))  (om-invalidate-view self t))

;;; pour les maquettes
(defmethod draw-editor-mode ((self sound) view)
  (draw-obj-in-rect self 0 (w view) 0 (h view) nil view)
  ;(om-draw-picture view (sound-get-pict self) (om-make-point 0 0) (om-make-point (w view) (h view)))
  (draw-carre view nil))

;;; synth maquette
(defmethod cons-maq-mini-pict ((self sound) frame fontsize size)
  (sound-get-pict self))




  