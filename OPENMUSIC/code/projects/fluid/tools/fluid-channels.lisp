;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; FLUID package
; Author: K. Haddad

;;;====================================
;;; CHANNELS FLUID MIXER 
;;;====================================

(in-package :om)

;================================================
;=== FLUIDSYNTH CHANNEL CONSOLE                     
;=== a controller panel 
;================================================

(defclass* Fluid-Chan-Ctrl () 
           ((nfsynth :initform 0 :initarg :nfsynth :accessor nfsynth :type nil)
            (midiport :initform 0 :initarg :midiport :accessor midiport :type integer)
            (midichannel :initform 1 :initarg :midichannel :accessor midichannel :type integer)
            (program :initform 0 :initarg :program :accessor program :type integer)
            (pan-ctrl :initform 64 :initarg :pan-ctrl  :accessor pan-ctrl :type integer)
            (control1-num :initform 1 :initarg :control1-num :accessor control1-num :type integer)
            (control2-num :initform 2 :initarg :control2-num :accessor control2-num :type integer)
            (control1-val :initform 0 :initarg :control1-val :accessor control1-val :type integer)
            (control2-val :initform 0 :initarg :control2-val :accessor control2-val :type integer)
            (vol-ctrl :initform 32 :initarg :vol-ctrl :accessor vol-ctrl :type integer)
            (pitch-ctrl :initform 8192 :initarg :pitch-ctrl :accessor pitch-ctrl :type integer)
            (tuning :initform 2 :initarg :tuning :accessor tuning :type integer)
            ))


(defmethod fluid-chan-ctrl-p ((self fluid-chan-ctrl))  t)
(defmethod fluid-chan-ctrl-p ((self t)) nil)

;====================================
;=== SETTINGS CONTROLLER          ===
;=== a set of synth controllers ===
;====================================
(defclass* fluid-chan-settings-ctrl (simple-score-element)
   ((midiport :initform nil :initarg :midiport :accessor midiport :type integer :documentation "output port number")
    (miditrack :initform 0 :accessor miditrack)
    (nbtracks :initform 16 :initarg :nbtracks :accessor nbtracks :type integer :documentation "number of tracks")
    (channels-ctrl :initform nil :accessor channels-ctrl :type t))
  (:icon 918))

;;; miditrack useful for QT player
(defmethod! set-track ((self fluid-chan-settings-ctrl) tracks)
  :icon 917
  (setf (miditrack self) tracks)
  self)

;;; used in the maquette, to check if we need to instanciate a specific track for this object
(defmethod obj-in-sep-track ((self fluid-chan-settings-ctrl)) nil)

(defmethod get-obj-dur ((self fluid-chan-settings-ctrl)) 0)

(defmethod allowed-in-maq-p ((self fluid-chan-settings-ctrl))  t)

(defmethod draw-mini-view  ((self t) (value fluid-chan-settings-ctrl)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value fluid-chan-settings-ctrl)) 
  (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self fluid-chan-settings-ctrl) x x1 y y1 edparams view) 
  (let ((pw (round (w view) (nbtracks self)))
        (pic (om-load-and-store-picture "audiotrack-bg" 'internal)))
    (loop
       for i from 0 to (nbtracks self)
       do (om-draw-picture view pic :pos (om-make-point (* i pw) 0)
			   :size (om-make-point pw (h view) )))))

;;; SOME SUBCLASSES MAY USE DIFFERENT CHANNEL CONTROLLERS
(defmethod get-fluid-chan-ctrl-class ((self t)) 'fluid-chan-ctrl)
;(defmethod get-fluid-ch-ctrl-class ((self t)) 'fluidmixerchannelview)



(defmethod initialize-instance :after ((self fluid-chan-settings-ctrl) &rest l) 
   (declare (ignore l))
   (if (< (nbtracks self) 1) (setf (nbtracks self) 1))
   (if (> (nbtracks self) 16) (setf (nbtracks self) 16))
   (setf (channels-ctrl self) 
         (loop for i from 1 to (nbtracks self) 
               collect (make-instance (get-fluid-chan-ctrl-class self)
                                      :midiport (midiport self)
                                      :midichannel i))))



;===========================
; THE 'REAL' OBJECT USED IN OM
;(TODO: REMOVE SETTINGS-CTRL SUPERCLASS?)
;===========================

;pour le moment ne pas l'utiliser comme box car probleme de port -- a verifier!

(defclass* fluid-mix-console (fluid-chan-settings-ctrl) 
           ((port :initform 0 :initarg :port :accessor port :type integer :documentation "output port number")
            (presets :accessor presets  :initform nil)
            ;(current-preset :accessor current-preset  :initform 0)
            (current-preset :accessor current-preset :initarg :current-preset :initform 0))
            (:icon 918)
           (:documentation "
   MIDI-MIX-CONSOLE represents a set of control events (volume, pan, program change, etc.) on <nbtracks> channels.

Open the mixing console editor after evaluating the box with the right number of tracks.
Modifications are sent immediately when performed in the editor.

The MIDI-MIX-CONSOLE object can also be 'played' as a musical object, from a patch window or in a maquette.
In this case, all internal events are sent simultaneously.
"
            ))

(defmethod initialize-instance :after ((self fluid-mix-console) &rest l)
  (declare (ignore l))
  (unless (presets self) (setf (presets self) (def-fluid-presets)))
  (setf (midiport self) (port self))
  (loop for i in (channels-ctrl self)
        do (setf (midiport i) (port self))))


(add-player-for-object 'fluid-mix-console '(:fluidsynth))

(defmethod default-edition-params ((self fluid-mix-console))
  (pairlis '(player)
           '(:fluidsynth)))

(defmethod get-impulsion-pict ((self fluid-mix-console)) 
  (om-load-and-store-picture "audiotrack-bg" 'internal))

;added:
(defmethod Class-has-editor-p  ((self fluid-mix-console)) t )
(defmethod get-editor-class ((self fluid-mix-console)) 'FLUIDChannelsEditor)


(defmethod draw-mini-view  ((self t) (value fluid-mix-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value fluid-mix-console)) 
   (om-invalidate-view self t))


(defmethod draw-obj-in-rect ((self fluid-mix-console) x x1 y y1 edparams view) 
  (let ((pw (round (w view) (nbtracks self)))
        (pic (om-load-and-store-picture "audiotrack-bg" 'internal)))
    (loop for i from 0 to (nbtracks self) do
          (om-draw-picture view pic :pos (om-make-point (* i pw) 0)
                           :size (om-make-point pw (h view))
                           ))))


(defmethod omNG-copy ((self fluid-mix-console))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self)
                             :midiport ,(midiport self)
                             :nbtracks ,(nbtracks self)
                             :port ,(port self))))
     (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                               (omNG-copy ctrl))))
     (setf (miditrack rep) ',(miditrack self))
     (setf (presets rep) ',(presets self))
     rep
     ))

(defmethod copy-container  ((self fluid-mix-console) &optional (pere nil)) 
  "Cons a Lisp expression that returns a copy of self when it is evaluated."
  (let ((rep (make-instance (type-of self)
                :midiport (midiport self)
                :nbtracks (nbtracks self)
                :port (port self))))
    (setf (channels-ctrl rep) (loop for ctrl in (channels-ctrl self) collect
                                    (eval (omNG-copy ctrl))))
    (setf (miditrack rep) (miditrack self))
    (setf (presets rep) (presets self))
    rep
    ))

(defmethod omNG-save ((self fluid-mix-console) &optional (values? nil))
  "Cons a Lisp expression that retuns a copy of self when it is evaluated."
  `(when (find-class ',(type-of self) nil)
     (let ((rep (make-instance ',(type-of self) 
                               :midiport ,(midiport self) 
                               :nbtracks ,(nbtracks self)
                               :port ,(port self))))
       (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                                 (omNG-save ctrl))))
       (setf (miditrack rep) ',(miditrack self))
       (setf (presets rep) ',(presets self))
       rep
       )))


;;;===============================
;;; FOR PLAY OR SAVE AS MIDI
;;;===============================

(defmethod* Play ((self fluid-mix-console) &key (player t))
   :initvals '(nil nil 2 nil nil) 
   :indoc '("object" "a player designator") 
   :icon 207
   :doc "Plays any OM Musical object.
<player> designates a particular player (t = dispatch automatically)"
   (let ((crtls (channels-ctrl self)))
     (loop for i in crtls
           do (send-all-to-fluids-ch i))))


(defmethod* send-all-to-fluids-ch ((self fluid-chan-ctrl))
  (let* ((port (midiport self))
         (pgm (program self))
         (chan (midichannel self))
         (pan (pan-ctrl self))
         (vol (vol-ctrl self)))
    (progn 
      (fluid-pgm-change pgm chan :port port)
     ; (fluid-gain (om/ vol 127.0) port) ;a faire pour fluid-chan-ctrl
      (fluid-pan pan chan port))
  ))
;;;;;;;

(defvar *fluid-channels-window* nil)
(defparameter *fluid-channels* nil)

(defclass fluid-channels (fluid-mix-console OMBasicObject) 
  ()
  (:default-initargs :nbtracks 16))

(defun init-fluid-channels (port &optional vals)
  (make-instance 'fluid-channels 
                 :presets (or vals (def-fluid-presets))
                 :midiport port
                 ))

(defun def-fluid-presets () '(("-----" nil)))


;;;==================================
;;; EDITOR
;;;==================================

(defclass FLUIDChannelsEditor (FluidChannelConsoleEditor) ()
  (:default-initargs :delta-tracks 2 :send-rt t))

(defmethod get-panel-class ((Self FLUIDChannelsEditor)) 'FluidChannelsPanel)
(defclass FluidChannelsPanel (FluidChannelcontrollerPanel om-scroller) ())
(defmethod get-channelpanel-class ((self FluidChannelsPanel)) 'fluidchannelsview)
(defclass fluidchannelsview (fluid-chan-panelview) ())

;channelpanelview ---> fluidchannelpanelview 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;======== CHaNNeL ConTrOllEr PAneL =====
;;; superclass for channelpanelview and simplechanelpanel

;;for fluid-mix-console
(defclass FluidchanPanel () 
  ((portctr :initform 0 :initarg :portctr :accessor portctr)
   (channelctr :initform nil :initarg :channelctr :accessor channelctr)
   (channelText :initform nil :accessor channelText :type t)
   (channelBox :initform nil :accessor channelBox :type t)
   (programMenu :initform nil :accessor programMenu :type t)
   (volumeText :initform nil :accessor volumeText :type t)
   (volumeVal :initform nil :accessor volumeVal :type t)
   (volumpeSlider :initform nil :accessor volumeSlider :type t)
   (pitchText :initform nil :accessor pitchText :type t)
   (pitchVal :initform nil :accessor pitchVal :type t)
   (pitchSlider :initform nil :accessor pitchSlider :type t)
   (panText :initform nil :accessor panText :type t)
   (panVal :initform nil :accessor panVal :type t)
   (panSlider :initform nil :accessor panSlider :type t)
   (ctrl1menu :initform nil :accessor ctrl1menu :type t)
   (ctrl1Val :initform nil :accessor ctrl1Val :type t)
   (ctrl1Slider :initform nil :accessor ctrl1Slider :type t)
   (ctrl2menu :initform nil :accessor ctrl2menu :type t)
   (ctrl2Val :initform nil :accessor ctrl2Val :type t)
   (ctrl2Slider :initform nil :accessor ctrl2Slider :type t)
   (resetbutton :initform nil :accessor resetbutton :type t)
))


(defclass fluid-chan-panelview (fluidchanpanel om-view) ())

(defmethod editor ((self fluidchanpanel)) 
  (editor (om-view-container self))) 

(defmethod update-subviews ((self fluidchanpanel))
  (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self fluidchanPanel))
  (call-next-method))


(defmethod get-object ((Self fluidchanPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self fluidchanPanel))
  (report-modifications (om-view-container self)))

(defmethod get-channelpanel-class ((self fluidchannelcontrollerpanel)) 'fluid-chan-panelview)

(defvar *channel-w* 140)
(setf *channel-w* 80)

;;================================================================
; EDITOR
;;================================================================
  

(defmethod simple-controls-only ((self fluidchanpanel)) nil)


(defmethod initialize-instance :after ((self fluidchanpanel) &rest l) 
  (declare (ignore l))
  (do-initialize-channel self)
  )

(defmethod make-channel-title-items ((self fluidchanpanel))
  ;(om-inspect self)
  (setf (channelText self) (om-make-dialog-item 'om-static-text
                                                        (om-make-point 8 5) 
                                                        (om-make-point 76 20) "CHANNEL" 
                                                        :font *om-default-font2b*))
(setf (channelBox self) (om-make-dialog-item 'om-static-text
                                                 (om-make-point 27 25) 
                                                 (om-make-point 20 20) 
                                                 (number-to-string (midichannel (channelctr self)))
                                                 :font *om-default-font3b*
                                                 ))
  (list  (channelText self) (channelBox self))
  )


(defmethod do-initialize-channel ((self fluidchanpanel)) 
  (let* ((ctrlList *midi-controllers*) bar1 bar2 bar3 bar4
         (port (midiport (channelctr self)))
         (progList (fluid-make-presets port))
         (pos 0)
         (bgcolor *om-light-gray-color*)
         ctrlchgText
         (title-items (make-channel-title-items self)))
    (setf pos (+ pos 55))
    (setf (programMenu self) 
          (om-make-dialog-item 'om-pop-up-dialog-item 
                               (om-make-point 2 pos) 
                               (om-make-point 76 12)
                               ""
                               :di-action (om-dialog-item-act item
                                            (change-program self (second (nth (om-get-selected-item-index item) progList)))
                                            (setf (program (channelctr self))
                                                  (second (nth (om-get-selected-item-index item) progList))))
                               :font *om-default-font1*
                               :range (loop for item in progList collect (first item))
                               :value (first (nth (program (channelctr self)) progList))
                               )
          )
    
    (setf pos (+ pos 30))
    (setf (panText self) (om-make-dialog-item 'om-static-text
                                              (om-make-point 16 pos) 
                                              (om-make-point 40 16)
                                              "Pan"
                                              :font *om-default-font2*
                                              ))
    
    (setf (panVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 45 pos) 
                                             (om-make-point 30 16)
                                             (pan2str (pan-ctrl (channelctr self)))
                                             :font *om-default-font2*
                                             ))
    
    (setf pos (+ pos 20))
    (setf (panSlider self) 
          (om-make-view 'graphic-numbox :position (om-make-point 30 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-pan self (value item)))
                        :font *om-default-font2*
                        :value (pan-ctrl (channelctr self))
                        :min-val 0
                        :max-val 127))
    
    (setf pos (+ pos 26))
    (setf bar1 (om-make-view 'bar-item 
                             :position (om-make-point 4 pos) 
                             :size (om-make-point 72 10)
                             :bg-color bgcolor)) 
    
    (setf pos (+ pos 5))
    (setf (volumeText self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 16 pos) 
                                                 (om-make-point 40 16)
                                                 "Vol"
                                                 :font *om-default-font2*
                                                 ))
    
    (setf (volumeVal self) (om-make-dialog-item 'om-static-text 
                                                (om-make-point 45 pos) 
                                                (om-make-point 30 16)
                                                (format nil "~D" (vol-ctrl (channelctr self)) )
                                                :font *om-default-font2*
                                                ))
    
    (setf pos (+ pos 20))
    (setf (volumeSlider self) (om-make-dialog-item 'om-slider  
                                                   (om-make-point 30 pos) 
                                                   (om-make-point 20 100) ""
                                                   :di-action (om-dialog-item-act item
                                                                (change-volume self (om-slider-value item)))
                                                                :increment 1
                                                   :range '(0 127)
                                                   :value (vol-ctrl (channelctr self))
                                                   :direction :vertical
                                                   :tick-side :none
                                                   ))
    
    
    
    (unless (simple-controls-only self)

      (setf pos (+ pos 110))
      (setf bar2 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 72 10)
                               :bg-color bgcolor
                               ))
      
      (setf pos (+ pos 4))
      
      (setf (pitchText self) (om-make-dialog-item 'om-static-text
                                                  (om-make-point 5 pos) 
                                                  (om-make-point 40 16)
                                                  "Pitch"
                                                  :font *om-default-font2*
                                                  ))
      
      (setf (pitchVal self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 40 pos) 
                                                 (om-make-point 40 16)
                                                 (format nil "~D" (pitchwheel-to-mc (pitch-ctrl (channelctr self))))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 26))
      (setf (pitchSlider self) (om-make-dialog-item 'om-slider  
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 20)
                                                    ""
                                                    :di-action (om-dialog-item-act item
                                                                 (change-pitchbend self (om-slider-value item)))
                                                    :increment 1
                                                    :range '(0 16383)
                                                    :value (pitch-ctrl (channelctr self))
                                                    :direction :horizontal
                                                    :tick-side :none
                                                    ))
      
      (setf pos (+ pos 40))
      (setf bar3 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 72 10)
                               :bg-color bgcolor
                               ))
      
      (setf pos (+ pos 8))
      
      (setf ctrlchgText (om-make-dialog-item 'om-static-text
                                             (om-make-point 6 pos) 
                                             (om-make-point 70 20)
                                             "CtrlChange"
                                             :font *om-default-font2*
                                             ))
      
      (setf pos (+ pos 22))
      
      (setf (ctrl1Menu self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                                  (om-make-point 5 pos) 
                                                  (om-make-point 70 12) 
                                                  ""
                                                  :di-action (om-dialog-item-act item
                                                               (change-ctrl1-num self (second (nth (om-get-selected-item-index item) ctrlList))))
                                                  :font *om-default-font1*
                                                  :range (loop for item in ctrlList collect (first item))
                                                  :value (first (nth (control1-num (channelctr self)) ctrlList))
                                                  ))
      
      (setf pos (+ pos 22))
      (setf (ctrl1Slider self) (om-make-dialog-item 'om-slider  
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 20) 
                                                    ""
                                                    :di-action (om-dialog-item-act item
                                                                 (change-ctrl1-val self (om-slider-value item)))
                                                    :increment 1
                                                    :range '(0 127)
                                                    :value (control1-val (channelctr self))
                                                    :direction :horizontal
                                                    :tick-side :none
                                                    ))
      
      (setf pos (+ pos 18))
      (setf (ctrl1Val self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 10 pos) 
                                                 (om-make-point 30 16)
                                                 (format nil "~D" (control1-val (channelctr self)))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 26))
      (setf (ctrl2Menu self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                                  (om-make-point 5 pos) 
                                                  (om-make-point 70 12)
                                                  ""
                                                  :di-action (om-dialog-item-act item
                                                               (change-ctrl2-num self (second (nth (om-get-selected-item-index item) ctrlList))))
                                                  :font *om-default-font1*
                                                  :range (loop for item in ctrlList collect (first item))
                                                  :value (first (nth (control2-num (channelctr self)) ctrlList))
                                                  ))
      
      (setf pos (+ pos 22))
      (setf (ctrl2Slider self) (om-make-dialog-item 'om-slider  
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 20)
                                                    ""
                                                    :di-action (om-dialog-item-act item
                                                                 (change-ctrl2-val self (om-slider-value item)))
                                                    :increment 1
                                                    :range '(0 127)
                                                    :value (control2-val (channelctr self))
                                                    :direction :horizontal
                                                    :tick-side :none
                                                    ))
      
      (setf pos (+ pos 18))
      (setf (ctrl2Val self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 10 pos) 
                                                 (om-make-point 30 16)
                                                 (format nil "~D" (control2-val (channelctr self)))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 22))
      (setf bar4 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 72 10)
                               :bg-color bgcolor
                               ))

      (setf pos (+ pos 6))
    
    
      (setf (resetbutton self) (om-make-dialog-item 'om-button
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 16)
                                                    "Reset"
                                                    :di-action (om-dialog-item-act item
                                                                 (reset-all-values self))
                                                    :font *controls-font*)  
            
            )   ;;; fin unless simple-controls-only
          
      )
   
    (apply 'om-add-subviews self title-items)
    
    (om-add-subviews self  
                     (programMenu self) 
                     bar1 
                     (volumeSlider self) 
                     (volumeText self) 
                     (volumeVal self)
                     (panSlider self) 
                     (panText self) 
                     (panVal self)
                     )
    
    (unless (simple-controls-only self)
      (om-add-subviews self bar2 bar3 bar4
                       (pitchSlider self) 
                       (pitchText self) (pitchVal self)
                       (ctrl1Slider self) 
                       (ctrl1Menu self) 
                       (ctrl1Val self)
                       (ctrl2Slider self) 
                       (ctrl2Menu self) 
                       (ctrl2Val self)
                       ctrlchgText
                       (resetbutton self)
                       ))
    
    ))


(defmethod set-channel-values ((self fluidchanpanel)
                               &key program vol pan pitch ctr1 ctr1val ctr2 ctr2val)
  (when program
    (om-set-selected-item-index (programMenu self) program))
  (when vol
     (om-set-slider-value (volumeSlider self) vol)
     (om-set-dialog-item-text (volumeVal self) (number-to-string vol)))
  (when pan
     (set-value (panSlider self) pan)
     (om-set-dialog-item-text (panVal self) (pan2str pan)))
  (when pitch
    (om-set-slider-value (pitchSlider self) pitch)
    (om-set-dialog-item-text (pitchVal self) (number-to-string (pitchwheel-to-mc pitch))))
  (when ctr1
    (om-set-selected-item-index (ctrl1Menu self) ctr1))
  (when ctr1val
    (om-set-slider-value (ctrl1Slider self) ctr1val)
    (om-set-dialog-item-text (ctrl1Val self) (number-to-string ctr1val))
    )
  (when ctr2
    (om-set-selected-item-index (ctrl2Menu self) ctr2))
  (when ctr2val
    (om-set-slider-value (ctrl2Slider self) ctr2val)
    (om-set-dialog-item-text (ctrl2Val self) (number-to-string ctr2val))
    )
  )

;==============================
;=== ACTIONS ON CHANNEL PANELS:
;==============================

(defmethod change-channel ((self Fluidchanpanel) value)
  (setf (midichannel (channelctr self)) value)
  (report-modifications self))

;;;;;;PRG CHANGE
(defmethod change-program ((self Fluidchanpanel) value) 
(let* ((port (midiport (channelctr self)))
      (chan (midichannel (channelctr self)))
      ;(tuning (tuning (nfsynth (channelctr self))))
    ;  (channels (get-chans-from-tuning tuning chan))
    ;  (subv (om-subviews (om-view-container self)))
    ;  (sel (posn-match (om-subviews (om-view-container self)) (om- channels 1)))
      (pgtext (oa::om-get-selected-item (programmenu self)))
      )
  (fluid-pgm-change value chan :port port)
  ))




;;;;;VOLUME CHANGE

(defmethod change-volume ((self Fluidchanpanel) value)
(let* ((port (midiport (channelctr self)))
       (chan (midichannel (channelctr self))))
  (setf (vol-ctrl (channelctr self)) value)
 ; (setf (nth (1- chan) (vol-ctrl (nfsynth (channelctr self)))) value)
  (fluid-volume value chan port)
  (let ((new-str (integer-to-string value))
        (target (volumeVal self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)
      ))
  (report-modifications self)))


(defun pan2str (panvalue)
  (let* ((value (- panvalue 64))
         (new-str (cond ((= value 0) (integer-to-string value))
                        ((< value 0) (format nil "L~D" (- value)))
                        ((> value 0) (format nil "R~D" value)))))
    new-str))

(defmethod change-pan ((self Fluidchanpanel) value)
  (let* ((port (midiport (channelctr self)))
         (chan (midichannel (channelctr self)))
         ;(pan-lst (pan-ctrl (nfsynth (channelctr self))))
         )
    (setf (pan-ctrl (channelctr self)) value)
   ; (setf (nth (1- chan) pan-lst) value)
    (fluid-pan value chan port)
    (let* ((target (panVal self))
           (new-str (pan2str value)))
      (unless (string= new-str (om-dialog-item-text target))
        (om-set-dialog-item-text target new-str)
        (om-redraw-view target)))
    (report-modifications self)))


(defmethod change-ctrl1-val ((self Fluidchanpanel) value)
  (setf (control1-val (channelctr self)) value)
  ;(when (send-rt (editor self)) 
  ;  (channel-send-ct1 (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (ctrl1Val self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl1-num ((self Fluidchanpanel) value)
  (setf (control1-num (channelctr self)) value)
  (report-modifications self))

(defmethod change-ctrl2-val ((self Fluidchanpanel) value)
  (setf (control2-val (channelctr self)) value)
  ;(when (send-rt (editor self)) 
  ;  (channel-send-ct2 (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (ctrl2Val self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl2-num ((self Fluidchanpanel) value)
  (setf (control2-num (channelctr self)) value)
  (report-modifications self))

(defmethod change-pitchbend ((self Fluidchanpanel) value)
  (setf (pitch-ctrl (channelctr self)) value)
  ;(when (send-rt (editor self)) 
  ;  (channel-send-pitch (channelctr self)))
  (let ((new-str (integer-to-string (pitchwheel-to-mc value)))
        (target (pitchVal self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))


(defmethod reset-all-values ((self Fluidchanpanel))
  (change-program self 0)
  (om-set-selected-item-index (programMenu self) 0)
  (change-pan self 64)
  (set-value (panSlider self) 64)
  (change-volume self 100)
  (om-set-slider-value (volumeSlider self) 100)
  (change-pitchbend self 8192)
  (om-set-slider-value (pitchSlider self) 8192)
  (change-ctrl1-val self 0)
  (om-set-slider-value (ctrl1Slider self) 0)
  (change-ctrl2-val self 0)
  (om-set-slider-value (ctrl2Slider self) 0)
)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PRESETS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-preset ((self Fluid-Mix-Console) preset);((self Fluid-Channels) preset)
  "sets the vals in nth preset in the editor (and send them)"
  (when preset
    (loop for chan in preset 
        for ch-ctrl in (channels-ctrl self)
        do
        
        (setf (program ch-ctrl) (nth 0 chan)
              (vol-ctrl ch-ctrl) (nth 1 chan)
              (pan-ctrl ch-ctrl) (nth 2 chan)
              (pitch-ctrl ch-ctrl) (nth 3 chan)
              (control1-num ch-ctrl) (nth 4 chan)
              (control1-val ch-ctrl) (nth 5 chan)
              (control2-num ch-ctrl) (nth 6 chan)
              (control2-val ch-ctrl) (nth 7 chan))
        )
    ;(send-midi-settings self)
    ;a FAIRE
  ))

(defmethod set-mixer-editor ((self FLUIDChannelsEditor) (ref Fluid-Mix-Console))
  (loop for ch-ctrl in (channels-ctrl ref) 
        for ch-panel in (ch-panels self) 
        do
        (set-channel-values ch-panel
                            :program (program ch-ctrl)
                            :vol (vol-ctrl ch-ctrl)
                            :pan (pan-ctrl ch-ctrl)
                            :pitch (pitch-ctrl ch-ctrl)
                            :ctr1 (control1-num ch-ctrl)
                            :ctr1val (control1-val ch-ctrl)
                            :ctr2 (control2-num ch-ctrl)
                            :ctr2val (control2-val ch-ctrl))
        )
  )

(defmethod get-current-values ((self FLUIDChannelsEditor))
  "get the current values as a list (preset)"
  (loop for chan-ctrl in (channels-ctrl (object self)) collect 
        (list (program chan-ctrl)
              (vol-ctrl chan-ctrl)
              (pan-ctrl chan-ctrl)
              (pitch-ctrl chan-ctrl)
              (control1-num chan-ctrl)
              (control1-val chan-ctrl)
              (control2-num chan-ctrl)
              (control2-val chan-ctrl)
              )))

(defmethod make-preset-view ((self FLUIDChannelsEditor))
  (let* ((preset-view (om-make-view 'om-view 
                                   :position (om-make-point (delta-tracks self) 540);(- (h self) (delta-tracks self) 45)) 
                                   :scrollbars nil
                                   :retain-scrollbars nil
                                   :field-size  (om-make-point (- (* *channel-w* 16) 5) 45)
                                   :size (om-make-point (- (* *channel-w* 16) (* 2 (delta-tracks self))) 45)
                                   :bg-color (om-make-color 0.85 0.85 0.85) ;(om-make-color 0.1 0.4 0.54)
                                   :accepts-focus-p t))
         (title (om-make-dialog-item 'om-static-text
                                     (om-make-point 10 13)
                                     (om-make-point 130 20) "PRESETS :"
                                     :font *om-default-font1*
                                     ;:fg-color *om-white-color*
                                     ))
             
         (preset-list (om-make-dialog-item 'om-pop-up-dialog-item 
                                           #+linux(om-make-point 75 8) 
                                           #+macosx(om-make-point 75 12) 
                                    (om-make-point 120 12)
                                    ""
                                    :di-action #'(lambda (item)
                                                 (let ((presetnum (om-get-selected-item-index item))) ;;; 0 is reserved for the current values
                                                   (setf (current-preset (object self)) presetnum)
                                                   (when (> (current-preset (object self)) 0)
                                                     (set-preset (object self) (cadr (nth presetnum (presets (object self)))))
                                                     (set-mixer-editor self (object self))
                                                     )))
                                    :font *om-default-font1*
                                    :range (mapcar 'car (presets (object self)))
                                    :value (car (nth (current-preset (object self)) (presets (object self))))))

         (save-preset (om-make-dialog-item 'om-button
                                           (om-make-point 260 8)
                                           (om-make-point 75 12)
                                           "SAVE"
                                           :di-action (om-dialog-item-act item 
                                                        (if (> (current-preset (object self)) 0)
                                                            (progn
                                                              (setf (cadr (nth (current-preset (object self)) (presets (object self)))) (get-current-values self))
                                                              (print (string+ "Preset " (car (nth (current-preset (object self)) (presets (object self)))) " saved !"))
                                                              (save-midi-presets-in-preferences (object self)))
                                                          (om-message-dialog "Can't save: please select or create a preset !"))
                                                        )
                                           :font *om-default-font1*))
         
         (new-preset (om-make-dialog-item 'om-button
                                          (om-make-point 335 8)
                                          (om-make-point 75 12)
                                          "NEW"
                                          :di-action (om-dialog-item-act item 
                                                       (let ((name (om-get-user-string 
                                                                    "Enter a name for this preset" 
                                                                    :initial-string (format nil "Preset ~A" (length (presets (object self)))))))
                                                         (when name
                                                           (setf (presets (object self))
                                                                 (append (presets (object self))
                                                                         (list (list name (get-current-values self)))))
                                                           (om-enable-dialog-item preset-list t)
                                                           (om-set-item-list preset-list (mapcar 'car (presets (object self))))
                                                           (om-set-selected-item preset-list name)
                                                           (setf (current-preset (object self)) (1- (length (presets (object self)))))
                                                           (save-midi-presets-in-preferences (object self)))))
                                          :font *om-default-font1*))

         (delete-preset (om-make-dialog-item 'om-button
                                             (om-make-point 410 8)
                                             (om-make-point 75 12)
                                             "DELETE"
                                             :di-action (om-dialog-item-act item
                                                          (if (> (current-preset (object self)) 0)
                                                              (progn
                                                                (setf (presets (object self)) (append (subseq (presets (object self)) 0 (om-get-selected-item-index preset-list))
                                                                                                      (subseq (presets (object self)) (1+ (om-get-selected-item-index preset-list)))))
                                                                (om-set-item-list preset-list (mapcar 'car (presets (object self))))
                                                                (om-set-selected-item-index preset-list 0)
                                                                (if (<= (length (presets (object self))) 1) (om-enable-dialog-item preset-list nil))
                                                                (setf (current-preset (object self)) 0)
                                                                (save-midi-presets-in-preferences (object self)))
                                                            (om-beep))
                                                          )
                                             :font *om-default-font1*)))
    (if (<= (length (presets (object self))) 1) (om-enable-dialog-item preset-list nil))
    (om-add-subviews preset-view title preset-list save-preset new-preset delete-preset)    
    
    (om-set-view-size (om-view-container self) (om-make-point 1284 605))
    (om-set-view-size self (om-make-point 1284 605)) ;1290 611
    preset-view))
