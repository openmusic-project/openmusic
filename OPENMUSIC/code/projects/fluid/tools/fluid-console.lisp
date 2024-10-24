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
; Author: Karim Haddad
;==============================
; THE FLUID CONSOLE IS A SET OF FLUID SETTINGS TO BE SENT SIMULTANEOUSLY TO THE PLAYER 
; (A FLUID SETUP)
;==============================

(in-package :om)

;================================================
;=== SYNTH CONTROLLER                     
;=== a single synth controller 
;================================================
(defclass* Fluid-Ctrl () 
           ((midiport :initform 0 :initarg :midiport :accessor midiport :type integer)
            (midichannel :initform '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) :initarg :midichannel :accessor midichannel :type list)
            (program :initform '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :initarg :program :accessor program :type list)
            (prg-main :initform t :initarg :prg-main  :accessor prg-main :type nil)
            (pan-ctrl :initform '(64 64 64 64 64 64 64 64 64 64 64 64 64 64 64 64) :initarg :pan-ctrl  :accessor pan-ctrl :type list)
            (control1-num :initform '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :initarg :control1-num :accessor control1-num :type list)
            (control2-num :initform '(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) :initarg :control2-num :accessor control2-num :type list)
            (control1-val :initform '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :initarg :control1-val :accessor control1-val :type list)
            (control2-val :initform '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :initarg :control2-val :accessor control2-val :type list)
            (gain-ctrl :initform 32 :initarg :gain-ctrl :accessor gain-ctrl :type integer)
            (vol-ctrl :initform '(64 64 64 64 64 64 64 64 64 64 64 64 64 64 64 64) :initarg :vol-ctrl :accessor vol-ctrl :type list)
            (pitch-ctrl :initform 8192 :initarg :pitch-ctrl :accessor pitch-ctrl :type integer)
            (tuning :initform 6 :initarg :tuning :accessor tuning :type integer)
            ;;reverb
            (rev-on :initform nil :initarg :rev-on  :accessor rev-on :type nil)
            (room-ctrl :initform 20 :initarg :room-ctrl  :accessor room-ctrl :type integer)
            (damp-ctrl :initform 20 :initarg :damp-ctrl  :accessor damp-ctrl :type integer)
            (width-ctrl :initform 20 :initarg :width-ctrl  :accessor width-ctrl :type integer)
            (rlevel-ctrl :initform 20 :initarg :rlevel-ctrl  :accessor rlevel-ctrl :type integer)
            ;;chorus
            (ch-on :initform nil :initarg :ch-on  :accessor ch-on :type nil)
            (nr-ctrl :initform 20 :initarg :nr-ctrl  :accessor nr-ctrl :type integer)
            (clevel-ctrl :initform 20 :initarg :clevel-ctrl  :accessor clevel-ctrl :type integer)
            (speed-ctrl :initform 20 :initarg :speed-ctrl  :accessor speed-ctrl :type integer)
            (depth-ctrl :initform 20 :initarg :depth-ctrl  :accessor depth-ctrl :type integer)
            (ch-type-ctrl :initform 0 :initarg :ch-type-ctrl  :accessor ch-type-ctrl :type integer)
            ;;individual channel settings
            (i-chans :initform nil :initarg :i-chans  :accessor i-chans :type nil)
            ))
  

(defmethod fluid-ctrl-p ((self fluid-ctrl))  t)
(defmethod fluid-ctrl-p ((self t)) nil)

;(length cl-fluid::*fl-synths*)
;====================================
;=== SETTINGS CONTROLLER          ===
;=== a set of synth controllers ===
;====================================
(defclass* fluid-settings-ctrl (simple-score-element)
   ((midiport :initform nil :initarg :midiport :accessor midiport :type integer :documentation "output port number")
    (miditrack :initform 0 :accessor miditrack)
    (nbtracks :initform (length cl-fluid::*fl-synths*) :initarg :nbtracks :accessor nbtracks :type integer :documentation "number of tracks")
    (channels-ctrl :initform nil :accessor channels-ctrl :type t))
  (:icon 918))

;;; miditrack useful for QT player
(defmethod! set-track ((self fluid-settings-ctrl) tracks)
  :icon 917
  (setf (miditrack self) tracks)
  self)

;;; used in the maquette, to check if we need to instanciate a specific track for this object
(defmethod obj-in-sep-track ((self fluid-settings-ctrl)) nil)

(defmethod get-obj-dur ((self fluid-settings-ctrl)) 0)

(defmethod allowed-in-maq-p ((self fluid-settings-ctrl))  t)

(defmethod draw-mini-view  ((self t) (value fluid-settings-ctrl)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value fluid-settings-ctrl)) 
  (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self fluid-settings-ctrl) x x1 y y1 edparams view)
  (let ((pw (round (w view) (nbtracks self)))
        (pic (om-load-and-store-picture "audiotrack-bg" 'internal)))
    (loop
       for i from 0 to (nbtracks self)
       do (om-draw-picture view pic :pos (om-make-point (* i pw) 0)
			   :size (om-make-point pw (h view))))))

;;; SOME SUBCLASSES MAY USE DIFFERENT CHANNEL CONTROLLERS
(defmethod get-fluid-ctrl-class ((self t)) 'fluid-ctrl)

(defmethod initialize-instance :after ((self fluid-settings-ctrl) &rest l)
   (declare (ignore l))
   (if (< (nbtracks self) 1) (setf (nbtracks self) 1))
   (setf (channels-ctrl self) 
         (loop for i from 0 to (1- (nbtracks self))
               collect (make-instance (get-fluid-ctrl-class self)
                                      :midiport i))))
   

;===========================
; THE 'REAL' OBJECT USED IN OM
; (TODO: REMOVE SETTINGS-CTRL SUPERCLASS?)
;===========================

(defclass* fluid-synth-console (fluid-settings-ctrl) ()
  (:icon 918)
(:documentation "
   MIDI-MIX-CONSOLE represents a set of control events (volume, pan, program change, etc.) on <nbtracks> channels.

Open the mixing console editor after evaluating the box with the right number of tracks.
Modifications are sent immediately when performed in the editor.

The MIDI-MIX-CONSOLE object can also be 'played' as a musical object, from a patch window or in a maquette.
In this case, all internal events are sent simultaneously.
"
 ))

(add-player-for-object 'fluid-synth-console '(:fluidsynth))

(defmethod default-edition-params ((self fluid-synth-console))
  (pairlis '(player)
           '(:midi-player)))

(defmethod get-impulsion-pict ((self fluid-synth-console)) 
  (om-load-and-store-picture "audiotrack-bg" 'internal))

(defmethod Class-has-editor-p  ((self fluid-synth-console)) t )
(defmethod get-editor-class ((self fluid-synth-console)) 'FluidConsoleEditor)

(defmethod draw-mini-view  ((self t) (value fluid-synth-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value fluid-synth-console)) 
   (om-invalidate-view self t))


(defmethod draw-obj-in-rect ((self fluid-synth-console) x x1 y y1 edparams view)
  (let ((pw (round (w view) (nbtracks self)))
        (pic (om-load-and-store-picture "audiotrack-bg" 'internal)))
    (loop for i from 0 to (nbtracks self) do
          (om-draw-picture view pic :pos (om-make-point (* i pw) 0)
                           :size (om-make-point pw (h view))
                           ))))


(defmethod omNG-copy ((self fluid-synth-console))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self)
                :midiport ,(midiport self)
                :nbtracks ,(nbtracks self))))
     (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                             (omNG-copy ctrl))))
     (setf (miditrack rep) ',(miditrack self))
     rep
     ))

(defmethod copy-container  ((self fluid-synth-console) &optional (pere nil))
  "Cons a Lisp expression that returns a copy of self when it is evaluated."
  (let ((rep (make-instance (type-of self)
                :midiport (midiport self)
                :nbtracks (nbtracks self))))
    (setf (channels-ctrl rep) (loop for ctrl in (channels-ctrl self) collect
                                    (eval (omNG-copy ctrl))))
    (setf (miditrack rep) (miditrack self))
    rep
    ))

(defmethod omNG-save ((self fluid-synth-console) &optional (values? nil))
  "Cons a Lisp expression that retuns a copy of self when it is evaluated."
  `(when (find-class ',(type-of self) nil)
     (let ((rep (make-instance ',(type-of self) 
                               :midiport ,(midiport self) 
                               :nbtracks ,(nbtracks self))))
       (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                                 (omNG-save ctrl))))
       (setf (miditrack rep) ',(miditrack self))
       rep
       )))


;;;===============================
;;; FOR PLAY OR SAVE AS MIDI
;;;===============================

(defmethod* Play ((self fluid-synth-console) &key (player t))
   :initvals '(nil nil 2 nil nil) 
   :indoc '("object" "a player designator") 
   :icon 207
   :doc "Plays any OM Musical object.

<player> designates a particular player (t = dispatch automatically) 
"
   (let ((crtls (channels-ctrl self)))
     (loop for i in crtls
           do (send-all-to-fluids i))
     ))


(defmethod* send-all-to-fluids ((self fluid-ctrl))
  (let* ((port (midiport self))
         (pgm (program self))
         (pan (pan-ctrl self))
         (vol (gain-ctrl self))
         (tuning (tuning self)))
    (progn 
      (fluid-pgm-change pgm '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port port)
      (fluid-gain (/ vol 127.0) port)
      (change-tuning self tuning)
      (fluid-pan (car pan) *all-chans* port)
      )
  ))

;;================================================================
; EDITOR
;;================================================================

(defclass FluidConsoleEditor (EditorView) 
  ((ch-panels :initform nil :accessor ch-panels :type list)
   (presets-view :initform nil :initarg :presets-view :accessor presets-view)
   (delta-tracks :initform 0 :accessor delta-tracks :initarg :delta-tracks)
   (send-rt :initform nil :accessor send-rt :initarg :send-rt)))


(defmethod class-has-editor-p  ((self fluid-settings-ctrl)) t )
(defmethod get-editor-class ((self fluid-settings-ctrl)) 'FluidConsoleEditor)

;=== The Editor will be a scrollable editor with fixed size 
;(defmethod get-win-ed-size ((self settings-ctrl)) (om-make-point (+ 6 (min (* *fl-channel-w* 6) (* *fl-channel-w* (nbtracks self)))) 560))
(defmethod get-win-ed-size ((self fluid-settings-ctrl)) (om-make-point (min (* *fl-channel-w* 16) (* *fl-channel-w* (nbtracks self))) 760)) ;860 height c'est la!

(defmethod default-edition-params ((self fluid-settings-ctrl))
  (pairlis '(winsize winpos) 
           (list (get-win-ed-size self) (om-make-point 300 20))))

(defmethod make-editor-window ((class (eql 'FluidConsoleEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow nil) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize t ;nil ;ICI!!
                                                      :close-p t :winshow t
                                                      )))
    win))




(defmethod get-panel-class ((Self FluidConsoleEditor)) 'fluidcontrollerPanel)

(defmethod update-subviews ((Self FluidConsoleEditor))
   (om-set-view-size (panel self) (om-make-point (w self) (h self))))


;=== MAIN PANEL ===
(defclass fluidcontrollerPanel (om-scroller) ()
  ;;;(:default-initargs :scrollbars :h :retain-scrollbars t)
   )

(defmethod editor ((self fluidcontrollerPanel)) 
  (om-view-container self))

(defmethod get-object ((Self fluidcontrollerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self fluidcontrollerPanel))
  (report-modifications (om-view-container self)))
                                   

;======== CHaNNeL ConTrOllEr PAneL =====
;;; superclass for channelpanelview and simplechanelpanel
(defclass fluidPanel () 
  ((channelctr :initform nil :initarg :channelctr :accessor channelctr)
   (channelText :initform nil :accessor channelText :type t)
   (channelBox :initform nil :accessor channelBox :type t)
   (programMenu :initform nil :accessor programMenu :type t)
   (volumeText :initform nil :accessor volumeText :type t)
   (volumeVal :initform nil :accessor volumeVal :type t)
   (volumpeSlider :initform nil :accessor volumeSlider :type t)
   (pitchText :initform nil :accessor pitchText :type t)
   (pitchVal :initform nil :accessor pitchVal :type t)
   (pitchSlider :initform nil :accessor pitchSlider :type t)
   
   (scalamenu :initform nil :accessor scala1menu :type t)
   
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
   (chanpanel :initform nil  :accessor chanpanel :type t)
  ;REVERB
   (revbutton :initform nil :accessor revbutton :type t)
   (revroom :initform nil :accessor revroom :type t)
   (roomVal :initform nil :accessor roomVal :type t)
   (revdamp :initform nil :accessor revdamp :type t)
   (dampVal :initform nil :accessor dampVal :type t)
   (revwidth :initform nil :accessor revwidth :type t)
   (widthVal :initform nil :accessor widthVal :type t)
   (revlevel :initform nil :accessor revlevel :type t)
   (rlevelVal :initform nil :accessor rlevelVal :type t)
  ;CHORUS
   (cbutton :initform nil :accessor cbutton :type t)
   (cnr :initform nil :accessor cnr :type t)
   (cnrVal :initform nil :accessor cnrVal :type t)
   (clevel :initform nil :accessor clevel :type t)
   (clevelVal :initform nil :accessor clevelVal :type t)
   (cspeed :initform nil :accessor cspeed :type t)
   (cspeedVal :initform nil :accessor cspeedVal :type t)
   (cdepth :initform nil :accessor cdepth :type t)
   (cdepthVal :initform nil :accessor cdepthVal :type t)
   (ctype :initform nil :accessor ctype :type t)
   ))

(defclass fluidpanelview (fluidpanel om-view) ())

(defmethod editor ((self fluidpanel)) 
  (editor (om-view-container self))) 

(defmethod update-subviews ((Self fluidpanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self fluidPanel))
   (call-next-method))


(defmethod get-object ((Self fluidPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self fluidPanel))
  (report-modifications (om-view-container self)))

(defmethod get-channelpanel-class ((self fluidcontrollerpanel)) 'fluidpanelview)

(defvar *fl-channel-w* 140)
(setf *fl-channel-w* 120)

;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod window ((self FluidConsoleEditor)) 
  (play (object self))
  (om-view-window self))

(defmethod metaobj-scrollbars-params ((self FluidConsoleEditor))  '(:h t))

(defmethod make-preset-view ((self FluidConsoleEditor)) nil)

(defmethod initialize-instance :after ((self FluidConsoleEditor) &rest l)
   (declare (ignore l))
   
   (setf (panel self) (om-make-view (get-panel-class self) 
                                                     :owner self
                                                     :position (om-make-point 0 0) 
                                                     :bg-color *om-light-gray-color* ;(om-make-color 0.5 0.5 0.5)
                                                   ;  :default-width (h self)
                                                   ;  :visible-min-width (h self)
                                                    
                                                   ;  :external-max-width 800
                                                   ;  :external-min-height 270 
                                                     :scrollbars (first (metaobj-scrollbars-params self)) ;t
                                                   ;  :retain-scrollbars t
                                                   ;   :horizontal-scroll t
                                                   ;  :scrollbars (first (metaobj-scrollbars-params self))
                                                   ;  :retain-scrollbars (second (metaobj-scrollbars-params self))
                                                     :field-size  (om-make-point (* *fl-channel-w* (nbtracks (object self))) 540)
                                                     ;;;:size (om-make-point (w self) (- (h self) 15)))
                                                     :size (om-make-point (w self) (h self)))
      )
   
   (setf (presets-view self) (make-preset-view self))
   
   
   (setf (ch-panels self) 
      (loop for chctrl in (channels-ctrl (object self))
             for i = 0 then (+ i 1) collect
                (om-make-view (get-channelpanel-class (panel self))
                 :channelctr chctrl
                 :bg-color *om-light-gray-color*
                 :position (om-make-point (+ (delta-tracks self) (* i *fl-channel-w*)) (delta-tracks self)) 
                 :size (om-make-point (- *fl-channel-w* (* 1 (delta-tracks self))) ;750 ;ici j'ai change!
                                      (+ 200 (- (h self) 
                                                (if (presets-view self) (+ (h (presets-view self)) (delta-tracks self)) 0)
                                                (* 2 (delta-tracks self)))
                                         );;;a adapter plus tard
                                      ))))
   
   (apply 'om-add-subviews (cons (panel self) 
                                 (ch-panels self))) 
   (when (presets-view self) (om-add-subviews self (presets-view self))))

(defmethod update-editor-after-eval ((self FluidConsoleEditor) val) 
  (setf (object self) val)
  (om-set-view-size (window self) (get-win-ed-size (object self)))
  (om-set-view-size self (get-win-ed-size (object self)))
  (om-set-field-size (panel self) (om-make-point (* *fl-channel-w* (nbtracks (object self))) 540))
  (loop for chpan in (ch-panels self) do 
        (om-remove-subviews (panel self) chpan))
  (setf (ch-panels self) 
      (loop for chctrl in (channels-ctrl (object self))
             for i = 0 then (+ i 1) collect
                (om-make-view (get-channelpanel-class (panel self))
                                             :channelctr chctrl
                                             :owner (panel self)
                                             :bg-color *om-light-gray-color*
                                             :position (om-make-point (* i *fl-channel-w*) 0) 
                                             :size (om-make-point *fl-channel-w* (- (h self) 15))))))
  



(defmethod simple-controls-only ((self fluidpanel)) nil)


(defmethod initialize-instance :after ((self fluidPanel) &rest l)
  (declare (ignore l))
  (do-initialize-channel self))

(defmethod make-channel-title-items ((self fluidpanel)) 
  (setf (channelText self) (om-make-dialog-item 'om-static-text
                                                (om-make-point 8 5) 
                                                (om-make-point 76 20) "FLUIDSYNTH" 
                                                :font 
                                                #+linux *om-default-font2b*
                                                #+macosx (om-make-font oa::*om-def-bold-font-face* 10 :style '(:bold))
                                                #+win32 *om-default-font1b*
                                                ))

  (setf (channelBox self) 
        (om-make-dialog-item 'om-static-text (om-make-point 36 25) (om-make-point 28 18) 
                             (format nil " ~D" (1+ (midiport (channelctr self))))
                             :font *om-default-font2b*))
  
  ;(om-set-bg-color (channelBox self) *om-white-color*)
  (list  (channelText self) (channelBox self))
  )

;(1- (midichannel (channelctr self))) ;; is the port of synth

#|
(defparameter *tuning-table*
      (loop for i in (editor-tone-list)
              collect (cons (car (reverse i)) i)))

(defun get-tuning-val (val)
(car (cassq val *tuning-table*)))
|#



(defun get-tuning-val (val)
(car (nth val *edo-list*)))



(defmethod do-initialize-channel ((self fluidPanel)) 
  (let* ((progList (fluid-make-presets (midiport (channelctr self))))
         (ctrlList *midi-controllers*) bar1 bar2 bar3 bar4
         (pos 0)
         (bgcolor *om-light-gray-color*)
         ctrlchgText
         (title-items (make-channel-title-items self)))
    
    (setf pos (+ pos 55))
    (setf (programMenu self) 
          (om-make-dialog-item 'om-pop-up-dialog-item 
                               (om-make-point 10 pos) 
                               (om-make-point 76 12)
                               ""
                               :di-action (om-dialog-item-act item
                                            (change-all-pgm self (second (nth (om-get-selected-item-index item) progList))))
                               :font *om-default-font1*
                               :range (loop for item in progList collect (first item))
                               :value (first (nth (car (program (channelctr self))) progList))
                               )
          )
    
    (setf resetprg (om-make-view 'om-icon-button 
                                 :icon1 "stop" :icon2 "stop-pushed"
                                 :position (om-make-point 90 pos) :size (om-make-point 26 25) 
                                 :action (om-dialog-item-act item
                                           (declare (ignore item))
                                           (propagate-pgm-change self (car (program (channelctr self))))
                                           ;(change-all-pgm self (car (program (channelctr self))))
                                           )))
    (setf pos (+ pos 30))

    (setf (panText self) (om-make-dialog-item 'om-static-text
                                              (om-make-point 24 pos) 
                                              (om-make-point 40 16)
                                              "Pan"
                                              :font *om-default-font2*
                                              ))
    
    (setf (panVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 53 pos) 
                                             (om-make-point 30 16) 
                                             (pan2str (car (pan-ctrl (channelctr self))))
                                             :font *om-default-font2*
                                             ))
    
    (setf pos (+ pos 20))
    (setf (panSlider self) 
          (om-make-view 'graphic-numbox :position (om-make-point 38 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-pan self (value item))
                                     )
                        :font *om-default-font2*
                        :value (car (pan-ctrl (channelctr self)))
                        :min-val 0
                        :max-val 127))
    
    
    ;;Volume
    (setf pos (+ pos 26))
    (setf bar1 (om-make-view 'bar-item 
                             :position (om-make-point 4 pos) 
                             :size (om-make-point 90 10)
                             :bg-color bgcolor)) 
    
    (setf pos (+ pos 5))
    (setf (volumeText self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 20 pos) 
                                                 (om-make-point 40 16)
                                                 "Gain"
                                                 :font *om-default-font2*
                                                 ))
    
    (setf (volumeVal self) (om-make-dialog-item 'om-static-text 
                                                (om-make-point 50 pos) 
                                                (om-make-point 30 16)
                                                (format nil "~D" (gain-ctrl (channelctr self)) )
                                                :font *om-default-font2*
                                                ))
    
    (setf pos (+ pos 20))
    (setf (volumeSlider self) (om-make-dialog-item 'om-slider  
                                                   (om-make-point 38 pos) 
                                                   (om-make-point 20 100) ""
                                                   :di-action (om-dialog-item-act item
                                                                (change-volume self (om-slider-value item)))
                                                   :increment 1
                                                   :range '(0 127)
                                                   :value (gain-ctrl (channelctr self))
                                                   :direction :vertical
                                                   :tick-side :none
                                                   ))
    
    
    
    (unless (simple-controls-only self)

      (setf pos (+ pos 110))
      (setf bar2 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 90 10)
                               :bg-color bgcolor
                               ))
      
      (setf pos (+ pos 4))
      ;;;APPROX
      
      (setf scalaitem (om-make-dialog-item 'om-static-text 
                                           (om-make-point 28 pos) 
                                           (om-make-point 52 20) "Tuning"
                                        :font *om-default-font1*
                                        :bg-color *controls-color*))
      (setf pos (+ pos 20))
      (setf scalapop (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 5 pos)
                                       (om-make-point 92 20) ""
                                       :di-action (om-dialog-item-act item
                                                               (let ((newtone (second (nth (om-get-selected-item-index item) 
                                                                                           *edo-list*
                                                                                         ))))
                                                                 (progn 
                                                                   (setf (tuning (channelctr self)) newtone)
                                                                   (change-tuning self newtone)
                                                                   )
                                                                 ))
                                       :font (om-make-font *om-def-font-face* 10) ;*om-default-font1*
                                       :range *edo-names-0* ;(loop for item in (editor-tone-list)  collect (car item)) 
                                       :value (get-tuning-val (tuning (channelctr self)))
                                       ))
      
    ;  (setf pos (+ pos 40))
      #|
      (setf bar3 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 90 10)
                               :bg-color bgcolor
                               ))
      
      (setf pos (+ pos 8))
      |#
      #|
      (setf ctrlchgText (om-make-dialog-item 'om-static-text
                                             (om-make-point 14 pos) 
                                             (om-make-point 70 20)
                                             "CtrlChange"
                                             :font *om-default-font2*
                                             ))
      
      (setf pos (+ pos 22))
      
      (setf (ctrl1Menu self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                                  (om-make-point 13 pos) 
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
                                                    (om-make-point 13 pos) 
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
                                                 (om-make-point 18 pos) 
                                                 (om-make-point 30 16)
                                                 (format nil "~D" (control1-val (channelctr self)))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 26))
      (setf (ctrl2Menu self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                                  (om-make-point 13 pos) 
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
                                                    (om-make-point 13 pos) 
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
                                                 (om-make-point 18 pos) 
                                                 (om-make-point 30 16)
                                                 (format nil "~D" (control2-val (channelctr self)))
                                                 :font *om-default-font2*
                                                 ))
      
    |#
      
      ;;;REVERB 
      (setf pos (+ pos 32))
      (setf bar5 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 90 10)
                               :bg-color bgcolor
                               ))
      (setf pos (+ pos 8))
      (setf revon (om-make-dialog-item 'om-check-box (om-make-point 5 pos) (om-make-point 20 5) "" 
                                       :di-action
                                       (om-dialog-item-act item
                                         (if (om-checked-p item) 
                                             (progn
                                               (setf (revbutton self) t)
                                               (set-rev self t)
                                               (fluid-reverb-on 1 (midiport (channelctr self))))
                                           (progn
                                             (setf (revbutton self) nil)
                                             (set-rev self nil)
                                             (fluid-reverb-on 0 (midiport (channelctr self))))
                                           ))
                                           
                                       :font *controls-font*
                                       :checked-p (rev-on (channelctr self))
                                       ))
      (setf revtitle (om-make-dialog-item 'om-static-text
                                          (om-make-point 26 (+ pos 2)) ;(om-make-point 24 pos)
                                             (om-make-point 70 20)
                                             "Reverb"
                                             :font *om-default-font2*
                                             ))
     
    (setf pos (+ pos 20))
    
    (setf roomtitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 16 pos) 
                                             (om-make-point 70 20)
                                             "room"
                                             :font *om-default-font1*
                                             ))
    (setf damptitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 46 pos) 
                                             (om-make-point 70 20)
                                             "damp"
                                             :font *om-default-font1*
                                             ))
    (setf pos (+ pos 15))
    (setf (revroom self) 
          (om-make-view 'graphic-numbox :position (om-make-point 20 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-room self (value item)))
                        :font *om-default-font2*
                        :value (room-ctrl (channelctr self))
                        :min-val 0
                        :max-val 100))
    (setf (revdamp self) 
          (om-make-view 'graphic-numbox :position (om-make-point 50 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-damp self (value item)))
                        :font *om-default-font2*
                        :value (damp-ctrl (channelctr self))
                        :min-val 0
                        :max-val 100))

    (setf pos (+ pos 20))
    (setf (roomVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 20 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (room-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))
    (setf (dampVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 50 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (damp-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))

    (setf pos (+ pos 15))
    (setf widthtitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 16 pos) 
                                             (om-make-point 70 20)
                                             "width"
                                             :font *om-default-font1*
                                             ))
    (setf rleveltitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 46 pos) 
                                             (om-make-point 70 20)
                                             "level"
                                             :font *om-default-font1*
                                             ))
     (setf pos (+ pos 15))
    (setf (revwidth self) 
          (om-make-view 'graphic-numbox :position (om-make-point 20 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-width self (value item)))
                        :font *om-default-font2*
                        :value (width-ctrl (channelctr self))
                        :min-val 0
                        :max-val 100))
    (setf (revlevel self) 
          (om-make-view 'graphic-numbox :position (om-make-point 50 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-rlevel self (value item)))
                        :font *om-default-font2*
                        :value (rlevel-ctrl (channelctr self))
                        :min-val 0
                        :max-val 100))
     (setf pos (+ pos 20))
    (setf (widthVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 20 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (width-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))
    (setf (rlevelVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 50 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (rlevel-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))
      ;;;CHORUS 
      (setf pos (+ pos 32))
      (setf bar6 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 90 10)
                               :bg-color bgcolor
                               ))
      (setf pos (+ pos 8))
      (setf choruson (om-make-dialog-item 'om-check-box (om-make-point 5 pos) (om-make-point 20 5) "" 
                                          :di-action
                                          (om-dialog-item-act item
                                            (if (om-checked-p item) 
                                                (progn
                                                  (setf (cbutton self) t)
                                                  (set-chorus self t)
                                                  (fluid-chorus-on 1 (midiport (channelctr self))))
                                              (progn
                                                (setf (cbutton self) nil)
                                                (set-chorus self nil)
                                                (fluid-chorus-on 0 (midiport (channelctr self))))
                                              ))
                                          :font *controls-font*
                                          :checked-p (ch-on (channelctr self))
                                          ))
      (setf chorustitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 26 (+ pos 2)) ;(om-make-point 24 pos)
                                             (om-make-point 70 20)
                                             "Chorus"
                                             :font *om-default-font2*
                                             ))
     
    (setf pos (+ pos 20))
    
    (setf nrtitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 16 pos) 
                                             (om-make-point 70 20)
                                             "nr"
                                             :font *om-default-font1*
                                             ))
    (setf cleveltitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 46 pos) 
                                             (om-make-point 70 20)
                                             "level"
                                             :font *om-default-font1*
                                             ))
    (setf pos (+ pos 15))
    (setf (cnr self) 
          (om-make-view 'graphic-numbox :position (om-make-point 20 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-nr self (value item)))
                        :font *om-default-font2*
                        :value (nr-ctrl (channelctr self))
                        :min-val 0
                        :max-val 99))
    (setf (clevel self) 
          (om-make-view 'graphic-numbox :position (om-make-point 50 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-clevel self (value item)))
                        :font *om-default-font2*
                        :value (clevel-ctrl (channelctr self))
                        :min-val 0
                        :max-val 100))

    (setf pos (+ pos 20))
    (setf (cnrVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 20 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (nr-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))
    (setf (clevelVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 50 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (clevel-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))

    (setf pos (+ pos 15))
    (setf speedtitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 16 pos) 
                                             (om-make-point 70 20)
                                             "speed"
                                             :font *om-default-font1*
                                             ))
    (setf depthtitle (om-make-dialog-item 'om-static-text
                                             (om-make-point 46 pos) 
                                             (om-make-point 70 20)
                                             "depth"
                                             :font *om-default-font1*
                                             ))
     (setf pos (+ pos 15))
    (setf (cspeed self) 
          (om-make-view 'graphic-numbox :position (om-make-point 20 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-speed self (value item)))
                        :font *om-default-font2*
                        :value (speed-ctrl (channelctr self))
                        :min-val 0
                        :max-val 500))
    (setf (cdepth self) 
          (om-make-view 'graphic-numbox :position (om-make-point 50 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-depth self (value item)))
                        :font *om-default-font2*
                        :value (depth-ctrl (channelctr self))
                        :min-val 0
                        :max-val 210))
     (setf pos (+ pos 20))
    (setf (cspeedVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 20 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (speed-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))
    (setf (cdepthVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 50 pos) 
                                             (om-make-point 30 16)
                                             (integer-to-string (depth-ctrl (channelctr self)))
                                             :font *om-default-font1*
                                             ))
    (setf pos (+ pos 20))
    (setf (ctype self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                            (om-make-point 10 pos) 
                                            (om-make-point 70 24) ""
                                            :di-action (om-dialog-item-act item
                                                         (let ((val (om-get-selected-item-index item)))
                                                           (if (= 0 val)
                                                               (progn
                                                                 (setf (ch-type-ctrl (channelctr self)) 0)
                                                                 (change-ctype self val)
                                                                 )
                                                             (progn
                                                                 (setf (ch-type-ctrl (channelctr self)) 1)
                                                                 (change-ctype self val)
                                                                 )
                                                             )))
                                            :range '("Sine" "Triangle")
                                            :value (if (= 0 (ch-type-ctrl (channelctr self))) "Sine" "Triangle")
                                           :font *om-default-font1* ;*controls-font*
                                           ))
    ;;;
      (setf pos (+ pos 32))
      (setf bar4 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 90 10)
                               :bg-color bgcolor
                               ))

            
            
    (setf pos (+ pos 6))
    (setf (resetbutton self) (om-make-dialog-item 'om-button
                                                    (om-make-point 10 pos) 
                                                    (om-make-point 70 16)
                                                    "Reset"
                                                    :di-action (om-dialog-item-act item
                                                                 (reset-all-values self))
                                                    :font *controls-font*))  
      (setf pos (+ pos 32))
      (setf bar5 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 90 10)
                               :bg-color bgcolor
                               ))
     (setf pos (+ pos 6))
     (setf (chanpanel self) (om-make-dialog-item 'om-button
                                                 (om-make-point 10 pos) 
                                                 (om-make-point 70 50)
                                                 "Channels"
                                                 :di-action (om-dialog-item-act item
                                                              (let ((port (midiport (channelctr self))))
                                                                (show-fluid-mixer-win port (channelctr self))))
                                                 :font *controls-font*))
      
      
     )
    
    (apply 'om-add-subviews self title-items)
    
    (om-add-subviews self  
                     (programMenu self) 
                     resetprg
                     bar1 
                     (volumeSlider self) 
                     (volumeText self) 
                     (volumeVal self)
                     (panSlider self) 
                     (panText self) 
                     (panVal self)
                     bar5
                     ;;reverb
                     revon revtitle
                     roomtitle damptitle
                     (revroom self) (revdamp self)
                     (roomVal self) (dampVal self)
                     widthtitle rleveltitle
                     (revwidth self) (revlevel self)
                     (widthVal self) (rlevelVal self)
                     bar6
                     ;;chorus
                     choruson chorustitle
                     nrtitle cleveltitle
                     (cnr self) (clevel self)
                     (cnrVal self) (clevelVal self)
                     speedtitle depthtitle
                     (cspeed self)  (cdepth self)
                     (cspeedVal self)  (cdepthVal self)
                     (ctype self)
                     )
    
    (unless (simple-controls-only self)
      (om-add-subviews self bar2 ;bar3 
                       bar4 bar5
                      ; (pitchSlider self) 
                      ; (pitchText self) (pitchVal self)
                       scalaitem scalapop
                      ; (ctrl1Slider self) 
                      ; (ctrl1Menu self) 
                      ; (ctrl1Val self)
                      ; (ctrl2Slider self) 
                      ; (ctrl2Menu self) 
                      ; (ctrl2Val self)
                      ; ctrlchgText
                       (resetbutton self)
                       (chanpanel self)
                       )) 
    
    ))


(defmethod set-channel-values ((self fluidPanel) 
                               &key program vol pan pitch ctr1 ctr1val ctr2 ctr2val revroom) 
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

(defparameter *all-chans* 
               '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))



(defmethod change-channel ((self fluidPanel) value)
  (setf (midichannel (channelctr self)) value)
  (report-modifications self))

(defmethod change-all-pgm ((self fluidPanel) value)
  (let ((port (midiport (channelctr self))))
  (setf (program (channelctr self)) (repeat-n value 16))
  (fluid-pgm-change value '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port port)
  (report-modifications self)))

(defmethod propagate-pgm-change ((self fluidPanel) value)
  (let ((port (midiport (channelctr self))))
    (fluid-pgm-change value  '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port port)
    (when (i-chans (channelctr self))
    (loop for i in (channels-ctrl (i-chans (channelctr self)))
          do (setf (program i) value)))))
       

(defmethod change-program ((self fluidPanel) value)
  (let ((port (midiport (channelctr self))))
    (setf (program (channelctr self)) value)
    (fluid-pgm-change value '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port port)
    (report-modifications self)))

(defmethod change-volume ((self fluidPanel) value)
  (let ((port (midiport (channelctr self))))
  (setf (gain-ctrl (channelctr self)) value)
  (fluid-gain (/ value 127.0) port)
  (when (send-rt (editor self))
    (channel-send-vol (channelctr self))
    )
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

(defmethod change-pan ((self fluidPanel) value)
  (let ((port (midiport (channelctr self))))
  (setf (pan-ctrl (channelctr self)) (repeat-n value 16))
  (fluid-pan (repeat-n value 16) *all-chans* port)
  (when (send-rt (editor self))
    (channel-send-pan (channelctr self)))
  (let* ((target (panVal self))
         (new-str (pan2str value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self)))

(defmethod change-ctrl1-val ((self fluidPanel) value)
  (setf (control1-val (channelctr self)) value)
  (when (send-rt (editor self)) 
    (channel-send-ct1 (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (ctrl1Val self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl1-num ((self fluidPanel) value)
  (setf (control1-num (channelctr self)) value)
  (report-modifications self))

(defmethod change-ctrl2-val ((self fluidPanel) value)
  (setf (control2-val (channelctr self)) value)
  (when (send-rt (editor self)) 
    (channel-send-ct2 (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (ctrl2Val self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl2-num ((self fluidPanel) value)
  (setf (control2-num (channelctr self)) value)
  (report-modifications self))

(defmethod change-pitchbend ((self fluidPanel) value)
  (setf (pitch-ctrl (channelctr self)) value)
  (when (send-rt (editor self)) 
    (channel-send-pitch (channelctr self)))
  (let ((new-str (integer-to-string (pitchwheel-to-mc value)))
        (target (pitchVal self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))


;;reverb

(defmethod set-rev ((self fluidPanel) value)
  "sets reverhb on/off for panel"
  (setf (rev-on (channelctr self)) value)
  (report-modifications self))


(defmethod* get-rev-values ((self fluidpanel))
    (let* ((ctrl (channelctr self))
           (room (coerce (/ (room-ctrl ctrl) 100) 'double-float))
           (damp (coerce (/ (damp-ctrl ctrl) 100) 'double-float)) 
           (width  (coerce (/ (width-ctrl ctrl) 100) 'double-float)) 
           (level  (coerce (/ (rlevel-ctrl ctrl) 100) 'double-float)))
      (list room damp width level)))


(defmethod change-room ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-rev-values self)))
  (setf (room-ctrl (channelctr self)) value)
  (cl-fluidsynth::fluid_synth_set_reverb_roomsize 
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (car vals))
  (when (send-rt (editor self))
    (channel-send-pan (channelctr self))) ;;to send value
  (let* ((target (roomVal self))
         (new-str (integer-to-string value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self)))

(defmethod change-damp ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-rev-values self)))
    (setf (damp-ctrl (channelctr self)) value)
    (cl-fluidsynth::fluid_synth_set_reverb_damp 
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     (second vals))

    (when (send-rt (editor self))
      (channel-send-pan (channelctr self))) ;;to send value
    (let* ((target (dampVal self))
           (new-str (integer-to-string value)))
      (unless (string= new-str (om-dialog-item-text target))
        (om-set-dialog-item-text target new-str)
        (om-redraw-view target)))
    (report-modifications self)))

(defmethod change-width ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-rev-values self)))
    (setf (width-ctrl (channelctr self)) value)    
    (cl-fluidsynth::fluid_synth_set_reverb_width 
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     (third vals))
    (when (send-rt (editor self))
    (channel-send-pan (channelctr self))) ;;to send value
  (let* ((target (widthVal self))
         (new-str (integer-to-string value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self)))

(defmethod change-rlevel ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-rev-values self)))
    (setf (rlevel-ctrl (channelctr self)) value)    
    (cl-fluidsynth::fluid_synth_set_reverb_level 
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     (fourth vals))
    (when (send-rt (editor self))
    (channel-send-pan (channelctr self))) ;;to send value
  (let* ((target (rlevelVal self))
         (new-str (integer-to-string value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self)))

;;;
;;chorus

(defmethod set-chorus ((self fluidPanel) value)
  "sets reverhb on/off for panel"
  (setf (ch-on (channelctr self)) value)
  (report-modifications self))


(defmethod* get-chorus-values ((self fluidpanel))
    (let* ((ctrl (channelctr self))
           (nr (nr-ctrl ctrl))
           (clevel (coerce (/ (clevel-ctrl ctrl) 10) 'double-float)) 
           (speed  (coerce (/ (speed-ctrl ctrl) 100) 'double-float)) 
           (depth  (coerce (/ (depth-ctrl ctrl) 10) 'double-float))
           (type  (ch-type-ctrl ctrl)))
      (list nr clevel speed depth 0)))


(defmethod change-nr ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-chorus-values self)))
  (setf (nr-ctrl (channelctr self)) value)
  (cl-fluid::fluid_synth_set_chorus_nr
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (car vals))
  (when (send-rt (editor self))
    (channel-send-pan (channelctr self))) ;;to send value
  (let* ((target (cnrVal self))
         (new-str (integer-to-string value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self)))

(defmethod change-clevel ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-chorus-values self)))
    (setf (clevel-ctrl (channelctr self)) value)
    (cl-fluid::fluid_synth_set_chorus_level
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     (second vals))
    (when (send-rt (editor self))
      (channel-send-pan (channelctr self))) ;;to send value
    (let* ((target (clevelVal self))
           (new-str (integer-to-string value)))
      (unless (string= new-str (om-dialog-item-text target))
        (om-set-dialog-item-text target new-str)
        (om-redraw-view target)))
    (report-modifications self)))

(defmethod change-speed ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-chorus-values self)))
    (setf (speed-ctrl (channelctr self)) value)
    (cl-fluid::fluid_synth_set_chorus_speed
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     (third vals))
    (when (send-rt (editor self))
      (channel-send-pan (channelctr self))) ;;to send value
    (let* ((target (cspeedVal self))
           (new-str (integer-to-string value)))
      (unless (string= new-str (om-dialog-item-text target))
        (om-set-dialog-item-text target new-str)
        (om-redraw-view target)))
    (report-modifications self)))

(defmethod change-depth ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-chorus-values self)))
    (setf (depth-ctrl (channelctr self)) value)
    (cl-fluid::fluid_synth_set_chorus_depth
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     (fourth vals))
    (when (send-rt (editor self))
      (channel-send-pan (channelctr self))) ;;to send value
    (let* ((target (cdepthVal self))
           (new-str (integer-to-string value)))
      (unless (string= new-str (om-dialog-item-text target))
        (om-set-dialog-item-text target new-str)
        (om-redraw-view target)))
    (report-modifications self)))


(defmethod change-ctype ((self fluidPanel) value)
  (let ((port (midiport (channelctr self)))
        (vals (get-chorus-values self)))
  (setf (ch-type-ctrl (channelctr self)) value)
  (cl-fluid::fluid_synth_set_chorus_type
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (fifth vals))))

;;;

(defmethod reset-all-values ((self fluidPanel))
  (change-program self 0)
  (om-set-selected-item-index (programMenu self) 0)
  (change-pan self 64)
  (set-value (panSlider self) 64)
  (change-volume self 32)
  (om-set-slider-value (volumeSlider self) 32)
  ;(change-pitchbend self 8192)
  ;(om-set-slider-value (pitchSlider self) 8192)
 ; (change-ctrl1-val self 0)
 ; (om-set-slider-value (ctrl1Slider self) 0)
 ; (change-ctrl2-val self 0)
 ; (om-set-slider-value (ctrl2Slider self) 0)
)
  
 
;;;;;====================
;;; SIMPLE SYNTH MIXER
;;;;;====================

(defclass* simple-fluid-synth-console (fluid-synth-console) ()
  (:icon 918))

(defmethod get-synth-ctrl-class ((self simple-fluid-synth-console)) 'simple-fluid-ctrl)

(defclass* simple-Fluid-Ctrl (fluid-ctrl) ())

(defmethod send-midi-settings ((self simple-fluid-ctrl))
  (channel-send-prog self) 
  (channel-send-vol self) 
  (channel-send-pan self))

(defmethod! get-midievents ((self simple-fluid-ctrl) &optional test)
  (declare (ignore test))
  (let ((evtList nil) 
        (progevt (make-instance 'MidiEvent
                       :ev-date 0
                       :ev-type :ProgChange
                       :ev-chan (midichannel self)
                       :ev-port (midiport self)
                       :ev-fields (program self)))
        
        (volevt (make-instance 'MidiEvent
                       :ev-date 0
                       :ev-type :CtrlChange
                       :ev-chan (midichannel self)
                       :ev-port (midiport self)
                       :ev-fields (list 7 (vol-ctrl self))))
        
        (panevt (make-instance 'MidiEvent
                       :ev-date 0
                       :ev-type :CtrlChange
                       :ev-chan (midichannel self)
                       :ev-port (midiport self)
                       :ev-fields (list 10 (pan-ctrl self))))
        
        )
        
    (setf evtlist (list progevt volevt panevt))

    evtList))

;;;=== sStrange tools ===


;utilities (pas parfait!)
(defmethod! micro-calc ((div number))
  :numouts 4
  (let* ((midicstep (round (/ 200 div)))
        (tunning (round (/ 8192 div)))
        (scale1 (arithm-ser 6000 6200 midicstep))
        (scale2 (arithm-ser 0 8192 tunning)))
    (values midicstep scale1 tunning scale2)))


(defun get-midic-div (div)
  (let* ((arit (arithm-ser 6000 7200 (/ 200 div)))
         (mod1 (om-floor (om-mod arit 1200)))
         (md (if (modulo3-p div) 200 100))
         (mod2 (om-mod mod1 md))
         (res (list (car mod2))))
    (pop mod2)
    (loop  while (not (= (car mod2) 0))
           do (prog1 
                  (push (car mod2) res)
                (pop mod2)))
    (reverse res)))


(defun get-pbend-div (div)
  (let* ((arit (arithm-ser 0 8192 (/ 8192 div)))
         (md (if (modulo3-p div) 8192 4096))
         (mod1 (om-floor (om-mod arit md)))
         (res (list (car mod1))))
    (pop mod1)
    (loop  while (not (= (car mod1) 0))
           do (prog1 
                  (push (car mod1) res)
                (pop mod1)))
    (reverse res)))

;;;=== EDITOR ===

(defmethod get-win-ed-size ((self simple-fluid-synth-console)) (om-make-point (+ 6 (min (* *fl-channel-w* 6) (* 140 (nbtracks self)))) 340))

(defmethod make-editor-window ((class (eql 'simpleFluidConsoleEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow nil) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize t ;nil
                                                      :close-p t :winshow t
                                                      )))
    win))

(defclass simpleFluidConsoleEditor (FluidConsoleEditor) ())
(defclass simplefluidcontrollerPanel (fluidcontrollerPanel) ())
(defclass simplefluidpanel (fluidpanel om-item-view) ())

(defmethod get-editor-class ((self simple-fluid-synth-console)) 'simpleFluidConsoleEditor)
(defmethod get-panel-class ((Self simpleFluidConsoleEditor)) 'simplefluidcontrollerPanel)
(defmethod get-channelpanel-class ((self simplefluidcontrollerPanel)) 'simplefluidpanel)


(defmethod simple-controls-only ((self simplefluidpanel)) t)

(defmethod reset-all-values ((self simplefluidPanel))
  (change-pan self 64)
  (set-value (panSlider self) 64)
  (change-volume self 100)
  (om-set-slider-value (volumeSlider self) 100))


