;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;Macintosh version (Digitool Macintosh Common Lisp - MCL)
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
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
;Authors: Augusto Agon and Jean Bresson
;;===========================================================================

;;===========================================================================
;DocFile
; AUDIO CLASSES AND FUNCTIONS
;;===========================================================================


(in-package :om-api)

(export '(
          om-start-audio
          *om-player-sample-rate*

          om-open-audio-player
          om-close-audio-player
          om-add-sound-to-player

          om-start-player
          om-pause-player
          om-continue-player
          om-stop-player
          om-reset-player
          
          om-set-track-volume
          om-set-track-pan
          
          om-play-one-sound
          ) :om-api)

(defvar *tracks-list* nil)
(defvar *init-settings-table* nil)

(defun om-start-audio ()
  (setf *tracks-list* (make-hash-table))
  (setf *init-settings-table* (make-hash-table))
  (loop for i from 0 to 32 do (setf (gethash i *init-settings-table*) (list 1.0 0.5)))
  (setf las::*libaudiostream* nil)
  (setf las::*libaudiostream-pathname* (om-lib-pathname las::*libaudiostream-pathname*))
  (las::libaudiostream-framework))

(defvar *om-player-sample-rate* 44100)
(defvar *om-player-n-channels* 2)
(setf *om-player-n-channels* 2)


(defun om-open-audio-player ()
  (let ((player (las::OpenAudioPlayer 0 oa::*om-player-n-channels* 32 oa::*om-player-sample-rate* 512 65536 65536 las::kCoreAudioRenderer 1)))
    (las::StartAudioPlayer player)
    player))

(defun om-close-audio-player (player)
  (when player
    (las::CloseAudioPlayer player)))


;; (las::MakeReadSound "/Users/bresson/Desktop/circle1a.aiff")

;;; sound
;;; removed : convert-filename-encoding
(defmethod om-read-sound ((self om-sound) &optional start end)
  (om-sound-protect self
    (let ((s (if (and start end)
                 (las::MakeRegionSound (namestring (filename self)) (round (* start (sample-rate self))) (round (* end (sample-rate self))))
                (las::MakeReadSound (namestring (filename self))))))
      (if (las::las-null-ptr-p s) nil s))))


;;; stream
(defmethod om-read-sound ((self t) &optional start end)
  (if (and start end)
    (let ((s (las::MakeCutSound self (round (* start *om-player-sample-rate*)) (round (* end *om-player-sample-rate*)))))
      (if (las::las-null-ptr-p s) nil s))
    self))


(defun om-add-sound-to-player (player sound at &optional start end (tracknum 1) (vol nil) (pan nil))
  ;(print (list "add sound" sound))
  (let ((snd nil))
      ;;; si intervalle
    (if (and start end)
      (unless (> at end) ;;; on joue pas
        (if (>= start at)  ;;; on prend une region du son
          (setq snd (om-read-sound sound
                                   (/ (- start at) 1000.0) 
                                   (/ (- end at) 1000.0)))
          ;;; on ajoute un blanc avant et on prend une region plus petite
          (let ((s (om-read-sound sound
                                0 
                                (/ (- end at) 1000.0))))
                (if s (setq snd 
                            (las::MakeSeqSound 
                             (las::MakeNullSound (round (* (- at start) *om-player-sample-rate*) 1000))
                             s
                             0))  
                    (print "error read sound")
                    ))))
      
      ;;; sinon on joue le son (eventuellement shift de AT ms)
      (progn   
        (setq snd (om-read-sound sound))
        (if snd 
          (when (not (zerop at))  
            (let ((position (round (* at *om-player-sample-rate*) 1000)))
              (setq snd (las::MakeSeqSound (las::MakeNullSound position) snd 0))) 
            )
          (print "error read sound")
          ))
      
      )  ;;; endif

    (when (and snd (or vol pan))
      (setq effectlist (las::MakeAudioEffectList))
      (when pan
        (case (las::GetChannelsSound snd)
          (1 (setq effectlist (las::AddAudioEffect effectlist (las::MakeMonoPanAudioEffect pan))))
          (2 (let ((pan2 (pan2panpan pan)))
               (setq effectlist (las::AddAudioEffect effectlist (las::MakeStereoPanAudioEffect (car pan2) (cadr pan2))))))
          (otherwise nil))
        )
      (when vol 
        (setq effectlist (las::AddAudioEffect effectlist (las::MakeVolAudioEffect vol)))
        )
      (setq snd (las::MakeTransformSound snd effectlist 10 10))
      )
          
      ;;; mix le son dans son channel
      (when snd
        (let ((trk (gethash tracknum *tracks-list*)))
          (if trk
            (setf (gethash tracknum *tracks-list*) (las::MakeMixSound trk snd))
            ;(setf (gethash tracknum *tracks-list*) snd)
            (setf (gethash tracknum *tracks-list*) (las::MakeMixSound (las::MakeNullSound *om-player-sample-rate*) snd))
            )
          ))  
  ))


(defun om-reset-player (player)
  (clrhash *tracks-list*))

(defvar *player-state* nil)

; (mapcar 'pan2panpan '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))

; L = 1.0
; R = 0.0
(defun pan2panpan (pan)
  ;(print pan)
  (if (>= pan 0.5) 
      ;; Left
      (list 1.0 (* 2.0 (- pan 0.5)))
    ;; Right
    (list (* pan 2.0) 0.0)))   


(defun om-start-player (player)
  (if *player-state*
    ;;; loop : restart sans recharger les canaux
    (progn 
      ;(las::StopAudioPlayer player)
      (maphash #'(lambda (num snd)
                   (las::StopChannel player num)
                   (las::StartChannel player num)
                   ) *tracks-list*))
    (progn
      (maphash #'(lambda (num snd)
                   (when (> (las::GetLengthSound snd) 0)
                     (let ((pan2 (if (= 1 (las::GetChannelsSound snd))
                                     (list (cadr (gethash num *init-settings-table*)) (cadr (gethash num *init-settings-table*)))
                                   (pan2panpan (cadr (gethash num *init-settings-table*)))
                                   )))
                       
                       (las::LoadChannel player snd num (car (gethash num *init-settings-table*)) 
                                         (car pan2) (cadr pan2)
                                         )
                       (las::StopChannel player num)
                       (las::StartChannel player num)
                       )
                     )) *tracks-list*)
      (las::StartAudioPlayer player)
      ))
    
    (setf *player-state* t)
    )

#|
(setf player (las::OpenAudioPlayer 0 2 32 44100 1024 65536 65536 las::kPortAudioRenderer 1))
(rlet ((i :TChannelInfo))
  (las::getinfochannel player 0 i)
  (print (las::status i)))
|#

(defun om-pause-player (player)
  (maphash #'(lambda (num snd)
               (las::StopChannel player num))
           *tracks-list*))


(defun om-continue-player (player)
  (maphash #'(lambda (num snd)
               (las::ContChannel player num))
           *tracks-list*)  
  (las::StartAudioPlayer player))


(defun om-stop-player (player)
  (when *player-state*
    (maphash #'(lambda (num snd)
                 (las::StopChannel player num)) *tracks-list*)
    (clrhash *tracks-list*)
    (las::StopAudioPlayer player)
    (setf *player-state* nil)
    ;;; pour liberer des fichiers...
    ;(sys::gc-all)
    ))


(defun om-set-track-volume (player tracknum vol)
  (las::SetVolChannel player tracknum vol)
  (setf (car (gethash tracknum *init-settings-table*)) vol)  )




;;; !!!!
(defun om-set-track-pan (player tracknum pan)
  (when (gethash tracknum *tracks-list*)
    (let ((nchnls (las::GetChannelsSound (gethash tracknum *tracks-list*))))
      (case nchnls 
        (1 (las::SetPanChannel player tracknum pan pan))
        (2 (let ((pan2 (pan2panpan pan)))
             (las::SetPanChannel player tracknum (car pan2) (cadr pan2))))
        (otherwise nil))
      (setf (cadr (gethash tracknum *init-settings-table*)) pan)
      ))
  )



;;;; ---------------------
;;; TEST PLAY
(defun om-play-one-sound (snd player)
  (om-reset-player player)
  (let ((trk (gethash 16 oa::*tracks-list*)))
    (if trk
        (setf (gethash 16 oa::*tracks-list*) (las::MakeMixSound trk snd))
      (setf (gethash 16 oa::*tracks-list*) snd))
    )
  (om-start-player player))


