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
;=========================================================================

(in-package :om)

;;;=====================
;;; REGISTERING PLAYER 
;;;=====================

(defvar *enabled-players* nil)  

(defun enable-player (player)
  (unless (find player *enabled-players*)
    (pushr player *enabled-players*)))

(defun disable-player (player)
  (when (find player *enabled-players*)
    (setf *enabled-players* (remove player *enabled-players*))))


;(defun add-player-for-object (type player)
;  (let* ((curlist (players-for-object (make-instance type)))
;              (newlist (if (listp player) 
;                           (remove-duplicates (append curlist player))
;                         (pushnew player curlist))))
;    (eval `(defmethod players-for-object ((self ,type)) ',newlist))))

(defvar *player-assignations* nil)

(defun add-player-for-object (type player)
  (if (find type *player-assignations* :key 'car)
    (let* ((pos (position type *player-assignations* :key 'car))
                (players (cadr (nth pos *player-assignations*))))
      (setf (cadr (nth pos *player-assignations*)) (remove-duplicates (append players (list! player)))))
    (push (list type (list! player)) *player-assignations*)))

;;; DEFAULT ASSIGNMENTS
;;; defined here and there for the different OM classes
(defmethod players-for-object ((self t)) 
  (let ((cl-list (mapcar 'class-name (get-class-precedence-list (class-of self)) ))
        (players nil))
      (loop for cl in cl-list while (not players) do
            (setf players (cadr (find cl *player-assignations* :key 'car))))
      players))

(defmethod enabled-players-for-object ((self t))
  ;;; intersection does not preserve the original order
  (loop for p in (players-for-object self)
        when (find p *enabled-players*)
        collect p))


(defun close-players ()
  (mapcar 'player-close *enabled-players*))

;;; open is called in the init-OM-session
(om-add-exit-cleanup-func 'close-players)


;;;=====================
;;; SUB-PLAYERS (AKA "ENGINES")
;;;=====================

;;; METHODS TO REDEFINE FOR EVERY PLAYER                   
(defmethod player-name ((player t)) "XXX")   ;;; A short name
(defmethod player-desc ((player t)) "undefined player")   ;;; a description
(defmethod player-special-action ((player t)) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player t)) nil)   ;;; the default values for the player params
(defmethod player-type ((player t)) nil)   ;;; communication protocol (:midi / :udp)

;;; CALLED AT STARTUP
(defmethod player-open ((player t)) t)  
;;; CALLED AT EXIT
(defmethod player-close ((player t)) t)  

;;; CALLED WHEN SELECTED
(defmethod player-init ((self t)) nil)

;;; IN THE EDITORS
(defmethod make-player-specific-controls ((self t) control-view) nil)

;;;=============================================
;;; DEPENDING ON PLAYER-TYPE, ONE CAN DEFINE OPTIONS THAT APPEAR ALONG WITH PLAYER SELECTION
;;; CURRENTLY IMPLEMENTED FOR :MIDI
;;;=============================================

;;; REDEFINE FOR TYPE = :MIDI, :UDP etc.
(defmethod player-selection-settings-pane ((type t) paneports reference selected-player) NIL)
(defmethod set-param-from-settings-pane ((type t) paneports reference) NIL)

;;;=============================================
;;; GENERAL PLAYER SELECTION INTERFACE
;;;=============================================

;;; FOR THE REFERENCE IF IT IS NOT AN EDITOR
(defmethod update-controls-view ((self t)) nil)

;; called by 'reference' (e.g. Box or Editor) to change the player
;; reference maty have stored options for the other players as well
;; this functions manages all the edition-params settings in reference but not the possible extra actions to perform after these changes

(defmethod reference-object ((self t)) (object self))

(defun select-player (reference)
  (let* ((players-in-dialog (enabled-players-for-object (reference-object reference)))
         
         (dialog (om-make-window 'om-dialog
                                 :window-title (string+ "Player Settings for " (name reference))
                                 :position :centered
                                 :size (om-make-point 690 (+ 120 (* (length players-in-dialog) 50)))
                                 :maximize nil :resizable nil
                                 :font *om-default-font4*
                                 :bg-color (om-make-color 0.623 0.623 0.623)))
          
          ;(midi? (find :midi players-in-dialog :key 'player-type))
          ;(udp? NIL) ;;; (find :udp players-in-dialog :key 'player-type))
         (player-types (remove-duplicates players-in-dialog :key 'player-type))
         
         (paneplayer (om-make-view 'om-view :bg-color *om-white-color*
                                   :position (om-make-point 10 40) :size (om-make-point 320 (+ 20 (* (length players-in-dialog) 50)))))
         (paneports (om-make-view 'om-view :bg-color *om-white-color*
                                  :position (om-make-point 350 40) :size (om-make-point 320 170)))
         (y 10) (y2 10)
         (selected-player (get-edit-param reference 'player)))

      (om-add-subviews dialog
                       (om-make-dialog-item 'om-static-text (om-make-point 10 y) (om-make-point 300 20) 
                                            (if players-in-dialog (string+ "Select a player mode for " (name reference) " :")
                                              (string+ "No player available for " (name reference) "..."))
                                            :font *om-default-font1b*))
      
      (player-selection-settings-pane (player-type selected-player) paneports reference selected-player)
      
      (loop for pl in players-in-dialog do
            (om-add-subviews paneplayer
                             (om-make-dialog-item 'om-radio-button (om-make-point 10 y)
                                                  (om-make-point 300 20) (player-name pl)
                                                  :checked-p (equal pl selected-player)
                                                  :di-action (let ((p pl))
                                                               (om-dialog-item-act item
                                                                 (declare (ignore item))
                                                                 (setf selected-player p)
                                                                 (player-selection-settings-pane (player-type selected-player) 
                                                                                                paneports reference 
                                                                                                selected-player)
                                                                 ))
                                                  :font *om-default-font2*)
                             ;(om-make-dialog-item 'om-static-text (om-make-point 40 (+ y 20)) (om-make-point 160 20) 
                             ;                     (string+ "type: " (if (player-type pl) (symbol-name (player-type pl)) "undefined"))
                             ;                     :font *om-default-font1*)
                             (om-make-dialog-item 'om-static-text (om-make-point 40 (+ y 20)) (om-make-point 280 60) 
                                                  (player-desc pl)
                                                  :font *om-default-font1*)
                             )
            (incf y 50))
                             
      (incf y 60)

      (om-add-subviews dialog
                       paneplayer paneports
                       (om-make-dialog-item 'om-button (om-make-point 170 y) (om-make-point 80 24) "Cancel" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog nil)))
                       (om-make-dialog-item 'om-button (om-make-point 260 y) (om-make-point 80 24) "OK" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (set-edit-param reference 'player selected-player)
                                                         (set-param-from-settings-pane (player-type selected-player) paneports reference)
                                                         (om-return-from-modal-dialog dialog selected-player)
                                                         )
                                            :default-button t))
      (om-modal-dialog dialog)))





