;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;Define a general class to PLAY.
;Last Modifications :
;17/11/97 first date.
;DocFile


(in-package :om)

;==================================================
;Play box
;==================================================

(defmethod* Play ((self t) &key (player t))
   :initvals '(nil nil 2 nil nil) 
   :indoc '("object" "a player designator" "micro interval approx" "selection in object" "") 
   :icon 207
   :doc "Plays any OM Musical object.

<player> designates a particular player (t = dispatch automatically) 
<approx> sets the temperament (2, 4, 8) in the case of MIDI player
<interval> allows to select a time interval to play '(begin[ms] end[ms])
<port> plays to a particular MIDI port.
"
   (declare (ignore approx port))
   (general-stop *general-player*)
   nil)
   
(defmethod* Stop ((setf t))
  :icon 229
  (general-stop *general-player*))


(defmethod* Play ((self simple-container) &key (player nil))
   (call-next-method)
   (when (play-obj? self)
     (player-schedule  *general-player*
                      self 
                      (or player (car (players-for-object self))) 
                      :at (get-player-time *general-player*)))
   (general-play *general-player*))


(defmethod* Play ((self list) &key (player t) (approx 2) interval port)
   (call-next-method)
   (loop for obj in self do
         (Play obj :player player)))




;==================================================
;PrepareTo Play allows to recursively build low-level structures for the players
;Can be (shall be) specialized for objects and players
;==================================================

(defmethod PrepareToPlay ((player t) (self t) at &key  approx port interval voice)
  (declare (ignore approx seq port))
  nil)

(defmethod PrepareToPlay ((player t) (list list) at &key  approx port interval voice)
    (loop for object in list
         collect (PrepareToPlay player object at 
                           :approx approx 
                           :port port
                           :interval interval
                           :voice voice)))

;=== general containers play all sub components
(defmethod PrepareToPlay ((player t) (self container) at &key approx port interval voice)
  (remove nil
          (loop for sub in (inside self) collect
                (let ((objstart (+ at (offset->ms sub))))
                  (let ((in-interval (or (null interval)
                                         (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
                    (when in-interval 
                      (PrepareToPlay player sub objstart 
                                     :approx approx 
                                     :port port
                                     :interval interval
                                     :voice voice)))
                  ))))

;;; FOR THE MAQUETTE CONTENTS
;;; check if we need to instanciate a specific track for this object
(defmethod obj-in-sep-track ((self t)) nil)
(defmethod obj-in-sep-track ((self simple-container)) t)

(defmethod player-from-params (paramkey) nil)
(defmethod player-from-params ((paramkey symbol)) paramkey)


;;; WARNING: *def-midi-out* makes this dependent from the MIDI project
(defmethod PrepareToPlay ((player t) (self maquette-obj) at &key approx port interval voice)
   (declare (ignore approx port))
   (let ((i 0))
     (loop for object in (inside self)
           for param in (param-list self) do
           (let* ((objstart (+ at (offset->ms object)))
                  (track (or voice (if (obj-in-sep-track object) (setf i (+ i 1)) 0)))
                  (pl (player-from-params (cdr (assoc 'player param))))
                  (in-interval (or (null interval) 
                                   (interval-intersec interval (list objstart (+ objstart (get-obj-dur object)))))))
             (when in-interval 
               ;(print (list object (get-obj-dur object) newinterval interval))
               (PrepareToPlay pl
                              object objstart
                              :approx (cdr (assoc 'approx param))
                              :port (case (cdr (assoc 'outport param))
                                      (:default *def-midi-out*)
                                      (t (cdr (assoc 'outport param))))
                              :interval interval
                              :voice track)))
               )))


(defmethod PrepareToPlay ((player t) (self ommaquette) at &key approx port interval voice)
  (PrepareToPlay player (value self) at
                 :approx approx
                 :port port
                 :interval interval
                 :voice voice))













