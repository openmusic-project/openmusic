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

(defmethod* Play ((self t) &key (player t) (approx 2) interval port)
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
   nil)
   

(defmethod* Play ((self simple-container) &key (player t) (approx 2) interval port)
   (setf port (verify-port port))
   (PlayAny player self :approx approx :port port :interval interval))

(defmethod* Play ((self list) &key (player t) (approx 2) interval port)
   (when self
     (setf port (verify-port port))
     (PlayAny player self :approx approx :port port :interval interval)))

(defmethod* Stop ((setf t))
  :icon 229
  (Stop-Player *general-player*))



;==================================================
;Prepare to play
;==================================================

(defmethod* PrepareToPlay ((player t) (self t) at &key  approx port interval voice)
   :initvals '(nil 0 2 nil nil) 
   :indoc '("object" "start time" "approx" "port" "selection") 
   :icon 207
   :doc "use to redifine Play for new classes, see the manual"
   (declare (ignore approx seq port))
   (if interval
     (let ((newinterval (interval-intersec interval (list at (+ at (get-obj-dur self))))))
       (when newinterval
         (push (list self (- at (first interval)) (om- newinterval at))
               (list-to-play *general-player*))))
     (push (list self at interval) (list-to-play *general-player*))) t)

(defmethod obj-in-sep-track ((self t)) nil)
(defmethod obj-in-sep-track ((self simple-container)) t)

(defmethod player-from-params (paramkey) nil)
(defmethod player-from-params ((paramkey symbol)) (interne paramkey))

;(defmethod player-from-params ((paramkey (eql :midishare))) 'midishare)
;(defmethod player-from-params ((paramkey (eql :microplayer))) 'microplayer)
;(defmethod player-from-params ((paramkey (eql :multiplayer))) 'multiplayer)

(defmethod* PrepareToPlay ((player t) (self maquette-obj) at &key approx port interval voice)
   (declare (ignore approx port))
   (let ((i 0))
     (loop for object in (inside self)
           for param in (param-list self) do
           (let ((objstart (+ at (offset->ms object)))
                 (track (or voice (if (obj-in-sep-track object) (setf i (+ i 1)) 0)))
                 (pl (player-from-params (cdr (assoc 'player param)))))
             
             ;(print (list object pl (cdr (assoc 'player param))))
             (if interval
                 (let ((newinterval (interval-intersec interval 
                                                       (list objstart 
                                                             (+ objstart (get-obj-dur object))))))
                   (when newinterval 
                     ;(print (list object (get-obj-dur object) newinterval interval))
                     (PrepareToPlay pl
                                    object objstart
                                    :approx (cdr (assoc 'approx param))
                                    :port (case (cdr (assoc 'outport param))
                                            (:default *outmidiport*)
                                            (t (cdr (assoc 'outport param))))
                                    :interval interval
                                    :voice track)))
               (PrepareToPlay pl
                              object objstart
                              :approx (cdr (assoc 'approx param))
                              :port (case (cdr (assoc 'outport param))
                                      (:default *outmidiport*)
                                      (t (cdr (assoc 'outport param))))
                              :voice track))))))

(defmethod* PrepareToPlay ((player t) (list list) at &key  approx port interval voice)
   (setf port (verify-port port))
   (loop for object in list
         do (PrepareToPlay player object at 
                           :approx approx 
                           :port port
                           :interval interval
                           :voice voice)))

;=== general containers play all sub components

(defmethod* PrepareToPlay ((player t) (self container) at &key approx port interval voice)
   ;(setf port (verify-port port))
   (loop for sub in (inside self) do
         (let ((objstart (+ at (offset->ms sub))))
          ;(print (list player sub))
           (if interval
             (let ((newinterval (interval-intersec interval 
                                                   (list objstart (+ objstart (get-obj-dur sub))))))
               (when newinterval
                 (PrepareToPlay player sub objstart 
                                :approx approx 
                                :port port
                                :interval interval
                                :voice voice)))
             (PrepareToPlay player sub objstart 
                            :approx approx 
                            :port port
                            :voice voice)))))


(defmethod* PrepareToPlay ((player t) (self t) at &key approx port interval voice) nil)

;(defmethod* PrepareToPlay ((player t) (self listtoplay) at &key  approx port interval voice)
;   (declare (ignore approx))
;   (loop for object in (thelist self)
;        for param in (params self)
;         do 
;         ;(print (list (cdr (assoc 'player param)) object))
;         (PrepareToPlay (player-from-params (cdr (assoc 'player param)))
;                        object at 
;                        :approx (cdr (assoc 'approx param)) 
;                        :interval interval
;                        :port (case (cdr (assoc 'outport param))
;                                (:default *outmidiport*)
;                                (t (cdr (assoc 'outport param))))
;                        :voice voice)))

(defun verify-port (port) t)

(defmethod get-obj-to-play ((self cursor-play-view-mixin))
   (list (object (om-view-container self))))

(defmethod selection-to-play-? ((self cursor-play-view-mixin))
  (and (cursor-p self) 
       (cursor-interval self) 
       (not (= (car (cursor-interval self)) (cadr (cursor-interval self))))
       ))



;(defmethod convert-interval ((self cursor-play-view-mixin))
  ;;; attention ici on fait des copies de tous les sound files de la maquette...
;   (let* ((obj (car (get-obj-to-play self)))
;          (dur (get-obj-dur obj))
;          (int (cursor-interval self))  
;          x x1
;          rep)
;     
;     (if (listp int)
;       (setf x (max 0 (om-point-h (pixel2point self (om-make-point (car int) 0))))
;             x1 (min dur (om-point-h (pixel2point self (om-make-point (second int) 0)))))
;       (setf x (max 0 (om-point-h (pixel2point self (om-make-point int 0)))) 
;             x1 (get-obj-dur obj)
;             ));;
;
;     (setf (cursor-pos self) x)
;     
;       (setf rep (if (<= x x1)
;                   (list x x1)
;                   (list x1 x)))
;       (if (< (car rep) dur)
;         rep
;         '(0 0))
;       ))


;(defmethod scroll-play-window ((self cursor-play-view-mixin)) 
  ;(om-without-interrupts
;   (setf (rangex self) (list (cursor-pos *general-player*) 
;                             (+ (cursor-pos *general-player*) 
;                                (- (second (rangex self)) (first (rangex self))))))
;  (change-view-ranges self)
;  (om-redraw-view self)
;  (om-redraw-view (rulerx self))
;  );)










