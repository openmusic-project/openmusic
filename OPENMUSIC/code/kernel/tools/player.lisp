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

(defclass general-player ()
   ((playing-etat :initform "Idle" :accessor playing-etat)
    (list-to-play :initform nil :accessor list-to-play)
    (playing-list :initform nil :accessor playing-list)
    (cursor-pos :initform 0 :accessor cursor-pos)
    (delta-cursor-pos :initform 0 :accessor delta-cursor-pos)
    (recording-view :initform nil :accessor recording-view)
    (pausetime :initform 0 :accessor pausetime)
    (loopplay? :initform nil :accessor loopplay?)
    (forlooplist :initform nil :accessor forlooplist)
    (start-time :initform 0 :accessor start-time)
    (playedobjdur :initform 0 :accessor playedobjdur)
    (assoc-players :initform nil :accessor assoc-players)))

;etats "Idle" "Playing" "Pause"

(defvar *general-player* (make-instance 'general-player))

(defmethod Idle-p ((self general-player))
  (and (string-equal (playing-etat self) "Idle")
       (Idle-p (assoc-players self))))


(defmethod reset-player ((self t)  &optional view) nil)

(defmethod reset-player ((self general-player) &optional view)
   (setf (playing-etat self) "Idle")
   (setf (playing-list self) nil)
   (setf (forlooplist self) nil)
   (setf (list-to-play self) nil)
   (loop for player in (assoc-players self) do
         (reset-player player))
   (if (equal view (recording-view *general-player*))
     (setf (recording-view self) nil))
   (setf (cursor-pos self) 0)
   (setf (delta-cursor-pos self) 0)
   (setf (playedobjdur self) 0)
   (setf (pausetime self) 0)
   (reset-scheduler))

(defmethod get-player-etat ((self general-player))
   (playing-etat self))


(defmethod set-player-etat ((self general-player) etat)
   (setf (playing-etat self) etat)) 

(defmethod add-assoc-player ((self general-player) newplayer)
  (unless (member newplayer (assoc-players self))
    (pushr newplayer (assoc-players self))))

(defmethod remove-assoc-player ((self general-player) player)
  (remove player (assoc-players self)))

(defmethod Play-player ((self general-player))
   (when (or (list-to-play self) (assoc-players self))
     (setf (playing-etat self) "Playing")
     (setf (cursor-pos self) 0)
     (setf (pausetime self) 0)
     (setf (forlooplist self) (copy-list (list-to-play self)))
     (loop for player in (assoc-players self) do
           (play-player player))
     ;; test...
     (om-with-error-handle 
       (om-without-interrupts
         (start
           (loop for item in (list-to-play self) do
                 (let ((start (or (car (third item)) 0))
                       (end (or (second (third item)) (extent (first item)))))
                   ;(setf (cursor-pos self) start)
                   (dfuncall (second item) 'DoPlay (first item) start end)
                   (dfuncall (+ (second item) end) 'DoStop (first item))
                   ))
           (dfuncall (playedobjdur *general-player*) 'FinalPlay)
           )
         ))
     (setf (start-time self) (real-time)))
   )

(defmethod Continue-Player ((self general-player))
   (setf (playing-etat self) "Playing")
   (setf (start-time self) (real-time))
   (loop for item in (playing-list self) do
         (DoContinue item))
   (loop for player in (assoc-players self) do
           (Continue-player player))
   (start
     (loop for item in (list-to-play self) do
           (dfuncall (- (second item) (pausetime self)) 'DoPlay (first item) 0 (extent (first item))))
     (dfuncall (- (playedobjdur self) (pausetime self)) 'FinalPlay)))

(defmethod Pause-Player ((self general-player))
   (setf (playing-etat self) "Pause")
   (setf (pausetime self) (+ (pausetime self) (- (real-time) (start-time self))))
   (loop for item in (playing-list self) do
         (DoPause item))
   (loop for player in (assoc-players self) do
           (Pause-player player))
   (reset-scheduler))

(defmethod Stop-Player ((self general-player) &optional view)
   (if (and view (equal view (recording-view self)))
     (doStopRecord (recording-view self))
     ;;; test
     (om-with-error-handle 
       ;(om-without-interrupts 
         (loop for item in (playing-list self) do
               (DoStop item))
         (loop for player in (assoc-players self) do
            (Stop-player player view))
        ;  )
     ))
   (reset-player self view))

(defmethod InitPlayingSeq ((self general-player) dur &key port)
  (loop for player in (assoc-players self) do
        (InitPlayingSeq player dur :port port)))

(defmethod InitPlayingSeq ((self t) dur &key port) nil)


(defmethod FinalizePlayingSeq ((self general-player) dur &key port)
  (loop for player in (assoc-players self) do
        (FinalizePlayingSeq player dur :port port)))

(defmethod FinalizePlayingSeq ((self t) dur &key port) nil)

(defmethod incf-cursor-pos ((self general-player))
   (setf (cursor-pos self) (round (+ (pausetime self) (- (real-time) (start-time self)) (delta-cursor-pos self)))))

(defun Finalplay ()
  (if (loopplay? *general-player*)
      (when (or (forlooplist *general-player*) (assoc-players *general-player*))
        (setf (playing-etat *general-player*) "Playing")
        (setf (cursor-pos *general-player*) 0)
        (setf (pausetime *general-player*) 0)
        (setf (list-to-play *general-player*) (copy-list (forlooplist *general-player*)))
        (loop for player in (assoc-players *general-player*) do
              (play-player player))
        (om-without-interrupts
        (om-with-error-handle 
          (loop for item in (list-to-play *general-player*) do
                (let ((start (or (car (third item)) 0))
                      (end (or (second (third item)) (extent (first item)))))
                  (dfuncall (second item) 'DoPlay (first item) start end)))
          (dfuncall (playedobjdur *general-player*) 'FinalPlay)
          )
          )
        (setf (start-time *general-player*) (real-time)))
    (progn
      (setf (playing-etat *general-player*) "Idle")
      (if (and *palette* (editor-assoc *palette*))
          (palette-act *palette* 1)
        (progn
          (stop-player *general-player*)                   
          (loop for player in (assoc-players *general-player*) do
                (stop-player player))
          ))
      )))

(defmethod Idle-p ((self list))
   (let ((rep t))
     (loop for item in self
           while rep do
           (unless (isidle? item)
             (setf rep nil)))
     rep))


(defmethod DoStopRecord ((self t)) t)

(defmethod isidle? ((self t)) t)

(defmethod DoStop ((self t)) t)

(defmethod DoPause ((self t)) t)

(defmethod DoContinue ((self t)) t)

(defmethod DoPlay ((self t) start end) 
   (when (string-equal (playing-etat *general-player*) "Playing")
     (setf (list-to-play *general-player*) (remove self (list-to-play *general-player*) :test 'equal :key 'car))
     (push self (playing-list *general-player*))
     t))


(defmethod PlayAny ((player t) (object t) &key (approx nil) (port nil) (interval nil))
  (if  (Idle-p *general-player*)
      (let ((objdur (get-obj-dur object))
            (avance (real-duration object 0)) startevent finalevent)
        (when (second interval)
          (setf objdur (- (second interval) (first interval))))
        (setf avance (if (< avance 0) (abs avance) 0))
        ;; (reset-player *general-player*)
        (if (> objdur 0)
            (progn
              (setf (playedobjdur *general-player*) objdur)
              (InitPlayingSeq *general-player* objdur)
              (PrepareToPlay player object avance 
                             :approx approx
                             :port port
                             :interval interval)
              (FinalizePlayingSeq *general-player* objdur)
              (play-player *general-player*))
          (progn
            (stop-player *general-player*)
            (reset-player *general-player*)
            (palette-restore-stop)
            )))
    (om-beep-msg  "Wait end of previous play !")))



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
         (push (list self (- at (first interval)) (loop for pos in newinterval collect (- pos at)))
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


(defmethod* PrepareToPlay ((player t) (self listtoplay) at &key  approx port interval voice)
   (declare (ignore approx))
   (loop for object in (thelist self)
         for param in (params self)
         do 
         ;(print (list (cdr (assoc 'player param)) object))
         (PrepareToPlay (player-from-params (cdr (assoc 'player param)))
                        object at 
                        :approx (cdr (assoc 'approx param)) 
                        :interval interval
                        :port (case (cdr (assoc 'outport param))
                                (:default *outmidiport*)
                                (t (cdr (assoc 'outport param))))
                        :voice voice)))

(defun verify-port (port) t)

;===================
;Palette

(defclass playing-palette (general-palette) 
   ((whoplay :initform nil :accessor whoplay)
    (pause-push-p :initform nil :accessor pause-push-p)
    (buttons :initform nil :initarg :buttons :accessor buttons))
   (:default-initargs :delta-inpix (om-make-point 175 0))
   )

;(defclass playing-palette-view (palette-view) ())
;(defmethod palette-view-class ((self playing-palette)) 'playing-palette-view)


(defmethod init-play-palette ((self editorview))
  (let ((pict (get-palette-pict  self)))
    (om-set-view-size *palette-win* (om-add-points (om-get-picture-size *play-controls-pict*)
                                                   (om-make-point (+ 25 (if pict (om-point-h (om-get-picture-size pict)) 0))
                                                                  (if (buttons *palette*) 25 0))))))

(defmethod stop-palette ((self t))
  (setf (whoplay self) nil))


(defmethod draw-more-palette ((self playing-palette) view)
  (if (and (editor-assoc self) (equal (panel (editor-assoc self)) (recording-view *general-player*)))
      (setf (whoplay self) 3))
  (om-with-focused-view view
    (om-draw-picture view *play-controls-pict*)
    (when (whoplay self)
      (push-button view (whoplay self)))
    (when (pause-push-p self)
      (push-button view 2))
    (when (and (editor-assoc self) (loopplay? (panel (editor-assoc self))))
      (push-button view 5))))

(defmethod click-button-inpal ((self Playing-Palette) x)
  (unless (equal x 6)
    (call-next-method)))

(defmethod palette-act ((self Playing-Palette) x)
  (when (editor-assoc self)
   (let ((ed-view (panel (editor-assoc self))))
   (when (<= x 5) 
     (case x
       ;PLAY
       (0 (unless (whoplay self)
            (setf (whoplay self) 0)
            (play-from-palette ed-view)
            ))
       ;STOP
       (1 
        (setf (whoplay self) nil
              (pause-push-p self) nil)
        (Stop-from-palette ed-view))
       ;PAUSE
       (2 
        (when (whoplay self)
          (if (string-equal (get-player-etat *general-player*) "Playing")
            (setf (pause-push-p self) t)
            (setf (pause-push-p self) nil))
          (pause-from-palette ed-view)))
       ;RECORD
       (3 (unless (whoplay self)
            (setf (whoplay self) 3)
            (unless (record-from-palette ed-view)
              (setf (whoplay self) nil))))
       ;PLAY SELECTION
       (4 (when (selection-to-play-? ed-view)
            (unless (whoplay self)
              (setf (whoplay self) 4)
              (play-selection-from-palette ed-view))))
       ;LOOP
       (5 (push-loop-button ed-view)))
     (om-invalidate-view (view *palette-win*))
     t))))


;============Views which play, show cursor and allow selection

(defclass cursor-play-view-mixin (om-view-cursor-play) 
   ((loopplay? :initform nil :accessor loopplay?)
    (recording? :initform nil :accessor recording?)
    (cursor-interval :initform '(0 0) :accessor cursor-interval)
    (cursor-p :initform nil :accessor cursor-p)
    (cursor-pos :initform 0 :accessor cursor-pos)
    ))


(defmethod new-interval-cursor ((self cursor-play-view-mixin) where)
  (om-init-motion-functions self 'interval-select-action 'release-interval-select)
  (om-new-movable-object self (om-point-h where) 0 4 (h self) 'om-selection-rectangle))

(defmethod interval-select-action ((self cursor-play-view-mixin) pos)
 (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (om-v-scroll-position self) 
                                (max 4 (third rect)) (om-point-v (om-interior-size self))))))

(defmethod release-interval-select ((self cursor-play-view-mixin) pos)  
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
         (minpixel 2)  position)
    (when rect
      (om-erase-movable-object self)
      (setf position (if (>  (third rect)  minpixel)
                         (list (car  rect) (+ (car rect) (third rect) ) )
                       (car rect)))
      (if (listp position)
          (progn
            (setf (cursor-interval self) position)
            (setf (cursor-interval self) (convert-interval self)))
        (progn
          (setf (cursor-interval self) '(0 0))
          (setf (cursor-pos self) (max 0 (om-point-h (pixel2point self (om-make-point position 0)))))))
      (om-invalidate-view self) )))

;--------------


(defmethod convert-interval ((self cursor-play-view-mixin))
  ;;; attention ici on fait des copies de tous les sound files de la maquette...
   (let* ((obj (car (get-obj-to-play self)))
          (dur (get-obj-dur obj))
          (int (cursor-interval self))  
          x x1
          rep)
     
     (if (listp int)
       (setf x (max 0 (om-point-h (pixel2point self (om-make-point (car int) 0))))
             x1 (min dur (om-point-h (pixel2point self (om-make-point (second int) 0)))))
       (setf x (max 0 (om-point-h (pixel2point self (om-make-point int 0)))) 
             x1 (get-obj-dur obj)
             ))

     (setf (cursor-pos self) x)
     
       (setf rep (if (<= x x1)
                   (list x x1)
                   (list x1 x)))
       (if (< (car rep) dur)
         rep
         '(0 0))
       ))


(defmethod draw-interval-cursor ((self cursor-play-view-mixin))
   (let* ((sys-etat (get-system-etat self))
          (interval (cursor-interval self))
          (pixel-interval (list (om-point-h (point2pixel  self (om-make-big-point (car interval) 0) sys-etat))
                                (om-point-h (point2pixel  self (om-make-big-point (second interval) 0) sys-etat)))))
     (om-with-focused-view self
       (draw-h-rectangle (list (car pixel-interval) 0 (second pixel-interval) (h self)) t))
     ))

(defmethod set-unset-cursor ((self cursor-play-view-mixin))
   (setf (cursor-p self) (not (cursor-p self)))
   (om-invalidate-view self t))



;;; new pixel = position-x ou il faut dessiner (defaut = current pos)
;;; si draw? dessine une ligne verticale en xor
(defmethod draw-line-cursor ((self cursor-play-view-mixin) &key newpixel (draw? t))
  (unless newpixel
    (setf newpixel (om-point-h (point2pixel self (om-make-big-point (cursor-pos *general-player*) 0) (get-system-etat self)))))
  (when draw? 
    (om-update-movable-cursor self newpixel 0 4 (h self)))
   newpixel)


(defmethod stop-play-on-palette ((self t)) nil)

(defmethod stop-play-on-palette ((self playing-palette))
  (setf (whoplay self) nil)
  (when *palette-win* 
    (om-invalidate-view (view *palette-win*) t)))


;;; play from palette : draw? = nil
(defmethod draw-play-cursor ((self cursor-play-view-mixin) st end &optional (draw? t))
  (when (and (om-view-window self) (om-window-open-p (om-view-window self)))
    (let* ((change-win t) (newpixel 0))
      (setf newpixel (draw-line-cursor self :draw? draw?))
      (mapc #'(lambda (view) 
                (draw-line-cursor view :draw? t))
            (attached-cursor-views self))
      (when (and (> newpixel (+ (w self) (om-h-scroll-position self)))
                 (< (cursor-pos *general-player*) end))
        (setf newpixel 0 
              change-win nil)
        (scroll-play-window self))
      (incf-cursor-pos *general-player*)
      (if (Idle-p *general-player*)
          (let ((palette *palette*))
            (stop-play-on-palette palette)
            (om-erase-movable-cursor self)
            (mapc #'(lambda (view) 
                      (om-erase-movable-cursor view))
                  (attached-cursor-views self))
            )
        (dfuncall #+win32 100 #-win32 50 'draw-play-cursor self st end change-win)))))


(defmethod scroll-play-window ((self cursor-play-view-mixin)) 
  ;(om-without-interrupts
   (setf (rangex self) (list (cursor-pos *general-player*) 
                             (+ (cursor-pos *general-player*) 
                                (- (second (rangex self)) (first (rangex self))))))
  (change-view-ranges self)
  (om-redraw-view self)
  (om-redraw-view (rulerx self))
  );)

(defmethod get-selection-to-play ((self cursor-play-view-mixin))
   (let ((interval (cursor-interval self)))
     (values  (list (object (om-view-container self))
                    :interval interval)
              (first interval)
              (second interval))))

(defmethod get-obj-to-play ((self cursor-play-view-mixin))
   (list (object (om-view-container self))))

(defmethod pause-from-palette ((self cursor-play-view-mixin))
   (setf (loopplay? *general-player*) (loopplay? self))
   (cond ((string-equal (get-player-etat *general-player*) "Playing")
          (Pause-Player *general-player*))
         ((string-equal (get-player-etat *general-player*) "Pause")
          (Continue-Player *general-player*)
          (om-erase-movable-cursor self )
          (om-new-movable-cursor self 0 0 4 (h self) 'om-cursor-line)
          (start (draw-play-cursor self 0 100000)))))


(defmethod start-position ((self cursor-play-view-mixin)) 
  (or (cursor-pos self) 0))

(defmethod attached-cursor-views ((self cursor-play-view-mixin)) nil)

(defmethod scroll-to-0 ((self cursor-play-view-mixin)) t)


(defmethod play-from-palette ((self cursor-play-view-mixin))
  (when (Idle-p *general-player*)
    (setf (loopplay? *general-player*) (loopplay? self))
    (let ((obj (get-obj-to-play self)))
      (when (and (scroll-to-0 self) (not (= 0 (om-h-scroll-position self))))
        (om-set-scroll-position self (om-make-point 0 (om-v-scroll-position self)))
        (mapc #'(lambda (view) (om-invalidate-view view)) (attached-cursor-views self)))
      (if (allowed-in-maq-p (car obj))
           (progn 
             (om-erase-movable-cursor self)
             (om-new-movable-cursor self (start-position self) 0 4 (h self) 'om-cursor-line)
             (mapc #'(lambda (view) (om-new-movable-cursor view (start-position self) 0 4 (h self) 'om-cursor-line))
                   (attached-cursor-views self))
             (draw-line-cursor self :draw? t)
             (apply 'PlayAny (cons (get-score-player self) obj))
             (start (draw-play-cursor self 0 (get-obj-dur obj) t))
             )
        (palette-act *palette* 1)
        ))))



(defmethod play-selection-from-palette ((self cursor-play-view-mixin))
   (setf (loopplay? *general-player*) (loopplay? self))
   (multiple-value-bind (obj start end) (get-selection-to-play self)
     (when obj 
       (when (and (scroll-to-0 self) (not (= 0 (om-h-scroll-position self)))
               (or (< (car interval) (get-ms-pos self (om-h-scroll-position self) (staff-zoom self)))
                   (> (car interval) (get-ms-pos self (+ (om-h-scroll-position self) (w self)) (staff-zoom self)))))
        (om-set-scroll-position self (om-make-point 0 (om-v-scroll-position self)))
        (mapc #'(lambda (view) (om-invalidate-view view)) (attached-cursor-views self)))
       (let ((s (om-point-h (point2pixel self (om-make-point start 0) (get-system-etat self))))) ; (start-position self)))
       (setf (delta-cursor-pos *general-player*) start)
       (om-new-movable-cursor self s 0 4 (h self) 'om-cursor-line)
       (mapc #'(lambda (view) (om-new-movable-cursor view s 0 4 (h self) 'om-cursor-line))
             (attached-cursor-views self))
       (apply 'PlayAny (cons (get-score-player self) obj))
       (start (draw-play-cursor self start end t))
       ))))

(defmethod stop-from-palette ((self cursor-play-view-mixin))
  (om-erase-movable-cursor self)
  (mapc #'om-erase-movable-cursor (attached-cursor-views self))
  (Stop-Player *general-player* self))




(defmethod push-loop-button ((self cursor-play-view-mixin))
   (setf (loopplay? self) (not (loopplay? self)))
   (setf (loopplay? *general-player*) (loopplay? self)))


(defmethod panel-record ((self cursor-play-view-mixin)) t)
(defmethod allow-record ((self cursor-play-view-mixin)) nil)

(defmethod record-from-palette ((self cursor-play-view-mixin)) 
  (if (allow-record self)
      (when  (panel-record self)
        (setf (recording-view *general-player*) self)
        (setf (recording? self) t))
    (om-beep-msg "This editor does not allow recording.")))

(defmethod selection-to-play-? ((self cursor-play-view-mixin))
  (and (cursor-p self) 
       (cursor-interval self) 
       (not (= (car (cursor-interval self)) (cadr (cursor-interval self))))
       ))








