(in-package :om)

;;;=================================
;;; THE PLAYER
;;;=================================
(defclass omplayer () 
  ((state :accessor state :initform :stop)    ; :play :pause :stop :record
   (loop-play :accessor loop-play :initform nil)
   (start-time :accessor start-time :initform 0)
   (player-offset :accessor player-offset :initform 0)
   (ref-clock-time :accessor ref-clock-time :initform 0)
   (caller :initform nil :accessor caller :initarg :caller)
   (callback-fun :initform nil :accessor callback-fun :initarg :callback-fun)
   (callback-process :initform nil :accessor callback-process)
   (callback-tick :initform 0.01 :accessor callback-tick :initarg :callback-tick)
   (stop-fun :initform nil :accessor stop-fun :initarg :stop-fun)))

(defmethod class-from-player-type ((type t)) 'omplayer)

(defmethod make-player-specific-controls ((self omplayer) control-view) nil)

(defmacro get-player-time (player)
  `(cond ((equal (state ,player) :play)
          (+ (player-offset ,player) (start-time ,player) (- (clock-time) (ref-clock-time ,player))))
         ((equal (state ,player) :pause)
          (+ (player-offset ,player) (start-time ,player)))
         (t 0)))

(defmethod player-init ((self omplayer)) t)

(defmethod idle-p ((self omplayer)) 
  (not (equal (state self) :play)))

(defmethod player-play ((player omplayer) obj &key interval)
  (cond ((equal (state player) :play) nil)
        ((equal (state player) :pause)
         (player-continue player))
        (t 
         (let ((stop-time (or (cadr interval) (get-obj-dur obj))))
           (when (callback-process player)
             (om-kill-process (callback-process player)))
           (when (callback-fun player)
             (setf (callback-process player)
                   (om-run-process "editor player callback"
                                   #'(lambda ()
                                       (loop 
                                        (if (>= (get-player-time player) stop-time)
                                            (progn 
                                              (player-stop player obj)
                                              (return))
                                          (funcall (callback-fun player) 
                                                   (caller player) 
                                                   (get-player-time player)))
                                        (sleep (callback-tick player))
                                        )))
                   ))
           (setf (state player) :play
                 (start-time player) (or (car interval) 0)
                 (ref-clock-time player) (clock-time))
           )
        )))


(defmethod player-pause ((player omplayer) &optional object)
  (when (equal (state player) :play)
    (setf (start-time player) (get-player-time player)
          (state player) :pause
          )))

(defmethod player-continue ((player omplayer))
  (setf (ref-clock-time player) (clock-time)
        (state player) :play
        ))

(defmethod player-stop ((player omplayer) &optional object)
  (setf (state player) :stop
        (ref-clock-time player) (clock-time)
        (start-time player) 0)
  (when (stop-fun player)
    (funcall (stop-fun player) (caller player)))
  (when (callback-process player)
    (om-kill-process (callback-process player))
    (setf (callback-process player) nil)))


;;;=================================
;;; AN EDITOR ASSOCIATED WITH A PLAYER
;;;=================================
(defclass play-editor-mixin ()
   ((player :initform nil :accessor player)
    (player-type :initform nil :accessor player-type)
    (loop-play :initform nil :accessor loop-play)
    (end-callback :initform nil :accessor end-callback)))

(defun init-editor-player (editor)
  (setf (player editor) (make-instance (class-from-player-type (get-score-player editor))
                                     :caller editor
                                     :callback-fun 'play-editor-callback
                                     :stop-fun 'stop-editor-callback)))

(defmethod initialize-instance :after ((self play-editor-mixin) &rest initargs)
  (init-editor-player self))

(defmethod reset-editor-player ((self play-editor-mixin))
  (init-editor-player self)
  (player-init (player self)))


;;; RETURNS OBJ, TMIN, TMAX
(defmethod get-obj-to-play ((self play-editor-mixin))
  (let* ((pan (panel (editor self)))
         (interval (if (selection-to-play-? pan) 
                       (nth 2 (get-selection-to-play pan))))
         (tmin (if interval (car interval)))
         (tmax (if interval (cadr interval))))
    (values (object self) tmin tmax)))


(defmethod editor-play ((self play-editor-mixin))
  (setf (loop-play (player self)) (loop-play self))
  (multiple-value-bind (obj t1 t2)
      (get-obj-to-play self)
    (setf (callback-fun (player self))
          #'(lambda (editor time)
              (handler-bind ((error #'(lambda (e) 
                                        (om-kill-process (callback-process (player self)))
                                        (abort e))))
                (play-editor-callback editor time)
                )))
    (mapcar #'(lambda (view) (start-cursor view)) (cursor-panes self))
    (player-play (player self) obj :interval (and t1 t2 (list t1 t2)))
    ))


(defmethod editor-pause ((self play-editor-mixin))
  (player-pause (player self) (get-obj-to-play self)))

(defmethod editor-stop ((self play-editor-mixin))
  (player-stop (player self) (get-obj-to-play self)))

(defmethod editor-play/stop ((self play-editor-mixin))
  (if (idle-p (player self))
      (editor-play self)
    (editor-stop self)))

;;; temp compatibility
(defmethod recording? ((self play-editor-mixin))
  (equal (state (player self)) :record))


;;; A REDEFINIR PAR LES SOUS-CLASSES
(defmethod cursor-panes ((self play-editor-mixin)) nil)

(defmethod play-editor-callback ((self play-editor-mixin) time)
  (mapcar #'(lambda (view) (update-cursor view time))
          (cursor-panes self)))

(defmethod stop-editor-callback ((self play-editor-mixin)) nil)

;;;===================================
; VIEW WITH CURSOR
;;;===================================

(defclass cursor-play-view-mixin (om-view-cursor-play) 
  ((cursor-mode  :initform :normal :accessor cursor-mode :initarg :cursor-mode)   ;; :normal ou :interval
   (cursor-interval :initform '(0 0) :accessor cursor-interval)
   (cursor-pos :initform 0 :accessor cursor-pos)))

(defmethod set-cursor-mode ((self cursor-play-view-mixin))
  (setf (cursor-mode self) (if (equal (cursor-mode self) :normal) :interval :normal))
  (om-invalidate-view self t))

(defmethod cursor-p ((self t)) 
  (om-beep-msg "!!! CURSOR-P DOES NOT EXIST ANYMORE!!!"))

(defmethod get-obj-to-play ((self cursor-play-view-mixin))
  (values (object (om-view-container self))
          (car (cursor-interval self))
          (cadr (cursor-interval self))
          ))

(defmethod start-position ((self cursor-play-view-mixin)) 
  (or (cursor-pos self) 0))

;--------------------
; INTERVAL SELECTION
;--------------------

(defmethod new-interval-cursor ((self cursor-play-view-mixin) where)
  (om-init-motion-functions self 'interval-select-action 'release-interval-select)
  (om-new-movable-object self (om-point-h where) 0 4 (h self) 'om-selection-rectangle))

(defmethod interval-select-action ((self cursor-play-view-mixin) pos)
 (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (om-v-scroll-position self) 
                                (max 4 (third rect)) (om-point-v (om-interior-size self))))))

(defmethod release-interval-select ((self cursor-play-view-mixin) pos)  
  (let ((rect (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
        (minpixel 2) position)
    (when rect
      (om-erase-movable-object self)
      (setf position (if (> (third rect) minpixel)
                         (list (car rect) (+ (car rect) (third rect)))
                       (car rect)))
      (if (listp position)
          (setf (cursor-interval self) (list (om-point-x (pixel2point self (om-make-point (car position) 0)))
                                             (om-point-x (pixel2point self (om-make-point (cadr position) 0)))))
        (progn
          (setf (cursor-interval self) nil)
          (setf (cursor-pos self) (max 0 (om-point-h (pixel2point self (om-make-point position 0)))))))
      (om-invalidate-view self))))

(defmethod draw-interval-cursor ((self cursor-play-view-mixin))
   (let* ((sys-etat (get-system-etat self))
          (interval (cursor-interval self))
          pixel-interval)
     (when interval
       (setq pixel-interval (list (om-point-h (point2pixel self (om-make-big-point (car interval) 0) sys-etat))
                             (om-point-h (point2pixel self (om-make-big-point (second interval) 0) sys-etat))))
       (om-with-focused-view self
         (draw-h-rectangle (list (car pixel-interval) 0 (second pixel-interval) (h self)) t))
       )))



(defmethod start-cursor ((self cursor-play-view-mixin))
  (om-erase-movable-cursor self)
  (om-new-movable-cursor self (start-position self) (start-position self) 4 (h self) 'om-cursor-line))

(defmethod update-cursor ((self cursor-play-view-mixin) time &optional y1 y2)
  (let ((y (or y1 0))
        (h (if y2 (- y2 y1) (h self)))
        (pixel (om-point-x (point2pixel self (om-make-point time 0) (get-system-etat self)))))
    (om-update-movable-cursor self pixel y 4 h)))


#|
(defmethod editor-play :around ((self cursor-play-view-mixin))
  

(mapc #'(lambda (view) (om-new-movable-cursor view (start-position self) 0 4 (h self) 'om-cursor-line))
        (attached-cursor-views self))



(unless (= 0 (om-h-scroll-position self))
        (om-set-scroll-position self (om-make-point 0 (om-v-scroll-position self)))
        (mapc #'(lambda (view) (om-invalidate-view view)) (attached-cursor-views self)))
  
      (if (allowed-in-maq-p (car obj))
           (progn 
             
             
             (mapc #'(lambda (view) (om-new-movable-cursor view (start-position self) 0 4 (h self) 'om-cursor-line))
                   (attached-cursor-views self))
             (draw-line-cursor self :draw? t)



(start (draw-play-cursor self 0 (get-obj-dur obj) t))



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
        (dfuncall #+win32 100 #-win32 5 'draw-play-cursor self st end change-win)))))











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defmethod stop-play-on-palette ((self t)) nil)

(defmethod stop-play-on-palette ((self playing-palette))
  (setf (whoplay self) nil)
  (when *palette-win* 
    (om-invalidate-view (view *palette-win*) t)))





(defmethod scroll-play-window ((self cursor-play-view-mixin)) 
  ;(om-without-interrupts
   (setf (rangex self) (list (cursor-pos *general-player*) 
                             (+ (cursor-pos *general-player*) 
                                (- (second (rangex self)) (first (rangex self))))))
  (change-view-ranges self)
  (om-redraw-view self)
  (om-redraw-view (rulerx self))
  );)



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

(defmethod stop-from-palette ((self cursor-play-view-mixin))
  (om-erase-movable-cursor self)
  (mapc #'om-erase-movable-cursor (attached-cursor-views self))
  (Stop-Player *general-player* self))





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

|#





