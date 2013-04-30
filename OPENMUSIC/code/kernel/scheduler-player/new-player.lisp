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
   ;;; CALLBACKS
   (callback-tick :initform 0.01 :accessor callback-tick :initarg :callback-tick)
   (caller :initform nil :accessor caller :initarg :caller)
   (callback-fun :initform nil :accessor callback-fun :initarg :callback-fun)
   (callback-process :initform nil :accessor callback-process)
   (stop-fun :initform nil :accessor stop-fun :initarg :stop-fun)
   ;;; SCHEDULING TASKS
   (events :initform nil :accessor events :initarg :events)
   (scheduling-process :initform nil :accessor scheduling-process)
   ))

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
           (when (scheduling-process player)
             (om-kill-process (scheduling-process player)))
           
           (om-with-priority 80000000
             (setf (scheduling-process player)
                   (om-run-process "player scheduling"
                                   #'(lambda ()
                                       (loop
                                        (loop while (and (events player) (>= (get-player-time player) (car (car (events player))))) do
                                              (funcall (cdr (pop (events player)))))
                                        (if (> (get-player-time player) stop-time) (player-stop player obj))
                                        (sleep (callback-tick player))
                                        )))))
           
           (when (callback-fun player)
             (om-with-priority 10
               (setf (callback-process player)
                     (om-run-process "editor player callback"
                                     #'(lambda ()
                                         (loop 
                                          (funcall (callback-fun player) 
                                                   (caller player) 
                                                   (get-player-time player))
                                          (sleep (callback-tick player))
                                         )))
                     )))
           
           (setf (state player) :play
                 (start-time player) (or (car interval) 0)
                 (ref-clock-time player) (clock-time))
           
           ;(om-delayed-funcall stop-time #'player-stop player obj)
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
    (setf (callback-process player) nil))
  (when (scheduling-process player)
    (om-kill-process (scheduling-process player))
    (setf (scheduling-process player) nil))
  )

(defmethod player-schedule ((player omplayer) task at)
  (push (cons at task) (events player))
  (setf (events player) (sort (events player) '< :key 'car)))

(defmethod player-unschedule-all ((player omplayer))
  (setf (events player) nil))

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

(defmethod get-obj-to-play ((self play-editor-mixin)) nil)

(defmethod get-duration ((self play-editor-mixin)) 
  (get-obj-dur (get-obj-to-play self)))   ;;; = 0 if obj = NIL

(defmethod get-interval-to-play ((self play-editor-mixin))
  (let ((selection-pane (car (cursor-panes self)))
        (object (get-obj-to-play self)))
    (when (and selection-pane object)
      (cond ((and (cursor-interval selection-pane) 
                  (not (= (car (cursor-interval selection-pane)) (cadr (cursor-interval selection-pane)))))
             (cursor-interval selection-pane))
            ((and (cursor-pos selection-pane) (not (zerop (cursor-pos selection-pane))))
             (list (cursor-pos selection-pane) (get-duration self)))
            (t nil)))))

(defmethod editor-play ((self play-editor-mixin) )
  (setf (loop-play (player self)) (loop-play self))
  (let ((obj (get-obj-to-play self))
        (interval (get-interval-to-play self)))
    (setf (callback-fun (player self))
          #'(lambda (editor time)
              (handler-bind ((error #'(lambda (e) 
                                        (om-kill-process (callback-process (player self)))
                                        (abort e))))
                (play-editor-callback editor time)
                )))
    (mapcar #'(lambda (view) (start-cursor view)) (cursor-panes self))
    (player-play (player self) obj :interval interval)))

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

;(defmethod get-obj-to-play ((self cursor-play-view-mixin))
;  (values (object (om-view-container self))
;          (car (cursor-interval self))
;          (cadr (cursor-interval self))
;          ))

(defmethod start-position ((self cursor-play-view-mixin)) 
  (or (cursor-pos self) 0))

(defmethod view-turn-pages-p ((self cursor-play-view-mixin)) t)

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
       (setq pixel-interval (list (om-point-h (point2pixel self (om-make-point (car interval) 0) sys-etat))
                             (om-point-h (point2pixel self (om-make-point (second interval) 0) sys-etat))))
       (om-with-focused-view self
         (draw-h-rectangle (list (car pixel-interval) 0 (second pixel-interval) (h self)) t))
       )))

(defmethod start-cursor ((self cursor-play-view-mixin))
  (let* ((dur (get-obj-dur (object (om-view-container self))))
         (range (rangex (panel (om-view-container self))))
         (xview (- (second range) (first range)))
         (start (start-position self))
         (dest (+ start xview))
         (at-pix (om-point-x (point2pixel self (om-make-point start 0) (get-system-etat self)))))
    (when (and (view-turn-pages-p self) (< dest dur))
      (scroll-play-view self at-pix))
    (om-erase-movable-cursor self)
    (om-new-movable-cursor self (start-position self) (start-position self) 4 (h self) 'om-cursor-line)))

(defmethod update-cursor ((self cursor-play-view-mixin) time &optional y1 y2)
  (let* ((y (or y1 0))
         (h (if y2 (- y2 y1) (h self)))
         (pixel (om-point-x (point2pixel self (om-make-point time 0) (get-system-etat self))))
         (dur (get-obj-dur (object (om-view-container self))))
         range
         xview
         dest
        ;(pixel (xpoint2pixel self time (get-system-etat self)))
         )
    (when (and (view-turn-pages-p self)
               (> pixel (+ (w self) (om-h-scroll-position self)))
               (< time dur)) 
      (progn
        (setf range (rangex (panel (om-view-container self))))
        (setf xview (- (second range) (first range)))
        (setf dest (+ time xview))
        (if (> dest dur)
            (setf pixel (om-point-x (point2pixel self (om-make-point (- dur xview) 0) (get-system-etat self)))))
        (scroll-play-view self pixel)))
    (om-update-movable-cursor self pixel y 4 h)))

(defmethod scroll-play-view ((self cursor-play-view-mixin) &optional at-pixel)
  (om-set-scroll-position self (om-make-point at-pixel 0)))
  


