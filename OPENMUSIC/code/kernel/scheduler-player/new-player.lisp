(in-package :om)

;;;=================================
;;; THE PLAYER
;;;=================================
(defclass omplayer () 
  ((state :accessor state :initform :stop)    ; :play :pause :stop :record
   (loop-play :accessor loop-play :initform nil)
   (start-time :accessor start-time :initform 0)
   (stop-time :accessor stop-time :initform 0)
   (play-interval  :accessor play-interval :initform nil) ;;; check if this is necessary or if we can do everything with start-time and end-time....
   (player-offset :accessor player-offset :initform 0)
   (ref-clock-time :accessor ref-clock-time :initform 0)
   ;;; CALLBACKS
   (callback-tick :initform 0.1 :accessor callback-tick :initarg :callback-tick)
   (caller :initform nil :accessor caller :initarg :caller)
   (callback-fun :initform nil :accessor callback-fun :initarg :callback-fun)
   (callback-process :initform nil :accessor callback-process)
   (stop-fun :initform nil :accessor stop-fun :initarg :stop-fun)
   ;;; SCHEDULING TASKS
   (events :initform nil :accessor events :initarg :events)
   (scheduling-process :initform nil :accessor scheduling-process)
   (scheduler-tick :initform 0.01 :accessor scheduler-tick :initarg :scheduler-tick)
   ;;; OBJECTS
   (play-list :initform nil :accessor play-list :initarg :play-list)
   ;;; ENGINES
   (engines :initform nil :accessor engines :initarg :engines)
   ))


;(defstruct player-task (object nil) (engine nil) (at 0) (interval nil))

(defun clock-time () (get-internal-real-time))

(defmethod player-init ((self omplayer)) t)

;(defmacro get-player-time (player)
;  `(cond ((equal (state ,player) :play)
;          (+ (player-offset ,player) (start-time ,player) (- (clock-time) (ref-clock-time ,player))))
;         ((equal (state ,player) :pause)
;          (+ (player-offset ,player) (start-time ,player)))
;         (t 0)))

(defun get-player-time (player)
  (cond ((equal (state player) :play)
         (+ (player-offset player) (start-time player) (- (clock-time) (ref-clock-time player))))
        ((equal (state player) :pause)
         (+ (player-offset player) (start-time player)))
        (t 0)))


(defmethod idle-p ((self omplayer)) 
  (not (member (state self) '(:play :record))))

(defmethod sort-events ((self omplayer))
  (setf (events self) (sort (events self) '< :key 'car)))

(defmethod schedule-task ((player omplayer) task at &optional (sort t))
  (push (cons at task) (events player))
  (when sort (sort-events player)))

(defmethod unschedule-all ((player omplayer))
  (setf (events player) nil))

(defun get-my-play-list (engine play-list)
  (mapcar 'cadr (remove-if-not #'(lambda (x) (equal x engine)) play-list :key 'car)))



;;; THIS METHOD WHEN THE PLAYER HAS TO PLAY SEVERAL THINGS OR PREPARE THEM IN ADVANCE
;;; SAYS ENGINE TO PREPARE FOR PLAYING <INTERVAL> (optional) IN <OBJ> WITH< ENGINE> AT TIME <at>
(defmethod player-schedule ((player omplayer) obj engine &key (at 0) interval params)
  ;(print (list obj engine at))
  (let ((engines-available (enabled-players-for-object obj)))
    (unless (find engine engines-available)
      (print (format nil "Warning: player engine ~s not available for ~A (will be played on ~s)." engine obj (car engines-available)))
      (setf engine (car engines-available))))
  (unless (find engine (engines player)) (push engine (engines player)))
  (push (list engine obj) (play-list player))
  (prepare-to-play engine player obj at interval params))  

(defmethod player-schedule ((player omplayer) (obj maquette-obj) engine &key (at 0) interval params)
   (loop for object in (inside obj)
           for param in (param-list obj) do
           (player-schedule player object
                            (cdr (assoc 'player param))
                            :at (+ at (offset->ms object))
                            :interval interval)
           ))


(defmethod general-play ((player omplayer)) ;;; &key (start-t 0) (end-t 3600000))
  (let ((start-t (or (car (play-interval player)) 0))
            (end-t (or (cadr (play-interval player )) 3600000)))
  (cond ((equal (state player) :play)
         ;;; prolonge la durée de vie du player
         (setf (stop-time player) (max (stop-time player) end-t)))
        
        (t 
         (setf (stop-time player) end-t)
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
                                      (when (> (get-player-time player) (stop-time player))
                                        (if (loop-play player) (general-loop player) (general-stop player)))
                                      (sleep (scheduler-tick player))
                                      )))))
           
         (when (callback-fun player)
           (om-with-priority 10
             (setf (callback-process player)
                   (om-run-process "editor player callback"
                                   #'(lambda ()
                                       (loop 
                                        (funcall (callback-fun player) (caller player) (get-player-time player))
                                        (sleep (callback-tick player))
                                        ;;; tester MP::PROCESS-WAI-WITH-TIMEOUT
                                        )))
                   )))
           

         (when (loop-play player) 
           (mapcar #'(lambda (pl) (player-set-loop pl start-t end-t)) 
                   (engines player)))

         (mapcar #'player-start (engines player) 
                 (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
           
         (setf (state player) :play
               (start-time player) start-t
               (ref-clock-time player) (clock-time))
           
           ;(om-delayed-funcall stop-time #'player-stop player obj)
         )
         )))

(defmethod general-pause ((player omplayer))
  (mapcar #'player-pause (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  (when (equal (state player) :play)
    (setf (start-time player) (get-player-time player)
          (state player) :pause
          )))

(defmethod general-continue ((player omplayer))
  (mapcar #'player-continue (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  (setf (ref-clock-time player) (clock-time)
        (state player) :play
        ))

(defmethod general-loop ((player omplayer))
       ;(print "general loop")
  ;(setf (stop-time player) (cadr (play-interval player)))
  (setf (start-time player) (or (car (play-interval player)) 0)
        (ref-clock-time player) (clock-time))
  ;;; ask every engine to reschedule their play-list
  (mapcar #'(lambda (engine play-list) (player-loop engine player play-list))
          (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  )


(defmethod general-stop ((player omplayer))
  (mapcar #'player-stop (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  (unschedule-all player)
  (setf (engines player) nil
        (play-list player) nil)
  (when (stop-fun player)
    (funcall (stop-fun player) (caller player)))
  (when (callback-process player)
    (om-kill-process (callback-process player))
    (setf (callback-process player) nil))
  (setf (state player) :stop
        (ref-clock-time player) (clock-time)
        (start-time player) 0)
  (when (scheduling-process player)
    (om-kill-process (scheduling-process player))
    (setf (scheduling-process player) nil))
  )


(defmethod general-record ((player omplayer))
  (if (equal (state player) :stop)
    (progn
      (setf (state player) :record)
      (mapcar #'player-record (engines player)))
    (om-beep)))

(defmethod general-stop-record ((player omplayer))
  (setf (play-list player)
        (mapcar #'(lambda (pl) (list pl (player-record-stop pl))) (engines player)))
  (when (callback-process player)
    (om-kill-process (callback-process player))
    (setf (callback-process player) nil))
  (setf (state player) :stop
        (ref-clock-time player) (clock-time)
        (start-time player) 0)
  (when (scheduling-process player)
    (om-kill-process (scheduling-process player))
    (setf (scheduling-process player) nil))
  (setf (engines player) nil)
  )


;;;=====================
;;; SUB-PLAYERS (AKA "ENGINES")
;;;=====================

;;; SPECIFIES SOMETHING TO BE PLAYED ATHER A GIVEN DELAY (<at>) PAST THE CALL TO PLAYER-START
;;; THE DEFAULT BEHAVIOUR IS TO SCHEDULE 'player-play' AT DELAY
(defmethod prepare-to-play ((engine t) (player omplayer) object at interval params)
  (schedule-task player 
                 #'(lambda () 
                     (player-play-object engine object :interval interval :params params))
                 at))

;;; PLAY (NOW)
(defmethod player-play-object ((engine t) object &key interval params)
  (declare (ignore interval))
  ;(print (format nil "~A : play ~A - ~A" engine object interval))
  t)

;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine t) &optional play-list)
  ;(print (format nil "~A : start" engine))
  t)

;;; PAUSE (all)
(defmethod player-pause ((engine t) &optional play-list)
  ;(print (format nil "~A : pause" engine))
  t)

;;; CONTINUE (all)
(defmethod player-continue ((engine t) &optional play-list)
  ;(print (format nil "~A : continue" engine))
  t)

;;; STOP (all)
(defmethod player-stop ((engine t) &optional play-list)
  ;(print (format nil "~A : stop" engine))
  t)

;;; SET LOOP (called before play)
(defmethod player-set-loop ((engine t) &optional start end)
  ;(print (format nil "~A : set loop" engine))
  t)

;;; an engine must choose a strategy to reschedule it's contents on loops
(defmethod player-loop ((engine t) player &optional play-list)
  ;(print (format nil "~A : loop" engine))
  t)

(defmethod player-record ((engine t))
  ;(print (format nil "~A : record" engine))
  t)

;;; must return the recorded object
(defmethod player-record-stop ((engine t))
  ;(print (format nil "~A : record stop" engine))
  nil)

;;;=================================
;;; GENERAL PLAYER: USED IN PATCH EDITORS
;;;=================================

(defparameter *general-player* (make-instance 'omplayer 
                                              ;;; :callback-fun 'general-player-callback
                                              :callback-tick 1.0
                                              :stop-fun 'general-player-stop
                                              ))

(defvar *play-boxes* nil)

;(defun general-player-callback (caller time)
;  (mapcar #'(lambda (box)
;              (om-invalidate-view (car (frames box))))
;          *play-boxes*))

(defun general-player-stop (caller)
  (declare (ignore caller))
  (mapcar #'(lambda (box)
              (setf (play-state box) nil)
              (if (car (frames box))
                  (om-invalidate-view (car (frames box)))))
          *play-boxes*)
  (setf *play-boxes* nil))

(defmethod play-obj? ((self t)) (allowed-in-maq-p self))


(defmethod get-obj-to-play ((self ombox)) (play-obj-from-value (value self) self))
(defmethod play-obj-from-value (value box) value)

(defmethod play-boxes ((boxlist list))
  (mapcar #'(lambda (box)
              (when (play-obj? (value box))
                (player-schedule *general-player*
                                 (get-obj-to-play box)
                                 (get-edit-param box 'player) :at (get-player-time *general-player*)
                                 :params (additional-player-params box))
                (setf (play-state box) t)
                (push box *play-boxes*)
                ))
          boxlist)
  (when *play-boxes*
    (setf (play-interval *general-player*) (list 0 (loop for box in boxlist maximize (get-obj-dur (get-obj-to-play box)))))
    (general-play *general-player*) ;;; :end-t (loop for box in boxlist maximize (get-obj-dur (value box))))
    ))


(defmethod stop-boxes ((boxlist list))
  (mapcar #'(lambda (box)
              (when (play-obj? (value box))
                (player-stop (get-edit-param box 'player) (list (value box)))
                (setf (play-state box) nil)
                (setf *play-boxes* (remove box *play-boxes*))
                ))
          boxlist)
  (unless *play-boxes* (general-stop *general-player*))
  )

(defmethod stop-all-boxes ()
  (stop-boxes *play-boxes*)
  (general-player-stop nil))


(defmethod additional-player-params ((self omboxeditcall))
  (list :port (get-edit-param self 'outport)
        :approx (get-edit-param self 'approx)))


;;;=================================
;;; AN EDITOR ASSOCIATED WITH A PLAYER
;;;=================================
(defclass play-editor-mixin ()
   ((player :initform nil :accessor player)
    (player-type :initform nil :accessor player-type)
    (loop-play :initform nil :accessor loop-play)
    (end-callback :initform nil :accessor end-callback)
    (player-specific-controls :initform nil :accessor player-specific-controls)))

(defun init-editor-player (editor)
  (setf (player editor) (make-instance 'omplayer ;; (class-from-player-type (get-score-player editor))
                                     :caller editor
                                     :callback-fun 'play-editor-callback
                                     :stop-fun 'stop-editor-callback)))

(defmethod initialize-instance :after ((self play-editor-mixin) &rest initargs)
  (init-editor-player self))


(defmethod make-player-specific-controls (player control-view) nil)

(defmethod update-player-controls ((self play-editor-mixin) player &optional view)
  (apply #'om-remove-subviews (or view self) (player-specific-controls self))
  (setf (player-specific-controls self)
        (make-player-specific-controls player self))
  (apply #'om-add-subviews (or view self) (player-specific-controls self))
  )
  


(defmethod get-player-engine ((self play-editor-mixin)) t)
(defmethod get-player-engine ((self editorview)) (get-edit-param self 'player))

(defmethod get-obj-to-play ((self play-editor-mixin)) nil)

(defmethod get-duration ((self play-editor-mixin)) 
  (get-obj-dur (get-obj-to-play self)))  ;;; = 0 if obj = NIL

;; priorité sur le mode
(defmethod play-selection-first ((self t)) nil)

(defmethod get-interval-to-play ((self play-editor-mixin))
  (let ((selection-pane (car (cursor-panes self)))
        (object (get-obj-to-play self)))
    (when (or (and (play-selection-first self) (cursor-interval selection-pane))
              (and selection-pane (equal (cursor-mode selection-pane) :interval) object))
      (cond ((and (cursor-interval selection-pane) 
                  (not (= (car (cursor-interval selection-pane)) (cadr (cursor-interval selection-pane)))))
             (cursor-interval selection-pane))
            ((and (cursor-pos selection-pane) (not (zerop (cursor-pos selection-pane))))
             (list (cursor-pos selection-pane) (get-duration self)))
            (t nil)))))

;;; THE USER PRESSES PLAY IN THE EDITOR

(defmethod additional-player-params ((self t)) nil)

(defmethod schedule-editor-contents ((self play-editor-mixin))
  (player-schedule (player self) 
                   (get-obj-to-play self)
                   (get-player-engine self)
                   :at 0 
                   :interval (get-interval-to-play self)
                   :params (additional-player-params self)))

(defmethod get-editor-callback ((self play-editor-mixin))
  #'(lambda (editor time)
      (handler-bind ((error #'(lambda (e) 
                                          ;(print e)
                                (om-kill-process (callback-process (player self)))
                                (abort e))))
        (play-editor-callback editor time)
        )))
  
(defmethod editor-play ((self play-editor-mixin))
  (setf (loop-play (player self)) (loop-play self))
  (if (equal (state (player self)) :pause)
      (general-continue (player self))
    (let ((obj (get-obj-to-play self))
          (interval (get-interval-to-play self)))
      (setf (callback-fun (player self)) 
            (and ; *events-play-cursor*
                 (get-editor-callback self)))
      (mapcar #'(lambda (view) (start-cursor view)) (cursor-panes self))
      (schedule-editor-contents self)
      (setf (play-interval (player self)) (list  (or (car interval) 0) (or (cadr interval) (get-obj-dur obj))))
      (general-play (player self) 
                    ;:start-t (or (car interval) 0)
                    ;:end-t (or (cadr interval) (get-obj-dur obj))
                    )
      )
    ))


(defmethod editor-pause ((self play-editor-mixin))
  (general-pause (player self)))

(defmethod editor-stop ((self play-editor-mixin))
  (if (equal (state (player self)) :record)
      (editor-stop-record self))
  (general-stop (player self)))

(defmethod editor-play/stop ((self play-editor-mixin))
  (if (idle-p (player self))
      (editor-play self)
    (editor-stop self)))

(defmethod editor-record ((self play-editor-mixin))
  (setf (engines (player self)) (list (get-player-engine self)))
  (general-record (player self)))

(defmethod editor-stop-record ((self play-editor-mixin))
  (general-stop-record (player self))
  (cadr (car (play-list (player self)))))

;;; A REDEFINIR PAR LES SOUS-CLASSES
(defmethod cursor-panes ((self play-editor-mixin)) nil)

(defmethod play-editor-callback ((self play-editor-mixin) time)
  (mapcar #'(lambda (view) (update-cursor view time))
          (cursor-panes self)))

(defmethod stop-editor-callback ((self play-editor-mixin)) nil)

(defmethod close-editor-before ((self play-editor-mixin))
  (general-stop (player self))
  (call-next-method))

(defmethod update-player-interval (editor interval) nil)
(defmethod update-player-interval ((editor play-editor-mixin) interval)
  (unless (cadr interval) (setf (cadr interval) (get-obj-dur (object editor))))
  (setf (play-interval (player editor)) interval))


;;;===================================
; VIEW WITH CURSOR
;;;===================================

(defclass cursor-play-view-mixin (om-view-cursor-play) 
  ((cursor-mode  :initform :normal :accessor cursor-mode :initarg :cursor-mode)   ;; :normal ou :interval
   (cursor-interval :initform '(0 0) :accessor cursor-interval)
   (cursor-pos :initform 0 :accessor cursor-pos)))

(defmethod change-cursor-mode ((self cursor-play-view-mixin) &optional mode)
  (setf (cursor-mode self) (or mode (if (equal (cursor-mode self) :normal) :interval :normal)))
  (om-invalidate-view self t))

(defmethod cursor-p ((self t)) 
  (equal (cursor-mode self) :interval))
;; "!!! CURSOR-P DOES NOT EXIST ANYMORE!!!"


;(defmethod get-obj-to-play ((self cursor-play-view-mixin))
;   (list (object (om-view-container self))))

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
                                             (om-point-x (pixel2point self (om-make-point (cadr position) 0))))
                (cursor-pos self) (om-point-x (pixel2point self (om-make-point (car position) 0))))
        (progn
          (setf (cursor-interval self) nil)
          (setf (cursor-pos self) (max 0 (om-point-h (pixel2point self (om-make-point position 0)))))))
      (update-player-interval (om-view-container self) (or (cursor-interval self) (list (cursor-pos self) nil)))
      (om-invalidate-view self))))


(defmethod draw-interval-cursor ((self cursor-play-view-mixin))
  (let* ((cursor-pos-pix (time-to-pixels self (cursor-pos self)))
         (interval (cursor-interval self)))
    (om-with-focused-view self
      (when interval
        (draw-h-rectangle (list (time-to-pixels self (car interval)) 0 (time-to-pixels self (cadr interval)) (h self)) t))
       
      ;;; start pos
      (om-with-fg-color self *om-red2-color*
        (om-with-dashline 
            (om-with-line-size 2 
              (om-draw-line cursor-pos-pix 0 cursor-pos-pix (h self))
              )))
      )))


(defmethod get-x-range ((self cursor-play-view-mixin)) 
  (list (max 0 (om-point-x (pixel2point self (om-scroll-position self))))
        (om-point-x (pixel2point self (om-add-points (om-scroll-position self) (om-view-size self))))))

(defmethod start-cursor ((self cursor-play-view-mixin))
  (let* (;;(dur (get-obj-dur (object (om-view-container self))))
         (range (get-x-range (panel (om-view-container self))))
         ;;(xview (- (second range) (first range)))
         (start (start-position self))
         (at-pix (om-point-x (point2pixel self (om-make-point start 0) (get-system-etat self))))
         ;;(dest (+ start xview))
	 )
    (when (and (view-turn-pages-p self) (or (< start (car range))
                                                   (> start (cadr range))))
      (scroll-play-view self at-pix)
      )
    (om-erase-movable-cursor self)
    (om-new-movable-cursor self (start-position self) 0 4 (h self) 'om-cursor-line)))

(defmethod reset-cursor ((self cursor-play-view-mixin))
  (setf (cursor-pos self) 0)
  (setf (cursor-interval self) '(0 0))
  (om-invalidate-view self))

;;;===================
;;; TOOLS
;;; NEEDS CLEANUP !!!
;;;===================

(defmethod time-to-pixels ((self t) time)
  (om-point-x (point2pixel self (om-make-point time 0) (get-system-etat self))))

;;;===================

(defmethod update-cursor ((self cursor-play-view-mixin) time &optional y1 y2)
  (let* ((y (or y1 0))
         (h (if y2 (- y2 y1) (h self)))
         (pixel (time-to-pixels self time))
         (dur (get-obj-dur (object (om-view-container self))))
         range
	 durview
         ;xview
         dest
        ;(pixel (xpoint2pixel self time (get-system-etat self)))
         )
    (when (and (view-turn-pages-p self)
               (> pixel (+ (w self) (om-h-scroll-position self) -20))
               (< time dur))
      (setf range (get-x-range (panel (om-view-container self))))
      (setf durview (- (second range) (first range)))
      (setf dest (+ time durview))
      (if (> dest dur)
          (setf pixel (time-to-pixels self (- dur durview))))
      (scroll-play-view self (- pixel (get-key-space self)))
      ;(om-invalidate-view self)
      )
    (om-update-movable-cursor self pixel y 4 h)
    ))

(defmethod scroll-play-view ((self cursor-play-view-mixin) &optional at-pixel)
  (om-set-scroll-position self (om-make-point at-pixel 0)))
  


