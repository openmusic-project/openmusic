(in-package :om)



#|

(show-cercle 8 '(1 3 1 3))

(condensation 16 32 '(4 5 4 11) 8)

(condensation 16 16 '(4 5 4 11) 8)

|#

;=============================================
;CLASS
;=============================================
(defclass ryth-coerce ()
  ((cercle-interval :initform '(60 72) :initarg :cercle-interval :accessor cercle-interval)))


(defclass! n-cercle (ryth-coerce)
   ((n :initform 12 :initarg :n :accessor n)
    (puntos :initform '(0 5 7) :accessor puntos :initarg :puntos))
   (:icon 421))

;(defmethod initialize-instance :after ((self n-cercle) &key controls)
; (when (not (consp (car (puntos self))))
;    (setf (puntos self) (list (puntos self)))))

(defmethod initialize-instance :after ((self n-cercle) &key controls)
  (when (not (consp (car (puntos self))))
    (setf (puntos self) (list (puntos self))))
  (setf (puntos self)
        (remove nil (loop for cercle in (puntos self) collect
                          (if (and (listp cercle)
                                   (list-subtypep cercle 'integer))
                              cercle
                            (om-beep-msg (format nil "Wrong data in N-CERCLE: ~A" cercle)))))))



(defmethod Class-has-editor-p ((self n-cercle)) t)
(defmethod get-editor-class ((self n-cercle)) 'cercleEditor)


(defmethod draw-obj-in-rect ((self n-cercle) x x1 y y1 edparams view)
   (draw-cercle self view (+ x (round (- x1 x) 2)) (+ y (round (- y1 y) 2)) (round (min (- x1 x) (- y1 y)) 2.2)
                3 4 t 0))

(defmethod draw-cercle ((self n-cercle) view centrex centrey radio vert blue polygon cur &optional hide-back)
  (let* ((step (/ (* pi 2) (n self)))
         (point-list (if (consp (car (puntos self))) (puntos self) (list (puntos self))))
         
         (modulo-points-l (loop for points in point-list
                                collect (loop for item in (remove nil points) collect (mod item (n self))))))
    (om-draw-ellipse centrex centrey radio radio)
    (om-with-font *om-default-font1*
    (om-draw-string (- centrex 5) centrey (format nil "~D" (n self))))
    (loop for i from 0 to (- (n self) 1) do
          (let ((point (polar2car (+ (/ pi -2) (* step i)) radio)))
            (om-with-fg-color view *om-gray-color*
              (om-draw-ellipse  (+ centrex (car point))
                                (+ centrey (second point)) 
                                vert vert))))
    (unless hide-back
      (loop for modulo-points in  modulo-points-l
            for j = 0 then (+ j 1) do
            (unless (= j cur)
              (draw-one-list (n self) modulo-points centrex centrey radio j *om-gray-pattern* polygon step blue view))))
    (draw-one-list (n self) (nth cur modulo-points-l) centrex centrey radio cur *om-black-pattern* polygon step blue view)))


(defun num-in-list-mod (list n num)
   (find-if #'(lambda (x) (= (mod x n) num )) list))


(defun draw-one-list (n modulo-points centrex centrey radio j patt polygon step blue view)
  (when modulo-points
  (let (last-point first-point)
    (om-with-fg-color view  (nth (mod (+ 1 j) 15) *16-color-list*)
      (om-with-pen (view :pattern patt)
        (loop for i from 0 to (- n 1) do
              (when (num-in-list-mod modulo-points n i)
                (let ((point (polar2car (+ (/ pi -2) (* step i)) radio)))
                  (when polygon
                    (if last-point 
                      (om-draw-line (+ centrex  (car last-point)) (+ centrey (second last-point))
                                 (+ centrex  (car point)) (+ centrey (second point)))
                      (setf first-point point))
                    (setf last-point point))
                  
                  (om-fill-ellipse (+ centrex (car point))
                                (+ centrey (second point)) 
                                blue blue)))))
      (when (and polygon first-point)
        (om-draw-line (+ centrex  (car last-point)) (+ centrey (second last-point))
                   (+ centrex  (car first-point)) (+ centrey (second first-point))))))))


(defmethod draw-first ((self n-cercle) centrex centrey radio view)
   (let ((point (polar2car (+ (/ pi -2) ) radio)))
     (om-with-fg-color view *om-red-color*
       (om-draw-ellipse (+ centrex (car point)) (+ centrey (second point)) 10 10))))


(defmethod in-cercle-point ((self n-cercle) centrex centrey nsize ballsize radio point)
  (let ((r (om-make-rect (- centrex nsize) (- centrey nsize) (+ centrex nsize) (+ centrey nsize))))
    (if (om-point-in-rect-p point r) -1
      (let* ((step (/ (* pi 2) (n self))) rep)
        (loop for i from 0 to (- (n self) 1)
            while (not rep) do
             (let ((ppoint (polar2car (+ (/ pi -2) (* step i)) radio)))
                (let ((pr (om-make-rect (- (+ centrex (car ppoint)) ballsize)
                                        (- (+ centrey (second ppoint)) ballsize)
                                        (+ (+ centrex (car ppoint)) ballsize)
                                        (+ (+ centrey (second ppoint)) ballsize) 
                                        )))
                  (when (om-point-in-rect-p point pr)
                    (setf rep i)))))
        rep ))))


(defmethod! midifs-from-cercle ((self n-cercle) )
  :initvals '(nil) :indoc '("the n-cercle")
  :doc "get midi floats from the cercle" :icon 421
  (let* ((puntos (puntos self))
         (n (n self))
         (start (car (cercle-interval self)))
         (end (second (cercle-interval self)))
         (ratio (* 1.0 (/ (- end start) n))))
    (loop for item in puntos collect 
          (loop for punto in item collect (+ start (* punto ratio))))))


;=============================================
;COERCING
;=============================================

(defmethod* Objfromobjs ((Self chord) (Type n-cercle))
  (chord2c self 2))




;=============================================
;EDITOR
;=============================================
(omg-defclass cercleEditor (editorview) ())

(defmethod get-panel-class ((self cercleEditor)) 'cerclePanel)

(defmethod initialize-instance :after ((self cercleEditor) &rest L) 
  (declare (ignore l))
  (let* ((Panel (om-make-view (get-panel-class self) 
                              :position (om-make-point 0 0) 
                              :size (om-view-size self))))
    (om-add-subviews self panel)
    (setf (panel self) panel)))

(defmethod update-subviews ((self cercleEditor))
   (om-set-view-size (panel self ) (om-view-size self))
   (om-invalidate-view self t))

;=============================================
;PANEL
;=============================================
(defclass cerclePanel (om-view) 
   ((show-polygon :initform t :accessor show-polygon)
    (show-bkg :initform t :accessor show-bkg)
    (rot-mode :initform nil :accessor rot-mode)
    (current-list :initform 0 :accessor current-list)
    (n-control :accessor n-control :initform nil)))

(defmethod editor ((self cerclepanel)) (om-view-container self))

(defmethod multiple? ((self cerclePanel))
  (and (puntos (object (om-view-container self)))
       (listp (car (puntos (object (om-view-container self)))))))

(defmethod get-cur-list ((self cerclePanel))
   (if (multiple? self)
     (nth (current-list self) (puntos (object (om-view-container self))))
     (puntos (object (om-view-container self)))))

(defmethod set-cur-list ((self cerclePanel) newlist )
   (if (multiple? self)
     (setf (nth (current-list self) (puntos (object (om-view-container self)))) newlist)
     (setf (puntos (object (om-view-container self))) newlist))) 

(defmethod om-draw-contents ((self cerclePanel))
   (call-next-method)
   (om-with-focused-view self
     (om-with-line-size 2 
     (draw-cercle  (object (om-view-container self)) self (round (w self) 2) 
                   (round (h self) 2) (round (min (w self) (h self)) 2.5)
                   3 4 (show-polygon self)  (current-list self) (not (show-bkg self)))
   (when (rot-mode self)
     (draw-first (object (om-view-container self)) (round (w self) 2) (round (h self) 2) 
                 (round (min (w self) (h self)) 2.5) self)))))

(defmethod update-panel ((self cerclePanel) &optional (updateref nil))
   (update-panel (om-view-container self) updateref))

;--------------EVENTS
(defvar *rot-cursor* nil)

(defun init-cercle-cursor ()
   (setf *rot-cursor* (om-make-cursor "rot-cursor" (om-make-point 8 8))))

(om-add-init-func 'init-cercle-cursor)

(defmethod om-view-cursor ((self cerclePanel))
   (if (rot-mode self) 
     *rot-cursor*
    (call-next-method)))  

(defmethod om-view-click-handler ((self cerclePanel) where)
   (when (n-control self) 
       (om-remove-subviews self (n-control self))
       (setf (n-control self) nil))
   (let ((in-points? (in-cercle-point (object (om-view-container self)) 
                                      (round (w self) 2) 
                                      (round (h self) 2) 
                                      12 4 (round (min (w self) (h self)) 2.5) where)))
     (when in-points?
       (if (rot-mode self)
         (make-rotation-cercle self in-points?) 
         (if (= in-points? -1) (change-n-cercle self)
             (Add-rem-point self in-points?))))))

(defmethod get-help-list ((self cercleeditor)) 
  (list '(("tab" "Change Front List")
          (("b") "Show/Hide Background")
          (("c") "Complement")
          (("i") "Inverse")
          (("r") "Set Rotation Mode")
          (("m") "Set MIDI Interval"))))

(defmethod handle-key-event ((self cerclePanel) char)
  (case char
    (#\h (show-help-window "Commands for N-CERCLE Editor"
                           (get-help-list (editor self))))
    (#\c (complement-cercle self))
    (#\b (masque-others self))
    (:om-key-tab (change-current self))
    (#\p (provisoir-play self))
    (#\r (set-rotation-mode self))
    (#\i (inversion-cercle self))
    (#\m (change-interval-cercle self))
    (otherwise (om-beep))))


;--------------ACTIONS

(defmethod make-rotation-cercle ((self cerclePanel) delta) 
   (set-cur-list self  (om+ (get-cur-list self) delta))
   (update-panel self t))


(defmethod change-current ((self cerclePanel)) 
   (when (multiple? self)
     (setf (current-list self) (mod  (+ (current-list self) 1) 
                                     (length (list! (puntos (object (om-view-container self)))))))
     (update-panel self)))

(defmethod selected-point ((self cerclePanel) num) 
   (let ((obj (object (om-view-container self))))
     (find-if #'(lambda (x) (= (mod x (n obj)) num )) (get-cur-list self))))

(defmethod Add-rem-point ((self cerclePanel) num) 
   (let ((obj (object (om-view-container self))))
     (if (selected-point self num)
       (set-cur-list self  (remove-if #'(lambda (x) (= (mod x (n obj)) num )) (get-cur-list self)))
       (set-cur-list self (sort (cons num (get-cur-list self)) '<)))
     (update-panel self t)))

(defmethod inversion-cercle ((self cerclePanel)) 
   (set-cur-list self (sort (x->dx (reverse (dx->x 0 (get-cur-list self)))) '<))
   (update-panel self t))

(defmethod complement-cercle ((self cerclePanel)) 
   (let ((obj (object (om-view-container self))))
     (set-cur-list self (loop for item from 0 to (- (n obj) 1)
                              when (not (selected-point self item)) collect item))
     (update-panel self t)))

(defmethod masque-others ((self cerclePanel)) 
   (setf (show-bkg self) (not (show-bkg self))) 
   (update-panel self t))

(defmethod set-rotation-mode ((self cerclePanel)) 
   (setf (rot-mode self) (not (rot-mode self)))
   (update-panel self t))


(defmethod change-n-cercle ((self cerclePanel)) 
   (let* ((obj (object (om-view-container self)))
          (x (- (round (w self) 2) 10) )
          (y (- (round (h self) 2) 10 )))
          (setf (n-control self) (om-make-dialog-item 'numbox (om-make-point x y) (om-make-point 28 18) (format nil " ~D" (n obj))
                                                                                         :value (n obj)
                                                                                         :min-val 1
                                                                                         :max-val 1000000
                                                                                         :bg-color *om-white-color*
                                                                                         :afterfun #'(lambda (item)
                                                                                                               (setf (n obj) (value item))
                                                                                                               ;;;(om-remove-subviews self item)
                                                                                                               (update-panel self t))
                                                                                         :font *om-default-font1*))
     (om-add-subviews self (n-control self))
     (om-view-click-handler (n-control self) (om-mouse-position (n-control self)))
     ))
     

(defmethod change-interval-cercle ((self cerclePanel))
  (let* ((cercle  (object (om-view-container self)))
         (params (list   (car (cercle-interval cercle)) (second (cercle-interval cercle))  ))
         (bgcol *om-light-gray-color*)
         (font *controls-font*)
         (win (om-make-window 'om-dialog
                              :window-title "Midi Cercle Interval"
                              :bg-color *om-light-gray-color*
                              :position :centered 
                              :close t
                              :resizable nil
                              :maximize nil
                              :size (om-make-point 290 115)))
         (y 10)
         (defvals '("60" "72"))
         paramtxt1 paramtxt2 low hig okb annulerb defb)
    (setf paramtxt1 (om-make-dialog-item 'om-static-text (om-make-point 20 (+ y 4)) (om-make-point 120 20) 
                                        "Low:" 
                                        :bg-color bgcol :font font))
    (setf paramtxt2 (om-make-dialog-item 'om-static-text (om-make-point 160 (+ y 4)) (om-make-point 120 20) 
                                        "High:" 
                                        :bg-color bgcol :font font))
    
    (setf low (om-make-dialog-item 'om-editable-text (om-make-point 60 y)
                                   (om-make-point 50 20) (num2string (first params))))
    (setf hig (om-make-dialog-item 'om-editable-text (om-make-point 200 y)
                                   (om-make-point 50 20) (num2string (second params))))
    (incf y 50)
    (setf defb (om-make-dialog-item 'om-button 
                                    (om-make-point 15 y)  
                                    (om-make-point 80 20)
                                    "Defaults"
                                    :font font
                                    :di-action (om-dialog-item-act item
                                                 (declare (ignore item))
                                                 (om-set-dialog-item-text low (first defvals))
                                                 (om-set-dialog-item-text hig (second defvals)))))
    (setf okb (om-make-dialog-item 'om-button 
                                   (om-make-point 200 y)  
                                   (om-make-point 80 20)
                                   "OK"
                                   :font font
                                   :di-action (om-dialog-item-act item
                                                (declare (ignore item))
                                                (let ((h (string2num (om-dialog-item-text hig)))
                                                      (l (string2num (om-dialog-item-text low))))
                                                  (if (and l h (< l h))
                                                    (progn
                                                      (setf (cercle-interval cercle) (list l h))
                                                      (om-return-from-modal-dialog win  t))
                                                    (om-beep-msg "Bad Interval !"))))))
    (setf annulerb (om-make-dialog-item 'om-button 
                                        (om-make-point 120 y)
                                        (om-make-point 80 20)
                                        "Cancel"
                                        :font font
                                        :di-action (om-dialog-item-act item
                                                     (declare (ignore item))
                                                     (om-return-from-modal-dialog win nil))))
    
    (om-add-subviews win  paramtxt1 paramtxt2 low hig okb annulerb defb)
    (om-modal-dialog win)))


(defmethod! midifs-from-cercle ((self n-cercle) )
  :initvals '(nil) :indoc '("the n-cercle")
  :doc "get midi floats from the cercle" :icon 421
  (let* ((puntos (puntos self))
         (n (n self))
         (start (car (cercle-interval self)))
         (end (second (cercle-interval self)))
         (ratio (* 1.0 (/ (- end start) n))))
    (loop for item in puntos collect 
          (loop for punto in item collect (+ start (* punto ratio))))))

(defmethod provisoir-play ((self cerclePanel) )
  (setf *MidiShare-start-time* 1)
  ;(om-midi-set-player *midiplayer* (om-midi-new-seq) 1000)
  (setf *microosc-packets* 
        (loop for item in (nth  (current-list self) (midifs-from-cercle (object (om-view-container self))))
              collect (list "/play.µt/fifos" 0  item 100  1000 1)))
  (setf *index-packets* 0) 
  (send-200) 
  (micro-start))


;=============================================
;TOOLS
;=============================================
(defun polar2car (teta rad)
  (list (round (* rad (cos teta))) (round (* rad (sin teta)))))

;----------
(defmethod! chord2c ((Self chord) approx)
  :initvals '(nil 2) :indoc '("the chord"  "approx")
  :doc "It gives a possible circular representation of a chord by choosing the division of the tone (2 equal the semitone approx)" :icon 421
  (let ((n (case approx (2 12) (4 24) (8 48) (otherwise 12)))
        (div (case approx (2 100) (4 50) (8 25) (otherwise 100))))
    
    (make-instance 'n-cercle
      :n n
      :puntos (remove-duplicates (sort (loop for item in (om/ (om- (approx-m (Lmidic self) approx) 6000) div )
                                 collect (mod item n)) '<)))))

(defmethod! c2chord ((self n-cercle) index base approx)
  :initvals '(nil 0 6000 100) :indoc '("the n-cercle" "index" "initial value" "approx")
  :doc "It makes a chord starting from the points of an n-cercle" :icon 421
  (let ((points (puntos self)))
    (make-instance 'chord
      :lmidic (om+ base (om* approx (nth index points))))))

;----------
;changer
(defmethod! c2chord-seq ((self n-cercle) base approx)
  :initvals '(nil 6000 100) :indoc '("the cercle" "initial value" "approx")
  :doc "It makes a sequence of chords starting from the polygons of the cercle" :icon 421
  (let ((points (puntos self)))
    (make-instance 'chord-seq
      :lmidic (loop for item in points
                    collect (om+ base (om* approx item))))))

;a faire
(defmethod! chord-seq2c ((self chord-seq)  approx)
  :initvals '(nil 2 ) :indoc '("the chord-seq"  "approx")
  :doc "It gives some possible circular representations of a given sequence of chords by choosing the division of the tone (2 equal the semitone approx)" :icon 421
  (let ((n (case approx (2 12) (4 24) (8 48) (otherwise 12)))
        (div (case approx (2 100) (4 50) (8 25) (otherwise 100))))
    
    (make-instance 'n-cercle
      :n n
      :puntos (loop for chord in (inside self) collect
                    (remove-duplicates (sort (loop for item in (om/ (om- (approx-m (Lmidic chord) approx) 6000) div )
                                                   collect (mod item n)) '<))))))
;----------

(defmethod! c2rhythm ((self n-cercle) index signature times)
  :initvals '(nil 0 (12 8) 3) :indoc '("the cercle" "index" "signature" "times" )
  :doc "It makes a rhythm starting from the points of the index polygon in the cercle and by repeating the rhythmic pattern a given number of times " :icon 421
  (let ((points (repeat-n (list signature (n-structure (nth index (puntos self)) (n self))) times)))
    (make-instance 'voice
      :tree points)))


(defmethod! rhythm2c ((self voice) n)
  :initvals '(nil 0 (12 8) 3) :indoc '("the cercle" "index" "signature" "times" )
  :doc "It gives a possible circular representation of a rhythmic pattern by choosing the value n of the corresponding n-cercle" :icon 421
  (let* ((ratio (tree2ratio (tree self))) 
         (step (car ratio)))
    (loop for item in (cdr ratio) do
          (setf step (pgcd step item)))
    (setf ratio (om/ ratio step))
    (make-instance 'n-cercle
      :n n
      :puntos  (n-scale ratio))))




