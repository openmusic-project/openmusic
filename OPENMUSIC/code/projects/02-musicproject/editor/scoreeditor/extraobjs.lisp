(in-package :om)



;================================================
;extra-obj
;================================================

(defclass extra-o ()
   ((object :initform nil :initarg :object :accessor object)
    (levelsize :initform 1 :accessor levelsize)
    (graphic-frame :initform nil :accessor graphic-frame)
    (uord :initform 1 :accessor uord)))


(defclass! extra-objet (extra-o)
   ((deltax :initform 0  :initarg :deltax :accessor deltax :documentation "horizontal offset")
    (deltay :initform 0  :initarg :deltay :accessor deltay :documentation "vertical offset")))


(defmethod extra-p ((self extra-objet)) t)
(defmethod extra-p ((self t)) nil)

(defmethod transpose-a ((self extra-objet) trans)
   (setf (deltay self) (+ (deltay self) (* -1 (round trans 100)))))

(defmethod move-in-x ((self extra-objet) trans)
   (setf (deltax self) (+ (deltax self) trans)))

(defmethod click-in-extra ((self extra-objet)) t)


(defmethod general-delete ((view voicePanel) (self extra-objet))
   (setf (extra-obj-list (object self)) (remove self (extra-obj-list (object self)) :test 'equal)))

(defmethod general-delete ((view polyPanel) (self extra-objet))
   (setf (extra-obj-list (object self)) (remove self (extra-obj-list (object self)) :test 'equal)))

(defmethod general-delete ((view scorePanel) (self extra-objet))
   (setf (extra-obj-list (object self)) (remove self (extra-obj-list (object self)) :test 'equal)))

(defmethod add-new-extra ((self t) (mode t))  nil)

(defmethod set-extra-in-list ((extra t) (self t))
   (setf (object extra) self)
   (push extra (extra-obj-list self)))


(defmethod delete-extra ((self extra-objet))
   (when (object self)
     (setf (extra-obj-list (object self)) 
           (remove self (extra-obj-list (object self)) :test 'equal))))

(defmethod score-move-a  ((self extra-objet) panel trans)
  (transpose-a self trans))


(defmethod copy-container ((self extra-objet) &optional pere)
  (let ((rep (clone self)))
    (setf (object rep) pere)
    rep))


;-------------graphic-extra-object---------------

(defclass grap-extra-objet ()
   ((rectangle :initform (list 0 0 0 0 ) :accessor rectangle)
    (reference :initform nil :initarg :reference :accessor reference)
    (selected :initform nil  :accessor selected)
    (gobject :initform nil :initarg :gobject :accessor gobject)))

(defmethod grap-extra-p ((self grap-extra-objet)) t)
(defmethod grap-extra-p ((self t)) nil)

(defmethod draw-score-selection ((self grap-extra-objet) selection system size)
   (if (member (reference self) selection :test 'equal)
     (draw-h-rectangle (rectangle self)  t)))

(defmethod open-extra-editor ((self scorepanel) (gtext grap-extra-objet)) t)

(defmethod extra-rectangle-selection ((self grap-extra-objet))
  (rectangle self))



;============================================================
; DIVERS TYPES OF EXTRA OBJECTS
;============================================================

;***************
;TEXT
;***************
(defclass! text-extra (extra-objet)
   ((thetext :initform "text" :initarg :thetext :accessor thetext :documentation "text (string)")
    (font :initform nil :accessor font))
   (:icon 279)
   (:documentation 
"
A text to be attached somewhere in a score object.

EXTRA objects are additional data or graphics integrated in the score objects (voice, chord-seq, etc.)
They can be added and manipulated thanks to the Extra package functions (add-extra, etc.)

TEXT-EXTRA is a string (<text>) attached to a particular chord or note in the score.
<deltax> and <deltay> are relative horizontal and vertical offsets (arbitrary non-temporal unit depending on the zoom and font size)
")
   )

(defmethod text-extra-p ((self text-extra)) t)
(defmethod text-extra-p ((self t)) nil)


(defmethod add-new-extra ((self t) (mode (eql 'text)))
   (let ((newextra (make-instance 'text-extra
                     :object self)))
     (setf (uord newextra) 1)
     (push newextra (extra-obj-list self))))

(defmethod add-new-extra-drag (self where obj (mode (eql 'text)) dc)
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos*) size))
    (setf newextra (make-instance 'text-extra :object (reference obj)))
    (setf (thetext newextra) "text")
    (setf (deltax newextra) (om-point-h (car points))
          (deltay newextra) (om-point-v (car points)))
    (push newextra (extra-obj-list (reference obj)))
    (update-panel self t)))


(defmethod draw-obj-in-rect ((self  text-extra) x x1 y y1 edparams  view)
  (let* ((fontsize 14)
         (thefont (om-make-font *om-def-font-face* fontsize))
         (sizetext (round (get-name-size (thetext self) thefont) 2)))
    (om-with-font thefont
                  (om-draw-string (round (- (+ x (/ (- x1 x) 2)) sizetext)) (round (+ y (/ (- y1 y) 2))) (thetext self))
                  )))

;----------------

(defclass grap-extra-text (grap-extra-objet) ())

(defmethod make-graph-extra-obj ((self text-extra) gobj)
   (let ((rep (make-instance 'grap-extra-text
     :reference  self
     :gobject gobj)))
    (setf (graphic-frame self) rep))
   )
   
(defmethod draw-graph-extra-obj ((self grap-extra-text) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          (rect (rectangle grap-obj))
          (fontsize (round size 2.8))
          (text (thetext (reference self)))
          (thefont (om-make-font *om-def-font-face* fontsize))
          (ls (round size 4))
          (sizetext (get-name-size text thefont))
          x y)
     (setf y (cond
              ((= (uord object) 1)
               (+ ls (fourth rect)))
              (t (+ ls (second rect)))))
     (setf x (- (+ (first rect) (round (- (third rect) (first rect)))) (round sizetext 2)))
     (setf y (+ y (* ls (deltay (reference self)))))
     (setf x (+ x (* ls (deltax (reference self)))))
     (om-with-font thefont
                   (om-draw-string x y (thetext (reference self))) 
                   (setf (rectangle self) (list x (- y fontsize) (+ x sizetext) y)))))

;------------------------
(defmethod open-extra-editor ((self scorepanel) (gtext grap-extra-text))
  (let* ((grap-text (reference gtext))
         (editor (om-view-container self))
         (rect (rectangle gtext))
         (size (round (staff-size self) 2.8)))
    (when (text-view editor) (exit-from-dialog (text-view editor) (om-dialog-item-text (text-view editor))))
    (setf (text-view editor) (om-make-dialog-item 'text-edition-class
                                                  (om-make-point (first rect) (second rect))
                                                  (om-make-point (- (third rect) (first rect)) (- (fourth rect) (second rect)))
                                                  (thetext grap-text)
                                                  :object grap-text 
                                                  :font (om-make-font *om-def-font-face* size)))
    (om-add-subviews self (text-view editor))
    (text-view editor)))

(omg-defclass text-edition-class (edit-text-enter-view) 
   ((object :initform nil :initarg :object :accessor object)))

(defmethod exit-from-dialog ((self text-edition-class) newtext)
   
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (setf (text-view (editor (om-view-container self))) nil)
                            (om-remove-subviews (om-view-container self) self)
                            (om-beep)
                            (abort))))
     (setf (thetext (object self)) (om-dialog-item-text self)) 
     (setf (text-view (editor (om-view-container self))) nil)
     (om-remove-subviews (om-view-container self) self)))


;***************
;NOTE-HEAD
;***************
(defclass! head-extra (extra-objet)
   ((thehead :initform (head-1) :initarg :thehead :accessor thehead)
   (deltax :initform 0  :accessor deltax)
   (deltay :initform 0  :accessor deltay))
   (:icon 280)
   (:documentation 
"
A special note-head to be attached to a particular chord or note in the score.

EXTRA objects are additional data or graphics integrated in the score objects (voice, chord-seq, etc.)
They can be added and manipulated thanks to the Extra package functions (add-extra, etc.)

<thehead> is a string of a single character interpreted graphically in the OM head fonts.
"))

(defmethod get-slot-in-out-names ((self head-extra)) 
   (values '("self" "thehead")
           '(nil  "Q")
           '("object" "head figure")
           '(nil (( 1 (("whole" (head-1)) ("half" (head-1/2)) ("quarter" (head-1/4)) ("square" (head-carre))  
                       ("diamond" (head-losange)) ("rect" (head-rect)) 
                       ("triangle" (head-triangle)) ("circle" (head-cercle))))))))



(defmethod draw-obj-in-rect ((self  head-extra) x x1 y y1 edparams  view)
  (let* ((fontsize 48)
         (thefont (om-make-music-font *heads-font* fontsize))
         (sizetext (round (get-name-size (thehead self) thefont) 2)))
    (om-with-font thefont
                  (om-draw-string (round (- (+ x (/ (- x1 x) 2)) sizetext)) (round (+ y (/ (- y1 y) 2))) (thehead self))
                  )))

(defmethod head-extra-p ((self head-extra)) t)
(defmethod head-extra-p ((self t)) nil)

(defmethod notesforhead ((self container))
   (loop for item in (inside self)
         append (notesforhead item)))
(defmethod notesforhead ((self simple-score-element)) nil)
(defmethod notesforhead ((self note)) (list self))

(defmethod add-head-extra ((self t) val)
   (loop for note in (notesforhead self) do
         (let ((newextra (make-instance 'head-extra :object note)))
           (setf (thehead newextra) val)
           (setf (extra-obj-list note) (remove-if 'head-extra-p (extra-obj-list note))) 
           (push newextra (extra-obj-list note)))))

(defmethod make-graph-extra-obj ((self head-extra) gobj) nil)

(defun get-extra-head (note)
   (let ((head (find-if 'head-extra-p (extra-obj-list note))))
     (when head
       (thehead head))))

(defmethod set-extra-in-list ((extra head-extra) (self t))
   (loop for note in (notesforhead self) do
         (let ((newextra (clone extra)))
           (setf (object newextra) note)
           (setf (extra-obj-list note) (remove-if 'head-extra-p (extra-obj-list note))) 
           (push newextra (extra-obj-list note)))))

;***************
;CHAR-EXTRAS
;***************

(defclass! char-extra (extra-objet)
   ((thechar :initform (head-1/4) :initarg :thechar :accessor thechar))
   (:icon 303))

(defmethod char-extra-p ((self char-extra)) t)
(defmethod char-extra-p ((self t)) nil)


(defmethod draw-obj-in-rect ((self  char-extra) x x1 y y1 edparams  view)
  (let* ((fontsize 24)
         (thefont (om-make-music-font *extras-font* fontsize))
         (sizetext (round (get-name-size (thechar self) thefont) 2)))
    (om-with-font thefont
                  (om-draw-string (round (- (+ x (/ (- x1 x) 2)) sizetext)) (round (+ y (/ (- y1 y) 2))) (thechar self))
                  )))

(defmethod add-new-extra-drag (self where obj (mode (eql 'accent)) dc)
  (when obj
  (let ((newextra (make-instance 'char-extra :object obj)))
     (setf (thechar newextra) (string (car (get-extra-param *extramanager* (edit-mode *extramanager*)))))
     (push newextra (extra-obj-list obj))
     (update-panel self t))))

;;; !!! duplicate
(defmethod add-new-extra-drag (self where obj (mode (eql 'accent)) dc)
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos*) size))
    (setf newextra (make-instance 'char-extra
                                  :object (reference obj) ))
    (setf (thechar newextra) (string (car (get-extra-param *extramanager* (edit-mode *extramanager*)))))
    (setf (deltax newextra) (om-point-h (car points))
          (deltay newextra) (om-point-v (car points)))
    (push newextra (extra-obj-list (reference obj)))
    (update-panel self t)))

;--------------------

(defclass grap-extra-char (grap-extra-objet) ())

(defmethod make-graph-extra-obj ((self char-extra) gobj) 
  (let ((rep (make-instance 'grap-extra-char
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-char) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          (rect (rectangle grap-obj))
          (fontsize size)
          (text (thechar (reference self)))
          (thefont (om-make-music-font *extras-font* fontsize))
          (ls (round size 4))
          (sizetext (get-name-size text thefont))
          points x y)
     (setf points (convert-delta-to-points grap-obj (list (om-make-point (deltax (reference self)) (deltay (reference self))))  size))
     (setf y (om-point-v (car points)))
     (setf x (om-point-h (car points)))
     (om-with-font thefont
                   (om-draw-string (om-point-h (car points)) (om-point-v (car points)) text))
     (setf (rectangle self) (list x (+  ls (- y fontsize)) (+ x sizetext) (+ ls y))))
  )

;***************
;VELOCITIES
;***************


(defclass! vel-extra (extra-objet) 
   ((thechar :initform (dyn-fff) :initarg :thechar :accessor thechar))
   (:icon 499)
   (:documentation 
"
A velocity symbol to be attached to a particular chord or note in the score.

EXTRA objects are additional data or graphics integrated in the score objects (voice, chord-seq, etc.)
They can be added and manipulated thanks to the Extra package functions (add-extra, etc.)

<dynamic> is a string of a single character interpreted graphically in the OM extra fonts.
<deltax> and <deltay> are relative horizontal and vertical offsets (arbitrary non-temporal unit depending on the zoom and font size)

"))

(defmethod get-slot-in-out-names ((self vel-extra))
   (values '("self" "deltax" "deltay" "dynamic")
           '(nil 0 0 "h")
           '("object" "horizontal offset" "vertical offset" "dynamics character" )
           '(nil nil nil (( 3 (("fff" (dyn-fff))  ("ff" (dyn-ff)) ("f" (dyn-f))
                               ("mf" (dyn-mf)) ("mp" (dyn-mp)) ("ppp" (dyn-ppp)) ("pp" (dyn-pp)) ("p" (dyn-p))))))))


(defmethod draw-obj-in-rect ((self vel-extra) x x1 y y1 edparams view)
  (let* ((fontsize 24)
         (thefont (om-make-music-font *extras-font* fontsize))
         (sizetext (round (get-name-size (thechar self) thefont) 2)))
     (om-with-font thefont
                   (om-draw-string (round (- (+ x (/ (- x1 x) 2)) sizetext)) 
                                   (round (+ y (/ (- y1 y) 2))) (thechar self)))))

(defmethod vel-extra-p ((self vel-extra)) t)
(defmethod vel-extra-p ((self t)) nil)


(defmethod add-new-extra-drag (self where obj (mode (eql 'dynamic)) dc)
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos*) size))
    (setf newextra (make-instance 'vel-extra
                                  :object (reference obj) ))
    (setf (thechar newextra) (string (car (get-extra-param *extramanager* (edit-mode *extramanager*)))))
    (setf (deltax newextra) (om-point-h (car points))
          (deltay newextra) (om-point-v (car points)))
    (push newextra (extra-obj-list (reference obj)))
    (set-vel (reference obj) (get-vel-midi (thechar newextra)))
    (update-panel self t)))


(defmethod add-vel-extra ((self t)) 
   (let* ((newextra (make-instance 'vel-extra :object self))
          (notes (notesforhead self))
          (vel (vel (car notes)))
          (dyn (get-dyn-from-vel vel)))
     (setf (thechar newextra) dyn)
     (push newextra (extra-obj-list self))
     (set-vel self vel)))

(defmethod set-extra-in-list ((extra vel-extra) (self t))
   (setf (object extra) self)
   (push extra (extra-obj-list self))
   (set-vel self (get-vel-midi (thechar extra))))


(defun get-vel-midi (str)
   (let ((pos (position str *cur-dynamic-chars* :test 'string-equal)))
   (cond
    (pos (- (nth pos (cdr *cur-dynamic-list*)) 1))
    (t 10))))


(defmethod! set-vel ((self container) vel)
   (loop for item in (inside self) do (set-vel item vel)))

(defmethod! set-vel ((self note) vel)
   (setf (vel self) vel))

(defmethod! set-vel ((self t) vel) t)
;--------------------

(defclass grap-extra-vel (grap-extra-objet) ())

(defmethod make-graph-extra-obj ((self vel-extra) gobj)
   (let ((rep (make-instance 'grap-extra-vel
     :reference  self
     :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-vel) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          (rect (rectangle grap-obj))
          (fontsize (round size 4/3))
          (text (thechar (reference self)))
          (thefont (om-make-music-font *extras-font* fontsize))
          (ls (round size 4))
          (sizetext (get-name-size text thefont))
          points x y)
     (setf points (convert-delta-to-points grap-obj (list (om-make-point (deltax (reference self)) (deltay (reference self))))  size))
     (setf y (om-point-v (car points)))
     (setf x (om-point-h (car points)))
     (om-with-font thefont
                   (om-draw-string (om-point-h (car points)) (om-point-v (car points)) text))
     (setf (rectangle self) (list x (+  ls (- y fontsize)) (+ x sizetext) (+ ls y)))))



;***************
;extras with points
;***************

(defclass compose-extra-object (extra-objet) 
  ((p-points :initform nil :initarg :p-points  :accessor p-points)
   (gparams :initform nil :initarg :gparams  :accessor gparams)))

(defmethod compose-extra-p ((self compose-extra-object)) t)
(defmethod compose-extra-p ((self t)) nil)


(defmethod omNG-save ((self compose-extra-object) &optional (values? nil))
  `(let ((copy ,(call-next-method)))
     (setf (p-points copy) ,(omng-save (p-points self)))
     (setf (gparams copy) ,(omng-save (gparams self)))
   copy))

(defmethod extra-movable-edit-class ((self compose-extra-object))
  'movable-extra-line)

(defmethod click-in-other-point ((self compose-extra-object) where) nil)

(defvar *comp-last-click* nil)
(defvar *which-point* nil)
(defvar *which-extra* nil)
(defvar *pixpoints* nil)

(defmethod do-click-compose-extra ((score scorepanel) (self compose-extra-object) where)
  (let* ((selec-size 6)
         (frame (graphic-frame self))
         (pixpoints (convert-delta-to-points (gobject frame) (p-points self) (staff-size score)))
         point)
    (loop for item in pixpoints
          for i = 0 then (+ i 1)
          while (not point) do
          (when (point-in-rectangle-p where (om-point-v item) (om-point-h item) (+ (om-point-v item) selec-size)  (+ (om-point-h item) selec-size))
            (setf point i)))
    (unless point
      (let ((select-rect (extra-rectangle-selection frame)))
        (when (and select-rect
                   (point-in-rectangle-p where (second select-rect) (car select-rect) (fourth select-rect) (third select-rect)))
          (setf point -1))))
    (unless point
      (setf point (click-in-other-point self where)))
    (when point
      (setf *comp-last-click* where)
      (setf *which-point* point)
      (setf *which-extra* self)
      (setf *pixpoints* pixpoints)
      (om-init-motion-functions score 'drag-points-geometry 'release-points-geometry)
      (om-new-movable-points-geometry score (extra-movable-edit-class self) pixpoints)
      t)))

(defmethod drag-points-geometry ((self scorepanel) pos)
  (if (=  *which-point* -1)
      (let ((diff-point (om-subtract-points *comp-last-click* pos)))
        (setf *pixpoints* (loop for item in *pixpoints*
                              collect (om-make-point (- (om-point-h item)  (om-point-h diff-point))
                                                     (- (om-point-v item)  (om-point-v diff-point))))))
    (setf (nth *which-point* *pixpoints*) pos))
  (setf *comp-last-click* pos)
  (om-update-points-geometry self *pixpoints*))

(defmethod release-points-geometry ((self scorepanel) pos) 
  (let* ((gobj (gobject (graphic-frame *which-extra*)))
         (x0 (car (rectangle gobj)))
         (y0 (second (rectangle gobj))))
  (setf (p-points *which-extra*) (convert-points-to-delta x0 y0 *pixpoints* (staff-size self)))
  (om-invalidate-view self)))


(defun get-color-to-compose-extra (fill color)
 (if fill
      (om-make-color-alpha (om-color-r color) (om-color-g color)  (om-color-b color) 0.4)
    color))

(defclass grap-compose-extra (grap-extra-objet) 
  ((selection-rec :initform nil :initarg :selection-rec :accessor selection-rec)))


(defmethod extra-rectangle-selection ((self grap-compose-extra))
  (append (selection-rec self) (om+ (selection-rec self) '(6 6))))


(defmethod draw-score-selection ((self grap-compose-extra) selection system size)
  (if (member (reference self) selection :test 'equal)
       (let* ((object (reference self))
              (selec-size 6))
         (loop for item in (convert-delta-to-points (gobject self) (p-points object) size) do
               (om-draw-rect  (om-point-h item)  (om-point-v item) selec-size selec-size)))))

(defun convert-points-to-delta (x0 y0 points size)
  (loop for item in points collect
        (om-make-point (/ (- (om-point-h item) x0) size)
                       (/ (- (om-point-v item) y0) size))))

(defun convert-delta-to-points (gobj points size)
  (let ((x0 (car (rectangle gobj)))
        (y0 (second (rectangle gobj))))
  (loop for item in points collect
        (om-make-point (round (+ x0 (* (om-point-h item) size)))
                       (round (+ y0 (* (om-point-v item) size)))))))

;***************
;LINES
;***************

(defclass! line-extra (compose-extra-object) ()
   (:icon 490)
   (:documentation 
"
A vertical line mark symbol to be attached to a particular chord or note in the score.

EXTRA objects are additional data or graphics integrated in the score objects (voice, chord-seq, etc.)
They can be added and manipulated thanks to the Extra package functions (add-extra, etc.)

<deltax> and <deltay> are relative horizontal and vertical offsets (arbitrary non-temporal unit depending on the zoom and font size)
"))

(defclass! arrow-extra (line-extra) ()
   (:icon 490))

(defmethod line-extra-p ((self line-extra)) t)
(defmethod line-extra-p ((self t)) nil)

(defmethod add-new-extra ((self t) (mode (eql 'line)))
   (let ((newextra (make-instance 'line-extra
                     :object self)))
     (push newextra (extra-obj-list self))))

(defmethod do-release-extra-action ((self scorepanel) (mode (eql 'line)) pos) 
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos* pos) size))
    (setf newextra (make-instance 'line-extra
                                  :object (reference obj) 
                                  :p-points points
                                  :gparams (copy-list (score-get-extra-params) )))
   (push newextra (extra-obj-list (reference obj)))
   (update-panel self t)))

(defmethod add-new-extra-drag (self where obj (mode (eql 'line)) dc)
  (om-init-motion-functions self 'make-connection-motion 'release-connection-motion)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-movable-line))

(defmethod draw-obj-in-rect ((self  line-extra) x x1 y y1 edparams  view)
   (om-draw-line (round (+ x (/ (- x1 x) 2))) y (round (+ x (/ (- x1 x) 2))) y1))

(defclass grap-extra-line (grap-compose-extra) ())

(defmethod make-graph-extra-obj ((self line-extra) gobj)
  (let ((rep (make-instance 'grap-extra-line
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-line) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          points
          (params (gparams object))
          (selec-size 6))
     (setf points (convert-delta-to-points grap-obj (p-points object) size))
     (if (and points (>= (length points) 2)) 
     
     (om-with-fg-color nil (fourth params)
       (om-with-line-size (third params)
         (if (equal (second params) 'dash)
             (om-with-dashline
               (om-draw-line (om-point-h (car points)) (om-point-v (car points)) (om-point-h (second points)) (om-point-v (second points))))
           (om-draw-line (om-point-h (car points)) (om-point-v (car points)) (om-point-h (second points)) (om-point-v (second points))))))

       (let* ((rect (rectangle grap-obj))
              (exy (fourth rect))
              (exx (+ (first rect) (round (- (third rect) (first rect))))))
         (om-with-fg-color nil *om-red-color*
           (om-draw-line exx 0 exx (h view)))
         (setf (rectangle self) (list (- exx 2) 0 (+ exx 2) (h view)))))
    (when (equal (score-get-extra-mode) 'line)
       (setf (selection-rec self) (list (+ (om-point-h (car points)) (round (- (om-point-h (second points)) (om-point-h (car points))) 2))
                                        (+ (om-point-v (car points)) (round (- (om-point-v (second points)) (om-point-v (car points))) 2))))
        (om-with-fg-color nil *om-red-color*
          (om-draw-rect (car (selection-rec self)) (second (selection-rec self)) selec-size selec-size)))))



;***************
;RECT
;***************

(defclass! rect-extra (compose-extra-object) ()
   (:icon 490))

(defmethod extra-movable-edit-class ((self rect-extra))
  'movable-extra-rect)

(defmethod do-release-extra-action ((self scorepanel) (mode (eql 'rect)) pos) 
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos* pos) size))
    (setf newextra (make-instance 'rect-extra
                                  :object (reference obj) 
                                  :p-points points
                                  :gparams (copy-list (score-get-extra-params) )))
   (push newextra (extra-obj-list (reference obj)))
   (update-panel self t)))

(defmethod add-new-extra-drag (self where obj (mode (eql 'rect)) dc)
  (om-init-motion-functions self 'make-connection-motion 'release-connection-motion)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-movable-rectangle))

(defmethod draw-obj-in-rect ((self  rect-extra) x x1 y y1 edparams  view)
   (om-draw-line (round (+ x (/ (- x1 x) 2))) y (round (+ x (/ (- x1 x) 2))) y1))

(defclass grap-extra-rect (grap-compose-extra) ())

(defmethod make-graph-extra-obj ((self rect-extra) gobj)
  (let ((rep (make-instance 'grap-extra-rect
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-rect) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          points
          (params (gparams object))
          (selec-size 6) fun)
     (setf points (convert-delta-to-points grap-obj (p-points object) size))
     (setf fun (if (fifth params) 'om-fill-rect  'om-draw-rect))
     (om-with-fg-color nil (get-color-to-compose-extra (fifth params) (fourth params))
       (om-with-line-size (third params)
         (if (equal (second params) 'dash)
             (om-with-dashline
               (funcall fun (om-point-h (car points)) (om-point-v (car points)) 
                         (- (om-point-h (second points)) (om-point-h (car points))) 
                         (- (om-point-v (second points)) (om-point-v (car points)))))
           (funcall fun (om-point-h (car points)) (om-point-v (car points)) 
                         (- (om-point-h (second points)) (om-point-h (car points))) 
                         (- (om-point-v (second points)) (om-point-v (car points)))))))
     (when (equal (score-get-extra-mode) 'rect)
       (setf (selection-rec self) (list (+ (om-point-h (car points)) (round (- (om-point-h (second points)) (om-point-h (car points))) 2))
                                        (+ (om-point-v (car points)) (round (- (om-point-v (second points)) (om-point-v (car points))) 2))))
        (om-with-fg-color nil *om-red-color*
          (om-draw-rect (car (selection-rec self)) (second (selection-rec self)) selec-size selec-size)))))

;***************
;cercle
;***************

(defclass! cercle-extra (compose-extra-object) ()
   (:icon 490))

(defmethod extra-movable-edit-class ((self cercle-extra))
  'movable-extra-cercle)

(defmethod do-release-extra-action ((self scorepanel) (mode (eql 'circ)) pos) 
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos* pos) size))
    (setf newextra (make-instance 'cercle-extra
                                  :object (reference obj) 
                                  :p-points points
                                  :gparams (copy-list (score-get-extra-params) )))
   (push newextra (extra-obj-list (reference obj)))
   (update-panel self t)))

(defmethod add-new-extra-drag (self where obj (mode (eql 'circ)) dc)
  (om-init-motion-functions self 'make-connection-motion 'release-connection-motion)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-movable-rectangle))

(defmethod draw-obj-in-rect ((self  rect-extra) x x1 y y1 edparams  view)
   (om-draw-line (round (+ x (/ (- x1 x) 2))) y (round (+ x (/ (- x1 x) 2))) y1))

(defclass grap-extra-cercle (grap-compose-extra) ())

(defmethod make-graph-extra-obj ((self cercle-extra) gobj)
  (let ((rep (make-instance 'grap-extra-cercle
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-cercle) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          points
          (params (gparams object))
          (selec-size 6)
          x0 y0 w h fun)
     (setf points (convert-delta-to-points grap-obj (p-points object) size))
     (setf w (round (- (om-point-h (second points)) (om-point-h (car points))) 2)
           h (round (- (om-point-v (second points)) (om-point-v (car points))) 2)
           x0 (+ (om-point-h (car points)) w)
           y0 (+ (om-point-v (car points)) h))
     (setf fun (if (fifth params) 'om-fill-ellipse 'om-draw-ellipse))
     (om-with-fg-color nil  (get-color-to-compose-extra (fifth params) (fourth params))
       (om-with-line-size (third params)
         (if (equal (second params) 'dash)
             (om-with-dashline
               (funcall fun x0 y0 w h))
           (funcall fun x0 y0 w h))))
      (when (equal (score-get-extra-mode) 'circ)
       (setf (selection-rec self) (list (+ (om-point-h (car points)) (round (- (om-point-h (second points)) (om-point-h (car points))) 2))
                                        (+ (om-point-v (car points)) (round (- (om-point-v (second points)) (om-point-v (car points))) 2))))
       (om-with-fg-color nil *om-red-color*
         (om-draw-rect (car (selection-rec self)) (second (selection-rec self))  selec-size  selec-size)))))


;***************
;polygon
;***************

(defclass! polygon-extra (compose-extra-object) ()
   (:icon 490))

(defmethod extra-movable-edit-class ((self polygon-extra))
  'movable-extra-polygon)

(defvar *polygon-points* nil)

(defmethod add-new-extra-drag (self where obj (mode (eql 'polyg)) dc)
  (push where *polygon-points*)
  (om-with-focused-view self
    (loop for item in *polygon-points*
          for next in (cdr *polygon-points*) do
          (om-draw-line (om-point-h item) (om-point-v item) (om-point-h next) (om-point-v next))))
  (when dc
    (close-score-polygon self)))


(defmethod close-score-polygon ((self scorePanel))
  (when *polygon-points*
    (let ((points (loop for item in *polygon-points* append (list (om-point-h item) (om-point-v item)))))
      (gp::draw-polygon self points :closed t))
    (let* ((size (staff-size self))
           newextra obj points)
      (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
      (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) *polygon-points* size))
      (setf newextra (make-instance 'polygon-extra
                                    :object (reference obj)
                                    :p-points points
                                    :gparams (copy-list (score-get-extra-params) )))
      (push newextra (extra-obj-list (reference obj)))
      (setf *polygon-points* nil)
      (update-panel self t))))

(defmethod draw-obj-in-rect ((self  polygon-extra) x x1 y y1 edparams  view)
   (om-draw-line (round (+ x (/ (- x1 x) 2))) y (round (+ x (/ (- x1 x) 2))) y1))

(defclass grap-extra-polygon (grap-compose-extra) ())

(defmethod make-graph-extra-obj ((self polygon-extra) gobj)
  (let ((rep (make-instance 'grap-extra-polygon
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-polygon) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          points
          (params (gparams object))
          (selec-size 6))
     (setf points (convert-delta-to-points grap-obj (p-points object) size))
     (setf fun (if (fifth params) 'om-fill-polygon 'om-draw-polygon))
     (om-with-fg-color nil (get-color-to-compose-extra (fifth params) (fourth params))
       (om-with-line-size (third params)
         (if (equal (second params) 'dash)
             (om-with-dashline
               (funcall fun points))
           (funcall fun points))))
     (when (equal (score-get-extra-mode) 'polyg)
       (setf (selection-rec self) (list (om-point-h (car points)) (om-point-v (car points)) ))
       (om-with-fg-color nil *om-red-color*
         (om-draw-rect (car (selection-rec self)) (second (selection-rec self)) (* 2 selec-size) (* selec-size 2))))))

(defmethod extra-rectangle-selection ((self grap-extra-polygon))
  (append (selection-rec self) (om+ (selection-rec self) '(12 12))))


;***************
; pen
;***************

(defclass! draw-extra (compose-extra-object) ()
   (:icon 490))



;***************
;dynamic-dynamic
;***************

(defclass! d-dynamic-extra (compose-extra-object)
((cresc :initform nil :initarg :cresc :accessor cresc)
 (start-val :initform nil :initarg :start-val :accessor start-val)
 (end-val :initform nil :initarg :end-val :accessor end-val))
   (:icon 490))

(defmethod extra-movable-edit-class ((self d-dynamic-extra))
  'movable-extra-rect)

(defmethod d-dynamic-extra-p ((self d-dynamic-extra)) t)
(defmethod d-dynamic-extra-p ((self t)) nil)

(defmethod draw-cresc ( x x1 y y1  )
  (om-draw-line x (round (+ y (/ (- y1 y) 2))) x1 y)
  (om-draw-line  x (round (+ y (/ (- y1 y) 2))) x1 y1))

(defmethod draw-decresc ( x x1 y y1   )
  (om-draw-line x y  x1 (round (+ y (/ (- y1 y) 2))))
  (om-draw-line  x y1  x1 (round (+ y (/ (- y1 y) 2)))))

;cresc
(defclass! crescendo (d-dynamic-extra) ()
 (:default-initargs  :cresc t))

(defmethod crescendo-p ((self crescendo)) t)
(defmethod crescendo-p ((self t)) nil)


(defmethod draw-obj-in-rect ((self  crescendo) x x1 y y1 edparams  view)
  (om-draw-line x (round (+ y (/ (- y1 y) 2))) x1 y)
  (om-draw-line  x (round (+ y (/ (- y1 y) 2))) x1 y1))

(defmethod do-release-extra-action ((self scorepanel) (mode (eql 'cresc)) pos) 
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos* pos) size))
    (setf newextra (make-instance 'crescendo
                                  :object (reference obj) 
                                  :p-points points
                                  :gparams (copy-list (score-get-extra-params) )))
   (push newextra (extra-obj-list (reference obj)))
   (update-panel self t)))

(defmethod add-new-extra-drag (self where obj (mode (eql 'cresc)) dc)
  (om-init-motion-functions self 'make-connection-motion 'release-connection-motion)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-movable-cresc))

;decresc
(defclass! diminuendo (d-dynamic-extra) ()
           (:default-initargs  :cresc nil))

(defmethod draw-obj-in-rect ((self  diminuendo) x x1 y y1 edparams  view)
  (om-draw-line x y  x1 (round (+ y (/ (- y1 y) 2))))
  (om-draw-line  x y1  x1 (round (+ y (/ (- y1 y) 2)))))

(defmethod do-release-extra-action ((self scorepanel) (mode (eql 'decresc)) pos) 
  (let ((size (staff-size self))
        newextra obj points)
    (setf obj  (get-near-obj-from-pixel (graphic-obj self) t *extra-initial-pos*))
    (setf points (convert-points-to-delta (car (rectangle obj)) (second (rectangle obj)) (list *extra-initial-pos* pos) size))
    (setf newextra (make-instance 'diminuendo
                                  :object (reference obj) 
                                  :p-points points
                                  :gparams (copy-list (score-get-extra-params) )))
   (push newextra (extra-obj-list (reference obj)))
   (update-panel self t)))

(defmethod add-new-extra-drag (self where obj (mode (eql 'decresc)) dc)
  (om-init-motion-functions self 'make-connection-motion 'release-connection-motion)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-movable-decresc))


(defclass grap-d-dynamic-extra (grap-compose-extra) ())

(defmethod make-graph-extra-obj ((self d-dynamic-extra) gobj)
  (let ((rep (make-instance 'grap-d-dynamic-extra
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-d-dynamic-extra) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          points
          (params (gparams object))
          (selec-size 6) fun)
     (setf points (convert-delta-to-points grap-obj (p-points object) size))
     (setf fun (if (crescendo-p (reference self)) 'draw-cresc 'draw-decresc))
     (om-with-fg-color view (fourth params)
       (om-with-line-size (third params)
         (if (equal (second params) 'dash)
             (om-with-dashline
               (funcall fun (om-point-h (car points)) (om-point-h (second points))
                        (om-point-v (car points))  (om-point-v (second points))))
           (funcall fun (om-point-h (car points)) (om-point-h (second points))
                    (om-point-v (car points)) (om-point-v (second points))))))
     (when (or (equal (score-get-extra-mode) 'cresc) (equal (score-get-extra-mode) 'decresc))
       (setf (selection-rec self) (list (+ (om-point-h (car points)) (round (- (om-point-h (second points)) (om-point-h (car points))) 2))
                                        (+ (om-point-v (car points)) (round (- (om-point-v (second points)) (om-point-v (car points))) 2))))
        (om-with-fg-color nil *om-red-color*
          (om-draw-rect (car (selection-rec self)) (second (selection-rec self)) selec-size selec-size)))))

;***************
;slur
;***************

(defvar *slur-accuracy* 5)
(setf *slur-accuracy* 20)

(defun output-bezier (x0 y0 x1 y1 x2 y2 x3 y3 n)
  (let* ((cx (* 3 (- x1 x0)))
	 (cy (* 3 (- y1 y0)))
	 (bx (- (* 3 (- x2 x1)) cx))
	 (by (- (* 3 (- y2 y1)) cy))
	 (ax (- x3 x0 cx bx))
	 (ay (- y3 y0 cy by))
	 (incr (/ 1.0 n))
         (lastx x0)
         (lasty y0))
    (loop for i from 0 to 1 by incr do
      (om-draw-line lastx lasty
		(+ x0 (* i (+ cx (* i (+ bx (* i ax))))))
		(+ y0 (* i (+ cy (* i (+ by (* i ay)))))))
      (setf lastx (+ x0 (* i (+ cx (* i (+ bx (* i ax))))))
            lasty (+ y0 (* i (+ cy (* i (+ by (* i ay))))))))))

(defun g-curveto ( x0 y0 x1 y1 x2 y2 x3 y3)
  (output-bezier  x0 y0 x1 y1 x2 y2 x3 y3 *slur-accuracy*)
  (output-bezier  x0 y0 x1 (- y1 1) x2 (- y2 1) x3 y3 *slur-accuracy*))


(defclass! slur (compose-extra-object)
           ((slurname :initform nil :initarg :slurname :accessor slurname)
            (b-s-p :initform t :initarg :b-s-p  :accessor b-s-p))
           (:icon 490))

(defmethod extra-movable-edit-class ((self slur))
  'movable-extra-slur)

(defmethod add-new-extra-drag (self where obj (mode (eql 'slur)) dc)
  (if obj
     (progn
       (om-init-motion-functions self 'make-connection-motion 'release-connection-motion)
       (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-movable-line))
    (om-beep)))

(defmethod do-release-extra-action ((self scorepanel) (mode (eql 'slur)) pos) 
  (let* ((size (staff-size self))
        (mode-obj (grap-class-from-type  (obj-mode self)))
        (target (get-click-in-obj self (graphic-obj self) mode-obj pos))
        newextra obj points0 points1)
    (if target
        (let* ((slurname (gensym)) newextrab newextrae)
          (setf points0 (convert-points-to-delta (car (rectangle *start-extra-gobj-click*)) (second (rectangle *start-extra-gobj-click*)) 
                                                 (list *extra-initial-pos* (om-add-points *extra-initial-pos* (om-make-point size size))) size))
          (setf points1 (convert-points-to-delta (car (rectangle target)) (second (rectangle target)) (list (om-add-points pos (om-make-point (* -1 size) size)) pos) size))
          (setf newextrab (make-instance 'slur
                                    :object *start-extra-obj-click*
                                    :slurname slurname
                                    :p-points points0
                                    :b-s-p t))
          (setf newextrae (make-instance 'slur
                                    :object (reference target)
                                    :slurname slurname
                                    :p-points points1
                                    :gparams (copy-list (score-get-extra-params) )
                                    :b-s-p nil))
          (push newextrab (extra-obj-list *start-extra-obj-click*))
          (push newextrae (extra-obj-list (reference target)))
          (update-panel self t))
      (om-beep))))


(defmethod draw-obj-in-rect ((self  slur) x x1 y y1 edparams  view)
   (om-draw-line (round (+ x (/ (- x1 x) 2))) y (round (+ x (/ (- x1 x) 2))) y1))

(defclass grap-slur-extra (grap-compose-extra) ())

(defmethod make-graph-extra-obj ((self slur) gobj)
  (let ((rep (make-instance 'grap-slur-extra
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-slur-extra) view size staff)
   (let* ((grap-obj (gobject self))
          (object (reference self))
          points0 points1
          (params (gparams object))
          (selec-size 6))
     (push self *extra-with-pairs-list*)
     (unless (b-s-p object)
       (let ((beginslur (get-init-slur object)))
         (when beginslur
           (let ((object0 (reference beginslur)))
             (setf points0 (convert-delta-to-points (gobject beginslur) (p-points object0) size))
             (setf points1 (convert-delta-to-points (gobject self) (p-points object) size))
           (apply 'g-curveto (loop for item in (append points0 points1) append (list (om-point-h item) (om-point-v item))))
           (when (equal (score-get-extra-mode) 'slur)
             (setf (selection-rec self) (list (+ (om-point-h (car points0)) (round (- (om-point-h (second points1)) (om-point-h (car points0))) 2))
                                              (+ (om-point-v (car points0)) (round (- (om-point-v (second points1)) (om-point-v (car points0))) 2))))
            (om-with-fg-color nil *om-red-color*
              (om-draw-rect (car (selection-rec self)) (second (selection-rec self)) selec-size selec-size)))))))))

(defmethod draw-score-selection ((self grap-slur-extra) selection system size)
  (when (member (reference self) selection :test 'equal)
     (let* ((beginslur (get-init-slur (reference self)))
            (points (get-slur-points beginslur self size))
            (selec-size 6))
      (loop for item in points do
            (om-draw-rect  (om-point-h item)  (om-point-v item) selec-size selec-size))
      (om-draw-rect (+ (om-point-h (second points)) (round (- (om-point-h (third points)) (om-point-h (second points))) 2))
                    (+ (om-point-v (second points)) (round (- (om-point-v (third points)) (om-point-v (second points))) 2))
                    selec-size selec-size)
      (om-with-fg-color nil *om-gray-color*
        (om-draw-polygon points)))))

(defmethod get-slur-points ((beginslur grap-slur-extra) (endslur grap-slur-extra) size)
   (append (convert-delta-to-points (gobject beginslur) (p-points (reference beginslur)) size)
           (convert-delta-to-points (gobject endslur) (p-points (reference endslur)) size)))
 
(defun get-init-slur (end-slur)
  (let (rep)
    (loop for item in *extra-with-pairs-list* 
          while (not rep) do
          (when (and (equal (slurname (reference item)) (slurname end-slur))
                     (b-s-p (reference item)))
            (setf rep item)))
    rep))

(defvar *which-slur* nil)
(defmethod do-click-compose-extra ((score scorepanel) (self slur) where)
  (let* ((selec-size 6)
         (size (staff-size score))
         (frame (graphic-frame self))
         (beginslur (get-init-slur (reference frame)))
         (pixpoints (get-slur-points beginslur frame size))
         point)
    (loop for item in pixpoints
          for i = 0 then (+ i 1)
          while (not point) do
          (when (point-in-rectangle-p where (om-point-v item) (om-point-h item) (+ (om-point-v item) selec-size)  (+ (om-point-h item) selec-size))
            (setf point i)))
    (unless point
      (let ((select-rect (extra-rectangle-selection frame)))
      (when (point-in-rectangle-p where (second select-rect) (car select-rect) (fourth select-rect) (third select-rect) )
        (setf point -1))))
    (unless point
      (when (point-in-rectangle-p where 
                                  (+  (om-point-v (second pixpoints)) (round (- (om-point-v (third pixpoints)) (om-point-v (second pixpoints))) 2))
                                  (+ (om-point-h (second pixpoints)) (round (- (om-point-h (third pixpoints)) (om-point-h (second pixpoints))) 2)) 
                                  (+ selec-size (om-point-v (second pixpoints)) (round (- (om-point-v (third pixpoints)) (om-point-v (second pixpoints))) 2)) 
                                  (+ selec-size (om-point-h (second pixpoints)) (round (- (om-point-h (third pixpoints)) (om-point-h (second pixpoints))) 2)) )
        (setf point -2)))
    (when point
      (setf *comp-last-click* where)
      (setf *which-point* point)
      (setf *which-extra* self)
      (setf *which-slur* (reference beginslur) )
      (setf *pixpoints* pixpoints)
      (om-init-motion-functions score 'drag-points-slur 'release-points-slur)
      (om-new-movable-points-geometry score (extra-movable-edit-class self) pixpoints)
      t)))

(defmethod drag-points-slur ((self scorepanel) pos)
  (let ((diff-point (om-subtract-points *comp-last-click* pos)))
       (cond
        ((=  *which-point* -1)
         (setf *pixpoints* (loop for item in *pixpoints*
                                 collect (om-make-point (- (om-point-h item)  (om-point-h diff-point))
                                                        (- (om-point-v item)  (om-point-v diff-point))))))
        ((=  *which-point* -2)
         (setf (nth 1 *pixpoints*) (om-make-point (- (om-point-h (nth 1 *pixpoints*))  (om-point-h diff-point))
                                                        (- (om-point-v (nth 1 *pixpoints*))  (om-point-v diff-point))))
         (setf (nth 2 *pixpoints*) (om-make-point (- (om-point-h (nth 2 *pixpoints*))  (om-point-h diff-point))
                                                        (- (om-point-v (nth 2 *pixpoints*))  (om-point-v diff-point)))))
        (t (setf (nth *which-point* *pixpoints*) pos)))
  (setf *comp-last-click* pos)
  (om-update-points-geometry self *pixpoints*)))

(defmethod release-points-slur ((self scorepanel) pos) 
  (let* ((gobj (gobject (graphic-frame *which-slur*)))
         (x0 (car (rectangle gobj)))
         (y0 (second (rectangle gobj)))
         (gobj1 (gobject (graphic-frame *which-extra*)))
         (x1 (car (rectangle gobj1)))
         (y1 (second (rectangle gobj1))))
  (setf (p-points *which-slur*) (convert-points-to-delta x0 y0 (list (car *pixpoints*) (second *pixpoints*)) (staff-size self)))
  (setf (p-points *which-extra*) (convert-points-to-delta x1 y1 (list (third *pixpoints*) (fourth *pixpoints*)) (staff-size self)))
  (om-invalidate-view self)))

;***************
;PICTS
;***************

(defclass! pict-extra (extra-objet picture) 
   ((factx :initform 1 :accessor factx)
    (facty :initform 1 :accessor facty))
   (:icon 491))

(defmethod pict-extra-p ((self pict-extra)) t)
(defmethod pict-extra-p ((self t)) nil)


(defmethod omNG-save ((self pict-extra) &optional (values? nil))
  `(let ((copy ,(call-next-method)))
     (setf (factx copy) ,(factx self))
     (setf (facty copy) ,(facty self))
     (setf (deltax copy) ,(deltax self))
     (setf (deltay copy) ,(deltay self))
     copy))


(defmethod make-one-instance ((class pict-extra) &rest slots-vals) 
   (let ((val (get-picture-file)))
     (change-class val 'pict-extra)
     (setf (deltax val) (car slots-vals))
     (setf (deltay val) (second slots-vals))
     val))

(defmethod convert-extra ((self pict-extra)) self)

(defmethod omNG-copy ((self pict-extra))
  `(let ((copy ,(call-next-method)))
     (setf (factx copy) ,(factx self))
     (setf (facty copy) ,(facty self))
     (setf (deltax copy) ,(deltax self))
     (setf (deltay copy) ,(deltay self))
     copy))


(defclass grap-extra-pict (grap-extra-objet) ())

(defmethod make-graph-extra-obj ((self pict-extra) gobj)
  (let ((rep (make-instance 'grap-extra-pict
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-pict) view size staff)
  (when view
    (when (thepict (reference self))
      (let* ((picture (thepict (reference self)))
             (grap-obj (gobject self))
             (object (reference self))
             (rect (rectangle grap-obj))
             (ls (round size 4))
             (size-pict (om-get-picture-size picture))
             x y)
        (when picture
          (setf y (+ (* 3 ls) (second rect)))
          (setf x (- (+ (first rect) (round (- (third rect) (first rect)))) (round (om-point-h size-pict) 2) ls))
          (setf y (+ y (* ls (deltay (reference self)))))
          (setf x (+ x (* ls (deltax (reference self)))))
          (om-draw-picture (om-get-current-port) picture :pos (om-make-point x y) :size size-pict))
        (setf (rectangle self) (list x y (+ x (om-point-h size-pict)) (+ y (om-point-v size-pict)))))
      )))

(defmethod draw-score-selection ((self grap-extra-pict) selection system size)
   (if (member (reference self) selection :test 'equal)
     (draw-h-rectangle (rectangle self) )))




