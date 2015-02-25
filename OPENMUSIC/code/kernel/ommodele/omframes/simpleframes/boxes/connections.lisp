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
;This file defines a class for connections between OMBox objects
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defclass c-connection ()
   ((points :initform nil :accessor points)
    (index :initform nil :initarg :index :accessor index)
    (selected? :initform nil :accessor selected?)
    (thebox :initform nil :initarg :thebox :accessor thebox)
    (ccolor :initform 0 :initarg :ccolor :accessor ccolor)
    (point-sel :initform nil :accessor point-sel)))

(defun get-ramped-sine-pts (pts resolution)
  (let ((x1 (om-point-h (car pts)))
        (y1 (om-point-v (car pts)))
        (x2 (om-point-h (last-elem pts)))
        (y2 (om-point-v (last-elem pts))))
        
    (let* ((width (abs (- x2 x1)))
         ;calculate 'mirrored' y2 (a clipped linear function)
           (anti-y2 (om-max 
                     (+ (* -1/3 y2)
                        (* 4/3 (+ y1 (* 1/2 width))))
                     y2)
                    ))

      (loop for k from 0 to resolution
            for rad = (om-scale k -1.57 1.57 0 resolution)
            for ramp = (* (* (+ (sin (om-scale k -1.57 1.57 0 resolution)) 1) 0.5) ;; 0 to 1 half-sine-curve 
                          (- anti-y2 y2)) ;; positive number
            collect
            (om-make-point (om-scale (sin rad) x1 x2 -1 1)
                           (- (om-scale k y1 anti-y2 0 resolution)
                              ramp
                              ))))))

(defparameter *curve-draw-resolution* 50)
(defparameter *curve-detect-resolution* 250)

(defmethod class-of-connection ((self t)) 'c-connection)
(defmethod get-graph-points ((self c-connection))
   (if *curved-connections*
       (get-ramped-sine-pts (points self) *curve-draw-resolution*)
     (points self)))

(defmethod get-graph-point-sel ((self c-connection))
   (point-sel self))

(defmethod get-rectangle-space ((self c-connection))
   (let ((minx 6000) (miny 6000)
         (maxx 0) (maxy 0))
     (loop for item in (get-graph-points self) do
           (setf minx (min minx (om-point-h  item)))
           (setf maxx (max maxx (om-point-h  item)))
           (setf miny (min miny (om-point-v  item)))
           (setf maxy (max maxy (om-point-v  item))))
     (list minx maxx miny maxy)))
        
(defmethod select-connection ((self c-connection))
  (draw-connection self nil)
  (cond ((om-shift-key-p) (setf (selected? self) (not (deactivate-connect self))))
        (t (unless (selected? self)
             (setf (selected? self ) t))))
  (draw-connection self t))

(defvar *con-last-click* nil)
(defvar *con-offset-click* nil)
(defvar *con-first-click* nil)
(defvar *cur-drag-connection* nil)
(defvar *first-point* nil)
(defvar *init-con-rect* nil)

(defmethod scroll-points ((Self c-connection))
  (unless *curved-connections*
    (let* ((scroller (panel (thebox self)))
           (where (om-mouse-position scroller)))
      (setf *con-last-click* where)
      (setf *con-first-click* where)
      (setf *con-offset-click* (om-make-point 0 0))
      (setf *cur-drag-connection* self)
      (setf *first-point* (copy-list (points *cur-drag-connection*)))
      (setf *init-con-rect* (get-rectangle-space *cur-drag-connection*))
      (om-init-motion-functions scroller 'make-drag-conections 'release-drag-conections)
  ;(om-new-movable-object scroller (first (get-rectangle-space *cur-drag-connection*))
  ;                       (third (get-rectangle-space *cur-drag-connection*)) 4 4 'om-movable-line)
      )))

(defmethod release-drag-conections ((Self relationPanel) Where)
  (let (finalrect)
    (setf (point-sel *cur-drag-connection*) 
	  (loop for item in (point-sel *cur-drag-connection*) 
                collect (nth (position item *first-point*) (points *cur-drag-connection*))))
    (unless (equal (points *cur-drag-connection*) *first-point*)
      (setf (nth 2 (connected? (object (nth (index *cur-drag-connection*) (inputframes (thebox *cur-drag-connection*))))))
	    (points *cur-drag-connection*)))
    (setf finalrect (get-rectangle-space *cur-drag-connection*))
    (om-invalidate-corners self 
			   (om-make-point (- (min (first *init-con-rect*) (first finalrect)) 4)
					  (- (min (third *init-con-rect*) (third finalrect)) 4))
			   (om-make-point (+ 4 (max (second *init-con-rect*) (second finalrect)))
					  (+ 4 (max (fourth *init-con-rect*) (fourth finalrect)))))))


(defun invalidate-connection-region (connection pane)
  (let ((rect (get-rectangle-space connection)))
    (om-invalidate-corners pane 
                           (om-make-point (- (first rect) 8) (- (third rect) 8))
                           (om-make-point (+ 8 (second rect)) (+ 8 (fourth rect))))
    ))

(defmethod make-drag-conections ((Self relationPanel) Where) 
  (let* ((old-mouse *con-last-click*)
         (first-mouse *con-first-click*)
         (new-mouse old-mouse))
    (setq new-mouse where)
    (draw-connection *cur-drag-connection* NIL)
    (invalidate-connection-region *cur-drag-connection* self)
    (loop for point in (point-sel *cur-drag-connection*) do
          (let ((pos (position point *first-point*))) 
            (setf (nth pos (points *cur-drag-connection*))
                  (scr-point-con *cur-drag-connection* self (nth pos *first-point*) 
                                 (om-subtract-points new-mouse first-mouse))))) 
    (draw-connection *cur-drag-connection* 'redraw)
    (setq *con-last-click* where)))


(defmethod scr-point-con ((self c-connection) container point0 delta)
   (declare (ignore container))
   (om-add-points point0 delta))

(defun m-line (point0 point1)
   (cond
    ((= (om-point-h  point1) (om-point-h  point0)) 10000)
    (t (* -1.0 (/ (- (om-point-v  point1) (om-point-v  point0)) (- (om-point-h  point1) (om-point-h  point0) ))))))


(defmethod point-in-connection ((self c-connection) container where)
   (let* ((points (copy-list (get-graph-points self)))
          (len (- (length points) 1))
          (primo (pop points))
          (i 0) region rep
          (tolerance 3))
     (when points
       (om-with-focused-view container
         (loop for item in (get-graph-points self)
               while (not rep) do
                   (when (not (or (= i 0) (= i len)))
                       (setf region (om-make-rect (- (om-point-h item) tolerance) (- (om-point-v item) tolerance)
                                                  (+ (om-point-h item) tolerance) (+ (om-point-v item) tolerance)))
                       (when (om-point-in-rect-p where region)
                           (setf rep i)))
               (incf i))
         (unless rep
            (om-open-region container)
           (loop while points do
                     (let* ((seco (car points)))
                       (unless (om-points-equal-p primo seco)  
                          (om-open-region-add-line primo seco 4))
                       (setf primo (pop points))))
            (setf region (om-close-region container))
            (setf rep (om-point-in-region-p region where))
            (om-dispose-region region)
            )
   rep))))


(defmethod deactivate-connect ((self c-connection))
   (when (selected? self)
     (draw-connection self nil)
     (setf (point-sel self) nil)
     (setf (selected? self ) nil)
     (invalidate-connection-region self (connection-container (thebox self)))
     (draw-connection self t) t))
  
(defmethod new-color-connection ((self c-connection))
   (setf (ccolor self) (mod (+ (ccolor self) 1) 16))
   (setf (nth 3 (connected? (object (nth (index self) (inputframes (thebox self))))))
         (ccolor self))
   (draw-connection self t))

(defmethod change-position ((self c-connection))
  (let ((new-points (get-connection-lines (thebox self) (index self)))) 
    (setf (points self) new-points)
    (when (nth 2 (connected? (nth (index self) (inputs (object (thebox self))))))
      (setf new-points (list+ (list (car new-points)) 
                              (cdr (butlast (nth 2 (connected? (nth (index self) 
                                                                    (inputs (object (thebox self))))))))
                              (last new-points)))
      (setf (nth 2 (connected? (nth (index self) (inputs (object (thebox self)))))) new-points)
      (setf (points self) new-points))))

(defmethod reinit-connection ((self c-connection))
  (let ((new-points (get-connection-lines (thebox self) (index self)))) 
    (setf (points self) new-points)
    (when (nth 2 (connected? (nth (index self) (inputs (object (thebox self))))))
      (setf new-points (list+ (list (car new-points)) 
                              (cdr (butlast (nth 2 (connected? (nth (index self) 
                                                                    (inputs (object (thebox self))))))))
                              (last new-points)))
      (setf (nth 2 (connected? (nth (index self) (inputs (object (thebox self)))))) new-points)
      (setf (points self) new-points))))

(defmethod new-connection ((self omboxframe) index)
  (let ((theconnection (make-instance (class-of-connection self) :index index :thebox self)))
    (setf (points theconnection)
          (get-connection-lines self index))
    ;;; test supprimer
    ;;;(draw-connection theconnection t)
    ;(loop for p in (points theconnection) do (print-point p))
    theconnection))

(defmethod connection-container ((self t)) (om-view-container self))

(defmethod draw-connection ((self c-connection) val)
  (let* ((thepoints (copy-list (get-graph-points self)))
         (prim (pop thepoints))
         (sel? (selected? self))
         (color (if (zerop (ccolor self)) *om-black-color* (nth (-  (ccolor self) 1) *16-color-list*))))
    (om-with-focused-view (connection-container (thebox self)) 
      (om-with-fg-color nil color
	(om-with-line-size (if sel? 2 1)
          (if val
              (loop while thepoints do
		   (om-draw-line (om-point-h prim) (om-point-v prim) 
                                 (om-point-h (car thepoints)) (om-point-v (car thepoints))
                                 :erasable (equal val 'redraw))
		   (setf prim (pop thepoints))
		   (when thepoints
		     (if (member prim (point-sel self))
			 (om-fill-rect (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4 :erasable (equal val 'redraw))
			 (when (and sel? (not *curved-connections*))
			   (om-draw-rect (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4 :erasable (equal val 'redraw)))
			 ))
		   )
	      (loop while thepoints do
		   (om-erase-line (om-point-h  prim) (om-point-v  prim) (om-point-h (car thepoints)) (om-point-v (car thepoints)))
		   (setf prim (pop thepoints))
		   (when thepoints
		     (if (member prim (point-sel self))
			 (om-erase-rect-content (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4)
			 (when sel?
			   (om-erase-rect (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4))
			 ))
		   )))))))


(defmethod redraw-connections ((self omboxframe))
  (when (connections self)
    (let ((eraserect (list 10000 0 10000 0)))
      (mapc #'(lambda (conection)
		(let ((initrect (get-rectangle-space conection)) finalrect)
		  (setf eraserect (list (min (first eraserect) (first initrect))
					(max (second eraserect) (second initrect))
					(min (third eraserect) (third initrect))
					(max (fourth eraserect) (fourth initrect))))
		  (change-position conection)
		  (setf finalrect (get-rectangle-space conection))
		  (setf eraserect (list (min (first eraserect) (first finalrect))
					(max (second eraserect) (second finalrect))
					(min (third eraserect) (third finalrect))
					(max (fourth eraserect) (fourth finalrect)))))) (connections self))
      (om-invalidate-corners (om-view-container self) 
			     (om-make-point (- (first eraserect) 4) (- (third eraserect) 4))
			     (om-make-point (+ 4 (second eraserect)) (+ 4 (fourth eraserect))))
      )))

(defmethod box-draw-connections ((self omboxframe) val)
  (mapc #'(lambda (conection)
            (draw-connection conection val)) (connections self)))

(defmethod remove-connection ((self omboxframe) index)
  (flet ((match (x) (= index (index x))))
    (let ((count (position-if #'match (connections self))))
      (when count
        (let ((rectangle (get-rectangle-space (nth count (connections self))))) 
          (setf (connections self) (remove-if #'match (connections self)))
          (om-invalidate-corners (om-view-container self) 
                              (om-make-point (- (first rectangle) 4) (- (third rectangle) 4))  
                              (om-make-point (+ 4 (second rectangle)) (+ 4 (fourth rectangle)))))))))


(defmethod get-connect-pos-source ((self t))
  (om-view-position self))

(defmethod get-connection-lines ((self omboxframe) i)
  (let* ((ctrl (nth i (inputframes self)))
         (connection (connected? (object ctrl)))
         (boxsource (first connection))
         (possource (get-connect-pos-source (car (frames boxsource))))
         (sizesource (om-view-size (car (frames boxsource)))))
    (get-panel-connections-lines self (panel self) ctrl connection boxsource possource sizesource)))

(defmethod get-panel-connections-lines ((self omboxframe) (panel t) in connection boxsource possource sizesource)
  (let ((x-self (x self))
        (y-self (y self))
        (in-xs (x in))
        x1 y1 xi yi)
    (setq x1 (+ (om-point-h possource)
                (- (* (+ (second connection) 1) (round (om-point-h sizesource) (+ (numouts boxsource) 1))) 2)))
    (setq y1 (- (+ (om-point-v possource) (om-point-v sizesource)) 2)) 
    (setq xi (+ x-self in-xs 4))
    (setq yi  y-self)
    (get-rect-connection x1 y1 xi yi)))

(defun get-rect-connection (x1 y1 xi yi)
  (let ((midx (round (abs (- xi x1)) 2))
        (midy (round (abs (- yi y1)) 2)))
    (cond
     ((< y1 yi)
      (list (om-make-point x1  y1)
            (om-make-point x1 (+ y1 midy))
            (om-make-point xi (+ y1 midy))
            (om-make-point xi yi)))
     ((>= x1 xi) 
      (list (om-make-point x1  y1)
            (om-make-point (- x1 midx) y1)
            (om-make-point (- x1 midx) (- yi 5))
            (om-make-point xi (- yi 5))
            (om-make-point xi yi)))
     (t
      (list (om-make-point x1  y1)
            (om-make-point (+ x1 midx) y1)
            (om-make-point (+ x1 midx) (- yi 5))
            (om-make-point xi (- yi 5))
            (om-make-point xi yi))))
    ))

(defun get-line-connection (x1 y1 xi yi)
  (list (om-make-point x1 y1) (om-make-point xi yi)))

;=======PRINT

;;;(defmethod print-connection ((self c-connection))
;;;   (let* ((thepoints (copy-list (get-graph-points self)))
;;;          (prim (pop thepoints))
;;;          (color (if (zerop (ccolor self)) *black-color* (nth (-  (ccolor self) 1) *16-color-list*)))) 
;;;     (om-with-fg-color nil color
;;;       (loop while thepoints do
;;;                 ;;;(#_MoveTo :long prim)
;;;                 ;;;(#_LineTo :long (car thepoints))
;;;                 (om-draw-line prim (car thepoints))
;;;             (setf prim (pop thepoints))))))

;=======Maquettes
;connections in maquettes are relatives
(defclass maq-connection (c-connection) ())

(defmethod get-graph-points ((self maq-connection))
  (let ((container (om-view-container (thebox self))))
    (denormalize-points container (points self))))

(defmethod get-graph-point-sel ((self maq-connection))
   (let ((container (om-view-container (thebox self))))
    (denormalize-points container (point-sel self))))

(defmethod scr-point-con ((self maq-connection) container point0 delta)
   (let ((x (pixel2norme container 'x (om-point-h  delta)))
         (y (* -1 (pixel2norme container 'y (om-point-v  delta)))))
     (om-add-points point0 (om-make-point x y))))


   
