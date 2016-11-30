(in-package :om)

(defun page-size (self) (om-make-point 800 600))
(defvar *strech-tol* 90)

(defvar *max-score-length* 100000000000000)

(defmethod params-for-draw-all ((self multiseqPanel)) (list nil (edition-values self)))
(defmethod params-for-draw-all ((self polyPanel)) (list nil (edition-values self)))
(defmethod params-for-draw-all ((self scorePanel)) nil)


;;;============================
;;; a partir de la on s'en set plus...
;;;============================

(defmethod draw-all-score ((self scorePanel))
   (let* ((size (staff-size self))
          (view (om-make-window 'om-window 
                  :window-show nil
                  :window-title ""
                  :font (om-make-music-font *signs-font* size)
                  :size (om-view-size self)))
          (deltax (get-key-space self))
          (linparam (params-for-draw-all self))
          (deltay (round (* size (fifth (def-prefer self))))))
     (when (graphic-obj self)
       (om-with-focused-view view
         (draw-object  (graphic-obj self) self deltax deltay 
                       (staff-zoom self) 0 *max-score-length* 0 *max-score-length* 
                       (slots-mode self) size linparam (staff-sys self) (grille-step-p self) (noteaschan? self))))
     (om-close-window view)))

(defmethod edit-preferences ((self chordseqPanel))
   (setf (linear? self) (not (linear? self)))
   (unless (linear? self)
     (draw-all-score self)
     (cons-pages-list self (cons-score-pages self) (round (* (staff-size self) (fifth (def-prefer self))))
                      (def-prefer self)
                      (staff-size self) (staff-sys self) (- (om-point-v (page-size self)) (staff-size self))
                      (get-key-space self)))
   (update-panel self))

(defmethod cons-score-pages ((self chordseqPanel))
   (let ((break-points (get-break-line-points (graphic-obj self))))
     break-points))


(defmethod cons-pages-list ((self chordseqPanel) list y linear?  size staff pageh x)
   (let* ((widt (round (- (* (second linear?) size) x)))
          (staff-sizeh (system-size-in-pix staff size))
          (interline (round (* size (third linear?))))
          (h-pointer y)
          (num-line 0)
          (hline-rect (list (+ y (* 2 size) interline) (+ y interline (* 2 size) staff-sizeh)))
          line page rep)
     (loop for item in list
           for i = 0 then (+ i 1) do
                      (when (< i 10) item)

           (if (<= (- (- (third item) x) (* num-line widt)) widt)
             (progn (push i line)
                    (setf hline-rect (list (min (first  hline-rect) (second item) )
                                           (max (second  hline-rect) (fourth item)))))
             (progn
               (cond
                ((< (+ h-pointer (- (second hline-rect) (first hline-rect)) interline) pageh)
                 (push (append (list h-pointer num-line)  (reverse  line)) page)
                 (setf h-pointer (+ h-pointer (- (second hline-rect) (first hline-rect)) interline))
                 (setf line (list i))
                 (setf hline-rect (list (+ y interline) (+ y interline staff-sizeh)))
                 (setf hline-rect (list (min (first  hline-rect) (second item))
                                        (max (second  hline-rect) (fourth item)))))
                (t
                 (push (reverse page) rep)
                 (setf page nil)
                 (setf h-pointer y)
                 (push (append (list h-pointer num-line)  (reverse  line)) page)
                 (setf h-pointer (+ h-pointer (- (second hline-rect) (first hline-rect)) interline))
                 (setf line (list i))
                 (setf hline-rect (list (+ y interline) (+ y interline staff-sizeh)))
                 (setf hline-rect (list (min (first  hline-rect) (second item))
                                        (max (second  hline-rect) (fourth item))))))
               (setf num-line (+ num-line 1)))))
     (push (append (list h-pointer num-line)  (reverse  line)) page)
     (push (reverse page) rep)
     (setf (pagination self) (reverse rep))))

(defmethod get-objs-to-draw-in-pg-mode ((self grap-chord-seq)) (inside self))
(defmethod get-obj-to-draw-in-pg-mode ((self grap-chord-seq) list i)  (nth i list))

; linea = position tamano indices
(defmethod draw-in-page-mode ((self grap-chord-seq) pagination pagesizeh firstpage lastpage 
                                x y zoom minx maxx miny maxy slot size params staff grille-p chnote)
   (let* ((leftmarg (round (* (first params) size)))
          (objs2draw (get-objs-to-draw-in-pg-mode self))
          (widt (round (* (second params) size))))
     (loop for page from firstpage to lastpage 
           for p = (* pagesizeh firstpage) then (+ p pagesizeh) do
           (let ((thepage (nth page pagination)))
             (loop for line in thepage do
                   (let ((posy-line (+ p (first line)))
                         (ind-line (cddr line))
                         (num-line (second line)))
                     (draw-system  staff leftmarg widt size posy-line nil nil nil)
                     (loop for obj in ind-line do
                           (let* ((theobj (get-obj-to-draw-in-pg-mode self objs2draw obj))
                                  (realdeltax (if (zerop num-line) 0 (* (- widt x)  num-line))))
                             (draw-object theobj (-  (+ x leftmarg)  realdeltax)
                                          posy-line
                                          zoom minx maxx miny maxy slot size params staff grille-p chnote)))))))))


(defmethod get-break-line-points ((self grap-chord-seq))
   (loop for item in (inside self)
         collect (rectangle item)))

(defun intersec-interval (l1 l2)
  (if (or (and (< (first l1) (second l2)) (> (first l1) (first l2)))
          (and (< (first l2) (second l1)) (> (first l2) (first l1) )))
    (list (max (first l1) (first l2)) (min (second l1) (second l2))))) 



     
     