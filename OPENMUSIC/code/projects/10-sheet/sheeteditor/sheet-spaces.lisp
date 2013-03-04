;;;; DRAW OBJECTS IN SHEET

(in-package :om)

(defclas grap-track (grap-container) ())

(defclas grap-track-ev (simple-graph-container) 
   ((index :initform nil)
    (start? :initform nil)
    (segmentations :initform nil)
    (internal-rects :initform nil)))  ;a tenir en compte


(defmethod get-x-pos ((self sheet-scorepanel) (time-ms integer) zoom)
  (let ((rep (if (timebpf self)
                 (let ((max (car (last (x-points (timebpf self)))))
                       (min (car (x-points (timebpf self)))))
                   (cond
                    ((> time-ms max) (round (+ (bpf-get-val (timebpf self) max) (round (- time-ms max) (staff-size self)))))
                    ((< time-ms min) (round (- (bpf-get-val (timebpf self) min) (round (- min time-ms) (staff-size self)))))
                    (t (round (bpf-get-val (timebpf self) time-ms)))))
               (round time-ms (staff-size self)))))
    (* rep (or zoom 1.0))))

(defmethod get-ms-pos  ((self sheet-scorepanel) (x-pos integer) zoom)
  (if (timebpf self)
      (let ((max (car (last (y-points (timebpf self)))))
            (min (car (y-points (timebpf self)))))
        (cond
         ((> x-pos max) (round (+ (car (y-transfer (timebpf self) max 0)) (* (- x-pos max) (staff-size self)))))
         ((< x-pos min) (max 0 (round (- (car (y-transfer (timebpf self) min 0)) (* (- min x-pos) (staff-size self))))))
         (t (round (car (y-transfer (timebpf self) x-pos 0))))))
    (round (* x-pos (staff-size self)))))




(defmethod collect-temporal-objects ((self grap-chord) (father sheet-track-obj))
   (list (list (+ (start-t father) (offset->ms (reference self) father)) self)))

(defmethod collect-temporal-objects ((self grap-rest) (father sheet-track-obj)) 
   (list (list (+ (start-t father) (offset->ms (reference self) father)) self)))

(defmethod collect-temporal-objects ((self grap-measure) (father sheet-track-obj)) 
   (cons (list (+ (start-t father) (offset->ms (reference self) father)) self)
         (loop for item in (inside self)
               append (collect-temporal-objects item father))))



(defmethod collect-bpftime-objects ((self grap-track-ev) father size)
  (let ((sheettrackobj father)) ;; (reference father)
    (list (list (if (start? self)
                    (start-t sheettrackobj)
                  (end-t sheettrackobj))
                (car (main-point self))))))

(defmethod collect-bpftime-objects ((self grap-rest) (father sheet-track-obj) size) 
   (list (list (+ (start-t father)
                  (offset->ms (reference self) father))
               (car (main-point self)))))

(defmethod collect-bpftime-objects ((self grap-ryth-chord) (father sheet-track-obj) size)
  (list (list (+ (start-t father)
                 (offset->ms (reference self) father))
              (car (main-point self)))))

(defmethod collect-bpftime-objects ((self grap-chord) (father sheet-track-obj) size)
   (list (list (+ (start-t father)
                  (offset->ms (reference self) father))
              (car (main-point self)))))



;;;========================
;; GRAPHIC OBJS
;;;========================

(defmethod make-grap-obj ((self sheet-scoreobjectframe) &optional (updateref nil))
  ;(set-editor-tonality self)
  (let ((linespace (/ (staff-size self) 4)))
    (setf (graphic-obj self) (make-graph-ryth-obj (obj (reference self)) (top-in-midi (staff-sys self)) (staff-sys self) linespace 
                                                  (get-current-scale (staff-tone self))
                                                  (selection? self) 
                                                  (reference self) ; nil 
                                                  (get-obj-dur (obj (reference self)))
                                                  nil
                                                  ))))

(defmethod set-graph-rectangles ((self grap-track-ev) )
   (setf (rectangle self) (list (car (main-point self)) (second (main-point self)) 
                                (+ (car (main-point self)) (third (rectangle self))) 
                                (+ (second (main-point self)) (fourth (rectangle self))))) )


;il faut le faire pour le chord-seq
(defmethod make-graph-ryth-obj ((self chord-seq) top staffsys linespace scale sel pere durtot &optional ryth)
  (declare (ignore ryth))
  (let* ((newc-s (make-instance 'grap-chord-seq
                   :reference self
                   :parent pere))
         (inside (loop for item in (inside self) 
                       for i = 0 then (+ i 1)
                       collect 
                       (make-graph-ryth-prop-obj item  top staffsys linespace  scale sel newc-s))))
    (setf (inside newc-s) inside)
    (set-parent newc-s inside)
    (make-graphic-extras newc-s)
    newc-s))


(defmethod make-graph-ryth-prop-obj ((self chord)  top staffsys linespace scale sel pere)
  (let* ((thenotes (sort (copy-list (inside self)) '< :key 'midic))
         (offsets (Loffset self))
         (zigzag-list (make-alt-zig-zag self scale))
         (note-head-list (make-chord-zig-zag self scale))
         (maxw 0)
         (grap-notes (loop for item in thenotes
                           for pos in note-head-list
                           for i = 0 then (+ i 1)
                           collect
                           (let (notegrap notew)
                             (setf notegrap (make-graph-ryth-prop-obj item  top staffsys linespace scale sel self))
                             (setf (delta-head notegrap) pos)
                             (when (natural-alt-char notegrap)
                               (setf (alteration notegrap) (correct-alteration notegrap (pop zigzag-list))))
                             (setf notew (round (* 2 (max (* linespace pos)
                                                          (* linespace (if (alteration notegrap) (* -1 (- (alteration notegrap) 1)) 0))))))
                             (setf (rectangle notegrap) (list 0 0 notew (round linespace)))
                             (setf (nth 0 (main-point notegrap)) (round (* linespace pos)))
                             (setf maxw (max maxw notew))
                             notegrap)))
         (newchord (make-instance 'grap-chord
                     :reference self
                     :parent pere
                     :rectangle (list 0 0 maxw 0)
                     :stem (round (* 3 linespace))
                     :selected (member self sel :test 'equal)
                     :inside grap-notes)) )
    (set-parent newchord grap-notes)
    (make-graphic-extras newchord)
    newchord))

(defmethod make-graph-ryth-prop-obj ((self note) top staffsys linespace scale sel  pere)
   (declare (ignore ryth))
   (let* ((ypos (get-graphic-pos self top linespace scale)) 
          (alt-char (get-alt-char self scale (armure staffsys)))
          alteration thenote)
     (when alt-char (setf alteration (get-alteration-n alt-char)))
     (setf thenote
           (make-instance 'grap-note
             :reference self
             :alt-char alt-char
             :alteration alteration
             :parent pere
             :main-point (list 0 (- ypos (round linespace 2)))
             :selected (member self sel :test 'equal)
             :auxlines (get-aux-lines self staffsys top scale linespace ypos)))
     (make-graphic-extras thenote)
     thenote))



;;;==========

(defmethod make-grap-obj ((self sheet-linobjectframe) &optional (updateref nil)) 
  (setf (graphic-obj self) (make-graph-linear-obj self)))

(defmethod make-graph-linear-obj ((self sheet-linobjectframe))
  (declare (ignore ryth))
  (let* ((newtrack (make-instance 'grap-track
                                  :reference self
                                  :rectangle (list 0 0 0 0)))
         (inside (make-graph-linear-bounds self)))
    (setf (inside newtrack) inside)
    newtrack))

(defmethod make-graph-linear-bounds ((self sheet-linobjectframe))
  (let* ((new-ev-s (make-instance 'grap-track-ev
                     :reference self
                     :main-point (list 0 0)
                     :start? t
                     :rectangle (list 0 0 1 1)))
         (new-ev-e (make-instance 'grap-track-ev
                     :reference self
                     :start? nil
                     :main-point (list 0 0)
                     :rectangle (list 0 0 1 1))))
    (list new-ev-s new-ev-e)))

(defmethod collect-temporal-objects ((self grap-track-ev) father)
   (list (list (if (start? self)
                   (start-t (reference father))
                 (end-t (reference father)))
               self)))

(defmethod collect-temporal-objects ((self grap-track-ev) father)
  (let ((sheettrackobj father)) ;; (reference father)
    (list (list (if (start? self)
                    (start-t sheettrackobj)
                  (end-t sheettrackobj))
                self))))







;;;=========== SPACE GIRLS ===========

;IL FAUT REVISER BIEN
;===============chord is i
;------last element
(defmethod space-girl ((obji+1 null) onseti+1 (obji grap-chord)  onseti  count size maxdur)
  (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
  (loop for item in (inside obji) do
        (setf (main-point item) (list (+ count (first (main-point item))  maxdur) (second (main-point item)))))
  (+ count  size ))

;------before a measure
(defmethod space-girl ((obji+1 grap-measure) (onseti+1 integer) (obji grap-chord) (onseti integer) count size maxdur) 
  (let ((maxdur maxdur))
    (setf (main-point obji) (list (+ count maxdur) (second (main-point obji))))
    (loop for item in (inside obji) do
          (setf (main-point item) (list (+ count (first (main-point item)) maxdur) (second (main-point item)))))
    (if (zerop (- onseti+1 onseti))
        (+ count (round size 2))
      (+ count (* maxdur 2) (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4)))))
  

;(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-measure) (onseti integer) count size maxdur) 
;   (setf (main-point obji) (list count (second (main-point obji))))
;   (+ count (round size 4) (get-chiffrage-space obji size )))



;------before a chord or a rest
(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-chord) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (loop for item in (inside obji) do
         (setf (main-point item) (list (+ count (first (main-point item)) maxdur ) (second (main-point item)))))
   (if (zerop (- onseti+1 onseti))
     count
     (+ count (* maxdur 2) (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))



;------last element
(defmethod space-girl ((obji+1 null) onseti+1 (obji grap-track-ev)  onseti  count size maxdur)
  (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
  (+ count  size ))

;------before a measure
(defmethod space-girl ((obji+1 grap-measure) (onseti+1 integer) (obji grap-track-ev) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (if (zerop (- onseti+1 onseti))
     (+ count  (round size 2)) 
     (+ count (* maxdur 2)  (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))

;------before a chord or a rest
(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-track-ev) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (if (zerop (- onseti+1 onseti))
     count
     (+ count (* maxdur 2) (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))


;;; NOT USED ?? =======================
(defclas grap-start-event (simple-graph-container) ())
(defclas grap-end-event (simple-graph-container) ())
(defmethod space-size ((self grap-start-event) ) 24)
(defmethod space-size ((self grap-end-event) ) 0)

(defmethod measures-first ((a grap-measure) (b grap-start-event))  nil)
(defmethod measures-first ((a grap-start-event) b)  t)
;;;(defmethod measures-first ((a grap-ryth-chord) (b grap-chord))  t)

(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-end-event) (onseti integer) count size maxdur) 
  (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
  (if (zerop (- onseti+1 onseti))
    count
    (+ count maxdur   )))

(defmethod space-girl ((obji+1 grap-end-event) (onseti+1 integer) (obji t) (onseti integer) count size maxdur)
  (setf (main-point obji) (list (+ count  (*  size 3) ) (second (main-point obji))))
  (+ count  (*  size 3) ))

(defmethod space-girl ((obji+1 null) onseti+1 (obji grap-end-event)  onseti  count size maxdur)
  (+ count  (*  size 3) ))




;;;=============================================

(defmethod sheet-key-space ((self scorePanel))
   (* (staff-size self) 3))

(defmethod space-size ((self grap-chord))
  (loop for item in (inside self) maximize (third (rectangle item))))

(defmethod get-sheet-temporal-objects (temp-list)
  (let ((maxdur 0)
        serie? rep)
    (loop for item in temp-list do
          ;(print (list item serie? maxdur))
          (if serie?
            (if (= (first item) (first (car serie?)))
              (progn
                (push item serie?)
                (setf maxdur (max maxdur (space-size (second item)))))
              (progn
                (setf rep (append rep (put-maxdur (sort serie? 'measures-first :key 'second) maxdur)))
                (setf serie? (list item))
                (setf maxdur (space-size (second item)))))
            (progn 
              (setf serie? (list item))
              (setf maxdur (max maxdur (space-size (second item)))))))
    (setf rep (append rep (put-maxdur (sort serie? 'measures-first :key 'second) maxdur)))
    rep))


(defun space-object-list (list size)
   (let ((count 0)
         (maxsize 0))
     (loop for i from 0 to (- (length list) 1) do
           (let ((obji (nth i list))
                 (obji+1 (nth (+ i 1) list)))
             (setf count (space-girl (second obji+1) (first obji+1)  
                                     (second obji) (first obji) count size (round (third obji) 2)))
             ;(print (list obji obji+1 count))
             ))))

(defun make-dummybox (sheet)
  (let ((obj (make-instance 'sheet-track-obj))
        (voice (make-instance 'voice :tree '(1 (((1 4) (1)))))))
    (setf (parent voice) obj)
    (setf (parent obj) sheet)
    (list (make-graph-ryth-obj voice
                               0 (get-staff-system 'g) 4 *current-1/2-scale* nil nil 1000 nil)
          0 1000
          obj)))

(defmethod space-sheet-tracks ((self sheet-scorePanel)) 
  (let* ((dummy-box (make-dummybox (object (sheet-editor self)))) 
         (gobjs (cons dummy-box
                       (loop for event in (get-sheet-objframes self)
                             collect (list (graphic-obj event) (start-t (reference event)) (end-t (reference event)) (reference event)))))
          (timedobjlist (sort 
                 ;;; ((t obj) ...)
                 (loop for item in gobjs append 
                       (collect-temporal-objects (car item) (nth 3 item)))
                 '< :key 'car)))
         ;;; space things
    (space-object-list
           (get-sheet-temporal-objects timedobjlist)
     (staff-size self))
    
    (setf (timebpf self) (make-sheet-bpf-time self (append '((0 0)) 
                                                            (loop for item in gobjs append 
                                                                 (collect-bpftime-objects (car item) (nth 3 item) ;; (reference (car item)) 
                                                                                          (staff-size self))))))
    ;;; (setf *bpftime* (timebpf self))

    (loop for item in gobjs do
          (set-graph-rectangles (car item)))
    (timebpf self)
    ))


;;; (defun bpftime () *bpftime*)

(defmethod make-sheet-bpf-time ((self sheet-scorePanel) list)
  (let (lx ly)
    (setf list (sort (remove-duplicates list :key 'first) '< :key 'first))
    (loop for item in list do
          (push (first item) lx)
          (push (second item) ly))
    (simple-bpf-from-list (reverse (cons (+ 1000 (car lx)) lx)) 
                          (om+ (sheet-key-space self) 
                               (reverse (cons (+ 20 (car ly)) ly))))))

