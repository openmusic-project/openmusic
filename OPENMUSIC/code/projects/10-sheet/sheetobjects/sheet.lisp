(in-package :om)



;=====================================================================
;LA CLASSE SHEET
;=====================================================================

(defclass! OMSheet (OMBasicObject superposition) 
  ((voices :initform nil :accessor voices :initarg :voices :documentation "list of sheet tracks or musical objects")
   (patch-list :initform nil :accessor patch-list :initarg :patch-list :documentation "list of internal patches"))
  (:icon 127)
  (:documentation "The OM Sheet is a special doculent gathering score tracks (or <voices>) made of any kind of musical objects.
The more common classes of supported objects in the sheet are CHORD-SEQ, VOICE, BPF or SOUND.

The SHEET also contains a number of internal PATCHES (<patch-list>) allowing to define relationships between the objects in the different voices.

See the dedicated chapter in the OM User Manual for more details.")
  )

(defmethod voices ((self OMSheet))
  (inside self))

(defmethod initialize-instance ((self OMSheet) &rest initargs &key (Empty nil))
  (declare (ignore initargs)) 
  (call-next-method)
  (unless (name self) (setf (name self) "OMSheet"))
  (unless (icon self) (setf (icon self) 127))
  (unless (patch-list self) (add-patch-in-sheet self))
  (setf (inside self) ; (slot-value self 'inside) 
        (remove nil (loop for item in (list! (slot-value self 'voices)) append (list! (collect-sheet-voice item)))))
  (setf (slot-value self 'voices) nil)
  (init-objects-ids self)
  (loop for patch in (slot-value self 'patch-list) do
        (unless (equal 'sheet-patch (type-of patch))
          (change-class patch 'sheet-patch)))
  self)


(defmethod default-edition-params ((self omsheet))
  (append (default-edition-params self)
          (pairlis '(show-tracks show-time grille grille-step patch-open) 
                   '(t t nil 1000 nil))
          ))

(defmethod omNG-copy ((self OMSheet))
  `(copy-container ,self))


;=====================================================================
; SHEET TRACKS
;=====================================================================
(defclass* sheet-track (container named-score-object)
  ((track-size :initform 80 :accessor track-size)
   (track-pos :initform 1 :accessor track-pos)
   (objs :initform nil :accessor objs :initarg :objs :documentation "list of musical objects")
   )
  (:icon 461)
  (:documentation "The SHEET TRACK is an internal class used to store a set of time ordered objects in a sheet voice. It is usually generated automatically but can be created in OM visual program as well."))

(defmethod get-fonde-pict ((self sheet-track)) nil)

(defmethod track-p ((self sheet-track)) t)
(defmethod track-p ((self t)) nil)

(defmethod objs ((self sheet-track))
  (inside self))

(defmethod initialize-instance ((self sheet-track) &rest initargs)
  (declare (ignore initargs)) 
  (call-next-method)
  (setf (inside self) (slot-value self 'objs))
  (setf (slot-value self 'objs) nil)
  self)

(defmethod sheet-init ((self sheet-track)) t)

(defmethod omNG-save ((self sheet-track) &optional (values? nil))
  `(let ((sheet-track ,(call-next-method)))
     (setf (track-size sheet-track) ,(track-size self))
     (setf (track-pos sheet-track) ,(track-pos self))
     sheet-track))


;=====================================================================
; SHEET OBJECTS
;=====================================================================
(defclass* sheet-track-obj (container)
  ((obj :initform nil :accessor obj :initarg :obj :documentation "a musical object")
   (id :initform nil :accessor id :initarg :id :documentation "object ID (integer)")
   (start-t :initform 0 :accessor start-t :initarg :start-t :documentation "object onset (ms)")
   (end-t :initform 500 :accessor end-t)
   (obj-size :initform 20 :accessor obj-size)  
   (obj-margin :initform 1 :accessor obj-margin)
   (obj-staff :initform 'g :accessor obj-staff)
   (editorframe :accessor editorframe :initform nil)
   (frame :accessor frame :initform nil)
   )
  (:icon 460)
  (:documentation "The SHEET-TRACK-OBJ is an internal class used to store objects in the sheet tracks. It is usually generated automatically but can be created in OM visual program as well.

<obj> is the musical object object.
<id> is used to identify the object in the sheet (for algorithmic manipulations).
<start-t> is the absolute onset of the object in the sheet track."))

(defmethod get-fonde-pict ((self sheet-track-obj)) nil)

(defmethod obj ((self sheet-track-obj))
  (car (inside self)))

(defmethod get-obj-dur ((self sheet-track-obj))
  (if (obj self) (get-obj-dur (obj self)) 500))
  

(defmethod track-obj-p ((self sheet-track-obj)) t)
(defmethod track-obj-p ((self t)) nil)

(defmethod (setf value) ((value t) (self sheet-track-obj))
  (change-object self value))

(defmethod change-object ((self sheet-track-obj) (obj t))
  (setf (inside self) (list obj)))

(defmethod change-object ((self sheet-track-obj) (obj chord-seq))
  (let ((start (offset->ms (car (inside obj)))))
    (setf (lonset obj) (om- (lonset obj) start))
    (call-next-method)
    (setf (start-t self) (+ (start-t self) start))))

(defmethod change-object ((self sheet-track-obj) (obj multi-seq))
  (change-object self (objfromobjs obj (make-instance 'chord-seq))))

(defmethod change-object ((self sheet-track-obj) (obj sheet-track-obj))
  (change-object self (clone (obj obj))))


(defmethod sheet-init ((self sheet-track-obj))
  (setf (end-t self) (+ (start-t self) (if (obj self) (get-obj-dur (obj self)) 1000))))


(defmethod initialize-instance ((self sheet-track-obj) &rest initargs)
  (declare (ignore initargs)) 
  (call-next-method)
  (if (slot-value self 'obj)
      (setf (slot-value self 'end-t) 
            (+ (slot-value self 'start-t) (get-obj-dur (slot-value self 'obj)))))
  (when (and (slot-value self 'obj) (or (allowed-in-track (slot-value self 'obj)) (om-beep)))
    (setf (inside self) (list (slot-value self 'obj))))
  (setf (slot-value self 'obj) nil)
  self)

;;; for SAVE obj avec non-musical objects...
(defmethod (setf parent) (parent (self t)) nil)
(defmethod cons-extra-pairs ((self t)) nil)
(defmethod cons-mus-color ((self t)) nil)
(defmethod cons-patch-pairs ((self t)) nil)
(defmethod first-container ((self t) (type t) &key (reverse nil)) nil)

(defmethod omNG-save ((self sheet-track-obj) &optional (values? nil))
  `(let ((sheet-obj (make-instance 'sheet-track-obj :id ,(id self) :start-t ,(start-t self)
                                   :obj ,(if (maquette-p (obj self)) (om-save (obj self)) (omng-save (obj self))))))
     (setf (end-t sheet-obj) ,(end-t self))
     (setf (obj-size sheet-obj) ,(obj-size self))
     (setf (obj-margin sheet-obj) ,(obj-margin self))
     (setf (obj-staff sheet-obj) ',(obj-staff self))
     sheet-obj))

;=====================================================================
; SHEET CREATION AND METHODS
;=====================================================================

(defmethod allowed-in-sheet ((self t)) nil)
(defmethod allowed-in-sheet ((self poly)) t)
(defmethod allowed-in-sheet ((self voice)) t)
(defmethod allowed-in-sheet ((self chord-seq)) t)
(defmethod allowed-in-sheet ((self multi-seq)) t)
(defmethod allowed-in-sheet ((self chord)) nil)

(defmethod allowed-in-sheet ((self OMSheet)) nil)
(defmethod allowed-in-sheet ((self sound)) t)
(defmethod allowed-in-sheet ((self midifile)) nil)
(defmethod allowed-in-sheet ((self ommaquette)) t)

(defmethod allowed-in-sheet ((self bpf)) t)

(defmethod allowed-in-sheet ((self sheet-track-obj)) t)

(defmethod allowed-in-track ((self t)) (allowed-in-sheet self))
(defmethod allowed-in-track ((self multi-seq)) nil)
(defmethod allowed-in-track ((self poly)) nil)


(defmethod! make-sheet-obj ((self t) (time integer))
            :icon 450
  (when (allowed-in-track self)
    (make-instance 'sheet-track-obj
                   :obj (clone self)
                   :start-t time
                   )))

(defmethod! make-sheet-obj ((self chord-seq) (time integer))
    (let ((rep (call-next-method))
          (start (offset->ms (car (inside self)))))
      (setf (lonset (obj rep)) (om- (lonset (obj rep)) start))
      (setf (start-t rep) (+ (start-t rep) start))
      rep))

(defmethod! make-sheet-obj ((self sheet-track-obj) (time integer))
    (let ((rep (clone self)))
      (setf (start-t rep) time)
      rep))

(defmethod! make-sheet-obj ((self sheet-track) time)
            :icon 450
            (clone self))


(defmethod collect-sheet-voice ((self poly)) 
  (loop for v in (voices self) collect (collect-sheet-voice v)))

(defmethod collect-sheet-voice ((self multi-seq)) 
  (loop for v in (chord-seqs self) collect (collect-sheet-voice v)))

(defmethod collect-sheet-voice ((self sheet-track)) self)

(defmethod collect-sheet-voice ((self OMSheet)) 
  (loop for v in (voices self) collect (collect-sheet-voice v)))

(defmethod collect-sheet-voice ((self list))
  (let ((objs nil)
        (time 0))
    (loop for elt in self do
          (when (allowed-in-track elt)
            (pushr (make-sheet-obj elt time) objs)
            (setf time (+ time (get-obj-dur elt) 0))))
    (make-instance 'sheet-track :objs objs)))

(defmethod collect-sheet-voice ((self t))
  (when (allowed-in-sheet self)
    (make-instance 'sheet-track
      :objs (list (make-sheet-obj self 0))
      )))

(defmethod collect-sheet-voice ((self sheet-track-obj))
    (make-instance 'sheet-track
      :objs (list (clone self))))

(defmethod get-obj-dur ((self OMSheet))
  (or 
   (loop for track in (voices self) maximize 
         (get-obj-dur track))
   0))

(defmethod get-obj-dur ((self sheet-track))
  (or (loop for obj in (objs self) maximize (end-t obj)) 0))
  

(defmethod init-objects-ids ((self OMSheet))
  (let ((currid 0))
    (loop for tr in (voices self) do 
          (loop for ev in (objs tr) do 
                (setf (id ev) currid)
                (setf currid (1+ currid))))))

(defmethod get-objects-ids ((self OMSheet))
  (sort (loop for tr in (voices self) append 
        (loop for ev in (objs tr) collect (id ev))) '<))

(defmethod fill-objects-ids ((self OMSheet))
  (let ((currid (list-max (get-objects-ids self))))
    (loop for tr in (voices self) do 
          (loop for ev in (objs tr) when (null (id ev)) do 
               (setf currid (1+ currid))
               (setf (id ev) currid)
               ))))

(defmethod find-box-in-sheet ((self OMSheet) id)
  (let ((found nil))
    (loop for tr in (voices self) while (not found) do 
          (setf found (find id (objs tr) :key 'id :test '=)))
    found))

;=====================================================================
; SHEET OPERATIONS
;=====================================================================

;=======================================
; add track
(defmethod add-one-track ((self omsheet) &optional (pos 0) track) 
  (setf (inside self) (insert-in-list (inside self) (or track (make-instance 'sheet-track)) pos)))

;=======================================
; remove track
(defmethod remove-one-track ((self omsheet) pos)
  (setf (inside self) (append (subseq (inside self) 0 pos) (subseq (inside self) (+ pos 1)))))


;=======================================
; switch tracks
(defun switch-up (list obj)
  (let ((pos (position obj list :test 'equal)))
    (if (<= pos 0)
      list
      (insert-in-list (remove obj list :test 'equal) obj (- pos 1)))))

(defun switch-down (list obj)
  (let ((pos (position obj list :test 'equal)))
    (if (>= pos (- (length list) 1))
      list
      (insert-in-list (remove obj list :test 'equal) obj (+ pos 1)))))

(defun nth-switch-up (list n)
  (if (or (<= n 0) (>= n (length list)))
    list
    (append (subseq list 0 (- n 1)) (list (nth n list) (nth (- n 1) list)) (subseq list (+ n 1)))))

(defun nth-switch-down (list n)
  (if (or (< n 0) (>= n (- (length list) 1)))
    list
    (append (subseq list 0 n) (list (nth (+ n 1) list) (nth n list)) (subseq list (+ n 2)))))

(defmethod switch-track ((self omsheet) (track sheet-track) direction)
  (if (< direction 0)
      (setf (inside self) (switch-up (inside self) track))
    (setf (inside self) (switch-down (inside self) track))
    ))



;=======================================
; add object
(defmethod add-one-object ((sheet omsheet) (track sheet-track) object &optional (time-ms 0))
  (when (or (null object) (allowed-in-sheet  object))
    (let* ((time (max 0 time-ms))
           (ev (make-instance 'sheet-track-obj
                              :obj object
                              :start-t time
                              )))
      (when (null object) (setf (end-t ev) (+ time 1000)))
      (setf (id ev) (1+ (or (car (last (get-objects-ids sheet))) 0)))
      (setf (inside track) (sort (append (objs track) (list ev)) '< :key 'start-t))
      )))
  
(defmethod remove-one-object  ((self omsheet) (object sheet-track-obj)) 
  (let ((thetrack nil))
    (loop for tr in (voices self) while (not thetrack) do
          (when (find object (objs tr) :test 'equal)
            (setf thetrack tr)))
    (setf (inside thetrack) (remove object (objs thetrack) :test 'equal))))

;=====================================================================
; SHEET BOX
;=====================================================================

(defmethod Class-has-editor-p ((self OMSheet)) t)

(defmethod default-obj-box-size ((self OMSheet)) (om-make-point 40 60))

(defmethod spec-obj-icon-size ((self OMSheet)) '(32 32))

(defmethod draw-mini-view ((self t) (value OMSheet))
  (when (inside value)
    (let*  ((trp 0)
            (total-h (+ 1 (* 3 (length (inside value)))))
            (h-fact (/ (h self) total-h))
            (w-fact (/ (- (w self) 6) (max 1 (get-obj-dur value)))))
      (om-with-focused-view self
        (om-with-font (om-make-font *om-def-font-face* 8)
         (loop for tr in (inside value)
              for n = 0 then (+ n 1)
               do
               (om-with-fg-color self *om-light-gray-color*
                 (om-fill-rect 0 (+ h-fact (* 3 n h-fact)) 
                               (w self) 
                               (* 2 h-fact)
                               ))
               (om-with-fg-color self (om-make-color 0.75 0.7 0.7) 
                 (loop for obj in (objs tr) do
                       (let ((x (+ 3 (round (* (start-t obj) w-fact))))
                             (y (+ (+ h-fact (* 3 n h-fact)) 3))
                             (w (* (- (end-t obj) (start-t obj)) w-fact))
                             (h (- (* 2 h-fact) 6)))
                         (om-fill-rect x y w h)
                         (om-with-fg-color self *om-gray-color* 
                           (om-draw-string (+ 6 (round (* (start-t obj) w-fact)))
                                           (- (* h-fact (+ 3 (* 3 n))) 6)
                                           (string+ (number-to-string (id obj))
                                                    " - "
                                                    (string (type-of (obj obj)))
                                                    )
                                           ))
                         )))
               ))))))


(defmethod update-miniview ((self t) (type OMSheet))  (om-invalidate-view self t))

(defmethod spec-obj-icon-size ((self OMSheet)) '(32 32))

(defmethod get-editor-class ((self OMSheet)) 'sheetEditor)


;=====================================================================
; PLAY
;=====================================================================

(defmethod player-schedule ((player omplayer) (obj omsheet) engine &key (at 0) interval params)
  (loop for tr in (inside obj) do
        (loop for tr-obj in (objs tr) do
              (player-schedule player (obj tr-obj)
                      (car (players-for-object (obj tr-obj)))
                      :at (+ at (start-t tr-obj))
                      :interval interval)
              )))
                      





