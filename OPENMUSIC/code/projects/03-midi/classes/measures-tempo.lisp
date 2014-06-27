;;; MIDI TEMPO AND BARS

(in-package :om)


;===================
;==== TEMPO-MAP ====
;===================
;=== A Tempo-Map is an object containing a tempo changes and time signatures (also used as bar markers)
;=== of an object (MidiFile, EventMidi-seq, Voice, ...)
;=== It can be extracted from an object this method get-midievents is defined for this object
(defclass* Tempo-map (sequence*) 
   ((tempo-Evts :initform nil :accessor tempo-Evts :initarg :tempo-Evts :type t :documentation "tempo changes")
    (timeSign-Evts :initform nil :accessor timeSign-Evts :initarg :timeSign-Evts :type t :documentation "measure changes"))
    (:icon 911)
    (:documentation "
A TEMPO-MAP represents the tempo events and measure changes (also used as bar markers) in a MIDI sequence.

<tempo-evts> is a list of ((t1 tempo1) (t2 tempo2) ...)
<timesign-evts> is a list of ((t1 (measure-info1)) (t2 (measure-info2)) ...)

"
     ))

(defmethod update-miniview ((self t) (value Tempo-map)) 
  (om-invalidate-view self t))

;=== Creates a list of MidiEvents 
(defmethod! get-midievents ((self Tempo-Map) &optional test)
  :icon 902
  (let ((evtList nil) evt fields)
  (loop for tempoitem in (tempo-Evts self) do
        (setf event (make-instance 'MidiEvent
          :ev-date (first tempoitem)
          :ev-type :Tempo
          :ev-ref 0
          :ev-fields (second tempoItem)))
        (if (or (not test) (funcall test event))
          (push event evtList)))
  (loop for timesignitem in (timeSign-Evts self) do
        (setf event (make-instance 'MidiEvent
          :ev-date (first timesignitem)
          :ev-type :TimeSign
          :ev-ref 0
          :ev-fields (second timesignItem)))
        (if (or (not test) (funcall test event))
          (push event evtList)))
  (reverse evtList)))


;=== Extract tempo-map from a simple-container
;=== get-midievent method must be defined for this container
(defmethod! get-TempoMap ((self simple-container))
  :initvals '(nil) 
  :indoc '("a musical object or MIDI sequence") 
  :icon 905
  :doc "Extracts and generates a TEMPO-MAP object from <self>."
  (let ((tempoEvents nil)
        (tempoMap (make-instance 'tempo-Map))
        (tempoList nil) (timeSignList nil))
    (setf tempoEvents (get-midievents self #'(lambda (x) (or (test-type x 'tempo) (test-type x 'timeSign)))))
    (loop for event in tempoEvents do
          (cond
           ((equal (ev-type event) :Tempo)
            (push (list (ev-date event) (first (ev-fields event))) tempoList))
           ((equal (ev-type event) :TimeSign)
            (push (list (ev-date event) (ev-fields event)) timeSignList))
           (t nil)))
    
    (setf (tempo-Evts tempoMap) (reverse tempoList))
    (setf (timeSign-Evts tempoMap) (reverse timesignList))
    tempoMap))


(defmethod* objFromObjs ((self simple-container) (type Tempo-Map))
  (get-tempoMap self))


(defmethod draw-mini-view  ((self t) (value Tempo-Map)) 
   (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self Tempo-Map) x x1 y y1 edparams view)
   (om-with-focused-view view
     (loop for tpair in (tempo-evts self)
           for i = 1 then (+ i 1) do
           (if (listp tpair) 
               (om-draw-string 10 (* 12 i) (format nil "~D: ~D" (car tpair) (cadr tpair)))
             (om-draw-string 10 (* 12 i) (format nil "Ill-formed time-value: ~D" tpair))
           ))))

;=== If a tempo-map can be extracted from an object,
;=== we are able to find time of begining of each measure
;=== (can be useful to test midievents with measure number instead of time)
(defmethod! mesure-time ((self simple-container) num)
  :indoc '("a score object" "measure number")
  :initvals '(nil 0)
  :doc "
Returns time (ms) of measure <num> in <self>
"
  :icon 919
  (let ((mesureList (timeSign-Evts (get-tempomap self)))
        mesureList2 
        (last (- 1)))
    (loop for mesure in mesureList do
          (if (not (= last (first mesure))) (push mesure mesureList2))
          (setf last (first mesure))
          )
    (first (nth num (reverse mesureList2)))
))
