(in-package :oa)

(multiple-value-bind (err sequence nbtracks clicks format timedef)
    (om-midi-load-file (pathname "/home/andersvi/test.midi") (om-midi-new-seq))
  (om::get-midievents (first (midi-seq-events sequence))))

(setf om::*midiplayer* t)
(setf a (om::load-midi-file "/home/andersvi/test.midi"))


(make-instance 'midi::tempo-message)
(make-instance 'om::midievent)

(make-instance 'cl-midievent :type (om-midi-get-num-from-type "Tempo") :event-fields 100)

(om-midi-evt-get (setf tm (make-instance 'om::midievent :ev-type (om-midi-get-num-from-type "Tempo"))) :type)

(let ((a (make-instance 'midi::note-on-message :time 0 :key 60 :velocity 80))
      (b (make-instance 'midi::note-off-message :time 123 :key 60)))
  (setf (slot-value a 'midi::channel) 8
	(slot-value b 'midi::channel) 8)
  (make-event-from-message a)
  (make-event-from-message b))

(let ((a (make-instance 'midi::tempo-message :time 0 :tempo 1098)))
  (let ((b (make-event-from-message a)))
    (setf (ev-link b) 10)
    b))

