(in-package :om)



;================================
;Global Vars
;================================

(defvar *recording-midi-p* nil) ;t is recording
(defvar *midi-tempo* 1000000)   ; noire = 60 en microsecondes
(defvar *MidiShare-start-time*  0)

(defvar *recording-midi-seq* nil)
(defvar *playing-midi-seq* nil)

(defvar *play-chseq-w/offset* nil)



;==================================
;=== Time and tempo conversions ===
;==================================
(defmacro convert-time (time unit/sec)
  `(* (/  *midi-tempo* 1000000) (/ ,time ,unit/sec) 1000))

(defmacro convert-time-1 (time unit/sec)
  `(* ,time  (/ ,unit/sec 1000 )))



(defun logical-time (abstract-time cur-tempo tempo-change-abst-time tempo-change-log-time unit/sec)
  (+ tempo-change-log-time
     (round (* (/ 1000.0 unit/sec) 
               (* (- abstract-time tempo-change-abst-time)
                  (/ cur-tempo *midi-tempo*))))))

(defun mstempo2bpm (mstempo)
   (round (* (/ *midi-tempo* mstempo) 60)))

(defun bpm2mstempo (bpm)
  (round (* (/ 60 bpm) *midi-tempo*)))

;=== streches dates of an object which Qtempo = qtpo
(defun strechDate (date qtpo)
  (round (* date (/ 60 qtpo))))



;by JB
;=====================================================
;=== Converts midi events time info to miliseconds    ===
;=== (Keeping tempo events)                           ===
;=====================================================
(defun convert-tempo-info (seq units/sec)
  (when *midiplayer*
    (let ((cur-tempo *midi-tempo*)
          (tempo-change-abst-time 0)
          (tempo-change-log-time 0) event date initdate)
      (setf event (om-midi-seq-first-evt seq))
      (setf initdate (om-midi-evt-get event :date))
      (loop while event for i = 1 then (+ i 1) do
            (let ()
              (setf date (- (om-midi-evt-get event :date) initdate))
              (when (= 144 (om-midi-evt-get event :type))
                (setf tempo-change-log-time (logical-time date cur-tempo tempo-change-abst-time 
                                                          tempo-change-log-time units/sec))
                (setf cur-tempo (om-midi-evt-get event :tempo))
                (setf tempo-change-abst-time date)
                )
              (case (om-midi-evt-get event :type)
                (0  (om-midi-evt-set event :dur (logical-time (om-midi-evt-get event :dur)  
                                                                 cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))
                    (om-midi-evt-set event :date (logical-time (om-midi-evt-get event :date)  
                                                               cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)))
                (otherwise (om-midi-evt-set event :date (logical-time (om-midi-evt-get event :date)  
                                                                      cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))))
              (setf event (om-midi-next-evt event))
              ))
      seq)))


;;;=====================================
;;; Remove tempo events from a midi file 
;;; and converts all dates to the same tempo
;;;=====================================
(defun delete-tempo-info (seq units/sec)
  (when *midiplayer*
    (let ((recording-seq (om-midi-new-seq))
          (cur-tempo *midi-tempo*)
          (tempo-change-abst-time 0)
          (tempo-change-log-time 0) event date initdate)
      (setf event (om-midi-seq-first-evt seq))
      
      (setf initdate (om-midi-evt-get event :date))
      (loop while event do
            (let (newevent)
              (setf date (-  (om-midi-evt-get event :date) initdate))
              (if (= 144  (om-midi-evt-get event :type))
                (setf 
                 tempo-change-log-time (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                 cur-tempo (om-midi-evt-get event :tempo)
                 tempo-change-abst-time date)
                (progn
                  (setf newevent (om-midi-copy-evt event))
                  (case (om-midi-evt-get event :type)
                    (0  (om-midi-evt-set newevent :dur (logical-time (om-midi-evt-get event :dur)   
                                                               cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))
                        (om-midi-evt-set newevent :date (logical-time (om-midi-evt-get event :date)   
                                                              cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)))
                    (otherwise (om-midi-evt-set  newevent :date (logical-time (om-midi-evt-get event :date)  
                                                                        cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))))
                  (om-midi-seq-add-evt recording-seq newevent)))
              (setf event (om-midi-next-evt event))
              ))
      (om-midi-free-seq seq)
      recording-seq)))


;=== KEY FUNCTION : to upgrade when voice accept tempo map... 
;=== Converts a sequence in tempo 60 into other tempo
(defun insert-tempo-info (seq tempo) 
  (let ((mySeq (om-midi-new-seq)) event newevent tempoFactor)
    (setf tempoFactor (/ (bpm2mstempo tempo) *midi-tempo*))
    (setf newevent (om-midi-new-evt (om-midi-get-num-from-type "Tempo") :date 0 :ref 0 :vals (bpm2mstempo tempo)))
    (om-midi-seq-add-evt mySeq newevent)
    (setf event (om-midi-seq-first-evt seq))
    (loop while event do
          (setf newevent (om-midi-copy-evt event))
          (om-midi-evt-set newevent :date (round (/ (om-midi-evt-get event :date) tempoFactor)))
          (if (= (om-midi-evt-get event :type) (om-midi-get-num-from-type "Note")) 
              (om-midi-evt-set newevent :dur (round (/ (om-midi-evt-get event :dur) tempoFactor))))
          ;(print (midishare::date event))(print (midishare::date newevent))
          (om-midi-seq-add-evt mySeq newevent)
          (setf event (om-midi-next-evt event))
          )
    mySeq))   

;;;====================================================================

(defun close-notes-on (list pitch chan date track) 
  (flet ((match (x) (and (equal (first x) pitch) (equal (fifth x) chan) (not (plusp (third x))) (equal (sixth x) track))))
    (let ((pos (position-if #'match list :from-end t)))
      (if pos
        (setf (nth 2 (nth pos list))  (- date (* -1 (nth 2 (nth pos list)))))
        (om-print (format nil "Warning: this MIDI sequence has unterminated notes in track ~D / channel ~D: NoteOn ~D (t=~Dms)." track chan pitch date))
        ))))

;(midic date dur vel chan track)

(defun mievents2midilist (seq &optional port)
   (when *midiplayer*
     (let (event info rep)
       (setf event (om-midi-seq-first-evt seq))
       (loop while event do
             (when (or (not port) (= port (om-midi-evt-get event :port)))
               (case (om-midi-evt-get event :type)
                 (0  (push (list (om-midi-evt-get event :pitch)
                                 (om-midi-evt-get event :date)
                                 (om-midi-evt-get event :dur) 
                                 (om-midi-evt-get event :vel)
                                 (1+ (om-midi-evt-get event :chan))
                                 (om-midi-evt-get event :ref)) rep))
                 (1 (if  (= (om-midi-evt-get event :vel) 0)
                      (close-notes-on rep
                                      (om-midi-evt-get event :pitch) 
                                      (1+ (om-midi-evt-get event :chan))
                                      (om-midi-evt-get event :date)
                                      (om-midi-evt-get event :ref))
                      (push (list  (om-midi-evt-get event :pitch) 
                                   (om-midi-evt-get event :date)
                                   (* -1 (om-midi-evt-get event :date))
                                   (om-midi-evt-get event :vel) 
                                   (1+ (om-midi-evt-get event :chan))
                                   (om-midi-evt-get event :ref)) rep)))
                 (2 (close-notes-on rep
                                    (om-midi-evt-get event :pitch) 
                                    (1+ (om-midi-evt-get event :chan))
                                    (om-midi-evt-get event :date)
                                    (om-midi-evt-get event :ref)))))
             (setf event (om-midi-next-evt event)))
       (reverse rep))))


;================================
;PLAY
;================================

(defun verify-port (port) 
   (or port *Outmidiport*))

(defclass arp-chord ()
  ((notes :initform nil :initarg :notes :accessor notes)))

(defmethod extent ((self arp-chord))
   (* (length (notes self)) 500))

(defmethod get-obj-dur ((self arp-chord)) (extent self))


;================================
;================================



