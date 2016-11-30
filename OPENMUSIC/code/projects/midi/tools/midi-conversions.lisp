;===================================================
;=== CONVERSION BETWEEN MIDI AND MUSICAL OBJECTS ===
;===================================================


(in-package :om)


;=========================================
; GENERAL FUNCTION FOR MIDI CONVERSION -> get-midievents 
;=========================================

;=== Calculates maximum division of a measure
; bug quand on a des fractions etc...
; om-round pour le cas des liasions 1.0
(defmethod max-div ((self measure))
  (denominator (* 4 (list-min (tree2ratio (list '? (om-round (list (tree self)))))))))

(defmethod! get-midievents ((self Note) &optional test)
   :icon 902
   (when (not (memq (tie self) '(continue end)))
       (let ((events (NoteToEvents self)))
         (if test
             (loop for e in events when (funcall test event) collect e)
           events))))

(defmethod! get-midievents ((self Rest) &optional test) nil)

(defmethod! get-midievents ((self score-element) &optional test)
  :initvals '(nil nil) 
  :indoc '("score element object" "lambda test function for midi events to extract") 
  :icon 902
  :doc "Extracts a list of midi events from a score element (chord, chord-seq, multi-seq, voice, poly)"
  (let ((evtList nil) (subList nil) (reponse nil)) 
    (loop for sub in (inside self) 
          for i = 1 then (+ 1 i) do              
          (setf sublist (get-midievents sub))
          (if (or (multi-seq-p self) (poly-p self)) 
            (loop for event in subList do
                  (setf (ev-ref event) i))) 
          (if sublist (push sublist evtList))
          )
    (setf evtList (flat (reverse evtList)))
    (loop for event in evtList do 
          (if (parent self) (setf (ev-date event) (+ (ev-date event) (offset->ms self)))))
    
    (if (measure-p self) (push (make-instance 'MidiEvent   
                                 :ev-type :TimeSign 
                                 :ev-date (if (parent self) (offset->ms self) 0)
                                 :ev-ref 0 
                                 :ev-port 0
                                 :ev-chan 1
                                 :ev-fields (list (first (first (tree self)))
                                                           (round (log (second (first (tree self))) 2))
                                                           24 
                                                           8   ;;; (max-div self)
                                                           ))
                           evtList))

    (if (voice-p self) (push (make-instance 'MidiEvent   
                                 :ev-type :Tempo
                                 :ev-date (if (parent self) (offset->ms self) 0)
                                 :ev-ref 0 
                                 :ev-port 0
                                 :ev-chan 1
                                 :ev-fields (list (tempo-a-la-noire (car (tempo self)))))
                           evtList))
    
    (loop for event in evtList do 
          (if (or (not test) (funcall test event)) (push event reponse)))
    
    (reverse reponse)))

;=========================================
;;; SCORE TO MIDI
;=========================================

(defmethod* NoteToEvents ((self Note))
 (list (make-instance 'MidiEvent   
                      :ev-type :KeyOn
                      :ev-date (if (parent self) (offset->ms self) 0)
                      :ev-ref 1 
                      :ev-port (port self)
                      :ev-chan (chan self)
                      :ev-fields (list (round (/ (midic self) 100)) (vel self)))
       (make-instance 'MidiEvent   
                           :ev-type :KeyOff
                           :ev-date (+ (if (parent self) (offset->ms self) 0)
                                       (real-dur self))
                           :ev-ref 1 
                           :ev-port (port self)
                           :ev-chan (chan self)
                           :ev-fields (list (round (/ (midic self) 100)) 0))))

(defmethod* objFromObjs ((self Note) (type MidiEvent))
  (let ((evt (make-instance 'MidiEvent   
                           :ev-type :Note
                           :ev-date (if (parent self) (offset->ms self) 0)
                           :ev-ref 1 
                           :ev-port (port self)
                           :ev-chan (chan self)
                           :ev-fields (list (round (/ (midic self) 100)) (vel self) (real-dur self)))))
    evt))

(defmethod* objFromObjs ((self simple-container) (type eventmidi-seq))
  (let ((newseq (objFromObjs (get-midievents self) type)))
    (if (eventmidi-seq-p self) (setf (name newseq) (name self)))
    newseq))


;=========================================
;;; MIDI TO SCORE
;=========================================

;;; option in score editors
(defmethod score-import ((format (eql 'midi)) object)
  (let ((name (catch-cancel (om-choose-file-dialog :types '("MIDI files" "*.mid;*.midi" "All files" "*.*")))))
    (when name ; (and name (stringp (pathname-type name)))
      (objfromobjs (load-midifile name) object)
      )))

(defmethod* objFromObjs ((self eventmidi-seq) (type chord-seq))
  (let ((newcs (make-instance (type-of type)))
        (midiList (sort (evm-seq2midiList self) '< :key 'second)))
    (setQValue newcs 1000 :recursive nil)
    (setf (inside newcs) nil)
    (when midilist
      (setf (inside newcs)  (make-quanti-chords-with-midiport midilist *global-deltachords*))
      (adjust-extent newcs)
      (QNormalize newcs))
    newcs))

(defmethod* objFromObjs ((self eventmidi-seq) (type multi-seq))
  (let ((midilist (midilist2trackList (evm-seq2midiList self))) csList)
    (loop for item in midilist do
          (let ((newcs (make-instance 'chord-seq)))
            (setQValue newcs 1000 :recursive nil)
            (setf (inside newcs) nil)
            (setf (inside newcs) (make-quanti-chords-with-midiport item *global-deltachords*))
            (QNormalize newcs)
            (push newcs csList)))
    (make-instance 'multi-seq
      :chord-seqs (reverse csList))
))


;=========================================
;;; USING TEMPO MAP
;=========================================

;=== Coverts a chord-seq + tempo-map to voice
(defmethod! cseq+tempo->voice ((self chord-seq) (tmap tempo-map) &optional (type 'voice))
  :icon 921
  :indoc '("a chord-seq" "a tempo-map") 
  :doc "
Converts <self> (chord-seq) into a VOICE object acoording to <tmap> (a TEMPO-MAP object, see MIDI section).
"
 (let* ((cseq (merge-chords (clone self))) 
         (quantypar (copy-list *quantify-def-params*)) 
         (tempo (second (first (tempo-Evts tMap)))) 
         (signature (second (first (timesign-Evts tMap))))
         (durs (append (butlast (x->dx (lonset cseq)))
                       (list (extent->ms (car (last (chords cseq)))))))
         (durs (if (zerop (car (lonset cseq)))
                 durs (cons (* (car (lonset cseq)) -1) durs)))
         newvoice)
    
    (if tempo (setf (first quantypar) tempo))
    (if signature (progn
                    (setf (second quantypar) (list (first signature) (expt 2 (second signature))))
                    (setf (third quantypar) (fourth signature))
                    ))
    (setf newvoice (make-instance type
                     :tree (omquantify durs
                                       (first quantypar)
                                       (second quantypar)
                                       (third quantypar)
                                       (fifth quantypar)
                                       (fourth quantypar)
                                       (sixth quantypar))
                     :tempo (first quantypar)
                     :legato 100
                     :chords (chords cseq)))                        
    newvoice))

(defmethod* objFromObjs ((self midi-score-element) (type voice))
  (let ((tempoMap (get-tempoMap self))
        (midichords (objfromObjs self (make-instance 'chord-seq))))
    (cseq+tempo->voice midichords tempoMap (type-of type))))


(defmethod* Objfromobjs ((Self midi-score-element) (Type poly))
  (let ((voice (make-instance 'voice))  
        (m-seq (objfromobjs self (make-instance 'multi-seq)))
        (tempoMap (get-tempoMap self)))
    (make-instance (type-of type) :voices (mapcar #'(lambda (chseq) (cseq+tempo->voice chseq tempoMap)) (inside m-seq)))))












