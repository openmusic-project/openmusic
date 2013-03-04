(in-package :om)

;===================================================
;=== CONVERSION BETWEEN MIDI AND MUSICAL OBJECTS ===
;===================================================

;===========================================
;Save objects in MidiFile (const tempo = 60)
;===========================================

(defvar *def-midi-format* 1)

(defmethod* save-as-midi ((object t) &optional filename &key  (approx 2) (format nil)) 
    :initvals '(nil) 
  :icon 900
  :doc "Saves <object> as a MIDI file.

- <filename> defines the target pathname. If not specified, will be asked through a file choose dialog.
- <approx> specifies the tone division (2, 4 or 8).
- <format> alows to choose the MIDIFile format (0 or 1)

For POLY objects: If all voice have same tempo, this tempo is saved in MidiFile. Else All voices are saved at tempo 60."
  (when *midiplayer*
      (let ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog  :directory (def-save-directory) 
                                                                                     :prompt (om-str :save-as) 
                                                                                     :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi")))))
      (when name 
        (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
        (save-midifile name object approx (or format *def-midi-format*))
        (namestring name)
        ))))


(defmethod* save-as-midi ((object voice) &optional filename &key (approx 2) (format nil)) 
  (when *midiplayer*
    (let ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog :directory (def-save-directory) 
                                                                                  :prompt (om-str :save-as) 
                                                                                  :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi")))))
      (when name 
        (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
        (save-midifile-with-tempo name object approx (tempo-a-la-noire (car (tempo object))) (or format *def-midi-format*))
        (namestring name)))))


(defun save-midifile (name obj approx &optional (format 1))
  (MidiSaveAny obj approx)
  (save-seq *playing-midi-seq* name format))

(defmethod MidiSaveAny ((object t) approx)
  (when *midiplayer*
    (setf *MidiShare-start-time* 0)
    (setf *playing-midi-seq* (om-midi-new-seq))
    (PrepareToPlay t object 0 :approx approx :voice 1)))


;==== Saves sequence with tempo 60
;==== modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60
(defun save-seq (seq name &optional (format 1))
  (let ((tempo-evnt (om-midi-new-evt  (om-midi-get-num-from-type "Tempo")
                                     :date 0 :vals 1000000)))
    (om-midi-seq-concat-evt seq tempo-evnt nil)
    (om-midi-save-seq-in-file seq (om-path2cmdpath name) :fileformat format)
    ))
   

;=======================================================
;== Save voice/poly in midifile                       ==
;== Considering tempo and time signature information ==
;=======================================================

(defun save-voice-seq (seq name &optional (format 1))
  (om-midi-save-seq-in-file seq (om-path2cmdpath name) :fileformat format))

(defun save-midifile-with-tempo (name obj approx tempo &optional (format 1))
  (let (newSeq)
    (MidiSaveAny obj approx)
    (setf newSeq (insert-tempo-info *playing-midi-seq* tempo))
    (save-voice-seq newSeq name format)))



(defmethod* save-as-midi ((object poly) &optional filename &key (approx 2) (format nil))
  (when *midiplayer*
    (let* ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog
                                                          :directory (def-save-directory) 
                                                          :prompt (om-str :save-as) 
                                                          :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi"))))
           (tempo (poly-same-tempo object)))
      (when name 
        (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
        (if tempo 
          (save-midifile-with-tempo name object approx (tempo-a-la-noire (car tempo)) (or format *def-midi-format*))
          (save-midifile name object approx (or format *def-midi-format*)))  
        (namestring name)))))


;=== Tests if all voices of a ply object have the same tempo
;=== returns the tempo in case true and nil if not
(defmethod poly-same-tempo ((self poly))
  (let* ((alltempo (loop for voiceItem in (inside self) collect (tempo voiceItem)))
         (currtempo (car (first alltempo))))
    
    (loop for item in alltempo 
          while currtempo do
          
          (if (or (cadr item) ;; tempo changes in a voice
                  (not (= (/ (cadr (car item)) (car (car item)))
                          (/ (cadr currtempo) (car currtempo)))))
                  (setf currtempo nil) (setf currtempo (car item))))
    (list currtempo nil)))







;===========================
;=== OBJECTS CONVERSIONS ===
;===========================

(defmethod* objFromObjs ((self Note) (type MidiEvent))
  (let ((evt (make-instance 'MidiEvent   
                           :ev-type (om-midi-get-num-from-type "Note")
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

     


;=====================
;==== VOICES/POLY ====
;=====================

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

(defmethod* objFromObjs ((self Midi-score-element) (type voice))
  (let ((tempoMap (get-tempoMap self))
        (midichords (objfromObjs self (make-instance 'chord-seq))))
    (cseq+tempo->voice midichords tempoMap (type-of type))))


(defmethod* Objfromobjs ((Self midi-score-element) (Type poly))
  (let ((voice (make-instance 'voice))  
        (m-seq (objfromobjs self (make-instance 'multi-seq)))
        (tempoMap (get-tempoMap self)))
    (make-instance (type-of type) :voices (mapcar #'(lambda (chseq) (cseq+tempo->voice chseq tempoMap)) (inside m-seq)))))




;=========================================
;=== MIDI CONVERSION -> get-midievents ===
;=========================================

;=== Calculates maximum division of a measure
; bug quand on a des fractions etc...
; om-round pour le cas des liasions 1.0
(defmethod max-div ((self measure))
  (denominator (* 4 (list-min (tree2ratio (list '? (om-round (list (tree self)))))))))


(defmethod! get-midievents ((self score-element) &optional test)
  :initvals '(nil nil) 
  :indoc '("score element object" "lambda test function for midi events to extract") 
  :icon 902
  :doc "Extracts a list of midi events from a score element (chord, chord-seq, multi-seq, voice, poly)"
  (let ((evtList nil) (subList nil) (reponse nil)) 
    (loop for sub in (inside self) 
          for i = 1 then (+ 1 i) do              
          (setf sublist (get-midievents sub))
          (if (or (multi-seq-p self)(poly-p self)) 
            (loop for event in subList do
                  (setf (ev-ref event) i))) 
          (if sublist (push sublist evtList))
          )
    (setf evtList (flat (reverse evtList)))
    (loop for event in evtList do 
          (if (parent self) (setf (ev-date event) (+ (ev-date event) (offset->ms self)))))
    
    (if (measure-p self) (push (make-instance 'MidiEvent   
                                 :ev-type (om-midi-get-num-from-type "TimeSign") 
                                 :ev-date (if (parent self) (offset->ms self) 0)
                                 :ev-ref 0 
                                 :ev-port (verify-port 0)
                                 :ev-chan 1
                                 :ev-fields (list (first (first (tree self)))
                                                           (round (log (second (first (tree self))) 2))
                                                           24 
                                                           8   ;;; (max-div self)
                                                           ))
                           evtList))

    (if (voice-p self) (push (make-instance 'MidiEvent   
                                 :ev-type (om-midi-get-num-from-type "Tempo") 
                                 :ev-date (if (parent self) (offset->ms self) 0)
                                 :ev-ref 0 
                                 :ev-port (verify-port 0)
                                 :ev-chan 1
                                 :ev-fields (list (tempo-a-la-noire (car (tempo self)))))
                           evtList))
    
    (loop for event in evtList do 
          (if (or (not test) (funcall test event)) (push event reponse)))
    
    (reverse reponse)))


(defmethod! get-midievents ((self Note) &optional test)
   :icon 902
   (when (not (memq (tie self) '(continue end)))
       (let ((event (objfromobjs self (make-instance 'MidiEvent))))
         (if (or (not test) (funcall test event)) (list event) nil))))

(defmethod! get-midievents ((self Rest) &optional test))
