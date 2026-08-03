;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================


(in-package :om)

(defmethod additional-player-params ((self scoreeditor))
  (list :port (get-edit-param self 'outport) 
        :approx (get-edit-param self 'approx)))


(defmethod get-interval-to-play ((self scoreeditor))
  (let ((interval (call-next-method)))
    (if (recording self)
        (list (or (car interval) 0) 3600000)
      interval)))

;;;=========================
;;; MIDI RECORD
;;;=========================

(defstruct midi-recorder 
  (editor) 
  (process)
  (t0)
  (memory))

(defvar *midi-recorder* (make-midi-recorder))

(defmethod timed-recording ((self scoreeditor)) t)
(defmethod timed-recording ((self noteeditor)) nil)
(defmethod timed-recording ((self chordeditor)) nil)
(defmethod timed-recording ((self chordseqeditor)) nil)
(defmethod timed-recording ((self voiceeditor)) nil)
(defmethod timed-recording ((self polyeditor)) t)


(defmethod start-recording ((self scoreeditor))
  (om-print "start MIDI recording" "OM ::")
  (if (midi-recorder-editor *midi-recorder*)
      (om-beep-msg (format nil "ERROR: MIDI recoring is already ON" 
                           (om-window-title (om-view-window (midi-recorder-editor *midi-recorder*)))))
    (progn 
      (setf (recording self) t)
      (setf (midi-recorder-editor *midi-recorder*) self)
      (when (midi-recorder-process *midi-recorder*) ;; just in case...
        (midi-in-stop (midi-recorder-process *midi-recorder*)))
      (setf (midi-recorder-memory *midi-recorder*) nil)
      (setf (midi-recorder-t0 *midi-recorder*) (get-internal-real-time))
      (setf (midi-recorder-process *midi-recorder*) 
            (midi-in-start  *def-midi-in* 
                           #'(lambda (msg time) 
                                ;; the player is "activated"
                                ;; (print msg)
                               (cond 
                                ((equal (state (player self)) :play)
                                 (setf (om-midi:midi-evt-date msg) (get-player-time (player self)))
                                 (push msg (midi-recorder-memory *midi-recorder*)))
                                ((not (timed-recording self)) ;; e.g. chord, note... 
                                 (setf (om-midi:midi-evt-date msg) (- (get-internal-real-time) (midi-recorder-t0 *midi-recorder*)))
                                 (push msg (midi-recorder-memory *midi-recorder*)))
                                (t nil))
                               )
                           1 *def-midi-out*))
      )
    ))


(defmethod stop-recording ((self scoreeditor))
  (unwind-protect
      (progn 
        (when (recording self)
          (om-print "stop MIDI recording" "OM ::"))
        
        (let* ((midilist (midievents2midilist (reverse (midi-recorder-memory *midi-recorder*))))
               (note-list (loop for item in midilist collect 
                                (list (nth 1 item)
                                      (make-instance 'note 
                                                     :midic (* 100 (nth 0 item))
                                                     :dur (if (minusp (nth 2 item)) 1000 (nth 2 item))
                                                     :vel (nth 3 item)
                                                     :chan (nth 4 item))))))
          (when note-list 
            (add-recorded-seq-to-editor self note-list))
          ))
    (setf (midi-recorder-editor *midi-recorder*) nil)
    (setf (midi-recorder-memory *midi-recorder*) nil)
    (midi-in-stop (midi-recorder-process *midi-recorder*))
    (setf (recording self) nil)
    ))
                  

(defmethod close-editor-before ((self scoreeditor))
  (stop-recording self)
  (call-next-method))


(defmethod add-recorded-seq-to-editor ((editor t) seq)
  (om-beep-msg "Sorry, can't record that..."))

(defmethod add-recorded-seq-to-editor ((editor scoreeditor) seq)
  (let ((newobject (record2obj editor seq))
        (lastobject (object editor)))
    (when newobject
      (setf (object editor) newobject)
      (change-val-of-reference editor newobject lastobject)
      (update-panel (panel editor) t))))

;;;;;;
(defmethod record2obj ((self t) seq) nil)

(defmethod record2obj ((self noteeditor) list)
  (cadr (last-elem list)))

(defmethod record2obj ((self chordeditor) list)
  (let ((chord (objfromobjs (mapcar 'cadr list) (make-instance 'chord)))
        (onsets (mapcar 'car list)))
    (when (= (get-edit-param self 'mode) 4) ;;; "offset" mode
      (setf (loffset chord) (om- onsets (list-min onsets))))
    chord))

(defun chords-from-list (list)
  (make-quanti-chords-MC
   (mapcar #'(lambda (elt)
               (list (midic (cadr elt))
                     (car elt)
                     (dur (cadr elt))
                     (vel (cadr elt))
                     (chan (cadr elt))))
           list)
   *global-deltachords*))

;when cursor-mode = :interval, records starting from the interval's offset
;when obj-mode = chord-seq, records starting from the end of duration of last-chord 
;when obj-mode = note, appends recording from begining
;when obj-mode = chord, erases everything and record anew
;note that obj-mode is a string "chord", "chord-seq" etc...
 
(defmethod record2obj ((editor chordseqeditor) list) 
  (let* ((obj (object editor))
         (panel (panel editor))
         (cursormode (cursor-mode panel))
         (objmode (obj-mode panel)))
    (when list
       (close-attached-editors editor)
       (let ((chords (chords-from-list list)))
         (loop for item in chords do
                 (cond
                  ((equal cursormode :interval) 
                   (let ((off (cursor-pos panel)))
                   (setf (offset item) (+ off (offset item)))))
                  ((equal objmode "chord-seq") 
                   (let ((off (+ (offset (last-elem (chords obj))) (car (ldur (last-elem (chords obj)))))))
                     (setf (offset item) (+ off (offset item)))))
                  ((equal objmode "chord") (setf (inside obj) nil))
                  (t ))
                 (setf (parent item) obj))
         (loop for item in (chords obj) do
               (setf (offset item) (offset->ms item)))
         (setQValue obj 1000 :recursive nil)
         ;(setf (inside obj) nil)
         (setf (inside obj) (sort (append chords (chords obj)) '< :key 'offset))
         (setf (Qvalue obj) 1000)
         (adjust-extent obj)
         (QNormalize obj)))
     obj))
           

(defmethod record2obj ((editor multiseqeditor) list)
   (let ((obj (object editor))
         (chords (chords-from-list list)))
     (let ((newcs (make-instance 'chord-seq)))
       (setQValue newcs 1000 :recursive nil)
       (setf (inside newcs) nil)
       (setf (inside newcs) (sort chords '< :key 'offset))
       (loop for ch in (chords newcs) do (setf (parent ch) newcs))
       (adjust-extent newcs)
       (QNormalize newcs)
       (change-multi-inside (panel editor) (append (inside obj) (list newcs)))
       obj)))


(defmethod record2obj ((editor voiceeditor) list)
   (let ((obj (object editor))
         (chords (chords-from-list list)))
     (close-attached-editors editor)
     (setf (chords obj) (append chords (nthcdr (length chords) (chords obj))))
     obj))

(defmethod record2obj ((editor polyeditor) list)
  (let ((obj (object editor))
        (chords (chords-from-list list)))
    (let ((newcs (make-instance 'chord-seq)))
      (setQValue newcs 1000 :recursive nil)
      (setf (inside newcs) nil)
      (setf (inside newcs) (sort chords '< :key 'offset))
      (loop for ch in (chords newcs) do 
            (setf (offset ch) (- (offset ch) (car (car list))))
            (setf (parent ch) newcs))
      (adjust-extent newcs)
      (QNormalize newcs)
      (change-multi-inside (panel editor) (append (inside obj) (list (objfromobjs newcs (make-instance 'voice)))))
      obj)))




;;;=========================
;;; PREPARE TO PLAY
;;;=========================


;;;FUNCTIONS FOR EXTRAS

;;TRILL

(defmethod make-trill-from-chords ((self chord) (chord2 chord) (n number) (ms number))
  "<number> being number of repetition of the two chords"
  (let* ((c1 (clone self))
         (c1 (setf (ldur c1) (list ms)))
         (c1n (repeat-n c1 n))
         (c2 (clone chord2))
         (c2 (setf (ldur c2) (list ms)))
         (c2n (repeat-n c2 n))
         (chrdlst (flat (mat-trans (list c1n c2n))))
         (chordseq (objfromobjs chrdlst (make-instance 'chord-seq))))
         (setf (lonset chordseq) (list 0 ms))))

;here when lmidic given.
(defmethod make-trill-from-chords ((self chord) (chord2 list) (n number) (ms number))
  "<number> being number of repetition of the two chords"
  (let* ((c1 (clone self))
         (c1 (setf (ldur c1) (list ms)))
         (c1n (repeat-n c1 n))
         (c2 (clone self))
         (c2 (setf (lmidic c2) chord2))
         (c2 (setf (ldur c2) (list ms)))
         (c2n (repeat-n c2 n))
         (chrdlst (flat (mat-trans (list c1n c2n))))
         (chordseq (objfromobjs chrdlst (make-instance 'chord-seq))))
         (setf (lonset chordseq) (list 0 ms))))

;not used
(defmethod prepare-to-trill ((self1 chord) (self2 t) (ms number));default dur = 85
  (when (get-trill self1)
  (let* ((trill (get-trill self1))
         (offs (msoffsets trill))
         (dur (car (ldur self1)))
         (ndurs (ceiling (/ (ceiling (/ dur ms)) 2))))
    (if self2
         (make-trill-from-chords self1 self2 ndurs ms)
      (let* ((step (approx-factor (get-scale-from-approx (approx self1))))
             (clone (clone self1)))
        (setf (lmidic clone) (om+ (lmidic clone) (ceiling step)))
        (make-trill-from-chords self1 clone ndurs ms))))))


(defmethod chdeq-from-trill ((self1 chord));default *gdur* = 85
  (when (get-trill self1)
  (let* ((trill (get-trill self1))
         (offs (msoffsets trill))
         (dur (- (second offs) (car offs)))
         (ndurs (ceiling (/ (ceiling (/ dur *gdur*)) 2))))

    (let* ((step (approx-factor (get-scale-from-approx (approx self1))))
           (clone (clone self1)))
      (setf (lmidic clone) (lmidic trill))
        (make-trill-from-chords self1 clone ndurs *gdur*)))))

;;;;;;;

(defmethod PrepareToPlay ((player t) (self chord) at &key approx port interval voice)
  (append  
    ;;gracenotes 
   (when (gnotes self)
     (let ((chseq (make-instance 'chord-seq 
                                 :lmidic (mapcar 'lmidic (glist (gnotes self)))
                                 :lonset (list 0 *gdur*)
                                 :ldur (list (- *gdur* 1))
                                 :lvel (list (car (lvel self)))
                                 :lchan (list (car (lchan self))))))
       (PrepareToPlay player chseq (- at (* *gdur* (length (glist (gnotes self)))))
                      :approx approx 
                      :port port
                      :interval interval
                      :voice voice)))
   ;;crescendo,diminuendo
   (when (or (get-crescendo self) (get-diminuendo self))
     (let* ((extra (or (get-crescendo self) (get-diminuendo self)))
            (offs (msoffsets extra))
            (start-vel (start-val extra))
            (end-vel (end-val extra))
            (timestamps  (arithm-ser (car offs)  (- (second offs) 1)  20))
            (velvalues (bpf-sample (om-make-bpf 'bpf offs (list start-vel end-vel)
                                                0)
                                   (car offs) (second offs) (length timestamps))))
       (list
        (loop for off in timestamps
              for val in velvalues
              collect(om-midi::make-midi-evt :type :CtrlChange
                                             :date  off
                                             :port (or (car (lport self)) *def-midi-out*)
                                             :chan (car (lchan self))
                                               ;:ref 0
                                             :fields (list 7 val));expression = 11
                )
        ;reset volume
        (om-midi::make-midi-evt :type :CtrlChange
                                :date  (last-elem offs)
                                :port (or (car (lport self)) *def-midi-out*)
                                :chan (car (lchan self))
                                :fields (list 7 100)
                                )
           
        )))

      ;;sostpedal (faire une fonction pour les extras);
   (when (get-sost-pedal self)
     (let ((offs (msoffsets (get-sost-pedal self))))
       (list
        (om-midi::make-midi-evt :type :CtrlChange
                                :date (car offs)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 64 127))
        (om-midi::make-midi-evt :type :CtrlChange
                                :date (second offs)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 64 0)))
       ))
   ;;trill
   (when (get-trill self)
     (let ((chseq (chdeq-from-trill self)))
       (PrepareToPlay player chseq at
                      :approx approx 
                      :port port
                      :interval interval
                      :voice voice)
       ))
   ;;gliss
   (when (get-gliss-extra self)
     (let* ((offs (msoffsets (get-gliss-extra self)))
            (start-mc (car (lmidic self)))
            (end-mc (car (targetmc (get-gliss-extra self))))
            (delta-mc (- end-mc start-mc))       
            (bend-range 48) ;4 octaves
            (bend-val (round (+ 8192
                                (* (/ delta-mc (* bend-range 100))
                                   8191)))) 
            (timestamps  (arithm-ser (car offs)  (- (second offs) 1)  20))
            (bendvalues (bpf-sample (om-make-bpf 'bpf offs (list 8192 bend-val) ;16383)
                                                 0)
                                    (car offs) (second offs) (length timestamps))))
       (x-append
        
        (om-midi::make-midi-evt :type :CtrlChange
                                :date 0;(- (car offs) 5)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 101 0))

        
        (om-midi::make-midi-evt :type :CtrlChange
                                :date 0;(- (car offs) 5)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 100 0))
        
        
        (om-midi::make-midi-evt :type :CtrlChange
                                :date (- (car offs) 5)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 6 bend-range));12 interval in semitones of gliss
        
        
        (om-midi::make-midi-evt :type :CtrlChange
                                :date (- (car offs) 5)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 38 0));0 8192
        
       
        (loop for off in timestamps
              for val in bendvalues
              collect  (om-midi::make-midi-evt :type :PitchBend
                                               :date  off
                                               :port (or (car (lport self)) *def-midi-out*)
                                               :chan (car (lchan self))
                                               ;:ref 0
                                               :fields (list val)))
        (om-midi::make-midi-evt :type :PitchBend
                                :date (second offs)
                                :port (or (car (lport self)) *def-midi-out*)
                                :chan (car (lchan self))
                                               ;:ref 0
                                :fields (list 8192))
        
        )))
   (call-next-method)))


;prepare-to-play

(defmethod PrepareToPlay ((player t) (self rest) at &key approx port interval voice)
  (append
    ;;gracenotes
   (when  (gnotes self)
     (let ((chseq (make-instance 'chord-seq 
                                 :lmidic (mapcar 'lmidic (glist (gnotes self)))
                                 :lonset (list 0 *gdur*)
                                 :ldur (list (- *gdur* 1)))))
       (PrepareToPlay player chseq (- at (* *gdur* (length (glist (gnotes self)))))
                      :approx approx 
                      :port port
                      :interval interval
                      :voice voice)))
     ;;sostpedal
   (when (get-sost-pedal self)
     (let ((offsets (msoffsets (get-sost-pedal self))))
       (list
        (om-midi::make-midi-evt :type :CtrlChange
                                :date (car offsets)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 64 127))
        (om-midi::make-midi-evt :type :CtrlChange
                                :date (second offsets)
                                :chan (car (lchan self))
                                :port (or (car (lport self)) *def-midi-out*)
                                :fields (list 64 0)))
       ))
   (call-next-method)))


;;;;;

;-----------ARP-CHORD

(defclass arp-chord ()
  ((notes :initform nil :initarg :notes :accessor notes)))

(add-player-for-object 'arp-chord '(:midi-player :osc-scoreplayer :microplayer))

(defmethod extent ((self arp-chord))
   (* (length (notes self)) 500))

(defmethod get-obj-dur ((self arp-chord)) (extent self))

(defmethod play-obj? ((self arp-chord)) t)

(defmethod chord-obj-to-play ((self chord) mode)
  (if (find mode '(1 2 3) :test '=)     
     (let ((notes (copy-list (inside self))))
       (case mode
         (1 (make-instance 'arp-chord :notes (sort notes '< :key 'midic)))
         (2 (make-instance 'arp-chord :notes (sort notes '> :key 'midic)))
         (3 (make-instance 'arp-chord :notes notes)))
       )
    self))

;=== Play a chord in "arp" mode
(defmethod PrepareToPlay ((player t) (chord arp-chord) at &key  approx port interval voice)
     ;(setf port (verify-port port))
    (loop for note in (notes chord)
          for offset from 0 by 400
          collect (PrepareToPlay player note (+ offset at) 
                                 :approx approx
                                 :port port :interval interval :voice voice)))

;;; request from TM/JF: never play a box in "arp" mode
;;; to do it, just use mode = 0 always
(defmethod play-obj-from-value ((value chord) (box omboxeditcall)) 
  (chord-obj-to-play value (get-edit-param box 'mode)))

(defmethod get-obj-to-play ((self chordeditor))
  (chord-obj-to-play (object self) (staff-mode (panel self))))


