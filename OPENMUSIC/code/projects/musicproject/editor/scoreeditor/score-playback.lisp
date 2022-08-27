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








