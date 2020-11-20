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
;;; authors  G. Assayag, C. Agon, K. Haddad
;=========================================================================
;
;
;============================================================================
; File author: Karim Haddad
;============================================================================
;;Obj info


(in-package :om)

;from draw-time-selection in scoreeditors.lisp
(defmethod get-obj-selection ((self voicepanel))
  (let ((b 0)
        (e (get-obj-dur (object (editor self)))))
        (if (and (linear? self) (cursor-p self) (cursor-interval self) (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
            (setf b (car (cursor-interval self))
                  e (cadr (cursor-interval self)))
        ;nil
          (when (selection? self) 
            (setf b e) (setf e 0)
            (loop for item in (selection? self) do
                  (unless (or (system? item) (extra-p item))
                    (when (< (offset->ms item (object (editor self))) b) (setf b (offset->ms item (object (editor self)))))
                    (when (> (+ (offset->ms item (object (editor self))) (get-obj-dur item)) e) (setf e (+ (offset->ms item (object (editor self))) (get-obj-dur item))))
                    ))))
        (list b e)))

(defmethod get-obj-selection ((self polypanel))
  (let ((b 0)
        (e (get-obj-dur (object (editor self)))))
        (if (and (linear? self) (cursor-p self) (cursor-interval self) (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
            (setf b (car (cursor-interval self))
                  e (cadr (cursor-interval self)))
        ;nil
          (when (selection? self) 
            (setf b e) (setf e 0)
            (loop for item in (selection? self) do
                  (unless (or (system? item) (extra-p item))
                    (when (< (offset->ms item (object (editor self))) b) (setf b (offset->ms item (object (editor self)))))
                    (when (> (+ (offset->ms item (object (editor self))) (get-obj-dur item)) e) (setf e (+ (offset->ms item (object (editor self))) (get-obj-dur item))))
                    ))))
        (list b e)))

(defmethod get-obj-selection ((self chordseqpanel))
  (let ((b 0)
        (e (get-obj-dur (object (editor self)))))
        (if (and (linear? self) (cursor-p self) (cursor-interval self) (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
            (setf b (car (cursor-interval self))
                  e (cadr (cursor-interval self)))
        ;nil
          (when (selection? self) 
            (setf b e) (setf e 0)
            (loop for item in (selection? self) do
                  (unless (or (system? item) (extra-p item))
                    (when (< (offset->ms item (object (editor self))) b) (setf b (offset->ms item (object (editor self)))))
                    (when (> (+ (offset->ms item (object (editor self))) (get-obj-dur item)) e) (setf e (+ (offset->ms item (object (editor self))) (get-obj-dur item))))
                    ))))
        (list b e)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-slots-info ((self t)) nil)

(defmethod get-slots-info ((self note))
  (let ((midic (midic self))
        (vel (vel self))
        (dur (dur self))
        (chan (chan self))
        (port (port self)))
    (list midic vel dur chan port)))
        
(defmethod get-slots-info ((self chord))
  (let ((midic (lmidic self))
        (vel (lvel self))
        (dur (ldur self))
        (chan (lchan self))
        (off (loffset self))
        (port (lport self)))
    (list midic vel dur chan off port)))

(defmethod get-slots-info ((self continuation-chord))
  nil)

(defmethod get-slots-info ((self rest))
 nil)

;;;;;;;;;;;;;;pour VOICE only;;;;;;;;;;;

(defmethod get-only-chords ((self t))
t)

(defmethod get-only-chords ((self list))
  (remove nil (mapcar #'get-only-chords self)))


(defmethod get-only-chords ((self rest))
nil)

(defmethod get-only-chords ((self continuation-chord))
nil)

(defmethod get-only-chords ((self chord))
  self)

(defmethod get-only-chords ((self group))
  (let* ((inside (inside self)))
    (remove nil (flat (mapcar #'get-only-chords inside)))))

(defmethod get-only-chords ((self measure))
    (let* ((inside (inside self)))
    (remove nil (flat (mapcar #'get-only-chords inside)))))

(defmethod get-only-chords ((self voice))
    (let* ((inside (inside self)))
    (remove nil (flat (mapcar #'get-only-chords inside)))))

(defun get-voice (self)
  "starting from an inside obj of voice, returns voice."
  (let ((pere (parent self)))
    (if (voice-p pere) 
        pere 
      (get-voice pere))))


(defmethod evt-pos ((self rest))
nil)

(defmethod evt-pos ((self note))
nil)

(defmethod evt-pos ((self continuation-chord))
nil)

(defmethod evt-pos ((self group))
nil)


(defmethod evt-pos ((self chord))
  (let* ((voice (get-voice self))
         (chords (get-only-chords voice)))
    (1+ (position self chords :test 'equal))))

(defmethod evt-pos ((self measure))
  (let* ((voice (get-voice self)))
    (1+ (position self (inside voice) :test 'equal))))

(defmethod evt-pos ((self voice))
  (let((pere (parent self)))
    (1+ (position self (inside pere) :test 'equal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-slots (selection)
  (if (= 1 (length selection))
      (let* ((sel (car selection))
             (notes (car sel))
             (vel (second sel))
             (dur (third sel))
             (chan (fourth sel))
             (off (fifth sel))
             (port (sixth sel)))
        (list notes vel dur chan off port))
    (let* ((sel (mat-trans selection))
           (notes (car sel))
           (vel (second sel))
           (dur (third sel))
           (chan (fourth sel))
           (off (fifth sel))
           (port (sixth sel)))
      (list notes vel dur chan off port)))
  )


(defmethod get-obj-info ((self t)) nil)


(defmethod get-obj-info ((self voicepanel))
  (let* ((selection (selection? self))
         (pere (parent (car selection)))
         (name (cond
                ((rest-p (car selection)) "REST")
                ((cont-chord-p (car selection)) "TIE") 
                 (t (string-upcase (obj-mode self)))))
         (voice (object (om-view-container self)))
         (chords-only (get-only-chords selection))
         (pos (if (not (voice-p (car selection))) 
                  (if (= (length selection) 1)
                      (evt-pos (car selection))
                    (let ((posi (remove nil (loop for i in selection 
                                      collect (evt-pos i)))))
                      (list (last-elem posi) (car posi))))))

         (time-selection (get-obj-selection self))
         (slots 
          (loop for i in chords-only
                collect
                (get-slots-info i)))
         (fslots (format-slots (reverse slots))))
    (setf om-edit::*info-pack* (list 
                     name
                     (if (null pos)
                         (format nil "pos:")
                       (if (and pos (atom pos))
                           (format nil "pos: ~S" pos)
                         (format nil "pos: ~S - ~S " (car pos) (second pos))))
                     (format nil "~S - ~S ms" (car time-selection) (second time-selection))
                     (format nil "~S" (car fslots))
                     (format nil "~S" (second fslots))
                     (format nil "~S" (third fslots))
                     (format nil "~S" (fourth fslots))
                     (format nil "~S" (fifth fslots))
                     (format nil "~S" (sixth fslots))
                     ))
    (let ((ept (om-edit::open-chordseq-info chords-only)))
      (setf (om-edit::ompanel ept) self)
      (setf (om-edit::intfunc ept) #'om::set-obj-info))
    ))


(defmethod get-obj-info ((self polypanel))
  (let* ((selection (car (selection? self)))
         (pere (parent selection))
         (name (if (rest-p selection) "REST" (string-upcase (obj-mode self))))
         (voice (object (om-view-container self)))
         (pos (if (not (poly-p selection))
                  (if (= (length (selection? self)) 1)
                      (evt-pos selection)
                    (let ((posi (loop for i in (selection? self)
                                      collect (evt-pos i))))
                      (list (last-elem posi) (car posi))))
                ))
         (time-selection (get-obj-selection self))
         (slots 
          (loop for i in (selection? self)
                collect
                (get-slots-info i)))
         (fslots (format-slots (reverse slots))))
    (setf om-edit::*info-pack* (list 
                     name
                     (if (atom pos)
                         (format nil "pos: ~S" pos)
                       (format nil "pos: ~S - ~S " (car pos) (second pos)))
                     (format nil "~S - ~S ms" (car time-selection) (second time-selection))
                     ;(format-slots  slots)
                     (format nil "~S" (car fslots))
                     (format nil "~S" (second fslots))
                     (format nil "~S" (third fslots))
                     (format nil "~S" (fourth fslots))
                     (format nil "~S" (fifth fslots))
                     (format nil "~S" (sixth fslots))
                     ))
    ))


;NOTE, CHORD AND CHORDSEQ
(defmethod get-obj-info ((self notepanel))
  (let* ((selection (selection? self))
         (name (string-upcase (obj-mode self)))
         (slots (get-slots-info (car selection))))
    (setf om-edit::*info-pack* (list 
                       name
                       (format nil "")
                       (format nil "")
                       (format nil "~S" (car slots))
                       (format nil "~S" (second slots))
                       (format nil "~S" (third slots))
                       (format nil "~S" (fourth slots))
                       (format nil "~S" (fifth slots))
                       (format nil "~S" (sixth slots))
                       ))
    (let ((ept (om-edit::open-note-info selection)))
      (setf (om-edit::ompanel ept) self)
      (setf (om-edit::intfunc ept) #'om::set-obj-info))
    ))




(defmethod get-obj-info ((self chordpanel))
  (let* ((selection (selection? self))
         (pere (parent (car selection)))
         (name (string-upcase (obj-mode self)))
         (slots (get-slots-info (car selection))))
    (setf om-edit::*info-pack* (list 
                       name
                       (format nil "")
                       (format nil "")
                       (format nil "~S" (car slots))
                       (format nil "~S" (second slots))
                       (format nil "~S" (third slots))
                       (format nil "~S" (fourth slots))
                       (format nil "~S" (fifth slots))
                       (format nil "~S" (sixth slots))
                       ))
  (let ((ept (om-edit::open-chord-info selection)))
      (setf (om-edit::ompanel ept) self)
      (setf (om-edit::intfunc ept) #'om::set-obj-info))
 ))



(defun ordered-selection (sel pos)
  (let* ((jum (loop for i in sel 
                    for p in pos
                    collect (list i p)))
         (sort (sort. jum '< 'second)))
    (mapcar 'first sort)))


(defmethod get-obj-info ((self chordseqpanel))
  (let* ((selection (selection? self))
         (pere (parent (car selection)))
         (name (string-upcase (obj-mode self)))
         (pos (if (not (or (chord-seq-p (car selection)) (note-p (car selection))))   
                  (if (= (length selection) 1)
                      (list (1+ (position (car selection) (inside pere) :test 'equal)))
                    (let ((posi (loop for i in selection
                                      collect (1+ (position i (inside pere) :test 'equal)))))
                      (sort. posi '<))))) 
         (ordered (ordered-selection selection pos))
         (time-selection (get-obj-selection self))
         (slots 
          (loop for i in (if (note-p (car selection)) selection ordered)
                collect
                (get-slots-info i)))
         (fslots (format-slots (reverse slots))))
    (setf om-edit::*info-pack* (list 
                       name
                       (if (= 1 (length pos))
                           (format nil "pos: ~S" (car pos))
                         (format nil "pos: ~S - ~S " (car pos) (last-elem pos)))
                       (format nil "~S - ~S ms" (car time-selection) (second time-selection))
                       (format nil "~S" (car fslots))
                       (format nil "~S" (second fslots))
                       (format nil "~S" (third fslots))
                       (format nil "~S" (fourth fslots))
                       (format nil "~S" (fifth fslots))
                       (format nil "~S" (sixth fslots))
                       ))
    (let ((ept (om-edit::open-chordseq-info selection)))
      (setf (om-edit::ompanel ept) self)
      (setf (om-edit::intfunc ept) #'om::set-obj-info))
    ))





(defmethod chng-obj-inf ((self chord) nt vel dur chan off port)
  (setf (lmidic self) nt
        (lvel self) vel
        (ldur self) dur
        (lchan self) chan 
        (loffset self) off)
  (set-port self port))


(defmethod chng-obj-inf ((self note) nt vel dur chan off port)
  (setf (midic self) nt
        (vel self) vel
        (dur self) dur
        (chan self) chan)
  (set-port self port))

(defmethod chng-obj-inf ((self rest) nt vel dur chan off port)
t)

(defmethod chng-obj-inf ((self continuation-chord) nt vel dur chan off port)
t)

(defun set-obj-info (ompanel objs data)
  (let* ((obj (car objs))
         (voice (object (om-view-container ompanel)))
         (tree (if (voice-p voice) (tree voice)))
         (notes (str->list (car data)))
         (vels   (str->list (second data)))
         (durs  (str->list (nth 2 data)))
         (chans  (str->list (nth 3 data)))
         (offs  (str->list (nth 4 data)))
         (ports  (str->list (nth 5 data))))
    (if (= 1 (length objs))
        (loop for i in objs
              for n in notes
              for vel in vels
              for dur in durs
              for chan in chans
              for off in offs
              for port in  ports
              do (chng-obj-inf i n vel dur chan off port))
      (if (not (note-p obj))
      (loop for i in (reverse objs)
            for n in (car notes)
            for vel in (car vels)
            for dur in (car durs)
            for chan in (car chans)
            for off in (car offs)
            for port in (car ports)
            do (chng-obj-inf i n vel dur chan off port))
      (om-beep-msg "Select only one NOTE object!")))
    ;reopen
    (get-obj-info ompanel)
    (if (voice-p voice) 
        (setf (tree voice) (check-tree-for-contchord tree voice)))
    (update-panel ompanel t)
    ))