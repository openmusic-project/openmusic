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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

(in-package :om)

;================================
; A BPF with basic playback features
;================================
(defmethod player-name ((self (eql :bpfplayer))) "BPF-Player")
(defmethod player-desc ((self (eql :bpfplayer))) "Special player for BPFs and automations")
(defmethod player-type ((player (eql :bpfplayer))) :internal)

(enable-player :bpfplayer)


(defclass! BPF-controller (simple-container BPF) 
  ((player-fun :initform nil :accessor player-fun)))

(defmethod play-obj? ((self BPF-controller)) t)
(defmethod allowed-in-maq-p ((self BPF-controller)) t)
(defmethod get-obj-dur ((self BPF-controller)) (last-elem (x-points self)))


(add-player-for-object 'BPF-controller '(:bpfplayer))

(defmethod default-edition-params ((self BPF-controller)) 
  (pairlis '(player) '(:bpfplayer)))

(defmethod get-editor-class ((self BPF-controller)) 'bpfcontroleditor)

(defmethod get-player-action ((self t)) nil)
(defmethod get-player-action ((self BPF-controller)) (player-fun self))

(defmethod prepare-to-play ((self (eql :bpfplayer)) (player omplayer) (object bpf) at interval params)
  (let ((fun (get-player-action object))) 
    (when fun
      (if interval
          (progn
            (mapcar #'(lambda (point)
                        (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                            (schedule-task player
                                           #'(lambda () (funcall fun (cadr point))) 
                                           (+ at (car point)))))
                    (point-pairs object)))
        (progn
          (mapcar #'(lambda (point)
                      (schedule-task player
                                     #'(lambda () (funcall fun (cadr point))) 
                                     (+ at (car point))))
                  (point-pairs object)))))))
  
;; (defmethod player-loop ((self (eql :bpfplayer)) player &optional play-list) (call-next-method))
  
;================================
; EDITOR
;================================

(defclass bpfcontroleditor (bpfeditor play-editor-mixin) ())

(defmethod cursor-panes ((self bpfcontroleditor)) (list (panel self)))
(defmethod get-panel-class ((Self bpfcontroleditor)) 'bpfcontrolpanel)

(defclass bpfcontrolpanel (bpfpanel cursor-play-view-mixin) ())

(defmethod view-turn-pages-p ((self bpfcontrolpanel)) t)

(defmethod om-draw-contents ((Self bpfcontrolpanel))
  (call-next-method)
  (draw-control-info self (currentbpf self)))

(defmethod draw-control-info ((self t) (object t)) nil)

(defmethod time-to-pixels ((self bpfcontrolpanel) time)
  (call-next-method self (* time (expt 10 (decimals (object (editor self)))))))

(defmethod om-set-scroll-position ((self bpfcontrolpanel) pos) nil)

(defmethod get-x-range ((self bpfcontrolpanel))
  (let* ((bpf (object (editor self)))
         (range (give-bpf-range bpf)))
    (list (nth 0 range) (nth 1 range))))

(defmethod handle-key-event ((Self bpfcontrolpanel) Char)
  (cond ((equal char #\SPACE) (editor-play/stop (editor self)))
        (t (call-next-method))))


