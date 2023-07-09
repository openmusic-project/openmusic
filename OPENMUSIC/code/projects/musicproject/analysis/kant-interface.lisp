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

;;;===========================
;;; KANT INTERFACE
;;;===========================

(in-package :om)

;;;============================
;;; KANT
;;; Segments = note marker, until next one (TO DO)
;;; Segment-data = a VOICE


;;In Progress



#|
(setf onsets '(0 819 2208 3347 4153 4889 6514 7597 8986 9653 10778 11319 11833 12764 13667 14208 15056 16056))
(setf endtime (om+ onsets '(1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000)))
 

(nth 
(position-if  #'(lambda (x) (>= x 2000 )) onsets) onsets)

(nth 
 (position-if-not  #'(lambda (x) (<=  6000 x )) endtime :from-end 't) endtime)
|#

; a faire en mieux!
(defun time-krop (l1 l2 onsets offsets)
  (list 
   (nth 
    (position-if  #'(lambda (x) (>= x l1 )) onsets) onsets)
   (nth 
    (position-if-not  #'(lambda (x) (<=  l2 x )) offsets :from-end 't) offsets)))



(defmethod crop-segment ((self time-segment))
  "Crops a time-segment with the first note to the duration of last note falling in the range of the segment"
  (let* ((l1 (t1 self))
         (l2 (t2 self))
         (anal (container-analysis self))
         (obj (analysis-object anal))
         (onsets (lonset obj))
         (offsets (om+ onsets (mapcar 'car (ldur obj))))
         )
(time-krop l1 l2 onsets offsets)))
    
(defmethod create-kant-seg-analysis ((self t)) nil)

(defmethod create-kant-seg-analysis ((self chord-seq))
  (let ((anal (make-instance 'kant-seg)))
    (setf (analysis-object anal) self)
    (if (analysis self)
        (push anal (analysis self))
    (setf (analysis self) (list anal))))) 



(defun add-time-segment (self)         
"self is the  abstract-analysis"
  (when self
  (let ((time-seg (make-instance 'time-segment :t1 0 :t2 1000))
        (panel (panel (editorframe
                       (associated-box (analysis-object self))))))
    (ts-data-window time-seg)
    (add-in-analysis self time-seg)
     (update-panel panel t)
     )))
 

(defmethod add-time-seg ((self chordseqpanel) (anal abstract-analysis))
  (let* ((int (cursor-interval self))
         (time-seg (make-instance 'time-segment :t1 (car int) :t2 (second int))))
    (add-in-analysis anal time-seg)
    (update-panel self t)))


(defmethod get-chords-in-selection ((self chordseqpanel))
  (let* ((int (cursor-interval self))
         (obj (object (om-view-container self)))
         (chords (inside obj))
         (onsets (lonset obj))
         (pos 
          (remove-dup
          (remove nil (list
                       (position-if #'(lambda (x) (and (>= x (car int)) (<= x (second int)))) onsets)
                       (position-if #'(lambda (x) (and (>= x (car int)) (<= x (second int)))) onsets :from-end 't))) '= 1))
         (onset (when pos (nth (car pos) onsets)))
         (offset (when pos (nth (1+ (last-elem pos)) onsets)))
         )
    (list  onset offset (car pos))))

(defmethod make-chord-markers ((self chordseqpanel))
  (let* ((obj (object (om-view-container self)))
         (data (get-chords-in-selection self))
         (marker (when (second data) 
                   (make-instance 'chord-marker 
                                  :tb (car data)
                                  :te (second data)
                                  :chord-id (third data)
                                  ))))
    marker))


(defmethod add-chord-mark ((self chordseqpanel) (anal abstract-analysis))
  (let ((marker (make-chord-markers self)))
    (progn
      (add-in-analysis anal marker)
      (update-panel self)
      (om-invalidate-view (title-bar (om-view-container self)))))) 

; peut-etre enlever!
(defun get-cursormode (self)
  (let* ((ed (editorframe (associated-box self)))
         )
    (when ed
      (let ((panel (panel ed)))
        (if (equal (cursor-mode panel) :interval)
    (print (list "ed" ed panel "yes" (cursor-interval panel)))
          (print (list "ed" ed panel "no" (cursor-mode panel)))     
          )))))




(defmethod ts-data-window ((kdata time-segment))
  (let ((win (om-make-window 'om-window 
                             :position :centered 
                             :size (om-make-point 430 200)
                             :resizable nil
                             ))
        (pane (om-make-view 'om-view
                            :size (om-make-point 400 180)
                            :position (om-make-point 10 10)
                            :bg-color *om-white-color*))
        (panel (editorframe
                (associated-box
                 (analysis-object (container-analysis kdata)))))
        (i 0)
        tbtxt tetxt)

    (om-add-subviews 
     pane
     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 16))
                          (om-make-point 380 40)
                          "Set time for selected segment:"
                          :font *om-default-font2b*)
     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Begin time"
                          :font *om-default-font1*)
     (setf tbtxt (om-make-dialog-item 'edit-numbox 
                                      (om-make-point 140 i)  
                                      (om-make-point 46 18)
                                      (format nil "~D" (t1 kdata)) 
                                      :font *om-default-font1*
                                      :min-val (t1 kdata)
                                      :di-action (om-dialog-item-act item
                                                  (setf (min-val item) 0);necessaire
                                                   (setf (t1 kdata) (value item))
                                                   (update-panel panel t)
                                                   )
                                      :afterfun #'(lambda (item)
                                                    (progn
                                                      
                                                      (setf (t1 kdata) (value item))
                                                      (update-for-subviews-changes panel t)))
                                      ))
     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "End time"
                          :font *om-default-font1*)
     (setf tetxt (om-make-dialog-item 'edit-numbox 
                                      (om-make-point 140 i)  
                                      (om-make-point 46 18)
                                      (format nil "~D" (t2 kdata))
                                      :font *om-default-font1*
                                      :min-val (t2 kdata)
                                      :di-action (om-dialog-item-act item
                                                   (setf (min-val item) 0);necessaire
                                                   (setf (t2 kdata) (value item))
                                                   (update-panel panel t))
                                      :afterfun #'(lambda (item)
                                                    (progn
                                                      (setf (t2 kdata) (value item))
                                                      (update-for-subviews-changes panel t)))
                                      ))
     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Crop"
                          :font *om-default-font1*)
     
     (om-make-view 'om-icon-button 
                   :icon1 "stop" :icon2 "stop-pushed"
                   :position (om-make-point 140 i) :size (om-make-point 26 25)
                   :action (om-dialog-item-act item
                             (declare (ignore item))
                             (let ((vals (crop-segment kdata)))
                               (print (list "vals" vals))
                               (om-set-dialog-item-text tbtxt (write-to-string (car vals)))
                               (om-set-dialog-item-text tetxt (write-to-string(second vals)))    
                               (setf (t1 kdata) (car vals)
                                     (t2 kdata) (second vals))
                               (update-panel panel t)
                               )))


     (om-make-dialog-item 'om-button (om-make-point 200 (incf i 35)) (om-make-point 80 20) "Cancel"
                          :di-action (om-dialog-item-act item 
                                       ;(om-return-from-modal-dialog win nil)
                                       (om-close-window (om-view-window item))))
     (om-make-dialog-item  'om-button (om-make-point 300 i) (om-make-point 80 20) "OK"
                           :di-action (om-dialog-item-act item 
                                        (let ((timebeg (ignore-errors (read-from-string (om-dialog-item-text tbtxt))))
                                              (timeend (ignore-errors (read-from-string (om-dialog-item-text tetxt)))))
                                          (setf (t1 kdata) timebeg
                                                (t2 kdata) timeend)
                                          ;(setf (updateflag kdata) nil)
                                          (om-close-window (om-view-window item))
                                          ;(om-return-from-modal-dialog win t)
                                          )))
     )
    (om-add-subviews win pane)
    ;(om-modal-dialog win)
    win
    ))



(defmethod segment-handle-add-click ((self segment) (analysis kant-seg) panel pos) 
  (print (list "clic" self analysis panel pos))
  (print (list "time" (pixels-to-time panel (om-point-h pos))))
  ;(add-in-analysis analysis (make-instance 'chord-marker
                                           
  ;(om-inspect analysis)
  )

(defmethod handle-add-click-analysis ((self scorepanel) where) 
  (if (and (equal (cursor-mode self) :interval) (om-shift-key-p))
      (let* ((time (cursor-interval self))
            (time-seg (make-instance 'time-segment :t1 (car time) :t2 (second time))))
        (add-in-analysis (analysis (object self)) time-seg))))
            




#|
(print (list "handle" self where 
             (pixels-to-time self (om-point-h where))
             (analysis (object (editor self)))))
|#

#|
  (unless (analysis (object (editor self)))
    (setf (analysis (object (editor self)))
          (list (make-instance 'simple-segmentation))))
  (when (analysis (object (editor self)))
    (kant-seg-p (car (analysis (object (editor self)))))
  (add-in-analysis (car (analysis (object (editor self))))
                   (make-instance 'chord-marker )
)
  (analysis-add-click (car (analysis (object (editor self)))) self where)
  (om-invalidate-view (title-bar (editor self)))
  (update-panel self))
|#



;;;For sequential kant-segements using shift+clic

(defmethod om-click-release-handler ((self time-segment) pos) nil)

(defmethod handle-add-kant-click ((self t)) nil)

(defmethod handle-add-kant-click ((self chordseqpanel)) 
  (let* ((ms (pixel-toms self (om-mouse-position self)))
         (last (loop for i in (analysis (object (om-view-container self)))
                    collect (when i
                              (mapcar #'t2 (analysis-segments i))))))
    (if last
        (let ((time-seg (make-instance 'time-segment :t1 (+ (last-elem (car last)) 1) :t2 ms)))
          (add-in-analysis (car (analysis (object (om-view-container self)))) time-seg))

      (let* ((time-seg (make-instance 'time-segment :t1 0 :t2 ms))
             (kantseg (make-instance 'kant-seg :analysis-segments (list time-seg))))
        (set-object-analysis (object (om-view-container self)) kantseg)
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;faire aussi pour time-segment
(defmethod segment-data-window ((kdata chord-marker))
  (let ((win (om-make-window 'om-dialog :position :centered 
                             :size (om-make-point 430 200)))
        (pane (om-make-view 'om-view
                            :size (om-make-point 400 180)
                            :position (om-make-point 10 10)
                            :bg-color *om-white-color*))
        (i 0)
        tbtxt tetxt)
    
    (om-add-subviews 
     pane
     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 16))
                          (om-make-point 380 40)
                          "Set time for selected segment:"
                          :font *om-default-font2b*)
     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Begin time"
                          :font *om-default-font1*)
     (setf tbtxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i)  (om-make-point 37 13)
                                      (format nil "~D" (tb kdata)) 
                                      :font *om-default-font1*))
     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "End time"
                          :font *om-default-font1*)
     (setf tetxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i)  (om-make-point 37 13)
                                      (format nil "~D" (te kdata))
                                      :font *om-default-font1*))
     (om-make-dialog-item 'om-button (om-make-point 200 (incf i 35)) (om-make-point 80 20) "Cancel"
                          :di-action (om-dialog-item-act item 
                                       (om-return-from-modal-dialog win nil)))
     (om-make-dialog-item  'om-button (om-make-point 300 i) (om-make-point 80 20) "OK"
                           :di-action (om-dialog-item-act item 
                                        (let ((timebeg (ignore-errors (read-from-string (om-dialog-item-text tbtxt))))
                                              (timeend (ignore-errors (read-from-string (om-dialog-item-text tetxt)))))
                                          (setf (tb kdata) timebeg
                                                (te kdata) timeend)
                                          ;(setf (updateflag kdata) nil)
                                          (om-return-from-modal-dialog win t))))
     )
    (om-add-subviews win pane)
    (om-modal-dialog win)))

;(om-command-key-p)
;(om-shift-key-p)
;(om-option-key-p)




(defmethod handle-segment-click ((self abstract-analysis) segment panel pos) 
  (cond 
   ((om-shift-key-p) 
    (progn
    (print (list "resize" segment pos panel))
    (om-click-motion-handler segment pos)
    (om-click-release-handler segment pos)
    (print (list "x-range" 
                 (om-mouse-position
                 (panel (editorframe
                 (associated-box
                 (analysis-object self)))))))
    ))
   ((om-command-key-p) (print (list self segment pos)))
   ((om-option-key-p) (segment-info segment)); faire cela avec 'i' comme info
    (t (print "nothin'"))))

#|
  (segment-data-window segment)
  ;(setf (tb segment) (- (tb segment) 500))
  nil)
|#

(defmethod segment-info ((self segment))
  (if (time-segment-p self)
      (ts-data-window self)
      (segment-data-window self)
    ))





;backdspace delete selected

(defvar *score-analysis-help* nil)
(setf *score-analysis-help* '(("alt+clic" "New Object")
                    ("del" " selected segment")
                    (("d") "Delete Analysis")
                    (("i") "Get Info of selected segment")
                    (("t") "Create Time segment")
                    (("c") "Show Channel Color")
                    (("C") "Change Selection Color")
                    (("o") "Open Selection Internal Editor")
                    ("ud" "Transpose Selection")
                    ("lr" "Change Selection Offset/Duration")
                    ("space" "Play/Stop")))

(defmethod analysis-help-list ((self scorepanel)) (list *score-analysis-help*))



(defun remove-all-analysis (truc)
  (let ((segs (analysis-segments truc)))
    (loop for i in segs
          do (let ((data (segment-data i))) 
               (setf (updateflag data) nil)
               (setf (voice data) nil)
               ))))

(defun remove-selected-analysis (data)
  (setf (updateflag (segment-data data)) nil)
  (setf (voice (segment-data data)) nil))



;from scoreeditor-analysis
(defmethod analysis-handle-key-event ((self ScorePanel) char)
  (let* ((obj (object (om-view-container self)))
         (anal (if (analysis obj)
                   (analysis obj)
                 (create-kant-seg-analysis obj)
                 ))
         (selected (when anal (selected-segments (car anal))))
         )
    (case char
      (:om-key-tab (change-current-analysis self))
      (:om-key-esc (off-analysis-selection self)
       (update-panel self)
       (setf (cursor-interval self) nil)
       (editor-stop (editor self)))
      (#\h (show-help-window (format nil "Commands for ~A Editor [analysis mode]" 
                                     (string-upcase (class-name (class-of (object (editor self)))))) 
                             (analysis-help-list self)))
      (#\n (change-analysis-name self))
      (#\i (segment-info (car selected)))
      (#\t (if (equal (cursor-mode self) :interval)
               (add-time-seg self (car anal))
             (add-time-segment (car anal))))
      (#\c (if (equal (cursor-mode self) :interval)
               (add-chord-mark self (car anal))
             (om-beep "use interval selection mode")))
         
      (#\d (remove-object-analysis obj (car anal)))
      (#\a 
       (progn
         (loop for seg in selected 
               do  (analyse-one-segment (car anal) seg obj))
         (update-panel self)
         (om-invalidate-view (title-bar (om-view-container self)))))
       
      (#\A 
       (progn
         (analyse-segments (car anal) obj)
         (update-panel self)
         (om-invalidate-view (title-bar (om-view-container self)))))
      (#\R (progn
             (remove-all-analysis (car anal))
             (update-panel self)
             (om-invalidate-view (title-bar (om-view-container self)))))
      (#\r (progn 
             (remove-selected-analysis (car selected))
             (update-panel self)
             (om-invalidate-view (title-bar (om-view-container self)))))
   ; (#\x (print selected))
      (#\Space (play-in-analysis self))
    
      (otherwise 
       (when (car (list! (analysis (object (editor self)))))
         (analysis-key-event (car (list! (analysis (object (editor self))))) self char)
         (om-invalidate-view (title-bar (editor self)))
         (update-panel self))))))