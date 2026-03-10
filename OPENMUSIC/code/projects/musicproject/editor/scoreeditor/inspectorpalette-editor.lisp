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
;;replaces inspectorpalette.lisp

(in-package :om)

;;; pb de activate / deactivate sur windows...
#-win32 (defmethod score-tools-palettes-p ((editor scoreeditor)) t)

;;;============================
;;; SCORE INSPECTOR :

(defclass score-inspector-window #+macosx(om-window) #-macosx(om-dialog)
              ((inspector :initform nil :initarg :inspector :accessor inspector)))

(defclass score-inspector ()
  ((win :initform nil  :accessor win)
   (winpos :initform nil  :accessor winpos)
   (show :initform nil :accessor show)
   (inspected :initform nil :accessor inspected)
   (fleches :initform nil :accessor fleches)
   (curent-editor :initform nil :accessor current-editor :initarg :current-editor)
   ))

(defvar *scoreinspector* nil)
;(setf *scoreinspector* (make-instance 'score-inspector))
;(defvar *show-scoreinspector* nil)

(defmethod om-window-close-event :around ((self score-inspector-window))
  (setf (winpos *scoreinspector*) (om-view-position self))
  (setf (win *scoreinspector*) nil)
  (setf (show *scoreinspector*) nil)
  (setf (current-editor *scoreinspector*) nil)
  (setf *scoreinspector* nil) 
  (om-close-window (win *scoreinspector*)))



#|
(defmethod open-win-palettes ((pal (eql 'inspector)) editor)
    ;(when (and *scoreinspector* (win *scoreinspector*))
    ;  (om-hide-window (win *scoreinspector*)))
  (get-inspector editor))


(defmethod close-win-palettes ((pal (eql 'inspector)) editor)
  (when (and *scoreinspector* (win *scoreinspector*))
    (let ((show? (show *scoreinspector*)))
      (om-close-window (win *scoreinspector*))
      (setf (show *scoreinspector*) show?)
      )))


  
;; menu call
(defun show-score-inspector (editor) 
  (unless *scoreinspector* (setf *scoreinspector* (make-instance 'score-inspector)))
  (setf (show *scoreinspector*) t)
  (get-inspector editor))

(defun show-score-inspector-enabled ()
  (or (not *scoreinspector*)
      (not (win *scoreinspector*))))

|#


;necessaire
(defmethod get-inspector ((self t)) t)
(defmethod show-inspector ((self t)) t) ;for test reasons

(defmethod show-inspector ((self scorepanel)) 
  (setf *scoreinspector* (make-instance 'score-inspector))
  (let ((win (om-make-window 'score-inspector-window :window-title "Score Inspector"
                             :inspector *scoreinspector*
                             :resizable nil :maximize nil :minimize nil
                             :window-show nil
                             :size (om-make-point 250 254)
                             )))
    
    (setf *scoreinspector* (inspector win))
    (setf (win *scoreinspector*) win)
    (update-selection self 0)
    (setf (current-editor *scoreinspector*) self)
    (push win (attached-editors (om-view-container (current-editor *scoreinspector*))))
    ;(om-show-window win)
    (om-set-view-position win  (om-make-point 500 500))
    ))

(defmethod close-inspector-win ((self score-inspector-window))
  (om-close-window self)
  (setf *scoreinspector* nil)
  )

#|
(defmethod show-inspector-window ((self scorepanel))
  (if (win *scoreinspector*)
      (progn 
        (close-inspector-win (win *scoreinspector*))
        (show-inspector self))
    (show-inspector self)))
|#

(defmethod show-inspector-window ((self scorepanel))
    (show-inspector self))

(defmethod show-inspector-from-menu ((self scorepanel))
  (if (win *scoreinspector*)
      (progn
        (close-inspector-win (win *scoreinspector*))
        (show-inspector self)
        (update-inspector (editor self) 0)
        (om-show-window (win *scoreinspector*)))
    (progn 
      (show-inspector self)
      (update-inspector (editor self) 0)
      (om-show-window (win *scoreinspector*)))
    ))

(defmethod update-selection ((self scorepanel) index)
  (let* ((selection (selection? self))
         (controls (get-inspector-info self (nth index selection) 
                                       index (om-make-point 100 0))))
    (setf (inspected *scoreinspector*) controls)
    ))


(defmethod update-inspector ((self scoreeditor) index)
  (when (and *scoreinspector* (win *scoreinspector*))
    (let* ((panel (panel self))
           (selection 
            (if (edit-cursor panel) (list (edit-cursor panel))
              (selection? panel)))
           (controls (get-inspector-info panel (nth index selection) 
                                         index (om-make-point 100 0))))
      ;(erase-controls (win *scoreinspector*))
      (if controls
          (let ((inspectwin (om-select-window (win *scoreinspector*))))
           ; (erase-controls (win *scoreinspector*))
            (setf (inspected *scoreinspector*) controls)
            ;;; stop les boutons fleches
            ;;;(if (> (length selection) 1)
            ;;;   (add-buttons-fleches (title-bar self)))
            (om-set-window-title inspectwin (format nil "~A Inspector" 
                                                    (string-upcase (string (obj-mode (panel self))))))
            (when (car controls) (om-set-view-size inspectwin (om-make-point 300 (car controls)))) ;205-> 300 width
            (if selection
                (if (> (length selection) 1)
                    (unable-inspector-section inspectwin "(Multiple selection)")
                  (show-inspector-section inspectwin))
              (unable-inspector-section inspectwin "(No selection)")
              )
            (om-set-view-position (win *scoreinspector*)  (om-make-point 500 500));here!
            )
        (when (win *scoreinspector*) (om-hide-window (win *scoreinspector*))))
      (setf *scoreinspector* nil)
      )))


(defmethod add-buttons-fleches ((self score-inspector))
   (setf (fleches self) *bfleches*)
   (mapc #'(lambda (item)
             (om-add-subviews self item)) *bfleches*))

(defmethod erase-controls ((self score-inspector-window))
   (mapc #'(lambda (item)
             (om-remove-subviews self item)) (om-subviews self)) 
         ;(append (when (inspected (inspector self)) (third (inspected (inspector self)))) (fleches (inspector self))))
   (setf (fleches (inspector self)) nil))

(defmethod reset-inspector ((self score-inspector-window))
  (erase-controls self)
  (setf (inspected (inspector self)) nil))

(defmethod show-inspector-section ((self score-inspector-window))
   (mapc #'(lambda (item)
             (om-add-subviews self item )) (third (inspected (inspector self)))))

(defmethod unable-inspector-section ((self score-inspector-window) &optional text)
  (let ((txt (or text "Unabled Inspector")))
    (om-add-subviews self (om-make-dialog-item 'om-static-text (om-make-point 80 10)
                                               (om-make-point 120 20)
                                               txt
                                               :fg-color *om-gray-color*
                                               :font *om-default-font1*))))
  

;==============================================================

(defmethod get-inspector-info ((panel scorepanel) (self t) index p0) 
  (if (or (string-equal (obj-mode panel) 'chord)
          (string-equal (obj-mode panel) 'group))
      (list 50 self nil) ; nil
    (list 50 self nil)))

(defmethod dur-dialogs ((panel voicepanel) (self note) index p0) nil)
(defmethod dur-dialogs ((panel polypanel) (self note) index p0) nil)

(defmethod dur-dialogs ((panel scorepanel) (self note) index p0)
   (list (om-make-dialog-item 'om-static-text (om-make-point 130 26) (om-make-point 40 16)
                                                    "dur" :font *controls-font*)
         (om-make-dialog-item 'numbox (om-make-point 170 28) (om-make-point 50 18) (format nil " ~D" (dur self)) 
                 :font *om-default-font2*
                 :min-val 0 :max-val 10000000
                 :value (dur self)
                 :bg-color *om-white-color*
                 :afterfun #'(lambda (x) 
                               (set-extent-ms self (value x))
                               (update-panel panel)
                               (update-slot-edit panel)))))

(defmethod get-inspector-info ((panel scorepanel) (self note) index p0)
  (list 50 self
        (append (dur-dialogs panel self index p0)
                (list 
                 (om-make-dialog-item 'om-static-text (om-make-point 10 4) (om-make-point 40 20)
                                      "midic" :font *controls-font*)
                 (om-make-dialog-item 'om-static-text (om-make-point 130 4) (om-make-point 40 20)
                                      "vel" :font *controls-font*)
                 (om-make-dialog-item 'om-static-text (om-make-point 10 26) (om-make-point 40 20)
                                      "chan" :font *controls-font*)

                 (om-make-dialog-item 'numbox (om-make-point 55 6) (om-make-point 50 18)
                                      (format nil " ~D" (midic self)) 
                                      :font *om-default-font2*
                                      :min-val 0 :max-val 12700
                                      :value (midic self)
                                      :bg-color *om-white-color*
                                      :afterfun #'(lambda (x) 
                                                    (change-midic self (value x))
                                                    (update-panel panel)
                                                    (update-slot-edit panel)))
                 (om-make-dialog-item 'numbox (om-make-point 170 6) (om-make-point 40 18)
                                           (format nil " ~D" (vel self)) 
                                           :font *om-default-font2*
                                           :value (vel self)
                                           :min-val 0 :max-val 127
                                           :bg-color *om-white-color*
                                           :afterfun #'(lambda (x) 
                                                         (setf (vel self) (value x))
                                                         (update-panel panel)
                                                         (update-slot-edit panel)))
                 (om-make-dialog-item 'numbox (om-make-point 55 28) (om-make-point 25 18)
                                      (format nil " ~D" (chan self)) 
                                      :min-val 1 :max-val 16
                                      :value (chan self)
                                      :font *om-default-font2*
                                      :bg-color *om-white-color*
                                      :afterfun #'(lambda (x) 
                                                    (setf (chan self) (value x))
                                                    (update-slot-edit panel)
                                                    (update-panel panel)))
                 ))))

(defmethod get-inspector-info ((panel scorepanel) (self measure) index p0)
  (let* ((tree (tree self))
         (sign (car tree))
         (hv (car sign)) (lv (second sign))
         (h-sig (om-make-dialog-item 'numbox (om-make-point 130 6) (om-make-point 40 18)
                                     (format nil " ~D" hv)
                                     :font *om-default-font2b*
                                     :bg-color *om-white-color*
                                     :min-val 1 :max-val 1000
                                     :value (car sign)
                                     :afterfun #'(lambda (x) 
                                                   (setf hv (value x))
                                                   (change-signature panel self (list hv lv)))))
         (l-sig (om-make-dialog-item 'numbox (om-make-point 130 28) (om-make-point 40 18)     
                                     (format nil " ~D" lv)
                                     :font *om-default-font2b*
                                     :bg-color *om-white-color*
                                     :min-val 1 :max-val 1000
                                     :value (second sign)
                                     :afterfun #'(lambda (x) 
                                                   (setf lv (value x))
                                                   (change-signature panel self (list hv lv))))))
    (list 54 self
          (list 
           (om-make-dialog-item 'om-static-text (om-make-point 20 6) (om-make-point 100 20)
                                   "Time Signature"
                                   :font *controls-font*)
           h-sig l-sig
           ))
    ))

(defmethod get-inspector-info ((panel scorepanel) (self voice) index p0)
  (list 72 ;height of window
        self
        (list  
         (om-make-dialog-item 'om-static-text (om-make-point 20 4) (om-make-point 80 20)
                              "Tempo" :font *controls-font*)
         (om-make-dialog-item 'edit-numbox (om-make-point 90 6) (om-make-point 30 18) 
                              (format nil " ~D" (tempo-a-la-noire (car (tempo self))))
                              :font *om-default-font2*
                              :value (tempo-a-la-noire (car (tempo self))) :min-val 10 :max-val 1000
                              :bg-color *om-white-color*
                              :afterfun #'(lambda (x) 
                                            (setf (tempo self) (list (value x) (second (tempo self))))
                                            (update-panel panel)))
         (om-make-dialog-item 'om-static-text (om-make-point 20 26) (om-make-point 80 20) 
                              "Channel" :font *controls-font*)
         (om-make-dialog-item 'edit-numbox (om-make-point 90 28) (om-make-point 40 18) 
                              (format nil "~D" (get-first-chan self))
                              :fg-color *om-gray-color*
                              :font *om-default-font2*
                              :value 0 :min-val 1 :max-val *chan-count*
                              :bg-color *om-white-color*
                              :afterfun (om-dialog-item-act item
                                          (set-channel self (read-from-string (om-dialog-item-text item)))
                                          (om-set-fg-color item *om-black-color*)
                                          (update-panel panel)))
               
         (om-make-dialog-item 'om-static-text (om-make-point 20 48) (om-make-point 80 20)
                              "Port" :font *controls-font*)
         (om-make-dialog-item 'edit-numbox (om-make-point 90 50) (om-make-point 30 18) 
                              (format nil "~D" (get-first-port self))
                              :fg-color *om-gray-color*
                              :font *om-default-font2*
                              :value 0 :min-val 0 :max-val 255
                              :bg-color *om-white-color*
                              :afterfun (om-dialog-item-act item
                                          (set-port self (read-from-string (om-dialog-item-text item)))
                                          (om-set-fg-color item *om-black-color*)
                                          (update-panel panel)))
                              (om-make-dialog-item 'om-check-box  (om-make-point 170 2) (om-make-point 80 20)
                                                   " Mute" :font *controls-font* :enable t)
                              )))

  (defmethod get-inspector-info ((panel scorepanel) (self chord-seq) index p0)
    (list 72 self
          (list  
           (om-make-dialog-item 'om-static-text (om-make-point 20 12) (om-make-point 80 20)
                                "Channel" :font *controls-font*)
           (om-make-dialog-item 'om-static-text (om-make-point 20 38) (om-make-point 80 20)
                                "Port" :font *controls-font*)
           (om-make-dialog-item 'edit-numbox (om-make-point 90 14) (om-make-point 30 18)
                                (format nil "~D" (get-first-chan self))
                                :fg-color *om-gray-color*
                                :font *om-default-font2*
                                :value 0 :min-val 1 :max-val *chan-count*
                                :bg-color *om-white-color*
                                :afterfun  (om-dialog-item-act item
                                             (set-channel self (read-from-string (om-dialog-item-text item)))
                                             (om-set-fg-color item *om-black-color*)
                                             (update-panel panel)))
           (om-make-dialog-item 'edit-numbox (om-make-point 90 40) (om-make-point 30 18) 
                                (format nil "~D" (get-first-port self))
                                :fg-color *om-gray-color*
                                :font *om-default-font2*
                                :value 0 :min-val 0 :max-val 255
                                :bg-color *om-white-color*
                                :afterfun (om-dialog-item-act item
                                            (set-port self (read-from-string (om-dialog-item-text item)))
                                            (om-set-fg-color item *om-black-color*)
                                            (update-panel panel)))
               
               
           (om-make-dialog-item 'om-check-box  (om-make-point 170 10) (om-make-point 80 20)
                                " Mute" :font *controls-font* :enable t)
           )))



(defmethod get-first-chan ((self voice))
  (let ((chords (chords self)))
        (car (lchan (car chords)))))

(defmethod get-first-port ((self voice))
  (let ((chords (chords self)))
    (car (lport (car chords)))))

(defmethod get-first-chan ((self chord-seq))
        (caar (lchan self)))

(defmethod get-first-port ((self chord-seq))
  (caar (lport self)))


(defmethod get-inspector-info ((panel scorepanel) (self poly) index p0)
  (let* ((numvoices (length (inside self)))
         (delta 24) (i0 30))
    (list  (+ i0 (* numvoices delta) 4) self
          (append 
           (list 
            (om-make-dialog-item 'om-static-text (om-make-point 10 5) (om-make-point 50 18)
                                 "Voice" :font *om-default-font2b*)
             (om-make-dialog-item 'om-static-text (om-make-point 76 5) (om-make-point 50 18)
                                  "Tempo" :font *controls-font*)
             (om-make-dialog-item 'om-static-text (om-make-point 135 5) (om-make-point 70 18)
                                  "Channel" :font *controls-font*)
             (om-make-dialog-item 'om-static-text (om-make-point 200 5) (om-make-point 70 18)
                                  "Port" :font *controls-font*)
             (om-make-dialog-item 'om-static-text (om-make-point 246 5) (om-make-point 50 18)
                                  "Mute" :font *controls-font* ;:fg-color *om-gray-color*
                                  )
             )
          (loop for voice in (inside self) 
                for i = 0 then (+ i 1) 
                append 
                (list (om-make-dialog-item 'om-static-text (om-make-point 25 (+ i0 (* i delta))) (om-make-point 50 18)
                                           (format nil "~D" (+ i 1)) :font *om-default-font2b*)
                      (om-make-dialog-item 'edit-numbox (om-make-point 82 (+ 2 (+ i0 (* i delta)))) (om-make-point 30 18)
                                                  (format nil " ~D" (tempo-a-la-noire (car (tempo voice))))
                                                  :font *om-default-font2* :bg-color *om-white-color*
                                                  :value (tempo-a-la-noire (car (tempo voice))) :min-val 1 :max-val 1000
                                                  :name i
                                                  :afterfun  (om-dialog-item-act item
                                                                (let ((vx (slot-value item 'capi::name)))
                                                                  (setf (tempo (nth vx (inside self))) (read-from-string (om-dialog-item-text item)))
                                                                  (update-panel panel))))
                      
                      
                      (om-make-dialog-item 'edit-numbox (om-make-point 142 (+ 2 (+ i0 (* i delta)))) (om-make-point 30 18) 
                                           (format nil "~D" (get-first-chan voice))
                                           :font *om-default-font2*
                                           :fg-color *om-gray-color* :bg-color *om-white-color*
                                           :value 1 
                                           :min-val 1 
                                           :max-val *chan-count*
                                           :name i
                                           :afterfun (om-dialog-item-act item
                                                        (let ((vx (slot-value item 'capi::name)))
                                                          (set-channel (nth vx (inside self)) (read-from-string (om-dialog-item-text item)))
                                                          (om-set-fg-color item *om-black-color*)
                                                          (update-panel panel))))
                      
                      (om-make-dialog-item 'edit-numbox (om-make-point 200 (+ 2 (+ i0 (* i delta)))) (om-make-point 30 18) 
                                           (format nil "~D" (get-first-port voice))
                                                  :font *om-default-font2*
                                                  :fg-color *om-gray-color* :bg-color *om-white-color*
                                                  :value 0 :min-val 0 :max-val 255
                                                  :name i
                                                  :afterfun (om-dialog-item-act item
                                                        (let ((vx (slot-value item 'capi::name)))
                                                          (set-port (nth vx (inside self)) (read-from-string (om-dialog-item-text item)))
                                                          (om-set-fg-color item *om-black-color*)
                                                          (update-panel panel))))
                      
                      (om-make-dialog-item 'om-check-box (om-make-point 244 (- (+ i0 (* i delta)) 2)) (om-make-point 20 20) "" 
                                           :font *controls-font* :enable t)
                      
                      ))))))


(defmethod get-inspector-info ((panel scorepanel) (self multi-seq) index p0)
  (let* ((numvoices (length (inside self)))
         (delta 24) (i0 30))
    (list  (+ i0 (* numvoices delta) 4) self
          (append 
           (list 
            (om-make-dialog-item 'om-static-text (om-make-point 10 5) (om-make-point 50 18)
                                 "Voice" :font *om-default-font2b*)
            (om-make-dialog-item 'om-static-text (om-make-point 90 5) (om-make-point 70 18)
                                  "Channel" :font *controls-font*)
             (om-make-dialog-item 'om-static-text (om-make-point 180 5) (om-make-point 70 18)
                                  "Port" :font *controls-font*)
             (om-make-dialog-item 'om-static-text (om-make-point 246 5) (om-make-point 50 18)
                                  "Mute" :font *controls-font* ;:fg-color *om-gray-color*
                                  )
             )
          (loop for voice in (inside self) 
                for i = 0 then (+ i 1) 
                append 
                (list (om-make-dialog-item 'om-static-text (om-make-point 25 (+ i0 (* i delta))) (om-make-point 50 18)
                                           (format nil "~D" (+ i 1)) :font *om-default-font2b*)
                      
                      (om-make-dialog-item 'edit-numbox (om-make-point 100 (+ 2 (+ i0 (* i delta)))) (om-make-point 30 18) 
                                           (format nil "~D" (get-first-chan voice))
                                           :font *om-default-font2*
                                           :fg-color *om-gray-color* :bg-color *om-white-color*
                                           :value 1 
                                           :min-val 1 
                                           :max-val *chan-count*
                                           :name i
                                           :afterfun (om-dialog-item-act item
                                                        (let ((vx (slot-value item 'capi::name)))
                                                          (set-channel (nth vx (inside self)) (read-from-string (om-dialog-item-text item)))
                                                          (om-set-fg-color item *om-black-color*)
                                                          (update-panel panel))))
                      
                      (om-make-dialog-item 'edit-numbox (om-make-point 180 (+ 2 (+ i0 (* i delta)))) (om-make-point 30 18) 
                                           (format nil "~D" (get-first-port voice))
                                                  :font *om-default-font2*
                                                  :fg-color *om-gray-color* :bg-color *om-white-color*
                                                  :value 0 :min-val 0 :max-val 255
                                                  :name i
                                                  :afterfun (om-dialog-item-act item
                                                        (let ((vx (slot-value item 'capi::name)))
                                                          (set-port (nth vx (inside self)) (read-from-string (om-dialog-item-text item)))
                                                          (om-set-fg-color item *om-black-color*)
                                                          (update-panel panel))))
                      
                      (om-make-dialog-item 'om-check-box (om-make-point 244 (- (+ i0 (* i delta)) 2)) (om-make-point 20 20) "" 
                                           :font *controls-font* :enable t)
                      
                      ))))))


;;;======================================
;;; CURSOR

(defmethod get-inspector-info ((panel scorepanel) (self cursor-editor) index p0) 
  (list 50 self 
        (list (om-make-dialog-item 'om-static-text (om-make-point 40 10) (om-make-point 200 20)
                                   (string+ "Adding note: midic = " (integer-to-string (pos-pitch self)))
                                   :font *controls-font* :fg-color *om-dark-gray-color*))))


(defmethod get-inspector-info ((panel voicepanel) (self cursor-editor) index p0) 
   (if (assoc-chord self)
       (call-next-method)
     (rythm-get-inspector panel self index p0)))
  
(defmethod get-inspector-info ((panel polypanel) (self cursor-editor) index p0) 
   (if (assoc-chord self)
       (call-next-method)
     (rythm-get-inspector panel self index p0)))

(defun rythm-get-inspector (panel self index p0)
   (list 100 self
         (list 
          (om-make-dialog-item  'om-static-text (om-make-point 10 10) (om-make-point 90 20)
                                                          "Insert Mode: "
                                                          :font *controls-font*)
          (om-make-dialog-item 'om-radio-button (om-make-point 10 31) (om-make-point 20 20) ""      
                               :di-action (om-dialog-item-act x 
                                            (setf (measure-edition-mode panel) 0)
                                            (om-select-window (window panel)))
                               :radio-button-cluster 'insert-mode
                               :checked-p (= (measure-edition-mode panel) 0))
          (om-make-dialog-item 'om-radio-button (om-make-point 10 52) (om-make-point 20 20) ""
                               :di-action (om-dialog-item-act x 
                                            (setf (measure-edition-mode panel) 2)
                                            (om-select-window (window panel)))                                                
                               :radio-button-cluster 'insert-mode
                               :checked-p (= (measure-edition-mode panel) 2))
          (om-make-dialog-item 'om-radio-button (om-make-point 10 73) (om-make-point 20 20) "" 
                               :di-action (om-dialog-item-act x 
                                            (setf (measure-edition-mode panel) 1)
                                            (om-select-window (window panel))
                                            )
                               :radio-button-cluster 'insert-mode
                               :checked-p (= (measure-edition-mode panel) 1))
          (om-make-view 'picture-view
                        :pict *insertmodes*
                        :position (om-make-point 40 32)
                        :size (om-get-picture-size  *insertmodes*))
                (om-make-dialog-item  'om-static-text (om-make-point 150 10) (om-make-point 90 20)
                                                          "Subdivision:"
                                                          :font *controls-font*)
               (om-make-dialog-item 'numbox (om-make-point 170 30) (om-make-point 26 20) (format nil " ~D" (def-subdiv panel))
                                     :font *om-default-font2*
                                     :bg-color *om-white-color*
                                     :min-val 1
                                     :max-val 16
                                     :value (def-subdiv panel)
                                     :afterfun #'(lambda (x) 
                                                   (setf (def-subdiv panel) (value x))
                                                   (update-panel panel)))))
   )


