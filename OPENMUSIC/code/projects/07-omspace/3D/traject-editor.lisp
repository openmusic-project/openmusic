;OpenMusic : trajectory editor
;
;Copyright (C) 1997-2010 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: J. Bresson

(in-package :om)

;;; EDITOR
(defclass traject-editor (3DEditor) ())

(defmethod get-editor-class ((self 3D-trajectory)) 'traject-editor)


(defmethod special-move-point ((self bpcPanel) (point timedpoint) i)
  (let* ((dec (decimals (get-bpf-obj self)))
         (xsize (max 50 (* 10 dec)))
         (mydialog (om-make-window 'om-dialog
                                   :size (om-make-point (max 180 (+ 100 (* 12 dec))) 120)
                                   :window-title ""
                                   :position (om-add-points (om-view-position (window self)) (om-mouse-position self))))
         (xed (om-make-dialog-item 'om-editable-text (om-make-point 30 5) (om-make-point xsize 10)
                                   (if (zerop dec) (format () "~D" (om-point-h point))
                                       (format () (format nil "~S,~DF" '~ dec) 
                                               (/ (om-point-h point) (expt 10.0 dec))))
                                   ))
         (yed (om-make-dialog-item 'om-editable-text (om-make-point 30 35) (om-make-point xsize 10)
                                   (if (zerop dec) (format () "~D" (om-point-v point))
                                       (format () (format nil "~S,~DF" '~ dec) 
                                               (/ (om-point-v point) (expt 10.0 dec))))
                                   ))
         (time-ed (om-make-dialog-item 'om-editable-text (om-make-point 30 65) (om-make-point xsize 10)
                                       (format () "~D" (timedpoint-time point))
                                   ))
         )
    (om-add-subviews mydialog 
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 5 5) (om-make-point 20 20) (x-label self) 
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 35)  (om-make-point 20 20) (y-label self) 
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 65) (om-make-point 20 20) "time" 
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     xed yed time-ed
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 5) (om-make-point 70 20) "Cancel"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :default-button nil)
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 36) (om-make-point 70 20) "OK"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (let ((tvalue (ignore-errors (read-from-string (om-dialog-item-text time-ed)))))
                                                         (if 
                                                             (move-timedpoints-in-bpf 
                                                              (currentbpf self) 
                                                              (list (list point i)) 
                                                              (- (round (* (read-from-string (om-dialog-item-text xed)) (expt 10.0 dec))) (om-point-h point)) 
                                                              (- (round (* (read-from-string (om-dialog-item-text yed)) (expt 10.0 dec))) (om-point-v point))
                                                              (if (numberp tvalue) tvalue nil))
                                                                  
                                                         (do-after-move self)

                                                         (om-beep-msg "Ilegal move"))
                                                         (om-return-from-modal-dialog mydialog ())))
                                          :default-button t))
    (om-modal-dialog mydialog)))


(defmethod move-timedpoints-in-bpf ((self bpc) points deltax deltay time)
  (let ((legalmove t))
    (loop for point in points do
          (let* ((pos (second point))
                 (tmin (or (list-max (remove nil (mapcar 'get-point-time (subseq (point-list self) 0 pos))))
                           0.0))
                 (tmax (list-min (remove nil (mapcar 'get-point-time (subseq (point-list self) (1+ pos)))))))          
            (if (or (null time)
                    (and (or (equal pos 0) (> time tmin)) 
                         (or (null tmax) (< time tmax))))
                (setf (nth pos (point-list self))
                      (make-timedpoint :point (om-make-point (+ (om-point-h (car point)) deltax)
                                                             (+ (om-point-v (car point)) deltay))
                                       :time time))
              (setf legalmove nil))))
    legalmove))


(defmethod init-tmp-objs ((self traject-editor))
  (call-next-method)
  (set-times-to-tmpobjs self))  

(defmethod editor-change-precision ((self traject-editor) newvalue)
  (call-next-method)
  (set-times-to-tmpobjs self))

(defun set-times-to-tmpobjs (traject-editor)
  (let ((curr3dc (if (multibpf? traject-editor)
                     (if (selected-component traject-editor) (nth (selected-component traject-editor) (bpf-list (object traject-editor)))
                       (car (bpf-list (object traject-editor))))
                   (object traject-editor))))
    (mapcar #'(lambda (tobj)
                (setf (point-list tobj)
                      (loop for pt in (point-list tobj) 
                            for trpoint in (point-list curr3Dc)
                            collect (make-timedpoint 
                                     :point pt
                                     :time (timedpoint-time trpoint))))
                ) (tmpview-objs traject-editor))
    ))

(defmethod report-bpfmodif ((self traject-editor) &key xpts ypts zpts times)
  (let ((3Dobj (if (multibpf? self) (nth (selected-component self) (bpf-list (object self)))
                  (object self))))
    (unless xpts (setf xpts (x-points 3Dobj)))
    (unless ypts (setf ypts (y-points 3Dobj)))
    (unless zpts (setf zpts (z-points 3Dobj)))
    (let ((new-bpf (traject-from-list xpts ypts zpts times 
                                      (type-of 3Dobj) (decimals 3Dobj)
                                      (sample-params 3Dobj) (interpol-mode 3Dobj)
                                       )))
      (cons-bpf 3Dobj (point-list new-bpf))
      ;(setf (traject-points 3Dobj) (traject-points new-bpf))
      )
    (init-tmp-objs self)
    (update-editor-contents self)))


(defmethod more-bpc-draw ((self internalbpcpanel) point pos index)
  (call-next-method)
  (when (and (show-time self) (get-point-time point))
    (om-with-fg-color self *om-red2-color*
    (om-draw-string (+ (om-point-h pos) -6) (+ (om-point-v pos) 14) (format nil "~d" (get-point-time point))))))




(defmethod initialize-instance :after ((self traject-editor) &rest l)
  (declare (ignore l))

  (om-add-subviews (ctrlp self)


                   (om-make-dialog-item 'om-static-text (om-make-point 8 380) (om-make-point 70 40)
                                        "Sample Rate"
                                        :font *controls-font*
                                        :fg-color *om-black-color*)
                   (om-make-dialog-item 'om-editable-text (om-make-point 70 385) (om-make-point 40 20) 
                                        (format nil " ~a" (sample-params (object self)))
                                        :-action nil
                                        :font *controls-font*
                                        :bg-color *om-white-color*
                                        :modify-action #'(lambda (item)
                                                           (let ((val (ignore-errors (read-from-string (om-dialog-item-text item)))))  
                                                             (when (or (numberp val) (null val) (and (listp val) (list-subtypep val 'number)))
                                                               (setf (sample-params (object self)) val))))
                                        )
                   
                   (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 8 420) (om-make-point 100 20) ""
                                        :range '("points (constant time)" "distance (constant speed)")
                                        :value (if (equal (interpol-mode (object self)) 'dist) 
                                                   "distance (constant speed)" "points (constant time)")
                                        :di-action (om-dialog-item-act item
                                                      (setf (interpol-mode (object self))
                                                            (if (= (om-get-selected-item-index item) 1) 'dist 'points))))
                   
                   ))



(defmethod get-help-list ((self  3DEditor))
  (remove nil 
          (list '((#+cocoa cmd+clic #-cocoa ctrl+clic "Add point / Draw")
                  (lrud "Move selected points")
                  (del "Delete selected points")
                  (("c") "Change curve color")
                  (("b") "Change 3D background color")
                  (("p") "Show point indices")
                  (("t") "Show point times")
                  )
                (when (multibpf? self)
                  '((tab "Change current curve")
                    (("n") "Change curve name")
                    )))))

 
