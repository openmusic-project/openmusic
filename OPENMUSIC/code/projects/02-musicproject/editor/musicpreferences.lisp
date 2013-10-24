;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
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
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)



;;;;=================================================
;;;;MUSIC PREFERENCES MODULE 
;;;;=================================================

;;;--------
(defvar *page-reglages* '(1 15 2 2 1))
; (margenizq  ,  ancho ,  separation systemas , separation entre staffs, margen de arriba)
; Non editable 

(defvar *music-fontsize* 24)
(defvar *default-satff* 'g)
(defvar *cur-dynamic-list* '(-1 20 40 55 60 85 100 115 127))

(defmethod put-preferences ((iconID (eql :score)))
   (let ((modulepref (find-pref-module iconID)))
     (setf *global-midi-approx* (get-pref modulepref :approx))
     (setf *music-fontsize* (get-pref modulepref :fontsize))
     (setf *default-satff* (get-pref modulepref :staff))
     (setf *system-color* (or (get-pref modulepref :sys-color) *om-black-color*))
     (setf *select-color* (or (get-pref modulepref :select-color) *om-gray-color*))
     (setf *cur-dynamic-list* (or (get-pref modulepref :dyn-list) '(-1 20 40 55 60 85 100 115 127)))
     (when *om-tonalite* (put-tonal-prefs (get-pref modulepref :tonal-options)))
     ;(setf *current-1/2-scale*  (or (nth 8 (defvals modulepref)) *2-tone-chromatic-scale*))
     ;(setf *current-1/4-scale* (or (nth 9 (defvals modulepref)) *4-tone-chromatic-scale*) )     
     ;(setf *current-1/8-scale* (or (nth 10 (defvals modulepref)) *8-tone-chromatic-scale*)  ) 
     (setf *diapason-freq* (get-pref modulepref :diapason))
     ))


;(defmethod save-pref ((iconID (eql 225)) item)
;   (list 225 `(list ,.(append (loop for i from 0 to 7 collect (omng-save (nth i (defvals item))))
;                     (list  (omng-save *current-1/2-scale*)  (omng-save *current-1/4-scale*)
;                            (omng-save *current-1/8-scale*))))))

(defmethod get-def-vals ((iconID (eql :score)))
   (list :approx 2 :fontsize 24 :staff 'g :sys-color *om-black-color* :select-color *om-gray-color* 
         :dyn-list '(-1 20 40 55 60 85 100 115 127)
         :tonal-options (when *om-tonalite* (tonal-defaults))
         :diapason 440.0))

; *2-tone-chromatic-scale* *4-tone-chromatic-scale* *8-tone-chromatic-scale*))

(omg-defclass change-color-dialog-item (om-static-text) 
   ((object :initform 0 :initarg :object :accessor object)
    (i :initform 0 :initarg :i :accessor i)))

(defmethod om-dialog-item-action ((item change-color-dialog-item))
   (let ((newco (om-choose-color-dialog :color (get-pref (object item) (i item)))))
     (when newco
       (set-pref (object item) (i item) newco)
       (om-set-fg-color item newco)
       (om-invalidate-view item))))

(defmethod om-component-border ((self change-color-dialog-item)) :line)

(defmethod om-view-click-handler ((self change-color-dialog-item) where)
  (declare (ignore where))
  (om-dialog-item-action self))

(defmethod make-new-pref-scroll ((num (eql :score)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "Score"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 66 0)
                                 :scrollbars :v 
                                 :bg-color *om-light-gray-color*
                                 :retain-scrollbars t))
        (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2)))
    (om-add-subviews thescroll

                     (om-make-dialog-item 'om-static-text (om-make-point 20 15) (om-make-point 200 30) "Score Editors"
                                          :font *om-default-font3b*)

                     (om-make-dialog-item 'om-static-text  (om-make-point 20 45) (om-make-point 150 20) "Default approx."
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-radio-button (om-make-point 140 45) (om-make-point 80 20) "1/2 tone" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (set-pref modulepref :approx 2))
                                          :checked-p (= (get-pref modulepref :approx) 2)  
                                          :radio-button-cluster 'approx
                                          :font *om-default-font2*)
                     
                     (om-make-dialog-item 'om-radio-button (om-make-point 220 45) (om-make-point 80 20) "1/4 tone" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (set-pref modulepref :approx 4))
                                          :checked-p (= (get-pref modulepref :approx) 4)  
                                          :radio-button-cluster 'approx
                                          :font *om-default-font2*)
                     
                     (om-make-dialog-item 'om-radio-button (om-make-point 300 45) (om-make-point 80 20) "1/8 tone" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (set-pref modulepref :approx 8))
                                          :checked-p (= (get-pref modulepref :approx) 8)  
                                          :radio-button-cluster 'approx
                                          :font *om-default-font2*)
                     
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 20 75) (om-make-point 120 20) "Music Font Size"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 160 75) 
                                       (om-make-point 80 20)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (set-pref modulepref :fontsize
                                                              (cadr (nth (om-get-selected-item-index item) *mus-font-size*))))
                                       :font *controls-font* 
                                       :range (loop for item in *mus-font-size* collect (car item)) 
                                       :value (integer-to-string (get-pref modulepref :fontsize))
                                       )
                     
                     
                     
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 20 105) (om-make-point 120 20) "Staff System"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item  'om-pop-up-dialog-item
                                           (om-make-point 160 105)
                                           (om-make-point 80 20)
                                           ""
                                           :range (loop for item in *all-satff-om* collect (string item))
					   :font *om-default-font2*
                                           :value (string (get-pref modulepref :staff))
                                           :di-action (om-dialog-item-act item
                                                         (set-pref modulepref :staff 
                                                                   (nth (om-get-selected-item-index item) *all-satff-om*)))
                                           )
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 140) (om-make-point 80 20) "Staff Colors"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item 'change-color-dialog-item  (om-make-point 160 140) (om-make-point 80 22) " Normal"
                                          :fg-color (get-pref modulepref :sys-color)
                                          :font *controls-font*
                                          :bg-color *om-white-color*
                                          :object  modulepref 
                                          :i :sys-color)
         
                     (om-make-dialog-item 'change-color-dialog-item  (om-make-point 260 140) (om-make-point 80 22) " Selected"
                                          :fg-color (get-pref modulepref :select-color)
                                          :font *controls-font*
                                          :bg-color *om-white-color*
                                          :object  modulepref 
                                          :i :select-color)


                     (om-make-dialog-item 'om-static-text  (om-make-point 20 210) (om-make-point 150 20) "Diapason"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 235) (om-make-point 350 22) 
                                              "(Frequency of the A4, used for freq-MIDI conversions)"
                                              :font *om-default-font1*)
                     
                     (om-make-dialog-item 'om-editable-text 
                                              (om-make-point 160 210)
                                              (om-make-point 60 13)
                                              (format nil "~D" (get-pref modulepref :diapason)) 
                                              :modify-action (om-dialog-item-act item
                                                              (let ((text (om-dialog-item-text item))
                                                                    number)
                                                                (unless (string= "" text)
                                                                  (setf number (ignore-errors (read-from-string text)))
                                                                  (when (numberp number)
                                                                    (set-pref modulepref :diapason number))
                                                                  )))
                                              :font *om-default-font2*)

                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 30) 45) (om-make-point 80 20) "Dynamics"
                                          :font *controls-font*)
                     (om-make-view 'dynamics-view :position (om-make-point (+ l2 30) 70) :size (om-make-point 140 160) 
                                          :object modulepref :bg-color *om-white-color*)
                     )
    
    (when *om-tonalite*
      (om-add-subviews thescroll 
                       (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 190) 45) (om-make-point 120 20) "Tonal Display"
                                            :font *controls-font*)
                       (om-make-view 'tonaloptions-view :position (om-make-point (+ l2 190) 70) :size (om-make-point 150 140) 
                                     :object modulepref)
                       ))

    thescroll))

;=======================================================
;Dynamic table
;======================================================



(omg-defclass dynamics-view (om-view) 
  ((object :initform nil :initarg :object :accessor object)))

(defmethod om-component-border ((self dynamics-view)) :line)

(defmethod om-draw-contents ((self dynamics-view))
  (call-next-method)
  (let ((list (get-pref (object self) :dyn-list)))
    (om-with-focused-view self
      (om-with-font (om-make-music-font *extras-font* 16)
                    (loop for i from  0 to (- (length *cur-dynamic-chars*) 1) do
                          (om-draw-string 6 (+ 14 (* i 20)) (nth i *cur-dynamic-chars*))))
      (om-with-font *om-default-font2*
                    (loop for dyn in (butlast list)
                          for i = 0 then (+ 1 i) do
                          (om-draw-string  40 (+ 17 (* i 20)) (string+ (format () "~D" (+ dyn 1)) " to ")))))
    
    ))
  
(defmethod initialize-instance :after ((self dynamics-view) &rest initargs)
  (let ((pos (position :object initargs)) object)
    (when pos
      (setf object (nth (+ pos 1) initargs))
      (loop for diain in (butlast (cdr (get-pref object :dyn-list)))
            for i = 0 then (+ 1 i)  do
            (om-add-subviews self
                             (om-make-dialog-item 'numbox (om-make-point 84 (+ 1 (* i 20))) (om-make-point 36 19) (format ()  " ~D" diain)
                                                  :value diain
                                                  :bg-color *om-white-color*
                                                  :min-val 1
                                                  :max-val 127
                                                  :name i
                                                  :enabled t
                                                  :afterfun #'(lambda (item)
                                                                (let ((newnum (value item))
                                                                      (i (+ 1 (om-get-view-name item))))
                                                                  (if (and (integerp newnum) (< i (- (length (get-pref object :dyn-list)) 1))
                                                                           (> newnum (nth (- i 1) (get-pref object :dyn-list)))
                                                                           (< newnum (nth (+ i 1) (get-pref object :dyn-list))))
                                                                      (let ((newlist (get-pref object :dyn-list)))
                                                                          (setf (nth i newlist) newnum)
                                                                          (set-pref object :dyn-list newlist)
                                                                          (om-invalidate-view self t))
                                                                    (progn
                                                                      (set-value item (nth i (get-pref object :dyn-list)))
                                                                      (om-beep)))))
                                                  :font *om-default-font2*)))
      ;;; last value (127) is not editable
      (om-add-subviews self
                             (om-make-dialog-item 'om-static-text 
                                                  (om-make-point 84 (+ 1 (* 7 20))) 
                                                  (om-make-point 36 19) 
                                                  (format ()  " ~D" 127)
                                                  :border :line
                                                  :bg-color *om-white-color*
                                                  :font *om-default-font2*))
      )))





;;;;=================================================
;;;; QUANTIFY PREFERENCES MODULE  icon 252
;;;;=================================================

;;; PREF 252 =

(defvar *quantify-def-params* '(60 (4 4) 8  0 nil 0.5))
(defvar *global-deltachords* 100)

(defmethod get-def-vals ((iconID (eql :conversion)))
   (list :delta-chords 100 :quantify '(60 (4 4) 8 0 nil 0.5)))

(defun check-deltachords (delta)
  (let ((defval (nth (1+ (position :delta-chords (get-def-vals :conversion))) (get-def-vals :conversion))))
    (if (and (integerp delta) (>= delta 0) (<= delta 100000))
        delta
      (progn 
        (om-beep-msg "Wrong value for DELTA-CHORDS! Restoring default.")
        defval))))
  
(defun check-quanti-par (paramlist)
  (let ((deflist (nth (1+ (position :quantify (get-def-vals :conversion))) (get-def-vals :conversion))))
    (list (if (or (integerp (nth 0 paramlist)) (listp (nth 0 paramlist)))
              (nth 0 paramlist) 
            (progn (om-beep-msg "Wrong value for TEMPO! Restoring default.") 
              (nth 0 deflist)))
          (if (consp (nth 1 paramlist))
              (nth 1 paramlist) 
            (progn (om-beep-msg "Wrong value for MEASURES! Restoring default.") 
              (nth 1 deflist)))
          (if (numberp (nth 2 paramlist))
              (nth 2 paramlist)
            (progn (om-beep-msg "Wrong value for MAX-DIV! Restoring default.") 
              (nth 2 deflist)))
          (if (numberp (nth 3 paramlist))
              (nth 3 paramlist) 
            (progn (om-beep-msg "Wrong value for OFFSET! Restoring default.") 
              (nth 3 deflist)))
          (if (or (integerp (nth 4 paramlist)) (listp (nth 4 paramlist)))
              (nth 4 paramlist) 
            (progn (om-beep-msg "Wrong value for FORBIDDEN-DIV! Restoring default.") 
              (nth 4 deflist)))
          (if (numberp (nth 5 paramlist))
              (nth 5 paramlist) 
            (progn (om-beep-msg "Wrong value for PRECISION! Restoring default.") 
              (nth 5 deflist))))))

; (get-def-vals 252)

(defun set-quantipar (module param value)
  (let ((list (get-pref module :quantify)))
    (setf (nth param list) value)
    (set-pref module :quantify list)))

(defun get-quantipar (module param)
  (nth param (get-pref module :quantify)))

    
(defmethod put-preferences ((iconID (eql :conversion)))
  (let* ((modulepref (find-pref-module iconID))
         (delta (get-pref modulepref :delta-chords))
         (quantparams (get-pref modulepref :quantify)))
    
    (set-pref modulepref :quantify (check-quanti-par quantparams))
    (set-pref modulepref :delta-chords (check-deltachords delta))
    
    (setf *global-deltachords* (get-pref modulepref :delta-chords))
    (setf *quantify-def-params* (get-pref modulepref :quantify))
    ))

(defmethod make-new-pref-scroll ((num (eql :conversion)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :name "Quantification" 
                                 :pref-id num
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 66 0)
                                 :scrollbars :v 
                                 :bg-color *om-light-gray-color*
                                 :retain-scrollbars t))
        (i 0))
    (om-add-subviews thescroll

                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 15)) (om-make-point 330 30) "Quantification / Conversion"
                                          :font *om-default-font3b*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 50)) (om-make-point 120 22) "Delta Chords (ms):"
                     :font *controls-font*)
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 20)) (om-make-point 350 22) "(Maximum interval for grouping notes in a same chord)"
                     :font *om-default-font1*)
                     
                     (om-make-dialog-item 'om-editable-text (om-make-point 340 (- i 10)) (om-make-point 45 13)
                                          (format nil "~D" (get-pref modulepref :delta-chords)) 
                                          :modify-action (om-dialog-item-act item
                                                           (let* ((text (om-dialog-item-text item))
                                                                  number)
                                                            (unless (string= "" text)
                                                              (setq number (ignore-errors (read-from-string text)))
                                                              (when number 
                                                                (set-pref modulepref :delta-chords number)))))
                                          :font *om-default-font2*)

                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 40)) (om-make-point 120 20) "Quantify:"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 20)) (om-make-point 350 20) "(Default OM quantification parameters)"
                                          :font *om-default-font1*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Tempi"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 150 i)  (om-make-point 37 13)
                                          (format nil "~D" (get-quantipar modulepref 0)) 
                                          :modify-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (ignore-errors (read-from-string text)))
                                                              (when number 
                                                                  (set-quantipar modulepref 0 number)
                                                                ))))
                                          :font *om-default-font2*)

                     (om-make-dialog-item 'om-static-text  (om-make-point 230 i) (om-make-point 120 20) "Forbidden Div."
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 37 13)
                                          (format nil "~D" (get-quantipar modulepref 4)) 
                                          :modify-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (ignore-errors (read-from-string text)))
                                                              (when number
                                                                  (set-quantipar modulepref 4 number)
                                                                ))))
                                          :font *om-default-font2*)

                         (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Measure"
                                          :font *controls-font*)
                         (om-make-dialog-item 'om-editable-text (om-make-point 150 i)  (om-make-point 37 13)
                                              (format nil "~D" (get-quantipar modulepref 1)) 
                                          :modify-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (ignore-errors (read-from-string text)))
                                                              (if number
                                                                  (set-quantipar modulepref 1 number)
                                                               ))))
                                          :font *om-default-font2*)
                         
                         (om-make-dialog-item 'om-static-text  (om-make-point 230 i) (om-make-point 120 20) "Precision"
                                              :font *controls-font*)
                         (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 37 13)
                                              (format nil "~D" (get-quantipar modulepref 5)) 
                                          :modify-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (ignore-errors (read-from-string text)))
                                                              (if number
                                                                  (set-quantipar modulepref 5 number)
                                                                ))))
                                          :font *om-default-font2*)

                         (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Max. Division"
                                          :font *controls-font*)
                         (om-make-dialog-item 'om-editable-text (om-make-point 150 i) (om-make-point 37 13)
                                              (format nil "~D" (get-quantipar modulepref 2)) 
                                          :modify-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (ignore-errors (read-from-string text)))
                                                              (if number
                                                                  (set-quantipar modulepref 2 number)
                                                                ))))
                                          :font *om-default-font2*)


                         (om-make-dialog-item 'om-static-text  (om-make-point 230 i) (om-make-point 120 20) "Offset"
                                          :font *controls-font*)
                         (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 37 13)
                                              (format nil "~D" (get-quantipar modulepref 3)) 
                                          :modify-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (ignore-errors (read-from-string text)))
                                                              (if number
                                                                  (set-quantipar modulepref 3 number)
                                                                ))))
                                          :font *om-default-font2*)
                         
                         )
    thescroll))


            

;;;==== ADD MIDI AND MUSIC PREFERENCE MODULES ========
(defun add-music-preferences ()
  (push-pref-module (list :score (get-def-vals :score)))
  (push-pref-module (list :conversion (get-def-vals :conversion))))


(add-init-user-func 'add-music-preferences)


