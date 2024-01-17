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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; FLUID package
; Author: Karim Haddad
;==============================
; A SET OF INDEPENDANT FLUID INTERFACE BOXES
; 
;==============================

(in-package :om)


;=========================
; FLUID MICRO TUNE
;========================

(defclass! fluid-microtune (om-pop-up-dialog-item fluid-i-box) 
           ((name :initform "fl-microtune" :accessor name)) 

   (:icon 297)
   (:documentation " Sets tuning of the fluidsynth. Second input is the Port input."
))

;; compat
(defclass! fluid-microtune-box (fluid-microtune) ()) 

(defmethod get-slot-in-out-names ((self fluid-microtune))
  (values '("items" "port") 
          '(("1" "1#" "1/2" "1/3" 
             "1/3#" "1/4" "1/5" "1/5#"
             "1/6" "1/7" "1/7#" "1/8" 
             "1/10" "1/12" "1/14" "1/16") nil)
          '("Tunings" "Port")
          '(nil nil)))

(defmethod omng-save ((self fluid-microtune) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fluid-microtune (om-make-point 1 0 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled"
                                   :range ',(om-get-item-list self))))
     (oa::om-set-selected-item-index rep ',(om-get-selected-item-index self))
     rep))


(defmethod get-super-default-value ((type (eql 'fluid-microtune)))
  (om-make-dialog-item 'fluid-microtune (om-make-point 1 4) (om-make-point 50 20) "untitled" 
                       :range '("1" "1#" "1/2" "1/3" 
                                "1/3#" "1/4" "1/5" "1/5#"
                                "1/6" "1/7" "1/7#" "1/8" 
                                "1/10" "1/12" "1/14" "1/16")))

(defmethod update-di-size ((self fluid-microtune) container) 
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))


(defmethod om-dialog-item-action ((self fluid-microtune)) 
  (when (oa::di-action self)
    (funcall (oa::di-action self) self)))


(defmethod set-dialog-item-params ((self fluid-microtune) box args) 
  (let* ((boxframe (om-view-container self))
         (newpop (om-make-dialog-item 'fluid-microtune (om-make-point 1 4) (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
                                      "untitled" 
                                      :range (if (and (pathnamep (car args)) (directoryp (car args)))
                                                 (om-directory (car args))
                                               (car args))))) 
 
    (when (om-view-container self)
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newpop)
      (om-set-dialog-item-action-function newpop #'(lambda (x) 
                                                (let ((tuning (om-get-selected-item newpop))
                                                      (port (omNG-box-value (second (inputs self)))))
                                                  (if port
                                                      (change-tuning port tuning)
                                                    (change-tuning 0 tuning)
                                                    ))))
      (update-di-size newpop boxframe)) 
    newpop))

(defmethod rep-editor ((self fluid-microtune) num) 
  (cond
   ((= num 0) (om-get-selected-item-index self))
   ((= num 1) (om-get-selected-item self))
   (t nil)))

(defmethod (setf value) :after ((value fluid-microtune) (self FLDIntbox)) ; (self OMBoxEditCall)) 
  (om-set-dialog-item-action-function value #'(lambda (x) 
                                                (let ((tuning (om-get-selected-item value))
                                                      (port (omNG-box-value (second (inputs self)))))
                                                  (if port
                                                      (change-tuning port tuning)
                                                    (change-tuning 0 tuning)
                                                    )))))
#|
(defmethod* Play ((self fluid-microtune) &key (player t))
   :initvals '(nil nil) 
   :indoc '("object" "a player designator") 
   :icon 207
   :doc "Plays any OM Musical object.

<player> designates a particular player (t = dispatch automatically) 
"
   (print "HELLLLLLLLLLLLLLLLLLIO!")
   )
|#
;=========================
; FLUID PGM
;========================

(defclass! fluid-pgm (om-pop-up-dialog-item fluid-i-box) 
           ((name :initform "fl-pgm" :accessor name))
   (:icon 297)
   (:documentation " Allocate program change to all channels of a fluidsynth instance. Third input is the Port input."
))

;; compat
(defclass! fluid-pgm-box (fluid-pgm) ()) 


(defmethod get-slot-in-out-names ((self fluid-pgm))
  (let ((pgms (mapcar 'car (fluid-make-presets 0))))
    ;(print (list "getslot" pgms))
    (values '("items" "channel" "port") 
            '(pgms nil nil)
            '("PGM" "Channel" "Port")
            '(nil nil nil))))

(defmethod omng-save ((self fluid-pgm) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fluid-pgm (om-make-point 1 0 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled"
                                   :range ',(om-get-item-list self))))
     (oa::om-set-selected-item-index rep ',(om-get-selected-item-index self))
     rep))


(defmethod get-super-default-value ((type (eql 'fluid-pgm)))
  (let ((pgms (mapcar 'car (fluid-make-presets 0))))
   ;(print pgms)
    (om-make-dialog-item 'fluid-pgm (om-make-point 1 4) (om-make-point 50 20) "untitled" 
                       :range pgms
                       )))

(defmethod update-di-size ((self fluid-pgm) container) 
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))


(defmethod om-dialog-item-action ((self fluid-pgm)) 
  (when (oa::di-action self)
    (funcall (oa::di-action self) self)))


(defmethod set-dialog-item-params ((self fluid-pgm) box args)
  (let* ((boxframe (om-view-container self))
         (newpop (om-make-dialog-item 'fluid-pgm (om-make-point 1 4) (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
                                      "untitled" 
                                      :range (mapcar 'car (fluid-make-presets 0))
                                      ))) 
    
    (when (om-view-container self)
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newpop)
      (om-set-dialog-item-action-function newpop #'(lambda (x)
                                                     (let ((prg (om-get-selected-item-index value))
                                                           (chan (omNG-box-value (second (inputs self))))
                                                           (port (omNG-box-value (third (inputs self)))))
                                                       (if chan
                                                           (fluid-pgm-change prg chan :port port)
                                                         (if port
                                                             (propagate-pgm-change port prg)
                                                           (propagate-pgm-change 0 prg)
                                                           )))))
      (update-di-size newpop boxframe)) 
    newpop))

(defmethod rep-editor ((self fluid-pgm) num) 
  (cond
   ((= num 0) (om-get-selected-item-index self))
   ((= num 1) (om-get-selected-item self))
   (t nil)))

(defmethod (setf value) :after ((value fluid-pgm) (self FLDIntbox))
  (om-set-dialog-item-action-function value #'(lambda (x) 
                                                (let ((prg (om-get-selected-item-index value))
                                                      (chan (omNG-box-value (second (inputs self))))
                                                      (port (omNG-box-value (third (inputs self)))))
                                                  (if chan
                                                      (fluid-pgm-change prg chan :port port)
                                                    (if port
                                                        (propagate-pgm-change port prg)
                                                      (propagate-pgm-change 0 prg)
                                                      ))))))


(defmethod propagate-pgm-change ((self number) value) ;self -> port, value -> pgm number
    (fluid-pgm-change value  '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port self))


;==================
;FLUID-GAIN
;==================

(defclass! fl-gain (om-slider fluid-i-box)  
           ((name :initform "fl-gain" :accessor name))
           (:icon 298)
           (:documentation 
            "port is the fifth input"
            ))

;; compat
(defclass! fl-gain-box (fl-gain) ()) 

(defmethod omng-save ((self fl-gain) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fl-gain (om-make-point 1 1) (om-make-point ,(om-width self) ,(om-height self)) "untitled"
                                   :direction ,(om-get-slider-orientation self)
                                   :range ',(om-get-slider-range self)
                                   :increment 1
                                   :value ,(om-slider-value self))))
     rep))


(defmethod get-slot-in-out-names ((self fl-gain))
   (values '("direction" "range" "increment" "value" "port") 
           '(:horizontal '(0 127) 1 64 nil)
           '("vertical or horizontal" "min and max values" "step" "fl-gain value" "port number")
           '(((0 (("horizontal" :horizontal) ("vertical" :vertical)))) nil nil nil nil)))

(defmethod get-super-default-value ((type (eql 'fl-gain)))
  (om-make-dialog-item 'fl-gain (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled" :range '(0 127) :increment 1 :value 64))


(defmethod update-di-size ((self fl-gain) container)
   (if (equal (om-get-slider-orientation self) :horizontal)
       (progn
         (om-set-view-position self (om-make-point 8 (- (round (h container) 2) 12)))
         (om-set-view-size self (om-make-point (- (w container) 16) 24)))
     (progn
         (om-set-view-position self (om-make-point (- (round (w container) 2) 12) 8))
         (om-set-view-size self (om-make-point 24 (- (h container) 16))))))



(defmethod set-dialog-item-params ((self fl-gain) box args)
  (let* ((boxframe (om-view-container self))
         (newslider (om-make-dialog-item 
                     'fl-gain 
                     (if (equal (car args) :horizontal)
                         (om-make-point 8 (if boxframe (- (round (h boxframe) 2) 12) 20))
                       (om-make-point (if boxframe (- (round (w boxframe) 2) 12) 20) 8))
                     (if (equal (car args) :horizontal)
                         (om-make-point (if boxframe (- (w boxframe) 16) 60) 24)
                       (om-make-point 24 (if boxframe (- (h boxframe) 16) 60)))
                     "untitled"
                     :di-action (om-dialog-item-act item
                                  (let ((val (port (omNG-box-value (fifth (inputs self))))))
                                    (if val
                                        (change-volume val (om-slider-value x))
                                      (change-volume 0 (om-slider-value x))
                                      )))
                     :direction (car args) :range (second args) 
                     :increment (third args) :value (fourth args))))
    
    (when boxframe
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newslider)
      (update-di-size newslider boxframe)
      )
    newslider))

(defmethod rep-editor ((self fl-gain) num)
  (cond
   ((= num 0) (om-get-slider-orientation self))
   ((= num 1) (om-get-slider-range self))
   ((= num 2) (om-slider-increment self))
   ((= num 3) (om-slider-value self))
   (t  (om-dialog-item-action self))))

(defmethod (setf value) :after ((value fl-gain) (self FLDIntbox)) 
  (let ((val (omNG-box-value (fifth (inputs self)))))
    (om-set-dialog-item-action-function value #'(lambda (x) 
                                                  (if val
                                                      (change-volume val (om-slider-value value))
                                                    (change-volume 0 (om-slider-value value))
                                                    )))))



(defmethod change-volume ((self number) value)
  (let ((port self))
  (fluid-gain (/ value 127.0) port)
))


