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
    (print (list "getslot" pgms))
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
    (print pgms)
    (om-make-dialog-item 'fluid-pgm (om-make-point 1 4) (om-make-point 50 20) "untitled" 
                       :range pgms
                       )))

(defmethod update-di-size ((self fluid-pgm) container) 
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))


(defmethod om-dialog-item-action ((self fluid-pgm)) 
  (when (oa::di-action self)
    (funcall (oa::di-action self) self)))


(defmethod set-dialog-item-params ((self fluid-pgm) box args) (print (list "set-dia" self))
  (let* ((boxframe (om-view-container self))
         (newpop (om-make-dialog-item 'fluid-pgm (om-make-point 1 4) (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
                                      "untitled" 
                                      :range (mapcar 'car (fluid-make-presets 0))
                                      ))) 
    
    (when (om-view-container self)
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newpop)
      (om-set-dialog-item-action-function newpop #'(lambda (x) 
                                                     (let ((tuning (om-get-selected-item newpop))
                                                           (port (omNG-box-value (second (inputs self)))))
                                                       (if port
                                                           (print (list "aaaaa"  port tuning))
                                                           (print (list "aaaaa"  port tuning))
                                                         ))))
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
                                                      (port (omNG-box-value (third (inputs self)))))
                                                  (if port
                                                      (propagate-pgm-change port prg)
                                                    (propagate-pgm-change 0 prg)
                                                    )))))


(defmethod propagate-pgm-change ((self number) value) ;self -> port, value -> pgm number
    (fluid-pgm-change value  '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port self))

(defmethod change-program ((self number) value) ;self -> port
    (setf (program (channelctr self)) value)
    (fluid-pgm-change value '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16) :port self))
