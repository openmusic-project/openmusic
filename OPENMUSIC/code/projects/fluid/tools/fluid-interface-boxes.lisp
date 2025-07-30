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

(defclass! fl-microtune (om-pop-up-dialog-item fluid-i-box) 
           ((name :initform "fl-microtune" :accessor name)) 

   (:icon 297)
   (:documentation " Sets tuning of the fluidsynth. Second input is the Port input."
))

;; compat
(defclass! fl-microtune-box (fl-microtune) ()) 

(defmethod get-slot-in-out-names ((self fl-microtune))
  (values '("items" "port") 
          '(*edo-names-0* nil)
          '("Tunings" "Port")
          '(nil nil)))

(defmethod omng-save ((self fl-microtune) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fl-microtune (om-make-point 1 0 ) (om-make-point ,(om-width self) ,(om-height self))
                                   "untitled"
                                   :range ',(om-get-item-list self))))
     (om-set-selected-item-index rep ',(om-get-selected-item-index self))
     rep))


(defmethod get-super-default-value ((type (eql 'fl-microtune)))
  (om-make-dialog-item 'fl-microtune (om-make-point 1 4) (om-make-point 50 20) 
                       "untitled"
                       :value (nth 6 *edo-names-0*)
                       :range *edo-names-0*))

(defmethod update-di-size ((self fl-microtune) container)
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 30))) ;24


(defmethod om-dialog-item-action ((self fl-microtune)) 
  (when (oa::di-action self)
    (funcall (oa::di-action self) self)))


(defmethod set-dialog-item-params ((self fl-microtune) box args) 
  (let* ((boxframe (om-view-container self))
         (newpop (om-make-dialog-item 'fl-microtune (om-make-point 1 4) (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
                                      "untitled" 
                                      :range *edo-names-0*))) 
 
    (when (om-view-container self)
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newpop)
      (om-set-dialog-item-action-function newpop #'(lambda (x) 
                                                     (let* ((tuning (om-get-selected-item newpop))
                                                            (pos (position tuning *edo-list* :test 'equal :key #'car))
                                                            (tun-num (second (nth pos *edo-list*)))
                                                            (port (omNG-box-value (second (inputs self)))))
                                                       (if port
                                                      (change-tuning port tun-num)
                                                    (change-tuning 0 tun-num)
                                                    )
                                                  )))
      (update-di-size newpop boxframe)) 
    newpop
    ))

(defmethod rep-editor ((self fl-microtune) num) 
  (cond
   ((= num 0) (om-get-selected-item-index self))
   ((= num 1) (om-get-selected-item self))
   (t nil)))

(defmethod (setf value) :after ((value fl-microtune) (self FLDIntbox)) ; (self OMBoxEditCall)) 
  (om-set-dialog-item-action-function value #'(lambda (x) 
                                                (let* ((tuning (om-get-selected-item value))
                                                       (pos (position tuning *edo-list* :test 'equal :key #'car))
                                                       (tun-num (second (nth pos *edo-list*)))
                                                       (port (omNG-box-value (second (inputs self)))))
                                                  (if port
                                                      (change-tuning port tun-num)
                                                    (change-tuning 0 tun-num)
                                                    )
                                                  ))))


;=========================
; FLUID PGM
;========================

(defclass! fl-pgm (om-pop-up-dialog-item fluid-i-box) 
           ((name :initform "fl-pgm" :accessor name))
   (:icon 297)
   (:documentation " Allocate program change to all channels of a fluidsynth instance. Third input is the Port input."
))

;; compat
(defclass! fl-pgm-box (fl-pgm) ()) 


(defmethod get-slot-in-out-names ((self fl-pgm))
  (let ((pgms (mapcar 'car (fluid-make-presets 0))))
    (values '("items" "channel" "port") 
            '(pgms nil nil)
            '("PGM" "Channel" "Port")
            '(nil nil nil))))

(defmethod omng-save ((self fl-pgm) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fl-pgm (om-make-point 1 0 ) (om-make-point ,(om-width self) ,(om-height self))
                                   "untitled"
                                   :range ',(om-get-item-list self))))
     (om-set-selected-item-index rep ',(om-get-selected-item-index self))
     rep))


(defmethod get-super-default-value ((type (eql 'fl-pgm)))
  (let ((pgms (mapcar 'car (fluid-make-presets 0))))
   ;(print pgms)
    (om-make-dialog-item 'fl-pgm (om-make-point 1 4) (om-make-point 50 20) "untitled" 
                       :range pgms
                       )))

(defmethod update-di-size ((self fl-pgm) container) 
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 30)));24


(defmethod om-dialog-item-action ((self fl-pgm)) 
  (when (oa::di-action self)
    (funcall (oa::di-action self) self)))


(defmethod set-dialog-item-params ((self fl-pgm) box args)
  (let* ((boxframe (om-view-container self))
         (newpop (om-make-dialog-item 'fl-pgm (om-make-point 1 4) (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
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

(defmethod rep-editor ((self fl-pgm) num) 
  (cond
   ((= num 0) (om-get-selected-item-index self))
   ((= num 1) (om-get-selected-item self))
   (t nil)))

(defmethod (setf value) :after ((value fl-pgm) (self FLDIntbox))
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
  (let ((nsynth (get-synth-channel-count 0)))
    (when (numberp nsynth)
      (let ((lst (arithm-ser 1 nsynth 1)))
        (fluid-pgm-change value  lst :port self)))))

(defmethod propagate-pgm-change ((self list) value) ;self -> port, value -> pgm number
  (let ((nsynth (get-synth-channel-count 0)))
        (when (numberp nsynth)
          (let ((lst (arithm-ser 1 nsynth 1)))
            (loop for i in self
                  do (fluid-pgm-change value lst :port i))))))

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

;==================
;FLUID-PAN
;==================

(defclass! fl-pan (om-slider fluid-i-box)  
           ((name :initform "fl-pan" :accessor name))
           (:icon 298)
           (:documentation 
            "port is the fifth input"
            ))

;; compat
(defclass! fl-pan-box (fl-pan) ()) 

(defmethod omng-save ((self fl-pan) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fl-pan (om-make-point 1 1) (om-make-point ,(om-width self) ,(om-height self)) "untitled"
                                   :direction ,(om-get-slider-orientation self)
                                   :range ',(om-get-slider-range self)
                                   :increment 1
                                   :value ,(om-slider-value self))))
     rep))


(defmethod get-slot-in-out-names ((self fl-pan))
   (values '("direction" "range" "increment" "value" "channel" "port") 
           '(:horizontal '(0 127) 1 64 nil nil)
           '("vertical or horizontal" "min and max values" "step" "fl-pan value" "channel(s)" "port number")
           '(((0 (("horizontal" :horizontal) ("vertical" :vertical)))) nil nil nil nil nil)))

(defmethod get-super-default-value ((type (eql 'fl-pan)))
  (om-make-dialog-item 'fl-pan (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled" :range '(0 127) :increment 1 :value 64))


(defmethod update-di-size ((self fl-pan) container)
   (if (equal (om-get-slider-orientation self) :horizontal)
       (progn
         (om-set-view-position self (om-make-point 8 (- (round (h container) 2) 12)))
         (om-set-view-size self (om-make-point (- (w container) 16) 24)))
     (progn
         (om-set-view-position self (om-make-point (- (round (w container) 2) 12) 8))
         (om-set-view-size self (om-make-point 24 (- (h container) 16))))))



(defmethod set-dialog-item-params ((self fl-pan) box args)  
  (let* ((boxframe (om-view-container self))
         (newslider (om-make-dialog-item 
                     'fl-pan 
                     (if (equal (car args) :horizontal)
                         (om-make-point 8 (if boxframe (- (round (h boxframe) 2) 12) 20))
                       (om-make-point (if boxframe (- (round (w boxframe) 2) 12) 20) 8))
                     (if (equal (car args) :horizontal)
                         (om-make-point (if boxframe (- (w boxframe) 16) 60) 24)
                       (om-make-point 24 (if boxframe (- (h boxframe) 16) 60)))
                     "untitled"
                     :di-action (om-dialog-item-act item
                                  (let ((chan (omNG-box-value (fifth (inputs self))))
                                        (port (omNG-box-value (sixth (inputs self)))))
                                    ;(show-data box (om-slider-value value))
                                   
                                    (if port
                                        (change-pan-val val (om-slider-value value) chan port)
                                      (change-pan-val (om-slider-value value) chan 0)
                                      )))
                     :direction (car args) :range (second args) 
                     :increment (third args) :value (fourth args))))
    ;
    (when boxframe 
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newslider)
      (update-di-size newslider boxframe)
      )
    newslider))

(defmethod rep-editor ((self fl-pan) num) 
  (cond
   ((= num 0) (om-get-slider-orientation self))
   ((= num 1) (om-get-slider-range self))
   ((= num 2) (om-slider-increment self))
   ((= num 3) (om-slider-value self))
   (t  (om-dialog-item-action self))))

(defmethod (setf value) :after ((value fl-pan) (self FLDIntbox)) 
  (let ((chan (omNG-box-value (fifth (inputs self))))
        (port (omNG-box-value (sixth (inputs self)))))
    ;(show-data self (om-slider-value value))
    (om-set-dialog-item-action-function value #'(lambda (x) 
                                                  (if port
                                                      (change-pan-val (om-slider-value value) chan port)
                                                    (change-pan-val (om-slider-value value) chan 0)
                                                    )))))

(defmethod change-pan-val ((val number) channels port)
  (if channels
  (fluid-pan val channels port)
    (fluid-pan val *all-chans* port)))



