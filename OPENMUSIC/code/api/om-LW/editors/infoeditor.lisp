;;===========================================================================
;LW Score Editors 
;Interface tools for score editing and inspection 
;;===========================================================================

;===========================================================================
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; See file LICENSE for further informations on licensing terms.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Author: Karim Haddad
;;===========================================================================

;;===========================================================================
;DocFile
;This file loads the LW Score Editors
;;===========================================================================


(in-package :om-edit)


(defparameter info-x 375)
(defparameter info-y 325)
(defparameter info-width 415)
(defparameter info-height 315)


(defclass om-obj-info (capi::interface)
  ((ep :initform nil :accessor ep)
   (panes :initform nil :accessor panes)
   (sel :initform nil :accessor sel)
   (intfunc :initform t :accessor intfunc)
   (ompanel :initform t :accessor ompanel)
   (info :initform nil :accessor info)
   )
  (:default-initargs
   :title "Self info"
   :best-x info-x
   :best-y info-y
   :best-width info-width
   :best-height info-height
   :color-mode :aqua
   :layout (make-instance 'capi:grid-layout
                          :right-extend :bottom-extend
                          :y-adjust :center
                          :columns 1
                          )
   ))


(defvar *obj-info* 'om-obj-info)
(defvar *info-pack* nil)
(defvar *def-objinfo-font* 
  #+linux(gp::make-font-description :family "Liberation Mono" :size 11)
  #-linux(gp::make-font-description :family "Consolas" :size 11))



;chord-seq
(defun open-chordseq-info (objs)
 (setf info-x (+ info-x 15)
       info-y (+ info-y 15))
  (setf win (make-instance *obj-info*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :background (color:make-rgb  0.772 0.855 0.788) ;*azulito*
                           :display-state :normal
                           :title "Score Inpsector" ;(format nil "~S INFO" (car *info-pack*)) 
                           ))
  (setf (ep win) win)
  (setf (sel win) objs)       
  (setf pane1 (make-instance 'capi::title-pane
                               :font (gp::make-font-description :size 12)
                               :text (car *info-pack*)
                               ;:background :grey
                               :visible-min-height '(character 2)
                               :visible-max-width nil)) ;object
  (setf pane1b (make-instance 'capi::title-pane
                        ;  :font (gp::make-font-description :size 12)
                                :text (nth 1 *info-pack*)
                                :visible-min-height '(character 2)
                                :visible-max-width nil));pos if
  (setf pane2 (make-instance 'capi::display-pane
                               :font *def-objinfo-font*
                               :text (nth 2 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;selection
  (setf pane2b (make-instance 'capi::display-pane
                               :font *def-objinfo-font*
                               :text (nth 3 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;duration

  (setf pane3 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 4 *info-pack*)
                               :change-callback 'update-callback
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;midics
                
  (setf pane4 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 5 *info-pack*) 
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;vels
  (setf pane5 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 6 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));durs
  (setf pane6 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 7 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));chans
  (setf pane7 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 8 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));offs
  (setf pane8 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 9 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));ports
  (setf button1 (make-instance 'capi::push-button
                                 :text "Set"
                                 :data *info-pack* ;:push-button
                                 :callback-type :data-interface
                                 :selection-callback 'button-set-info-callback))

  (setf button2 (make-instance 'capi::push-button
                                 :text "Reset"
                                 :data *info-pack* 
                                 :callback-type :data-interface
                                 :selection-callback 'button-reset-callback))
    
  (setf button3 (make-instance 'capi::push-button
                                 :text "Close"
                                 :data *info-pack* 
                                 :callback-type :data-interface
                                 :selection-callback 'close-info-callback))
    
  
  (setf row1 (make-instance 'capi::row-layout
                           :y-adjust :center
                           :columns 2 ;1
                           :description (list pane1)))
  (setf row1b (make-instance 'capi::row-layout
                          :y-adjust :center
                          :columns 1
                          :description (list pane1 pane1b)))

  (setf row2 (make-instance 'capi::grid-layout
                         :y-adjust :center
                         :columns 2
                         :description (list "SELECTION:" pane2
                                            "DURATION:" pane2b)))
  (setf row3 (make-instance 'capi::grid-layout
                         :y-adjust :center
                         :columns 2
                         :description (list "MIDIC:" pane3
                                            "VELOCITY:" pane4
                                            "DURATION:" pane5
                                            "CHANNEL:" pane6)))
    
  (setf row4 (make-instance 'capi::grid-layout
                         :y-adjust :center
                         :columns 2
                         :description (list "MIDICS:" pane3
                                            "VELOCITIES:" pane4
                                            "DURATIONS:" pane5
                                            "CHANNELS:" pane6
                                            "OFFSETS:" pane7
                                            "PORTS:" pane8)))
  (setf button-row1 (make-instance 'capi::row-layout
                         :y-adjust :center
                         :description (list button1 button2 button3)))
  (setf button-row2 (make-instance 'capi::row-layout
                                   :y-adjust :center
                                   :description (list button3)))


    (cond ((equal (car *info-pack*) "NOTE")
           (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1 row2 row3 button-row1)))
          ((equal  (car *info-pack*) "CHORD")
           (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1b row2 row4 button-row1)))
          ((equal  (car *info-pack*) "GRACE-CHORD")
           (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1b row2 row4 button-row1)))
          ((equal (car *info-pack*) "GROUP")
           (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1 row2 button-row2)))
          (t (setf (capi::layout-description (capi::pane-layout win)) 
                   (list row1b row2 button-row2))))
    (capi::display win)
    win)

;chord
(defun open-chord-info (objs)
  (setf info-x (+ info-x 15)
        info-y (+ info-y 15))
  (setf win (make-instance *obj-info*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :background (color:make-rgb  0.772 0.855 0.788) ;*azulito*
                           :display-state :normal
                           :title "OBJ INFO" ;(format nil "~S INFO" (car *info-pack*))  
                           ))
  (setf (ep win) win)
  (setf (sel win) objs)       
  (setf pane1 (make-instance 'capi::title-pane
                               :font (gp::make-font-description :size 12)
                               :text (car *info-pack*)
                               ;:background :grey
                               :visible-min-height '(character 2)
                               :visible-max-width nil)) ;object
  (setf pane1b (make-instance 'capi::title-pane
                        ;  :font (gp::make-font-description :size 12)
                                :text (nth 1 *info-pack*)
                                :visible-min-height '(character 2)
                                :visible-max-width nil));pos if
;  (setf pane2 (make-instance 'capi::display-pane
;                               :font *def-objinfo-font*
;                               :text (nth 2 *info-pack*)
;                               :visible-min-height '(character 1)
;                               :visible-max-width nil)) ;selection

  (setf pane3 (setf pane3 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 3 *info-pack*)
                               :change-callback 'update-callback
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;midics
                )
  (setf pane4 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 4 *info-pack*) 
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;vels
  (setf pane5 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 5 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));durs
  (setf pane6 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 6 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));chans
  (setf pane7 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 7 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));offs
  (setf pane8 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 8 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));ports
  (setf button1 (make-instance 'capi::push-button
                                 :text "Set"
                                 :data *info-pack* ;:push-button
                                 :callback-type :data-interface
                                 :selection-callback 'button-set-info-callback))

  (setf button2 (make-instance 'capi::push-button
                                 :text "Reset"
                                 :data *info-pack* 
                                 :callback-type :data-interface
                                 :selection-callback 'button-reset-callback))
    
  (setf button3 (make-instance 'capi::push-button
                                 :text "Close"
                                 :data *info-pack* 
                                 :callback-type :data-interface
                                 :selection-callback 'close-info-callback))
    
  
  (setf row1 (make-instance 'capi::row-layout
                           :y-adjust :center
                           :columns 2 ;1
                           :description (list pane1)))
  (setf row1b (make-instance 'capi::row-layout
                          :y-adjust :center
                          :columns 1
                          :description (list pane1 pane1b)))

  (setf row3 (make-instance 'capi::grid-layout
                         :y-adjust :center
                         :columns 2
                         :description (list "MIDIC:" pane3
                                            "VELOCITY:" pane4
                                            "DURATION:" pane5
                                            "CHANNEL:" pane6
                                            "PORT:" pane8)))
    
  (setf row4 (make-instance 'capi::grid-layout
                         :y-adjust :center
                         :columns 2
                         :description (list "MIDICS:" pane3
                                            "VELOCITIES:" pane4
                                            "DURATIONS:" pane5
                                            "CHANNELS:" pane6
                                            "OFFSETS:" pane7
                                            "PORTS:" pane8)))
  (setf button-row1 (make-instance 'capi::row-layout
                         :y-adjust :center
                         :description (list button1 button2 button3)))
  (setf button-row2 (make-instance 'capi::row-layout
                                   :y-adjust :center
                                   :description (list button3)))


    (cond ((equal (car *info-pack*) "NOTE")
           (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1 row3 button-row1)))
          ((equal  (car *info-pack*) "CHORD")
           (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1b row4 button-row1)))          
          (t (setf (capi::layout-description (capi::pane-layout win)) 
                   (list row1b button-row2))))
    (capi::display win)
    win)


;note
(defun open-note-info (objs)
(setf info-x (+ info-x 15)
        info-y (+ info-y 15))       
  (setf win (make-instance *obj-info*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :background (color:make-rgb  0.772 0.855 0.788) ;*azulito*
                           :display-state :normal
                           :title "OBJ INFO" ;(format nil "~S INFO" (car *info-pack*))  
                           ))
  (setf (ep win) win)
  (setf (sel win) objs)       
  (setf pane1 (make-instance 'capi::title-pane
                               :font (gp::make-font-description :size 12)
                               :text (car *info-pack*)
                               ;:background :grey
                               :visible-min-height '(character 2)
                               :visible-max-width nil)) ;object

  (setf pane3 (setf pane3 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 3 *info-pack*)
                               :change-callback 'update-callback
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;midics
                )
  (setf pane4 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 4 *info-pack*) 
                               :visible-min-height '(character 1)
                               :visible-max-width nil)) ;vels
  (setf pane5 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 5 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));durs
  (setf pane6 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 6 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));chans
  (setf pane8 (make-instance 'capi::multi-line-text-input-pane
                               :font *def-objinfo-font*
                               :text (nth 8 *info-pack*)
                               :visible-min-height '(character 1)
                               :visible-max-width nil));ports
  (setf button1 (make-instance 'capi::push-button
                                 :text "Set"
                                 :data *info-pack* ;:push-button
                                 :callback-type :data-interface
                                 :selection-callback 'button-set-info-callback))

  (setf button2 (make-instance 'capi::push-button
                                 :text "Reset"
                                 :data *info-pack* 
                                 :callback-type :data-interface
                                 :selection-callback 'button-reset-callback))
    
  (setf button3 (make-instance 'capi::push-button
                                 :text "Close"
                                 :data *info-pack* 
                                 :callback-type :data-interface
                                 :selection-callback 'close-info-callback))
    
  
  (setf row1 (make-instance 'capi::row-layout
                           :y-adjust :center
                           :columns 2 ;1
                           :description (list pane1)))
  (setf row1b (make-instance 'capi::row-layout
                          :y-adjust :center
                          :columns 1
                          :description (list pane1 pane1b)))

  (setf row3 (make-instance 'capi::grid-layout
                         :y-adjust :center
                         :columns 2
                         :description (list "MIDIC:" pane3
                                            "VELOCITY:" pane4
                                            "DURATION:" pane5
                                            "CHANNEL:" pane6
                                            "PORT:" pane8)))
    
  
  (setf button-row1 (make-instance 'capi::row-layout
                         :y-adjust :center
                         :description (list button1 button2 button3)))
  (setf button-row2 (make-instance 'capi::row-layout
                                   :y-adjust :center
                                   :description (list button3)))
  
  (setf (capi::layout-description (capi::pane-layout win)) 
                 (list row1 row3 button-row1))
           (capi::display win)
    win)

;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------
;already in treeeditor
;(defun update-callback (pane point old-length new-length)
;t)

;button's callbacks
;SET button

(defun set-info-button (type data interface)
  (with-slots (ep) interface
      (setf  (nth 3 *info-pack*) (capi::text-input-pane-text pane3))
      (setf  (nth 4 *info-pack*) (capi::text-input-pane-text pane4))
      (setf  (nth 5 *info-pack*) (capi::text-input-pane-text pane5))
      (setf  (nth 6 *info-pack*) (capi::text-input-pane-text pane6))
      (setf  (nth 7 *info-pack*) (capi::text-input-pane-text pane7))
      (setf  (nth 8 *info-pack*) (capi::text-input-pane-text pane8))
   ; (print (list (selection interface) (cdddr *info-pack*)))
     ; (print (list obj tree))
      (multiple-value-setq (info-x info-y info-width info-height)
        (capi:top-level-interface-geometry interface))
      (apply (intfunc interface) (list (ompanel interface) (sel interface) (cdddr *info-pack*)))
      ;(om::set-obj-info (ompanel interface) (sel interface) (cdddr *info-pack*))
      ))
    

(defun button-set-info-callback (&rest args)
  (apply 'set-info-button "selected" args)
  (apply 'close-button-info "closed" args)
  )

;RESET button

(defun reset-button (type data interface)
  (with-slots (ep) interface
    (setf  (capi::text-input-pane-text pane3) (nth 3 *info-pack*))
    (setf  (capi::text-input-pane-text pane4) (nth 4 *info-pack*))
    (setf  (capi::text-input-pane-text pane5) (nth 5 *info-pack*))
    (setf  (capi::text-input-pane-text pane6) (nth 6 *info-pack*))
    (setf  (capi::text-input-pane-text pane7) (nth 7 *info-pack*))
    (setf  (capi::text-input-pane-text pane8) (nth 8 *info-pack*))
    ))

(defun button-reset-callback (&rest args)
  (apply 'reset-button "selected" args)
  )

;CLOSE button

(defun close-button-info (type data interface)
  (with-slots (ep) interface 
    (multiple-value-setq (info-x info-y info-width info-height)
        (capi:top-level-interface-geometry interface)) 
    (capi:quit-interface ep)  
    ))

(defun close-info-callback (&rest args)
  (apply 'close-button-info "closed" args)
  )

