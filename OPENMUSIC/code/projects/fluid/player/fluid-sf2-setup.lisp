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

;;; Fluidsynth package

;;;==============================
;;; SF2 LOADING SETUP TOOL
;;;==============================

(in-package :om)

(defmethod sf2-setup (settings &optional action)
  (let* ((inf (reverse (cadr settings)))
         (def-path (third inf))
         (info (loop for i from 0 to (1- (car inf))
                     collect (list (list i) (list def-path)))))
    (show-sf2-dialog info action)))


(defparameter *sf2-setup-window* nil)

(defclass sf2-ports-dialog (oa::om-window) 
  ((portviews :accessor portviews :initform nil :initarg :portviews)
   (settings :accessor settings :initform nil :initarg :settings)))

(defmethod oa::om-window-close-event ((self sf2-ports-dialog))
  (setf *sf2-setup-window* NIL))

(defclass sf2-ports-view (oa::om-scroller) 
  ((portlines :accessor portlines :initform nil :initarg :portlines)
   (direction :accessor direction :initform nil :initarg :direction)))

(defmethod oa::om-resize-callback ((self sf2-ports-dialog) x y w h)
  (call-next-method)
  (let ((buttons (reverse (oa:om-subviews self)))
        (panel1 (car (portviews self)))
        (panel2 (second (portviews self)))
        )
    (when buttons ;(car buttons) 
      (oa::om-set-view-position (car buttons) (oa::om-make-point (abs (- 680 (abs ( - 210 (+ 770 w))))) (abs (- (- h  20) 20))))
      (oa::om-set-view-position (second buttons) (oa::om-make-point (abs (- 780 (abs ( - 210 (+ 770 w))))) (abs (- (- h  20) 20))))
      (oa::om-set-view-position (third buttons) (oa::om-make-point (abs (- 880 (abs ( - 210 (+ 770 w))))) (abs (- (- h  20) 20))))
      )
    (when panel1
      (oa::om-set-view-size panel1 (oa::om-make-point  (abs (- 680 (abs ( - 120 (+ 770 w))))) ;770 
                                                      (- h 90))))
     ))

;(cl-fluid::name-fsynths cl-fluid::*fl-synths*)


(defmethod set-sf2-directory-view ((self sf2-ports-view) dialog) 
  (let* ((devices (remove nil (loop for ref in (cl-fluid::name-fsynths cl-fluid::*fl-synths*) 
                                    collect (second ref)))) ;unused!
         (pos-in-settings 0)
         (settings-list (nth pos-in-settings (settings dialog)))
         (dy 35))
    (oa::om-with-delayed-update self
      (apply 'oa::om-remove-subviews (cons self (portlines self)))
      (oa::om-set-field-size self (oa::om-make-point (oa::om-point-x (oa::om-view-size self)) 
                                                     (+ 20 (* (length settings-list) 25))))
      (apply 'oa::om-add-subviews 
             (cons self
                   (setf (portlines self)
                         (loop for synth in cl-fluid::*fl-synths* 
                               for i = 0 then (+ i 1) collect
                                       
                                 (let* ((y (+ 10 (* i dy)))
                                        (vv (oa::om-make-view 'oa::om-view 
                                                              :size (oa::om-make-point 800 dy)
                                                              :position (oa::om-make-point 20 y)))
                                        sf2txt)
                                   (oa::om-add-subviews vv
                                                        (setf sf2txt (om-make-dialog-item 'om-static-text  (om-make-point 120 i) (om-make-point 320 45)
                                                                                          (cl-fluid::sf2path synth)
                                                                                          :font *om-default-font1*))
                                                        (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 20 3) (oa::om-make-point 30 20)
                                                                                 (format nil "~D" i)
                                                                                 :font oa::*om-default-font2b*)
                                                        (om-make-view 'om-icon-button 
                                                                      :icon1 "folder" :icon2 "folder-pushed"
                                                                      :position (om-make-point 60 3) :size (om-make-point 26 25) 
                                                                      :action (om-dialog-item-act item
                                                                                (let* ((newsf2 (om-choose-file-dialog :directory 
                                                                                                                      ;(pathname (cl-fluid::sf2path synth))
                                                                                                                      oa::*last-directory*
                                                                                                                      ))
                                                                                       (pos (position (om-view-container item) 
                                                                                                      (om-subviews (om-view-container (om-view-container item)))))
                                                                                       (snt (nth pos cl-fluid::*fl-synths*)))
                                                                                  (om-set-dialog-item-text sf2txt (om-namestring newsf2))
                                                                                  (fluid-load-sf2 pos (om-namestring newsf2))
                                                                                  (setf (cl-fluid::sf2path snt) (om-namestring newsf2))
                                                                                  (push newsf2 (cl-fluid::sf2stack snt))))))
                                   vv))))))))



(defun show-sf2-dialog (settings &optional action)
  (if *sf2-setup-window*
      (oa::om-select-window *sf2-setup-window*)
    (let* ((dd (oa::om-make-window 'sf2-ports-dialog 
                                   :window-title "SF2 Port Setup"
                                   :bg-color oa::*om-light-gray-color*
                                   :size (oa::om-make-point 800 270)
                                   :resizable t
                                  ; :external-min-width 800
                                   :external-max-width 800
                                   :external-min-height 270 
                                   :settings settings
                                   ))
        
           (inv (oa::om-make-view 'sf2-ports-view :position (oa::om-make-point 10 40)
                                  :size (oa::om-make-point 770 180) :scrollbars :v :retain-scrollbars nil
                                  :direction :in)))
      (unless cl-fluid::*fl-synths*
        (om-add-subviews inv
                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 250 75) (oa::om-make-point 280 120) "No FluidSynths loaded!"
                                                  :font oa::*om-default-font4b*
                                                  :fg-color *om-red-color*
                                                  )
                         ))
      (oa::om-add-subviews dd 
                          
                           (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 40 10) (oa::om-make-point 120 20) "Ports"
                                                    :font oa::*om-default-font2b*)
                           (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 180 10) (oa::om-make-point 120 20) "Paths"
                                                    :font oa::*om-default-font2b*)
                           
                           )
      (setf (portviews dd) (list inv))
      (oa::om-add-subviews dd inv ) 
      (set-sf2-directory-view inv dd)
  
      (oa::om-add-subviews dd 
                           (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 480 230) (oa::om-make-point 100 20) "Load"
                                                    :di-action #'(lambda (item) 
                                                                   (declare (ignore item))
                                                                   (load-sf2-settings inv))
                                                    )
                           (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 580 230) (oa::om-make-point 100 20) "Save"
                                                    :di-action #'(lambda (item) 
                                                                   (declare (ignore item))
                                                                   (save-sf2-settings inv))
                                                                   
                                                                   ;(when action (funcall action (settings dd))))
                                                    )
                           (oa::om-make-dialog-item 'oa::om-button  (oa::om-make-point 680 230) (oa::om-make-point 80 22) "OK" 
                                                    :di-action #'(lambda (item) (progn 
                                                                                  (when action (funcall action (settings dd)))
                                                                                  (oa::om-close-window (oa::om-view-window item))))

                                                    )
                           )
      
      (setf *sf2-setup-window* dd)
      (oa:om-show-window dd)
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SAVING SF2 LOAD SETTINGS INTO A TXT FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-sf2-settings (settings)
  (let* ((pathname (om-choose-new-file-dialog))
         (outfile (make-pathname :directory (pathname-directory pathname) :name (pathname-name pathname)))
         (views (om-subviews settings))
         (paths (loop for i in views
                      collect (om-dialog-item-text (car (om-subviews i))))))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      (loop for elt in paths 
            do (format out "~A~%" elt)))
    pathname))


(defun read-sf2-settings (&optional path)
  (let ((pathname (or path (om-choose-file-dialog)))
        sf2)
    (with-open-file (stream pathname)
      (loop for line = (read-line stream nil)
            while line
            collect (push line sf2)))
   (reverse sf2)))

(defun load-sf2-settings (settings &optional path)
  (let ((sf2 (read-sf2-settings))
        (views (om-subviews settings)))
   ; (om-inspect settings)
    (loop for i in views
          for txt in sf2
          do (om-set-dialog-item-text (car (om-subviews i)) txt))
    (loop for i from 0 to (1- *n-fsynth*)
          for sf in sf2 do (fluid-load-sf2 i sf))
    ))

