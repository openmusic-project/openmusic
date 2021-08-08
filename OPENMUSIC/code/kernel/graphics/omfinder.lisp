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
;DocFile
;Author: Karim Haddad
;OM Finder editor is defined in this file.
;Last Modifications : 05/08/21
;DocFile


(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;GRAPHICS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *finder-window* nil)
(defparameter *om-finder-search-name* nil)
(setf *om-finder-search-name* "")


(defclass omfinder-window (om-dialog)())

(defun make-finder-win (editor)
  (setf *om-finder-search-name* "")
  (let* ((win (om-make-window 'omfinder-window :window-title "OM Finder" 
                              :size (om-add-points (om-make-point 500 100) (om-make-point 0 50)) 
                              :position (om-make-point 100 50) :close t :resizable nil
                              ))
         (b-posy (+ 75 5))
         (posy 0)
         (dy 30)
         (l1 20))
    
    (om-add-subviews win      
                 
                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "Find what:"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 100) posy)
                                          (om-make-point 300 15) *om-finder-search-name*
                                          :di-action (om-dialog-item-act item 
                                                          (setf *om-finder-search-name* (om-dialog-item-text item)))
                                          :font *controls-font*
                                          )
                     #|
                     (om-make-dialog-item 'om-check-box (om-make-point l1 (incf posy 40)) (om-make-point 180 15) "Patches" 
                                          ;:di-action (om-dialog-item-act item 
                                          ;             (set-pref modulepref :handle-errors (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p nil)
                     (om-make-dialog-item 'om-check-box (om-make-point (+ l1 100) posy) (om-make-point 180 15) "Folders" 
                                          ;:di-action (om-dialog-item-act item 
                                          ;             (set-pref modulepref :handle-errors (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p nil)
                 
                     (om-make-dialog-item 'om-check-box (om-make-point (+ l1 200) posy) (om-make-point 180 15) "ALL" 
                                          ;:di-action (om-dialog-item-act item 
                                          ;             (set-pref modulepref :handle-errors (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p nil)
                     |#
                     (om-make-dialog-item 'om-button (om-make-point (- 550 375) (+ 30 b-posy)) (om-make-point 80 22) "Find" 
                                          :di-action (om-dialog-item-act  item 
                                                       (om-finder editor (string-downcase *om-finder-search-name*))
                                                       ))
                    
                     (om-make-dialog-item 'om-button (om-make-point (- 550 295) (+ 30 b-posy)) (om-make-point 80 22) "Cancel" 
                                          :di-action (om-dialog-item-act  item
                                                       (om-close-window (om-view-window item))
                                                       )))

    win))
     
(defun omG-make-finder-dialog (editor)
   (if (and *finder-window* (om-window-open-p *finder-window*))
     (om-select-window *finder-window*)
     (setf *finder-window* (om-select-window (make-finder-win editor)))))


;should go in menu
(defun show-finder-win (editor) 
  (omG-make-finder-dialog editor))

