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
;;===========================================================================


(in-package :om)

(defvar *audio-master-gain* 1.0)
(defvar *audio-master-window* nil)

;;; do nothing for the moment..

(defun show-audio-master-win ()
  (if (and *audio-master-window* (om-window-open-p *audio-master-window*)) 
      (om-select-window *audio-master-window*)
    (setf *audio-master-window* 
          (let ((win (om-make-window 
                      'om-window
                      :window-title "Master"
                      :size (om-make-point 140 230)
                      :resizable nil
                      :bg-color *om-window-def-color*))
                (text  (om-make-dialog-item 
                        'om-static-text
                        (om-make-point 80 100)
                        (om-make-point 60 60)
                        (format nil "~,2f" *audio-master-gain*)
                        :font *om-default-font3b*)))

            (om-add-subviews win
                             text
                             (om-make-dialog-item 
                              'om-slider
                              (om-make-point 35 10)
                              (om-make-point 20 200)
                              "Gain"
                              :range '(0 100)
                              :value (round (* *audio-master-gain* 100))
                              :di-action #'(lambda (item)
                                             (setf *audio-master-gain* (* (om-slider-value item) 0.01))
                                             (om-set-dialog-item-text text (format nil "~,2f" *audio-master-gain*)))
                              ))
            win))
    ))