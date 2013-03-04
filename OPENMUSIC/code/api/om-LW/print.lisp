;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; PRINT UTILITY
;DocFile
;;===========================================================================


(in-package :om-api)

(export '(om-page-setup
          om-print-window 
          om-print-one-page
          om-print-view
          om-compute-page-number
          om-print-document
          om-score-paper-size
          ) :om-api)


(defvar *score-thickness* nil)
(setf *score-thickness* 0.4)

(defun om-draw-score-line (x1 y1 x2 y2)  
  (gp:draw-line *curstream* x1 y1 x2 y2  :thickness *score-thickness* ))


(defun om-page-setup ()
  (capi::page-setup-dialog))

(defmethod om-print-window ((self om-abstract-window))
  (om-print-document self))



;print the window in one page IL FAUT ALIGNER A LA TAILLE DU PATCH ET NON DE LA FENETRE
(defun om-print-one-page (self) 
  (let ((image (gp::make-image-from-port (om-get-view self) 0 0 (- (om-width self) 15) (- (om-height self) 15))))
    (when image
      (let ((printer (capi:print-dialog :print-pages-p nil :print-copies-p t)))
        (when printer
          (let ((printer-metrics (get-printer-metrics printer))
                (dpi-x (printer-metrics-dpi-x printer-metrics))
                (dpi-y (printer-metrics-dpi-y printer-metrics)))
        (when printer
          (capi:with-print-job (printer-port :printer printer)
            (multiple-value-bind
                (page-width page-height)
                (capi:get-page-area printer)
              (let* ((drawing-width (- (om-width self) 15))
                     (drawing-height  (- (om-height self) 15))
                     (widen-p (> (/ page-width page-height)
                                 (/ drawing-width drawing-height)))
                     (page-transform-x 0)
                     (page-transform-y 0)
                     (page-transform-width (if widen-p
                                               (* page-width
                                                  (/ drawing-height page-height))
                                             drawing-width))
                     (page-transform-height (if widen-p
                                                drawing-height
                                              (* page-height
                                                 (/ drawing-width page-width)))))
                (capi:with-document-pages (page 1 1) ; all on one page
                  (capi:with-page-transform (page-transform-x
                                             page-transform-y
                                             page-transform-width
                                             page-transform-height)
                    (draw-image printer-port image 0 0)))))))
        )))
      )))


;print in n pages
(defvar *default-printer-port* nil)
(defmethod om-print-document ((self om-graphic-object)) 
  (let* ((printer (capi:print-dialog :print-pages-p nil
                                     :print-copies-p t))
         (printer-metrics (get-printer-metrics printer))
         (dpi-x (printer-metrics-dpi-x printer-metrics))
         (dpi-y (printer-metrics-dpi-y printer-metrics)))
    (when printer
      (capi:with-print-job (printer-port :printer printer)
        (multiple-value-bind
            (page-width page-height)
            (capi:get-page-area printer)
          (let* ((page-count (om-compute-page-number self (om-make-point page-width page-height))))
            (capi:with-document-pages (page 1 page-count) 
              (let ((*default-printer-port* printer-port))
                (om-print-view self (om-make-point page-width page-height) page page-count)))))))))


(defmethod om-compute-page-number ((view om-graphic-object) page-size )
  (declare (ignore page-size ))
  1)

(defmethod om-score-paper-size ()
  (let ((printer (current-printer)) rep)
    (multiple-value-bind
            (page-width page-height)
            (capi:get-page-area printer)
      (setf rep (om-make-point page-width page-height)))
    rep))






    