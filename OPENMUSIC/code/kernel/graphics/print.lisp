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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================




(in-package :om)
 
;;;(defun get-print-style ()
;;;  (rlet ((pmsession :ptr)
;;;         (pageformat :ptr))
;;;    (unwind-protect
;;;      (let ((orientation (#_Newptr 2)) or 
;;;            (scale (#_Newptr 64)) sc 
;;;            *pmsession* *pageformat* prect res)
;;;        (let ((err (#_PMCreateSession pmsession)))
;;;          (when (neq err #$NoErr)
;;;            (ccl::printer-error "when create session" err)))
;;;        (setq *pmsession* (%get-ptr pmsession))        
;;;        (let ((err (#_PMCreatePageFormat pageformat)))
;;;          (when (neq err #$noERR)
;;;            (ccl::printer-error nil err)))
;;;        (setq *pageformat* (%get-ptr pageformat))
;;;        (let ((err (#_PMSessionDefaultPageFormat *pmsession* *pageformat*)))
;;;          (when (neq err #$noerr)
;;;            (#_PMRelease *pageformat*) (setq *pageformat* nil)
;;;            (ccl::printer-error nil err)))
;;;        
;;;        (#_PMGetOrientation *pageformat* orientation)
;;;        (setf or (%GET-UNSIGNED-word orientation))
;;;        (#_disposeptr orientation)
;;;        
;;;        (#_PMGetScale *pageformat* scale)
;;;        (setf sc (%GET-double-float scale))
;;;        (#_disposeptr scale)
;;;        
;;;        (rlet ((pagerect :pmrect)) 
;;;          (#_PMGetAdjustedPageRect *pageformat* pagerect)
;;;          (setf prect (list (rref pagerect :pmrect.top)
;;;                            (rref pagerect :pmrect.left)
;;;                            (rref pagerect :pmrect.bottom)
;;;                            (rref pagerect :pmrect.right)))) 
;;;        
;;;        (rlet ((resolution :pmresolution)) 
;;;          (#_PMGetresolution *pageformat* resolution)
;;;          (setf res (list (rref resolution :pmresolution.hRes)
;;;                          (rref resolution :pmresolution.VRes))))
;;;        
;;;        (when *pmsession*
;;;          (#_PMRelease *pmsession*)
;;;          (setq *pmsession* nil))
;;;        (list or sc prect res)))))
;;;
;;;(defmethod ccl::page-size ((window t))
;;;  (let* ((info (get-print-style))
;;;         (rect (third info)))
;;;    (om-make-point (round (- (fourth rect) (first rect)))
;;;                (round (- (third rect) (second rect))))))
;;;    
;;;
;;;
;;;(defmethod ccl::window-hardcopy ((w om::Editorwindow) &optional (show-dialog t))
;;; (om-beep-msg "Not yet implemented")
;;; nil)