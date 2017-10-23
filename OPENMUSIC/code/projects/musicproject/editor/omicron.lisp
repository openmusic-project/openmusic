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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson
;=========================================================================

;;===========================================================================
;DocFile
; MUSIC FONTS (for OMICRON.TTF) 
;;===========================================================================

(in-package :om)

(defun fig-1 () (format nil "~D" (code-char 81)))
(defun fig-1/2 () (format nil "~D" (code-char 79)))
(defun fig-1/4 () (format nil "~D" (code-char 78)))
(defun fig-1/8 () (format nil "~D" (code-char 77)))
(defun fig-1/16 () (format nil "~D" (code-char 76)))

(defun omicronpoint () (format nil "~D" (code-char 46)))
;;;;;
(defun get-stringfrom-num (num)
   (case num
     (1 (string+ (fig-1/2) (omicronpoint) )) 
     (2 (fig-1/2)) 
     (3 (string+ (fig-1/4) (omicronpoint) ))
     (4 (fig-1/4)) 
     (8 (fig-1/8)) 
     (16 (fig-1/16))))

;;;alterations
;=====================SCALES================================

(defun diese () (code-char 35))
(defun bemol () (code-char 98))
(defun beca () (code-char 110))
(defun db-diese () (code-char 45))
(defun db-bemol () (concatenate 'string (string (code-char 98)) (string (code-char 98))))
(defun inv-bemol () (code-char 96))

(defun t-1/16 () (code-char 107))
(defun t-1/8 () (code-char 108))
(defun t-3/16 () (code-char 76))  ;(defun t-3/16 () (code-char 220))
(defun t-1/4 () (code-char 43))
(defun t-5/16 () (code-char 42))
(defun t-3/8 () (code-char 41))
(defun t-7/16 () (code-char 40))
(defun t-1/2 () (code-char 35))
(defun t-9/16 () (code-char 33))
(defun t-5/8 () (code-char 34))
(defun t-11/16 () (code-char 36))
(defun t-3/4 () (code-char 48))  
(defun t-13/16 () (code-char 49))  ;(defun t-13/16 () (code-char 191))
(defun t-7/8 () (code-char 50))  ;(defun t-7/8 () (code-char 192))
(defun t-15/16 () (code-char 51))  ;(defun t-15/16 () (code-char 193))

(defun beca-3 () (code-char 100))
(defun t-1/12 () (code-char 107))
(defun t-2/12 () (code-char 108))
(defun t-3/12 () (code-char 76))   ;(defun t-3/12 () (code-char 220))
(defun t-4/12 () (code-char 113))
(defun t-5/12 () (code-char 119))
(defun t-6/12 () (code-char 35))
(defun t-7/12 () (code-char 77))  ;(defun t-7/12 () (code-char 224))
(defun t-8/12 () (code-char 114))
(defun t-9/12 () (code-char 120))
(defun t-10/12 () (code-char 54))
(defun t-11/12 () (code-char 78))
;;;
;;;
(defun d-1/3 () (code-char 60))
(defun d-2/3 () (code-char 61))
;;;
;;;
(defun beca-5 () (code-char 100))
(defun t-1/10 () (code-char 108))  
(defun t-2/10 () (code-char 113))
(defun t-3/10 () (code-char 53))
(defun t-4/10 () (code-char 114))
(defun t-5/10 () (code-char 35))
(defun t-6/10 () (code-char 115))
(defun t-7/10 () (code-char 55))
(defun t-8/10 () (code-char 116))
(defun t-9/10 () (code-char 56))

(defun d-1/5 () (code-char 60))
(defun d-2/5 () (code-char 61))
(defun d-3/5 () (code-char 62))
(defun d-4/5 () (code-char 63))

(defun beca-7 () (code-char 100 ))
(defun t-1/14 () (code-char 108))
(defun t-2/14 () (code-char 113))
(defun t-3/14 () (code-char 53))
(defun t-4/14 () (code-char 114))
(defun t-5/14 () (code-char 54))
(defun t-6/14 () (code-char 115))
(defun t-7/14 () (code-char 35))
(defun t-8/14 () (code-char 116))
(defun t-9/14 () (code-char 56))
(defun t-10/14 () (code-char 117))
(defun t-11/14 () (code-char 57))
(defun t-12/14 () (code-char 118))
(defun t-13/14 () (code-char 58))

(defun d-1/7 () (code-char 60))
(defun d-2/7 () (code-char 61))
(defun d-3/7 () (code-char 62))
(defun d-4/7 () (code-char 63))
(defun d-5/7 () (code-char 64))
(defun d-6/7 () (code-char 37)) 

;;;=============dynamics
(defun dyn-ppp () (code-char 82))
(defun dyn-pp () (code-char 81))
(defun dyn-p () (code-char 112))
(defun dyn-mp () (code-char 80))
(defun dyn-mf () (code-char 70))
(defun dyn-f () (code-char 102))
(defun dyn-ff () (code-char 103))
(defun dyn-fff () (code-char 104))


;;;==============Key
(defun key-g () (code-char 38))
(defun key-f () (code-char 63))
(defun key-c () (code-char 66))

;;;;======================head notes

(defun head-8 () (format nil "~D" (code-char 83)))
(defun head-4 () (format nil "~D" (code-char 73)))
(defun head-2 () (format nil "~D" (code-char 82)))
(defun head-1 () (format nil "~D" (code-char 81)))
(defun head-1/2 () (format nil "~D" (code-char 80)))
(defun head-1/4 () (format nil "~D" (code-char 110)))

(defun head-carre () (format nil "~D" (code-char 101)))
(defun head-losange () (format nil "~D" (code-char 100)))
(defun head-rect () (format nil "~D" (code-char 98)))
(defun head-triangle () (format nil "~D" (code-char 102)))
(defun head-cercle () (format nil "~D" (code-char 104)))



;;;;=======================rests
(defun rest-4 () (format nil "~D" (code-char 93)))
(defun rest-2 () (format nil "~D" (code-char 92)))
(defun rest-1 () (format nil "~D" (code-char 91)))
(defun rest-1/2 () (format nil "~D" (code-char 90)))
(defun rest-1/4 () (format nil "~D" (code-char 89)))
(defun rest-1/8 () (format nil "~D" (code-char 88))) 
(defun rest-1/16 () (format nil "~D" (code-char 87)))
(defun rest-1/32 () (format nil "~D" (code-char 86)))
(defun rest-1/64 () (format nil "~D" (code-char 85)))
(defun rest-1/128 () (format nil "~D" (code-char 84)))

;;;;=======================beams
(defun beam-dwn () (format nil "~D" (code-char 74)))
(defun beam-up () (format nil "~D" (code-char 75)))
;;;;================Digits as strings

(defun num2sstr (num)
   (let ((str (format nil "~D" num))
         (rep ""))
     (loop for i from 0 to (- (length str) 1) do
           (setf rep (string+ rep (digi2sstr (elt str i)))))
     rep))

(defun digi2sstr (char)
   (case char
     (#\1 (format nil "~D" (code-char 85))) 
     (#\2 (format nil "~D" (code-char 86)))
     (#\3 (format nil "~D" (code-char 87)))
     (#\4 (format nil "~D" (code-char 88))) 
     (#\5 (format nil "~D" (code-char 89))) 
     (#\6 (format nil "~D" (code-char 90))) 
     (#\7 (format nil "~D" (code-char 91))) 
     (#\8 (format nil "~D" (code-char 92))) 
     (#\9 (format nil "~D" (code-char 93)))
     (#\0 (format nil "~D" (code-char 84)))))






