;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================


;;===========================================================================
;DocFile
; TOOLTIPS
;DocFile
;;===========================================================================



(in-package :om-api)


;==========
; export :
;==========
(export '(
                om-set-help
                om-help-on?
                om-show-tooltip
                om-hide-tooltip
                ) :om-api)

;======================
(in-package :oa)


(defvar *helpon* t)

(defun om-set-help (mode)
  (setf *helpon* mode))

(defun om-help-on? () *helpon*)

;(defmethod om-show-tooltip ((self om-graphic-object) &optional (remove nil)) t)
;(defvar *ttopen* nil)

(defmethod om-show-tooltip ((self om-graphic-object) &optional (remove nil) (short nil))
  (when (and (om-get-view self) *helpon*)
    (internal-show-tooltip self remove short))
  nil)


(defmethod internal-show-tooltip ((self om-graphic-object) &optional (remove nil) (short nil))
  (if (and
       (om-get-help-spec self)
       (not (string-equal (om-get-help-spec self) ""))
       t)
      (let ((text (reduce #'(lambda (val segment) (concatenate 'string val segment (string #\Newline)))
                          (oa::text-wrap-pix (om-get-help-spec self) *om-default-font1* 200)
                          :initial-value "")))
        (if short
            (setf text (string-downcase (read-from-string text)))
	    (setf text (subseq text 0 (- (length text) 1))))
        (multiple-value-bind (x y) (capi::current-pointer-position :relative-to (om-get-view self))
          (capi:display-tooltip (om-get-view self) :x x :y (- y 20) :text text)
          #+cocoa(sleep (if remove 0.05 0.1))
          ))
      (capi:display-tooltip (om-get-view self))
      ))


;; protect, e.g. om-tab-layout
(defmethod om-hide-tooltip ((self t)) nil)

(defmethod om-hide-tooltip ((self capi::output-pane))
  (when (and (om-get-view self) *helpon*)
    (capi:display-tooltip (om-get-view self)))
  nil)

(defmethod om-show-tooltip ((self om-standard-dialog-item) &optional remove short) nil)
(defmethod om-hide-tooltip ((self om-standard-dialog-item)) nil)


;;; NOT USED...
(defmethod om-help-callback ((interface om-abstract-window) pane type key)
  ;(print (list interface pane type key))
  (when (and *helpon* (om-command-key-p))
    (multiple-value-bind (x y) (capi::current-pointer-position :relative-to pane)
      (let ((hview (or (capi::pinboard-object-at-position pane x y) pane)))
        (case type
          (:tooltip (om-get-help-spec hview))
          (t nil)
          )))))
;    (:pointer-documentation-enter
;     (when (stringp key)
;       (setf (capi:titled-object-message interface)
;             key)))
;    (:pointer-documentation-leave
;     (setf (capi:titled-object-message interface)
;           "Something else"))
;    (:help (do-detailed-help interface )))
