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

;Author: Paulo Henrique Raposo

(in-package :om)

;;; Icon-browser panels (PackagePanel, PackageContentsPanel, InstancePanel) opt
;;; out of the canvas zoom (see nonrelationcontainer.lisp for the broader set).

;;; Single opt-out: gestures handlers may run but apply nothing.

(defmethod oa::om-zoom-applies-p ((pane PackageContentsPanel)) nil)
(defmethod oa::om-zoom-applies-p ((pane arraypanel))           nil)
(defmethod oa::om-zoom-applies-p ((pane soundPanel))           nil)
(defmethod oa::om-zoom-applies-p ((pane sheet-scorepanel))     nil)
(defmethod oa::om-zoom-applies-p ((pane scorePanel))           nil)

;;; Double opt-out: also pin om-zoom-of to 1.0 so ancestor-walk lookups
;;; from frames inside nested panels never observe a non-trivial factor.

(defmethod oa::om-zoom-applies-p ((pane workSpacePanel))     nil)
(defmethod oa::om-zoom-applies-p ((pane folderPanel))        nil)
(defmethod oa::om-zoom-applies-p ((pane GlobalsfolderPanel)) nil)
(defmethod oa::om-zoom-applies-p ((pane classPanel))         nil)
(defmethod oa::om-zoom-applies-p ((pane GenericFunPanel))    nil)
(defmethod oa::om-zoom-applies-p ((pane PackagePanel))       nil)
(defmethod oa::om-zoom-applies-p ((pane InstancePanel))      nil)
(defmethod oa::om-zoom-applies-p ((pane MaquettePanel))      nil)

(defmethod oa::om-zoom-of ((pane workSpacePanel))     1.0)
(defmethod oa::om-zoom-of ((pane folderPanel))        1.0)
(defmethod oa::om-zoom-of ((pane GlobalsfolderPanel)) 1.0)
(defmethod oa::om-zoom-of ((pane classPanel))         1.0)
(defmethod oa::om-zoom-of ((pane GenericFunPanel))    1.0)
(defmethod oa::om-zoom-of ((pane PackagePanel))       1.0)
(defmethod oa::om-zoom-of ((pane InstancePanel))      1.0)
(defmethod oa::om-zoom-of ((pane MaquettePanel))      1.0)

(defmethod (setf oa::om-zoom-of) (value (pane workSpacePanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane folderPanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane GlobalsfolderPanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane classPanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane GenericFunPanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane PackagePanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane InstancePanel))
  (declare (ignore value)) 1.0)
(defmethod (setf oa::om-zoom-of) (value (pane MaquettePanel))
  (declare (ignore value)) 1.0)

;;; Touch-only opt-out: pane may respond to programmatic / keyboard zoom
;;; but pinch is inert.

(defmethod oa::om-zoom-touch-update ((pane FluidChannelsPanel) scale ax ay)
  (declare (ignore scale ax ay)) nil)

(defmethod oa::om-zoom-touch-update ((pane fluidcontrollerPanel) scale ax ay)
  (declare (ignore scale ax ay)) nil)

;;; Pinch override: score panes opt out of canvas zoom (om-zoom-applies-p nil
;;; above) but accept pinch dispatch so their own om-zoom-touch-update can
;;; scale the music font (zoom-font-scale).

(defmethod oa::om-zoom-touch-applies-p ((pane scorePanel))    t)
(defmethod oa::om-zoom-touch-applies-p ((pane notePanel))     t)
(defmethod oa::om-zoom-touch-applies-p ((pane chordPanel))    t)
(defmethod oa::om-zoom-touch-applies-p ((pane chordseqPanel)) t)
