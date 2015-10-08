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
; OM-GRAPHIC OBJECTS ACTIONS CALLBACKS
;DocFile
;;===========================================================================


(export '(
          om-view-key-handler
          om-view-click-handler
          om-view-doubleclick-handler
          om-view-mouse-enter-handler
          om-view-mouse-leave-handler
          om-window-mouse-moved-handler
          
          om-init-motion
          om-click-motion-handler
          om-click-release-handler
           
          om-shift-key-p
          om-control-key-p
          om-command-key-p
          om-option-key-p
          
          om-mouse-position
          
          om-view-cursor
          om-set-view-cursor

          ) :om-api)



(in-package :om-api)

;;;=======================
;;; MOUSE
;;;=======================
;;;===============
;;; TOOLS
;;;===============

(defun internal-mouse-position (view)
  (or (ignore-errors 
        (multiple-value-bind (x y)
            (if view (capi::current-pointer-position :relative-to view :pane-relative-p t)
              (capi::current-pointer-position))
          (om-make-point x y)))
      (om-make-point 0 0)))

(defmethod om-mouse-position ((view null))
  (internal-mouse-position nil))

(defmethod om-mouse-position ((view om-graphic-object))
  (internal-mouse-position view))

(defmethod om-mouse-position ((view om-item-view))
  (om-convert-coordinates (internal-mouse-position (item-container view))
                          (item-container view) view))


;;; Recursive event dispatcher on subviews (om-view catches events but om-item-views do not)
;;; if function callback is non-NIL for one of the subviews, recursive dispatching is stopped.
(defmethod apply-in-item-subview ((self om-view) function position)
  (let ((clicked self)) 
    ;(when (equal function 'om-view-click-handler)
    ;  (print (list "clic in" self "at "))
    ;  (print-point position))
    (loop for item in (item-subviews self)
          while (equal clicked self) do
          (when (om-view-contains-point-p item (om-convert-coordinates position self (vcontainer item)))
            ;(when (equal function 'om-view-click-handler)
            ; (print (list "subview clicked" item)))
            ;(print-point (om-convert-coordinates position self item)))
            (setf clicked (apply-in-item-subview item function (om-add-points (om-convert-coordinates position self item) (om-scroll-position item))))))
    (when (or (null clicked) (equal clicked self))
      ;(print self)
      (setf clicked (funcall function self position)))
    clicked))
           

(defmethod apply-in-item-subview ((self om-graphic-object) function position)
   (apply-in-subview self function position))

(defmethod apply-in-subview ((self om-graphic-object) function position)
  (let ((clicked self)
        (pos position))
    (loop for item in (vsubviews self)
          while (equal clicked self) do
          
          (when (om-view-contains-point-p item (om-convert-coordinates pos self (vcontainer item)))
            ;(print (list self (vcontainer item) position (om-convert-coordinates position self (vcontainer item))))
            ;(print (list "sub" item))
            (setf clicked (apply-in-subview item function (om-add-points (om-convert-coordinates pos self item) (om-scroll-position item))))))
    (when (or (null clicked) (equal clicked self))
      (setf clicked (funcall function self pos)))
    clicked))

;;;=============
;;; CLIC 
;;;=============
(defvar *clicked-view* nil)

(defmethod om-clic-callback ((self om-graphic-object) x y type)
  (om-with-error-handle 
    (set-meta-keys (eval type))
    (apply-in-item-subview self 'om-view-click-handler (om-make-point x y))
    ))

(defmethod om-view-click-handler ((self om-graphic-object) position) nil)
(defmethod om-view-click-handler ((self om-abstract-window) position) self)
(defmethod om-view-click-handler ((self window-layout) position) 
  (om-view-click-handler (capi::top-level-interface self) position)
  t)

(defmethod om-view-click-handler :before ((self om-graphic-object) position)
 (setf *clicked-view* self))

;;;=================
;;; CLIC + MOVE
;;;=================
(defmethod om-init-motion (self x y))

(defmethod om-clic-motion-callback ((self om-graphic-object) x y type)
  (set-meta-keys (eval type))
  ;(print (list self x y *clicked-view*))
  (unless (equal *clicked-view* :abort) 
    (if *clicked-view* (om-click-motion-handler *clicked-view* (om-convert-coordinates (om-make-point x y) self *clicked-view*))
      ; ?!! verifier si tout va bien...
      ;(apply-in-item-subview *clicked-view* 'om-click-motion-handler (om-convert-coordinates (om-make-point x y) self *clicked-view*))
      (apply-in-item-subview self 'om-click-motion-handler (om-make-point x y)))))

(defmethod om-clic-motion-callback ((self window-layout) x y type)
  (set-meta-keys (eval type))
  ; click in window, pos in layout
  (unless (equal *clicked-view* :abort) 
    (if *clicked-view* (om-click-motion-handler *clicked-view* (om-convert-coordinates (om-make-point x y) self *clicked-view*)))
    (apply-in-item-subview self 'om-click-motion-handler (om-make-point x y))))
   
(defmethod om-click-motion-handler (self pos) t)


;;;==============
;;; RELEASE
;;;==============
(defmethod om-clic-release-callback ((self om-graphic-object) x y type) 
  (set-meta-keys (eval type))
  (unless (equal *clicked-view* :abort) 
    (if *clicked-view* 
        (om-click-release-handler *clicked-view* (om-convert-coordinates (om-make-point x y) self *clicked-view*)))
    (apply-in-item-subview self 'om-click-release-handler (om-make-point x y))))

(defmethod om-click-release-handler ((self om-graphic-object) pos) nil) 

(defmethod om-click-release-handler :before ((self om-graphic-object) position) 
  (setf *clicked-view* nil))

;;;=================
;;; DOUBLE CLIC
;;;=================
(defmethod om-double-clic-callback ((self om-graphic-object) x y type)
  (setf *clicked-view* :abort)
  (om-with-error-handle 
    (apply-in-item-subview self 'om-view-doubleclick-handler (om-make-point x y))))                         

(defmethod om-view-doubleclick-handler ((self om-abstract-window) pos) self)
(defmethod om-view-doubleclick-handler ((self om-graphic-object) pos) self)

;;;=================
;;; MOVE
;;;=================
(defvar *last-containing-view* nil)

(defmethod om-motion-callback ((self om-graphic-object) x y type)
  (set-meta-keys (eval type))
  (apply-in-subview self 'internal-motion-callback (om-make-point x y)))

(defun tooltip-key-down ()
  (om-command-key-p))

(defmethod internal-motion-callback ((self om-graphic-object) pos)
  (update-view-cursor self)
  ;; (print (list self pos *clicked-view*))
  #+cocoa(when (tooltip-key-down) (om-show-tooltip self))
  (unless (equal *last-containing-view* self)
    (when *last-containing-view*
      (om-view-mouse-leave-handler *last-containing-view*)
      (when (om-item-view-p *last-containing-view*) 
        (update-view-cursor (item-container *last-containing-view*)))
      ;; BUG IN SHEET EDITOR...
      ;;#+win32(when *last-containing-view* (om-hide-tooltip *last-containing-view*))
      )
    (om-view-mouse-enter-handler self)
    #+(or win32 linux) (when (tooltip-key-down) (om-show-tooltip self))
    ;(capi::call-HELP-CALLBACK (om-view-window self) (om-get-view self) :tooltip t)  
    (setf *last-containing-view* self))
  (when (om-view-window self)
    (om-window-mouse-moved-handler (om-view-window self) 
                                   (om-convert-coordinates pos self (om-view-window self))))
    t)
  

;;; OM MOUSE MOVED EVENT IS HANDLED BY THE TOP LEVEL WINDOW
(defmethod om-window-mouse-moved-handler ((self t) position) 
  (declare (ignore self position)) nil)

(defmethod om-window-mouse-moved-handler ((self om-abstract-window) position) 
  (declare (ignore self position)) t)

(defmethod om-view-mouse-enter-handler ((self om-graphic-object)) t)

(defmethod om-view-mouse-leave-handler ((self om-graphic-object)) t)

;;; tester si pas de pb avec om-view-container
(defmethod update-view-cursor ((self om-view))
  ;(print (list "view" self (om-view-cursor self)))
  (when (om-view-p (om-view-container self))
    (setf (capi::simple-pane-cursor (om-view-container self)) (om-view-cursor self)))
  (setf (capi::simple-pane-cursor self) (om-view-cursor self)))

(defmethod update-view-cursor ((self om-item-view))
  ;(print (list "item" self (item-container self) (om-view-cursor self)))
  (setf (capi::simple-pane-cursor (item-container self)) (om-view-cursor self)))

(defmethod update-view-cursor ((self t)) nil)


;;; version CALLBACK   (MCL style)
(defmethod om-view-cursor ((self om-graphic-object)) nil)

;;; version SETF   (LW style)
(defmethod om-set-view-cursor ((self om-graphic-object) cursor) 
  (setf (capi::simple-pane-cursor (om-get-view self)) cursor))

;;;=====================
;;; KEYBOARD
;;;=====================
        
;;; omchar from character
(defun get-om-character (c)
  (case (char-code c)
    (63276  :om-key-pageup)
    (63277 :om-key-pagedown)
    (63275  :om-key-end)
    (63273  :om-key-home)
    (63234  :om-key-left)
    (63232  :om-key-up)
    (63235 :om-key-right)
    (63233  :om-key-down)
    (127  :om-key-delete)
    (8  :om-key-delete)
    (3  :om-key-enter)
    (13  :om-key-return)
    (27  :om-key-esc)
    (9  :om-key-tab)
    (otherwise c)))


(defmethod om-char-callback ((self om-abstract-window) x y c type)
  (set-meta-keys (eval type))
  (om-with-error-handle 
    (om-view-key-handler self (get-om-character c)))
  (set-pane-focus (car (om-subviews self)))
  )

(defmethod om-char-callback ((self om-view) x y c type)
  (om-char-callback (om-view-window self) x y c type))

;;; omchar from gspec data
(defun get-om-spec-character (c)
  (cond ((integerp c) (get-om-character (code-char c)))
        ((equal :up c) :om-key-up)
        ((equal :down c) :om-key-down)
        ((equal :left c) :om-key-left)
        ((equal :right c) :om-key-right)
        ((equal :next c) :om-key-pagedown)
        ((equal :prior c) :om-key-pageright)
        ((equal :end c) :om-key-end)
        ((equal :home c) :om-key-home)
        ((equal :kp-enter c) :om-key-enter)   
        (t nil)))

(defun get-om-spec-modifiers (mod)
  (case mod
    (0 '(nil nil nil))
    (1 '(t nil nil))  ; SHIFT
    (2 '(nil nil nil))  ; CTRL
    (3 '(t nil nil))  ; CTRL + SHIFT ;; ??? LINUX: TODO, check various key combinations here
    (4 '(nil nil t))  ; ALT
    (5 '(t nil t))  ; ALT + SHIFT
    (6 '(nil nil t))  ; ALT + CTRL
    (7 '(t nil t))  ; ALT + SHIFT + CTRL
    (8 '(nil t nil))  ; CMD ?
    (9 '(t t nil))  ; CMD + SHIFT
    (10 '(nil t nil)) ; CMD + CTRL
    (11 '(t t nil)) ; CMD + CTRL + SHIFT
    (12 '(nil t t)) ; CMD + ALT
    (13 '(t t t)) ; CMD + ALT + SHIFT
    (14 '(nil t t)) ; CMD + ALT + CTRL
    (15 '(t t t)) ; CMD + ALT + CTRL + SHIFT
    (otherwise '(nil nil nil))))

(defmethod om-char-spec-callback ((self om-abstract-window) x y spec)
    ;(print self)
   (let ((data (sys:gesture-spec-data spec))
        (mods (sys:gesture-spec-modifiers spec)))
    ;(print data)
    ;(print mods)
     ; ça ne marche plus...
     ;(capi::set-interface-focus self) 
     (set-meta-keys (get-om-spec-modifiers mods))
     (om-view-key-handler self (get-om-spec-character data))
     (release-meta-keys)
     (capi::set-interface-focus self)
     ))

(defmethod om-char-spec-callback ((self om-view) x y spec)
  ;(print self)
  (om-char-spec-callback (om-view-window self) x y spec)
  (capi::set-pane-focus self))

(defmethod capi::accepts-focus-p ((self om-view))
  (om-view-key-handler (om-view-window self) nil))

;;; MUST return something in order to activate the key handler
(defmethod om-view-key-handler ((self t) key) 
  (declare (ignore key)) 
  nil)


(defvar *om-shift-key-p* nil)
(defvar *om-control-key-p*  nil)
(defvar *om-command-key-p*  nil)
(defvar *om-option-key-p*  nil)

;;; LIST = SHIFT - CMD - ALT
(defun set-meta-keys (list)
  (setf *om-shift-key-p* (first list))
  (setf *om-command-key-p*  (second list) )
  (setf *om-option-key-p*   (third list)))

(defun release-meta-keys ()
  (setf *om-shift-key-p* nil)
  (setf *om-command-key-p*  nil)
  (setf *om-option-key-p*  nil))

(defun om-shift-key-p () *om-shift-key-p* )
(defun om-command-key-p () *om-command-key-p*)
(defun om-option-key-p ()  *om-option-key-p*)

(defun om-control-key-p () nil)



