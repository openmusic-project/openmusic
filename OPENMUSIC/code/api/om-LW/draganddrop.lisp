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
; Authors: Jean Bresson, Carlos Agon, Karim Haddad
;=========================================================================


;;===========================================================================
;DocFile
; DRAG AND DROP
;;===========================================================================

(in-package :om-api)

(export '(
          om-drag-view
          om-drop-view
          om-drag-reference-view
          om-drag-container-view
          om-drag-start
          om-drag-receive
          om-drag-enter-view
          om-drag-leave-view
          om-draw-contents-for-drag
          om-import-files-in-app
          om-drag-string-in-app
          om-drop-callback
          ) :om-api)


;;;==================
;;; DRAG/DROP VIEW
;;;==================

(defclass om-drag-view () 
  ((cursor-pos :initform (om-make-point 0 0) :accessor om-drag-view-cursor-pos)))

(defmethod om-drag-view-p ((self t)) nil)
(defmethod om-drag-view-p ((self om-drag-view)) t)

;;; called before drag: must return T or drag will not start
(defmethod om-drag-start ((self om-drag-view)) t)

(defclass om-drop-view () ()
  (:default-initargs 
   :drop-callback 'om-drop-callback))

(defmethod om-drop-view-p ((self t)) nil)
(defmethod om-drop-view-p ((self om-drop-view)) t)

(defmethod om-draw-contents-for-drag ((self t))
  (om-draw-contents self))

(defmethod om-drag-reference-view ((self t)) self)
(defmethod om-drag-container-view ((self t)) (om-view-container (om-drag-reference-view self)))

(defmethod build-d&d-image ((dragged om-drag-view) pane)
  (let* ((size (om-view-size (om-drag-container-view (om-drag-reference-view dragged))))
         (pp (gp:create-pixmap-port pane
				    ;; 50 50
				    (om-point-x size) (om-point-y size) 
				    :clear t
				    :background :transparent 
				    ;; :background (color:make-rgb 0.8 0.8 0.8 0.9)
				    ))

         (posi (om-subtract-points 
                (om-convert-coordinates (om-drag-view-cursor-pos dragged) dragged (om-drag-container-view dragged))
                (om-scroll-position (om-drag-container-view dragged)))))
    (unwind-protect
	 (progn 
	   (om-with-focused-view pp
	     (gp::set-graphics-port-coordinates pp :left (om-h-scroll-position (om-drag-container-view dragged)) 
						:top (om-v-scroll-position (om-drag-container-view dragged)))
	     (om-draw-contents-for-drag dragged)
	     ;; (cl-user::compositing-mode-simple-example-draw-ellipses pp) ;FIXME:AV
	     )
	   
	   (values (gp:make-image-from-port pp) 
	   	   (om-point-x posi) 
	   	   (round (om-point-y posi)))
	   )
      (gp:destroy-pixmap-port pp)
      )))

(defun internal-drag-start (self)
  (and (om-drag-start self)
       *click-motion-view* ;to be tested
       ;*clicked-view* ;to be tested
       (capi:drag-pane-object  
        ;(om-get-view self)  
        (capi::pane-layout (capi::top-level-interface self))
        self 
        :plist (list :om-object self) ; :string "OM" 
        :operations '(:move :copy)
        :image-function #'(lambda (pane) (build-d&d-image self pane))
        )))

(defmethod om-click-motion-handler :before ((self om-drag-view) pos)
  (unless *click-motion-action* ;; cf. transient-drawing.lisp
    (setf (om-drag-view-cursor-pos self) pos)
    (internal-drag-start self)))

(defvar *last-pinboard-under-mouse* nil)

(defmethod om-drop-callback ((self t) drop-object stage)
  (handler-bind ((error #'abort))
    (flet ((set-effect-for-operation (drop-object)
	     ;; In a real application, this would be clever about which effects to allow.
             (dolist (effect '(:move :copy))
	       (when (capi:drop-object-allows-drop-effect-p drop-object effect)
               	 (setf (capi:drop-object-drop-effect drop-object) effect)
		 (return t)))))
      (case stage
	(:formats
	 (capi:set-drop-object-supported-formats drop-object '(:string :value :om-object :filename-list)))
	(:enter
	 ;; (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
	 ;;  (let ((dropview (or (capi::pinboard-object-at-position self x y)  
	 ;;                      self)))
	 ;;    (set-effect-for-operation drop-object)
	 ;;    (when (and *last-pinboard-under-mouse*
	 ;;               (not (equal dropview *last-pinboard-under-mouse*)))
	 ;;      (om-drag-leave-view *last-pinboard-under-mouse*))
	 ;;    (om-drag-enter-view dropview)
	 ;;    (setf *last-pinboard-under-mouse* dropview)))
	 (set-effect-for-operation drop-object))
	(:leave
	 ;; (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
	 ;;  (let ((dropview (or (capi::pinboard-object-at-position self x y)  
	 ;;                      self)))
	 ;;    (set-effect-for-operation drop-object)
	 ;;    (om-drag-leave-view dropview)))
	 (set-effect-for-operation drop-object))
	(:drag        
	 ;; (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
	 ;;  (let ((dropview (or (capi::pinboard-object-at-position self x y)  
	 ;;                      self)))
	 ;;    (when (and dropview (not (equal dropview *last-pinboard-under-mouse*)))
	 ;;      (when *last-pinboard-under-mouse*
	 ;;        (om-drag-leave-view *last-pinboard-under-mouse*))
	 ;;      (om-drag-enter-view dropview)
	 ;;      (setf *last-pinboard-under-mouse* dropview))
	 ;;     ;(print (capi:drop-object-provides-format drop-object :om-object))
	 ;;    ))
         
	 (set-effect-for-operation drop-object))
	(:drop
	 (multiple-value-bind (x y) (capi::current-pointer-position 
                                     :relative-to self 
                                     :pane-relative-p t
                                     )
	   (capi::current-pointer-position 
            :relative-to self 
            :pane-relative-p t
            )
	   (let ((dropview (or (om-get-real-view (capi::pinboard-object-at-position self x y))
                               self)))
	     (setf *last-pinboard-under-mouse* nil)
            
	     (if (or 
		  (and (capi:drop-object-provides-format drop-object :filename-list)
		       (om-import-files-in-app self (capi:drop-object-get-object drop-object self :filename-list)))
		  (and (capi:drop-object-provides-format drop-object :string)
		       (om-drag-string-in-app self (capi:drop-object-get-object drop-object self :string))))
		 (set-effect-for-operation drop-object)
		 (let ((dragged-view (capi:drop-object-get-object drop-object self :om-object)))
		   (set-effect-for-operation drop-object)
                   
		   (when dragged-view
		     (unless (om-drag-receive
			      dropview dragged-view
			      (om-make-point (- (capi::drop-object-pane-x drop-object) 
						(om-point-x (om-drag-view-cursor-pos dragged-view)))
					     (- (capi::drop-object-pane-y drop-object)
						(om-point-y (om-drag-view-cursor-pos dragged-view))))
			      (capi:drop-object-drop-effect drop-object))
		       (setf (capi:drop-object-drop-effect drop-object) nil)))
		   )))))
          
	))))


(defmethod om-import-files-in-app ((self t) file-list) nil)
(defmethod om-drag-string-in-app ((self t) str) nil)

(defmethod om-drag-receive ((view t) (dragged-view t) position &optional (effect nil))
  (declare (ignore view dragged-view effect)))

(defmethod om-drag-enter-view ((self t)) nil)
(defmethod om-drag-leave-view ((self t)) nil)


