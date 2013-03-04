;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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

;;;===============================
;;; GESTION PALETTES
;;;===============================

(defclass general-palette ()
   ((pos-palette :initform (om-make-point 500 10) :accessor pos-palette)
    ;(source-name :initform nil :initarg :source-name :accessor source-name)
    (delta-inpix :initform (om-make-point 0 0) :initarg :delta-inpix :accessor delta-inpix)
    (editor-assoc :initform nil :initarg :editor-assoc :accessor editor-assoc)))

(defclass palette-win (om-windoid)
  ((palette :initform nil :accessor palette)
   (view :initform nil :initarg :view :accessor view) 
   (thepict :initform nil :accessor thepict)))

#+win32(defmethod om-window-check-before-close ((self palette-win)) nil)

;====================================
; PALETTE VIEW
;====================================

(defclass palette-view (om-transparent-view) ())
(defmethod palette-view-class ((self general-palette)) 'palette-view)

(defmethod om-window-resized ((self palette-win) size-point) 
  (when (view self)
    (om-set-view-size (view self) size-point)))

(defmethod om-draw-contents :before ((self palette-view))
  (when *palette*
    (let ((pict (get-palette-pict  (editor-assoc *palette*))))
      (when  pict
        (om-draw-picture self pict (delta-inpix *palette*)))
      (draw-more-palette *palette* self)
      (when (and (editor-assoc *palette*) (mode (editor-assoc *palette*)))
        (push-button self (mode (editor-assoc *palette*))))
      )))


(defmethod get-palette-pict ((self t)) nil)
(defmethod palette-pict-pos ((self t)) 0)
(defmethod draw-more-palette ((self t) view) nil)

(defmethod om-view-click-handler ((self palette-view) where)
    (when (< (om-point-v where) 25)
      (let ((x (floor (om-point-h where) 25)))
        (setf x (click-button-inpal self x)))))

(defmethod om-view-key-handler ((self palette-view) key)
  (handle-key-event (editor-assoc (om-view-window self)) key))

;====================================
; PALETTE MANAGEMENT
;====================================
; (om-close-window *palette-win*)

(defvar *palette* nil)
(defvar *palette-win* nil)

(defmethod window-has-palette-p ((self t)) nil)
(defmethod editor-has-palette-p ((self t)) nil)

(defmethod window-has-palette-p ((self EditorWindow))
  (and (editor self) (editor-has-palette-p (editor self))))

(defmethod palette-close ()
  (when *palette-win* 
    (when *palette*
      (setf (pos-palette *palette*) (om-view-position *palette-win*))
      (setf (editor-assoc *palette*) nil))
    (om-hide-window *palette-win*))
  (mapc #'(lambda (pal) (close-win-palettes pal)) *palettes*))

(defmethod om-window-close-event ((self palette-win))
  (setf *activate-handler* t)
  (setf *palette-win* nil)
  (call-next-method)
  (setf *activate-handler* nil))

(defmethod palette-init ((self t)) nil)

(defmethod get-editor-assoc ((self t)) self)

(defmethod palette-init ((self editorview))
  (unless (equal (get-editor-assoc self) (editor-assoc *palette*))
    (setf *palette* (make-instance (editor-has-palette-p self)))
    (setf (editor-assoc *palette*) (get-editor-assoc self))
    (let ((pict (get-palette-pict  self)))
      (when pict
        (om-set-view-size *palette-win* (om-get-picture-size pict))))))

(defmethod palette-clean ((self t)) nil)

(defmethod palette-open ((win t))
  (unless *palette*  (setf *palette* (make-instance 'general-palette)))
  (unless *palette-win* (create-palette-win))
   (om-show-window *palette-win*))
  
(defun create-palette-win ()
   (setf *palette-win*
         (om-make-window 'palette-win
                         :window-title ""
                         :size (om-make-point 50 25)
                         :position (pos-palette *palette*)
                         ;:close nil 
                         :resizable nil
                         :bg-color *om-window-def-color*
                         ))
   (om-add-subviews *palette-win* 
                    (setf (view *palette-win*) 
                          (om-make-view (palette-view-class *palette*)
                                        :size  (om-make-point 50 25)
                                        :bg-color *om-window-def-color*)))
   *palette-win*)


;;==================
;; SECONDARY PALETTES
;;==================
(defvar *palettes* nil)
(defmethod open-win-palettes ((pal t) editor) nil)
(defmethod close-win-palettes ((pal t)) nil)

;;==================
;; OPEN
;;==================
(defparameter *activate-handler* nil)

(defmethod window-open-palette ((self t))
  (if (window-has-palette-p self)
      (when (not *activate-handler*)
        (setf *activate-handler* t)
        (palette-open self)
        (setf *activate-handler* nil)
        (palette-init (editor self)))
    (palette-close)
    ))

(defmethod om-window-activate ((self EditorWindow) &optional (activatep t))
  (call-next-method)
  (if  activatep
    (window-open-palette self)
    ))

(defmethod om-select-window ((self EditorWindow))
  (call-next-method)
  (setf *activate-handler* nil)
  (window-open-palette self)
  self)

(defmethod (setf selected-p) :after (selected-p (self EditorView))
   (if selected-p
       (progn (palette-open (window self)) (palette-init self))
     (palette-close)))

(defmethod om-window-close-event ((self EditorWindow)) 
  (when (and *palette* (equal (editor-assoc *palette*) (editor self)))
    (palette-close))
  (call-next-method)
  )


;===================================
;==========Buttons in palettes==========

(defmethod push-button ((view palette-view) i)
  (draw-push-button view i))

(defmethod draw-push-button ((view palette-view) i)
  (om-with-focused-view view
    (om-draw-hilite-rect (+ 1 (* 25 i)) 1 24 24 *om-black-color*)))

(defmethod unpush-button ((view palette-view) i)
  (draw-unpush-button view i))

(defmethod draw-unpush-button ((view palette-view) i)
  (om-invalidate-rectangle view (+ 2 (* 25 i)) 2 23 23))

(defun fill-button (i)
  (om-fill-rect (+ 1 (* 25 i)) 1 22 21))

(defvar *palette-mode* nil)

(defmethod click-button-inpal (self mode)
  (push-button self mode)
  (setf *palette-mode* mode)
  (om-init-motion-functions self 'make-push-button 'release-push-button)
  mode)

(defmethod make-push-button ((self palette-view) where)
  ;; cancel push and action if mouse leaves button before release
  (when *palette-mode*
    (let ((r (om-pts-to-rect (om-make-point (+ 1 (* 25 *palette-mode*)) 0)  
                             (om-make-point (+ 1 (* 25 (+ 1 *palette-mode*))) 25))))
      (unless (om-point-in-rect-p where r)
        (unpush-button self *palette-mode*)
        (setf *palette-mode* nil)))))

(defmethod release-push-button ((self palette-view) where)
   (when *palette-mode*
     (palette-act *palette* *palette-mode*)
     (unpush-button self *palette-mode*)))

(defmethod palette-act ((self general-palette) x) 
  (editor-palette-act (editor-assoc self) x))








#|
(defmethod close-editor-before :before ((self EditorView))  
  (palette-close self))

;;; !!!! 
(defmethod palette-open ((self EditorWindow))
  (when (editor self)
    (if (editor-has-palette-p (editor self))
        (progn
          (if (and *palette-win* (palette *palette-win*) 
                   )
              (when (and (om-window-open-p *palette-win*)
                         (not (om-window-visible-p *palette-win*)))
                (om-show-window *palette-win*))
            (editor-open-palette (editor self)))
          ;; pb : on windows the palette comes to front and intercepts focus
          ;; on mac this slows down the window opening
          ;#+win32(om-select-window self)
          )
      (call-next-method))
    (mapc #'(lambda (pal) (open-win-palettes pal (editor self))) *palettes*)
    ))

(defmethod editor-open-palette ((self EditorView))
  (when (editor-has-palette-p self)
    (create-palette-win)
    ;(om-close-window *palette-win*)
    ;(clean-palette (palette *palette-win*))
    (setf (palette *palette-win*)
          (make-instance (editor-has-palette-p self)
                         :editor-assoc self))
    (init-palette (palette *palette-win*))
    ;(om-show-window *palette-win*)
    (unless (om-window-visible-p *palette-win*)
      (om-select-window *palette-win*))
    ;#+win32(om-select-window (om-view-window self))
    ))

(defmethod init-palette ((self general-palette) &key size+)
  (let* ((pict (if (source-name self) (om-load-and-store-picture (source-name self) 'internal)))
         (pictsize (if pict (om-get-picture-size pict) (om-make-point 0 26)))
         (real-size (om-add-points (om-add-points pictsize (delta-inpix self)) (or size+ (om-make-point 0 0)))))
    (when (thepict *palette-win*) (om-kill-picture (thepict *palette-win*)))
    (setf (thepict *palette-win*) pict)
    (om-set-view-size *palette-win* real-size)
    (unless (equal (palette-view-class self) (type-of (view *palette-win*)))
      (when (view *palette-win*) (om-remove-subviews *palette-win* (view *palette-win*)))
     (om-add-subviews *palette-win* (setf (view *palette-win*) 
                                         (om-make-view (palette-view-class self)
                                                       :size  real-size
                                                       :bg-color *om-window-def-color*)))
     )
    t))
|#