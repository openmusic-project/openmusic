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

;DocFile
;Red patches and red maquettes are defined in this file.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)

;==================THE BOX patch CLASS=======================
(defclass OMBoxAbsPatch (OMBoxPatch) ()
   (:documentation "Boxes having a OMPatchAbs as reference are instances of this class. #enddoc#
#seealso# (OMPatch OMPatchAbs patchboxabsFrame) #seealso#"))

(defmethod get-frame-class ((self OMBoxAbsPatch)) 'patchboxabsFrame)
(defmethod get-box-documentation ((self OMBoxAbsPatch))
  (or (get-documentation (reference self))
      (get-documentation self)))
(defmethod get-documentation ((self OMBoxAbsPatch)) (string+ "This box's reference is an internal " (get-object-insp-name (reference self)) ". It is not a persistant object."))
(defmethod get-object-insp-name ((self OMBoxAbsPatch)) (string+ "Internal " (get-object-insp-name (reference self)) " box"))

;;; pour info window
(defmethod update-close ((self OMBoxAbsPatch) win)
  (set-doc (reference self) (om-dialog-item-text (doc-item win))))

;=====SAVE

(defmethod omNG-save ((self OMBoxAbsPatch) &optional (values? nil))
   "Save the box 'self' and its patch reference in the same file as the patch containing 'self'."
   (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
          (value (omNG-save (value self) values?)))
     `(om-load-boxcall 'abstraction ,(name self) ,(om-save (reference self))
                       ',inputs ,(om-save-point (frame-position self))
                       ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self)
                       )))

(defmethod om-load-boxcall ((self (eql 'abstraction)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numoouts))
  (let ((newbox (omNG-make-new-boxcall reference (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (setf (value newbox) value)
    (setf (allow-lock newbox) lock)
    newbox))

;=============COPY

(defmethod omNG-copy ((self OMBoxAbsPatch))
  `(let ((copy ,(omNG-make-new-boxcall (eval (omng-copy (reference self)))
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     copy))


;===========INTERNALIZE

(defmethod internalize-patch ((self t))
   "A blue patch or maquette  becomes a red one."
   nil)

(defmethod internalize-patch ((self patchboxframe))
   "A blue patch becomes a red patch."
   (when (subtypep (type-of (object self)) 'OMBoxPatch)
     (let* ((container (om-view-container self))
            (object (object self))
            (newpatch (patch2abs (reference object)))
            (newbox (omNG-make-new-boxcall newpatch
                                           (frame-position object)
                                           (mk-unique-name container (name self))))
            frame conec-to-me)
       (setf (frame-position newbox) (borne-position (frame-position object)))
       (setf (frame-size newbox) (frame-size object))
       (setf (frame-name newbox) (frame-name object))
       (setf (allow-lock newbox) (allow-lock object))
       (setf (value newbox) (eval (omNG-copy (value object))))
       (setf (inputs newbox) (eval (omNG-copy (inputs object))))
       (loop for input in (inputs object)
             for in in (inputs newbox) do
             (setf (connected? in) (connected? input)))
       (set-box-to-inputs (inputs newbox) newbox)
       (setf conec-to-me (get-conect-to-me object))
       (loop for item in conec-to-me do
             (change-conections  object item newbox))
       (omg-remove-element container self)
       (setf frame (make-frame-from-callobj newbox))
       (omg-add-element  container frame)
       (compile-patch newpatch)
       (update-graphic-connections frame (get-elements (object container))))))


(defmethod close-frame ((self patchboxframe))
   "Close the 'self' editor if 'self' is a red patch."
  (when (subtypep (type-of (object self)) 'OMBoxAbsPatch)
    (when (EditorFrame (reference (object self)))
      (om-close-window (window (EditorFrame (reference (object self)))))))
  (setf (frames (object self)) nil))

;===========EXTERNALIZE

(defmethod externalize ((self patchboxframe) newpatch) 
   "A red patch becomes a blue patch."
  (when (subtypep (type-of (object self)) 'OMBoxAbsPatch)
    (let* ((container (om-view-container self))
           (object (object self))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (set-box-to-inputs (inputs newbox) newbox)
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections object item newbox))
      (setf frame (make-frame-from-callobj newbox))
      (omg-remove-element container self)
      (compile-patch newpatch)
      (omg-add-element  container frame)
      (update-graphic-connections frame (get-elements (object container)))
      (omng-save newpatch)
      )))



(defun change-conections  (self item newbox)
  (loop for inp in (inputs item) do 
        (let ((connec (connected? inp)))
          (when (and connec (equal (first connec) self))
            (setf (nth 0 (connected? inp)) newbox)))))


(defmethod get-icon-box-class ((self OMBoxPatch)) 'patch-icon-box)

(defclass patch-icon-box (icon-box) ())

;(defmethod om-draw-contents :after ((self patch-icon-box))
;  (when (lisp-exp-p (reference (object (om-view-container self))))
;    (om-with-focused-view self
;      (om-with-fg-color self *om-white-color*
;        (om-with-font *om-default-font1*
;          (om-draw-string 8 20 "lisp"))))))

;==========================================
;MAQUETTE ABSTRACTION
;==========================================

       
;==================THE BOX MAQ CLASS=======================
(defclass OMBoxAbsmaq (OMBoxmaquette) ()
   (:documentation "Boxes having a OMMaqAbs as reference are instances of this class. #enddoc#
#seealso# (OMMaquette OMMaqAbs maquetteabsframe) #seealso#"))

(defmethod get-frame-class ((self OMBoxAbsmaq)) 'maquetteabsframe)
(defmethod get-documentation ((self OMBoxAbsmaq)) "Build, modify or computes an internal Maquette.")
(defmethod get-box-documentation ((self OMBoxAbsmaq))
  (or (get-documentation (reference self))
      (get-documentation self)))
(defmethod get-object-insp-name ((self OMBoxAbsmaq)) "Internal Maquette Box")

;;; pour info window
(defmethod update-close ((self OMBoxAbsMaq) win)
  (set-doc (reference self) (om-dialog-item-text (doc-item win))))



;=====SAVE


(defmethod omNG-save ((self OMBoxAbsmaq) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (omNG-save (value self) values?)))
    (if (= (mode self) 1)
      `(let ((box (om-load-boxcall 'maqabs ,(name self) ,(om-save (reference self))
                      ',inputs ,(om-save-point (frame-position self))
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self)
                      )))
         (change-mode box 1 t)
         box)
      `(om-load-boxcall 'maqabs ,(name self) ,(om-save (reference self))
                      ',inputs ,(om-save-point (frame-position self))
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self)
                      ))
    ))


(defun om-load-maq-abs1 (name boxes connections range markers 
                              &optional (colormaq nil) (metricparam '((4 60) ((4 4)) 16 t)) (show-connect t) (version nil) fond-ec
                              (doc "")
                              show-metric xparam yparam
                              evalfunc
                              wpos wsize
                              &rest args)
   (let ((newmaquette (make-instance 'OMMaqAbs :name name :icon 265)))
     (setf (boxes newmaquette) nil)
     (mapc #'(lambda (box) (omNG-add-element newmaquette (eval box)))  boxes)
     (setf (boxes newmaquette) (reverse (boxes newmaquette)))
     (setf (connec newmaquette) connections)
     (remk-markers markers (boxes newmaquette))
     (when fond-ec (setf (pictu newmaquette) fond-ec))
     (when version (setf (omversion newmaquette) version))
     (setf (doc newmaquette) (str-with-nl doc))
     (setf (params newmaquette) (make-instance 'maquette-params 
                                  :range range
                                  :maq-color (om-correct-color colormaq)
                                  :metricparam metricparam
                                  :show-ruler-metric show-metric
                                  :show-conect show-connect))
     (when yparam
       (setf (yparam (params newmaquette)) yparam))
     (when xparam
       (setf (xparam (params newmaquette)) xparam))
     (when evalfunc
       (setf (eval-func newmaquette) evalfunc))
     (when wpos (setf (w-pos newmaquette) wpos))
     (when wsize (setf (w-size newmaquette) wsize))
     (push newmaquette *loaading-stack*)
     newmaquette))

(defmethod om-load-boxcall ((self (eql 'maqabs)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (let ((newbox (omNG-make-new-boxcall reference (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (setf (value newbox) value)
    (setf (allow-lock newbox) lock)
    newbox))

;=============COPY

(defmethod omNG-copy ((self OMBoxAbsmaq))
  `(let ((copy ,(omNG-make-new-boxcall (eval (omng-copy (reference self)))
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     ,(if (= (mode self) 1) `(change-mode copy 1 t))
     copy))



;===========INTERNALIZE


(defmethod internalize-patch ((self maquetteframe))
  "A blue maquette becomes a red one"
  (when (equal (type-of (object self)) 'OMBoxMaquette)
    (let* ((container (om-view-container self))
           (object (object self))
           (newpatch (maq2abs (reference object)))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections  object item newbox))
      (omg-remove-element  container self)
      (setf frame (make-frame-from-callobj newbox))
      (omg-add-element  container frame)
      (update-graphic-connections frame (get-elements (object container))))))


(defmethod close-frame ((self maquetteframe))
  "Close the 'self' editor if 'self' is a red maquette."
  (when (equal (type-of (object self)) 'OMBoxAbsmaq)
    (when (EditorFrame (reference (object self)))
      (om-close-window (window (EditorFrame (reference (object self)))))))
  (setf (frames (object self)) nil))


;===========EXTERNALIZE

(defmethod externalize ((self maquetteframe) newpatch)
  "A red maquette becomes a blue one." 
  (when (equal (type-of (object self)) 'OMBoxAbsmaq)
    (let* ((container (om-view-container self))
           (object (object self))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections  object item newbox))
      (setf frame (make-frame-from-callobj newbox))
      (omg-remove-element  container self)
      ;(compile-patch newpatch)
      (omg-add-element  container frame)
      (update-graphic-connections frame (get-elements (object container)))
      (omng-save newpatch)
      )))



