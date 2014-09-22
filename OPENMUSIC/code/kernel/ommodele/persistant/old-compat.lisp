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
;Patch compatibility for OM < 5
;DocFile

(in-package :om)

(defmethod load-picts ((self t)) nil)

(defun load-pict-extra (class idp deltax deltay factx facty) nil)

(defun om-load-tempobj (name inputs refer numouts posx sizex clorf value pict 
                             sizey posy strechfact 
                             &optional (store nil) (params nil) (lock nil))
  (let (reference newtempob maqpos)
    (cond
     ((equal (first refer) 'maq) 
      (setf reference (mk-object-refer 'maquette (second refer))))
     (t (setf reference (eval (second refer)))))
    (when reference
      (setf maqpos (om-make-big-point posx posy))
      (setf newtempob (omNG-make-tempobj reference maqpos name))   
      (setf (numouts newtempob) numouts)
      (setf (inputs newtempob) (mapcar #'(lambda (input) (eval input)) inputs))
      (set-box-to-inputs (inputs newbox) newbox)
      (setf (extend newtempob) sizex)
      (setf (colorframe newtempob) (om-correct-color clorf))
      (setf (free-store newtempob) store)
      (when value
        (setf (value newtempob) (list! value)))
      (setf (slot-value newtempob 'strech-fact)  (or strechfact 1))
      (setf (colorframe newtempob) (om-correct-color clorf))
      (setf (slot-value newtempob 'sizey) sizey)
      (setf (allow-lock newtempob) lock)
      (setf (name newtempob) name)
      (setf (edition-params newtempob) (corrige-edition-params (car (value newtempob)) params))
      newtempob)))

(defun om-load-editor-box (name reference inputs position size value lock 
                                &optional fname editparams spict meditor)

  (when (and (equal reference 'EventMidi-seq) (< *load-version* 4.8))
    (setf reference 'eventmidi-seq-old))
  (let ((dead? (not (find-class reference nil))) newbox)
    (setf newbox (if dead? 
                   (omNG-make-new-boxcall 'dead (om-correct-point position) name)
                   (make-new-EditorCall (find-class reference) (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (name newbox) name)
    (setf (inputs newbox) (correct-box-inputs reference (mapcar #'(lambda (input) (eval input)) inputs)))
    (set-box-to-inputs (inputs newbox) newbox)
    (when (and value (not dead?))
      (setf (value newbox) value))
    (if dead?
      (setf (numouts newbox) (length (inputs newbox))
            (mesage newbox) (string+ "Class " (string reference) " not found, this editor is dead")
            (save-code newbox) 
            `(om-load-editor-box ,name ',reference ',inputs ,(om-save-point (om-correct-point position)) ,(om-save-point (om-correct-point size))
                                 ,(omng-save value t) ,lock  ,fname 
                                 ,(if (listp editparams)
                                    (save-alist editparams) (omng-copy editparams)) ,spict))
      (setf (allow-lock newbox) lock
            (edition-params newbox) (corrige-edition-params (value newbox) editparams)
            (showpict newbox) spict
            minieditor? meditor))
    newbox))


(defun om-load-patch (name boxes connections &optional (fond-ec nil) (version nil) (pictueditors nil))
  (setf *load-version* version)
  (let ((newpatch (omNG-make-new-patch name)))
    (setf (boxes newpatch) nil)
    (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
    (setf (boxes newpatch) (reverse (boxes newpatch)))
    (setf (connec newpatch) (loop for i in connections collect (load-connection i)))
    ;(setf (pictu-list newpatch) (list fond-ec pictueditors))
    (when version
      (setf (omversion newpatch) version))
    (setf *load-version* *om-version*)
    newpatch))

(defun om-load-ominstance (class name icon instance edparams &optional pictlist)
   (let ((copy (make-instance class
                 :name name
                 :icon icon)))
     (setf (instance copy) (eval instance))
     (setf (edition-params copy) edparams)
     ;(setf (pictu-list copy) pictlist)
     copy))

(defun om-load-maq (name boxes connections range markers 
                         &optional (colormaq nil) (metricparam '((4 60) ((4 4)) 16 t))
                         (pictlist nil) (fond-ec nil) (show-connect t) (version nil) (pictueditors nil))
  (setf *load-version* version)
  (let ((newmaquette (omNG-make-new-maquette name)))
    (when (null (nth 3 range))
      (setf range (list+ range (list 0 100))))
    (setf (boxes newmaquette) nil)
    (mapc #'(lambda (box) (omNG-add-element newmaquette (eval box))) boxes)
    (setf (boxes newmaquette) (reverse (boxes newmaquette)))
    (setf (connec newmaquette) (loop for i in connections collect (load-connection i)))
    (remk-markers markers (boxes newmaquette))
    (setf (params newmaquette) (make-instance 'maquette-params 
                                 :range range
                                 :maq-color (om-correct-color colormaq)
                                 :metricparam metricparam
                                 :show-conect show-connect))
    ; (setf (code newmaquette) (list fond-ec pictlist pictueditors))
    (when version
      (setf (omversion newmaquette) version))
    (setf *load-version*  *om-version*)
    newmaquette))

(defmethod om-load-boxwithed ((class t) name reference inputs position size value lock boxes conec numouts 
                                &optional fname pictlist)
   (let ((newbox (omNG-make-new-boxcall (mk-object-refer class reference) (om-correct-point position) name)))
     (setf (frame-size newbox) (om-correct-point size))
     (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
     (set-box-to-inputs (inputs newbox) newbox)
     (setf (value newbox) (if (listp value) value (eval value)))
     (setf (allow-lock newbox) lock)
     (setf (frame-name newbox) fname)
     (setf (boxes (patch newbox)) nil)
     (mapc #'(lambda (box) (omNG-add-element (patch newbox) (eval box)))  boxes)
     (setf (boxes (patch newbox)) (reverse (boxes (patch newbox))))
     (setf (connec (patch newbox)) (loop for i in conec collect (load-connection i)))
     (setf (numouts newbox) numouts)
     ; (setf (pictu-list (patch newbox)) pictlist)
     (push (patch newbox) *loaading-stack*)
     newbox))

(defmethod pictu-list ((self t)) nil)


(defun om-load-maq-abs (name boxes connections range markers 
                              &optional (colormaq nil) (metricparam '((4 60) ((4 4)) 16 t)) (show-connect t) (version nil))
   (let ((newmaquette (make-instance 'OMMaqAbs :name name :icon 265)))
     (setf (boxes newmaquette) nil)
     (mapc #'(lambda (box) (omNG-add-element newmaquette (eval box)))  boxes)
     (setf (boxes newmaquette) (reverse (boxes newmaquette)))
     (setf (connec newmaquette) (loop for i in connections collect (load-connection i)))
     (remk-markers markers (boxes newmaquette))
     (when version
       (setf (omversion newmaquette) version))
     (setf (params newmaquette) (make-instance 'maquette-params 
                                  :range range
                                  :maq-color (om-correct-color colormaq)
                                  :metricparam metricparam
                                  :show-conect show-connect))
     (push newmaquette *loaading-stack*)
     newmaquette))


(defun om-load-patch-abs (name boxes connections &optional (version nil) (pictlist nil))
   "This function is called when you load a saved Abstraction patch."
   (let ((newpatch (make-instance 'OMPatchAbs :name name :icon 210))) 
     (setf (boxes newpatch) nil)
     (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
     (setf (boxes newpatch) (reverse (boxes newpatch)))
     (setf (saved? newpatch) (loop for i in connections collect (load-connection i)))
     (remk-connections (boxes newpatch) connections)
     ; (setf (pictu-list newpatch) pictlist)
     (compile-patch newpatch)
     (when version
       (setf (omversion newpatch) version))
     newpatch))


(defun om-load-temp-patch (name boxes connections &optional (version nil))
   (let ((newpatch (make-instance 'OMPatchAbs :name name)))
     (setf (boxes newpatch) nil)
     (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
     (setf (boxes newpatch) (reverse (boxes newpatch)))
     (setf (connec newpatch) (loop for i in connections collect (load-connection i)))
     (when version
       (setf (omversion newpatch) version))
     (push newpatch *loaading-stack*)
     newpatch))


(defun om-load-method-ws (name icon doc boxes connections lambda-list initvals indocs qualy flag
                                &optional (numouts 1))
   (ignore-error-msg (format nil "The method ~D ~D was not loaded because un error ocurred." name lambda-list)  
     (let* ((existe-genfun (fboundp name))
            (new-method (eval `(defmethod* ,name ,.qualy ,lambda-list
                                 :initvals ',initvals
                                 :indoc ',indocs
                                 :icon ,(car (list! icon))
                                 :doc ,doc
                                 :numouts ,numouts
                                 nil))))
       (setf (graph-fun new-method) boxes)
       (setf (saved-connections new-method) (loop for i in connections collect (load-connection i)))
       (setf (class-method-p new-method) flag)
       (setf (protected-p new-method) nil)
       (unless existe-genfun
         (setf (protected-p (fdefinition name)) nil)
         (when (listp icon)
           (icon-for-user-package (fdefinition name) (second icon))))
       new-method)))


(defun om-load-inputfunmenu (class doc name value items) 
  (make-instance class
                 :doc-string  (str-with-nl doc)
                 :name name
                 :value value
                 :thepopup items))




