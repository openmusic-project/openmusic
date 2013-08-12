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
;This file implements the meta-object OMPatch.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)
   
#|
(defclass OMPatch (OMPersistantObject) 
   ((boxes :initform nil :initarg :boxes :accessor boxes)
    (code :initform (gensym) :accessor code)
    (connec :initform nil  :accessor connec)
    (compiled? :initform nil  :accessor compiled?)
    (pictu-list :initform nil :accessor pictu-list)
    (lisp-exp-p :initform nil :accessor lisp-exp-p)
    (show-connections? :initform t :accessor show-connections?))
   (:documentation "The class of the OM patch metaobjects. #enddoc#
#seealso# (OMPersistantObject OMmaquette OMTemporalPatch OMPatchAbs) #seealso#
#boxes# A list of the boxes which define the patch. #boxes#
#code# A symbol to reference the code definition of the patch. #code#
#connec# Used to save connections beetween boxes in the patch definition. #connec#
#compiled?# Nil if the patch was modified and not compiled.#compiled?#
#pictu-list# This slot has a list of pictures's names pointed to pictures resources in the patch file.#pictu-list#
#lisp-exp-p# Patches can be defined by a lambda expression in place of graphicly,
in this case this slot keeps the lambda expression.#lisp-exp-p#
#show-connections?# Flag to show or hide connections.#show-connections?#")
   (:metaclass omstandardclass))
|# 

;;; compatibility 
(defmethod lisp-exp ((self ompatch)) (lisp-exp-p self))
(defmethod (setf lisp-exp) (exp (self ompatch)) (setf (lisp-exp-p self) exp))

;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------

(defmethod allow-alias ((self OMPatch)) "Patches allow alias, but it is not yet implemented." t)

(defmethod get-elements ((self OMPatch)) "Elements of patches are boxes." (boxes self))

(defmethod get-class-icon ((self OMPatch)) 'patch-icon-frame)


(defmethod obj-file-type ((self OMPatch)) :PATC)
(defmethod obj-file-extension ((self OMPatch)) "omp")

(defmethod get-object-insp-name ((self OMpatch)) "Patch")




(defmethod omNG-add-element ((self OMPatch) elem)
   "Add the box 'elem' to the patch 'self'"
   (when elem
     (setf (mycontainer elem) self)
     (push elem (boxes self))))

(defmethod omng-remove-element ((self OMPatch) elem)
   "Remove the box 'elem' from the patch 'self', see also remove-extra."
   (remove-extra self elem)
   (setf (boxes self) (remove elem (boxes self) :test 'equal)))

(defvar *saving-patch* nil)

(defmethod omng-save :before ((self OMPersistantObject) &optional values)
  (setf (omversion self) *om-version*)
  (when (create-info self) (setf (cadr (create-info self)) (om-get-date))))


(defmethod omNG-save ((self OMPatch) &optional (values? nil))
  "Save the patch <self> in the file <mypathname> if it is not null."
  (if (not (mypathname self))
      (dialog-message "This patch has no associated file, you can not save it.")
    (let ((tempfile (om-put-path-extension (mypathname self) (string+ (obj-file-extension self) ".tmp"))))
      (when (probe-file (mypathname self))
        (rename-file (mypathname self) tempfile))
      (delete-file-protection (mypathname self))
      (unless (string-equal (pathname-type (mypathname self)) (obj-file-extension self))
        (setf (mypathname self)  (om-put-path-extension (mypathname self) (obj-file-extension self))))
      (setf *saving-patch* self)
      (setf *libs-to-load* nil)
      (setf *resources-to-load* nil)
      ;(setf (cadr (create-info self)) (om-get-date))
      (with-open-file (out (mypathname self) :direction :output  
			   :if-does-not-exist :create :if-exists :supersede)
        (handler-bind 
            ((error #'(lambda (err)
                        (capi::display-message "An error of type ~a occurred: ~a~%~%File ~s could not be saved." 
                                               (type-of err) (format nil "~A" err) (mypathname self))
                        (close out)
                        (rename-file tempfile (mypathname self))
                        (setf *saving-patch* nil)
                        (setf *libs-to-load* nil)
                       (setf *resources-to-load* nil)
                        (abort err))))
        (write-header self  out)
        (let ((*package* (find-package :om))
              (patch-code (om-save self values?)))
          (write-resources self *resources-to-load* out)
          (prin1 '(in-package :om) out)
          (prin1 `(load-lib-for ',(remove-duplicates *libs-to-load* :test 'string-equal)) out) 
          (prin1 patch-code out)))
      (setf (saved? self) t)
      (setf *saving-patch* nil)
      (setf *resources-to-load* nil)
      (setf *libs-to-load* nil))
      
      (when (and (mypathname self) (probe-file (mypathname self)))
        (when (editorframe self) (om-set-window-title (window (editorframe self)) (name self)))
        (om-delete-file tempfile)
        (om-print (string+ (namestring (mypathname self)) " saved")))
      t)))



(defvar *loaading-stack* nil "Define a stack used to load patches.")
 

(defmethod get-editor-class ((self OMPatch)) 'PatchEditor)


;;; !!! pictu-list

(defvar *show-version-warning* t)

(defun version-warning (version)
  (if (and (< version 4.99) *show-version-warning*)
    (when
    (om-y-or-n-dialog (format nil "Warning :~%This patch was created with OM ~D.~%It will not be possible anymore to open it with older versions after saving it with OM ~D.~%~%Do you want to upgrade this patch ?" 
                                         version 
                                         *version-string*)
                      :size (om-make-point 400 200)
                      :default-button :yes)
      (setf *loading-old-patches* t))
    t))


(defmethod corrige-version ((self t) (number t))
  "Sometimes change of OM version needs code to upgrade old patches,
put this code in this method."
  (setf (omversion self) *om-version*)
  (print (format nil "~A ~s was upgraded to OM ~D" (type-of self) (name self) *version-string*)) t)

(defmethod load-abstraction-attributes ((self ompatch) currentpersistent)
  (setf (boxes self) nil)
  (setf (connec self) (connec currentpersistent))
  (mapc #'(lambda (box) (omNG-add-element self (eval box))) (reverse (boxes currentpersistent)))
  (setf (pictu-list self) (pictu-list currentpersistent)))


(defmethod load-patch ((self OMPatch))
   "Open the patch editor, this method open too all persistantes objects referenced into the patch."
   (declare (special *om-current-persistent*))
   (unless (loaded? self)
       (om-with-error-handle 
         (setf *loaading-stack* nil)
         (setf *loading-old-patches* nil)
         (setf *om-current-persistent* nil)
         (om-print (string+ "Loading patch: " (namestring (mypathname self))))
         (push self *loaading-stack*)
         (om-with-cursor *om-wait-cursor* 
           (eval-non-text-file (mypathname self))
           (when *om-current-persistent*
             (let ((up? (version-warning (omversion *om-current-persistent*))))
               (if up?
                   (progn
                     (when *om-current-persistent*
                       (setf *om-search-all* nil 
                             *user-search-all* nil
                             *file-search-all* nil)
                       (load-abstraction-attributes self *om-current-persistent*)
                       (when (< (omversion *om-current-persistent*) *om-version*)
                         (corrige-version self (omversion *om-current-persistent*))))
                     (update-patches-pile)
                     (setf *om-current-persistent* nil))
                 (om-abort)))))))
     self)


(defmethod OpenEditorframe ((self OMPatch))
   "Open the patch editor, this method open too all persistantes objects referenced into the patch."
   (declare (special *om-current-persistent*))
   (load-patch self)  
   (or (editorframe self)
       (if (lisp-exp-p self)
         (edit-existing-lambda-expression self)
         (panel (open-new-relationframe self (if (saved? self) (name self) 
                                                 (string+ "^" (name self))) (get-elements self)
                                        )))))

(defmethod set-doc ((self OMPatch) newdoc)
   "Patch doc is limited to 149 chars"
   ;(setf (changed-wsparams? self) t)
   (when (> (length newdoc) 150)
     (setf newdoc (subseq newdoc 0 149)))
   (setf (doc self) newdoc))

;--------------------------------------------------
;Other methods
;--------------------------------------------------

(defmethod patch-p ((self OMPatch)) t)
(defmethod patch-p ((self t)) nil)

(defvar *ready-question-p* nil)

(defmethod omNG-delete ((self OMPatch))
   "Delete a patch and update all attached objects."
   (when (attached-objs self)
     (if (or *ready-question-p*
             (om-y-or-n-dialog (string+ "Warning, some elements are attached to this (these) patch(es), still want to erase?")))
       (progn
         (setf *ready-question-p* t) 
         (mapc #'(lambda (el) (dead-reference el)) (attached-objs self)))
       (om-abort)))
   (mapc #'(lambda (box) (remove-extra self box)) (boxes self))
   (kill-patch-picts self)
   (rem-obj-table self)
   (setf (boxes self) nil) t)

(defmethod get-default-inputs-patch ((self OMPatch))
   "Get the default vallues of all inputs boxes the patch 'self'."
   (let* ((in-boxes (get-patch-inputs self)))
     (mapcar #'(lambda (thein) (defval thein)) in-boxes)))


;;; compile : une fonction a n entrees et m sortrie avec tempin = 1e entree et tempout = 1e sortie
;;; si ils existent
(defmethod compile-patch ((self OMPatch)) 
  "Generation of lisp code from the graphic boxes."
  (unless (compiled? self)
    (if (lisp-exp-p self)
      (compile (eval `(defun ,(intern (string (code self)) :om)
                    ,.(cdr (get-lisp-exp (lisp-exp-p self))))))
      (let* ((boxes (boxes self))
             (out-box (find-class-boxes boxes 'OMout))
             (temp-out-box (find-class-boxes boxes 'OMtempOut))
             (self-boxes (patch-has-temp-in-p self))
             (in-boxes (find-class-boxes boxes 'OMin))
             (out-symb (code self))
             (oldletlist *let-list*) symbols body)
        (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
        (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
        (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
        (setf *let-list* nil)
        (setf body `(values ,.(mapcar #'(lambda (theout)
                                          (gen-code theout 0)) out-box)))
        (eval `(defun ,(intern (string out-symb) :om)  (,.symbols)
                  (let* ,(reverse *let-list*) ,body)))
        
        (setf *let-list* oldletlist)))
    (setf (compiled? self) t)))




(defmethod update-last-saved  ((self t))
   "Close the patch and load the last save version of the patch."
   nil)

(defmethod update-last-saved ((self OMPatch))
   "Close the patch and load the last save version of the patch."
   (when (editorframe self)
     (om-close-window (window (editorframe self))))
   (when (mypathname self)
     (setf (loaded? self) nil)
     (OpenobjectEditor self)))


(defmethod get-patch-inputs ((self OMPatch))
   (if (lisp-exp-p self)
       (progn 
         (unless (compiled? self)
           (compile-lisp-patch-fun self))
         (let* ((args (arglist (intern (string (code self)) :om)))
                (numins (min-inp-number-from-arglist args)) (i -1))
           (mapcar #'(lambda (name) 
                       (make-instance 'omin
                                      :indice (incf i)
                                      :name (string name))) 
                   (subseq args 0 numins))))
     (let* ((boxes (boxes self))
            (inputs-normal (list+ (patch-has-temp-in-p self) (sort (find-class-boxes boxes 'OMin) '< :key 'indice))))
       inputs-normal)))

(defmethod get-patch-outputs ((self OMPatch))
   (if (lisp-exp-p self)
     (list (make-instance 'omout
             :name "lisp function output"
             :indice 0))
     (let* ((boxes (boxes self))
            (outs (list+ (find-class-boxes boxes 'OMtempOut) (sort (find-class-boxes boxes 'OMout) '< :key 'indice))))
       outs)))

(defmethod kill-patch-picts ((self t)) t)
(defmethod kill-patch-picts ((self OMPatch))
   (loop for pic in (pictu-list self) do
             (setf (thepict pic) nil)
             (setf (name pic) nil)
             ))

(defmethod patch-with-temp-in-outs-p ((self OMPatch))
   "T if 'self' do not have temporal I/O boxes."
   (let* ((boxes (boxes self))
          (outs (find-class-boxes boxes 'OMtempOut)))
     (not (null outs))))

(defmethod patch2abs ((self OMPatch))
   "Cons a new instance of 'OMPatchAbs from the patch 'self'."
   (let ((newabs (make-instance 'OMPatchAbs :name (name self)  :icon 210)))
     (if (lisp-exp-p self)
         (setf (lisp-exp-p newabs) (lisp-exp-p self))
       (let ((boxes (boxes self)))
         (loop for item in (reverse (mapcar #'omNG-copy boxes)) do
               (omng-add-element newabs (eval item)))
         (copy-connections boxes (boxes newabs))
         (setf (pictu-list newabs) (mapcar 'copy-picture (pictu-list self))))
       )
       (set-icon-pos newabs (get-icon-pos self))
       (set-win-size newabs (get-win-size self))
       (setf (doc newabs) (doc self))
       newabs))

(defmethod save-all-persistants ((self OMPatch))
   "Save a patch at the end of the session."
   (unless (saved? self) (omng-save self)))


;------------------
;----------Tools

;--------Patch builder   


(defun omNG-make-new-patch (name &optional (posi (om-make-point 0 0)))
   "Make an instance of patch."
   (let ((newpatch (make-instance *def-metaclass-patch* :name name :icon 183)))
     (set-icon-pos newpatch posi)
     newpatch))

;When the patch is loaded in the *loaading-stack* var there is a list of another
;patches and objects loaded this function is called in order to finish the load processus
;for objects in this list.
(defun update-patches-pile ()
   (mapc #'(lambda (patch)
             (mapc #'(lambda (box) (update-from-reference box)) 
                   (attached-objs patch))) *loaading-stack*)
   (mapc #'(lambda (patch) 
             (remk-connections (boxes patch) (connec patch))) *loaading-stack*)
   (mapc #'(lambda (patch) 
                     (compile-patch patch)
                     (setf (loaded? patch) t)) *loaading-stack*)
   (setf *loaading-stack* nil))


;-- only patches and maquettes have pictures...
(defmethod pictu-list ((self t)) nil)


;==========================================
;PATCH IN MAQUETTE
;==========================================

(defmethod add-temp-boxes ((self OMPatch))
   (unless (find-class-boxes (boxes self) 'selfTempIn)
     (omNG-add-element self (make-self-temp-input "self" (om-make-point 90 50))))
   (unless  (find-class-boxes (boxes self) 'OMtempOut)
     (omNG-add-element self (make-new-temp-output "tempout" (om-make-point 220 300)))))

(defmethod patch-temp-p ((self t)) nil)
(defmethod patch-temp-p ((self OMPatch)) (patch-has-temp-in-p self))

(defmethod update-box-reference ((self OMPatch) box)
   (loop for item in (find-class-boxes (boxes self) 'selfTempIn) do
         (setf (defval item) box)))      
      

;==========================================
;PATCH ABSTRACTION (reference of red patches boxes
;==========================================

(defclass OMPatchAbs (OMPatch) 
  ((w-size :accessor w-size :initform (om-make-point 400 500))
   (w-pos :accessor w-pos :initform (om-make-point 200 200)))
   (:documentation "This is the class of the patch references of red patch boxes.
The difference with normal patches is that this patches are not saved in a owner file.
These patches are saved in the patch file which contains the red boxes.
So abstractions or red patches can not be sharing.#enddoc#
#seealso# (OMPatch OMTemporalPatch OMMaqAbs) #seealso#")
   (:metaclass omstandardclass))


(defmethod abspatch-p ((self t)) nil)
(defmethod abspatch-p ((self OMPatchAbs)) t)
;--------------------------------------------------
;Method redefinition of OMPatch
;--------------------------------------------------
(defmethod omNG-save ((self OMPatchAbs) &optional (values? nil))
   "Abstractions or red box's references are saved by the red box."
   (declare (ignore values?))
   (om-beep-msg "This patch is into a patch, try saving the containing patch") nil)
       
(defmethod omNG-copy ((self OMPatchAbs))
   "Not duplication or the mypathname slot"
   (let ((obj (call-next-method)))
     `(let ((copy ,obj))
        (compile-patch copy)
        copy)))

;--------------------------------------------------
;Other Methods
;--------------------------------------------------

(defmethod box-class ((self OMPatchAbs)) 'OMBoxAbsPatch)

(defmethod omNG-make-new-boxcall ((patch OMPatchAbs) posi name)
   "Cons a red box having by reference the patch 'self'."
   (let* ((rep (make-instance (box-class patch) 
                 :name name
                 :reference patch 
                 :icon (icon patch))))
     (setf (inputs rep) 
           (mapcar #'(lambda (input) 
                              (make-instance 'input-funbox
                                :name (or (frame-name input) (name input))
                                :value (eval (defval input))
                                :box-ref rep
                                :doc-string (docu input))) 
                   (get-patch-inputs patch)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs patch))
     rep))

(defmethod set-win-size ((self OMPatchAbs) newsize) 
  (setf (w-size self) newsize))

(defmethod set-win-position ((self OMPatchAbs) newpos) 
  (setf (w-pos self) newpos))

(defmethod OpenEditorframe ((self OMPatchAbs))
   "Open the patch editor, this method open too all persistantes objects referenced into the patch."
   (declare (special *om-current-persistent*))
   (load-patch self)
   (or (editorframe self)
       (if (lisp-exp-p self)
         (edit-existing-lambda-expression self)
         (panel (open-new-relationframe  self (if (saved? self) (name self) 
                                                 (string+ "^" (name self))) (get-elements self)
                                         nil
                                         (w-pos self) (w-size self)
                                         )))))


(defmethod abs2patch ((self OMPatchAbs) name pos)
   "Cons a new instance of 'OMPatch from the abstraction patch 'self'."
  (let ((newabs (omNG-make-new-patch name pos)))
    (if (lisp-exp-p self)
        (setf (lisp-exp-p newabs) (lisp-exp-p self))
      (let ((boxes (boxes self)))
        (loop for item in (reverse (mapcar #'omNG-copy boxes)) do
              (omng-add-element newabs (eval item)))
        (copy-connections boxes (boxes newabs))
        (setf (pictu-list newabs) (mapcar 'copy-picture (pictu-list self)))))
    (set-icon-pos newabs (get-icon-pos self))   
    (set-win-size newabs (get-win-size self))
    (setf (doc newabs) (doc self))
    newabs))



(defmethod om-save ((self OMPatchAbs) &optional (values? nil))
   "Generation of code to save 'self'."
    (if (lisp-exp-p self)
        `(om-load-lisp-abspatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp-p self)))
      (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
            (connectiones (mk-connection-list (boxes self)))
            (doc (str-without-nl (doc self)))
            pictlist)
        (setf pictlist (omng-save (pictu-list self)))
        (when (editorframe self) 
          (set-win-size self (om-interior-size (window (editorframe self))))
          (set-win-position self (om-view-position (window (editorframe self)))))
        `(om-load-patch-abs1 ,(name self) ',boxes ',connectiones ,*om-version* ,pictlist ,doc 
                             ,(om-save-point (w-pos self)) ,(om-save-point (w-size self))))))

(defun om-load-patch-abs1 (name boxes connections 
                                &optional (version nil) (pictlist nil) (doc "") wpos wsize
                                &rest args)
   "This function is called when you load a saved Abstraction patch."
   (let ((newpatch (make-instance 'OMPatchAbs :name name :icon 210))) 
     (setf (boxes newpatch) nil)
     (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
     (setf (boxes newpatch) (reverse (boxes newpatch)))
     (setf (saved? newpatch) connections)
     (remk-connections (boxes newpatch) (loop for i in connections collect (load-connection i)))
     (setf (pictu-list newpatch) pictlist)
     (setf (doc newpatch) (str-with-nl doc))
     (when wpos (setf (w-pos newpatch) wpos))
     (when wsize (setf (w-size newpatch) wsize))
     (compile-patch newpatch)
     (when version
       (setf (omversion newpatch) version))
     newpatch))






