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

;DocFile
;A class to inspect graphic objects is defined here.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defun apply-win (win fun &rest args)
  (let ((controls (get-actives  (panel win))))
    (loop for item in controls do
          (eval `(funcall ',fun ,item ,.args)))))

(defmethod get-object-insp-name ((self t))
  "Get a string to describe the class" "????")

(defmethod get-info-window ((self t)) (om-beep))

(defmethod get-obj-for-info ((self t)) nil)

(defmethod get-obj-for-info ((self icon-finder))
  (object self))

(defmethod get-obj-for-info ((self omboxframe))
  (object self))

(defmethod get-obj-for-info ((self instboxframe))
  (reference (object self)))

;==============================================

(defclass info-window (om-dialog)
  ((sujet :initform nil :initarg :sujet :accessor sujet)
   (doc-item :initform nil :initarg :doc-item :accessor doc-item)
   (cp-item :initform nil :accessor cp-item)))

(defmethod om-window-class-menubar ((self info-window))
  (list (om-make-menu "File"
                      (list (om-new-leafmenu "Close" #'(lambda () (om-close-window self)) "w")
                            (om-new-leafmenu "Close All" #'(lambda () (close-all-info self)) "z")))
        (om-make-menu "Edit" (list 
                              (list (om-new-leafmenu "Cut" #'(lambda () (cut self)) "x")
                                    (om-new-leafmenu "Copy" #'(lambda () (copy self)) "c") 
                                    (om-new-leafmenu "Paste" #'(lambda () (paste self)) "v"))
                              (list (om-new-leafmenu "Set" #'(lambda () (update-close (sujet self) self)) "s"))
                                   
                                   ))
        (make-om-menu 'windows :editor self)))

(defmethod om-window-close-event :after ((self info-window))
  (update-close (sujet self) self)
  (setf (infowin (sujet self)) nil))

(defmethod close-all-info ((self info-window))
  (mapc 'om-close-window (om-get-all-windows 'info-window)))

(defmethod cut ((self info-window)) 
  (if (cp-item self) (om-cut-command (cp-item self)) (om-beep)))
(defmethod copy ((self info-window)) 
  (if (cp-item self) (om-copy-command (cp-item self)) (om-beep)))
(defmethod paste ((self info-window)) 
  (if (cp-item self) (om-paste-command (cp-item self)) (om-beep)))

(defmethod update-close ((self t) win)
  (when (doc-item win)
    (set-doc self (om-dialog-item-text (doc-item win)))))


(defmethod show-general-info-window ((self t) &optional (i 0))
  (if (infowin self)
    (om-select-window (infowin self))
    (let* ((info (get-info-components self))
           (pos (if i (om-make-point (+ 30 (* (mod i 4) 250)) (+ 50 (* (floor i 4) 40))) (om-make-point 30 50)))
           (dialog (om-make-window 'info-window
                                   :position pos
                                   :sujet self
                                   :size (car info)
                                   :resizable nil :minimize nil :maximize nil
                                   :window-title (string+ (string-upcase (name self)) " Info")
                                   :font *om-default-font2* 
                                   )))
      (eval `(om-add-subviews ,dialog ,.(cdr info)))
      (setf (doc-item dialog) (nth 1 info))
      (setf (cp-item dialog) (nth 4 info))
      
      (setf (infowin self) dialog)
      (om-add-menu-to-win dialog)
      (om-select-window dialog)
      (om-invalidate-view dialog)
      )))

(defmethod show-info-window ((self OMBasicObject) &optional (i 0))
  (show-general-info-window self i))

(defmethod show-info-window ((self OMFuncallableBasicObject) &optional (i 0))
  (show-general-info-window self i))


;----------------- Info Window Components -----------------

;;; position doc-item &rest
(defmethod get-info-components ((self t))
  (list (om-make-point 200 50) nil))

;;; UNPROTECTED free doc / icon + dates
(defmethod user-unprotected-info-components ((self t) &optional (taille (om-make-point 255 320)))
     
  (list taille
            
        (om-make-dialog-item 'om-text-edit-view (om-make-point 10 160) (om-make-point 225 100)
                             (get-documentation self) 
                             :scrollbars :v)
        
        (om-make-view 'button-icon
                      :iconID (icon self)
                      :position (om-make-point 10 10)
                      :size (om-make-point 32 32)
                      :action  #'(lambda (item) 
                                   (omg-change-icon self nil)
                                   (setf (iconID item) (icon self))))
        
        (om-make-dialog-item 'om-static-text (om-make-point 80 5) (om-make-point 225 20) 
                             "Name:"
                             :font *om-default-font2* 
                             )
        (om-make-dialog-item 'om-static-text (om-make-point 140 5) (om-make-point 300 20)
                             (name self)
                             :font *om-default-font2b*)
        (om-make-dialog-item 'om-static-text (om-make-point 80 30) (om-make-point 60 20) 
                             "Type:" 
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 140 30) (om-make-point 72 20)  (get-object-insp-name self)
                             :font *om-default-font2*)
        
        (om-make-view 'bar-item :position (om-make-point 5 60) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
  
        (om-make-dialog-item 'om-static-text (om-make-point 10 70) (om-make-point 225 20) 
                             "Created:"
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 80 70) (om-make-point 300 20)
                             (or (car (create-info self)) "?")
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 10 95) (om-make-point 225 20) 
                             "Modified:"
                             :font *om-default-font2*)
        
        (om-make-dialog-item 'om-static-text (om-make-point 80 95) (om-make-point 300 20)
                             (or (cadr (create-info self)) "?")
                             :font *om-default-font2*)
        
        (om-make-view 'bar-item :position (om-make-point 5 125) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
        
        (om-make-dialog-item 'om-static-text (om-make-point 10 130) (om-make-point 200 18)
                             "Documentation/Comments" 
                             :font *om-default-font2*)
        ))

;;; UNPROTECTED = free doc / icon  no dates
(defmethod om-unprotected-info-components ((self t) &optional (taille (om-make-point 255 300)))
   
  (list taille 
            
        (om-make-dialog-item 'om-text-edit-view (om-make-point 10 135) (om-make-point 225 120)
                             (get-documentation self) 
                             :scrollbars :v)
        
        (om-make-view 'button-icon
                      :iconID (icon self)
                      :position (om-make-point 10 10)
                      
                      :size (om-make-point 32 32)
                      :action  #'(lambda (item) 
                                   (omg-change-icon self nil)
                                   (setf (iconID item) (icon self))))
        
         (om-make-dialog-item 'om-static-text (om-make-point 80 5) (om-make-point 225 20) 
                                                    "Name:"
                                                    
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 80 26) (om-make-point 225 20) 
                                                    (name self)
                                                    
                                                    :font *om-default-font2b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 54) (om-make-point 60 20) "Type:"
                                                     :font *om-default-font2*)
         (om-make-dialog-item 'om-static-text (om-make-point 60 54) (om-make-point 140 20) (get-object-insp-name self)
                                                    :font *om-default-font2*)
         
         (om-make-view 'bar-item :position (om-make-point 5 80) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
        
        (om-make-dialog-item 'om-static-text (om-make-point 10 100) (om-make-point 200 18)
                             "Documentation/Comments" 
                             :font *om-default-font2*)
        ))

; static icon / doc + dates
(defmethod user-protected-info-components ((self t) &optional (taille (om-make-point 255 320)))
  
   (list taille
         
         (om-make-dialog-item 'om-static-text (om-make-point 10 170) (om-make-point 230 120) 
                              (string-until-CR (or (get-documentation self) ""))
                              :font *om-default-font2*
                              :bg-color *om-white-color*)
         
         (om-make-view 'button-icon
                       :iconID (icon self)
                       :position (om-make-point 10 10)
                       :size (om-make-point 32 32)
                       
                       :action  #'(lambda (item) (declare (ignore item)) t))

         (om-make-dialog-item 'om-static-text (om-make-point 80 5) (om-make-point 225 20) 
                                                    "Name:"
                                                    
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 80 26) (om-make-point 225 20) 
                                                    (name self)
                                                    
                                                    :font *om-default-font2b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 54) (om-make-point 60 20) "Type:"
                                                     :font *om-default-font2*)
         (om-make-dialog-item 'om-static-text (om-make-point 60 54) (om-make-point 140 20) (get-object-insp-name self)
                                                    :font *om-default-font2*)
         
         (om-make-view 'bar-item :position (om-make-point 5 80) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
         
         (om-make-dialog-item 'om-static-text (om-make-point 10 85) (om-make-point 225 20) 
                             "Created:"
                             
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 80 85) (om-make-point 300 20)
                             (or (car (create-info self)) "?")
                             
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 10 110) (om-make-point 225 20) 
                             "Modified:"
                             
                             :font *om-default-font2*)
        
        (om-make-dialog-item 'om-static-text (om-make-point 80 110) (om-make-point 300 20)
                             (or (cadr (create-info self)) "?")
                             
                             :font *om-default-font2*)
         
        (om-make-view 'bar-item :position (om-make-point 5 135) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)

         (om-make-dialog-item 'om-static-text (om-make-point 10 145) (om-make-point 130 18) 
                              "Documentation:"  :font *om-default-font2*)
         ))

;;; PROTECTED = static doc / icon   no dates
(defmethod om-protected-info-components ((self t) &optional (taille (om-make-point 255 300)))
  
  (list taille
         
        (om-make-dialog-item 'om-static-text (om-make-point 15 115) (om-make-point 220 155) 
                             ;(or (string-until-CR (get-documentation self)) "")
                             (or (get-documentation self) "")
                             :font *om-default-font1*
                             :bg-color *om-white-color*)
         
        (om-make-view 'button-icon
                      :iconID (icon self)
                      :position (om-make-point 10 10)
                      :size (om-make-point 32 32)
                       
                      :action  #'(lambda (item) (declare (ignore item)) t))

        (om-make-dialog-item 'om-static-text (om-make-point 80 5) (om-make-point 225 20) 
                             "Name:"
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 80 26) (om-make-point 225 20) 
                             (name self)
                             :font *om-default-font2b*)
        (om-make-dialog-item 'om-static-text (om-make-point 10 54) (om-make-point 60 20) "Type:"
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 80 54) (om-make-point 140 20) (get-object-insp-name self)
                             :font *om-default-font2*)
         
        (om-make-view 'bar-item :position (om-make-point 5 80) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
        (om-make-dialog-item 'om-static-text (om-make-point 15 88) (om-make-point 130 18) 
                             "Documentation:"  :font *om-default-font2*)
        ))

(defmethod update-close ((self omboxeditcall) win)
  (when (car (frames self)) (om-invalidate-view (car (frames self)))))

(defmethod get-info-components ((self omboxeditcall))
  (let ((action (om-dialog-item-act item
                  (setf (name self) (om-dialog-item-text item))
                  (om-set-window-title (infowin self) (string+ (name self) " Info"))
                  ;(set-name (value self) (name self))
                  (when (editorframe self) 
                    (om-set-window-title (window (editorframe self)) (name self)))
                  (om-invalidate-view (car (frames self))))))
    
    (list (om-make-point 255 280)
         
          (om-make-dialog-item 'om-static-text (om-make-point 15 130) (om-make-point 220 120) 
                               (string-until-CR (or (get-documentation (reference self)) ""))
                               :font *om-default-font2*
                               :bg-color *om-white-color*)
          
          (om-make-view 'button-icon
                        :iconID (icon self)
                        :position (om-make-point 10 10)
                        :size (om-make-point 32 32)
                        
                        :action  #'(lambda (item) (declare (ignore item)) t))
          
          (om-make-dialog-item 'om-static-text (om-make-point 80 5) (om-make-point 225 20) 
                               "Name:"
                               
                               :font *om-default-font2*)
          
          (om-make-dialog-item 'om-editable-text (om-make-point 80 28)
                               (om-make-point 150 20)
                               (name self)
                               :after-action action 
                               :di-action action)
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 55) (om-make-point 60 20) "Type:"
                                :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 60 55) (om-make-point 140 40) (string+ "Editor/Factory for class " (name (reference self)))
                                :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 100) :size (om-make-point 235 2)
                        :fg-color *om-gray-color*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 15 108) (om-make-point 130 18) 
                               "Documentation:"  :font *om-default-font2*)
          )
    ))


(defmethod get-info-components ((self ombasicobject))
  (if (protected-p self)
      (om-protected-info-components self)
    (user-unprotected-info-components self)))

(defmethod get-info-components ((self omfuncallablebasicobject))
   (if (protected-p self)
      (om-protected-info-components self)
     (om-unprotected-info-components self)))

(defmethod get-info-components ((self ommethod))
  (if (protected-p self)
      (om-protected-info-components self)
    (user-protected-info-components self)))

(defmethod get-info-components ((self OMBasictype))
   (om-protected-info-components self))

(defmethod get-info-components ((self OMBox))
  (om-protected-info-components self))

(defmethod get-info-components ((self OMboxclass))
   (let ((class (find-class (reference self))))
     (if (protected-p class)
       (om-protected-info-components class)
       (user-unprotected-info-components class))))

(defmethod get-info-components ((self OMboxalias))
   (get-info-components (reference self)))

(defmethod get-info-components ((self OMslot))
   (let ((class (find-class (classname self))))
     (if (protected-p class)
       (om-protected-info-components self)
       (om-unprotected-info-components self))))

(defmethod get-info-components ((self ompackage))
   (if (protected-p self)
     (om-protected-info-components self)
     (call-next-method)))

(defmethod get-info-components ((self omglobalsfolder))
   (om-protected-info-components self))

(defvar *om-patch-lispbox-disable* "A variable to set to nil to avoid the check box action to be called")
(setf *om-patch-lispbox-disable* nil)

(defmethod get-info-components ((self ompatch))
  (if (equal (type-of self) 'ompatch)
      (list (om-make-point 255 285) 
            
               (om-make-dialog-item 'om-text-edit-view (om-make-point 10 160) (om-make-point 225 100)
                                    (get-documentation self) 
                                    :scrollbars :v)
               
               (om-make-view 'button-icon
                             :iconID (icon self)
                             :position (om-make-point 10 10)
                             
                             :size (om-make-point 32 32)
                             :action  #'(lambda (item) 
                                          (omg-change-icon self nil)
                                          (setf (iconID item) (icon self))))
               
               (om-make-dialog-item 'om-static-text (om-make-point 60 5) (om-make-point 225 20) 
                                    "Name:"
                                    
                                    :font *om-default-font2*)
               (om-make-dialog-item 'om-static-text (om-make-point 120 5) (om-make-point 300 20)
                                    (name self)
                                    
                                    :font *om-default-font2b*)
               (om-make-dialog-item 'om-static-text (om-make-point 60 30) (om-make-point 60 20) 
                                    "Type:"   :font *om-default-font2*)
               (om-make-dialog-item 'om-static-text (om-make-point 120 30) (om-make-point 72 20) (get-object-insp-name self)
                                     :font *om-default-font2*)
               
               (om-make-view 'bar-item :position (om-make-point 5 60) :size (om-make-point 235 2)
                             :fg-color *om-gray-color*)
               
               #|
               (om-make-dialog-item 'om-check-box 
                                    (om-make-point 70 68) (om-make-point 100 18) "  Lisp Code"
                                    :font *om-default-font2*
                                    :di-action (om-dialog-item-act item
                                                 (unless *om-patch-lispbox-disable*
                                                   (if   (om-y-or-n-dialog (format nil "Switching from Lisp code to graphic program or from graphic program to Lisp code will erase the current patch contents.~%~%Are you sure ?"))
                                                       (progn
                                                         (when (editorframe self)
                                                           (om-close-window (window (editorframe self))))
                                                         (setf (lisp-exp-p self) (not (lisp-exp-p self)))
                                                         (when (om-checked-p item)
                                                           (edit-new-lambda-expression self)))
                                                     (progn
                                                       (setf *om-patch-lispbox-disable* t)
                                                       (om-set-check-box item (not (om-checked-p item)))
                                                       (setf *om-patch-lispbox-disable* nil))
                                                     )))
                                    :checked-p (lisp-exp-p self)
                                    )
               |#
               
               ;(om-make-view 'bar-item :position (om-make-point 5 95) :size (om-make-point 235 2)
               ;              :fg-color *om-gray-color*)
               
          (om-make-dialog-item 'om-static-text (om-make-point 10 70) (om-make-point 225 20) 
                                                    "Created:"
                                                    
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 80 70) (om-make-point 300 20)
                                                     (or (car (create-info self)) "?")
                                                     
                                                     :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 95) (om-make-point 225 20) 
                                                    "Modified:"
                                                    
                                                    :font *om-default-font2*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 80 95) (om-make-point 300 20)
                                                     (or (cadr (create-info self)) "?")
                                                     
                                                     :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 125) :size (om-make-point 235 2)
                        :fg-color *om-gray-color*)

          (om-make-dialog-item 'om-static-text (om-make-point 10 130) (om-make-point 200 18)
                                                     "Documentation/Comments" 
                                                     :font *om-default-font2*)
          )
     (call-next-method)))

(defmethod get-info-components ((self OMInstance))
  (list (om-make-point 255 300) 
            
        (om-make-dialog-item 'om-text-edit-view (om-make-point 10 175) (om-make-point 225 100)
                             (get-documentation self) 
                             :scrollbars :v)
        
        (om-make-view 'button-icon
                        :iconID (icon self)
                        :position (om-make-point 10 10)
                        :size (om-make-point 32 32)
                        
                        :action  #'(lambda (item) (declare (ignore item)) t))
        
        (om-make-dialog-item 'om-static-text (om-make-point 80 5) (om-make-point 225 20) 
                             "Name:"
                             
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 130 3) (om-make-point 300 20)
                             (name self)
                             
                             :font *om-default-font3b*)
        
        (om-make-dialog-item 'om-static-text (om-make-point 80 30) (om-make-point 120 40)  (string+ "Instance of class " (string (type-of (instance self))))
                              :font *om-default-font2*)
        
        (om-make-view 'bar-item :position (om-make-point 5 80) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
  
        (om-make-dialog-item 'om-static-text (om-make-point 10 90) (om-make-point 225 20) 
                             "Created:"
                             
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 80 90) (om-make-point 300 20)
                             (or (car (create-info self)) "?")
                             
                             :font *om-default-font2*)

        (om-make-dialog-item 'om-static-text (om-make-point 10 115) (om-make-point 225 20) 
                             "Modified:"
                             
                             :font *om-default-font2*)
        
        (om-make-dialog-item 'om-static-text (om-make-point 80 115) (om-make-point 300 20)
                             (or (cadr (create-info self)) "?")
                             
                             :font *om-default-font2*)
        
        (om-make-view 'bar-item :position (om-make-point 5 145) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
        
        (om-make-dialog-item 'om-static-text (om-make-point 10 150) (om-make-point 200 18)
                             "Documentation/Comments" 
                             :font *om-default-font2*)
        ))

(defmethod get-info-components ((self OMBoxInstance))
  (get-info-components (reference self)))

;;; BOX PATCH / MAQUETTE

(defmethod get-info-components ((self OMBoxAbsPatch))
  (list (om-make-point 255 265)
         (om-make-dialog-item (if (mypathname (reference self)) 'om-static-text 'om-text-edit-view)
                              (om-make-point 10 160) (om-make-point 230 80)
                              (get-documentation (reference self))
                              :font *om-default-font2* :bg-color *om-white-color*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 12) (om-make-point 70 20) "Name : " 
                                                     :font *om-default-font2*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 80 12) (om-make-point 120 20) 
                               (name self) :font *om-default-font2b*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 42) (om-make-point 60 20) "Type : " 
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 70 42) (om-make-point 140 20) (get-object-insp-name self) 
                               :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 72) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
          (om-make-dialog-item 'om-static-text (om-make-point 10 78) (om-make-point 230 60) 
                               (get-documentation self)
                               :font *om-default-font1*)
          
          (om-make-view 'bar-item :position (om-make-point 5 124) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 130) (om-make-point 200 20) 
                               (if (mypathname (reference self)) "Documentation:"  "Comments/Documentation:")
                                                    :font *om-default-font2*)
         
         ))


(defmethod get-info-components ((self box-with-patch))
  (list (om-make-point 255 265)
         (om-make-dialog-item 'om-text-edit-view
                              (om-make-point 10 160) (om-make-point 230 80)
                              (get-documentation (patch self))
                              :font *om-default-font2* :bg-color *om-white-color*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 12) (om-make-point 70 20) "Name : " 
                                                     :font *om-default-font2*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 80 12) (om-make-point 120 20) 
                               (name self) :font *om-default-font2b*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 42) (om-make-point 60 20) "Type : " 
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 70 42) (om-make-point 140 20) (get-object-insp-name self) 
                               :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 72) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
          (om-make-dialog-item 'om-static-text (om-make-point 10 78) (om-make-point 230 60) 
                               (get-documentation self)
                               :font *om-default-font1*)
          
          (om-make-view 'bar-item :position (om-make-point 5 124) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
  
          (om-make-dialog-item 'om-static-text (om-make-point 10 130) (om-make-point 200 20) 
                               "Comments/Documentation:"
                               :font *om-default-font2*)
          
         ))

(defmethod update-close ((self box-with-patch) win)
  (set-doc (patch self) (om-dialog-item-text (doc-item win))))



(defmethod get-info-components ((self OMBoxMaquette))
  (patch-info-components self))
(defmethod get-info-components ((self OMBoxPatch))
  (patch-info-components self))


(defmethod patch-info-components ((self OMBoxCall))
  (list (om-make-point 255 270)
         (om-make-dialog-item (if (mypathname (reference self)) 'om-static-text 'om-text-edit-view)
                              (om-make-point 10 164) (om-make-point 230 80)
                              (get-documentation (reference self))
                              :font *om-default-font2* :bg-color *om-white-color*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 12) (om-make-point 70 20) "Name : " 
                                                     :font *om-default-font2*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 80 12) (om-make-point 120 20) 
                               (name self) :font *om-default-font2b*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 42) (om-make-point 60 20) "Type : " 
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 70 42) (om-make-point 140 20) (get-object-insp-name self) 
                               :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 72) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
          (om-make-dialog-item 'om-static-text (om-make-point 10 78) (om-make-point 230 60) 
                               (get-documentation self)
                               :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 132) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)

          (om-make-dialog-item 'om-static-text (om-make-point 10 140) (om-make-point 200 20) 
                               (if (mypathname (reference self)) "Documentation:"  "Comments/Documentation:")
                                                    :font *om-default-font2*)
          
         ))



;;; SPECIAL MAQUETTE


(defclass info+offset-window (info-window)
  ((posx :initform nil :initarg :posx :accessor posx)
   (changes :initform nil  :accessor changes)
   (name-info :initform nil  :accessor name-info)
   (offset-info :initform nil  :accessor offset-info)
   (posy-info :initform nil  :accessor posy-info)
   (dur-info :initform nil  :accessor dur-info)
   (sizey-info :initform nil  :accessor sizey-info)))

;editing shortcuts

(defmethod cut ((self info-window)) 
(cond 
   ((equal (name-info self) oa::*edited-self*)
    (om-cut-command (name-info self)))
   ((equal (offset-info self) oa::*edited-self*)
    (om-cut-command (offset-info self)))
   ((equal (posy-info self) oa::*edited-self*)
    (om-cut-command (posy-info self)))
   ((equal (dur-info self) oa::*edited-self*)
    (om-cut-command (dur-info self)))
   ((equal (sizey-info self) oa::*edited-self*)
    (om-cut-command (sizey-info self)))
   ((equal (cp-item self) oa::*edited-self*)
    (om-cut-command (cp-item self)))
   (t (om-beep))))


(defmethod copy ((self info-window)) 
(cond 
   ((equal (name-info self) oa::*edited-self*)
    (om-copy-command (name-info self)))
   ((equal (offset-info self) oa::*edited-self*)
    (om-copy-command (offset-info self)))
   ((equal (posy-info self) oa::*edited-self*)
    (om-copy-command (posy-info self)))
   ((equal (dur-info self) oa::*edited-self*)
    (om-copy-command (dur-info self)))
   ((equal (sizey-info self) oa::*edited-self*)
    (om-copy-command (sizey-info self)))
   ((equal (cp-item self) oa::*edited-self*)
    (om-copy-command (cp-item self)))
   (t (om-beep))))


(defmethod paste ((self info+offset-window)) 
(cond 
   ((equal (name-info self) oa::*edited-self*)
    (om-paste-command (name-info self)))
   ((equal (offset-info self) oa::*edited-self*)
    (om-paste-command (offset-info self)))
   ((equal (posy-info self) oa::*edited-self*)
    (om-paste-command (posy-info self)))
   ((equal (dur-info self) oa::*edited-self*)
    (om-paste-command (dur-info self)))
   ((equal (sizey-info self) oa::*edited-self*)
    (om-paste-command (sizey-info self)))
   ((equal (cp-item self) oa::*edited-self*)
    (om-paste-command (cp-item self)))
   (t (om-beep))))


(defmethod update-close ((self TemporalBox) win)
  (call-next-method)
  (let ((items (posx win))
        (changs (remove-duplicates (changes win)))
        (oldextend (extend self)))
    (loop for item in changs do
          (case item
            (0  (setf (offset self) (read-info-metric? self (read-from-string (om-dialog-item-text (first items))))))
            (1 (setf (posy self) (read-from-string (om-dialog-item-text (second items)))))
            (2 (setf (strech-fact self) (/ (read-from-string (om-dialog-item-text (third items))) oldextend)))
            (3 (setf (sizey self) (read-from-string (om-dialog-item-text (fourth items)))))
            (4 (setf (name self) (om-dialog-item-text (fifth items)))
               ;;; rename reference
               (omng-rename (reference self) (name self))
               (when (car (frames self)) (om-invalidate-view (car (frames self)))))
            ))))

(defmethod pos-info-metric? ((self TemporalBox) val)
  (if (string-equal (maq-show-ruler (mycontainer self)) "off") val
      (let* ((themaq (mycontainer self))
             (params (metricparam (params themaq))))
        (ms2listmetric val (first params) (second params) (third params) (fourth params)))))


(defmethod read-info-metric? ((self TemporalBox) val)
  (if (listp val)
    (let* ((themaq (mycontainer self))
           (params (metricparam (params themaq))))
      (listmetric2ms val (first params) (second params) (third params) (fourth params)))
    val))

(defmethod show-info-window ((self TemporalBox) &optional (i 0))
  (if (infowin self)
    (om-select-window (infowin self))
    
    (let* ((pos (if i (om-make-point (+ 30 (* (mod i 4) 250)) (+ 50 (* (floor i 4) 40))) (om-make-point 30 50)))
           (info (get-info-components self))
           (posx (om-make-dialog-item 'om-editable-text (om-make-point 60 250) (om-make-point 60 20) (format () "~D" (pos-info-metric? self (offset self)))
                                      :modify-action (om-dialog-item-act item
                                                   (push 0 (changes (om-view-container item)))) :font *controls-font*))
           (sizex (om-make-dialog-item 'om-editable-text (om-make-point 180 250) (om-make-point 60 20) 
                                       (format () "~D" (if (strech-fact self)
                                                         (round (* (extend self) (strech-fact self)))
                                                         (extend self)))
                                       :modify-action (om-dialog-item-act item
                                                       (push 2 (changes (om-view-container item)))) 
                                       :font *controls-font*))
           (posy (om-make-dialog-item 'om-editable-text (om-make-point 60 275) (om-make-point 60 20) (format () "~D" (posy self))
                                      :modify-action (om-dialog-item-act item
                                                   (push 1 (changes (om-view-container item)))) :font *controls-font*))
           
           (sizey (om-make-dialog-item 'om-editable-text (om-make-point 180 275) (om-make-point 60 20) (format () "~D" (sizey self))
                                       :modify-action (om-dialog-item-act item
                                                    (push 3 (changes (om-view-container item)))) :font *controls-font*))
           (name (if (and (patch-p (reference self))
                          (mypathname (reference self)))
                     (om-make-dialog-item 'om-static-text (om-make-point 70 12) (om-make-point 160 20) (name self) 
                                          :font *om-default-font1b*)
                   (om-make-dialog-item 'om-editable-text (om-make-point 70 12) (om-make-point 160 20) (name self) 
                                        :modify-action (om-dialog-item-act item
                                                        (push 4 (changes (om-view-container item))))
                                        :font *controls-font*)))
           (dialog (om-make-window 'info+offset-window
                                   :position pos
                                   :sujet self
                                   :posx (list posx posy sizex sizey name)
                                   :size (car info)
                                   :window-title (string+ (name self) " Info")
                                   :resizable nil :minimize nil :maximize nil
                    :font *om-default-font2*
                    )))
      (eval `(om-add-subviews ,dialog ,.(cdr info) ,.(posx dialog)))
      (setf (doc-item dialog) (cadr info))
      (setf (name-info dialog) (nth 20 info))
      (setf (offset-info dialog) (nth 16 info))
      (setf (posy-info dialog) (nth 17 info))
      (setf (dur-info dialog) (nth 18 info))
      (setf (sizey-info dialog) (nth 19 info))
      (setf (cp-item dialog)  (cadr info));doc
      (setf (infowin self) dialog)
      (om-add-menu-to-win dialog)
      (om-select-window dialog)
      )
    ))


(defmethod get-info-components ((self TemporalBox))
  (list (om-make-point 255 360)
        (om-make-dialog-item 'om-editable-text (om-make-point 10 130) (om-make-point 230 90) 
                             (get-box-doc self)
                             :font *controls-font*)
        (om-make-dialog-item 'om-static-text (om-make-point 10 12) (om-make-point 70 20) "Name : " 
                             :font *om-default-font2*)
          
        (om-make-dialog-item 'om-static-text (om-make-point 10 42) (om-make-point 60 20) "Type : " 
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 70 42) (om-make-point 140 20) (get-object-insp-name self) 
                             :font *om-default-font2*)
          
        (om-make-dialog-item 'om-static-text (om-make-point 10 72) (om-make-point 80 20) "Reference : " 
                             :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 90 72) (om-make-point 140 20) (get-object-insp-name (reference self))
                             :font *om-default-font2*)
        (om-make-view 'bar-item :position (om-make-point 5 105) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
          ;(om-make-dialog-item 'om-static-text (om-make-point 10 78) (om-make-point 230 60) 
          ;                 (string-until-CR (get-documentation self))
          ;                 :font *om-default-font2*)
          ;(om-make-view 'bar-item :position (om-make-point 5 132) :size (om-make-point 235 2)
          ;            :fg-color *om-gray-color*)

        (om-make-dialog-item 'om-static-text (om-make-point 10 110) (om-make-point 130 20) "Comments:" 
                             :font *om-default-font2*)
          
        (om-make-view 'bar-item :position (om-make-point 5 238) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
          
        (om-make-dialog-item 'om-static-text (om-make-point 10 250) (om-make-point 45 20) "Offset" :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 130 250) (om-make-point 45 20) "Dur."  :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 10 275) (om-make-point 45 20) "Pos. Y" :font *om-default-font2*)
        (om-make-dialog-item 'om-static-text (om-make-point 130 275) (om-make-point 70 20) "Size Y" :font *om-default-font2*)
        (om-make-view 'bar-item :position (om-make-point 5 300) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
        (om-make-dialog-item 'om-button  (om-make-point 180 305) (om-make-point 60 10) "Set" 
                             :di-action (om-dialog-item-act  item
                                          (update-close self (infowin self))
                                          ))
        ))




;;; MARKER 

(defmethod get-info-components ((self temp-marker))
   (list (om-make-point 255 300)
         (om-make-dialog-item 'om-editable-text (om-make-point 10 170) (om-make-point 230 60) 
                              (get-box-doc self)
                              :font *controls-font*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 16) (om-make-point 70 20) "Name : " 
                                                     :font *om-default-font2*)
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 45) (om-make-point 60 20) "Type : " 
                                                    :font *om-default-font2*)
          (om-make-dialog-item 'om-static-text (om-make-point 70 45) (om-make-point 140 20) "Marker" 
                               :font *om-default-font2*
                               )
          
          (om-make-view 'bar-item :position (om-make-point 5 80) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)
         
          (om-make-dialog-item 'om-static-text (om-make-point 10 86) (om-make-point 230 60) 
                           "You can connect this marker (SHIFT + Drag) to the end or the offset of a temporal box"
                                                    :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 140) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)

          (om-make-dialog-item 'om-static-text (om-make-point 10 145) (om-make-point 130 20) "Comments:" 
                                                    :font *om-default-font2*)
          
          (om-make-view 'bar-item :position (om-make-point 5 235) :size (om-make-point 235 2)
                      :fg-color *om-gray-color*)

          ;(om-make-dialog-item 'om-static-text (om-make-point 10 250) (om-make-point 45 20) "Offset : " 
          ;                     :font *om-default-font2*)
          ))

(defmethod show-info-window ((self temp-marker) &optional (i 0))
  (if (infowin self)
    (om-select-window (infowin self))
    
    (let* ((pos (if i (om-make-point (+ 30 (* (mod i 4) 250)) (+ 50 (* (floor i 4) 40))) (om-make-point 30 50)))
           
           (info (get-info-components self))
           
           (name (om-make-dialog-item 'om-editable-text (om-make-point 70 16) (om-make-point 120 20) (name self) 
                                                    :modify-action (om-dialog-item-act item
                                                                    (push 1 (changes (om-view-container item))))

                                                    :font *controls-font*))
           
           ;(posx (om-make-dialog-item 'om-editable-text (om-make-point 80 250) (om-make-point 90 20) (format () "~D" (offset self))
           ;                           :after-action (om-dialog-item-act item
           ;                                        (push 0 (changes (om-view-container item))))
           ;                           :di-action (om-dialog-item-act item
           ;                                        (push 0 (changes (om-view-container item))))))
           (dialog (om-make-window 'info+offset-window
                                   :position pos
                                   :sujet self
                                   :posx (list name) ; (list posx name)
                                   :size (car info)
                                   :window-title (string+ (name self) " Info")
                                   :font *om-default-font2*
                                   )))
      (eval `(om-add-subviews ,dialog ,.(cdr info) ,.(posx dialog)))
      (setf (doc-item dialog) (cadr info))
      (setf (infowin self) dialog)
      (om-add-menu-to-win dialog)
      (om-select-window dialog)
      )
    ))

(defmethod update-close ((self temp-marker) win)
  (call-next-method)
  (let ((items (posx win))
        (changs (remove-duplicates (changes win))))
    (loop for item in changs do
          (case item
            ;(0 (change-offset self (read-from-string (om-dialog-item-text (nth 0 items)))))
            (1 (rename-marker self (om-dialog-item-text (nth 0 items))))))))

