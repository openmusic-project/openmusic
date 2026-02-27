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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;DocFile
;Package  editor definition.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defclass Mult-scrollerEditor (nonrelationEditor) 
   ((current-scroll :initform 0 :accessor current-scroll)
    (scroll-list :initform nil  :accessor scroll-list))
   (:documentation "This is the class of windows containing one or more scrollers.#enddoc#
#seealso# (OMPackage Package-WinFrame PackagePanel) #seealso#
#presentation# 0 show normal icons, 1 show by list and 2 by type. #presentation#
#hardcopy-in-progress# A flag used for printing. #hardcopy-in-progress#"))


(defmethod get-editor-panel-class ((self Mult-scrollerEditor))  'Mult-scrollerPanel)

;(setf *package-color-1* (om-choose-color-dialog :color *package-color-1*))

(defparameter *package-color-1* *om-light-gray-color*) ; (om-make-color 0.79 0.79 0.79))
;(defparameter *package-color-1* (om-make-color 0.4497343 0.5172331 0.56108594))
(defparameter *package-color-2* *package-color-1*)
(defparameter *package-color-3* *package-color-1*)

(defparameter *package-color-bg* *om-dark-gray-color*)

(defmethod initialize-instance :after ((self Mult-scrollerEditor) &key controls)
   (declare (ignore controls))
   (om-set-bg-color self *package-color-bg*)
   (setf (scroll-list self) (list (slot-value self 'panel)))
   (setf (presentation self) 1))


(defmethod panel ((self Mult-scrollerEditor))
   "Return the current scroller of the window 'self'."
   (nth (current-scroll self) (scroll-list self)))

(defmethod get-clipboard ((self Mult-scrollerEditor)) nil)

(defmethod editor-change-presentation ((self Mult-scrollerEditor) presentation)
   (declare (ignore presentation))
   (om-beep))

(defmethod close-editor-before ((self Mult-scrollerEditor))
   (call-next-method)
   (setf (current-scroll (editor self)) 0))

(defparameter *inter-panes* 2)

(defmethod om-set-view-size ((self Mult-scrollerEditor) size-point)
   (declare (ignore size-point))
   (call-next-method)  
   (let* ((scroll-list (scroll-list self))
          (length (length scroll-list))
          (delta *inter-panes*)
          size (i -1))
     (setf size (floor (- (om-point-h (om-view-size self)) (* delta (- length 1))) length))
     (mapc #'(lambda (scroll)
               (setf i (+ i 1))
               (om-set-view-size scroll (om-make-point size (om-point-v (panel-size self))))
               (om-set-view-position scroll (om-make-point (if (= i (- length 1)) (- (w self) size)
                                                             (* i (+ size delta)))
                                                           (om-point-v (panel-position self))))
                       ) 
         scroll-list)))

#|
(defmethod UpDateScrollers ((self mult-scrollerEditor))
   (let* ((scroll-list (scroll-list self))
          (size (om-view-size (nth 0 scroll-list)))
          (delta *inter-panes*))
     (om-set-interior-size (window self) (om-make-point (- (* (+ (om-point-h size) *inter-panes*) (length scroll-list))
                                                           *inter-panes*)
                                                        (om-point-v (om-view-size self))))
     (om-invalidate-view self)))
|#

(defmethod UpDateScrollers ((self mult-scrollerEditor))
   (let* ((scroll-list (scroll-list self))
          (lgt-scroll (length scroll-list))
          (vsize (om-view-size (nth 0 scroll-list)))
          (size (om-make-point (* lgt-scroll (om-point-h vsize)) (* lgt-scroll (om-point-h vsize)))))
  
     #-linux(om-set-interior-size (window self) size)
     #-linux(om-set-view-size self size)
     (om-invalidate-view self)
     ))

(defmethod remove-scrolls ((editor mult-scrollerEditor)) 
  (let ((delete (subseq (scroll-list editor) (+ 1 (current-scroll editor)))))
    (setf (scroll-list editor) (subseq (scroll-list editor) 0 (+ 1 (current-scroll editor))))
    ; (UpDateScrollers editor)
    (apply 'om-remove-subviews (cons editor delete))
    (setf (scroll-list editor) (subseq (scroll-list editor) 0 (+ 1 (current-scroll editor))))))

(defmethod set-scrolls ((editor mult-scrollerEditor) &optional (num 0))
  (setf (current-scroll editor) num)
  (remove-scrolls editor)     
  (UpDateScrollers editor))

(defmethod open-packpanel (editor element at)
  (setf (current-scroll editor) at)
  (om-with-delayed-update editor
    (let ((new-scroll (make-new-scroll element)))
      (when new-scroll
        (remove-scrolls editor)
        (om-add-subviews editor new-scroll)
        (setf (current-scroll editor) (+ 1 (current-scroll editor)))
        (setf (scroll-list editor) (list+ (scroll-list editor) (list new-scroll)))
        (show-elements element new-scroll)
        ))
    (UpDateScrollers editor)
    ))


(defmethod get-info-window-components ((self mult-scrollerEditor))
  (if (get-actives (panel self))
      (mapcar 'get-obj-for-info (get-actives (panel self)))
    (list (object (panel self)))))

;------------------------------------------------------
;Package browser
;------------------------------------------------------

(defclass PackageEditor (Mult-scrollerEditor)  
   ((panel-title :initarg :panel-title :initform "" :accessor panel-title)
    ;(titles :initarg :titles :initform nil :accessor titles)
    (selection :accessor selection :initarg :selection :initform nil))
   (:documentation "This is the class of windows containing one or more scrollers.#enddoc#
#seealso# (OMPackage Package-WinFrame PackagePanel) #seealso#
#presentation# 0 show normal icons, 1 show by list and 2 by type. #presentation#
#hardcopy-in-progress# A flag used for printing. #hardcopy-in-progress#"))

(defmethod get-editor-panel-class ((self PackageEditor)) 'PackagePanel)

(defmethod panel-position ((self PackageEditor)) (om-make-point 0 0))

(defmethod panel-size ((self PackageEditor)) 
   (om-make-point (w self) (h self)))

(defmethod editor-minimum-size ((self packageeditor)) (om-make-point 400 300))

(defmethod make-new-scroll ((self t)) 
   "Open a new editor containing the simple frames elements of 'self'."
   nil)

(defclass PackagePanel (nonrelationPanel) ()
   ;;;(:default-initargs :retain-scrollbars t)

              #+(and win32 (not ml-maquette)) 
              (:default-initargs :draw-pinboard-objects :local-buffer)

   (:documentation "This is the class for package's editors.
Packages panels contains icons of sub-packages, classes and sometimes slots.#enddoc#
#seealso# (OMPackage package-icon-frame class-icon-frame  slot-icon-frame) #seealso#"))

(defmethod metaobj-scrollbars-params ((self packagepanel))  '(t nil))

(defmethod set-panel-color ((self PackagePanel))
  (om-set-bg-color self *package-color-1*))

(defmethod sort-subframes ((self PackagePanel) elements) elements)

(defmethod update-subviews ((self PackageEditor))
   (set-field-size (panel self)))

(defmethod om-view-cursor ((self PackagePanel)) *om-arrow-cursor*)

(defmethod om-get-menu-context ((self packagepanel))
  ;(list (om-new-leafmenu "Show Package Classes" #'(lambda () (open-icon-scroller-classes self)))
  ;      (om-new-leafmenu "Show Package Functions" #'(lambda () (open-icon-scroller self))))
  nil)


;(defmethod om-draw-contents ((self packagepanel)) 
;  (let ((str "Packages"))
;    (when str
;      (om-with-focused-view self
;        (om-with-font *om-default-font1b*
;                      (om-with-fg-color self *om-black-color*
;                        (om-draw-string (- (w self) (om-string-size str *om-default-font2b*) 30) 14 str)
;                        ))))))



;-------------------
;PANEL 1 : packages
;-------------------

(defmethod om-view-click-handler ((self packagePanel) where)
  (declare (ignore where))
  (when (equal self (call-next-method))
    (let ((ed (editor self))
          begin-remove)
      (unless (= (position (panel self) (scroll-list ed) :test 'equal) (current-scroll ed))
        (set-scrolls ed (position (panel self) (scroll-list ed) :test 'equal))
        ))))

;------------------------------------
;PANEL 2 : functions / classes 
;------------------------------------

(defclass PackageContentsPanel (metaobj-panel) 
              ((funpanel :accessor funpanel :initarg :funpanel :initform nil)
               (clsspanel :accessor clsspanel :initarg :clsspanel :initform nil)))

(defmethod get-subframes ((self PackageContentsPanel))
  (append (get-subframes (funpanel self)) (get-subframes (clsspanel self))))

(defmethod get-actives ((self PackageContentsPanel) &optional class)
  (append (get-actives (funpanel self)) (get-actives (clsspanel self))))

(defmethod omg-remove-element ((self PackageContentsPanel) frame)
   "Remove 'frame' from 'self', this function call the 'omng-remove-element' function."
   (cond ((member frame (om-subviews (funpanel self)))
          (omg-remove-element (funpanel self) frame))
         ((member frame (om-subviews (clsspanel self)))
          (omg-remove-element (clsspanel self) frame))
         (t nil)))
   
(defclass packagesubpanel (packagepanel) 
              ((panel-title :accessor panel-title :initarg :panel-title :initform "")))

(defclass packageclassespanel (packagesubpanel) ())
(defclass packagefunspanel (packagesubpanel) ())

(defmethod editor ((self packagesubpanel))
  (editor (om-view-container self)))

(defmethod panel ((self packagesubpanel))
  (om-view-container self))

(defmethod om-view-click-handler :before ((self packagesubpanel) pos)
  (mapc #'(lambda (view) (do-click-event-handler view pos)) (om-subviews (om-view-container self))))

(defmethod handle-key-event ((self packageeditor) key)
  (handle-key-event (panel self) key))

(defmethod handle-key-event ((self PackageContentsPanel) key)
  (cond ((equal key #\d) (mapcar 'show-big-doc (get-actives self)))
        (t (call-next-method))))


(defmethod (setf object) (obj (self PackageContentsPanel))
  (call-next-method)
  (setf (object (funpanel self)) obj)
  (setf (object (clsspanel self)) obj))

(defmethod initialize-instance :after ((self PackageContentsPanel) &rest args)
  (setf (funpanel self) (om-make-view 'packagefunspanel
                                      :size (om-make-point (w self) (round (h self) 2))
                                      :position (om-make-point 0 0)
                                      :scrollbars t :retain-scrollbars t
                                      :object (object self)
                                      :bg-color *package-color-2*
                                      :panel-title "Functions in package "))
  (setf (clsspanel self) (om-make-view 'packageclassespanel
                                       :size (om-make-point (w self) (round (h self) 2))
                                       :position (om-make-point 0 (round (h self) 2))
                                       :scrollbars t :retain-scrollbars t
                                       :object (object self)
                                       :bg-color *package-color-2*
                                       :panel-title "Classes in package "))
  (om-add-subviews self (funpanel self) (clsspanel self))
  )

(defmethod set-panel-color ((self PackageContentsPanel))
  (om-set-bg-color self *package-color-bg*))
  
(defmethod set-field-size ((self PackageContentsPanel))
  (call-next-method)
  (set-field-size (funpanel self))
  (set-field-size (clsspanel self)))

(defmethod om-set-view-size ((self PackageContentsPanel) size)
  (call-next-method)
  (let ((delta (round *inter-panes* 2)))
    (om-set-view-size (funpanel self) (om-make-point (om-point-h size) (- (round (om-point-v size) 2) delta)))
  (om-set-view-position (clsspanel self) (om-make-point 0 (+ (round (om-point-v size) 2) delta)))
  (om-set-view-size (clsspanel self) (om-make-point (om-point-h size) (- (round (om-point-v size) 2) delta)))
  ))
  
(defmethod om-draw-contents ((self packagesubpanel))
  (draw-browser-panel-title self (panel-title self) (string (name (object self)))))


;;; ADD / EDIT ITEMS

(defmethod om-get-menu-context ((self packageclassespanel))
  (list
   (om-new-leafmenu "Import Class"
                    #'(lambda ()
                        (let ((file (om-choose-file-dialog :prompt "Choose a Class file" :types '("OM Classes" "*.omc" "All files" "*.*"))))
                          (when (and file
                                     (import-user-class (object self) file))
                            (open-packpanel (editor self) (car (get-actives (car (scroll-list (editor self)))))
                                            (position (panel self) (scroll-list (editor self)) :test 'equal))
                            ))))
   (om-new-leafmenu "Import Package"
                    #'(lambda ()
                        (let ((dir (om-choose-directory-dialog :prompt "Choose a Package directory")))
                          (when (and dir 
                                     (import-user-package (object self) dir))
                            (open-packpanel (editor self) (car (get-actives (car (scroll-list (editor self)))))
                                            (position (panel self) (scroll-list (editor self)) :test 'equal))))))))

(defmethod om-get-menu-context ((self packagefunspanel))
  (list
   (om-new-leafmenu "Import Method"
                    #'(lambda ()
                        (let ((file (om-choose-file-dialog :prompt "Choose a method file" :types '("OM Methods" "*.ome" "All files" "*.*"))))
                          (when (and file (import-user-method (object self) file))
                              (open-packpanel (editor self) (car (get-actives (car (scroll-list (editor self)))))
                                              (position (panel self) (scroll-list (editor self)) :test 'equal))))))
   (om-new-leafmenu "Import Package"
                    #'(lambda ()
                        (let ((dir (om-choose-directory-dialog :prompt "Choose a package directory")))
                          (when (and dir (import-user-package (object self) dir))
                              (open-packpanel (editor self) (car (get-actives (car (scroll-list (editor self)))))
                                              (position (panel self) (scroll-list (editor self)) :test 'equal))))))))


(defmethod editor-make-new-icon-window ((self PackageEditor) &optional type)
     (if (not (= (current-scroll self) 1))
         (om-message-dialog "Open an unprotected package to add new elements !")
       (let* ((scrollframe (nth 0 (scroll-list self)))
              (iconframe (car (get-actives scrollframe)))
              (container-object (object iconframe))  ; (object scrollframe)
              new-object new-frame)
         (if (not (add-subpackages-p container-object))
             (dialog-message (string+ "You can create "
                                      (cond ((equal type 'p) "subpackages")
                                            ((equal type 'p) "classes")
                                            ((equal type 'p) "generic functions")
                                            (t "new elements"))
                                      " only in the USER package or in its subpackages."))
           (progn
             (setf (current-scroll self) 0)
             (remove-scrolls self)
             (UpDateScrollers self)
             (cond ((equal type 'p) 
                    ;;; NEW PACKAGE
                    (setf new-object (make-instance 'OMPackage :name (unique-name-from-list "New Package" (subpackages container-object)) :icon 22))
                    ;(setf new-frame (make-icon-from-object new-object 10 10 1 1))
                    (omng-add-element (object iconframe) new-object)
                    ;;; test 
                    ;(set-field-size scrollframe)
                    (unless (triangle iconframe)
                      (add-triangle iconframe scrollframe))
                    (when (open? (triangle iconframe))
                      (open/close-triangle (triangle iconframe) nil))
                    (open/close-triangle (triangle iconframe) t)
                    )
                   ((equal type 'c) 
                    ;;; NEW CLASS
                    (let ((dial (get-classname-dialog)))
                      (when dial
                        (setf string (first dial))
                        (setf iconID (third dial))
                        (setf doc (second dial))
                        (when (string-equal doc "")
                          (setf doc "No Documentation"))
                        (when (and string (not (string-equal "" string)))
                          (if (exist-class-p string) 
                              (progn
                                (dialog-message (string+ "The class " string " is already defined!"))
                            ;(om-abort)
                                (editor-make-new-icon-window self type))
                            (progn
                              (setf new-object (eval `(defclass* ,(interne string ) () () 
                                                                 (:icon ,(car (list! iconID))) (:documentation ,doc))))
                              (when (listp iconID)
                                (icon-for-user-package new-object (second iconID)))
                              (setf (create-info new-object) (list (om-get-date) (om-get-date)))
                          ;(setf new-frame (make-icon-from-object new-object 10 10 1 1))
                          ;(omg-add-element scrollframe new-frame)
                          (omNG-add-element container-object new-object)
                          ))))
                      )
                    (open-icon-scroller iconframe))
                   ((equal type 'f) 
                    ;;; NEW GENERIC FUNCTION
                    (let ((diag (get-funname-dialog)) string iconid doc)
                      (if diag
                        (progn 
                          (setf string (first diag))
                          (setf doc (second diag))
                          (setf iconid (third diag))
                          (when (string-equal doc "")
                            (setf doc "No Documentation"))
                          (when (and string (not (string-equal "" string)))
                            (if (fboundp (interne string))
                                (progn (dialog-message (string+ "A function " string " already exists."))
                                  (editor-make-new-icon-window self type))
                              (make-new-genfunwin self string iconid doc container-object)))
                          )
                      (open-icon-scroller iconframe))))
                   )
             
             )))))

;------------------------------------
;PANEL 3 : methods
;------------------------------------



;------------------------------------
;
;------------------------------------

;To enable reoppening (linux editor)

#+linux
(defmethod close-editorFrame ((self packageeditor)) 
      (setf (Editorframe (object self)) nil)
    (call-next-method))
