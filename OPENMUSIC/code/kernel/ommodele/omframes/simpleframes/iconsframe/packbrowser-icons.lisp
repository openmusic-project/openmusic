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
;Icon interface for OMBasicObjects
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


(defclass pckbrowser-icon-frame (icon-finder) ())

(defclass pckbrowser-icon (icon-finder-icon) ())
(defclass pckbrowser-name (icon-finder-name) ())

;; no copy
(defmethod om-drag-selection-p ((self pckbrowser-icon) mouse-position)
   (not (or (om-control-key-p)
            (om-option-key-p)
            (om-command-key-p))))

(defmethod open-icon-scroller ((self pckbrowser-icon-frame))
   (let* ((subpanel (panel self))
          (panel (panel subpanel))
          (editor (editor panel))
          (actives (get-actives subpanel))
          (title-item nil))
     (when (and (editorframe (object self)) (equal (class-name (class-of (editorframe (object self)))) 'classTreePanel))
       (om-close-window (window (EditorFrame (object self)))))
     (if (= 1 (length actives))
         (unless (equal (object (nth (current-scroll editor) (scroll-list editor)))
                        (object (car actives)))
           (open-packpanel editor self 
                           (position panel (scroll-list editor) :test 'equal)))
       ;(progn
       ;  (remove-scrolls editor)
       ;  (UpDateScrollers editor))
       )))

;;; special behavior:
;;; in case of multiple selection, click on one package unselects the others
(defmethod toggle-icon-active-mode ((self pckbrowser-icon-frame))
   "Select or unselect 'self'."
   (cond ((om-shift-key-p) 
          (if (active-mode self)
              (omG-unselect self) 
            (omG-select self)))
         (t  (mapc #'(lambda (icon)
                        (omG-unselect icon)) (get-actives (om-view-container self)))
              (omG-select self)))
   t)


(defmethod om-view-click-handler ((self pckbrowser-icon) where)
   (let* ((icn-finder (icon-finder self))
          (pan (om-view-container icn-finder))
          (ed (om-view-container (panel pan))))
     (toggle-icon-active-mode icn-finder)
     (when (subtypep (class-of pan) (find-class 'packagepanel))
       (if (equal (object icn-finder) *library-package*)
          (progn 
            (setf (current-scroll ed) (position pan (scroll-list ed) :test 'equal))
            (remove-scrolls ed)    
            (UpDateScrollers ed))
         (when (and (> (current-scroll ed) (position (panel pan) (scroll-list ed) :test 'equal))
                    (or (not (equal (type-of (object icn-finder)) 'ompackage))
                        t ;(>= (om-point-v where) 10)
                        ))
           (open-icon-scroller icn-finder))
         ))
     t))

(defmethod om-view-click-handler ((self pckbrowser-icon-frame) where)
  (om-view-click-handler (iconview self) (om-make-point 6 12)))

(defmethod om-view-click-handler ((self pckbrowser-name) where)
  (om-view-click-handler (iconview (icon-finder self)) (om-make-point 6 12)))
(defmethod om-view-doubleclick-handler ((self pckbrowser-name) where)
  (om-view-doubleclick-handler (iconview (icon-finder self)) (om-make-point 6 12)))


;============================
;PACKAGES
;============================
(defclass package-icon-frame (pckbrowser-icon-frame) ()
   (:documentation "The class of simple frames for OMpackage meta objects.#enddoc#
#seealso# (OMpackage) #seealso#"))

(defclass pckicon-finder-icon (pckbrowser-icon) ())
(defmethod get-class-icon-icon ((self OMPackage)) 'pckicon-finder-icon)
(defmethod get-class-icon-name ((self OMPackage)) 'pckbrowser-name)

(defmethod initialize-instance :after ((self package-icon-frame) &key controls) 
   (declare (ignore controls))
   (setf (container-p self) 
         (or (equal (object self) *package-user*)
             (typep (object self) 'OMLib)
             (and 
              (not (equal (object self) *om-package-tree*))
              (subpackages (object self))))))



(defmethod make-extra-info ((self package-icon-frame) (object OMPackage))
   (setf (change-name-p self) (not (or (equal object *package-user*) (protected-p object)))))

(defmethod om-view-doubleclick-handler ((self pckicon-finder-icon) where)
    "Double click on 'self' open a subpackage editor or a class hierarchical tree editor"
    (let* ((iconframe (icon-finder self))
          (pack (object iconframe)))
      (if (null (father pack))
          (OpenObjectEditor pack)
        (unless (equal pack *library-package*)
          (progn
            (if (< (om-point-v where) 10)
                (openclasses iconframe)
              (open-icon-scroller iconframe)
              )
            (close-triangle iconframe)
            ))
        )))



(defun close-triangle (iconframe)
  (when (and (not (big-icon-p (editor (om-view-container iconframe))))
             (triangle iconframe)
             (open? (triangle iconframe)))
    (setf (iconID (triangle iconframe)) 164)
    (setf (open? (triangle iconframe)) nil)
    (delete-sub-icons iconframe (om-view-container iconframe))))

(defmethod OpenClasses ((self package-icon-frame))
   "Open the hierarchical tree editor of the package 'self'."
   (when (and (EditorFrame (object self))
              (not (equal (class-name (class-of (EditorFrame (object self)))) 'classTreePanel)))
     (om-close-window (window (EditorFrame (object self)))))
   
   (let* ((panel (om-view-container self))
         (ed (editor panel)))
     (unless (= (position panel (scroll-list ed) :test 'equal) (current-scroll ed))
       (setf (current-scroll ed) (position panel (scroll-list ed) :test 'equal))
       (remove-scrolls ed)     
       (UpDateScrollers ed)
       ))

   (if (EditorFrame (object self))
     (om-select-window (window (EditorFrame (object self))))
     (if (or (add-subpackages-p (object self)) (classes (object self)))
         (let ((treescroll (panel (open-new-RelationTree (object self) 
                                                         (string+ "Package " (name (object self)) " - Class Tree") 
                                                         (get-class+alias (object self))))))
           (setf (object treescroll) (object self))
           (setf (EditorFrame (object self)) treescroll)
           (om-select-window (window (EditorFrame (object self)))))
       (om-message-dialog (format nil "Package ~A is protected and has no direct classes." (name (object self))))
       )))

(defmethod make-new-scroll ((self package-icon-frame)) 
   "Create a Package panel to put in a package browser."
   (let ((thescroll (om-make-view 'PackageContentsPanel
                                  :size (om-make-point 500 500) :position (om-make-point 0 0)
                                  :scrollbars nil :retain-scrollbars nil
                                  )))
     (set-panel-color thescroll)
     (setf (object thescroll) (object self))
     thescroll))


(defmethod show-elements ((self package-icon-frame) container)
   "Show the generic functions of 'self'."
   (let ((i 1))
     (om-with-delayed-update container
     (mapc #'(lambda (elem)
               (add-icon-finder (make-icon-from-object elem 10 10 1 (incf i)) container)) 
         (get-functions (object self))))))

(defmethod show-elements-classes ((self package-icon-frame) container)
   "Show the generic functions of 'self'."
   (let ((i 1))
     (om-with-delayed-update container
     (mapc #'(lambda (elem)
               (add-icon-finder (make-icon-from-object elem 10 10 1 (incf i)) container)) 
         (get-classes (object self))))))

(defmethod show-elements ((self package-icon-frame) (container packagecontentspanel))
   "Show the generic functions of 'self'."
   (show-elements self (funpanel container))
   (show-elements-classes self (clsspanel container)))


(defmethod om-view-cursor :around ((self pckicon-finder-icon))
  (let ((pos-in-icon (om-mouse-position self)))
    (when pos-in-icon
     (if (father (object (icon-finder self)))
         (if (< (om-point-v pos-in-icon) 10)
             *om-tree-cursor* *om-pack-cursor*)
       (call-next-method)))))


(defmethod om-drag-receive ((view packagesubpanel) 
                            (dragged-view t) position &optional (effect nil)) 
  nil)


;;; PACKAGE "LIBRARIES"

(defun refresh-lib-list (frame)
  (when (equal (object frame) *library-package*)
    (close-triangle frame)
    (reload-user-libs)
    (open/close-triangle (triangle frame) t)
    ))


;============================
;OMLIB
;============================
(defclass lib-icon-frame (package-icon-frame) ()
  (:documentation "The class of simple frames for OMLib meta objects.#enddoc#
#seealso# (OMLib) #seealso#"))

(defclass libicon-finder-icon (pckicon-finder-icon) ())
(defmethod get-class-icon-icon ((self OMLib)) 'libicon-finder-icon)
(defmethod get-class-icon-name ((self OMLib)) 'pckbrowser-name)

(defmethod om-draw-contents :after ((self libicon-finder-icon))
    (om-with-focused-view self
      (om-with-fg-color self 
          (if (loaded? (object (om-view-container self)))
              *om-green2-color*
            *om-red2-color*)
        (om-with-line-size 3
          ;(om-draw-line 0 4 4 8)
          ;(om-draw-line 4 8 12 0)
          (om-fill-rect 1 3 4 4)
          ))))

(defmethod open-icon-scroller ((self lib-icon-frame))
  (if (loaded? (object self))
     (call-next-method)
    (let ((editor (editor (om-view-container self))))
      (setf (current-scroll editor) 0)
      (remove-scrolls editor)
      (updatescrollers editor))
    ))


(defmethod om-view-doubleclick-handler ((self libicon-finder-icon) where)
    "When you double click this method load the lib if it is not loaded."
    (let* ((iconframe (icon-finder self))
          (lib (object iconframe)))
      (if (loaded? lib)
          (call-next-method)
        (let ((editor (editor (om-view-container iconframe))))
          (load-om-lib lib)
          (open-icon-scroller iconframe)))))


(defmethod om-get-help-spec ((self pckicon-finder-icon)) 
  (let ((obj (object (icon-finder self))))
    (string-until-cr (get-documentation obj))))



;============================
;GenFun
;============================
(defclass genfun-icon-frame (pckbrowser-icon-frame) ()
   (:documentation "The class of simple frames for OMGenericFunction meta objects.#enddoc#
#seealso# (OMGenericFunction) #seealso#"))

(defclass genfun-finder-icon (pckbrowser-icon) ())
(defmethod get-class-icon-icon ((self OMGenericFunction)) 'genfun-finder-icon)
(defmethod get-class-icon-name ((self OMGenericFunction)) 'pckbrowser-name)

(defmethod make-new-scroll ((self genfun-icon-frame))
   "Open a new editor containing simple method frames."
   (let ((thescroll (om-make-view 'GenericFunPanel
                                                           :size (om-make-point 500 500)
                                                           :position (om-make-point 0 0)
                                                           :scrollbars t
                                                           :retain-scrollbars t
                                                           :bg-color *package-color-3*
                                                           ;;; :draw-scroller-outline nil
                                                           ))
           )
     (setf (object thescroll) (object self))
     thescroll))

(defmethod show-elements ((self genfun-icon-frame) container)
   "Show the methods of 'self' as simple frames."
   (let ((i 0))
     (mapc #'(lambda (elem)
               (om-add-subviews container (make-icon-from-method elem (om-make-point 10 (+ 0 (* (incf i) 50))))))
         (get-elements (object self)))))

(defmethod open-icon-win ((self genfun-icon-frame) where)
   "Double click on 'self' open a subpackage editor or a class hierarchical tree editor"
   (OpenObjectEditor (object self)))

(defmethod om-view-doubleclick-handler ((self genfun-finder-icon) where)
    (open-icon-win (om-view-container self) (om-make-point 200 100)))

(defmethod show-big-doc ((self genfun-icon-frame))
  (show-big-doc (function-name (object self))))

;============================
;LispFun
;============================
(defclass lispfun-icon-frame (pckbrowser-icon-frame) ()
   (:documentation "The class of simple frames for OMGenericFunction meta objects.#enddoc#
#seealso# (OMGenericFunction) #seealso#"))

(defclass lispfun-finder-icon (pckbrowser-icon) ())
(defmethod get-class-icon-icon ((self OMLispFun)) 'lispfun-finder-icon)
(defmethod get-class-icon-name ((self OMLispFun)) 'pckbrowser-name)

;;; no scroller for LispFuns
(defmethod open-icon-scroller ((self lispfun-icon-frame))
  (let* ((subpanel (panel self))
         (panel (panel subpanel))
         (editor (editor panel)))
    (setf (current-scroll editor) (position panel (scroll-list editor) :test 'equal))
    (remove-scrolls editor)
    (UpDateScrollers editor)
    ))

(defmethod show-big-doc ((self lispfun-icon-frame))
  (show-big-doc (funname (object self))))

;============================
;CLASSES
;============================
(defclass class-icon-frame (pckbrowser-icon-frame) ()
   (:documentation "The class of simple frames for OMClass meta objects.#enddoc#
#seealso# (OMClass) #seealso#"))

(defclass class-finder-icon (pckbrowser-icon) ())
(defmethod get-class-icon-icon ((self OMClass)) 'class-finder-icon)
(defmethod get-class-icon-name ((self OMClass)) 'pckbrowser-name)

(defmethod initialize-instance :after ((self class-icon-frame) &key controls)
  (declare (ignore controls))
  ;;;(setf (container-p self) t)
  t)


(defmethod make-new-scroll ((self class-icon-frame))
   "When you click+alt on 'self' it open a new scroll with the internal method of the class."
   (let ((theclass (object self)))
     (if (protected-p theclass)
       (not (dialog-message "This class is protected, no access to internal methods"))
       (let ((thescroll (om-make-view 'InternalMethPanel
                                                               :size (om-make-point 500 500)
                                                               :position (om-make-point 0 0)
                                                               :scrollbars t
                                                               :bg-color *package-color-3*
                                                               ;;;  :draw-scroller-outline nil
                                                               ))
              )
         (setf (object thescroll) (object self))
         thescroll))))



(defmethod show-elements ((self class-icon-frame) container)
   "Show the internal methods of 'self'."
   (let ((i 0))
     (mapc #'(lambda (elem)
               (om-add-subviews container (make-icon-from-method elem (om-make-point 10 (+ 0 (* (incf i) 50))))))
           (internal-met (object self)))))


(defmethod open-icon-win ((self class-icon-frame) where)
   "Called when you double-click on a class icon."
   (declare (ignore where))
   (when (and (not (big-icon-p (editor (om-view-container  self))))
              (triangle self)
              (open? (triangle self)))
     (setf (iconID (triangle self)) 164)
     (setf (open? (triangle self)) nil)
     ;;;(om-draw-contents (triangle self))
     (om-invalidate-view (triangle self))
     (delete-sub-icons self (om-view-container  self)))
   (unless (EditorFrame (object self))
     (setf (EditorFrame  (object self)) (OpenEditorframe (object self))))
   (cond
    ((om-command-key-p)
     (when (equal (class-name (class-of (EditorFrame (object self)))) 'classPanel)
       (om-close-window (window (Editorframe (object self))))
       (setf (EditorFrame  (object self)) (OpenEditorframe (object self))))
     (om-select-window  (window (EditorFrame  (object self)))))
    (t 
     (when (equal (class-name (class-of (EditorFrame (object self)))) 'GenFunScroller)
       (om-close-window (window (Editorframe (object self))))
       (setf (EditorFrame  (object self)) (OpenEditorframe (object self))))
     (om-select-window  (window (EditorFrame  (object self)))))))
 

(defmethod show-big-doc ((self class-icon-frame))
  (let ((refdir (or (special-ref-location (make-instance (class-name (object self))))
                    (lib-ref-location (object self)))))
    (om-show-reference-doc (class-name (object self)) refdir)))

;=================
; GLOBALS
;=================
(defclass globals-folder-icon (pckbrowser-icon-frame) ()
   (:documentation "The class of simple frames for OMGlobalsFolder meta objects.#enddoc#
#seealso# (OMglobalsFolder) #seealso#"))

(defmethod initialize-instance :after ((self globals-folder-icon) &key controls)
   (declare (ignore controls))
   (setf (container-p self) nil)
   (setf (change-name-p self) nil))

(defmethod open-icon-win ((self globals-folder-icon) where)
   "Double click on 'self' open a subpackage editor or a class hierarchical tree editor"
   (if (< (om-point-v where) 10)
       (let ((ed (editor (om-view-container self)))) 
         (remove-scrolls ed)
         (updatescrollers ed)
         (OpenObjectEditor (object self)))
     (progn
       (when (editorframe (object self))
         (om-close-window (window (EditorFrame (object self)))))
       (open-icon-scroller self))))

(defmethod openclasses ((self globals-folder-icon))
   "Double click on 'self' open a subpackage editor or a class hierarchical tree editor"
      (let* ((panel (om-view-container self))
             (ed (editor panel)))
        (unless (= (position panel (scroll-list ed) :test 'equal) (current-scroll ed))
          (setf (current-scroll ed) (position panel (scroll-list ed) :test 'equal))
          (remove-scrolls ed)     
          (UpDateScrollers ed)
          ))
      (OpenObjectEditor (object self)))


(defmethod make-new-scroll ((self globals-folder-icon)) 
   "Create a Package panel to put in a package browser."
   (let ((thescroll (om-make-view 'GlobalsfolderPanel
                                  :size (om-make-point 500 500) :position (om-make-point 0 0)
                                  :scrollbars t
                                  :bg-color *package-color-2*
                                  :presentation 1
                                  )))
     (setf (object thescroll) (object self))
     thescroll))

(defmethod show-elements ((self globals-folder-icon) container)
   "Show the methods of 'self' as simple frames."
   (let ((i 0))
     (mapc #'(lambda (elem)
               (add-icon-finder (make-icon-from-object elem 10 10 1 (incf i)) container))
           (get-elements (object self)))))




; GENERAL MENUS

(defmethod om-get-menu-context ((self package-icon-frame))
  (remove nil  
          (list 
           (if (equal (object self) *library-package*) 
             (list 
                 (om-new-leafmenu "Refresh Lib List" #'(lambda () (refresh-lib-list self)))
                 )
             (list 
                 (om-new-leafmenu "Open Package" #'(lambda () (open-icon-scroller self)))
                 (om-new-leafmenu "Open Class Tree" #'(lambda () (openclasses self)))
                 ))
           (when (mypathname (object self)) (list (om-new-leafmenu "Export" #'(lambda () (export-user-package (object self))))))
           (om-new-leafmenu "Get Info" #'(lambda () (show-info-window (get-obj-for-info self))))
           )))

(defun remove-all-scrolls (ed)
  (setf (current-scroll ed) 0)
  (remove-scrolls ed)     
  (UpDateScrollers ed))

(defmethod om-get-menu-context ((self lib-icon-frame))

          (list (if (loaded? (object self))
                (list 
                 (om-new-leafmenu "Open Library" #'(lambda () (open-icon-scroller self)))
                 (om-new-leafmenu "Open Lib Class Tree" #'(lambda () (openclasses self)))
                 (om-new-leafmenu "Reload Library" #'(lambda () 
                                                       (setf (loaded? (object self)) nil)
                                                       (remove-all-scrolls (editor (om-view-container self)))
                                                       (load-om-lib (object self))
                                                       (open-icon-scroller self))))
          (list 
           (om-new-leafmenu "Load Library" #'(lambda () 
                                               (load-om-lib (object self))
                                               (open-icon-scroller self)
                                               ))))
        
          (om-new-leafmenu "Get Info" #'(lambda () (show-info-window (get-obj-for-info self))))
            (om-new-leafmenu "Documentation" #'(lambda () (om-show-reference-doc 'index (lib-ref-path (object self)))) nil 
                             (and (lib-ref-path (object self)) (probe-file (lib-ref-path (object self)))))
          
          ))



(defmethod om-get-menu-context ((self genfun-icon-frame))
  (remove nil 
          (list (list 
                  (om-new-leafmenu "Show Function Methods" #'(lambda () (open-icon-win self (om-make-point 0 0))))
                 )
                ;(when (mypathname (object self)) (om-new-leafmenu "Export" #'(lambda () (export-user-fun (object self)))))
                (list 
        (om-new-leafmenu "Get info" #'(lambda () (show-info-window (get-obj-for-info self))))
        ))))

(defmethod om-get-menu-context ((self class-icon-frame))   
  (remove nil 
   (list (list 
          (om-new-leafmenu "Open" #'(lambda () (open-icon-win self (om-make-point 0 0))))
          (om-new-leafmenu "Show Internal Methods" #'(lambda () (open-icon-scroller self)))
          )
         (when (mypathname (object self)) (om-new-leafmenu "Export" #'(lambda () (export-user-class (object self)))))
         (list (om-new-leafmenu "Get info" #'(lambda () (show-info-window (get-obj-for-info self))))))))



