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
;This File contains high level functions and macros for menubars management.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


;-------------------high level menu functions (compatibility)--------------------------

;enable all menu items in the menu <menu>
(defun enable-all-menu-items (menu)
  (loop for item in  (om-menu-items menu) do
        (om-menuitem-enable item)))

;disable all menu items in the menu <menu>
(defun disable-all-menu-items (menu)
  (loop for item in  (om-menu-items menu) do
        (om-menuitem-disable item)))


;disable the menu items which name in the list <list> in the menu <menu>
(defun disable-this-menu-items (menu list)
  (loop for item in list do
        (let ((rep (om-find-menu-item menu item)))
          (when rep
            (om-menuitem-disable rep)))))

;disable the menu items which name in the list <list> in the menu <menu>
(defun enable-this-menu-items (menu list)
  (loop for item in list do
        (let ((rep (om-find-menu-item menu item)))
          (when rep
            (om-menuitem-enable rep)))))


;;;==========================
;;; MENUS CALLBACKS
;;;==========================
(defun show-about-win ()
  (show-kero-pict t))

(defun show-workspace-win ()
  (showobjecteditor *current-workspace*))

(defun show-packages-win () 
  (OpenObjectEditor *om-package-tree*))

(defun show-globals-win () 
  (OpenObjectEditor *om-globalsfolder*))

(defun show-preferences-win () 
  (omG-make-preference-dialog))


;;; redefined in the midi project if loaded
(defun show-midi-mixer-win () nil)

;;; redefined in the audio project if loaded
(defun show-audio-mixer-win () nil)

(defun show-listener-win () 
  (om-make-new-listener :initial-lambda #'(lambda () (in-package :om)) :input *listener-input*))

;;; redefined in scorepalette
(defun show-score-inspector (editor) nil)

(defun show-score-inspector-enabled () nil)

(defmethod score-tools-palettes-p (editor) nil)

;;;==========================
;;; MENUS
;;;==========================

(defun init-app-menu ()
  (setf *om-app-menu-items* (list (string+ "OM " *version-string*) 
                                  (list '("About OM..." om::show-about-win)
                                        '("Preferences" om::show-preferences-win ",")
                                        '("Quit" om::om-confirmed-quit "q")))))

(om-add-init-func 'init-app-menu)

(defmethod make-new-menu ((self t))
  (list (om-new-leafmenu "New" nil nil nil)))

(defmethod font-menu ((self t)) nil)


; disable is a list of disabled items
(defun make-om-menu (menuid &key disable editor more-items)
  (let ((win (if editor (om-view-window editor) (om-front-window)))
        (add-items (mapcar #'(lambda (item) (om-new-leafmenu (car item) (cadr item))) more-items)))
    (cond ((equal menuid 'windows)
           (om-make-menu "Windows" (append 
                                    (unless (om-standalone-p) 
                                      (list (list (om-new-leafmenu "Preferences" 'show-preferences-win "," 
                                                                   (if (member "Preferences" disable :test 'string-equal) nil t)))))
                                    (list 
                                     (list 
                                      (om-new-leafmenu "Workspace" 'show-workspace-win "W" 
                                                       (if (member "Workspace" disable :test 'string-equal) nil t))
                                      (om-new-leafmenu "Library" 'show-packages-win "P" 
                                                       (if (member "Library" disable :test 'string-equal) nil t))
                                    ;(om-new-leafmenu "Global Variables" 'show-globals-win nil 
                                    ;                 (if (member "Global Variables" disable :test 'string-equal) nil t))
                                    ;(om-new-leafmenu "Resources" 'show-resources-win nil 
                                    ;                 (if (member "Resources" disable :test 'string-equal) nil nil))
                                      )
                                     (list
                                    ;(om-new-leafmenu "General Palette" #'(lambda () (show-palette-win (om-view-window editor))) nil 
                                    ;                 (if (and (not (member "General Palette" disable :test 'string-equal)) editor 
                                    ;                          (editor-has-palette-p editor)) t nil))
                                      (om-new-leafmenu "MIDI Mixer" 'show-midi-mixer-win)
                                      (om-new-leafmenu "Audio Mixer" 'show-audio-mixer-win)
                                      (when (score-tools-palettes-p editor)
                                        (om-new-leafmenu "Score Inspector" #'(lambda () (show-score-inspector editor))  nil 
                                                         (if (member "Score Inspector" disable :test 'string-equal) nil
                                                           #'(lambda () (show-score-inspector-enabled)))
                                                         ))
                                      (when (score-tools-palettes-p editor)
                                        (om-new-leafmenu "Extra Edition Palette" #'(lambda () (show-extra-palette editor))  nil 
                                                         (if (member "Extra Edition Palette" disable :test 'string-equal) nil
                                                           #'(lambda () (show-extra-palette-enabled))))
                                        )
                                      )
                                     (list 
                                      (om-new-leafmenu "Lisp Listener" 'show-listener-win "L" (if (member "Lisp Listener" disable :test 'string-equal) nil t))
                                      (om-new-leafmenu "Lisp Editor" 'om-open-new-text-editor nil (if (member "Lisp Editor" disable :test 'string-equal) nil t))
                                      )
                                     #'(lambda (w) 
                                         (mapcar #'(lambda (w) (om-new-leafmenu (om-window-title w)
                                                                                #'(lambda () (om-select-window w))
                                                                                nil 
                                                                                #'(lambda () (not (equal win w)))))
                                                 (remove-if 
                                                  #'(lambda (www) (or (equal 'packageeditor (type-of (editor www)))
                                                                      (equal 'workspaceeditor (type-of (editor www)))))
                                                  (om-get-all-windows 'editorwindow))))
                                     ()))))
          ((equal menuid 'presentation)
           (om-make-menu "Presentation" (list
                                         (list :selection
                                               (om-new-leafmenu "Icons" #'(lambda () (omG-change-presentation win 0)
                                                                          ;(om-add-menu-to-win win)
                                                                            ) 
                                                                nil
                                                                (if (member "Icons" disable :test 'string-equal) nil t)
                                                                #'(lambda () (= (presentation (editor win)) 0)))
                                               (om-new-leafmenu "List" #'(lambda () (omG-change-presentation win 1)
                                                                           (om-add-menu-to-win win)) nil 
                                                                (if (member "Name" disable :test 'string-equal) nil t)
                                                                #'(lambda () (= (presentation (editor win)) 1)))
                                               )
                                         (when win
                                           #'(lambda (win)
                                               (list (if (list-presentation? win)
                                                         (om-new-menu "Sort..."
                                                                      (om-new-leafmenu "By Name" #'(lambda () 
                                                                                                     (change-frame-presentation (panel (editor win)) 1)
                                                                                             ;(omG-change-presentation win 1)
                                                                                                     ))
                                                              
                                                                      (om-new-leafmenu "By Type" #'(lambda () 
                                                                                                     (change-frame-presentation (panel (editor win)) 2)
                                                                                             ;(omG-change-presentation win 2)
                                                                                                     ))
                                                                      )
                                                       (om-new-leafmenu "Align" #'(lambda () (omG-align-presentation win))))))))
                         ))
          ((equal menuid 'file)
           (om-make-menu "File" (append 
                                 (make-new-menu editor)
                                 (list
                                  (list 
                                   (om-new-leafmenu "Save" #'(lambda () 
                                                               (window-save win)) "s" 
                                                    (if (member "Save" disable :test 'string-equal) nil t))
                                   (om-new-leafmenu "Last Saved" #'(lambda () (window-last-saved (editor win))) nil 
                                                    (if (member "Last Saved" disable :test 'string-equal) nil t))
                                   )
                                  (list 
                                   (om-new-leafmenu "Close" #'(lambda () (om-close-window win)) "w" 
                                                    (if (member "Close" disable :test 'string-equal) nil t))
                                   )
                                  add-items
                                  (list 
                                 ;(om-new-leafmenu "Make Alias" #'(lambda () (alias-window win)) "m" 
                                 ;                 (if (member "Make Alias" disable :test 'string-equal) nil t))
                                   (om-new-leafmenu "Get Info" #'(lambda() (get-info-window win)) "i" 
                                                    (if (member "Get Info" disable :test 'string-equal) nil t))
                                   )
                                  (list 
                                   (om-new-leafmenu "Page Setup" #'(lambda () (om-page-setup)) nil 
                                                    (if (member "Page Setup" disable :test 'string-equal) nil t))
                                   (om-new-leafmenu "Print" #'(lambda () (om-print-window win)) "p" 
                                                    (if (member "Print" disable :test 'string-equal) nil t))
                                   )
                                  ))))
          ((equal menuid 'minifile)
           (om-make-menu "File" (append 
                                 (make-new-menu editor)
                                 (list 
                                  (om-new-leafmenu "Close" #'(lambda () (om-close-window win)) "w" 
                                                   (if (member "Close" disable :test 'string-equal) nil t))
                                  )
                                 )))
          ((equal menuid 'edit)
           (om-make-menu "Edit" (remove nil 
                                        (list
                                         (om-new-leafmenu "Undo" #'(lambda() (undo win)) "z" 
                                                          (if (member "Undo" disable :test 'string-equal) nil t))
                                         (list 
                                          (om-new-leafmenu "Cut" #'(lambda () (cut win)) "x" 
                                                           (if (member "Cut" disable :test 'string-equal) nil t))
                                          (om-new-leafmenu "Copy" #'(lambda () (copy win)) "c" 
                                                           (if (member "Copy" disable :test 'string-equal) nil t))
                                          (om-new-leafmenu "Paste" #'(lambda () (paste win)) "v" 
                                                           (if (member "Paste" disable :test 'string-equal) nil t))
                                          (om-new-leafmenu "Duplicate" #'(lambda () (duplicate-window win)) "d" 
                                                           (if (member "Duplicate" disable :test 'string-equal) nil t))
                                          (om-new-leafmenu "Clear" #'(lambda () (clear win)) nil 
                                                           (if (member "Clear" disable :test 'string-equal) nil t))
                                          )
                                         (om-new-leafmenu "Select All" #'(lambda () (select-all win)) "a" 
                                                          (if (member "Select All" disable :test 'string-equal) nil t))
                                         (when (font-menu editor)
                                           (om-make-menu "Font..." 
                                                         (list
                                                          (om-new-leafmenu "Bold" #'(lambda () (edit-bold editor)) "B")
                                                          (om-new-leafmenu "Italics" #'(lambda () (edit-italics editor)) "I"))))
                                         )))
           )
          ((equal menuid 'functions)
           (om-package-fun2menu *om-package-tree* nil #'(lambda (f) (add-box-from-menu f nil))))
          ((equal menuid 'classes)
           (om-package-classes2menu *om-package-tree* nil #'(lambda (c) (add-box-from-menu c nil))))
          ((equal menuid 'help)
           (om-make-menu "Help" (remove nil (append 
                                             (list
                                              (om-new-leafmenu "Editor Command Keys..." #'(lambda() (when editor
                                                                                                      (let ((obj (string-upcase (class-name (class-of (object editor))))))
                                                                                                        (show-help-window (format nil "Commands for ~A Editor" obj) 
                                                                                                                          (get-help-list editor))))) 
                                                               "H" (and (not (find "Commands" disable :test 'string-equal))
                                                                        (get-help-list editor) t)))
                                             (help-items editor)))
                         ))
          )))

(defmethod help-items ((self t)) nil)

(defmethod edit-bold ((self t)) nil)
(defmethod edit-italics ((self t)) nil)

;;; FILE et EDIT sont par defaut dans le LISTENER
(defmethod om-window-class-menubar ((self om-listener))
  (list (make-om-menu 'windows :disable '("Lisp Listener"))))

;=============================
; SPECIAL CLASSES/FUNCTIONS MENU
;=============================

(defvar *adding-a-box* nil)



(defun add-box-from-menu (item &optional pos)
  (let* ((window (om-front-window))
         (thepatch (get-patchpanel (editor window))))
    (setf *adding-a-box* (list item window))
    (when pos
      (om-view-click-handler thepatch (setf *new-obj-initial-pos* pos))
      )))

; (om-inspect (package-classes2menu *kernel-package* "ctrl"))

(defun empty-fun-p (pack)
  (and (null (functions pack))
       (or (null (subpackages pack))
           (let ((empty t))
             (loop for p in (subpackages pack) while empty do
                   (unless (empty-fun-p p) (setf empty nil)))
             empty))))
(defun empty-class-p (pack)
  (and (null (classes pack))
       (or (null (subpackages pack))
           (let ((empty t))
             (loop for p in (subpackages pack) while empty do
                   (unless (empty-class-p p) (setf empty nil)))
             empty))))


(defun package-fun2menu (package &optional name action)
  (let ((subpack (copy-list (subpackages package))))
    (om-make-menu (or name "Functions")
                  (list  
                   (mapcar #'(lambda (f) (om-new-leafmenu (name f) #'(lambda () (funcall action f))))
                           (functions package))
                   (remove nil 
                           (loop for item in subpack collect
                                 (when (and (not (and (omlib-p item) (not (loaded? item))))
                                            (or (not (empty-fun-p item)) (equal item *package-user*)))
                                   (package-fun2menu item (name item) action)))
                           )))))


(defun om-package-fun2menu (package &optional name action)
 (let ((subpack (copy-list (butlast (subpackages package)))))
    (om-make-menu (or name "Functions")
                  (list 
                   (mapcar #'(lambda (f) (om-new-leafmenu (name f) #'(lambda () (funcall action f))))
                           (functions package))
                   (remove nil 
                           (loop for item in (butlast subpack 2) collect
                                 (unless (and (omlib-p item) (not (loaded? item)))
                                   (package-fun2menu item (name item) action)))
                           )
                   #'(lambda (w) 
                       (list (package-fun2menu (car (last subpack 2)) (name (car (last subpack 2))) action)))
                   #'(lambda (w) 
                       (list (package-fun2menu  (car (last subpack)) (name (car (last subpack))) action)))
                   ))))

(defun package-classes2menu (package &optional name action)
  (let ((subpack (copy-list (subpackages package))))
    (om-make-menu (or name "Classes")
                  (list  
                   (mapcar #'(lambda (c) (om-new-leafmenu (name c) #'(lambda () (funcall action c))))
                           (classes package))
                   (remove nil 
                           (loop for item in subpack collect
                                 (when (and (not (and (omlib-p item) (not (loaded? item))))
                                            (or (not (empty-class-p item)) (equal item *package-user*)))
                                   (package-classes2menu item (name item) action)))
                           )))))


(defun om-package-classes2menu (package &optional name action)
  (let ((subpack (copy-list (butlast (subpackages package)))))
    (om-make-menu (or name "Classes")
                  (list  
                   (mapcar #'(lambda (c) (om-new-leafmenu (name c) #'(lambda () (funcall action c))))
                           (classes package))
                   (remove nil 
                           (loop for item in (butlast subpack 2) collect
                                 (when (and (not (and (omlib-p item) (not (loaded? item))))
                                            (or (subpackages item) (classes item) (equal item *package-user*)))
                                   (package-classes2menu item (name item) action))))
                   #'(lambda (w) 
                       (list (package-classes2menu (car (last subpack 2)) (name (car (last subpack 2))) action)))
                   #'(lambda (w) 
                       (list (package-classes2menu  (car (last subpack)) (name (car (last subpack))) action)))
                   ))))



;=============================
; MENUS
;=============================

(defmethod get-menubar ((self t)) nil)
(defmethod update-menubar ((self t)) nil)

(defmethod get-menubar ((self EditorView)) (list (make-om-menu 'minifile :editor self) 
                                                 (make-om-menu 'windows :editor self)))

(defmethod update-menubar ((self EditorView))
  (om-add-menu-to-win (om-view-window self)))

(defmethod get-menubar ((self NonRelationEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save As..." "Page Setup" "Print") :editor self) 
        (make-om-menu 'edit :disable '("Undo") :editor self)  
        (make-om-menu 'presentation :editor self) 
        (make-om-menu 'windows :disable '("Resources") :editor self)))

(defmethod get-menubar ((self WorkspaceEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save As..." "Make Alias" "Page Setup" "Print") :editor self
                      :more-items (list (list "Upgrade Workspace" 'upgrade-dialog)))
        (make-om-menu 'edit :disable '("Undo") :editor self)  
        (make-om-menu 'presentation :editor self) 
        (make-om-menu 'windows :disable '("Workspace") :editor self)
        (make-om-menu 'help :disable '("Commands") :editor self)))

(defvar *new-menu-items* nil)

(setf *new-menu-items*
      (list (list "New Patch" 'p "1")
            (list "New Maquette" 'm "2")
            (list "New Lisp Function" 'l nil)))

(defun make-menu-item (win text symb shortcut)
  (om-new-leafmenu text #'(lambda () (omG-make-new-icon-window win symb)) shortcut))

(defmethod make-new-menu ((self WorkspaceEditor))
  (list 
   (loop for item in *new-menu-items* collect (apply 'make-menu-item (cons (om-view-window self) item)))  
   (om-new-leafmenu "New Folder" #'(lambda() (omG-make-new-icon-window (om-view-window self) 'f)) "n")))
  
(defmethod get-menubar ((self FolderEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save As..." "Make Alias" "Page Setup" "Print") :editor self) 
        (make-om-menu 'edit :disable '("Undo") :editor self)  
        (make-om-menu 'presentation :editor self) 
        (make-om-menu 'windows :disable '() :editor self)
        (make-om-menu 'help :disable '("Commands") :editor self)))

(defmethod make-new-menu ((self FolderEditor))
  (list 
   (loop for item in *new-menu-items* collect (apply 'make-menu-item (cons (om-view-window self) item)))
   (om-new-leafmenu "New Folder" #'(lambda() (omG-make-new-icon-window (om-view-window self) 'f)) "n")))

(defmethod get-menubar ((self MaquetteEditor)) 
  (list (make-om-menu 'file :editor self 
                      :disable (if (mypathname (object self)) nil '("Save" "Save As..." "Last Saved")) 
                      :editor self)
        (make-om-menu 'edit :editor self)  
        (make-om-menu 'windows :disable '() :editor self)
        (make-om-menu 'help :editor self)))

(defmethod get-menubar ((self PatchEditor)) 
  (list (make-om-menu 'file :editor self :disable (if (mypathname (object self)) nil '("Save" "Save As..." "Last Saved")))
        (make-om-menu 'edit :editor self)  
        (make-om-menu 'classes :editor self) (make-om-menu 'functions :editor self) 
        (make-om-menu 'windows :disable '() :editor self)
        (make-om-menu 'help :editor self)))

(defmethod font-menu ((self patcheditor)) t)

(defmethod get-menubar ((self MethodEditor)) 
  (list (make-om-menu 'file :editor self :disable '("Save" "Save As..." "Last Saved") :editor self)
        (make-om-menu 'edit :editor self)  
        (make-om-menu 'classes :editor self) (make-om-menu 'functions :editor self) 
        (make-om-menu 'windows :disable '() :editor self)
        (make-om-menu 'help :editor self)))

(defmethod get-menubar ((self GlobalsfolderEditor)) (list (make-om-menu 'file :disable '("Last saved" "Save As..." "Make Alias" "Page Setup" "Print") :editor self) 
                                                   (make-om-menu 'edit :disable '("Undo") :editor self)  
                                                   (make-om-menu 'presentation :editor self) 
                                                   (make-om-menu 'windows :disable '("Global Variables") :editor self)))

(defmethod get-menubar ((self classtreeEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save" "Save As..." "Page Setup" "Print") :editor self) 
        (make-om-menu 'edit :disable '("Undo" "Cut" "Copy" "Paste" "Duplicate" "Undo") :editor self)  
        (make-om-menu 'windows :editor self)))

(defmethod make-new-menu ((self classtreeEditor))
  (list
   (om-new-leafmenu "New Class" #'(lambda () (omG-make-new-icon-window  (om-view-window self) 'c)) "n")
   ))

(defmethod get-menubar ((self classEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save" "Page Setup" "Print") :editor self) 
        (make-om-menu 'edit :disable '("Undo" "Cut" "Copy" "Paste" "Duplicate" "Undo") :editor self)  
        (make-om-menu 'windows :editor self)))

(defmethod make-new-menu ((self classEditor))
  (list
   (om-new-leafmenu "New Slot" #'(lambda () (omG-make-new-icon-window (om-view-window self))) "n")
   ))

(defmethod make-new-menu ((self GlobalsfolderEditor))
  (list (om-new-leafmenu "New Variable" #'(lambda() (omG-new-var (panel self))) "n")))


(defmethod get-menubar ((self PackageEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save" "Save As..." "Make Alias" "Page Setup" "Print") :editor self) 
        (make-om-menu 'edit :disable '("Cut" "Copy" "Paste" "Duplicate" "Undo") :editor self)
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self)))


(defmethod make-new-menu ((self PackageEditor))
  (list
   (list 
    (om-new-leafmenu "New User Package" #'(lambda () (omG-make-new-icon-window (om-view-window self) 'p)))
    (om-new-leafmenu "New Class" #'(lambda () (omG-make-new-icon-window (om-view-window self) 'c)))
    (om-new-leafmenu "New Generic Function" #'(lambda () (omG-make-new-icon-window (om-view-window self) 'f)))
    )
   (list 
    (om-new-leafmenu "Add Remote User Library..." #'(lambda () 
                                                      (let ((path (om-choose-directory-dialog :directory (om-user-home))))
                                                        (when path 
                                                          (add-one-lib path)))))
    (om-new-leafmenu "Refresh Libaries Package" #'(lambda () (reload-user-libs) (show-libs)))
    (om-make-menu "Load Library..." (list #'(lambda (x)
                                             (mapcar #'(lambda (lib)
                                                         (om-new-leafmenu (name lib) #'(lambda () (load-om-lib lib) (show-libs))))
                                                     (subpackages *library-package*))
                                             )))
    )))






(defmethod get-menubar ((self GenericFunEditor)) 
  (list (make-om-menu 'file :disable '("Last saved" "Save" "Save As..." "Make Alias" "Page Setup" "Print") :editor self) 
        (make-om-menu 'edit :disable '("Cut" "Copy" "Paste" "Duplicate" "Undo") :editor self)  
        (make-om-menu 'windows :editor self)))

(defmethod make-new-menu ((self GenericFunEditor))
  (list (om-new-leafmenu "New Method" #'(lambda () (omG-make-new-icon-window (om-view-window self) 'm)))))




;(defmethod get-menubar ((self instanceEditor)) *simplest-menubar*)

;(defmethod extra-menu-info ((self instanceEditor))
;  (disable-this-menu-items *window-menu-file*  '("New" "Save" "Duplicate" "Make alias"))
;  (disable-all-menu-items *window-menu-edit*))



                             

;======================================
;ACTIONS FOR CONTEX MENUS
;======================================

;===========================input-menu
(defmethod om-get-menu-context ((object input-funboxframe)) nil)

;===========================loop
(defmethod om-get-menu-context ((object loopboxframe))
  (list+ (list (om-new-leafmenu "Update Doc" #'(lambda () (apply-win (om-view-window object) 'update-doc)))) 
         (boxframe-default-list object)))

;===============WS and FOLDERS
(defmethod om-get-menu-context ((self metaobj-panel))
  (setf *new-obj-initial-pos* (om-mouse-position self))

  (list 
   (list 
    (apply 'om-new-menu (append (list "New...")
                                (loop for item in *new-menu-items* collect (apply 'make-menu-item (cons (om-view-window self) item)))               
                                (list (om-new-leafmenu "New Folder" #'(lambda() (omG-make-new-icon-window (om-view-window self) 'f))))
				)))
   (list 
    (om-new-leafmenu "Import File" #'(lambda () (import-file self)))
    (om-new-leafmenu "Import Folder" #'(lambda () (import-folder self)))
    ) ;(om-new-leafmenu "-" #'(lambda () nil))
   (om-new-menu "Presentation"
                (om-new-leafmenu "Icons" #'(lambda() (omG-change-presentation (om-view-window self) 0)
                                             (om-add-menu-to-win (om-view-window self))))
                (om-new-leafmenu "List" #'(lambda() (omG-change-presentation (om-view-window self) 1)
                                            (om-add-menu-to-win (om-view-window self)))))))

(defmethod om-get-menu-context ((self globalsFolderPanel))
  (list (om-new-menu "View"
                     (om-new-leafmenu "Icons" #'(lambda() (omG-change-presentation (om-view-window self) 0)
                                                  (om-add-menu-to-win (om-view-window self))))
                     (om-new-leafmenu "List" #'(lambda() (omG-change-presentation (om-view-window self) 1)
                                                 (om-add-menu-to-win (om-view-window self)))))))


(defmethod om-get-menu-context ((self icon-finder-icon))
  (om-get-menu-context (om-view-container self)))

(defmethod om-get-menu-context ((self icon-finder))
  (list (list 
         (om-new-leafmenu "Open" #'(lambda () (apply-win (om-view-window self) 'open-icon-win (om-make-point 0 0)))))
        (om-new-leafmenu "Get Info" #'(lambda () (get-info-window (om-view-window self))))
        (om-new-leafmenu "Duplicate" #'(lambda () (duplicate-window (om-view-window self))))
        (om-new-leafmenu "Export" #'(lambda () (export-file (object self))))))






;==========================
;DONT HAVE MENU
;==========================
(defmethod simplest-menu-context ((self t))  
  (list (list 
         (om-new-leafmenu "Open" #'(lambda () (apply-win (om-view-window self) 'open-icon-win (om-make-point 0 0)))))
        (om-new-leafmenu "Get Info" #'(lambda () (get-info-window (om-view-window self))))))



(defmethod om-get-menu-context ((self globals-folder-icon)) (simplest-menu-context self))
(defmethod om-get-menu-context ((self lispfun-icon-frame))   (simplest-menu-context self))
(defmethod om-get-menu-context ((self type-icon-frame))   (simplest-menu-context self))
(defmethod om-get-menu-context ((self instance-icon-frame))   (simplest-menu-context self))
(defmethod om-get-menu-context ((self slot-icon-frame))   (slot-menu-context self))


(defmethod simplest-box-menu-context ((self t))  
  (list (list (om-new-leafmenu "Open" #'(lambda () (OpenObjectEditor (object  self)))) )
        (om-new-leafmenu "Get Info" #'(lambda () (get-info-window (om-view-window self))))))


(defmethod om-get-menu-context ((self aliasboxframe))   (simplest-box-menu-context self))
(defmethod om-get-menu-context ((self classboxframe))   (simplest-box-menu-context self))
(defmethod om-get-menu-context ((self inframe))   (simplest-box-menu-context self))
(defmethod om-get-menu-context ((self TypedInFrame))   (simplest-box-menu-context self))


;==========================
;DONT HAVE MENU
;==========================
(defmethod om-get-menu-context ((self classPanel)) nil)
(defmethod om-get-menu-context ((self GenericFunPanel)) nil)
(defmethod om-get-menu-context ((self InstancePanel)) nil)
(defmethod om-get-menu-context ((self  outframe)) nil)
(defmethod om-get-menu-context ((self  tempOutFrame)) nil) 



;EDITOR BOXES


(defun boxframe-default-list (object)
  (list (list 
         (om-new-leafmenu  "Copy" #'(lambda () (copy (om-view-window object))))
         (om-new-leafmenu  "Cut" #'(lambda () (cut (om-view-window object))))
         (om-new-leafmenu  "Duplicate" #'(lambda () (duplicate-window (om-view-window object))))
         )
        (list 
         (om-new-leafmenu  "Get Info" #'(lambda () (get-info-window (om-view-window object))))
         (om-new-leafmenu  "Show Documentation" #'(lambda () (show-big-doc object))))
        (list
         (if (lock-button object)
             (om-new-leafmenu "Unlock Eval" #'(lambda () (apply-win (om-view-window object) 'add-rem-lock-button)))
           (om-new-leafmenu "Lock Eval" #'(lambda () (apply-win (om-view-window object) 'add-rem-lock-button))))
         (om-new-leafmenu "Eval Box" #'(lambda ()
                                    (om-eval-enqueue 
                                     `(progn
                                        (eval-box ,object)
                                        (clear-ev-once ,(om-view-container object)))))))))


(defmethod om-get-menu-context ((self icon-view))
  (om-get-menu-context (om-view-container self)))

(defmethod om-get-menu-context ((object omboxframe))
  (boxframe-default-list object))

(defmethod om-get-menu-context ((object patchboxFrame))
  (list+  (list (list (om-new-leafmenu "Update Doc"
                                       #'(lambda () (apply-win (om-view-window object) 'update-doc))))
                ) 
          (boxframe-default-list object)))


(defmethod om-get-menu-context ((self maquetteframe))
  (list+ (call-next-method)
         (list (if (= 0 (mode (object self)))
                 (om-new-leafmenu "Functional Inputs" #'(lambda () (change-mode (object self) 1)))
                 (om-new-leafmenu "Maquette Object Inputs" #'(lambda () (change-mode (object self) 0)))))
         ))


(defmethod om-get-menu-context ((self miniview))
  (om-get-menu-context (om-view-container self)))

(defmethod om-get-menu-context ((self commentview))
  (om-get-menu-context (om-view-container self)))
  
(defmethod om-get-menu-context ((object commentboxframe))
  (list (om-new-leafmenu "Text Color" #'(lambda () (color-comments (panel (om-view-window object)))))
        (om-new-leafmenu "Text Font" #'(lambda () (font-comments (panel (om-view-window object)))))))



(defmethod player-menu-item ((self t)) nil)

(defmethod om-get-menu-context ((object boxeditorframe))
  (remove nil 
   (list+  ;(list (om-new-leafmenu (if (view-of-patch (object object)) "Free View of Patch" "Set View of Patch")
          ;                 #'(lambda () (change-view-of-patch object))))
    (list (list 
           (import-menu (object object))
           (export-menu (object object))
           ))
    (player-menu-item (object object))
    (boxframe-default-list object))))


(defmethod om-get-menu-context ((self tempobjframe))
  (let (items)
    (setf items (remove nil 
                        (list
                         (remove nil 
                                 (list (om-new-leafmenu "Open" #'(lambda () (OpenObjectEditor (object self))))
                                       (when (patch-p (reference (object self)))    ;;  (Class-has-editor-p (get-mus-ob self))
                                         (om-new-leafmenu "Open Value" #'(lambda () 
                                                                           (let ((ed (or (editorframe (object self))
                                                                                         (editor (make-editor-window (get-editor-class (get-mus-ob (object self)))
                                                                                                           (get-mus-ob (object self)) (name (object self)) (object self))))))
                                                                             (setf (editorframe (object self)) ed)
                                                                             (om-select-window (window ed))))
                                                          nil (Class-has-editor-p (get-mus-ob (object self)))))
                                       (om-new-leafmenu "Get info" #'(lambda () (get-info-window (om-view-window self))))
                                       ))
                        
                         (if (and (pictu (object  self)) (thepict (pictu (object  self))))
                              (list (setf items (list+ items (list (om-new-leafmenu "Change Picture"
                                                                              #'(lambda () 
                                                                                  (let ((newpict (get-picture-file)))
                                                                                  (when newpict
                                                                                    (mapcar #'(lambda (frame) 
                                                                                                (setf (pictu (object frame)) 
                                                                                                      (copy-picture newpict 'om-picture))
                                                                                                (om-invalidate-view frame))
                                                                                            (get-actives (om-view-container self)))
                                                                                    ))))
                                                                   (om-new-leafmenu "Remove Picture"
                                                                              #'(lambda () 
                                                                                  (mapcar #'(lambda (frame) 
                                                                                              (setf (pictu (object frame)) nil)
                                                                                              (om-invalidate-view frame))
                                                                                          (get-actives (om-view-container self)))
                                                                                  ))))))
                             (list (setf items (list+ items (list (om-new-leafmenu "Set Picture"
                                                                            #'(lambda () 
                                                                                (let ((newpict (get-picture-file)))
                                                                                  (when newpict
                                                                                    (mapcar #'(lambda (frame) 
                                                                                                (setf (pictu (object frame)) (copy-picture newpict 'om-picture))
                                                                                                (om-invalidate-view frame))
                                                                                            (get-actives (om-view-container self)))
                                                                                    )))))))))
                          
                          (list (om-new-leafmenu "Player"
                                                 #'(lambda () (select-maquette-player (object self)))))

                       (list 
                        (if (mute (object self))
                            (om-new-leafmenu  "Mute Off" #'(lambda () 
                                                             (setf (mute (object self)) nil)
                                                             (om-invalidate-view self t)))
                          (om-new-leafmenu  "Mute" #'(lambda () 
                                                       (setf (mute (object self)) t)
                                                       (om-invalidate-view self t)))
                          )
                        ;(if (lock (object self))
                        ;    (om-new-leafmenu  "Unlock" #'(lambda () 
                        ;                                   (setf (lock (object self)) nil)
                        ;                                   (om-invalidate-view self t)))
                        ;  (om-new-leafmenu  "Lock" #'(lambda () 
                        ;                               (setf (lock (object self)) t)
                        ;                               (om-invalidate-view self t)))
                        ;  )
                        ))))
          
    items))





;;;========================
;;; Test IN /OUT dans la maquette
;;;========================

(defmethod add-input ((self t) pos) nil)
(defmethod add-output ((self t) pos) nil)

(defmethod om-get-menu-context ((self MaquettePanel))
  (let ((pos (om-mouse-position self)))
    (list 
     (list 
      (om-new-leafmenu "New Input" #'(lambda () (add-input self pos)))
      (om-new-leafmenu "New Output" #'(lambda () (add-output self pos)))
      )
     (list 
      (om-new-leafmenu "Connections"
                       #'(lambda () 
                           (setf (show-conect (params (object self))) (not (show-conect (params (object self)))))
                           (om-invalidate-view self))
                       nil t (show-conect (params (object self))))
      (om-new-leafmenu "Metric Ruler"
                       #'(lambda () 
                           (if (rulermetric (editor self))
                               (delete-ruler-metric (editor self))
                             (add-ruler-metric (editor self))))
                         nil t (rulermetric (editor self)))
      (om-new-leafmenu "Grid"
                       #'(lambda () (setf (grille-p self) (not (grille-p self)))
                           (om-invalidate-view self))
                       nil t (grille-p self))

      )
     (list 
      (om-new-leafmenu (string+ (om-str :bg-pict) "...")
                       #'(lambda () 
                           (let ((rep (picture+params-dialog (pictu (object self)) :defoption '(c 0 0 1000 100))))
                             (when rep
                               (setf (pictu (object self)) rep)
                               (om-invalidate-view self)))))

      (om-new-leafmenu (string+ (om-str :rulers-settings) "...")
                       #'(lambda () 
                           (let ((rep (edit-rulers-settings (object self))))
                             (when rep
                               ;;; XXXX
                               ))))

      (om-new-leafmenu "Player" #'(lambda () (select-maquette-player (object self))))
      )
     ;(list 
     ; (om-new-leafmenu "Eval" #'(lambda () (eval-maquette self)))
     ; (om-new-leafmenu "Play" #'(lambda () (play-from-palette self)))
     ; )
     (list 
      (om-new-leafmenu "Last Saved" #'(lambda () (window-last-saved (editor self))) nil
                       (not (om-maquette-abs-p (object (editor self)))))
      ))))






