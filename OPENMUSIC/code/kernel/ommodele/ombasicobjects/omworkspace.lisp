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
;This file defines the OMWorkspace meta-object, it is not a computable objects in OM.
;The Workspace implements an idea of persistence, that is you expect to find your workspace as you 
;left it at the end of the previous session. 
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defvar *current-workSpace* nil "The WorkSpace used in the current session.")
(defvar *om-workSpace-win* nil "The WorkSpace's editor window is always opened, this window is kept in this variable.")

(defvar *workSpace-win-size* nil "The WorkSpace's editor window size")
(defvar *workSpace-win-pos* nil "The WorkSpace's editor window position")

;-----------------
;The WorkSpace in OM
;--------------------------------------------------
#|
(defclass OMWorkSpace (OMPersistantFolder) 
   ((elements :initform nil :accessor elements)
    (packages :initform nil :accessor packages))
   (:documentation "The class of the OM Workspace. #enddoc#
#seealso# (OMFolder OMPackage) #seealso#
#elements# A list of objects contained in the Folder i.e. patches, maquettes or  folders. #elements#
#packages# Point to the main package of the session. #packages#"))
|#

;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------

(defmethod get-elements ((self OMWorkSpace)) 
   "WorkSpace's elements are folders patches or maquettes." (elements self))

(defmethod get-class-icon ((self OMWorkSpace)) 'ws-icon-frame)

(defmethod get-object-insp-name ((self OMWorkSpace)) "WorkSpace")

(defmethod presentation ((self OMWorkSpace))
  (if (and (ensure-ws-params self) (integerp (car (wsparams self))))
    (car (wsparams self)) 1))

(defmethod set-presentation ((self OMWorkSpace) val)
  (ensure-ws-params self)
  (when (wsparams self)
    (setf (nth 0 (wsparams self)) val)))

;--------------------------------------------------
;Other Methods
;--------------------------------------------------
;For workspaces the elements are not in the pathname, but in "pathname:elements:" directory.
(defmethod elements-pathname ((self OMWorkSpace)) 
   (om-make-pathname 
    :device (mypathname self)
    :directory  (append (pathname-directory (mypathname self)) (list "elements"))))

(defmethod InitWorkSpace ((self OMWorkSpace))
   "Load the globals folder, the package folder, the preferences file and all elements of 'self'."
   (declare (special *om-package-tree* *package-user*))
   (om-with-load-verbose nil
     (om-with-compiler-warnings nil
       
       (init-globalsfolder)
       
       (setf *current-workSpace* self)
       
       (setf *package-user* (add-new-packages (list "User" nil nil nil nil) *om-package-tree*))
       (init-user-pathname)
       (load-package-from-folders)
       (ws-load-user-pack)
       
        (load-elements self)  ;; will also open the listener
        (setf (packages *current-workSpace*) (list *om-package-tree*))
        
        (setf (mypathname *om-package-tree*) (mypathname *current-workspace*))
        (setf (mypathname *om-globalsfolder*) 
              (om-make-pathname :directory (append (pathname-directory (mypathname self)) (list "globals"))))
        (setf *all-files* (loop for i in (all-ws-files *current-workspace*)
                                collect (mypathname i))) ;;;loads all elements/files in variable (for send/receive probe)

       ; (set-presentation self 0);view mode Icons by default
        
        (setf *om-white-color* *gen-bg-color*)
        (AddPackage2Pack *om-globalsfolder* *om-package-tree*)
     )))


(defvar *loading-ws* nil)

;(init-output)
(defun init-output ()
  (let ((om-lisp::*om-prompt* "")
        (initstr
         (format nil 
                 "====================
 OpenMusic v. ~D                            
 r. ~A
 (c) IRCAM - Representations Musicales
 http://repmus.ircam.fr/openmusic/
 ====================
" 
                 *version-str-full* *release-date*)))
    (om-print initstr nil)
    ))


(defmethod load-elements ((self OMWorkSpace))
   "Make instances for all elements in 'self', but their are not yet loaded."
   (let ((j -1)
         (skip-libs *skip-libs*)
         (elements (om-directory (om-make-pathname :device (mypathname self) 
                                                   :directory (append (pathname-directory (mypathname self))
                                                                      (list "elements"))) :files t :directories t)))
     (setf *loading-ws* t)

     ;(oa::om-make-new-listener :initial-lambda #'(lambda () (in-package :om)) :input *listener-input*)
     ;(init-output)
     (om-print (string+ "Loading workspace: '" (name self) "' ..."))
     (setf (elements self) (remove nil (mapcar #'(lambda (x) (ws-load-element x (incf j))) elements) :test 'equal))
     (setf *loading-ws* nil)
     (setf *skip-libs* skip-libs)
     (when *error-files* 
       (print "==============================================")
       (print "Some files could not be loaded in the workspace (see documentation window).")
       (om-show-output-lines (append (list "THE FOLLOWING FILES COULD NOT BE LOADED IN THE WORKSPACE:") *error-files*)))
     (om-print "Workspace loaded")
     
     ))

(defmethod get-editor-class ((self OMWorkSpace)) 'WorkSpaceEditor)

(defmethod def-ws-params ((self OMWorkSpace))
  (list 1 (om-make-point 20 50) (om-make-point 500 400)))

(defmethod ShowObjectEditor ((self OMWorkSpace))
  (if (and *om-workSpace-win* (Editorframe self))
      (om-select-window *om-workSpace-win*)
    (let ((i 0) (j 0))
      (setf *workSpace-win-size* (nth 2 (ensure-ws-params self)))
      (setf *workSpace-win-pos* (nth 1 (ensure-ws-params self)))
      (setf *om-workSpace-win*
            (make-editor-window (get-editor-class self) *current-workSpace* (name self) nil 
                                :winsize *workSpace-win-size*             
                                :winpos *workSpace-win-pos*             
                                :close-p nil
				:winshow t))
      (setf (editorFrame self) (panel *om-workSpace-win*))
         
      (om-with-delayed-update (panel *om-workSpace-win*)
       
        (mapc #'(lambda (elem)
                  (add-icon-finder
                   (make-icon-from-object elem
                                          (om-point-h (get-icon-pos elem)) 
                                          (om-point-v (get-icon-pos elem))
                                          1
                                          (incf i))
                   (panel *om-workSpace-win*)))
              (sort-subframes (panel *om-workSpace-win*) 
                              (elements *current-workSpace*)))
        )
      (if (= 0 (get-pref (find-pref-module :appearance) :folder-pres))
          (omG-change-presentation *om-workSpace-win* 0)
        (omG-change-presentation *om-workSpace-win* 1))
      (om-invalidate-view (editor *om-workSpace-win*))
      (set-field-size (panel *om-workSpace-win*))
      *om-workSpace-win*)))
 


(defmethod save-all-persistants ((self omworkspace))
   "This method is called by the WorkSpace at the end of the OM session."
   (loop for item in (elements self) do
         (save-all-persistants item)))

;--------------------------------------------------
;Tools
;--------------------------------------------------

;----Builder
(defun make-new-WorkSpace (name)
   "Make an instance of the OMWorkspace class."
   (let ((new-ws (make-instance 'OMWorkSpace
                   :icon 203
                   :name (name-of-directory name)
                   :mypathname name)))
     new-ws))


;----Init session

;Choose the workspace path
(defmethod start-from-ws ((name t))
  (show-workspaces-dialog))

(defmethod start-from-ws ((name string))
   (init-om-session (OMRoot (string+ "workspaces;" name ";"))))

(defmethod start-from-ws ((name list))
   (let ((string (car name)))
     (cond
      ((or (stringp string) (directory-pathname-p string))
       (init-om-session string))
      (t (show-workspaces-dialog)))))

(defmethod start-from-ws ((name pathname))
  (init-om-session name))


;Init the WorkSpace specified by pathname.
(defun init-OM-session (pathname)
   (declare (special *patch-menu-functions* *patch-menu-classes* *om-package-tree*))
   ;(setf *splash-screen* (show-kero-pict nil))
   

   
   (init-om-package)                    
   (load-om-libs)
   
   
   ;load all defs for completion
   (init-all-defs)

   ;;; check if it is the right place...
   ;;; start-audio now
   (mapcar 'player-open *enabled-players*)
   
   (workspace-from-name pathname)      ;; will set the preferences             
      
   (catch :load-prefs 
     (handler-bind 
         ((error #'(lambda (err)
                     (om-message-dialog (format nil "Warning: An error occurred while setting the workspace preferences.~%=> ~A" err))
                     (delete-file (preferences-file) nil)
                      (setf *saved-pref* nil)
                      (throw :load-prefs :err)
                      )))
       (restore-preferences)
       ))
   
   (oa::om-make-new-listener :initial-lambda #'(lambda () (in-package :om)) :input *listener-input*)
   (init-output)

   (initWorkSpace *current-workSpace*) 
   
   (libs-autoload)

   (set-ompref 'prev-ws (mypathname *current-workSpace*))
   (save-omprefs)
   (save-preferences);test
   ;(om-close-window *splash-screen*)
   ;(setf *splash-screen* nil)
   (ShowObjectEditor *current-workSpace*)
   )


;This function load all files in the PATCHES folder.
#|
(defun Load-modif-patches ()
  (om-with-redefinition-warnings nil
    (om-with-compiler-warnings nil
      (om-with-load-verbose nil
        (when (probe-file (OMRoot "patches;"))
          (mapc #'(lambda (file)
                    (om-load-file file :verbose nil)) 
                (sort (append (om-directory  (OMRoot "patches;") :type "lisp" :files t :directories nil)
                              (om-directory  (OMRoot "patches;") :type *om-compiled-type* :files t :directories nil)) 'string< :key 'pathname-name)))
        
        ))))
|#

(defun get-init-patches-folder () 
  (merge-pathnames (make-pathname 
                    :directory 
                    (cons :relative 
                          #-macosx(list "init")
                          #+macosx(if (member :om-deliver *features*) 
                                      (list (concatenate 'string "OM " *version-str* ".app") "Contents" "Init")
                                    (list "init"))
                          ))
                   *om-root*))
   
 
(defun load-modif-patches ()
  (om-with-redefinition-warnings nil
    (om-with-compiler-warnings nil
      (om-with-load-verbose nil
        (check-folder (get-init-patches-folder))
        (mapc #'(lambda (file)
                  (om-load-file file :verbose nil))
              (sort (append (om-directory  (get-init-patches-folder) :type "lisp" :files t :directories nil)
                            (om-directory  (get-init-patches-folder) :type *om-compiled-type* :files t :directories nil)) 
                    'string< :key 'pathname-name))
        ))))

; (load-modif-patches)

(defun get-preferences-version (name)
  (let (rep)
    (WITH-OPEN-FILE (in (om-make-pathname :directory name :name "preferences" :type "lisp")
                         :direction :input 
                         :if-does-not-exist nil) 
       (let ((char (read-char in)))
         (when (equal char #\;)
           (setf rep (read-line in)))))
    rep))


(defvar *ws-params* nil)

(defun workspace-from-name (name)
  (let ((preffile (om-make-pathname :directory  name :name  "preferences" :type "lisp")))
    (when (probe-file (om-make-pathname :directory name :name  "modifs" :type "lisp"))
      (om-load-file (om-make-pathname :directory name :name  "modifs" :type "lisp")))
    (unless (probe-file (om-make-pathname :directory name))
      (om-create-directory (om-make-pathname :directory name) :if-exists nil))
    (unless (probe-file (om-make-pathname :directory name :name  "preferences" :type "lisp"))
      (WITH-OPEN-FILE (out (om-make-pathname :directory name :name  "preferences" :type "lisp")
                           :direction :output 
                           :if-does-not-exist :create :if-exists :supersede
			   :external-format :utf-8) 
        (write-line (format nil ";~D" *om-version*) out)
        (prin1 '(in-package :om) out)))
    (unless (probe-file (om-make-pathname :device name :directory (append (pathname-directory name) (list "elements"))))
      (om-create-directory (om-make-pathname :device name :directory (append (pathname-directory name) (list "elements"))) :if-exists nil))
    (unless (probe-file (om-make-pathname :device name :directory (append (pathname-directory name) (list "globals"))))
      (om-create-directory (om-make-pathname :device name :directory (append (pathname-directory name) (list "globals"))) :if-exists nil))
    (unless (probe-file (om-make-pathname :device name :directory (append (pathname-directory name) (list "user"))))
      (om-create-directory (om-make-pathname :device name :directory (append (pathname-directory name) (list "user"))) :if-exists nil))
    ;;; --- new for resources
    (unless (probe-file (om-make-pathname :device name :directory (append (pathname-directory name) (list "resources"))))
      (om-create-directory (om-make-pathname :device name :directory (append (pathname-directory name) (list "resources"))) :if-exists nil))
    (unless (probe-file (om-make-pathname :device name :directory (append (pathname-directory name) (list "resources" "icon"))))
      (om-create-directory (om-make-pathname :device name :directory (append (pathname-directory name) (list "resources" "icon"))) :if-exists nil))
    (unless (probe-file (om-make-pathname :device name :directory (append (pathname-directory name) (list "resources" "pict"))))
      (om-create-directory (om-make-pathname :device name :directory (append (pathname-directory name) (list "resources" "pict"))) :if-exists nil))

  
    (unless (get-preferences-version name)
      (WITH-OPEN-FILE (out preffile
                           :direction :output 
                           :if-does-not-exist :create :if-exists :supersede
			   :external-format :utf-8) 
        (write-line (format nil ";~D" *om-version*) out)
        (prin1 '(in-package :om) out)))
    (handler-bind 
        ((error #'(lambda (err)
                    (om-message-dialog (format nil "Warning: An error occurred while loading the workspace preferences.~%=> ~A" err))
                    (delete-file preffile nil))))
      (om-load-file preffile))  
    
    
    (setf  *current-workSpace* (make-new-WorkSpace name))
  
    (when (probe-file (om-make-pathname :directory name :name  "wsparams" :type "lisp"))
      (om-load-file (om-make-pathname :directory  name :name  "wsparams" :type "lisp")))

    (when *ws-params*
      (setf (wsparams *current-workSpace*) *ws-params*))
  
    ;;; new
    (user-init-funcs)
    
    (put-all-preferences);init pref. file - required when creating new workspace
  
    ))

(defvar *libs-auto-load* nil)

; (libs-autoload)

(defun libs-autoload ()
  (setf *libs-auto-load*
        (remove 
         nil 
         (loop for lib in *libs-auto-load*
               collect (let* ((libname (lib-true-name (car lib)))
                              (omlib (exist-lib-p libname)))
                         (if omlib
                             (progn 
                               (load-om-lib omlib)
                                 lib)
                           (progn 
                             (om-message-dialog (string+ "Library " (car lib) " not found: will be removed from auto-load list."))
                             nil))))
         )))

;;;;;;;;;;;;;;;;;
(defvar *user-init-funcs* "les fonctions qui sont chargees apres initialisation du workspace")
(setf *user-init-funcs* nil)
(defun add-init-user-func (func-name)
  (unless (member func-name *user-init-funcs* :test 'equal)
      (push func-name *user-init-funcs*)))

(defun user-init-funcs ()
   (mapc #'(lambda (x) (funcall x)) (reverse *user-init-funcs*)))
;;;;;;;;;;;;;;;;;




;=============End of a session===================

(defmethod save-all-persistants ((self t))
   "This method is called by the WorkSpace at the end of the OM session."
   t)


(defun save-ws-ws-params ()
  (when (and *current-workSpace* *om-workSpace-win*)
    (let ((path (om-make-pathname :directory  (mypathname *current-workSpace*) :name  "wsparams" :type "lisp")))
      (delete-file-protection path)
      (WITH-OPEN-FILE (out path :direction :output  :external-format :utf-8
                           :if-does-not-exist :create :if-exists :supersede) 
        (prin1 '(in-package :om) out)
        (prin1 `(setf *ws-params* (list ,(omng-save (presentation *current-workSpace*))
                                         ,(omng-save (om-view-position *om-workSpace-win*))
                                         ,(omng-save (om-view-size *om-workSpace-win*)))) out))
      )))



(defun save-ws-contents ()
  (when *current-workspace*
    (show-message-win "Saving workspace...")
    (setf *save-apply-all* nil)
    (om-with-cursor *om-wait-cursor*
      (Save-ws-ws-params)
      ;; sauve les valeurs, noms et positions des globals
      (ws-save-globals)
      ;; sauve juste la position du package user 
      (ws-save-user-package)
      (omng-save-ws *current-workSpace*))
    (setf *save-apply-all* nil)
    (hide-message-win)))


;=============Generation of new WorkSpace in Disk

(defun Make-new-WS-folder ()
   (let ((wspath (OMRoot "workspaces;")))
     (om-create-directory wspath)))


(defun Make-one-new-WS-folder (name)
   (let ((wspath (om-make-pathname :device (OMRoot "workspaces;")
                                :directory  (append (pathname-directory (OMRoot "workspaces;")) (list name ))))
         (*package* (find-package :om)))
     (om-create-directory wspath :if-exists nil)
     (om-create-directory (om-make-pathname :device wspath :directory  (append (pathname-directory wspath) (list "elements" ))) :if-exists nil)
     (om-create-directory (om-make-pathname :device wspath :directory  (append (pathname-directory wspath) (list "globals" ))) :if-exists nil)
     (om-create-directory (om-make-pathname :device wspath :directory  (append (pathname-directory wspath) (list "user" ))) :if-exists nil)
     ;;; --- new for resources
     (om-create-directory (om-make-pathname :device wspath :directory  (append (pathname-directory wspath) (list "resources" ))) :if-exists nil)
     (om-create-directory (om-make-pathname :device wspath :directory  (append (pathname-directory wspath) (list "resources" "icon"))) :if-exists nil)
     (om-create-directory (om-make-pathname :device wspath :directory  (append (pathname-directory wspath) (list "resources" "pict"))) :if-exists nil)
     ;;; ---
     (let ((thepath (om-make-pathname :directory wspath :name  "preferences" :type "lisp")))
       (delete-file-protection thepath)
       (WITH-OPEN-FILE (out thepath :direction :output :external-format :utf-8
                            :if-does-not-exist :create :if-exists :supersede) 
         (write-line (format nil ";~D" *om-version*) out)
         (prin1 '(in-package :om) out)
         )
       (put-all-preferences);necessaire pour init les prefs
       ;(setf thepath (make-pathname :directory (pathname-directory wspath) :name  "userpackage" :type "lisp"))
       ;(delete-file-protection thepath)
       ;(WITH-OPEN-FILE (out thepath :direction :output  :external-format :utf-8
       ;                     :if-does-not-exist :create :if-exists :supersede) 
       ;  (prin1 '(in-package :om) out)
       ;  (prin1 '(setf *package-user* (add-new-packages (list "User" nil nil nil nil)  *om-package-tree*)) out)
       ;  (prin1 '(init-user-pathname) out))
       )
     ))


;;;==================================================
;; OM PREFS

(defvar *omprefs* nil)

(defun ompref-file ()
  (let* ((userpref (om-user-pref-folder)))
    (make-pathname
     :device (pathname-device userpref) :host (pathname-host userpref)
     :directory (append (pathname-directory userpref) 
                        (list "OpenMusic" 
                              (cl-user::version-to-string *version* nil nil)
                              ))
     :name "OMPrefs" :type "lisp")))

(defmethod save-omprefs ()
  (let ((path (ompref-file)))
    (delete-file-protection path)
    (om-create-directory (om-make-pathname :directory path) :if-exists nil)
    (WITH-OPEN-FILE (out path :direction :output  :external-format :utf-8
                         :if-does-not-exist :create :if-exists :supersede) 
      (write-line (format nil ";~D" *om-version*) out)
      (prin1 '(in-package :om) out)
      (prin1 `(setf *omprefs* ,(omng-save *omprefs*)) out))
    t))

(defun get-ompref (key)
  (cadr (find key *omprefs* :test 'equal :key 'car)))

(defun set-ompref (key val)
  (let ((pos (position key *omprefs* :test 'equal :key 'car)))
    (if pos 
      (setf (nth pos *omprefs*) (list key val))
      (setf *omprefs* (append *omprefs* (list (list key val)))))))
