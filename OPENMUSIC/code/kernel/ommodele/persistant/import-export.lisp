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
; Import/Export OM objects from/to files
;DocFile

(in-package :om)

(defvar *last-imported* nil)
(defvar *last-exported* nil)

(defvar *import-log* nil)
(defvar *error-files* nil)
; (mapcar 'print *import-log*)

;;; ADD IMPORTED OBJ AS AN ICON
(defun add-object-frame (obj container pos)
  (let ((new-frame (make-icon-from-object obj (om-point-h pos) (om-point-v pos) 1 1)))
    (unless (big-icon-p (editor container))
     (let* ((frames (get-subframes container))
            (newcol (findcol container frames new-frame)))
       (setf (col new-frame) newcol)))
    (add-icon-finder new-frame container)
    (set-field-size container)))

;;; IMPORT FILE
(defun import-file (container)
  (catch-cancel
    (let ((pos (om-mouse-position container))
          (path (om-choose-file-dialog :prompt "Import File... " :button-string "Import " 
                                       :directory *last-imported*
                                       :types '("All OM Documents" "*.omp;*.omm;*.oml" "OM Patch" "*.omp" 
                                                               "OM Maquette" "*.omm" "OM Lisp Function" "*.oml" 
                                                               "All Files" "*.*"))))
      (when path
        (setf *last-imported* (om-make-pathname :directory path))
        (import-file-to-ws container path pos)))))


(defun import-file-to-ws (container path pos)
  (handler-bind ((error #'(lambda (c) 
                            (om-message-dialog (format nil "Error while importing ~D ~%   ~D ~%"
                                                       (namestring path) 
                                                       (om-report-condition c)
                                                       )
                                               :size (om-make-point 300 300))
                            (om-abort)
                            )))
    (setf *import-log* nil)  
    (let ((type (file-type path)))
      (cond
       ((equal :patc type) 
        (make-new-file container path 'omNG-make-new-patch pos))
       ((equal :maqt type)
        (make-new-file container path 'omNG-make-new-maquette pos))
       ((equal :lisp type)
        (make-new-file container path 'omNG-make-new-lisp-patch pos))
       (t (let ((mkfunc (mkobjfromtype type)))
            (if mkfunc 
                (make-new-file container path mkfunc pos)
              (om-beep-msg (format nil "File ~A is not a valid OM document" path))))))
      )))


;;; SEARCH TYPE
(defun find-type-by-loading (pathname)
  (let ((rec *om-current-persistent*)
        (skip-libs *skip-libs*)
        (om-search-all *om-search-all*)
        (user-search-all *user-search-all*)
        (file-search-all *file-search-all*)
        (rep nil))
    ; (setf *skip-libs* :all)
    (setf *om-search-all* 0)
    (setf *user-search-all* 0)
    (setf *file-search-all* 0)
    (handler-bind ((error #'(lambda (c)
                             (pushr (print (string+ "Error while checking " (namestring pathname) ": "
                                                    (om-report-condition c))) *import-log*)
                             (setf *om-current-persistent* rec)
                             (setf *skip-libs* skip-libs)
                             (throw 'check-error :ERROR)
                             )))
      (pushr (print (format nil "Type of ~s could not be found... File will be loaded for contents checking." (namestring pathname))) 
             *import-log*)
      (setf *om-current-persistent* nil)
      (om-load-file pathname)
      (setf rep 
            (cond ((and (member :omsheet *features*) (subtypep (type-of *om-current-persistent*) 'OMSheet)) :OMSH)
                  ((subtypep (type-of *om-current-persistent*) 'OMMaquette) :MAQT)
                  ((subtypep (type-of *om-current-persistent*) 'OMPatch) :PATC)
                  (t nil)))
      (if rep 
          (pushr (print (format nil "Type of ~s: ~A" (namestring pathname) (string rep))) *import-log*)
        (pushr (print (format nil "Type of ~s  definitely not found." (namestring pathname))) *import-log*))
      )
    (setf *om-current-persistent* rec)
    ;(setf *skip-libs* skip-libs)
    (setf *om-search-all* om-search-all)
    (setf *user-search-all* user-search-all)
    (setf *file-search-all* file-search-all)
    rep))

(defun file-type (pathname)
  "Return a keyword indicating the type of pathname"
  (let ((rep :????))
    (if (file-has-comments pathname) 
     (if (file-has-old-comments pathname)
         (progn
           (print "=============================================")
           (pushr (print (format nil "File ~s has old header format.." (namestring pathname))) *import-log*)
           (setf rep (read-old-type pathname)))
       (setf rep (second (get-pathname-wsparams pathname))))
      (progn 
        (print "=============================================")
        (pushr (print (format nil "File ~s has no header.." (namestring pathname))) *import-log*)))
    (when (and (> (length (pathname-name pathname)) 0)
               (or (equal rep :????) (not (symbolp rep))))
      (setf rep (catch 'check-error (find-type-by-loading pathname)))
      (unless rep 
        (with-open-file (f pathname)
          (if (file-eof-p f) (setf rep :EMPTY))))
      )
    rep))


;;; OBJECT FROM FILE
; (find-type-by-loading #P"/Users/bresson/Desktop/EMER/elements/abstracts/pw->om/info.omp")
; (object-from-file "/Users/bresson/Dropbox/workspaces/om-acmmm/elements/Patch 2.omp.tmp")

(defun object-from-file (pathname)
  (let ((pathtype (catch 'check-error (file-type pathname)))
        (name (real-pathname-name pathname))
        newobj)
    (cond
     ((equal pathtype :PATC)          
      (setf newobj (omNG-make-new-patch name (om-make-point 20 20))))
     ((equal pathtype :MAQT)
      (setf newobj (omNG-make-new-maquette name (om-make-point 20 20))))
     ((equal pathtype :LISP)          
      (setf newobj (omNG-make-new-lisp-patch name (om-make-point 20 20))))
     ((equal pathtype :EMPTY)
      (pushr (print (format nil "!!! Element ~s could not be loaded. File is EMPTY !" (namestring pathname))) *import-log*)
      (pushr (string+ (namestring pathname) " -->  File is empty") *error-files*))
     ((equal pathtype :ERROR)
      (pushr (string+ (namestring pathname) " -->  Error") *error-files*)
      (pushr (car (last *import-log*)) *error-files*)
      (pushr (print (format nil "!!! Element ~s could not be loaded. Error in File !" (namestring pathname))) *import-log*))
     ((equal pathtype :FOLD)
      ; a folder params file was caught as a document
      (pushr (print (format nil "Element ~s is an old folder params file. File will be removed." (namestring pathname))) *import-log*)
      (om-delete-file pathname))
     (pathtype (setf newobj (ws-load-more-element pathtype pathname name)))
     (t (pushr (print (format nil "!!! Element ~s could not be loaded" (namestring pathname))) *import-log*)
        (pushr (string+ (namestring pathname) " -->  Unknown Error") *error-files*)))
    (when newobj
      (when (or (not (file-has-comments pathname)) (file-has-old-comments pathname))
        (setf (omversion newobj) 4))
      (setf (loaded? newobj) nil))
    newobj))


;;; IMPORT VALID FILE
(defun make-new-file (self pathfile fun pos)
  (let ((abort-import nil)
        (name (real-pathname-name pathfile)))
   (loop while (and (not abort-import) (name-exist-p (object self) name)) do
         (let ((userrep (om-get-user-string (format nil "This name already exists! Please choose a new name or cancel to abort importation."))))
           (if userrep (setf name userrep) (setf abort-import t))))
   (unless abort-import
     (let* ((newpatch (funcall fun name pos)))
       (setf (mypathname newpatch) (om-make-pathname :directory (pathname-directory (elements-pathname (object self)))
                                                     :host (pathname-host (elements-pathname (object self)))
                                                     :device (pathname-device (elements-pathname (object self)))
                                                     :name name
                                                     :type (obj-file-extension newpatch)))
       (copy-file-sp pathfile (mypathname newpatch))            
       (if (or (not (file-has-comments (mypathname newpatch))) (file-has-old-comments (mypathname newpatch)))
         (setf (omversion newpatch) 4)
         (let ((wsparams (get-init-wsparams (mypathname newpatch))))
            (set-doc newpatch (or (str-with-nl (sixth wsparams)) ""))
            ;;; test
            (setf (omversion newpatch) (car wsparams))))
       (set-finder-comment (mypathname newpatch) newpatch)
       (setf (loaded? newpatch) nil)
       (really-add (object self) newpatch)
       (add-object-frame newpatch self pos)
       t))))

(defmethod mkobjfromtype (type) nil)

;;; IMPORT FOLDER

(defun import-folder (container)
  (catch-cancel
     (let ((pos (om-mouse-position container))
           (dir (om-choose-directory-dialog :directory *last-imported*)))
    (when (and dir (directoryp dir))
      (setf *last-imported* (om-make-pathname :directory (butlast (pathname-directory dir))
                                              :device (pathname-device dir)  :host (pathname-host dir)))
      (make-new-folder container dir pos)))))

(om-directory "Users/bresson/OM-SRC/OPENMUSIC/resources/sample-ws/OM-Tutorials/elements/tutorial patches/" :files t :directories t)

;;; creates a new OM folder from an external folder
(defun make-new-folder (self pathfile pos &optional newname)  
  (let ((abort-import nil)
        (name (or newname (name-of-directory pathfile))))
   (loop while (and (not abort-import) (name-exist-p (object self) name)) do
         (let ((userrep (om-get-user-string (format nil "This name already exists! Please choose a new name or cancel to abort importation."))))
           (if userrep (setf name userrep) (setf abort-import t))))
     (unless abort-import
       (setf *import-log* nil)
       (setf *error-files* nil)
       (show-message-win (string+ "Importing folder: " (namestring pathfile)))
       (let* ((targetdir (make-pathname :directory (append (pathname-directory (elements-pathname (object self)))
                                                          (list name))))
              (newfolder (omNG-make-new-folder name pos))
              (sub-elts (om-directory pathfile :files t :directories t :type '("omp" "omm" "oml" "oms"))))
           (setf (mypathname newfolder) targetdir)
           (om-create-directory (mypathname newfolder))
           (let ((info (find "finderinfo" sub-elts :test 'string-equal :key 'pathname-type)))
             (when info
               (copy-file-in-dir info (mypathname newfolder))
               (let ((wsparams (get-init-wsparams (mypathname newfolder))))
                    ;(set-finder-comment (mypathname newobj) newobj)
                 (setf (wsparams newfolder) (loop for i in (subseq wsparams 2 5) collect (eval i)))
                 (set-doc newfolder (or (str-with-nl (sixth wsparams)) ""))
                 (setf (omversion newfolder) (car wsparams)))
               ))
           (setf (elements newfolder) (remove nil (mapcar #'(lambda (elt) (ws-import-element elt (mypathname newfolder))) sub-elts) :test 'equal))
           (really-add (object self) newfolder)
           (add-object-frame newfolder self pos)
           (omNG-save-ws newfolder)
           (hide-message-win)
           t))))

;;; FOR OTHER DOC TYPES (SHEET, ML-MAQUETTE, ETC.)
(defmethod ws-load-more-element (type path name) 
  (pushr (print (format nil "Element ~s could not be loaded: unknown type '~s'" path type)) *import-log*)
  (pushr (string+ (namestring path) " -->  Unknown Type: :" (string type))  *error-files*)
  nil)


;;; .finderinfo !!

(defun ws-import-element (element targetdir)
  (handler-bind ((error #'(lambda (c) 
                            (om-message-dialog (format nil "Error while importing ~A: ~D"
                                                       (namestring element) 
                                                       (om-report-condition c))
                                               :size (om-make-point 300 300))
                            (om-abort)
                 ))) 
      (let (newobj)
        ;(print (list element (pathname-name element) (pathname-type element)))
        (if (and (directoryp element) (not (systemdirectoryp element)))
            
            (let ((elements (om-directory element :files t :directories t :type '("omp" "omm" "oml" "oms"))))
              (setf newobj (omNG-make-new-folder (name-of-directory element) (om-make-point 20 20)))
              (setf (mypathname newobj) (make-pathname :device (pathname-device targetdir)
                                                       :directory  (append (pathname-directory targetdir) (list (name newobj)))))
              (om-create-directory (mypathname newobj))
              
              (let ((info (find "finderinfo" elements :test 'string-equal :key 'pathname-type)))
                (when info
                  (copy-file-in-dir info (mypathname newobj))
                  (let ((wsparams (get-init-wsparams (mypathname newobj))))
                    ;(set-finder-comment (mypathname newobj) newobj)
                    (setf (wsparams newobj) (loop for i in (subseq wsparams 2 5) collect (eval i)))
                    (set-doc newobj (or (str-with-nl (sixth wsparams)) ""))
                    (setf (omversion newobj) (car wsparams)))
                  ))
              
              (setf (elements newobj) (remove nil (mapcar #'(lambda (elt) (ws-import-element elt (mypathname newobj))) elements) :test 'equal)))
          
          (when (and (stringp (pathname-name element))
                     (not (string-equal "" (pathname-name element))))
            (let ((obj (object-from-file element)))
              (print (string+ "Importing " (namestring element) "..."))
              (when obj
                (setf (mypathname obj) (make-pathname :device (pathname-device targetdir)
                                                      :directory (pathname-directory targetdir)
                                                      :name (pathname-name element)
                                                      :type (obj-file-extension obj)))
                (copy-file-sp element (mypathname obj))
                (setf newobj obj)))
            ;(if (string-equal "finderinfo" (pathname-type element))
            ;    (copy-file-in-dir element targetdir))
            
            ))
        (when newobj
          (let ((wsparams (get-init-wsparams (mypathname newobj))))
            (set-finder-comment (mypathname newobj) newobj)
            (pushr (format nil "~s: header updated" (mypathname newobj)) *import-log*)
            (setf (wsparams newobj) (loop for i in (subseq wsparams 2 5) collect (eval i)))
            (set-doc newobj (or (str-with-nl (sixth wsparams)) ""))
            ;;; test
            (setf (omversion newobj) (car wsparams))))
          newobj)))

;;; WS LOAD: THE ELEMENT IS ALREADY IN THE WS DIRECTORY
(defun ws-load-element (path &optional (i 0))
  (let* ((*package* (find-package :om))
         newicon newobj)
    (handler-bind (
                   (error #'(lambda (c) 
                                  (om-message-dialog (format nil "Error while loading ~A in WorkSpace: ~D"
                                                             (namestring path) 
                                                             (om-report-condition c)
                                                             )
                                                     :size (om-make-point 300 300))
                                  (om-abort)
                                  )))
      
      (if (and (directoryp path) (not (systemdirectoryp path)))
          (let ((j -1) (elements (om-directory path :files t :directories t)))
            (setf newobj (omNG-make-new-folder (car (last (pathname-directory path)))))
            (setf (mypathname newobj) (make-pathname 
                                       :device (pathname-device path)
                                       :directory  (append (butlast (pathname-directory path)) (list (name newobj)))))
            (setf (elements newobj) (remove nil (mapcar #'(lambda (elt) (ws-load-element elt (incf j))) elements))))
        (when (and (stringp (pathname-name path))
                   (not (string-equal "" (pathname-name path))))
          (let ((type (pathname-type path)))
              (when (string-equal type "tmp")
                (let* ((newpath1 (namestring (om-make-pathname :directory (pathname-directory path) :name (pathname-name path))))
                       (newpath2 (om-make-pathname :directory (pathname-directory path) :name (string+ (pathname-name newpath1) " (recovered)") 
                                                   :type (pathname-type newpath1))))
                  (rename-file path newpath2)
                  (setq path newpath2))))
          (let ((obj (object-from-file path)))
            (when obj
              (setf (mypathname obj) path)
              (setf newobj obj)
              ))))
        (when newobj
          (let* ((wsparams (get-init-wsparams path))
                 (wspar (subseq wsparams 2 5))
                 (doc (str-with-nl (nth 5 wsparams))))
            (setf newicon (nth 6 wsparams))
            (setf (wsparams newobj) (loop for item in wspar collect (eval item)))
            (setf (doc newobj) doc)
            (setf (omversion newobj) (car wsparams))
            (setf (create-info newobj) (list (nth 8 wsparams) (nth 9 wsparams)))
            (when (directoryp path) (setf (presentation newobj) (or (nth 7 wsparams) 0)))
            (when newicon (setf (icon newobj) (eval newicon)))
            (pushr (format nil "Element ~s loaded" path) *import-log*)
            newobj)))))


;;; AVOID MODIFYING WSPARAMS FLAG AT STARTUP:
(defmethod (setf changed-wsparams?) (changed (self ompersistantobject))
  (unless (and (boundp '*om-startup*) *om-startup*)
    (setf (slot-value self 'changed-wsparams?) changed)))

;;; WS AUTO UPGRADE

(defun load-patch-no-errors (self)
  (catch 'load-error
    (handler-bind ((error #'(lambda (c) 
                              (pushr (print (string+ "ERROR " (om-report-condition c))) *import-log*)
                              (throw 'load-error 'err)
                              )))
      (unless (loaded? self)
        (setf *loaading-stack* nil)
        (setf *loading-old-patches* nil)
        (setf *om-current-persistent* nil)
        (string+ "Loading..." (namestring (mypathname self)))
        (push self *loaading-stack*)
        (om-with-cursor *om-wait-cursor* 
          (eval-non-text-file (mypathname self))
          (when *om-current-persistent*
            (when *om-current-persistent*
              (setf (connec self) (connec *om-current-persistent*))
              (setf (boxes self) nil)
              (mapc #'(lambda (box) (omNG-add-element self box)) (reverse (boxes *om-current-persistent*)))
              (setf (pictu-list self) (pictu-list *om-current-persistent*))
              (setf (lisp-exp self) (lisp-exp *om-current-persistent*))
              (when (< (omversion *om-current-persistent*) *om-version*)
                (corrige-version self (omversion *om-current-persistent*))))
            (update-patches-pile)
            (setf *om-current-persistent* nil)
            (setf (saved? self) nil)
            ))))
    ))

(defun upgrade-ws-elt (elt &optional version)
  (when elt
    (if (folder-p elt)
      (loop for item in (elements elt) do (upgrade-ws-elt item))
      (if (or (not (numberp version)) (< (omversion elt) version))
          (let ((rep (load-patch-no-errors elt)))
            (if (equal rep 'err) 
                (pushr (print (string+ "--> File " (namestring (mypathname elt)) " could not be upgraded.")) *import-log*)
              (pushr (print (string+ "File " (namestring (mypathname elt)) " upgraded.")) *import-log*))))
      )))

(defvar *upgrading-ws* nil)

(defun upgrade-ws (&optional version) 
  (setf *show-version-warning* nil)
  (setf *upgrading-ws* t)
  (setf *import-log* nil)
  (setf *om-search-all* 1 
        *user-search-all* nil
        *file-search-all* 0)
  (loop for item in (elements *current-workspace*) do (upgrade-ws-elt item version))
  (setf *show-version-warning* t)
  (setf *upgrading-ws* nil)
  (print (string+ "Done: Workspace " (name *current-workspace*) " upgraded OM " *version-string*))
  (when (and *import-log* (om-y-or-n-dialog (format nil "Workspace upgrade completed.~%~%See upgrade report ?")))
    (om-show-output-lines *import-log*)))

; (upgrade-ws)

(defun upgrade-dialog ()
  (when (om-y-or-n-dialog "The UPGRADE WORKSPACE command will try to open and upgrade all the patches in this workspace. The workspace must still be saved afterwhile in order to confirm the patches upgrade. Do you want to proceed ?")
    (upgrade-ws)))

;;;===============================
;;; drag and drop finder
;;;===============================


(defmethod get-obj-from-file ((type t) filename)
  (print (format nil "Unknown type: ~A" type))
  nil)



(defmethod import-dragged-object ((self patchpanel) filename pos)
  (let ((newbox nil))
    (if (and (om-persistant-p filename) (equal (file-type filename) :inst))
        (let ()
          (eval-non-text-file filename)
          (when *instance-to-load*
            (setf newbox (omNG-make-new-boxcall (clone *instance-to-load*) pos (name *instance-to-load*)))
            ))
      (let ((obj (get-obj-from-file (interne (pathname-type filename)) filename)))
        (when obj 
          (setf newbox (omNG-make-new-boxcall (class-of obj) pos 
                                              (or (get-name obj)
                                                  (mk-unique-name self (string (class-name (class-of obj)))))))
          (setf (value newbox) obj))
        ))
    newbox))

(defmethod import-dragged-object ((self maquettepanel) filename pos)
  (let ((newbox nil))
    (if (and (om-persistant-p filename) (equal (file-type filename) :inst))
        (let ()
          (eval-non-text-file filename)
          (when *instance-to-load*
            (setf newbox (omNG-make-tempobj (clone *instance-to-load*) 
                                            (get-offset/posy-from-pixel self pos)
                                            (name *instance-to-load*)))
            ))
      (let ((obj (get-obj-from-file (interne (pathname-type filename)) filename)))
        (when obj 
          (setf newbox (omNG-make-tempobj (omNG-make-new-instance obj "instance") 
                                          (get-offset/posy-from-pixel self pos)
                                          (mk-unique-name self (string (class-name (class-of obj))))))
          )
        ))
    newbox))

(defmethod om-import-files-in-app ((self patchpanel) files)
  (when (= 1 (length files))
    (let ((newbox (import-dragged-object self (pathname (car files)) (om-mouse-position self))))
      (if newbox 
        (progn (omG-add-element self (make-frame-from-callobj newbox)) t)
        (om-beep-msg (string+ "File: " (namestring (pathname (car files))) " can not be imported in the patch."))))))

(defmethod om-drag-string-in-app ((self patchpanel) str)
  (when (or (fboundp (intern str))
            (find-class (intern str) nil))
    (add-box-in-patch-panel str self (om-mouse-position self))
    t))


;;; PLUS UTILISEE ...
(defmethod new-package-from-folder ((self ompackage) pathfile) 
  (setf *loading-classes* nil
        *loading-methods* nil
        *loading-initmet* nil)
  (mapc #'(lambda (newpath) 
            (fill-package-from-path newpath self))
        (om-directory  pathfile :files t :directories t))
  
  (let* ((*package* (find-package :om))
         (classes *loading-classes*)
         (methods *loading-methods*)
         (init-class-met *loading-initmet*)
         err smethods badclasses)
    (setf err (detect-class-redefinition classes))
    (if err
      (om-beep-msg (format nil "This classes are already defined :~%~{~D~}
you can not import this package." err))
      (progn  
        (setf badclasses (eval-initial-classes classes))
        (setf err (detect-genfun-redefinition methods))
        (setf err (remove-duplicates err :test 'equal))
        (when err
          (om-beep-msg (format nil "WARNING These generic functions are already defined: ~%~{~D ~}" 
                            (mapcar #'(lambda (elem) (string (eval elem))) err))))
        
        (setf smethods (mapcar #'(lambda (elem) (let ((new-semimethod (eval (second elem))))
                                                  (when (ommethod-p new-semimethod) 
                                                    (setf (mypathname new-semimethod) (first elem))
                                                    new-semimethod))) methods))
        (mapc #'(lambda (elem) (when elem (define-really-method elem))) smethods)
        (initial-methods-for-classes init-class-met badclasses)
        (mapc #'(lambda (newpath) 
                  (put-in-packpackages newpath self badclasses))
              (om-directory  pathfile :files t :directories t))
        t))))

;;; PLUS UTILISEE ...
(defun deep-copy-package (path-source path-target)
   (if (directory-pathname-p path-source)
     (let ((newpath (OMfile2folder (make-pathname :directory  (pathname-directory path-target) :name  (name-of-directory path-source))))
           (directories (om-directory  path-source :files t :directories t)))
       (om-create-directory newpath :if-exists nil)
       (loop for item in directories do
             (deep-copy-package item newpath)))
     (let ((pathtype (file-type path-source))
           (name (real-pathname-name path-source)))
       (if (or (equal pathtype :CLAS) (equal pathtype :METH))
           (copy-file-sp path-source (make-pathname :directory (pathname-directory path-target) :name name))))))


;==================To FINDER=============
(defmethod external-object-p ((self OMFolder)) t)
(defmethod external-object-p ((self OMglobalsFolder)) nil)

(defmethod external-object-p ((self OMInstance)) t)
(defmethod external-object-p ((self OMPatch)) t)
(defmethod external-object-p ((self OMPackage)) (and (father self) (not (kernel-p self))))

(defmethod external-object-p ((self OMClass)) (mypathname self))
(defmethod external-object-p ((self OMMethod)) (mypathname self))



(defmethod external-object-p ((self OMBoxEditCall)) t)
(defmethod om-filetype ((self OMBoxEditCall)) :|INST|)


(defmethod external-object-p ((self OMBoxInstance)) t)
(defmethod om-filetype ((self OMBoxInstance)) :|INST|)

(defmethod external-object-p ((self TemporalBox)) t)
(defmethod om-filetype ((self TemporalBox)) :|INST|)

(defmethod om-filetype ((self OMPatch)) :|PATC|)
(defmethod om-filetype ((self OMMaquette)) :|MAQT|)
(defmethod om-filetype ((self OMInstance)) :|INST|)
(defmethod om-filetype ((self OMFolder)) :|fold|)
(defmethod om-filetype ((self OMPackage)) :|fold|)
(defmethod om-filetype ((self OMClass)) :|CLAS|)
(defmethod om-filetype ((self OMMethod)) :|METH|)


;;; EXPORT

(defmethod export-file ((self OMPatch))
   (catch-cancel
     (let* ((name (om-choose-new-file-dialog :name (name self)    
                                             :prompt (string+ "Save " (get-object-insp-name self) " as...")
                                             :directory (or *last-exported* (om-user-home))
                                             :types (list (get-object-insp-name self) (string+ "*." (obj-file-extension self))))))
       (when name
         (setf *last-exported* (make-pathname :directory (pathname-directory name)))
         (unless (pathname-type name)
           (setf name (pathname (string+ (namestring name) "." (obj-file-extension self)))))
         (do-copy-to-finder self name)))))

(defmethod export-file ((self t)) t)

(defmethod export-file ((self OMFolder))
   (catch-cancel
     (let* ((name (om-choose-new-directory-dialog :directory (or *last-exported* (om-user-home)) :defname (name self))))
       (when name
         (setf *last-exported* (make-pathname :directory (butlast (pathname-directory name))))
         (folder2finder self name)))))


;;;=== from persistant to finder ===
;;; !!! marche pas stream-reader stream-writer sur Allegro
(defun my-copy-file (from-file to-file &aux char)
  (delete-file-protection to-file)
  (with-open-file (from-stream from-file)
    (with-open-file (to-stream to-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (multiple-value-bind (reader reader-arg)
                           (stream-reader from-stream)
        (multiple-value-bind (writer writer-arg)
                             (stream-writer to-stream)
          (loop (unless (setq char (funcall reader reader-arg))
                  (return))
                    (funcall writer writer-arg char)))))))

(defmethod folder2finder ((self OMFolder) path)
  (om-create-directory path)
  (loop for item in (get-elements self) do
        (cond
         ((folder-p item) 
          (folder2finder item (make-pathname :directory (append (pathname-directory path) (list (name item))))))
         (t (om-copy-file (mypathname item) (make-pathname :directory (pathname-directory path)
                                                           :name (name item)
                                                           :type (obj-file-extension item))))))
  (let ((finderinfo (om-make-pathname :directory (mypathname self)
                                      :type "finderinfo")))
    (when (probe-file finderinfo)
      (om-copy-file finderinfo (om-make-pathname :directory path :type "finderinfo"))))
  )
  
(defmethod do-copy-to-finder ((self OMBasicObject) path)
  (if (om-shift-key-p)
    (my-copy-file (mypathname self) path)
    (progn
      (om-copy-file (mypathname self) path)
      (unless (saved? self)
        (let ((oldpat (mypathname self)))
          (setf (mypathname self) path)
          (omng-save self)
          (setf (saved? self) nil)
          (setf (mypathname self) oldpat))))) t)

(defmethod do-copy-to-finder ((self OMPackage) path)
   (let ((depend? (depending-p self)))
     (if depend?
       (om-beep-msg (string+ depend? " Try dragging the package " (name (father self))))
       (let ((unicpath (OMfile2folder path)))
         (loop while (probe-file unicpath)
               for i = 0 then (+ i 1) do
               (setf unicpath (OMfile2folder path i)))
       (send2finder self unicpath)))))

(defmethod send2finder ((self OMPackage) thepath)
   (unless (probe-file thepath) (om-create-file thepath))
   (loop for class in (classes self) do
         (omNG-save class thepath))
   (loop for met in (direct-pack-methods self) do
         (omNG-save met thepath))
   (loop for pack in (subpackages self) do
         (send2finder pack (make-pathname :directory (append (pathname-directory thepath) (list (name pack)))))))
