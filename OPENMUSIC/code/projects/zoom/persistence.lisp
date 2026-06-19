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

;Author: Paulo Henrique Raposo

(in-package :om)

;;; ============================================================
;;; OMPersistantObject: zoom storage in wsparams[3] + header field 11
;;; ============================================================

(defmethod oa::get-win-zoom ((self OMPersistantObject))
  (let ((ws (wsparams self)))
    (or (and (>= (length ws) 4) (fourth ws)) 1.0)))

(defmethod oa::set-win-zoom ((self OMPersistantObject) zoom)
  (ensure-ws-params self)
  (setf (changed-wsparams? self) t)
  (let ((ws (wsparams self)))
    (if (>= (length ws) 4)
        (setf (nth 3 ws) zoom)
        (nconc ws (list zoom)))))

(defmethod header-comment-from-obj ((self OMPersistantObject))
  (let* ((icon (icon self))
         (ftype (obj-file-type self))
         (version (omversion self))
         (ws (ensure-ws-params self))
         (wspar (om-save-point-list (subseq ws 0 (min 3 (length ws)))))
         (zoom (oa::get-win-zoom self))
         (params (list version ftype (first wspar) (second wspar) (third wspar)
                       (str-without-nl (doc self)) (save-icon icon) 0
                       (car (create-info self)) (cadr (create-info self))
                       zoom)))
    (string+ ";" (format nil " ~S" params))))

;;; ============================================================
;;; Body save: emit (get-win-zoom self) trailing arg of loader call
;;; ============================================================

(defmethod om-save ((self OMPatchAbs) &optional (values? nil))
  (if (lisp-exp-p self)
      `(om-load-lisp-abspatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp-p self))
                              ,(oa::get-win-zoom self))
      (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
            (connectiones (mk-connection-list (boxes self)))
            (doc (str-without-nl (doc self)))
            pictlist)
        (setf pictlist (omng-save (pictu-list self)))
        (when (editorframe self)
          (set-win-size self (om-interior-size (window (editorframe self))))
          (set-win-position self (om-view-position (window (editorframe self)))))
        `(om-load-patch-abs1 ,(name self) ',boxes ',connectiones ,*om-version* ,pictlist ,doc
                             ,(om-save-point (w-pos self)) ,(om-save-point (w-size self))
                             ,(oa::get-win-zoom self)))))

(defmethod om-save ((self OMLispPatch) &optional (values? nil))
  (declare (ignore values?))
  `(setf *om-current-persistent*
         (om-load-lisp-patch ,(name self) ,*om-version*
                             ,(str-without-nl (lisp-exp self))
                             ,(oa::get-win-zoom self))))

(defmethod om-save ((self OMLispPatchAbs) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-lisp-abspatch ,(name self) ,*om-version*
                          ,(str-without-nl (lisp-exp self))
                          ,(oa::get-win-zoom self)))

;;; ============================================================
;;; Body load: accept trailing zoom and store via set-win-zoom
;;; ============================================================

(defun om-load-patch-abs1 (name boxes connections
                                &optional (version nil) (pictlist nil) (doc "") wpos wsize wzoom
                                &rest args)
  (declare (ignore args))
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
    (when (numberp wzoom) (oa::set-win-zoom newpatch wzoom))
    (compile-patch newpatch)
    (when version
      (setf (omversion newpatch) version))
    newpatch))

(defun om-load-lisp-patch (name version expression &optional (zoom 1.0))
  (let ((newpatch (omNG-make-new-lisp-patch name)))
    (setf (omversion newpatch) version)
    (setf (lisp-exp newpatch) (get-lisp-str expression))
    (when (numberp zoom) (oa::set-win-zoom newpatch zoom))
    newpatch))

(defun om-load-lisp-abspatch (name version expression &optional (zoom 1.0))
  (let ((newpatch (make-instance 'OMLispPatchAbs :name name :icon 123)))
    (setf (omversion newpatch) version)
    (setf (lisp-exp newpatch) (get-lisp-str expression))
    (when (numberp zoom) (oa::set-win-zoom newpatch zoom))
    (compile-lisp-patch-fun newpatch)
    newpatch))

;;; ============================================================
;;; Workspace loaders: read header[10] (zoom field) and restore
;;; ============================================================

(defun ws-load-element (path &optional (i 0))
  (declare (ignore i))
  (let* ((*package* (find-package :om))
         newicon newobj)
    (handler-bind ((error
                    #'(lambda (c)
                        (om-message-dialog (format nil "Error while loading ~A in WorkSpace: ~D"
                                                   (namestring path)
                                                   (om-report-condition c))
                                           :size (om-make-point 300 300))
                        (om-abort))))
      (if (and (directoryp path) (not (systemdirectoryp path)))
          (let ((j -1) (elements (om-directory path :files t :directories t)))
            (setf newobj (omNG-make-new-folder (car (last (pathname-directory path)))))
            (setf (mypathname newobj) (make-pathname
                                       :device (pathname-device path)
                                       :directory (append (butlast (pathname-directory path)) (list (name newobj)))))
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
                (setf newobj obj)))))
      (when newobj
        (let* ((wsparams (get-init-wsparams path))
               (wspar (subseq wsparams 2 5))
               (doc (str-with-nl (nth 5 wsparams))))
          (setf newicon (nth 6 wsparams))
          (setf (wsparams newobj) (loop for item in wspar collect (eval item)))
          (let ((zoom (and (>= (length wsparams) 11) (nth 10 wsparams))))
            (when (and zoom (numberp zoom) (/= zoom 1.0))
              (oa::set-win-zoom newobj zoom)))
          (setf (doc newobj) doc)
          (setf (omversion newobj) (car wsparams))
          (setf (create-info newobj) (list (nth 8 wsparams) (nth 9 wsparams)))
          (when (directoryp path) (setf (presentation newobj) (or (nth 7 wsparams) 0)))
          (when newicon (setf (icon newobj) (eval newicon)))
          (pushr (format nil "Element ~s loaded" path) *import-log*)
          newobj)))))

(defun help-load-element (path &optional (i 0))
  (declare (ignore i))
  (let* ((*package* (find-package :om))
         newicon newobj)
    (handler-bind ((error
                    #'(lambda (c)
                        (om-message-dialog (format nil "Error while loading help file ~A: ~D"
                                                   (namestring path)
                                                   (om-report-condition c))
                                           :size (om-make-point 300 300))
                        (om-abort))))
      (if (and (directoryp path) (not (systemdirectoryp path)))
          (let ((j -1) (elements (om-directory path :files t :directories t)))
            (setf newobj (omNG-make-new-folder (car (last (pathname-directory path)))))
            (setf (mypathname newobj) (make-pathname
                                       :device (pathname-device path)
                                       :directory (append (butlast (pathname-directory path)) (list (name newobj)))))
            (setf (elements newobj) (remove nil (mapcar #'(lambda (elt) (help-load-element elt (incf j))) elements))))
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
                (setf newobj obj)))))
      (when newobj
        (let* ((wsparams (get-init-wsparams path))
               (wspar (subseq wsparams 2 5))
               (doc (str-with-nl (nth 5 wsparams))))
          (setf newicon (nth 6 wsparams))
          (setf (wsparams newobj) (loop for item in wspar collect (eval item)))
          (let ((zoom (and (>= (length wsparams) 11) (nth 10 wsparams))))
            (when (and zoom (numberp zoom) (/= zoom 1.0))
              (oa::set-win-zoom newobj zoom)))
          (setf (doc newobj) doc)
          (setf (omversion newobj) (car wsparams))
          (setf (create-info newobj) (list (nth 8 wsparams) (nth 9 wsparams)))
          (when (directoryp path) (setf (presentation newobj) (or (nth 7 wsparams) 0)))
          (when newicon (setf (icon newobj) (eval newicon)))
          (pushr (format nil "Element ~s loaded" path) *import-log*)
          newobj)))))

;;; ============================================================
;;; omNG-copy: carry zoom across patch copies
;;; ============================================================

(defmethod omNG-copy ((self OMPatch))
  (unless (loaded? self)
    (load-patch self))
  `(let ((copy ,(call-next-method)))
     (loop for item in (list ,.(reverse (mapcar #'omNG-copy (boxes self)))) do
           (omng-add-element copy item))
     (copy-connections ',(boxes self) (boxes copy))
     (set-icon-pos copy ,(om-copy-point (get-icon-pos self)))
     (set-win-size copy ,(om-copy-point (get-win-size self)))
     (oa::set-win-zoom copy ,(oa::get-win-zoom self))
     (setf (omversion copy) ,(omversion self))
     (setf (pictu-list copy) ',(mapcar 'copy-picture (pictu-list self)))
     (setf (lisp-exp-p copy) ,(lisp-exp-p self))
     copy))

;;; ============================================================
;;; omfolder.lisp override: limit wsparams slice to first 3 (zoom suffix)
;;; ============================================================

(defmethod header-comment-from-obj ((self OMFolder))
  (let* ((icon (icon self))
         (ftype (obj-file-type self))
         (version *om-version*)
         (ws (ensure-ws-params self))
         (wspar (om-save-point-list (subseq ws 0 (min 3 (length ws)))))
         (size (if (editorframe self)
                   (read-from-string (format-folder-size self))
                   (third wspar)))
         (params (list version ftype (first wspar) (second wspar)
                       size
                       (str-without-nl (doc self)) (save-icon icon) (presentation self)
                       (car (create-info self)) nil)))
    (string+ ";" (format nil " ~S" params))))

;;; ============================================================
;;; omlib.lisp override: AddPackage2Pack tolerates non-numeric versions
;;; ============================================================

(defmethod AddPackage2Pack ((new-Package OMLib) inPackage &key (protect t))
  (let ((subpackages (subpackages inPackage)))
    (if (find new-Package subpackages
              :test #'(lambda (lib1 lib2)
                        (and (string-equal (name lib1) (name lib2))
                             (cond ((and (version lib1) (version lib2))
                                    (if (and (numberp (version lib1)) (numberp (version lib2)))
                                        (= (version lib1) (version lib2))
                                        (equal (version lib1) (version lib2))))
                                   ((or (version lib1) (version lib2)) t)
                                   (t nil)))))
        (om-beep-msg (format nil "Library ~A~A already exists!!" (name new-Package)
                             (if (version new-Package) (format nil " (~D)" (version new-Package)) "")))
        (progn
          (when protect (omNG-protect-object new-package))
          (omNG-add-element inPackage new-package)
          new-package))))

;;; ============================================================
;;; OMPatchAbs + OMLispPatch override: per-instance w-zoom slot + accessors
;;; ============================================================

(defclass OMPatchAbs (OMPatch)
  ((w-size :accessor w-size :initform (om-make-point 400 500))
   (w-pos :accessor w-pos :initform (om-make-point 200 200))
   (w-zoom :accessor w-zoom :initform 1.0))
  (:documentation "OMPatchAbs with per-instance zoom.")
  (:metaclass omstandardclass))

(defclass OMLispPatch (OMPatch)
  ((list-exp :initform nil :initarg :lisp-exp :accessor lisp-exp)
   (w-zoom :accessor w-zoom :initform 1.0))
  (:icon 124)
  (:documentation "A patch written in Lisp")
  (:metaclass omstandardclass))

(defclass OMLispPatchAbs (OMLispPatch ompatchabs) ()
  (:documentation "A patch written in Lisp (abstraction).")
  (:metaclass omstandardclass))

(defmethod oa::get-win-zoom ((self OMLispPatch)) (w-zoom self))

(defmethod oa::set-win-zoom ((self OMLispPatch) zoom)
  (when (numberp zoom) (setf (w-zoom self) zoom)))

(defmethod load-abstraction-attributes ((self omlisppatch) currentpersistent)
  (call-next-method)
  (setf (lisp-exp self) (lisp-exp currentpersistent))
  (when (and (slot-exists-p currentpersistent 'w-zoom)
             (slot-boundp currentpersistent 'w-zoom))
    (setf (w-zoom self) (w-zoom currentpersistent)))
  (when (lisp-exp self) (compile-lisp-patch-fun self)))

(defun capture-lisp-window-zoom (patch)
  (let ((editor (editorframe patch))
        (cls (find-class 'patch-lambda-exp-window nil)))
    (when (and editor cls (typep editor cls))
      (let ((ep (om-lisp::ep editor)))
        (when (and ep (capi:capi-object-property ep :om-zoom-default-font))
          (setf (w-zoom patch) (oa::pane-current-zoom-ratio ep)))))))

(defmethod omNG-copy ((self OMLispPatch))
  `(let ((copy ,(call-next-method)))
     (setf (lisp-exp copy) (lisp-exp ,self))
     (setf (w-zoom copy) ,(w-zoom self))
     (compile-lisp-patch-fun copy)
     copy))

;;; ============================================================
;;; lisppatcheditor.lisp overrides: capture+restore zoom on lambda-exp editor
;;; ============================================================

(defun apply-saved-lisp-zoom (editor patch)
  (when (and (slot-exists-p patch 'w-zoom)
             (slot-boundp patch 'w-zoom))
    (let ((ep (om-lisp::ep editor))
          (ratio (w-zoom patch)))
      (when (and ep
                 (capi:capi-object-property ep :om-zoom-default-font)
                 (not (= ratio 1.0)))
        (oa::set-pane-zoom-ratio ep ratio)))))

;;; ============================================================
;;; saveheader.lisp override: default header-comment-from-obj emits zoom
;;; ============================================================

(defmethod header-comment-from-obj ((self t))
  (let* ((icon (icon self))
         (ftype (obj-file-type self))
         (version (omversion self))
         (ws (ensure-ws-params self))
         (wspar (om-save-point-list (subseq ws 0 (min 3 (length ws)))))
         (zoom (oa::get-win-zoom self))
         (params (list version ftype (first wspar) (second wspar) (third wspar)
                       (str-without-nl (doc self)) (save-icon icon) 0
                       (car (create-info self)) (cadr (create-info self))
                       zoom)))
    (string+ ";" (format nil " ~S" params))))

(defmethod om-window-close-event ((self patch-lambda-exp-window))
  (handler-bind ((error #'(lambda (c)
                            (om-message-dialog
                             (format nil "Patch Definition Error: ~S. ~%No modification will be made to the patch."
                                     (om-report-condition c)))
                            (om-kill-window-buffer self)
                            (when (patchref self)
                              (setf (editorframe (patchref self)) nil))
                            (setf *patch-abort-definition* nil)
                            (om-abort))))
    (loop for item in (attached-objs (patchref self)) do
          (update-from-reference item))
    (setf (compiled? (patchref self)) t)
    (when (patchref self) (capture-lisp-window-zoom (patchref self)))
    (setf (editorframe (patchref self)) nil)
    (setf *patch-abort-definition* nil)
    (om-kill-window-buffer self)
    (call-next-method)))

(defmethod edit-existing-lambda-expression ((self ompatch))
  (let ((editor (om-make-window 'patch-lambda-exp-window
                                :patchref self
                                :size (om-make-point 300 200)
                                :window-title (string+ "Lisp Patch - " (name self))
                                :window-show nil)))
    (om-set-text editor (lisp-exp self))
    (om-add-menu-to-win editor)
    (apply-saved-lisp-zoom editor self)
    editor))

(defmethod oa::editor-window-font ((self patch-lambda-exp-window))
  om-lisp::*om-lisp-patch-font*)
