;=========================================================================
; OM API
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; API PROTOTYPES FOR EXTERNAL LIBRARY CALLS
;DocFile
;;===========================================================================

(in-package :oa)

; (setq fli::*default-dlflags* 6)
; (setq fli::*can-use-dlopen* nil)

;;;====================================
;;; STANDARD ACCES TO EXTERNAL PROGRAMS:
;;;====================================
(export '(om-external-app om-default-application-path) :om-api)

(defun om-external-app (folders appname)
  (make-pathname :directory (append (list :ABSOLUTE "Applications") folders)
                 :name appname))

(defun om-default-application-path (folders appname)
  #-linux  (make-pathname :directory (append (list :ABSOLUTE "Applications") folders 
					     (when appname (list (concatenate 'string appname ".app")))))
  #+linux (user-homedir-pathname)
  )




;;;====================================
;;; POINTERS
;;;====================================
(export '(om-make-pointer
          om-free-pointer
          om-read-ptr
          om-write-ptr
          om-cleanup-mixin
          om-cleanup)
        :om-api)

(defun om-make-pointer (size &key (type :byte) (clear nil))
  (if clear
      (fli:allocate-foreign-object :type type :nelems size :fill 0)
    (fli:allocate-foreign-object :type type :nelems size)))

(defun om-free-pointer (ptr)
  (fli::free-foreign-object ptr))

;;; !!! does not work very: crashes a lot on Mac, e.g. with SDIF files
(defun om-write-ptr (ptr pos type value)
  (setf (fli:dereference  ptr :type type :index pos) value))

(defun om-read-ptr (ptr pos type)
  (fli:dereference ptr :type type :index pos))

 
;;;========================
;;; CLEANUP/DEALLOC UTILS
;;;========================

(hcl::add-special-free-action 'om-cleanup)

;;; to be subclassed by special-cleanup objects
(defclass om-cleanup-mixin () ())

(defmethod initialize-instance :before ((self om-cleanup-mixin) &rest args) 
  (hcl::flag-special-free-action self))

;;; to be redefined for specific om-cleanup-mixin subclasses
(defmethod om-cleanup ((self t)) nil)




;;;========================
;;; EXTERNAL LIBRARIES
;;;========================
(export '(
          om-lib-directory
          om-lib-pathname
          ) :om-api)


(defvar *om-lib-directory* nil)

(defun om-lib-pathname (lib)
  (set-lib-dir)
  (if (and *om-lib-directory* (probe-file *om-lib-directory*))
    #+(or win32 linux)
    (namestring (make-pathname :directory (pathname-directory *om-lib-directory*)
                                        :host (pathname-host *om-lib-directory*) :device (pathname-device *om-lib-directory*)
                                        :name (pathname-name lib)
                                        :type (pathname-type lib)))
    #+cocoa
    (let ((frameworkpos (position "framework" (cdr (pathname-directory lib))
                                  :test 'string-equal :key #'(lambda (item) (cadr (multiple-value-list (string-until-char item ".")))))))

          (make-pathname :directory (append (pathname-directory *om-lib-directory*) (if frameworkpos (subseq (pathname-directory lib) (1+ frameworkpos))))
                         :host (pathname-host *om-lib-directory*) :device (pathname-device *om-lib-directory*)
                         :name (pathname-name lib)
                         :type (pathname-type lib)))
    lib))


(defun set-lib-dir ()
  (setf *om-lib-directory* (om-lib-directory)))

;;; a redefinir pour changer de repertoire par defaut...
;(defun om-lib-directory () nil)

;;; dans le dossier de l'appli
(defun om-lib-directory ()
  #+win32(make-pathname :directory (pathname-directory (LISP-IMAGE-NAME))
                 :host (pathname-host (LISP-IMAGE-NAME)) :device (pathname-device (LISP-IMAGE-NAME)))

  #+macosx(make-pathname :directory (append (pathname-directory *om-root*) 
                                            (if (member :om-deliver *features*)
                                                (list (concatenate 'string "OM " *version-str* ".app") "Contents" "Frameworks")
                                              (list "resources" "lib" "mac"))
                                            )
                 :host (pathname-host *om-root*) :device (pathname-device *om-root*))
  #+linux(make-pathname :directory (append (pathname-directory *om-root*) '("resources" "lib" "linux"))
                 :host (pathname-host *om-root*) :device (pathname-device *om-root*))
  )

;(om-api-add-init-func 'set-lib-dir)
