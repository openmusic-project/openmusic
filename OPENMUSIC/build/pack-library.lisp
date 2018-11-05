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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;=========================================================================
;;; OpenMusic Library packaging            
;;; this file is used by OM release scripts to load OM and build releases of 
;;; a library locates in ../../lib-name.lisp.tmp
;;; nb: no need to load OM here, these are just folder copies / cleanup
;=========================================================================


(in-package "CL-USER")

(load-all-patches)

(load (current-pathname "build-om"))

(oa::om-root-init)
(oa::init-def-rsrc)
(oa::init-sub-rsrc)

(clos::set-clos-initarg-checking nil)

(print "== OM IS LOADED ! ==")

;;;===========================
;;; LIB DEFINITION
;;;===========================

(defparameter *lib-name-path* (print (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2) :name "lib-name" :type "lisp.tmp")))

;;; will be set in the file *lib-name-path*
(defparameter *lib-name* nil) 

;;; can also be set by the file *lib-name-path*
(defvar *release-dir* (make-pathname :directory (append (butlast (pathname-directory (current-pathname)) 3) '("OM-LIBRARIES-RELEASE"))))

(if (probe-file *lib-name-path*)
  (load *lib-name-path*)
  (progn (print "Quitting (no lib to deliver...)") 
    (quit)))

(om::om-create-directory *release-dir*)

(defvar *current-lib-version* nil)

(om::init-omlib-directory)

(defun register-lib (pathname)
  (let* ((omlib (om::omng-make-new-lib pathname)))
    (when omlib
      (om::AddPackage2Pack omlib om::*library-package*))))

(defun prepare-and-pack-lib (libpath target)
  (let* ((libfullname (car (last (pathname-directory libpath))))
         (libname (om::string-until-space libfullname))
         (omlib (om::find-library libname))
         (om::*current-lib* omlib))
    (when omlib
      (load (om::lib-pathname omlib))
      (om::gen-lib-reference omlib)
      (let* ((version (or (om::lib-release omlib) 
                         (when (search " " libfullname) (read-from-string (om::string-from-space libfullname)))))
            (packedlibpath (make-pathname :directory (append (pathname-directory target) 
                                                             (list 
                                                              (if version
                                                                  (concatenate 'string libname " " (format nil "~D" version))
                                                                libname))))))
        (when (probe-file packedlibpath)
          (om::om-delete-directory packedlibpath))
        (print (concatenate 'string "== DELIVERING " libname " => " (namestring packedlibpath)))
        (om::om-copy-directory libpath packedlibpath)
        (clean-sources packedlibpath '("64xfasl" "nfasl" "ofasl"))
        ))))

(defmethod om::require-library (lib &optional (abort-if-not-found nil))
  (let* ((old-lib om::*current-lib*)
         (thelib (om::find-library lib)))
    (unless thelib 
      (let ((path (om::om-make-pathname :directory (append (butlast (pathname-directory (om::lib-pathname old-lib)))
                                                           (list lib)))))
        (register-lib path)
        (setf thelib (om::find-library lib))))
    (if thelib
        (progn 
          (setf om::*current-lib* thelib)
          (unless (om::loaded? thelib)
            (load (om::lib-pathname thelib)))
          (setf om::*current-lib* old-lib)
          t)
      (when abort-if-not-found
        (print (om::string+ "Library " lib " not found!"))
        (oa::om-abort)))))


(when *lib-name*
  (print (concatenate 'string "== READY TO PACK LIB " (namestring *lib-name*)))
  (register-lib *lib-name*)
  (prepare-and-pack-lib *lib-name* *release-dir*)
  (terpri))


(quit)

                  
