;;===========================================================================
;LW Lisp Tools 
;Lisp programming tools for LispWorks delivered applications
;;===========================================================================

;;===========================================================================
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Author: Jean Bresson
;Contributions: Sheldon Ball, Nicholas Ellis
;;===========================================================================

;;===========================================================================
;DocFile
;This file loads the LW Lisp tools
;;===========================================================================

(defpackage "OM-LISP"
  (:use "COMMON-LISP" "CL-USER" "CAPI" "LISPWORKS"))

(in-package :om-lisp)
 
(defvar *lw-lisp-tools-directory* nil)
(setf *lw-lisp-tools-directory* (pathname-directory (truename *load-pathname*)))

(defun compile-if-needed-and-load (file &optional (verbose t))
   (let* ((lisp-file (truename (if (pathname-type file) file (concatenate 'string (namestring file) ".lisp"))))
          (fasl-file (probe-file (make-pathname :directory (pathname-directory lisp-file)
                                                :device (pathname-device lisp-file)
                                                :name (pathname-name lisp-file) :type (pathname-type (cl-user::compile-file-pathname "")))))
          (fasl-outofdate (and fasl-file
                               (or (not (file-write-date lisp-file))
                                   (not (file-write-date fasl-file))
                                   (> (file-write-date lisp-file) (file-write-date fasl-file))))))
     (when (and (fboundp 'compile-file)
                (or (not fasl-file) fasl-outofdate))
       (compile-file file :verbose verbose)
       (setf fasl-outofdate nil))
     (if fasl-outofdate
         (progn (print (format nil "WARNING: File ~A is older than the LISP source file. File ~A will be loaded instead."
                               fasl-file lisp-file))
           (load lisp-file :verbose verbose))
       (catch 'faslerror
         (handler-bind ((conditions::fasl-error #'(lambda (c) 
                                                    (when (and (fboundp 'compile-file) fasl-file)
                                                      (print (format nil "File ~s will be recompiled..." fasl-file))
                                                      (compile-file file :verbose verbose)
                                                      (load file :verbose verbose)
                                                      (throw 'faslerror t)
                                                      ))))
           (load file :verbose verbose)
           ))
       )))

(mapc #'(lambda (filename) (compile-if-needed-and-load 
                            (make-pathname :directory *lw-lisp-tools-directory* 
                                           :name filename))) 
      '("textbuffer" "listener" "texteditor" "editor-find-dialogs" "find-definition" "eval-process" "output"))






