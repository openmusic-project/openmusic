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
;=========================================================================

;=========================================================================
;;; OpenMusic runtime on Windows
;;; load this file to produce a copy of the SBCL runtime carrying the OM icon
;=========================================================================

(in-package "CL-USER")

(ql:quickload :cffi)

(defparameter *om-icon* (merge-pathnames (make-pathname :directory '(:relative :up :up "win64")
                                                        :name "om" :type "ico")
                                         *load-truename*))
(defparameter *om-runtime* (merge-pathnames "om-runtime.exe" *load-truename*))

(cffi:define-foreign-library kernel32 (:windows "kernel32.dll"))
(cffi:use-foreign-library kernel32)

(cffi:defcfun ("BeginUpdateResourceW" begin-update-resource) :pointer
  (file (:string :encoding :utf-16le)) (delete-existing :int))
(cffi:defcfun ("UpdateResourceW" update-resource) :int
  (update :pointer) (type :pointer) (name :pointer) (language :uint16) (data :pointer) (size :uint32))
(cffi:defcfun ("EndUpdateResourceW" end-update-resource) :int
  (update :pointer) (discard :int))

(defconstant +rt-icon+ 3)
(defconstant +rt-group-icon+ 14)

(defun u16 (octets offset) (logior (aref octets offset) (ash (aref octets (1+ offset)) 8)))
(defun u32 (octets offset) (logior (u16 octets offset) (ash (u16 octets (+ offset 2)) 16)))

(defun put-u16 (octets offset value)
  (setf (aref octets offset) (ldb (byte 8 0) value)
        (aref octets (1+ offset)) (ldb (byte 8 8) value)))

(defun put-u32 (octets offset value)
  (put-u16 octets offset (ldb (byte 16 0) value))
  (put-u16 octets (+ offset 2) (ldb (byte 16 16) value)))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun update-resource-octets (update type name octets)
  (cffi:with-pointer-to-vector-data (data octets)
    (when (zerop (update-resource update type name 0 data (length octets)))
      (error "UpdateResourceW failed for resource type ~D" (cffi:pointer-address type)))))

(defun add-icon-resource (exe ico group-name)
  (let* ((icon (read-octets ico))
         (count (u16 icon 4))
         (group (make-array (+ 6 (* 14 count)) :element-type '(unsigned-byte 8) :initial-element 0))
         (update (begin-update-resource (uiop:native-namestring exe) 0)))
    (when (cffi:null-pointer-p update)
      (error "BeginUpdateResourceW failed for ~A" exe))
    (put-u16 group 2 1)
    (put-u16 group 4 count)
    (dotimes (i count)
      (let* ((entry (+ 6 (* 16 i)))
             (size (u32 icon (+ entry 8)))
             (offset (u32 icon (+ entry 12)))
             (out (+ 6 (* 14 i))))
        (update-resource-octets update (cffi:make-pointer +rt-icon+) (cffi:make-pointer (1+ i))
                                (subseq icon offset (+ offset size)))
        (replace group icon :start1 out :start2 entry :end2 (+ entry 12))
        (put-u16 group (+ out 12) (1+ i))))
    (cffi:with-foreign-string (name group-name :encoding :utf-16le)
      (update-resource-octets update (cffi:make-pointer +rt-group-icon+) name group))
    (when (zerop (end-update-resource update 0))
      (error "EndUpdateResourceW failed for ~A" exe))
    exe))

(uiop:run-program (list "objcopy" "--strip-debug"
                        (uiop:native-namestring sb-ext:*runtime-pathname*)
                        (uiop:native-namestring *om-runtime*))
                  :output t :error-output t)
(add-icon-resource *om-runtime* *om-icon* "GLFW_ICON")
(format t "~&OM runtime with icon: ~A~%" *om-runtime*)
(uiop:quit 0)
