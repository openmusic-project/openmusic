(in-package :oa)

(export '(om-external-app
          om-default-application-path
          om-make-pointer
          om-free-pointer
          om-read-ptr
          om-write-ptr
          om-cleanup-mixin
          om-cleanup
          om-lib-directory
          om-lib-pathname)
        :om-api)

;;; External application path

(defun om-external-app (folders appname)
  (make-pathname :directory (append (list :absolute "Applications") folders)
                 :name appname))

(defun om-default-application-path (folders appname)
  #-linux (make-pathname :directory (append (list :absolute "Applications") folders
                                            (when appname
                                              (list (concatenate 'string appname ".app")))))
  #+linux (user-homedir-pathname))

;;; Foreign pointers
;;; OM projects call (fli:dereference ptr :index i) with NO :type on buffers from
;;; om-make-pointer, so the element type is registered with the FLI package
;;; (alloy-desktop, lispworks/fli.lisp: register-pointer-type).

;;; The typed zero for a CFFI element type -- LW's :fill 0 zeroes every byte, so
;;; a cleared buffer reads back as 0.0 / 0.0d0 / 0 / null per element type. Passing
;;; the integer 0 as :initial-element for a float type signals a TYPE-ERROR in cffi.
(defun %zero-of (ctype)
  (case ctype
    ((:float) 0.0)
    ((:double) 0.0d0)
    ((:pointer) (cffi:null-pointer))
    (otherwise 0)))

(defun om-make-pointer (size &key (type :byte) (clear nil))
  (let ((ctype (fli:cffi-type type)))
    (fli:register-pointer-type
     (if clear
         (cffi:foreign-alloc ctype :count size :initial-element (%zero-of ctype))
         (cffi:foreign-alloc ctype :count size))
     ctype)))

(defun om-free-pointer (ptr)
  (fli:unregister-pointer-type ptr)
  (cffi:foreign-free ptr))

;(defun om-read-ptr (ptr pos type)
;  (cffi:mem-aref ptr (fli:cffi-type type) pos))

;(defun om-write-ptr (ptr pos type value)
;  (setf (cffi:mem-aref ptr (fli:cffi-type type) pos) value))

;;; Cleanup mixin. om-cleanup runs only when called explicitly: SBCL sb-ext:finalize
;;; cannot pass the dying object like LispWorks hcl:add-special-free-action, so the
;;; vanilla automatic free-on-GC is unportable.

(defclass om-cleanup-mixin () ())

(defmethod om-cleanup ((self t)) nil)

;;; External library directory resolution

(defvar *om-lib-directory* nil)

(defun om-lib-directory ()
  #+win32 (make-pathname :directory (pathname-directory sb-ext:*runtime-pathname*)
                         :host      (pathname-host sb-ext:*runtime-pathname*)
                         :device    (pathname-device sb-ext:*runtime-pathname*))
  #+darwin (make-pathname :directory (append (pathname-directory *om-root*)
                                             (if (member :om-deliver *features*)
                                                 (list (concatenate 'string "OM " *version-str* ".app")
                                                       "Contents" "Frameworks")
                                                 (list "resources" "lib" *macdylibfolder*)))
                          :host   (pathname-host *om-root*)
                          :device (pathname-device *om-root*))
  #+linux (make-pathname :directory (append (pathname-directory *om-root*)
                                            '("resources" "lib" "linux"))
                         :host   (pathname-host *om-root*)
                         :device (pathname-device *om-root*)))

(defun %set-lib-dir ()
  (setf *om-lib-directory* (om-lib-directory)))

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun om-lib-pathname (lib)
  (%set-lib-dir)
  (if (and *om-lib-directory* (probe-file *om-lib-directory*))
      #+(or win32 linux)
      (namestring (make-pathname :directory (pathname-directory *om-lib-directory*)
                                 :host (pathname-host *om-lib-directory*)
                                 :device (pathname-device *om-lib-directory*)
                                 :name (pathname-name lib)
                                 :type (pathname-type lib)))
      #+darwin
      (make-pathname :directory (append (pathname-directory *om-lib-directory*)
                                        (let ((frameworkpos
                                                (position "framework" (cdr (pathname-directory lib))
                                                          :test 'string-equal
                                                          :key #'(lambda (item)
                                                                   (cadr (multiple-value-list
                                                                          (string-until-char item ".")))))))
                                          (when frameworkpos
                                            (subseq (pathname-directory lib) (1+ frameworkpos)))))
                     :host (pathname-host *om-lib-directory*)
                     :device (pathname-device *om-lib-directory*)
                     :name (pathname-name lib)
                     :type (pathname-type lib))
      lib))
