;=========================================================================
;;; OpenMusic delivery on SBCL
;;; load this file to produce an executable from OM sources
;=========================================================================

(in-package "CL-USER")

(defvar *emacs-mode* nil)

(print "==============================")
(print "LOADING SOURCES")
(print "==============================")

(load (make-pathname :directory (butlast (pathname-directory *load-truename*))
                     :name "build-om" :type "lisp" :defaults *load-truename*))

(ql:quickload :deploy)

(when *emacs-mode*
  (ql:quickload "swank/exts")
  (setf (symbol-value (find-symbol "*GLOBALLY-REDIRECT-IO*" "SWANK")) nil))

(print "==============================")
(print "APPLICATION SETUP")
(print "==============================")

(defvar *app-name+version* (concatenate 'string #-linux "OM " #+linux "OM_" *version-str* #+linux "_SBCL"))

(defparameter *om-root-folders* (butlast (pathname-directory *load-truename*) 2))

(defparameter *release-folder*
  (make-pathname :device (pathname-device *load-truename*)
                 :directory #+linux *om-root-folders*
                            #-linux (append (butlast *om-root-folders*)
                                            (list (concatenate 'string *app-name+version* " SBCL")))))

#+darwin (require :asdf)

#+darwin
(defparameter *om-asdf-system*
  (eval `(asdf:defsystem "openmusic"
           :long-name ,*app-name+version*
           :version ,*version-str*
           :author "IRCAM-Centre Georges Pompidou")))

#+darwin
(defparameter *om-sbcl-mac-directory*
  (merge-pathnames (make-pathname :directory '(:relative "mac"))
                   (make-pathname :name nil :type nil :defaults *load-truename*)))

#+darwin
(defparameter *om-mac-resources-directory*
  (make-pathname :device (pathname-device *load-truename*)
                 :directory (append (butlast (pathname-directory *load-truename*))
                                    '("mac"))))

#+darwin
(setf deploy:*info-plist-template* (merge-pathnames "Info.plist.in" *om-sbcl-mac-directory*)
      deploy::*macos-app-icon* (merge-pathnames "om.icns" *om-mac-resources-directory*))

;;;==========================
;;; DOC
;;;==========================

(print "==============================")
(print "DOC GENERATION:")
(print "==============================")

(setf oa::*om-resources-folder* (make-pathname :device (pathname-device *load-truename*)
                                               :directory (append *om-root-folders* '("resources"))))

(oa::init-sub-rsrc)
(om::set-ref-dir)
(om::gen-om-reference)

;;;==========================
;;; SOURCE DEFINITIONS
;;;==========================

(defvar *recorded-root* cl-user::*om-src-directory*)

;;;==========================
;;; DEPLOY HOOKS
;;;==========================

(deploy:define-hook (:deploy om-alloy-shaders) ()
  (deploy:status 1 "Caching Alloy shaders in-memory")
  (org.shirakumo.alloy.renderers.opengl::cache-shader-source-files)
  (setf (fdefinition 'org.shirakumo.alloy.renderers.opengl::shader-source)
        #'org.shirakumo.alloy.renderers.opengl::shader-source-from-cache))

;;;==========================
;;; INIT FUNCALL
;;;==========================

#+darwin
(defun om-bundle-contents ()
  "Contents/ of the bundle this executable runs from."
  (let ((runtime (truename sb-ext:*runtime-pathname*)))
    (make-pathname :host (pathname-host runtime) :device (pathname-device runtime)
                   :directory (butlast (pathname-directory runtime))
                   :name nil :type nil)))

(defun init-om-root ()
  (push :om-deliver *features*)
  (oa::om-root-init)
  (setf oa::*om-fonts-dir* (merge-pathnames (make-pathname :directory '(:relative "resources" "fonts"))
                                            oa::*om-root*)
        alld:*bundled-font-directory* oa::*om-fonts-dir*)
  #+darwin
  (let ((resources (merge-pathnames "Resources/" (om-bundle-contents))))
    (setf oa::*om-resources-folder* resources
          oa::*om-fonts-dir* (merge-pathnames (make-pathname :directory '(:relative "fonts"))
                                              resources)
          alld:*bundled-font-directory* oa::*om-fonts-dir*))
  (om-lisp::init-root-definition-pathname
   *recorded-root*
   #+darwin (merge-pathnames "Resources/" (om-bundle-contents))
   #-darwin oa::*om-root*))

#+darwin
(defparameter *om-heap-mb* (round (sb-ext:dynamic-space-size) (* 1024 1024)))

#+darwin
(defun om-reexec-with-heap ()
  (let* ((arguments (list* (uiop:native-namestring sb-ext:*runtime-pathname*)
                           "--dynamic-space-size" (format nil "~D" *om-heap-mb*)
                           "--core" (uiop:native-namestring sb-ext:*core-pathname*)
                           "--end-runtime-options"
                           (cdr sb-ext:*posix-argv*)))
         (count (length arguments)))
    (cffi:foreign-funcall "setenv" :string "OM_HEAP_REEXEC" :string "1" :int 1 :int)
    (let ((argv (cffi:foreign-alloc :pointer :count (1+ count))))
      (loop for argument in arguments
            for index from 0
            do (setf (cffi:mem-aref argv :pointer index)
                     (cffi:foreign-string-alloc argument)))
      (setf (cffi:mem-aref argv :pointer count) (cffi:null-pointer))
      (cffi:foreign-funcall "execv" :string (first arguments) :pointer argv :int)
      (format *error-output* "~&OM: execv returned; going on with ~D MB of heap.~%"
              (round (sb-ext:dynamic-space-size) (* 1024 1024))))))

#+darwin
(deploy:define-hook (:boot om-heap most-positive-fixnum) ()
  (when (< (sb-ext:dynamic-space-size) (* *om-heap-mb* 1024 1024))
    (if (uiop:getenv "OM_HEAP_REEXEC")
        (format *error-output*
                "~&OM: asked for ~D MB of heap, got ~D MB, and the re-exec already ran; going on.~%"
                *om-heap-mb* (round (sb-ext:dynamic-space-size) (* 1024 1024)))
        (om-reexec-with-heap))))

(deploy:define-hook (:boot om-root #+darwin (1- most-positive-fixnum)
                                   #-darwin most-positive-fixnum) ()
  (init-om-root))

(defun init-om ()
  (start-openmusic))

(defun init-om-emacs ()
  (setf om-lisp:*emacs-mode* t)
  (setf *package* (find-package :om))
  (handler-bind ((error #'invoke-debugger))
    (sb-impl::toplevel-repl nil)))

;;;==========================
;;; BUILD IMAGE
;;;==========================

(print "==============================")
(print "CREATING APP")
(print "==============================")

#+win32
(let ((system-root (string-downcase (uiop:getenv "SystemRoot"))))
  (dolist (library (deploy:list-libraries))
    (let ((path (deploy:library-path library)))
      (when (and path (uiop:string-prefix-p system-root (string-downcase (uiop:native-namestring path))))
        (setf (deploy:library-dont-deploy-p library) t)))))

#+linux
(dolist (library (deploy:list-libraries))
  (let ((path (deploy:library-path library)))
    (when (and path (some (lambda (prefix) (uiop:string-prefix-p prefix (uiop:native-namestring path)))
                          '("/lib/" "/lib64/" "/usr/lib/" "/usr/lib64/")))
      (setf (deploy:library-dont-deploy-p library) t))))

#+linux
(setf deploy:*library-location* #p"resources/lib/linux/")

#+darwin
(dolist (library (deploy:list-libraries))
  (let ((path (deploy:library-path library)))
    (when (or (null path)
              (some (lambda (prefix) (uiop:string-prefix-p prefix (uiop:native-namestring path)))
                    '("/usr/lib/" "/System/" "/Library/Apple/"))
              (not (string-equal "dylib" (pathname-type path))))
      (setf (deploy:library-dont-deploy-p library) t))))

#+darwin
(deploy:define-hook (:deploy om-bundle) (directory op)
  (when (typep op 'asdf:macos-app-deploy-op)
    (let* ((contents (uiop:pathname-parent-directory-pathname directory))
           (resources directory)
           (frameworks (merge-pathnames "Frameworks/" contents))
           (source (make-pathname :device (pathname-device *load-truename*)
                                  :directory *om-root-folders*))
           (libraries (merge-pathnames (make-pathname :directory
                                                      (list :relative "resources" "lib"
                                                            #+arm64 "m1" #-arm64 "mac"))
                                       source)))
      (dolist (icon '("patch.icns" "maq.icns" "sheet.icns" "class.icns" "meth.icns"))
        (let ((file (merge-pathnames icon *om-mac-resources-directory*)))
          (unless (probe-file file)
            (error "Missing document icon for the application bundle: ~A" file))
          (deploy:copy-file file (merge-pathnames icon resources))))
      (unless (probe-file libraries)
        (error "Missing the macOS libraries of the release: ~A" libraries))
      (deploy:copy-directory-tree libraries frameworks :copy-root nil)
      (deploy:copy-directory-tree
       (merge-pathnames "resources/" source) resources :copy-root nil
       :exclude (lambda (path &optional destination)
                  (declare (ignore destination))
                  (equal "lib" (car (last (pathname-directory path))))))
      (deploy:copy-directory-tree (merge-pathnames "code/" source) resources)
      (deploy:copy-directory-tree (merge-pathnames "init/" source)
                                  (merge-pathnames "Init/" contents) :copy-root nil)
      (let ((launcher (merge-pathnames "openmusic" resources)))
        (with-open-file (out launcher :direction :output :if-exists :supersede)
          (format out "#!/bin/sh~%")
          (format out "here=$(cd -- \"$(dirname -- \"$0\")\" && pwd -P)~%")
          (format out "exec \"$here/../MacOS/~A\" --core \"$here/openmusic.core\" \"$@\"~%"
                  (asdf:system-long-name *om-asdf-system*)))
        (uiop:run-program (list "chmod" "755" (uiop:native-namestring launcher)))))))

#+darwin
(when *emacs-mode* (setf deploy:*library-location* #p"../Frameworks/"))

(deploy:remove-hook :build 'deploy::finalize-clos :disable t)

(defvar *app-pathname*
  #+darwin (merge-pathnames "application" *release-folder*)
  #-darwin (merge-pathnames (make-pathname :name *app-name+version*
                                           :type #+win32 "exe" #-win32 nil)
                            *release-folder*))

(defvar *emacs-app-pathname*
  #+darwin (make-pathname :device (pathname-device *release-folder*)
                          :directory (append (pathname-directory *release-folder*)
                                             (list (concatenate 'string *app-name+version* ".app")
                                                   "Contents" "Resources"))
                          :name "openmusic")
  #-darwin (merge-pathnames (make-pathname :name "openmusic" :type #+win32 "exe" #-win32 nil)
                            *release-folder*))

#+darwin
(let ((operation (asdf:make-operation (if *emacs-mode*
                                          'deploy:deploy-image-op
                                          'asdf:macos-app-deploy-op))))
  (reinitialize-instance operation
                         :system *om-asdf-system*
                         :output-file (if *emacs-mode* *emacs-app-pathname* *app-pathname*)
                         :entry-point (if *emacs-mode* 'init-om-emacs 'init-om))
  (deploy:deploy operation))

#-darwin
(if *emacs-mode*
    (deploy:deploy 'deploy:deploy-console-op :output-file *emacs-app-pathname* :entry-point 'init-om-emacs)
    (deploy:deploy 'deploy:deploy-op :output-file *app-pathname* :entry-point 'init-om))
