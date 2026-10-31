(in-package :cl-user)

(unless (find-package "ALLOY-DESKTOP")
  (error "ALLOY-DESKTOP must be loaded by build-om.lisp before the OM-API bridge."))

(defpackage "OM-API"
  (:nicknames "OA")
  (:use "COMMON-LISP" "CL-USER")
  (:import-from "ORG.SHIRAKUMO.ALLOY" "DECLINE")
  (:import-from "ALLOY-DESKTOP" "*CURRENT-RENDERER*" "*CURRENT-FG-COLOR*" "*CURRENT-BG-COLOR*"
                "*CURRENT-PEN-SIZE*" "*CURRENT-LINE-STYLE*" "*CURRENT-DASH-PATTERN*"
                "*CURRENT-VIEW-HEIGHT*" "*POX*" "*POY*" "*LAST-DIRECTORY*"
                "*PROCESS-INITIAL-BINDINGS*")
  (:local-nicknames ("ALLOY"          "ORG.SHIRAKUMO.ALLOY")
                    ("WINDOWING"      "ORG.SHIRAKUMO.ALLOY.WINDOWING")
                    ("GLFW"           "ORG.SHIRAKUMO.ALLOY.RENDERERS.GLFW")
                    ("%GLFW"          "ORG.SHIRAKUMO.FRAF.GLFW")
                    ("SIMPLE"         "ORG.SHIRAKUMO.ALLOY.RENDERERS.SIMPLE")
                    ("PRESENTATIONS"  "ORG.SHIRAKUMO.ALLOY.RENDERERS.SIMPLE.PRESENTATIONS")
                    ("CHROMA"         "ORG.SHIRAKUMO.ALLOY.CHROMA")
                    ("COLORS"         "ORG.SHIRAKUMO.ALLOY.CHROMA.COLORS")
                    ("COM"            "ORG.SHIRAKUMO.COM-ON")
                    ("ALLD"           "ALLOY-DESKTOP")))

(defpackage "OM-LISP")

(defpackage "OM-EDIT"
  (:nicknames "OE"))

;; LispWorks compatibility layer: SBCL stubs for LW-specific symbols
;; that the kernel and externals expect. 
(compile&load (make-pathname :directory (pathname-directory (truename *load-pathname*))
                     :name "lw-compat" :type "lisp"))

(in-package :oa)

(defvar *api-directory* nil)
(setf *api-directory* (pathname-directory (truename *load-pathname*)))

(defun %resolve-relative (relative)
  "Resolve 'subdir/name' or 'name' relative to *api-directory*."
  (let ((slash-pos (position #\/ relative :from-end t)))
    (if slash-pos
        (make-pathname :directory (append *api-directory*
                                          (list (subseq relative 0 slash-pos)))
                       :name (subseq relative (1+ slash-pos)))
        (make-pathname :directory *api-directory* :name relative))))

(defvar *api-files* nil)
(setf *api-files* '("om-api"
                    "mop"
                    "system"
                    "tools"
                    "graphics"
                    "graphic-object"
                    "view"
                    "window"
                    "item-view"
                    "draw"
                    "actions"
                    "transient-drawing"
                    "icons-picts"
                    "menu"
                    "dialog-items"
                    "user-interface"
                    "draganddrop"
                    "tooltips"
                    "print"
                    "libraries"
                    "om-special"))

(mapc #'(lambda (rel) (cl-user::compile&load (%resolve-relative rel)))
      *api-files*)

(load (merge-pathnames "sbcl-lisp-tools/load-sbcl-lisp-tools.lisp" *load-pathname*))
(load (merge-pathnames "editors/load-editors.lisp" *load-pathname*))

(pushnew :om-api *features*)
