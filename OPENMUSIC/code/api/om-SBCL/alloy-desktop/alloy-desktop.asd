(in-package :asdf-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (merge-pathnames "alloy-editor/"
                            (make-pathname :name nil :type nil :defaults *load-truename*))
           asdf:*central-registry*
           :test #'equal))

(asdf:defsystem "alloy-desktop"
  :depends-on ("cffi"
               "com-on"
               "alloy"
               "alloy-glfw"
               "alloy-simple-presentations"
               "bordeaux-threads"
               "colored"
               "file-select"
               "color-select"
               "font-select"
               "paper-print"
               "imagine/formats"
               "swank"
               "trivial-gray-streams"
               "cl-ppcre"
               "zpb-ttf"
               "3d-math"
               "os-fonts"
               "alloy-editor")
  :serial t
  :components ((:file "package")
               (:file "lispworks/fli")
               (:file "core/protocol")
               (:file "core/float-traps")
               (:file "core/ui-thread")
               (:file "core/desktop-session")
               (:file "core/event-pump")
               (:file "core/frame-scheduling")
               (:file "input/modifiers")
               (:file "system/process")
               (:file "system/thread")
               (:file "system/serial-worker")
               (:file "system/finalize")
               (:file "system/pathname")
               (:file "system/compile")
               (:file "system/platform")
               (:file "system/environment")
               (:file "native/native")
               (:file "native/gestures" :if-feature :darwin)
               (:file "window/window")
               (:file "window/registry")
               (:file "window/ownership")
               (:file "window/native")
               (:file "window/geometry")
               (:file "window/transient")
               (:file "window/focus")
               (:file "window/cues")
               (:file "window/modal")
               (:file "window/palette")
               (:file "window/assembly")
               (:file "graphics/context-resources")
               (:file "window/window-lifecycle")
               (:file "input/pointer")
               (:file "input/wheel")
               (:file "window/close")
               (:file "view/view")
               (:file "view/placement")
               (:file "view/pinboard")
               (:file "view/scroll")
               (:file "view/tabs")
               (:file "widgets/dialog-item")
               (:file "widgets/controls")
               (:file "widgets/chrome")
               (:file "widgets/choice")
               (:file "widgets/grid")
               (:file "menu/menu")
               (:file "widgets/choice-popup")
               (:file "widgets/filter")
               (:file "native/macos-menu")
               (:file "text/text")
               (:file "text/caret")
               (:file "text/selection")
               (:file "lisp/environment")
               (:file "lisp/introspection")
               (:file "graphics/scale")
               (:file "graphics/font-metrics")
               (:file "graphics/font")
               (:file "core/desktop-application")
               (:file "graphics/text-measure")
               (:file "graphics/text-render")
               (:file "graphics/drawing")
               (:file "view/render")
               (:file "graphics/graphics-port")
               (:file "graphics/image")
               (:file "services/services")
               (:file "services/cursor")
               (:file "services/clipboard")
               (:file "services/timer")
               (:file "services/drag-and-drop")
               (:file "services/tooltip")
               (:file "services/transient-drawing")
               (:file "dialogs/dialogs")
               (:file "dialogs/file-selection")
               (:file "dialogs/file-chooser")
               (:file "dialogs/prompts")
               (:file "dialogs/progress")
               (:file "printing/printing")
               (:file "printing/gdi-port" :if-feature :win32)
               (:file "printing/cg-port" :if-feature :darwin)))
