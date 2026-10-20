(asdf:defsystem #:alloy-editor
  :description "A text editor over Alloy: a buffer, its layout and a view of it."
  :serial T
  :depends-on (#:alloy-glfw
               #:alloy-simple-presentations
               #:3b-bmfont
               ;; characters to bytes and back, strictly: already under the toolkit
               ;; through the foreign-function layer (cffi.asd: the cffi system)
               #:babel
               ;; a lock, without naming one implementation: text handed in from
               ;; other threads is queued under it
               #:bordeaux-threads
               ;; the character output stream printing goes through
               #:trivial-gray-streams)
  :components ((:file "package")
               (:file "buffer")
               (:file "document")
               (:file "layout")
               (:file "policy")
               (:file "view")
               (:file "commands")
               (:file "lisp-mode")
               (:file "output")
               (:file "input")))
