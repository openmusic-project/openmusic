(in-package "CL-USER")

(load-all-patches)

(require "hqn-web")

#+win32(require "ole")

(load (current-pathname "build-om"))


(defvar *app-name+version* "OM")
(setf *app-name+version* (concatenate 'string #-linux "OM " #+linux "OM_" *version-str*))


;;;==========================
;;; DEFAULT INTERFACE (MACOS)(defmethod osc-start-receive ((box ReceiveBox))
;;;==========================

#+cocoa
(capi:define-interface om-application (capi::cocoa-default-application-interface) ()
  (:menus
   (application-menu
      *app-name+version*
      ((:component
        (("About OM"
          :callback 'om::show-about-win
          :callback-type :none)))
       (:component
        (("Preferences..."
          :callback 'om::show-preferences-win
          :accelerator "accelerator-,"
          :callback-type :none)))

       ;(:component
       ; ()
        ;; This is a special named component where the CAPI will
        ;; attach the standard Services menu.
       ; :name :application-services)
       (:component
        (("Hide OM"
          :accelerator "accelerator-h"
          :callback-data :hidden)
         ("Hide Others"
          :accelerator "accelerator-meta-h"
          :callback-data :others-hidden)
         ("Show All"
          :callback-data :all-normal))
        :callback #'(setf capi:top-level-interface-display-state)
        :callback-type :data-interface)
       (:component
        (("Quit OM"
          :accelerator "accelerator-q"
          :callback #'(lambda (interface)
                        (capi:destroy interface))
          :callback-type :interface)))))
   (windows-menu
      "Windows"
      ((:component
        (("Workspace"
          :callback 'om::show-workspace-win
          :accelerator "accelerator-shift-w"
          :callback-type :none)
         ("Library"
          :callback 'om::show-packages-win
          :accelerator "accelerator-shift-p"
          :callback-type :none
          :enabled-slot nil)
         ))
       (:component
        (("Lisp Listener"
          :callback 'om::show-listener-win
          :callback-type :none)
         ("Lisp Editor"
          :callback 'oa::om-open-new-text-editor
          :callback-type :none)))
       ))
   )
  (:menu-bar application-menu windows-menu)
  (:default-initargs
   :title *app-name+version*
   :application-menu 'application-menu

   :confirm-destroy-function 'quit-callback
   ;:destroy-callback #'(lambda (interface) (oa::om-exit-funcall))
   ;:top-level-hook 'oa::interface-handle-error
   ;:window-styles '(:internal-borderles :never-iconic :textured-background) ;; :hides-on-deactivate-window) :toolbox
   ;:display-state :normal
   ; POUR ouvrir des fichiers directement
   :message-callback 'om-application-callback
   :dock-menu 'om-dock-menu
   ))


#+cocoa
(capi:define-menu om-dock-menu (self)
  "Dock Menu"
  ((:component
        (("Workspace"
          :callback 'om::show-workspace-win
          :accelerator "accelerator-shift-w"
          :callback-type :none)
         ("Library"
          :callback 'om::show-packages-win
          :callback-type :none
          :enabled-slot nil)
         ))
       (:component
        (("Lisp Listener"
          :callback 'om::show-listener-win
          :callback-type :none)
         ("Lisp Editor"
          :callback 'oa::om-open-new-text-editor
          :callback-type :none)))
       )
  )

#+cocoa
(defun om-application-callback (self message &rest args)
  (declare (ignore self))
  (case message
    (:open-file
     (if om::*current-workspace*
     (let ((filename (pathname (car args))))
       (om::show-workspace-win)
       (cond ((or (string-equal "omp" (pathname-type filename))
                  (string-equal "omm" (pathname-type filename))
                  (string-equal "she" (pathname-type filename)))
             (when (capi:prompt-for-confirmation (format nil "Import ~A to your workspace?" (pathname-name filename))
                                                  :cancel-button nil)
             (om::import-file-to-ws (om::panel om::*om-workspace-win*) filename (om::om-make-point 40 40))
             ))
             ((oa::directoryp filename)
              (when (capi:prompt-for-confirmation (format nil "Import folder ~A to your workspace?"
                                                          (car (last (pathname-directory filename))))
                                                 :cancel-button nil)
                (om::make-new-folder (om::panel om::*om-workspace-win*) filename (om::om-make-point 40 40))
                ))
             ((and (oa::directoryp (pathname (concatenate 'string (car args) "/")))
                   (probe-file (pathname (concatenate 'string (car args) "/"))))
              (setf filename (pathname (concatenate 'string (car args) "/")))
              (when (capi:prompt-for-confirmation (format nil "Import folder ~A to your workspace?"
                                                          (car (last (pathname-directory filename))))
                                                  :cancel-button nil)
                (om::make-new-folder (om::panel om::*om-workspace-win*) filename (om::om-make-point 40 40))
                ))
             ((string-equal "lisp" (pathname-type filename))
              (oa::om-open-new-text-file filename))
             (t nil))
       )
       (lispworks::quit)))))

#+cocoa
(defun default-interface ()
  (capi:set-application-interface
   (setf oa::*om-app* (make-instance 'om-application))))

#+cocoa
(defun quit-callback (interface)
  (om::quit-om-callback))


;;;==========================
;;; DOC
;;;==========================

(setf oa::*om-resources-folder* 
      (make-pathname :directory (append (butlast (pathname-directory (current-pathname)) 2) (list "resources"))))
(oa::init-sub-rsrc)
(om::set-ref-dir)
(clos::set-clos-initarg-checking nil)
(om::gen-om-reference)

;;;==========================
;;; SOURCE DEFINITIONS
;;;==========================

; (*active-finders*)

(dspec::save-tags-database (make-pathname :directory (append (butlast (pathname-directory (current-pathname)) 2) (list "resources"))
                                          :name "dspec-database" :type oa::*om-compiled-type*))

(dspec:discard-source-info)


;;;==========================
;;; BUILD IMAGE
;;;==========================


(defvar *app-name* nil)
#+cocoa
(when (save-argument-real-p)
  (compile-file-if-needed (sys:example-file  "configuration/macos-application-bundle") :load t)
  (setq *app-name*
        (write-macos-application-bundle (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2)
                                                       :name *app-name+version*)
                                        :document-types '(("Patch" ("omp") "./mac/patch.icns")
                                                          ("Maquette" ("omm") "./mac/maq.icns")
                                                          ("Sheet" ("oms") "./mac/sheet.icns")
                                                          ("Class" ("omc") "./mac/class.icns")
                                                          ("Method" ("ome") "./mac/meth.icns")
                                                          ("Workspace" ("omws") "./mac/om.icns"))
                                        :application-icns "./mac/om.icns"
                                        :identifier "ircam.openmusic"
                                        :version (version-to-string *version* t nil)
                                        )))

#+win32
(setf *app-name* (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2) :name *app-name+version* :type "exe"))
#+linux
(setf *app-name* (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2) :name *app-name+version*))


(setf *debugger-hook* 'oa::om-debugger-hook)

;;; INIT FUNCALL
(defun init-om ()
  (setf om::*om-startup* t)
  (push :om-deliver *features*)
  #+cocoa(default-interface)
  (oa::om-root-init)
  (oa::om-api-init)
  (om-lisp::set-om-debugger)
  #+cocoa(objc:make-autorelease-pool)
  (oa::set-om-logical-path)
  (om::load-modif-patches)
  (clos::set-clos-initarg-checking nil)
  #+win32 (define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)
  (om::set-language *release-language*)
  (oa::om-init-funcall)
  (setf dspec::*active-finders* (append dspec::*active-finders*
                                        (list (make-pathname
                                               :directory (pathname-directory (om::omroot "resources;"))
                                               :name "dspec-database" :type oa::*om-compiled-type*))))

  (setf *print-case* :downcase)
  #+cocoa(setf system::*stack-overflow-behaviour* nil)

  (in-package :om)
  (om::show-workspaces-dialog)
  (oa::om-select-window om::*om-workspace-win*)
  (capi::execute-with-interface om::*om-workspace-win* #'(lambda () (in-package :om)))
  (setf om::*om-startup* nil)
)

(defun version-to-hex (n)
  (format nil "#x~4,'0X~4,'0X~4,'0X~4,'0X"
          (round n)
          (round (* (cadr (multiple-value-list (round n))) 100))
          (round (* (cadr (multiple-value-list (round (* 100 n)))) 100))
          (round (* (cadr (multiple-value-list (round (* 10000 n)))) 100))
          ))



; (version-to-hex 6.020005)
; #x0006000200000005

(deliver 'init-om
         *app-name*
         0
         :interface :capi
	 :keep-editor t
	 :keep-debug-mode t
         :keep-load-function t
         #+win32 :editor-style #+win32 :pc
         #+win32 :keep-gc-cursor #+ win32 nil
         #+win32 :versioninfo #+win32 (list :binary-version (read-from-string (version-to-hex *version*))
                                            :version-string (version-to-string *version* t nil)
                                            :company-name "IRCAM" :product-name "OpenMusic" :file-description "")
         #+win32 :console #+win32 :input
         :quit-when-no-windows #+win32 t #-win32 nil
         #+(or cocoa win32) :packages-to-keep #+cocoa '(:objc)  #+win32 '(:comm)
         #+win32 :icon-file #+win32 "./win/OpenMusic.ico")


;  :editor-commands-to-keep :all-groups
;========================

;;; MAC :
; /Applications/LispWorks\ 5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal -build deliver.lisp
; (save-universal-from-script "../../image/macos-i/OM 6.0" "deliver.lisp")


;;; WIN :
; lispworks-5-1-0-x86-win32.exe -build deliver.lisp
;

;;; LINUX : 
; lispworks-6-1-0-x86-linux -build deliver.lisp		 ; this file
