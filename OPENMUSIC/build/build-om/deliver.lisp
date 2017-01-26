(in-package "CL-USER")

(load-all-patches)

#+lispworks6(require "hqn-web")

#+win32(require "ole")

(print "==============================")
(print "LOADING SOURCES")
(print "==============================")

(load (current-pathname "build-om"))


(print "==============================")
(print "APPLICATION SETUP")
(print "==============================")


(defvar *app-name+version* "OM")
(setf *app-name+version* (concatenate 'string #-linux "OM " #+linux "OM_" *version-str*))

(defparameter *om-directory-folders* (butlast (pathname-directory (current-pathname)) 2))

;;;==========================
;;; DEFAULT INTERFACE (MACOS)
;;;==========================

#+cocoa
(capi:define-interface om-application (capi::cocoa-default-application-interface) ()
  (:menus
   (application-menu
      "OM" ; *app-name+version*
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
          :callback-type :interface)))
       ))
   ;(windows-menu
   ;   "Windows"
   ;   ((:component
   ;     (("Workspace"
   ;       :callback 'om::show-workspace-win
   ;       :accelerator "accelerator-shift-w"
   ;       :callback-type :none)
   ;      ("Library"
   ;       :callback 'om::show-packages-win
   ;       :accelerator "accelerator-shift-p"
   ;       :callback-type :none
   ;       :enabled-slot nil)
   ;      ))
   ;    (:component
   ;     (("Lisp Listener"
   ;       :callback 'om::show-listener-win
   ;       :callback-type :none)
   ;      ("Lisp Editor"
   ;       :callback 'oa::om-open-new-text-editor
   ;       :callback-type :none)))
   ;    ))
   )
  (:menu-bar application-menu )  ; windows-menu
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
;;; OM is loaded in LW
;;; Resources are not (yet) in the .app

(print "==============================")
(print "DOC GENERATION:")
(print "==============================")


(setf oa::*om-resources-folder* (make-pathname :directory (append *om-directory-folders* '("resources"))))


(oa::init-sub-rsrc)
(om::set-ref-dir)
(clos::set-clos-initarg-checking nil)
(om::gen-om-reference)

(defparameter *startup-bmp* nil)
(setq *startup-bmp* 
      (or (find *version-str* 
                (directory (make-pathname :directory (pathname-directory (current-pathname))) :directories nil)
                :key 'pathname-name :test 'string-equal)
          (probe-file (merge-pathnames (make-pathname :name "om" :type "bmp") (current-pathname)))))
    

;;;==========================
;;; SOURCE DEFINITIONS
;;;==========================
; doesn't work anymore in LW 7 ?
;*active-finders*

(print "==============================")
(print "SOURCE TRACKING")
(print "==============================")

(dspec::save-tags-database (make-pathname :directory (pathname-directory oa::*om-resources-folder*)
                                          :name "dspec-database" :type oa::*om-compiled-type*))
(dspec:discard-source-info)

(defvar *recorded-root* cl-user::*om-src-directory*)

;;;==========================
;;; BUILD IMAGE
;;;==========================

(print "==============================")
(print "CREATING APP")
(print "==============================")


(defvar *app-name* nil)
#+cocoa
(when (save-argument-real-p)
  (compile-file-if-needed (sys:example-file  "configuration/macos-application-bundle") :load t)
  (setq *app-name*
        (create-macos-application-bundle (make-pathname :directory *om-directory-folders*
                                                       :name *app-name+version*)
                                         :document-types `(("Patch" ("omp") ,(merge-pathnames "mac/patch.icns" *load-pathname*))
                                                           ("Maquette" ("omm") ,(merge-pathnames "mac/maq.icns" *load-pathname*))
                                                           ("Sheet" ("oms") ,(merge-pathnames "mac/sheet.icns" *load-pathname*))
                                                           ("Class" ("omc") ,(merge-pathnames "mac/class.icns" *load-pathname*))
                                                           ("Method" ("ome") ,(merge-pathnames "mac/meth.icns" *load-pathname*))
                                                           ("Workspace" ("omws") ,(merge-pathnames "mac/om.icns" *load-pathname*)))
                                        :application-icns (merge-pathnames "mac/om.icns" *load-pathname*)
                                        :identifier "fr.ircam.repmus.openmusic"
                                        :version (version-to-string *version* t nil)
                                        )))

#+win32
(setf *app-name* (make-pathname :directory *om-directory-folders* :name *app-name+version* :type "exe"))
#+linux
(setf *app-name* (make-pathname :directory *om-directory-folders* :name *app-name+version*))


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
  #+(or linux win32) (define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)
  (om::set-language *release-language*)
  
  (om-lisp::init-root-definition-pathname *recorded-root* om::*om-root*)
  
  (oa::om-init-funcall)
  
  (setf dspec::*active-finders* (append dspec::*active-finders*
                                        (list (merge-pathnames 
                                               #+macosx(concatenate 'string *app-name+version* ".app/Contents/Resources/dspec-database." oa::*om-compiled-type*)
                                               #-macosx(concatenate 'string "resources/dspec-database." oa::*om-compiled-type*)
                                               om-api:*om-root*))))

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

(print "==============================")
(print "MOVING RESOURCES")
(print "==============================")

#+macosx
(let ((libs-folder (merge-pathnames "lib/mac/" oa::*om-resources-folder*))
      (app-libs-folder (make-pathname 
                        :directory (append 
                                    *om-directory-folders* 
                                    (list (concatenate 'string *app-name+version* ".app") "Contents" "Frameworks"))))
      (app-resources-folder (make-pathname 
                        :directory (append 
                                    *om-directory-folders* 
                                    (list (concatenate 'string *app-name+version* ".app") "Contents" "Resources")))))
  
  (print (format nil "COPYING LIBRARIES TO: ~A" app-libs-folder))
  (unless (string-equal (namestring libs-folder) (namestring app-libs-folder))
    (om::copy-folder libs-folder app-libs-folder))
  
  (print (format nil "COPYING RESOURCES TO: ~A" app-resources-folder))
  (loop for item in (oa::om-directory oa::*om-resources-folder* :files t :directories t) 
        unless (string-equal "lib" (car (last (pathname-directory item)))) do
        (if (om::directoryp item)
          (om::copy-folder item (make-pathname :device (pathname-device app-resources-folder) 
                                               :directory (append (pathname-directory app-resources-folder) (last (pathname-directory item)))))
          (om::om-copy-file item (make-pathname :device (pathname-device app-resources-folder) 
                                            :directory (pathname-directory app-resources-folder)
                                            :name (pathname-name item) :type (pathname-type item)))))
  )

(print "==============================")
(print "DELIVER")
(print "==============================")
(deliver 'init-om
         *app-name*
         0
         #+macosx :split #+macosx :resources
         :interface :capi
	 :keep-editor t
	 :keep-debug-mode t
         :keep-load-function t :keep-pretty-printer t
         :keep-complex-numbers nil
         :keep-conditions :all
         :keep-xref-info t   ;; ??
         :editor-style :default
         :startup-bitmap-file NIL ;; *startup-bmp*  ;; removed because of a delivery bug with menus in OM 7
         #+win32 :keep-gc-cursor #+win32 nil
         #+win32 :versioninfo #+win32 (list :binary-version (read-from-string (version-to-hex *version*))
                                            :version-string (version-to-string *version* t nil)
                                            :company-name "IRCAM" :product-name "OpenMusic" :file-description "")
         #+win32 :console #+win32 :input
         :quit-when-no-windows #+win32 t #-win32 nil
         #+(or cocoa win32) :packages-to-keep #+cocoa '(:objc)  #+win32 '(:comm)
         #+win32 :icon-file #+win32 "./win/OpenMusic.ico")




  
;(loop for lib in (directory libs-folder :directories t)
;        do (print (format nil "COPY: ~A => ~A" (namestring lib) (namestring app-libs-folder)))
        


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
