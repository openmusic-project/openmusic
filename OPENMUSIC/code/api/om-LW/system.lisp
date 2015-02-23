;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; SYSTEM UTILITIES (File management, foreign calls, scheduler, etc...)
;DocFile
;;===========================================================================

(in-package :om-api)

;;;===================
;;; export :
;;;===================
(export '(
          om-add-init-func
          om-init-funcall
          om-add-exit-cleanup-func
          *om-os*
          throw-cancel
          catch-cancel
          om-confirmed-quit
          om-eval-enqueue
          om-set-eval-process
          om-delayed-funcall

          om-gc
          om-with-redefinitions
          om-redefinition-warnings
          om-set-redefinition-warnings
          om-with-redefinition-warnings
          om-compiler-warnings
          om-set-compiler-warnings
          om-with-compiler-warnings
          om-load-verbose
          om-set-load-verbose
          om-with-load-verbose
          
          *om-separator*
          *om-compiled-type*
          directoryp
          directory-pathname-p
          systemdirectoryp
          
          om-make-pathname
          om-create-directory
          om-create-file
          om-copy-file
          om-copy-directory
          om-delete-directory
          om-delete-file
          stream-eofp
          ; om-deep-delete-file
          om-load-file
          om-directory
          om-read-line
          om-correct-line
          om-namestring
          *om-root*
          
          om-get-user-name 
          om-user-home 
          om-user-pref-folder
          om-get-date
          om-path2cmdpath
          om-cmd-line
          om-run-application
          om-run-program
          *om-open-cmd*
	  om-find-process
          ;om-select-process
          om-select-program

          om-run-process
          om-kill-process
          om-with-priority
          om-with-new-gc
          om-without-interrupts
          om-process-state

          om-error-handle-funcall
          om-with-error-handle

          om-standalone-p
          
          om-set-clipboard
          om-get-clipboard
          
          ) :om-api)



(defvar *om-os* nil)
#+macosx(setf *om-os* :mac)
#+win32(setf *om-os* :win)
#+linux(setf *om-os* :linux)

(defparameter *lw-version* 
  (read-from-string (subseq (lisp-implementation-version) 0 1)))

;;; a faire au debut...
;;; (objc:make-autorelease-pool)

(defmacro catch-cancel (&body body)
  `(catch :cancel ,@body))

(defmacro throw-cancel (body)
  `(throw :cancel ,body))

(defun om-set-clipboard (value)
  (capi::set-clipboard (om-front-window) value))

(defun om-get-clipboard ()
  (capi::clipboard (om-front-window)))



;;;==========================
;;; ERROR HANDLERS
;;;==========================

(defun om-error-handle-funcall (func)
  (handler-bind 
      ((error #'(lambda (err)
                  (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                  (abort err))))
    (funcall func)))

(defmacro om-with-error-handle (&body body)
  `(if (om-standalone-p)
      (handler-bind 
           ((error #'(lambda (err)
                       (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                       (abort err))))
         ,@body)
    (progn ,@body))
  )


(defparameter *log-location* nil)

;;; Bind this function to cl::*debugger-hook* in order to catch all unexpected errors...
(defun om-debugger-hook (condition old-debugger-hook)
  (declare (ignore old-debugger-hook))
  (let ((log (capi:prompt-for-confirmation (format nil "An  error occured : ~a~%~%Display Log file ?" condition)
                                           :default-button  nil)))
    (when log
      (let* ((logpath (make-pathname :directory (append (butlast (pathname-directory (dbg::logs-directory))) (list "OpenMusic"))
                                     :name (concatenate 'string "OM-" (substitute #\- #\. *version-str*) "-Log_" 
                                                        (substitute #\- #\: (substitute #\- #\/ (substitute #\_ #\Space (sys::date-string)))))))
             (path 
              (dbg:log-bug-form (format nil "An error occured : ~a" condition)
                                :message-stream t
                                :log-file logpath)))
        (om-lisp::om-open-new-text-file path)
        (unless *log-location*
          (setf *log-location* t)
          (capi::display-message "The log file has been written in ~a" path))))
    (abort)))


(defun om-gc () 
  (system::gc-all))

(defun om-standalone-p ()
  (if (member :om-deliver *features*) t nil))

(setf system::*stack-overflow-behaviour* nil)


;(hcl::current-stack-length)
;(hcl:extend-current-stack 50)

;;;==== INIT AND EXIT FUNCTIONS ====
;------

(defvar *init-func-list* nil)

(defun om-add-init-func (func-name)
   (unless (member func-name *init-func-list* :test 'equal)
      (push func-name *init-func-list*)))

(defun om-init-funcall ()
   (mapc #'(lambda (x) (funcall x)) (reverse *init-func-list*)))

;-------
(defvar *om-cleanup-forms* nil)

(defun om-add-exit-cleanup-func (func-name &optional last?)  
  (unless (member func-name *om-cleanup-forms* :test 'equal)
    (if last?
	(push func-name  *om-cleanup-forms*)
	(setf  *om-cleanup-forms* (append  *om-cleanup-forms* (list func-name)))
	)))

(defun om-exit-funcall ()
  (mapc #'(lambda (x) (funcall x)) (reverse *om-cleanup-forms*))
  t)

;------

(defvar *confirmed-quit* nil)

(defun om-confirmed-quit ()
  (unless *confirmed-quit*
    ;; (setf *confirmed-quit* t)
    ;;; (om-exit-funcall)
    (quit :confirm nil :ignore-errors-p t)))

;------

(defparameter *use-eval-process* t)

(defun om-set-eval-process (mode)
  (setq *use-eval-process* mode))

(defun om-eval-enqueue (form &optional view) 
  (if *use-eval-process*
      (om-lisp::om-eval-on-process #'(lambda () 
                                       (om-listener-echo "Running...")
                                       (if view 
                                           (capi::apply-in-pane-process view 'eval form)
                                         (eval form))
                                       (om-listener-echo "Ready")
                                       )
                                   )
    (eval form)))

(om-add-exit-cleanup-func 'om-lisp::om-abort-eval-process)

;------

(defvar *scheduler-timer* nil)

(defun scheduler-fun () t)

;(defun om-start-scheduler (fun)
;   (setq *scheduler-timer*  (mp:make-named-timer 'om-scheduler fun nil))
;   (mp:schedule-timer *scheduler-timer* 1 0.01))


;(defun om-stop-scheduler ()
;  (when *scheduler-timer*
;    (mp:unschedule-timer *scheduler-timer*)
;    (setf *scheduler-timer* nil)))
  
(defun om-delayed-funcall (time func &rest args)
  (let ((timer (apply 'mp:make-timer (cons func args))))
    (mp:schedule-timer-relative-milliseconds timer time)
    timer))

;(om-add-exit-cleanup-func 'om-stop-scheduler)




;------
(defmacro om-with-redefinitions (&body body)
  `(let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil)) ,@body))

;------
(defun om-redefinition-warnings () lispworks::*HANDLE-WARN-ON-REDEFINITION*)

;------
(defun om-set-redefinition-warnings (t-or-nil) 
  (setf lispworks::*HANDLE-WARN-ON-REDEFINITION* t-or-nil))

;------
(defmacro om-with-redefinition-warnings (t-or-nil &body body)
  `(let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil)) ,@body))

;------
(defun om-compiler-warnings () t)

;------
(defun om-set-compiler-warnings (t-or-nil) t)

;------
(defmacro om-with-compiler-warnings (t-or-nil &body body)
  `(let () ,@body))

;------
(defun om-load-verbose () *load-verbose* )

;------
(defun om-set-load-verbose (t-or-nil) (setf *load-verbose* t-or-nil))

;------
(defmacro om-with-load-verbose (t-or-nil &body body)
   `(let ((*load-verbose* ,t-or-nil)) ,@body))


;;;=============================
;;; FILES
;;;=============================

(defvar *om-separator* "/")

(defvar *om-compiled-type* 
  #+win32 "ofasl"
  #+macosx (if (member :X86 *features*) "xfasl" "nfasl")
  #+linux (pathname-type (cl-user::compile-file-pathname "")))

(defvar *om-root* nil)

;------
;;; ajoute un butlast
(defun om-root-init ()
  (if (om-standalone-p)
      (setf *om-root* (make-pathname 
		       :device (pathname-device (LISP-IMAGE-NAME)) :host (pathname-host (LISP-IMAGE-NAME))
		       :directory 
		       #+cocoa(butlast (pathname-directory (truename (PATHNAME-LOCATION (LISP-IMAGE-NAME)))) 3)
		       #+win32(pathname-directory (truename (PATHNAME-LOCATION (LISP-IMAGE-NAME))))
		       #+linux (pathname-directory
				(cond ((member (second (pathname-directory (lisp-image-name))) '("usr" "opt") :test #'string=)
				       (make-pathname :directory (append (butlast (pathname-directory
										   (truename (pathname-location (lisp-image-name)))))
									 '("share" "openmusic"))))
				      (t (truename (pathname-location (lisp-image-name)))))))
	    cl-user::*om-src-directory* *om-root*)
      (setf *om-root* cl-user::*om-src-directory*)))

(om-api-add-init-func 'om-root-init)


;;; COMPATIBILITY... :

(defun set-om-logical-path ()
  (let* ((root-path  (make-pathname :device (pathname-device (PATHNAME-LOCATION (LISP-IMAGE-NAME)))
                                    :host (pathname-host (PATHNAME-LOCATION (LISP-IMAGE-NAME)))
                                    :directory
                                    #+macosx(butlast (pathname-directory (truename  (PATHNAME-LOCATION (LISP-IMAGE-NAME)))) 3)
                                    #-macosx(pathname-directory (truename (PATHNAME-LOCATION (LISP-IMAGE-NAME))))))
	 (path-code  (make-pathname :device (pathname-device root-path)
				    :directory (append (pathname-directory root-path) (list "CODE"))))
	 (path-api  (make-pathname :device (pathname-device root-path)
				   :directory (append (pathname-directory root-path) (list "CODE" "API" "OM-LW")))))
    ;(print (format nil "~A**;*.*" (namestring root-path)))
    (when (om-standalone-p)
      (setf (logical-pathname-translations "CL")
	    (list (list "**;*.*.*" (format () "~A**/*.*" (namestring root-path)))))
      (setf (logical-pathname-translations "code")
	    (list (list "**;*.*.*" (format () "~A**/*.*" (namestring path-code)))))
      (setf (logical-pathname-translations "api")
	    (list (list "**;*.*.*" (format () "~A**/*.*" (namestring path-api))))))))


(om-api-add-init-func 'set-om-logical-path)


;------

;; handles device, host, etc.
;; dircetory is a pathname
(defun om-make-pathname (&key directory name type host device)
  (make-pathname #+win32 :host 
                 #+win32 (or host 
                             (and device (pathnamep device) (pathname-host device))
                             (and directory (pathnamep directory) (pathname-host directory)))
                 :device 
                 (if device 
                     (if (pathnamep device) (pathname-device device) device)
                   (and directory (pathnamep directory) (pathname-device directory)))
                 :directory (if (pathnamep directory) (pathname-directory directory) directory)
                 :name name :type type))


(defun om-create-file (pathname)
  (with-open-file (file pathname :direction :output :if-does-not-exist :create)))

(defun om-create-directory (pathname &key (if-exists nil))
   (ENSURE-DIRECTORIES-EXIST pathname))

;;; COMPAT
(defun create-file (path) (om-create-file path))
(defun create-directory  (path) (om-create-directory path))
(defun create-folder (path) (om-create-directory path))

;;;
(export '(create-file create-directory create-folder) :om-api)

;------
(defun om-copy-file (sourcepath targetpath &key (if-exists :supersede))
  (handler-bind 
      ((error #'(lambda (err)
                  (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                  (abort err))))
    (when (probe-file targetpath)
      (delete-file targetpath))
    (system::copy-file sourcepath targetpath) ;;:if-exists if-exists
    targetpath
    ))



(defun om-copy-directory (sourcedir targetpath)
  (system::call-system (concatenate 'string "cp -R \"" (namestring sourcedir) "\" \"" 
                                    (namestring targetpath) "\"")))


(defun om-delete-directory (path)
  (if (system::directory-pathname-p path)
      (let ((directories (om-directory  path :directories t :files t)))     
        (loop for item in directories do
              (om-delete-directory item))
        (delete-directory path :no-error)
        t)
    (delete-file path :no-error)))

(defun om-delete-file (name)
  (when (and name (probe-file name))
    (delete-file name :no-error)))

;------
;(defun om-load-file (path)
;  (cl-user::extended-time (cl-user::profile (load path))))

(defun om-load-file (path)
  (load path))

;------

(defvar *volume-sys* nil)

(defun init-volume-var ()
   t)
  
;(eval-when (:load-toplevel :execute)
;  (ccl::def-load-pointers load-volume-var () (init-volume-var)))

(defun real-pathname-name1 (path)
  (let ((type (pathname-type path)))
    (if (or (equal type :UNSPECIFIC) (not type))
      (om-namestring (pathname-name path))
      (concatenate 'string (om-namestring (pathname-name path)) "." type)
      )))

(defun path2unixpath (path)
   path)

(defun om-path2cmdpath (path) 
  (namestring path))
  
(defun corrige-path-space (name) name)

(defun directoryp (p)
  (system::directory-pathname-p p))

(defun systemdirectoryp (p)
  (and (system::directory-pathname-p p)
       (equal (elt (car (last (pathname-directory p))) 0) #\.)))

;(defmacro om-without-interrupts (&body body)
;  `(progn ,.body))

(defmacro om-without-interrupts (&body body)
  (if (>= *lw-version* 6)
      `(mp::allowing-block-interrupts t ,.body)
    `(without-interrupts ,.body)))

;------
(defun om-namestring (path)
  (namestring path))

(defun OM-directory (path &key (type nil) (directories t) (files t) (resolve-aliases nil) (hidden-files nil))
 (let ((rep (directory (namestring path) :link-transparency resolve-aliases)))
   (when (not files)
     (setf rep (remove-if-not 'directoryp rep)))
   (when (not directories)
     (setf rep (remove-if 'directoryp rep)))
   (when (not hidden-files)
     (setf rep (remove-if #'(lambda (item) (or (and (directoryp item) 
                                                   (string-equal (subseq (car (last (pathname-directory item))) 0 1) "."))
                                              (and (stringp (pathname-name item)) 
                                                   (or (= (length (pathname-name item)) 0)
                                                       (string-equal (subseq (pathname-name item) 0 1) ".")))))
                          rep)))
   (when type
     (cond ((stringp type)
            (setf rep (loop for item in rep when (or (directoryp item) (string-equal (pathname-type item) type)) collect item)))
           ((consp type)
            (setf rep (loop for item in rep when (or (directoryp item) 
                                                     (member (pathname-type item) type :test 'string-equal)) collect item)))
           (t nil)))
   rep))
              

(defun om-read-line (file)
  (read-line file nil 'eof))

; enleve les mauvais caracteres à la fin
(defun om-correct-line (line &optional stream) 
  (if (stringp line)
      (if (> (length line) 0)
          (let ((lastchar (elt line (- (length line) 1))))
            (if (or (equal lastchar #\Space)
                    (equal lastchar #\LineFeed))
                (om-correct-line (subseq line 0 (- (length line) 1)))
              line))
        line)
    line))

                 

(defun stream-eofp (s)
  ;(stream::stream-check-eof-no-hang s)
  (let ((c (read-char s nil 'eof)))
    (unless (equal c 'eof) (unread-char c s))
    (equal c 'eof)))



;;;===================
;;; system

(defun om-get-user-name ()
  (system::get-user-name))

(defun om-user-home ()
 (USER-HOMEDIR-PATHNAME))

(defun om-user-pref-folder ()
  (let* ((userhome (om-user-home)))
    (make-pathname
     :device (pathname-device userhome)
     :host (pathname-host userhome) :device (pathname-device userhome)
     :directory 
     #+macosx(append (pathname-directory userhome) (list "Library" "Preferences"))
     #+win32(append (pathname-directory userhome) (list "Application Data"))
     #+linux(append (pathname-directory userhome) (list ".local" "share"))
     )))

(defun om-get-date ()
  (sys::date-string))

;;;===================================
;;; multi-processing


(defvar *current-priority* 1)

(defmacro om-with-priority (priority &body body)
  `(let ((*current-priority* ,priority)) ,@body))

(defmacro om-run-process (name func &rest args)
   `(mp:process-run-function ',name '(:priority ,(or *current-priority* 10))
                             (if (functionp ,func) ,func ',func) ,.args))

(defmacro om-with-new-gc (&body body)
  #+cocoa `(objc:with-autorelease-pool nil ,@body)
  #-cocoa `(progn ,@body))



(defun om-kill-process (process)
   (mp:process-kill process))

(defun om-find-process (id)
  (mp:find-process-from-name id))

(defun om-process-state (process)
  (mp:process-state process))


;;;===================
;;;; external apps 

(defun om-cmd-line (str &optional (redirect-output nil) (wait t) (current-path nil))
  #+(or linux macosx) 
  (when current-path 
    (setf str (concatenate 'string (format nil "cd ~s; " (namestring current-path)) str)))
  (if (and (member *om-os* '(:mac :linux))
	   (pathnamep redirect-output))
      ;;(let ((tempfile "~/om-log.txt"))
      (sys:run-shell-command str :show-window t :wait wait :output redirect-output :error-output redirect-output 
                             :if-output-exists :append :if-error-output-exists :append)
					;(om-open-new-text-file (pathname tempfile)))
      (progn
	(if redirect-output
	    (sys:call-system-showing-output str :wait wait :output-stream *om-stream* 
					    :prefix ":: "
					    #+win32 :current-directory #+win32 current-path)
	    #-win32(sys:run-shell-command str :wait wait)
	    #+win32(sys:call-system str :wait wait  :current-directory current-path)
	    ))))

;(sys::change-directory  (om-make-pathname :directory om::*om-midi-settings-app-path*))
;(hcl::get-working-directory)
; (sys:run-shell-command (format nil "~s" (namestring om::*om-midi-settings-app-path*)))
;(system::call-system (format nil "~s" (namestring om::*om-midi-settings-app-path*)) :wait t)

;;; doit retourner un ID !!
;;; path = un exe ou bien un .app dont on vet executer l'exe

(defun om-run-program (path &optional afterfun)
  (let* ((pathstr (namestring path))
         (dir (om-make-pathname :directory path))
         (name (pathname-name path)))
    (when (equal (elt pathstr (- (length pathstr) 1)) #\/)
      (setf pathstr (subseq pathstr 0 (- (length pathstr) 1)))
      (let ((appname (pathname-name (pathname (subseq pathstr 0 (- (length pathstr) 1))))))
        (setf pathstr (namestring (make-pathname :directory (append (pathname-directory path) 
                                                                    (list "Contents" "MacOS"))
                                                 :name appname)))))
        (mp:process-run-function (namestring path) nil
                                 #'(lambda ()
                                     ;;; (sys::cd dir)
                                     #-win32(system::run-shell-command pathstr :wait t)
                                     #+win32(system::call-system (format nil "~s" (namestring pathstr)) :wait t)
                                     (when afterfun 
                                       (funcall afterfun))))
        (namestring path)))


(defvar *om-open-cmd* #+linux "xdg-open" #+(or win32 cocoa) "open")

(defun om-run-application (path)
  (system::call-system (format nil  "~A ~s" *om-open-cmd* (namestring path)) :wait nil)
  (namestring path))


;;; marche pour un process créé avec la fonction om-run-program ou om-run-application
(defun om-select-program (id)
  (system::call-system (concatenate 'string *om-open-cmd* " " (namestring id))))
