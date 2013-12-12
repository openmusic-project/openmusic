;;; OpenMusic build file               
;;; load this file then evaluate the following form :
;;; (om::start-openmusic)
;;;
(in-package "CL-USER") 

;(set-up-profiler :package :all)
;(set-up-profiler :package :all :call-counter t)

;;; (om::gen-om-reference)

;;;=======================================
(defparameter *app-name* "OM")
(defparameter *version* 6.070101)
(defparameter *beta-release* t)
(defparameter *version-str* "")
(defparameter *release-language* :en)
(defparameter *release-date* (subseq (sys::date-string nil nil) 0 10))
(defparameter *release-author* "j. bresson")

(defun version-to-string (num &optional (full nil) (show-beta nil))
  (let* ((str (format nil "~,6f" num))
         (pos (position #\. str))
         (v (read-from-string (subseq str 0 pos)))
         (rest (subseq str (+ 1 pos)))
         (v2 (read-from-string (subseq rest 0 2)))
         (v3 (read-from-string (subseq rest 2 (min 4 (length rest)))))
         (beta (if (> (length rest) 4) (read-from-string (subseq rest 4)))))
    (concatenate 'string (format nil "~d.~d" v v2) 
                 (if (and (zerop v3) (null full)) "" (format nil ".~d" v3))
                 (if (or (null show-beta) (zerop beta)) ""
		     #-linux (format nil " beta ~d" beta)
		     #+linux (format nil "_beta_~d" beta))
                 )))

(setf *version-str* (version-to-string *version* nil *beta-release*))

(export '(*app-name* *version* *beta-release* *version-str* *release-language* *release-date* *release-author*) :cl-user)

;;;=======================================

(defvar *this-file* *load-pathname*)
(defvar *om-src-directory*  
  (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)
                 :device (pathname-device *load-pathname*) #+win32 :host #+win32 (pathname-host *load-pathname*)))

(defvar *compile-type* "xfasl")
;;; should be : "xfasl" on MacIntel, "nfasl" on MacPPC, "ofasl" on Win32, "ufasl" on 32bit linux ("64ufasl" on 64bit linux)
(setf *compile-type* (pathname-type (cl-user::compile-file-pathname "")))

#+win32(editor::bind-key "Find Source" "Control-." :global :pc)



(defvar *remove-error-fasl* nil)

(defun compile&load (file &optional (verbose t))
   (let* ((lisp-file (truename (if (pathname-type file) file (concatenate 'string (namestring file) ".lisp"))))
          (fasl-file (probe-file (make-pathname :directory (pathname-directory lisp-file)
                                                :device (pathname-device lisp-file)
                                                :name (pathname-name lisp-file) :type *compile-type*)))
          (fasl-outofdate (and fasl-file
                               (or (not (file-write-date lisp-file))
                                   (not (file-write-date fasl-file))
                                   (> (file-write-date lisp-file) (file-write-date fasl-file))))))
     (when (and (not (member :om-deliver *features*))
                (or (not fasl-file) fasl-outofdate))
       (compile-file file :verbose verbose)
       (setf fasl-outofdate nil))
     (if fasl-outofdate
         (progn (print (format nil "WARNING: File ~A is older than the LISP source file. File ~A will be loaded instead."
                               fasl-file lisp-file))
           (load lisp-file :verbose verbose))
       (catch 'faslerror
         (handler-bind ((conditions::fasl-error #'(lambda (c) 
                                                    (if (and nil (fboundp 'compile-file) fasl-file)
                                                        (progn 
                                                          (print (format nil "File ~s will be recompiled..." fasl-file))
                                                          (compile-file file :verbose verbose)
                                                          (load file :verbose verbose))
                                                      (progn 
                                                        (print (format nil "FASL error: ~s ..." fasl-file))
                                                        (when *remove-error-fasl* (delete-file fasl-file nil))
                                                        (load lisp-file :verbose verbose)))
                                                    (throw 'faslerror t)
                                                    )))
           (load file :verbose verbose)
           )))))

(export 'compile&load :cl-user)


(defun set-cl-logical-path ()
   (let* ((root-path  (make-pathname :device (pathname-device *om-src-directory*)
                                     :directory (pathname-directory *om-src-directory*)))
	  (path-code  (make-pathname :device (pathname-device root-path)
				     :directory (append (pathname-directory root-path) (list "CODE"))))
	  (path-api  (make-pathname :device (pathname-device root-path)
                                    :directory (append (pathname-directory root-path) (list "CODE" "API" "OM-LW")))))
     (print (format () "~A**;*.*" (namestring root-path)))
     (setf (logical-pathname-translations "CL")
	   (list (list "**;*.*.*" (format () "~A**/*.*" (namestring root-path)))))
     (setf (logical-pathname-translations "code")
	   (list (list "**;*.*.*" (format () "~A**/*.*" (namestring path-code)))))
     (setf (logical-pathname-translations "api")
	   (list (list "**;*.*.*" (format () "~A**/*.*" (namestring path-api)))))
     (print (list  "--> logical path translation" (truename "cl:")))
     ))

(set-cl-logical-path)


;;; TEMP -- BUG LISPWORKS
; (trace (error :backtrace :bug-form :trace-output *terminal-io*))
;(editor:defcommand "Buffer List To File" (p)
;  ""
;  (with-open-file (out "~/LispWorks-Buffer-List.txt"
;                       :direction :output
;                       :if-exists :supersede)
;    (print editor::*buffer-list* out)))
;(editor:bind-key "Buffer List To File" #("control-c" "z"))
;;; END

(defun clean-svn (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))))
    (mapc #'(lambda (file) 
             
              (if (system::directory-pathname-p file)
                  (if (string-equal ".svn" (car (last (pathname-directory file))))
                      (system::call-system (concatenate 'string "rm -Rf \"" (namestring file) "\""))
                    (clean-svn file))
                (when (and (pathname-type file)
                           (find (pathname-type file) '("lisp~" "DS_STORE" "docx") :test 'string-equal))
                  (delete-file file))
                ))
          (directory (namestring src-root) :directories t))))
              
; (clean-svn (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3) '("OM-FORUM-LIBRARIES" "OMChroma"))))

(defun clean-sources (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (clean-sources file)
                (when (and (pathname-type file)
                           (or (find (pathname-type file) '("xfasl" "fasl" "DS_STORE" "nfasl" "ofasl" "ufasl" "lisp~") :test 'string-equal)
			       (string= (pathname-type file) *compile-type*))) ; remove compiled files
                  (print (concatenate 'string "Deleting " (namestring file) " ..."))
                  (delete-file file)
                  )))
          (directory (namestring src-root) :directories t))
    ))


; (clean-sources)
; (clean-sources (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3) '("LIBRARIES" "OM-FORUM-LIBRARIES"))))


(defun count-lines (file)
  (flet ((delete-spaces (string)
           (let ((pos (position-if #'(lambda (x) (not (member x (list #\Linefeed #\Space #\Tab) :test 'equal))) string)))
             (if pos (subseq string pos) ""))))
    (let ((n 0))
      (with-open-file (f file :direction :input)
        (let ((line (read-line f nil 'eof)))
          (loop while (and line (not (equal line 'eof))) do
                (unless (or (string-equal (delete-spaces line) "")
                            (equal (elt (delete-spaces line) 0) #\;))
                  (setf n (+ n 1)))
                (setf line (read-line f nil 'eof))
                )))
      n)))


(defun count-sources (&optional dir)
  (let ((nfiles 0)
        (nlines 0)
        (src-root (or dir (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2) '("code"))))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (let ((count (count-sources file)))
                    (setf nfiles (+ nfiles (car count)))
                    (setf nlines (+ nlines (caDr count))))
                (when (and (pathname-type file)
                           (string-equal (pathname-type file) "lisp"))
                  (setf nfiles (+ nfiles 1))
                  (setf nlines (+ nlines (count-lines file)))
                  )
                  ))
          (directory (namestring src-root) :directories t))
    (list nfiles nlines)
    ))

; (count-sources)
; ==> 444 files,  132219 lines of code,  183377 lines

; zarbi : (directory dir) different avec ou sans namestring si dir = (truename "cl:")
; (directory (make-pathname :directory "Users"))

(defparameter *om-libs* (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2) '("libraries"))))
(defparameter *ircam-libs* (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3) '("LIBRARIES" "IRCAM-LIBS"))))

; (clean-sources *om-libs*)
; (clean-sources *ircam-libs*)

;;;; LOAD OM

(load (make-pathname :directory (append (pathname-directory *om-src-directory*) '("code" "api" "om-LW"))
                       :name "load-api" :type "lisp"))

(load (make-pathname :directory (append (pathname-directory *om-src-directory*) '("code" "api" "externals"))
                     :name "externals" :type "lisp"))


(oa::load-external-libs '(:midi #-linux :midishare :audio :xml :sdif :udp :osc :opengl :json :yason #+linux :jack #+linux :fluidsynth))
;(oa::load-om-libs '(:osc))

(defpackage "OpenMusic"
    (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP")
    (:import-from "CL-USER")
    (:nicknames "OM"))


(in-package :om)

(defun load-om-kernel ()
  (load (make-pathname :directory (append (pathname-directory cl-user::*om-src-directory*) '("code" "kernel"))
                       :name "kernelfiles" :type "lisp")))

(defun load-om-projects (&optional projects)
  (let ((project-dir (make-pathname :directory (append (pathname-directory cl-user::*om-src-directory*) '("code" "projects"))))
        (projects-list (if projects projects (oa::om-directory project-dir
                                              :files nil :directories t))))
    (mapc #'(lambda (x) 
              (let ((file (if (numberp (read-from-string (subseq x 0 2))) (subseq x 3) x))) 
                (load (make-pathname :directory (append (pathname-directory project-dir) (list x)) :name file :type "lisp") :if-does-not-exist nil)))
          projects-list)))



(load-om-kernel)
  
(load-om-projects '("01-basicproject" "02-musicproject"))
(load-om-projects '("03-midi"))
(load-om-projects '("05-mathtools"))
(load-om-projects '("06-omsounds"))
(load-om-projects '("07-omspace"))
(load-om-projects '("09-harmonicproject"))
(load-om-projects '("10-sheet"))



(push :om *features*)

(defvar om::*om-startup* nil)

(defun start-openmusic ()
  (setf om::*om-startup* t)
  (oa::om-api-init)
  (om-lisp::set-om-debugger)
  (om::load-modif-patches)
  #+cocoa(objc:make-autorelease-pool)
  (clos::set-clos-initarg-checking nil)
  (setf *print-case* :downcase)
  (setf *msg-error-label-on* nil)
  (in-package :om)
  (om::set-language *release-language*)
  (oa::om-init-funcall)
    
  #+(or linux win32) (define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)
  
  ;(oa::om-make-new-listener :initial-lambda #'(lambda () (in-package :om)))
  
  (om::show-workspaces-dialog)
  
  ;(om::gen-reference om::*om-ref-entries* om::*om-reference-dir*)

  (capi::execute-with-interface 
   om::*om-workspace-win* 
   #'(lambda () (in-package :om)))
  
  (setf om::*om-startup* nil)
  )

(defun quit-om-callback () 
  (let ((rep (capi:prompt-for-confirmation "Quit OpenMusic ?" :cancel-button nil :default-button :ok)))
    (when rep
      (setf rep (om-lisp::check-buffers-before-close)))
    (when rep 
      (oa::om-exit-funcall)
      (om::save-before-quit)
      #+macosx(mapcar 'oa::om-close-window (oa::om-get-all-windows 'capi::interface))
      )
    rep))

;(quit-om-callback)
;(defun om-stop-scheduler () nil)

;; (gen-lib-reference (find-library "OM-Spat"))
;;; avant de faire un package :
;; (om::load-all-om-libs)

;;; (start-openmusic)
;;; (om::show-workspace-win)
;;; (om::gen-om-reference)

;;; WINDOWS :
;;; INSTALL OM FONTS IN C:/WINDOWS/Fonts/
;;; LIBS in C:/WINDOWS/   : 
;;   mshare32.dll player32.dll msmmsystem.dll msmmsystem.ini midishare.ini
;;   libaudiostream.dll libsndfile.dll
;;   sdif.dll

;;; MAC :
;;; INSTALL OM FONTS IN /Library/Fonts/
;;; Run MidiShare installer
;;; LIBS in /Library/Frameworks   : 
;;; LibAudioStream.frameworks SDIF.framework


