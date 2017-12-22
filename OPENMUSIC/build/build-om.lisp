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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;=========================================================================
;;; OpenMusic build file               
;;; load this file then evaluate the following form :
;;; (om::start-openmusic)
;=========================================================================

(in-package "CL-USER") 

;(set-up-profiler :package :all)
;(set-up-profiler :package :all :call-counter t)

;;; (om::gen-om-reference)

;;;=======================================
(defparameter *app-name* "OM")

(defparameter *version* 6.130002)

(defparameter *beta-release* nil) 
(defparameter *version-str* "")
(defparameter *version-str-full* "")
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
                 (if show-beta
		     (format nil ".~d" beta)
                     "")
                 )))

(setf *version-str* (version-to-string *version* nil *beta-release*))
(setf *version-str-full* (version-to-string *version* t t))

(export '(*app-name* *version* *beta-release* *version-str* *version-str-full* *release-language* *release-date* *release-author*) :cl-user)

;;;=======================================

(defvar *this-file* *load-pathname*)
(defvar *om-src-directory*  
  (make-pathname :directory (butlast (pathname-directory *load-pathname*))
                 :device (pathname-device *load-pathname*) #+win32 :host #+win32 (pathname-host *load-pathname*)))

(defvar *compile-type* "xfasl")
;;; should be : "xfasl" on MacIntel, "nfasl" on MacPPC, "ofasl" on Win32, "ufasl" on 32bit linux ("64ufasl" on 64bit linux)
(setf *compile-type* (pathname-type (cl-user::compile-file-pathname "")))

#+win32(editor::bind-key "Find Source" "Control-." :global :pc)



(defvar *remove-error-fasl* nil)

;;; equivalent to LW'w CURRENT-PATHNAME, allowing other reference path like in om-relative-path
(defun decode-local-path (path &optional (relative-path :current))
  (labels ((string-until-char (string char)
             (let ((index (search char string)))
               (if index (values (subseq string 0 index) (subseq string (+ index 1)))
                 (values string nil))))
           (str2list-path (str)
             (let (list)
               (loop while str do
                 (let ((rep (multiple-value-list (string-until-char str "/"))))
                   (setf str (second rep))
                   (when (first rep) (push (first rep) list))))
               (reverse list))))
    (let ((decoded-path (str2list-path path))
          (ref *load-pathname*))
      (make-pathname
       :host (pathname-host ref) :device (pathname-device ref) 
       :directory (append (pathname-directory ref) (butlast decoded-path))
       :name (car (last decoded-path))))))

(defun compile&load (file &optional (verbose t))
   (let* ((lisp-file (truename (if (pathname-type file) file (concatenate 'string (namestring file) ".lisp"))))
          (fasl-file (probe-file (merge-pathnames (make-pathname :type *compile-type*) lisp-file)))
	  (load-file (make-pathname :directory (pathname-directory file) :name (pathname-name file)))
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
                                                          (load load-file :verbose verbose))
                                                      (progn 
                                                        (print (format nil "FASL error: ~s ..." fasl-file))
                                                        (when *remove-error-fasl* (delete-file fasl-file nil))
                                                        (load lisp-file :verbose verbose)))
                                                    (throw 'faslerror t)
                                                    )))
           (load load-file :verbose verbose)
           )))))

(export 'compile&load :cl-user)


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
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*))))))
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

(defun clean-sources (&optional dir keep-fasl-types)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*)))))
        (ext-list (remove-if 
                   #'(lambda (ext) (find ext keep-fasl-types :test 'string-equal))
                   '("xfasl" "64xfasl" "fasl" "DS_STORE" "nfasl" "ofasl" "ufasl" "lisp~"))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (clean-sources file keep-fasl-types)
                (when (and (pathname-type file)
                           (or (find (pathname-type file) ext-list :test 'string-equal)
			       ;;; (string= (pathname-type file) *compile-type*) ; remove compiled files ; why ?
                               )) 
                  (print (concatenate 'string "Deleting " (namestring file) " ..."))
                  (delete-file file)
                  )))
          (directory (namestring src-root) :directories t))
    ))


; (clean-sources)
; (clean-sources (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 4) '("omlibraries"))))
; (clean-sources (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 4) '("OTHER-LIBS" "OM-SoX 1.0b7"))))

; does not count blank lines and Lisp comments
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
        (src-root (or dir (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) '("code")))))
        (types '("lisp")))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (let ((count (count-sources file)))
                    (setf nfiles (+ nfiles (car count)))
                    (setf nlines (+ nlines (caDr count))))
                (when (and (pathname-type file)
                           (find (pathname-type file) types :test 'string-equal))
                  (setf nfiles (+ nfiles 1))
                  (setf nlines (+ nlines (count-lines file)))
                  )
                  ))
          (directory (namestring src-root) :directories t))
    (list nfiles nlines)
    ))

; (setf *folder* (om-api:om-choose-directory-dialog))
; (count-sources *folder*)
; ==> 444 files,  132219 lines of code,  183377 lines

; zarbi : (directory dir) different avec ou sans namestring si dir = (truename "cl:")
; (directory (make-pathname :directory "Users"))

; (clean-sources (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) '("libraries"))))
; (clean-sources (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3) '("omlibraries"))))

;;;; LOAD OM

(load (make-pathname :directory (append (pathname-directory *om-src-directory*) '("code" "api" "om-LW"))
                       :name "load-api" :type "lisp"))

(load (make-pathname :directory (append (pathname-directory *om-src-directory*) '("code" "api" "foreign-interface")) :name "foreign-interface"))

(load (make-pathname :directory (append (pathname-directory *om-src-directory*) '("code" "api" "externals"))
                     :name "externals" :type "lisp"))

(oa::load-external-libs '(:midi :portmidi :xml :udp :osc :opengl :json :yason :svg))
;; #+linux (oa::load-external-libs '(:jack))



(defpackage "OpenMusic"
    (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP")
    (:import-from "CL-USER")
    (:nicknames "OM" "OPENMUSIC"))


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
  
(load-om-projects '("basicproject" "musicproject"))
(load-om-projects '("midi"))
(load-om-projects '("sdif"))
(load-om-projects '("mathtools"))
(load-om-projects '("omsounds"))
(load-om-projects '("space"))
(load-om-projects '("harmonicproject"))
(load-om-projects '("sheet"))


(push :om *features*)

(defvar om::*om-startup* nil)


(defun start-openmusic ()
  (setf om::*om-startup* t)
  (oa::om-api-init)
  (om-lisp::set-om-debugger)
  (om::load-modif-patches)
  #+cocoa(objc:make-autorelease-pool)
  (clos::set-clos-initarg-checking nil)
  (editor:setup-indent "defmethod!" 2 2 2)
  (editor:setup-indent "defmethod*" 2 2 2)  
  (init-root-definition-pathname cl-user::*om-src-directory* om::*om-root*)

  (setf *print-case* :downcase)
  (setf *msg-error-label-on* nil)
  (in-package :om)
  (om::set-language *release-language*)
  (oa::om-init-funcall)
    
  #+(or linux win32) (define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)
  (om::show-workspaces-dialog)
  (when om::*om-workspace-win* (capi::execute-with-interface om::*om-workspace-win* #'(lambda () (in-package :om))))
  (setf om::*om-startup* nil)
  )

(defun cl-user::start-openmusic () (start-openmusic))

(defun quit-om-callback () 
  (let ((rep (oa::om-y-n-cancel-dialog (format nil "Quitting OM... ~%Do you want to save your workspace?") :default-button :yes)))
    (if (equal rep :cancel) NIL  ;;; don't quit !
      (progn 
        (when rep
          (oa::om-exit-funcall)
          ;; (om-lisp::check-buffers-before-close)
          (om::save-ws-contents)
          (om::save-preferences)
        #+macosx(mapcar 'oa::om-close-window (oa::om-get-all-windows 'capi::interface))
        )
        T) ;;; quit !
      )))

;(quit-om-callback)
;(defun om-stop-scheduler () nil)

;; (gen-lib-reference (find-library "OM-Spat"))
;;; avant de faire un package :
;; (om::load-all-om-libs)

;;; (start-openmusic)
;;; (om::show-workspace-win)
;;; (om::gen-om-reference)
;;; (om::show-about-win)

;;; WINDOWS :
;;; INSTALL OM FONTS IN C:/WINDOWS/Fonts/
;;; LIBS in C:/WINDOWS/   : 
;;   mshare32.dll player32.dll msmmsystem.dll msmmsystem.ini midishare.ini
;;   libaudiostream.dll libsndfile.dll libsamplerate.dll
;;   sdif.dll

;;; MAC :
;;; INSTALL OM FONTS IN /Library/Fonts/



