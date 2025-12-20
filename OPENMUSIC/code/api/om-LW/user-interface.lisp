;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;;===========================================================================
;DocFile
; PREDEFINED DIALOGS AND MESSAGES 
;DocFile
;;===========================================================================



(in-package :om-api)

;;;==========
;;; export :
;;;==========
(export '(
                om-get-user-string
                om-choose-directory-dialog
                om-choose-new-directory-dialog
                om-choose-file-dialog
                om-choose-new-file-dialog
                om-y-or-n-dialog
                om-y-n-cancel-dialog
                om-choose-color-dialog
                om-pick-color-view
                om-choose-font-dialog
                om-message-dialog
                om-beep
                ) :om-api)


;;;==========

(defvar *last-directory* nil)

(defun def-dialog-owner ()
 #-cocoa nil
 #+cocoa (capi::convert-to-screen))

;;; ASK USER FOR STRING
(defun om-get-user-string (prompt &key (initial-string "") (window-title "Get string") owner 
                                  (size (om-make-point 365 100))
                                  (position (om-make-point 200 140)))
  (prompt-for-string  prompt :initial-value initial-string))
     
     
;;; CHOOSE A FILE
;(om-choose-file-dialog :prompt "escoja" :types '("All" "*.*"))


(defun om-choose-file-dialog (&key (prompt "Choose a File") (directory nil) (button-string "OK") (types nil))
  (let ((rep (capi::prompt-for-file prompt :filters types :filter (if types (cadr types)) :owner (def-dialog-owner)
                                    :pathname (or directory *last-directory*))))
    (when rep
      (setf *last-directory* (make-pathname :directory (pathname-directory rep))))
    rep))
      


;(export 'om-with-file-dialog-process :om-api)

;(defmacro om-with-file-dialog-process ((file &key (prompt "Choose a File") (directory nil) (button-string "OK") (types nil))
;                                       &body body)
;  `(capi:with-dialog-results (,file ok-p)
;      (capi::prompt-for-file ,prompt ;:filters ',types :filter (if ',types (cadr ',types)) 
;                             :owner ,(def-dialog-owner)
;                             :pathname ,(or directory *last-directory*))
     ;(when ,file
     ;  (setf *last-directory* (make-pathname :directory (pathname-directory ,file))))
;     ,@body))

;;; CHOOSE A NEW FILE
(defun om-choose-new-file-dialog (&key (prompt "Choose a New File") (directory nil) (name "") (button-string "OK") (types nil))
   (declare (ignore name))
   (let* ((dir (or directory *last-directory*))
         (rep (capi::prompt-for-file prompt :filters types :filter (if types (cadr types)) :owner (def-dialog-owner) 
                    :pathname (make-pathname :directory (when dir (pathname-directory dir)) :name name) 
                    :operation :save)))
     (when rep 
       (setf *last-directory* (make-pathname :directory (pathname-directory rep))))
     rep))
     

; (om-choose-new-file-dialog :prompt "escoja" :types '("MIDI Files" "*.mid; *.midi" "SND Files" "*.wav; *.aiff"))


;;; CHOOSE A DIRECTORY
(defun om-choose-directory-dialog (&key (prompt "Choose a Directory") (directory nil))
  (let ((rep (prompt-for-directory prompt :owner (def-dialog-owner) :pathname (or directory *last-directory*))))
    (when rep 
      (setf *last-directory* (make-pathname :directory (pathname-directory rep))))
    rep))

;; (prompt-for-file "HEllo" :filter nil :filters nil :pathname (make-pathname :directory oa::*api-directory*) :operation :save)
;; (setf ppp (om-choose-new-directory-dialog :directory (make-pathname :directory oa::*api-directory*)))


;;; CHOOSE A NEW DIRECTORY
(defun om-choose-new-directory-dialog (&key (prompt "Choose location and name for the new directory") (directory nil) (defname nil))
  (let* ((dir (or directory *last-directory*))
         (def (if dir 
                  (om-make-pathname :directory (pathname-directory dir) :name defname)
                defname))
         (path (prompt-for-file prompt :owner (def-dialog-owner)
                                :filter nil :filters nil :pathname def
                                :operation :save)))
    (when path
      (setf *last-directory* (om-make-pathname :directory path))
      (om-make-pathname :device (pathname-device path) :host (pathname-host path)
                        :directory 
                        #+macosx 
                        (if (not (equal (pathname-type path) :unspecific))
                            (append (pathname-directory path) (list (concatenate 'string (pathname-name path) "." (pathname-type path))))                          
                          (append (pathname-directory path) (list (pathname-name path))))
                        #-macosx  
                        (if (pathname-type path)
                            (append (pathname-directory path) (list (concatenate 'string (pathname-name path) "." (pathname-type path)))) 
                          (append (pathname-directory path) (list (pathname-name path))))
                        )
      )))

;;; YES OR NO DIALOG
(defun om-y-or-n-dialog (message &key (size (om-make-point 300 150)) (default-button nil))
  (capi::prompt-for-confirmation message :default-button (if (equal default-button :yes) :ok nil)))

(defun om-y-n-cancel-dialog (message &key (size (om-make-point 300 150)) (default-button nil))
  (multiple-value-bind (answer successp)
      (capi:prompt-for-confirmation message :cancel-button t 
                                    :default-button (if (equal default-button :yes) :ok nil))
    (if successp answer :cancel)))



;; (om-y-or-n-dialog "message" :default-button :yes)

;;; CHOOSE A COLOR
(defclass om-pick-color-view (om-view)
  ((color :accessor color :initarg :color :initform (color:make-rgb 0 0 0))
   (after-fun :accessor after-fun :initform nil :initarg :after-fun)))

(defmethod om-draw-contents ((self om-pick-color-view))
  (gp::with-graphics-state  (self :thickness 2 :foreground (color::make-rgb 0.2 0.2 0.2))
    (gp:draw-rectangle self 0 0 (- (vw self) 1) (- (vh self) 1) :filled nil)))

(defmethod om-view-click-handler ((self om-pick-color-view) pos)
  (declare (ignore pos))
  (let* ((c (prompt-for-color "Color Chooser")) ; :color (c (color self))))
         (color (make-instance 'omcolor :c (color:make-rgb 
                                            (coerce (color::color-red c) 'single-float) 
                                            (coerce (color::color-green c) 'single-float) 
                                            (coerce (color::color-blue c) 'single-float) 
                                            (coerce (color::color-alpha c) 'single-float)))))
    (om-set-bg-color self color)
    (setf (color self) color)
    (when (after-fun self) (funcall (after-fun self) self))))

#+cocoa
(defun om-choose-color-dialog (&key color)
  (let ((win (om-make-window 'om-dialog :size (om-make-point 240 110)
                             :resize nil :window-title "Choose a New Color..."))
        (col (or color *om-black-color*))
        coloritem)
    (om-add-subviews win 
                     (setf coloritem (om-make-view 'om-pick-color-view
                                       :position (om-make-point 40 20)
                                       :size (om-make-point 60 60)
                                       :color col
                                       :bg-color col))
                     (om-make-dialog-item 'om-button (om-make-point 130 26) (om-make-point 80 24)
                                          "Cancel"
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog win nil)))
                     (om-make-dialog-item 'om-button (om-make-point 130 58) (om-make-point 80 24)
                                          "OK"
                                          :default-button t
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog win (color coloritem)))))
    (om-modal-dialog win)))
        
#-cocoa             
(defun om-choose-color-dialog (&key color)        
  (let ((rep (capi::prompt-for-color  "Choose a color" :color (when color (c color)))))
    (when rep 
      (make-instance 'omcolor :c rep))))

; (om-choose-color-dialog)

;;;MESSAGE
(defun om-message-dialog (message &key (window-title "Warning") owner (size (om-make-point 335 100)) 
                                  (position (om-make-point 200 140)))
  (declare (ignore owner))
  (capi::display-message message))

;;; SYSTEM BEEP
(defun om-beep ()
   (beep-pane nil))

(defun om-choose-font-dialog (&key (font *om-default-font2*))
  (let ((font (capi::prompt-for-font "Choose a font" :font font :owner nil)))
    (and font (gp::font-description font))))

; (om-choose-font-dialog)



