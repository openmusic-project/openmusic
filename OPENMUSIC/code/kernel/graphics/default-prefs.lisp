;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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

;DocFile
; Default OM preferences
;DocFile


(in-package :om)

;=================================================
;DEFAULT PREFERENCES MODULE  icon 214
;=================================================

(defvar *composer-name* nil)
(setf *composer-name* "Guarigocha")

(defvar *msg-error-label-on* t)
; (setf *msg-error-label-on* nil)

(defvar *eval-process* :on)
; (setf *eval-process* nil)

(defvar *reactive-patches* nil)

(defvar *listener-input* nil)

(defmethod get-def-vals ((iconID (eql :general)))
   (list :handle-errors t 
         :user-name "Guarigocha" 
         :eval-process :on
         :listener-on-top :no
         :listener-input nil
         :reactive nil
         :out-files-dir (check-folder (make-pathname :device (pathname-device (mypathname *current-workspace*)) 
                                                     :host (pathname-host (mypathname *current-workspace*))
                                                    :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                        (list "out-files"))))
         :tmp-files-dir (check-folder (make-pathname :device (pathname-device (mypathname *current-workspace*)) 
                                                    :host (pathname-host (mypathname *current-workspace*))
                                                    :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                       (list "out-files"))))
         :in-files-dir (check-folder (make-pathname :device (pathname-device (mypathname *current-workspace*)) 
                                                    :host (pathname-host (mypathname *current-workspace*))
                                                    :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                       (list "in-files"))))))


; (find-pref-module :general)

(defmethod put-preferences ((iconID (eql :general)))
   (let ((modulepref (find-pref-module iconID)))
     (setf *msg-error-label-on* (get-pref modulepref :handle-errors))
     (when (get-pref modulepref :eval-process) 
       (setf *eval-process* (get-pref modulepref :eval-process))
       (om-set-eval-process (equal *eval-process* :on)))

     (setf *reactive-patches* (get-pref modulepref :reactive))
     
     (unless (equal *listener-input* (get-pref modulepref :listener-input))
       (setf *listener-input* (get-pref modulepref :listener-input))
       (when om-lisp::*om-listener* (om-close-window om-lisp::*om-listener*))
       (unless om::*om-startup* (show-listener-win)))

     (when (get-pref modulepref :listener-on-top) 
       (unless (equal om-lisp::*listener-on-top* (equal (get-pref modulepref :listener-on-top) :yes))
         (om-close-window om-lisp::*om-listener*)
         (setf om-lisp::*listener-on-top* (equal (get-pref modulepref :listener-on-top) :yes))
         (show-listener-win)))
    
     (setf *composer-name* (get-pref modulepref :user-name))

     (if (probe-file (get-pref modulepref :out-files-dir))
       (setf *om-outfiles-folder* (get-pref modulepref :out-files-dir))
       (push :out-files-dir *restore-defaults*)
       )
     (if (probe-file (get-pref modulepref :tmp-files-dir))
       (setf *om-tmpfiles-folder* (get-pref modulepref :tmp-files-dir))
       (push :tmp-files-dir *restore-defaults*)
       )
     (if (probe-file (get-pref modulepref :in-files-dir))
       (setf *om-infiles-folder* (get-pref modulepref :in-files-dir))
       (push :tmp-files-dir *restore-defaults*)
       )    
     ))

(defmethod save-pref-module ((iconID (eql :general)) item)
   (list iconID `(list :handle-errors ,*msg-error-label-on* 
                       :user-name ,*composer-name* 
                       :eval-process ,*eval-process*
                       :reactive ,*reactive-patches*
                       :listener-input ,*listener-input*
                       :listener-on-top ,(if om-lisp::*listener-on-top* :yes :no)
                       :out-files-dir ,(om-save-pathname *om-outfiles-folder*)
                       :tmp-files-dir ,(om-save-pathname *om-tmpfiles-folder*)
                       :in-files-dir ,(om-save-pathname *om-infiles-folder*)
                       ) *om-version*))



(defmethod make-new-pref-scroll  ((num (eql :general)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                  :pref-id num
                                  :name "General"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 0 0)
                                 :font *controls-font* 
                                 ;:scrollbars :v 
                                 ;:retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                 ))
        (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2))
        (posy 0)
        outtxt tmptxt intxt)
    
    (om-add-subviews thescroll 
                     ;(om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 5)) (om-make-point 200 30) "General"
                     ;                      :font *om-default-font4b*)
                     

                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 30)) (om-make-point 90 24) "User Name"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 100) posy)
                                          (om-make-point 200 15)
                                          (get-pref modulepref :user-name)
                                          :after-action (om-dialog-item-act item 
                                                          (set-pref modulepref :user-name (om-dialog-item-text item)))
                                          :font *controls-font*
                                          )
                     
                     (om-make-dialog-item 'om-check-box (om-make-point l1 (incf posy 40)) (om-make-point 180 15) " Handle Error Messages" 
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :handle-errors (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :handle-errors))

                     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy 20)) (om-make-point 330 40) "(Catch Lisp erros and display a simple message window)"
                                          :font *om-default-font1*)
                     
                     (om-make-dialog-item 'om-check-box (om-make-point l1 (incf posy 30)) (om-make-point 200 15) " Enable Evaluation Process" 
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :eval-process (if (om-checked-p item) :on :off)))
                                          :font *controls-font*
                                          :checked-p (equal :on (get-pref modulepref :eval-process)))

                     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy 20)) (om-make-point 330 40) "(Evaluate visual programs on a spearate process)"
                                          :font *om-default-font1*)

                     (om-make-dialog-item 'om-check-box (om-make-point l1 (incf posy 30)) (om-make-point 200 15) " Keep Listener in Front" 
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :listener-on-top (if (om-checked-p item) :yes :no)))
                                          :font *controls-font*
                                          :checked-p (equal :yes (get-pref modulepref :listener-on-top)))
                     
                     (om-make-dialog-item 'om-check-box (om-make-point l1 (incf posy 30)) (om-make-point 200 15) " Enable Listener input" 
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :listener-input (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :listener-input))
                     
                     (om-make-dialog-item 'om-check-box (om-make-point l1 (incf posy 30)) (om-make-point 200 15) " Enable reactivity" 
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :reactive (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :reactive))

                     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy 20)) (om-make-point 330 40) "(push 'r' to set boxes active/unactive)"
                                          :font *om-default-font1*)
                     )

    (om-add-subviews thescroll
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (setf posy 50)) (om-make-point 200 30) "Default Folders"
                                          :font *om-default-font2b*)

                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 30)) (om-make-point 80 22) "Output Files:"
                                          :font *controls-font*)
                     (setf outtxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                       (om-namestring (get-pref modulepref :out-files-dir))
                                          :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                                      :icon1 "folder" :icon2 "folder-pushed"
                                                      :position (om-make-point (+ l2 310) (- posy 5)) :size (om-make-point 26 25) 
                                                      :action (om-dialog-item-act item
                                                                         (declare (ignore item))
                                                                         (let ((newfolder (om-choose-directory-dialog :directory
                                                                                                                      (get-pref modulepref :out-files-dir))))
                                                                           (when newfolder
                                                                             (om-set-dialog-item-text outtxt (om-namestring newfolder))
                                                                             (set-pref modulepref :out-files-dir newfolder)))))
                     
                      
                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 40)) (om-make-point 80 22) "Input Files:" :font *controls-font*)
                     (setf intxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                      (om-namestring (get-pref modulepref :in-files-dir))
                                                      :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point (+ l2 310) (- posy 5)) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((newfolder (om-choose-directory-dialog :directory (get-pref modulepref :in-files-dir))))
                                               (when newfolder
                                                 (om-set-dialog-item-text intxt (om-namestring newfolder))
                                                 (set-pref modulepref :in-files-dir newfolder)))))
                      

                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 40)) (om-make-point 80 22) "Temp Files:" :font *controls-font*)
                     (setf tmptxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                       (om-namestring (get-pref modulepref :tmp-files-dir))
                                          :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point (+ l2 310) (- posy 5)) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (let ((newfolder (om-choose-directory-dialog :directory (get-pref modulepref :tmp-files-dir))))
                                                         (when newfolder
                                                           (om-set-dialog-item-text tmptxt (om-namestring newfolder))
                                                          (set-pref modulepref :tmp-files-dir newfolder)))))
                     )
          
    thescroll))





;;;;=================================================
;;;; APPEARANCE PREFERENCES MODULE  icon 201
;;;;=================================================


(defvar *comment-style* nil)
(setf *comment-style* *om-default-font2*)

(defvar *comment-color* nil)
(defvar *ws-color* nil)

(defvar *default-folder-pres* 0)

(defvar *patch-show-win-buttons* t)
(defvar *curved-connections* nil)

;;; ??
(defun init-om-pref-color ()
  (setf *comment-color* *om-black-color*)
  (setf *ws-color* *workspace-color*))

(om-add-init-func 'init-om-pref-color) 



(defmethod put-preferences ((iconID (eql :appearance)))
  (let ((modulepref (find-pref-module iconID)))
    (setf *comment-style*      (eval (get-pref modulepref :comment-font)))
    (setf *comment-color*      (eval (get-pref modulepref :comment-color)))
    (setf *ws-color*         (get-pref modulepref :ws-color)) ;; eval ?
    (when *om-workspace-win* (om-set-bg-color (panel *om-workspace-win*) *ws-color*))
    (setf *default-folder-pres*     (get-pref modulepref :folder-pres))
    (setf *icon-size-factor* (get-pref modulepref :box-fact))
    (setf *ombox-font* (or (eval (get-pref modulepref :boxname-font)) *om-default-font1*))
    (setf *patch-show-win-buttons* (get-pref modulepref :patch-win-buttons))
    (setf *curved-connections* (get-pref modulepref :curved-connections))
    (setf *miniview-font-size* (get-pref modulepref :mv-font-size))
    
    ;;; maquette
    (when (om-color-p (get-pref modulepref :maq-color))
      (setf *maq-color* (get-pref modulepref :maq-color)))
    (when (om-color-p (get-pref modulepref :patchtemp-color))
      (setf *patch-box-color* (get-pref modulepref :patchtemp-color)))
    (when (om-color-p (get-pref modulepref :maqtemp-color))
      (setf *maquette-box-color* (get-pref modulepref :maqtemp-color)))
    (when (om-color-p (get-pref modulepref :objtemp-color))
      (setf *obj-box-color* (get-pref modulepref :objtemp-color)))
    (setf *minipict-bg* (get-pref modulepref :temp-minipict-bg))
    (setf *minipict-mode* (get-pref modulepref :temp-minipict-mode))
    
    (setf *maq-show-icons* (get-pref modulepref :temp-show-icons))
    t))


(defmethod get-def-vals ((iconID (eql :appearance)))
  (list 
   :comment-font *om-default-font2* 
   :comment-color (om-make-color 0 0 0)
   :ws-color *workspace-color* :box-fact 1 
   :boxname-font *om-default-font1*
   :folder-pres 1
   :patch-win-buttons t
   :curved-connections nil
   :mv-font-size 20

   :maq-color (om-make-color 0.85 0.85 0.83) 
   :patchtemp-color (om-make-color 0.5 0.5 0.6)
   :maqtemp-color (om-make-color 0.6 0.5 0.5)
   :objtemp-color (om-make-color 0.5 0.6 0.5)
   :temp-minipict-bg nil
   :temp-minipict-mode :score
   :temp-show-icons t
))
  


(defmethod make-new-pref-scroll ((num (eql :appearance)) modulepref)
   (let ((thescroll (om-make-view 'preference-pane
                                  :pref-id num
                                  :name " Appearance "
                                  :size (get-pref-scroll-size)
                                  :position (om-make-point 0 0)
                                  :scrollbars nil 
                                  :retain-scrollbars t
                                  :bg-color *om-light-gray-color*))
         (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2))
         (posy 0)
         test-wscolor test-comment boxfont)
     (om-add-subviews thescroll       
                      
                      (om-make-dialog-item 'om-static-text (om-make-point l1 (setf posy 20)) (om-make-point 200 30) "Workspace / Folders"
                                           :font *om-default-font2b*)
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 25)) (om-make-point 120 24) "Workspace Color"
                                          :font *controls-font*)
                     
                     (om-make-view 'om-color-view 
                                   :position (om-make-point (+ l1 170) posy) :size (om-make-point 60 25) 
                                   :bg-color (get-pref modulepref :ws-color)
                                   :color (get-pref modulepref :ws-color)
                                   :after-fun #'(lambda (item) (set-pref modulepref :ws-color (color item))))

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 30)) (om-make-point 120 24) "Default Presentation"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l1 170) posy) (om-make-point 80 24) ""
                                          :range '("icons" "list")
                                          :value (if (= 0 *default-folder-pres*) "icons" "list")
					  :di-action (om-dialog-item-act item 
                                                        (let ((choice (om-get-selected-item item)))
                                                          (set-pref modulepref :folder-pres
                                                              (if (string-equal choice "icons") 0 1)))))
       
                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 40)) (om-make-point 90 24) "Patches"
                                          :font *om-default-font2b*)        
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 25)) (om-make-point 200 26) "Show Input/Output buttons"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point (+ l1 220) posy) (om-make-point 200 26) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :patch-win-buttons)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :patch-win-buttons (om-checked-p item))))
                                                       
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 25)) (om-make-point 200 26) "Curved connections"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point (+ l1 220) posy) (om-make-point 200 26) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :curved-connections)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :curved-connections (om-checked-p item)))) 
                                                       
  
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 30)) (om-make-point 90 24) "Boxes"
                                          :font *om-default-font1b*)
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 25)) (om-make-point 70 26) "Size"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l1 60) posy) (om-make-point 80 24) ""
                                          :range '("50%" "75%" "100%" "150%" "200%")
                                          :value (cond ((<= (get-pref modulepref :box-fact) 0.5) "50%")
                                                       ((< (get-pref modulepref :box-fact) 0.8) "75%")
                                                       ((>= (get-pref modulepref :box-fact) 2) "200%")
                                                       ((>= (get-pref modulepref :box-fact) 1.5) "150%")
                                                       (t "100%"))
                                          :di-action (om-dialog-item-act item 
                                                        (let ((choice (om-get-selected-item item)))
                                                          (set-pref modulepref :box-fact
                                                              (cond ((string-equal choice "50%") 0.5)
                                                                    ((string-equal choice "75%") 0.75)
                                                                    ((string-equal choice "150%") 1.5)
                                                                    ((string-equal choice "200%") 2)
                                                                    (t 1))))))
                     
                     (om-make-dialog-item 'om-button (om-make-point (+ l1 150) posy) (om-make-point 105 24) "Name Font"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore button))
                                                       (let* ((font (om-choose-font-dialog :font (get-pref modulepref :boxname-font))))
                                                         (when font
                                                           (set-pref modulepref :boxname-font font)
                                                           (om-set-font boxfont font)
                                                           (om-invalidate-view thescroll)))))

                     (setf boxfont (om-make-dialog-item 'om-static-text (om-make-point (+ l1 260) posy) (om-make-point 70 24) "mybox"
                                          :font (get-pref modulepref :boxname-font) :bg-color *om-white-color*))
                     
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 20) (incf posy 35)) (om-make-point 90 24) "Comments"
                                          :font *om-default-font1b*)
                     
                     (setf test-comment (om-make-dialog-item 'om-static-text (om-make-point (+ l1 40) (incf posy 25)) 
                                                             (om-make-point 100 20) "My comment..."
                                           :font (get-pref modulepref :comment-font)
                                           :bg-color *om-white-color*
                                           :fg-color (get-pref modulepref :comment-color)))
                     
                     (om-make-dialog-item 'om-button (om-make-point (+ l1 150) (- posy 5)) (om-make-point 70 24) "Font"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore button))
                                                       (let* ((font (om-choose-font-dialog :font (get-pref modulepref :comment-font))))
                                                         (when font
                                                           (set-pref modulepref :comment-font font)
                                                           (om-set-font test-comment font)
                                                           (om-invalidate-view thescroll)))))

                     (om-make-dialog-item 'om-button (om-make-point (+ l1 220) (- posy 5)) (om-make-point 70 24) "Color"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore button))
                                                       (let* ((newcolor (om-choose-color-dialog :color (get-pref modulepref :comment-color))))
                                                         (when newcolor
                                                           (set-pref modulepref :comment-color newcolor)
                                                           (om-set-fg-color test-comment newcolor)
                                                           (om-invalidate-view thescroll)))))
                     )   

     (om-add-subviews thescroll
                      
                      (om-make-dialog-item 'om-static-text (om-make-point l2 (setf posy 20)) (om-make-point 330 30) "MiniViews"
                                          :font *om-default-font2b*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) (incf posy 25)) (om-make-point 250 22) "Font Size:"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 120) posy) (om-make-point 60 24) ""
                                          :range '("8" "10" "12" "14" "16" "18" "20" "24")
                                          :value (number-to-string (get-pref modulepref :mv-font-size))
                                          :di-action (om-dialog-item-act item 
                                                          (set-pref modulepref :mv-font-size (read-from-string (om-get-selected-item item)))))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy 40)) (om-make-point 330 30) "Maquette"
                                          :font *om-default-font2b*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) (incf posy 30)) (om-make-point 250 22) "Default Background Color:"
                                          :font *controls-font*)
                
                     (om-make-view 'om-color-view 
                                   :position (om-make-point (+ l2 180) posy) :size (om-make-point 55 25) 
                                   :bg-color (get-pref modulepref :maq-color)
                                   :color (get-pref modulepref :maq-color)
                                   :after-fun #'(lambda (item) (set-pref modulepref :maq-color (color item))))

                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) (incf posy 30)) (om-make-point 250 22) "Temporal Boxes Color:"
                                           :font *controls-font*)

                     (om-make-view 'om-color-view 
                                   :position (om-make-point (+ l2 180) posy) :size (om-make-point 55 25) 
                                   :bg-color (get-pref modulepref :patchtemp-color)
                                   :color (get-pref modulepref :patchtemp-color)
                                   :after-fun #'(lambda (item) (set-pref modulepref :patchtemp-color (color item))))
              
                     (om-make-view 'om-color-view 
                                   :position (om-make-point (+ l2 240) posy) :size (om-make-point 55 25) 
                                   :bg-color (get-pref modulepref :maqtemp-color)
                                   :color (get-pref modulepref :maqtemp-color)
                                   :after-fun #'(lambda (item) (set-pref modulepref :maqtemp-color (color item))))

                     (om-make-view 'om-color-view 
                                   :position (om-make-point (+ l2 300) posy) :size (om-make-point 55 25) 
                                   :bg-color (get-pref modulepref :objtemp-color)
                                   :color (get-pref modulepref :objtemp-color)
                                   :after-fun #'(lambda (item) (set-pref modulepref :objtemp-color (color item))))

                    (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 182) (incf posy 22)) (om-make-point 250 22) "patch"
                                          :font *controls-font*)

                    (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 240) posy) (om-make-point 250 22) "maquette"
                                                             :font *controls-font*)
                     
                     

                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 302) posy) (om-make-point 250 22) "instance"
                                          :font *controls-font*)
                                           
                     
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) (incf posy 30)) (om-make-point 250 22) "Miniview Background:"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 220) posy) (om-make-point 130 24) ""
                                          :range '("Transparent" "White" "Box Color")
                                          :value (cond ((equal (get-pref modulepref :temp-minipict-bg) :white) "White")
                                                       ((equal (get-pref modulepref :temp-minipict-bg) :box) "Box Color")
                                                       (t "Transparent"))
                                          :di-action (om-dialog-item-act item 
                                                        (let ((choice (om-get-selected-item item)))
                                                          (set-pref modulepref :temp-minipict-bg
                                                               (cond ((string-equal choice "White") :white)
                                                                     ((string-equal choice "Box Color") :box)
                                                                     (t nil))
                                                               ))))

                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) (incf posy 30)) (om-make-point 250 22) "Score Box Mode:"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 220) posy) (om-make-point 130 24) ""
                                          :range '("Score" "Piano Roll")
                                          :value (cond ((equal (get-pref modulepref :temp-minipict-mode) :score) "Score")
                                                       ((equal (get-pref modulepref :temp-minipict-mode) :pianoroll) "Piano Roll")
                                                       (t "Score"))
                                          :di-action (om-dialog-item-act item 
                                                        (let ((choice (om-get-selected-item item)))
                                                          (set-pref modulepref :temp-minipict-mode
                                                               (cond ((string-equal choice "Score") :score)
                                                                     ((string-equal choice "Piano Roll") :pianoroll)
                                                                     (t nil))
                                                               ))))
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) (incf posy 30)) (om-make-point 250 22) "Show Box Icons:"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-check-box (om-make-point (+ l2 220) posy) (om-make-point 20 20) "" 
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :temp-show-icons (om-checked-p item)))
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :temp-show-icons))
                     )
     thescroll))



;;;;=================================================
;;;; LIBS PREFERENCES MODULE
;;;;=================================================

(defmethod put-preferences ((prefID (eql :userlibs)))
  (let ((modulepref (find-pref-module prefID)))
    (setf *user-lib-dir* (remove nil (list! (or (get-pref modulepref :user-lib-dirs)
                                                (get-pref modulepref :user-lib-dir) ;;; compat
                                                ))))
    (reload-user-libs)
    (setf *libs-auto-load* (get-pref modulepref :auto-load-libs))  
    (libs-autoload)
    t))


(defmethod save-pref-module ((iconID (eql :userlibs)) values)
  (list iconID `(list :auto-load-libs ,(omng-save *libs-auto-load*) :user-lib-dirs ,(omng-save (list! *user-lib-dir*))) *om-version*))

(defmethod get-def-vals ((iconID (eql :userlibs)))
  (list :auto-load-libs nil :user-lib-dirs nil))
  
;;; (get-pref (get-pref-by-icon 286) :auto-load-libs)
;;; (setf *libs-auto-load* nil)

(defmethod make-new-pref-scroll ((pref (eql :userlibs)) modulepref)
   (let* ((thescroll (om-make-view 'preference-pane
                                  :pref-id pref
                                  :size (get-pref-scroll-size)
                                  :position (om-make-point 0 0)
                                  :scrollbars nil 
                                  :name "Libraries"
                                  :retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                  ))
         (ww (om-point-h (get-pref-scroll-size)))
         (l1 20) (l2 (round ww 2))
         libslist)
     (om-add-subviews thescroll
                      (om-make-dialog-item 'om-static-text (om-make-point 20 15) (om-make-point 200 30) "Libraries"
                                           :font *om-default-font3b*)
                      (om-make-dialog-item 'om-static-text  (om-make-point 20 45) (om-make-point 80 22) 
                                           "Auto Load:" :font *controls-font*)
                      (om-make-dialog-item 'om-static-text  (om-make-point 200 47) (om-make-point 200 22) "(Libraries to load at startup)"
                     :font *om-default-font1*)
                      
                      (setf libslist (om-make-view 'loadlibs-view :position (om-make-point 20 75) :size (om-make-point 360 210) 
                                    :prefobject modulepref :scrollbars :v :bg-color *om-white-color*
                                    :field-size (om-make-point 360 (* 20 (+ 1 (round (length (get-elements *library-package*)) 2))))
                                   ))
                      (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) 45) (om-make-point 260 22) 
                                           "External User Libs Directories:" :font *om-default-font2b*)
                      
                      (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) 65) (om-make-point (- l2 25) 55)
                                           (format nil "~A~%~A" "Select folders where OM will look to find extra libraries."
                                                   "The library folders should contain ONLY OM libraries and no other subfolders.")
                                           :font *om-default-font1*))

     (let ((pos 120)
           (libfolders (remove nil (list! (get-pref modulepref :user-lib-dirs)))))
       (mapcar #'(lambda (folder)  
                   (let ((dirtxt (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) pos) (om-make-point (- l2 85)  55)
                                                      (namestring folder)
                                                      :font *om-default-font1*
                                                      :fg-color (if (probe-file folder) *om-black-color* *om-red-color* ))))
               
                     (om-add-subviews thescroll 
                                      dirtxt
                                      (om-make-view 'om-icon-button 
                                                    :icon1 "folder" :icon2 "folder-pushed"
                                                    :position (om-make-point (- ww 70) pos) :size (om-make-point 20 20) 
                                                    :action (om-dialog-item-act item
                                                              (let ((newfolder (om-choose-directory-dialog :directory folder)))
                                                                (when newfolder
                                                                  (om-set-dialog-item-text dirtxt (namestring newfolder))
                                                                  (pushr newfolder libfolders)
                                                                  (set-pref modulepref :user-lib-dirs libfolders)
                                                                  (om-set-fg-color dirtxt *om-black-color*)))))
                                      (om-make-view 'om-icon-button 
                                                    :icon1 "x" :icon2 "x-pushed"
                                                    :position (om-make-point (- ww 48) pos) :size (om-make-point 20 20) 
                                                    :action (om-dialog-item-act item
                                                              (om-set-dialog-item-text dirtxt "----")
                                                              (setf libfolders (remove folder libfolders))
                                                              (set-pref modulepref :user-lib-dirs libfolders)
                                                              (om-set-fg-color dirtxt *om-black-color*)))
                                      )
                     (setf pos (+ pos 35))
                     ))
               libfolders)
       
       (let ((newdirtxt (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 20) pos) (om-make-point (- l2 85)  55)
                                             "----"
                                             :font *om-default-font1*)))
         (om-add-subviews thescroll 
                          newdirtxt
                          (om-make-view 'om-icon-button 
                                        :icon1 "folder" :icon2 "folder-pushed"
                                        :position (om-make-point (- ww 70) pos) :size (om-make-point 20 20) 
                                        :action (om-dialog-item-act item
                                                  (let ((newfolder (om-choose-directory-dialog :directory nil)))
                                                    (when newfolder
                                                      (om-set-dialog-item-text newdirtxt (namestring newfolder))
                                                      (pushr newfolder libfolders)
                                                      (set-pref modulepref :user-lib-dirs libfolders)
                                                      (om-set-fg-color newdirtxt *om-black-color*))))))
         (setf pos (+ pos 35))
         ))
              
     thescroll))


(omg-defclass loadlibs-view (om-scroller) 
  ((prefobject :initform nil :initarg :prefobject :accessor prefobject)))

(defmethod om-component-border ((self loadlibs-view)) :line)

;(mapcar 'name (get-elements *library-package*))

(defmethod initialize-instance :after ((self loadlibs-view) &rest initargs)
  (let ((pos (position :prefobject initargs)) object)
    (when pos
      (setf object (nth (+ pos 1) initargs))
      (let ((notfound (loop for l in *libs-auto-load* when (not (exist-lib-p (car l))) collect l))
            (max (* (ceiling (length (get-elements *library-package*)) 2) 20))
            (pos 5) (incr 20) (c 10))
        (om-with-delayed-update self
        (mapc #'(lambda (lib) 
                  (om-add-subviews self
                                   (om-make-dialog-item 'om-static-text (om-make-point c pos) (om-make-point 160 20)
                                                        (name lib) :font *controls-font*)
                                   (om-make-dialog-item 'om-check-box (om-make-point (+ c 120) (- pos 2)) (om-make-point 20 20)
                                                        "" 
                                                        :di-action (om-dialog-item-act item                                                                  
                                                                     (if (om-checked-p item)
                                                                         (unless (member (name lib) (get-pref object :auto-load-libs) 
                                                                                         :key 'car :test 'string-equal)
                                                                           (set-pref object :auto-load-libs 
                                                                                     (append (get-pref object :auto-load-libs) 
                                                                                           (list (list (name lib) (lib-pathname lib))))))
                                                                       (set-pref object :auto-load-libs 
                                                                                 (remove (name lib) 
                                                                                         (get-pref object :auto-load-libs) 
                                                                                         :test 'string-equal :key 'car)))
                                                                     )
                                                        :checked-p (member (name lib) (get-pref object :auto-load-libs) :test 'string-equal :key 'car)
                                                        ))
                  (setf pos (+ pos incr))
                  (when (>= pos max)
                    (setf pos 5 c (+ c (round (om-width self) 2))))
                  )
                 (get-elements *library-package*))
        )
        ;(om-set-field-size self (om-make-point (om-width self) 2000))
        ;(om-set-view-size self (om-make-point (om-width self) 100))
        )
      )))

;(defun refresh-libslist (view) 
;  (om-set-field-size view (om-make-point 360 (* 20 (+ 1 (round (length (get-elements *library-package*)) 2)))))
;  )

; (setf *libs-auto-load* '(("hjghkjhg" nil) ("kjhkjh" nil) ("repmus" nil)))
;;;====================================================
;;; appele juste apres la creation du workspace


;;;;=================================================
;;;; EXTERNALs PREFERENCES MODULE  icon 922
;;;;=================================================

(defvar *external-prefs* nil)

(defun add-external-pref-module (module)
  (unless (member module *external-prefs*)
    (pushr module *external-prefs*)))

(defmethod put-preferences ((id (eql :externals)))
  (let ((modulepref (find-pref-module id)))
    (loop for item in *external-prefs* do 
          (put-external-preferences item modulepref)
          )
    t))

(defun put-external-pref-values (module)
  (put-external-preferences module (find-pref-module :externals)))

(defmethod save-pref-module ((iconID (eql :externals)) item)
  (list iconID `(list ,.(loop for item in *external-prefs* append (save-external-prefs item))) *om-version*))

(defmethod get-def-vals ((iconID (eql :externals)))
  (loop for item in *external-prefs* append (get-external-def-vals item)))

; (corrige-preference-list 922 (defvals (get-pref-by-icon 922)) 0)

; pspecial : on peut avoir des prefs qui ne sont pas par défaut initialement...
(defmethod corrige-preference-list ((iconid (eql :externals)) list &optional (version 0))
  (let ((def (get-def-vals iconID))
        (saved (clone list))
        (rep nil))
    (loop for i = 0 then (+ i 2)
          while (< i (length def)) do
          (let ((pos (position (nth i def) saved)))
            (if pos 
                (setf rep (append rep (list (nth i def) (nth (+ pos 1) saved)))
                      saved (append (subseq saved 0 pos) (subseq saved (+ pos 2))))
              (setf rep (append rep (list (nth i def) (nth (+ i 1) def)))))
            ))
    (append rep saved)))

(defmethod get-external-name ((module t)) "")
(defmethod get-external-icon ((module t)) 642)

(defmethod save-external-prefs ((module t)) nil)
(defmethod put-external-preferences ((module t) modulepref) nil)
(defmethod get-external-def-vals ((module t)) nil)

(defmethod get-external-module-vals ((module t) modulepref) nil)
(defmethod get-external-module-path ((module t) modulepref) nil)
(defmethod set-external-module-vals ((module t) modulepref path) nil)
(defmethod set-external-module-path ((module t) modulepref vals) nil)
(defmethod show-external-prefs-dialog ((module t) vals) nil)



(defclass om-clickable-static-text (om-static-text) ())

(defmethod om-view-doubleclick-handler ((self om-clickable-static-text) where)
  (om-funcall-dialog-item-action self))

(defmethod om-component-border ((self om-clickable-static-text)) *om-gray-color*)

(defmethod make-new-pref-scroll ((num (eql :externals)) modulepref)
   (let ((thescroll (om-make-view 'preference-pane
                                  :pref-id num
                                  :name " Externals "
                                  :size (get-pref-scroll-size)
                                  :position (om-make-point 66 0)
                                  :scrollbars :v 
                                  :retain-scrollbars t
                                  :bg-color *om-light-gray-color*))
         (pos 40) (xpos 20))
     
        (om-add-subviews thescroll
                         (om-make-dialog-item 'om-static-text (om-make-point 20 10) (om-make-point 300 30) "Externals"
                                              :font *om-default-font3b*))
              
        (loop for item in (remove-duplicates *external-prefs*) do
	     (let* ((mod item)
		    (textvar (om-make-dialog-item 'om-clickable-static-text (om-make-point xpos (+ pos 20)) (om-make-point 246 44) 
						  (if (get-external-module-path item modulepref) 
						      (namestring (get-external-module-path item modulepref))
						      "NOT FOUND")
						  :font *om-default-font1*
						  :fg-color (if (and (get-external-module-path item modulepref)
								     (probe-file (get-external-module-path item modulepref)))
								*om-black-color* *om-red-color*)
						  :bg-color *om-white-color*
						  :di-action #'(lambda (ti)
								 (let* ((path (om-get-user-string (string+ "Choose a new path for " (get-external-name mod) ":")
												  :initial-string (or (and (get-external-module-path mod modulepref) 
															   (namestring (get-external-module-path mod modulepref)))
														      ""))
									  ))
								   (when path 
								     (if (probe-file path)
									 (progn 
									   (set-external-module-path mod modulepref (pathname path))
									   (om-set-dialog-item-text ti path)
									   (om-set-fg-color ti *om-black-color*)
									   (om-invalidate-view thescroll)
									   )
									 (om-beep-msg (string+ "File " (namestring path) " does not exist")))))) 
						  ))
		    )
                
	       (om-add-subviews thescroll
				(om-make-dialog-item 'om-static-text (om-make-point xpos pos) (om-make-point 180 24) 
						     (get-external-name mod) :font *om-default-font2b*)
                                 
				textvar

				(om-make-view 'om-icon-button 
					      :icon1 "folder" :icon2 "folder-pushed"
					      :position (om-make-point (+ xpos 250) (+ pos 20)) :size (om-make-point 26 25) 
					      :action (om-dialog-item-act button
							(declare (ignore button))
							(let* ((path (om-choose-file-dialog 
								      :directory (om-default-application-path nil nil))))
							  (when path 
							    (if (probe-file path)
								(progn 
								  (set-external-module-path mod modulepref path)
								  (om-set-dialog-item-text textvar (namestring path))
								  (om-set-fg-color textvar *om-black-color*)
								  (om-invalidate-view thescroll)
								  )
								(om-beep-msg (string+ "Bad path for " 
										      (get-external-name mod))))))))
                                 
				)
                
	       (when (get-external-module-vals mod modulepref)
		 (om-add-subviews thescroll (om-make-dialog-item 'om-button
								 (om-make-point (+ xpos 280) (+ pos 20)) 
								 (om-make-point 75 24) "Options"
								 :font *om-default-font1*
								 :di-action #'(lambda (button) 
										(declare (ignore button))
										(let ((rep (show-external-prefs-dialog mod (get-external-module-vals mod modulepref))))
										  (when rep
										    (set-external-module-vals mod modulepref rep))))
;;;:iconid (get-external-icon item))
                                 
								 )))
                
	       (setf pos (+ pos 65))
	       (when (> pos (- (om-point-v (get-pref-scroll-size)) 100))
		 (setf pos 44
		       xpos (round (om-point-h (get-pref-scroll-size)) 2)))
	       ))
      thescroll))


; (get-external-icon 'orchidee)

;(setf *p* (om-choose-file-dialog))
;(find-true-external *p*)

(defun find-true-external (path)
  (let ((name (car (last (pathname-directory path)))))
    (if (and (directoryp path)
             (string-equal "app" (subseq name (- (length name) 3))))
        ;;; path is an application bundle
        (make-pathname :directory (append (pathname-directory path) (list "Contents" "MacOS"))
                       :name (subseq name 0 (- (length name) 4)))
      ;;; otherwise...
      path)))
    





;;;;===============
;;; SETUP
;;;;===============


(defun add-kernel-prefs ()
  (push-pref-module (list :general (get-def-vals :general)))
  (push-pref-module (list :appearance (get-def-vals :appearance)))
  (push-pref-module (list :userlibs (get-def-vals :userlibs)))
  (push-pref-module (list :externals (get-def-vals :externals)))
  )

(add-init-user-func 'add-kernel-prefs)

; (setf *pref-item-list* nil)
; (add-kernel-prefs)
; (add-music/midi-preferences)
