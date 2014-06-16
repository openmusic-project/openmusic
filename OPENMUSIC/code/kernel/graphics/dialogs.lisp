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
;This File contains all dynamic dialogs used in OM.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)



;;; used for possible dialog message translations
(defun getstr (str) str)

(defun get-funname-dialog ()
  "Get the name, icon and doc for a new Generic function"
  (let ((dialog (om-make-window 'om-dialog
                                :position :centered
                                :window-title "New Generic Function"
                                :size (om-make-point 320 205)
                                :font *om-default-font1*
                                ;:bg-color (om-make-color 0.875 0.875 0.875)
                                :maximize nil :minimize nil :resizable nil :close nil
                                ))
        (scroller (om-make-dialog-item 'om-text-edit-view
                                       (om-make-point 128 40) (om-make-point 154 87) ""
                                       :scrollbars :v 
                                       ))
        (name (om-make-dialog-item 'om-editable-text 
                                   (om-make-point 128 9) (om-make-point 154 19) 
                                   "MyFunction"
                                   ))
        (icon 150))
    (om-set-font scroller *om-default-font1*)
    (om-add-subviews dialog scroller name
                     (om-make-view 'button-icon
                                   :iconID icon
                                   :size (om-make-point 24 24)
                                   :position (om-make-point 131 150)
                                   :action  #'(lambda (item) 
                                                (let ((newicon (choise-icon)))
                                                  (when newicon icon 
                                                        (setf icon newicon)
                                                        (setf (iconID item) icon)
                                                        (om-draw-contents item)))))
                     (om-make-dialog-item 'om-static-text (om-make-point 3 9) (om-make-point 100 16) "Function Name"
                                          :bg-color *om-window-def-color*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 3 41) (om-make-point 100 18)  "Documentation"
                                          :bg-color *om-window-def-color*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 10 149) (om-make-point 56 16)  "Icon"
                                          :bg-color *om-window-def-color*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-button (om-make-point 191 134) (om-make-point 80 25)  "Cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog nil))
                                          :default-button t)
                     (om-make-dialog-item 'om-button (om-make-point 193 164) (om-make-point 80 22)  "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog (list (om-dialog-item-text name)
                                                                                                 (om-dialog-item-text scroller)
                                                                                                 icon)))
                                          ;:default-button t
                                          )
                     )
    (om-modal-dialog dialog)))




(defun get-classname-dialog ()
  "Get the name, icon and doc for a new Class"
  (let ((dialog (om-make-window 'om-dialog
                                ;;;  :window-type :double-edge-box
                                :position :centered
                                :window-title "New Class"
                                :size (om-make-point 320 205)
                                :font *om-default-font1*
                                :maximize nil :minimize nil :resizable nil :close nil
                                ))
        (scroller (om-make-dialog-item 'om-text-edit-view
                                       (om-make-point 128 40) (om-make-point 154 87) ""
                                       :scrollbars :v 
                                       ))
        (name (om-make-dialog-item 'om-editable-text 
                                   (om-make-point 128 9) (om-make-point 154 19) 
                                   "MyClass"
                                   ))
        (icon 136))
    (om-add-subviews dialog scroller name
                     (om-make-view 'button-icon
                                   :iconID icon
                                   :size (om-make-point 24 24)
                                   :position (om-make-point 131 150)
                                   :action  #'(lambda (item) 
                                                (let ((newicon (choise-icon)))
                                                  (when newicon icon 
                                                    (setf icon newicon)
                                                    (setf (iconID item) icon)
                                                    (om-draw-contents item)))))
                     (om-make-dialog-item 'om-static-text (om-make-point 3 9) (om-make-point 100 16)  "Class Name" 
                                          :bg-color *om-window-def-color*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 3 41) (om-make-point 100 18)  "Documentation"
                                          :bg-color *om-window-def-color*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 10 149) (om-make-point 56 16)  "Icon"
                                          :bg-color *om-window-def-color*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-button (om-make-point 191 134) (om-make-point 80 25)  "Cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 193 164) (om-make-point 80 22)  "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog (list (om-dialog-item-text name)
                                                                                                 (om-dialog-item-text scroller)
                                                                                                 icon)))
                                          :default-button t))
    (om-modal-dialog dialog)))




;;;=======================
;;;OM PRESENTATION + About
;;;=======================

(omg-defclass splash-screen (om-view) 
   ((thepict :initform nil :initarg :thepict :accessor thepict))
   (:documentation "The class of the About OM window.#enddoc#
#thepict# This slot contains the *graph-pres* global variable.#thepict#"))
    
(defvar *splash-screen* nil)

(defmethod om-view-click-handler ((self splash-screen) where)
  "When you click in the about window it is hidden"
  (declare (ignore where))
  (om-close-window (om-view-window self))
  (setf *splash-screen* nil))


(defmethod om-draw-contents ((self splash-screen)) 
  (om-draw-picture self (thepict self))
  (call-next-method))

(defun show-kero-pict (&optional (credits nil))
  (if *splash-screen* (om-select-window *splash-screen*)
    (let ((name (string+ "OpenMusic " *version-string*))
          (textcolor  (om-make-color 0.384 0.302 0.266))
          (backcolor (om-make-color 0.9529412 0.9294118 0.803916))
          (mainfont  (om-make-font "Verdana" (nth 0 *om-def-font-sizes*)))
          (boldfont  (om-make-font "Verdana" (nth 0 *om-def-font-sizes*) :style '(:bold)))
          win view)
      (setf win (om-make-window 'om-no-border-win
                                :window-title "About OpenMusic"
                                :close nil
                                :minimize nil
                                :maximize nil
                                :resizable nil
                                         :window-show nil
                                         :position :centered 
                                         :bg-color backcolor
                                         :size (om-add-points 
                                                (or (om-get-picture-size *graph-pres*) (om-make-point 20 20))
                                                (if credits (om-make-point 300 0) (om-make-point 0 0)))
                                         ))

    (om-add-subviews win (setf view (om-make-view 'splash-screen
                                                  :thepict *graph-pres*
                                                   :bg-color backcolor
                                                   :size (om-interior-size win)
                                                   :subviews (list (om-make-dialog-item 'om-static-text  
                                                                                        (om-make-point 58 6) (om-make-point 400 36) 
                                                                                        name
                                                                                        :font (om-make-font "Arial" (nth 4 *om-def-font-sizes*) :style '(:bold))
                                                                                        :fg-color textcolor
                                                                                        :bg-color backcolor
                                                                                        )
                                                                   (om-make-dialog-item 'om-static-text  
                                                                                        (om-make-point 92 57) (om-make-point 300 18) 
                                                                                        (string+ "(c) 1995-"
                                                                                                 (subseq *release-date* 0 4)
                                                                                                 " Ircam-Centre Georges Pompidou")
                                                                                        :font mainfont
                                                                                        :fg-color textcolor
                                                                             :bg-color backcolor
                                                                             )
                                                        (om-make-dialog-item 'om-static-text  
                                                                             (om-make-point 118 42) (om-make-point 250 20) 
                                                                             (string+ (string-until-space *release-date*)
                                                                                      " - Music Representations Group")
                                                                             :font mainfont
                                                                             :fg-color textcolor
                                                                             :bg-color backcolor
                                                                             )
                                                        
                                                        (om-make-dialog-item 'om-static-text  
                                                                             (om-make-point 58 320) (om-make-point 300 36)
                                                                             (format nil  "release ~D" *om-version*)
                                                                             :font mainfont
                                                                             :fg-color textcolor
                                                                             :bg-color backcolor
                                                                             )

                                                        (om-make-dialog-item 'om-static-text  
                                                                             (om-make-point 58 350) (om-make-point 300 36)
                                                                             "Dedicated to the memory of Gérard Grisey (1946-1998)"
                                                                             :font mainfont
                                                                             :fg-color textcolor
                                                                             :bg-color backcolor
                                                                             )
                                                        )
                                       )))
 
    (when credits (om-add-subviews view
                                   (om-make-dialog-item 'om-static-text  
                                                        (om-make-point 490 16) (om-make-point 210 600) 
                                                        *credits-1*
                                                        :font boldfont
                                                        :fg-color textcolor
                                                        :bg-color backcolor)
                                   (om-make-dialog-item 'om-static-text  
                                                        (om-make-point 390 60) (om-make-point 330 600) 
                                                        *credits-2*
                                                        :font mainfont
                                                        :fg-color textcolor
                                                        :bg-color backcolor)
                                   ))
    
    (setf *splash-screen* win)
    (om-select-window win))))

; (show-kero-pict t)

(defvar *credits-1* "OM Credits")
(defvar *credits-2* "OM Credits")

(setf *credits-1* 
"Design and development: 
C. Agon, G. Assayag, J. Bresson")

(setf *credits-2* 
"Contributions:
E. Amiot, M. Andreatta, D. Bouche, O. Delerue, K. Haddad, M. Laurson, S. Lemouton, G. Nouno, J. Podrazik, C. Rueda, M. Schumacher, M. Stroppa, C. Truchet, A. Vinjar, F. Voisin, ...

External code from: 
Grame (Midishare / LibAudioStream interfaces), H. Taube (MidiShare bindings), C. T. Binghe (UDP), N. Gafney (OSC), S. Ball (Lisp editor tools), J. Bielman, L. Oliveira (CFFI), S. Van Caekenberghe (XML)

External Libraries:
  MIDI support: MidiShare (c) Grame
  Audio support: LibAudioStream (c) Grame
        use LibSndFile (c) Erik de Castro Lopo
  Sound Description Interchange Format SDIF (c) Ircam

ArtWork: A. Mohsen
")

; Musical Expertise:
;J. Baboni, A. Bancquart, G. Bloch, J. Fineberg, K. Haddad, J.-L. Hervé, M. Lanza, P. Leroux, F. Lévy, G. Lorieux, C. Malherbe, M. Malt, Y. Maresz, T. Murail, P. Nauert, O. Sandred, K. Sprotte, M. Stroppa, H. Tutschku, ...    



;;;=======================
;;; MESSAGE WIN
;;;=======================
(defvar *message-win* nil)

(defclass om-message-win (om-windoid)
  ((pane :initarg :pane :accessor pane :initform nil)
   (label :initarg :label :accessor label :initform nil)))

(defun init-message-win ()
  (let ((newwindow (om-make-window 'om-message-win :size (om-make-point 200 100) :position :centered 
                                      :window-show t
                                      :window-title ""
                                      :bg-color *om-window-def-color*
                                      :resizable nil
                                      :minimize nil :maximize nil
                                      :topmost t
                                      )))
  
  (om-add-subviews newwindow (setf (pane newwindow)
                                       (om-make-view 'om-view :size (om-make-point 200 100)
                                                     :position (om-make-point 0 0)
                                                     :bg-color *om-window-def-color*)))
  (om-add-subviews (pane newwindow)
                   (setf (label newwindow)
                         (om-make-dialog-item 'om-static-text 
                                              (om-make-point 10 30) (om-make-point 180 70)
                                              "..."
                                              :font *om-default-font2*)))
  (setf *message-win* newwindow)))

(defun show-message-win (message &key size)
  (when (or (and *message-win* (om-window-open-p *message-win*)) 
            (init-message-win))
    (when message 
      (om-set-dialog-item-text (label *message-win*) (print message)))
    (om-select-window *message-win*)
    (when size 
      (om-set-view-size *message-win* size)
      (om-set-view-size (label *message-win*) size)
      (om-set-view-size (label *message-win*)
                        (om-subtract-points size (om-make-point 20 40))))
    ))

; (show-message-win "Saving folder...")
; (change-message-win "ooo")


(defun change-message-win (message)
  (declare (special *message-win*))
  (when *message-win*
    (om-set-dialog-item-text (label *message-win*) (print message))
    message))

(defun close-message-win ()
  (when *message-win* 
    (om-close-window *message-win*)))

(defun hide-message-win ()
  (when *message-win* 
    (om-hide-window *message-win*)))
    
;;;=======================
;;;Dialog for init
;;;=======================
(defun transforme-if-alias (name folders names)
  (if (stringp name)
    (let* ((folders-without-alias (om-directory (OMroot "WorkSpaces;") :directories t :files nil))
           (names-without-alias (mapcar #'(lambda (direc) (name-of-directory direc))
                                        folders-without-alias)))
      (if (member name names-without-alias :test 'equal)
        name
        (let ((position (position name names :test 'equal)))
          (if position
            (list (nth position folders))
            name))))
    name))

(defun set-ws-user-pref (file)
  (om-create-directory (make-pathname :directory (pathname-directory file)))
  (om-create-file file)
  (with-open-file (out file :direction :output 
                       :if-does-not-exist :create :if-exists :supersede) 
    (write-line (format nil ";~D" *om-version*) out)
    (prin1 '(in-package :om) out)
    (prin1 '(setf *omprefs* nil) out)
    ))
     

; (ws-dialog)


(defun ws-dialog ()
  (let* ((userpref (ompref-file))
         (newuser (not (probe-file userpref)))
         (font *om-default-font2*) ;(om-make-font "Arial" (nth 1 *om-def-font-sizes*)))
         (smallfont *om-default-font1*)
         previous)

    (when newuser 
      (set-ws-user-pref userpref))

    (handler-bind 
        ((error #'(lambda (err)
                    (delete-file userpref nil))))
      (load userpref)
      (setf previous (get-ompref 'prev-ws)))
  
    (let ((win (om-make-window 'om-dialog :size (om-make-point 420 330)
                               :resizable nil :maximize nil :minimize nil
                               :window-title "OpenMusic - Workspaces"
                               :owner nil
                               ))
          (view (om-make-view 'om-view :size (om-make-point 370 250) 
                              :position (om-make-point 20 20)
                              :bg-color *om-white-color*))
          (i 0)
          install prev exist new ttt)
      (om-add-subviews win view)
      (setf i (+ i 10))
     
      (when t  ; (member t (om-directory (OMRoot "resources;sample-ws;")) :key 'directoryp)
        (om-add-subviews view
                         ;(setf install (om-make-dialog-item 'om-radio-button (om-make-point 20 i) (om-make-point 200 10)
                         ;                                   (str-check " Install Ircam Workspaces")
                         ;                                   :checked-p newuser
                         ;                                   :font font
                         ;                                   :radio-button-cluster 'proj
                         ;                                   ))
                         (setf install (om-make-dialog-item 'om-static-text 
                                                            (om-make-point 15 i) 
                                                            (om-make-point 340 22)
                                                            (format nil "Choose or create a workspace...")
                                                            :font *om-default-font2b*))
                         (setf ttt (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 25 (+ i 20)) 
                                                        (om-make-point 335 60)
                                                        (format nil "A workspace corresponds to a directory on your computer and contains all the data of an OM session (patches, resource files, user-defined classes and functions, preferences, ...)")
                                                        :fg-color *om-dark-gray-color*
                                                        :font smallfont))
                         )
        (setf i (+ i 90))
        )
      
      (let ((prev-ok (and previous (pathnamep previous) (probe-file previous))))
        (om-add-subviews view
                         (setf prev (om-make-dialog-item 'om-radio-button (om-make-point 20 i) (om-make-point 200 20)
                                                         " Open previous workspace"
                                                         :checked-p (and prev-ok (not newuser))
                                                         :enable prev-ok
                                                         :font font
                                                         :radio-button-cluster 'proj
                                                         ))
                         (om-make-dialog-item 'om-static-text (om-make-point 40 (+ i 24)) (om-make-point 320 40)
                                              (if prev-ok (namestring previous) "...")
                                              :fg-color (if prev-ok *om-dark-gray-color* *om-gray-color*)
                                              :font smallfont)
                         )
        (setf i (+ i 55))
        )
          
        
      (om-add-subviews view
                       (setf exist (om-make-dialog-item 'om-radio-button (om-make-point 20 i) (om-make-point 200 20)
                                                        " Open a workspace"
                                                        :checked-p (or (not previous) newuser)
                                                        :font font
                                                        :radio-button-cluster 'proj
                                                        ))
                       (om-make-dialog-item 'om-static-text (om-make-point 40 (+ i 24)) (om-make-point 290 40)
                                            "[Select the workspace root directory]"
                                            :fg-color *om-dark-gray-color*
                                            :font smallfont)
                       )
      
      (setf i (+ i 50))
      (om-add-subviews view
                       (setf new (om-make-dialog-item 'om-radio-button (om-make-point 20 i) (om-make-point 200 20)
                                                      " Create a new workspace"
                                                      :checked-p nil
                                                      :font font
                                                      :radio-button-cluster 'proj
                                                      ))
                       )
        
        
      (setf i (+ i 80))
      (om-add-subviews win 
                       (om-make-dialog-item 'om-button (om-make-point (- (w win) 100) (- (h win) 50))
                                            (om-make-point 80 24) "OK"
                                            :default-button t
                                            :focus t
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog win 
                                                                                      (cond 
                                                                                         ;((and install (om-checked-p install)) 'install)
                                                                                       ((and prev (om-checked-p prev)) 'previous)
                                                                                       ((om-checked-p exist) 'existing)
                                                                                       ((om-checked-p new) 'new)
                                                                                       (t nil))
                                                                                      )
                                                         )
                                            )
                       (om-make-dialog-item 'om-button (om-make-point (- (w win) 200) (- (h win) 50))
                                            (om-make-point 80 24) "Quit"
                                            :default-button nil
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog win 'quit))))
       
      (om-modal-dialog win)
      )))

; (ws-dialog)



(defun choose-user-ws-folder ()
  (let* ((choix (ws-dialog))
        (dir (cond
              ((equal 'new choix)
               (let* ((prev (get-ompref 'prev-ws))
                      (folder (if prev 
                                (om-make-pathname :device (pathname-device prev) :directory (butlast (pathname-directory prev))) 
                                (om-user-home)))
                      (ws (om-choose-new-directory-dialog :prompt "Choose a location and name for the new Workspace" :directory folder)))
                 (if (pathnamep ws) ws (choose-user-ws-folder))
                 ))
              ((equal 'existing choix)
               (let* ((prev (get-ompref 'prev-ws))
                      (folder (if prev 
                                (om-make-pathname :device (pathname-device prev) :directory (butlast (pathname-directory prev))) 
                                (om-user-home)))
                      (ws (om-choose-directory-dialog :prompt "Please select an existing Workspace directory" :directory folder)))
                 (if (pathnamep ws) ws (choose-user-ws-folder))
                 ))
              ((equal 'previous choix)
               (get-ompref 'prev-ws))
              ((equal 'install choix)
               (install-om-workspaces)
               (choose-user-ws-folder))
              ((equal 'quit choix)
               (om-confirmed-quit))
              (t nil))))
    dir))

(defun show-workspaces-dialog ()
    (let ((initws (catch-cancel (choose-user-ws-folder))))
    (if (not (pathnamep initws))
      (show-workspaces-dialog)
      (let* ((list (om-directory initws))
             dirs files)
        (loop for item in list do
              (if (directoryp item) 
                (push (car (last (pathname-directory item))) dirs)
                (push (pathname-name item) files)))
        (if (or (equal nil list) 
                (and (member "resources" dirs :test 'string-equal) (member "elements" dirs :test 'string-equal)))
          ;;; + tests...
          (start-from-ws initws)
          (if (om-y-or-n-dialog (str-check (format nil "Are you sure this is a OM Workspace ?~%(This folder seems to contain other types of documents)"))) 
            (start-from-ws initws)
            (show-workspaces-dialog)
            ))))))

(defun install-om-workspaces ()
  (let ((targetdir (om-choose-directory-dialog :prompt "Choose a directory for the OM example Workspaces" :directory (om-user-home)))
        srcdir copied-ws)
    (when (and targetdir (probe-file targetdir))
      (show-message-win (format nil "Importing Default Workspaces...") :size (om-make-point 200 100))
      (loop for item in (om-directory (OMRoot "resources;sample-ws;")) do
            (when (directoryp item)
              (let ((tg (make-pathname :device (pathname-device targetdir)
                                       :directory (append (pathname-directory targetdir) (last (pathname-directory item))))))
                (if (probe-file tg) 
                  (om-message-dialog (format nil "~D already exists.~%Please remove it or choose another install directory." 
                                             (car (last (pathname-directory item)))))
                  (progn
                    (copy-folder item 
                                 (make-pathname :device (pathname-device targetdir)
                                                :directory (append (pathname-directory targetdir) (last (pathname-directory item))))
                           )
                    (push (car (last (pathname-directory tg))) copied-ws)
                    )))))
      (when (probe-file (make-pathname :directory (append (pathname-directory targetdir) '("OMWorkspace"))))
        (set-ompref 'prev-ws (make-pathname :directory (append (pathname-directory targetdir) '("OMWorkspace")))))
      (om-hide-window *message-win*)
      (om-message-dialog (if copied-ws
                             (let ((str (string+ "The following workspace directories have been created in " 
                                                 (namestring targetdir) ": ")))
                               (loop for item in copied-ws do (setf str (string+ str (format nil "~%    ~a" item))))
                               (setf str (string+ str (format nil "~%Select \"Open Workspace\" option, then choose one of those directories for starting a session with the corresponding workspace.")))
                               str)
                           "Warning: the IRCAM workspaces could not be installed."))
      )))






;;;==============================
;;;This dialog is diaplyed when you cons an image
;;;It allows you to choise the projects (Music, kernel, etc that you want include in your image
;;;

(defun choise-directories (pathlist)
  (when pathlist
    (let* ((names (loop for item in pathlist collect (name-of-directory item)))
           (dialog (om-make-window 'om-dialog
                                   :position :centered
                                   :size (om-make-point 235 (+ 60 (* 20 (length pathlist))))
                                   :close nil :maximize nil :minimize nil :resizable nil
                                   :window-title "Choose your projects"
                                   :bg-color *om-window-def-color*)) rep)
      (loop for item in names
            for i = 0 then (+ i 1) do
            (let ((newdialog (om-make-dialog-item 'om-check-box (om-make-point 5 (+ 10 (* i 20))) (om-make-point 200 20) 
                                                  item
                                                  :font *om-default-font2*
                                                  :checked-p t
                                                  :bg-color *om-window-def-color*
                                                  :enable (not (string-equal item "basicproject")))))
              (push newdialog rep)
              (om-add-subviews dialog newdialog)))
      (setf rep (reverse rep))
      (om-add-subviews dialog
                       (om-make-dialog-item 'om-button (om-make-point 33 (- (om-point-v (om-interior-size dialog)) 35)) (om-make-point 70 20) 
                                            "Cancel"
                                            :di-action (om-dialog-item-act item 
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog nil)))
                       (om-make-dialog-item 'om-button (om-make-point 132 (- (om-point-v (om-interior-size dialog)) 35)) (om-make-point 70 20) 
                                            "OK"
                                            :di-action (om-dialog-item-act item 
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog
                                                                                      (loop for item in rep
                                                                                            when (om-checked-p item) collect (om-dialog-item-text item))))
                                            :default-button t))
      (om-modal-dialog dialog))))


;;;==============================


;;;==============================
;;; CHOOSE PICTURE DIALOG

(omg-defclass om-preview-pane (om-view) 
   ((pict :accessor pict :initform nil :initarg :pict)))

(defmethod om-draw-contents ((self om-preview-pane))
   (when (pict self)
     (let ((resize-params (resize-to-fit (om-get-picture-size (pict self)) (om-view-size self))))          
       (om-draw-picture self (pict self) 
                        :pos (second resize-params)
                        :size (first resize-params)))))


;(choose-resource-dialog :pict)

(defun check-file-type (path types)
  (cond ((stringp types) (string-equal types (pathname-type path)))
        ((consp types) (member (pathname-type path) types :test 'string-equal))
        (t nil)))

(defun def-file-type (types)
  (cond ((stringp types) types)
        ((consp types) (car types))
        (t nil)))

(defun unique-icon-pathname (dir &optional type)
  (let* ((i 1)
         (ftype (or type (car *om-icon-type*)))
         (icons (mapcar #'(lambda (file) (when (and (pathname-name file)
                                                    (> (length (pathname-name file)) 0))
                                           (if (integerp (read-from-string (pathname-name file)))
                                               (read-from-string (pathname-name file))
                                             nil)))
                        (om-directory dir :files t :directories nil))))
    (loop while (member i icons :test '=) do (setf i (+ i 1)))
    (make-pathname :device (pathname-device dir) :directory (pathname-directory dir) 
                   :name (format nil "~d" i) :type ftype)))


; Returns (symbol-name pict-hdler kernel-or-user) ou nil
(defun choose-resource-dialog (resource-type &key (kernel t) (user t))
  (if (not user) (setf kernel t))
  (let* ((twoorone (if (and user kernel) 2 1))
         (thedialog (om-make-window 'om-dialog  
                                    :window-title (string+ "Select " (if (equal resource-type :icon) "an icon" "a picture")) 
                                    :position :centered
                                    :size (om-make-point (+ 230 (* 130 twoorone)) 205) 
                                    :close t :resizable nil :maximize nil :minimize nil
                                    :bg-color *om-window-def-color*))
         (preview (om-make-view 'om-preview-pane :position (om-make-point 20 10) :size (om-make-point 130 130) :bg-color *om-white-color*))
         (sizetxt (om-make-dialog-item 'om-static-text (om-make-point 20 140) (om-make-point 50 16) "" :font *om-default-font1* :fg-color *om-gray-color*))
         (listkernel nil) (listuser nil) scrolluser scrollkernel usertxt kerneltxt
         (tmp-pict nil) (resfolder nil) (name nil))
    (when kernel
      
      
      (setf listkernel (if (equal resource-type :icon) 
                           (om-get-all-icons-id)
                         (om-loaded-pictures #'(lambda (type) (not (member type '(user internal di)))))))
      (setf scrollkernel (om-make-dialog-item 'om-single-item-list 
                                              (om-make-point 180 30) 
                                              (om-make-point 130 128) 
                                              "OM Resources"     
                                              :scrollbars :v
                                              :di-action 
                                              (om-dialog-item-act item
                                                (if (om-get-selected-item item)
                                                    (let* ((ind (om-get-selected-item-index item))
                                                           (elem (nth ind listkernel)))
                                                      (setf name (om-get-selected-item item)
                                                            resfolder (if (equal resource-type :icon) 'kernel (cadr (car elem)))
                                                            tmp-pict  (if (equal resource-type :icon) 
                                                                          (cadr (get&corrige-icon elem))
                                                                        (cadr elem))))
                                                      (setf name nil resfolder nil tmp-pict nil))
                                                
                                                (setf (pict preview) tmp-pict)
                                                (om-set-dialog-item-text sizetxt (format nil "~Dx~D" (om-pict-width tmp-pict) (om-pict-height tmp-pict)))
                                                (om-invalidate-view preview t)
                                                (om-invalidate-view item t)
                                                (when user (om-invalidate-view scrolluser t))
                                                )
                                              :range (if (equal resource-type :icon) 
                                                         (mapcar 'integer-to-string listkernel)
                                                       (mapcar #'(lambda (elem) (string (caar elem))) listkernel))
                                              :container thedialog
                                              ))
      (setf kerneltxt (om-make-dialog-item 'om-static-text 
                                           (om-make-point 180 6) 
                                           (om-make-point 130 20) 
                                           "OM Resources"  
                                           :font *controls-font*
                                           :container thedialog 
                                           ;:bg-color *om-window-def-color*
                                           ))
      )
    (when user
      (setf listuser (loop for file in  (om-directory  
                                         (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                           (list "resources" (if (equal resource-type :icon) "icon" "pict")))) 
                                         :type (if (equal resource-type :ICON) *om-icon-type* *om-pict-type*))
                           when (om-pict-p file)
                           collect (om-namestring (pathname-name file))))
      (setf scrolluser (om-make-dialog-item 'om-single-item-list 
                                            (om-make-point (if kernel 330 190) 30) 
                                            (om-make-point 130 110) 
                                            "User Resources"  
                                            :scrollbars :v
                                            :di-action (om-dialog-item-act item
                                                         (if (om-get-selected-item item)
                                                             (setf name (om-get-selected-item item)
                                                                   resfolder 'user
                                                                   tmp-pict (om-load-pixmap (om-get-selected-item item) (if (equal resource-type :icon) *om-icon-type* *om-pict-type*)
                                                                                            (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                                                                              (list "resources" (if (equal resource-type :icon) "icon" "pict")))))
                                                                   )
                                                           (setf name nil resfolder nil tmp-pict nil))
                                                         
                                                         (setf (pict preview) tmp-pict)
                                                         (om-set-dialog-item-text sizetxt (format nil "~Dx~D" (om-pict-width tmp-pict) (om-pict-height tmp-pict)))
                                                         (om-invalidate-view preview t)
                                                         (om-invalidate-view item t)
                                                         (when kernel 
                                                           (om-invalidate-view scrollkernel t)))
                                            :range listuser
                                            :container thedialog
                                            ))
      (setf usertxt (om-make-dialog-item 'om-static-text 
                                         (om-make-point (if kernel 330 190) 6) 
                                         (om-make-point 120 16) 
                                         "User Resources"    
                                         :font *controls-font*
                                         :container thedialog 
                                         ;:bg-color *om-window-def-color*
                                         ))
      (om-add-subviews thedialog 
                       (om-make-dialog-item 'om-button (om-make-point (if kernel 380 240) 142) (om-make-point 40 24)  "+" 
                                            :di-action (om-dialog-item-act item
                                                         (let ((file (om-choose-file-dialog :prompt "Select an image to import")))
                                                           (when file
                                                             (if (check-file-type file (if (equal resource-type :icon) *om-icon-type* *om-pict-type*))
                                                                 (let* ((resdir (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                                                                  (list "resources" (if (equal resource-type :ICON) "icon" "pict")))))
                                                                        (newfile (if (equal resource-type :icon) 
                                                                                     (unique-icon-pathname resdir (pathname-type file)) 
                                                                                   (unique-pathname resdir (pathname-name file) (pathname-type file))))
                                                                        (newname (pathname-name newfile)))
                                                                   (om-copy-file file newfile)
                                                                   (setf listuser (append listuser (list newname)))
                                                                   (om-set-item-list scrolluser listuser)
                                                                   (om-select-item-index scrolluser (- (length listuser) 1))
                                                                   (setf name newname
                                                                         resfolder 'user
                                                                         tmp-pict (om-load-pixmap newname 
                                                                                                  (if (equal resource-type :icon) *om-icon-type* *om-pict-type*)
                                                                                                  (make-pathname 
                                                                                                   :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                                                                      (list "resources" (if (equal resource-type :ICON) "icon" "pict")))))
                                                                         )
                                                                   (setf (pict preview) tmp-pict)
                                                                   (om-set-dialog-item-text sizetxt (format nil "~Dx~D" (om-pict-width tmp-pict) (om-pict-height tmp-pict)))
                                                                   (om-invalidate-view preview t)
                                                                   (om-invalidate-view scrolluser t)
                                                                   )
                                                               (om-message-dialog (format nil "Bad file type or extension!
 Please check the original file has type/extension ~s" (if (equal resource-type :icon) *om-icon-type* *om-pict-type*)))
                                                             ))))
                                            )
                       (om-make-dialog-item 'om-button (om-make-point (if kernel 420 280) 142) (om-make-point 40 24)  "-" 
                                            :di-action (om-dialog-item-act item
                                                         (when (om-get-selected-item scrolluser)
                                                           (let* ((file (om-get-resource-file (om-get-selected-item scrolluser)
                                                                                              (append (pathname-directory (mypathname *current-workspace*)) 
                                                                                                      (list "resources" 
                                                                                                            (if (equal resource-type :icon) "icon" "pict")))
                                                                                       (if (equal resource-type :icon) *om-icon-type* *om-pict-type*))
                                                                        ))
                                                             (when file 
                                                               (om-delete-file file))
                                                             (setf listuser (remove (om-get-selected-item scrolluser) listuser))
                                                             (om-set-item-list scrolluser listuser)
                                                             (setf name nil resfolder nil tmp-pict nil (pict preview) nil)
                                                             (om-set-dialog-item-text sizetxt "")
                                                             (om-invalidate-view preview t)
                                                             )))
                                            )
                       )
      )
    
    (om-add-subviews thedialog
                     preview sizetxt
                     (om-make-dialog-item 'om-button (om-make-point 10 162) (om-make-point 70 24)  "Cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog thedialog nil))
                                          )
                     (om-make-dialog-item 'om-button (om-make-point 90 162) (om-make-point 70 24)  "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog thedialog (list name tmp-pict resfolder))
                                                       ))
                     )
    (om-modal-dialog thedialog)))

; (choose-resource-dialog :pict :kernel nil :user t)

;;; CHOSE BG PICT & PARAMS


(omg-defclass choose-picture-view (om-transparent-view) 
   ((picture :accessor picture :initarg :picture :initform (make-instance 'picture))))

(defmethod picture-dialog ((self choose-picture-view))
  (get-picture-file))

(defmethod initialize-instance :after ((self choose-picture-view) &rest initargs)
   (om-add-subviews self
                               (om-make-view 'om-icon-button :position (om-make-point 56 2) :size (om-make-point 22 22)
                                                    :icon1 "x" :icon2 "x-pushed"
                                                    :action (om-dialog-item-act item
                                                              (when (and (picture self) (thepict (picture self)))
                                                                (om-kill-picture (thepict (picture self))))
                                                              (setf (picture self) nil)
                                                              ;(setf (thepict (picture self)) nil)
                                                              ;(setf (name (picture self)) nil)
                                                              (om-invalidate-view self)))
                               (om-make-view 'om-icon-button :position (om-make-point 56 26) :size (om-make-point 22 22)
                                                    :icon1 "picture" :icon2 "picture-pushed"
                                                    :action (om-dialog-item-act item
                                                              (let ((p (picture-dialog self)))
                                                                (when p 
                                                                  (setf (picture self) p)
                                                                  (om-invalidate-view self)))         
                                                              )
                                                    )))

(defmethod om-draw-contents ((self choose-picture-view))
   (call-next-method)
   (om-with-focused-view self
   (if (and (picture self) (thepict (picture self)))
       (om-draw-picture self (thepict (picture self)) :size (om-make-point 48 48))
     (progn (om-with-fg-color self *om-white-color* (om-fill-rect 0 0 48 48))
       (om-draw-line 0 0 48 48) (om-draw-line 48 0 0 48)))
   (om-draw-rect 0 0 48 48)   
   ))




;===========================================
;help windows
;===========================================

(omg-defclass help-item-view (om-item-view) 
  ((pict :accessor pict :initarg :pict :initform nil)
   (keytxt :accessor keytxt :initarg :keytxt :initform nil)
   (text :accessor text :initarg :text :initform nil)))

;; pict = symbol --> picture
;; pict = list --> combinaison de symboles
(defmethod initialize-instance :after ((self help-item-view) &rest initargs)
  (let ((textl 90))
  (if (consp (pict self))
    (progn
      (setf (keytxt self) (pict self))
      (setf (pict self) (om-load-and-store-picture "bouton" 'full 
                                                   (make-pathname :device (pathname-device *om-resources-folder*)
                                                                  :directory (append (pathname-directory *om-resources-folder*) (list "help")))))
      
      (loop while (> (* (length (keytxt self)) (+ (om-point-h (om-get-picture-size (pict self))) 4)) textl)
            do (setf textl (+ textl 20)))
      )
    (progn
      (setf (pict self) (om-load-and-store-picture (string (pict self)) 'full 
                                                   (make-pathname :device (pathname-device *om-resources-folder*)
                                                                  :directory (append (pathname-directory *om-resources-folder*) (list "help")))))
      (loop while (> (om-point-h (om-get-picture-size (pict self))) textl) do (setf textl (+ textl 20)))
      )
    )
  (om-add-subviews self (om-make-dialog-item 'om-static-text 
                                             (om-make-point textl 2) (om-make-point (- (w self) textl) 20) (text self)
                                             :font *om-default-font2b* :bg-color (om-make-color 0.9 0.9 0.9)))
  ))



(defmethod om-draw-contents ((self help-item-view))
  (if (keytxt self)
      (om-with-focused-view self
        (om-with-font *om-default-font4b*
          (let ((i 0))            
           (loop for elt in (keytxt self) do
                 (when (stringp elt)
                     (progn
                       (om-draw-picture self (pict self) :pos (om-make-point i 0) 
                                        :size (om-make-point (om-point-h (om-get-picture-size (pict self))) (om-point-v (om-view-size self))))
                       (om-draw-string (+ i (round (- (/ (om-point-h (om-get-picture-size (pict self))) 2)
                                                      (/ (om-string-size elt *om-default-font4b*) 2))))
                                       19 elt)
                       (setf i (+ i 4 (om-point-h (om-get-picture-size (pict self)))))
                       )
                   ;(progn 
                   ;  (om-draw-string (+ i 2) 19 (string elt))
                   ;  (setf i (+ i (om-string-size (string elt) *om-default-font4b*))))
                   )))))
    (when (pict self) 
      (om-draw-picture self (pict self) 
                       :size (om-make-point (om-point-h (om-get-picture-size (pict self))) (om-point-v (om-view-size self)))))
    ))


  
;(show-help-window "Patch Help" (list *patchhelplist* *patchhelplist2*))

(defvar *help-window* nil)

(defclass help-window (om-window) ())

(defmethod om-window-class-menubar ((self help-window))
  (list (om-make-menu "File"
                      (list (om-new-leafmenu "Close" #'(lambda () (om-close-window self)) "w")))
        (make-om-menu 'windows :editor self)))

(defun show-help-window (title helplist &optional panel-w)
  "Show the help window for a patch"
  (when helplist
    (when *help-window* (om-close-window *help-window*))
    (let ((maxl (loop for l in helplist maximize (length l)))
          (panew (or panel-w 310)))
      (setf *help-window* (om-make-window 'help-window :window-title title 
                                          :size (om-make-point (+ 4 (* (+ 4 panew) (length helplist))) (+ 12 (* 30 maxl)))
                                          :resizable nil :maximize nil :minimize nil
                                          :bg-color *om-light-gray-color*))
      (om-with-delayed-update *help-window*
	(loop for h in helplist for i = 0 then (+ i 1) do
	     (let ((v (om-make-view 'om-view :size (om-make-point panew (+ 4 (* 30 maxl))) 
				    :position (om-make-point (+ (* i (+ 4 panew)) 4) 4)
				    :bg-color (om-make-color 0.9 0.9 0.9))))
	       (om-add-subviews *help-window* v)
	       (om-with-delayed-update v
		 (loop for elt in h for j = 0 then (+ j 1) do 
		      (om-add-subviews v (om-make-view 'help-item-view 
						       :size (om-make-point (- panew 8) 26) 
						       :position (om-make-point 4 (+ 4 (* j 30)))
						       :pict  (car elt) :text (cadr elt)
						       :bg-color (om-make-color 0.9 0.9 0.9)
						       )))))
	     )))
    #-linux (om-add-menu-to-win *help-window*)
    ))
