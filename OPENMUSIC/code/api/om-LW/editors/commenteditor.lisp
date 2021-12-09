;;===========================================================================
;LW Editors 
;Interface tools for editing
;;===========================================================================

;===========================================================================
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; See file LICENSE for further informations on licensing terms.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Author: Karim Haddad
;;===========================================================================

;;===========================================================================
;DocFile
;This file loads the LW Editors
;;===========================================================================

(in-package :om-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; COMMENT EDITOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *om-comment-editor* 'om-comment-editor)
(setf *om-comment-editor* 'om-comment-editor)
(defvar *comment-font* nil)
(defvar *def-comment-edit-font* 
  #+linux(gp::make-font-description :family "Liberation Mono" :size 9)
  #-linux(gp::make-font-description :family "Consolas" :size 11))
(defvar *comment-color* (color:make-rgb 1 1 1))
(defvar *comment-text*
    "no comment")


(defclass om-comment-editor (capi::interface)
  ((ep :initform nil :accessor ep)
   (panel :initform t :accessor panel)
   (mycomment :initform *comment-text* :accessor mycomment)
   (intfunc :initform t :accessor intfunc)
   (fontfunc :initform t :accessor fontfunc)
   (fontcolfunc :initform t :accessor fontcolfunc))
  (:default-initargs
   :title "Comment Editor"
   :buffer-modes '("Lisp")
   :echo-area t
   :auto-menus nil
   :layout (make-instance 'capi::grid-layout
                          :right-extend :bottom-extend
                          :y-adjust :center
                          :columns 1)
   ))



(defun  open-comment-editor-pane (panel)
  ;(setf *comment-text* "") ;deplace dans make-comment-box
  ;;main window
  (setf win (make-instance *om-comment-editor*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :display-state :normal
                           :title  "Add comment..."
                           :panel panel
                         ;  :mycomment comment-text
                           :lisp-editor? t))
  (setf (ep win) win)
  (setf (panel win) panel)
  (setf echo-area 
        (make-instance 'capi::echo-area-pane :max-height t))
   

  ;panes and buttons
  (setf comment-editor-pane 
        (make-instance 'capi::editor-pane
                      ; :flag 'minimal-example
                       :text *comment-text*
                       :font *def-comment-edit-font* 
                       :foreground *comment-color*
                       :background :yellow1 ;:whitesmoke
                       :enabled t
                       :buffer-name :temp 
                       :echo-area-pane echo-area
                       :visible-min-width '(character 30)
                       :visible-min-height '(character 10)
                       :change-callback 'update-callback
                       ))
#|
  (setf def-buttons
        (make-instance 'capi::push-button-panel
                       :items '("Beginning Of Buffer" 
                                "End Of Buffer"
                                "Kill Line"
                                "Undo"
                                )
                       :callback-type :data
                       :selection-callback #'(lambda (command)
                                               (capi:call-editor comment-editor-pane  command))));;
 |#
  (setf set-button 
        (make-instance 'capi::push-button
                       :text "Set"
                       :data *comment-text* ;:push-button
                       :callback-type :data-interface
                       :callback 'button-set-comment-callback ;selection-callback
                       ))
  
  (setf edit-button 
        (make-instance 'capi::push-button
                       :text "Edit"
                       :data *comment-text* ;:push-button
                       :callback-type :data-interface
                       :callback 'button-edit-comment-callback ;selection-callback
                       ))
  
  (setf close-button
        (make-instance 'capi:push-button
        :text "Close"
        :data *comment-text* ;:push-button
        :callback-type :data-interface
        :selection-callback 'button-close-callback
        ))

  
  
   ;menubar
  (setf file-menu
        (make-instance 'capi::menu
                       :title "File"
                       :items 
                       (list 
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Save As...."
                                                       :callback-type :interface
                                                       :callback 'save-comment-as-text-file
                                                       :accelerator #\s)))
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Import Text..."
                                                       :callback-type :interface
                                                       :callback 'import-text-from-file
                                                       :accelerator #\i)))
                        )))   

   (setf edit-menu
         (make-instance 'capi::menu
                        :title "Edit"
                        :items 
                        (list (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Undo"
                                                             :callback-type :interface
                                                             :callback 'comment-edit-undo
                                                             :accelerator #\z
                                                             )))
                              (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Cut"
                                                             :callback-type :interface
                                                             :callback 'comment-edit-cut
                                                             :accelerator #\x)
                                              (make-instance 'capi::menu-item
                                                             :title "Copy"
                                                             :callback-type :interface
                                                             :callback 'comment-edit-copy
                                                             :accelerator #\c)
                                              (make-instance 'capi::menu-item
                                                             :title "Paste"
                                                             :callback-type :interface
                                                             :callback 'comment-edit-paste
                                                             :accelerator #\v)))
                              (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Select All"
                                                             :callback-type :interface
                                                             :callback 'comment-select-all
                                                             :accelerator #\a
                                                             )))
                              (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Text Font"
                                                             :callback-type :interface
                                                             :callback 'change-comment-edit-font
                                                             :accelerator #\f
                                                             )
                                             (make-instance 'capi::menu-item
                                                             :title "Text Color"
                                                             :callback-type :interface
                                                             :callback 'change-comment-font-color
                                                             :accelerator #\C
                                                             ))))))
   (setf actions-menu
        (make-instance 'capi::menu
                       :title "Actions"
                       :items 
                       (list 
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Set Comment Text..."
                                                       :callback-type :data-interface
                                                       :callback  'button-set-comment-callback 
                                                       :accelerator #\q)))
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Edit Comment Text..."
                                                       :callback-type :data-interface
                                                       :callback  'button-edit-comment-callback 
                                                       :accelerator #\e)))
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Close"
                                                       :callback-type :data-interface
                                                       :selection-callback 'button-close-callback
                                                       ;:callback 'button-close-callback
                                                       :accelerator #\w)))
                        )))

   ;build menus in win
   (setf (capi::interface-menu-bar-items win)
         (list file-menu edit-menu actions-menu))
   ;layouts
   (setf editor-layout (make-instance 'capi::column-layout
                                      :description (list comment-editor-pane 
                                                         ;def-buttons
                                                         )))

    (setf but-layout1 (make-instance 'capi::row-layout
                                   :y-adjust :center
                                   :description  (list set-button edit-button close-button)))

  (setf  (capi::layout-description (capi:pane-layout win)) (list editor-layout but-layout1))
   (capi::display win)
   win)

(defmethod capi:interface-keys-style ((self om-comment-editor))
 #+linux :emacs
 #+macosx :mac
 #+win32 :win)

;(open-comment-editor-pane)


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------


;button's callbacks
;set button

(defun comment-button-action (type data interface)
  (with-slots (ep) interface 
    (setf *comment-text* (capi:editor-pane-text comment-editor-pane))
    (setf (mycomment interface) *comment-text*)
    (oa::om-set-dialog-item-text (panel interface)  *comment-text*)
    (apply (intfunc interface) (list (panel interface) *comment-text*))
    (apply (fontfunc interface) (list (panel interface) *def-comment-edit-font*))
    (apply (fontcolfunc interface) (list (panel interface) *comment-color*))
    ))

(defun button-edit-comment-callback (&rest args)
  (apply 'comment-button-action  "edit comment" args))

(defun button-set-comment-callback (&rest args)
  (apply 'comment-button-action  "set comment" args)
  (apply 'close-button-action "closed" args)
  )

;menu's callbacks

;;import

(defun import-text-from-file (interface &optional path)
  (with-slots (ep) interface
    (when-let* ((path (or path (capi:prompt-for-file "Open File:" 
                                                     :if-does-not-exist :error 
                                                     :filter "*.*"
                                                     :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (current (capi:editor-pane-buffer comment-editor-pane))
                (newbuffer (om-lisp::with-safe-file-operation interface path "open" nil 
                             (editor:find-file-buffer path)))
                (newtext (editor::use-buffer newbuffer
                           (editor:points-to-string 
                            (editor:buffers-start newbuffer) 
                            (editor:buffers-end newbuffer)))))
      (editor:process-character 
       (list #'(setf capi:editor-pane-text)
             newtext
             comment-editor-pane)
       (capi:editor-window comment-editor-pane))
      )))



(defun save-comment-as-text-file (interface)
  (with-slots (ep) interface
    (let ((current (capi:editor-pane-buffer comment-editor-pane)))
      (when-let (path (capi:prompt-for-file "Save file as:"
                                   ;:pathname (editor-file interface)
                                   :if-does-not-exist :ok
                                   :filter "*.*"
                                   :if-exists :prompt
                                   :operation :save))
        (om-lisp::save-to-file (editor:points-to-string 
                         (editor:buffers-start current) 
                         (editor:buffers-end current))
                      path interface)
        (setf *comment-text* (capi:editor-pane-text comment-editor-pane))
        (setf (mycomment interface) *comment-text*)
        ))))

;;;;


(defun change-comment-edit-font (interface)
  (with-slots (ep) interface
    (setf (capi::simple-pane-font comment-editor-pane) 
          (setf *def-comment-edit-font*
                 (gp::font-description 
                  (capi::prompt-for-font "" :font (capi::simple-pane-font comment-editor-pane)))))
    ))

(defun change-comment-font-color (interface)
  (with-slots (ep) interface
    (setf (capi::simple-pane-foreground comment-editor-pane) 
          (setf *comment-color*
                (capi::prompt-for-color "Choose a color" :color (capi::simple-pane-foreground comment-editor-pane))))
    ))
;;;;


(defun comment-edit-paste (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer comment-editor-pane)))
      (capi:call-editor comment-editor-pane (list 'editor::insert-cut-buffer-command buffer))
    )))

(defun comment-edit-copy (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer comment-editor-pane)))
      (capi:call-editor comment-editor-pane (list 'editor::copy-to-cut-buffer-command buffer))
    )))

(defun comment-edit-cut (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer comment-editor-pane)))
      (capi:call-editor comment-editor-pane (list 'editor::copy-to-cut-buffer-command buffer))
      (capi:call-editor comment-editor-pane (list 'editor::kill-region-command buffer))
    )))

(defun comment-select-all (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer comment-editor-pane)))
      (editor::use-buffer buffer
        (capi:call-editor comment-editor-pane (list 'editor::mark-whole-buffer-command buffer))))))
  
(defun comment-edit-undo (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer comment-editor-pane)))
      (capi:call-editor comment-editor-pane (list 'editor::undo-command buffer)))))


