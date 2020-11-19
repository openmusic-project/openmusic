;;===========================================================================
;LW Score Editors 
;Interface tools for score editing and inspection 
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
;This file loads the LW Score Editors
;;===========================================================================

;;Tree editor widget

(in-package :om-edit)

;;--------------------------------------------------------------------------
;;
;;GUI
;;
;;---------------------------------------------------------------------------

(defvar *omtree-editor* 'om-tree-editor)
(defvar *editor-text* nil)
(defvar *tree-font* nil)
(defvar *def-tree-edit-font* 
  #+linux(gp::make-font-description :family "Courier" :size 10)
  #-linux(gp::make-font-description :family "Consolas" :size 11))



(defclass  om-tree-editor (capi::interface)
  ((ep :initform nil :accessor ep)
   (score :initform t :accessor score)
   (sel :initform *editor-text* :accessor sel)
   (intfunc :initform t :accessor intfunc)
   (lisp-editor? :initform t :accessor lisp-editor? :initarg :lisp-editor?)
   )
  (:default-initargs
   :title "TREE EDITOR"
   :buffer-modes '("Lisp")
   :echo-area t
   :auto-menus nil
   :layout (make-instance 'capi::grid-layout
                          :right-extend :bottom-extend
                          :y-adjust :center
                          :columns 1)
   ))

  
(defun open-tree-editor (panel tree)
  ;;main window
  (setf win (make-instance *omtree-editor*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :display-state :normal
                           :title  "TREE EDITOR"
                           :score panel
                           :sel tree
                           :lisp-editor? t))

  (setf (ep win) win)
  (setf echo-area 
        (make-instance 'capi::echo-area-pane :max-height t))
   

  ;panes and buttons
  (setf tree-editor-pane 
        (make-instance 'capi::editor-pane
                      ; :flag 'minimal-example
                       :text *editor-text*
                       :font *def-tree-edit-font* 
                       :enabled t
                       :buffer-name :temp 
                       :echo-area-pane echo-area
                       :visible-min-width '(character 60)
                       :visible-min-height '(character 10)
                       :change-callback 'update-callback
                       ))
  (setf def-buttons
        (make-instance 'capi::push-button-panel
                       :items '("Beginning Of Buffer" 
                                "End Of Buffer"
                                "Kill Line"
                                "Undo"
                                )
                       :callback-type :data
                       :selection-callback #'(lambda (command)
                                               (capi:call-editor tree-editor-pane  command))));;
 
  (setf set-button 
        (make-instance 'capi::push-button
                       :text "Set"
                       :data *editor-text* ;:push-button
                       :callback-type :data-interface
                       :callback 'button-set-selection-callback ;selection-callback
                       ))

  
  (setf close-button
        (make-instance 'capi:push-button
        :text "Close"
        :data *editor-text* ;:push-button
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
                                                       :callback 'tree-save-as-text-file
                                                       :accelerator #\s)))
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Import From..."
                                                       :callback-type :interface
                                                       :callback 'import-tree-from-file
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
                                                             :callback 'tree-edit-undo
                                                             :accelerator #\z
                                                             )))
                              (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Cut"
                                                             :callback-type :interface
                                                             :callback 'tree-edit-cut
                                                             :accelerator #\x)
                                              (make-instance 'capi::menu-item
                                                             :title "Copy"
                                                             :callback-type :interface
                                                             :callback 'tree-edit-copy
                                                             :accelerator #\c)
                                              (make-instance 'capi::menu-item
                                                             :title "Paste"
                                                             :callback-type :interface
                                                             :callback 'tree-edit-paste
                                                             :accelerator #\v)))
                              (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Select All"
                                                             :callback-type :interface
                                                             :callback 'tree-select-all
                                                             :accelerator #\a
                                                             )))
                              (make-instance 'capi::menu-component
                                             :items 
                                             (list
                                              (make-instance 'capi::menu-item
                                                             :title "Text Font"
                                                             :callback-type :interface
                                                             :callback 'change-tree-edit-font
                                                             :accelerator nil
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
                                                       :title "Set Rhythm Tree"
                                                       :callback-type :data-interface
                                                       :callback  'button-set-selection-callback 
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
                                      :description (list tree-editor-pane def-buttons)))

    (setf but-layout1 (make-instance 'capi::row-layout
                                   :y-adjust :center
                                   :description  (list set-button close-button)))

  (setf  (capi::layout-description (capi:pane-layout win)) (list editor-layout but-layout1))
   (capi::display win)
   win)
      

;(open-tree-editor)


(defmethod capi:interface-keys-style ((self om-tree-editor))
 #+linux :emacs
 #+macosx :mac
 #+win32 :win)


;in order to have colored parenthesis:

(defmethod capi:interface-display :after ((win om-tree-editor))
  (with-slots (ep) win
    (capi::execute-with-interface win
                                  #'(lambda ()
                                      (when (lisp-editor? win) 
                                        (capi:call-editor tree-editor-pane
                                                          (list 'editor:lisp-mode-command 
                                                                (capi:editor-pane-buffer tree-editor-pane)))
                                      (om-lisp::echo-string win "")
                                        )))
    ;for indentation
    (let ((buffer (capi::editor-pane-buffer tree-editor-pane)))
       (editor::lisp-indent-region-for-commands
        (editor:buffers-start buffer)
        (editor:buffers-end buffer))
       )
     ))
  




;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun update-callback (pane point old-length new-length)
t)


;button's callbacks
;set button

(defun report-button-action (type data interface)
  (with-slots (ep) interface 
    (setf *editor-text* (capi:editor-pane-text tree-editor-pane))
    (setf (sel interface) *editor-text*)
    (apply (intfunc interface) (list (score interface) *editor-text*))
    ;(set-tree (score interface) *editor-text*)
    ))

(defun button-set-selection-callback (&rest args)
  (apply 'report-button-action  "set tree" args)
  )


;close button

(defun close-button-action (type data interface)
  (with-slots (ep) interface 
    (capi:quit-interface ep)  
    ))

(defun button-close-callback (&rest args)
  (apply 'close-button-action "closed" args)
  )

;menu's callbacks

;;import

(defun import-tree-from-file (interface &optional path)
  (with-slots (ep) interface
    (when-let* ((path (or path (capi:prompt-for-file "Open File:" 
                                                     :if-does-not-exist :error 
                                                     :filter "*.*"
                                                     :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (current (capi:editor-pane-buffer tree-editor-pane))
                (newbuffer (om-lisp::with-safe-file-operation interface path "open" nil 
                             (editor:find-file-buffer path)))
                (newtext (editor::use-buffer newbuffer
                           (editor:points-to-string 
                            (editor:buffers-start newbuffer) 
                            (editor:buffers-end newbuffer)))))
      (editor:process-character 
       (list #'(setf capi:editor-pane-text)
             newtext
             tree-editor-pane)
       (capi:editor-window tree-editor-pane))
      )))



(defun tree-save-as-text-file (interface)
  (with-slots (ep) interface
    (let ((current (capi:editor-pane-buffer tree-editor-pane)))
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
        (setf *editor-text* (capi:editor-pane-text tree-editor-pane))
        (setf (sel interface) *editor-text*)
        ))))

;;;;

(defun tree-edit-paste (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer tree-editor-pane)))
      (capi:call-editor tree-editor-pane (list 'editor::insert-cut-buffer-command buffer))
    )))

(defun tree-edit-copy (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer tree-editor-pane)))
      (capi:call-editor tree-editor-pane (list 'editor::copy-to-cut-buffer-command buffer))
    )))

(defun tree-edit-cut (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer tree-editor-pane)))
      (capi:call-editor tree-editor-pane (list 'editor::copy-to-cut-buffer-command buffer))
      (capi:call-editor tree-editor-pane (list 'editor::kill-region-command buffer))
    )))

(defun tree-select-all (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer tree-editor-pane)))
      (editor::use-buffer buffer
        (capi:call-editor tree-editor-pane (list 'editor::mark-whole-buffer-command buffer))))))
  
(defun tree-edit-undo (interface)
  (with-slots (ep) interface
    (let ((buffer (capi:editor-pane-buffer tree-editor-pane)))
      (capi:call-editor tree-editor-pane (list 'editor::undo-command buffer)))))


(defun change-tree-edit-font (interface)
  (with-slots (ep) interface
    (setf (capi::simple-pane-font tree-editor-pane) 
          (setf *def-tree-edit-font*
                (capi::prompt-for-font "" :font (capi::simple-pane-font tree-editor-pane))))))
