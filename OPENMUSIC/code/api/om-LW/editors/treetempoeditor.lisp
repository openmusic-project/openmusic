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

;;Tree/Tempo editor widget

(in-package :om-edit)

(defvar *om-tree-tempo-editor* 'om-tree-tempo-editor)

(defclass  om-tree-tempo-editor (capi::interface)
  ((ep :initform nil :accessor ep)
   ;(score :initform t :accessor score)
   ;(sel :initform *editor-text* :accessor sel)
   ;(intfunc :initform t :accessor intfunc)
   (lisp-editor? :initform t :accessor lisp-editor? :initarg :lisp-editor?)
   )
  (:default-initargs
   :title "OBJ EDITOR"
   :buffer-modes '("Lisp")
   :echo-area t
   :auto-menus nil
   :layout (make-instance 'capi::grid-layout
                          :right-extend :bottom-extend
                          :y-adjust :center
                          :columns 2)
   ))



(defun open-tree-tempo-editor (panel tree tempo)
  ;;main window

  (setf main-win (make-instance *om-tree-tempo-editor*
                                :name (string (gensym))
                                :parent (capi:convert-to-screen)
                                :display-state :normal
                                :title "EDITOR"
                                :list-editor? t))
  
  (setf  tree-win (make-instance *omtree-editor*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :display-state :normal
                           :title  "TREE:"
                           :score panel
                           :sel tree
                           :lisp-editor? t))

  (setf (ep tree-win) tree-win)
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
                       :visible-min-width '(character 40)
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
                                                       :title "Save Tree As...."
                                                       :callback-type :interface
                                                       :callback 'tree-save-as
                                                       :accelerator #\s)
                                        (make-instance 'capi::menu-item
                                                       :title "Save Tempo As...."
                                                       :callback-type :interface
                                                       :callback 'tempo-save-as
                                                       :accelerator #\S)))
                        (make-instance 'capi::menu-component
                                       :items 
                                       (list
                                        (make-instance 'capi::menu-item
                                                       :title "Import Tree From..."
                                                       :callback-type :interface
                                                       :callback 'import-tree
                                                       :accelerator #\i)
                                        (make-instance 'capi::menu-item
                                                       :title "Import Tempo From..."
                                                       :callback-type :interface
                                                       :callback 'import-tempo
                                                       :accelerator #\I)
                                        ))
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
                                                             :callback 'change-tree-tempo-edit-font
                                                             :accelerator nil
                                                             ))))))

   ;build menus in win

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;TEMPO EDITOR
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (setf tempo-win (make-instance *omtempo-editor*
                           :name (string (gensym))
                           :parent (capi:convert-to-screen)
                           :display-state :normal
                           :title  "TEMPO:"
                           :score panel
                           :sel tempo
                           :lisp-editor? t))

  (setf (ep tempo-win) tempo-win)
  (setf tempo-echo-area 
        (make-instance 'capi::echo-area-pane :max-height t))
   

  ;panes and buttons
  (setf tempo-editor-pane 
        (make-instance 'capi::editor-pane
                       ;:flag 'minimal-example
                       :text *tempo-editor-text*
                       :font *def-tree-edit-font* 
                       :enabled t
                       :buffer-name :temp
                       :echo-area-pane tempo-echo-area
                       :visible-min-width '(character 40)
                       :visible-min-height '(character 10)
                       :change-callback 'tempo-update-callback
                       ))
  (setf tempo-def-buttons
        (make-instance 'capi::push-button-panel
                       :items '("Beginning Of Buffer" 
                                "End Of Buffer"
                                "Kill Line"
                                "Undo"
                                )
                       :callback-type :data
                       :selection-callback #'(lambda (command)
                                               (capi:call-editor tempo-editor-pane  command))));;
 
  (setf tempo-set-button 
        (make-instance 'capi::push-button
                       :text "Set"
                       :data *tempo-editor-text* ;:push-button
                       :callback-type :data-interface
                       :selection-callback 'tempo-button-set-selection-callback
                       ))

 
  
   ;;
   ;build menus in win
   (setf (ep main-win) (list tree-win tempo-win))
   (setf  (capi::interface-menu-bar-items main-win)
         (list file-menu edit-menu))
   
   ;;
   ;layouts
   (setf editor-layout (make-instance 'capi::column-layout
                                      :description (list tree-editor-pane def-buttons)))
   (setf tempo-editor-layout (make-instance 'capi::column-layout
                                      :description (list tempo-editor-pane tempo-def-buttons)))


    (setf but-layout1 (make-instance 'capi::row-layout
                                   :y-adjust :center
                                   :description  (list set-button close-button)))
    
    (setf tempo-but-layout1 (make-instance 'capi::row-layout
                                     :y-adjust :center
                                     :description  (list tempo-set-button)))
        

  (setf  (capi::layout-description (capi:pane-layout tree-win)) (list editor-layout but-layout1))
  (setf  (capi::layout-description (capi:pane-layout tempo-win)) (list tempo-editor-layout tempo-but-layout1))  
  (setf (capi::layout-description (capi:pane-layout main-win)) (list tree-win tempo-win))
  


  (capi::display main-win)
  main-win)
      
(defmethod capi:interface-keys-style ((self om-tree-tempo-editor))
 #+linux :emacs
 #+macosx :mac
 #+win32 :win)


;in order to have colored parenthesis:

(defmethod capi:interface-display :after ((win om-tree-tempo-editor))
  (with-slots (ep) main-win
    (capi::execute-with-interface main-win
                                  #'(lambda ()
                                      (when (lisp-editor? main-win) 
                                        (capi:call-editor tree-editor-pane
                                                          (list 'editor:lisp-mode-command 
                                                                (capi:editor-pane-buffer tree-editor-pane)))
                                        (capi:call-editor tempo-editor-pane
                                                          (list 'editor:lisp-mode-command 
                                                                (capi:editor-pane-buffer tempo-editor-pane)))
                                      (om-lisp::echo-string main-win "")
                                        )))
    ;for indentation
    (let ((treebuffer (capi::editor-pane-buffer tree-editor-pane))
          (tempobuffer (capi::editor-pane-buffer tempo-editor-pane)))
       (editor::lisp-indent-region-for-commands
        (editor:buffers-start treebuffer)
        (editor:buffers-end treebuffer))
       
       (editor::lisp-indent-region-for-commands
        (editor:buffers-start tempobuffer)
        (editor:buffers-end tempobuffer))
       )
     ))



;menu's callbacks

;;import

(defun import-tree (interface &optional path)
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


(defun import-tempo (interface &optional path)
  (with-slots (ep) interface
    (when-let* ((path (or path (capi:prompt-for-file "Open File:" 
                                                     :if-does-not-exist :error 
                                                     :filter "*.*"
                                                     :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (current (capi:editor-pane-buffer tempo-editor-pane))
                (newbuffer (om-lisp::with-safe-file-operation interface path "open" nil 
                             (editor:find-file-buffer path)))
                (newtext (editor::use-buffer newbuffer
                           (editor:points-to-string 
                            (editor:buffers-start newbuffer) 
                            (editor:buffers-end newbuffer)))))
      (editor:process-character 
       (list #'(setf capi:editor-pane-text)
             newtext
             tempo-editor-pane)
       (capi:editor-window tempo-editor-pane))
      )))

;;;;save as


(defun tree-save-as (interface)
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
        (setf (sel (car ep)) *editor-text*)
        ))))

(defun tempo-save-as (interface)
  (with-slots (ep) interface
    (let ((current (capi:editor-pane-buffer tempo-editor-pane)))
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
        (setf *tempo-editor-text* (capi:editor-pane-text tempo-editor-pane))
        (setf (sel (second ep)) *tempo-editor-text*)
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


(defun change-tree-tempo-edit-font (interface)
  (with-slots (ep) interface
    (let ((settings (capi::prompt-for-font "" :font (capi::simple-pane-font tree-editor-pane))))
      (setf (capi::simple-pane-font tree-editor-pane) 
            (setf *def-tree-edit-font* settings))
      (setf (capi::simple-pane-font tempo-editor-pane) 
            (setf *def-tree-edit-font* settings)))))

      