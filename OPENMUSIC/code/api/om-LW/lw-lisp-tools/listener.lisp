;;===========================================================================
;LW Lisp Tools 
;Lisp programming tools for LispWorks delivered applications
;;===========================================================================

;;===========================================================================
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Author: Jean Bresson
;Contributions: Nicholas Ellis
;;===========================================================================

;;===========================================================================
; DocFile
; LISTENER
;;===========================================================================
;; Lisp Linener for delivered LispWorks appliations
;; J. Bresson

(in-package :om-lisp)

;;;===================
;;; export :
;;;===================
(export '(om-listener
          om-make-new-listener
          om-listener-echo
          om-listener-abort
          *om-stream*
          ) :om-lisp)

;;;=============================
;;; LISTENER
;;;=============================

(defvar *om-app* nil)

(defvar *om-listener* nil)
(defvar *om-stream* nil)
(defvar *om-prompt* "OM >")

;; (defclass om-listener-pane (capi:listener-pane capi:collector-pane) ())

(defun init-listener ()
  (setq *listener-font* (or *listener-font* (gp::make-font-description :family "Verdana" :size 8 :weight :bold)))
  (setq *om-stream* (capi:collector-pane-stream (make-instance 'capi:collector-pane)))
  (setf *trace-output* *om-stream*)
  (let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil))
    (defun print (something &optional stream)
      (declare (ignore stream))
      (format *om-stream* "~D ~D~%" *om-prompt* something)
      (when om-lisp::*om-listener* (listener-end-of-buffer om-lisp::*om-listener*))
      something
      ))
  )



(defclass om-listener (capi::interface) 
  ((ip :accessor ip :initarg :ip)
   (op :accessor op :initarg :op)))


(defclass om-listener-in-pane (capi::listener-pane) ())
(defclass om-listener-out-pane (capi::collector-pane) ())

#-linux (defvar *listener-font* nil)
#+linux (defvar *listener-font* (gp::make-font-description :family #+linux "Liberation Mono" :size 9))

;; do not work on mac...


(defparameter *listener-on-top* nil)

(defun listener-styles () 
  (append (if *listener-on-top* (list :always-on-top))
          (list ;:toolbox 
           :no-character-palette
           ;:shadowless
           ;:internal-borderless
           ;:textured-background  
           )))

;(setf om-lisp::*om-listener* nil)
(defun make-om-listener (&key title x y width height initial-lambda input)
  (or (and om-lisp::*om-listener* (capi::find-interface 'om-listener))
      (progn
        (init-listener)
        (setf om-lisp::*om-listener* 
              (let* ((in 
                      (when input 
                        (make-instance 'om-listener-in-pane
                                        ;:echo-area t
                                       :font *listener-font*
                                       :create-callback (if initial-lambda
                                                            (lambda (window) 
                                                              (declare (ignore window)) 
                                                              (capi:execute-with-interface *om-listener* initial-lambda))
                                                          (lambda (window)
                                                            (declare (ignore window))
                                                            (capi:execute-with-interface *om-listener* (lambda () (in-package :om-lisp))))))))
                     (out (make-instance 'om-listener-out-pane :stream *om-stream* :echo-area t :font *listener-font*))
                     (win (make-instance 'om-listener
                             :layout (make-instance 'capi:column-layout 
                                                    :description (if in (list in :divider out) (list out)) 
                                                    :ratios (if in '(1 nil 4) '(1)))
                             :window-styles (listener-styles)
                             :ip in :op out
                             :title (or title "OM Listener")
                             :best-x (or x 100)
                             :best-y (or y (round (- (capi::screen-height (capi:convert-to-screen)) 250)))
                             :best-width (or width 360) :best-height (or height 200)
                             :destroy-callback (lambda (window) (setf om-lisp::*om-listener* nil))
                             #+macos :activate-callback 
                             #+macos(lambda (window activatep) 
                               (when activatep (setf (capi::interface-menu-bar-items window)(internal-window-class-menubar window))))
                             )))
                (setf (capi::simple-pane-font (capi::editor-pane-echo-area (op win))) *listener-font*)
                win))
        
        (setf (capi::interface-menu-bar-items om-lisp::*om-listener*)
              (internal-window-class-menubar om-lisp::*om-listener*))
        ;(format *om-stream* "=======~%")
        (capi::display om-lisp::*om-listener*)
        )))




(defmethod internal-window-class-menubar ((self om-listener)) 
  (append (list (make-instance 'capi::menu :title "File"
                               :items 
                               (append (list (make-instance 'capi::menu-item :title "Close Listener"
                                                            :callback-type :interface
                                                            :callback 'quit-interface
                                                            :accelerator #\w))
                                     
                                       (if (handler-case (find-class 'om-lisp::om-text-editor) (error () nil))
                                           (list (make-instance 'capi::menu-component 
                                                                :items (list 
                                                                        (make-instance 'capi::menu-item :title "New..."
                                                                                       :callback-type :interface
                                                                                       :callback 'listener-new-text-editor
                                                                                       :accelerator #\n
                                                                                       :enabled-function 'file-operations-enabled)
                                                                        (make-instance 'capi::menu-item :title "Open..."
                                                                                       :callback-type :interface
                                                                                       :callback 'listener-new-text-file
                                                                                       :accelerator #\o
                                                                                       :enabled-function 'file-operations-enabled)))))
                                       
                                       ))
                (make-instance 'capi::menu :title "Edit"
                               :items (list (make-instance 'capi::menu-component 
                                                           :items (list 
                                                                   (make-instance 'capi::menu-item :title "Cut"
                                                                                  :callback-type :interface
                                                                                  :callback 'listener-cut
                                                                                  :accelerator #\x)
                                                                   (make-instance 'capi::menu-item :title "Copy"
                                                                                  :callback-type :interface
                                                                                  :callback 'listener-copy
                                                                                  :accelerator #\c)
                                                                   (make-instance 'capi::menu-item :title "Paste"
                                                                                  :callback-type :interface
                                                                                  :callback 'listener-paste
                                                                                  :accelerator #\v)))
                                            (make-instance 'capi::menu-component 
                                                           :items (list 
                                                                   (make-instance 'capi::menu-item :title "Select All" 
                                                                                  :callback 'listener-select-all 
                                                                                  :accelerator #\a
                                                                                  :callback-type :interface)
                                                                               
                                                                   ))
                                            
                                            (make-instance 'capi::menu-item :title "Text Font" 
                                                           :callback 'change-listener-font 
                                                           :accelerator nil
                                                           :callback-type :interface)))
                (make-instance 'capi::menu :title "Lisp"
                               :items (list 
                                       (make-instance 'capi::menu-component 
                                                      :items (list 
                                                              (make-instance 'capi::menu-item :title "Find Definition"
                                                                             :callback-type :interface
                                                                             :callback 'listener-find-definition
                                                                             :enabled-function 'lisp-operations-enabled
                                                                             :accelerator #\.)
                                                              (make-instance 'capi::menu-item :title "Abort"
                                                                             :callback-type :interface
                                                                             :callback 'listener-abort
                                                                                    ;:enabled-function 'lisp-operations-enabled
                                                                             :accelerator #\A)
                                                              (make-instance 'capi::menu-item :title "Last Error Backtrace"
                                                                             :callback-type :interface
                                                                             :callback 'listener-error-backtrace
                                                                             :enabled-function 'backtrace-enabled
                                                                             :accelerator #\B)))
                                              
                                       (make-instance 'capi::menu-component 
                                                      :items (list 
                                                              (make-instance 'capi::menu-item :title "Load File..."
                                                                             :callback-type :interface
                                                                             :callback 'load-a-lisp-file
                                                                             :accelerator nil
                                                                             :enabled-function 'file-operations-enabled)
                                                              ))
                                              
                                       )))
          (om-window-class-menubar om-lisp::*om-listener*)))


(defun close-listener ()
  (om-close-window om-lisp::*om-listener*))

;(om-add-exit-cleanup-func 'close-listener t)


;;; redefinie dans OM
(defmethod om-window-class-menubar ((self om-listener)) ())


(defun listener-new-text-file (listenerwin &optional path)
  (om-open-new-text-file path))

(defun listener-new-text-editor (listenerwin)
  (om-open-new-text-editor nil))

(defun find-capi-pane-with-focus (layout)
  (capi:map-pane-descendant-children 
   layout
   #'(lambda (p)
       (when (capi:pane-has-focus-p p)
         (return-from find-capi-pane-with-focus p)))))

(defun listener-paste (listenerwin)
  (let* ((pane (find-capi-pane-with-focus (pane-layout listenerwin)))
         (buffer (editor-pane-buffer pane)))
    (call-editor pane (list 'editor::insert-cut-buffer-command buffer))))

(defun listener-copy (listenerwin)
  (let* ((pane (find-capi-pane-with-focus (pane-layout listenerwin)))
         (buffer (editor-pane-buffer pane)))
    (call-editor pane (list 'editor::copy-to-cut-buffer-command buffer))))

(defun listener-cut (listenerwin)
  (let* ((pane (find-capi-pane-with-focus (pane-layout listenerwin)))
         (buffer (editor-pane-buffer pane)))
    (call-editor pane (list 'editor::copy-to-cut-buffer-command buffer))
    (call-editor pane (list 'editor::kill-region-command buffer))))

(defun listener-select-all (listenerwin)
  (let* ((pane (find-capi-pane-with-focus (pane-layout listenerwin)))
         (buffer (editor-pane-buffer pane)))
    (editor::use-buffer buffer
      (editor::with-point ((p (editor::buffer-point buffer)))
        (call-editor pane (list 'editor::beginning-of-buffer-cancelling-selection-command buffer))
        #+cocoa(call-editor pane (list 'editor::end-of-buffer-extending-selection-command buffer))
        #-cocoa(call-editor pane (list 'editor::end-of-buffer-modifying-selection-command buffer))
        ))))

(defun listener-end-of-buffer (listenerwin)
  (let* ((pane (op listenerwin))
         (buffer (editor-pane-buffer pane)))
    (capi::apply-in-pane-process pane
     #'(lambda (pa bu) 
         (editor::use-buffer bu
           (editor::with-point ((p (editor::buffer-point bu)))
             (call-editor pa (list 'editor::end-of-buffer-command bu))
             ))) 
     pane buffer)))


(defun om-make-new-listener (&key title x y width height initial-lambda (input t))
  (make-om-listener :title title :x x :y y :width width :height height :initial-lambda initial-lambda :input input))

(defvar *fasl-extension* (pathname-type (cl-user::compile-file-pathname "")))
(defvar *last-open-directory* nil)

(defmethod load-a-lisp-file ((self t))
  (let ((filename (capi::prompt-for-file "Choose a File to Load..." 
                                         :filters (list "All files" "*.*" "Lisp File" "*.lisp" "Compiled Lisp File" 
                                                      (concatenate 'string "*." *fasl-extension*))
                                         :pathname *last-open-directory*)))
    (when filename
      (if (probe-file filename)
          (progn 
            (setf *last-open-directory* (make-pathname :directory (pathname-directory filename)))
            (load filename)
            (print (concatenate 'string "File " (namestring filename) " loaded."))
            )
        (progn 
          (beep-pane nil)
          (print (concatenate 'string "File " (namestring filename) " not found."))
          ))
      )))

(defun listener-find-definition (listenerwin)
 (with-slots (ep) listenerwin
    (let* ((pane (find-capi-pane-with-focus (pane-layout listenerwin)))
          (buffer (editor-pane-buffer pane))
          (symbol nil))
      (editor::use-buffer buffer
        (setf symbol (editor::intern-symbol-from-string (editor::read-symbol-from-point :previous t :read-package-name t)))
        ;(print (list symbol (type-of symbol)))
        (when symbol (om-lisp::om-edit-definition symbol))
      ))))


(defun om-listener-echo (str)
  (when om-lisp::*om-listener*
    (capi:execute-with-interface om-lisp::*om-listener*
                                 #'(lambda ()
                                     (with-slots (op) om-lisp::*om-listener*
                                       (with-slots (editor-window) op
                                         (editor:process-character  
                                          (list 'editor:message str)
                                          editor-window)))))))

(defun listener-abort (listenerwin)
  (declare (ignore listenerwin))
  (om-abort-eval-process)
  (om-listener-echo "Aborted"))

(defun backtrace-enabled (listenerwin) *error-backtrace*)

(defun listener-error-backtrace (listenerwin) (om-show-error-backtrace))


(defmethod change-listener-font ((self om-listener))
  (with-slots (ip op) self
    (let ((newfont (capi::prompt-for-font "" :font (capi::simple-pane-font op))))
      (setf *listener-font* newfont)
      (when ip (setf (capi::simple-pane-font ip) *listener-font*))
      (setf (capi::simple-pane-font op) *listener-font*)
      (setf (capi::simple-pane-font (capi::editor-pane-echo-area op)) *listener-font*)
      )))


