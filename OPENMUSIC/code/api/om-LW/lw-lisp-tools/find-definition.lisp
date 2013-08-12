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
;;===========================================================================

;=======================
; find and edit source code for LW applications
;=======================


(in-package :om-lisp)

(export '(om-edit-definition
          restore-ompath) :om-lisp)

;=======================
; find and edit source code 
;=======================

 
; (when (setf layout (make-window-layout thewin ,bg-color)) (setf (pane-layout thewin) layout))


(defun show-definitions-dialog (symb deflist)
  (let* ((w 400) (h 300)
        (win (make-instance 'capi::interface
                             :title (concatenate 'string "Definitions of " (string-upcase (string symb)))
                             :name (gensym)
                             :width w :height h
                             :resizable nil
                             ;:window-styles nil
                             )))
    (set-hint-table win (list :external-min-width w :external-max-width w 
                              :external-min-height h :external-max-height h))
    (setf (pane-layout win) (make-instance 'pinboard-layout :internal-border nil :visible-border nil 
                                              #+cocoa :background #+cocoa :transparent))
    (apply-in-pane-process (pane-layout win)
                           (lambda (x) (setf (capi:layout-description (pane-layout x)) 
                                             (list
                                              (make-instance 'capi::list-panel
                                                             :x 10 :y 10
                                                             :width 370 :height 260
                                                             :interaction :single-selection
                                                             :retract-callback nil
                                                             :focus nil
                                                             :callback-type '(:collection)
                                                             :test-function 'string-equal
                                                             :items (loop for item in deflist collect (format nil "~A" (car item)))
                                                             :action-callback #'(lambda (list)
                                                                                  (let ((file (cadr (nth (capi::choice-selection list) deflist))))
                                                                                    (if (pathnamep file)
                                                                                        (if (probe-file file)
                                                                                            (om-open-new-text-editor-at file 
                                                                                                                        (car (nth (capi::choice-selection list) deflist)))
                                                                 (progn (beep-pane nil) 
                                                                   (print (format nil "File ~A not found" file))))
                                                             (progn (beep-pane nil) (print (format nil "Unknown location for ~A definition.." symb)))
                                                             )))
                                                             ))))
                           win)
    (capi::execute-with-interface win
                                 #'(lambda () (setf (capi::interface-menu-bar-items win) 
                                                    (list (make-instance 'capi::menu :title "File" 
                                                                          :items (list (make-instance 'capi::menu-item 
                                                                                                      :title "Close" 
                                                                                                      :accelerator "accelerator-w"
                                                                                                      :callback-type :interface  
                                                                                                      :callback #'(lambda (w) 
                                                                                                                    (capi::quit-interface w))
                                                                                                      )))))
                                     (capi::display win)             
                                     ))

    ))


;; to be redefined depending on actual source file location
  ;; (path records the original location at compiling the symbol
(defun restore-ompath (path) path)


(defun restore-definitions-pathnames (def-list)
  (loop for def in def-list collect
        (list (car def)
              (if (and (or (stringp (cadr def)) (pathnamep (cadr def))))
                  (let ((restored (restore-ompath (cadr def))))
                    (if (and (pathnamep restored) (probe-file restored))
                        (truename restored)
                      (cadr def)))
                (cadr def)))))

; (dspec:find-name-locations dspec:*dspec-classes* 'om::picture)


(defun om-edit-definition (symbol)
    (if (symbolp symbol)
      (let ((definitions 
             ;(dspec:name-definition-locations dspec:*dspec-classes* symbol)
             (ignore-errors (dspec:find-name-locations dspec:*dspec-classes* symbol))))
        (if definitions
            (progn   
              (setf definitions (restore-definitions-pathnames definitions))
              (cond 
               ((null definitions)
                (beep-pane nil)
                (print (concatenate 'string "No definition found for " (string-upcase (string symbol)))))
               ((= (length definitions) 1)
                (let ((file (cadr (car definitions))))
                  (if (pathnamep file)
                      (if (probe-file file)
                          (om-open-new-text-editor-at file (car (car definitions)))
                        (progn (beep-pane nil) (print (format nil "File ~A not found" file))))
                    (progn (beep-pane nil) (print (format nil "Unknown location for ~A definition..." symbol)))
                    )))
               (t
                (show-definitions-dialog symbol definitions))
               ))
          (progn (beep-pane nil)
            (print (format nil "~A is not a valid symbol" symbol)))))
    (progn (beep-pane nil)
      (print (format nil "~A is not a valid symbol" symbol)))))

; (om-edit-definition 'om::om+)
; (setf dspec::*active-finders* (list :internal dspec::*active-finders*))
; (dspec:find-name-locations dspec:*dspec-classes* 'om::om+)
; (dspec:name-definition-locations dspec:*dspec-classes* 'om::om+)
; (*active-finders*)
; save-tags-database