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

(export '(
          edit-definition
          add-class-definition
          add-method-definition
          init-root-definition-pathname
          ) :om-lisp)

;=======================
; find and edit source code 
;=======================

; 

(defun show-definitions-dialog (symb deflist)
  (let* ((w 300) (h 200)
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
                                                             :width 270 :height 160
                                                             :interaction :single-selection
                                                             :retract-callback nil
                                                             :focus nil
                                                             :callback-type '(:collection)
                                                             :test-function 'string-equal
                                                             :items (loop for item in deflist collect
                                                                          (if (listp (car item)) (format nil "~A ~A" (caar item) (cadar item))
                                                                            (format nil "~A" (car item))))
                                                                            
                                                             :action-callback #'(lambda (list)
                                                                                  (let ((def (car (nth (capi::choice-selection list) deflist)))
                                                                                        (file (cadr (nth (capi::choice-selection list) deflist))))
                                                                                    (if file
                                                                                        (if (probe-file file)
                                                                                            (progn
                                                                                              (om-open-new-text-editor-at 
                                                                                               file 
                                                                                               (if (listp def)
                                                                                                   (apply 'concatenate (append 
                                                                                                                        (list 'string 
                                                                                                                              (format nil "(def.*[^\s]~A" (car def)))
                                                                                                                        (loop for type in (cadr def) when (not (equal t type))
                                                                                                                              collect (format nil ".*~A" type))))
                                                                                                 (string def)))
                                                                                              (capi:quit-interface win)
                                                                                              )
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


;;;=====================
;;; CUSTOM SOURCE TRACKER
;;;=====================

(defvar *class-definitions* nil)
(defvar *method-definitions* nil)

(defun add-class-definition (class path)
  (let ((pathstr (and path (namestring path)))
        (name (class-name class)))
    (unless (find (list name pathstr) *class-definitions*
                  :test #'(lambda (a b) (and (equal (car a) (car b))
                                             (string-equal (cadr a) (cadr b)))))
      (push (list name pathstr) *class-definitions*))))

(defun add-method-definition (method path)
  (let ((name (clos::generic-function-name (clos::method-generic-function method)))
        (pathstr (and path (namestring path))))
      (unless (find (list (list name (mapcar 'class-name (clos::method-specializers method))) pathstr) 
                    *method-definitions*
                    :test #'(lambda (a b) (and (equal (caar a) (caar b))
                                               (equal (cadar a) (cadar b))
                                               (if (and (cadr a) (cadr b)) (string-equal (cadr a) (cadr b)) (equal (cadr a) (cadr b))))))
        (push (list 
               (list name (mapcar 'class-name (clos::method-specializers method)))
               pathstr) *method-definitions*))))



(defun restore-root (path oldroot newroot)
  (let ((rec-root-dir (pathname-directory oldroot))
        (path-dir (pathname-directory (translate-logical-pathname path))))
    (if (and (>= (length path-dir) (length rec-root-dir))
             (equal rec-root-dir (butlast path-dir (- (length path-dir) (length rec-root-dir)))))
        ;;; => path is recorded in the original rec-root-dir
        (merge-pathnames (make-pathname :name (pathname-name path)
                                        :type (or (pathname-type path) "lisp")
                                        :directory (append (pathname-directory newroot)
                                                           (nthcdr (length rec-root-dir) path-dir))) 
                         newroot)
      path)))

(defun init-root-definition-pathname (oldroot newroot) 
  (loop for def in *class-definitions* do
        (setf (nth 1 def) (namestring (restore-root (nth 1 def) oldroot newroot))))
  (loop for def in *method-definitions* do
        (setf (nth 1 def) (namestring (restore-root (nth 1 def) oldroot newroot)))))

(defun edit-definition (symbol &optional type)
  (let ((definitions 
         (loop for item in (cond ((equal type :method) *method-definitions*)
                                 ((equal type :class) *class-definitions*)
                                 (t (append *method-definitions* *class-definitions*)))
               when (if (listp (car item)) (equal symbol (caar item)) (equal symbol (car item)))
               collect (list (car item)
                             (and (cadr item) (merge-pathnames (cadr item) (make-pathname :type "lisp")))))))
    (if definitions
        (if (= (length definitions) 1)
            (let ((file (car (last (car definitions)))))
              (if (pathnamep file)
                  (if (probe-file file)
                      (om-open-new-text-editor-at file (string (if (listp (caar definitions)) (car (caar definitions)) (caar definitions))))
                    (progn (beep-pane nil) (print (format nil "File ~A not found" file))))
                (progn (beep-pane nil) (print (format nil "Unknown location for ~A definition..." symbol)))))
          (show-definitions-dialog symbol definitions)) 
      (progn (beep-pane nil)
        (print (format nil "No definition found for ~A" (string-upcase (string symbol))))))
    ))



#|

; (edit-definition 'om::om+)
; (setf dspec::*active-finders* (list :internal dspec::*active-finders*))
; (dspec:find-name-locations dspec:*dspec-classes* 'om::om+)
; (dspec:name-definition-locations dspec:*dspec-classes* 'om::om+)
; (*active-finders*)
; save-tags-database


;;; not used anymore
(defun restore-definitions-pathnames (def-list)
  (loop for def in def-list collect
        (list (car def)
              (if (and (or (stringp (cadr def)) (pathnamep (cadr def))))
                  (let ((restored (restore-ompath (cadr def))))
                    (if (and (pathnamep restored) (probe-file restored))
                        (truename restored)
                      (cadr def)))
                (cadr def)))))

(defun edit-definition (symbol)
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
|#