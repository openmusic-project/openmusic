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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;;;
;=========================================================================
;;;
; Author: Karim Haddad
;==================================
; omicron choose EDO scales dialog
;==================================

(in-package :om)


;=======================
; DATA
;=======================

;=======================
; INSPECTOR
;=======================

(defclass omicron-dialog (om-window)
  ((list1 :initarg :list1 :initform nil :accessor list1)
   (list2 :initarg :list2 :initform nil :accessor list2)
   (list3 :initarg :list3 :initform nil :accessor list3)
   (title1 :initarg :title1 :initform nil :accessor title1)
   (title2 :initarg :title2 :initform nil :accessor title2)
   (title3 :initarg :title3 :initform nil :accessor title3)
   (but1 :initarg :but1 :initform nil :accessor but1)
   (but2 :initarg :but2 :initform nil :accessor but2)
   (but3 :initarg :but3 :initform nil :accessor but3)
   (selected :initarg :selected :initform nil :accessor selected)
   (obj :initarg :obj :initform nil :accessor obj)
   (index1 :initarg :index1 :initform 6 :accessor index1)
   (index2 :initarg :index2 :initform 0 :accessor index2)
   ))



(defmethod oa::om-resize-callback ((self omicron-dialog) x y w h)
  (call-next-method)
  (let ((diff (abs (- w 760))))
    (when (list1 self)
          (om-set-view-size (list1 self) (om-make-point (round (+ (/ (- w 740) 3) 230))  (+ (- h 400) 280)))
          )
         (when (list2 self)
           (om-set-view-size (list2 self) (om-make-point (round (+ (/ (- w 740) 6) 230)) (+ (- h 400) 280)))
            (om-set-view-position (list2 self) (om-make-point (round (+ 250 (/ (- w 740) 3) ))  30)))
         (when (list3 self)
           (om-set-view-size (list3 self) (om-make-point (round (+ (/ (- w 740) 3) 230))  (+ (- h 400) 280)))
           (om-set-view-position (list3 self) (om-make-point (round (+ 490 (/ (- w 740) 2) ))  30)))
         (when (title2 self)
           (om-set-view-position (title2 self) (om-make-point (round (+ 250 (/ (- w 740) 3) ))  8))
          )
         (when (title3 self)
           (om-set-view-position (title3 self) (om-make-point (round (+ 490 (/ (- w 740) 2) ))  8))
           )
         (when (but1 self)
           (om-set-view-position (but1 self) (om-make-point 20  (+ (- h 400) 330)))
           )
         (when (but2 self)
           (om-set-view-position (but2 self) (om-make-point ;560 
                                                            (round (+ 560 (- w 740) ))
                                                            (+ (- h 400) 330)))
           )
         (when (but3 self)
           (om-set-view-position (but3 self) (om-make-point ;650  
                                                            (round (+ 650 (- w 740) ))
                                                            (+ (- h 400) 330)))
           )
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(cadr (car (decompose-omicron-object *datastuff*)))

(defun decompose-omicron-object (obj)
        (loop for item in obj
             ; for i = 0 then (+ i 1)
              collect (list item)))


(defun set-omicron-panel (item-list object)
  (setf (capi::collection-items item-list)
        (loop for elt in (decompose-omicron-object object) 
              collect (format nil "~A"  (car elt)))))

#+cocoa
(defmethod close-all-inspectors ((self omicron-dialog))
  (mapc 'om-close-window (om-get-all-windows 'omicron-dialog)))             

#+cocoa
(defmethod internal-window-class-menubar ((self omicron-dialog)) 
  (append (list (make-instance 'capi::menu :title "Window"
                               :items 
                               (append (list (make-instance 'capi::menu-item :title "Close all inspector windows"
                                                            :callback-type :interface
                                                            :callback 'close-all-inspectors
                                                            :accelerator #\w)))))))


;;;functions

(defun find-indx (approx)
(let* ((res
        (loop for i in   *omicron-scales-list*
              collect (loop for n in i  collect (position approx n))))
       (lst1
        (car (remove nil
                (loop for n from 0 to (length res)
                      for i in res collect (if (not (list-subtypep i 'null)) n)))))
       (lst2 (position-if-not #'null (nth lst1 res))))
  (list lst1 lst2)))

;(find-indx 6)

(defun give-index-off-approx (approx)
  (car 
   (remove nil
           (loop for i in *scales-list*
                 for n from 0 to (length *scales-list*)
                 collect (if (member approx i) n)))))
;(give-index-off-approx 2)

;(nth 12 *scales-list*)

(defun give-symbol-of-approx (approx)
  (let ((indx (give-index-off-approx approx)))
    (if indx
        (last-elem (nth indx *scales-list*))
      "12 EDO_#" ;put rather the *global-midi-approx* corresponding value
      )))


;(give-symbol-of-approx 2)

;;;

(defun get-control-button (self)
  "Returns 'approx' subview"
(let* ((ed (om-view-container self))
         (panel (panel ed))
         (obj (object ed)))
          (cond 
           ((or (chord-seq-p obj)
                (voice-p obj)
                (multi-seq-p obj)
                (poly-p obj))
            (if (linear? panel) 10 10));same ?
           ((chord-p  (if (linear? panel) 7 6)))
           (t 7))))


(defun om-micron (object controls &optional position) 
  (let* ((pos (or position (om-make-point 200 200)))
         (approx (staff-tone (panel (om-view-container controls))))
         (index (find-indx approx))
         (indx1 (car index))
         (indx2 (second index))
         (panel (panel (om-view-container controls)))
         (mode (score-mode panel))
         (*pmode* nil)
         (win (om-make-window 'omicron-dialog 
                              :size (om-make-point 740 400)
                              :position pos
                              :window-title "Tuning and notation"
                              :resizable t
                              :external-min-width 740
                              :external-min-height 400 
                              :bg-color *controls-color*
                              :menu-items (list (om-make-menu
                                                 "Window"
                                                 (list (om-new-leafmenu
                                                        "Close all inspector windows"
                                                        #'(lambda () (mapc 'om-close-window (om-get-all-windows 'omicron-dialog)))
                                                        "w"
                                                        ))))
                              ))

         )
    (when controls
    (setf (obj win) controls))
    (setf (title1 win) (om-make-dialog-item 'om-static-text (om-make-point 80 8) (om-make-point 150 50) "Tuning system"))
    (setf (list1 win) (om-make-dialog-item 'om-single-item-list
                                           (om-make-point 10 30)
                                           (om-make-point 230 280)
                                           ""
                                           :focus nil
                                           :resizable t
                                           :scrollbars :v
                                           :callback-type '(:collection)
                                           :test-function 'string-equal
                                           :range nil
                                         ;  :action-callback #'(lambda (list)
                                         ;                       (print "OK ACTION!"))
                                           :selection-callback #'(lambda (list) 
                                                                   (progn 
                                                                     (setf (selected win) (mapcar 'car *omicron-data*))
                                                                     (setf indx1 (capi::choice-selection list))
                                                                     (setf (index1 win) (capi::choice-selection list))
                                                                     ;(print (list "selection" (capi::choice-selection list) "indx:" indx1))
                                                                     (set-omicron-panel (list2 win) 
                                                                                        (second (nth (capi::choice-selection list) *omicron-data*)))
                                                                     (setf indx2 0);init choix 2
                                                                     (om-set-dialog-item-text (list3 win) 
                                                                                              (car (third (nth (capi::choice-selection list)  *omicron-data*))))
                                                                     )
                                                                   )))
    


    (setf (title2 win) (om-make-dialog-item 'om-static-text (om-make-point 300 8) (om-make-point 150 50) "Notation system"))
    (setf (list2 win) (om-make-dialog-item 'om-single-item-list
                                           (om-make-point 250 30)
                                           (om-make-point 230 280)
                                           ""
                                           :focus nil
                                           :resizable t
                                           :scrollbars :v
                                           :callback-type '(:collection)
                                           :test-function 'string-equal
                                           :range nil
                                           :selection-callback #'(lambda (list) 
                                                                  ; (print (list "second"  (capi::choice-selection list)))
                                                                   (setf indx2 (capi::choice-selection list))
                                                                   (setf (index2 win) (capi::choice-selection list))
                                                                   (om-set-dialog-item-text 
                                                                    (list3 win) 
                                                                    (nth 
                                                                     (capi::choice-selection list)
                                                                     (third (nth indx1  *omicron-data*)))))))
    
    (setf (title3 win) (om-make-dialog-item 'om-static-text (om-make-point 580 8) (om-make-point 80 50) "Description")) 
    (setf (list3 win) (om-make-dialog-item 'om-static-text
                                           (om-make-point 490 30)
                                           (om-make-point 230 280)
                                           ""
                                           :bg-color *om-white-color*
                                           ))
    (setf (but1 win) (om-make-dialog-item 'om-button (om-make-point 20 330) (om-make-point 160 20) 
                                          "Restore default"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                       (om-set-selected-item-index (list1 win) 1)
                                                       (om-set-selected-item-index (list2 win) 0)
                                                       (set-edit-param (om-view-container controls) 'approx
                                                                       (car (nth 0 (nth 1 *omicron-scales-list*))))
                                                       (om-set-dialog-item-text (nth 10 (om-subviews controls)) (give-symbol-of-approx 
                                                                                                                 (car (nth 0 (nth 6 *omicron-scales-list*)))
                                                                                                                 ))
                                                       (change-editor-tone (panel (om-view-container controls))
                                                                           (car (nth 0 (nth 6 *omicron-scales-list*))))
                                                       (om-close-window win)
                                                       )
                                          ))
    (setf (but2 win) (om-make-dialog-item 'om-button (om-make-point 560 330) (om-make-point 80 20) "Cancel"
                                           :di-action (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                        (om-close-window win))
                                                       ))
    (setf (but3 win) (om-make-dialog-item 'om-button (om-make-point 650 330) (om-make-point 80 20) "Apply"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                       (progn
                                                           #+linux (when (in-page-mode? panel)
                                                                   (change-score-mode (panel (om-view-container controls)) 0)
                                                                   (setf *pmode* t))
                                                         (set-edit-param (om-view-container controls) 'approx
                                                                         (car (nth indx2 (nth indx1 *omicron-scales-list*))))
                                                         (om-set-dialog-item-text (nth (get-control-button controls) (om-subviews controls)) 
                                                                                  (give-symbol-of-approx (car (nth indx2 (nth indx1 *omicron-scales-list*)))));display button name 10
                                                         
                                                         (change-editor-tone panel
                                                                             (car (nth indx2 (nth indx1 *omicron-scales-list*))))
                                                         
                                                         (om-close-window win)
                                                         (om-invalidate-view controls t)
                                                         #+linux (when *pmode*
                                                                   (change-score-mode panel 2)
                                                                   (setf *pmode* nil))
                                                         (update-slot-edit panel)
                                                         ))
                                        
))
                                                       
    (set-omicron-panel (list1 win) (mapcar 'car object))
    (om-set-selected-item-index (list1 win) indx1)
    (set-omicron-panel (list2 win) (nth indx1 (mapcar 'second object)))
    (om-set-selected-item-index (list2 win) indx2)
    (om-set-dialog-item-text (list3 win) (nth indx2 (nth indx1 (mapcar 'third object))))
    (om-add-subviews win (title1 win) (list1 win) (list2 win) (list3 win) (title1 win) (title2 win) (title3 win) (but1 win) (but2 win) (but3 win))
    #+cocoa(setf (capi::interface-menu-bar-items win)
                 (internal-window-class-menubar win))
    (om-select-window win)
 
    ))

;(om-micron *omicron-data* nil)
