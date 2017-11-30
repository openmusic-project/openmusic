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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================
;DocFile
;Preference editor is defined in this file.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;Kept for compatibility
(defvar *current-pref* nil)
(defvar *saved-pref* nil)

(defun find-pref-module (name &optional preflist)
   (unless preflist
     (setf preflist *current-pref*))
   (find name preflist :key 'car)) 


(defun get-pref (module key)
  (when module
    (let ((pos (position key (cadr module) :test 'equal)))
      (if pos (nth (+ pos 1) (copy-list (cadr module)))
        (let ((defpos (position key (get-def-vals (car module)) :test 'equal)))
          (when defpos
            (nth (1+ defpos) (copy-list (get-def-vals (car module))))))))))
  
(defun set-pref (module key val)
  (let ((pos (position key (cadr module) :test 'equal)))
    (if pos 
      (setf (nth (+ pos 1) (cadr module)) val)
      (setf (cadr module) (append (cadr module) (list key val))))))

(defmethod save-pref-module (modulename values)
   (list modulename (omng-save values) *om-version*))

(defmethod corrige-preference-list (modulename list &optional (version 0))
  (let ((def (get-def-vals modulename))
        (rep nil))
    (loop for i = 0 then (+ i 2)
          while (< i (length def)) do
          (let ((pos (position (nth i def) list)))
            (if pos 
                (setf rep (append rep (list (nth i def) (nth (+ pos 1) list))))
              (setf rep (append rep (list (nth i def) (nth (+ i 1) def)))))
            ))
    rep))

(defun push-pref-module (prlist)
  (let ((pos (position (car prlist) *current-pref* :test 'equal :key 'car)))
    (if pos (setf (nth pos *current-pref*) prlist)
      (setf *current-pref* (append *current-pref* (list prlist))))
    *current-pref*))

(defparameter *pref-order* '(:general :appearance :userlibs :score :conversion :midi :audio :externals))

(defun sort-pref-items (list)
  (sort list #'(lambda (elt1 elt2) 
                 (let ((p1 (position (car elt1) *pref-order*))
                       (p2 (position (car elt2) *pref-order*)))
                   (or (null p2) (and p1 p2 (<= p1 p2)))))))

(defvar *restore-defaults* nil)

(defmethod restore-preferences ()
   (when *saved-pref*
     (setf *restore-defaults* nil)
     (loop for item in *saved-pref* do
           (let ((modulepref (find-pref-module (car item))))
             (when modulepref
               (setf (cadr modulepref) (corrige-preference-list (car item) (eval (second item)) (or (third item) 0)))
               (put-preferences (car item)))))
     (setf *restore-defaults* nil)))

;(restore-preferences)
(defmethod put-all-preferences ()
  (om-print "APPLYING PREFERENCES...")
  (loop for item in *current-pref* do
        (put-preferences (car item))))


;;; never used
(defmethod put-default-preferences ()
  (loop for item in *current-pref* do
        (setf (cadr item) (get-def-vals (car item)))
        (put-preferences (car item))))

(defun preferences-file ()
  (om-make-pathname :directory (mypathname *current-workSpace*) :name "preferences" :type "lisp"))

(defmethod save-preferences ()
  (let ((path (preferences-file)))
    (delete-file-protection path)
    (WITH-OPEN-FILE (out path :direction :output 
                         :if-does-not-exist :create :if-exists :supersede) 
      (write-line (format nil "; OM Preferences - Saved ~D - OpenMusic ~D" (om-get-date) *version-string*) out)
      (prin1 '(in-package :om) out)
      (prin1 `(setf *saved-pref* ',(remove nil
                                           (loop for item in *current-pref*
                                                 collect (save-pref-module (car item) (cadr item))))) out))))



(defun display-om-preferences (&optional module)
  (let ((prefs-to-display (if module (list (find module *current-pref* :key 'car)) *current-pref*)))
    (print "============================")
    (print "CURRENT OM PREFERENCES:")
    (print "============================")
    (loop for item in prefs-to-display do
          (print (format nil "MODULE: ~A" (car item)))
          (loop for prefitem on (cadr item) by 'cddr do 
                (print (format nil "    ~A = ~A" (car prefitem) (cadr prefitem))))
          (print "============================"))
    ))

; (display-om-preferences)

;=================================================
;PREFERENCES WINDOW and EDITOR CLASS
;=================================================

(defvar *pref-window* nil)


(defclass ompref-window (om-dialog)
  ((tabs :accessor tabs :initarg :tabs :initform nil)
   (local-prefs :accessor local-prefs :initarg :local-prefs :initform nil)))

(defclass preference-pane (om-view)
  ((pref-id :accessor pref-id :initarg :pref-id :initform nil)))

(defun get-pref-scroll-size () 
  (om-make-point 800 350))

(defmethod update-pref-scroll ((self ompref-window) &optional selection)
  (let* ((selec 0)
         (panelist (loop for item in (local-prefs self) 
                         for i = 0 then (+ i 1) do
                         (when (and selection (equal selection (car item)))
                           (setf selec i))
                         collect 
                         (make-new-pref-scroll (car item) item)
                         ))
         
          (newtl (om-make-tab-layout panelist :position (om-make-point 0 0)
                                  :size (get-pref-scroll-size)
                                  :selection selec)))
    (om-remove-subviews self (tabs self))
    (om-add-subviews self (setf (tabs self) newtl))))
    

(defun make-preference-win ()
   (let* ((prefs (sort-pref-items (clone *current-pref*)))
          (panelist (mapcar #'(lambda (item)
                                (make-new-pref-scroll (car item) item))
                            prefs))
          (tl (om-make-tab-layout panelist :position (om-make-point 0 0)
                                  :size (get-pref-scroll-size )))
          (b-posy (+ (om-point-v (get-pref-scroll-size)) 5))
          (newwindow (om-make-window 'ompref-window :window-title "OpenMusic Preferences" 
                                     :size (om-add-points (get-pref-scroll-size) (om-make-point 0 50)) 
                                     :position (om-make-point 100 50) :close t :resizable nil
                                     :local-prefs prefs)))
     
     (om-add-subviews newwindow (setf (tabs newwindow) tl))
     (om-add-subviews newwindow              
                      (om-make-dialog-item 'om-button (om-make-point 25 b-posy) (om-make-point 100 22) "Restore..." 
                                           :di-action (om-dialog-item-act item
                                                        (let* ((win (om-view-window item))
                                                               (module (find-pref-module (pref-id (om-current-view (tabs win)))
											 (local-prefs win))))
                                                          (when module
                                                            (setf (cadr module) (get-def-vals (pref-id (om-current-view (tabs win)))))
                                                            #-linux (update-pref-scroll win (pref-id (om-current-view (tabs win))))
							    #+linux (om-select-window *pref-window*)))))
		      
                      (om-make-dialog-item 'om-button (om-make-point 130 b-posy) (om-make-point 80 22) "Apply" 
                                           :di-action (om-dialog-item-act item
                                                        (let ((win (om-view-window item)))
							  (setf *current-pref* (local-prefs win))
							  (put-all-preferences)
                                                          (save-preferences)
							  #-linux (update-pref-scroll win (pref-id (om-current-view (tabs win))))
							  #+linux (om-select-window *pref-window*))))

                      (om-make-dialog-item 'om-button (om-make-point (- (om-point-h (get-pref-scroll-size)) 185) b-posy) (om-make-point 80 22) "Cancel" 
                                           :di-action (om-dialog-item-act  item
                                                        (om-close-window (om-view-window item))
                                                        ))
                    
                      (om-make-dialog-item 'om-button (om-make-point (- (om-point-h (get-pref-scroll-size)) 100) b-posy) (om-make-point 80 22) "OK" 
                                           :di-action (om-dialog-item-act  item
                                                        (setf *current-pref* (local-prefs (om-view-window item)))
                                                        (put-all-preferences)
                                                        (save-preferences)
                                                        (om-close-window (om-view-window item)))))
     newwindow))

(defun omG-make-preference-dialog ()
   (if (and *pref-window* (om-window-open-p *pref-window*))
     (om-select-window *pref-window*)
     (setf *pref-window* (om-select-window (make-preference-win)))))

(defun init-preferences ()
  (setf *current-pref* nil)
  (setf *pref-window* nil))

(om-add-init-func 'init-preferences)
