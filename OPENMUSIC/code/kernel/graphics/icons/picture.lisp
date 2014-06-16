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
;This File defines functions for picture manegement.
;All init pictures for OM are loaded in this file.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

; name = string
; source = symbol
(defclass internepicture ()
   ((thepict :initform nil :accessor thepict :initarg :thepict)
    (name :initform nil :accessor name :initarg :name)
    (source :initform nil :accessor source :initarg :source)
    (pict-pathname :initform nil :accessor pict-pathname :initarg :pict-pathname)
    (extraobjs :initform nil :accessor extraobjs :initarg :extraobjs)
    (storemode :initform :external :accessor storemode :initarg :storemode)   
    (draw-params :initform '(p 0 0 100 100) :accessor draw-params :initarg :draw-params)))

;;; extraobjs = (tool points params)
;;; params (color size line fill/font)

(defun restore-pict-path (pathname)
  (if (and pathname (relative-path-reference))
      (let ((local-file? (om-make-pathname :directory (relative-path-reference) :name (pathname-name pathname) :type (pathname-type pathname))))
        (if (probe-file local-file?) local-file? pathname))
    pathname))

(defmethod omNG-save ((self internepicture) &optional (values? nil))
  (let* ((path (and (pict-pathname self) (om-save-pathname-relative (pict-pathname self))))
         (pict-data (if (equal (storemode self) :internal)
                       `(om-create-picture ,(omng-save (om-picture-values (thepict self))))
                     (if (pict-pathname self)
                         `(om-get-picture-file (restore-pict-path (restore-path ,path)))
                       (if (name self)
                           (if (stringp (name self)) 
                               `(om-get-picture ,(name self) ',(source self))
                             `(om-get-picture ',(name self)))
                         nil)
                       )))
         (name (name self))
         (src (source self)))
    
    (unless (stringp name)
      (multiple-value-bind (sss nnn) (string-until-char (string name) "-")
        (setf src (intern sss :om)
              name nnn)))
    
    (unless (equal (storemode self) :internal)
      (let* ((restoredfolder (restore-pict-folder src))
             (pfile (or (pict-pathname self)
                        (and restoredfolder
                             (make-pathname :directory (pathname-directory restoredfolder) 
                                            :host (pathname-device restoredfolder) 
                                            :device (pathname-device restoredfolder) 
                                            :name name)))))
        (when pfile (register-resource :picture pfile))))
        
    `(make-instance ',(type-of self)
                    :name ,name
                    :source ',src
                    :pict-pathname (restore-pict-path (restore-path ,path))
                    :thepict ,pict-data
                    :storemode ,(storemode self)
                    :draw-params ',(draw-params self)
                    :extraobjs ,(omng-save (extraobjs self)))))



(defmethod omNG-copy ((self internepicture))
  `(let ((newpict (make-instance ',(type-of self))))
     (setf (thepict newpict) ,(thepict self))
     (setf (name newpict) ',(name self))
     (setf (extraobjs newpict) ',(copy-container (extraobjs self)))
     (setf (source newpict) ',(source self))
     (setf (pict-pathname newpict) ,(pict-pathname self))
     (setf (storemode newpict) ,(storemode self))
     (setf (draw-params newpict) ',(draw-params self))
     newpict))

(defmethod copy-picture ((self internepicture) &optional type)
  (let ((newpict (make-instance (or type (type-of self)))))
     (setf (thepict newpict) (thepict self))
     (setf (name newpict) (name self))
     (setf (extraobjs newpict) (copy-container (extraobjs self)))
     (setf (source newpict) (source self))
     (setf (pict-pathname newpict) (pict-pathname self))
     (setf (storemode newpict) (storemode self))
     (setf (draw-params newpict) (draw-params self))
     newpict))


;;; old compat
(defmethod load-pict-instance (type name)
  (let ((newpict (make-instance type :name name
                                :source 'user)))
    (setf (thepict newpict) (om-get-picture (name newpict) 'user))
    newpict))

(defun draw-lost-picture (pict view &optional (x 0) (y 0) w h)
  (let ((ww (or w (w view))) (hh (or h (h view))))
    (when (pict-pathname pict)
      (om-with-fg-color view *om-dark-gray-color*
        (om-with-clip-rect view (om-make-rect x y (+ x ww 1) (+ y hh 1))
          (om-with-font *om-default-font1*
                        (om-with-line '(2 2)
                          (om-draw-rect x y ww hh)
                          (om-draw-string (+ x 10) (+ y 20) (string+ "Image: " (pathname-name (pict-pathname pict)) 
                                                                     "." (pathname-type (pict-pathname pict))))
                           ;(om-draw-string (+ x 10) (+ y 40) (namestring (make-pathname :directory (pathname-directory (pict-pathname pict)))))
                          )))))))
  
;========

;Used by temporal objects and maquettes
(defclass om-picture (internepicture select-object) ())

(defun make-new-om-pict (&optional ref) 
 (make-ref-picture ref))

(defmethod make-ref-picture (self)
  (make-instance 'om-picture))



;;; PICTURE FOR PATCH BG
;;; (currently must be created from a picture object)
;;;======================================
(defclass patch-picture (om-picture)
   ((pict-pos :initform (om-make-point 0 0) :accessor pict-pos)
    (pict-size :initform (om-make-point 10 10) :accessor pict-size)))

(defmethod copy-picture ((self patch-picture) &optional type)
  (let ((newpict (call-next-method)))
    (when (subtypep type 'patch-picture)
      (setf (pict-pos newpict) (pict-pos self))
      (setf (pict-size newpict) (pict-size self)))
    newpict))

(defmethod draw-pict-patch ((self patch-picture) view)
   (let ((pictsize (pict-size self))
           (x0 (om-point-h (pict-pos self)))
           (y0 (om-point-v (pict-pos self))))
     (if (thepict self) 
         (om-draw-picture view (thepict self) :pos (om-make-point x0 y0) :size pictsize)
       (draw-lost-picture self view x0 y0 (om-point-h pictsize) (om-point-v pictsize)))
     (loop for o in (extraobjs self) do (draw-pict-extraobj view o (om-make-point x0 y0) pictsize))
     (when (selected-p self)
       (om-with-line-size 2
         (om-draw-rect (+ 1 x0) (+ 1 y0) (- (om-point-h pictsize) 2) (- (om-point-v pictsize) 2))))))

(defmethod omNG-save ((self patch-picture) &optional (values? nil))
  `(let ((newpict ,(call-next-method)))
     (setf (pict-pos newpict) ,(om-save-point (pict-pos self)))
     (setf (pict-size newpict) ,(om-save-point (pict-size self)))
     newpict))

  
;---------------------------------------------
;OM PICTURE MANAGEMENT
;---------------------------------------------
; il faut simplifier et n'utiliser que le pathname !!

; name = symbol avec prefixe 'kernel ou 'user
;(defun om-get-picture (name &optional source)
;  (if name
;   (let* ((namevals (multiple-value-list (string-until-char (string name) "-"))))
;     (or (om-get-picture-loaded name)
;         (om-load-and-store-picture (second namevals) (intern (first namevals)))
;         )) *om-def-pict*))

;(om-get-picture |USER-petite icone son| (quote nil))
;(om-get-picture "petite icone son" (quote common-lisp-user::user))


(defun om-get-picture (name &optional source)
  (unless source
    (multiple-value-bind (src nnn) (string-until-char (string name) "-")
      (setf source (intern src)
            name nnn)))
  (if name
      (or (om-get-picture-loaded (intern name) source)
          (om-load-and-store-picture name source))
    *om-def-pict*))


(defun om-get-picture-file (pathname)
  (and pathname (probe-file pathname)
       (or (om-get-picture-loaded (namestring pathname) 'full)
           (let* ((pict (om-load-pixmap 
                         (pathname-name pathname) *om-pict-type*
                         (om-make-pathname :directory pathname))))
             (when pict
               (om-store-picture pict (namestring pathname) 'full))
             pict)
           *om-def-pict*)))

;(defun om-get-picture-file (pathname)
;  (let ((pictpath 
;         (if (and pathname (not (probe-file pathname)) (relative-path-reference))
;             (om-make-pathname :directory (relative-path-reference) :name (pathname-name pathname) :type (pathname-type pathname))
;           pathname)))
;    (if (and pictpath (probe-file pictpath))
;        (let ((picture (or (om-get-picture-loaded (namestring pictpath) 'full)
;                           (let* ((pict (om-load-pixmap 
;                                         (pathname-name pathname) *om-pict-type* 
;                                         (om-make-pathname :directory pictpath)
;                                         )))
;                             (when pict
;                               (om-store-picture pict (namestring pathname) 'full))
;                             pict)
;                           *om-def-pict*)))
;          (setf (pict-pathname picture) pictpath)
;          picture)
      ; *om-def-pict*
;      nil
;      )))
          

; (om-get-picture-loaded 'user-boitezn)       
      
;;; name = symbol (sans prefixe)   -    folder = 'user / 'kernel / 'internal / 'mylib / 'full
;(om-get-picture-loaded  (intern (string+ (string folder) "-" name)))
(defun om-load-and-store-picture (name folder &optional full-folder-path)
  (if (equal folder 'full)
      (or 
       (om-get-picture-loaded (namestring (make-pathname :directory (pathname-directory full-folder-path) :name name)) folder)
       (let ((pict (when full-folder-path 
                     (om-load-pixmap name *om-pict-type* 
                                     (make-pathname :directory (pathname-directory full-folder-path)) nil))))
         (when pict
           (om-store-picture pict (namestring (make-pathname :directory (pathname-directory full-folder-path) :name name))
                             folder))
         pict)
       *om-def-pict*)
    
    (or 
     (om-get-picture-loaded (intern name) folder)
     (let* ((folderpath (restore-pict-folder folder))
            (pict (when folderpath 
                    (om-load-pixmap name *om-pict-type* 
                                    (make-pathname :directory (pathname-directory folderpath)) nil))))
       (when pict
         (om-store-picture pict (intern name) folder))
       pict)
     *om-def-pict*)
    ))



(defun restore-pict-folder (folder)
  (cond ((equal folder 'user) 
         (or (and *current-workspace* (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*))
                                                                        (list "resources" "pict"))))
             *relative-path-reference*))
        ((equal folder 'kernel) *om-pict-folder*)
        ((equal folder 'di) *om-di-folder*)
        ((equal folder 'internal) (make-pathname :directory (append (pathname-directory *om-pict-folder*)
                                                                    (list "internal"))))
        (t (if (exist-lib-p (string folder))
               (probe-file (make-pathname :directory (append (pathname-directory (lib-pathname  (exist-lib-p (string folder))))
                                                             (list "resources" "pict"))))))
        ))




;---------------------------
;OM PICTURES
;---------------------------
(defvar *glass-pict* nil "The picture used in the instance boxes")
(defvar *glass-pict-per* nil "The picture used in the instance boxes")
(defvar *impulsion-pict* nil "Used in temporal objects with dur = 0")
(defvar *ctrl-impulsion-pict* nil "Used in temporal objects with dur = 0")
(defvar *boxedit-pict* "The picture used in the editor boxes")
(defvar *graph-pres* "Presentation pict")
(defvar *om-def-pict* nil "Default pict when picture not found")


(defun init-all-pict ()
   (setf *play-controls-pict* (om-load-and-store-picture "play-controls" 'internal))
   (setf *graph-pres* (om-load-and-store-picture "initgraph" 'internal))
   (setf *glass-pict* (om-load-and-store-picture "ml-editorpict" 'kernel))
   (setf *glass-pict-per* (om-load-and-store-picture "glasspict-2" 'kernel))
   (setf *impulsion-pict* (om-load-and-store-picture "impulsion" 'kernel))
   (setf *ctrl-impulsion-pict* (om-load-and-store-picture "ctrlimpulsion" 'kernel))
   (setf *maquette-pict* (om-load-and-store-picture "maq-pal" 'internal)) 
   (setf *boxedit-pict* (om-load-and-store-picture "editorpict" 'kernel))
   (setf *om-def-pict* (om-load-and-store-picture "def-pict" 'kernel))) 

(om-add-init-func 'init-all-pict)

; (init-all-pict)


(defun load-lib-pictures (omlib)
  (let ((pictdir (make-pathname :directory (append (pathname-directory (lib-pathname omlib))
                                                   (list "resources" "pict")))))
    (when (probe-file pictdir)
      (loop for file in (om-directory pictdir) do 
            (when (om-pict-p file)
              (om-load-and-store-picture (pathname-name file) (intern (name omlib)))
              ))
      )))



;;;========================
;;; draw


(defun draw-mosaic (pict view)
  (let* ((pictsize (om-get-picture-size pict))
         (x (om-point-h pictsize))
         (y (om-point-v pictsize))
         (sizex (ceiling (w view) x))
         (sizey (ceiling (h view) y)))
    (loop for i from 0 to sizex do
          (loop for j from 0 to sizey do
                (om-draw-picture view pict :pos (om-make-point (* i x) (* j y)) 
                                 :size (om-make-point x y))))))

;; fact-x fact-y offs-x offs-y
(defmethod get-system-etat ((self t)) (list 1 1 0 0))
(defmethod point2pixel ((self t) point se) point)
(defmethod get-draw-params (pict view) (draw-params pict))

(defmethod draw-om-picture ((self internepicture) view)
  (if (thepict self)
    (let ((mode (car (draw-params self)))
          (pts (cdr (get-draw-params self view))))
      (cond ((equal 'c mode)
             (let* ((syse (get-system-etat view))
                    (x (min (nth 0 pts) (nth 2 pts)))
                    (y (max (nth 1 pts) (nth 3 pts)))
                    (x2 (max (nth 0 pts) (nth 2 pts))) 
                    (y2 (min (nth 1 pts) (nth 3 pts))))          
               (om-draw-picture view (thepict self)
                                :pos (point2pixel view (om-make-point x y) syse)
                                :size (om-subtract-points (point2pixel view (om-make-point x2 y2) syse)
                                                    (point2pixel view (om-make-point x y) syse)))))
        ((equal 'p mode)
         (let ((syse (get-system-etat view))
               (x 0) (y (om-pict-height (thepict self)))
               (x2 (om-pict-width (thepict self))) (y2 0))
           (om-draw-picture view (thepict self) :pos (point2pixel view (om-make-point x y) syse)
                            :size (om-subtract-points (point2pixel view (om-make-point x2 y2) syse)
                                                (point2pixel view (om-make-point x y) syse)))))
        ((equal 'w mode)
         (om-draw-picture view (thepict self) :size (om-view-size view)))
        ((equal 'm mode)
         (draw-mosaic (thepict self) view))
        (t nil)))
    (if (pict-pathname self)
        (om-with-focused-view view
          (draw-lost-picture self view)))))

(defun picture+params-dialog (picture &key defoption xunits yunits (enable '(p c w m)))
  (let* ((winh 580)
         (win (om-make-window 'om-dialog
                              :window-title (string+ (om-str :bg-pict) "...")
                              :position :centered 
                              :close nil
                              :resizable nil
                              :maximize nil
                              :window-show nil
                              :size (om-make-point 290 winh)))
         (y 10) (deltay 30)
         (picture (or picture (make-instance 'picture :draw-params (or defoption '(p 0 0 100 100)))))
         (params (if (thepict picture) (draw-params picture)
                   (or defoption (list (car enable) 0 0 100 100))))
         pictxt picview layouttxt mosaic wsize custom
         psize
         x0 y0 x1 y1 x0txt y0txt x1txt y1txt)
    
    
    (setf pictxt (om-make-dialog-item 'om-static-text (om-make-point 20 y) (om-make-point 80 20) (om-str :picture)
                                      :font *controls-font*))

    (setf picview (om-make-view 'choose-picture-view :position (om-make-point 120 y) :size (om-make-point 180 48)
                                :picture picture))
    (incf y (+ deltay 30))
    (setf layouttxt (om-make-dialog-item 'om-static-text (om-make-point 20 y) (om-make-point 100 20) (om-str :layout)
                                         :font *controls-font*
                                         ))

    (om-add-subviews win pictxt picview layouttxt)
    
    (when (find 'p enable) 
      (incf y deltay)
      (setf psize (om-make-dialog-item 'om-radio-button (om-make-point 30 y) (om-make-point 200 20) (om-str :picture-size)
                                       :font *controls-font*
                                       :radio-button-cluster 'layout
                                       :checked-p (equal 'p (car params))
                                       :di-action (om-dialog-item-act item
                                                    (om-enable-dialog-item x0 nil)
                                                    (om-enable-dialog-item x0txt nil)
                                                    (om-enable-dialog-item y0 nil)
                                                    (om-enable-dialog-item y0txt nil)
                                                    (om-enable-dialog-item x1 nil)
                                                    (om-enable-dialog-item x1txt nil)
                                                    (om-enable-dialog-item y1 nil)
                                                    (om-enable-dialog-item y1txt nil)
                                                    )))
      (om-add-subviews win psize)
    
      )

    (when (find 'c enable) 
      (incf y deltay)
      (setf custom (om-make-dialog-item 'om-radio-button (om-make-point 30 y) (om-make-point 200 20) (om-str :fromrulers)
                                        :font *controls-font*
                                        :checked-p (or (equal 'c (car params)) (null (car params)))
                                        :radio-button-cluster 'layout
                                        :di-action (om-dialog-item-act item
                                                     (om-enable-dialog-item x0 t)
                                                     (om-enable-dialog-item x0txt t)
                                                     (om-enable-dialog-item y0 t)
                                                     (om-enable-dialog-item y0txt t)
                                                     (om-enable-dialog-item x1 t)
                                                     (om-enable-dialog-item x1txt t)
                                                     (om-enable-dialog-item y1 t)
                                                     (om-enable-dialog-item y1txt t)
                                                     )))
      (incf y deltay)
      (setf x0txt (om-make-dialog-item 'om-static-text (om-make-point 40 y) (om-make-point 70 20) (string+ "x min" (if xunits (string+ " [" xunits "]") ""))
                                       :font *controls-font*
                                       :enable (equal 'c (car params))))
      (setf x0 (om-make-dialog-item 'om-editable-text (om-make-point 115 y) (om-make-point 40 20) 
                                    (if (nth 0 (cdr params)) (format nil "~D" (nth 0 (cdr params))) "0") 
                                    :font *controls-font*
                                    :enable (equal 'c (car params))))
      (setf y0txt (om-make-dialog-item 'om-static-text (om-make-point 170 y) (om-make-point 60 20) (string+ "y min" (if yunits (string+ " [" yunits "]") ""))
                                       :font *controls-font*
                                       :enable (equal 'c (car params))))
      (setf y0 (om-make-dialog-item 'om-editable-text (om-make-point 220 y) (om-make-point 40 20) 
                                    (if (nth 1 (cdr params)) (format nil "~D" (nth 1 (cdr params))) "0")
                                    :font *controls-font*
                                    :enable (equal 'c (car params))))
      (incf y deltay)
      (setf x1txt (om-make-dialog-item 'om-static-text (om-make-point 40 y) (om-make-point 70 20) (string+ "x max" (if xunits (string+ " [" xunits "]") ""))
                                       :font *controls-font*
                                       :enable (equal 'c (car params))))
      (setf x1 (om-make-dialog-item 'om-editable-text (om-make-point 115 y) (om-make-point 40 20) 
                                    (if (nth 2 (cdr params)) (format nil "~D" (nth 2 (cdr params))) "100")
                                    :font *controls-font*
                                    :enable (equal 'c (car params))))
      (setf y1txt (om-make-dialog-item 'om-static-text (om-make-point 170 y) (om-make-point 60 20) (string+ "y max" (if yunits (string+ " [" yunits "]") ""))
                                       :font *controls-font*
                                       :enable (equal 'c (car params))))
      (setf y1 (om-make-dialog-item 'om-editable-text (om-make-point 220 y) (om-make-point 40 20) 
                                    (if (nth 3 (cdr params)) (format nil "~D" (nth 3 (cdr params))) "100")
                                    :font *controls-font*
                                    :enable (equal 'c (car params))))

      (om-add-subviews win custom x0 y0 x0txt y0txt x1 y1 x1txt y1txt)
      )

    (when (find 'w enable) 
      (incf y (+ deltay 10))
      (setf wsize (om-make-dialog-item 'om-radio-button (om-make-point 30 y) (om-make-point 200 20) (om-str :window-size)
                                       :font *controls-font*
                                       :radio-button-cluster 'layout
                                       :checked-p (equal 'w (car params))
                                       :di-action (om-dialog-item-act item
                                                    (om-enable-dialog-item x0 nil)
                                                    (om-enable-dialog-item x0txt nil)
                                                    (om-enable-dialog-item y0 nil)
                                                    (om-enable-dialog-item y0txt nil)
                                                    (om-enable-dialog-item x1 nil)
                                                    (om-enable-dialog-item x1txt nil)
                                                    (om-enable-dialog-item y1 nil)
                                                    (om-enable-dialog-item y1txt nil)
                                                    )))
      (om-add-subviews win wsize)
      )

    (when (find 'm enable) 
      (incf y deltay)
      (setf mosaic (om-make-dialog-item 'om-radio-button (om-make-point 30 y) (om-make-point 80 20) (om-str :mosaic)
                                        :font *controls-font*
                                        :radio-button-cluster 'layout
                                        :checked-p (equal 'm (car params))
                                        :di-action (om-dialog-item-act item
                                                     (om-enable-dialog-item x0 nil)
                                                     (om-enable-dialog-item x0txt nil)
                                                     (om-enable-dialog-item y0 nil)
                                                     (om-enable-dialog-item y0txt nil)
                                                     (om-enable-dialog-item x1 nil)
                                                     (om-enable-dialog-item x1txt nil)
                                                     (om-enable-dialog-item y1 nil)
                                                     (om-enable-dialog-item y1txt nil)
                                                     )))
      (om-add-subviews win mosaic)
      )
       
    (incf y (+ deltay 20))
    
    (om-add-subviews win (om-make-dialog-item 'om-button 
                                              (om-make-point 150 y)
                                              (om-make-point 90 22)
                                              (om-str :ok)
                                              :di-action (om-dialog-item-act item
                                                           (declare (ignore item))
                                                           (let ((pict (if (picture picview)
                                                                           (copy-picture (picture picview))
                                                                         (make-instance 'picture))))
                                                             (setf (draw-params pict)
                                                                   (list (cond ((and mosaic (om-checked-p mosaic)) 'm)
                                                                               ((and wsize (om-checked-p wsize)) 'w)
                                                                               ((and psize (om-checked-p psize)) 'p)
                                                                               (t 'c))
                                                                         (if x0 (read-from-string (om-dialog-item-text x0)) 0)
                                                                         (if y0 (read-from-string (om-dialog-item-text y0)) 0)
                                                                         (if x1 (read-from-string (om-dialog-item-text x1)) 100)
                                                                         (if y1 (read-from-string (om-dialog-item-text y1)) 100)))
                                                             (om-return-from-modal-dialog win pict)))
                                              :default-button t  
                                              ))
     
    (om-add-subviews win (om-make-dialog-item 'om-button 
                                              (om-make-point 60 y)
                                              (om-make-point 90 22)
                                              (om-str :cancel)
                                              :di-action (om-dialog-item-act item
                                                           (declare (ignore item))
                                                           (om-return-from-modal-dialog win nil))))
    
    (incf y (+ deltay 20))
    (om-set-interior-size win (om-make-point 290 y))
    (om-modal-dialog win)))


