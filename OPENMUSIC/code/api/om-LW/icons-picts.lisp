;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; PICTURES, ICONS, CURSORS
;DocFile
;;===========================================================================


(in-package :om-api)

;;;==========
;;; export :
;;;==========

(export '(
                *om-resources-folder*
                *om-pict-folder*
                *om-pict-type*
                *om-cursor-type*
                om-set-default-resource-folder
                om-get-resource-file
                
                om-pict-p
                om-load-pixmap
                om-store-picture
                om-loaded-pictures
                om-picture-values
                om-picture-array
                om-create-picture
                om-get-picture-loaded
                om-kill-picture
                om-pict-width
                om-pict-height
                om-get-picture-size
                om-draw-picture
                om-record-pict
                om-internal-picture-to-pict
                om-record-picture-in-pict
                om-save-picture

                
                om-make-cursor
                om-with-cursor
                *om-wait-cursor*
                *om-arrow-cursor*
                *om-verti-size-cursor* 
                *om-horiz-size-cursor*  
                *om-resize-cursor* 
                *om-i-beam-cursor*   
                *om-cross-cursor* 
                *om-hand-cursor*
                om-init-cursors
                
                om-open-score-page
                om-close-score-page
                ) :om-api)

;;;================

(defvar *om-resources-folder* nil)

(defvar *om-pict-folder* nil)
(defvar *om-cursor-folder* nil)

(defvar *om-pict-type* '("tif" "tiff" "gif" "jpg" "jpeg" "png" "bmp"))

(defvar *om-cursor-type* nil)
#-win32(setf *om-cursor-type* '("tif" "tiff"))
#+win32(setf *om-cursor-type* '("cur" "ani"))

(defun om-set-default-resource-folder (resource-type path)
   (case resource-type
     (:rsrc 
      (setf *om-resources-folder* path)
      (init-sub-rsrc))
     (:pict (setf *om-pict-folder* path))
     (:curs (setf *om-cursor-folder* path))
     ))

;;; default values for resource folders
(defun init-def-rsrc ()
   (setf *om-resources-folder* (make-pathname :device (pathname-device *om-root*) :host (pathname-host *om-root*) 
                                           :directory (append (pathname-directory *om-root*) (list "resources")))))

(defun init-sub-rsrc ()
   (om-set-default-resource-folder :pict (make-pathname :device (pathname-device *om-resources-folder*) 
                                                        :host (pathname-host *om-resources-folder*)
                                                        :directory (append (pathname-directory *om-resources-folder*) (list "pict"))))  
   (om-set-default-resource-folder :curs (make-pathname :device (pathname-device *om-resources-folder*) 
                                                        :host (pathname-host *om-resources-folder*)
                                                        :directory (append (pathname-directory *om-resources-folder*) (list "curs")))))

(om-api-add-init-func 'init-def-rsrc)
(om-api-add-init-func 'init-sub-rsrc)

(defun om-pict-p (pathname)
   (let ((type (pathname-type pathname)))
     (member type *om-pict-type* :test 'string-equal)))

(defun om-get-resource-file (name folder types)
  (let ((rep nil))
    (loop for typ in (if (listp types) types (list types))
          while (not rep) do
          (setf rep (probe-file (make-pathname :directory (if (pathnamep folder) (pathname-directory folder) folder) 
                                               :device (if (pathnamep folder) (pathname-device folder))
                                               :host (if (pathnamep folder) (pathname-host folder))
                                               :name name :type typ))))
    rep))

(defun true-load-pixmap (path)
  (gp::read-external-image path)) ; :transparent-c

; load a pixmap and set a mask if mask-p and returns the pixmap (without store)
; name = string    -    folder = pathname
(defun om-load-pixmap (name type &optional folder (mask-p nil))
  (let* ((path (if folder folder *om-pict-folder*))
         (file (om-get-resource-file name path type)))
    (when file (true-load-pixmap file))))


; (setf mypict (capi::prompt-for-file ""))
;(let* ((pl (make-instance 'capi:pinboard-layout))
;         (win (capi:display (make-instance 'capi:interface :display-state :hidden :layout pl))))
;    (setf  port pl))
; (gp::load-image port (gp::read-external-image mypict))
; (gp::convert-external-image *record-view* (gp::read-external-image mypict))
; (make-instance 'capi::image-pinboard-object :image (gp::read-external-image mypict))

;;;=====================


(defvar *om-pict-table* nil)

(defun find-pixmap (name src)
  (let ((pos 
         (if (stringp name)
             (position name *om-pict-table* :key 'car :test #'(lambda (a b) (and (stringp b) (string-equal a b))))
           (position (list name src) *om-pict-table* :key 'car :test 'equal))))
    (when pos (second (nth pos *om-pict-table*)))))

(defun push-pixmap (name hdl src)
  (if (stringp name)
      (push (list name hdl) *om-pict-table*)
    (push (list (list name src) hdl) *om-pict-table*)))

(defun remove-pixmap (pict)
  (let ((pos (position pict *om-pict-table*  :test 'equal :key 'cadr)))
    (when pos (setf *om-pict-table* (remove (nth pos *om-pict-table*) *om-pict-table* :test 'equal)))))

;;; POSSIBLE AUSSI DANS CAPI AVEC gp:register-image-translation et gp:load-image

; name = symbol ,  source = symbol   
(defun om-store-picture (pict name &optional source)
  (unless (find-pixmap name source)
    (push-pixmap name pict source)))

; name = symbol ,  source = symbol
(defun om-get-picture-loaded (name source)
   (find-pixmap name source))

(defun om-loaded-pictures (&optional typetest)
  (remove nil (loop for item in *om-pict-table* collect
                    (when (and (listp (car item))
                               (if typetest (funcall typetest (cadr (car item))) t))
                      item))))
                        

; kills a pixmap handler (created with om-load-icon or om-load-picture)
; A FAIRE !!!
(defmethod om-kill-picture ((image t))  t)

(defun find-pict-in-port (pict port)
  ;(print "=======================")
  ;(print port)
  ;(print (list "images" (images port)))
  ;(print (list "find" pict))
  (let ((item (find pict (images port) :test 'equal :key 'car)))
    (if item (cadr item)
      (let ((image (gp::load-image port pict)))
        (setf (images port) (append (images port) (list (list pict image))))
        image))))

(defmethod om-draw-picture (view pict &key
                                 (pos (om-make-point 0 0)) size
                                 (srctopleft (om-make-point 0 0)) (srcsize nil)
                                 selected alpha)
  ;(print (list view pict))
  ;(print (list view pict))
  (when pict
    (let* ((port (om-get-view view))
           (image (and port (find-pict-in-port pict port)))
           (a (or alpha 1.0)))
      ;(print image)
      (if (or size srcsize)
          (let ((srcw (if srcsize (om-point-h srcsize) (gp:image-width image)))
                (srch (if srcsize (om-point-v srcsize) (gp:image-height image)))
                (destw (if size (om-point-h size) (gp:image-width image)))
                (desth (if size (om-point-v size) (gp:image-height image))))
            (gp::draw-image port image (+ (om-point-h pos) *pox*) (+ (om-point-v pos) *poy*)
                            :to-width  destw :to-height desth
                            :from-x (om-point-h srctopleft) :from-y (om-point-v srctopleft)
                            :from-width srcw :from-height srch 
                            :global-alpha a
                      ))
        (gp::draw-image port image (+ (om-point-h pos) *pox*) (+ (om-point-v pos) *poy*) :global-alpha a))
      ;(gp::free-image port image)
      
      (when (and port selected)
         #+(or cocoa gtk)
         (gp:draw-rectangle port (+ (om-point-h pos) *pox*) (+ (om-point-v pos) *poy*) 
                            (if size (om-point-h size) (gp:image-width image)) 
                            (if size (om-point-v size) (gp:image-height image)) 
                            :filled t :foreground (color::make-rgb (om-color-r *om-select-color*)
                                                                   (om-color-g *om-select-color*)
                                                                   (om-color-b *om-select-color*)
                                                                   0.1))
         ;; :operation 5
        ;;; créer un PIXMAP (depth=1) pour dessiner un masque transparent...
        #+win32
        (om-with-focused-view view
          (om-draw-hilite-icon (om-point-h pos) (om-point-v pos) 
                               (if size (om-point-h size) (gp:image-width image)) 
                               (if size (om-point-v size) (gp:image-height image)))
        ; (gp::draw-image  image (om-point-h topleft) (om-point-v topleft) :global-alpha 0.8 :operation 5))
          )
        )       
      )
    ))


(defmethod om-get-picture-size (picture)
 (let ((image (gp::load-image *record-view* picture))
       rep)
   (setf rep (om-make-point (gp::image-width image) (gp::image-height image)))
   (gp::free-image *record-view* image)
   rep))

(defmethod om-pict-width (picture)
  (let ((image (gp::load-image *record-view* picture))
        rep)
    (setf rep (gp::image-width image))
    (gp::free-image *record-view* image)
    rep))

(defmethod om-pict-height (picture)
  (let ((image (gp::load-image *record-view* picture))
        rep)
    (setf rep (gp::image-height image))
    (gp::free-image *record-view* image)
    rep))

;===========

; executes body drawing on an invisible pict which is returned

(defclass internal-picture ()
  ((themetafile :initform nil :initarg :themetafile :accessor themetafile)
   (size-x :initform 0 :initarg :size-x :accessor size-x)
   (size-y :initform 0 :initarg :size-y :accessor size-y)))

(defmethod om-kill-picture ((self internal-picture))
  (when (themetafile self)
    (capi::free-metafile (themetafile self)))
  (setf (themetafile self) nil))

(defmethod om-get-view ((self t))  (or *default-printer-port* self))

(defmethod om-draw-picture (view (pict internal-picture) &key (pos (om-make-point 0 0)) size
                                  (srctopleft (om-make-point 0 0)) (srcsize nil))
  
  (when (themetafile pict)
    (let* ((port (om-get-view view))
           (size (or size (om-view-size view)))
           (dx 0) (dy 0)
           (pw (om-width view))
           (ph (om-height view)))
      (if (om-item-view-p view) 
         ; #-win32
        (setf dx (item-x view) dy (item-y view))
          ;#+win32
          ;(setf dx (- (item-x view) (om-h-scroll-position port)) dy (- (item-y view) (om-v-scroll-position port)))                             
        (if (scroller-p view) (setf pw (om-point-h (om-field-size view)) ph (om-point-v (om-field-size view)))
          ))
      (let* ((x (om-point-h pos))
             (y (om-point-v pos))
             (w  (om-point-h size))
             (h (om-point-v size)))
        (gp::with-graphics-state (port :mask (list dx dy pw ph))
          #+cocoa(capi::draw-metafile port (themetafile pict) (+ x *pox*) (+ y *poy*) w h)  ; (+ x dx) (+ y dy) w h)
          #-cocoa(capi::draw-metafile port (themetafile pict) (+ x *pox*) (+ y *poy*) w h)
	  )
      ;(capi::draw-metafile port (themetafile pict) 0 0 (om-point-h size)  (om-point-v size))
      ))))


(defmethod om-internal-picture-to-pict  ((self internal-picture) view)
  (when (themetafile self)
    (capi::draw-metafile-to-image view (themetafile self))))

(defmethod om-get-picture-size ((picture internal-picture))
  (om-make-point (size-x picture) (size-y picture)))

(defmethod om-pict-width ((picture internal-picture))
  (size-x picture))

(defmethod om-pict-height ((picture internal-picture))
  (size-y picture))


(defmacro om-record-pict (font size &body body)
  `(let ((metafile (capi:with-internal-metafile  
                       (port :pane nil :bounds (list 0 0 (om-point-h ,size) (om-point-v ,size)))
                     (let 
                         ((*curstream* port)
                          (*pox* 0) (*poy* 0))
                       (om-with-font ,font
                                     ,@body)))))
     (make-instance 'internal-picture
                    :themetafile metafile
                    :size-x (om-point-h ,size)
                    :size-y (om-point-v ,size))))

#|
(defmacro om-record-pict (font size &body body)
  (let ((portname (gensym))
        (imagename (gensym))
        (metafilename (gensym)))
    `(let* ((w (om-point-x ,size))
            (h (om-point-y ,size))
            (,metafilename (capi:with-internal-metafile  
                               (,portname :pane nil :bounds (list 0 0 w h))
                             (let ((*curstream* ,portname)) 
                               (om-with-font ,font ,@body)))))
       (,imagename (capi::draw-metafile-to-image *curstream* ,metafilename))
       (capi::free-metafile ,metafilename)
       ,imagename)))

|#




(defmethod om-record-picture-in-pict (pict &optional (pos (om-make-point 0 0)) size)
  (when pict
    (let* ((pw (om-pict-width pict))
           (ph (om-pict-height pict))
           (destw (if size (om-point-h size) pw))
           (desth (if size (om-point-v size) ph))
           (port (gp::create-pixmap-port *record-view* (om-point-h size) (om-point-v size) 
                                         :background :white :clear t))
           (image (gp::load-image port pict)))
      (gp::draw-image *curstream* image (om-point-h pos) (om-point-v pos)
                      :from-width pw :from-height ph
                      :to-width destw :to-height desth)
      (gp::free-image port image)
      )))


(defun om-open-score-page (size font)
  (let ((port (gp::create-pixmap-port *record-view* (om-point-h size) (om-point-v size)  :clear t)))
     (gp::set-graphics-state port :font (if (gp::font-description-p font)
                                                   (gp::find-best-font port font)
                                                 font))
       port))

(defun om-close-score-page (port size)
  (let ((image (gp::make-image-from-port port 0 0 (om-point-h size) (om-point-v size))))
         (gp::externalize-image port image)))


;;;====================================================================
;;;CURSORS
;;;====================================================================

(defun om-make-cursor (name &optional (click-pos (om-make-point 0 0)))
  (declare (ignore click-pos))
    (let ((cursorpath (om-get-resource-file name *om-cursor-folder* *om-cursor-type*))
          (cursor nil))
    (when cursorpath
	(setf cursor (capi::load-cursor (list (list :cocoa cursorpath :x-hot (om-point-h click-pos) :y-hot (om-point-v click-pos))
                                              (list :gtk cursorpath :x-hot (om-point-h click-pos) :y-hot (om-point-v click-pos))                    
                                              (list :win32 cursorpath)))))
    cursor))


(defvar *om-wait-cursor* nil)
(defvar *om-arrow-cursor* nil)
(defvar *om-verti-size-cursor* nil)  
(defvar *om-horiz-size-cursor* nil)  
(defvar *om-resize-cursor* nil)  
(defvar *om-i-beam-cursor* nil)  
(defvar *om-cross-cursor* nil)
(defvar *om-hand-cursor* nil)

;;; init built-in cursors
(defun om-init-cursors ()
  (setf *om-wait-cursor*
        #+cocoa(om-make-cursor "wait-cursor" (om-make-point 8 8))
        #-cocoa :busy
        )
  
  (setf *om-arrow-cursor* nil) ;;;  (:top-left-arrow)
  
  (setf *om-horiz-size-cursor* :h-double-arrow)  ;;; (om-make-cursor "cursor-ruler" (om-make-point 8 8)))
  
  (setf *om-verti-size-cursor* :v-double-arrow) ;;; (om-make-cursor "staff-cursor" (om-make-point 8 8))) 
  
  (setf *om-resize-cursor* 
                #+cocoa(om-make-cursor "resize-cursor" (om-make-point 8 8))
                #-cocoa :bottom-right-corner
                )
  
  (setf *om-i-beam-cursor* :i-beam)  
  
  (setf *om-cross-cursor* 
         #+cocoa(om-make-cursor "croix" (om-make-point 8 8))
         #-cocoa :fleur     ;;; :crosshair
         )
  
  (setf *om-hand-cursor*
                 #+cocoa  :open-hand
                 #-cocoa (om-make-cursor "hand-cursor")
                 )
)


#+win32(setf win32::*change-cursor-on-gc* nil)

(om-api-add-init-func 'om-init-cursors)

;; ça marche pas
(defmacro om-with-cursor (cursor &body body)
  `(let ()
     ;((view (om-find-view-containing-point (om-front-window) (om-mouse-position (om-front-window)))))
          ;(update-view-cursor view)
     ,@body
     ;(setf oa::*maxi-cursor* nil)
     ;(setf (capi::simple-pane-cursor view) (om-view-cursor view))
     ;(update-view-cursor (om-find-view-containing-point (om-front-window) (om-mouse-position (om-front-window))))
   ))


;;;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; SAVE / LOAD PICTURES
; (internalize-image *graph-pres*))

;;; IMAGES ARE NOT ALWAYS FREED !!!!!

(defvar *temp-pictwin* nil)
(defvar *temp-pictlayout* nil)

(defun init-pictwin ()
  (setf *temp-pictlayout* (make-instance 'capi:pinboard-layout)
        *temp-pictwin* (capi:display (make-instance 'capi:interface
                                           :display-state :hidden
                                           :layout *temp-pictlayout*))))


(defun ensure-pict-win ()
  (or (and *temp-pictwin* *temp-pictlayout*) 
      (init-pictwin)))

(defun clean-pict-win ()
  (when *temp-pictwin*
    (capi::destroy *temp-pictwin*)))

(om-add-exit-cleanup-func 'clean-pict-win)

(defmethod om-externalize-image ((img gp::image))
  (ensure-pict-win)
  (gp::externalize-image *temp-pictlayout* img))

(defmethod om-externalize-image ((img gp::external-image)) img)

(defmethod om-internalize-image ((img gp::image)) img)

(defmethod om-internalize-image ((img gp::external-image))
  (ensure-pict-win)
  (gp::load-image *temp-pictlayout* img :force-plain t))


 ;         (let* ((height (gp:image-access-height access))
 ;                (width (gp:image-access-width access))

;;; !!! ARRAY EN BGRA
(defun image-to-arrayvector (img)
  (ensure-pict-win)
  (let* ((intimg (om-internalize-image img))
         (ia (gp::make-image-access *temp-pictlayout* intimg))
         (w (gp:image-access-width ia))
         (h (gp:image-access-height ia))
         (bgra-vector (make-array (* h w 4)
                                  :element-type '(unsigned-byte 8)))
         (color-array (make-array (list h w 4)
                                  :element-type '(unsigned-byte 8)
                                  :displaced-to bgra-vector))
         (pix nil) (color nil))
  (gp::image-access-transfer-from-image ia)
  (gp:image-access-pixels-to-bgra ia bgra-vector)
  (gp::free-image-access ia)
  color-array))


(defmethod image-to-arraylist (img)
  (ensure-pict-win)

  (let* ((intimg (om-internalize-image img))
         (ia (gp::make-image-access *temp-pictlayout* intimg))
         (w (gp:image-access-width ia))
         (h (gp:image-access-height ia))
         color-array
         (pix nil) (color nil))
    (gp::image-access-transfer-from-image ia)
    (setf color-array
          (loop for j from 0 to (- h 1) collect
                (loop for i from 0 to (- w 1) 
                      do (setf color (color::get-color-spec (color::ensure-rgb (color::unconvert-color *temp-pictlayout* (gp::image-access-pixel ia i j)))))
                      collect
                      (list (aref color 1) (aref color 2) (aref color 3) (if (> (length color) 4) (aref color 4) 1.0)))))
    (gp::free-image-access ia)
      ;(capi:destroy win)
    color-array))

(defun pix-list-to-image (color-array)   
  (ensure-pict-win)
  (capi::hide-interface *temp-pictlayout* nil)
  (let* ((img1 (gp::make-image *temp-pictlayout* (length (car color-array)) (length color-array) :alpha t))
         (ia (gp::make-image-access *temp-pictlayout* img1))
         img2)
    (loop for line in color-array 
          for j = 0 then (+ j 1) do
          (loop for pix in line 
                for i = 0 then (+ i 1) do 
                (setf (gp::image-access-pixel ia i j) 
                      (color::convert-color *temp-pictlayout* (if (consp pix)
                                                                  (color::make-rgb (nth 0 pix) (nth 1 pix) (nth 2 pix) (nth 3 pix))
                                                                (color::make-rgb pix pix pix 1))))))
    (gp::image-access-transfer-to-image ia)
    (setf img2 (gp::externalize-image *temp-pictlayout* img1))
    (gp::free-image-access ia)
    (gp::free-image *temp-pictlayout* img1)
    img2))

;;; !!! ARRAY EN BGRA
(defun pix-array-to-image (array)   
  (ensure-pict-win)
  (let ((color-array array)
        (h (array-dimension array 0))
        (w (array-dimension array 1)))
    (when (= 2 (array-rank array))
      (setf color-array (make-array (list h w 4)
                                    ;:element-type '(unsigned-byte 8)
                                    ))
      (dotimes (i h)
        (dotimes (j w)
          (let ((pixel (aref array i j)))
            (cond ((numberp pixel)
                   (setf (aref color-array i j 0) pixel)
                   (setf (aref color-array i j 1) pixel)
                   (setf (aref color-array i j 2) pixel)
                   (setf (aref color-array i j 3) 1))
                  ((consp pixel)
                   (setf (aref color-array i j 0) (car pixel))
                   (setf (aref color-array i j 1) (cadr pixel))
                   (setf (aref color-array i j 2) (caddr pixel))
                   (setf (aref color-array i j 3) (or (cadddr pixel) 1)))))))
      )
    
    (let ((bgra-vector (make-array (* h w 4)
                                   ;:element-type '(unsigned-byte 8)
                                   :displaced-to color-array)))
      (or (and *temp-pictwin* *temp-pictlayout*) (init-pictwin))
      (capi::hide-interface *temp-pictlayout* nil)
      (let* ((img1 (gp::make-image *temp-pictlayout* w h :alpha t))
             (ia (gp::make-image-access *temp-pictlayout* img1))
             img2)
        (gp:image-access-pixels-from-bgra ia bgra-vector)
        (gp::image-access-transfer-to-image ia)
        (setf img2 (gp::externalize-image *temp-pictlayout* img1))
        (gp::free-image-access ia)
        (gp::free-image *temp-pictlayout* img1)
        img2)
      )))

(defun om-create-picture (array)
  (cond ((arrayp array) (pix-array-to-image array))
        ((consp array) (pix-list-to-image array))
        (t nil)))

(defun om-picture-values (pict)
  (let ((w (car (capi::collect-interfaces 'om-abstract-window :screen :any :sort-by :visible)))
        (pictarray (image-to-arraylist pict)))
    (om-select-window w)
    pictarray))

(defun om-picture-array (pict)
  (let ((w (car (capi::collect-interfaces 'om-abstract-window :screen :any :sort-by :visible)))
        (pictarray (image-to-arrayvector pict)))
    (om-select-window w)
    pictarray))

(defmethod om-save-picture (pict path)
  (gp::write-external-image (om-externalize-image pict) path :if-exists :supersede)
  path)

(defmethod om-save-picture ((pict internal-picture) path)
  (gp::write-external-image 
   (gp::externalize-image *record-view* (om-internal-picture-to-pict pict *record-view*))
   path :if-exists :supersede)
  path)

