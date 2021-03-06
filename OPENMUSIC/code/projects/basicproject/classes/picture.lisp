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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

(in-package :om)


;;; OM OBJECT
(defclass! picture (internepicture) 
   ((background :initarg :background :initform nil :accessor background :documentation "main background: external picture pathname or RGB array")
    (graphics :initarg :graphics :initform nil :accessor graphics :documentation "list of vectorial graphics"))
   (:icon 491)
   (:documentation "
The PICTURE box allows to display external images and draw simple graphics.

<background> allows to set or read the main background. It can be a pathname or a pixel array (list of lists of pixels (R G B alpha) between 0.0 and 1.0.)

<graphics> correspond to a list of vectorial graphics (graphic-object) displayed on top of the background picture.

Using the contextual menu on this box, the picture can be converted to a background picture for the patch.
Then use 'y' key to select then move, resize or delete background pictures, or convert them back to picture boxes with the same contextual menu.

The same contextual menu allow to choose to save or not the contents of the picture inside the patch. If this option is not enabled, the picture will need to be recomputed each time the patch is loaded.
"))


(defmethod! picture-hdl ((self picture))
   :icon 491
   :doc "Get the picture handle of <self> (a picture)."
   (thepict self))

(defmethod draw-obj-in-rect ((self picture) x x1 y y1 edparams view)
   (if (thepict self)
       (om-draw-picture view (thepict self) :pos (om-make-point x y) :size (om-make-point (- x1 x) (- y1 y))))
   (loop for o in (extraobjs self) do (draw-pict-extraobj view o)))


(defmethod update-miniview ((self t) (type picture)) (om-invalidate-view self t))

(defmethod get-initval ((self picture)) (make-instance 'picture))

(defmethod default-obj-box-size ((self picture)) (om-make-point 80 80))

(defmethod make-one-instance ((class picture) &rest slots-vals)
   (if (find nil slots-vals :test-not 'equal)
       (let ((rep (call-next-method)))
         (setf (graphics rep) (slot-value rep 'graphics))
         (when (car slots-vals) (setf (background rep) (car slots-vals)))
         rep)
   (or (get-picture-file) 
       (om-abort))))

(defmethod* get-picture-file () 
   :initvals nil
   :indoc nil
   :doc "load a pict file"
   :icon 148
  (let* ((pict-info (choose-resource-dialog :pict :kernel t :user t));;changed
         name)
    (when (consp pict-info)
        ;(setf name (intern (string+ (string (third pict-info)) "-" (string (first pict-info)))))
        (let ((rep (make-instance 'picture :name (first pict-info))))
          (setf (thepict rep) (om-get-picture (first pict-info) (third pict-info)))
          (setf (name rep) (first pict-info))
          (setf (source rep) (third pict-info))
          rep))))

;(defun load-pict (name)
;   (om-load-if name 'load-picture))

;(defmethod background ((self picture))
;  (when (thepict self)
;    (if (equal (storemode self) :external)
;        (or (pict-pathname self)
;            (list (source self) (name self)))
;      (om-picture-values (thepict self)))))

(defmethod background ((self picture))
  (when (thepict self)
    (or (pict-pathname self)
        (and (source self) (list (source self) (name self)))
        (om-picture-values (thepict self))
        (slot-value self 'background)
        )))

(defmethod (setf background) (bg (self picture))
  (setf (slot-value self 'background) bg)
  (cond ((pathnamep bg) (path-to-picture self bg))
        ((consp bg) (list-to-picture self bg))
        ((arrayp bg) (array-to-picture self bg))
        (t nil))
  bg)


(defun path-to-picture (pictureobj file)
  (when (and (probe-file file) (check-file-type file *om-pict-type*))
    (let ((pict (om-load-pixmap (pathname-name file) (pathname-type file) 
                                (make-pathname :directory (pathname-directory file) 
                                               :host (pathname-host file) :device (pathname-device file)))))
      (when pict
        (setf (thepict pictureobj) pict
              (pict-pathname pictureobj) file
              (source pictureobj) nil
              (name pictureobj) (pathname-name file)
              ;(storemode pictureobj) :external
              ))
      )))

(defun list-to-picture (pictureobj list)
  (when  (listp (car list)) 
    (when (numberp (caar list)) 
      (setf list (mapcar #'(lambda (line) (mapcar #'(lambda (pixel) (list pixel pixel pixel)) line)) list)))
    (when (and (listp (caar list)) (numberp (caaar list)))
      (setf (thepict pictureobj) (om-create-picture list)
            (pict-pathname pictureobj) nil
            (source pictureobj) nil
            (name pictureobj) "internal pixel array"
            ;(storemode pictureobj) :internal
            )
      )))

(defun array-to-picture (pictureobj array)
  (setf (thepict pictureobj) (om-create-picture array)
        (pict-pathname pictureobj) nil
        (source pictureobj) nil
        (name pictureobj) "internal pixel array"
        ;(storemode pictureobj) :internal
        ))



(defmethod! save-picture ((self picture) &optional (path nil) (with-graphics nil) (size nil))
   :initvals nil
   :indoc '("a picture object" "a pathname" "bg pixels or full picture" "size")
   :doc "Saves picture <self> in a file.
Exports as a raw bitmap (TIF format)

<path> must be a target pathname, or a file chooser dialog will ask one.

<with-graphics> (t or nil) determines wether the possible aadditional graphics in picture should be included in the exported picture.

<size> allows to set a particular size for the file (by default, will be the original size of the picture background).
"
   :icon 491
   (let ((file (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                   :types '("TIFF File" "*.tiff;*.tif" 
                                                            "BMP File" "*.bmp"
                                                            "JPEG File" "*.jpg;*.jpeg" 
                                                            "PNG File" "*.png" 
                                                            )))))
     (when file
       (setf *last-saved-dir* (make-pathname :directory (pathname-directory file)))
       (if (or (and with-graphics (extraobjs self)) size)
           (let* ((pictsize (or size (and (thepict self) (om-get-picture-size (thepict self)))
                                (om-make-point 100 100)))
                  (tmppict (om-record-pict nil pictsize 
                             (if (thepict self) 
                               (om-record-picture-in-pict (thepict self) (om-make-point 0 0) pictsize)
                               (om-with-fg-color nil *om-white-color* 
                                 (om-fill-rect 0 0 (om-point-h pictsize) (om-point-v pictsize))))
                             (when with-graphics 
                               (loop for o in (extraobjs self) do (draw-pict-extraobj nil o (om-make-point 0 0) pictsize)))
                             )))
             (om-save-picture tmppict file :tiff))
         (if (thepict self)
             (om-save-picture (thepict self) file :tiff)
           (progn (om-beep-msg "nothing to save...") (om-abort)))))))


;====================================================
;BOX and frame

(defmethod get-type-of-ed-box ((self picture))  'OMpicturebox)

(defclass OMPictureBox (OMBoxEditCall) ())

(defmethod get-frame-class ((self OMPictureBox)) 'PictboxFrame)

;(defmethod default-edition-params ((self picture)) 
;  (pairlis '(winsize winpos save-data) 
;           (list (or (get-win-ed-size self) (om-make-point 370 280))
;                 (or (get-win-ed-pos self) (om-make-point 400 20))
;                 t)))



;---------
;FRAME
(defclass PictboxFrame (boxEditorFrame) ()
   (:documentation "Simple frame for OMBoxEditCall meta objects. #enddoc#
#seealso# (OMBoxEditCall) #seealso#"))

(defmethod om-get-menu-context ((self PictboxFrame))
  (list (om-new-leafmenu  "Set as Background Picture" #'(lambda () (pict2bkg self)))
        (pict-save-menu (object self))))

;disabled
;(defun pict-save-menu (box)
;  (om-new-leafmenu (if (get-edit-param box 'save-data) "Do Not Save Picture Data with Patch" "Save Picture Data with Patch")
;                   #'(lambda () (set-edit-param box 'save-data (not (get-edit-param box 'save-data)))
;                       (setf (storemode (value box)) (if (get-edit-param box 'save-data) :internal :external)))))

(defun pict-save-menu (box)
  (om-new-leafmenu (if (equal (storemode (value box)) :internal) "Do Not Save Picture Data with Patch" "Save Picture Data with Patch")
                   #'(lambda () 
                       (setf (storemode (value box)) (if (equal (storemode (value box)) :internal) :external :internal)))
                   nil (not (source (value box)))))

  
;;; Redefinition : patch-picture contextual menu
(defmethod get-pict-menu-context ((object patch-picture) patch)
  (list (om-new-leafmenu "Make Box" #'(lambda () (bgk2pict object patch)))))


(defmethod bgk2pict ((self patch-picture) patch)
  (let ((picture (copy-picture self 'picture))
        mynew-pict)
     (setf newcall (omNG-make-new-boxcall (find-class 'picture) 
                                          (om-subtract-points (pict-pos self) (om-make-point 0 8))
                                          (mk-unique-name patch "pict")))  
     (setf (value newcall) picture)
     (setf (frame-size newcall) (om-add-points (pict-size self) (om-make-point 0 16)))
     (setf (showpict newcall) t)
     (omG-add-element patch (make-frame-from-callobj newcall))
     (setf (pictu-list (object patch)) (remove self (pictu-list (object patch)) :test 'equal))
     (om-invalidate-view patch t)))

(defmethod pict2bkg ((self PictboxFrame))
   (let* ((patchpanel (om-view-container self))
          newpict)
     (when (or (thepict (value (object self)))
               (extraobjs (value (object self))))
       (setf newpict (copy-picture (value (object self)) 'patch-picture))
       (setf (pict-pos newpict) (om-add-points (om-view-position self) (om-view-position (iconview self))))
       (setf (pict-size newpict) (om-view-size (iconview self)))
       (push newpict (pictu-list (object patchpanel)))
       (real-make-delete-before patchpanel (list self))
       (omg-remove-element patchpanel self)
       (om-invalidate-view patchpanel t)
       )))
   
(defmethod reinit-size ((boxframe PictboxFrame))
  (let ((thepict (thepict (value (object boxframe)))))
    (when thepict
      (let* ((pict-size (om-get-picture-size thepict))
             (pw (om-point-h pict-size)) (ph (om-point-v pict-size))
             (ref-h (- (h boxframe) 16)) (ref-w (w boxframe))
             (new-size (if (> ref-w ref-h) 
                           (om-make-point ref-w (* ref-w (/ ph pw)))
                         (om-make-point (* ref-h (/ pw ph)) ref-h))))
        (setf (frame-size (object boxframe)) (om-add-points new-size (om-make-point 0 16)))
        (box-draw-connections boxframe nil)
        (omG-select (redraw-frame boxframe))))))



;;;===================
;;; EDITOR


(defmethod class-has-editor-p ((self picture)) t)

(defmethod get-editor-class ((self picture)) 'picteditor)

(defmethod good-val-p? ((self picture))
   ;(thepict self) 
  t)

(defclass pictEditor (editorview) 
  ((mode :initform :normal :accessor mode)
   (selection :initform nil :accessor selection)
   (controlview :initform nil :accessor controlview)))

(defclass pictpanel (om-view om-drag-view om-drop-view) ())

(defmethod editor ((self pictpanel)) (om-view-container self))

(defmethod get-help-list ((self picteditor)) 
  '((("tab" "Select a Graphic Object")
     ("del" "Delete Selected Object"))))

(defmethod get-menubar ((self picteditor)) (list (om-make-menu "File"
                                                               (list (om-new-leafmenu "Save Picture" #'(lambda () (save-pict self)) "s")
                                                                     (om-new-leafmenu "Close" #'(lambda () (om-close-window (window self))) "w")))    
                                                 (om-make-menu "Edit"
                                                               (list (list 
                                                                      (om-new-leafmenu "Load Picture" #'(lambda () (load-new-pict self)))
                                                                      (om-new-leafmenu "Remove Picture" #'(lambda () (remove-pict self))
                                                                                       nil (thepict (object self))))
                                                                     (om-new-leafmenu "Remove All Graphics" #'(lambda () (remove-all-extraobjs self)))))
                                                 (make-om-menu 'help :editor self)))

(defmethod initialize-instance :after ((self picteditor) &rest args)
  (om-set-bg-color self (om-make-color 0.8 0.8 0.8))
  (om-add-subviews self
                   (setf (panel self) (om-make-view 'pictpanel
                                                    :position (om-make-point 0 0)
                                                    :size (om-make-point (w self) (- (h self) 40))))
                   (setf (controlview self) (om-make-view 'pict-controls
                                                          :position (om-make-point 0 (- (h self) 40))
                                                          :size (om-make-point (w self) 40)
                                                          :bg-color *om-light-gray-color*))))

(defmethod update-subviews ((self picteditor))
  (let ((pict (thepict (object self)))
        (vw (w self)) (vh (- (h self) 40)))
    (if pict
      (let ((pw (om-pict-width pict)) (ph (om-pict-height pict)))
        (if (<= (* vw (/ ph pw)) vh)
            (let ((vh2 (* vw (/ ph pw))))
              (om-set-view-size (panel self) (om-make-point vw vh2))
              (om-set-view-position (panel self) (om-make-point 0 (- (/ vh 2) (/ vh2 2)))))
          (let ((vw2 (* vh (/ pw ph))))
            (om-set-view-size (panel self) (om-make-point vw2 vh))
            (om-set-view-position (panel self) (om-make-point (- (/ vw 2) (/ vw2 2)) 0)))))
      (progn
        (om-set-view-size (panel self) (om-make-point vw vh))
        (om-set-view-position (panel self) (om-make-point 0 0))))
    (om-set-view-size (controlview self) (om-make-point (w self) 40))
    (om-set-view-position (controlview self) (om-make-point 0 (- (h self) 40)))))

(defmethod load-new-pict ((self picteditor))
  (let ((tmppict (get-picture-file)))
    (when (and tmppict (thepict tmppict))
      (setf (thepict (object self)) (thepict tmppict))
      (setf (name (object self)) (name tmppict))
      (setf (source (object self)) (source tmppict))
      (setf (storemode (object self)) :external)
      (om-invalidate-view (panel self))
      (om-add-menu-to-win (om-view-window self)))))

(defmethod remove-pict ((self picteditor))
  (setf (thepict (object self)) nil)
  (setf (name (object self)) nil)
  (setf (source (object self)) nil)
  (om-invalidate-view (panel self))
  (om-add-menu-to-win (om-view-window self)))

(defmethod save-pict ((self picteditor))
  (save-picture (object self) nil t))


(defmethod remove-all-extraobjs ((self picteditor))
  (setf (extraobjs (object self)) nil)
  (setf (selection self) nil)
  (om-invalidate-view (panel self)))


(defmethod handle-key-event ((self pictpanel) key)
  (cond ((and (equal (mode (editor self)) :text) *draw-text*)
         (cond ((equal key :om-key-delete)
                (unless (= 0 (length (cadr *draw-text*))) 
                  (setf (cadr *draw-text*) (subseq (cadr *draw-text*) 0 (- (length (cadr *draw-text*)) 1))))
                (report-modifications (editor self)))
               ((equal key :om-key-return)
                ;(setf (cadr *draw-text*) (concatenate 'string (cadr *draw-text*) (string #\Newline)))
                (finish-text-extra self)
                (report-modifications (editor self)))
               ((characterp key) ;;; + eviter les caract�res sp�ciaux !!
                (setf (cadr *draw-text*) (concatenate 'string (cadr *draw-text*) (string key)))
                (report-modifications (editor self)))
               (t nil)))
        (t (case key 
             (#\h (show-help-window "Picture Editor commands..." (get-help-list (editor self))))
             (:om-key-tab 
              (when (extraobjs (object (editor self)))
                (if (selection (editor self)) 
                    (setf (selection (editor self)) 
                          (mod (+ (selection (editor self)) 1) (length (extraobjs (object (editor self))))))
                  (setf (selection (editor self)) (- (length (extraobjs (object (editor self)))) 1)))))
             (:om-key-delete 
              (when (selection (editor self))
                (setf (extraobjs (object (editor self))) (remove (nth (selection (editor self)) (extraobjs (object (editor self))))
                                                                 (extraobjs (object (editor self)))))
                (setf (selection (editor self)) 
                      (if (zerop (length (extraobjs (object (editor self))))) nil
                        (min (selection (editor self)) (- (length (extraobjs (object (editor self)))) 1))))
                (report-modifications (editor self))
                )))))
  (om-invalidate-view self))
        
(defvar *draw-polyg* nil)
(defvar *draw-text* nil)
(defvar *draw-pen* nil)

(defmethod om-view-click-handler ((self pictpanel) pos)
  (unless (equal (mode (editor self)) :normal) (setf (selection (editor self)) nil))
  (unless (equal (mode (editor self)) :polygon) (setf *draw-polyg* nil))
  (case (mode (editor self))
    (:normal (call-next-method)) ;(move-pict-object self pos))
    (:pen (add-pen-extra self pos))
    (:line (add-line-extra self pos))
    (:arrow (add-fleche-extra self pos))
    (:rect (add-rect-extra self pos))
    (:ellipse (add-cerc-extra self pos))
    (:polygon (polygon-extra-clic self pos))
    (:text (text-extra-clic self pos))
    (otherwise t))
  (om-invalidate-view self))

(defmethod om-click-motion-handler ((self pictpanel) pos)
  (unless (equal (mode (editor self)) :normal) (setf (selection (editor self)) nil))
  (when (and (equal (mode (editor self)) :pen) *draw-pen*)
    (let ((pt (list (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self))))
          (lastpt (car (last *draw-pen*))))
          (unless (and (= (car lastpt) (car pt)) (= (cadr pt) (cadr lastpt)))
            (pushr pt *draw-pen*)))
    (om-invalidate-view self)))

(defmethod om-click-release-handler ((self pictpanel) pos)
  (unless (equal (mode (editor self)) :normal) (setf (selection (editor self)) nil))
  (when (and (equal (mode (editor self)) :pen) *draw-pen*)
    (let ((ctrl (controlview (editor self))))
      (pushr (list 'pen 
                  (copy-list *draw-pen*)
                  (list (currentcolor ctrl) (currentsize ctrl)
                        (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
                        (currentfill ctrl))
                  nil)
            (extraobjs (object (editor self))))))
  (setf *draw-pen* nil)
  (report-modifications (editor self))
  (om-invalidate-view self))

(defmethod add-pen-extra ((self pictpanel) pos)
  (setf *draw-pen* (list (list (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self))))))

(defmethod om-view-doubleclick-handler ((self pictpanel) pos)
  (setf (selection (editor self)) nil)
  (when (equal (mode (editor self)) :polygon) (finish-polygon self pos))
  (setf *draw-polyg* nil)
  (om-invalidate-view self))

(defmethod polygon-extra-clic ((self pictpanel) pos)
  (pushr (list (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self))) *draw-polyg*)
  (om-invalidate-view self))

(defmethod finish-polygon ((self pictpanel) pos)
  (let ((ctrl (controlview (editor self))))
    (polygon-extra-clic self pos)
    (pushr (list 'polyg 
                (copy-list *draw-polyg*)
                (list (currentcolor ctrl) (currentsize ctrl)
                      (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
                      (currentfill ctrl))
                nil)
          (extraobjs (object (editor self))))
    (om-invalidate-view self)))

(defmethod text-extra-clic ((self pictpanel) pos)
  (if *draw-text*
      (finish-text-extra self)
    (setf *draw-text* (list (list (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self))) "")))
  (om-invalidate-view self))

(defmethod finish-text-extra ((self pictpanel))
  (unless (string-equal "" (cadr *draw-text*))
    (let ((ctrl (controlview (editor self))))
      (pushr (list 'text (car *draw-text*) 
                  (list (currentcolor ctrl) (currentsize ctrl)
                        (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
                        (currentfont ctrl))
                  (cadr *draw-text*)) 
            (extraobjs (object (editor self))))))
  (setf *draw-text* nil)
  (om-invalidate-view self))
                

(defmethod add-line-extra ((self pictpanel) pos)
  (om-init-motion-click 
   self pos
   :motion-draw 'draw-line-extra :display-mode 2
   :release-action 'release-line-extra))

(defmethod draw-line-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (om-with-fg-color self (currentcolor ctrl)
                      (om-with-line (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
                        (om-with-line-size (currentsize ctrl)
                          (om-draw-line (om-point-x initpos) (om-point-y initpos) (om-point-x pos) (om-point-y pos)))))
    ))

(defmethod release-line-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (unless (om-points-equal-p initpos pos)
      (pushr (list 'line 
                  (list (/ (om-point-h initpos) (w self)) (/ (om-point-v initpos) (h self)) 
                        (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self)))
                  (list (currentcolor ctrl) (currentsize ctrl) 
                        (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2 (currentsize ctrl))) (currentline ctrl))
                        (currentfill ctrl))
                  nil)
            (extraobjs (object (editor self))))
      ))
  (om-invalidate-view self))


(defmethod add-fleche-extra ((self pictpanel) pos)
  (om-init-motion-click 
   self pos
   :motion-draw 'draw-line-extra :display-mode 2
   :release-action 'release-fleche-extra))

(defmethod release-fleche-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (unless (om-points-equal-p initpos pos)
      (pushr (list 'arrow 
                   (list (/ (om-point-h initpos) (w self)) (/ (om-point-v initpos) (h self)) 
                         (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self)))
                   (list (currentcolor ctrl) (currentsize ctrl) 
                         (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2 (currentsize ctrl))) (currentline ctrl))
                         (currentfill ctrl))
                   nil)
             (extraobjs (object (editor self))))
      ))
  (om-invalidate-view self))


(defmethod add-rect-extra ((self pictpanel) pos)
  (om-init-motion-click 
   self pos
   :motion-draw 'draw-rect-extra :display-mode 20
   :release-action 'release-rect-extra))

(defmethod draw-rect-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (multiple-value-bind (x y w h) (om-points-to-rect initpos pos)
      (om-with-fg-color self (currentcolor ctrl)
        (if (currentfill ctrl)
            (om-fill-rect x y w h)
          (om-with-line (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
            (om-with-line-size (currentsize ctrl)
              (om-draw-rect x y w h))))
        ))))

(defmethod release-rect-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (unless (om-points-equal-p initpos pos)
      (pushr (list 'rect 
                  (list (/ (om-point-h initpos) (w self)) (/ (om-point-v initpos) (h self)) 
                        (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self)))
                  (list (currentcolor ctrl) (currentsize ctrl) 
                        (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2 (currentsize ctrl))) (currentline ctrl))
                        (currentfill ctrl))
                  nil)
            (extraobjs (object (editor self)))))
    (om-invalidate-view self)))


(defmethod add-cerc-extra ((self pictpanel) pos)
  (om-init-motion-click 
   self pos
   :motion-draw 'draw-cercle-extra :display-mode 6
   :release-action 'release-cercle-extra))

(defmethod draw-cercle-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (multiple-value-bind (x y w h) (om-points-to-rect initpos pos)
      (om-with-fg-color self (currentcolor ctrl)
        (if (currentfill ctrl)
            (om-fill-ellipse (+ x (/ w 2)) (+ y (/ h 2)) (max 1 (/ w 2)) (max 1 (/ h 2)))
          (om-with-line (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
            (om-with-line-size (currentsize ctrl)
              (om-draw-ellipse (+ x (/ w 2)) (+ y (/ h 2)) (max 1 (/ w 2)) (max 1 (/ h 2))))))
        ))))

(defmethod release-cercle-extra ((self pictpanel) initpos pos)
  (let ((ctrl (controlview (editor self))))
    (unless (om-points-equal-p initpos pos)
       (multiple-value-bind (x y w h) (om-points-to-rect initpos pos)
         (pushr (list 'cercle 
                  (list (/ (+ x (/ w 2)) (w self))
                        (/ (+ y (/ h 2)) (h self)) 
                        (/ w 2 (w self))
                        (/ h 2 (h self)))
                  (list (currentcolor ctrl) (currentsize ctrl) 
                        (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2 (currentsize ctrl))) (currentline ctrl))
                        (currentfill ctrl))
                  nil)
            (extraobjs (object (editor self))))
      )))
  (om-invalidate-view self))

;;;==========================

(defmethod om-draw-contents ((self pictpanel))
  (let ((pict (thepict (object (editor self))))
        (objs (if (selection (editor self)) (list (nth (selection (editor self)) (extraobjs (object (editor self)))))
                (extraobjs (object (editor self))))))
    (om-with-focused-view self
    (when pict
      (om-draw-picture self pict :size (om-view-size self)))
    (if (selection (editor self))
        (loop for o in (remove (car objs) (extraobjs (object (editor self)))) do (draw-pict-extraobj self o nil nil :bg t)))
    (loop for o in objs do (draw-pict-extraobj self o))
    (when *draw-polyg*
      (loop for (a b) on *draw-polyg* by #'cdr do 
            (when (and a b) 
              (draw-pict-extraobj self (list 'line (list (car a) (cadr a) (car b) (cadr b))
                                             (list *om-dark-gray-color* (currentsize (controlview (editor self))) 'dash nil) nil))
              )))
    
    (when(extraobjs (object (editor self)))
      (om-with-fg-color self (om-make-color-alpha 0.6 0.6 0.6 0.6) 
        (om-with-focused-view self
          (om-with-font *controls-font*
                        (om-draw-string (- (w self) 130) 20
                                        (if (selection (editor self))
                                            (format nil "Selection = Figure ~D" (selection (editor self)))
                                          "No selection"))))))
    
    (when *draw-text*
      (draw-pict-extraobj self (list 'rect (list (caar *draw-text*) 
                                                 (- (cadar *draw-text*) (/ (* (om-string-h (currentfont (controlview (editor self))))) (h self)))
                                                 (+ (caar *draw-text*) (/ (om-string-size (string+ (cadr *draw-text*) "    ")
                                                                                          (currentfont (controlview (editor self)))) (w self)))
                                                 (+ (cadar *draw-text*) (/ (* (om-string-h (currentfont (controlview (editor self)))) 0.4) (h self))))
                                     (list *om-dark-gray-color* 1 'dash nil) nil))
      (draw-pict-extraobj self (list 'text (list (caar *draw-text*) (cadar *draw-text*))
                                     (list (currentcolor (controlview (editor self))) 12 'normal (currentfont (controlview (editor self)))) (cadr *draw-text*)))
      )
    (when *draw-pen*
      (loop for (a b) on *draw-pen* by #'cdr do 
            (if (and a b) 
		(draw-pict-extraobj self (list 'line (list (car a) (cadr a) (car b) (cadr b))
					       (list (currentcolor (controlview (editor self))) (currentsize (controlview (editor self))) 
						     (currentline (controlview (editor self))) nil) nil))
		(if a  (draw-pict-extraobj self (list 'line (list (car a) (cadr a) (car a) (cadr a))
						      (list (currentcolor (controlview (editor self))) (currentsize (controlview (editor self))) 
							    (currentline (controlview (editor self))) nil) nil)))
		)))
    )))


(defmethod draw-pict-extraobj ((self t) obj &optional pos size &key (bg nil))
  (let ((op (car obj))
        (points (cadr obj))
        (params (caddr obj))
        (data (cadddr obj))
        (x0 (if pos (om-point-h pos) 0))
        (y0 (if pos (om-point-v pos) 0))
        (xfact (if size (om-point-h size) (w self)))
        (yfact (if size (om-point-v size) (h self))))
  ;(om-with-focused-view self
    (om-with-fg-color nil (if bg (om-make-color-alpha (om-color-r (car params)) (om-color-g (car params)) (om-color-b (car params)) 0.5)
                             (car params))
      (om-with-line-size (cadr params)
        (om-with-line (caddr params)
          
          (case op
            ('line (om-draw-line (+ (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0)
                                 (+ (* (nth 2 points) xfact) x0) (+ (* (nth 3 points) yfact) y0))
                   )
            ('arrow 
             (let* ((k1 0.06) (k2 0.08)
                    (xa (+ (* (nth 0 points) xfact) x0)) (ya (- (h self) (+ (* (nth 1 points) yfact) y0)))
                    (xb (+ (* (nth 2 points) xfact) x0)) (yb (- (h self) (+ (* (nth 3 points) yfact) y0)))
                    (ab (sqrt (+ (sqr (- xb xa)) (sqr (- yb ya)))))
                    (d (* ab (- 1 k1)))
                    (lsur2 (/ (* ab k2) 2))
                    (cosa (/ (- xb xa) ab))
                    (sina (/ (- yb ya) ab))
                    (xc (+ xa (* d cosa)))
                    (yc (+ ya (* d sina)))    
                    (xi (- xc (* sina lsur2))) 
                    (yi (+ yc (* cosa lsur2)))
                    (xj (+ xc (* sina lsur2)))
                    (yj (- yc (* cosa lsur2))))
               (setf ya (- (h self) ya) yb (- (h self) yb)
                     yi (- (h self) yi) yj (- (h self) yj))
               
               (om-draw-line xa ya xb yb) 
               (om-draw-line xb yb xi yi)
               (om-draw-line xb yb xj yj)
               ))

            ('rect (if (cadddr params)
                       (om-fill-rect (+ (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0)
                                     (* (- (nth 2 points) (nth 0 points)) xfact) (* (- (nth 3 points) (nth 1 points)) yfact))
                       (om-draw-rect (+ (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0)
                                     (* (- (nth 2 points) (nth 0 points)) xfact) (* (- (nth 3 points) (nth 1 points)) yfact)
                                     :pensize (cadr params))))
            ('cercle (if (cadddr params)
                       (om-fill-ellipse (+ (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0)
                                        ;(* (- (nth 2 points) (nth 0 points)) xfact) (* (- (nth 3 points) (nth 1 points)) yfact)
                                        (max 1 (* (nth 2 points) xfact)) (max 1 (* (nth 3 points) yfact))
                                        )
                       (om-draw-ellipse (+  (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0)
                                     ;(* (- (nth 2 points) (nth 0 points)) xfact) (* (- (nth 3 points) (nth 1 points)) yfact)
                                     (max 1 (* (nth 2 points) xfact)) (max 1 (* (nth 3 points) yfact))
                                     )))
            ('polyg (if (cadddr params)
                        (om-fill-polygon (mat-trans (list (om+  (om* (car (mat-trans points)) xfact) x0) 
                                                          (om+ (om* (cadr (mat-trans points)) yfact) y0))))
                      (om-draw-polygon (mat-trans (list (om+  (om* (car (mat-trans points)) xfact) x0) 
                                                        (om+ (om* (cadr (mat-trans points)) yfact) y0))))
                      ))
            ('pen (loop for (a b) on points by #'cdr do 
                        (if (and a b) 
                            (om-draw-line (+ (* (car a) xfact) x0) (+ (* (cadr a) yfact) y0)
                                          (+ (* (car b) xfact) x0) (+ (* (cadr b) yfact) y0))
                          (if a (om-fill-rect (+ (* (car a) xfact) x0) (+ (* (cadr a) yfact) y0)
                                              (cadr params) (cadr params))))) )
            ('text (om-with-font (cadddr params) (om-draw-string (+ (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0) data)))
            (otherwise nil))
          )))
    ;)
  ))
     

;;;========================
;;; D&D : move selection

(defmethod om-drag-selection-p ((self pictpanel) position) 
  (and (selection (editor self))
       (equal (mode (editor self)) :normal)
       (let* ((obj (nth (selection (editor self)) (extraobjs (object (editor self)))))
              (r  (make-figure-region obj (w self) (h self))))
          (om-point-in-region-p r position))))

(defmethod om-drag-initpos ((self pictpanel)) *pict-drag-start*)

(defmethod om-draw-contents-for-drag ((self pictpanel))
  (when (selection (editor self))
    (draw-pict-extraobj self (nth (selection (editor self)) (extraobjs (object (editor self))))) 
    ))

(defun make-figure-region (fig w h)
  (let ((op (car fig))
        (points (cadr fig))
        (x0 0) (y0 0)
        (xfact w) (yfact h)
        (r (om-new-region)))
    (cond ((or (equal op 'line) (equal op 'arrow) (equal op 'rect))
           (om-region-add-rect r (+ (* (nth 0 points) xfact) x0) (+ (* (nth 1 points) yfact) y0)
                               (+ (* (nth 2 points) xfact) x0) (+ (* (nth 3 points) yfact) y0)))
      
      ((equal op 'cercle) 
       (om-region-add-rect r 
                           ;(+ (* (- (nth 0 points) (- (nth 2 points) (nth 0 points))) xfact) x0) 
                           (+ (* (- (nth 0 points) (nth 2 points)) xfact) x0) 
                           ;(+ (* (- (nth 1 points) (- (nth 3 points) (nth 1 points))) yfact) y0)
                           (+ (* (- (nth 1 points) (nth 3 points)) yfact) y0)
                           ;(* (nth 2 points) xfact) 
                           (+ (* (+ (nth 0 points) (nth 2 points)) xfact) x0)
                           ;(*  (nth 3 points) yfact)
                           (+ (* (+ (nth 1 points) (nth 3 points)) yfact) y0)
                           ))
      ((or (equal op 'polyg) (equal op 'pen)) 
       (let ((minx (car (car points))) 
             (maxx (car (car points)))
             (miny (cadr (car points))) 
             (maxy (cadr (car points))))
         (loop for p in (cdr points) do
               (if (< (car p) minx) (setf minx (car p))
                 (if (> (car p) maxx) (setf maxx (car p))))
               (if (< (cadr p) miny) (setf miny (cadr p))
                 (if (> (cadr p) maxy) (setf maxy (cadr p)))))
         (om-region-add-rect r (+ (* minx xfact) x0) (+ (* miny yfact) y0)
                             (+ (* maxx xfact) x0) (+ (* maxy yfact) y0))
         
         ))
      ((equal op 'text) 
       (om-region-add-rect r (+ (* (car points) xfact) x0) 
                           (- (* (cadr points) yfact) (om-string-h (cadddr (caddr fig))) y0) 
                           (+ (* (car points) xfact) (om-string-size (cadddr fig) (cadddr (caddr fig))) x0) 
                           (+ (* (cadr points) yfact) y0)))
      (otherwise nil))
    r))
    
(defmethod om-drag-container-view ((self pictpanel)) self)

(defmethod om-drag-start ((self pictpanel))
  (and (om-drag-selection-p self (om-mouse-position self))
       (setf *pict-drag-start* (om-mouse-position self))))

(defmethod om-drag-receive ((target pictpanel) (dragged-ref t) position &optional (effect nil))
  (let ((move (om-subtract-points (om-mouse-position target)
                                  (om-drag-initpos target)))
        (obj (nth (selection (editor target)) (extraobjs (object (editor target))))))
    (let ((dx (/ (om-point-h move) (w target)))
          (dy (/ (om-point-v move) (h target)))
          (op (car obj))
          (points (cadr obj)))
    (cond 
     ((or (equal op 'line) (equal op 'arrow) (equal op 'rect))
      (setf (cadr (nth (selection (editor target)) (extraobjs (object (editor target)))))
            (list (+ (nth 0 points) dx) (+ (nth 1 points) dy)
                  (+ (nth 2 points) dx) (+ (nth 3 points) dy))))
     
     ((equal op 'cercle) 
      (setf (cadr (nth (selection (editor target)) (extraobjs (object (editor target)))))
            (list (+ (nth 0 points) dx) (+ (nth 1 points) dy) 
                  (nth 2 points) (nth 3 points))))
    ((or (equal op 'polyg) (equal op 'pen))
     (setf (cadr (nth (selection (editor target)) (extraobjs (object (editor target)))))
           (loop for p in points collect (list (+ (car p) dx) (+ (cadr p) dy)))))
    ((equal op 'text) 
     (setf (cadr (nth (selection (editor target)) (extraobjs (object (editor target)))))
           (list (+ (nth 0 points) dx) (+ (nth 1 points) dy))))
    (otherwise nil))
    (setf *pict-drag-start* nil)
    (om-invalidate-view target)
    t)))

;=====================
(defclass pict-controls (3Dborder-view)  
  ((graphic-controls :initform nil :accessor graphic-controls)
   (edit-buttons :initform nil :accessor edit-buttons)
   
   (currentcolor :initform *om-black-color* :accessor currentcolor)            
   (currentsize :initform 1 :accessor currentsize)
   (currentline :initform 'line :accessor currentline)
   (currentfill :initform nil :accessor currentfill)
   (currentfont :initform *om-default-font1* :accessor currentfont)))

(defmethod initialize-instance :after ((self pict-controls) &rest args)
  (let ((graphics-begin 250))
    
    (setf (edit-buttons self)   
         (loop for mode in '(:normal :pen :line :arrow :rect :ellipse :polygon :text)
               for icon in '("mousecursor" "drawpen" "linebutton" "arrowbutton" "rectbutton" "ellipsebutton" "polygon" "text")
               for xx = 6 then (+ xx 26) 
               collect 
               (let ((m mode))
                 (om-make-view 'om-icon-button :position (om-make-point xx 6) :size (om-make-point 28 28)
                               :id mode
                               :icon1 icon :icon2 (string+ icon "-pushed")
                               :lock-push t
                               :selected-p (and (om-view-container self) (equal m (mode (om-view-container self))))
                               :action #'(lambda (item) 
                                           (setf (mode (om-view-container self)) m)
                                           (loop for button in (edit-buttons self) do
                                                 (setf (selected-p button) (equal m (id button))))
                                           (om-invalidate-view self))))
                 ))
    
    (setf (graphic-controls self)
          (list 
           (om-make-view 'om-pick-color-view
                         :position (om-make-point (+ graphics-begin 10) 10)
                         :size (om-make-point 40 20)
                         :color (currentcolor self)
                         :bg-color (currentcolor self)
                         :after-fun (lambda (item) (setf (currentcolor self) (om-get-bg-color item))))
           (om-make-dialog-item 'om-static-text (om-make-point (+ graphics-begin 70) 10)
                                (om-make-point 60 20) "Size" :font *om-default-font1*)
           (om-make-dialog-item 'om-pop-up-dialog-item
                                (om-make-point (+ graphics-begin 100) 8)
                                (om-make-point 50 20)
                                ""
                                :font *om-default-font1*
                                :range (mapcar 'integer-to-string (arithm-ser 1 10 1))
                                :di-action (om-dialog-item-act item (setf (currentsize self) 
                                                                          (read-from-string (om-get-selected-item item)))))
           (om-make-dialog-item 'om-static-text 
                                (om-make-point (+ graphics-begin 166) 10)
                                (om-make-point 60 20) "Line" :font *om-default-font1*)
           (om-make-dialog-item 'om-pop-up-dialog-item
                                (om-make-point (+ graphics-begin 200) 8)
                                (om-make-point 80 20)
                                "" 
                                :font *om-default-font1*
                                :range '("Normal" "Dashed")
                                :di-action (om-dialog-item-act item (setf (currentline self) 
                                                                          (if (= 0 (om-get-selected-item-index item)) 'line
                                                                            'dash))))
           (om-make-dialog-item 'om-check-box (om-make-point (+ graphics-begin 300) 8)
                                (om-make-point 60 20) "Fill"
                                :font *om-default-font1*
                                :di-action (om-dialog-item-act item (setf (currentfill self) (om-checked-p item))))
                   
           (om-make-dialog-item 'om-button (om-make-point (+ graphics-begin 350) 8)
                                (om-make-point 40 20) "A"
                                :font (currentfont self)
                                :di-action (om-dialog-item-act item 
                                             (setf (currentfont self) (om-choose-font-dialog :font (currentfont self)))
                                                     ;(om-set-font item (currentfont self))
                                                     ;(om-set-view-position item (om-make-point 380 8))
                                             ))
           ))
    
    
  
    (apply 'om-add-subviews self (append (graphic-controls self) (edit-buttons self)))
    ))


