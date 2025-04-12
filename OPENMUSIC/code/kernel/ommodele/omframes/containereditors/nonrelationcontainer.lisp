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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;DocFile
;Editor for metaobjects without connections i.e. folders generic functions, workspaces, etc.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;--------------------------
;EDITOR
;----------------------

(defclass nonrelationEditor (metaobj-Editor)  
   ((presentation :initform 0 :initarg :presentation :accessor presentation))
   (:documentation ""))

(defmethod get-editor-panel-class ((self nonrelationEditor))  'nonrelationPanel)


(defmethod big-icon-p ((self nonrelationEditor))
   (= (presentation self) 0))

(defmethod set-clipboard ((self nonrelationEditor) copies)
   (let* ((container (panel self)))
     (setf (scroll-scrap container) `(list ,@copies))))


(defmethod get-clipboard ((self nonrelationEditor))
   (let* ((container (panel self)))
     (scroll-scrap container)))

(defmethod editor-copy ((self nonrelationEditor))
   "Copy in the clipboard the selected icons in 'self'."
   (let* ((container (panel self))
          (subframes (get-actives container))
          (cannotcopy nil) copies)
     (if (text-view self)
       (om-copy-command (text-view self))
       (progn
         (loop for item in subframes
               while (not cannotcopy) do
               (setf cannotcopy (no-allow-copy-p (object item))))
         (if cannotcopy
           (not (dialog-message (string+ "Sorry I can't  copy " cannotcopy)))
           (om-with-cursor *om-wait-cursor* 
             (setf copies (mapcar #'(lambda (frame)
                                      (omNG-copy (object frame))) subframes))
             (set-clipboard self copies))))) t))

(defmethod om-paste ((self t) elem)
  (omNG-add-element self elem))

(defmethod editor-paste ((self nonrelationEditor))
   "Paste from the clipboard to 'self'."
   (let* ((container (panel self)))
     (if (text-view self)
       (om-paste-command (text-view self))
       (when (get-clipboard self)
         (let ((new-objs (eval (get-clipboard self)))
               (bx 10) (i -1))
           (loop for item in (get-actives container) do
                 (omG-unselect item))
           (mapcar #'(lambda (object)
                       (setf (name object) (mk-unique-name container (name object)))
                       (let* ((pos (or (find-a-position container) (om-make-point (+ 5 bx (* 50 (incf i))) 10)))
                              (new-frame (make-icon-from-object object (om-point-h pos) (om-point-v pos) 1 1)))
                         (unless (big-icon-p self)
                           (let* ((frames (get-subframes container))
                                  (newcol (findcol container frames new-frame)))
                             (setf (col new-frame) newcol)))
                         (add-icon-finder new-frame container)
                         (om-paste (object container) object)
                         (omG-select new-frame))) new-objs)
           (let ((copies (mapcar #'(lambda (object) (omNG-copy object)) new-objs)))
             (set-clipboard self copies))
           (set-field-size (panel self))))) t))


;save the size and the position of the editor.
(defmethod close-editor-before ((self nonrelationEditor))
  (call-next-method)
  (set-win-size (object (panel self)) (om-interior-size (window self)))
  (set-win-position (object (panel self)) (om-view-position (window self))))

(defmethod alias-editor ((self nonrelationEditor))
   "Make aliases of the selected icons."  nil)

(defmethod editor-list-presentation? ((self nonrelationEditor))
  (not (= (presentation self) 0)))

(defmethod editor-change-presentation ((self nonrelationEditor) presentation)
   "Show icons in 'self' by name by type or normal."
   (unless (= presentation (presentation self))
     (change-frame-presentation (panel self) presentation) t))

(defmethod editor-align-presentation ((self nonrelationEditor))
   "Show icons in 'self' by name by type or normal."
   (when (= 0 (presentation self))
     (align-frame-boxes (panel self)) 
     t))

;;; la position ou sera cree le nouvel objet
(defvar *new-obj-initial-pos* nil)

(defun space-available (panel x y needed-w needed-h)
  (let ((space-ok t))
    (loop for item in (get-subframes panel) while space-ok do
          (when (om-sect-rect (om-make-rect x y (+ x needed-w) (+ y needed-h))
                              (om-make-rect (bx item) (by item) (+ (bx item) (w item)) (+ (by item) (h item))))
            (setf space-ok nil)))
    space-ok))

(defun find-a-position (panel)
  (let ((needed-w 54) (needed-h 50)
        (pos nil))
    (loop for y = 10 then (+ y needed-h) while (and (not pos) (< (+ y needed-h) (h panel))) do
          (loop for x = 10 then (+ x needed-w) while (and (not pos) (< (+ x needed-w) (w panel))) do
                (when (space-available panel x y needed-w needed-h)
                  (setf pos (om-make-point x y)))))
    pos))

(defmethod editor-make-new-icon-window ((self nonrelationEditor) &optional type)
   "Called from New menu item type say if it create a patch a maquettte or a folder."
   (let* ((scrollframe (panel self))
         new-object new-frame 
         (pos (or (find-a-position scrollframe) *new-obj-initial-pos* (om-make-point 10 10))))
     (cond 
      ((equal type 'p) (setf new-object (omNG-make-new-patch (mk-unique-name scrollframe "Patch") pos)))
      ((equal type 'm) (setf new-object (omNG-make-new-maquette (mk-unique-name scrollframe "Maquette") pos)))
      ((equal type 'f) (setf new-object (omNG-make-new-folder (mk-unique-name scrollframe "NewFolder") pos)))
      ((equal type 'l) (setf new-object (omNG-make-new-lisp-patch (mk-unique-name scrollframe "LispFunction") pos)))
      (t (setf new-object (omNG-make-new-persistant type scrollframe pos))))
     (when new-object
       (setf (create-info new-object) (list (om-get-date) (om-get-date)))
       (setf new-frame (make-icon-from-object new-object (om-point-h pos) (om-point-v pos) 1 1))
       (omg-add-element scrollframe new-frame)
       ;;; new
       (set-field-size scrollframe)
       (mapcar 'omg-unselect (get-subframes scrollframe))   
       (omg-select new-frame) 
       (setf *new-obj-initial-pos* nil)
       )))

;;; objets persistants supplementaires...
(defmethod omNG-make-new-persistant ((type t) frame pos) nil)


(defmethod editor-cut ((self nonrelationEditor))
   "Cut the selected icons in 'self'."
   (let* ((container (panel self))
          (subframes (get-actives container)))
     (if (text-view self)
       (om-cut-command (text-view self))
       (when subframes
         (editor-copy self)
         (dolist (frame subframes)
           (omg-remove-element container frame))
         (setf *ready-question-p* nil)))))

;---------- Copy and paste

(defmethod editor-clear ((self nonrelationEditor))
   "Clear the selected icons in 'self'."
   (let* ((container (panel self))
          (subframes (get-actives container)))
     (when subframes
       (dolist (frame subframes)
         (omg-remove-element container frame))
       (setf *ready-question-p* nil))))


(defmethod editor-save ((self nonrelationEditor))
   "Save the object associated to the selected icons in 'self'."
   (let* ((container (panel self))
          (subframes (get-actives container)))
     (om-with-cursor *om-wait-cursor*
       (loop for item in subframes do
             (omNG-save (object item) nil)))))

(defmethod editor-select-all ((self nonrelationEditor))
   "Select all icons in 'self'."
   (let* ((subframes (get-subframes (panel self))))
     (mapc #'(lambda (icon)
               (omG-select icon)) subframes)))

;-------------------------------------
;PANEL
;-------------------------------------

(defclass nonrelationPanel (metaobj-Panel)
   ((scroll-scrap :initform nil :allocation :class :accessor scroll-scrap))
   (:documentation "Editorframes of meta objects where there are not connections beetwen their elements 
are instances of this class. (i.e. Folders, WorkSpaces, Classes, Generic functions, etc.) .#enddoc#
#seealso# (classpanel folderpannel instancepanel workspacepanel genericfunpanel) #seealso#
#scroll-scrap# This slot store, if exists, an expression used for copy and paste propose.#scroll-scrap#"))

;-------------------Protocole------------------------------
(defgeneric omg-remove-element (editor frame)  
   (:documentation "Remove graphically the subview 'frame' from the scroller 'editor' and call omng-remove-element
with the objects respectly associeted."))

(defgeneric omG-add-element (editor frame)  
   (:documentation "Add graphically the subview 'frame' to the scroller 'editor' and call omNG-add-element
with the objects respectly associeted."))

;-------------------------
(defmethod omg-remove-element ((self nonrelationPanel) frame)
   "Remove 'frame' from 'self', this function call the 'omng-remove-element' function."
   (let ((object-container (object self)))
     (if (protected-p (object frame))
       (dialog-message "This object is protected, you can not erase it")
       (progn
         (when (and (not (big-icon-p (editor self)))
                    (> (fil frame) 1))
           (if (get-parent frame self)
               (setf object-container (object (get-parent frame self)))
             (setf object-container nil)
             ))
         (when (EditorFrame (object frame))
           (om-close-window (window (EditorFrame (object frame)))))
         (when object-container
           (omng-remove-element object-container (object frame))
           (delete-icon-finder frame self)
           )))))
  
(defmethod omG-add-element ((self nonrelationPanel) (frame OMFrame))
   "Add 'frame' to 'self'."
   (unless (big-icon-p (editor self))
     (let* ((frames (get-subframes self))
            (newcol (findcol self frames frame)))
       (setf (col frame) newcol)))
   (omNG-add-element (object self) (object frame))
   (add-icon-finder frame self)
   (set-field-size self)
   )

(defmethod omG-add-element ((self nonrelationPanel) frame)
   "See generic function's documentation."
   (om-add-subviews self frame)
   ;;; new
   (om-invalidate-view self)
   (set-field-size self)
   ;;;
)

(defmethod handle-key-event ((self nonrelationPanel) char) 
  (when (om-command-key-p) 
    (scroll-pane self char))
  (case char
    (:om-key-return 
     (when (= 1 (length (get-actives self)))
       (om-view-doubleclick-handler (nameview (car (get-actives self))) (om-make-point 1 1))
       ))
    (#\h  (show-help-window (format nil "Commands for ~A Editor" 
                                      (string-upcase (class-name (class-of (object (editor self))))))
                            (get-help-list (editor self)) 410))
    (otherwise nil)))

 

;-----------------------------------
;Other methods
;-----------------------------------

(defmethod allow-move-element ((self nonrelationPanel))
   "You can move icons in 'self' if it is in normal icon mode."
   (big-icon-p (editor self)))

(defmethod get-subframes ((self nonrelationPanel))
   "Return a list with the icons (simpleframes) subviews of 'self'."
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (icon-finder-p icon)
                 (push icon rep))) (om-subviews self))
     (sort rep #'(lambda(x y) (or (< (col x) (col y))
                                  (and (= (col x) (col y)) (> (fil x) (fil y))))))))

(defmethod get-actives ((self t) &optional class) (declare (ignore class)) nil)
 
(defmethod get-actives ((self nonrelationPanel) &optional class)
   "Return a list with the selected icons (simpleframes) subviews of 'self'."
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (and (icon-finder-p icon)
                        (active-mode icon)) 
                 (push icon rep))) (om-subviews self))
     (reverse rep)))

;SORT by type or by name

(defmethod get-object-creation-date ((self ompersistantobject)) (car (create-info self)))
(defmethod get-object-modification-date ((self ompersistantobject)) (cadr (create-info self)))

(defmethod get-object-creation-date ((self OMframe)) (print self) (print (get-object-creation-date (object self))))
(defmethod get-object-modification-date ((self OMframe)) (get-object-modification-date (object self)))


(defmethod sort-subframes ((self nonrelationPanel) elements)
 (case (presentation (om-view-container self))
       (0 elements)
       (1  (sort elements #'string-lessp :key #'name))
       (2  (sort elements #'string-lessp :key #'get-object-insp-name))
       (3  (sort elements #'(lambda (a b) (if (null a) nil (if (null b) t (string-not-lessp a b)))) :key #'get-object-creation-date))
       (4  (sort elements #'(lambda (a b) (if (null a) nil (if (null b) t (string-not-lessp a b)))) :key #'get-object-modification-date))
     ))

;------------------------------------------------------------------
;The next methods are used to show icons by name or by type

;Called when you change the presentation (i.e. by name, by icon, by type).
(defmethod change-frame-presentation ((self nonrelationPanel) presentation)
  (let* ((icons (get-subframes self)))
    (om-with-delayed-update self 
      (unless (= (presentation (om-view-container self)) 0)
        (mapc #'(lambda (icon)
                  (if (> (fil icon) 1)
                      (delete-icon-finder icon self nil)
                    (remove-triangle icon self)))
              icons))
      
      (setf (presentation (om-view-container self)) presentation)
      (case presentation
        (0 (mapc #'(lambda (icon)
                     (set-size icon self)) (get-subframes self)))
        (otherwise (let ((i 1))
                     (mapc #'(lambda (icon)
                               (setf (col icon) i)
                               (setf (fil icon) 1)
                               (set-size icon self)
                               (incf i)) 
                           (sort-subframes self (get-subframes self))))
                   (set-triangles self))))))


;add triangles views to open or close folders.
(defmethod set-triangles ((self nonrelationPanel))
   (let* ((icons (get-subframes self)))
     (mapc #'(lambda (icon)
               (when (container-p icon)
                 (let ((triangle (om-make-view 'triangle-icon
                                   :position (om-make-point (- (x icon) 20) (+ (y icon) 5))
                                   :size (om-make-point 11 11)
                                   :icon-finder icon
                                   :iconID 164)))
                   (setf (open? triangle) nil)
                   (om-add-subviews self triangle)
                   (setf (triangle icon) triangle)))
               (add-extras-infos icon)) icons)))

;if the view is in list mode this method find the  horizontal position 
;of the new element in the view
(defmethod findcol ((self nonRelationPanel) list new)
   (if list
     (let ((i (col (car list))))
       (loop while list do
             (let ((frame (pop list)))
               (when (= (fil frame) (fil new))
                 (case (presentation (om-view-container self))
                   (1 (if (string-lessp  (string (name new)) (string (name frame)))
                        (progn
                          (setf list nil)
                          (setf i (- (col frame) 1)))))
                   (2 (if (string-not-greaterp (get-object-insp-name new) (get-object-insp-name frame))
                        (progn
                          (setf list nil)
                          (setf i (- (col frame) 1)))))))
               (incf i)))
       i) 1))

;update the position of the icons in self
(defmethod up-from ((self nonRelationPanel) pos)
   (let* ((controls (get-subframes self))
          (final-list (subseq controls (min pos (length controls)))))
     (mapc #'(lambda (icon)
               (change-icon-position icon (fil icon) (- (col icon) 1))
               )
           final-list)))


(defun find-best-pos (array x y)
  (let ((dist-list (loop for elt in array collect (if (listp elt) (sqrt (+ (expt (- x (car elt)) 2) (expt (- y (cadr elt)) 2))) nil))))
    (position (list-min (remove nil dist-list)) dist-list :test 'equal)))



(defmethod align-frame-boxes ((self nonrelationPanel))
  (when (get-subframes self)
   (let* ((icons (get-subframes self))
          (n (length icons))
          (x 10)
          (dx (loop for ic in icons maximize (w ic)))
          (maxx (- (w self) x))
          (nx (floor maxx dx))
          (ny (ceiling n nx))
          (array (make-list (* nx ny) :initial-element nil))
          (dy 60)
          (y 10))
     (loop for j from 0 to (- ny 1) do
           (loop for i from 0 to (- nx 1) do
                 (setf (nth (+ i (* nx j)) array) (list (+ x (* i dx)) (+ y (* j dy))))))
     
     (loop for icon in icons do
           (let* ((pos (find-best-pos array (bx icon) (by icon)))
                  (icnx (- (round dx 2) (round (w icon) 2))))
             
             (change-icon-position icon (+ (car (nth pos array)) icnx) (cadr (nth pos array)))
             (setf (nth pos array) t)
             ))
     )))



    
;------------------------------------------
;creating Windows and scrollers 
;------------------------------------------

(defmethod set-editor-presentation ((self editorview)) nil)

(defmethod wintype-from-obj ((self t)) nil)


(defun open-new-nonrelationFrame (object name elements)
     (let* ((i 0) newwindow)
       (setf newwindow (make-editor-window (get-editor-class object)
                                           object name nil 
                                           :winsize (get-win-size object)
                                           :winpos (get-win-position object)
                                           :winshow nil
                                           :wintype (wintype-from-obj object)))
       (set-editor-presentation (editor newwindow))
       (om-with-delayed-redraw (panel newwindow)
         (mapc #'(lambda (elem)
                  (add-icon-finder (make-icon-from-object elem  
                                                           (om-point-h (get-icon-pos elem)) (om-point-v (get-icon-pos elem)) 
                                                           1 (+ i 1))
                                    (panel newwindow))
                   (incf i)) (sort-subframes (panel newwindow) elements))
         (set-field-size (panel newwindow))
         )
       newwindow
       ))


(defmethod add-more-information ((self nonRelationPanel)) nil)


;;;;-----------------------------------
;;;; events
;;;;-----------------------------------

(defmethod om-view-click-handler ((self nonrelationPanel) where)
  (do-click-event-handler self where))

(defmethod do-click-event-handler ((self nonrelationPanel) where)
   (unless (om-shift-key-p) 
     (mapc #'(lambda (control) 
               (omG-unselect control)) (get-actives self)))
   (control-actives self where)
   self)
   

(defmethod control-actives ((view nonrelationPanel) where)
  (close-enter-dialog (editor view))
  (om-init-motion-click view where 
                       :motion-draw 'draw-selection-rectangle 
                       :release-action 'release-selection
                       :display-mode 2))

(defmethod release-selection ((self om-view) initpos pos)
  (let ((x1 (min (om-point-x pos) (om-point-x initpos)))
        (y1 (min (om-point-y pos) (om-point-y initpos)))
        (x2 (max (om-point-x pos) (om-point-x initpos)))
        (y2 (max (om-point-y pos) (om-point-y initpos))))
    (let ((rect (list x1 y1 (- x2 x1) (- y2 y1))))
      (when (not (= 0 (caddr rect) (cadddr rect)))
        (do-select-items-in-rect self rect))
      (om-invalidate-view self t))
    ;;lw81 scroll selection problem
    (let* ((pos (om-scroll-position self))
           (vpos (om-point-v pos))
           (hpos (om-point-h pos))
           (inc (if (om-shift-key-p) 500 50)))

      (om-set-scroll-position self (om-make-point (- hpos 1) vpos))
      (om-set-h-scroll-position self  (om-point-h (om-scroll-position self)))
      
      (om-set-scroll-position self (om-make-point (+ hpos 1) vpos))
      (om-set-h-scroll-position self  (om-point-h (om-scroll-position self))))
      )
    ))

(defmethod do-select-items-in-rect ((self nonrelationPanel) rect) 
   (let (user-rect scratch-rect-i scratch-rect-n i-rect n-rect)
     (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
      (dolist (item (get-subframes self))
        (let ((posIconx (om-add-points (om-view-position item) (om-view-position (iconView item))))
              (posNamex (om-add-points (om-view-position item) (om-view-position (nameView item))))) 
          (setf i-rect (om-pts-to-rect posIconx (om-add-points posIconx (om-view-size (iconView item)))))
          (setf scratch-rect-i (om-sect-rect user-rect i-rect))
          (setf n-rect (om-pts-to-rect posNamex (om-add-points posNamex (om-view-size (nameView item)))))
          (setf scratch-rect-n (om-sect-rect user-rect n-rect))
          (unless (and (om-rect-empty scratch-rect-i) (om-rect-empty scratch-rect-n))
            (omG-select item))))))


(defmethod om-view-cursor ((self nonrelationPanel)) *om-arrow-cursor*)


(defmethod scroll-pane ((self nonRelationPanel) char)
  (let* ((pos (om-scroll-position self))
         (vpos (om-point-v pos))
         (hpos (om-point-h pos))
         (inc (if (om-shift-key-p) 500 50)))
    (case char 
      (:om-key-right 
       (om-set-scroll-position self (om-make-point (+ hpos inc) vpos))
       (om-set-h-scroll-position self  (om-point-h (om-scroll-position self))))
      (:om-key-left
       (om-set-scroll-position self (om-make-point (- hpos inc) vpos))
       (om-set-h-scroll-position self  (om-point-h (om-scroll-position self))))
      (:om-key-up
       (om-set-scroll-position self (om-make-point hpos (- vpos inc)))
       (oa::om-set-v-scroll-position self  (om-point-v (om-scroll-position self))))
      (:om-key-down
       (om-set-scroll-position self (om-make-point hpos (+ vpos inc)))
       (oa::om-set-v-scroll-position self  (om-point-v (om-scroll-position self))))
      (:om-key-esc 
       (om-set-scroll-position self (om-make-point 0 0))
       (oa::om-set-h-scroll-position self 0)
       (oa::om-set-v-scroll-position self 0))
      )))




;************************************************************************
;Specific window and scrollers
;************************************************************************


;-----------------
;WorkSpace's Editor
;-----------------

;---------------------------------------------

(defclass workSpaceEditor (nonRelationEditor)  ()
   (:documentation "This is the class of the workspace's window.#enddoc#
#seealso# (OmWorkspace WorkSpaceScroller) #seealso#"))

(defmethod get-editor-panel-class ((self workSpaceEditor))  'workSpacePanel)

; (omNG-save-ws *current-workspace*)

(defvar *ws-saved* nil)

(defmethod editor-save ((self workSpaceEditor)) 
  (setf *ws-saved* nil)
  (om-lisp::om-eval-on-process #'(lambda () 
                                   (save-ws-contents)
                                   (setf *ws-saved* t)
                                   )))
                                   
(defmethod presentation ((self workSpaceEditor))
  (presentation (object self)))

(defmethod (setf presentation) ((val integer) (self workSpaceEditor))
  (set-presentation (object self) val))


(defmethod editor-close? ((self workSpaceEditor))
  #+win32 (om-confirmed-quit)
  #-win32 t
  )

;--------------------------
;PANEL
;--------------------------

(defclass workSpacePanel (nonRelationPanel) () 

              #+(and win32 (not ml-maquette)) 
              (:default-initargs :draw-pinboard-objects :local-buffer)
                         
	      (:documentation "The editorframe of the WorkSpace is an instance of this class.
Workspace Panels contain icons of patches, maquettes and folders
(patch-icon-frame, maquette-icon-frame and folder-icon-frame instances).#enddoc#
#seealso# (OmWorkspace patch-icon-frame maquette-icon-frame folder-icon-frame) #seealso#"))
 
(defmethod set-panel-color ((self workSpacePanel))
  (om-set-bg-color self *ws-color*))

(defmethod sort-subframes ((self nonrelationPanel) elements)
 (case (presentation (om-view-container self))
       (0 elements)
       (1  (sort elements #'string-lessp :key #'name))
       (2  (sort elements #'string-lessp :key #'get-object-insp-name))
       (3  (sort elements #'(lambda (a b) (if (null a) nil (if (null b) t (string-not-lessp a b)))) :key #'get-object-creation-date))
       (4  (sort elements #'(lambda (a b) (if (null a) nil (if (null b) t (string-not-lessp a b)))) :key #'get-object-modification-date))
     ))



 
(defun import-dragged-file (pane filename pos)
  (let ((dirname (make-pathname :directory (append (pathname-directory filename) (list (pathname-name filename))))))
    (cond ((or (string-equal "omp" (pathname-type filename)) 
               (string-equal "omm" (pathname-type filename))
               (string-equal "she" (pathname-type filename)))
           (import-file-to-ws pane filename pos))
          ((or 
            (directoryp filename)
            (and (directoryp dirname) (probe-file dirname)))
          (make-new-folder pane dirname pos))
        (t nil))
    ))

(defmethod om-import-files-in-app ((self workspacepanel) files)
  (when (= 1 (length files))
    (or (import-dragged-file self (pathname (car files)) (om-mouse-position self))
        (om-beep-msg (string+ "File: " (namestring (pathname (car files))) " can not be imported in the workspace.")))))


(defmethod help-items ((self workspaceeditor)) 
  (append
   (import-tutorial-menu self)
   (call-next-method)))

(defvar *wrkspchelp* '(
                       (#+(or linux win32)("Ctrl + 1") #+macosx("Cmd + 1") "New Patch")
                       (#+(or linux win32)("Ctrl + 2") #+macosx("Cmd + 2") "New Maquette")
                       (#+(or linux win32)("Ctrl + 3") #+macosx("Cmd + l") "New Lisp Function")
                       (#+(or linux win32)("Ctrl + N") #+macosx("Cmd + N")"New Folder")
                       ("m" "Check/Uncheck selected items")
                       ("c" "Open icon's dialog")
                       ("i" "re-Init icons of selected items")
                       ))

(defmethod get-help-list ((self workspacepanel)) 
  (list *wrkspchelp* ))


(defmethod om-check-items ((self ombasicobject))
  "Marks/unmakrs self's icon" 
  (cond 
   ((lisp-exp-p self) (print (list self "lispexp")))
   ((maquette-p self) (omg-change-icon self (if (= (icon self) 1820) 182 1820)))
   ((patch-p self)  (omg-change-icon self (if (= (icon self) 1830) 183 1830)))
   ((folder-p self) (omg-change-icon self (if (= (icon self) 1860) 186 1860)))
   (t nil)))

(defmethod om-init-item-icon ((self ombasicobject))
  "Sets self's icon to its original state"
  (cond 
   ((lisp-exp-p self) (omg-change-icon self 124))
   ((maquette-p self) (omg-change-icon self 182))
   ((patch-p self) (omg-change-icon self 183))
   ((folder-p self) (omg-change-icon self 186))
   (t nil)))

(defmethod om-choose-icon-dialog ((objs list))
  "Opens icon-dialog to set choosen icon"
  (let* ((di (choose-resource-dialog :icon :kernel t :user t))
         (icon (string-to-number (car di)))
         (loc (last-elem di))
         (path (if (equal loc 'user)
                   (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                          (list "resources" "icon")))
                 (make-pathname :directory (append (pathname-directory *om-resources-folder*) 
                                          (list "icon"))))))
    ;(print (list "infos" icon loc path objs))
    (loop for i in objs
          do (progn
               (omg-change-icon i icon)))))

(defmethod handle-key-event ((self workspacePanel) char)
  (let ((sel (remove nil 
                     (loop for i in (om-subviews self)
                           collect (if  (active-mode i) (object i))))))  
    (case char
      (#\m 
       (loop for i in sel
             collect (om-check-items i)))
      (#\c
       (om-choose-icon-dialog sel))
      (#\i
       (loop for i in sel
             do (om-init-item-icon i)))
      (otherwise (call-next-method)))))

;-----------------
;Folder's Editor
;-----------------


(defclass folderEditor (nonRelationEditor)  ()
   (:documentation "This is the class of the folder's window.#enddoc#
#seealso# (Omfolder FolderPanel) #seealso#"))

(defmethod get-editor-panel-class ((self folderEditor))  'folderPanel)



(defmethod editor-save ((self folderEditor)) 
  (show-message-win "Saving folder...")
  (setf *save-apply-all* nil)
  (omNG-save-ws (object self))
  (setf *save-apply-all* nil)
  (hide-message-win))

(defmethod presentation ((self folderEditor))
  (presentation (object self)))

(defmethod (setf presentation) ((val integer) (self folderEditor))
  (setf (presentation (object self)) val)
  (setf (changed-wsparams? (object self)) t))

(defmethod set-editor-presentation ((self foldereditor))
  (setf (presentation self) (presentation (object self)))
  (setf (changed-wsparams? (object self)) t))

(defmethod help-items ((self foldereditor)) 
  (append
   (import-tutorial-menu self)
   (call-next-method)))

;--------------------------
;PANEL
;--------------------------

(defclass folderPanel (nonRelationPanel) ()

              #+(and win32 (not ml-maquette)) 
              (:default-initargs :draw-pinboard-objects :local-buffer)

	      (:documentation "This is the class for folder'editors.
Elements in these editors are patch-icon-frame maquette-icon-frame or folder-icon-frame instances.#enddoc#
#seealso# (OMFolder patch-icon-frame maquette-icon-frame folder-icon-frame) #seealso#"))


(defmethod set-panel-color ((self folderPanel))
  (om-set-bg-color self (om-make-color 0.9 0.9 0.9)))

(defmethod om-import-files-in-app ((self folderpanel) files)
  (when (= 1 (length files))
    (or (import-dragged-file self (pathname (car files)) (om-mouse-position self))
        (om-beep-msg (string+ "File: " (namestring (pathname (car files))) " can not be imported in the workspace.")))))


;;same help as workspace
(defmethod help-items ((self foldereditor)) 
  (append
   (import-tutorial-menu self)
   (call-next-method)))

(defmethod get-help-list ((self folderpanel)) 
  (list *wrkspchelp* ))

(defmethod handle-key-event ((self folderPanel) char)
  (let ((sel (remove nil 
                     (loop for i in (om-subviews self)
                           collect (if  (active-mode i) (object i))))))  
    (case char
      (#\m 
       (loop for i in sel
             collect (om-check-items i)))
      (#\c
       (om-choose-icon-dialog sel))
      (#\i
       (loop for i in sel
             do (om-init-item-icon i)))
      (otherwise (call-next-method)))))

;;;=====================================
;;; PACKAGE BORWSER FRAMES
;;;=====================================

(defmethod wintype-from-obj ((self ompackage)) nil) ;'(:toolbox)

(defun draw-browser-panel-title (panel label name)
  (let* ((x (- (w panel) (max (om-string-size label *om-default-font2b*)
                             (om-string-size name *om-default-font2b*))
               30)))
    (when (and label name)
      (om-with-focused-view panel
        (om-with-font *om-default-font1b*
                      (om-with-fg-color panel *om-black-color*
                        (om-draw-string x 14 label) (om-draw-string x 28 name)
                        ))))))


;-----------------
;Global Folder
;-----------------

(defclass GlobalsfolderEditor (nonRelationEditor)  ()
   (:documentation "This is the class of the Globalsfolder's window.#enddoc#
#seealso# (OmGlobalsfolder GlobalsFolderPanel) #seealso#"))

(defmethod get-editor-panel-class ((self  GlobalsfolderEditor))  'GlobalsfolderPanel)

(defmethod set-clipboard ((self  GlobalsfolderEditor) copies)
   (let* ((container (panel self)))
     (setf (scroll-scrap-glo container) `(list ,@copies))))

(defmethod get-clipboard ((self  GlobalsfolderEditor))
   (let* ((container (panel self)))
     (scroll-scrap-glo container)))

(defmethod get-win-size ((self OMGlobalsFolder))
   (om-make-point 300 200))

(defmethod editor-save ((self GlobalsfolderEditor))
  (ws-save-globals))

(defclass GlobalsfolderPanel (nonRelationPanel)
   ((scroll-scrap-glo :initform nil :allocation :class :accessor scroll-scrap-glo))
   (:documentation "This is the class for OMGlobalsFolder'editors. 
Elements in these editors are instance-icon-frame instances.#enddoc#
#seealso# (OMglobalsFolder instance-icon-frame) #seealso#
#scroll-scrap-glo# Used to distinguish for the OMfolder scrap. #scroll-scrap-glo#"))

(defmethod omG-new-var ((self GlobalsfolderPanel))
   "Make a new global instance of the class store."
   (when (find-class 'store nil)
     (let* ((name (mk-unique-name self "variable"))  
            (obj (omNG-make-new-instance (make-instance 'store) name (om-make-point 22 22)))
            new-frame)
       (setf new-frame (make-icon-from-object obj 22 22 1 1))
       (omg-add-element self new-frame)
       (omng-save obj) t)))

(defmethod set-panel-color ((self GlobalsfolderPanel))
  (om-set-bg-color self (om-make-color 0.7 0.75 0.7)))

(defmethod om-draw-contents ((self GlobalsfolderPanel))
  (let ((str "Global Variables"))
    (when (and str (= (presentation (editor self)) 1))
      (om-with-focused-view self
        (om-with-font *om-default-font1b*
                      (om-with-fg-color self *om-black-color*
                        (om-draw-string (- (w self) (om-string-size str *om-default-font2b*) 30) 14 str)
                        ))))))

;-----------------
;Class
;-----------------

(defclass classEditor (nonRelationEditor)  
   ((initform-editor :initform nil :accessor initform-editor))
   (:documentation "")
   (:default-initargs  :presentation 1))

(defmethod get-editor-panel-class ((self classEditor))  'classPanel)

(defmethod get-clipboard ((self classEditor)) nil)

(defmethod get-win-size ((self OMClass))
   (om-make-point 500 200))

(defmethod editor-make-new-icon-window ((self classEditor) &optional type)
   "Add a slot of type 'type' to the class associeted to the self's scroller and redefine it."
   (declare (ignore type))
   (if (protected-p (object (panel self)))
     (dialog-message "This object is protected, you can not add slots")
     (let* ((scrollframe (panel self))
            new-object new-frame)
       (setf new-object (omNG-make-new-slot  (class-name (object scrollframe))   
                                             (mk-unique-name  scrollframe "slot") 't ':instance t t ))
       (setf new-frame (make-icon-from-object new-object 10 10 1 1))
       (unless (big-icon-p self)
         (let* ((frames (get-subframes scrollframe))
                (newcol (+ 1 (length frames))))
           (setf (col new-frame) newcol)))
       (add-icon-finder new-frame scrollframe)
       (omNG-add-element (object scrollframe) new-object))))


(defmethod close-editor-before ((self classEditor))  
   (call-next-method)
   (if (initform-editor self)
     (om-close-window (initform-editor self))))

(defclass classPanel (nonRelationPanel) ()
   (:documentation "This is the editor class for a OMClass. Elements of these editors are slots simple frames.#enddoc#
#seealso# (OMClass slot-icon-frame) #seealso# "))

(defmethod set-panel-color ((self classPanel))
  ;(om-set-bg-color self *package-color-3*)
  (om-set-bg-color self *om-light-gray-color*)
  )

(defmethod omg-remove-element ((self classPanel) frame)
   "When you remove one slot from the editor it is necessary to redefine the class."
   (let ((object-container (object self)))
     (if (protected-p (object frame))
       (dialog-message "This object is protected, I can not erase it")
       (progn
         (when (and (not (big-icon-p (editor self)))
                    (> (fil frame) 1))
           (setf object-container (object (get-parent frame self))))
         (when (EditorFrame (object frame))
           (om-close-window (om-view-container (EditorFrame (object frame)))))
         (omng-remove-element object-container (object frame))
         (delete-icon-finder frame self)))))


;Classes are showed allways by list.

(defmethod sort-subframes ((self classPanel) elements) elements)
(defmethod change-frame-presentation ((self classPanel) presentation)
   (declare (ignore presentation)) nil)

(defmethod re-sort-slots ((self classPanel))
   (let ((i 0)) 
     (loop for item in (om-subviews self) do
           (om-remove-subviews self item))
     (mapc #'(lambda (elem)
               (add-icon-finder (make-icon-from-object elem  (om-point-h (get-icon-pos elem)) (om-point-v (get-icon-pos elem)) 1 (+ i 1))
                                self)
               (incf i)) (sort-subframes self (get-elements (object self))))
     (add-titles self)
     ))

(defmethod add-titles ((self classPanel))
  (om-add-subviews self 
                   (om-make-dialog-item 'om-static-text (om-make-point 30 3) (om-make-point 50 20) "Slots"  :font *om-default-font3b* :fg-color *om-dark-gray-color*)                   
                   (om-make-dialog-item 'om-static-text (om-make-point 142 5) (om-make-point 50 20) "Show"  :font *om-default-font2b* :fg-color *om-dark-gray-color*)
                   (om-make-dialog-item 'om-static-text (om-make-point 195 5) (om-make-point 70 20) "Allocation"  :font *om-default-font2b* :fg-color *om-dark-gray-color*)
                   (om-make-dialog-item 'om-static-text (om-make-point 300 5) (om-make-point 100 20) "Default value"  :font *om-default-font2b* :fg-color *om-dark-gray-color*)))




(defmethod om-view-cursor ((self classPanel))
   *om-arrow-cursor*)


;-----------------
;Generic function's Editor
;-----------------

(defclass GenericFunEditor (nonRelationEditor)  ()
   (:documentation "This is the class of the GenericFun's window.#enddoc#
#seealso# (OmGenericFun GenericFunPanel) #seealso#"))

(defmethod get-editor-panel-class ((self GenericFunEditor))  'GenericFunPanel)

(defmethod editor-make-new-icon-window ((self GenericFunEditor) &optional type)
   "Open an editor to const a new method for the genric function 'self'."
   (declare (ignore type))
   (let ((scrollframe (panel self)))
     (make-new-method (object scrollframe))))

(defmethod get-clipboard ((self GenericFunEditor)) nil)

(defmethod editor-change-presentation ((self GenericFunEditor) presentation)
   (declare (ignore presentation)) nil)

(defclass GenericFunPanel (nonRelationPanel) ()

              #+(and win32 (not ml-maquette)) 
              (:default-initargs :draw-pinboard-objects :local-buffer)

	      (:documentation "This is the class for editor of Generic Functions meta objects.
 Elements of these editors are icon-method instances.#enddoc#
#seealso# (OMgenericFunction icon-method) #seealso#"))


(defmethod set-panel-color ((self GenericFunPanel))
  (om-set-bg-color self *package-color-3*))

(defmethod allow-move-element ((self GenericFunPanel))
   "You can not move the method'icons in a generic function editor." nil)

(defmethod get-subframes ((self GenericFunPanel))
   "Get a list of method's icons (instances of the icon-method class)." 
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (icon-method-p icon)
                 (push icon rep))) (om-subviews self))
     rep))

(defmethod get-actives ((self GenericFunPanel)  &optional class)
   "Get a list of selected method's icons (instances of the icon-method class)."
   (declare (ignore scroll))
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (and (icon-method-p icon)
                        (active-mode icon)) 
                 (push icon rep))) (om-subviews self))
     (reverse rep)))

(defmethod om-draw-contents ((self GenericFunPanel))
  (draw-browser-panel-title self
                            (if (omclass-p (object self)) "Internal methods of class" "Methods of function")
                            (name (object self))))

(defmethod om-view-cursor ((self GenericFunPanel))
   *om-arrow-cursor*)


;----------------
;Init methods and read/write slots methods for an OMCLASS
;----------------


(defclass InternalMethEditor (GenericFunEditor)  ()
   (:documentation "This is the class of the InternalMeth's window.#enddoc#
#seealso# (OmInternalMeth InternalMethScroller) #seealso#"))

(defmethod get-editor-panel-class ((self InternalMethEditor))  'InternalMethPanel)


(defclass InternalMethPanel (GenericFunPanel) ()
   (:documentation "This is the class for the editor of init methods and read/write slots methods associated to an OMClass.#enddoc#
#seealso# (OMClass) #seealso#"))


