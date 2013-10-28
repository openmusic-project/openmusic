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
;Patch editor definition.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;---------------------------------------------------------------------
;EDITOR
;---------------------------------------------------------------------

(defclass patchEditor (relationEditor)  ()
   (:documentation "This is the class for windows containing a patchPanel, so windows for Patch's editors.#enddoc#
#seealso# (patchPanel OMPatch) #seealso#"))


(defmethod get-editor-panel-class ((self patchEditor))  'patchPanel)

(defmethod key-event-around ((self patchEditor) char) 
   (let ((container (panel self)))
     (if (text-view self)
       t
       (case char
         (#\y (select-a-picture container) nil)
         (otherwise t)))))


(defmethod editor-make-new-icon-window ((self patchEditor) &optional type) nil)

;---------------------------------------------------------------------
;PANEL
;---------------------------------------------------------------------

(defclass patchPanel (relationPanel) ()

  #+(and win32 (not ml-maquette)) 
  (:default-initargs :draw-pinboard-objects :local-buffer)

   (:documentation "Patch editors are instance of this class.
Elements of patchPanels are instace of the boxframe class.#enddoc#
#seealso# (OMPatch boxframe) #seealso#"))

(defmethod patchpanel-p ((self t)) nil)
(defmethod patchpanel-p ((self patchpanel)) t)

(defmethod find-the-view-of-patch ((self patchPanel))
   (let (rep)
     (loop for box in (get-subframes self)
           while (not rep) do
           (when (and (has-mini-pict? (object box))
                      (view-of-patch (object box)))
             (setf rep box)))
     rep))

(defmethod find-the-box-view-of-patch ((self OMPatch))
   (let (rep)
     (loop for box in (boxes self)
           while (not rep) do
           (when (and (has-mini-pict? box)
                      (view-of-patch box))
             (setf rep box)))
     rep))


(defmethod om-view-click-handler :before ((self patchPanel) where)
   (declare (ignore where))
   (modify-patch self))

(defmethod get-patchpanel ((self t)) (panel self))

(defmethod om-view-click-handler ((view patchPanel) where)
  ;(om-inspect view) 
  (if *adding-a-box*
      ;;; a box is being added ...
    (progn
      (when (equal (window view) (second *adding-a-box*))
        (let* ((window (second *adding-a-box*))
               (item (first *adding-a-box*))
               (thepatch (get-patchpanel (editor window)))
               (newbox (omNG-make-new-boxcall item (om-make-point 22 22) (mk-unique-name thepatch (name item))))
               pospanel pos)
          (if *new-obj-initial-pos*
              ;;; CASE 1 : a box is added from context-menu 
              (progn
                (setf (frame-position newbox) *new-obj-initial-pos*)
                (omG-add-element thepatch (make-frame-from-callobj newbox))
                )
            ;;; CASE 2 : a box is added from window menu 
            (progn (setf pospanel (om-mouse-position thepatch)
                         pos (om-mouse-position window))
              (when (om-point-in-rect-p pos (om-pts-to-rect (om-make-point 0 0) (om-view-size window)))
                (setf (frame-position newbox) (borne-position pospanel))
                (omG-add-element thepatch (make-frame-from-callobj newbox)))
              ))))
      (setf *adding-a-box* nil)
      (setf *new-obj-initial-pos* nil)
      )
    (call-next-method)))

(defmethod om-view-doubleclick-handler ((Self patchPanel) Where)
  (when (equal self (call-next-method))
    (make-undefined-box self where)
    ))



(setf *patchhelp1* '((lrud "Move")
                     (shift+lrud "Move faster")
                     (del "Delete")
                     (("v") "eVal")
                     
                     (("b") "lock or change eval mode Button")
                     ((">" "<") "add/remove Optional inputs")
                     (("k" "K") "add/remove Keyword inputs")

                     (("d") "show Documentation")
                     (("e") "Edit lisp code")
                     (("t") "show Tutorial patch")
                     ))

(setf *patchhelp2* '((("c") "Change Connection Color")
                     (("A") "Align")
                     ;(("D") "Redraw All")
                     (("i") "reInitialize size")
                     (("I") "reInitialize value")
                     (("m") "show/hide Miniview")
                     (("M") "show/hide all Miniviews")
                     (("n") "show/hide Name")
                     (("a") "internalize patch Abstraction")
                     (("y") "activate/switch bg picture")
                     
                     (space "Play / Stop")
                     ;; (h "Show this Help window")
                     ))

(defmethod get-help-list ((self t)) nil)

(defmethod get-help-list ((self editorview)) 
  (get-help-list (panel self)))


(defmethod get-help-list ((self patchpanel)) 
  (list *patchhelp1* *patchhelp2*))

(defmethod editor-show-documentation ((self patcheditor))
  (mapc 'show-big-doc (get-actives (panel self))))

(defmethod editor-show-infowindow ((self patcheditor))
  (mapc 'show-info-window (get-actives (panel self))))

(defmethod editor-show-tutorial ((self patcheditor))
  (mapc 'show-online-tutorial (get-actives (panel self))))

(defmethod help-items ((self patcheditor)) 
  (append
   (list (unless (maquette-p (object self))
           (list 
            (om-new-leafmenu "Documentation" #'(lambda() (editor-show-documentation self)) "D")
            (om-new-leafmenu "Tutorial Patch" #'(lambda() (editor-show-tutorial self)) "T")
            ))
         (list 
          (om-new-leafmenu "Abort Evaluation" #'(lambda() 
                                                  (om-abort-eval-process)
                                                  (om-listener-echo "Aborted")
                                                  (clear-ev-once (panel self))) 
                           "A")
          ))
   (call-next-method)))

(defvar *cur-eval-panel* nil)

(defmethod handle-key-event ((self patchPanel) char) 
  (modify-patch self)
  (let* ((actives (get-actives self))
         (activeboxes (mapcar 'object actives)))
    (case char
      (:om-key-delete (delete-general self))
      ;;;(#\f (make-undefined-box self (om-mouse-position self)))
      (#\D  (om-invalidate-view self t))
      (#\d  (mapc 'show-big-doc actives))
      (#\c  (patch-color self))
      (#\e (mapc 'show-fun-code actives))
      (#\v  (om-eval-enqueue 
             `(progn
                (setf *cur-eval-panel* ,self)
                (mapc 'eval-box ',actives)
                (clear-ev-once ,self))
             ))
      (#\h  (show-help-window (format nil "Commands for ~A Editor" 
                                      (string-upcase (class-name (class-of (object (editor self))))))
                              (get-help-list (editor self)))) 
      ;;; in the menu
      (#\k (mapc 'add-keywords actives))
      (#\K (mapc 'erase-keywords actives))
      (#\i (mapc 'reinit-size actives) 
           (reinit-connections self)
           (reinit-bg-picts self))
      (#\I (mapc 'reinit-contents actives))
      (#\r (mapc 'update-doc actives))
      (#\b (mapc 'add-rem-lock-button actives))
      
      (#\l (mapc 'add-rem-lambda-button actives))
      (#\1 (mapc 'add-rem-evonce-button actives))

      (#\a (mapc 'internalize-patch actives))
      (#\A (mapc 'align-one-boxframe actives)
           (make-move-after self actives))

      (#\p (play-boxes activeboxes) (mapcar 'om-invalidate-view actives))
      (#\s (stop-boxes activeboxes) (mapcar 'om-invalidate-view actives))      
      (#\Space (if (idle-p *general-player*)
                   (play-boxes activeboxes)
                 (stop-all-boxes))
               (mapcar 'om-invalidate-view actives))
      
      (#\t (mapc 'show-online-tutorial actives))
      
      (#\m (mapc 'change-edit-mode actives))
      (#\n (mapc 'set-show-box-name actives))
      (#\M (change-edit-mode-all (get-subframes self)))
           
      (:om-key-up 
       (mapc #'(lambda (item) (move-frame-delta item 0)) actives)
       (make-move-after self actives))
      (:om-key-down 
       (mapc #'(lambda (item) (move-frame-delta item 1)) actives)
       (make-move-after self actives))
      (:om-key-left
       (if (om-option-key-p)
           (mapc #'(lambda (item) (delete-one-input item) t) actives)
         (progn
           (mapc #'(lambda (item) (move-frame-delta item 3)) actives)
           (make-move-after self actives))
         ))
      (:om-key-right 
       (if (om-option-key-p)
           (mapc #'(lambda (item) (add-one-input item)) actives)
         (progn (mapc #'(lambda (item) (move-frame-delta item 2)) actives)
           (make-move-after self actives))))

      (#\< (mapc #'(lambda (item) (delete-one-input item)) actives))
      (#\> (mapc #'(lambda (item) (add-all-inputs item)) actives))
      
      (otherwise (om-beep)))))



;--------------Shortcuts
;Open the little view allowing create a box in 'self' by name, 'pos' is the position of the view.
(defmethod make-undefined-box ((self patchPanel) pos)
   (let* ((thename (mk-unique-name self "undefined"))
          (new-box (omNG-make-new-boxcall 'undefined pos thename))
          (new-frame (make-frame-from-callobj new-box)))
     (om-select-window (window self))
     (omG-add-element self new-frame)
     (open-ttybox (iconview new-frame)) 
     ))

(defmethod delete-general :before ((self patchPanel))
  (setf (undo (editor self)) (append (list 'remove)
                                   (loop for box in (get-actives self) collect
                                         (object box)))))

(defmethod allow-remove (box panel) t)

(defmethod delete-general ((self patchPanel))
   "Remove the selected boxes from 'self'."
   (let ((actives (remove nil (mapcar #'(lambda (box) (when (allow-remove box self) box)) (get-actives self))))
         (connections (get-actives-connections self)))
    (real-make-delete-before self actives)
    (mapc #'(lambda (connec) 
              (let* ((thebox (thebox connec))
                     (thein (nth (index connec) (inputs (object thebox)))))
                (remove-connection thebox (index connec))
                (setf (connected? thein) nil))) connections)
    (om-with-delayed-update (panel self)
      (mapc #'(lambda (box) 
                (omg-remove-element self box)) actives))
    (om-invalidate-view self)))


(defmethod patch-color ((self patchPanel))
  (if (get-actives-connections self)
      (color-connections self)
    (color-comments self)))

(defmethod color-connections ((self patchPanel))
   "Set a new color to the selected connections in 'self'."
   (let ((connections (get-actives-connections self)))
     (mapc #'(lambda (connec) 
               (new-color-connection connec)) connections)))

(defmethod color-comments ((self patchPanel))
   "Set a new color to the selected comment boxes in 'self'."
   (let ((active-comments (loop for frame in (get-actives self)
                                when (commentframep frame) collect frame)) newcolor)
     (when active-comments
       (let ((newcolor (om-choose-color-dialog :color (textcolor (object (car active-comments))))))
         (when newcolor
           (mapc #'(lambda (frame) 
                     (comment-new-color frame newcolor)) active-comments))))))

(defmethod font-comments ((self patchPanel))
   "Set a new font to the selected comment boxes in 'self'."
   (let ((active-comments (loop for frame in (get-actives self)
                                when (commentframep frame) collect frame)) 
         newlist)
     (when active-comments
       (let ((newfont (om-choose-font-dialog :font (textstyle (object (car active-comments))))))
       (when newfont
         (mapc #'(lambda (frame) 
                   (comment-new-style frame newfont)) active-comments))))))

(defmethod edit-bold ((self patcheditor))
  (let* ((container (panel self))
         (subframes (get-actives container))
         (comments (loop for frame in subframes when (commentframep frame) collect frame)))
    (when comments
      (comment-style comments :bold))))

(defmethod edit-italics ((self patcheditor))
  (let* ((container (panel self))
         (subframes (get-actives container))
         (comments (loop for frame in subframes when (commentframep frame) collect frame)))
    (when comments
      (comment-style comments :italic))))


(defmethod reinit-connections ((self patchPanel))
   "Set the color to black and the edited points to nil for selected connections in 'self'."
   (let ((connections (get-actives-connections self))
         correct)
     (mapc #'(lambda (self) 
               (setf (ccolor self) 0)
               (when (nth 3 (connected? (object (nth (index self) (inputframes (thebox self))))))
                 (setf (nth 3 (connected? (object (nth (index self) (inputframes (thebox self)))))) 0))
               (setf (nth 2 (connected? (nth (index self) (inputs (object (thebox self)))))) nil)
               (push (thebox self) correct)) connections)
     (mapc #'(lambda (box) 
               (redraw-connections box)) (remove-duplicates correct :test 'equal))))


;------------------------------

(defmethod clear-ev-once ((self patchPanel))
   "After one evaluation this methods set the ev-once flag of all boxes in ev-once mode to nil."
   (mapc #'(lambda (box)
             (clear-ev-once (object box))) (get-subframes self))
   (setf *cur-eval-panel* nil))

(defmethod save-connections ((self patchPanel) (source patchPanel) list)
   "Save the connections beetwen boses of 'self' as a list."
   (declare (ignore source))
   (mk-connection-list list))

(defmethod remake-draggeds-connections ((self patchPanel) (source patchPanel) listobj list) 
   "Used to remake connections when you drag and drop a subset of boxes from 'self' to 'source'."
   (when (move-and-not-action source self)
     (let (boxes)
       (loop for item in listobj do
             (loop for input in (inputs item) do
                   (setf (connected? input) nil)))
       (remk-connections listobj (loop for con in list collect (load-connection con)))
       (setf boxes (get-elements (object self)))
       (mapc #'(lambda (elem)
                 (update-graphic-connections elem boxes)) (mapcar #'(lambda (x) (car (frames x))) listobj)))))

(defmethod modify-patch ((self patchPanel))
   "Set differents flag ON to say that the patch was modified."
   (setf (compiled? (object self)) nil)
   (setf (saved? (object self)) nil)
   (when (direct-window-p self)
     (om-set-window-title (window self) (string+ "^" (name (object self))))))

(defmethod add-window-buttons ((self patchPanel))
   "Add the input and output buttons at the top-button in 'self'."
   (when *patch-show-win-buttons*
     (om-add-subviews self (om-make-view 'om-icon-button
                                         :icon1 (if (add-output-enabled self 'out) "out" "out-disable")
                                         :icon2 (if (add-output-enabled self 'out) "out-pushed" nil)
                                         :position (om-make-point 5 5)
                                         :size (om-make-point 24 24)
                                         :action
                                         #'(lambda (item) (declare (ignore item)) 
                                             (modify-patch self)
                                             (add-output self nil))
                                         )
                      (om-make-view 'om-icon-button
                                    :icon1 (if (add-input-enabled self 'in) "in" "in-disable")
                                    :icon2 (if (add-input-enabled self 'in) "in-pushed" nil)
                                    :position (om-make-point 30 5)
                                       :size (om-make-point 24 24)
                                       :action #'(lambda (item) (declare (ignore item)) 
                                                   (modify-patch self)
                                                   (add-input self nil))
                                       ))))
   


(defmethod add-input-enabled ((self patchpanel) type) t)
(defmethod add-output-enabled ((self patchpanel) type) t)


(defmethod add-input ((self patchpanel) position)
  (when (add-input-enabled self 'in)
    (let* ((boxes (get-subframes self)) 
           (i (length (list+ (find-class-boxes boxes 'selfInFrame) (find-class-boxes boxes 'InFrame))))
           (pos (or position (om-make-point (+ 5 (* i 50)) 45))))
      (omG-add-element self
                       (make-frame-from-callobj 
                        (make-new-patch-input (mk-unique-name self "input") i pos)))
      (set-field-size self)
      t)
    ))

(defmethod add-output ((self patchpanel) position)
   (when (add-output-enabled self 'out)
     (let* ((boxes (get-subframes self)) 
            (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'outFrame))))
            (pos (or position (om-make-point (+ 5 (* i 50)) 240))))
       (omG-add-element self
                        (make-frame-from-callobj 
                         (make-new-output (mk-unique-name self "output") i pos)))
       (set-field-size self)
       )))


;--------------------------------
;Tools
;--------------------------------

(defun mk-connection-list (boxlist)
   "Save the connection beetwen boxes in 'boxlist' as a list.
Elements of the list are list as (source-position source-output target-position target-input connection-points connection-color)."
  (let (rep)
    (loop for box in boxlist
          for i from 0 to (length boxlist) do
          (let ((inputs (inputs box))
                (j 0))
            (mapc #'(lambda (input)
                      (when (connected? input)
                        (let ((posi (position (first (connected? input)) boxlist :test 'equal)))
                          (when posi
                            (push (list posi (second (connected? input)) i j 
                                        (om-save-point-list (third (connected? input))) 
                                        (fourth (connected? input))) rep))))
                      (incf j))
                  inputs)))
    (reverse rep)))


(defun remk-connections (listobj list)
   "Make connections beetwen box of 'listobj' connections are specified by 'list'."
   (loop for item in list do
         (when (and (nth (first item) listobj) (nth (third item) listobj))
           (omNG-connect (nth (first item) listobj)
                         (second item)
                         (nth (third item) listobj)
                         (fourth item)
                         (fifth item)
                         (sixth item)))))

(defun find-box-with-name (list name)
   "Return the box in 'list' named 'name'."
   (find-if #'(lambda (box) (string-equal (name box) name)) list))


(defun change-edit-mode-all (boxes)
   "Set the minipict mode of ALL editors ON or OFF."
   (let (firstval curval)
     (loop for item in boxes do
           (when (has-mini-pict? (object item))
             (unless firstval
               (setf firstval t
                     curval (showpict (object item))))
             (when (equal  curval (showpict (object item)))
               (change-edit-mode item))))))

;==========================================================================
;NEXT METHODS WERE ADDED FOR PICT MANAGEMENT
;==========================================================================


(defmethod om-draw-contents :before ((self patchPanel))
   (om-with-focused-view self
     (loop for item in (pictu-list (object self)) do
               (draw-pict-patch item self)))
   ) 

(defmethod select-a-picture ((self patchPanel))
   (let ((list (pictu-list (object self)))
         selected)
     (when list
       (loop for item in list
             for i = 0 then (+ i 1)
             while (not selected) do
             (when (selected-p item)
               (setf (selected-p item) nil)
               (invalidate-picture item self)
               (setf selected i)))
       (cond
        ((null selected) (setf selected 0))
        ((= selected (- (length list) 1)) (setf selected 0))
        (t (setf selected (+ selected 1))))
       (setf (selected-p (nth selected list)) t)
       (invalidate-picture (nth selected list) self))))

(defmethod get-selected-picts ((self patchPanel))
   (let ((list (pictu-list (object self))))
         (loop for item in list
               when (selected-p item) collect item)))
             
(defvar *pict-mov-size* nil)
(defvar *pict-mov-delta* nil)
(defvar *initial-rectangle* nil)

(defmethod om-view-click-handler :around ((self patchPanel) where)  
   (let ((sel (car (get-selected-picts self))))
     (om-with-focused-view self 
       (if sel
           (let ((w (om-point-h (pict-size sel)))
                 (h (om-point-v (pict-size sel)))
                 (x0 (om-point-h (pict-pos sel)))
                 (y0 (om-point-v (pict-pos sel))))
           (let ((r (om-make-rect x0 y0 (+ x0 w) (+ y0 h)))
                 (resizerect (om-make-rect (+ x0 w -8) (+ y0 h -8) (+ x0 w) (+ y0 h))))
             (modify-patch self)
             (if (om-point-in-rect-p where resizerect)
                 (progn
                   (setf *pict-mov-size* (pict-size sel))
                   (setf *pict-mov-delta* where)
                   (setf *initial-rectangle* r)
                   (om-init-motion-functions self 'make-resize-bg-pict 'release-resize-bg-pict)
                   (om-new-movable-object self x0 y0 w h 'om-selection-rectangle)
                   )
               (if (om-point-in-rect-p where r) 
                   (progn
                     (setf *pict-mov-size* (pict-size sel))
                     (setf *pict-mov-delta* (om-make-point (- (om-point-h where) x0) (- (om-point-v where) y0)))
                     (setf *initial-rectangle* r)
                     (om-init-motion-functions self 'make-move-bg-pict 'release-move-bg-pict)
                     (om-new-movable-object self x0 y0 w h 'om-selection-rectangle)
                     )
                 (progn
                   (setf (selected-p sel) nil)
                   (invalidate-picture sel self)
                   (call-next-method))))))
         (call-next-method)))))

(defmethod make-move-bg-pict ((Self patchPanel) pos)
   (om-update-movable-object self (- (om-point-h pos) (om-point-h *pict-mov-delta*))
                                (- (om-point-v pos) (om-point-v *pict-mov-delta*))
                                (om-point-h *pict-mov-size*) (om-point-v *pict-mov-size*)))

(defmethod make-resize-bg-pict ((Self patchPanel) pos)
   (om-update-movable-object self (om-rect-left *initial-rectangle*) (om-rect-top *initial-rectangle*)
                             (+ (om-point-h *pict-mov-size*) (- (om-point-h pos) (om-point-h *pict-mov-delta*)))
                             (+ (om-point-v *pict-mov-size*) (- (om-point-v pos) (om-point-v *pict-mov-delta*)))
                             ))

(defmethod release-move-bg-pict ((Self relationPanel) pos)
  (om-erase-movable-object self)
   (let ((sel (car (get-selected-picts self))))
     (when sel
       (if (om-view-contains-point-p self pos)
         (let ((newp (om-make-point (- (om-point-h pos) (om-point-h *pict-mov-delta*))
                                    (- (om-point-v pos) (om-point-v *pict-mov-delta*)))))
           (unless (om-points-equal-p (pict-pos sel) newp)
             (setf (pict-pos sel) newp)
             (when *initial-rectangle*
               (om-invalidate-corners self (om-rect-topleft *initial-rectangle*) 
                                      (om-add-points (om-rect-bottomright *initial-rectangle*) (om-make-point 4 4)))
               (invalidate-picture sel self)
               )
             ))
         (om-beep)))))


(defmethod release-resize-bg-pict ((Self relationPanel) pos)
  (om-erase-movable-object self)
   (let ((sel (car (get-selected-picts self))))
     (when sel
       (if (om-view-contains-point-p self pos)
         (let ((newp (om-make-point (+ (om-point-h *pict-mov-size*) (- (om-point-h pos) (om-point-h *pict-mov-delta*)))
                                    (+ (om-point-v *pict-mov-size*) (- (om-point-v pos) (om-point-v *pict-mov-delta*))))))
           (unless (om-points-equal-p (pict-size sel) newp)
             (setf (pict-size sel) newp)
             (when *initial-rectangle*
               (om-invalidate-corners self (om-rect-topleft *initial-rectangle*) 
                                      (om-add-points (om-rect-bottomright *initial-rectangle*) (om-make-point 4 4)))
               (invalidate-picture sel self)
               )
             ))
         (om-beep)))))

(defmethod invalidate-picture ((self patch-picture) view)
  (om-invalidate-corners view (pict-pos self) (om-make-point (+ (om-point-h (pict-pos self)) (om-point-h (pict-size self)) 4)
                                                             (+ (om-point-v (pict-pos self)) (om-point-v (pict-size self)) 4))))

(defmethod delete-general :around ((self patchPanel))
   (let ((sel (car (get-selected-picts self))))
     (if sel
       (progn
         (setf (pictu-list (object self)) (remove sel (pictu-list (object self)) :test 'equal))
         (om-kill-picture (thepict sel))
         (om-invalidate-view self t))
       (call-next-method))))


(defmethod reinit-bg-picts ((self patchpanel))
  (let ((selpic (car (get-selected-picts self))))
    (when (and selpic (thepict selpic)) 
        (let ((init-size (pict-size selpic)))
          (setf (pict-size selpic) (om-get-picture-size (thepict selpic)))
          (om-invalidate-corners self (pict-pos selpic) (om-make-point (+ (om-point-h (pict-pos selpic)) (max (om-point-h init-size) (om-point-h (pict-size selpic))) 4)
                                                                       (+ (om-point-v (pict-pos selpic)) (max (om-point-v init-size) (om-point-v (pict-size selpic))) 4)))))))

(defmethod om-view-cursor ((self patchPanel))
  (if (and *adding-a-box* (equal (window self) (second *adding-a-box*)))
    *om-box-cursor* 
    (let ((sel (car (get-selected-picts self))))
       (if (and sel (om-point-in-rect-p (om-mouse-position self)
                                        (om-make-rect (+ (om-point-h (pict-pos sel)) (om-point-h (pict-size sel)) -8) 
                                                      (+ (om-point-v (pict-pos sel)) (om-point-v (pict-size sel)) -8) 
                                                      (+ (om-point-h (pict-pos sel)) (om-point-h (pict-size sel))) 
                                                      (+ (om-point-v (pict-pos sel)) (om-point-v (pict-size sel))))))
           *om-resize-cursor*
         (call-next-method)))))


(defun add-special-box (type pos container)
  (let ((box (get-new-box-from-type type pos container)))
    (when box
      (omG-add-element container (make-frame-from-callobj box)))))



;;; In basic project there is a particular action defined 
;;; for creating a picture object from a patch-picture...
(defmethod get-pict-menu-context ((object t) patch) nil)


(defmethod make-bg-pict ((self patchpanel) pos)
    (let* ((pict-info (choose-resource-dialog :pict :kernel nil :user t))
           (patchpict (make-instance 'patch-picture))
           name src)
      (when (consp pict-info)
        ;(setf name (intern (string+ (string (third pict-info)) "-" (string (first pict-info)))))
        (setf name (first pict-info)
              src (third pict-info))
        (setf (thepict patchpict) (om-get-picture name src))
        (setf (pict-pos patchpict) pos)
        (setf (name patchpict) name)
        (setf (source patchpict) src)
        (setf (pict-size patchpict) (om-get-picture-size (thepict patchpict)))
        (push patchpict (pictu-list (object self)))
        (setf (selected-p patchpict) t)
        (om-invalidate-view self t)
        )
      ))

(defmethod om-get-menu-context ((self PatchPanel))
  (let ((posi (om-mouse-position self))
        (sel (car (get-selected-picts self))))
    (if sel
        (get-pict-menu-context sel self)
      (list 
       (om-new-leafmenu "Comment" #'(lambda () 
                                      (let ((newbox (omNG-make-new-boxcall 'comment posi "comment")))
                                        (when newbox
                                          (omG-add-element self (make-frame-from-callobj newbox))))))
       (om-new-leafmenu "Picture" #'(lambda () 
                                                 (make-bg-pict self posi)))
       (list 
        (om-package-fun2menu *om-package-tree* nil #'(lambda (f) (add-box-from-menu f posi)))
        (om-package-classes2menu *om-package-tree* nil #'(lambda (c) (add-box-from-menu c posi)))
        )
       (list 
        (om-new-menu "Internal..." 
                     (om-new-leafmenu "Patch" #'(lambda () (omG-add-element self (make-frame-from-callobj 
                                                                                  (omNG-make-new-boxcall 
                                                                                   (make-instance 'OMPatchAbs :name "mypatch" :icon 210)
                                                                                   posi (mk-unique-name self "mypatch"))))))
                     (om-new-leafmenu "Maquette" #'(lambda () (omG-add-element self (make-frame-from-callobj 
                                                                                     (omNG-make-new-boxcall 
                                                                                      (make-instance 'OMMaqAbs :name "mymaquette" :icon 265)
                                                                                      posi (mk-unique-name self "mymaquette"))))))
                     (om-new-leafmenu "Loop" #'(lambda () (add-box-from-menu (fdefinition 'omloop) posi)))
                     (om-new-leafmenu "Lisp Function" #'(lambda () (omG-add-element self 
                                                                                    (make-frame-from-callobj 
                                                                                     (omNG-make-new-boxcall 
                                                                                      (make-instance 'OMLispPatchAbs :name "lispfunction" :icon 123)
                                                                                      posi (mk-unique-name self "lispfunction"))))))))
       (list
        (om-new-leafmenu "Input" #'(lambda () (add-input self posi)) nil (add-input-enabled self 'in))
        (om-new-leafmenu "Output" #'(lambda () (add-output self posi)) nil (add-output-enabled self 'out))
        (om-new-menu "TemporalBoxes" 
                     (om-new-leafmenu "Self Input" #'(lambda () (add-special-box 'tempin posi self)) nil (add-input-enabled self 'tempin))
                     (om-new-leafmenu "Maq. Self Input" #'(lambda () (add-special-box 'maq-tempin posi self)) nil (add-input-enabled self 'maq-tempin))
                     (om-new-leafmenu "Temporal Output" #'(lambda () (add-special-box 'tempout posi self)) nil (add-output-enabled self 'tempout)))
        )
       (om-new-leafmenu "Last Saved"  #'(lambda () (window-last-saved (editor self))) nil (if (and 
                                                                                               (mypathname (object self))
                                                                                               (not (subtypep (class-of self) 'methodpanel))) t nil))
       ))))



;;;===============================================
;;;===============================================
;;; BUG FIX MCL ONLY :
;;; quand on passe vite sur un input
;;;===============================================
;;;===============================================
(defmethod window-update-cursor ((self EditorWindow) where)
   (if (editor-update-cursor (editor self) where)
     (call-next-method)))

(defmethod editor-update-cursor ((self EditorView) where) t)

(defmethod editor-update-cursor ((self patchEditor) where)
   (if (subviews self)
     (let ((container (panel self)))
       (if (and container (text-view self)
                (equal (class-name (class-of (text-view self))) 'text-enter-view)
                (view-contains-point-p (text-view self) where))
         (progn
           (exit-from-dialog (text-view self) (dialog-item-text (text-view self))) nil)
         t))
     t))
;;;===============================================
;;;===============================================
;;;===============================================




