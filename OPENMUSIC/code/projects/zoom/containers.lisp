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

;Author: Paulo Henrique Raposo

(in-package :om)

;;; ============================================================
;;; patchPanel: add-input / add-output zoom-aware (viewport-logical + ctx)
;;; ============================================================

(defmethod add-input ((self patchpanel) position)
  (when (add-input-enabled self 'in)
    (let* ((boxes (get-subframes self))
           (i     (length (list+ (find-class-boxes boxes 'selfInFrame)
                                 (find-class-boxes boxes 'InFrame)))))
      (multiple-value-bind (sx sy vw vh) (oa::om-zoom-viewport-logical self)
        (declare (ignore vw vh))
        (let ((pos (or position
                       (om-make-point (+ sx 10 (* i 50))
                                      (+ sy 10)))))
          (omG-add-element self
                           (oa::with-zoom-context-of self
                             (make-frame-from-callobj
                              (make-new-patch-input (mk-unique-name self "input") i pos))))
          (set-field-size self)
          t)))))

(defmethod add-output ((self patchpanel) position)
  (when (add-output-enabled self 'out)
    (let* ((boxes (get-subframes self))
           (i     (length (list+ (find-class-boxes boxes 'tempOutFrame)
                                 (find-class-boxes boxes 'outFrame)))))
      (multiple-value-bind (sx sy vw vh) (oa::om-zoom-viewport-logical self)
        (declare (ignore vw))
        (let ((pos (or position
                       (om-make-point (+ sx 10 (* i 50))
                                      (+ sy (max 10 (- vh 80)))))))
          (omG-add-element self
                           (oa::with-zoom-context-of self
                             (make-frame-from-callobj
                              (make-new-output (mk-unique-name self "output") i pos))))
          (set-field-size self))))))

;;; ============================================================
;;; methodEditor: zoom bar build (typed-input button)
;;; ============================================================

(defmethod om-zoom-editor-bar-build ((editor methodEditor))
  "Generic-function zoom bar: same widgets as the default patch bar, but the
   IN button is a TYPED-INPUT-BUTTON whose DROP-ACTION creates a typed input
   matching the dropped object's class (click still adds a generic typed input
   via ADD-INPUT methodPanel)."
  (let* ((panel (panel editor))
         (zoom  (if (typep panel 'oa::om-scroller) (oa::om-zoom-of panel) 1.0))
         (pct   (round (* zoom 100)))
         (bar (om-make-view 'generic-function-zoom-bar
                            :position (om-make-point 0 0)
                            :size (om-make-point (w editor) +om-zoom-bar-h+)
                            :bg-color *controls-color*))
         (out-btn (om-make-view
                   'om-icon-button
                   :position (om-make-point 5 1)
                   :size (om-make-point 24 24)
                   :icon1 "out"
                   :icon2 "out-pushed"
                   :action #'(lambda (item)
                               (declare (ignore item))
                               (modify-patch panel)
                               (add-output panel nil))))
         (in-btn (om-make-view
                  'methodEditor-input-button
                  :object (make-instance 'input-maker)
                  :target-panel panel
                  :position (om-make-point 30 1)
                  :size (om-make-point 24 24)
                  :icon1 "in"
                  :icon2 "in-pushed"))
         (zoom-bg (om-make-dialog-item
                   'om-static-text
                   (om-make-point 60 1)
                   (om-make-point 95 24)
                   ""
                   :font *om-default-font1*
                   :bg-color *controls-color*))
         (zoom-label (om-make-dialog-item
                      'om-static-text
                      (om-make-point 65 4)
                      (om-make-point 35 18)
                      "Zoom"
                      :font *om-default-font1*
                      :bg-color *controls-color*))
         (numbox (om-make-view
                  'oa::om-zoom-pop-up
                  :position (om-make-point 105 4)
                  :size (om-make-point 60 18)
                  :value pct
                  :presets '(50 100 150 200 300 400)
                  :font *om-default-font1*
                  :bg-color *om-white-color*
                  :di-action #'(lambda (widget new-pct)
                                 (declare (ignore widget))
                                 (om-zoom-numbox-apply panel (/ new-pct 100.0))))))
    (om-add-subviews bar out-btn in-btn zoom-bg zoom-label numbox)
    (om-add-subviews editor bar)
    (when panel
      (setf (oa::om-pane-property panel :om-zoom-numbox) numbox))
    (setf (oa::om-pane-property editor :om-zoom-editor-bar) bar)
    bar))

;;; ============================================================
;;; Box creation shortcuts (logical pos + ctx propagation)
;;; ============================================================

(defmethod make-undefined-box ((self patchPanel) pos)
  (let* ((thename (mk-unique-name self "undefined"))
         (new-box (omNG-make-new-boxcall 'undefined pos thename))
         (new-frame (oa::with-zoom-context-of self
                      (make-frame-from-callobj new-box))))
    (om-select-window (window self))
    (omG-add-element self new-frame)
    (open-ttybox (iconview new-frame))))

(defmethod make-undefined-funct-box ((self patchPanel) pos)
  (let* ((thename (mk-unique-name self "undefined"))
         (new-box (omNG-make-new-boxcall 'undefined pos thename))
         (new-frame (oa::with-zoom-context-of self
                      (make-frame-from-callobj new-box))))
    (omG-add-element self new-frame)
    (make-tty-win (om-view-container self) (iconview new-frame) pos)
    (om-modal-dialog *tty-window*)))

(defmethod create-comment-box ((self patchPanel) pos)
  (let* ((newbox (omNG-make-new-boxcall 'comment pos "comment"))
         (new-frame (oa::with-zoom-context-of self
                      (make-frame-from-callobj newbox))))
    (om-select-window (window self))
    (omG-add-element self new-frame)))

(defmethod make-comment-box ((self patchPanel) pos)
  (let* ((thename (mk-unique-name self "comment"))
         (newbox (omNG-make-new-boxcall 'comment pos thename))
         (new-frame (oa::with-zoom-context-of self
                      (make-frame-from-callobj newbox)))
         (init (progn (setf om-edit::*comment-text* "")
                      (setf om-edit::*def-comment-edit-font* *comment-style*)
                      (setf om-edit::*comment-color* (oa::om-color-to-capi *comment-color*))))
         (ept (om-edit::open-comment-editor-pane (iconview new-frame))))
    (declare (ignore init))
    (omG-add-element self new-frame)
    (setf (om-edit::intfunc ept) #'om::setfref)
    (setf (om-edit::fontfunc ept) #'om::setcomfontstyle)
    (setf (om-edit::fontcolfunc ept) #'om::setcomfontcolor)
    (setf om-edit::*comment-frame* new-frame)
    (setf (om-edit::resizefunc ept) #'om::reinit-size)
    ept))

(defmethod create-list-box ((self patchPanel) boxes pos)
  (when boxes
    (let* ((newbox (omNG-make-new-lispboxcall 'list pos "list"))
           (new-frame (oa::with-zoom-context-of self
                        (make-frame-from-callobj newbox)))
           (order (mapcar 'second
                          (sort-list
                           (loop for i in boxes
                                 collect (list (oa::vx i) i))
                           :test '< :key 'car))))
      (om-select-window (window self))
      (omG-add-element self new-frame)
      (loop for i from 1 to (length boxes)
            do (add-one-input new-frame))
      (let ((inputs (first-n (om-subviews (car (frames newbox))) (length boxes))))
        (loop for i in order
              for n in inputs
              do (if (boxeditorframe-p i)
                     (connect-box (last-elem (outframes i)) n)
                     (connect-box (car (outframes i)) n)))
        (mapcar 'omg-unselect boxes)))))

(defmethod create-x-append-box ((self patchPanel) boxes pos)
  (when (> (length boxes) 1)
    (let* ((newbox (omng-make-new-boxcall (make-instance 'omgenericfunction
                                                         :name 'x-append
                                                         :icon 235) pos "x-append"))
           (new-frame (oa::with-zoom-context-of self
                        (make-frame-from-callobj newbox)))
           (order (mapcar 'second
                          (sort-list
                           (loop for i in boxes
                                 collect (list (oa::vx i) i))
                           :test '< :key 'car))))
      (om-select-window (window self))
      (omG-add-element self new-frame)
      (loop for i from 1 to (length boxes)
            do (add-one-input new-frame))
      (let ((inputs (first-n (om-subviews (car (frames newbox))) (length boxes))))
        (loop for i in order
              for n in inputs
              do (if (boxeditorframe-p i)
                     (connect-box (last-elem (outframes i)) n)
                     (connect-box (car (outframes i)) n)))
        (mapcar 'omg-unselect boxes)))))

(defun add-special-box (type pos container)
  (let ((box (get-new-box-from-type type pos container)))
    (when box
      (omG-add-element container
                       (oa::with-zoom-context-of container
                         (make-frame-from-callobj box))))))

;;; ============================================================
;;; Click / doubleclick / key dispatch (visual->logical where; ctx propagation)
;;; ============================================================

(defmethod om-view-click-handler ((view patchPanel) where)
  (unless (and (get-selected-picts view)
               (handle-patch-pictures view (car (get-selected-picts view)) where))
    (cond (*adding-a-box*
           ;; (om-mouse-position thepatch) returns LOGICAL while bound.
           (let ((oa::*om-zoom-unscale-mouse-pos-p* t))
             (when (equal (window view) (second *adding-a-box*))
               (let* ((window (second *adding-a-box*))
                      (item (first *adding-a-box*))
                      (thepatch (get-patchpanel (editor window)))
                      (newbox (omNG-make-new-boxcall item (om-make-point 22 22)
                                                    (mk-unique-name thepatch (name item))))
                      pospanel pos)
                 (if *new-obj-initial-pos*
                     (progn
                       (setf (frame-position newbox) *new-obj-initial-pos*)
                       (omG-add-element thepatch
                                        (oa::with-zoom-context-of thepatch
                                          (make-frame-from-callobj newbox))))
                     (progn
                       (setf pospanel (om-mouse-position thepatch)
                             pos (om-mouse-position window))
                       (when (om-point-in-rect-p pos (om-pts-to-rect (om-make-point 0 0)
                                                                    (om-view-size window)))
                         (setf (frame-position newbox) (borne-position pospanel))
                         (omG-add-element thepatch
                                          (oa::with-zoom-context-of thepatch
                                            (make-frame-from-callobj newbox))))))))
             (setf *adding-a-box* nil)
             (setf *new-obj-initial-pos* nil)))
          ((and (om-shift-key-p) (om-option-key-p) (om-command-key-p))
           (let* ((selected (get-actives view))
                  (zoom (oa::om-zoom-of view))
                  (log-where (if (= zoom 1.0) where (oa::om-zoom-unscale-point where zoom))))
             (create-x-append-box view selected log-where)))
          ((and (om-shift-key-p) (om-command-key-p))
           (let* ((selected (get-actives view))
                  (zoom (oa::om-zoom-of view))
                  (log-where (if (= zoom 1.0) where (oa::om-zoom-unscale-point where zoom))))
             (create-list-box view selected log-where)))
          (t (call-next-method))))
  (om-invalidate-view view t))

(defmethod om-view-doubleclick-handler ((self patchPanel) where)
  (when (equal self (call-next-method))
    (let* ((zoom (oa::om-zoom-of self))
           (log-where (if (= zoom 1.0) where (oa::om-zoom-unscale-point where zoom))))
      (make-undefined-box self log-where))))

(defmethod handle-key-event :around ((self patchPanel) char)
  ;; Ctrl+= / Ctrl+- step by 1.1; Ctrl+0 resets to 100% and recenters.
  (cond ((and (om-command-key-p) (oa::om-zoom-applies-p self)
              (or (equal char #\=) (equal char #\+)))
         (oa::om-zoom-step-anchored self (* (oa::om-zoom-of self) 1.1))
         t)
        ((and (om-command-key-p) (oa::om-zoom-applies-p self)
              (equal char #\-))
         (oa::om-zoom-step-anchored self (/ (oa::om-zoom-of self) 1.1))
         t)
        ((and (om-command-key-p) (oa::om-zoom-applies-p self)
              (equal char #\0))
         (oa::om-zoom-reset-and-recenter self)
         t)
        (t (let ((oa::*om-zoom-unscale-mouse-pos-p* t))
             (call-next-method)))))

(defmethod om-get-menu-context ((self PatchPanel))
  (let* ((oa::*om-zoom-unscale-mouse-pos-p* t)
         (posi (om-mouse-position self))
         (sel  (car (get-selected-picts self))))
    (if sel
        (get-pict-menu-context sel self)
        (flet ((with-ctx (thunk)
                 (oa::with-zoom-context-of self (funcall thunk))))
          (list
           (om-new-leafmenu "Comment"
                            #'(lambda ()
                                (let ((newbox (omNG-make-new-boxcall 'comment posi "comment")))
                                  (when newbox
                                    (omG-add-element self
                                                     (with-ctx
                                                      (lambda ()
                                                        (make-frame-from-callobj newbox))))
                                    (reinit-size (car (frames newbox)))))))
           (om-new-leafmenu "Picture"
                            #'(lambda () (make-bg-pict self posi)))
           (list
            (om-package-fun2menu *om-package-tree* nil
                                 #'(lambda (f) (add-box-from-menu f posi)))
            (om-package-classes2menu *om-package-tree* nil
                                     #'(lambda (c) (add-box-from-menu c posi))))
           (list
            (om-new-menu "Internal..."
                         (om-new-leafmenu "Patch"
                                          #'(lambda ()
                                              (omG-add-element self
                                                               (with-ctx
                                                                (lambda ()
                                                                  (make-frame-from-callobj
                                                                   (omNG-make-new-boxcall
                                                                    (make-instance 'OMPatchAbs :name "mypatch" :icon 210)
                                                                    posi (mk-unique-name self "mypatch"))))))))
                         (om-new-leafmenu "Maquette"
                                          #'(lambda ()
                                              (omG-add-element self
                                                               (with-ctx
                                                                (lambda ()
                                                                  (make-frame-from-callobj
                                                                   (omNG-make-new-boxcall
                                                                    (make-instance 'OMMaqAbs :name "mymaquette" :icon 265)
                                                                    posi (mk-unique-name self "mymaquette"))))))))
                         (om-new-leafmenu "Loop"
                                          #'(lambda () (add-box-from-menu (fdefinition 'omloop) posi)))
                         (om-new-leafmenu "Lisp Function"
                                          #'(lambda ()
                                              (omG-add-element self
                                                               (with-ctx
                                                                (lambda ()
                                                                  (make-frame-from-callobj
                                                                   (omNG-make-new-boxcall
                                                                    (make-instance 'OMLispPatchAbs :name "lispfunction" :icon 123)
                                                                    posi (mk-unique-name self "lispfunction"))))))))))
           (list
            (om-new-leafmenu "Input"
                             #'(lambda () (add-input self posi)) nil (add-input-enabled self 'in))
            (om-new-leafmenu "Output"
                             #'(lambda () (add-output self posi)) nil (add-output-enabled self 'out))
            (om-new-menu "TemporalBoxes"
                         (om-new-leafmenu "Self Input"
                                          #'(lambda () (add-special-box 'tempin posi self))
                                          nil (add-input-enabled self 'tempin))
                         (om-new-leafmenu "Maq. Self Input"
                                          #'(lambda () (add-special-box 'maq-tempin posi self))
                                          nil (add-input-enabled self 'maq-tempin))
                         (om-new-leafmenu "Temporal Output"
                                          #'(lambda () (add-special-box 'tempout posi self))
                                          nil (add-output-enabled self 'tempout))))
           (om-new-leafmenu "Last Saved"
                            #'(lambda () (window-last-saved (editor self)))
                            nil
                            (and (mypathname (object self))
                                 (not (subtypep (class-of self) 'methodpanel)))))))))

;;; ============================================================
;;; Connection point list save (zoom unscale of stored points)
;;; ============================================================

(defun mk-connection-list (boxlist)
  ;; Stored connection points must be LOGICAL; CAPI gives VISUAL.
  (let* ((rep)
         (zoom (let ((b (find-if #'(lambda (bx) (car (frames bx))) boxlist)))
                 (if b (oa::om-zoom-effective (car (frames b))) 1.0))))
    (loop for box in boxlist
          for i from 0 to (length boxlist) do
            (let ((inputs (inputs box))
                  (j 0))
              (mapc #'(lambda (input)
                        (when (connected? input)
                          (let ((posi (position (first (connected? input)) boxlist :test 'equal)))
                            (when posi
                              (let ((raw-pts (third (connected? input))))
                                (push (list posi (second (connected? input)) i j
                                            (om-save-point-list
                                             (if (or (null raw-pts) (= zoom 1.0))
                                                 raw-pts
                                                 (mapcar #'(lambda (p) (oa::om-zoom-unscale-point p zoom))
                                                         raw-pts)))
                                            (fourth (connected? input))) rep)))))
                        (incf j))
                    inputs)))
    (reverse rep)))

;;; ============================================================
;;; patchPanel background picture: visual<->logical coords
;;; ============================================================

(defmethod handle-patch-pictures ((self patchPanel) sel where)
  ;; `where` is VISUAL from CAPI; pict-pos / pict-size are LOGICAL.
  (let* ((zoom      (oa::om-zoom-of self))
         (log-where (if (= zoom 1.0) where (oa::om-zoom-unscale-point where zoom)))
         (w  (om-point-h (pict-size sel)))
         (h  (om-point-v (pict-size sel)))
         (x0 (om-point-h (pict-pos sel)))
         (y0 (om-point-v (pict-pos sel))))
    (let ((r (om-make-rect x0 y0 (+ x0 w) (+ y0 h)))
          (resizerect (om-make-rect (+ x0 w -8) (+ y0 h -8) (+ x0 w) (+ y0 h))))
      (modify-patch self)
      (if (om-point-in-rect-p log-where resizerect)
          (progn
            (setf *pict-mov-size* (pict-size sel))
            (setf *pict-mov-delta* log-where)
            (setf *initial-rectangle* r)
            (om-init-motion-click self where :motion-action 'draw-resize-bg-pict :release-action 'draw-update-bg-pict)
            t)
          (if (om-point-in-rect-p log-where r)
              (progn
                (setf *pict-mov-size* (pict-size sel))
                (setf *pict-mov-delta* (om-make-point (- (om-point-h log-where) x0)
                                                     (- (om-point-v log-where) y0)))
                (setf *initial-rectangle* r)
                (om-init-motion-click self where :motion-action 'draw-move-bg-pict :release-action 'draw-update-bg-pict)
                t)
              (progn
                (setf (selected-p sel) nil)
                (invalidate-picture sel self)
                nil))))))

(defmethod draw-move-bg-pict ((self patchPanel) pos prev-pos)
  ;; `pos` VISUAL from motion; *pict-mov-delta* + pict-pos LOGICAL; invalidate-corners VISUAL.
  (declare (ignore prev-pos))
  (let* ((sel  (car (get-selected-picts self)))
         (zoom (oa::om-zoom-of self)))
    (when sel
      (if (om-view-contains-point-p self pos)
          (let* ((log-pos (if (= zoom 1.0) pos (oa::om-zoom-unscale-point pos zoom)))
                 (newp (om-make-point (- (om-point-h log-pos) (om-point-h *pict-mov-delta*))
                                      (- (om-point-v log-pos) (om-point-v *pict-mov-delta*)))))
            (unless (om-points-equal-p (pict-pos sel) newp)
              (setf (pict-pos sel) newp)
              (when *initial-rectangle*
                (let* ((tl     (om-rect-topleft *initial-rectangle*))
                       (br     (om-add-points (om-rect-bottomright *initial-rectangle*) (om-make-point 4 4)))
                       (vis-tl (if (= zoom 1.0) tl (oa::om-zoom-scale-point tl zoom)))
                       (vis-br (if (= zoom 1.0) br (oa::om-zoom-scale-point br zoom))))
                  (om-invalidate-corners self vis-tl vis-br))
                (invalidate-picture sel self))))
          (om-beep)))))

(defmethod draw-resize-bg-pict ((self patchPanel) pos prev-pos)
  ;; `pos` VISUAL; *pict-mov-size* + *pict-mov-delta* LOGICAL.
  (declare (ignore prev-pos))
  (let* ((sel  (car (get-selected-picts self)))
         (zoom (oa::om-zoom-of self)))
    (when sel
      (if (om-view-contains-point-p self pos)
          (let* ((log-pos (if (= zoom 1.0) pos (oa::om-zoom-unscale-point pos zoom)))
                 (newp (om-make-point (+ (om-point-h *pict-mov-size*)
                                         (- (om-point-h log-pos) (om-point-h *pict-mov-delta*)))
                                      (+ (om-point-v *pict-mov-size*)
                                         (- (om-point-v log-pos) (om-point-v *pict-mov-delta*))))))
            (unless (om-points-equal-p (pict-size sel) newp)
              (setf (pict-size sel) newp)
              (when *initial-rectangle*
                (let* ((tl     (om-rect-topleft *initial-rectangle*))
                       (br     (om-add-points (om-rect-bottomright *initial-rectangle*) (om-make-point 4 4)))
                       (vis-tl (if (= zoom 1.0) tl (oa::om-zoom-scale-point tl zoom)))
                       (vis-br (if (= zoom 1.0) br (oa::om-zoom-scale-point br zoom))))
                  (om-invalidate-corners self vis-tl vis-br))
                (invalidate-picture sel self))))
          (om-beep)))))

(defmethod invalidate-picture ((self patch-picture) view)
  ;; pict-pos/size LOGICAL; om-invalidate-corners consumes VISUAL.
  (let* ((zoom (oa::om-zoom-of view))
         (tl   (pict-pos self))
         (br   (om-make-point (+ (om-point-h (pict-pos self)) (om-point-h (pict-size self)) 4)
                              (+ (om-point-v (pict-pos self)) (om-point-v (pict-size self)) 4)))
         (vis-tl (if (= zoom 1.0) tl (oa::om-zoom-scale-point tl zoom)))
         (vis-br (if (= zoom 1.0) br (oa::om-zoom-scale-point br zoom))))
    (om-invalidate-corners view vis-tl vis-br)))

(defmethod om-view-cursor ((self patchPanel))
  (if (and *adding-a-box* (equal (window self) (second *adding-a-box*)))
      *om-box-cursor*
      (let* ((sel  (car (get-selected-picts self)))
             (zoom (if sel (oa::om-zoom-of self) 1.0))
             (vis-pos  (when sel (if (= zoom 1.0) (pict-pos  sel) (oa::om-zoom-scale-point (pict-pos  sel) zoom))))
             (vis-size (when sel (if (= zoom 1.0) (pict-size sel) (oa::om-zoom-scale-point (pict-size sel) zoom)))))
        (if (and sel (om-point-in-rect-p (om-mouse-position self)
                                         (om-make-rect (+ (om-point-h vis-pos) (om-point-h vis-size) -8)
                                                       (+ (om-point-v vis-pos) (om-point-v vis-size) -8)
                                                       (+ (om-point-h vis-pos) (om-point-h vis-size))
                                                       (+ (om-point-v vis-pos) (om-point-v vis-size)))))
            *om-resize-cursor*
            (call-next-method)))))

;;; ============================================================
;;; Zoom-aware chrome: suppress canvas buttons + sync display
;;; ============================================================

(defmethod add-window-buttons ((self patchPanel))
  ;; Editor zoom bar hosts in/out buttons; canvas buttons would duplicate.
  (when (and *patch-show-win-buttons*
             (not (oa::om-zoom-applies-p self)))
    (om-add-subviews self
                     (om-make-view 'om-icon-button
                                   :icon1 (if (add-output-enabled self 'out) "out" "out-disable")
                                   :icon2 (if (add-output-enabled self 'out) "out-pushed" nil)
                                   :position (om-make-point 5 5)
                                   :size (om-make-point 24 24)
                                   :action
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (modify-patch self)
                                       (add-output self nil)))
                     (om-make-view 'om-icon-button
                                   :icon1 (if (add-input-enabled self 'in) "in" "in-disable")
                                   :icon2 (if (add-input-enabled self 'in) "in-pushed" nil)
                                   :position (om-make-point 30 5)
                                   :size (om-make-point 24 24)
                                   :action #'(lambda (item)
                                               (declare (ignore item))
                                               (modify-patch self)
                                               (add-input self nil))))))

(defmethod oa::om-zoom-sync-display ((pane patchPanel) factor)
  (oa::om-zoom-sync-numbox pane factor)
  (let* ((editor (om-view-container pane))
         (patch  (and editor (object editor))))
    (when (and patch (typep patch 'OMPersistantObject)
               (not (oa::om-pane-property pane :om-zoom-restoring-p)))
      (set-win-zoom patch factor))))

(defun om-zoom-selected-frames (pane)
  (append (get-actives pane) (get-actives-connections pane)))

(setf oa::*om-zoom-selected-frames-fn* #'om-zoom-selected-frames)

;;; ============================================================
;;; methodPanel: input/output predicates + zoom-aware add-input
;;; methodEditor bar uses a typed-input-button child with :target-panel
;;; ============================================================

(defmethod add-output-enabled ((self methodPanel) type) t)
(defmethod add-input-enabled  ((self methodPanel) type) t)

;; Variant used by the methodEditor zoom bar: holds an explicit reference
;; to the target methodPanel so PERFORM-DROP can locate the panel where
;; the typed input should be added. Vanilla typed-input-button relies on
;; (om-view-container target) returning the panel, which is only true
;; when the button is a direct child of the panel.
(defclass methodEditor-input-button (typed-input-button)
  ((target-panel :initform nil :initarg :target-panel :accessor target-panel)))

;; PERFORM-DROP methods for methodEditor-input-button live in
;; kernel/graphics/dragdrop/performdrop.lisp, alongside the analogous
;; methods for typed-input-button — that file is loaded after OMDRAG-DROP
;; is defined.

;; ZOOM-COORD: place new typed input within current logical viewport.
;; Generic-function methods take TYPED inputs; route ADD-INPUT through
;; MAKE-NEW-TYPED-INPUT so the generic zoom-bar IN button (and any other
;; caller) yields a typed-input by default.
(defmethod add-input ((self methodPanel) position)
  (when (add-input-enabled self 'in)
    (let* ((boxes (get-subframes self))
           (i     (length (find-class-boxes boxes 'TypedInFrame))))
      (multiple-value-bind (sx sy vw vh) (oa::om-zoom-viewport-logical self)
        (declare (ignore vw vh))
        (let ((pos (or position
                       (om-make-point (+ sx 10 (* i 50))
                                      (+ sy 10)))))
          (omG-add-element self
                           (oa::with-zoom-context-of self
                             (make-frame-from-callobj
                              (make-new-typed-input
                               (unique-name-from-list-new
                                "input" (get-elements (object self))
                                :mode :num :space nil)
                               't (+ i 1) pos))))
          (set-field-size self)
          t)))))

(defmethod do-default-action ((self input-maker) icon)
  (let* ((thescroller (or (and (typep icon 'methodEditor-input-button)
                               (target-panel icon))
                          (om-view-container icon)))
         (boxes (get-subframes thescroller))
         (i (length (find-class-boxes boxes 'TypedInFrame)))
         (pos (om-make-point (+ 5 (* i 50)) 45)))
    (omG-add-element thescroller
                     (oa::with-zoom-context-of thescroller
                       (make-frame-from-callobj
                        (make-new-typed-input
                         (unique-name-from-list-new
                          "input" (get-elements (object thescroller))
                          :mode :num :space nil)
                         't (+ i 1) pos))))))

;;; ============================================================
;;; relationcontainer: open + paste/undo/alias ctx + add-element + click
;;; open-new-RelationFrame applies saved zoom BEFORE frame creation
;;; to avoid a tardy om-zoom-update on an unrealised panel.
;;; ============================================================

(defun open-new-RelationFrame (object name elements &optional ref pos size)
  (let* ((position (or pos (get-win-position object)))
         (siz (or size (get-win-size object)))
         (newwindow (make-editor-window (get-editor-class object)
                                        object name ref
                                        :winpos position
                                        :winsize siz
                                        :winshow nil))
         (panel (panel newwindow))
         ;; Apply saved zoom to panel BEFORE creating frames, so make-frame-from-callobj
         ;; sees it via *make-frame-zoom-context* / om-zoom-effective and builds frames
         ;; already scaled. This avoids a tardy om-zoom-update on a non-realized panel,
         ;; which racially crashed apply-in-pane-process on CAPI gadgets (cross-process).
         (saved-zoom (and (typep panel 'oa::om-scroller)
                          (let ((z (get-win-zoom object)))
                            (and (numberp z) (/= z 1.0) z)))))
    (when saved-zoom
      (setf (oa::om-pane-property panel :om-zoom-restoring-p) t)
      (unwind-protect
           (progn
             (setf (oa::om-zoom-of panel) saved-zoom)
             (oa::om-zoom-sync-display panel saved-zoom))
        (setf (oa::om-pane-property panel :om-zoom-restoring-p) nil)))
    (om-with-delayed-redraw panel
      (om-with-delayed-update panel
        (mapc #'(lambda (elem)
                  (let ((newframe (oa::with-zoom-context-of panel
                                    (make-frame-from-callobj elem))))
                    (om-add-subviews panel newframe)
                    (add-subview-extra newframe)))
              elements)
        (mapc #'(lambda (elem)
                  (update-graphic-connections elem elements))
              (get-subframes panel))
        (add-window-buttons panel)))
    (set-field-size panel)
    #+linux (oa::om-redisplay-element panel)
    newwindow))

(defmethod editor-paste ((self relationEditor))
  ; (print (eval (first (get-clipboard self)))))
  "Paste from the clipboard to 'self'."
  (let ((container (panel self)))
    (if (text-view self)
        (om-paste-command (text-view self))
        (let ((val (get-clipboard self)))
          (when val
            (let ((connections (second val))
                  (new-boxes (eval (first val)))
                  copies)
              (loop for item in (get-actives container) do
                    (omG-unselect item))
              (om-with-delayed-update container
                (mapcar #'(lambda (object)
                            (setf (name object) (mk-unique-name container (name object)))
                            (setf (frame-position object) (paste-position object self))
                            (let ((new-frame (oa::with-zoom-context-of container
                                               (make-frame-from-callobj object))))
                              (omG-add-element container new-frame)
                              (omG-select new-frame)))
                        new-boxes))
              (remake-draggeds-connections container container new-boxes connections)
              ; for multiple paste : make another copy in the clipboard
              (setf copies (mapcar #'(lambda (box) (omNG-copy box)) new-boxes))
              (set-clipboard self (list `(list ,.copies)
                                        (save-connections container container new-boxes)))
              (om-invalidate-view container)))))
    t))

(defmethod do-undo ((self relationeditor))
  (let ((type (car (undo self))))
    (cond ((equal type 'remove)
           (let (framelist)
             (om-with-delayed-update (panel self)
               (mapc #'(lambda (elem)
                         (let ((newframe (oa::with-zoom-context-of (panel self)
                                           (make-frame-from-callobj elem))))
                           (push newframe framelist)
                           (om-add-subviews (panel self) newframe)
                           (add-subview-extra newframe)))
                     (cdr (undo self))))
             (setf (undo self) (append (list 'add) framelist))))
          ((equal type 'add)
           (let (boxlist)
             (om-with-delayed-update (panel self)
               (mapc #'(lambda (frame)
                         (push (object frame) boxlist)
                         (omg-remove-element (panel self) frame))
                     (cdr (undo self))))
             (setf (undo self) (append (list 'remove) boxlist))))
          (t (setf (undo self) nil)))))

(defmethod alias-editor ((self relationEditor))
  "Make aliases of the selected icons."
  (let* ((container (panel self))
         (subframes (get-actives container))
         object)
    (when subframes
      (mapc #'omG-unselect subframes)
      (mapcar #'(lambda (oldframe)
                  (setf object (omNG-make-alias (object oldframe)))
                  (when object
                    (let ((new-frame (oa::with-zoom-context-of container
                                       (make-frame-from-callobj object))))
                      (omG-add-element container new-frame)
                      (omG-select new-frame))))
              subframes))))

(defmethod omG-add-element ((self relationPanel) frame)
  "Add a boxframe to the scroller, this method call the 'omNG-add-element' method
   with the objects referenced by 'self' and 'frame'."
  (omNG-add-element (object self) (object frame))
  (om-add-subviews self frame)
  (add-subview-extra frame)
  (when (and (typep frame 'boxframe)
             (function-without-name-p (reference (object frame))))
    (let ((logical-size (om-make-point 36 36)))
      (setf (frame-size (object frame)) logical-size)
      (om-set-view-size frame logical-size))))

(defmethod control-actives ((view relationPanel) where)
  (close-enter-dialog (editor view))
  (unless (click-in-connection view where)
    (unless (om-shift-key-p)
      (mapc #'(lambda (control) (deactivate-connect control))
            (get-connections view)))
    (cond ((and (om-command-key-p) (not (om-option-key-p)))
           (let* ((zoom (oa::om-zoom-of view))
                  (log-where (if (= zoom 1.0) where (oa::om-zoom-unscale-point where zoom))))
             (make-undefined-box view log-where)))
          (t (call-next-method)))))

;;; ============================================================
;;; metaobjectcontainer.lisp: lock-aware remove + logical bottom helper
;;; ============================================================

(defmethod om-remove-subviews ((self metaobj-panel) &rest subviews)
  (declare (ignore subviews))
  (call-next-method)
  (unless (om-api::locked self)
    (set-field-size self)))

(defun set-field-size-bottom-logical (view zoom)
  (when (and (boxframe-p view) (object view))
    (let ((zoom (or zoom 1.0)))
      (om-add-points
       (or (frame-position (object view))
           (if (= zoom 1.0)
               (om-view-position view)
               (oa::om-zoom-unscale-point (om-view-position view) zoom)))
       (or (frame-size (object view))
           (if (= zoom 1.0)
               (om-view-size view)
               (oa::om-zoom-unscale-point (om-view-size view) zoom)))))))

;;; ============================================================
;;; hierarchiecontainer.lisp: zoom-aware tree editor + class creation
;;; ============================================================

(defun open-new-RelationTree (object name elements)
  (let* (newwindow)
    (setf newwindow
          (make-editor-window 'classTreeEditor object name nil
                              :winsize (om-make-point 500 300)
                              :winpos (get-win-position object)))
    (om-with-delayed-update (panel newwindow)
      (mapc #'(lambda (elem)
                (let ((newframe (oa::with-zoom-context-of (panel newwindow)
                                  (make-frame-from-callobj elem))))
                  (om-add-subviews (panel newwindow) newframe)
                  (add-subview-extra newframe))) elements)
      (mapc #'(lambda (elem)
                (update-graphic-connections elem elements))
            (get-subframes (panel newwindow))))
    (add-window-buttons (panel newwindow))
    (set-field-size (panel newwindow))
    newwindow))

(defmethod editor-make-new-icon-window ((self classTreeEditor) &optional type)
  (declare (ignore type))
  (if (protected-p (object (panel self)))
      (dialog-message "This package is protected, you can not add classes to it!")
      (let* ((scrollframe (panel self))
             new-object new-frame dial string iconID doc)
        (setf dial (get-classname-dialog))
        (when dial
          (setf string (first dial))
          (setf iconID (third dial))
          (setf doc (second dial))
          (when (string-equal doc "")
            (setf doc "no documentation"))
          (when (and string (not (string-equal "" string)))
            (if (exist-class-p string)
                (progn
                  (dialog-message (string+ "The class " string " is already defined!"))
                  (editor-make-new-icon-window self type))
                (progn
                  (setf new-object (eval `(defclass* ,(interne string) () () (:icon ,(car (list! iconID))) (:documentation ,doc))))
                  (setf (create-info new-object) (list (om-get-date) (om-get-date)))
                  (when (listp iconID)
                    (icon-for-user-package new-object (second iconID)))
                  (setf new-frame (omNG-add-element (object scrollframe) new-object))
                  (om-add-subviews scrollframe
                                   (oa::with-zoom-context-of scrollframe
                                     (make-frame-from-callobj new-frame))))))))))

(defmethod om-get-menu-context ((self classTreePanel))
  (let ((pos (om-mouse-position self)))
    (if (protected-p (object (editor self)))
        (list (om-new-leafmenu "Protected Package" nil nil nil))
        (list
         (om-package-classes2menu *om-package-tree* "Select Superclass"
                                  #'(lambda (c)
                                      (let ((object (omNG-make-new-boxalias c pos (string+ (name c) "-alias"))))
                                        (when object
                                          (let ((new-frame (oa::with-zoom-context-of self
                                                             (make-frame-from-callobj object))))
                                            (omG-add-element self new-frame))))))
         (om-new-leafmenu "Import Class"
                          #'(lambda ()
                              (let ((file (om-choose-file-dialog :prompt "Choose a Class file"
                                                                 :types '("OM Classes" "*.omc" "All files" "*.*"))))
                                (when file
                                  (om-beep-msg "not available yet..."))))))) ))

;;; ============================================================
;;; import-export.lisp: zoom-aware file import
;;; ============================================================

(defmethod om-import-files-in-app ((self patchpanel) files)
  (when (= 1 (length files))
    (let ((newbox (import-dragged-object self (pathname (car files)) (om-mouse-position self))))
      (if newbox
          (progn (omG-add-element self
                                  (oa::with-zoom-context-of self
                                    (make-frame-from-callobj newbox)))
                 t)
          (om-beep-msg (string+ "File: " (namestring (pathname (car files))) " can not be imported in the patch."))))))

;;; ============================================================
;;; maquettecontainer.lisp: zoom-aware tempobj creation
;;; Maquette has its own native zoom (rangex/rangey + cursor-mode :zoom);
;;; the generic per-pane factor is pinned to 1.0 via opt-outs.lisp.
;;; ============================================================

(defmethod make-tempobj ((self MaquettePanel) x y)
  "Add a new empty temporalbox to 'self'. This method is called when you make ALT+CLICK+DRAG in 'self'."
  (let* ((thename (mk-unique-name self "tempobj"))
         (new-patch (make-instance 'OMPatchAbs :name thename :icon 210))
         (pixsizex (max 20 (- (om-point-h y) (om-point-h x))))
         (pixsizey (max 10 (- (om-point-v y) (om-point-v x))))
         (maqpos (get-offset/posy-from-pixel self (om-make-point (om-point-h x) (om-point-v x))))
         (y-size (pixel2norme self 'y pixsizey))
         (tempobj (omNG-make-tempobj new-patch maqpos thename))
         new-frame)
    (add-temp-boxes new-patch)
    (setf (slot-value tempobj 'extend) (pixel2norme self 'x pixsizex))
    (setf (slot-value tempobj 'sizey) y-size)
    (setf new-frame (oa::with-zoom-context-of self
                      (make-frame-from-callobj tempobj)))
    (omG-add-element self new-frame)))

(defmethod make-maq-tempobj ((self MaquettePanel) x y)
  "Add a new empty temporal maq to 'self'. This method is called when you make ALT+CLICK+DRAG in 'self'."
  (let* ((class (get-absmaqclass))
         (thename (mk-unique-name self (get-maq-obj-name class)))
         (new-patch (make-instance class :name thename :icon 265))
         (pixsizex (max 20 (- (om-point-h y) (om-point-h x))))
         (pixsizey (max 10 (- (om-point-v y) (om-point-v x))))
         (maqpos (get-offset/posy-from-pixel self (om-make-point (om-point-h x) (om-point-v x))))
         (y-size (pixel2norme self 'y pixsizey))
         (tempobj (omNG-make-tempobj new-patch maqpos thename))
         new-frame)
    (setf (slot-value tempobj 'extend) (pixel2norme self 'x pixsizex))
    (setf (slot-value tempobj 'sizey) y-size)
    (setf new-frame (oa::with-zoom-context-of self
                      (make-frame-from-callobj tempobj)))
    (omG-add-element self new-frame)))

;;; ============================================================
;;; methodcontainer.lisp: zoom-aware helpers
;;; ============================================================

(defun make-typed-input-from-obj (object panel &optional value)
  (let* ((thetype (class-name object))
         (boxes (get-subframes panel))
         (i (- (length (find-class-boxes boxes 'TypedInFrame)) 1))
         (pos (om-make-point (+ 5 (* (+ 1 i) 50)) 45))
         (new-input (make-new-typed-input
                     (unique-name-from-list-new "input" (get-elements (object panel))
                                                :mode :num :space nil)
                     thetype (+ i 1) pos)))
    (when value (setf (defval new-input) (clone value)))
    (omG-add-element panel
                     (oa::with-zoom-context-of panel
                       (make-frame-from-callobj new-input)))
    t))

(defun make-new-genfunwin (self name iconid doc &optional (package *package-user*))
  (declare (ignore self))
  (let* ((new-win (make-editor-window 'methodEditor
                                      (omNG-make-new-patch "genfunpatch") name nil
                                      :winsize (om-make-point 500 400)
                                      :winpos (om-make-point 50 38)
                                      :winshow nil))
         (thescroll (panel new-win))
         (editor (editor new-win)))
    (setf (pack editor) package)
    (setf (iconid editor) iconid)
    (setf (docu thescroll) doc)
    (setf (win-mod editor) :abs)
    (setf (name thescroll) name)
    (om-select-window new-win)
    ;; (newgenfun-add-window-buttons thescroll)  ; superseded by methodEditor zoom bar
    (set-field-size thescroll)))

(defmethod make-new-method ((self OMgenericFunction))
  (let* ((outputs (numouts self))
         (lambda-lis (arglist self))
         (ind 0) intype thescroll
         (new-win (om-make-window 'EditorWindow
                                  :window-title (string+ (name self) " (new method)")
                                  :position (om-make-point 50 38)
                                  :close t
                                  :window-show nil
                                  :size (om-make-point 400 330)))
         (editor (om-make-view 'MethodEditor
                               :ref nil
                               :owner new-win
                               :object (omNG-make-new-patch "genfunpatch")
                               :position (om-make-point 0 0)
                               :size (om-make-point 400 330)))
         (pb (om-make-dialog-item 'om-radio-button (om-make-point 40 8) (om-make-point 76 16) "primary" :checked-p t))
         (bb (om-make-dialog-item 'om-radio-button (om-make-point 120 8) (om-make-point 72 16) "before"))
         (ab (om-make-dialog-item 'om-radio-button (om-make-point 200 8) (om-make-point 72 16) "after"))
         (arb (om-make-dialog-item 'om-radio-button (om-make-point 280 8) (om-make-point 76 16) "around")))
    (setf (editor new-win) editor)
    (setf thescroll (panel new-win))
    (setf (object thescroll) (omNG-make-new-patch "genfunpatch"))
    (setf (name thescroll) (name self))
    (setf (win-mod editor) :new)
    (om-add-subviews thescroll pb bb ab arb)
    (setf (quali-buttons editor) (list pb bb ab arb))
    (loop for item in lambda-lis do
          (if (not (member item lambda-list-keywords :test 'equal))
              (let* ((inbox (make-new-typed-input (string-downcase (string item))
                                                  't ind (om-make-point (+ 5 (* ind 50)) 45))))
                (setf (enable inbox) nil)
                (setf (keys inbox) intype)
                (omG-add-element thescroll
                                 (oa::with-zoom-context-of thescroll
                                   (make-frame-from-callobj inbox)))
                (incf ind))
              (setf intype item)))
    (loop for i from 0 to (- outputs 1) do
          (omG-add-element thescroll
                           (oa::with-zoom-context-of thescroll
                             (make-frame-from-callobj
                              (make-new-output (mk-unique-name thescroll "output")
                                               i (om-make-point (+ 5 (* i 50)) 240))))))
    (set-field-size thescroll)
    (om-select-window new-win)))

;;; ============================================================
;;; ttybox: zoom-aware open + edit overlay; gesture-driven cancel;
;;; box materialization fork; logical-coordinate forwarding to
;;; add-box-in-patch-panel from each text-input variant.
;;; ============================================================

(defmethod open-ttybox ((self ttybox))
  (let* ((thetext (if (text-enter-multiline-p self)
                      (initial-text-ttybox self)
                      (remove #\Newline (initial-text-ttybox self))))
         (panel (om-view-container (om-view-container self)))
         (container (editor panel))
         (zoom (if (typep panel 'om-scroller) (oa::om-zoom-of panel) 1.0))
         (font (if (= zoom 1.0) *om-default-font1*
                   (oa::om-zoom-scale-font *om-default-font1* zoom))))
    (when (text-view container)
      (om-remove-subviews panel (text-view container))
      (setf (text-view container) nil))
    (setf (text-view container)
          (om-make-dialog-item (open-ttybox-class self)
                               (om-add-points (om-subtract-points
                                               (om-view-position self)
                                               #-win32 (om-make-point 2 2)
                                               #+win32 (om-make-point 0 0))
                                              (om-view-position (om-view-container self)))
                               (om-view-size self)
                               thetext
                               :allow-returns (text-enter-multiline-p self)
                               :focus t
                               :object self
                               :container panel
                               :font font
                               :in-place-completion-function
                               #'(lambda (item str)
                                   (declare (ignore item))
                                   (or (funcall 'box-name-completion str)
                                       (progn (capi::beep-pane) :destroy)))
                               :completion 'box-name-completion
                               :complete-do-action t
                               #+linux :bg-color #+linux *om-lgray-color*))
    (capi:redisplay-element container)))

(defun cancel-ttybox-edit (self)
  "ESC handler for ttybox text-input: discard the in-flight box and abort."
  (when (and self (om-view-container self))
    (let* ((box-frame (om-view-container (object self)))
           (scroller (and box-frame (om-view-container box-frame)))
           (theeditor (and box-frame (editor box-frame))))
      (when (and scroller box-frame)
        (omG-remove-element scroller box-frame))
      (when theeditor
        (setf (text-view theeditor) nil)
        (om-remove-subviews (panel theeditor) self)
        (om-invalidate-view (panel theeditor)))))
  (om-abort))

(defclass change-text-enter-view (edit-text-enter-view) ()
  (:default-initargs
   :gesture-callbacks `((#\Escape . cancel-ttybox-edit)
                        (#\Tab    . ,oa::om-text-input-tab-complete))))

(defun box-would-materialize-p (newval)
  "T iff editing a ttybox to NEWVAL should drop the ttybox and delegate to
   ADD-BOX-IN-PATCH-PANEL. NIL for literals/undef symbols, which stay as
   ttybox (vanilla SETF VALUE path)."
  (cond
    ((or (listp newval) (numberp newval) (stringp newval)) nil)
    ((not (symbolp newval)) nil)
    ((special-form-p newval) t)
    ((member newval '(patch lisp maquette comment)) t)
    ((member newval *spec-new-boxes-types*) t)
    ((and (find-class newval nil)
          (not (equal newval 'list))
          (omclass-p (class-of (find-class newval nil)))) t)
    ((fboundp newval) t)
    (t nil)))

(defmethod exit-from-dialog ((self change-text-enter-view) newtext)
  (handler-bind ((error #'(lambda (c) (declare (ignore c))
                            (when (om-view-container self)
                              (setf (text-view (editor (om-view-container self))) nil)
                              (om-remove-subviews (panel (editor (om-view-container self))) self))
                            (om-beep)
                            (om-abort))))
    (let* ((*package* (find-package :om))
           (newval (read-from-string newtext))
           (box-frame (om-view-container (object self)))
           (box (object box-frame))
           (scroller (om-view-container box-frame))
           (theeditor (editor box-frame)))
      (cond
        ((box-would-materialize-p newval)
         (let* ((zoom (if (typep scroller 'om-scroller) (oa::om-zoom-of scroller) 1.0))
                (pos (om-view-position box-frame))
                (logical-pos (if (= zoom 1.0) pos (oa::om-zoom-unscale-point pos zoom))))
           (unwind-protect
                (add-box-in-patch-panel newtext scroller logical-pos)
             (real-make-delete-before scroller (list box-frame))
             (omG-remove-element scroller box-frame)
             (setf (text-view theeditor) nil)
             (om-remove-subviews (panel theeditor) self)
             (om-invalidate-view (panel theeditor)))))
        (t
         (om-set-dialog-item-text (object self) newtext)
         (setf (value box) newval)
         (setf (thestring box) newtext)
         (reinit-size box-frame)
         (setf (text-view theeditor) nil)
         (om-remove-subviews (panel theeditor) self))))))

(defmethod exit-from-dialog ((self apply-text-enter-view) str)
  (handler-bind ((error #'(lambda (err)
                            (when (om-view-container (object self))
                              (setf (text-view (editor (om-view-container (object self)))) nil)
                              (om-remove-subviews (object self) self))
                            (om-beep)
                            (print (format nil "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err)))
                            (om-abort))))
    (let* ((box (om-view-container (object self)))
           (scroller (om-view-container box))
           (theeditor (editor (om-view-container box)))
           (zoom (if (typep scroller 'om-scroller) (oa::om-zoom-of scroller) 1.0))
           (pos (let ((vp (om-view-position box)))
                  (if (= zoom 1.0) vp (oa::om-zoom-unscale-point vp zoom)))))
      (unwind-protect
           (add-box-in-patch-panel str scroller pos)
        (omG-remove-element scroller box)
        (setf (text-view theeditor) nil)
        (om-remove-subviews (panel theeditor) self)
        (om-close-window *tty-window*)
        (setf *tty-window* nil)
        (om-invalidate-view (panel theeditor))))))

(defclass new-fun-enter-view (edit-text-enter-view) ()
  (:default-initargs
   :gesture-callbacks `((#\Escape . cancel-ttybox-edit)
                        (#\Tab    . ,oa::om-text-input-tab-complete))))

(defmethod exit-from-dialog ((self new-fun-enter-view) str)
  (handler-bind ((error #'(lambda (err)
                            (when (om-view-container self)
                              (setf (text-view (editor (om-view-container self))) nil)
                              (om-remove-subviews (panel (editor (om-view-container self))) self))
                            (om-beep)
                            (print (format nil "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err)))
                            (om-abort))))
    (let* ((box (om-view-container (object self)))
           (pos (om-view-position box))
           (scroller (om-view-container box))
           (theeditor (editor (om-view-container box)))
           (zoom (if (typep scroller 'om-scroller) (oa::om-zoom-of scroller) 1.0))
           (logical-pos (if (= zoom 1.0) pos (oa::om-zoom-unscale-point pos zoom))))
      (unwind-protect
           (add-box-in-patch-panel str scroller logical-pos)
        (omG-remove-element scroller box)
        (setf (text-view theeditor) nil)
        (om-remove-subviews (panel theeditor) self)
        (om-invalidate-view (panel theeditor))))))

(defun add-box-in-patch-panel (str scroller pos)
  (let ((*package* (find-package :om))
        (funname (read-from-string str))
        (args (decode-input-arguments str))
        (text (cadr (multiple-value-list (string-until-char str " "))))
        newbox)
    (cond
      ((or (listp funname) (numberp funname) (stringp funname))
       (setf newbox (omNG-make-new-boxcall (get-basic-type 'list) pos (mk-unique-name scroller "list")))
       (setf (value newbox) funname)
       (setf (thestring newbox) str)
       (setf (frame-size newbox) nil))
      ((special-form-p funname)
       (om-beep-msg (string+ "Special Lisp form " str)))
      ((equal funname 'patch)
       (setf newbox (omNG-make-new-boxcall
                     (make-instance 'OMPatchAbs
                                    :name (mk-unique-name scroller "mypatch") :icon 210)
                     pos
                     (mk-unique-name scroller "mypatch"))))
      ((equal funname 'lisp)
       (setf newbox (omNG-make-new-boxcall
                     (make-instance 'OMLispPatchAbs
                                    :name (mk-unique-name scroller "lispfunction")
                                    :icon 123)
                     pos
                     (mk-unique-name scroller "lispfunction"))))
      ((member funname *spec-new-boxes-types*)
       (setf newbox (get-new-box-from-type funname pos scroller)))
      ((equal funname 'maquette)
       (setf newbox (omNG-make-new-boxcall
                     (make-instance 'OMMaqAbs
                                    :name (mk-unique-name scroller "mymaquette") :icon 265)
                     pos
                     (mk-unique-name scroller "mymaquette"))))
      ((equal funname 'comment)
       (setf newbox (omNG-make-new-boxcall funname pos "comment"))
       (setf (reference newbox) (or text "Type your comments here")))
      ((and (find-class funname nil) (not (equal funname 'list))
            (omclass-p (class-of (find-class funname nil))))
       (cond ((om-shift-key-p)
              (setf newbox (omNG-make-new-boxcall-slots (find-class funname nil) pos (mk-unique-name scroller "slots"))))
             (t (let ((boxname (or text (mk-unique-name scroller (string funname)))))
                  (setf newbox (omNG-make-new-boxcall (find-class funname) pos boxname))
                  (if text (setf (show-name newbox) t))))))
      ((not (fboundp funname))
       (cond ((equal funname '??)
              (om-beep))
             (t
              (setf newbox (omNG-make-new-boxcall (get-basic-type 'list) pos (mk-unique-name scroller "list")))
              (setf (value newbox) funname)
              (setf (thestring newbox) str)
              (setf (frame-size newbox) nil))))
      ((OMGenfun-p (fdefinition funname))
       (setf newbox (omNG-make-new-boxcall (fdefinition funname) pos
                                           (mk-unique-name scroller (string funname))))
       (when args (add-args-to-box newbox args)))
      (t (setf newbox (omNG-make-new-lispboxcall funname pos
                                                 (mk-unique-name scroller (string funname))))
         (when args (add-args-to-box newbox args))))
    (when (and newbox (box-allowed-p newbox scroller))
      (when (and (allow-rename newbox) (car args))
        (set-patch-box-name newbox text))
      (omG-add-element scroller
                       (let ((oa::*make-frame-zoom-context*
                              (and (typep scroller 'om-scroller) (oa::om-zoom-of scroller))))
                         (make-frame-from-callobj newbox))))
    (when (equal funname 'comment)
      (reinit-size (car (frames newbox))))
    (when *auto-create-connect*
      (let ((input (car (om-subviews (car (frames newbox))))))
        (if (typep input 'input-funboxframe)
            (progn
              (connect-box *auto-create-connect* input)
              (select-connection (car (connections (om-view-container input))))
              (capi::set-pane-focus scroller))
            (progn
              (add-one-input (car (frames newbox)))
              (connect-box *auto-create-connect*
                           (car (om-subviews (car (frames newbox)))))))
        (setf *auto-create-connect* nil)))
    newbox))
