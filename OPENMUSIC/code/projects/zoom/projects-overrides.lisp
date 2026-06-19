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
;;; bpf: draw-obj-in-rect + draw-bpf-in-rect + draw-mini-view
;;; ============================================================

(defmethod draw-obj-in-rect ((self bpf) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom (oa::om-zoom-effective view))
         (pen  (max 1 (round zoom)))
         (oa::*om-zoom-mini-helper-scale* zoom))
    (om-with-focused-view view
      (om-with-line-size pen
        (om-with-fg-color view (bpfcolor self)
          (draw-bpf-in-rect self x x1 y y1 (give-bpf-range self)))))))

(defmethod draw-mini-view ((self t) (value bpf))
  (let* ((zoom   (oa::om-zoom-effective self))
         (margin (max 1 (round (* 3 zoom)))))
    (draw-obj-in-rect value margin (- (w self) margin) margin (- (h self) margin)
                      (give-bpf-range value) self)))

(defun draw-bpf-in-rect (bpf x x1 y y1 ranges)
  (let* ((pixels (- x1 x))
         (points (length (point-list bpf)))
         (draw-points (< points 50))
         (s    (or oa::*om-zoom-mini-helper-scale* 1.0))
         (mark (max 1 (round (* 3 s))))
         (off  (max 0 (round s))))
    (if (> points (* 4 pixels))
        (min-max-draw-bpf (point-list bpf) (- x1 x) (- y1 y) ranges x y)
        (loop for point-list on (point-list bpf) do
              (let* ((thepoint (car point-list))
                     (pix-point (point-to-pixel-with-sizes ranges thepoint (- x1 x) (- y1 y)))
                     (points-prev-next (give-prev+next-x bpf thepoint))
                     (prev-point (first points-prev-next))
                     (next-point (second points-prev-next)))
                (when prev-point
                  (let ((prev-pixel (point-to-pixel-with-sizes ranges prev-point (- x1 x) (- y1 y))))
                    (om-draw-line (+ x (om-point-h prev-pixel)) (+ y (om-point-v prev-pixel))
                                  (+ x (om-point-h pix-point)) (+ y (om-point-v pix-point)))))
                (when (or (and next-point (null (second point-list)))
                          (and (second point-list) next-point
                               (not (equal next-point (second point-list)))))
                  (let ((next-pixel (point-to-pixel-with-sizes ranges next-point (- x1 x) (- y1 y))))
                    (om-draw-line (+ x (om-point-h pix-point)) (+ y (om-point-v pix-point))
                                  (+ x (om-point-h next-pixel)) (+ y (om-point-v next-pixel)))))
                (when draw-points
                  (om-fill-rect (+ x (- (om-point-h pix-point) off))
                                (+ y (- (om-point-v pix-point) off))
                                mark mark)))))))

(defmethod draw-obj-in-rect ((self bpf-lib) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom     (oa::om-zoom-effective view))
         (pen      (max 1 (round zoom)))
         (bpf-list (bpf-list self))
         (ranges   (get-miniview-bpf-range bpf-list))
         (oa::*om-zoom-mini-helper-scale* zoom))
    (om-with-line-size pen
      (loop for bpf in bpf-list do
            (om-with-fg-color view (bpfcolor bpf)
              (draw-bpf-in-rect bpf x x1 y y1 ranges))))))

;;; ============================================================
;;; bpc: draw-obj-in-rect + draw-bpc-in-rect + bpc-lib
;;; ============================================================

(defmethod draw-obj-in-rect ((self bpc) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom (oa::om-zoom-effective view))
         (pen  (max 1 (round zoom)))
         (oa::*om-zoom-mini-helper-scale* zoom))
    (om-with-focused-view view
      (om-with-line-size pen
        (om-with-fg-color view (bpfcolor self)
          (draw-bpc-in-rect self x x1 y y1 (give-bpf-range self)))))))

(defun draw-bpc-in-rect (bpf x x1 y y1 ranges)
  (let* ((pl (length (point-list bpf)))
         (draw-points (< pl 50))
         (s    (or oa::*om-zoom-mini-helper-scale* 1.0))
         (mark (max 1 (round (* 3 s))))
         (off  (max 0 (round s))))
    (if (= 1 pl)
        (let* ((pix-point (point-to-pixel-with-sizes ranges (car (point-list bpf)) (- x1 x) (- y1 y))))
          (om-fill-rect (+ x (- (om-point-h pix-point) off))
                        (+ y (- (om-point-v pix-point) off)) mark mark))
        (loop for thepoint in (point-list bpf)
              for i = 0 then (+ i 1) do
              (let* ((pix-point (point-to-pixel-with-sizes ranges thepoint (- x1 x) (- y1 y)))
                     (next-point (nth (+ i 1) (point-list bpf))))
                (when next-point
                  (let ((next-pixel (point-to-pixel-with-sizes ranges next-point (- x1 x) (- y1 y))))
                    (om-draw-line (+ x (om-point-h pix-point)) (+ y (om-point-v pix-point))
                                  (+ x (om-point-h next-pixel)) (+ y (om-point-v next-pixel)))))
                (when draw-points
                  (om-fill-rect (+ x (- (om-point-h pix-point) off))
                                (+ y (- (om-point-v pix-point) off)) mark mark)))))))

(defmethod draw-obj-in-rect ((self bpc-lib) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom     (oa::om-zoom-effective view))
         (pen      (max 1 (round zoom)))
         (bpf-list (bpf-list self))
         (ranges   (get-miniview-bpf-range bpf-list))
         (oa::*om-zoom-mini-helper-scale* zoom))
    (om-with-focused-view view
      (om-with-line-size pen
        (loop for bpf in bpf-list do
              (om-with-fg-color view (bpfcolor bpf)
                (draw-bpc-in-rect bpf x x1 y y1 ranges)))))))

;;; ============================================================
;;; class-array: zoom-aware row draw
;;; ============================================================

(defmethod draw-obj-in-rect ((self class-array) x x1 y y1 edparams view)
  (let* ((zoom     (oa::om-zoom-effective view))
         (pen      (max 1 (round zoom)))
         (visibles (get-draw-visibles-list self edparams))
         (numvis   (length visibles))
         (oa::*om-zoom-mini-helper-scale* zoom))
    (unless (or (<= numvis 0) (<= (numcols self) 0))
      (let ((deltay (max 8 (floor (- y1 y) numvis)))
            (width (- x1 x))
            (jumpx 1))
        (loop while (< (/ width (/ (numcols self) jumpx)) 12) do (incf jumpx))
        (let* ((nbobjsinx (max 1 (round (numcols self) jumpx)))
               (deltax (ceiling width nbobjsinx)))
          (om-with-focused-view view
            (om-with-line-size pen
              (loop for i in visibles
                    for posy = y then (+ posy deltay) do
                    (let* ((row (get-array-row self i))
                           (bpf? (get-row-bpf self row))
                           (color (nth i (get-param edparams 'color-list))))
                      (if bpf?
                          (progn
                            (setf (bpfcolor bpf?) color)
                            (draw-obj-in-rect bpf? x x1 posy (+ posy deltay) nil view))
                          (om-with-fg-color view (or color *om-black-color*)
                            (when (consp row)
                              (loop for di = 0 then (+ di 1)
                                    for posx = (+ x (* di deltax))
                                    while (< posx x1) do
                                    (om-with-clip-rect view (om-make-rect (min posx x) (min posy y)
                                                                          (min (+ posx deltax) x1)
                                                                          (min (+ posy deltay) y1))
                                      (draw-obj-in-rect (nth (* di jumpx) row) posx (+ posx deltax)
                                                        posy (+ posy deltay)
                                                        (default-edition-params (nth (* di jumpx) row)) view))
                                    (om-draw-line posx posy posx (+ posy deltay))))))
                      (om-draw-line x (+ posy deltay) x1 (+ posy deltay)))))))))))

;;; ============================================================
;;; n-cercle: zoom-aware draw + font scaling
;;; ============================================================

(defmethod draw-obj-in-rect ((self n-cercle) x x1 y y1 edparams view)
  ;; ZOOM-SCALE: pen and rebound fonts scale with view zoom.
  (let* ((zoom    (oa::om-zoom-effective view))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (pen     (if scale-p (max 1 (round zoom)) 1))
         (*om-default-font1*  (if scale-p (oa::om-zoom-scale-font *om-default-font1*  zoom) *om-default-font1*))
         (*om-default-font1b* (if scale-p (oa::om-zoom-scale-font *om-default-font1b* zoom) *om-default-font1b*)))
    (if scale-p
        (om-with-line-size pen
          (draw-cercle self view (+ x (round (- x1 x) 2)) (+ y (round (- y1 y) 2))
                       (round (min (- x1 x) (- y1 y)) 2.2) 3 4 t 0))
        (draw-cercle self view (+ x (round (- x1 x) 2)) (+ y (round (- y1 y) 2))
                     (round (min (- x1 x) (- y1 y)) 2.2) 3 4 t 0))))

;;; ============================================================
;;; sound: zoom-aware draw + font/labels scale
;;; ============================================================

(defmethod draw-obj-in-rect ((self sound) x x1 y y1 edparams view)
  (let* ((picture (if (and (pict-spectre self) (get-param edparams :show-spectrum))
                      (thepict (pict-spectre self))
                      (sound-get-pict self)))
         (zoom        (oa::om-zoom-effective view))
         (scaled-font (if (= zoom 1.0) *om-default-font1* (oa::om-zoom-scale-font *om-default-font1* zoom)))
         (xm  (max 1 (round (* 5  zoom))))
         (y1l (max 1 (round (* 14 zoom))))
         (y2l (max 1 (round (* 28 zoom)))))
    (om-with-focused-view view
      (if picture
          (let ((dur (/ (om-sound-n-samples self) (om-sound-sample-rate self)))
                (pos x) (w (- x1 x)))
            (om-with-fg-color view *om-dark-gray-color*
              (om-draw-picture view picture :pos (om-make-point x y) :size (om-make-point (- x1 x) (- y1 y))))
            (om-with-fg-color view *om-steel-blue-color*
              (om-with-line '(2 2)
                (loop for item in (markers self) do
                      (setf pos (+ x (round (* w item) dur)))
                      (om-draw-line pos y pos y1)))))
          (if (om-sound-n-channels self)
              (om-with-font scaled-font
                (om-draw-string xm y1l (format nil "~A" (file-namestring (filename self))))
                (om-draw-string xm y2l (format nil "~A channels" (om-sound-n-channels self))))
              (let ((path (om-sound-file-name self)))
                (when path
                  (om-with-font scaled-font
                    (om-draw-string xm y1l (string+ (pathname-name path)
                                                    (if (stringp (pathname-type path))
                                                        (string+ "." (pathname-type path)) "")
                                                    ":"))
                    (om-draw-string xm y2l (if (probe-file path) (om-str :file-error)
                                               (string+ (format nil (om-str :file-not-found) (pathname-name path)) "..."))))))))) ))

;;; ============================================================
;;; MidiControl: zoom-aware draw + font scale
;;; ============================================================

(defmethod draw-obj-in-rect ((self MidiControl) x x1 y y1 edparams view)
  ;; ZOOM-SCALE: font and margins scale with view zoom.
  (let* ((zoom        (oa::om-zoom-effective view))
         (scaled-font (if (= zoom 1.0) *om-default-font1* (oa::om-zoom-scale-font *om-default-font1* zoom)))
         (xm (max 1 (round (* 10 zoom))))
         (ym (max 1 (round (* 10 zoom))))
         (tmpBPF (create-bpf-with-initpt self)))
    (om-with-focused-view view
      (om-with-font scaled-font
        (if (stringp (ctrltype self)) (om-draw-string xm ym (string (ctrltype self))))
        (om-draw-rect 0 0 (- (w view) 1) (- (h view) 1))
        (draw-obj-in-rect tmpBPF x x1 y y1 (give-bpf-range tmpBPF) view)))))

;;; ============================================================
;;; musminiboxes: score miniview font scales with ancestor zoom
;;; ============================================================

(defmethod score-draw-mini-view ((self t) value)
  (let* ((zoom (oa::om-zoom-find-ancestor-zoom self))
         (*miniview-font-size* (if (and (numberp zoom) (/= zoom 1.0))
                                   (max 1 (round (* *miniview-font-size* zoom)))
                                   *miniview-font-size*)))
    (if (minipict self)
        (let ((x0 (initx self)) (y0 (inity self))
              (pictsize (om-get-picture-size (minipict self))))
          (om-draw-picture self (minipict self) :pos (om-make-point x0 y0) :size pictsize))
        (om-with-focused-view self
          (draw-mini-obj value self (mv-font-size value) (mv-view-size value self))))))

(defmethod score-update-miniview ((self t) value)
  (when (minipict self) (om-kill-picture (minipict self)) (setf (minipict self) nil))
  (if (and (not (equal (type-of value) 'midifile))
           (not (equal (type-of value) 'eventmidi-seq)))
      (let* ((zoom (oa::om-zoom-find-ancestor-zoom self))
             (*miniview-font-size* (if (and (numberp zoom) (/= zoom 1.0))
                                       (max 1 (round (* *miniview-font-size* zoom)))
                                       *miniview-font-size*)))
        (setf (minipict self) (cons-mini-pict value self (mv-font-size value) (mv-view-size value self)))))
  (om-invalidate-view self t))

(defmethod score-update-miniview ((self tempobjframe) value)
  (when (minipict self) (om-kill-picture (minipict self)) (setf (minipict self) nil))
  (if (and (not (equal (type-of value) 'midifile))
           (not (equal (type-of value) 'eventmidi-seq)))
      (let* ((zoom (oa::om-zoom-of self))
             (fs   (max 1 (round (* (mv-font-size value) zoom)))))
        (setf (minipict self) (cons-mini-pict value self fs (mv-view-size value self)))))
  (om-invalidate-view self t))

(defmethod score-draw-obj-in-rect (self x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom (oa::om-zoom-find-ancestor-zoom view))
         (*miniview-font-size* (if (and (numberp zoom) (/= zoom 1.0))
                                   (max 1 (round (* *miniview-font-size* zoom)))
                                   *miniview-font-size*))
         (size (om-make-point (- x1 x) (- y1 y)))
         (thepict (cons-mini-pict self view (mv-font-size view) size)))
    (om-draw-picture view thepict :pos (om-make-point x y) :size size)
    (om-kill-picture thepict) t))

(defmethod update-miniview ((self t) (value multi-seq))
  (when (minipict self) (om-kill-picture (minipict self)) (setf (minipict self) nil))
  (set-mini-param self 'staff (correct-staff-val value (get-mini-param self 'staff) self))
  (let* ((zoom (oa::om-zoom-find-ancestor-zoom self))
         (*miniview-font-size* (if (and (numberp zoom) (/= zoom 1.0))
                                   (max 1 (round (* *miniview-font-size* zoom)))
                                   *miniview-font-size*)))
    (setf (minipict self) (cons-mini-pict value self (mv-font-size value) (mv-view-size value self))))
  (om-invalidate-view self t))

;;; ============================================================
;;; contextmenu: scoreeditor menu unscales click pos + ctx
;;; ============================================================

(defmethod scorepatch-menu-items ((self scoreeditor))
  (let* ((vis-posi (om-mouse-position self))
         (panel    (panel self))
         (zoom     (if panel (oa::om-zoom-of panel) 1.0))
         (log-posi (if (= zoom 1.0) vis-posi (oa::om-zoom-unscale-point vis-posi zoom))))
    (list
     (om-new-leafmenu "Comment"
                      #'(lambda ()
                          (let ((newbox (omNG-make-new-boxcall 'comment log-posi "comment")))
                            (when newbox
                              (let ((p (panel self)))
                                (omG-add-element p
                                                 (oa::with-zoom-context-of p
                                                   (make-frame-from-callobj newbox))))))))
     (list
      (om-package-fun2menu *om-package-tree* nil #'(lambda (f) (add-box-from-menu f vis-posi)))
      (om-package-classes2menu *om-package-tree* nil #'(lambda (c) (add-box-from-menu c vis-posi))))
     (list
      (om-new-menu "Internal..."
                   (om-new-leafmenu "Patch"
                                    #'(lambda ()
                                        (omG-add-element panel
                                                         (oa::with-zoom-context-of panel
                                                           (make-frame-from-callobj
                                                            (omNG-make-new-boxcall
                                                             (make-instance 'OMPatchAbs :name "mypatch" :icon 210)
                                                             log-posi (mk-unique-name panel "mypatch")))))))
                   (om-new-leafmenu "Maquette"
                                    #'(lambda ()
                                        (omG-add-element panel
                                                         (oa::with-zoom-context-of panel
                                                           (make-frame-from-callobj
                                                            (omNG-make-new-boxcall
                                                             (make-instance 'OMMaqAbs :name "mymaquette" :icon 265)
                                                             log-posi (mk-unique-name panel "mymaquette")))))))
                   (om-new-leafmenu "Loop" #'(lambda () (add-box-from-menu (fdefinition 'omloop) vis-posi)))
                   (om-new-leafmenu "Lisp Function"
                                    #'(lambda ()
                                        (omG-add-element panel
                                                         (oa::with-zoom-context-of panel
                                                           (make-frame-from-callobj
                                                            (omNG-make-new-boxcall
                                                             (make-instance 'OMLispPatchAbs :name "lispfunction" :icon 123)
                                                             log-posi (mk-unique-name panel "lispfunction"))))))))))))

;;; ============================================================
;;; scorepatch: score-box make-frame zoom-aware + ctx propagation
;;; ============================================================

(defmethod make-frame-from-callobj ((self score-box))
  (let* ((zoom    (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (logical-pos  (frame-position self))
         (logical-size (frame-size self))
         (module-vpos  (if (and scale-p logical-pos)
                           (oa::om-zoom-scale-point logical-pos zoom)
                           logical-pos))
         (module-vsize (if (and scale-p logical-size)
                           (oa::om-zoom-scale-point logical-size zoom)
                           logical-size))
         (module (om-make-view (get-frame-class self)
                               :position module-vpos
                               :help-spec (get-documentation self)
                               :size module-vsize
                               :object self))
         (io-size (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8))
         (off-x   (if scale-p (oa::om-zoom-scale-int 4 zoom) 4))
         (off-y10 (if scale-p (oa::om-zoom-scale-int 10 zoom) 10))
         (off-y2  (if scale-p (oa::om-zoom-scale-int 2 zoom) 2))
         thenewout)
    (setf thenewout (om-make-view (get-out-class self)
                                  :position
                                  #+(or linux win32)
                                  (om-make-point (- (round (om-point-h module-vsize) 2) off-x)
                                                 (+ (h module) off-y10))
                                  #+(or macosx darwin darwin-target)
                                  (om-make-point (- (round (om-point-h module-vsize) 2) off-x)
                                                 (+ (h module) off-y2))
                                  :size (om-make-point io-size io-size)
                                  :help-spec "option-click to evalue or drag for connections"
                                  :index 0))
    (push thenewout (outframes module))
    (om-add-subviews module thenewout)
    (setf (name module) (frame-name self))
    (setf (frames self) (list module))
    module))

;;; ============================================================
;;; picture: bgk2pict ctx propagation
;;; ============================================================

(defmethod bgk2pict ((self patch-picture) patch)
  (let ((picture (copy-picture self 'picture))
        newcall)
    (setf newcall (omNG-make-new-boxcall (find-class 'picture)
                                         (om-subtract-points (pict-pos self) (om-make-point 0 8))
                                         (mk-unique-name patch "pict")))
    (setf (value newcall) picture)
    (setf (frame-size newcall) (om-add-points (pict-size self) (om-make-point 0 16)))
    (setf (showpict newcall) t)
    (omG-add-element patch
                     (oa::with-zoom-context-of patch
                       (make-frame-from-callobj newcall)))
    (setf (pictu-list (object patch)) (remove self (pictu-list (object patch)) :test 'equal))
    (om-invalidate-view patch t)))

;;; ============================================================
;;; fluid-ibox: FLDIntbox frame + lock + centre + update-di-size
;;; ============================================================

(defmethod make-outputs-from-names ((self FLDIntbox) value module)
  (let* ((numouts  (numouts self))
         (nameouts (get-outs-name value))
         (zoom    (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (io-size (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8))
         (off-x   (if scale-p (oa::om-zoom-scale-int 4 zoom) 4))
         (off-y   (if scale-p (oa::om-zoom-scale-int 9 zoom) 9)))
    (loop for i from 0 to (- numouts 1) do
          (let ((thenewout (om-make-view (get-out-class self)
                                         :position (om-make-point
                                                    (- (* (+ i 1) (round (w module) (+ numouts 1))) off-x)
                                                    (- (h module) off-y))
                                         :size (om-make-point io-size io-size)
                                         :help-spec (nth i nameouts)
                                         :index i)))
            (push thenewout (outframes module))
            (om-add-subviews module thenewout)))))

(defmethod make-frame-from-callobj ((self FLDIntbox))
  "Make a simple frame for the editor factory 'self'."
  (let ((name (string-downcase (name self)))
        (defsize (get-boxsize self))
        (numouts (numouts self))
        (numins (length (inputs self)))
        (index 0)
        (module (om-make-view (get-frame-class self)
                              :name (name (value self))
                              :position (frame-position self)
                              :object self))
        title)
    (unless (frame-size self)
      (setf (frame-size self) (om-make-point
                               (apply #'max (list (om-point-h defsize) (* 8 numouts) (* 8 numins)))
                               (om-point-v defsize))))
    (setf (inputframes module)
          (mapcar #'(lambda (input)
                      (setf index (+ index 1))
                      (om-make-view (get-input-class-frame self)
                                    :object input
                                    :help-spec (string+ "<" (string-downcase (name input))
                                                        "> " (doc-string input))
                                    :size (om-make-point 8 8)
                                    :position (om-make-point
                                               (- (* index (round (om-point-h (frame-size self)) (+ numins 1))) 4)
                                               1)))
                  (inputs self)))
    (setf title (om-make-dialog-item 'om-static-text (om-make-point 5 8) (om-make-point 90 24) (name module)
                                     :font *controls-font*))
    (loop for input-f in (inputframes module) do (om-add-subviews module input-f))
    (make-outputs-from-names self (value self) module)
    (setf (iconview module) (value self))
    (om-add-subviews module (iconview module) title)
    (setf (frames self) (list module))
    (setf (name module) name)
    (add-box-resize module)
    (om-set-view-size module (frame-size self))
    (when (frame-position self)
      (om-set-view-position module (frame-position self)))
    (update-di-size (value self) module)
    (setf (oa::om-zoom-logical-size module) (frame-size self))
    (when (frame-position self)
      (setf (oa::om-zoom-logical-pos module) (frame-position self)))
    (when (and (value self)
               (typep (value self) 'oa::om-standard-dialog-item)
               (not (oa::om-zoom-logical-font (value self))))
      (setf (oa::om-zoom-logical-font (value self))
            (or (om-get-font (value self)) *controls-font*)))
    (when (allow-lock self)
      (add-lock-button module (allow-lock self)))
    module))

(defmethod oa::om-zoom-resolve-touch-target ((pane FIEditorframe) x y)
  "CAPI delivers (:touch :zoom) to this nested pinboard-layout; redirect the
   zoom to the parent patch panel with the anchor in the scroller's frame.
   Win32 anchor expects viewport coord; Mac/Linux expects pinboard."
  (let ((scroller (panel pane)))
    (when (typep scroller 'oa::om-scroller)
      (let* ((px (+ (oa::vx pane) x))
             (py (+ (oa::vy pane) y)))
        (values scroller
                #+win32 (- px (oa::tracked-scroll-x scroller))
                #-win32 px
                #+win32 (- py (oa::tracked-scroll-y scroller))
                #-win32 py)))))

(defmethod add-lock-button ((self FIEditorframe) &optional (mode "x"))
  "Add a lock button, if the box referenced by 'self' allow it."
  (when (allow-lock-button (object self))
    (setf (lock-button self) (make-lock-button self mode))
    (let* ((zoom (or oa::*make-frame-zoom-context* (oa::om-zoom-effective self) 1.0))
           (scale-p (and (numberp zoom) (/= zoom 1.0)))
           (off-y   (if scale-p (oa::om-zoom-scale-int 8 zoom) 8)))
      (om-set-view-position (lock-button self) (om-make-point 0 off-y)))
    (om-add-subviews self (lock-button self))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) mode)))

(defmethod centre-icon ((self FIEditorframe))
  (let* ((zoom   (oa::om-zoom-effective self))
         (widget (iconview self)))
    (when widget (update-di-size widget self))
    (when widget
      (let ((logical-font (oa::om-zoom-logical-font widget)))
        (when logical-font
          (om-set-font widget
                       (if (= zoom 1.0)
                           logical-font
                           (oa::om-zoom-scale-font logical-font zoom))))))))

(defmethod update-di-size ((self fluid-i-box) container)
  (let ((zoom (oa::om-zoom-effective container)))
    (om-set-view-position self
                          (om-make-point (round (* 10 zoom)) (round (* 18 zoom))))
    (om-set-view-size self
                      (om-make-point (- (om-width container) (round (* 20 zoom)))
                                     (max (round (* 20 zoom))
                                          (- (om-height container) (round (* 36 zoom))))))
    (let ((logical-font (oa::om-zoom-logical-font self)))
      (when logical-font
        (om-set-font self (if (= zoom 1.0)
                              logical-font
                              (oa::om-zoom-scale-font logical-font zoom)))))))

;;; ============================================================
;;; textfile: w-zoom slot + persistence + zoom-aware draw
;;; (Editor-window zoom capture/apply deferred — requires api helpers not
;;; yet ported: pane-current-zoom-ratio, set-pane-zoom-ratio, :om-zoom-default-font.)
;;; ============================================================

(defclass* TextFile (object-in-box)
  ((ed-mode :initform "supersede" :initarg :ed-mode :accessor ed-mode)
   (eval-mode :initform "list" :initarg :eval-mode :accessor eval-mode)
   (file-name :initform nil :accessor file-name)
   (buffer-text :initform nil :accessor buffer-text)
   (w-zoom :accessor w-zoom :initform 1.0))
  (:icon 203))

(defmethod draw-obj-in-rect ((self textfile) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom     (oa::om-zoom-effective view))
         (fontsize (max 1 (round (* 10 zoom))))
         (fontface (om-font-face (om-get-font view)))
         (lines    (list-of-lines (buffer-text self)))
         (x-margin (max 1 (round (* 5 zoom))))
         (line-h   (max 1 (round (* 12 zoom)))))
    (when (buffer-text self)
      (om-with-focused-view view
        (om-with-font (om-make-font fontface fontsize)
          (let* ((frame (associated-box self))
                 (width (om-point-x (frame-size frame)))
                 (charwidth (floor (/ width 5))))
            (loop for item in lines
                  for i = 1 then (+ i 1)
                  while (< (+ (* i line-h) y) y1) do
                  (om-draw-string (+ x x-margin) (+ (* i line-h) y) item
                                  :end (if (> (length item) charwidth)
                                           (- charwidth 1)
                                           (length item))))))))))

(defmethod omng-copy ((self TextFile))
  `(let ((newtextfile (make-instance ',(type-of self)
                                     :ed-mode ,(ed-mode self) :ed-mode ,(ed-mode self))))
     (setf (buffer-text newtextfile) ,(om-copy-buffer (buffer-text self)))
     (setf (file-name newtextfile) ,(file-name self))
     (setf (w-zoom newtextfile) ,(w-zoom self))
     newtextfile))

(defmethod omNG-save ((self TextFile) &optional (values? nil))
  (declare (ignore values?))
  (when (file-name self)
    (register-resource :text (file-name self)))
  (if (file-name self)
      `(load-textfile ,(om-save-pathname-relative (file-name self))
                      ',(type-of self) ,(ed-mode self) ,(eval-mode self) ,(w-zoom self))
      `(load-buffer-textfile ',(list-of-lines (buffer-text self))
                             ',(type-of self) ,(ed-mode self) ,(eval-mode self) ,(w-zoom self))))

(defun load-textfile (filename class edmode &optional (evmode "text") (zoom 1.0))
  (let ((newtextfile (make-instance class :ed-mode edmode :eval-mode evmode)))
    (setf (buffer-text newtextfile) (om-make-buffer))
    (if (and filename (pathnamep filename) (probe-file (restore-path filename)))
        (progn
          (om-buffer-insert-file (buffer-text newtextfile) (restore-path filename))
          (setf (file-name newtextfile) (restore-path filename)))
        (om-beep-msg (string+ "I didn't open the file " (namestring filename))))
    (when (numberp zoom) (setf (w-zoom newtextfile) zoom))
    newtextfile))

(defun load-buffer-textfile (listline class edmode &optional (evmode "text") (zoom 1.0))
  (let ((newtextfile (make-instance class :ed-mode edmode :eval-mode evmode)))
    (setf (buffer-text newtextfile) (om-make-buffer))
    (loop for item in listline do
          (om-buffer-insert (buffer-text newtextfile) item)
          (om-buffer-insert-newline (buffer-text newtextfile)))
    (when (numberp zoom) (setf (w-zoom newtextfile) zoom))
    newtextfile))

;;; ============================================================
;;; TextFile editor: TextEditorWindow + zoom persistence per textfile
;;; ============================================================

(defmethod get-slot-in-out-names ((self TextFile))
  (values '("self" "exp-list" "ed-mode" "eval-mode")
          '(nil nil "supersede" "list")
          '("object" "input data or text" "append or supersede" "eval interpretation mode")
          '(nil nil (( 2 (("append" "append") ("supersede" "supersede"))))
                (( 3 (("text" "text") ("data list" "data") ("list" "list") ("value" "value")))))))

(defclass TextEditorWindow (oa::om-text-edit-window)
  ((object :initform nil :initarg :object :accessor object)
   (ref :initform nil :initarg :ref :accessor ref)))

(defmethod oa::editor-window-input-model ((self TextEditorWindow))
  (oa::om-zoom-pane-font-input-entries))

(defmethod editor ((self TextEditorWindow)) self)

(defmethod Class-has-editor-p ((self TextFile)) t)

(defmethod make-editor-window ((class (eql 'TextEditorWindow)) object name ref &key
                               winsize winpos (close-p t) (winshow t) (resize nil) (maximize nil))
  (declare (ignore winsize winpos close-p winshow resize maximize name))
  (open-new-textedit object ref))

(defun find-textfile-editor (textfile)
  "Locate the open TextEditorWindow whose OBJECT is TEXTFILE, or NIL."
  (loop for win in (om-get-all-windows 'TextEditorWindow)
        when (eql (object win) textfile) return win))

(defun capture-textfile-window-zoom (textfile &optional editor)
  (let ((win (or editor (find-textfile-editor textfile))))
    (when win
      (let ((ep (om-lisp::ep win)))
        (when (and ep (oa::om-pane-property ep :om-zoom-default-font))
          (setf (w-zoom textfile) (oa::pane-current-zoom-ratio ep)))))))

(defun apply-saved-textfile-zoom (editor textfile)
  (when (and (slot-exists-p textfile 'w-zoom)
             (slot-boundp textfile 'w-zoom))
    (let ((ep (om-lisp::ep editor))
          (ratio (w-zoom textfile)))
      (when (and ep
                 (oa::om-pane-property ep :om-zoom-default-font)
                 (not (= ratio 1.0)))
        (oa::set-pane-zoom-ratio ep ratio)))))

(defmethod open-new-textedit ((self TextFile) box)
  (unless (buffer-text self)
    (setf (buffer-text self) (om-make-buffer)))
  (setf (editorframe box) (om-make-window 'TextEditorWindow
                                          :object self
                                          :ref box
                                          :font (om-make-font "courier" 20)
                                          :window-title (if (file-name self)
                                                            (namestring (file-name self))
                                                            "TextFile")
                                          :size (om-make-point 500 400)
                                          :position :centered
                                          :ask-for-save nil
                                          :editor-buffer (buffer-text self)
                                          :editor-file (file-name self)))
  (om-add-menu-to-win (editorframe box))
  (apply-saved-textfile-zoom (editorframe box) self)
  (editorframe box))

(defmethod om-window-close-event :before ((self TextEditorWindow))
  (when (object self) (capture-textfile-window-zoom (object self) self)))

(defmethod om-window-close-event :after ((self TextEditorWindow))
  (cond ((null (ref self)) nil)
        ((slot-initform-p (ref self))
         (change-initform-ed (ref self) (object self)))
        ((EditorView-p (ref self))
         (setf (attached-editors (ref self))
               (remove self (attached-editors (ref self)) :test 'equal))
         (om-invalidate-view (ref self) t))
        ((ominstance-p (ref self))
         (setf (Editorframe (ref self)) nil))
        ((is-boxpatch-p (ref self))
         (setf (Editorframe (ref self)) nil)
         (om-invalidate-view (car (frames (ref self)))))))

(defmethod correct-box-inputs ((class (eql 'textfile)) inputs)
  (loop for input in (get-inputs-from-inst (make-instance class))
        for i = 0 then (+ i 1) collect
        (let ((inp (if (and (nth i inputs) (string-equal (name input) (name (nth i inputs))))
                       (nth i inputs) input)))
          inp)))

(defmethod omNG-box-value ((self OMTextFilebox) &optional (numout 0))
  (let ((intype (omNG-box-value (third (inputs self))))
        (ev-mode (if (fourth (inputs self)) (omNG-box-value (fourth (inputs self))) "text")))
    (unless (value self)
      (setf (value self) (get-super-default-value 'textfile)))
    (setf (ed-mode (value self)) intype)
    (setf (eval-mode (value self)) ev-mode)
    (cond
      ((equal (allow-lock self) "o") self)
      ((equal (allow-lock self) "l") "sorry not lambda mode for textfile boxes")
      ((and (equal (allow-lock self) "x") (value self)) (rep-editor (value self) numout))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (rep-editor (value self) numout))
      (t (let ((val (omNG-box-value (second (inputs self))))
               (newtextfile (make-instance (type-of (value self))
                                           :ed-mode intype
                                           :eval-mode ev-mode)))
           (when (and (slot-exists-p (value self) 'w-zoom)
                      (slot-boundp (value self) 'w-zoom))
             (setf (w-zoom newtextfile) (w-zoom (value self))))
           (when (editorframe self)
             (om-close-window (om-view-window (editorframe self)))
             (om-kill-buffer (buffer-text (value self))))
           (if (connected? (first (inputs self)))
               (let* ((inv (omNG-box-value (first (inputs self))))
                      (val (objfromobjs inv newtextfile)))
                 (if val
                     (setf (value self) val
                           (eval-mode (value self)) ev-mode
                           (ed-mode (value self)) intype)
                     (progn
                       (om-beep-msg (format nil "Error: ~A can not be converted to TextFile !" (type-of inv)))
                       (om-abort))))
               (progn
                 (setf (file-name newtextfile) (file-name (value self)))
                 (setf val (list! val))
                 (add/replace-to-buffer (value self) val)
                 (if (buffer-text (value self))
                     (setf (buffer-text newtextfile) (om-copy-buffer (buffer-text (value self))))
                     (setf (buffer-text newtextfile) (om-make-buffer)))
                 (setf (value self) newtextfile)
                 (update-if-editor self)))
           (rep-editor (value self) numout))))))

;;; ============================================================
;;; measures-tempo.lisp override: zoom-aware draw-obj-in-rect Tempo-Map
;;; ============================================================

(defmethod draw-obj-in-rect ((self Tempo-Map) x x1 y y1 edparams view)
  ;; ZOOM-SCALE: font and margins scale with view zoom.
  (let* ((zoom    (oa::om-zoom-effective view))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (scaled-font (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
         (xm     (if scale-p (max 1 (round (* 10 zoom))) 10))
         (line-h (if scale-p (max 1 (round (* 12 zoom))) 12)))
    (om-with-focused-view view
      (flet ((body ()
               (loop for tpair in (tempo-evts self)
                     for i = 1 then (+ i 1) do
                     (if (listp tpair)
                         (om-draw-string xm (* line-h i) (format nil "~D: ~D" (car tpair) (cadr tpair)))
                         (om-draw-string xm (* line-h i) (format nil "Ill-formed time-value: ~D" tpair))))))
        (if scale-p
            (om-with-font scaled-font (body))
            (body))))))

;;; ============================================================
;;; midi-sequence.lisp override
;;; ============================================================

(defmethod draw-obj-in-rect ((self EventMidi-seq) x x1 y y1 edparams view)
  ;; ZOOM-SCALE: font and text positions scale with view zoom.
  (let* ((zoom        (oa::om-zoom-effective view))
         (scale-p     (and (numberp zoom) (/= zoom 1.0)))
         (scaled-font (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
         (text-x      (if scale-p (max 1 (round (* 10 zoom))) 10))
         (text-y      (if scale-p (max 1 (round (* 15 zoom))) 15))
         (long-tick   (if scale-p (max 1 (round (* 5 zoom))) 5))
         (short-tick  (if scale-p (max 1 (round (* 2 zoom))) 2))
         (mid (round (/ (h view) 2)))
         (long (w view))
         l f dur xpt)
    (om-with-focused-view view
      (flet ((draw-name ()
               (om-draw-string text-x text-y
                               (if (stringp (name self)) (name self) "MidiEvents sequence"))))
        (if scale-p
            (om-with-font scaled-font (draw-name))
            (draw-name)))
      (om-draw-rect 0 0 (w view) (h view))
      (om-draw-line 0 mid (w view) mid)
      (unless (empty-midiseq-p self)
        (setf l (nth (- (length (LDate self)) 1) (Ldate self)))
        (setf f (nth 0 (lDate self)))
        (setf dur (- l f))
        (if (= dur 0)
            (om-draw-line (round (/ (w view) 2)) (+ mid long-tick)
                          (round (/ (w view) 2)) (- mid long-tick))
            (loop for date in (Ldate self) do
                  (setf xpt (round (* long (/ date dur))))
                  (om-draw-line xpt (+ mid short-tick) xpt (- mid short-tick))))))))

;;; ============================================================
;;; midievents.lisp override
;;; ============================================================

(defmethod draw-obj-in-rect ((self MidiEvent) x x1 y y1 edparams view)
  ;; ZOOM-SCALE: font and line metrics scale with view zoom.
  (let* ((zoom        (oa::om-zoom-effective view))
         (scale-p     (and (numberp zoom) (/= zoom 1.0)))
         (scaled-font (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
         (xm  (if scale-p (max 1 (round (* 10 zoom))) 10))
         (y1l (if scale-p (max 1 (round (* 20 zoom))) 20))
         (y2l (if scale-p (max 1 (round (* 30 zoom))) 30)))
    (om-with-focused-view view
      (om-draw-rect 0 0 (w view) (h view))
      (flet ((draw-text ()
               (om-draw-string xm y1l "MIDI Event")
               (om-draw-string xm y2l (string (ev-type self)))))
        (if scale-p
            (om-with-font scaled-font (draw-text))
            (draw-text))))))

;;; ============================================================
;;; midifile.lisp override: draw-mini-view miniview MidiFile
;;; ============================================================

(defmethod draw-mini-view ((self miniview) (value MidiFile))
  ;; ZOOM-SCALE: font and margins scale with miniview ancestor zoom.
  (let* ((zoom        (oa::om-zoom-effective self))
         (scale-p     (and (numberp zoom) (/= zoom 1.0)))
         (scaled-font (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
         (xm (if scale-p (max 1 (round (* 5  zoom))) 5))
         (ym (if scale-p (max 1 (round (* 15 zoom))) 15)))
    (if (midifilename value)
        (om-with-focused-view self
          (draw-mini-midi self (real-dur value) value))
        (om-with-focused-view self
          (flet ((draw-text ()
                   (om-draw-string xm ym "No file attached")))
            (if scale-p
                (om-with-font scaled-font (draw-text))
                (draw-text)))))))

;;; ============================================================
;;; osc-events.lisp override
;;; ============================================================

(defmethod draw-obj-in-rect ((self OSCEvent) x x1 y y1 edparams view)
  ;; ZOOM-SCALE: font and line metrics scale with view zoom.
  (let* ((zoom        (oa::om-zoom-effective view))
         (scale-p     (and (numberp zoom) (/= zoom 1.0)))
         (scaled-font (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
         (xm  (if scale-p (max 1 (round (* 10 zoom))) 10))
         (y1l (if scale-p (max 1 (round (* 20 zoom))) 20))
         (y2l (if scale-p (max 1 (round (* 30 zoom))) 30))
         (y3l (if scale-p (max 1 (round (* 40 zoom))) 40)))
    (om-with-focused-view view
      (om-draw-rect 0 0 (w view) (h view))
      (flet ((draw-text ()
               (om-draw-string xm y1l "OSC Event")
               (om-draw-string xm y2l (string+ (host self) " - " (integer-to-string (port self))))
               (om-draw-string xm y3l (format nil "~s" (bundle self)))))
        (if scale-p
            (om-with-font scaled-font (draw-text))
            (draw-text))))))

;;; ============================================================
;;; board.lisp override: zoom-aware draw-obj-in-rect
;;; ============================================================

(defmethod draw-obj-in-rect ((self board) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom (oa::om-zoom-effective view))
         (pen  (max 1 (round zoom)))
         (data (data self))
         datax datay x0 y0)
    (when data
      (setf data (mat-trans data))
      (setf datax (om-scale (first data) 0 (- x1 x) (apply 'min (first data)) (apply 'max (first data))))
      (setf datay (om-scale (second data) 0 (- y1 y) (apply 'min (second data)) (apply 'max (second data))))
      (setf x0 (round (+ x (car datax)))
            y0 (round (+ y (car datay))))
      (om-with-focused-view view
        (om-with-line-size pen
          (om-with-fg-color view (bcolor self)
            (loop for xp in (cdr datax)
                  for yp in (cdr datay) do
                  (om-draw-line x0 y0 (round (+ x xp)) (round (+ y yp)))
                  (setf x0 (round (+ x xp))
                        y0 (round (+ y yp))))))))))

;;; ============================================================
;;; soundeditor.lisp override: om-drag-receive zoom context
;;; ============================================================

(defmethod om-drag-receive ((target patchpanel) (dragged-ref t) position &optional (effect nil))
  (declare (ignore position effect))
  (let ((dragged (dragged-view *OM-drag&drop-handler*))
        (posi (om-mouse-position target)))
    (if (subtypep (class-name (class-of dragged)) 'soundpanel)
        (unless (integerp (car (selection? dragged)))
          (let* ((newsoundfile (extract-sound-selection dragged))
                 newsound newbox)
            (when newsoundfile (setf newsound (load-sound-file newsoundfile)))
            (when newsound
              (setf newbox (make-instance 'OMBoxEditCall
                                          :name "sound extract"
                                          :reference (class-of newsound)
                                          :icon (icon (class-of newsound))
                                          :inputs (get-inputs-from-inst newsound)))
              (setf (value newbox) newsound)
              (set-edition-params (value newbox) newbox)
              (push newbox (attached-objs (class-of newsound)))
              (setf (numouts newbox) (length (get-outs-name (value newbox))))
              (setf (frame-position newbox) (borne-position posi))
              (loop for item in (get-actives target) do
                    (omG-unselect item))
              (let ((new-frame (oa::with-zoom-context-of target
                                 (make-frame-from-callobj newbox))))
                (omG-add-element target new-frame)
                (omG-select new-frame)))
            (om-invalidate-view target)
            t))
        (call-next-method))))

;;; ============================================================
;;; scorepatch.lisp override: set-panel-boxes + mk-musobj-box zoom context
;;; ============================================================

(defmethod set-panel-boxes ((self ScorePanel))
  (let ((elements (boxes (object self))))
    (loop for i in (send-receive-keys self)
          do (open-all-keys i))
    (mapc #'(lambda (elem)
              (let ((newframe (oa::with-zoom-context-of self
                                (make-frame-from-callobj elem))))
                (om-add-subviews self newframe)
                (add-subview-extra newframe))) elements)
    (mapc #'(lambda (elem)
              (update-graphic-connections elem elements)) (get-subframes self))
    (revise-references self)))

(defmethod mk-musobj-box ((self scorePanel))
  (let* ((mode-obj (grap-class-from-type (obj-mode self)))
         (size (round (staff-size self) 8)))
    (list mode-obj (selection? self))
    (loop for item in (get-graph-selection? self mode-obj) do
          (let* ((newconst (make-score-box (reference item) 'score-box
                                           (get-posi-score-box item size)
                                           "musical box"
                                           (get-size-score-box item size)))
                 (frame (oa::with-zoom-context-of self
                          (make-frame-from-callobj newconst))))
            (setf (selection? self) (remove (reference item) (selection? self) :test 'equal))
            (setf (selected item) nil)
            (omG-add-element self frame)))))

;;; ============================================================
;;; scoretools.lisp override: draw-auxiliar-lines uses FLOOR
;;; ============================================================

;;; FLOOR (not ROUND) so the step is never larger than the auxlines gap (size/4).
;;; With ROUND, sizes where (round size 4) > size/4 (e.g., 27, 30, 31, 35) lose all aux lines.

(defun draw-auxiliar-lines (self x y size realpos headsizex)
  (when (auxlines self)
    (om-with-fg-color nil *system-color*
      (let ((dir (car (auxlines self)))
            (topy (+ (- y (round size 8)) (second (auxlines self))))
            (limy (+ (- y (round size 8)) (third (auxlines self)))))
        (if (equal dir 'dw)
            (progn
              (setf topy (+ topy (floor size 4)))
              (loop while (<= topy limy) do
                    (om-draw-line (- realpos (round size 8)) topy
                                  (+ headsizex realpos) topy)
                    (setf topy (+ topy (floor size 4)))))
            (progn
              (setf topy (- topy (floor size 4)))
              (loop while (>= topy limy) do
                    (om-draw-line (- realpos (round size 8)) topy
                                  (+ headsizex realpos) topy)
                    (setf topy (- topy (floor size 4))))))))))

;;; ============================================================
;;; dragagain.lisp override: make-and-add-box zoom-aware
;;; ============================================================

(defmethod make-and-add-box ((self patchpanel) obj position)
  (let* ((newcall (omNG-make-new-boxcall (class-of obj) position (mk-unique-name self "dragged"))))
    (setf (value newcall) obj)
    (setf (showpict newcall) t)
    (omG-add-element self
                     (oa::with-zoom-context-of self
                       (make-frame-from-callobj newcall)))
    t))

(defmethod make-and-add-box ((self maquettepanel) obj position)
  (let* ((maqpos (get-offset/posy-from-pixel self position))
         (newcall (omNG-make-tempobj (omNG-make-new-instance obj "dragged") maqpos
                                     (mk-unique-name self "dragged"))))
    (setf (offset newcall) (om-point-h maqpos))
    (setf (posy newcall) (om-point-v maqpos))
    (setf (showpict newcall) t)
    (omG-add-element self
                     (oa::with-zoom-context-of self
                       (make-frame-from-callobj newcall)))
    t))

;;; ============================================================
;;; sdiffile.lisp override
;;; ============================================================

(defmethod draw-obj-in-rect ((self sdiffile) x x1 y y1 params view)
  ;; ZOOM-SCALE: font and label margins scale with view zoom.
  (let* ((zoom        (oa::om-zoom-effective view))
         (scale-p     (and (numberp zoom) (/= zoom 1.0)))
         (scaled-font (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
         (m2  (if scale-p (max 1 (round (* 2  zoom))) 2))
         (m6  (if scale-p (max 1 (round (* 6  zoom))) 6))
         (m12 (if scale-p (max 1 (round (* 12 zoom))) 12))
         (m14 (if scale-p (max 1 (round (* 14 zoom))) 14))
         (m24 (if scale-p (max 1 (round (* 24 zoom))) 24)))
    (flet ((body ()
             (cond ((and (filepathname self) (loaded self))
                    (om-draw-string (+ x m2) (+ y m12) "SDIF FILE contents:")
                    (loop for str in (streamsdesc self) for pos = m24 then (+ pos m12) do
                          (om-draw-string (+ x m6) (+ y pos)
                                          (format nil "~D:~A ~A"
                                                  (fstreamdesc-id str)
                                                  (fstreamdesc-fsig str)
                                                  (mapcar 'mstreamdesc-msig (fstreamdesc-matrices str))))))
                   ((filepathname self)
                    (om-with-fg-color view *om-red-color*
                      (om-draw-string m6 m14 (namestring (filepathname self)))))
                   ((not (filepathname self))
                    (om-draw-string m6 m14 "[no file loaded]")))))
      (if scale-p
          (om-with-font scaled-font (body))
          (body)))))

;;; ============================================================
;;; Generic class miniview: pen/font/margins scale with view zoom
;;; ============================================================

#-linux
(defmethod draw-obj-in-rect ((self t) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom        (oa::om-zoom-effective view))
         (pen         (max 1 (round zoom)))
         (scaled-font (if (= zoom 1.0) *om-default-font1*
                          (oa::om-zoom-scale-font *om-default-font1* zoom)))
         (m2  (max 1 (round (* 2  zoom))))
         (m3  (max 1 (round (* 3  zoom))))
         (m4  (max 1 (round (* 4  zoom))))
         (m15 (max 1 (round (* 15 zoom)))))
    (om-with-focused-view view
      (om-with-line-size pen
        (om-with-font scaled-font
          (if (omclass-p (class-of (class-of self)))
              (let* ((thelist (name-values-list self))
                     (length  (length thelist))
                     (y       (+ y m15)))
                (loop for item in thelist
                      for i = 0 then (+ i 1)
                      while (< y y1) do
                      (om-draw-string (+ x m3) (- y m3)
                                      (format nil "~D : ~D" (car item) (second item)))
                      (unless (= i (- length 1))
                        (om-with-fg-color view *om-gray-color*
                          (om-draw-line x y x1 y)))
                      (setf y (+ y m15))))
              (om-draw-string (+ x m2)
                              (+ y (round (- y1 y) 2) m4)
                              (format nil "~D" self))))))))

#+linux
(defmethod draw-obj-in-rect ((self t) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (let* ((zoom        (oa::om-zoom-effective view))
         (pen         (max 1 (round zoom)))
         (scaled-font (if (= zoom 1.0) *om-default-font1*
                          (oa::om-zoom-scale-font *om-default-font1* zoom)))
         (m2  (max 1 (round (* 2  zoom))))
         (m3  (max 1 (round (* 3  zoom))))
         (m4  (max 1 (round (* 4  zoom))))
         (m15 (max 1 (round (* 15 zoom)))))
    (om-with-focused-view view
      (om-with-line-size pen
        (om-with-font scaled-font
          (if (omclass-p (class-of (class-of self)))
              (let* ((thelist (name-values-list self))
                     (length  (length thelist))
                     (y       (+ y m15)))
                (loop for item in thelist
                      for i = 0 then (+ i 1)
                      while (< y y1) do
                      (let* ((strg (format nil "~D : ~D" (car item) (second item)))
                             (lgt  (length strg)))
                        (if (> (* lgt 5) x1)
                            (om-draw-string (+ x m3) (- y m3) strg :end (- (floor (/ x1 5)) 3))
                            (om-draw-string (+ x m3) (- y m3) strg))
                        (unless (= i (- length 1))
                          (om-with-fg-color view *om-gray-color*
                            (om-draw-line x y x1 y)))
                        (setf y (+ y m15)))))
              (om-draw-string (+ x m2)
                              (+ y (round (- y1 y) 2) m4)
                              (format nil "~D" self))))))))

;;; ============================================================
;;; fluid interface boxes: position+size scale with container zoom;
;;; font restamped from logical-font.
;;; ============================================================

(defmethod update-di-size ((self fl-microtune) container)
  (let ((zoom (oa::om-zoom-effective container)))
    (om-set-view-position self
                          (om-make-point (round (* 10 zoom))
                                         (- (round (h container) 2)
                                            (round (* 11 zoom)))))
    (om-set-view-size self
                      (om-make-point (- (w container) (round (* 20 zoom)))
                                     (round (* 30 zoom))))
    (let ((logical-font (oa::om-zoom-logical-font self)))
      (when logical-font
        (om-set-font self (if (= zoom 1.0)
                              logical-font
                              (oa::om-zoom-scale-font logical-font zoom)))))))

(defmethod update-di-size ((self fl-pgm) container)
  (let ((zoom (oa::om-zoom-effective container)))
    (om-set-view-position self
                          (om-make-point (round (* 10 zoom))
                                         (- (round (h container) 2)
                                            (round (* 11 zoom)))))
    (om-set-view-size self
                      (om-make-point (- (w container) (round (* 20 zoom)))
                                     (round (* 30 zoom))))
    (let ((logical-font (oa::om-zoom-logical-font self)))
      (when logical-font
        (om-set-font self (if (= zoom 1.0)
                              logical-font
                              (oa::om-zoom-scale-font logical-font zoom)))))))

(defmethod update-di-size ((self fl-gain) container)
  (let ((zoom (oa::om-zoom-effective container)))
    (if (equal (om-get-slider-orientation self) :horizontal)
        (progn
          (om-set-view-position self
                                (om-make-point (round (* 8 zoom))
                                               (- (round (h container) 2)
                                                  (round (* 12 zoom)))))
          (om-set-view-size self
                            (om-make-point (- (w container) (round (* 16 zoom)))
                                           (round (* 24 zoom)))))
        (progn
          (om-set-view-position self
                                (om-make-point (- (round (w container) 2)
                                                  (round (* 12 zoom)))
                                               (round (* 8 zoom))))
          (om-set-view-size self
                            (om-make-point (round (* 24 zoom))
                                           (- (h container) (round (* 16 zoom)))))))
    (let ((logical-font (oa::om-zoom-logical-font self)))
      (when logical-font
        (om-set-font self (if (= zoom 1.0)
                              logical-font
                              (oa::om-zoom-scale-font logical-font zoom)))))))

(defmethod update-di-size ((self fl-pan) container)
  (let ((zoom (oa::om-zoom-effective container)))
    (if (equal (om-get-slider-orientation self) :horizontal)
        (progn
          (om-set-view-position self
                                (om-make-point (round (* 8 zoom))
                                               (- (round (h container) 2)
                                                  (round (* 12 zoom)))))
          (om-set-view-size self
                            (om-make-point (- (w container) (round (* 16 zoom)))
                                           (round (* 24 zoom)))))
        (progn
          (om-set-view-position self
                                (om-make-point (- (round (w container) 2)
                                                  (round (* 12 zoom)))
                                               (round (* 8 zoom))))
          (om-set-view-size self
                            (om-make-point (round (* 24 zoom))
                                           (- (h container) (round (* 16 zoom)))))))
    (let ((logical-font (oa::om-zoom-logical-font self)))
      (when logical-font
        (om-set-font self (if (= zoom 1.0)
                              logical-font
                              (oa::om-zoom-scale-font logical-font zoom)))))))

;;; ============================================================
;;; Score editor zoom: zoom-numbox + font-zoom widget on
;;; omcontrols-view; staff-zoom Ctrl+= /- /0 on scorePanel;
;;; pinch-driven om-zoom-touch-update across score panels;
;;; font size step + scale on note/chord/chordseq panels.
;;; ============================================================

(defclass omcontrols-view (3dBorder-view)
  ((slotedit :initform nil :accessor slotedit)
   (zoom-numbox :initform nil :accessor zoom-numbox)
   (font-zoom-widget :initform nil :accessor font-zoom-widget))
  (:default-initargs
   :draw-with-buffer t
   :c++ *controls-color++* :c+ *controls-color+*
   :c-- *controls-color--* :c- *controls-color-*))

(defgeneric make-mus-font-widget (control-view panel position font font-size-init)
  (:documentation "Build the font-size widget for CONTROL-VIEW. Default = legacy pop-up; specialize for zoom-aware variants."))

(defmethod make-mus-font-widget ((self omcontrols-view) panel position font font-size-init)
  (om-make-dialog-item 'om-pop-up-dialog-item
                       position
                       (om-make-point 56 22)
                       ""
                       :di-action (om-dialog-item-act item
                                    (let ((newsize (cadr (nth (om-get-selected-item-index item) *mus-font-size*))))
                                      (change-editor-size panel newsize)))
                       :font font
                       :range (loop for item in *mus-font-size* collect (car item))
                       :value font-size-init))

(defmethod initialize-instance :after ((self omcontrols-view) &rest l
                                       &key (tone "1/2") (staff "ffgg") (font-size "24") (zoom 1) (mode 0) (onset 0) (measure 1))
  (declare (ignore l tone staff mode onset measure))
  (setf onsetval 0)
  (setf measnumval 1)
  (let* ((editor (om-view-container self))
         (obj (object editor))
         (panel (panel editor))
         (di-font *controls-font*)
         (approx (staff-tone (panel (om-view-container self))))
         (approx (if (listp approx) (car approx) approx))
         (l1 230)
         (l2 380)
         (l3 500)
         (l4 620)
         (c1 2)
         (c2 *second-row-y*)
         (minied (om-make-dialog-item 'edit-numbox (om-make-point 96 (+ c1 2)) (om-make-point 50 18) " "
                                      :value nil
                                      :font *om-default-font1*
                                      :bg-color *om-white-color*
                                      :help-spec ""))
         (realmidics (om-make-dialog-item 'om-check-box (om-make-point 148 (+ c1 0)) (om-make-point 25 15)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (if (associated-box obj)
                                                           (set-edit-param (associated-box obj) 'approx? (if (om-checked-p item) 1 0))
                                                           (if (om-checked-p item) 1 0)))
                                          :font *controls-font*
                                          :checked-p
                                          (let ((app (if (not (typep obj 'scale))
                                                         (get-edit-param (associated-box obj) 'approx?))))
                                            (if app
                                                (if (equal 1 app) t nil)
                                                *global-approx-midics?*))))
         (slotbut (om-make-dialog-item 'om-pop-up-dialog-item
                                       (om-make-point 5 c1)
                                       (om-make-point 80 22)
                                       ""
                                       :range (loop for item in (GET-slot-LIST self) collect (car item))
                                       :value "midic"
                                       :font di-font
                                       :di-action
                                       (om-dialog-item-act item
                                         (let ((newslot (cadr (nth (om-get-selected-item-index item) (GET-slot-LIST self)))))
                                           (change-slot-edit panel newslot)))))
         (sizeitem (om-make-dialog-item 'om-static-text
                                        (om-make-point (- l1 (om-string-size "Font size" *om-default-font1*) 8) (+ c2 2))
                                        (om-make-point 90 16) "Font size"
                                        :font *om-default-font1*
                                        :bg-color *controls-color*))
         (sizebut (make-mus-font-widget self panel (om-make-point l1 c2) di-font font-size))
         (staffitem (om-make-dialog-item 'om-static-text (om-make-point (- l2 36) (+ c1 2)) (om-make-point 60 20) "Staff"
                                         :font *om-default-font1*
                                         :bg-color *controls-color*))
         (staffbut (om-make-dialog-item 'om-pop-up-dialog-item
                                        (om-make-point l2 c1)
                                        #+(or linux win32) (om-make-point 90 22)
                                        #+macosx (om-make-point 102 22)
                                        ""
                                        :di-action (om-dialog-item-act item
                                                     (let ((newstaff (cadr (nth (om-get-selected-item-index item) (GET-staff-LIST self)))))
                                                       (change-system panel newstaff)))
                                        :font di-font
                                        :range (loop for item in (GET-staff-LIST self) collect (car item))
                                        :value (car (find (get-edit-param editor 'staff) (GET-staff-LIST self) :key 'cadr :test 'equal))))
         (toneitem (om-make-dialog-item 'om-static-text (om-make-point (- l2 50) (+ c2 2)) (om-make-point 52 20) "Approx"
                                        :font *om-default-font1*
                                        :bg-color *controls-color*))
         (edobut (om-make-dialog-item 'om-button
                                      #+linux (om-make-point l2 c2)
                                      #+win32 (om-make-point l2 (- c2 3))
                                      #+macosx (om-make-point (- l2 5) (- c2 5))
                                      #+(or linux win32) (om-make-point 90 25)
                                      #+macosx (om-make-point 110 30)
                                      (format nil "~A" (give-symbol-of-approx approx))
                                      :font *om-default-font1*
                                      :di-action (om-dialog-item-act item
                                                   (declare (ignore item))
                                                   (om-micron *omicron-data* self))))
         (onsetitem (om-make-dialog-item 'om-static-text (om-make-point (- l3 5) (+ c1 2)) (om-make-point 60 20) "Onset"
                                         :font *om-default-font1*
                                         :bg-color *controls-color*))
         (onset-ms (om-make-dialog-item 'edit-numbox (om-make-point (+ l3 45) (+ c1 2)) (om-make-point 46 18)
                                        (format nil " ~D" onsetval)
                                        :di-action (om-dialog-item-act item
                                                     (change-editor-onset panel (value item)))
                                        :min-val 0
                                        :font *om-default-font1*
                                        :bg-color *om-white-color*
                                        :afterfun #'(lambda (item)
                                                      (change-editor-onset panel (value item))
                                                      (update-for-subviews-changes panel t))
                                        :value onsetval))
         (measureitem (om-make-dialog-item 'om-static-text (om-make-point (- l3 5) (+ c1 2)) (om-make-point 60 20)
                                           "Measure"
                                           :font *om-default-font1*
                                           :bg-color *controls-color*))
         (meas-num (om-make-dialog-item 'edit-numbox
                                        (om-make-point (+ l3 45) (+ c1 2)) (om-make-point 46 18)
                                        (format nil " ~D" measnumval)
                                        :di-action (om-dialog-item-act item
                                                     (setf measnumval (value item))
                                                     (change-editor-measure panel (value item))
                                                     (capi:redisplay-element panel))
                                        :font *om-default-font1*
                                        :bg-color *om-white-color*
                                        :value measnumval
                                        :afterfun #'(lambda (item)
                                                      (setf measnumval (value item))
                                                      (change-editor-measure panel (value item))
                                                      (update-for-subviews-changes panel t))
                                        :min-val 1))
         (meas-max (om-make-dialog-item 'om-static-text
                                        #+linux (om-make-point (- l3 5) (- c2 12))
                                        #+(or win32 macosx) (om-make-point (- l3 5) (- c2 2))
                                        (om-make-point 80 20)
                                        (format nil "Max:")
                                        :font *om-default-font1*
                                        :bg-color *controls-color*))
         (meas-count (om-make-dialog-item 'om-static-text
                                          #+linux (om-make-point (+ l3 45) (- c2 12))
                                          #+(or win32 macosx) (om-make-point (+ l3 45) (- c2 2))
                                          (om-make-point 80 20)
                                          (format nil "~A" (meas-count obj))
                                          :font *om-default-font1*
                                          :bg-color *controls-color*))
         (duration (om-make-dialog-item 'om-static-text (om-make-point (+ l4 5) (+ c1 2)) (om-make-point 260 50)
                                        ""
                                        :font *om-default-font1*
                                        :bg-color *controls-color*)))
    (setf (slotedit self) minied)
    (cond
      ((measure-p obj) (om-add-subviews self duration staffitem staffbut sizeitem slotbut toneitem
                                        minied sizebut measureitem meas-num edobut realmidics))
      ((or (voice-p obj) (poly-p obj)) (om-add-subviews self duration staffitem staffbut sizeitem slotbut toneitem
                                                        minied sizebut measureitem meas-num edobut meas-max meas-count realmidics))
      ((or (chord-seq-p obj) (multi-seq-p obj)) (om-add-subviews self duration staffitem staffbut sizeitem slotbut toneitem
                                                                 minied sizebut onsetitem onset-ms edobut realmidics))
      (t (om-add-subviews self staffitem staffbut sizeitem slotbut toneitem
                          minied sizebut edobut realmidics)))
    (add-zoom2control self zoom (om-make-point l1 c1))
    (om-set-bg-color self *controls-color*)))

(defun add-zoom2control (control zoom &optional position)
  (setf zoom (if zoom (round (* zoom 100)) 100))
  (let* ((pos (or (om-add-points position (om-make-point 3 2))
                  (om-make-point 100 2)))
         (zoom-nb (om-make-dialog-item 'edit-numbox pos (om-make-point 46 18) (format nil " ~D" zoom)
                                       :di-action (om-dialog-item-act item
                                                    (change-editor-zoom (panel (om-view-container (om-view-container item))) (value item)))
                                       :font *om-default-font1*
                                       :bg-color *om-white-color*
                                       :value zoom
                                       :afterfun #'(lambda (item)
                                                     (change-editor-zoom (panel (om-view-container (om-view-container item))) (value item)))
                                       :min-val 1
                                       :max-val 1000)))
    (om-add-subviews control
                     zoom-nb
                     (om-make-dialog-item 'om-static-text (om-make-point (- (om-point-h pos) 40) 4) (om-make-point 40 20) "Zoom"
                                          :font *om-default-font1*
                                          :bg-color *controls-color*))
    (when (slot-exists-p control 'zoom-numbox)
      (setf (zoom-numbox control) zoom-nb))
    zoom-nb))

(defun find-score-zoom-numbox (panel)
  (let* ((ed  (and (typep panel 'om-graphic-object) (editor panel)))
         (ctr (and ed (slot-exists-p ed 'ctr-view) (ctr-view ed))))
    (and ctr (slot-exists-p ctr 'zoom-numbox) (zoom-numbox ctr))))

(defmethod handle-key-event ((self scorePanel) char)
  (cond ((in-patch-mode? self)
         (case char
           (#\t (mk-musobj-box self))
           (#\s (if (om-command-key-p)
                    (omng-save (mycontainer (associated-box (object (editor self)))))
                    (toggle-normal-mode self)))
           (#\SPACE (editor-play/stop (editor self)))
           (:om-key-tab (change-obj-mode self 1))
           (t (call-next-method))))
        ((om-command-key-p)
         (cond
           ((or (equal char #\=) (equal char #\+))
            (change-editor-zoom self
                                (max 1 (min 1000 (round (* (staff-zoom self) 110)))))
            t)
           ((equal char #\-)
            (change-editor-zoom self
                                (max 1 (min 1000 (round (/ (* (staff-zoom self) 100) 1.1)))))
            t)
           ((equal char #\0)
            (change-editor-zoom self 100)
            t)
           (t (scroll-pane self char))))
        ((edit-cursor self)
         (cond
           ((and (rythmic? (edit-cursor self)) (char-is-figure char))
            (add-or-replace-in-measure self char))
           ((or (equal char :om-key-return) (equal char :om-key-enter))
            (if (assoc-chord (edit-cursor self))
                (enter-a-note self)
                (om-beep-msg "Enter a number between 0 and 7")))
           ((and (rythmic? (edit-cursor self)) (equal char #\TAB))
            (create-new-edit-cursor self))
           ((equal char :om-key-up)
            (move-editor-pitch self 0)
            (update-panel self t))
           ((equal char :om-key-down)
            (move-editor-pitch self 1)
            (update-panel self t))
           ((and (rythmic? (edit-cursor self)) (equal char :om-key-left))
            (advance-edit-cursor self -1))
           ((and (rythmic? (edit-cursor self)) (equal char :om-key-right))
            (advance-edit-cursor self 1))
           ((equal char #\.) (point-edit-cursor self))))
        (t
         (case char
           (#\SPACE (editor-play/stop (editor self)))
           (:om-key-esc
            (if (and (selection? self) (idle-p (player (editor self))))
                (toggle-selection self)
                (let ()
                  (editor-stop (editor self))
                  (when (recording (editor self))
                    (stop-recording (editor self))
                    (setf (selected-p (nth 3 (play-buttons (title-bar (editor self))))) nil)))))
           (#\s (if (om-command-key-p)
                    (omng-save (mycontainer (associated-box (object (editor self)))))
                    (toggle-patch-mode self)))
           (#\x (show-extra-palette-tools self))
           (#\r (get-score-tree self))
           (#\R (get-tree-tempo-list self))
           (#\m (get-tempo-tree self))
           (#\M (remove-tempo self))
           (#\i (get-obj-info self))
           (#\I (show-inspector-window (panel self)))
           (#\y (om-inspect (car (selection? self))))
           (#\c (note-chan-color self))
           (#\n (set-name-to-mus-obj self))
           (#\h (show-help-window (format nil "Commands for ~A Editor"
                                          (string-upcase (class-name (class-of (object (editor self))))))
                                  (get-help-list self)))
           (#\t (when *om-tonalite* (score-set-tonalite self)))
           (#\T (when *om-tonalite* (score-remove-tonalite self)))
           (#\q (progn
                  (set-pushed (editor self) "rec" t)
                  (start-recording (editor self))))
           (#\w (progn
                  (set-pushed (editor self) "rec" nil)
                  (stop-recording (editor self))))
           (#\S (create-editor-scale (editor self)))
           (:om-key-tab (change-obj-mode self 1))
           (:om-key-up   (move-selection self 0))
           (:om-key-down (move-selection self 1))
           (otherwise
            (if (selection? self)
                (if (char-is-digit char)
                    (do-subdivise self char)
                    (case char
                      (#\1 (subdivide-more self))
                      (#\0 (add-grace-notes-dialog (car (selection? self)) self))
                      (#\. (progn
                             (delete-grace-notes (car (selection? self)))
                             (update-panel self)))
                      (:om-key-left (cond ((extra-p (car (selection? self)))
                                           (advance-extras self -1))
                                          ((equal (slots-mode self) 'dur)
                                           (change-dur self -1))
                                          (t (change-x self -1))))
                      (:om-key-right (cond ((extra-p (car (selection? self)))
                                            (advance-extras self 1))
                                           ((equal (slots-mode self) 'dur)
                                            (change-dur self 1))
                                           (t (change-x self 1))))
                      (#\C (set-color-to-mus-obj self))
                      (:om-key-delete (delete-selection self))
                      (#\+ (do-union self))
                      (#\- (un-group self))
                      (#\_ (do-group self))
                      (#\* (push-group-up self))
                      (#\= (tie-selection self))
                      (#\/ (untie-selection self))
                      (#\o (open-internal-editor self))
                      (otherwise (om-beep))))
                (case char
                  (:om-key-esc (reset-cursor self))))))
         (when (editor self) (update-inspector (editor self) 0)))))

(defmethod change-editor-zoom ((self scorePanel) newzoom)
  (unless (= (staff-zoom self) newzoom)
    (setf (staff-zoom self) (/ newzoom 100))
    (set-edit-param (om-view-container self) 'zoom (/ newzoom 100))
    (unless (score-page-mode self)
      (om-redraw-view self))
    (when (or (score-page-mode self) (in-patch-mode? self))
      (update-panel self)
      #+(or macosx darwin darwin-target) (update-slot-edit self))
    (let ((nb (find-score-zoom-numbox self)))
      (when (and nb (not (= (value nb) newzoom)))
        (set-value nb newzoom)))))

(defmethod oa::om-zoom-touch-update ((pane scorePanel) scale anchor-x anchor-y)
  (declare (ignore anchor-x anchor-y))
  (let* ((current      (staff-zoom pane))
         (new-fraction (* (or current 1) scale))
         (new-pct      (max 1 (min 1000 (round (* new-fraction 100))))))
    (change-editor-zoom pane new-pct)))

(defun build-zoom-font-widget (control-view panel position font)
  (let* ((initial-size (or (and panel (staff-size panel)) 24))
         (widget (om-make-view 'oa::om-zoom-musical-font-pop-up
                               :position position
                               :size (om-make-point 56 22)
                               :value initial-size
                               :presets (mapcar #'cadr *mus-font-size*)
                               :font font
                               :bg-color *om-white-color*
                               :di-action #'(lambda (w new-size)
                                              (declare (ignore w))
                                              (change-editor-size panel new-size)
                                              (oa::om-redisplay-element panel)))))
    (setf (font-zoom-widget control-view) widget)
    widget))

(defmethod make-mus-font-widget ((self omnote-controls-view) panel position font font-size-init)
  (declare (ignore font-size-init))
  (build-zoom-font-widget self panel position font))

(defun find-score-font-widget (panel)
  (let* ((ed  (and (typep panel 'om-graphic-object) (editor panel)))
         (ctr (and ed (slot-exists-p ed 'ctr-view) (ctr-view ed))))
    (and ctr (slot-exists-p ctr 'font-zoom-widget) (font-zoom-widget ctr))))

(defun zoom-font-scale (pane scale)
  (let* ((current  (or (staff-size pane) 24))
         (new-size (max 8 (min 72 (round (* current scale))))))
    (unless (= new-size current)
      (change-editor-size pane new-size)
      (let ((w (find-score-font-widget pane)))
        (when w (oa::om-set-zoom-musical-font-pop-up-value w new-size))))))

(defun zoom-font-step (pane delta &optional resetp)
  (let* ((cur (or (staff-size pane) 24))
         (new (cond (resetp                24)
                    ((plusp delta)         (min 72 (+ cur delta)))
                    (t                     (max 8  (+ cur delta))))))
    (unless (= new cur)
      (change-editor-size pane new)
      (let ((w (find-score-font-widget pane)))
        (when w (oa::om-set-zoom-musical-font-pop-up-value w new))))))

(defmethod make-mus-font-widget ((self omchord-controls-view) panel position font font-size-init)
  (declare (ignore font-size-init))
  (build-zoom-font-widget self panel position font))

(defmethod oa::om-zoom-touch-update ((pane notePanel) scale anchor-x anchor-y)
  (declare (ignore anchor-x anchor-y))
  (zoom-font-scale pane scale))

(defmethod oa::om-zoom-touch-update ((pane chordPanel) scale anchor-x anchor-y)
  (declare (ignore anchor-x anchor-y))
  (zoom-font-scale pane scale))

(defmethod handle-key-event ((self notePanel) char)
  (cond ((not (om-command-key-p))   (call-next-method))
        ((member char '(#\= #\+))   (zoom-font-step self 1)    t)
        ((eql char #\-)             (zoom-font-step self -1)   t)
        ((eql char #\0)             (zoom-font-step self 0 t)  t)
        (t                          (call-next-method))))

(defmethod handle-key-event ((self chordPanel) char)
  (cond ((not (om-command-key-p))   (call-next-method))
        ((member char '(#\= #\+))   (zoom-font-step self 1)    t)
        ((eql char #\-)             (zoom-font-step self -1)   t)
        ((eql char #\0)             (zoom-font-step self 0 t)  t)
        (t                          (call-next-method))))

(defmethod make-mus-font-widget ((self chordseq-controls-view) panel position font font-size-init)
  (declare (ignore font-size-init))
  (build-zoom-font-widget self panel position font))

(defmethod oa::om-zoom-touch-update ((pane chordseqPanel) scale anchor-x anchor-y)
  (declare (ignore anchor-x anchor-y))
  (zoom-font-scale pane scale))

(defmethod handle-key-event :around ((self chordseqPanel) char)
  (cond ((not (om-command-key-p))   (call-next-method))
        ((member char '(#\= #\+))   (zoom-font-step self 1)    t)
        ((eql char #\-)             (zoom-font-step self -1)   t)
        ((eql char #\0)             (zoom-font-step self 0 t)  t)
        (t                          (call-next-method))))

