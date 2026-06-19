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
;;; patchboxes: zoom-aware module + outputs + miniview construction
;;; (w module) / (h module) are VISUAL after the omboxframe override.
;;; ============================================================

(defmethod make-outputs-from-names ((self t) value module)
  "The outputs of these boxes depent from the initarg slots of the class reference."
  ;; ZOOM-SCALE: output frame metrics honor the make-frame zoom context.
  (let* ((numouts  (numouts self))
         (nameouts (get-outs-name value))
         (zoom     (or oa::*make-frame-zoom-context* 1.0))
         (scale-p  (and (numberp zoom) (/= zoom 1.0)))
         (io-size  (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8))
         (off-x    (if scale-p (oa::om-zoom-scale-int 4 zoom) 4))
         (off-y    (if scale-p (oa::om-zoom-scale-int 9 zoom) 9)))
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

(defmethod make-frame-from-callobj ((self OMBoxEditCall))
  "Make a simple frame for the editor factory 'self'."
  (let* ((name (string-downcase (name self)))
         (defsize (get-boxsize self))
         (numouts (numouts self))
         (numins (length (inputs self)))
         (index 0) input-frames boxframex
         module boxsize miniview
         (zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0))))
    (setf boxframex (if (frame-size self)
                        (om-point-h (frame-size self))
                        (apply #'max (list (om-point-h defsize) (* 10 numouts) (* 10 numins)))))
    (setf boxsize (if (frame-size self)
                      (frame-size self)
                      (om-make-point boxframex (om-point-v defsize))))
    (let* ((module-vsize (if scale-p (oa::om-zoom-scale-point boxsize zoom) boxsize))
           (logical-pos  (frame-position self))
           (module-vpos  (if (and scale-p logical-pos)
                             (oa::om-zoom-scale-point logical-pos zoom)
                             logical-pos))
           (io-size-v (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8))
           (io-y-v    (if scale-p (oa::om-zoom-scale-int-min 1 0 zoom) 1)))
      (setf input-frames
            (mapcar #'(lambda (input)
                        (setf index (+ index 1))
                        (let* ((x-log (- (* index (round (om-point-h boxsize) (+ numins 1))) 4))
                               (x-v   (if scale-p (oa::om-zoom-scale-int x-log zoom) x-log)))
                          (om-make-view (get-input-class-frame self)
                                        :object input
                                        :help-spec (string+ "<" (string-downcase (name input))
                                                            "> " (doc-string input))
                                        :size (om-make-point io-size-v io-size-v)
                                        :position (om-make-point x-v io-y-v))))
                    (inputs self)))
      (setq module (om-make-view (get-frame-class self)
                                 :position module-vpos
                                 :size module-vsize
                                 :object self))
      (setf (inputframes module) input-frames)
      (loop for input-f in input-frames do (om-add-subviews module input-f))
      (make-outputs-from-names self (value self) module)
      (let* ((mini-pos-v    (if scale-p
                                (om-make-point 0 (oa::om-zoom-scale-int-min 8 0 zoom))
                                (om-make-point 0 8)))
             (mini-size-log (om-subtract-points boxsize (om-make-point 0 17)))
             (mini-size-v   (if scale-p (oa::om-zoom-scale-point mini-size-log zoom) mini-size-log)))
        (setf miniview (if (minieditor? self)
                           (om-make-view (get-editor-class (value self))
                                         :ref self
                                         :mini-editor-p t
                                         :object (value self)
                                         :position mini-pos-v
                                         :size mini-size-v)
                           (om-make-view (get-miniview-class self)
                                         :position mini-pos-v
                                         :font (if scale-p
                                                   (oa::om-zoom-scale-font *om-default-font1* zoom)
                                                   *om-default-font1*)
                                         :help-spec (string+ "Make an instance of the class "
                                                             (string-downcase (class-name (reference self))) ".")
                                         :size mini-size-v))))
      (when (and scale-p miniview (not (minieditor? self)))
        (setf (oa::om-zoom-logical-font miniview) *om-default-font1*))
      (setf (iconview module) miniview)
      (om-add-subviews module miniview)
      (setf (frames self) (list module))
      (if scale-p
          (setf (frame-size self) boxsize)
          (setf (frame-size self) (om-view-size module)))
      (setf (name module) name)
      (add-box-resize module)
      (when (allow-lock self)
        (add-lock-button module (allow-lock self)))
      module)))

(defmethod make-frame-from-callobj ((self OMBoxTypeCall))
  "Cons simple frames for 'self'."
  (let* ((name (string-downcase (name self)))
         module boxframex ttybox
         (zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0))))
    (setf boxframex (if (frame-size self) (frame-size self)
                        (om-add-points (good-text-box-size (thestring self) *ombox-font*)
                                       (om-make-point -10 0))))
    (let* ((module-vsize (if scale-p (oa::om-zoom-scale-point boxframex zoom) boxframex))
           (logical-pos  (frame-position self))
           (module-vpos  (if (and scale-p logical-pos)
                             (oa::om-zoom-scale-point logical-pos zoom)
                             logical-pos)))
      (setq module (om-make-view (get-frame-class self)
                                 :position module-vpos
                                 :size module-vsize
                                 :object self))
      (let* ((out-x-v   (- (round (w module) 2)
                           (if scale-p (oa::om-zoom-scale-int-min 4 1 zoom) 4)))
             (out-y-v   (- (h module)
                           (if scale-p (oa::om-zoom-scale-int-min 9 1 zoom) 9)))
             (out-side-v (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8)))
        (om-add-subviews module
                         (first (setf (outframes module)
                                      (list (om-make-view 'outfleche
                                                          :position (om-make-point out-x-v out-y-v)
                                                          :size (om-make-point out-side-v out-side-v)
                                                          :help-spec "option-click to evalue or drag for connections"
                                                          :index 0))))))
      (let* ((tty-pos-v   (if scale-p
                              (om-make-point (oa::om-zoom-scale-int-min 1 0 zoom)
                                             (oa::om-zoom-scale-int-min 1 0 zoom))
                              (om-make-point 1 1)))
             (tty-size-log (om-subtract-points boxframex (om-make-point 3 11)))
             (tty-size-v   (if scale-p (oa::om-zoom-scale-point tty-size-log zoom) tty-size-log))
             (tty-font     (if scale-p (oa::om-zoom-scale-font *ombox-font* zoom) *ombox-font*)))
        (setf ttybox (om-make-dialog-item 'boxtype-iconview tty-pos-v tty-size-v " "
                                          :bg-color *patch-bg-color*
                                          :font tty-font))
        (when scale-p
          (setf (oa::om-zoom-logical-font ttybox) *ombox-font*)))
      (setf (iconview module) ttybox)
      (om-add-subviews module ttybox)
      (om-set-dialog-item-text ttybox (thestring self))
      (setf (frames self) (list module))
      (unless (frame-size self)
        (if scale-p
            (setf (frame-size self) boxframex)
            (setf (frame-size self) (om-view-size module))))
      (setf (name module) name)
      (add-box-resize module)
      module)))

(defmethod make-frame-from-callobj ((self OMBoxundefined))
  (let* ((zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (logical-size (om-make-point 66 18))
         (logical-pos  (frame-position self))
         (module-vsize (if scale-p (oa::om-zoom-scale-point logical-size zoom) logical-size))
         (module-vpos  (if (and scale-p logical-pos)
                           (oa::om-zoom-scale-point logical-pos zoom)
                           logical-pos))
         (module (om-make-view 'boxTypeFrame
                               :position module-vpos
                               :size module-vsize
                               :object self)))
    (let* ((tty-pos-v   (if scale-p
                            (om-make-point (oa::om-zoom-scale-int-min 1 0 zoom)
                                           (oa::om-zoom-scale-int-min 1 0 zoom))
                            (om-make-point 1 1)))
           (tty-size-log (om-subtract-points logical-size (om-make-point 2 3)))
           (tty-size-v   (if scale-p (oa::om-zoom-scale-point tty-size-log zoom) tty-size-log))
           (tty-font     (if scale-p (oa::om-zoom-scale-font *om-default-font1* zoom) *om-default-font1*))
           (ttyw (om-make-dialog-item 'undef-ttybox tty-pos-v tty-size-v "undefined"
                                      :font tty-font
                                      :help-spec "not yet defined box"
                                      :bg-color *undefbox-color*)))
      (when scale-p
        (setf (oa::om-zoom-logical-font ttyw) *om-default-font1*))
      (om-add-subviews module (setf (iconview module) ttyw)))
    (set-value (iconview module) 'undefined)
    (setf (frames self) (list module))
    (unless (frame-size self)
      (if scale-p
          (setf (frame-size self) logical-size)
          (setf (frame-size self) (om-view-size module))))
    (setf (name module) "undefined")
    module))

(defmethod make-frame-from-callobj ((self OMBoxcomment))
  "Cons a simple frame for the comment box 'self'."
  (let* ((zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (sizemodule (if (frame-size self) (frame-size self) (om-make-point 80 28)))
         (module-vsize (if scale-p (oa::om-zoom-scale-point sizemodule zoom) sizemodule))
         (logical-pos  (frame-position self))
         (module-vpos  (if (and scale-p logical-pos)
                           (oa::om-zoom-scale-point logical-pos zoom)
                           logical-pos))
         (module (om-make-view 'commentboxframe
                               :position module-vpos
                               :size module-vsize
                               :object self)))
    (let* ((cv-pos-v   (if scale-p
                           (om-make-point (oa::om-zoom-scale-int-min 3 0 zoom)
                                          (oa::om-zoom-scale-int-min 3 0 zoom))
                           (om-make-point 3 3)))
           (cv-size-log (om-subtract-points sizemodule (om-make-point 6 6)))
           (cv-size-v   (if scale-p (oa::om-zoom-scale-point cv-size-log zoom) cv-size-log))
           (base-font   (textstyle self))
           (scaled-font (if scale-p (oa::om-zoom-scale-font base-font zoom) base-font))
           (cv-instance (om-make-dialog-item 'commentview cv-pos-v cv-size-v
                                             (reference self)
                                             :font scaled-font)))
      (when scale-p
        (setf (oa::om-zoom-logical-font cv-instance) base-font))
      (om-add-subviews module (setf (iconview module) cv-instance)))
    (setf (frames self) (list module))
    (om-set-fg-color (iconview module) (textcolor self))
    (unless (frame-size self)
      (if scale-p
          (setf (frame-size self) sizemodule)
          (setf (frame-size self) (om-view-size module))))
    (setf (name module) "comment")
    (add-box-resize module)
    module))

(defun make-lisp-boxes (function patch)
  ;; ZOOM-CTX: propagate target zoom into the Apropos-created lispboxcall frame.
  (if (not (fboundp function))
      (dialog-message (string+ "no such function " (string function)))
      (let* ((new-call (omNG-make-new-lispboxcall function (om-make-point 20 20)
                                                  (mk-unique-name (panel patch) (string function))))
             (target (panel patch))
             (new-frame (oa::with-zoom-context-of target
                          (make-frame-from-callobj new-call))))
        (omG-add-element target new-frame))))

(defmethod make-outputs-of-frame ((self OMBoxPatch) module)
  "The outputs of the box are made from the OMOut boxes in the patch reference."
  (let* ((numouts (numouts self))
         (outsname (list+ (find-class-boxes (boxes (reference self)) 'OMTempOut)
                          (sort (find-class-boxes (boxes (reference self)) 'OMout)
                                '< :key 'indice)))
         (zoom    (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (io-size (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8))
         (off-x   (if scale-p (oa::om-zoom-scale-int 4 zoom) 4))
         (off-y   (if scale-p (oa::om-zoom-scale-int 9 zoom) 9)))
    (setf outsname (loop for item in outsname collect (get-frame-name item)))
    (loop for i from 0 to (- numouts 1) do
          (let ((thenewout (om-make-view (get-out-class self)
                                         :position (om-make-point
                                                    (- (* (+ i 1) (round (w module) (+ numouts 1))) off-x)
                                                    (- (h module) off-y))
                                         :size (om-make-point io-size io-size)
                                         :help-spec (nth i outsname)
                                         :index i)))
            (when (and (= i 0) (find-class-boxes (boxes (reference self)) 'OMtempout))
              (setf (iconID thenewout) 227))
            (push thenewout (outframes module))
            (om-add-subviews module thenewout)))))

(defmethod make-outputs-of-frame ((self OMBoxMaquette) module)
  "Cons a list of views which are the outputs of the box."
  (let* ((numouts (numouts self))
         (outsname (mapcar 'get-frame-name
                           (sort (find-class-boxes (boxes (reference self)) 'maq-OMout)
                                 '< :key 'indice)))
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
                                         :help-spec (if (= i 0) "maquette global output" (nth (1- i) outsname))
                                         :index i)))
            (when (= i 0) (setf (iconID thenewout) 227))
            (push thenewout (outframes module))
            (om-add-subviews module thenewout)))))

;;; ============================================================
;;; ombox: helper + OMBox make-frame + outputs (zoom-aware base)
;;; ============================================================

(defmethod get-frame-name ((self OMBox))
  (unless (function-without-name-p (reference self))
    (let ((thename (string (reference self))))
      (string-downcase thename))))

(defmethod def-icon-size ((self ombox))
  (if (function-without-name-p (reference self))
      (let ((icn (second (get&corrige-icon (icon self)))))
        (list (om-pict-width icn) (om-pict-height icn)))
      (or (spec-obj-icon-size (reference self))
          nil)))

(defmethod make-frame-from-callobj ((self OMBox))
  (let* ((icon (icon self))
         (iconsize (icon-sizes icon (def-icon-size self)))
         (name (if (frame-name self) (frame-name self) (get-frame-name self)))
         (boxnamefont *ombox-font*)
         (numouts (numouts self))
         (index 0)
         (size-name (round (get-name-size name boxnamefont)))
         (h-name (if name (+ 3 (om-string-h boxnamefont)) 0))
         (zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         input-frames module boxframex)
    (setf (inputs self) (update-inputs (reference self) (inputs self)))
    (setf boxframex (if (frame-size self)
                        (om-point-h (frame-size self))
                        (apply #'max (list (first iconsize)
                                           (* (boxinputs-sizefactor self) numouts)
                                           (* (boxinputs-sizefactor self) (length (inputs self)))
                                           size-name))))
    (let* ((module-logical-size (om-make-point boxframex (+ (second iconsize) 19 h-name)))
           (module-logical-pos  (frame-position self))
           (module-vsize (if scale-p (oa::om-zoom-scale-point module-logical-size zoom) module-logical-size))
           (module-vpos  (if (and scale-p module-logical-pos)
                             (oa::om-zoom-scale-point module-logical-pos zoom)
                             module-logical-pos))
           (io-size-v (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8))
           (io-y-v    (if scale-p (oa::om-zoom-scale-int-min 1 0 zoom) 1)))
      (setf input-frames
            (mapcar #'(lambda (input)
                        (let ((docstr (doc-string input)))
                          (setf index (+ index 1))
                          (let* ((x-log (- (* index (round boxframex (+ (length (inputs self)) 1))) 4))
                                 (x-v   (if scale-p (oa::om-zoom-scale-int x-log zoom) x-log)))
                            (om-make-view (or (spec-input-frame self (- index 1))
                                              (get-input-class-frame self))
                                          :object input
                                          :help-spec (string+ "<" (string-downcase (name input))
                                                              ">" (if (and docstr (not (string-equal docstr "")))
                                                                      (string+ " " (doc-string input)) ""))
                                          :size (om-make-point io-size-v io-size-v)
                                          :position (om-make-point x-v io-y-v)))))
                    (inputs self)))
      (setq module (om-make-view (get-frame-class self)
                                 :position module-vpos
                                 :size module-vsize
                                 :object self
                                 :subviews input-frames))
      (setf (inputframes module) input-frames)
      (make-outputs-of-frame self module)
      (setf (outframes module) (reverse (outframes module)))
      (setf (name module) name)
      (setf (frames self) (list module))
      (setf (oa::om-zoom-logical-size module) module-logical-size)
      (when module-logical-pos
        (setf (oa::om-zoom-logical-pos module) module-logical-pos))
      (let* ((iw-v (if scale-p (oa::om-zoom-scale-int-min (first iconsize) 1 zoom) (first iconsize)))
             (ih-v (if scale-p (oa::om-zoom-scale-int-min (second iconsize) 1 zoom) (second iconsize)))
             (ix-v (round (- (om-point-h module-vsize) iw-v) 2))
             (iy-v (if scale-p (oa::om-zoom-scale-int-min 10 0 zoom) 10)))
        (om-add-subviews module (setf (iconView module)
                                      (om-make-view (get-icon-box-class self)
                                                    :iconID icon
                                                    :help-spec (get-box-documentation self)
                                                    :size (om-make-point iw-v ih-v)
                                                    :position (om-make-point ix-v iy-v))))
        (when name
          (let* ((scaled-font (if scale-p (oa::om-zoom-scale-font boxnamefont zoom) boxnamefont))
                 (real-text-h (max 1 (om-string-h scaled-font)))
                 (nw-v (if scale-p (oa::om-zoom-scale-int-min size-name 1 zoom) size-name))
                 (nh-v (+ 3 real-text-h))
                 (nx-v (+ ix-v (round (- iw-v nw-v) 2)))
                 (icon-bottom-v (+ iy-v ih-v))
                 (ny-v (- icon-bottom-v 1))
                 (nameview-instance
                   (om-make-dialog-item 'box-dialog-name
                                        (om-make-point nx-v ny-v)
                                        (om-make-point nw-v nh-v)
                                        name
                                        :value name
                                        :font scaled-font
                                        :help-spec (get-documentation self))))
            (when scale-p
              (setf (oa::om-zoom-logical-font nameview-instance) boxnamefont))
            (om-add-subviews module (setf (nameView module) nameview-instance))))
        (when (allow-lock self)
          (add-lock-button module (allow-lock self)))
        (add-box-resize module)
        module))))

(defmethod make-outputs-of-frame ((self OMBox) module)
  (let* ((numouts (numouts self))
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
                                         :help-spec (get-output-text self i)
                                         :index i)))
            (push thenewout (outframes module))
            (om-add-subviews module thenewout)))))

;;; ============================================================
;;; temporalboxes: TemporalBox make-frame + dead-reference ctx
;;; ============================================================

(defmethod make-frame-from-callobj ((self TemporalBox))
  (let* ((numouts (numouts self))
         (numins (length (inputs self)))
         outsname module
         (zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (io-side-v (if scale-p (oa::om-zoom-scale-int-min 8 1 zoom) 8)))
    (setq module (om-make-view (get-frame-class self)
                               :position (om-make-point 0 0)
                               :help-spec ""
                               :size (om-make-point 0 0)
                               :object self))
    (unless (zerop numouts)
      (if (ominstance-p (reference self))
          (setf outsname (list "self"))
          (setf outsname (loop for item in (sort (find-class-boxes (boxes (reference self)) 'OMout)
                                                 '< :key 'indice)
                               collect (get-frame-name item))))
      (loop for i from 0 to (- numouts 1) do
            (let ((thenewout (om-make-view (get-out-class self)
                                           :position (om-make-point 0 0)
                                           :size (om-make-point io-side-v io-side-v)
                                           :help-spec (nth i outsname)
                                           :index i)))
              (setf (outframes module) (list+ (outframes module) (list thenewout)))
              (om-add-subviews module thenewout))))
    (loop for input in (inputs self)
          for i from 0 to (- numins 1) do
          (let ((newinput (om-make-view (get-input-class-frame self)
                                        :object input
                                        :help-spec (string+ "<" (string-downcase (name input))
                                                            "> " (doc-string input))
                                        :size (om-make-point io-side-v io-side-v)
                                        :position (om-make-point 0 0))))
            (setf (inputframes module) (list+ (inputframes module) (list newinput)))
            (om-add-subviews module newinput)))
    (setf (iconview module) (pictu self))
    (setf (name module) (name self))
    (setf (frames self) (list module))
    (when (allow-lock self)
      (add-lock-button module (allow-lock self)))
    (add-box-resize module)
    module))

(defmethod dead-reference ((self TemporalBox))
  (let* ((new-patch (make-instance 'OMPatchAbs :name (name self) :icon 210))
         (tempobj (omNG-make-tempobj new-patch (om-make-point (offset self) (posy self)) (name self)))
         (comment (omNG-make-new-boxcall 'comment (om-make-point 150 180) "comment")))
    (add-temp-boxes new-patch)
    (setf (reference comment) "The reference of this box has been deleted.")
    (setf (frame-size comment) (om-make-point 200 50))
    (setf (textstyle comment) (om-make-font "arial" 16))
    (omNG-add-element new-patch comment)
    (setf (slot-value tempobj 'extend) (extend self))
    (setf (slot-value tempobj 'sizey)  (sizey self))
    (setf (show-name tempobj) (show-name self))
    (setf (pictu tempobj) (copy-picture (pictu self)))
    (when (frames self)
      (let ((container (om-view-container (car (frames self)))))
        (real-make-delete-before container (frames self))
        (omg-remove-element container (car (frames self)))
        (omG-add-element container
                         (oa::with-zoom-context-of container
                           (make-frame-from-callobj tempobj)))))
    tempobj))

;;; ===== abspatch.lisp overrides =====

(defmethod internalize-patch ((self patchboxframe))
  "A blue patch becomes a red patch."
  (when (subtypep (type-of (object self)) 'OMBoxPatch)
    (let* ((container (om-view-container self))
           (object (object self))
           (newpatch (patch2abs (reference object)))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (set-box-to-inputs (inputs newbox) newbox)
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections object item newbox))
      (omg-remove-element container self)
      (setf frame (oa::with-zoom-context-of container
                    (make-frame-from-callobj newbox)))
      (omg-add-element container frame)
      (compile-patch newpatch)
      (update-graphic-connections frame (get-elements (object container))))))

(defmethod externalize ((self patchboxframe) newpatch)
  "A red patch becomes a blue patch."
  (when (subtypep (type-of (object self)) 'OMBoxAbsPatch)
    (let* ((container (om-view-container self))
           (object (object self))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (set-box-to-inputs (inputs newbox) newbox)
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections object item newbox))
      (setf frame (oa::with-zoom-context-of container
                    (make-frame-from-callobj newbox)))
      (omg-remove-element container self)
      (compile-patch newpatch)
      (omg-add-element container frame)
      (update-graphic-connections frame (get-elements (object container)))
      (omng-save newpatch))))

(defmethod internalize-patch ((self maquetteframe))
  "A blue maquette becomes a red one."
  (when (equal (type-of (object self)) 'OMBoxMaquette)
    (let* ((container (om-view-container self))
           (object (object self))
           (newpatch (maq2abs (reference object)))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections object item newbox))
      (omg-remove-element container self)
      (setf frame (oa::with-zoom-context-of container
                    (make-frame-from-callobj newbox)))
      (omg-add-element container frame)
      (update-graphic-connections frame (get-elements (object container))))))

(defmethod externalize ((self maquetteframe) newpatch)
  "A red maquette becomes a blue one."
  (when (equal (type-of (object self)) 'OMBoxAbsmaq)
    (let* ((container (om-view-container self))
           (object (object self))
           (newbox (omNG-make-new-boxcall newpatch
                                          (frame-position object)
                                          (mk-unique-name container (name self))))
           frame conec-to-me)
      (setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (frame-size newbox) (frame-size object))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections object item newbox))
      (setf frame (oa::with-zoom-context-of container
                    (make-frame-from-callobj newbox)))
      (omg-remove-element container self)
      (omg-add-element container frame)
      (update-graphic-connections frame (get-elements (object container)))
      (omng-save newpatch))))

;;; ===== boxwithpatch.lisp overrides =====

(defclass patchForBox (OMPatch)
   ((box :initform nil :accessor box)
    (w-zoom :accessor w-zoom :initform 1.0))
   (:documentation "Some boxes as omloop have a patch that define the meaning of the box."))

(defmethod do-add-one-input-extra ((self box-with-patch))
  "When you add one input to the box you must add one input to the patch too."
  (let* ((container (editorframe (patch self)))
         (i (- (length (inputs self)) 1))
         (input (make-new-patch-input (string+ "input" (format () "~D" i))
                                      i (om-make-point (+ 5 (* i 30)) 40))))
    (if container
        (omG-add-element container
                         (oa::with-zoom-context-of container
                           (make-frame-from-callobj input)))
        (omNG-add-element (patch self) input))
    t))

(defmethod get-win-zoom ((self patchForBox)) (w-zoom self))

(defmethod set-win-zoom ((self patchForBox) zoom)
  (setf (w-zoom self) zoom))

(defmethod get-win-zoom ((self OMPatchAbs)) (w-zoom self))

(defmethod set-win-zoom ((self OMPatchAbs) zoom)
  (setf (w-zoom self) zoom))

(defmethod omNG-save ((self box-with-patch) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (when values? (omNG-save (value self) values?)))
         (boxes (boxes (patch self))) pictlist)
    (setf pictlist (omng-save (pictu-list (patch self))))
    (when (editorframe (patch self))
      (let ((p (panel (editorframe (patch self)))))
        (when (and p (typep p 'oa::om-scroller))
          (set-win-zoom (patch self) (oa::om-zoom-of p)))))
    (let ((saved-zoom (w-zoom (patch self))))
      `(let ((newbox (om-load-boxwithed1 'box-with-win ,(name self) ',(reference self) ',inputs
                                         ,(om-save-point (frame-position self))
                                         ,(om-save-point (frame-size self)) ,value ,(allow-lock self)
                                         ,(omNG-save boxes) ',(mk-connection-list boxes) ,(numouts self)
                                         ,(frame-name self) ,pictlist)))
         (when (and (patch newbox) (numberp ,saved-zoom) (/= ,saved-zoom 1.0))
           (setf (w-zoom (patch newbox)) ,saved-zoom))
         newbox))))

;;; ===== omloop.lisp overrides =====

(defparameter *om-loop-iterator-refs*
  '(forloop whileloop listloop onlistloop)
  "Loop-iterator constructors hosted by the iterators block in the zoom bar.
   Boxes built from these refs are placed along the TOP of the visible viewport,
   matching the patch in-box placement.")

(defparameter *om-loop-accumulator-refs*
  '(counter sum minim maxi listing accumulator)
  "Loop-accumulator constructors hosted by the accumulators block in the zoom bar.
   Boxes built from these refs are placed along the BOTTOM of the visible viewport,
   matching the patch out-box placement.")

(defun om-loop-add-box (panel ref name)
  "Add a new loop construct box of class REF to PANEL with default name NAME.
   Placement rule:
     REF in *om-loop-iterator-refs*    -> top of viewport (next to inputs).
     REF in *om-loop-accumulator-refs* -> bottom of viewport (next to outputs).
   In a normal patch inputs occupy the top row and outputs the bottom row,
   so the panel never has two-row collisions. In a loopPanel iterators share
   the top row with inputs and accumulators share the bottom row with outputs;
   to avoid overlap the iterator x-offset starts AFTER the existing inputs
   (and symmetrically for accumulators vs outputs)."
  (multiple-value-bind (sx sy vw vh) (oa::om-zoom-viewport-logical panel)
    (declare (ignore vw))
    (let* ((iter?   (member ref *om-loop-iterator-refs* :test 'eq))
           (accum?  (member ref *om-loop-accumulator-refs* :test 'eq))
           (subs    (get-subframes panel))
           (num-in  (length (list+ (find-class-boxes subs 'selfInFrame)
                                   (find-class-boxes subs 'InFrame))))
           (num-out (length (list+ (find-class-boxes subs 'tempOutFrame)
                                   (find-class-boxes subs 'outFrame))))
           (i-iter  (length (remove-if-not
                             #'(lambda (f) (typep (object f) 'loopIterators))
                             subs)))
           (i-accum (length (remove-if-not
                             #'(lambda (f) (typep (object f) 'acumboxes))
                             subs)))
           (x       (cond
                     (iter?  (+ sx 10 (* num-in  50) (* i-iter  60)))
                     (accum? (+ sx 10 (* num-out 50) (* i-accum 60)))
                     (t      (+ sx 20 (* (+ i-iter i-accum) 60)))))
           (y       (cond
                     (accum? (+ sy (max 10 (- vh 80))))
                     (t      (+ sy 10))))
           (pos     (om-make-point x y)))
      (omG-add-element panel
                       (oa::with-zoom-context-of panel
                         (make-frame-from-callobj
                          (omNG-make-new-boxcall (fdefinition ref)
                                                 pos
                                                 (mk-unique-name panel name))))))))

(defun om-loop-make-button (class panel icon help ref name)
  "Construct one loop construct button.
   CLASS is 'om-icon-button (ICON is :icon1 string) or 'button-icon (ICON is :iconID int)."
  (if (eq class 'om-icon-button)
      (om-make-view 'om-icon-button
                    :icon1 icon
                    :size (om-make-point 24 24)
                    :help-spec help
                    :action #'(lambda (item) (declare (ignore item))
                                (om-loop-add-box panel ref name)))
      (om-make-view 'button-icon
                    :iconID icon
                    :size (om-make-point 24 24)
                    :help-spec help
                    :action #'(lambda (item) (declare (ignore item))
                                (om-loop-add-box panel ref name)))))

(defmethod add-input ((self looppanel) position)
  "On a loopPanel the top row hosts BOTH patch inputs and loop iterators.
   Shift past iterators by the same 60-px stride used by om-loop-add-box,
   so the two groups never overlap regardless of creation order."
  (if position
      (call-next-method)
      (multiple-value-bind (sx sy vw vh) (oa::om-zoom-viewport-logical self)
        (declare (ignore vw vh))
        (let* ((subs   (get-subframes self))
               (num-in (length (list+ (find-class-boxes subs 'selfInFrame)
                                      (find-class-boxes subs 'InFrame))))
               (i-iter (length (remove-if-not
                                #'(lambda (f) (typep (object f) 'loopIterators))
                                subs)))
               (pos    (om-make-point (+ sx 10 (* i-iter 60) (* num-in 50))
                                      (+ sy 10))))
          (call-next-method self pos)))))

(defmethod om-get-menu-context ((self loopPanel))
  (let ((pos (om-mouse-position self)))
    (flet ((mk-leaf (label ref unique-name)
             (om-new-leafmenu label
                              #'(lambda ()
                                  (omG-add-element
                                   self
                                   (oa::with-zoom-context-of self
                                     (make-frame-from-callobj
                                      (omNG-make-new-boxcall (fdefinition ref)
                                                             pos
                                                             (mk-unique-name self unique-name)))))))))
      (append
       (list
        (list
         (om-make-menu "Iterators"
                       (list
                        (mk-leaf "For"           'forloop    "for")
                        (mk-leaf "While"         'whileloop  "while")
                        (mk-leaf "List Loop"     'listloop   "inlist")
                        (mk-leaf "On List Loop"  'onlistloop "onlist")))
         (om-make-menu "Accumulators"
                       (list
                        (mk-leaf "Collect"         'listing     "collect")
                        (mk-leaf "Sum"             'sum         "sum")
                        (mk-leaf "Count"           'counter     "count")
                        (mk-leaf "Min"             'minim       "min")
                        (mk-leaf "Max"             'maxi        "max")
                        (mk-leaf "General Accum." 'accumulator "accum")))))
       (call-next-method)))))

;;; ===== maquette-markers.lisp override =====

;;; LOGICAL geometry on the OBJECT (frame-position) is unchanged.
;; ZOOM-SCALE: scale temp-marker frame size and position with zoom context.

(defmethod make-frame-from-callobj ((self temp-marker))
  "Cons a simple frame for a temp-marker instance."
  (let* ((zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (logical-size (om-make-point 8 10))
         (logical-pos  (frame-position self))
         (module-vsize (if scale-p (oa::om-zoom-scale-point logical-size zoom) logical-size))
         (module-vpos  (if (and scale-p logical-pos)
                           (oa::om-zoom-scale-point logical-pos zoom)
                           logical-pos))
         (module (om-make-view 'markerframe
                               :position module-vpos
                               :size  module-vsize
                               :object self)))
    (om-add-subviews module (setf (iconView module)
                                  (om-make-view 'bandera
                                                :iconID (icon self)
                                                :help-spec (name self)
                                                :size module-vsize
                                                :position (om-make-point 0 0))))
    (setf (name module) (name self))
    (setf (frames self) (list module))
    module))

;;; ===== maquette-rulers.lisp override =====

(defmethod add-new-marquer ((self maq-ruler) where)
  (let* ((x (om-point-h (pixel2point (assoc-view self) where)))
         (target (assoc-view self)))
    (omg-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj
                        (omNG-make-new-marker (om-make-point (om-point-h where)
                                                             (- (h target) 26))
                                              (mk-unique-name target "marker") x))))))

(defmethod add-new-marquer ((self metric-ruler) where)
  (let* ((x (om-point-h (pixel2point (assoc-view self) where)))
         (target (assoc-view self)))
    (omg-add-element target
                     (oa::with-zoom-context-of target
                       (make-frame-from-callobj
                        (omNG-make-new-marker (om-make-point (om-point-h where)
                                                             (- (h target) 26))
                                              (mk-unique-name target "marker") x))))))

;;; ===== connections.lisp override =====

(defmethod draw-connection ((self c-connection) val)
  ;; ZOOM-SCALE: pen / mark / offset scale with the box's zoom.
  (let* ((thepoints (copy-list (get-graph-points self)))
         (prim (pop thepoints))
         (sel? (selected? self))
         (color (if (zerop (ccolor self)) *om-black-color* (nth (- (ccolor self) 1) *16-color-list*)))
         (zoom (oa::om-zoom-effective (thebox self)))
         (base-pen (if sel? 2 1))
         (pen      (max 1 (round (* base-pen zoom))))
         (mark     (max 1 (round (* 4 zoom))))
         (mark-off (max 0 (round (* 2 zoom)))))
    (om-with-focused-view (connection-container (thebox self))
      (om-with-fg-color nil color
        (om-with-line-size pen
          (if val
              (loop while thepoints do
                    (om-draw-line (om-point-h prim) (om-point-v prim)
                                  (om-point-h (car thepoints)) (om-point-v (car thepoints))
                                  :erasable (equal val 'redraw))
                    (setf prim (pop thepoints))
                    (when thepoints
                      (if (member prim (point-sel self))
                          (om-fill-rect (- (om-point-h prim) mark-off) (- (om-point-v prim) mark-off) mark mark :erasable (equal val 'redraw))
                          (when (and sel? (not (= 1 *connection-style*)))
                            (om-draw-rect (- (om-point-h prim) mark-off) (- (om-point-v prim) mark-off) mark mark :erasable (equal val 'redraw))))))
              #-(and cocoa lispworks8)
              (loop while thepoints do
                    (om-erase-line (om-point-h prim) (om-point-v prim)
                                   (om-point-h (car thepoints)) (om-point-v (car thepoints)))
                    (setf prim (pop thepoints))
                    (when thepoints
                      (if (member prim (point-sel self))
                          (om-erase-rect-content (- (om-point-h prim) 2) (- (om-point-v prim) 2) 4 4)
                          (when sel?
                            (om-erase-rect (- (om-point-h prim) 2) (- (om-point-v prim) 2) 4 4)))))))))))

;;; ===== maq-in-out.lisp override =====

(defmethod add-input ((self maquettepanel) pos)
  (let* ((boxes (get-subframes self))
         (i (length (find-class-boxes boxes 'maqinFrame)))
         (in (make-new-patch-input (mk-unique-name self "input")
                                   i pos 245 'maq-omin))
         (maqpos (get-offset/posy-from-pixel self pos))
         frame)
    (setf (posy in) (om-point-v maqpos))
    (setf (offset in) (om-point-h maqpos))
    ;; ZOOM-CTX: propagate maquette zoom to new input frame.
    (setf frame (oa::with-zoom-context-of self
                  (make-frame-from-callobj in)))
    (omG-add-element self frame)))

(defmethod add-output ((self maquettepanel) pos)
  (let* ((boxes (get-subframes self))
         (i (length (find-class-boxes boxes 'maqoutFrame)))
         (out (make-new-output (mk-unique-name self "output")
                               i pos 245 'maq-omout))
         (maqpos (get-offset/posy-from-pixel self pos))
         frame)
    (setf (posy out) (om-point-v maqpos))
    (setf (offset out) (om-point-h maqpos))
    ;; ZOOM-CTX: propagate maquette zoom to new output frame.
    (setf frame (oa::with-zoom-context-of self
                  (make-frame-from-callobj out)))
    (omG-add-element self frame)))

;;; ===== basicboxes.lisp overrides =====

(defmethod* addBox2Maquette ((self temporalbox) (maquette ommaquette))
  :icon 328
  :initvals '(nil nil)
  :indoc '("temporal object" "a maquette")
  :doc "Adds <self> in <maquette>."
  (let ((newbox (clone self)))
    (if (not (editorframe maquette))
        (omng-add-element maquette newbox)
        (let ((ef (editorframe maquette)))
          (omg-add-element ef
                           (oa::with-zoom-context-of ef
                             (make-frame-from-callobj newbox)))))
    self))

(defmethod* put-in-maq ((self temporalbox) (maquette ommaquette))
  :icon 239
  :initvals '(nil nil)
  :indoc '("the temporal object" "the maquette")
  :doc "put in the maquette"
  (if (not (editorframe maquette))
      (omng-add-element maquette self)
      (let ((ef (editorframe maquette)))
        (omg-add-element ef
                         (oa::with-zoom-context-of ef
                           (make-frame-from-callobj self)))))
  self)

;;; ===== send-receive.lisp overrides =====

(defmethod draw-send-box ((container t) object)
  ;; ZOOM-SCALE: index label offsets scale with container zoom.
  (let* ((zoom (oa::om-zoom-effective container))
         (offx (max 1 (round (* 7 zoom))))
         (offy (max 1 (round (* 12 zoom)))))
    (om-with-focused-view container
      (om-with-font (oa::om-current-default-font1b container)
        (om-draw-string (- (round (w container) 2) offx) offy
                        (format () "~2D" (indice object)))))))

(defmethod add-send ((self patchpanel) position)
  (when (add-send-enabled self 'send)
    (let* ((boxes (get-subframes self))
           (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'sendFrame))))
           (pos (or position (om-make-point (+ 5 (* i 50)) 240)))
           (newsend (make-new-send (mk-unique-name self "send") i pos t)))
      (omG-add-element self
                       ;; ZOOM-CTX: propagate panel zoom to new send frame.
                       (oa::with-zoom-context-of self
                         (make-frame-from-callobj newsend)))
      (set-field-size self))))

(defmethod OMGMoveObject ((self sendFrame) new-position)
  "If shift-key is down when drag self it do not move but it create and slot box."
  (if (or (om-shift-key-p) (shift-key-p *OM-drag&drop-handler*))
      (let* ((target (om-view-container self)) newobj)
        (when target
          (setf newobj (eval (omng-replicate (object self) (borne-position new-position))))
          (omG-add-element target
                           ;; ZOOM-CTX: propagate target zoom to receive frame from shift-drag.
                           (oa::with-zoom-context-of target
                             (make-frame-from-callobj newobj)))))
      (call-next-method)))

(defmethod draw-send-receive-box ((container t) object)
  ;; ZOOM-SCALE: index label offsets scale with container zoom.
  (let* ((zoom (oa::om-zoom-effective container))
         (offx (max 1 (round (* 7 zoom))))
         (offy (max 1 (round (* 12 zoom)))))
    (om-with-focused-view container
      (om-with-font (oa::om-current-default-font1b container)
        (om-draw-string (- (round (w container) 2) offx) offy
                        (format () "~2D" (indice object)))))))

;;; ===== picture.lisp (kernel) override =====

(defmethod draw-pict-patch ((self patch-picture) view)
  (let* ((zoom     (oa::om-zoom-of view))
         (vis-pos  (if (= zoom 1.0) (pict-pos self)  (oa::om-zoom-scale-point (pict-pos self)  zoom)))
         (vis-size (if (= zoom 1.0) (pict-size self) (oa::om-zoom-scale-point (pict-size self) zoom)))
         (x0       (om-point-h vis-pos))
         (y0       (om-point-v vis-pos))
         (w        (om-point-h vis-size))
         (h        (om-point-v vis-size))
         (pen      (max 1 (round (* 2 zoom)))))
    (if (thepict self)
        (om-draw-picture view (thepict self) :pos vis-pos :size vis-size)
        (draw-lost-picture self view x0 y0 w h))
    (loop for o in (extraobjs self) do (draw-pict-extraobj view o vis-pos vis-size))
    (when (selected-p self)
      (om-with-line-size pen
        (om-draw-rect (+ 1 x0) (+ 1 y0) (- w 2) (- h 2))))))

;;; ===== graphictools.lisp overrides =====

(defmethod om-view-click-handler ((self c-resize-box) where)
  (declare (ignore where))
  (let* ((boxframe (get-box-frame self))
         (theeditor (editor (om-view-container boxframe)))
         (panel (om-view-container (get-box-frame self)))
         (dp (om-subtract-points (om-view-size self) where))
         (rx (x boxframe))
         (ry (y boxframe)))
    (declare (ignore rx ry))
    (when (text-view theeditor)
      (exit-from-dialog (text-view theeditor)
                        (om-dialog-item-text (text-view theeditor))))
    (om-init-motion-click self
                          where
                          :motion-draw #'(lambda (view pp1 pp2)
                                           (declare (ignore pp1))
                                           (let* ((p1 (om-view-position boxframe))
                                                  (p2 pp2)
                                                  (x (om-point-x p1)) (y (om-point-y p1))
                                                  (w (+ (- (om-point-x p2) (om-point-x p1)) (om-point-x dp)))
                                                  (h (+ (- (om-point-y p2) (om-point-y p1)) (om-point-y dp))))
                                             (om-with-fg-color view (om-make-color-alpha 0 0 0 0.1)
                                               (om-fill-rect x y w h))
                                             (om-with-fg-color view *om-blue-color*
                                               (om-with-line '(2 2) (om-draw-rect x y w h :pensize 2)))))
                          :draw-pane panel :display-mode nil
                          :release-action #'(lambda (view pp1 pp2)
                                              (declare (ignore view))
                                              (let* ((z (oa::om-zoom-effective boxframe))
                                                     (new-vis-size (om-add-points (om-view-size boxframe)
                                                                                  (om-subtract-points pp2 pp1)))
                                                     (new-log-size (if (= z 1.0)
                                                                       new-vis-size
                                                                       (oa::om-zoom-unscale-point new-vis-size z))))
                                                (change-boxframe-size boxframe new-log-size))))))

;;; ===== filebox.lisp overrides =====

(defmethod add-window-buttons ((self openfilePanel))
  "Add iterator and accumulator buttons to the patch."
  (call-next-method)
  (om-add-subviews self
                   (om-make-view 'button-icon
                                 :iconID 649
                                 :position (om-make-point 440 5)
                                 :size (om-make-point 24 24)
                                 :help-spec "File I/O"
                                 :action
                                 #'(lambda (item) (declare (ignore item))
                                     (omG-add-element
                                      self
                                      (oa::with-zoom-context-of self
                                        (make-frame-from-callobj
                                         (omNG-make-new-boxcall (fdefinition 'StreamFile)
                                                                (om-make-point 50 30)
                                                                (mk-unique-name self "StreamFile")))))))))

;;; ============================================================
;;; tempobjframe: resize/lock chrome scales with make-frame zoom context;
;;; miniview font scales with ancestor zoom; internalize/externalize
;;; propagate zoom into replacement frames.
;;; ============================================================

(defmethod add-box-resize ((self tempobjframe))
  (let* ((zoom    (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (size-v  (if scale-p (max 1 (round (* 8 zoom))) 8))
         (off-v   size-v))
    (om-add-subviews self
                     (setf (resize-box self)
                           (om-make-view 'maq-c-resize-box
                                         :size (om-make-point size-v size-v)
                                         :position (om-make-point (- (w self) off-v)
                                                                  (- (h self) off-v)))))))

(defmethod score-draw-mini-view ((self tempobjframe) value)
  (ignore-errors
   (let* ((zoom (oa::om-zoom-of self))
          (fs   (max 1 (round (* (mv-font-size value) zoom)))))
     (if (equal *minipict-mode* :pianoroll)
         (draw-mini-piano-roll value self (mv-view-size value self))
         (if (minipict self)
             (let ((x0 (initx self)) (y0 (inity self))
                   (pictsize (om-get-picture-size (minipict self))))
               (om-draw-picture self (minipict self) :pos (om-make-point x0 y0) :size pictsize))
             (om-with-focused-view self
               (draw-mini-obj value self fs (mv-view-size value self))))))))

(defmethod add-lock-button ((self tempobjframe) &optional (mode "x"))
  "Mode lambda 'l' and reference 'o' are forbidden for temporal boxes."
  (when (find mode (allowed-lock-modes (object self)) :test 'string-equal)
    (let* ((zoom    (or oa::*make-frame-zoom-context* (oa::om-zoom-effective self) 1.0))
           (scale-p (and (numberp zoom) (/= zoom 1.0)))
           (size-v  (if scale-p (max 1 (round (* 10 zoom))) 10)))
      (setf (lock-button self)
            (om-make-view 'lock-button
                          :IconID (get-icon-lock mode)
                          :size (om-make-point size-v size-v)
                          :position (om-make-point 0 0)
                          :owner self
                          :action #'(lambda (item)
                                      (let* ((modes (allowed-lock-modes (object self)))
                                             (mpos (position (mode item) modes :test 'string-equal))
                                             (newmode (nth (mod (1+ mpos) (length modes)) modes)))
                                        (setf (mode item) newmode
                                              (iconID item) (get-icon-lock newmode))
                                        (setf (allow-lock (object self)) newmode))))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) mode)))

(defmethod internalize-patch ((self tempobjframe))
  "A blue patch becomes a red patch."
  (when (or (equal (type-of (reference (object self))) 'OMPatch)
            (equal (type-of (reference (object self))) 'OMMaquette))
    (let* ((container (om-view-container self))
           (object (object self))
           newpatch newbox frame conec-to-me)
      (setf newpatch (if (maquette-p (reference object))
                         (maq2abs (reference object))
                         (patch2abs (reference object))))
      (setf newbox (omNG-make-tempobj newpatch (om-view-position self) (name self)))
      ;(setf (frame-position newbox) (borne-position (frame-position object)))
      (setf (offset newbox) (offset object))
      (setf (posy newbox) (posy object))
      (setf (show-name newbox) (show-name object))
      (setf (pictu newbox) (copy-picture (pictu object)))
      (setf (extend newbox) (extend object))
      (setf (strech-fact newbox) (strech-fact object))
      (setf (sizey newbox) (sizey object))
      ;(setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (set-box-to-inputs (inputs newbox) newbox)
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do (change-conections object item newbox))
      (omg-remove-element container self)
      (setf frame (let ((oa::*make-frame-zoom-context*
                         (and (typep container 'om-scroller) (oa::om-zoom-of container))))
                    (make-frame-from-callobj newbox)))
      (omg-add-element container frame)
      (compile-patch newpatch)
      (update-graphic-connections frame (get-elements (object container))))))

(defmethod externalize ((self tempobjframe) newpatch)
  "A red patch becomes a blue patch."
  (when (or (equal (type-of (reference (object self))) 'OMPatchAbs)
            (equal (type-of (reference (object self))) 'OMMaqAbs))
    (let* ((container (om-view-container self))
           (object (object self))
           (newbox (omNG-make-tempobj newpatch (om-view-position self) (name self)))
           frame conec-to-me)
      ;(setf (frame-position newbox) (borne-position (frame-position object)))
      ;(setf (frame-size newbox) (frame-size object))
      (setf (offset newbox) (offset object))
      (setf (posy newbox) (posy object))
      (setf (sizey newbox) (sizey object))
      (setf (strech-fact newbox) (strech-fact object))
      (setf (extend newbox) (extend object))
      (setf (show-name newbox) (show-name object))
      (setf (pictu newbox) (copy-picture (pictu object)))
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do (change-conections object item newbox))
      (setf frame (let ((oa::*make-frame-zoom-context*
                         (and (typep container 'om-scroller) (oa::om-zoom-of container))))
                    (make-frame-from-callobj newbox)))
      (omg-remove-element container self)
      (compile-patch newpatch)
      (omg-add-element container frame)
      (update-graphic-connections frame (get-elements (object container)))
      (omng-save newpatch))))

;;; ============================================================
;;; Dialog-item boxes (DIEditorframe + d-i-box widgets):
;;; frame creation stamps logical pos/size/font; chrome offsets
;;; (resize, lock, drag-region, outfleche, active border) scale
;;; with zoom; update-di-size on every widget kind respects ancestor
;;; container zoom.
;;; ============================================================

(defmethod make-frame-from-callobj ((self OMDIebox))
  "Make a simple frame for the editor factory 'self'."
  (let ((name (string-downcase (name self)))
        (defsize (get-boxsize self))
        (numouts (numouts self))
        (numins (length (inputs self)))
        (index 0)
        (module (om-make-view (get-frame-class self)
                              :position (frame-position self)
                              :object self)))
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
    (loop for input-f in (inputframes module) do (om-add-subviews module input-f))
    (make-outputs-from-names self (value self) module)
    (setf (iconview module) (value self))
    (om-add-subviews module (iconview module))
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

(defmethod oa::om-zoom-resolve-touch-target ((pane DIEditorframe) x y)
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

(defmethod om-show-tooltip ((self input-funboxframe) &optional (remove nil) (short nil))
  (unless (dieditorframe-p (om-view-container self))
    (call-next-method)))

(defmethod om-view-mouse-enter-handler :after ((self outfleche))
  (let ((parent (om-view-container self)))
    (when (and parent
               *mag-in-out*
               (dieditorframe-p parent))
      (let* ((parent-h (om-height parent))
             (own-h    (om-point-y (om-view-size self)))
             (cur-pos  (om-view-position self)))
        (when (> (+ (om-point-y cur-pos) own-h) parent-h)
          (om-set-view-position self
                                (om-make-point (om-point-x cur-pos)
                                               (- parent-h own-h)))
          #+win32 (oa::om-redraw-pinboard-object self))))))

(defmethod add-lock-button ((self DIEditorframe) &optional (mode "x"))
  "Add a lock button, if the box referenced by 'self' allow it."
  (when (allow-lock-button (object self))
    (setf (lock-button self) (make-lock-button self mode))
    (let* ((zoom (or oa::*make-frame-zoom-context* (oa::om-zoom-effective self) 1.0))
           (scale-p (and (numberp zoom) (/= zoom 1.0)))
           (off-y (if scale-p (round (* 8 zoom)) 8)))
      (om-set-view-position (lock-button self) (om-make-point 0 off-y)))
    (om-add-subviews self (lock-button self))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) mode)))

(defmethod centre-icon ((self DIEditorframe))
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

(defmethod make-drag-region ((self DIEditorframe) region x0 y0 view)
  (declare (ignore view))
  (let* ((zoom (oa::om-zoom-effective self))
         (off  (round (* 16 zoom)))
         (x    (- (x self) x0))
         (y    (- (y self) y0)))
    (om-set-rect-region region x y (+ x (w self)) (- (+ y (h self)) off)))
  region)

(defmethod om-draw-contents ((self dieditorframe))
  (call-next-method)
  (when (active-mode self)
    (let* ((zoom (oa::om-zoom-effective self))
           (mx   (max 1 (round (* 1 zoom))))
           (my   (round (* 8 zoom)))
           (dx   (round (* 3 zoom)))
           (dy   (round (* 17 zoom)))
           (pen  (max 1 (round (* 2 zoom)))))
      (om-with-focused-view self
        (om-with-fg-color self *om-gray-color*
          (om-draw-rect mx my (- (w self) dx) (- (h self) dy) :pensize pen))))))

(defmethod update-di-size ((self d-i-box) container)
  (let ((zoom (oa::om-zoom-effective container)))
    (om-set-view-position self (om-make-point (round (* 10 zoom)) (round (* 18 zoom))))
    (om-set-view-size self (om-make-point (- (om-width container) (round (* 20 zoom)))
                                          (max (round (* 20 zoom))
                                               (- (om-height container) (round (* 36 zoom))))))))

(defmethod omng-save ((self text-box) &optional (values? nil))
  (declare (ignore values?))
  (let* ((lf      (oa::om-zoom-logical-font self))
         (current (om-get-font self))
         (zoom    (oa::om-zoom-effective self))
         (logical (cond ((om-font-p lf) lf)
                        ((and current (om-font-p current) (/= zoom 1.0))
                         (oa::om-zoom-scale-font current (/ 1.0 zoom)))
                        (t current))))
    `(om-make-dialog-item 'text-box (om-make-point 1 1) (om-make-point ,(om-width self) ,(om-height self))
                          ,(om-dialog-item-text self)
                          :font ,(om-save-font logical))))

(defmethod update-di-size ((self text-box) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 12 zoom)))
         (my   #+win32 (round (* 12 zoom))
               #+(or macosx darwin darwin-target) (round (* 10 zoom))
               #+linux (round (* 20 zoom)))
         (dx   (round (* 28 zoom)))
         (dy   #+win32 (round (* 24 zoom))
               #+(or macosx darwin darwin-target) (round (* 20 zoom))
               #+linux (round (* 40 zoom))))
    (om-set-view-position self (om-make-point mx my))
    (om-set-view-size self (om-subtract-points (om-view-size container)
                                               (om-make-point dx dy)))))

(defmethod update-di-size ((self text-view) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 12 zoom)))
         (my   #+win32 (round (* 12 zoom))
               #-win32 (round (* 10 zoom)))
         (dx   (round (* 28 zoom)))
         (dy   #+win32 (round (* 24 zoom))
               #-win32 (round (* 20 zoom))))
    (om-set-view-position self (om-make-point mx my))
    (om-set-view-size self (om-subtract-points (om-view-size container)
                                               (om-make-point dx dy)))))

(defmethod update-di-size ((self button) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 10 zoom)))
         (yoff #+(or mswindows win32) (round (* 12 zoom))
               #-(or mswindows win32) (round (* 11 zoom)))
         (dx   (round (* 20 zoom)))
         (bh   (round (* 24 zoom))))
    (om-set-view-position self (om-make-point mx (- (round (h container) 2) yoff)))
    (om-set-view-size self (om-make-point (- (w container) dx) bh))))

(defmethod update-di-size ((self check-box) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 10 zoom)))
         (yoff (round (* 12 zoom)))
         (dx   (round (* 20 zoom)))
         (bh   (round (* 24 zoom))))
    (om-set-view-position self (om-make-point mx (- (round (h container) 2) yoff)))
    (om-set-view-size self (om-make-point (- (w container) dx) bh))))

(defmethod update-di-size ((self radio-button) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 10 zoom)))
         (yoff (round (* 12 zoom)))
         (dx   (round (* 20 zoom)))
         (bh   (round (* 24 zoom))))
    (om-set-view-position self (om-make-point mx (- (round (h container) 2) yoff)))
    (om-set-view-size self (om-make-point (- (w container) dx) bh))))

(defmethod update-di-size ((self single-item-list) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 12 zoom)))
         (my   (round (* 8  zoom)))
         (dx   (round (* 24 zoom)))
         (dy   (round (* 16 zoom))))
    (om-set-view-position self (om-make-point mx my))
    (om-set-view-size self (om-subtract-points (om-view-size container)
                                               (om-make-point dx dy)))))

(defmethod update-di-size ((self multi-item-list) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (mx   (round (* 12 zoom)))
         (my   (round (* 8  zoom)))
         (dx   (round (* 24 zoom)))
         (dy   (round (* 16 zoom))))
    (om-set-view-position self (om-make-point mx my))
    (om-set-view-size self (om-subtract-points (om-view-size container)
                                               (om-make-point dx dy)))))

(defmethod update-di-size ((self pop-up-menu) container)
  (let* ((zoom  (oa::om-zoom-effective container))
         (cw    (w container))
         (ch    (h container))
         (mx    (max 1 (round (* 10 zoom))))
         (bh    (max 1 (round (* 24 zoom))))
         (pos-y (max 0 (round (- ch bh) 2))))
    (om-set-view-position self (om-make-point mx pos-y))
    (om-set-view-size self (om-make-point (max 1 (- cw (* 2 mx))) bh))))

(defmethod update-di-size ((self slider) container)
  (let* ((zoom (oa::om-zoom-effective container))
         (m8   (round (* 8  zoom)))
         (m12  (round (* 12 zoom)))
         (m16  (round (* 16 zoom)))
         (m24  (round (* 24 zoom))))
    (if (equal (om-get-slider-orientation self) :horizontal)
        (progn
          (om-set-view-position self (om-make-point m8 (- (round (h container) 2) m12)))
          (om-set-view-size self (om-make-point (- (w container) m16) m24)))
        (progn
          (om-set-view-position self (om-make-point (- (round (w container) 2) m12) m8))
          (om-set-view-size self (om-make-point m24 (- (h container) m16)))))))

;;; ============================================================
;;; Box-level zoom: connection-drag, outfleche enter/leave,
;;; before/selected box backgrounds, omboxframe resize/relayout,
;;; encapsulation, and the function-without-name protocol.
;;; ============================================================

(defmethod draw-connection-drag ((self om-view) init-pos pos)
  (let* ((zoom    (oa::om-zoom-effective self))
         (thick   (max 1 (round (* 4 zoom))))
         (thin    (max 1 (round (* 2 zoom))))
         (icon-sz (max 1 (round (* 12 zoom))))
         (offx    (round (* 5  zoom)))
         (offy    (round (* 10 zoom))))
    (if (and *mag-in-out* (input? (om-find-view-containing-point self pos)))
        (progn
          (om-with-line-size thick
            (om-with-line #+(or linux macosx) '(2 5) #+win32 '(1 1)
              (om-with-fg-color self
                  (om-make-color-alpha 0 0 1 0.7)
                (om-draw-line (om-point-x init-pos) (om-point-y init-pos)
                              (om-point-x pos) (om-point-y pos)))))
          (om-draw-picture self (om-load-icon 1550)
                           :pos (om-make-point (- (om-point-x pos) offx)
                                               (- (om-point-y pos) offy))
                           :size (om-make-point icon-sz icon-sz)))
        (om-with-line-size thin
          (om-with-line '(2 3)
            (om-with-fg-color self
                (om-make-color-alpha 0 0 0 0.5)
              (om-draw-line (om-point-x init-pos) (om-point-y init-pos)
                            (om-point-x pos) (om-point-y pos))))))))

(defmethod draw-auto-connection-drag ((self om-view) init-pos pos)
  (let ((thick (max 1 (round (* 3 (oa::om-zoom-effective self))))))
    (om-with-line-size thick
      (om-with-line '(2 3)
        (om-with-fg-color self
            (om-make-color-alpha 1 0 0 0.5)
          (om-draw-line (om-point-x init-pos) (om-point-y init-pos)
                        (om-point-x pos) (om-point-y pos)))))))

(defmethod om-view-mouse-enter-handler ((self outfleche))
  (when *mag-in-out*
    (setf (iconid self) 1550)
    (let* ((zoom (oa::om-zoom-effective self))
           (big  (round (* 12 zoom)))
           (off  (round (* 2 zoom)))
           (pos  (om-view-position self))
           (ypos (om-point-y pos))
           (xpos (om-point-x pos)))
      (om-set-view-size self (om-make-point big big))
      (om-set-view-position self (om-make-point (- xpos off) ypos)))
    #+win32 (oa::om-redraw-pinboard-object self)))

(defmethod om-view-mouse-leave-handler ((self outfleche))
  (when *mag-in-out*
    (setf (iconid self) 185)
    (let* ((zoom    (oa::om-zoom-effective self))
           (small   (round (* 8 zoom)))
           (off2    (round (* 2 zoom)))
           (off9    (round (* 9 zoom)))
           (parsize (om-view-size (om-view-container self)))
           (psizey  (om-point-y parsize)))
      (om-set-view-size self (om-make-point small small))
      (om-set-view-position self (om-make-point (+ (om-point-x (om-view-position self)) off2)
                                                (- psizey off9))))
    #+win32 (oa::om-redraw-pinboard-object self)))

(defmethod draw-before-box ((self omboxframe))
  (when (frame-size (object self))
    (let* ((zoom (oa::om-zoom-effective self))
           (top  (round (* 8 zoom)))
           (bot  (round (* 17 zoom))))
      (om-with-focused-view self
        (om-with-fg-color nil (om-make-color 0.921 0.921 0.921)
          (om-fill-rect 0 top (w self) (- (h self) bot)))))))

(defmethod draw-selected-box ((self omboxframe))
  (when (frame-size (object self))
    (let* ((zoom (oa::om-zoom-effective self))
           (top  (round (* 8 zoom)))
           (bot  (round (* 17 zoom))))
      (om-with-focused-view self
        (om-with-fg-color nil (om-make-color .821 0.821 0.821)
          (om-fill-rect 0 top (w self) (- (h self) bot)))))))

(defmethod add-box-resize ((self omboxframe))
  "add a resize view to 'self'"
  (let* ((zoom (or oa::*make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (size-v (if scale-p (max 1 (round (* 10 zoom))) 10))
         (off-v  size-v))
    (om-add-subviews self
                     (setf (resize-box self)
                           (om-make-view 'c-resize-box
                                         :size (om-make-point size-v size-v)
                                         :position (om-make-point (- (w self) off-v)
                                                                  (- (h self) off-v)))))))

(defmethod redraw-frame ((self omboxframe))
  "Update graphically 'self'."
  (let ((thescroll (om-view-container self))
        (object (object self)) frame)
    (when thescroll
      (om-remove-subviews thescroll self)
      (let ((oa::*make-frame-zoom-context*
             (and (typep thescroll 'om-scroller) (oa::om-zoom-of thescroll))))
        (setf frame (make-frame-from-callobj object)))
      (om-add-subviews thescroll frame)
      (update-graphic-connections frame (get-elements (object thescroll)))
      (make-move-after thescroll (list frame))
      frame)))

(defmethod OMGMoveObject ((self omboxframe) new-position)
  "Move SELF to NEW-POSITION (LOGICAL)."
  (setf new-position (borne-position new-position))
  (om-set-view-position self new-position)
  (om-highlight-view self nil))

(defmethod move-frame-delta ((self omboxframe) dir)
  "Move self by 1 or 10 logical units."
  (let* ((pixnum (if (om-shift-key-p) 10 1))
         (pos    (or (and (object self) (frame-position (object self)))
                     (om-view-position self))))
    (case dir
      (0 (omGMoveObject self (om-subtract-points pos (om-make-point 0 pixnum))))
      (1 (omGMoveObject self (om-add-points      pos (om-make-point 0 pixnum))))
      (2 (omGMoveObject self (om-add-points      pos (om-make-point pixnum 0))))
      (3 (omGMoveObject self (om-subtract-points pos (om-make-point pixnum 0)))))))

(defun box-logical-size (box)
  (let ((obj (object box)))
    (or (and obj (frame-size obj))
        (let ((z (oa::om-zoom-effective box)))
          (if (= z 1.0)
              (om-view-size box)
              (oa::om-zoom-unscale-point (om-view-size box) z))))))

(defmethod box-resize-x-plus ((box omboxframe))
  "Enlarge box horizontally by 10 logical px."
  (let* ((size (box-logical-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
    (change-boxframe-size box (om-make-point (+ x 10) y))))

(defmethod box-resize-x-minus ((box omboxframe))
  "Shrink box horizontally by 10 logical px."
  (let* ((size (box-logical-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
    (if (>= x 40)
        (change-boxframe-size box (om-make-point (- x 10) y))
        (om-beep-msg "Minimum size reached!"))))

(defmethod box-resize-y-plus ((box omboxframe))
  "Enlarge box vertically by 10 logical px."
  (let* ((size (box-logical-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
    (change-boxframe-size box (om-make-point x (+ y 10)))))

(defmethod box-resize-y-minus ((box omboxframe))
  "Shrink box vertically by 10 logical px."
  (let* ((size (box-logical-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
    (if (>= y 60)
        (change-boxframe-size box (om-make-point x (- y 10)))
        (om-beep-msg "Minimum size reached!"))))

(defmethod allow-new-size ((self omboxframe) new-pos)
  (when (> (om-point-h new-pos) 10)
    (om-make-point (om-point-h new-pos)
                   (om-point-v (box-logical-size self)))))

(defmethod reinit-size ((box boxtypeframe))
  "Set the size of SELF to the initial size."
  (let ((goodsize (good-text-box-size (om-dialog-item-text (iconview box)) *ombox-font*)))
    (setf (frame-size (object box)) goodsize)
    (box-draw-connections box nil)
    (omG-select (redraw-frame box))))

(defmethod move-frame-delta ((self boxEditorFrame) dir)
  "If option-key is down move the pict in self's miniview, not 'self'."
  (if (om-option-key-p)
      (move-miniview (iconview self) dir)
      (let* ((pixnum (if (om-shift-key-p) 10 1))
             (pos (or (and (object self) (frame-position (object self)))
                      (om-view-position self)))
             (new-position
              (borne-position (case dir
                                (0 (om-subtract-points pos (om-make-point 0 pixnum)))
                                (1 (om-add-points      pos (om-make-point 0 pixnum)))
                                (2 (om-add-points      pos (om-make-point pixnum 0)))
                                (3 (om-subtract-points pos (om-make-point pixnum 0)))))))
        (om-set-view-position self new-position)
        (setf (frame-position (object self)) new-position))))

(defmethod change-boxframe-size ((view boxEditorFrame) new-size)
  "NEW-SIZE is LOGICAL."
  (when (setf new-size (allow-new-size view new-size))
    (om-set-view-size view new-size)
    (make-move-after (om-view-container view) (list view))
    (when (showpict (object view))
      (update-miniview (iconview view) (value (object view))))
    (om-invalidate-view view)))

(defmethod OMGMoveObject ((self boxEditorFrame) new-position)
  "If shift-key is down when dragging self, create a slot box instead."
  (if (or (om-shift-key-p) (shift-key-p *OM-drag&drop-handler*))
      (let* ((target (om-view-container self)) newobj)
        (when target
          (setf newobj (omNG-make-new-boxcall-slots (reference (object self))
                                                    (borne-position new-position)
                                                    (mk-unique-name target "slot")))
          (omG-add-element target
                           (let ((oa::*make-frame-zoom-context*
                                  (and (typep target 'om-scroller) (oa::om-zoom-of target))))
                             (make-frame-from-callobj newobj)))))
      (call-next-method)))

(defmethod add-lock-button ((self maquetteframe) &optional (mode "x"))
  "Not lambda mode for maquette boxes."
  (when (and (allow-lock-button (object self))
             (find mode (allowed-lock-modes (object self)) :test 'string-equal))
    (let* ((zoom    (or oa::*make-frame-zoom-context* (oa::om-zoom-effective self) 1.0))
           (scale-p (and (numberp zoom) (/= zoom 1.0)))
           (size-v  (if scale-p (max 1 (round (* 10 zoom))) 10)))
      (setf (lock-button self)
            (om-make-view 'lock-button
                          :IconID (get-icon-lock mode)
                          :size (om-make-point size-v size-v)
                          :position (om-make-point 0 0)
                          :owner (iconview self)
                          :action #'(lambda (item)
                                      (let* ((modes (allowed-lock-modes (object self)))
                                             (mpos (position (mode item) modes :test 'string-equal))
                                             (newmode (nth (mod (1+ mpos) (length modes)) modes)))
                                        (setf (mode item) newmode
                                              (iconID item) (get-icon-lock newmode))
                                        (setf (allow-lock (object self)) newmode))))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) mode)))

(defmethod make-graph-instance ((self t) container posi box)
  (cond
    ((omclass-p (class-of (class-of self)))
     (let ((theclass (class-of self)))
       (let* ((instance (omNG-make-new-instance (clone self) (mk-unique-name container (name theclass))))
              (obj (omNG-make-new-boxcall instance posi (name instance)))
              (frame (let ((oa::*make-frame-zoom-context*
                            (and (typep container 'om-scroller) (oa::om-zoom-of container))))
                       (make-frame-from-callobj obj))))
         (if (and (edition-params (object box))
                  (equal (type-of (value (object box))) (type-of (instance instance))))
             (setf (edition-params instance) (eval (copy-value-params self (object box))))
             (setf (edition-params instance) (default-edition-params (instance instance))))
         (omG-add-element container frame))))
    ((listp self)
     (let* ((instance (omNG-make-new-instance (clone self) (mk-unique-name container (if (null self) "nil" "list"))))
            (obj (omNG-make-new-boxcall instance posi (name instance)))
            (frame (let ((oa::*make-frame-zoom-context*
                          (and (typep container 'om-scroller) (oa::om-zoom-of container))))
                     (make-frame-from-callobj obj))))
       (omG-add-element container frame)))))

(defmethod redraw-frame ((self classboxFrame))
  (let ((thescroll (om-view-container self))
        (object (object self)) frame)
    (om-remove-subviews thescroll self)
    (let ((oa::*make-frame-zoom-context*
           (and (typep thescroll 'om-scroller) (oa::om-zoom-of thescroll))))
      (setf frame (make-frame-from-callobj object)))
    (om-add-subviews thescroll frame)
    (update-graphic-connections frame (get-class+alias (object thescroll)))))

(defvar *function-without-name* (list 'om+ 'om* 'om- 'om/ 'om^ 'omand 'omor 'om-e 'om-log 'om< 'om> 'om<= 'om>= 'om= 'om/=)
  "Built-in list of function names whose boxes render only the icon (no name strip).")

(defgeneric function-without-name-p (reference)
  (:documentation "T if a box whose REFERENCE renders without name strip (icon-only)."))

(defmethod function-without-name-p (reference)
  (and (symbolp reference) (member reference *function-without-name*)))

(defun frame-logical-position (frame)
  (or (and (object frame) (frame-position (object frame)))
      (om-view-position frame)))

(defun average-position (frames)
  (om-make-point (average (mapcar #'(lambda (frame)
                                      (om-point-h (frame-logical-position frame)))
                                  frames)
                          nil)
                 (average (mapcar #'(lambda (frame)
                                      (om-point-v (frame-logical-position frame)))
                                  frames)
                          nil)))

(defmethod make-incoming-threshold-connections ((patch OMPatchAbs) bridges outside-boxes inside-boxes)
  (loop for conn in bridges
        for source = (nth (first conn) outside-boxes)
        with index = 1
        with entries
        with input-ord
        for input = (or (let ((match (find conn entries
                                           :test #'(lambda (c entry)
                                                     (and (= (first c) (second entry))
                                                          (= (second c) (third entry)))))))
                          (when match
                            (setf input-ord (fourth match))
                            (first match)))
                        (let* ((src-pos (frame-logical-position source))
                               (newin (make-new-patch-input (string+ "input"
                                                                     (if (= index 1)
                                                                         ""
                                                                         (string+ " " (prin1-to-string index))))
                                                            (1- index)
                                                            (om-make-point (+ (om-point-h src-pos) (* (second conn) 35))
                                                                           (om-point-v src-pos)))))
                          (omng-add-element patch newin)
                          (push (list newin (first conn) (second conn) index) entries)
                          (setf input-ord (1- index))
                          (incf index)
                          newin))
        do
        (omng-connect input 0
                      (nth (third conn) inside-boxes) (fourth conn)
                      nil)
        (omng-connect (object source) (second conn)
                      (first (attached-objs patch)) input-ord
                      nil)
        finally
        (loop for item in (attached-objs patch) do
              (update-from-reference item))))

(defmethod make-outgoing-threshold-connections ((patch OMPatchAbs) bridges outside-boxes inside-boxes)
  (loop for conn in bridges
        for destination = (nth (third conn) outside-boxes)
        for index from 1
        for dest-pos = (frame-logical-position destination)
        for newout = (make-new-output (string+ "output"
                                               (if (= index 1)
                                                   ""
                                                   (string+ " " (prin1-to-string index))))
                                      (1- index)
                                      (om-make-point (+ (om-point-h dest-pos) (* (fourth conn) 35))
                                                     (om-point-v dest-pos)))
        do
        (omng-add-element patch newout)
        (omng-connect (nth (first conn) inside-boxes) (second conn)
                      newout 0
                      nil)
        (omng-connect (first (attached-objs patch)) (1- index)
                      (object destination) (fourth conn)
                      nil)
        finally
        (loop for item in (attached-objs patch) do
              (update-from-reference item))))

(defmethod om-encapsulate ((self patchPanel) actives)
  (modify-patch self)
  (let* ((inactives (get-inactives self))
         (patchabs (make-instance 'OMPatchAbs :name (mk-unique-name self "mypatch") :icon 210))
         (pboxcall (omNG-make-new-boxcall patchabs
                                          (average-position actives)
                                          (mk-unique-name self "mypatch")))
         (pframe (let ((oa::*make-frame-zoom-context*
                        (and (typep self 'om-scroller) (oa::om-zoom-of self))))
                   (make-frame-from-callobj pboxcall))))
    (omG-add-element self pframe)
    (let ((copies (loop for frame in actives
                        collect (eval (omng-copy (object frame)))))
          (clist (mk-connection-list (mapcar 'object actives)))
          (coming-in (sort (mk-bridge-list (mapcar 'object inactives) (mapcar 'object actives))
                           #'<
                           :key #'(lambda (bridge)
                                    (om-point-h (frame-logical-position (nth (car bridge) inactives))))))
          (going-out (sort (mk-bridge-list (mapcar 'object actives) (mapcar 'object inactives))
                           #'<
                           :key #'(lambda (bridge)
                                    (om-point-h (frame-logical-position (nth (third bridge) inactives)))))))
      (loop for copy in copies
            do (omng-add-element patchabs copy))
      (loop for connect in clist
            do
            (omng-connect (nth (nth 0 connect) copies)
                          (nth 1 connect)
                          (nth (nth 2 connect) copies)
                          (nth 3 connect)
                          (nth 4 connect)
                          (nth 5 connect)))
      (make-incoming-threshold-connections patchabs coming-in inactives copies)
      (make-outgoing-threshold-connections patchabs going-out inactives copies)
      (normalize-positions patchabs)
      (shrink-patch-window patchabs)
      (loop for frame in actives
            do
            (omg-remove-element self frame))
      (omg-select (car (frames (car (attached-objs patchabs))))))))

(defmethod om-unencapsulate ((self patchPanel) (active patchboxframe))
  (modify-patch self)
  (load-patch (reference (object active)))
  (let* ((sub-patch (reference (object active)))
         (parent-patch (object self))
         (copies (loop for box in (boxes sub-patch)
                       collect (eval (omng-copy box))))
         inlet-alist
         outlet-alist)
    (loop for box in (boxes sub-patch)
          for k from 0
          if (equalp (type-of box) 'omin) do (push (list k (indice box)) inlet-alist)
          if (equalp (type-of box) 'omout) do (push (list k (indice box)) outlet-alist))
    (loop for box in (reverse copies)
          unless (is-inout-p box)
          do (omg-add-element self
                              (let ((oa::*make-frame-zoom-context*
                                     (and (typep self 'om-scroller) (oa::om-zoom-of self))))
                                (make-frame-from-callobj box))))
    (remk-connections copies (mk-connection-list (boxes sub-patch)))
    (let ((parent-connections (mk-connection-list (boxes parent-patch)))
          (sub-patch-position (position (object active) (boxes parent-patch))))
      (loop for conn in (mk-connection-list (boxes sub-patch))
            for inlet-number = (assoc (first conn) inlet-alist)
            for outlet-number = (assoc (third conn) outlet-alist)
            do
            (when inlet-number
              (let ((outer-connection (connected? (nth (second inlet-number)
                                                       (inputs (object active))))))
                (when outer-connection
                  (omNG-connect (first outer-connection)
                                (second outer-connection)
                                (nth (third conn) copies)
                                (fourth conn)
                                (fifth conn)
                                (sixth conn)))))
            (when outlet-number
              (loop for pc in parent-connections
                    do
                    (when (and (= (first pc) sub-patch-position)
                               (= (second pc) (second outlet-number)))
                      (omNG-connect (nth (first conn) copies)
                                    (second conn)
                                    (nth (third pc) (boxes parent-patch))
                                    (fourth pc)
                                    (fifth pc)
                                    (sixth pc)))))))
    (center-positions-around (object self) copies (frame-position (object active)))
    (omg-remove-element self active)
    (mapc #'(lambda (copy)
              (when (frames copy)
                (redraw-frame (first (frames copy)))))
          copies)
    (mapc #'(lambda (c) (let ((frame (car (frames c))))
                          (when frame
                            (omg-select frame))))
          copies)))

;;; ============================================================
;;; dead-reference OMBoxcall: replacement frame inherits container zoom
;;; ============================================================

(defmethod dead-reference ((self OMBoxcall))
  "This method is called when the reference of 'self' is deleted."
  (let ((newobj (omNG-make-new-boxcall (fdefinition 'dead-method) (frame-position self) (name self))))
    (if (frames self)
        (let ((container (om-view-container (car (frames self)))))
          (when container
            (real-make-delete-before container (frames self))
            (omg-remove-element container (car (frames self)))
            (omG-add-element container
                             (let ((oa::*make-frame-zoom-context*
                                    (and (typep container 'om-scroller) (oa::om-zoom-of container))))
                               (make-frame-from-callobj newobj)))))
        (progn
          (setf (reference self) 'dead-method)
          (setf (inputs self) nil)
          (setf (icon self) 190)
          (change-class self 'box-dead)))))

;;; ============================================================
;;; DIEditorframe: touch-target redirection + zoom-aware resize
;;; ============================================================

(defmethod change-boxframe-size ((view DIEditorframe) new-size)
  (when (setf new-size (allow-new-size view new-size))
    (om-set-view-size view new-size)
    (make-move-after (om-view-container view) (list view))
    (update-di-size (value (object view)) view)
    (om-invalidate-view view)
    (om-invalidate-view (om-view-container view))
    #+linux (update-for-subviews-changes (om-view-container view) t)))

