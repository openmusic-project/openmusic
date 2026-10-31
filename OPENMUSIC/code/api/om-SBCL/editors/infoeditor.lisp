(in-package :oa)

(defclass om-obj-info (om-look-window)
  ((ep                 :accessor ep                 :initform nil)
   (panes              :accessor panes              :initform nil)
   (sel                :accessor sel                :initform nil)
   (intfunc            :accessor intfunc            :initform (constantly nil))
   (ompanel            :accessor ompanel            :initform nil)
   (info               :accessor info               :initform nil)
   (info-pack          :accessor info-pack          :initform nil)
   (info-pack-original :accessor info-pack-original :initform nil)))

(defun %info-pack-update-fields (pack input-widgets indices)
  (loop for idx in indices
        for w   in input-widgets
        do (let ((txt (alloy:value w)))
             (setf (nth idx pack) (or txt "")))))

(defun %info-pack-reset-fields (pack input-widgets indices)
  (loop for idx in indices
        for w   in input-widgets
        do (setf (alloy:value w) (or (nth idx pack) ""))
           (alloy:mark-for-render w)))

(defun %info-action-set (window indices)
  (let ((pack (info-pack window))
        (inputs (panes window)))
    (when (and pack inputs)
      (%info-pack-update-fields pack inputs indices))
    (when (and (intfunc window) (ompanel window))
      (funcall (intfunc window) (ompanel window) (sel window) (cdddr pack)))
    (multiple-value-bind (x y w h)
        (let ((b (alloy:bounds window)))
          (values (alloy:pxx b) (alloy:pxy b) (alloy:pxw b) (alloy:pxh b)))
      (setf info-x x info-y y info-width w info-height h))
    (om-close-window window)))

(defun %info-action-reset (window indices)
  (let ((original (info-pack-original window))
        (inputs   (panes window)))
    (when (and original inputs)
      (%info-pack-reset-fields original inputs indices))))

(defun %info-action-close (window)
  (let ((b (alloy:bounds window)))
    (setf info-x (alloy:pxx b) info-y (alloy:pxy b)
          info-width (alloy:pxw b) info-height (alloy:pxh b)))
  (om-close-window window))

(defun %build-info-window (title pack objs labels-with-indices show-position-p show-reset-p)
  (%score-editor-init-defaults)
  (incf info-x 15)
  (incf info-y 15)
  (let* ((row-h      (max 22 *editor-button-h*))
         (label-w    110)
         (header-h   28)
         (data-rows  (length labels-with-indices))
         (data-h     (* row-h data-rows))
         (window     (om-make-window 'om-obj-info
                                     :window-title title
                                     :size      (alloy:px-size info-width info-height)
                                     :min-size  (alloy:px-size 320 200)
                                     :position  (om-make-point info-x info-y)
                                     :bg-color (om-make-color-alpha 0.772 0.855 0.788 1.0))))
    (setf (ep                 window) window)
    (setf (ompanel            window) (and pack (first objs)))
    (setf (sel                window) objs)
    (setf (info               window) pack)
    (setf (info-pack          window) pack)
    (setf (info-pack-original window) (copy-list pack))
    (let* ((root        (om-window-root  window))
           (focus       (om-window-focus window))
           (panel-bg    (om-make-color-alpha 0.772 0.855 0.788 1.0))
           (outer       (make-instance 'alloy:border-layout))
           (header      (make-instance 'alloy:border-layout))
           (rest-bl     (make-instance 'alloy:border-layout))
           (rows-fixed  (make-instance 'alld:fixed-layout))
           (footer      (make-instance 'alloy:horizontal-linear-layout
                                       :align :start
                                       :cell-margins (alloy:margins 4 *editor-margin*)))
           (object-lbl  (om-make-dialog-item 'om-static-text
                                             (om-make-point 0 0) (om-make-point 200 24)
                                             (or (car pack) "")
                                             :bg-color panel-bg
                                             :fg-color *om-black-color*))
           (pos-lbl     (when (and show-position-p (cadr pack))
                          (om-make-dialog-item 'om-static-text
                                               (om-make-point 0 0) (om-make-point 200 24)
                                               (format nil "[~A]" (cadr pack))
                                               :bg-color panel-bg
                                               :fg-color *om-black-color*))))
      (alloy:with-unit-parent root
        (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
      (alloy:enter header  outer :place :north
                   :size (alloy:un (+ header-h (* 2 *editor-margin*))))
      (alloy:enter rest-bl outer :place :center)
      (alloy:enter rows-fixed rest-bl :place :north :size (alloy:un data-h))
      (alloy:enter footer     rest-bl :place :south
                   :size (alloy:un (+ *editor-button-h* (* 2 *editor-margin*))))
      (setf (alloy:cell-margins header)
            (alloy:margins *editor-margin* *editor-margin* *editor-margin* *editor-margin*))
      (alloy:enter object-lbl header :place :center)
      (when pos-lbl
        (alloy:enter pos-lbl header :place :east :size (alloy:un 200)))
      (let ((inputs nil)
            (indices nil))
        (loop for (label . idx) in labels-with-indices
              for i from 0
              do (let* ((row     (make-instance 'alloy:border-layout))
                        (lbl     (om-make-dialog-item 'om-static-text
                                                       (om-make-point 0 0) (om-make-point label-w 22)
                                                       label
                                                       :bg-color panel-bg
                                                       :fg-color *om-black-color*))
                        (data    (make-instance 'alloy:value-data
                                                :value (or (nth idx pack) "")))
                        (input   (make-instance 'oa::om-text-edit-view :data data :wrap-p t))
                        (scroll  (make-instance 'alloy:scroll-view
                                                :layout input :focus input
                                                :focus-parent focus :scroll :y)))
                   (when *def-objinfo-font* (om-set-font input *def-objinfo-font*))
                   (alloy:enter row rows-fixed
                                :x 0
                                :y (alloy:un (* (- data-rows 1 i) row-h))
                                :w (alloy:pw 1)
                                :h (alloy:un (- row-h 2)))
                   (alloy:enter lbl row :place :west :size (alloy:un label-w))
                   (alloy:enter (alloy:layout-element scroll) row :place :center)
                   (push input inputs)
                   (push idx indices)))
        (setf (panes window) (nreverse inputs))
        (let ((saved-indices (nreverse indices)))
          (cond (show-reset-p
                 (let ((set-btn   (om-make-dialog-item 'om-button
                                                       (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                       "Set"
                                                       :di-action (om-dialog-item-act _
                                                                    (declare (ignore _))
                                                                    (%info-action-set window saved-indices))))
                       (reset-btn (om-make-dialog-item 'om-button
                                                       (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                       "Reset"
                                                       :di-action (om-dialog-item-act _
                                                                    (declare (ignore _))
                                                                    (%info-action-reset window saved-indices))))
                       (close-btn (om-make-dialog-item 'om-button
                                                       (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                       "Close"
                                                       :di-action (om-dialog-item-act _
                                                                    (declare (ignore _))
                                                                    (%info-action-close window)))))
                   (alloy:enter set-btn   footer)
                   (alloy:enter reset-btn footer)
                   (alloy:enter close-btn footer)
                   (alloy:enter set-btn   focus)
                   (alloy:enter reset-btn focus)
                   (alloy:enter close-btn focus)))
                (t
                 (let ((close-btn (om-make-dialog-item 'om-button
                                                      (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                      "Close"
                                                      :di-action (om-dialog-item-act _
                                                                   (declare (ignore _))
                                                                   (%info-action-close window)))))
                   (alloy:enter close-btn footer)
                   (alloy:enter close-btn focus))))))
      (alloy:refit (om-window-chrome window)))
    (om-show-window window)
    window))

(defun open-chordseq-info (objs)
  (let* ((pack (or *info-pack* '("CHORD-SEQ" "" "" "" "" "" "" "" "" ""))))
    (cond ((equal (car pack) "NOTE")
           (%build-info-window "Score Inspector" pack objs
                               '(("SELECTION:" . 2) ("DURATION:" . 3)
                                 ("MIDIC:"     . 4) ("VELOCITY:" . 5)
                                 ("DURATION:"  . 6) ("CHANNEL:"  . 7))
                               nil t))
          ((or (equal (car pack) "CHORD") (equal (car pack) "GRACE-CHORD"))
           (%build-info-window "Score Inspector" pack objs
                               '(("SELECTION:"  . 2) ("DURATION:"   . 3)
                                 ("MIDICS:"     . 4) ("VELOCITIES:" . 5)
                                 ("DURATIONS:"  . 6) ("CHANNELS:"   . 7)
                                 ("OFFSETS:"    . 8) ("PORTS:"      . 9))
                               t t))
          ((equal (car pack) "GROUP")
           (%build-info-window "Score Inspector" pack objs
                               '(("SELECTION:" . 2) ("DURATION:" . 3))
                               nil nil))
          (t
           (%build-info-window "Score Inspector" pack objs
                               '(("SELECTION:" . 2) ("DURATION:" . 3))
                               t nil)))))

(defun open-chord-info (objs)
  (let* ((pack (or *info-pack* '("CHORD" "" "" "" "" "" "" "" ""))))
    (cond ((equal (car pack) "NOTE")
           (%build-info-window "Chord Inspector" pack objs
                               '(("MIDIC:"    . 3) ("VELOCITY:" . 4)
                                 ("DURATION:" . 5) ("CHANNEL:"  . 6)
                                 ("PORT:"     . 8))
                               nil t))
          ((equal (car pack) "CHORD")
           (%build-info-window "Chord Inspector" pack objs
                               '(("MIDICS:"     . 3) ("VELOCITIES:" . 4)
                                 ("DURATIONS:"  . 5) ("CHANNELS:"   . 6)
                                 ("OFFSETS:"    . 7) ("PORTS:"      . 8))
                               t t))
          (t
           (%build-info-window "Chord Inspector" pack objs nil t nil)))))

(defun open-note-info (objs)
  (let* ((pack (or *info-pack* '("NOTE" "" "" "" "" "" "" "" ""))))
    (%build-info-window "Note Inspector" pack objs
                        '(("MIDIC:"    . 3) ("VELOCITY:" . 4)
                          ("DURATION:" . 5) ("CHANNEL:"  . 6)
                          ("PORT:"     . 8))
                        nil t)))
