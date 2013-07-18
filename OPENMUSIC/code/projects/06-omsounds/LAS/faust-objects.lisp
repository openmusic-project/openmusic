;;;================================================================================================================================================================
;;;                                                                            faust
;;;================================================================================================================================================================

(in-package :om)

(defvar paramnum 0) ;For graphic purpose
(defvar *faust-effects-to-compile* nil)
(defvar *faust-synths-to-compile* nil)


;======================================================
;===      SINGLE FAUST PARAMETER CONTROLLER         ===
;=== a single parameter of the general faust effect ===
;======================================================

(defclass* faust-effect-parameter-controller () 
  ((param-type :initform 'vslider :initarg :param-type :accessor param-type :type t)
   (label :initform nil :initarg :label :accessor label :type t)
   (index :initform nil :initarg :index :accessor index)
   (defval :initform nil :initarg :defval :accessor defval :type float)
   (minval :initform nil :initarg :minval :accessor minval :type float)
   (maxval :initform nil :initarg :maxval :accessor maxval :type float)
   (stepval :initform nil :initarg :stepval :accessor stepval :type float)
   (effect-ptr :initform nil :initarg :effect-ptr :accessor effect-ptr)
   (tracknum :initform 0 :initarg :tracknum :accessor tracknum)
   (display :initform nil :initarg :display :accessor display)))


;=======================================
;===     PARAMETERS CONTROLLER       ===
;=== a set of parameters controllers ===
;=======================================

(defclass* faust-effect-console (simple-score-element)
   ((effect-txt :initform nil :initarg :effect-txt :accessor effect-txt :documentation "Faust Code, written in a Textfile")
    (effect-ptr :initform nil :accessor effect-ptr)
    (effect-name :initform nil :initarg :effect-name :accessor effect-name :documentation "The name of the Faust Effect")
    (tracknum :initform 0 :initarg :tracknum :accessor tracknum :documentation "The track on which the effect will be pluged (0 = no specific track)")
    (effect-dsp :initform nil :accessor effect-dsp)
    (effect-svg :initform nil :accessor effect-svg)
    (nbparams :initform 0 :accessor nbparams :type t)
    (params-ctrl :initform nil :accessor params-ctrl :type t)
    (ui-tree :initform nil :accessor ui-tree))
   (:icon 918)
   (:documentation "Faust Effect"))

(defmethod initialize-instance ((self faust-effect-console) &rest l)
  (let ((rep (call-next-method)))
    (print (list "GC-REGISTERING_CONSOLE" rep))
    (hcl::flag-special-free-action rep)
    rep))

(defmethod initialize-instance :after ((self faust-effect-console) &rest l)
  (declare (ignore l))
  (when (effect-txt self)
    (build-faust-effect-console self))
  self)

(defmethod faust-cleanup ((self faust-effect-console))
  (print (list "GC-FAUST_FX_CLEANUP" self))
  (las-faust-effect-cleanup (effect-ptr self)))

(defmethod faust-cleanup ((self t)) nil)

;Add the faust cleanup to garbage functions
(hcl::add-special-free-action 'faust-cleanup)

(defmethod build-faust-effect-console ((self faust-effect-console))
  ;;Check if user plugged a Faust code to the box. If yes, build, if no, exit.
  (if (effect-txt self)
      (let ((parlist (list-of-lines (buffer-text (effect-txt self))))
            name effect-string effect-result)
        ;;Set name, or add a default name
        (setf name (set-effect-name self))
        ;;Check if the name  is already used. If yes, exit. If no, build effect.
        (let ((namesearch (las-faust-search-effect-name-in-register name)))
          (if (car namesearch)
              (progn
                (om-message-dialog (format nil "An effect called \"~A\" already exists. It has been replaced by the new one.~%~%NOTE : If this box already contains a Faust FX not called \"~A\", it won't be deleted, but you won't be able to access to its console anymore."  name name))
                (if (car (gethash (cadr namesearch) *faust-effects-register*))
                    (las-faust-effect-cleanup (car (gethash (cadr namesearch) *faust-effects-register*))))
                (if *general-mixer-window*
                    (update-general-mixer-effects-lists (car (om-subviews *general-mixer-window*)))))))
        ;;Build string from textfile
        (loop for line in parlist do
              (setf effect-string (concatenate 'string effect-string (format nil "~%") line)))
        ;;Save as a dsp file
        (save-data (list (list effect-string)) (format nil "OM-Faust_effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))
        ;;Get result from the compilation with the faust-api.
        (setf effect-result (las-faust-make-effect 
                             (concatenate 'string (directory-namestring *om-outfiles-folder*) (format nil "OM-Faust_effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))
                             *om-outfiles-folder*))
        (setf (effect-ptr self) (nth 1 effect-result))
        ;;Save code as DSP and set some slots for SVG display
        (set-effect-dsp-and-svg self)
        ;;Check if faust-api returned a compilation error. If yes, exit, if no, build
        (if (/= (car effect-result) 1)
            (om-message-dialog (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (nth 2 effect-result)))
          ;;Get tree from Json, init params, register effect, plug if a track is specified.
          (let ((param-list (finalize-effect-building self name)))
            (when (and param-list (> (nbparams self) 0))
              (setf (params-ctrl self)
                    (loop for param from 0 to (- (nbparams self) 1) collect (make-instance 'faust-effect-parameter-controller
                                                                                           :param-type (param-type (nth param param-list))
                                                                                           :label (label (nth param param-list))
                                                                                           :index param
                                                                                           :defval (string-to-number (init-val (nth param param-list)))
                                                                                           :minval (string-to-number (min-val (nth param param-list)))
                                                                                           :maxval (string-to-number (max-val (nth param param-list)))
                                                                                           :stepval (string-to-number (step-val (nth param param-list)))
                                                                                           :effect-ptr (effect-ptr self)
                                                                                           :tracknum (tracknum self)
                                                                                           )))))) self)))


(defmethod set-effect-name ((self faust-effect-console))
  (or (effect-name self)
      (let ((name (format nil "Faust-FX-~A" (+ 1 (las-get-number-faust-effects-register)))))
        (om-message-dialog (format nil "WARNING : You didn't give a name to the effect. It's now called ~A.~%~%NOTE : If this box already contains a Faust FX, it won't be deleted, but you won't be able to access to its console anymore." name))
        (setf (effect-name self) name)
        name)))

(defmethod set-effect-dsp-and-svg ((self faust-effect-console))
  (setf (effect-dsp self) (format nil "OM-Faust_effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))
  (setf (effect-svg self) (format nil "./OM-Faust_effect~A-svg/process.svg" (+ 1 (las-get-number-faust-effects-register)))))

(defun faust-show-svg (pathname dsp svg)
  (om-cmd-line (format nil "open ~A" svg) nil t pathname))

(defun las-clean-faust-files ()
  (om-cmd-line "rm -Rf OM-Faust_*" nil t *om-outfiles-folder*))

(defmethod finalize-effect-building ((self faust-effect-console) name)
  (progn
    (print "Effet Faust créé avec succès")
    (setf (ui-tree self) (las-faust-parse (las-faust-get-json (effect-ptr self))))
    (setf param-list (las-faust-translate-tree (ui-tree self)))
    (las-faust-add-effect-to-register (effect-ptr self) (tracknum self) name)
    (if (and (tracknum self) (> (tracknum self) 0))
        (las-faust-add-effect-to-track (effect-ptr self) name (- (tracknum self) 1)))
    (if *general-mixer-window*
        (update-general-mixer-effects-lists (car (om-subviews *general-mixer-window*))))
    (setf (nbparams self) (length param-list))
    param-list))

(defmethod allowed-in-maq-p ((self faust-effect-console))  nil)

(defmethod Class-has-editor-p  ((self faust-effect-console)) t)

(defmethod get-editor-class ((self faust-effect-console)) 'faustcontrollerEditor)

(defmethod draw-mini-view ((self t) (value faust-effect-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value faust-effect-console)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self faust-effect-console) x x1 y y1 edparams view)
  (let ((w (w view))
        (pic (om-load-and-store-picture "faustlogo-bg" 'internal)))
    (om-draw-picture view pic (om-make-point 0 0) (om-make-point w (h view)))
    (om-with-focused-view view
      (om-draw-string 5 15 (or (effect-name self) "! NO NAME !")))))

(defmethod omNG-copy ((self faust-effect-console))
   "Cons a Lisp expression that return a copy of self when it is valuated."
   `(let ((rep (make-instance ',(type-of self))))
      rep))

(defmethod copy-container  ((self faust-effect-console) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self))))
    rep))


(defmethod omNG-save ((self faust-effect-console) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((text (effect-txt self))
        (name (effect-name self))
        (track (tracknum self)))
    (if (and (effect-ptr self) (not (las-faust-null-ptr-p (effect-ptr self))))
        (progn
          `(let ((rep (make-instance ',(type-of self))))
             (setf (effect-txt rep) ,(omng-save text)
                   (effect-name rep) ',name
                   (tracknum rep) ',track)
             ;;;PROCESS SEPARE SINON CA MOULINE INDEFINIMENT. A VOIR
             ;(if (om-y-or-n-dialog "A Faust effect is trying to compile. Accept?" :default-button t)
             ;    (om-run-process ,name #'(lambda () (build-faust-effect-console rep))))
             (push rep *faust-effects-to-compile*)
             rep))
      (progn
        `(let ((rep (make-instance ',(type-of self))))
           rep)))))


(defun compile-faust-objects ()
       (om-run-process "faust objects compiler" 
                       #'(lambda ()
                           (mapcar 
                            #'(lambda (fx) 
                                (cond ((typep fx 'faust-effect-console)
                                       (build-faust-effect-console fx))
                                      ((typep fx 'faust-synth-console)
                                       (build-faust-synth-console fx))
                                      (t nil)))
                            (append *faust-effects-to-compile* *faust-synths-to-compile*))
                           (setf *faust-effects-to-compile* nil
                                 *faust-synths-to-compile* nil))))


(defmethod load-patch :after ((self ompatch))
  (compile-faust-objects))

(defmethod get-obj-dur ((self faust-effect-console)) 0)

(defmethod object-remove-extra ((self faust-effect-console) box)
  (let* ((ptr (effect-ptr self)))
    (if ptr
        (las-faust-effect-cleanup ptr))
    (if *general-mixer-window*
        (update-general-mixer-effects-lists (car (om-subviews *general-mixer-window*))))))

;================ CONTROLLER EDITOR ===================

(omg-defclass faustcontrollerEditor (EditorView) 
  ((params-panels :initform nil :accessor params-panels :type list)
   (tree :initform nil :accessor tree :type nil)
   (bottom-bar :initform nil :accessor bottom-bar :type t)))


(defmethod make-editor-window ((class (eql 'faustcontrollerEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                :close-p t :winshow t :bg-color *om-light-gray-color*
                                                      )))
    win))


(defmethod get-win-ed-size ((self faust-effect-console)) 
  (if (ui-tree self)
     (om-make-point (min 500 (if (> (+ 75 (cadr (las-faust-get-group-size (ui-tree self)))) 500)
                                 (+ (car (las-faust-get-group-size (ui-tree self))) 14)
                               (car (las-faust-get-group-size (ui-tree self))))) (min 500 (+ 75 (cadr (las-faust-get-group-size (ui-tree self))))))
    (om-make-point 75 75)))



(defmethod editor-has-palette-p ((self faustcontrollerEditor)) nil)

(defmethod get-panel-class ((self faustcontrollerEditor)) 'faustcontrollerPanel)

(defmethod update-subviews ((self faustcontrollerEditor))
   (om-set-view-size (panel self) (om-make-point (min 500 (w self)) (min 500 (h self)))))


;=== MAIN PANEL ===
(omg-defclass faustcontrollerPanel (om-scroller) ())

(defmethod get-object ((Self faustcontrollerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self faustcontrollerPanel))
  (report-modifications (om-view-container self)))


;======== Parameters controllers panels =====

(omg-defclass faustparamPanel () 
  ((paramctr :initform nil :initarg :paramctr :accessor paramctr)
   (paramText :initform nil :accessor paramText :type t)
   (paramVal :initform nil :accessor paramVal :type t)
   (paramGraph :initform nil :accessor paramGraph :type t)
   (paramReset :initform nil :accessor paramReset :type t)))


(defclass faustparamPanelview (faustparamPanel om-view) ())

(defmethod update-subviews ((Self faustparamPanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self faustparamPanel))
   (call-next-method))



(defmethod get-object ((Self faustparamPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self faustparamPanel))
  (report-modifications (om-view-container self)))


(defmethod get-parampanel-class ((self faustcontrollerPanel)) 'faustparamPanelview)



;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod metaobj-scrollbars-params ((self faustcontrollerEditor))  '(:h nil))

(defmethod initialize-instance :after ((self faustcontrollerEditor) &rest l)
  (declare (ignore l))
  (let ((x (if (ui-tree (object self))
               (max 75 (car (las-faust-get-group-size (ui-tree (object self)))))
             75))
        
        (y (if (ui-tree (object self))
               (+ 75 (cadr (las-faust-get-group-size (ui-tree (object self)))))
             75))
        (xwin (om-point-h (get-win-ed-size (object self))))
        (ywin (om-point-v (get-win-ed-size (object self))))
        (orange (om-make-color 1 0.5 0))
        group-type
        groups
        (xgrp 0)
        (ygrp 0)
        (xpars 0)
        (ypars 0)
        (offset 0))
    (if (and (effect-ptr (object self)) (ui-tree (object self)))
        (progn
          (setf (tree self) (ui-tree (object self)))
          (setf group-type (las-faust-get-group-type (tree self)))
          (setf groups (las-faust-get-groups-only (tree self)))
          (setf params (las-faust-get-params-only (tree self)))
          (setf (panel self) (om-make-view (get-panel-class self) 
                                           :owner self
                                           :position (om-make-point 0 0) 
                                           :scrollbars (cond ((and (>= x 500) (>= y 500)) t)
                                                             ((and (>= x 500) (< y 500)) :h)
                                                             ((and (< x 500) (>= y 500)) :v)
                                                             (t nil))
                                           :retain-scrollbars nil
                                           :field-size  (om-make-point x y)
                                           :size (om-make-point (w self) (h self))
                                           :bg-color *om-light-gray-color*))
     
          (make-faust-group-view self (tree self))
          (setf paramnum 0)
     
          (setf (bottom-bar self) (om-make-view (get-panel-class self)
                                                :owner (panel self)
                                                :bg-color *om-dark-gray-color*
                                                :position (om-make-point 0 (- y 50))
                                                :size (om-make-point (+ 20 x) 50)))
     
          (om-add-subviews (bottom-bar self) (om-make-dialog-item 'om-button (om-make-point (- (round x 2) 30) 5) (om-make-point 60 24)
                                                                  "SVG"
                                                                  :di-action (om-dialog-item-act item
                                                                               (faust-show-svg *om-outfiles-folder* (effect-dsp (object self)) (effect-svg (object self)))))))
      (progn
        (setf (panel self) (om-make-view (get-panel-class self) 
                                         :owner self
                                         :position (om-make-point 0 0) 
                                         :scrollbars nil
                                         :retain-scrollbars t
                                         :field-size  (om-make-point x (- y 50))
                                         :size (om-make-point (w self) (h self))
                                         :bg-color *om-dark-gray-color*))
        (om-add-subviews (panel self) (om-make-dialog-item 'om-static-text
                                                           (om-make-point 17 0)
                                                           (om-make-point 75 75)
                                                           "X"
                                                           :fg-color *om-white-color*
                                                           :font (om-make-font oa::*om-def-bold-font-face* 48 :style '(:bold))))
        ))))

(defmethod make-faust-param-view ((self faustcontrollerEditor) paractrl x y size)
  (let ((res (om-make-view (get-parampanel-class (panel self))
                           :paramctr paractrl
                           :owner (panel self)
                           :bg-color *om-light-gray-color*
                           :position (om-make-point x y)
                           :size (om-make-point (car size) (cadr size)))))
    (setf (display paractrl) res)
    res))

(defmethod make-faust-group-view ((self faustcontrollerEditor) group &optional (x 0) (y 0))
  (let* ((grouptype (las-faust-get-group-type group))
         (orange (om-make-color 1 0.5 0))
         (children (las-faust-get-group-items group))
         (numchildren (length children))
         (size (las-faust-get-group-size group))
         childlist)
    ;;///////////////////ON CREE L'ESPACE DU GROUPE
    (om-make-view 'om-view
                  :owner (panel self)
                  :bg-color *om-light-gray-color*
                  :position (om-make-point x y)
                  :size (om-make-point (car size) (cadr size)))
    ;;///////////////////ON CREE LES VIEW DES ENFANTS
    (loop for i from 0 to (- numchildren 1) do
          (if (typep (nth i children) 'faust-group)
              (progn
                (make-faust-group-view self (nth i children) x y)
                (if (string= grouptype "hgroup")
                    (incf x (+ 5 (car (las-faust-get-group-size (nth i children)))))
                  (incf y (+ 5 (cadr (las-faust-get-group-size (nth i children)))))))
            (let* ((type (param-type (nth i children)))
                   (disable 0)
                   (size (cond ((string= type "hslider") hsliderSize)
                               ((string= type "vslider") vsliderSize)
                               ((string= type "checkbox") checkboxSize)
                               ((string= type "numentry") numentrySize)
                               ((string= type "button") buttonSize)
                               (t (progn (setf disable 1) buttonsize)))))
              (if (= disable 0)
                  (make-faust-param-view self (nth paramnum (params-ctrl (object self))) x y size))
              (incf paramnum)
              (if (string= grouptype "hgroup") 
                  (incf x (car size))
                (incf y (cadr size))))))))


(defmethod update-editor-after-eval ((self faustcontrollerEditor) val)
  (setf (object self) val)
  (progn
    (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-view-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-field-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (print (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self))))
    (loop for parampan in (params-panels self) do 
          (om-remove-subviews (panel self) parampan))
    (if (ui-tree (object self))
        (make-faust-group-view self (ui-tree (object self))))
    (setf paramnum 0)))


(defmethod initialize-instance :after ((self faustparamPanel) &rest l)
   (declare (ignore l))
   (do-initialize-param self))


(defmethod do-initialize-param ((self faustparamPanel))  
   (let* ((ptr (effect-ptr (paramctr self)))
          (number (index (paramctr self)))
          (color (om-make-color 0.9 0.9 0.9))
          (orange (om-make-color 1 0.5 0))
          (type (param-type (paramctr self)))
          (name (label (paramctr self)))
          (min (minval (paramctr self)))
          (max (maxval (paramctr self)))
          (def (defval (paramctr self)))
          (range 0)
          (val 0)
          (tracknum (tracknum (paramctr self)))
          (editor (om-view-container (om-view-container self)))
          (curval (las-faust-get-control-value ptr number)))
   (if (= min max) (let () (setf min 0) (setf max 1)) nil)
   (setf range (/ (- max min) 1.0))
   (setf val (* 100.0 (/ (- curval min) range)))
   (om-set-bg-color self color) 

         (cond ((string= type "hslider")
                (progn
                  (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                              (om-make-point 5 0) 
                                                              (om-make-point (- (car hsliderSize) 5) 40)
                                                              (format nil "~D" name)
                                                              :font *om-default-font1*
                                                              :bg-color color))
                  (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                             (om-make-point (- (round (car hsliderSize) 2) 30) (- (cadr hsliderSize) 31 20)) 
                                                             (om-make-point 60 20)
                                                             (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                             :font *om-default-font1*
                                                             :fg-color orange
                                                             :bg-color color))
                  (setf (paramGraph self) (om-make-dialog-item 'om-slider  
                                                               (om-make-point 10 (- (cadr hsliderSize) 36)) 
                                                               (om-make-point 94 31) ""
                                                               :di-action 
                                                               (om-dialog-item-act item
                                                                 (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                                                   (las-faust-set-control-value ptr number valeur)
                                                                   (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                                               :increment 1
                                                               :range '(0 100)
                                                               :value val
                                                               :direction :horizontal
                                                               :tick-side :none))))
               ((string= type "vslider")
                (progn
                  (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                              (om-make-point 5 0) 
                                                              (om-make-point (- (car vsliderSize) 15) 50)
                                                              (format nil "~D" name)
                                                              :font *om-default-font1*
                                                              :bg-color color))
                  (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                             (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                             (om-make-point 60 20)
                                                             (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                             :font *om-default-font1*
                                                             :fg-color orange
                                                             :bg-color color))
                  (setf (paramGraph self)
                        (om-make-dialog-item 'om-slider  
                                             (om-make-point 10 (- (cadr vsliderSize) 104)) 
                                             (om-make-point 31 94) ""
                                             :di-action 
                                             (om-dialog-item-act item
                                               (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                                 (las-faust-set-control-value ptr number valeur)
                                                 (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                             :increment 1
                                             :range '(0 100)
                                             :value val
                                             :direction :vertical
                                             :tick-side :none))))
               ((string= type "checkbox")
                (progn
                  (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                              (om-make-point 5 0) 
                                                              (om-make-point (- (car checkboxSize) 5) 50)
                                                              (format nil "~D" name)
                                                              :font *om-default-font1*
                                                              :bg-color color))
                  (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                             (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                             (om-make-point 60 20)
                                                             ""
                                                             :font *om-default-font1*
                                                             :fg-color *om-blue-color*
                                                             :bg-color color))
                  (setf (paramGraph self)
                        (om-make-dialog-item 'om-check-box  
                                             (om-make-point 20 5) 
                                             (om-make-point 31 94) ""
                                             :di-action 
                                             (om-dialog-item-act item 
                                               (if (= (las-faust-get-control-value ptr number) 1.0) 
                                                   (las-faust-set-control-value ptr number 0.0) 
                                                 (las-faust-set-control-value ptr number 1.0))
                                               (om-set-dialog-item-text (paramVal self) (format nil "~D" (round (las-faust-get-control-value ptr number)))))
                                             :font *om-default-font1*
                                             :checked-p (if (= (las-faust-get-control-value ptr number) 1.0) t nil)))))
               ((string= type "button")
                (progn
                  (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                              (om-make-point 5 0) 
                                                              (om-make-point (- (car checkboxSize) 5) 50)
                                                              ""
                                                              :font *om-default-font1*
                                                              :bg-color color))
                  (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                             (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                             (om-make-point 60 20)
                                                             ""
                                                             :font *om-default-font1*
                                                             :fg-color *om-blue-color*
                                                             :bg-color color))
                  (setf (paramGraph self)
                        (om-make-dialog-item 'om-button  
                                             (om-make-point 20 5) 
                                             (om-make-point 80 30) 
                                             (format nil "~D" name)
                                             :di-action 
                                             (om-dialog-item-act item
                                               (if (= (las-faust-get-control-value ptr number) max) 
                                                   (progn
                                                     (las-faust-set-control-value ptr number 0.0)
                                                     (las-faust-set-control-value ptr number (+ 0.000001 (las-faust-get-control-value ptr number))))
                                                   (las-faust-set-control-value ptr number (+ 0.000001 (las-faust-get-control-value ptr number)))
                                                 )
                                               (om-set-dialog-item-text (paramVal self) (format nil "~D" (round (las-faust-get-control-value ptr number)))))
                                             :font *om-default-font1*))))
               (t (progn
                    (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                                (om-make-point 5 0) 
                                                                (om-make-point (- (car vsliderSize) 15) 50)
                                                                (format nil "~D" name)
                                                                :font *om-default-font1*
                                                                :bg-color color))
                    (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                               (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                               (om-make-point 60 20)
                                                               (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                               :font *om-default-font1*
                                                               :fg-color orange
                                                               :bg-color color))
                    (setf (paramGraph self)
                          (om-make-view 'graphic-numbox :position (om-make-point 10 45) 
                                        :size (om-make-point 31 94)
                                        :pict (om-load-and-store-picture "fader" 'di)
                                        :nbpict 77
                                        :pict-size (om-make-point 31 94)
                                        :di-action (om-dialog-item-act item
                                                     (let ((valeur (+ (* (/ (value item) 100.0) range) min)))
                                                       (las-faust-set-control-value ptr number valeur)
                                                       (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                        :font *om-default-font2*
                                        :value val
                                        :min-val 0
                                        :max-val 100)))))

   (om-add-subviews self
                    (paramText self)
                    (paramVal self)
                    (paramGraph self)
                    ;(paramReset self)
                    )))


;;;================================================================================================================================================================
;;;                                                                            faust SYNTH
;;;================================================================================================================================================================



(defclass* faust-synth-parameter-controller () 
  ((param-type :initform 'vslider :initarg :param-type :accessor param-type :type t)
   (label :initform nil :initarg :label :accessor label :type t)
   (index :initform nil :initarg :index :accessor index)
   (defval :initform nil :initarg :defval :accessor defval :type float)
   (minval :initform nil :initarg :minval :accessor minval :type float)
   (maxval :initform nil :initarg :maxval :accessor maxval :type float)
   (stepval :initform nil :initarg :stepval :accessor stepval :type float)
   (synth-ptr :initform nil :initarg :synth-ptr :accessor synth-ptr)
   (tracknum :initform 0 :initarg :tracknum :accessor tracknum)
   (display :initform nil :initarg :display :accessor display)))


;=======================================
;===     PARAMETERS CONTROLLER       ===
;=== a set of parameters controllers ===
;=======================================

(defclass* faust-synth-console (simple-score-element)
   ((synth-txt :initform nil :initarg :synth-txt :accessor synth-txt :documentation "Faust Code, written in a Textfile")
    (synth-ptr :initform nil :accessor synth-ptr)
    (synth-name :initform nil :initarg :synth-name :accessor synth-name :documentation "The name of the Faust synth")
    (tracknum :initform 0 :initarg :tracknum :accessor tracknum :documentation "The track on which the synth will be pluged (0 = no specific track)")
    (duration :initform 10 :initarg :duration :accessor duration :documentation "The duration (in sec) during the synth will play (default is 10 sec)")
    (nullsnd :initform nil :accessor nullsnd)
    (synth-dsp :initform nil :accessor synth-dsp)
    (synth-svg :initform nil :accessor synth-svg)
    (nbparams :initform 0 :accessor nbparams :type t)
    (params-ctrl :initform nil :accessor params-ctrl :type t)
    (ui-tree :initform nil :accessor ui-tree)
    (is-copy :initform nil :accessor is-copy))
   (:icon 917)
   (:documentation "Faust synth"))

(defmethod faust-cleanup ((self faust-synth-console))
  (print (list "GC-FAUST_SYNTH_CLEANUP" self))
  (las-faust-synth-cleanup (synth-ptr self)))

(defmethod play-obj? ((self faust-synth-console)) t)

;/Redefinition of transport functions for this kind of box
(defmethod player-play-object ((engine (eql :libaudio)) (object faust-synth-console) &key interval)
  (las-synth-preview-play object))
(defmethod player-stop-object ((engine (eql :libaudio)) (object faust-synth-console) &key interval)
  (las-synth-preview-stop object))

(defmethod default-edition-params ((self faust-synth-console)) 
  (pairlis '(player) '(:libaudio) (call-next-method)))

(defmethod initialize-instance ((self faust-synth-console) &rest l)
  (let ((rep (call-next-method)))
    (print (list "GC-REGISTERING_CONSOLE" rep))
    (hcl::flag-special-free-action rep)
    rep))

(defmethod initialize-instance :after ((self faust-synth-console) &rest l)
  (declare (ignore l))
  (when (synth-txt self)
    (build-faust-synth-console self))
  self)

(defmethod build-faust-synth-console ((self faust-synth-console))
  ;Check whether it's a maquette copy. If no, build, if yes, don't do anything.
  (if (not (is-copy self))
      ;;Check if user plugged a Faust code to the box. If yes, build, if no, exit.
      (if (synth-txt self)
          (let ((parlist (list-of-lines (buffer-text (synth-txt self))))
                (nullptr (las-faust-make-null-sound (duration self)))
                name synth-string synth-result)
            ;;Set name, or add a default name
            (setf name (set-synth-name self))
            ;;Check if the name is already used. If yes, exit. If no, build synth.
            (let ((namesearch (las-faust-search-synth-name-in-register name)))
              (if (car namesearch)
                  (progn
                    (om-message-dialog (format nil "An synth called \"~A\" already exists. It has been replaced by the new one.~%~%NOTE : If this box already contains a Faust Synth not called \"~A\", it won't be deleted, but you won't be able to access to its console anymore."  name name))
                    (if (car (gethash (cadr namesearch) *faust-synths-register*))
                        (las-faust-synth-cleanup (car (gethash (cadr namesearch) *faust-synths-register*))))
                    (if *general-mixer-window*
                        (update-general-mixer-synths-lists (car (om-subviews *general-mixer-window*)))))))
            ;;Build string from textfile
            (loop for line in parlist do
                  (setf synth-string (concatenate 'string synth-string (format nil "~%") line)))
            ;;Save as a dsp file
            (save-data (list (list synth-string)) (format nil "OM-Faust_synth~A.dsp" (+ 1 (las-get-number-faust-synths-register))))
            ;;Get result from the compilation with the faust-api.
            (setf synth-result (las-faust-make-effect 
                                (concatenate 'string (directory-namestring *om-outfiles-folder*) (format nil "OM-Faust_synth~A.dsp" (+ 1 (las-get-number-faust-synths-register)))) 
                                *om-outfiles-folder*))
            (setf (synth-ptr self) (nth 1 synth-result))
            ;;Set a null snd object for this synth
            (setf (nullsnd self) (make-instance 'om-sound
                                                :number-of-channels 2
                                                :sndlasptr nullptr
                                                :sndlasptr-current nullptr
                                                :sndlasptr-current-save nullptr))
            (om-sound-set-sndlasptr-to-play (nullsnd self) nullptr)
            ;;Save code as DSP and set some slots for SVG display
            (set-synth-dsp-and-svg self)
            ;;Check if faust-api returned a compilation error. If yes, exit, if no, build
            (if (/= (car synth-result) 1)
                (om-message-dialog (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (nth 2 synth-result)))
              ;;Get tree from Json, init params, register synth, plug if a track is specified.
              (let (param-list)
                (setf param-list (finalize-synth-building self name))
                (if (> (nbparams self) 0)
                    (setf (params-ctrl self)
                          (loop for param from 0 to (- (nbparams self) 1) collect (make-instance 'faust-synth-parameter-controller
                                                                                                 :param-type (param-type (nth param param-list))
                                                                                                 :label (label (nth param param-list))
                                                                                                 :index param
                                                                                                 :defval (string-to-number (init-val (nth param param-list)))
                                                                                                 :minval (string-to-number (min-val (nth param param-list)))
                                                                                                 :maxval (string-to-number (max-val (nth param param-list)))
                                                                                                 :stepval (string-to-number (step-val (nth param param-list)))
                                                                                                 :synth-ptr (synth-ptr self)
                                                                                                 :tracknum (tracknum self)
                                                                                                 )))))) self))))


(defmethod set-synth-name ((self faust-synth-console))
  (or (synth-name self)
      (let ((name (format nil "Faust-Synth-~A" (+ 1 (las-get-number-faust-synths-register)))))
        (om-message-dialog (format nil "WARNING : You didn't give a name to the synth. It's now called ~A.~%~%NOTE : If this box already contains a Faust Synth, it won't be deleted, but you won't be able to access to its console anymore." name))
        (setf (synth-name self) name)
        name)))

(defmethod set-synth-dsp-and-svg ((self faust-synth-console))
  (setf (synth-dsp self) (format nil "OM-Faust_synth~A.dsp" (+ 1 (las-get-number-faust-synths-register))))
  (setf (synth-svg self) (format nil "./OM-Faust_synth~A-svg/process.svg" (+ 1 (las-get-number-faust-synths-register)))))

(defmethod finalize-synth-building ((self faust-synth-console) name)
  (let (param-list)
    (print "Synthetiseur Faust créé avec succès")
    (setf (ui-tree self) (las-faust-parse (las-faust-get-json (synth-ptr self))))
    (setf param-list (las-faust-translate-tree (ui-tree self)))
    (if (and (tracknum self) (> (tracknum self) 0))
        (if (not (gethash 0 (gethash (- (tracknum self) 1) *faust-synths-by-track*)))
            (progn
              (las-faust-add-synth-to-track (synth-ptr self) name (- (tracknum self) 1))
              (las-faust-add-synth-to-register (synth-ptr self) (tracknum self) name))
          (progn
            (print (format nil "A synth is already plugged on channel ~A. Your synth has been created but not plugged" (tracknum self)))
            (las-faust-add-synth-to-register (synth-ptr self) 0 name)))
      (progn
        (las-faust-add-synth-to-register (synth-ptr self) 0 name)))
    (las-faust-add-synth-console-to-register self (synth-ptr self) (nullsnd self))
    (if *general-mixer-window*
        (update-general-mixer-synths-lists (car (om-subviews *general-mixer-window*))))
    (setf (nbparams self) (length param-list))
    param-list))


(defmethod allowed-in-maq-p ((self faust-synth-console))  t)

(defmethod Class-has-editor-p ((self faust-synth-console)) t)

(defmethod get-editor-class ((self faust-synth-console)) 'faustSynthcontrollerEditor)

(defmethod draw-mini-view  ((self t) (value faust-synth-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value faust-synth-console)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self faust-synth-console) x x1 y y1 edparams view)
  (let ((w (w view))
        (pic (om-load-and-store-picture "faustlogo-bg" 'internal)))
    (om-draw-picture view pic (om-make-point 0 0) (om-make-point w (h view)))))



(defmethod omNG-copy ((self faust-synth-console))
  (call-next-method))

(defmethod copy-container  ((self faust-synth-console) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance 'faust-synth-console)))
    (setf (synth-txt rep) (synth-txt self)
          (synth-ptr rep) (synth-ptr self)
          (synth-name rep) (synth-name self)
          (tracknum rep) (nth 1 (gethash (las-faust-find-synth-in-register (synth-ptr self)) *faust-synths-register*))
          (duration rep) (duration self)
          (nullsnd rep) (nullsnd self)
          (synth-dsp rep) (synth-dsp self)
          (synth-svg rep) (synth-svg self)
          (nbparams rep) (nbparams self)
          (params-ctrl rep) (params-ctrl self)
          (ui-tree rep) (ui-tree self)
          (is-copy rep) t)
    (las-faust-add-synth-console-to-register rep (synth-ptr rep) (nullsnd rep))
    rep))


(defmethod omNG-save ((self faust-synth-console) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((text (synth-txt self))
        (name (synth-name self))
        (track (tracknum self))
        (copy-state (is-copy self)))
    (if (and (synth-ptr self) (not (las-faust-null-ptr-p (synth-ptr self))))
        (progn
          `(let ((rep (make-instance ',(type-of self))))
             (setf (synth-txt rep) ,(omng-save text)
                   (synth-name rep) ',name
                   (tracknum rep) ',track
                   (is-copy rep) ',copy-state)
             ;;;PROCESS SEPARE SINON CA MOULINE INDEFINIMENT. A VOIR
             ;(if (om-y-or-n-dialog "A Faust synth is trying to compile. Accept?" :default-button t)
             ;    (om-run-process ,name #'(lambda () (build-faust-synth-console rep))))
             (push rep *faust-synths-to-compile*)
             rep))
      (progn
        `(let ((rep (make-instance ',(type-of self))))
           rep)))))


(defun compile-faust-synths ()
  (om-run-process "faust synths compiler" 
                  #'(lambda ()
                      (mapcar 
                       #'(lambda (fx) (build-faust-synth-console fx))
                       *faust-synths-to-compile*)
                      (setf *faust-synths-to-compile* nil))))


(defmethod get-obj-dur ((self faust-synth-console)) (* 1000 (duration self)))


(defmethod object-remove-extra ((self faust-synth-console) box)
  (let* ((ptr (synth-ptr self)))
    (if ptr
        (las-faust-synth-cleanup ptr))
    (if *general-mixer-window*
        (update-general-mixer-synths-lists (car (om-subviews *general-mixer-window*))))))


;================ CONTROLLER EDITOR ===================

(omg-defclass faustSynthcontrollerEditor (EditorView) 
  ((params-panels :initform nil :accessor params-panels :type list)
   (tree :initform nil :accessor tree :type nil)
   (bottom-bar :initform nil :accessor bottom-bar :type t)))


(defmethod make-editor-window ((class (eql 'faustSynthcontrollerEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                :close-p t :winshow t :bg-color *om-dark-gray-color*)))
    win))


(defmethod get-win-ed-size ((self faust-synth-console)) 
  (if (ui-tree self)
      (om-make-point (min 500 (if (> (+ 75 (cadr (las-faust-get-group-size (ui-tree self)))) 500)
                                  (+ (car (las-faust-get-group-size (ui-tree self))) 14)
                                (car (las-faust-get-group-size (ui-tree self))))) (min 500 (+ 75 (cadr (las-faust-get-group-size (ui-tree self))))))
    (om-make-point 75 75)))

(defmethod editor-has-palette-p ((self faustSynthcontrollerEditor)) nil)

(defmethod get-panel-class ((self faustSynthcontrollerEditor)) 'faustSynthcontrollerPanel)

(defmethod update-subviews ((self faustSynthcontrollerEditor))
   (om-set-view-size (panel self) (om-make-point (min 500 (w self)) (min 500 (h self)))))


;=== MAIN PANEL ===
(omg-defclass faustSynthcontrollerPanel (om-scroller) ())

(defmethod get-object ((Self faustSynthcontrollerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self faustSynthcontrollerPanel))
  (report-modifications (om-view-container self)))


;======== Parameters controllers panels =====

(omg-defclass faustSynthparamPanel () 
  ((paramctr :initform nil :initarg :paramctr :accessor paramctr)
   (paramText :initform nil :accessor paramText :type t)
   (paramVal :initform nil :accessor paramVal :type t)
   (paramGraph :initform nil :accessor paramGraph :type t)
   (paramReset :initform nil :accessor paramReset :type t)))


(defclass faustSynthparamPanelview (faustSynthparamPanel om-view) ())

(defmethod update-subviews ((Self faustSynthparamPanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self faustSynthparamPanel))
   (call-next-method))



(defmethod get-object ((Self faustSynthparamPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self faustSynthparamPanel))
  (report-modifications (om-view-container self)))


(defmethod get-parampanel-class ((self faustSynthcontrollerPanel)) 'faustSynthparamPanelview)



;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod metaobj-scrollbars-params ((self faustSynthcontrollerEditor))  '(:h nil))

(defmethod initialize-instance :after ((self faustSynthcontrollerEditor) &rest l)
  (set-edit-param self 'player :libaudio)
  (declare (ignore l))
  (let ((x (if (ui-tree (object self))
               (max 75 (car (las-faust-get-group-size (ui-tree (object self)))))
             75))
        
        (y (if (ui-tree (object self))
               (+ 75 (cadr (las-faust-get-group-size (ui-tree (object self)))))
             75))
        (xwin (om-point-h (get-win-ed-size (object self))))
        (ywin (om-point-v (get-win-ed-size (object self))))
        (orange (om-make-color 1 0.5 0))
        group-type
        groups
        (xgrp 0)
        (ygrp 0)
        (xpars 0)
        (ypars 0)
        (offset 0))
    (if (and (synth-ptr (object self)) (ui-tree (object self)))
        (progn
          (setf (tree self) (ui-tree (object self)))
          (setf group-type (las-faust-get-group-type (tree self)))
          (setf groups (las-faust-get-groups-only (tree self)))
          (setf params (las-faust-get-params-only (tree self)))

          (setf (panel self) (om-make-view (get-panel-class self) 
                                           :owner self
                                           :position (om-make-point 0 0) 
                                           :scrollbars (cond ((and (>= x 500) (>= y 500)) t)
                                                             ((and (>= x 500) (< y 500)) :h)
                                                             ((and (< x 500) (>= y 500)) :v)
                                                             (t nil))
                                           :retain-scrollbars t
                                           :field-size  (om-make-point x y)
                                           :size (om-make-point (w self) (h self))
                                           :bg-color *om-light-gray-color*))
     
          (make-faust-group-view self (tree self))
          (setf paramnum 0)
     
          (setf (bottom-bar self) (om-make-view (get-panel-class self)
                                                :owner (panel self)
                                                :bg-color *om-dark-gray-color*
                                                :position (om-make-point 0 (- y 50))
                                                :size (om-make-point (+ 20 x) 50)))
     
          (om-add-subviews (bottom-bar self) (om-make-dialog-item 'om-button (om-make-point (- (round x 2) 30) 5) (om-make-point 60 24)
                                                                  "SVG"
                                                                  :di-action (om-dialog-item-act item
                                                                               (faust-show-svg *om-outfiles-folder* (synth-dsp (object self)) (synth-svg (object self)))))))
      (progn
        (setf (panel self) (om-make-view (get-panel-class self) 
                                         :owner self
                                         :position (om-make-point 0 0) 
                                         :scrollbars nil
                                         :retain-scrollbars t
                                         :field-size  (om-make-point x (- y 50))
                                         :size (om-make-point (w self) (h self))
                                         :bg-color orange))
        (om-add-subviews (panel self) (om-make-dialog-item 'om-static-text
                                                           (om-make-point 17 0)
                                                           (om-make-point 75 75)
                                                           "X"
                                                           :fg-color *om-white-color*
                                                           :font (om-make-font oa::*om-def-bold-font-face* 48 :style '(:bold))))
        ))))

(defmethod make-faust-param-view ((self faustSynthcontrollerEditor) paractrl x y size)
  (let ((res (om-make-view (get-parampanel-class (panel self))
                :paramctr paractrl
                :owner (panel self)
                :bg-color *om-light-gray-color*
                :position (om-make-point x y)
                :size (om-make-point (car size) (cadr size)))))
    (setf (display paractrl) res)
    res))

(defmethod make-faust-group-view ((self faustSynthcontrollerEditor) group &optional (x 0) (y 0))
  (let* ((grouptype (las-faust-get-group-type group))
         (orange (om-make-color 1 0.5 0))
         (children (las-faust-get-group-items group))
         (numchildren (length children))
         (size (las-faust-get-group-size group))
         childlist)
    ;;///////////////////ON CREE L'ESPACE DU GROUPE
    (om-make-view 'om-view
                  :owner (panel self)
                  :bg-color *om-light-gray-color*
                  :position (om-make-point x y)
                  :size (om-make-point (car size) (cadr size)))
    ;;///////////////////ON CREE LES VIEW DES ENFANTS
    (loop for i from 0 to (- numchildren 1) do
          (if (typep (nth i children) 'faust-group)
              (progn
                (make-faust-group-view self (nth i children) x y)
                (if (string= grouptype "hgroup")
                    (incf x (+ 5 (car (las-faust-get-group-size (nth i children)))))
                  (incf y (+ 5 (cadr (las-faust-get-group-size (nth i children)))))))
            (let* ((type (param-type (nth i children)))
                   (disable 0)
                   (size (cond ((string= type "hslider") hsliderSize)
                               ((string= type "vslider") vsliderSize)
                               ((string= type "checkbox") checkboxSize)
                               ((string= type "numentry") numentrySize)
                               ((string= type "button") buttonSize)
                               (t (progn (setf disable 1) buttonsize)))))
              (if (= disable 0)
                  (make-faust-param-view self (nth paramnum (params-ctrl (object self))) x y size))
              (incf paramnum)
              (if (string= grouptype "hgroup") 
                  (incf x (car size))
                (incf y (cadr size))))))))


(defmethod update-editor-after-eval ((self faustSynthcontrollerEditor) val)
  (setf (object self) val)
  (progn
    (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-view-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-field-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (print (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self))))
    (loop for parampan in (params-panels self) do 
          (om-remove-subviews (panel self) parampan))
    (if (ui-tree (object self))
        (make-faust-group-view self (ui-tree (object self))))
    (setf paramnum 0)))


(defmethod initialize-instance :after ((self faustSynthparamPanel) &rest l)
   (declare (ignore l))
   (do-initialize-param self))


(defmethod do-initialize-param ((self faustSynthparamPanel))  
  (let* ((ptr (synth-ptr (paramctr self)))
         (number (index (paramctr self)))
         (color (om-make-color 0.9 0.9 0.9))
         (orange (om-make-color 1 0.5 0))
         (type (param-type (paramctr self)))
         (name (label (paramctr self)))
         (min (minval (paramctr self)))
         (max (maxval (paramctr self)))
         (def (defval (paramctr self)))
         (range 0)
         (val 0)
         (tracknum (tracknum (paramctr self)))
         (editor (om-view-container (om-view-container self)))
         (curval (las-faust-get-control-value ptr number)))
    (if (= min max) (let () (setf min 0) (setf max 1)) nil)
    (setf range (/ (- max min) 1.0))
    (setf val (* 100.0 (/ (- curval min) range)))
    (om-set-bg-color self color) 

    (cond ((string= type "hslider")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car hsliderSize) 5) 40)
                                                         (format nil "~D" name)
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point (- (round (car hsliderSize) 2) 30) (- (cadr hsliderSize) 31 20)) 
                                                        (om-make-point 60 20)
                                                        (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                        :font *om-default-font1*
                                                        :fg-color orange
                                                        :bg-color color))
             (setf (paramGraph self) (om-make-dialog-item 'om-slider  
                                                          (om-make-point 10 (- (cadr hsliderSize) 36)) 
                                                          (om-make-point 94 31) ""
                                                          :di-action 
                                                          (om-dialog-item-act item
                                                            (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                                              (las-faust-set-control-value ptr number valeur)
                                                              (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                                          :increment 1
                                                          :range '(0 100)
                                                          :value val
                                                          :direction :horizontal
                                                          :tick-side :none))))
          ((string= type "vslider")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car vsliderSize) 15) 50)
                                                         (format nil "~D" name)
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                        (om-make-point 60 20)
                                                        (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                        :font *om-default-font1*
                                                        :fg-color orange
                                                        :bg-color color))
             (setf (paramGraph self)
                   (om-make-dialog-item 'om-slider  
                                        (om-make-point 10 (- (cadr vsliderSize) 104)) 
                                        (om-make-point 31 94) ""
                                        :di-action 
                                        (om-dialog-item-act item
                                          (let ((valeur (+ (* (/ (om-slider-value item) 100.0) range) min)))
                                            (las-faust-set-control-value ptr number valeur)
                                            (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                        :increment 1
                                        :range '(0 100)
                                        :value val
                                        :direction :vertical
                                        :tick-side :none))))
          ((string= type "checkbox")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car checkboxSize) 5) 50)
                                                         (format nil "~D" name)
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                        (om-make-point 60 20)
                                                        ""
                                                        :font *om-default-font1*
                                                        :fg-color *om-blue-color*
                                                        :bg-color color))
             (setf (paramGraph self)
                   (om-make-dialog-item 'om-check-box  
                                        (om-make-point 20 5) 
                                        (om-make-point 31 94) ""
                                        :di-action 
                                        (om-dialog-item-act item 
                                          (if (= (las-faust-get-control-value ptr number) 1.0) 
                                              (las-faust-set-control-value ptr number 0.0) 
                                            (las-faust-set-control-value ptr number 1.0))
                                          (om-set-dialog-item-text (paramVal self) (format nil "~D" (round (las-faust-get-control-value ptr number)))))
                                        :font *om-default-font1*
                                        :checked-p (if (= (las-faust-get-control-value ptr number) 1.0) t nil)))))
          ((string= type "button")
           (progn
             (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                         (om-make-point 5 0) 
                                                         (om-make-point (- (car checkboxSize) 5) 50)
                                                         ""
                                                         :font *om-default-font1*
                                                         :bg-color color))
             (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                        (om-make-point 11 (- (cadr checkboxSize) 94 30)) 
                                                        (om-make-point 60 20)
                                                        ""
                                                        :font *om-default-font1*
                                                        :fg-color *om-blue-color*
                                                        :bg-color color))
             (setf (paramGraph self)
                   (om-make-dialog-item 'om-button  
                                        (om-make-point 20 5) 
                                        (om-make-point 80 30) 
                                        (format nil "~D" name)
                                        :di-action 
                                        (om-dialog-item-act item
                                          (if (= (las-faust-get-control-value ptr number) max) 
                                              (progn
                                                (las-faust-set-control-value ptr number 0.0)
                                                (las-faust-set-control-value ptr number (+ 0.000001 (las-faust-get-control-value ptr number))))
                                            (las-faust-set-control-value ptr number (+ 0.000001 (las-faust-get-control-value ptr number)))
                                            )
                                          (om-set-dialog-item-text (paramVal self) (format nil "~D" (round (las-faust-get-control-value ptr number)))))
                                        :font *om-default-font1*))))
          (t (progn
               (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                           (om-make-point 5 0) 
                                                           (om-make-point (- (car vsliderSize) 15) 50)
                                                           (format nil "~D" name)
                                                           :font *om-default-font1*
                                                           :bg-color color))
               (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                                          (om-make-point 11 (- (cadr vsliderSize) 94 30)) 
                                                          (om-make-point 60 20)
                                                          (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                                          :font *om-default-font1*
                                                          :fg-color orange
                                                          :bg-color color))
               (setf (paramGraph self)
                     (om-make-view 'graphic-numbox :position (om-make-point 10 45) 
                                   :size (om-make-point 31 94)
                                   :pict (om-load-and-store-picture "fader" 'di)
                                   :nbpict 77
                                   :pict-size (om-make-point 31 94)
                                   :di-action (om-dialog-item-act item
                                                (let ((valeur (+ (* (/ (value item) 100.0) range) min)))
                                                  (las-faust-set-control-value ptr number valeur)
                                                  (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                   :font *om-default-font2*
                                   :value val
                                   :min-val 0
                                   :max-val 100)))))

    (om-add-subviews self
                     (paramText self)
                     (paramVal self)
                     (paramGraph self)
                    ;(paramReset self)
                     )))
