;;;================================================================================================================================================================
;;;                                                                            faust
;;;================================================================================================================================================================

(in-package :om)

(defvar *faust-compiler-pathname* nil)

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
   (tracknum :initform 0 :initarg :tracknum :accessor tracknum)))


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
    (ui-tree :initform nil :initarg :ui-tree :accessor ui-tree))
   (:documentation "Faust Effect"))


(defmethod initialize-instance :after ((self faust-effect-console) &rest l)
  (declare (ignore l))
  (let ()
    (if (effect-txt self)
        (let ((parlist (list-of-lines (buffer-text (effect-txt self))))
              effect-string
              effect-result) 

          (loop for line in parlist do
                (setf effect-string (concatenate 'string effect-string (format nil "~%") line)))

          (setf effect-result (las-faust-make-effect effect-string))
          (setf (effect-ptr self) (nth 1 effect-result))
          (setf (effect-dsp self) (format nil "effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))
          (setf (effect-svg self) (format nil "./effect~A-svg/process.svg" (+ 1 (las-get-number-faust-effects-register))))
          (save-data (list (list effect-string)) (format nil "effect~A.dsp" (+ 1 (las-get-number-faust-effects-register))))

          (if (/= (car effect-result) 1)
              (print (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (nth 2 effect-result)))
   
            (let ()
              (print "Effet Faust créé avec succès")
              
              (setf (ui-tree self) (las-faust-parse (las-faust-get-effect-json (effect-ptr self))))

              (if (effect-name self)
                  (setf name (effect-name self))
                (let ()
                  (print "WARNING : You didn't give a name to the effect. A default name will be set.")
                  (setf name "Faust-FX")))

              (setf (nbparams self) (las-faust-get-effect-control-count (effect-ptr self)))
              (if (> (nbparams self) 0)
                  (setf (params-ctrl self)
                        (loop for param from 0 to (- (nbparams self) 1) collect (make-instance 'faust-effect-parameter-controller
                                                                                               :param-type 'vslider ;;;EN ATTENTE
                                                                                               :label (car (las-faust-get-effect-control-params (effect-ptr self) param))
                                                                                               :index param
                                                                                               :defval (cadddr (las-faust-get-effect-control-params (effect-ptr self) param))
                                                                                               :minval (cadr (las-faust-get-effect-control-params (effect-ptr self) param))
                                                                                               :maxval (caddr (las-faust-get-effect-control-params (effect-ptr self) param))
                                                                                               :stepval nil ;;EN ATTENTE
                                                                                               :effect-ptr (effect-ptr self)
                                                                                               :tracknum (tracknum self)
                                                                                               ))) nil)))))))



(defmethod allowed-in-maq-p ((self faust-effect-console))  nil)

(defmethod Class-has-editor-p  ((self faust-effect-console)) t)

(defmethod get-editor-class ((self faust-effect-console)) 'faustcontrollerEditor)

(defmethod draw-mini-view  ((self t) (value faust-effect-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value faust-effect-console)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self faust-effect-console) x x1 y y1 edparams view)
  (let ((w (w view))
        (pic (om-load-and-store-picture "faustlogo-bg" 'internal)))
    (om-draw-picture view pic (om-make-point 0 0) (om-make-point w (h view)))))

;;;ATTENTION : ON CONSIDERE QUE L'ON COPIE JUSTE LA BOITE, VIDE.
;;;POUR COPIER L'EFFET ON DOIT EGALEMENT COPIER LE CODE ET LE NOM, ET EVALUER
(defmethod omNG-copy ((self faust-effect-console))
   "Cons a Lisp expression that return a copy of self when it is valuated."
   `(let ((rep (make-instance ',(type-of self))))
      rep))
(defmethod copy-container  ((self faust-effect-console) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self))))
    rep))

;;;ATTENTION : POUR LA SAUVEGARDE, ON SAUVEGARDE JUSTE LA BOITE VIDE EGALEMENT.
;;;LE CODE ASSOCIE AINSI QUE LE NOM ETANT SAUVEGARDES, IL SUFFIT DE REEVALUER LA BOITE
;;;LA POSITION DES SLIDERS EST DONC REINITIALISEE, SEULS LES AUTOMATIONS PEUVENT ETRE CONSERVEES
(defmethod omNG-save ((self faust-effect-console) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self))))
     rep))

(defmethod get-obj-dur ((self faust-effect-console)) 0)


(defmethod object-remove-extra ((self faust-effect-console) box)
  (let* ((ptr (effect-ptr self)))
    (if ptr
        (las-faust-effect-cleanup ptr))))


;================ CONTROLLER EDITOR ===================

(omg-defclass faustcontrollerEditor (EditorView) 
  ((params-panels :initform nil :accessor params-panels :type list)
   (bottom-bar :initform nil :accessor bottom-bar :type t)))


(defmethod make-editor-window ((class (eql 'faustcontrollerEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))


(defmethod get-win-ed-size ((self faust-effect-console)) 
  (let* ((n (nbparams self))
         (paramlist (params-ctrl self))
         (parameter nil)
         (parameter-type nil)
         (nbutton 0)
         (ncheckbox 0)
         (nhslider 0)
         (nvslider 0)
         (nnumentry 0)
         (xmax 0)
         (ymax 0)
         (xbutton (car buttonSize))
         (ybutton (cadr buttonSize))
         (xcheckbox (car checkboxSize))
         (ycheckbox (cadr checkboxSize))
         (xhslider (car hsliderSize))
         (yhslider (cadr hsliderSize))
         (xvslider (car vsliderSize))
         (yvslider (cadr vsliderSize))
         (xnumentry (car numentrySize))
         (ynumentry (cadr numentrySize))
         (xlist nil)
         (ylist nil))
    (loop for i from 0 to (- n 1) do
          (let ()
            (setf parameter (nth i paramlist))
            (setf parameter-type (param-type parameter))
            (cond ((eql parameter-type 'button) (incf nbutton))
                  ((eql parameter-type 'checkbox) (incf ncheckbox))
                  ((eql parameter-type 'hslider) (incf nhslider))
                  ((eql parameter-type 'vslider) (incf nvslider))
                  ((eql parameter-type 'numentry) (incf nnumentry)))))
    ;(setf xlist (list (* nbutton xbutton) (* ncheckbox xcheckbox) (* nhslider xhslider) (* nvslider xvslider) (* nnumentry xnumentry)))
    ;(setf ylist (list (* nbutton ybutton) (* ncheckbox ycheckbox) (* nhslider yhslider) (* nvslider yvslider) (* nnumentry ynumentry)))
    ;(setf xmax (apply 'max xlist))
    ;(setf ymax (apply 'max ylist))
    (setf xmax (* nvslider (+ 30 xvslider)))
    (setf ymax (+ 100 (cadr vsliderSize)))
    (om-make-point (if (= n 0) 60 xmax) (if (= n 0) 50 (+ ymax 50)))))



(defmethod editor-has-palette-p ((self faustcontrollerEditor)) nil)

(defmethod get-panel-class ((self faustcontrollerEditor)) 'faustcontrollerPanel)

(defmethod update-subviews ((self faustcontrollerEditor))
   (om-set-view-size (panel self) (om-make-point (w self) (h self))))


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
   (let ((x (om-point-x (get-win-ed-size (object self))))
         (y (om-point-y (get-win-ed-size (object self))))
         (nbutton -1)
         (ncheckbox -1)
         (nhslider -1)
         (nvslider -1)
         (nnumentry -1)
         (xbutton (car buttonSize))
         (ybutton (cadr buttonSize))
         (xcheckbox (car checkboxSize))
         (ycheckbox (cadr checkboxSize))
         (xhslider (car hsliderSize))
         (yhslider (cadr hsliderSize))
         (xvslider (car vsliderSize))
         (yvslider (cadr vsliderSize))
         (xnumentry (car numentrySize))
         (ynumentry (cadr numentrySize))
         (hbutton 0)
         (hcheckbox (cadr buttonSize))
         (hhslider (+ (cadr checkboxSize) (cadr buttonSize)))
         (hvslider (+ (cadr hsliderSize) (cadr checkboxSize) (cadr buttonSize)))
         (hnumentry (+ (cadr vsliderSize) (cadr hsliderSize) (cadr checkboxSize) (cadr buttonSize))))
     (setf (panel self) (om-make-view (get-panel-class self) 
                                      :owner self
                                      :position (om-make-point 0 0) 
                                      :scrollbars (first (metaobj-scrollbars-params self))
                                      :retain-scrollbars (second (metaobj-scrollbars-params self))
                                      :field-size  (om-make-point x (- y 50))
                                      :size (om-make-point (w self) (h self))))
     
     (setf (params-panels self) 
           (loop for paractrl in (params-ctrl (object self))
                 for i = 0 then (+ i 1) collect
                 (om-make-view (get-parampanel-class (panel self))
                               :paramctr paractrl
                               :owner (panel self)
                               :bg-color *om-light-gray-color*
                               :position (om-make-point (* 60 i) 0)
                               :size (om-make-point 60 200))))
     
     (setf (bottom-bar self) (om-make-view (get-panel-class self)
                                           :owner (panel self)
                                           :bg-color *om-dark-gray-color*
                                           :position (om-make-point 0 (- y 50))
                                           :size (om-make-point x 50)))
     
     (om-add-subviews (bottom-bar self) (om-make-dialog-item 'om-button (om-make-point (- (round x 2) 30) 5) (om-make-point 60 24)
                                          "SVG"
                                          :di-action (om-dialog-item-act item
                                                       (faust-show-svg *om-outfiles-folder* (effect-dsp (object self)) (effect-svg (object self))))))))



(defun faust-show-svg (pathname dsp svg)
  (let (res)
    (if *faust-compiler-pathname*
        (progn
          (setf res (om-cmd-line (format nil "~A faust ~A -svg" *faust-compiler-pathname* dsp) nil t pathname))
          (cond ((= res 0) (om-cmd-line (format nil "open ~A" svg) nil t pathname))
                ((= res 126) (progn
                               (om-message-dialog "The compiler you chose is not an executable. please locate it again.")
                               (set-faust-compiler-pathname)))
                ((= res 127) (progn
                               (om-message-dialog "Command not found! Please locate your Faust compiler again.")
                               (set-faust-compiler-pathname)))
                ((= res 2) (print "Incorrect usage of this command"))
                (t (print "Failure"))))
      (progn
        (if (set-faust-compiler-pathname)
            (faust-show-svg pathname dsp svg))))))



(defun set-faust-compiler-pathname ()
  (let (new)
    (setf new (om-choose-file-dialog 
     :directory (om-user-home)
     :prompt "Locate your Faust Compiler" :types (list (om-str :all-files) "*.*")))
    (if new
        (setf *faust-compiler-pathname* new))))

(defmethod update-editor-after-eval ((self faustcontrollerEditor) val)
  (setf (object self) val)
  (let ((n (nbparams (object self))))
    (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (om-set-field-size (panel self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (get-win-ed-size (object self)))))
    (loop for parampan in (params-panels self) do 
          (om-remove-subviews (panel self) parampan))
    (setf (params-panels self) 
          (loop for paractrl in (params-ctrl (object self))
            for i = 0 then (+ i 1) collect
            (om-make-view (get-parampanel-class (panel self))
                          :paramctr paractrl
                          :owner (panel self)
                          :bg-color *om-light-gray-color*
                          :position (om-make-point (* 60 i) 0)
                          :size (om-make-point 60 200))))))


(defmethod initialize-instance :after ((self faustparamPanel) &rest l)
   (declare (ignore l))
   (do-initialize-param self))



(defmethod do-initialize-param ((self faustparamPanel))  
   (let* ((ptr (effect-ptr (paramctr self)))
          (number (index (paramctr self)))
          (color (om-make-color 0.9 0.9 0.9))
          (type (param-type (paramctr self)))
          (name (label (paramctr self)))
          (min (minval (paramctr self)))
          (max (maxval (paramctr self)))
          (def (defval (paramctr self)))
          (range 0)
          (val 0)
          (tracknum (tracknum (paramctr self)))
          (editor (om-view-container (om-view-container self)))
          (curval (las-faust-get-effect-control-value ptr number)))
   (if (= min max) (let () (setf min 0) (setf max 1)) nil)
   (setf range (/ (- max min) 1.0))
   (setf val (* 100.0 (/ (- curval min) range)))
   (om-set-bg-color self color) 
   (setf (paramText self) (om-make-dialog-item 'om-static-text
                                                (om-make-point 5 0) 
                                                (om-make-point 60 15)
                                                (format nil "~D" name)
                                                :font *om-default-font1*
                                                :bg-color color))
   (setf (paramVal self) (om-make-dialog-item 'om-static-text 
                                              (om-make-point 11 15) 
                                              (om-make-point 60 30)
                                              (if (<= range 100) (format nil "~$" curval) (format nil "~D" (round curval)))
                                              :font *om-default-font1*
                                              :bg-color color
                                              ))
   (setf (paramGraph self) (om-make-view 'graphic-numbox :position (om-make-point 10 45) 
                                         :size (om-make-point 31 94)
                                         :pict (om-load-and-store-picture "fader" 'di)
                                         :nbpict 77
                                         :pict-size (om-make-point 31 94)
                                         :di-action (om-dialog-item-act item
                                                      (let ((valeur (+ (* (/ (value item) 100.0) range) min)))
                                                        (las-faust-set-effect-control-value ptr number valeur)
                                                        (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))))
                                         :font *om-default-font2*
                                         :value val
                                         :min-val 0
                                         :max-val 100))

   (setf (paramReset self) (om-make-view 'om-icon-button :position (om-make-point 15 150) :size (om-make-point 18 18)
                                         :icon1 "-" :icon2 "--pushed"
                                         :action #'(lambda (item) 
                                                     (let () 
                                                       (las-faust-set-effect-control-value ptr number def) (set-value (paramGraph self) val)
                                                       (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" def) 
                                                                                                  (format nil "~D" (round def))))))))
   (om-add-subviews self 
                    (paramText self) 
                    (paramVal self) 
                    (paramGraph self)
                    (paramReset self))))






;;;================================================================================================================================================================
;;;                                                                            faust pool
;;;================================================================================================================================================================

(in-package :om)


(defclass* faust-effect-controller () 
           ((effect-console :initform nil :initarg :effect-console :accessor effect-console)))


(defclass* faust-pool (simple-score-element)
           ((list-of-effects :initarg :list-of-effects :initform nil :accessor list-of-effects :documentation "A list of Faust-effect-console")
            (effect-list :initform (list) :accessor effect-list :documentation "A list of Faust effects"))
           (:documentation 
            "Faust Pool is used to collect a list of Faust Effects and manage them through the track system.
            You need ONLY ONE Faust Pool, or you might make mistake by pluging the same effect many times.
            The Faust Pool display is as it's described below :
                         -Left Panel : Summary of registered effects in the pool, with their affected track. You can change it dynamically.
                         -Right Panel : Summary of the effects by tracks. Here you can change the order of the effect list."))


(defmethod initialize-instance :after ((self faust-pool) &rest l)
  (declare (ignore l))
  (let ((nbeffects (length (list-of-effects self)))
        (i 0)
        plugres)
    (loop for effect in (list-of-effects self) do 
          (if (typep effect 'faust-effect-console)
              (if (and (effect-ptr effect) (not (las-faust-null-ptr-p (effect-ptr effect))))
                  (progn
                    (setf plugres (las-faust-effect-already-plugged-? (effect-ptr effect)))
                    (setf (effect-list self) (append (effect-list self) (list (make-instance 'faust-effect-controller
                                                                                             :effect-console effect))))
                    (if (not plugres)
                        (if (> (tracknum effect) 0)
                            (las-faust-add-effect-to-track (effect-ptr effect) (or (effect-name effect) (format nil "Faust-FX ~A" i)) (tracknum effect))))
                    (incf i)))))))


(defmethod allowed-in-maq-p ((self faust-pool))  nil)

(defmethod Class-has-editor-p  ((self faust-pool)) t)

(defmethod get-editor-class ((self faust-pool)) 'faust-pool-editor)

(defmethod draw-mini-view  ((self t) (value faust-pool)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value faust-pool)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self faust-pool) x x1 y y1 edparams view)
  (let ((w (w view))
        (pic (om-load-and-store-picture "faustlogo-pool-bg" 'internal)))
    (om-draw-picture view pic (om-make-point 0 0) (om-make-point w (h view)))))

;;;ATTETION VOIR POUR LA COPIE
(defmethod omNG-copy ((self faust-pool))
   "Cons a Lisp expression that return a copy of self when it is valuated."
   `(let ((rep (make-instance ',(type-of self))))
      rep
      ))

(defmethod copy-container  ((self faust-pool) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self))))
    rep
    ))

(defmethod omNG-save ((self faust-pool) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self))))
     rep
     ))

(defmethod get-obj-dur ((self faust-pool)) 0)


(omg-defclass faust-pool-editor (EditorView) 
  ((effect-panels :initform nil :initarg :effect-panels :accessor effect-panels)
   (recap :initform nil :initarg :recap :accessor recap)
   (recap-track :initform 0 :initarg :recap-track :accessor recap-track)
   (recap-item-list :initform nil :initarg :recap-item-list :accessor recap-item-list)
   (track-effect-list :initform nil :initarg :track-effect-list :accessor track-effect-list)))

(defmethod make-editor-window ((class (eql 'faust-pool-editor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))

(defmethod get-win-ed-size ((self faust-pool))
  (om-make-point (* 2 300) (max 200 (* 30 (length (effect-list self)))))
  )

(defmethod editor-has-palette-p ((self faust-pool-editor)) nil)

(defmethod get-panel-class ((self faust-pool-editor)) 'faust-pool-panel)

(defmethod update-subviews ((self faust-pool-editor))
   (om-set-view-size (panel self) (om-make-point (w self) (h self))))


(omg-defclass faust-pool-panel (om-scroller) ())

(defmethod get-object ((Self faust-pool-panel))
   (object (om-view-container self)))

(defmethod report-modifications ((self faust-pool-panel))
  (report-modifications (om-view-container self)))


(omg-defclass faust-effect-panel () 
  ((effect :initform nil :initarg :effect :accessor effect)
   (tracknum :initform nil :initarg :tracknum :accessor paramVal)))


(defclass faust-effect-panel-view (faust-effect-panel om-view) ())

(defmethod update-subviews ((Self faust-effect-panel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self faust-effect-panel))
   (call-next-method))



(defmethod get-object ((Self faust-effect-panel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self faust-effect-panel))
  (report-modifications (om-view-container self)))


(defmethod get-effectpanel-class ((self faust-pool-panel)) 'faust-effect-panel-view)



;=======================
;=== INITIALIZATIONS ===
;=======================
(defvar *track-name-list* nil) 
(loop for i from las-channels downto 1 do  (push (format nil "Track ~A" i) *track-name-list*))

(defmethod metaobj-scrollbars-params ((self faust-pool-editor))  '(:h t))

(defmethod initialize-instance :after ((self faust-pool-editor) &rest l)
   (declare (ignore l))
   (let ((x (om-point-x (get-win-ed-size (object self))))
         (y (om-point-y (get-win-ed-size (object self))))
         (color (om-make-color 0.9 0.9 0.9))
         (pool (object self))) (print (effect-list (object self)))
     (setf (panel self) (om-make-view (get-panel-class self) 
                                                     :owner self
                                                     :position (om-make-point 0 0) 
                                                     :scrollbars nil
                                                     :retain-scrollbars nil
                                                     :field-size  (om-make-point x y)
                                                     :size (om-make-point (w self) (h self))))
     (setf (effect-panels self)
           (loop for eff in (effect-list (object self))
            for i = 0 then (+ i 1) collect
            (om-make-view 'faust-effect-panel-view
                          :effect eff
                          :owner (panel self)
                          :bg-color *om-white-color*
                          :position (om-make-point 0 (* 30 i))
                          :size (om-make-point (round x 2) 200))))
     (setf (recap self) (om-make-view (get-panel-class self) 
                                      :owner self
                                      :position (om-make-point 300 0)
                                      :bg-color *om-dark-gray-color*
                                      :scrollbars nil
                                      :retain-scrollbars nil
                                      :field-size  (om-make-point (round x 2) 200)
                                      :size (om-make-point (w self) (h self))))
     (setf track-menu (om-make-dialog-item 'om-pop-up-dialog-item 
                                                (om-make-point (- (round 300 2) 45) 25) 
                                                (om-make-point 90 20) ""
                                                :font *om-default-font1*
                                                :range *track-name-list*
                                                :value nil
                                                :di-action  (om-dialog-item-act item
                                                              (update-effect-list-display self (om-get-selected-item-index item)))))
     (setf (track-effect-list self)
           (om-make-view (get-panel-class self)
                                      :owner self
                                      :position (om-make-point 50 50)
                                      :bg-color *om-white-color*
                                      :scrollbars nil
                                      :retain-scrollbars nil
                                      :field-size  (om-make-point 200 (- y 80))
                                      :size (om-make-point 200 (- y 80))))
     (setf move-up (om-make-dialog-item 'om-button (om-make-point 5 (round (- y 80) 2)) (om-make-point 40 24)  "+"
                                        :di-action (om-dialog-item-act item 
                                                     (if (las-faust-get-track-effects-pointer (om-get-selected-item-index track-menu))
                                                         (let* ((tracknum (om-get-selected-item-index track-menu))
                                                                (effectnum (om-get-selected-item-index (recap-item-list self)))
                                                                (namelist (las-faust-get-track-effects-name tracknum))
                                                                (ptrlist (las-faust-get-track-effects-pointer tracknum))
                                                                (max (length namelist))
                                                                select-ptr select-name next pivot-ptr pivot-name)
                                                           (if effectnum
                                                               (progn
                                                                 (setf select-ptr (nth effectnum ptrlist))
                                                                 (setf select-name (nth effectnum namelist))
                                                                 (setf next (if (>= (- effectnum 1) 0) (- effectnum 1) 0))
                                                                 (setf pivot-ptr (nth next ptrlist))
                                                                 (setf pivot-name (nth next namelist))
                                                                 (if (/= 0 effectnum)
                                                                     (progn
                                                                       (loop for ptr in ptrlist do
                                                                             (las-faust-remove-effect-from-track ptr tracknum))
                                                                       (setf (nth effectnum ptrlist) pivot-ptr)
                                                                       (setf (nth effectnum namelist) pivot-name)
                                                                       (setf (nth next ptrlist) select-ptr)
                                                                       (setf (nth next namelist) select-name)
                                                                       (loop for ptr in ptrlist do
                                                                             for name in namelist do
                                                                             (las-faust-add-effect-to-track ptr name tracknum))
                                                                       (update-effect-list-display self tracknum)
                                                                       (om-set-selected-item-index (recap-item-list self) next)
                                                                       )))
                                                         ))))))
     (setf move-down (om-make-dialog-item 'om-button (om-make-point 5 (+ (round (- y 80) 2) 30)) (om-make-point 40 24)  "-" 
                                            :di-action (om-dialog-item-act item
                                                         (if (las-faust-get-track-effects-pointer (om-get-selected-item-index track-menu))
                                                             (let* ((tracknum (om-get-selected-item-index track-menu))
                                                                    (effectnum (om-get-selected-item-index (recap-item-list self)))
                                                                    (namelist (las-faust-get-track-effects-name tracknum))
                                                                    (max (length namelist))
                                                                    (ptrlist (las-faust-get-track-effects-pointer tracknum))
                                                                    select-ptr select-name next pivot-ptr pivot-name)
                                                               (if effectnum
                                                                   (progn
                                                                     (setf select-ptr (nth effectnum ptrlist))
                                                                     (setf select-name (nth effectnum namelist))
                                                                     (setf pivot-ptr (nth (+ effectnum 1) ptrlist))
                                                                     (setf pivot-name (nth (+ effectnum 1) namelist))
                                                                     (if (/= (- max 1) effectnum)
                                                                         (progn
                                                                           (loop for ptr in ptrlist do
                                                                                 (las-faust-remove-effect-from-track ptr tracknum))
                                                                           (setf (nth effectnum ptrlist) pivot-ptr)
                                                                           (setf (nth effectnum namelist) pivot-name)
                                                                           (setf (nth (+ effectnum 1) ptrlist) select-ptr)
                                                                           (setf (nth (+ effectnum 1) namelist) select-name)
                                                                           (loop for ptr in ptrlist do
                                                                                 for name in namelist do
                                                                                 (las-faust-add-effect-to-track ptr name tracknum))
                                                                           (update-effect-list-display self tracknum)
                                                                           (om-set-selected-item-index (recap-item-list self) (+ effectnum 1))
                                                                           )))))))))
     (om-add-subviews (recap self) 
                      move-up
                      move-down
                      (om-make-dialog-item 'om-static-text (om-make-point (- (round 300 2) 48) 5) (om-make-point 96 20)
                                          "Effects by tracks" :font *om-default-font1* :fg-color *om-white-color*)
                      track-menu
                      (track-effect-list self))
     (update-effect-list-display self (recap-track self))))

(defmethod update-effect-list-display ((self faust-pool-editor) track)
  (declare (ignore l))
  (setf (recap-track self) track)
  (let ((effect-list (las-faust-get-track-effects-name track))
        (res (list))
        (i 0)
        (x (om-point-x (get-win-ed-size (object self))))
        (y (om-point-y (get-win-ed-size (object self)))))
    (loop for subv in (om-subviews (track-effect-list self)) do
          (om-remove-subviews (track-effect-list self) subv))
    (setf (recap-item-list self) (om-make-dialog-item 'om-single-item-list 
                         (om-make-point 0 0) 
                         (om-make-point 215 (- y 80)) 
                         "Effects on this track"  
                         :scrollbars :v
                         :bg-color *om-white-color*
                         :di-action (om-dialog-item-act item (om-get-selected-item item))
                         :range effect-list
                         :container (track-effect-list self)))))

(defmethod initialize-instance :after ((self faust-effect-panel) &rest l)
   (declare (ignore l))
   (do-initialize-effect self))

(defmethod do-initialize-effect ((self faust-effect-panel))  
  (print (effect self)) 
  (print (effect-console (effect self)))
  (let* ((color *om-light-gray-color*)
          (effect (effect self))
          (console (effect-console effect))
          (name (effect-name console))
          (ptr (effect-ptr console))
          (pool (om-view-container (om-view-container self))))
   (om-set-bg-color self color)
     (setf nameview (om-make-dialog-item 'om-static-text
                                                  (om-make-point 5 3) 
                                                  (om-make-point 200 19)
                                                  (format nil "~D" name)
                                                  :font *om-default-font1*
                                                  :bg-color color))
     (setf tracktextview (om-make-dialog-item 'om-static-text
                                                  (om-make-point (+ 5 200) 3) 
                                                  (om-make-point 50 19)
                                                  (format nil "Track :")
                                                  :font *om-default-font1*
                                                  :bg-color color))
     (setf trackview (om-make-dialog-item 'numBox
                                          (om-make-point (+ 5 200 50) 3)
                                          (om-make-point 30 19) (format () " ~D" (tracknum console))
                                          :min-val 0
                                          :max-val 32
                                          :bg-color *om-white-color*
                                          :font *om-default-font1*
                                          :value (tracknum console)
                                          :afterfun #'(lambda (item) 
                                                        (let ((trackdest (- (value item) 1))
                                                              (trackorigin (- (tracknum console) 1)))
                                                          (cond ((= trackdest trackorigin) nil)
                                                                (t (progn 
                                                                     (if (< trackdest 0)
                                                                         (if (>= trackorigin 0)
                                                                             (let ()
                                                                               (las-faust-remove-effect-from-track ptr trackorigin)
                                                                               (report-modifications self)))
                                                                       (let ()
                                                                         (if (>= trackorigin 0)
                                                                             (let ()
                                                                               (las-faust-remove-effect-from-track ptr trackorigin)
                                                                               (las-faust-add-effect-to-track ptr name trackdest)
                                                                               (report-modifications self))
                                                                           (let ()
                                                                             (las-faust-add-effect-to-track ptr name trackdest)
                                                                             (report-modifications self)))))
                                                                     (setf (tracknum console) (value item))
                                                                     (update-effect-list-display pool (recap-track pool)))))))))
     (om-add-subviews self
                      nameview
                      tracktextview
                      trackview)))

