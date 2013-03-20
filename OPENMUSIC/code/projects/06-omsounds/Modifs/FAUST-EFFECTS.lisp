;;;================================================================================================================================================================
;;;                                                                            faust
;;;================================================================================================================================================================

(in-package :om)


(defmethod remove-extra ((self OMPatch) (box OMBoxEditCall))
  (if (typep (value box) 'faust-effect-console)
      (let* ((console (value box))
             (ptr (effect-ptr console))
             (track (tracknum console)))
        (las::RemoveAudioEffect (gethash track *effects-lists*) ptr)
        ))
  (call-next-method)
  )

;======================================================
;===         CREATE 32 EMPTY EFFECTS LISTS          ===
;======================================================
(defvar *effects-lists* nil)

(defun plug-faust-effect-list-on-channel (player effectlist channel &optional (fadein 100) (fadeout 100))
  (las::SetEffectListChannel player channel effectlist fadein fadeout))

(defun ResetEffectsLists ()
  (setf *effects-lists* (make-hash-table))
  (loop for i from 0 to 31 do 
      (setf (gethash i *effects-lists*) (las::MakeAudioEffectList))
      (plug-faust-effect-list-on-channel *audio-player* (gethash i *effects-lists*) i)))

; (ResetEffectsLists)

;======================================================
;===         DEAL WITH DEAD PORTAUDIO               ===
;======================================================

;(defun ResetAudioPlayer ()
;  (let ()
 ;   (las::CloseAudioPlayer *audio-player*)
 ;   (setf *audio-player* (las::OpenAudioPlayer 0 2 32 44100 512 65536 65536 las::kCoreAudioRenderer 1))
 ;   (las::StartAudioPlayer *audio-player*)))

;(ResetAudioPlayer)

;(las::OpenAudioPlayer *om-player-n-channels* *om-player-n-channels* 32 *om-player-sample-rate* 512 65536 65536 las::kPortAudioRenderer 1)
(defun oa::om-open-audio-player ()
  (let ((player (las::OpenAudioPlayer oa::*om-player-n-channels* oa::*om-player-n-channels* 32 oa::*om-player-sample-rate* 512 65536 65536 las::kCoreAudioRenderer 1)))
    (las::StartAudioPlayer *audio-player*)
    (ResetEffectsLists)
    player))


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
   ((effect-txt :initform nil :initarg :effect-txt :accessor effect-txt)
    (effect-ptr :initform nil :accessor effect-ptr)
    (tracknum :initform 0 :initarg :tracknum :accessor tracknum)
    (nbparams :initform 0 :accessor nbparams :type t :documentation "number of parameters in the effect")
    (params-ctrl :initform nil :accessor params-ctrl :type t))
   (:documentation "Faust Effect"))


(defmethod initialize-instance :after ((self faust-effect-console) &rest l)
  (declare (ignore l))
  (if (= (tracknum self) 0) () (setf (tracknum self) (- (tracknum self) 1)))
  (if (effect-txt self)
      (let ((parlist (list-of-lines (buffer-text (effect-txt self))))
            (effect-code))
        (loop for line in parlist do
              (setf effect-code (concatenate 'string effect-code " " line)))
        (setf (effect-ptr self) (las::MakeFaustAudioEffect effect-code))
        (if (las::las-null-ptr-p (effect-ptr self)) 
            (print (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (las::getlastliberror)))
          (let* ((ptr (effect-ptr self)) 
                (effect-json (yason::parse (las::GetJsonEffect ptr) :object-as :alist)))
            (print (format nil "Effet Faust ~A créé avec succès ~%Cet effet s'applique sur la piste ~A" ptr (+ 1 (tracknum self))))
            (las::AddAudioEffect (gethash (tracknum self) *effects-lists*) ptr)
            ;(print (format nil "~%~%~%~A~%~%~%" (car (nth 1 (car effect-json)))))
            (setf (nbparams self) (las::getcontrolcount ptr))
            (if (> (nbparams self) 0)
                  (setf (params-ctrl self)
                        (loop for param from 0 to (- (nbparams self) 1) collect (make-instance 'faust-effect-parameter-controller
                                                                                               :param-type 'vslider ;;;EN ATTENTE
                                                                                               :label (car (las::getcontrolparam (effect-ptr self) param))
                                                                                               :index param
                                                                                               :defval (cadddr (las::getcontrolparam (effect-ptr self) param))
                                                                                               :minval (cadr (las::getcontrolparam (effect-ptr self) param))
                                                                                               :maxval (caddr (las::getcontrolparam (effect-ptr self) param))
                                                                                               :stepval nil ;;EN ATTENTE
                                                                                               :effect-ptr ptr
                                                                                               :tracknum (tracknum self)
                                                                                               ))) nil)))) nil))




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

;;;ATTETION VOIR POUR LA COPIE
(defmethod omNG-copy ((self faust-effect-console))
   "Cons a Lisp expression that return a copy of self when it is valuated."
   `(let ((rep (make-instance ',(type-of self))))
      rep
      ))

(defmethod copy-container  ((self faust-effect-console) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self))))
    rep
    ))

(defmethod omNG-save ((self faust-effect-console) &optional (values? nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self))))
     rep
     ))

(defmethod get-obj-dur ((self faust-effect-console)) 0)

;================ CONTROLLER EDITOR ===================

(omg-defclass faustcontrollerEditor (EditorView) 
  ((params-panels :initform nil :accessor params-panels :type list)))


(defmethod make-editor-window ((class (eql 'faustcontrollerEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))


(defconstant buttonSize (list 30 20))
(defconstant checkboxSize (list 30 20))
(defconstant hsliderSize (list 50 20))
(defconstant vsliderSize (list 30 94))
(defconstant numentrySize (list 20 15))




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
    (om-make-point xmax ymax)))



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
                                                     :field-size  (om-make-point x y)
                                                     :size (om-make-point (w self) (h self))))
   
   (setf (params-panels self) 
      (loop for paractrl in (params-ctrl (object self))
            for i = 0 then (+ i 1) collect
            (om-make-view (get-parampanel-class (panel self))
                          :paramctr paractrl
                          :owner (panel self)
                          :bg-color *om-light-gray-color*
                          :position (om-make-point (* 60 i) 0)
                          :size (om-make-point 60 200))))))




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
          (curval (las::GetControlValue ptr number)))
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
                                                        (las::SetControlValue ptr number valeur)
                                                        (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" valeur) (format nil "~D" (round valeur))))) 
                                                      )
                                         :font *om-default-font2*
                                         :value val
                                         :min-val 0
                                         :max-val 100))

   (setf (paramReset self) (om-make-view 'om-icon-button :position (om-make-point 15 150) :size (om-make-point 18 18)
                                         :icon1 "-" :icon2 "--pushed"
                                         :action #'(lambda (item) (let () (las::SetControlValue ptr number def) (set-value (paramGraph self) val)))
                                         ))
   (om-add-subviews self (paramText self) 
                    (paramVal self) 
                    (paramGraph self)
                    (paramReset self))))

;(ResetAudioPlayer)
;(ResetEffectsLists)