;;;================================================================================================================================================================
;;;                                                                            faust
;;;================================================================================================================================================================

(in-package :om)


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
    (effect-name :initform nil :initarg :effect-name :accessor effect-name)
    ;(tracknum :initform nil :initarg :tracknum :accessor tracknum)
    ;(sound :initform nil :initarg :sound :accessor sound)
    (nbparams :initform 0 :accessor nbparams :type t :documentation "number of parameters in the effect")
    (params-ctrl :initform nil :accessor params-ctrl :type t)
    (ui-type :initform nil :accessor ui-type)
    (ui-name :initform nil :accessor ui-name))
   (:documentation "Faust Effect"))


(defmethod initialize-instance :after ((self faust-effect-console) &rest l)
  (declare (ignore l))
  (let ()
    ;(if (tracknum self) 
    ;    (if (< (tracknum self) 1) 
    ;        (let () (print "The track number is invalid") 
    ;          (setf (tracknum self) nil)) 
    ;      (setf (tracknum self) (- (tracknum self) 1))))
    ;(if (sound self)
    ;    (if (not (typep (sound self) 'sound))
    ;        (print "You must connect a sound to the sound input.")))
    (if (effect-txt self)
        (let ((parlist (list-of-lines (buffer-text (effect-txt self))))
              (effect-code))
          (loop for line in parlist do
                (setf effect-code (concatenate 'string effect-code " " line)))
          (setf (effect-ptr self) (las::MakeFaustAudioEffect effect-code))
          (if (las::las-null-ptr-p (effect-ptr self)) 
              (print (format nil "~%Votre effet n'a pas pu être créé. Faust a renvoyé l'erreur suivante : ~%~A" (las::getlastliberror)))
            (let* ((ptr (effect-ptr self)) 
                   (effect-json (yason::parse (las::GetJsonEffect ptr) :object-as :plist))
                   (effect-ui (nth (- (length effect-json) 1) effect-json))
                   (name (nth 1 effect-json)))
              (print "Effet Faust créé avec succès")
              (if (effect-name self)
                  (setf name (effect-name self))
                (let ()
                  (print "WARNING : You didn't give a name to the effect. If there is no name definition in your Faust code, a default name will be set.")
                  (if (string= name "") (setf name "Faust-FX"))))

              (add-faust-effect-to-pool ptr name)

              ;///////////////////JSON parsing///////////////////////
              ;(setf (ui-type self) (cdr (nth (- (length (nth 1 (nth 0 effect-json))) 1) (nth 1 (nth 0 effect-json)))))
              ;(setf (ui-name self) (cdr (nth (- (length (nth 1 (nth 0 effect-json))) 2) (nth 1 (nth 0 effect-json)))))
              ;(print effect-json)
              ;(print effect-ui)
              ;(print (ui-name self))
              ;(print (length ui-items))
              ;(print (cdr (nth (- (length (nth 1 (nth 0 effect-json))) 2) (nth 1 (nth 0 effect-json)))))

              ;//////////////////////////////////////////////////////

              ;(if (tracknum self)
              ;    (let ()
              ;         (las::AddAudioEffect (gethash (tracknum self) oa::*effects-lists*) ptr)
              ;         (print (format nil "Cet effet s'applique sur la piste ~A" (+ 1 (tracknum self))))))
              ;(if (sound self)
              ;    (let ((temp-effect-list (las::MakeAudioEffectList)))
              ;      (if (not (oa::sndlasptr (sound self))) (oa::om-fill-sound-info (sound self)))
              ;        (las::AddAudioEffect temp-effect-list ptr)
              ;        (if (= (oa::current-is-original (sound self)) 1)
              ;            (setf (oa::sndlasptr-current-save (sound self)) (las::MakeTransformSound (oa::sndlasptr-current (sound self)) temp-effect-list 100 100))
              ;          (setf (oa::sndlasptr-current (sound self)) (las::MakeTransformSound (oa::sndlasptr-current (sound self)) temp-effect-list 100 100)))
              ;        (print (format nil "Cet effet s'applique sur l'objet sound ~A" (sound self)))
              ;         ;(update-buffer-with-current-las (sound self))
              ;      ))

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
                                                                                               ;:tracknum (tracknum self)
                                                                                               ))) nil))))
      ;(print "You are evaluating the Faust console without any Faust code as an input. It has no effect.")
      )))

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


(defmethod object-remove-extra ((self faust-effect-console) box)
  (let* ((ptr (effect-ptr self)))
    (if ptr
        (let ((n (get-number-faust-effects-pool))
              (track (- (nth 1 (gethash (find-effect-index-in-pool ptr) *faust-effects-pool*)) 1)))
          (if (>= track 0)
              (las::RemoveAudioEffect (gethash track oa::*effects-lists*) ptr))
          (setf (gethash (find-effect-index-in-pool ptr) *faust-effects-pool*) (list nil 0 "faust-effect"))
          (pack-faust-effects-pool n)
          ))))


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
                                         :action #'(lambda (item) (let () (las::SetControlValue ptr number def) (set-value (paramGraph self) val)
                                                                    (om-set-dialog-item-text (paramVal self) (if (<= range 100) (format nil "~$" def) 
                                                                                                               (format nil "~D" (round def))))))
                                         ))
   (om-add-subviews self (paramText self) 
                    (paramVal self) 
                    (paramGraph self)
                    (paramReset self))))






;;;================================================================================================================================================================
;;;                                                                            faust pool
;;;================================================================================================================================================================

(in-package :om)

(defvar *faust-effects-pool* (make-hash-table))
(defconstant *max-effects-number* (* 4 channels))

(defun init-faust-effects-pool ()
    (loop for i from 0 to *max-effects-number* do
          (setf (gethash i *faust-effects-pool*) (list nil 0 "faust-effect")))) ;(ptr track name)

(init-faust-effects-pool)


;;;//////////////////POOL TOOLS/////////////////////////////
(defun add-faust-effect-to-pool (ptr name)
  (let ((i 0))
    (while (nth 0 (gethash i *faust-effects-pool*))
          (incf i))
    (setf (gethash i *faust-effects-pool*) (list ptr 0 name))
  ))


(defun remove-faust-effect-from-list (ptr list)
  (las::RemoveAudioEffect list ptr))

(defun add-faust-effect-to-list (ptr list)
  (las::AddAudioEffect list ptr))


(defun find-effect-index-in-pool (ptr)
  (let ((i 0)
        (found 0))
    (while (= found 0)
      (if (eq ptr (nth 0 (gethash i *faust-effects-pool*)))
          (setf found 1)
        (incf i))
      (if (> i *max-effects-number*) (let ()
                                       (setf found 1)
                                       (setf i nil))))
    i))


(defun get-number-faust-effects-pool ()
  (let ((i 0))
    (while (nth 0 (gethash i *faust-effects-pool*))
          (incf i))
    i))

(defun find-hole-index-in-faust-effects-pool ()
  (let ((i 0)
        (found 0)
        (ptr nil)
        (marker 0)
        (res nil))
    (while (= found 0) 
      (setf ptr (nth 0 (gethash i *faust-effects-pool*)))
      (if (= 1 marker)
          (if (eq ptr nil)
              (let () (setf res nil) (setf found 1))
            (let () (setf res (- i 1)) (setf found 1)))
        (if (eq ptr nil) (setf marker 1)))
      (incf i)
      (if (> i *max-effects-number*) (let ()
                                       (setf found 1)
                                       (setf res nil))))
    res))

(defun pack-faust-effects-pool (n)
  (let ()
    (if (find-hole-index-in-faust-effects-pool)
        (let ((index (find-hole-index-in-faust-effects-pool)))
          (loop for i from index to (- n 1) do
            (setf (gethash i *faust-effects-pool*) (gethash (+ i 1) *faust-effects-pool*)))))))
;;;//////////////////////////////////////////////////////////

(defclass* faust-effect-controller () 
  ((label :initform nil :initarg :label :accessor label :type t)
   (faust-ptr :initform nil :initarg :faust-ptr :accessor faust-ptr)
   (tracknum :initform 0 :initarg :tracknum :accessor tracknum)))


(defclass* faust-pool (simple-score-element)
   ((effect-list :initform nil :accessor effect-list))
   (:documentation "Faust Pool"))




(defmethod initialize-instance :after ((self faust-pool) &rest l)
  (declare (ignore l))
  (let ((nbeffects 0)
        (i 0))
    (while (nth 0 (gethash i *faust-effects-pool*))
          (incf nbeffects)
          (incf i))
        (setf (effect-list self)
              (loop for effect from 0 to (- nbeffects 1) collect (make-instance 'faust-effect-controller
                                                                          :label (nth 2 (gethash effect *faust-effects-pool*))
                                                                          :faust-ptr (nth 0 (gethash effect *faust-effects-pool*))
                                                                          :tracknum (nth 1 (gethash effect *faust-effects-pool*))
                                                                          )))))


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
  ((effect-panels :initform nil :initarg :effect-panels :accessor effect-panels)))

(defmethod make-editor-window ((class (eql 'faust-pool-editor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))

(defmethod get-win-ed-size ((self faust-pool))
  (om-make-point 300 (max 33 (* 33 (length (effect-list self)))))
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

(defmethod metaobj-scrollbars-params ((self faust-pool-editor))  '(:h t))

(defmethod initialize-instance :after ((self faust-pool-editor) &rest l)
   (declare (ignore l))
   (let ((x (om-point-x (get-win-ed-size (object self))))
         (y (om-point-y (get-win-ed-size (object self))))
         (color (om-make-color 0.9 0.9 0.9))
         (name "test"))
     (setf (panel self) (om-make-view (get-panel-class self) 
                                                     :owner self
                                                     :position (om-make-point 0 0) 
                                                     :scrollbars (first (metaobj-scrollbars-params self))
                                                     :retain-scrollbars (second (metaobj-scrollbars-params self))
                                                     :field-size  (om-make-point x y)
                                                     :size (om-make-point (w self) (h self))))
     (setf (effect-panels self)
           (loop for eff in (effect-list (object self))
            for i = 0 then (+ i 1) collect
            (om-make-view 'faust-effect-panel-view
                          :effect eff
                          :owner (panel self)
                          :bg-color *om-light-gray-color*
                          :position (om-make-point 0 (* 30 i))
                          :size (om-make-point 1000 30))))))

(defmethod initialize-instance :after ((self faust-effect-panel) &rest l)
   (declare (ignore l))
   (do-initialize-effect self))

(defmethod do-initialize-effect ((self faust-effect-panel))  
   (let* ((color (om-make-color 0.9 0.9 0.9))
          (effect (effect self))
          (name (label effect))
          (ptr (faust-ptr effect)))
   (om-set-bg-color self color)
     (setf nameview (om-make-dialog-item 'om-static-text
                                                  (om-make-point 5 3) 
                                                  (om-make-point 200 30)
                                                  (format nil "~D" name)
                                                  :font *om-default-font1*
                                                  :bg-color color))
     (setf tracktextview (om-make-dialog-item 'om-static-text
                                                  (om-make-point (+ 5 200) 3) 
                                                  (om-make-point 50 30)
                                                  (format nil "Track :")
                                                  :font *om-default-font1*
                                                  :bg-color color))
     (setf trackview (om-make-dialog-item 'numBox
                                          (om-make-point (+ 5 200 50) 3)
                                          (om-make-point 28 19) (format () " ~D" (tracknum effect))
                                          :min-val 0
                                          :max-val 32
                                          :bg-color *om-white-color*
                                          :font *om-default-font1*
                                          :value (tracknum effect)
                                          :afterfun #'(lambda (item) (if (find-effect-index-in-pool ptr)
                                                                         (let ((trackdest (- (value item) 1))
                                                                               (trackorigin (- (tracknum effect) 1)))
                                                                           (if (< trackdest 0)
                                                                               (if (>= trackorigin 0)
                                                                                   (let ()
                                                                                     (remove-faust-effect-from-list ptr (gethash trackorigin oa::*effects-lists*))
                                                                                     (report-modifications self)))
                                                                             (let ()
                                                                               (if (>= trackorigin 0)
                                                                                   (let ()
                                                                                     (remove-faust-effect-from-list ptr (gethash trackorigin oa::*effects-lists*))
                                                                                     (setf (tracknum effect) (value item))
                                                                                     (add-faust-effect-to-list ptr (gethash trackdest oa::*effects-lists*))
                                                                                     (report-modifications self))
                                                                                 (let ()
                                                                                   (add-faust-effect-to-list ptr (gethash trackdest oa::*effects-lists*))
                                                                                   (report-modifications self)))))

                                                                           (setf (tracknum effect) (value item))
                                                                           (setf (nth 1 (gethash (find-effect-index-in-pool ptr) *faust-effects-pool*)) (value item)))))))

     (om-add-subviews self
                      nameview
                      tracktextview
                      trackview)
))

