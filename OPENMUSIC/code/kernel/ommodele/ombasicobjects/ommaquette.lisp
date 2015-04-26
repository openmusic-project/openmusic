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
;This file implements the meta-object OMMaquette.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


(defclass Maquette-params ()
   ((maq-color :initform *maq-color* :initarg :maq-color :accessor maq-color)
    (range :initform (list 0 10000 0 100) :initarg :range :accessor range)
    (metricparam :initform '((4 60) ((4 4)) 16 t) :initarg :metricparam :accessor metricparam)
    (yparam :initform '(1) :initarg :yparam :accessor yparam)
    (xparam :initform '(1) :initarg :xparam :accessor xparam)
    (snap :initform '(abs t) :initarg :snap :accessor snap)
    (show-ruler-metric :initform "off" :accessor show-ruler-metric :initarg :show-ruler-metric)
    (show-conect :initform t :initarg :show-conect :accessor show-conect))
   (:documentation "Differents parameters for maquette edition and visualitation are kept in instance of this class.#enddoc#
#seealso# (OMmaquette) #seealso#
#maq-color# The color of the maquette's background. #maq-color#
#range# The visible coordonates in x and y axes. #range#
#metricparam# A list (<tempo> <signature-list> <max subdivision> <loop flag>), if the maquette use a metric ruler. #metricparam#
#show-conect# show/hide connections. #show-conect#"))

(defun new-maquette-params ()
   (make-instance 'Maquette-params))

(defmethod get-metric-values ((self OMmaquette))
   "Get a list (tempo metric minval loopflag)."
   (metricparam (params self)))

(defmethod set-metric-values ((self OMmaquette) (list list))
   "Set a list (tempo metric minval loopflag)."
   (setf (metricparam (params self)) list))

(defmethod maq-changeparams ((self OMMaquette) types vals)
  (loop for item in (boxes self) do
        (maq-changeparams item types vals)))


(defmethod maq-changeparams ((self t) type val) t)

;===================================================================================

#|
(defclass OMMaquette (OMPatch) 
   ((pictu :initform (make-new-om-pict) :accessor pictu)
    (params :initform (new-maquette-params) :accessor params))
   (:documentation "The class of the om maquette metaobjects.#enddoc#
#seealso# (OMmaquette TemporalBox OMPatch OMmaqabs) #seealso#
#pictu# If there exists this slot store a background picture for the maquette. #pictu#
#params# A list of parameters for maquette edition and visualitation. #params#")
   (:metaclass omstandardclass))
|#

;--------------------------------------------------
;Method redefinition of OMPatch
;--------------------------------------------------

;;; redefinie dans maq-in-out
(defmethod compile-patch ((self OMMaquette))
   "Maquette are not compiled." t)



(defmethod get-elements ((self OMMaquette)) 
   "Elements of maquettes are temporal boxes." (boxes self))

(defmethod obj-file-type ((self OMMaquette)) :MAQT)
(defmethod obj-file-extension ((self OMMaquette)) "omm")

(defmethod get-class-icon ((self OMMaquette)) 'maquette-icon-frame)

(defmethod get-object-insp-name ((self OMMaquette)) "Maquette")

(defmethod get-editor-class ((self OMMaquette)) 'MaquetteEditor)

(defmethod load-abstraction-attributes ((self ommaquette) currentpersistent)
  (call-next-method) ;;; ompatch
  (setf (eval-func self) (eval-func currentpersistent))
  (setf (params self) (params currentpersistent)))

(defmethod load-maquette ((self OMMaquette))
  (unless (loaded? self)
    (setf *loaading-stack* nil)
    (om-print (string+ "Loading maquette: " (namestring (mypathname self))))
    (push self *loaading-stack*)
    (setf *om-current-persistent* nil)
    (om-with-cursor *om-wait-cursor* 
      (eval-non-text-file (mypathname self))
      (when *om-current-persistent*
        (let ((up? (version-warning (omversion *om-current-persistent*))))
          (if up?
            (progn
              (when *om-current-persistent*
                (setf *om-search-all* nil 
                      *user-search-all* nil
                      *file-search-all* nil)
                (load-abstraction-attributes self *om-current-persistent*)
                (when (< (omversion *om-current-persistent*) *om-version*)
                  (corrige-version self (omversion *om-current-persistent*))))
              (update-patches-pile)
              (setf *om-current-persistent* nil))
            (om-abort)))))))
  

(defmethod OpenEditorframe ((self OMMaquette))
  "Open the maquette editor, this method open too all persistantes objects referenced into the maquette."
  (declare (special *om-current-persistent*)) 
  (load-maquette self)

  (or (editorframe self)
      (panel (open-new-RelationFrame self (if (saved? self) (name self) 
                                              (string+ "^" (name self))) 
                                     (get-elements self)))))



(defmethod omNG-save ((self OMMaquette) &optional (values? nil))
  "Save the maquette 'self' in the file 'mypathname'if it is not null."
  (if (not (mypathname self))
    (dialog-message "Error: the maquette has no associated file, you must restart OM.")
    (let ((tempfile (om-put-path-extension (mypathname self) (string+ (obj-file-extension self) ".tmp"))))
      (when (probe-file (mypathname self))
        (rename-file (mypathname self) tempfile))
      (delete-file-protection (mypathname self))
      (unless (string-equal (pathname-type (mypathname self)) (obj-file-extension self))
        (setf (mypathname self)  (om-put-path-extension (mypathname self) (obj-file-extension self))))
      (setf *libs-to-load* nil)
      (setf *resources-to-load* nil)
      (setf *saving-patch* nil)
      (delete-file-protection (mypathname self))
      (WITH-OPEN-FILE (out (mypathname self) :direction :output  
                           :if-does-not-exist :create :if-exists :supersede ) 
        (handler-bind 
            ((error #'(lambda (err)
                        (capi::display-message "An error of type ~a occurred: ~a~%~%File ~s could not be saved." 
                                               (type-of err) (format nil "~A" err) (mypathname self))
                        (close out)
                        (rename-file tempfile (mypathname self))
                        (setf *saving-patch* nil)
                        (setf *libs-to-load* nil)
                        (setf *resources-to-load* nil)
                        (abort err))))
        
        (write-header self  out)
        (let ((*package* (find-package :om))
              (maq-code (om-save self values?)))
          (write-resources self *resources-to-load* out)
          (prin1 '(in-package :om) out)
          (prin1 `(load-lib-for ',(remove-duplicates *libs-to-load* :test 'string-equal)) out)
          (prin1 maq-code out))
        (setf *saving-patch* nil)
        (setf *libs-to-load* nil)
        (setf *resources-to-load* nil)
        (setf (saved? self) t)
        ))
      (when (and (mypathname self) (probe-file (mypathname self)))
        (when (editorframe self) (om-set-window-title (window (editorframe self)) (name self)))
        (om-print (string+ (namestring (mypathname self)) " saved"))
        (om-delete-file tempfile)
        ;(when (create-info self) (setf (cadr (create-info self)) (om-get-date)))
        )
      t)))



;--------------------------------------------------
;Other methods
;--------------------------------------------------

;--------------Predicats
(defmethod Maquette-p ((self t)) nil)
(defmethod Maquette-p ((self OMMaquette)) t)

(defmethod allow-strech-p ((self OMMaquette) (factor number)) factor)

;---------------Inits
(defun omNG-make-new-maquette (name &optional (posi (om-make-point 0 0)))
   (let ((newmaq (make-instance *def-metaclass-maq* 
                   :name name 
                   :icon 182)))
     (setf (code newmaq) nil)
     (set-icon-pos newmaq posi)
     newmaq))




;If it exists, kill the background pict of 'self'.
(defmethod kill-patch-picts ((self OMMaquette))
   (when (thepict (pictu self))
       (setf (thepict (pictu self)) nil)
       (setf (name (pictu self)) nil)))


;--------Tools

(defmethod maq-show-ruler ((self OMMaquette) &optional val)
   (if val
     (setf (show-ruler-metric (params self)) val)
     (show-ruler-metric (params self))))


(defmethod get-obj-dur ((self OMMaquette))
   "Offset+ Duration in ms of the last box in 'self'"
   (if (and (eval-func self) (value self) (not (maq-obj-p (value self)))
            (allowed-in-maq-p (value self)))
       (get-obj-dur (value self))
        (if (null (boxes self))
            5000
          (loop for item in (boxes self)
                when (boxtempobj-p item)
                maximize (+ (slot-value item 'offset) (round (* (strech-fact item) (extend item)))))))
   )

(defmethod get-internal-dur ((self OMMaquette))
  "Offset+ Duration in ms of the last box in 'self'"
  (if (null (boxes self))
      5000
    (loop for item in (boxes self)
          when (boxtempobj-p item)
          maximize (+ (slot-value item 'offset) (round (* (strech-fact item) (extend item)))))))
  

(defmethod cons-maquetteobj ((self OMMaquette) objs)
   "Cons a maquette-obj instance which contains the temporal elements in the maquette 'self'."
   (let* ((maxdur 0) tempobjs rep params)
     (loop for tempobj in objs do
           (let ((val-of-tempob (get-mus-ob tempobj)))
             (when (allowed-in-maq-p val-of-tempob)
               (setf (offset val-of-tempob) (slot-value tempobj 'offset))
               (unless (= 1 (strech-fact tempobj))
                 (stretch-in-maq val-of-tempob (strech-fact tempobj)))
               (setf maxdur (max maxdur (+ (offset val-of-tempob) (extent->ms val-of-tempob))))
               (push (edition-params tempobj) params)
               (push val-of-tempob tempobjs))
             (when (consp val-of-tempob)
               (loop for val in val-of-tempob do
                     (when (allowed-in-maq-p val)
                       (setf (offset val) (slot-value tempobj 'offset))
                       (unless (= 1 (strech-fact tempobj))
                         (stretch-in-maq val (strech-fact tempobj)))
                       (setf maxdur (max maxdur (+ (offset val) (extent->ms val))))
                       (push (edition-params tempobj) params)
                       (push val tempobjs))
                     ))
             ))
     (setf rep (make-instance 'maquette-obj 
                 :extent maxdur
                 :Qvalue 1000 
                 :inside (reverse tempobjs)))
     (setf (param-list rep) (reverse params))
     rep))

(defmethod cons-copy-maquetteobj ((self OMMaquette) objs)
   "Cons a maquette-obj instance which contains copies of the temporal elements in the maquette 'self'."
   (let* ((maxdur 0) tempobjs rep params)
     (loop for tempobj in objs do
           (let ((val-of-tempob (maq-copy-container (get-mus-ob tempobj))))
             (when (allowed-in-maq-p val-of-tempob)
               (setf (offset val-of-tempob) (slot-value tempobj 'offset))
               (unless (= 1 (strech-fact tempobj))
                 (stretch-in-maq val-of-tempob (strech-fact tempobj)))
               (setf maxdur (max maxdur (+ (offset val-of-tempob) (extent->ms val-of-tempob))))
               (push (edition-params tempobj) params)
               (push val-of-tempob tempobjs))
             (when (consp val-of-tempob)
               (loop for val in val-of-tempob do
                     (when (allowed-in-maq-p val)
                       (setf (offset val) (slot-value tempobj 'offset))
                       (unless (= 1 (strech-fact tempobj))
                         (stretch-in-maq val (strech-fact tempobj)))
                       (setf maxdur (max maxdur (+ (offset val) (extent->ms val))))
                       (push (edition-params tempobj) params)
                       (push val tempobjs))
                     ))
             ))
     (setf rep (make-instance 'maquette-obj 
                 :extent maxdur
                 :Qvalue 1000 
                 :inside (reverse tempobjs)))
     (setf (param-list rep) (reverse params))
     rep))

;;; !!!
;;; proteger pour les differents arguments
(defmethod call-synth-patch ((self ommaquette))
  (let ((arg (make-instance 'maquette-data)))
    (setf (maquette arg) self
          (duration arg) (if (boxes self)
                           (loop for item in (boxes self)
                                 when (boxtempobj-p item)
                                 maximize (+ (slot-value item 'offset) (round (* (strech-fact item) (extend item)))))
                           0)
          (boxes arg) (get-tempobjs (boxes self))
          (boxvalues arg) (get-tempobjs-value (boxes self))) 
      (funcall (intern (string (code (reference (eval-func self)))) :om) arg)))


(defmethod cons-maquette-object ((self OMMaquette) objs)
  (let ((func (eval-func self))
         (rep nil))
    (when func (setf func (reference func)))
     (if func
       (progn
         (show-message-win (string+ "Computing " (name self) "..."))
         (cond 
        ((patch-p func)
         (unless (loaded? func)
          (load-patch func))
        (unless (compiled? func)
          (compile-patch func))
         (setf rep 
               ;(funcall (intern (string (code func)) :om) objs)
               (call-synth-patch self)
               ))
        ((functionp func)
         (setf rep (funcall func objs)))
        ((and (symbolp func) (fboundp func))
         (setf rep (funcall func objs)))
        (t (hide-message-win)
            (om-message-dialog "Bad EVAL-FUNC, I cannot compute the maquette...")))
         (hide-message-win)
         )
       (setf rep (cons-maquetteobj self objs))
       )
     (setf (value self) rep)
     rep))

(defmethod cons-copy-maquette-object ((self OMMaquette) objs)
  (let ((func (eval-func self))
         (rep nil))
    (when func (setf func (reference func)))
    (if func
      (progn
        (show-message-win (string+ "Evaluation of " (name self) "..."))
        (cond 
         ((patch-p func)
          (unless (loaded? func)
            (load-patch func))
          (unless (compiled? func)
            (compile-patch func))
          (setf rep 
               ;(funcall (intern (string (code func)) :om) objs)
               (call-synth-patch self)
               )
          )
         ((functionp func)
          (setf rep (funcall func objs)))
         ((and (symbolp func) (fboundp func))
          (setf rep (funcall func objs)))
         (t (hide-message-win)
            (om-message-dialog "Bad EVAL-FUNC, I cannot compute the maquette...")))
        (hide-message-win)
        )
       (setf rep (cons-copy-maquetteobj self objs))
       )
    (setf (value self) rep)
    rep))

;;; mAQ-COPY-CONTAINER
;;; comme copy-container sauf que les sounds sont copies sans recharger un nouveau
(defmethod maq-copy-container ((self t) &optional (pere ())) (copy-container self pere))

(defmethod maq-copy-container ((self list) &optional (pere ()))
  (mapcar #'(lambda (X) (maq-copy-container x pere)) self))

(defmethod maq-copy-container ((self cons) &optional (pere ()))
  (cons (maq-copy-container (first self) pere) (maq-copy-container (rest self) pere)))



(defmethod get-temp-obj-in-range ((self OMMaquette) xmin xmax)
   "Return the list of 'visible' boxes in the maquette 'self'."
   (loop for item in (boxes self)
         if (or (and (<= (slot-value item 'offset) xmax) (>= (slot-value item 'offset) xmin))
                (> (+ (slot-value item 'offset) (extend item)) xmin))
         collect  item))

(defmethod corrige-version ((self OMMaquette) (number (eql 2.01)))
   "This method if keeped to load old maquettes."
   (om-beep-msg (format nil "This maquette has been converted from 2.0.1 version to ~D version" *om-version*))
   (let ((ranges (give-correction-range self)))
     (loop for item in (boxes self) do
           (setf (slot-value item 'posy) (- (fourth ranges) (posy item))))
     (setf (range (params self)) ranges)
     (loop for i from 0 to (- (length (connec self)) 1) do
           (setf (nth 4 (nth i (connec self))) nil))
     t))

(defmethod give-correction-range ((self OMMaquette))
   (loop for item in (boxes self) 
         maximize (+ (extend  item) (slot-value item 'offset)) into x
         maximize (+ (posy item) (sizey item)) into y
         minimize (posy item)  into y0
         finally (return (list 0 x y0 y))))

 
(defmethod corrige-metric-info ((self OMMaquette) timelist)
  (let ((params (metricparam (params self))))
    (loop for item in timelist
          collect (if (and (listp item) item (integerp (first item)) (integerp (second item)) (integerp (third item)))
                    (listmetric2ms item (first params) (second params) (third params) (fourth params))
                    item))))

(defmethod put-boxes-inmaquette ((self OMMaquette) time objs &optional connections)
   "Called by the evaluation of a omboxmaquette."
   (when objs
     (let* ((time-list (list! time))
            (time-list (corrige-metric-info self time-list))
            (objlist (list! objs))
            (deftime (default-from-list time-list))
            (facty (round 100 (length objlist)))
            (i 0) (old-time 0))
       (setf (boxes self) nil)
       (loop for item in objlist do
             (let* ((posi (if time-list (pop time-list) (+ old-time deftime))) newtemp)
               (if (boxtempobj-p item)
                   (progn (setf newtemp (clone item)) (incf i))
                 (progn
                   (setf newtemp (omNG-make-tempobj (clone item) (om-make-point 0 0)  (string+ "tempobj" (format () "~D" (incf i)))))
                   (setf (slot-value newtemp 'sizey) facty)
                   (setf (slot-value newtemp 'posy)  (* i facty))))
               (setf (slot-value newtemp 'offset) posi)
               (when newtemp
                 (omNG-add-element self newtemp))
               (setf old-time posi)))
       (setf (connec self) connections)
       (remk-connections (reverse (boxes self)) connections)
       )))

(defmethod maq2abs ((self OMMaquette))
   "Cons a new instance of 'OMMaqAbs from the maquette 'self'."
   (let ((newabs (make-instance 'OMMaqAbs :name (name self)  :icon 265)))
     (loop for item in (reverse (mapcar #'omNG-copy (boxes self))) do
           (omng-add-element newabs (eval item)))
     (setf (eval-func newabs) (clone (eval-func self)))
     (copy-connections (boxes self) (boxes newabs))
     (set-icon-pos newabs (get-icon-pos self))
     newabs))



;======================================================
;Red Maquettes in a patch
;======================================================

(defclass OMMaqAbs (OMMaquette) 
  ((w-size :accessor w-size :initform (om-make-point 500 200))
   (w-pos :accessor w-pos :initform (om-make-point 200 200)))
   (:documentation "This is the class of the maquette references of red maquette boxes.
The difference with normal maquettes is that this maquette are not saved in a owner file.
These maquettes are saved in the patch file which contains the red boxes.
So red maquettes can not be sharing.#enddoc#
#seealso# (OMPatch OMMaqAbs) #seealso#")
   (:metaclass omstandardclass)) 

(defmethod om-maquette-abs-p ((self OMMaqAbs)) t)
(defmethod om-maquette-abs-p ((self t)) nil)

(defmethod omNG-make-new-boxcall ((maquette OMMaqAbs) posi name)
   "Cons a red box having by reference the maquette 'self'."
   (let* ((inputs (list (make-instance 'input-funbox
                          :name "time"
                          :value 1000
                          :doc-string "time rate (number) or offset list")
                        (make-instance 'input-funbox
                          :name "objs"
                          :value nil
                          :doc-string "objects to put in maquette")))
          (rep (make-instance 'OMBoxAbsmaq 
                 :name name
                 :reference maquette 
                 :icon (icon maquette)
                 :inputs inputs)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs maquette))
     rep))

(defmethod set-win-size ((self OMMaqAbs) newsize) 
  (setf (w-size self) newsize))

(defmethod set-win-position ((self OMMaqAbs) newpos) 
  (setf (w-pos self) newpos))

(defmethod OpenEditorframe ((self OMMaqAbs))
  "Open the maquette editor, this method open too all persistantes objects referenced into the maquette."
  (declare (special *om-current-persistent*)) 
  (load-maquette self)
  (or (editorframe self)
      (panel (open-new-RelationFrame self (if (saved? self) (name self) (string+ "^" (name self)))
                                     (get-elements self)
                                     nil
                                     (w-pos self) (w-size self)
                                     ))))

(defmethod om-save ((self OMMaqAbs) &optional (values? nil))
   "Generation of code to save the maquette 'self'."
   (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
           (connectiones (mk-connection-list (boxes self)))
           (doc (str-without-nl (doc self)))
           (markers (mk-markers-list (boxes self))) fond-ec)
     (when (thepict (pictu self))
       (setf fond-ec (omng-save (pictu self))))
     (when (editorframe self) 
          (set-win-size self (om-interior-size (window (editorframe self))))
          (set-win-position self (om-view-position (window (editorframe self)))))
     `(om-load-maq-abs1 ,(name self) ',boxes ',connectiones ',(range (params self)) ',markers
                                            ,(om-save-color (maq-color (params self))) ',(metricparam (params self)) 
                                            ,(show-conect (params self)) ,*om-version* ,fond-ec ,doc
                                            ,(show-ruler-metric (params self)) ',(xparam (params self)) ',(yparam (params self))
                                            ,(omng-save (eval-func self))
                                            ,(om-save-point (w-pos self)) ,(om-save-point (w-size self)))))


(defmethod omNG-save ((thepatch OMMaqAbs) &optional (values? nil))
   "Abstractions or red box's references are saved by the red box."
   (declare (ignore values?))
   (om-beep-msg "This maquette is into a patch, try saving the patch") nil)

(defmethod abs2maquette ((self OMMaqAbs) name pos)
   "Cons a new instance of 'OMMaquette from the maquette 'self'."
   (let ((newabs (omNG-make-new-maquette name pos)))
     (loop for item in (reverse (mapcar #'omNG-copy (boxes self))) do
           (omng-add-element newabs (eval item)))
     (copy-connections (boxes self) (boxes newabs))
     (setf (eval-func newabs) (clone (eval-func self)))
     (set-icon-pos newabs (get-icon-pos self))
     newabs))


;======================================================
;The evaluation of a maquette produce a superposition of
;temporal objects contained in the maquette.
;======================================================

(defclass* maquette-obj (container)
   ((param-list :initform nil  :accessor param-list))
  (:icon 178)
  )

(defmethod maq-obj-p   ((self maquette-obj)) t)
(defmethod maq-obj-p  ((self t)) nil)

(defmethod shift20 ((self maquette-obj))
   (let ((minoff 100000) (realmin 100000)) 
     (loop for item in (inside self) do
           (setf minoff (min (slot-value item 'offset) minoff))
           (setf realmin (min (offset->ms item) realmin)))
     (loop for item in (inside self) do
           (setf (offset item) (- (offset item) minoff)))
     (setf (extent self) (- (extent self) minoff))
     (list self realmin)))

(defmethod Class-has-editor-p  ((self maquette-obj)) t)
(defmethod get-editor-class ((self maquette-obj)) 'maqobjEditor)

(defmethod get-win-ed-size  ((self maquette-obj)) (om-make-point 330 250))

(defclass* maquette-scr (maquette-obj) () (:icon 178))


;;; copy for play in maquette
(defmethod maq-copy-container ((self maquette-obj) &optional (pere ()))
   "Builds a deep copy of a container"
   (let ((copy (make-instance (type-of self)))
         (slots  (class-instance-slots (class-of self))))
     (setf (slot-value copy 'parent) pere)
     (loop for slot in slots
           when (not (eq (slot-definition-name slot) 'parent))
           do (setf (slot-value  copy  (slot-definition-name slot))
                    (maq-copy-container (slot-value self  (slot-definition-name slot)) copy)))
     copy))



;;;;============


