;;;================================================================================================================================================================
;;;================================================================================================================================================================
;;;==========================================================================  faust JSON parsing   ===============================================================
;;;================================================================================================================================================================
;;;================================================================================================================================================================
;;;THIS API NEEDS THE YASON LIBRARY

(in-package :om)

;;;================================================================================================================================================================
;;;=====================================================================  OBJECTS AND ACCESSORS   =================================================================
;;;================================================================================================================================================================
(defclass faust-group ()
  ((group-type :initform nil :initarg :group-type :accessor group-type)
   (label :initform nil :initarg :label :accessor label)
   (items :initform nil :initarg :items :accessor items)
   (size :initform nil :initarg :size :accessor size)))

(defmethod las-faust-get-group-type ((self faust-group))
  (group-type self))
(defmethod las-faust-get-group-label ((self faust-group))
  (label self))
(defmethod las-faust-get-group-items ((self faust-group))
  (items self))
(defmethod las-faust-get-group-size ((self faust-group))
  (size self))


(defclass faust-param ()
   ((param-type :initform nil :initarg :param-type :accessor param-type)
    (label :initform nil :initarg :label :accessor label)
    (osc-address :initform nil :initarg :osc-address :accessor osc-address)
    (metadata :initform nil :initarg :metadata :accessor metadata)
    ;(style :initform nil :initarg :style :accessor style)
    ;(tooltip :initform nil :initarg :tooltip :accessor tooltip)
    ;(unit :initform nil :initarg :unit :accessor unit)
    (init-val :initform nil :initarg :init-val :accessor init-val)
    (min-val :initform nil :initarg :min-val :accessor min-val)
    (max-val :initform nil :initarg :max-val :accessor max-val)
    (step-val :initform nil :initarg :step-val :accessor step-val)
    (size :initform nil :initarg :size :accessor size)))


(defmethod las-faust-get-param-type ((self faust-param))
  (param-type self))
(defmethod las-faust-get-param-label ((self faust-param))
  (label self))
(defmethod las-faust-get-param-address ((self faust-param))
  (osc-address self))
(defmethod las-faust-get-param-metadata ((self faust-param))
  (metadata self))
(defmethod las-faust-get-param-init-val ((self faust-param))
  (init-val self))
(defmethod las-faust-get-param-min-val ((self faust-param))
  (min-val self))
(defmethod las-faust-get-param-max-val ((self faust-param))
  (max-val self))
(defmethod las-faust-get-param-step-val ((self faust-param))
  (step-val self))
(defmethod las-faust-get-param-size ((self faust-param))
  (size self))

;;;================================================================================================================================================================
;;;=============================================================================  PARSER   ========================================================================
;;;================================================================================================================================================================
(defun las-faust-parse (string)
  (let ((children-list (list))
        father 
        obj
        type)
    (setf father (construct-faust-ui (get-faust-json-ui string)))
    (group-items-to-objects father)
    (set-group-size father)
    father
    ))

(defun las-faust-translate-tree (tree)
  (let (children final-list)
    (setf children (las-faust-get-group-items tree))
    (if (> (length children) 0)
        (loop for i from 0 to (- (length children) 1) do
              (cond ((typep (nth i children) 'faust-group)
                     (setf final-list (append final-list (las-faust-translate-tree (nth i children)))))
                    ((typep (nth i children) 'faust-param)
                     (setf final-list (append final-list (list (nth i children)))))))
        (setf final-list (list)))
    final-list))

(defun las-faust-get-groups-only (tree)
  (let (children final-list)
    (setf children (las-faust-get-group-items tree))
    (if (> (length children) 0)
        (loop for i from 0 to (- (length children) 1) do
              (cond ((typep (nth i children) 'faust-group)
                     (setf final-list (append final-list (list (nth i children)))))
                    ((typep (nth i children) 'faust-param)
                     nil)))
        (setf final-list (list)))
    final-list))

(defun las-faust-get-params-only (tree)
  (let (children final-list)
    (setf children (las-faust-get-group-items tree))
    (if (> (length children) 0)
        (loop for i from 0 to (- (length children) 1) do
              (cond ((typep (nth i children) 'faust-group)
                     nil)
                    ((typep (nth i children) 'faust-param)
                     (setf final-list (append final-list (list (nth i children)))))))
        (setf final-list (list)))
    final-list))
;;;================================================================================================================================================================
;;;=========================================================================  PARSING TOOLS   =====================================================================
;;;================================================================================================================================================================
(defun get-faust-json-ui (string)
  (let ((effect-json (yason:parse string :object-as :plist))
        top
        meta
        ui)
    ;;;ON VIDE LES SLOTS AVANT META
    (setf top (pop effect-json))
    (while (not (or (string= top "meta") (string= top "no-meta")))
      (setf top (pop effect-json))
      (if (string= top "ui")
          (progn
            (push "ui" effect-json)
            (setf top "no-meta"))))
    ;;;ON ENLEVE LES META (ON LES SAVE AU CAS OU)
    (if (not (string= top "no-meta"))
        (progn
          (setf meta (nth 0 effect-json))
          (setf effect-json (cdr effect-json))))
    (setf ui (car (nth 1 effect-json)))
    ui))


(defun group-items-to-objects (group &optional (ground 0))
  (let (type
        (top-list (list))
        (children-list (list))
        (max 100))
  (if (items group)
        (loop for i from 0 to (- (length (items group)) 1) do
              (setf type (check-type-key (nth i (items group))))
              (setf obj
                    (cond ((or (string= type "hgroup") (string= type "vgroup") (string= type "tgroup"))
                           (construct-faust-group (nth i (items group))))
                          (t
                           (construct-faust-param (nth i (items group))))))
              (if (<= ground max) (setf top-list (append top-list (list obj))))
              (if (typep obj 'faust-group) 
                  (group-items-to-objects obj (+ ground 1))
                (set-param-size obj))
              (setf children-list (append children-list (list obj)))))
  (setf (items group) children-list)
  (if (<= ground max) (setf (items group) top-list))))


(defun construct-faust-ui (ui)
  (cond ((or (string= (check-type-key ui) "hgroup") (string= (check-type-key ui) "vgroup"))
         (construct-faust-group ui))
        (t
         (construct-faust-param ui))))

(defun construct-faust-param (list)
  (let (type label address metadata init-val min-val max-val step-val)
    (progn
      (pop list)
      (setf type (pop list))
      (pop list)
      (setf label (list))
      (while (not (string= (car list) "address"))
        (setf label (append label (pop list))))
      (pop list)
      (setf address (pop list))
      (if (string= (car list) "meta")
          (progn
            (pop list)
            (setf metadata (pop list))))
      (pop list)
      (setf init-val (pop list))
      (pop list)
      (setf min-val (pop list))
      (pop list)
      (setf max-val (pop list))
      (pop list)
      (setf step-val (pop list))     
      (make-instance 'faust-param
                     :param-type type
                     :label label
                     :osc-address address
                     :metadata metadata
                     :init-val (if init-val init-val "0")
                     :min-val (if min-val min-val "0")
                     :max-val (if max-val max-val "1000000")
                     :step-val step-val))))

(defun construct-faust-group (list)
  (let (type label items)
    (progn
      (pop list)
      (setf type (pop list))
      (pop list)
      (setf label (list))
      (while (not (string= (car list) "items"))
        (setf label (append label (list (pop list)))))
      (pop list)
      (setf items (pop list))
      (make-instance 'faust-group
                     :group-type type
                     :label label
                     :items items))))

(defun check-type-key (list)
  (let ((curkey (pop list)) res)
    (if (string= curkey "type")
        (setf res (pop list)))
    res))

;;;================================================================================================================================================================
;;;==========================================================================  GRAPH TOOLS   ======================================================================
;;;================================================================================================================================================================
(defconstant buttonSize (list 124 40))
(defconstant checkboxSize (list 80 81))
(defconstant hsliderSize (list 124 81))
(defconstant vsliderSize (list 80 154))
(defconstant numentrySize (list 80 65))

(defmethod set-param-size ((self faust-param))
  (cond ((string= (param-type self) "checkbox") (setf (size self) checkboxSize))
        ((string= (param-type self) "button") (setf (size self) buttonSize))
        ((string= (param-type self) "hslider") (setf (size self) hsliderSize))
        ((string= (param-type self) "vslider") (setf (size self) vsliderSize))
        ((string= (param-type self) "numentry") (setf (size self) checkboxSize))
        (t (setf (size self) (list 25 25)))))

(defmethod set-group-size ((self faust-group))
  (let ((size (list 0 0))
        (type (group-type self))
        (x 0)
        (y 0)
        (max 0)
        curitem
        cursize)
    (if (string= type "hgroup")
        (progn
          (loop for i from 0 to (- (length (items self)) 1) do
                (setf curitem (nth i (items self)))
                (if (typep curitem 'faust-group) (set-group-size curitem))
                (setf cursize (size curitem))
                (setf x (+ x (car cursize)))
                (if (> (cadr cursize) max) (setf max (cadr cursize))))
          (setf size (list x max)))
      (progn
        (loop for i from 0 to (- (length (items self)) 1) do
              (setf curitem (nth i (items self)))
              (if (typep curitem 'faust-group) (set-group-size curitem))
              (setf cursize (size curitem))
              (setf y (+ y (cadr cursize)))
              (if (> (car cursize) max) (setf max (car cursize))))
        (setf size (list max y))))
    (setf (size self) size)))
