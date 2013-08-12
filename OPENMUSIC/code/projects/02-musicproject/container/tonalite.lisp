(in-package :om)


;;; soit comme ca soit en faisant deux classes diffŽrentes.. --> ?
(defclass tonal-object ()
  ((tonalite :accessor tonalite :initarg :tonalite :initform nil)
   (tonal-values :accessor tonal-values :initarg :tonal-values :initform nil)))

(defmethod tonal-object-p ((self t)) nil)
(defmethod tonal-object-p ((self tonal-object)) t)


;;; si on demande la tonalite d'un objet qui n'est pas un tonal-object :
(defmethod tonalite ((self t)) nil)

(defmethod set-tonalite ((self t) (tonalite t)) nil)


;;;====================================================

(defvar *om-tonalite* nil)

;; TO BE REDEFINED IN TONALITE
;; (si *om-tonalite* = t)

(defmethod score-set-tonalite ((self t))
  (print "sorry, harmonic project is not loaded..."))

(defmethod score-remove-tonalite ((self t))
  (om-print "sorry, harmonic project is not loaded..."))

(defmethod set-editor-tonality ((self t)) nil)

(defmethod draw-general-tonality ((self t)) nil)
(defmethod draw-tonalite ((self t) x y zoom size begin panel) nil)
(defmethod draw-modulation ((self t) (newchord t) grap-chord x y zoom size panel) nil)
(defmethod draw-degre ((self t) realpos y size) nil)
(defmethod draw-chiffrage ((self t) x y zoom size) nil)
(defmethod draw-armure ((self t) armure x top xsize fontsize deltay) nil)

