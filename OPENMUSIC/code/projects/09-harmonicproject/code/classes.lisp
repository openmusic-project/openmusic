;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  IRCAM - Music Represetation Team
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
;
;  Harmonic Project
;  Tonal properties for OM musical objects
;  C. Truchet, J. Bresson
;=========================================================================

(in-package :om)


;;; pour activer les appels dans le kernel
(setf *om-tonalite* t)


(defvar *majeur* '(0 200 400 500 700 900 1100))
(defvar *mineur* '(0 200 300 500 700 800 900 1000 1100)) 
(defvar *mineur-harmonique* '(0 200 300 500 700 800 1100))
(defvar *mineur-mel-ascendant* '(0 200 300 500 700 900 1100))
(defvar *mineur-mel-descendant* '(0 200 300 500 700 800 1000)) ;par defaut pour le mineur

(defclass tonalite ()
  ((tonmidi :initform 6000 :accessor tonmidi)
   (mode :initform *majeur* :initarg :mode :accessor mode)
   (tonnote :initform 'do :initarg :tonnote :accessor tonnote)
   (tonalt :initform 'becarre :initarg :tonalt :accessor tonalt)
   ))

(defmethod meme-tonalite? ((ton1 tonalite) (ton2 tonalite))
  (and ton1 ton2
   (equal (tonnote ton1) (tonnote ton2))
   (equal (tonalt ton1) (tonalt ton2))
   (equal (mode ton1) (mode ton2))))

(defmethod meme-tonalite? ((ton1 t) (ton2 t))
  (and ton1 ton2 (equal ton1 ton2)))

;(defclass simple-tonalite (tonalite)
;  ((ornement :initform nil :initarg :ornement :accessor ornement)
;   (accident :initform nil :initarg :accident :accessor accident)
;   (degre :initform nil :initarg :degre :accessor degre)
;   (cadence :initform nil :accessor cadence)))
(defclass simple-tonalite (tonalite) ())


(defclass tonalite-modulee (tonalite)
  ((tonsup :initform nil   :accessor tonsup)
   (modesup :initform nil :accessor modesup)
   (notesup :initform nil :accessor notesup)
   (altsup :initform nil :accessor altsup)))



;;; remplace simple-tonalite : c'est une classe a part qui contient les caracteristiques tonales d'un objet musical, 
;;; meme si celui-ci n'as pas de tonalite propre
(defclass tonal-prop ()
  ((ornement :initform nil :initarg :ornement :accessor ornement)
   (accident :initform nil :initarg :accident :accessor accident)
   (degre :initform nil :initarg :degre :accessor degre)
   (cadence :initform nil :accessor cadence)))


;;; herite de simple-tonalite : NOTE - REST
;;; herite de tonalite-modulee : CHORD-SEQ - METRIC-SEQUENCE (group measure voice) - MULTI-SEQ CHORD POLY (superposition)

(defmethod get-tonalite-class ((self t)) 'tonalite)
(defmethod get-tonalite-class ((self note)) 'simple-tonalite)
(defmethod get-tonalite-class ((self rest)) 'simple-tonalite)

(defmethod get-tonalite-class ((self chord)) 'tonalite-modulee)
(defmethod get-tonalite-class ((self chord-seq)) 'tonalite-modulee)
(defmethod get-tonalite-class ((self metric-sequence)) 'tonalite-modulee)
(defmethod get-tonalite-class ((self multi-seq)) 'tonalite-modulee)
(defmethod get-tonalite-class ((self poly)) 'tonalite-modulee)


(defun default-tonality (&optional obj) (make-instance (get-tonalite-class obj)))


;;; COPY / SAVE

(defmethod omNG-save ((self tonalite) &optional (values? nil))
  (let ((theclass (class-name (class-of self))))
  `(make-instance ',theclass
     :tonnote ',(tonnote self)
     :tonalt ',(tonalt self)
     :mode ',(mode self))))


(defmethod copy-container ((self tonalite) &optional pere)
  (make-instance (type-of self)
     :tonnote (tonnote self)
     :tonalt (tonalt self)
     :mode (mode self)))

(defmethod copy-container ((self tonal-prop) &optional pere)
  (make-instance (type-of self)))



