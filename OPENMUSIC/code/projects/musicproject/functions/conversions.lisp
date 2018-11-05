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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson
;=========================================================================

(in-package :om)


;==================================
;APPROX_M
;==================================

(defmacro cassq (item list) `(cdr (assoc ,item ,list :test #'eq)))
;(defmacro cassq (item list) `(cdr (assq ,item ,list)))

(defvar *global-midi-approx* 2)
(setf *global-midi-approx* 2)

(defmethod* approx-m  ((midic t) approx &optional (ref-midic 0))
  :numouts 1 
  :initvals '(6000 2 0) 
  :indoc '("pitch list (midicents)" "tone division")
  :icon 141
  :doc "
Returns an approximation of <midic> (in midicents) to the nearest tempered division of the octave.
<approx> = 1 whole tones
<approx> = 2 semi tones
<approx> = 4 quarter tones
<approx> = 8 eight tones

Floating values are allowed for <approx>.
<ref-midic> is a midicent that is subtracted from <midic> before computation: the computation can then be carried on an interval rather than an absolute pitch."
  (if (<= approx 0)
    midic
    (round (* (floor (+ (* (- midic ref-midic) approx) 100) 200) 200) approx)))

(defmethod* approx-m  ((self list) approx &optional (ref-midic 0))
  (if (<= approx 0)
    self
    (loop for item in self
          collect (approx-m item approx ref-midic))))

;==================================
;MIDIC conversionts
;==================================

(defvar *diapason-freq* 440.0)
(defvar *diapason-midic* 6900)

;; ---- midic -> frequency ----
(defmethod* mc->f  ((midics? number))
  :numouts 1 
  :initvals '(6000) 
  :indoc '("pitch or pitch list (midicents)")
  :icon 141
  :doc "
Converts a (list of) midicent pitch(es) <midics> to frequencies (Hz).
"
  (* *diapason-freq*
     (expt 2.0 (/ (- midics? *diapason-midic*) 1200.0)) ))


(defmethod* mc->f  ((midics? list))
  (loop for item in midics?
        collect (mc->f item)))

;; ---- frequency -> midic ----


(defvar *lowest-freq* (* 256 (max (mc->f most-negative-fixnum) least-positive-long-float)))

(defun abs-f1 (freq) (max *lowest-freq* (abs freq)))


(defun f->mf (freq)
  (+ (* (log (abs-f1 (/ freq *diapason-freq*))) #.(/ 1200 (log 2.0)))
     *diapason-midic*))


(defmethod* f->mc  ((freq number) &optional (approx 100) (ref-midic 0))
  :numouts 1 
  :initvals '(440 100 0) 
  :indoc '("frequency (Hz)" "approximation")
  :icon 141
  :doc "Converts a frequency or list of frequencies to midicents.

Approximation:
- <approx> = 1 whole tones
- <approx> = 2 semi tones
- <approx> = 4 quarter tones
- <approx> = 8 eight tones
Floating values are allowed for <approx>.

<ref-midic> is a midicent that is subtracted from <midic> before computation: the computation can then be carried on an interval rather than an absolute pitch."
  (approx-m (f->mf freq) approx ref-midic))

(defmethod* f->mc  ((freq list) &optional (approx 100) (ref-midic 0))
  (loop for item in freq
        collect (f->mc item approx ref-midic)))

;; ---- midic -> symbol ----

;; ----  symbol ->  midic ----

;; ---- interval -> symbol ----

;; ----  symbol ->  interval ----

;; ---- midic -> ASCII ----

;; ----  ASCII ->  midic ----


;; ---- number -> dB ----

;; ----  dB ->  midic ----


;; =============================================================================-======



(defvar *no-sharp-read-table*)

(set-syntax-from-char #\# #\K (setf *no-sharp-read-table* (copy-readtable nil)))
;; ---- midic -> symbol ----

(export '(*ascii-note-scales* *ascii-note-C-scale* *ascii-note-do-scale*))

(defvar *ascii-note-C-scale*)
(defvar *ascii-note-do-scale*)
(defvar *ascii-note-alterations*)
(defvar *ascii-note-scales* nil
  "The scales used by the functions mc->n and n->mc." )

(setf *ascii-note-C-scale*
  (mapc #'(lambda (x) (setf (car x) (string-upcase (string (car x)))))
    '((C) (C . :q) (C . :s) (D . :-q)
      (D) (D . :q) (E . :f) (E . :-q)
      (E) (E . :q)
      (F) (F . :q) (F . :s) (G . :-q)
      (G) (G . :q) (G . :s) (A . :-q)
      (A) (A . :q) (B . :f) (B . :-q)
      (B) (B . :q)  )))

(setf *ascii-note-do-scale*
  (mapc #'(lambda (x) (setf (car x) (string-downcase (string (car x)))))
    '((do) (do . :q) (do . :s) (re . :-q)
      (re) (re . :q) (mi . :f) (mi . :-q)
      (mi) (mi . :q)
      (fa) (fa . :q) (fa . :s) (sol . :-q)
      (sol)(sol . :q)(sol . :s)(la . :-q)
      (la) (la . :q) (si . :f) (si . :-q)
      (si) (si . :q)  )))

(setf *ascii-note-alterations*
   '((:s "#" +100) (:f "b" -100)
     (:s "s" +100) (:f "f" -100)
     (:q "+" +50) (:qs "#+" +150) (:-q "_" -50) (:f-q "b-" -150)
     (:s "d" +100)))

(setf *ascii-note-scales* (list *ascii-note-C-scale* *ascii-note-do-scale*))

(defun deep-mapcar (fun fun1 list? &rest args)
  "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
   (cond
    ((null list?) ())
    ((not (consp list?)) (apply fun1 list? args))
    (t (cons (apply #'deep-mapcar fun fun1 (car list?) args)
             (apply #'deep-mapcar fun fun1 (cdr list?) args)))))


(defun mc->n1 (midic &optional (ascii-note-scale (car *ascii-note-scales*)) (middle-C 3))
  "Converts <midic> to a string representing a symbolic ascii note."
  (let ((dmidic (/ 1200 (length ascii-note-scale))) note)
    (multiple-value-bind (midic/50 cents) (round midic dmidic)
      (multiple-value-bind (oct+2 midic<1200) (floor (* midic/50 dmidic) 1200)
        (setq note (nth (/ midic<1200 dmidic) ascii-note-scale))
        (format nil "~A~A~A~A~A"
          (car note) (or (car (cassq (cdr note) *ascii-note-alterations*)) "")
          (- oct+2 (- 5 middle-C)) (if (> cents 0) "+" "") (if (zerop cents) "" cents) )))))

; (mc->n1 6000)
; (mc->n1 6000 *ascii-note-do-scale* 3)

(defun n->mc1 (str &optional (*ascii-note-scale* (car *ascii-note-scales*)) (middle-C 3))
  "Converts a string representing a symbolic ascii note to a midic."
  (if (integerp str)
      str
      (progn 
	(setq str (string str))
	(let ((note (some #'(lambda (note)
			      (when (and (null (cdr note))
					 (eql 0 (search (car note) str :test #'string-equal)))
				note)) *ascii-note-scale*))
	      index midic alt)
	  (unless note (error "Note not found in ~S using the ~S ~%~S"
			      str '*ascii-note-scale* *ascii-note-scale*))
	  (setq midic (* (position note *ascii-note-scale*)
			 (/ 1200 (length *ascii-note-scale*))))
	  ;; at this point: "C" -> 0 ; "D" -> 100 ; "E" -> 200 ; etc.
	  (setq index (length (car note)))
	  ;; alteration
	  (when (setq alt (some #'(lambda (alt)
				    (when (eql index (search (cadr alt) str :start2 index
							     :test #'string-equal))
				      alt)) *ascii-note-alterations*))
	    (incf midic (third alt))			    ;it's there!
	    (incf index (length (second alt))))
	  ;; octave
	  (multiple-value-bind (oct i) (parse-integer str :start index :junk-allowed t)
	    (incf midic (* (+ oct (- 5 middle-C)) 1200))
	    (setq index i))
	  (unless (= index (length str))
	    (incf midic (parse-integer str :start index)))
	  midic))))

(defvar *ascii-intervals*)

(setf *ascii-intervals*
 '("1" "2m" "2M" "3m" "3M" "4" "4A" "5" "6m" "6M" "7m" "7M"))

(defun int->symb1 (int)
  "Converts a midic interval to a symbolic interval."
  (multiple-value-bind (oct cents) (floor int 1200)
    (let ((index (/ cents 100)))
      (unless (typep index 'fixnum) (error "Not yet implemented"))
      (if (zerop oct)
        (nth index *ascii-intervals*)
        (format () "~A~@D" (nth index *ascii-intervals*) oct)))))

;(defunt int->symb1 ((int fix)) string)

; called "itv->ascii" by CR
       

(om::defmethod! int->symb ((ints list))

  :initvals (list '(1 2)) 
  :indoc '("ints")
  :icon 128
  :doc  "<int->symb> takes an interval expressed in midi-cents, and returns a 
symbolic interval name.
Intervals are labeled as follows:

	1 = unison		2m = minor second
	2M = major second	3m = minor third
	3M = major third	4 = perfect fourth	
	4A = tritone		5 = perfect fifth	
	6m = minor sixth	6M = major sixth	
	7m = minor seventh	7M = major seventh

All intervals larger than an octave are expressed by adding or  subtracting an 
octave displacement after the simple interval name;
 for example, a major tenth becomes 3M+1, etc.  Note: for the time being,  the 
program has a strange way of expressing downward intervals:
 it labels the interval as its inversion, and then transposes downwards as
 necessary.  Thus, a major third down (-400 in midicents), returns 6m-1."
 
  (om::deep-mapcar #'int->symb #'int->symb1 ints))


(defun symb->int1 (int)
  (let* ((int-str (coerce (string int) 'list))
         (neg-oct (member #\- int-str :test #'char=))
         (rest-oct (or (member #\+ int-str :test #'char=) neg-oct))
         (oct (if rest-oct
                (read-from-string (coerce (cdr rest-oct) 'string))
                0))
         (pclass (coerce (butlast int-str (length rest-oct)) 'string)))
    (* 100  (+ (position pclass *ascii-intervals* :test #'string=)
               (* 12 (if neg-oct (- oct) oct))))))

(om::defmethod! symb->int ((ints list))
  :initvals (list '(1 2)) 
  :indoc '("ints")
  :icon 128
  :doc  "<symb->int> takes a symbolic interval name  , and returns an interval 
expressed in midi-cents. Intervals are labeled as follows:

	1 = unison			2m = minor second
	2M = major second	3m = minor third
	3M = major third		4 = perfect fourth	
	4A = tritone		5 = perfect fifth	
	6m = minor sixth		6M = major sixth	
	7m = minor seventh	7M = major seventh

All intervals larger than an octave are expressed by adding or subtracting an 
octave displacement after the simple interval name;
 for example, a major tenth becomes 3M+1, etc.  Note: for the time being,  
Patchwork has a strange way of expressing downward intervals:  it labels the 
interval as its inversion, and then transposes downwards as necessary. Thus, a 
major third down 6m-1, returns -400 in midicents ."
  
  (om::deep-mapcar #'symb->int #'symb->int1 ints))


(om::defmethod! mc->n ((midics? list) &optional (middle-C 3))
  :initvals '((6000)) 
  :indoc '("pitch or pitch list (midicents)")
  :icon 141
  :doc  "
Converts <midics> to symbolic (ASCII) note names. 

Symbolic note names follow standard notation.
Middle c (midicent 6000) being C3 by default, can be set to be C4 by the optional input.
Semitones are labeled with a '#' or a 'b.'  
Quartertone flats are labeled with a '_', and quartertone sharps with a '+' (ex. C3 a quartertone sharp (midi-cent 6050), would be labeled 'C+3'. 
Gradations smaller than a quartertone are expressed as the closest  quartertone + or - the remaining cent value (ex. midi-cent 8176 would be expressed as Bb4-24).
"
 
  (om::deep-mapcar 'mc->n #'(lambda (mc) (mc->n1 mc (car *ascii-note-scales*) middle-C)) midics?))

(om::defmethod! mc->n ((midic number) &optional (middle-C 3))
  (mc->n1 midic (car *ascii-note-scales*) middle-C))

(om::defmethod! n->mc ((strs list) &optional (middle-C 3))
  :initvals '(("C3") ) 
  :indoc '("note name or list of note names" "octave of middle C")
  :menuins '((1 (("middle-C = 3" 3) ("middle-C = 4" 4))))
  :icon 141
  :doc   "
Converts <strs> to pitch values in midicents. 

Symbolic note names follow standard notation. 
Middle c (midicent 6000) being C3 by default, can be set to be C4 by the optional input.
Semitones are labeled with a '#' or a 'b.', or a 'f' or a 's', e.g. 'G#3' or 'GS3'.
Quartertone flats are labeled with a '_', and quartertone sharps with a '+' (ex. C3 a quartertone sharp (midi-cent 6050), would be labeled 'C+3'. 
Gradations smaller than a quartertone are expressed as the closest  quartertone + or - the remaining cent value (ex. midi-cent 8176 would be expressed as Bb4-24).
" 
  (om::deep-mapcar 'n->mc #'(lambda (n) (n->mc1 n (car *ascii-note-scales*) middle-C)) strs))

(om::defmethod! n->mc ((strs string) &optional (middle-C 3))
  (car (n->mc (list strs) middle-C)))

(om::defmethod! n->mc ((symb symbol) &optional (middle-C 3))
  (n->mc (string symb) middle-C))

;;;=======================================
;;; TEMPO UTILS
;;;=======================================

(om::defmethod! beats->ms ((nb-beat number) (tempo number))
  :initvals '(1 60) 
  :indoc '("number of beats or beat division (ex. 1, 4, 1/8, ...)" "")
  :icon 141
  :outdoc '("duration (ms)")
  :doc   "
Converts a symbolic rhythmic beat division into the corresponding duration in milliseconds. 
" 
  (let ((b-ms (* 1000.0 (/ 60 tempo))))
    (round (* nb-beat b-ms)))
  )


;;;=======================================
;;; from OM2Csound
;;;=======================================

(defun deep-mapcar/1 (fun list? &rest args)
  (labels ((map-structure (str accum)
             (cond ((null str) (reverse accum))
                   ((not (consp str))
                    (if accum (reverse (cons (apply fun str args) accum)) (apply fun str args)))
                   (t (map-structure (cdr str) (cons (map-structure (car str) ()) accum))))))
    (map-structure list? nil)))

(defun LLdecimals (list nbdec)
  "Arrondit liste de profondeur quelconque avec <nbdec> decimales"
  (let ((ndec 
         (if (> nbdec 0 ) (float (expt 10 nbdec)) (expt 10 nbdec))))
    (deep-mapcar/1 '/  
                   (deep-mapcar/1 'round list (/ 1 ndec)) ndec )))

