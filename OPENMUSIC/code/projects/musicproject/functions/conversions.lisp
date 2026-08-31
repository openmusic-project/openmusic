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
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================

(in-package :om)

;==================================
;DEV UTILITY
;==================================


(defmethod const-oct-tuning ((self number))
  "returns a list of pitch tuning values according to edo <self>"
  (let* ((edo (butlast (gen-edo-scale self)))
         (mod (multiple-value-list (om// edo 100)))
         (lst (mat-trans (list (car mod) (second mod))))
         (res (list (car lst))))
    
    (loop for i in (cdr lst)
        do (if (= (car i) (caar res))
               (setf (car res) (x-append (car res) (second i)))
             (push i res)))
    (reverse res)))

(defun fill-list-with-0 (lst)
  (let ((res lst)
        (lgt (list-max (mapcar 'length lst)))) 
    (loop for i from 0 to (1- (length res))
            do
            (loop while (< (length (nth i res)) lgt)
                  do (setf (nth i res) (x-append (nth i res) 0))))
    res))


(defun distribute-oct-tuning (lst)
  (let ((res (cdr (mat-trans lst))))
    (loop for i in res
          collect (mapcar 'float i))))

  
(defmethod* octave-tuning-scales ((self number))
  :numouts 1 
  :initvals '(12) 
  :indoc '("division")
  :icon 141
  :doc " Developement utility: returns the octave tunings needed for some EDO's."
  (let* ((const (const-oct-tuning self))
         (fill (fill-list-with-0 const)))
    (distribute-oct-tuning fill)))

;==================================
;APPROX_EDO
;==================================
;Approximates to the nearest note of a given scale (EDO)


(defmethod* gen-Edo-scale ((edo number) &optional (root 6000))
  :numouts 1 
  :initvals '(12 6000) 
  :indoc '("division" "root note")
  :icon 141
  :doc "Generates edo midics"
  (let* ((beg root)
         (end (+ 1200 root))
         (scale (arithm-ser beg end (/ 1200 edo))))
    ;(om-round scale)
    (mapcar 'round scale)
    ))

(defun trans-in-octave (note root)
"transpose a note inside an octave"
  (let ((rep note))
    (if (< rep root)
         (progn 
           (setf rep (+ rep 1200))
           (trans-in-octave rep root))
      (if (> rep (+ root 1200))
          (progn 
            (setf rep (- rep 1200))
            (trans-in-octave rep root))
        rep))))

;(trans-in-octave 38 6100)
;(trans-in-octave 6038 7000)

(defun closest-int-in-list (int edo)
  "find the closest midic <int> to a ref scale <edo>"
  (let* ((refscale (gen-edo-scale edo))
         (trans (trans-in-octave int 6000))
         (corr (- int trans))
         (diff (om-abs (om- refscale trans)))
         (pos (position (list-min diff) diff :from-end t)))
    (+ (nth pos refscale) corr)))
#|
(defun closest-int-in-list (int edo)
  "find the closest midic <int> to a ref scale <edo>"
  (let* ((refscale (gen-edo-scale edo))
         (trans (trans-in-octave int 6000))
         (corr (- int trans))
         (diff (om-abs (om- refscale trans)))
         (pos (position (list-min diff) diff :from-end *approx-dir*)))
    (+ (nth pos refscale) corr)))
|#
;(closest-int-in-list1 4871 96)

(defmethod* approx-edo ((midic number) (edo number))
  :initvals '(6000 2) 
  :indoc '("pitch list (midicents)" "edo div")
  :icon 141
  :doc "Approximates <midic> to a given <edo>"
(closest-int-in-list midic edo))

(defmethod* approx-edo ((midic list) (edo number))
  (loop for i in midic
        collect (approx-edo i edo)))

;==================================
;APPROX_M
;==================================

(defmacro cassq (item list) `(cdr (assoc ,item ,list :test #'eq)))
;(defmacro cassq (item list) `(cdr (assq ,item ,list)))
;;maybe replace this with a function (the macro doesn't work with synbols)
#|
(defun cassq (sym l)
  (cdr (assoc sym l)))
|#

;moved to musicpreferences.lisp
;(defvar *global-midi-approx* 2)
;(setf *global-midi-approx* 2)

#|
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
|#

;;EDO compatibility
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
  (cond ((<= approx 0) midic)
        ((<= approx 16) 
         (round (* (floor (+ (* (- midic ref-midic) approx) 100) 200) 200) approx))
        ((> approx 16)  
         (approx-edo (- midic ref-midic) (round (/ approx 10))))
        (t midic)))

(defmethod* approx-m  ((self list) approx &optional (ref-midic 0))
  (if (<= approx 0)
    self
    (loop for item in self
          collect (approx-m item approx ref-midic))))

;==================================
;EDITOR's APPROX
;==================================

(defmethod get-approx-from-edparam ((self note))
  (let ((box (associated-box self)))
    (when box 
      (cdr (assoc 'approx (edition-params box))))))

(defmethod get-approx-from-edparam ((self chord))
  (let ((box (associated-box self)))
    (when box 
      (cdr (assoc 'approx (edition-params box))))))

(defmethod get-approx-from-edparam ((self chord-seq))
  (let ((box (associated-box self)))
    (when box 
      (cdr (assoc 'approx (edition-params box))))))

(defmethod get-approx-from-edparam ((self voice))
  (let ((box (associated-box self)))
    (when box 
      (cdr (assoc 'approx (edition-params box))))))

(defmethod real-midics ((self chord))
  (let* ((chords (inside self)))
    (loop for i in chords
          collect (mapcar 'midic (inside i)))))

(defmethod real-midics ((self chord-seq))
  (let* ((chords (inside self)))
    (loop for i in chords
          collect (mapcar 'midic (inside i)))))

(defmethod real-midics ((self voice))
  (let* ((chords (get-real-chords-and-graces self)))
    (loop for i in chords
          collect (mapcar 'midic (inside i)))))

(defmethod approx-midics ((self chord))
  (let* ((box (get-approx-from-edparam self))
         (apprx (if box box (approx self)))
         (chords (inside self)))
    (loop for i in chords
          collect (approx-m (midic i) apprx))))

(defmethod approx-midics ((self chord-seq))
  (let* ((box (get-approx-from-edparam self))
         (apprx (if box box (approx self)))
         (chords (inside self)))
    (loop for i in chords
          collect (approx-m (lmidic i) apprx))))


(defmethod approx-midics ((self voice))
  (let* ((box (get-approx-from-edparam self))
         (apprx (if box box (approx self)))
         (chords (chords self)))
    (loop for i in chords
          collect (approx-m (lmidic i) apprx))))

;==================================
;MIDIC conversions
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


(defmethod* f->mc  ((freq number) &optional (approx nil) (ref-midic 0))
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
- <approx> = 0 no approximation.
- <approx> = nil (default) = rounded to one midicent resolution.

<ref-midic> is a midicent that is subtracted from <midic> before computation: the computation can then be carried on an interval rather than an absolute pitch."
  (if approx
  (approx-m (f->mf freq) approx ref-midic)
    (round (f->mf freq))))

(defmethod* f->mc  ((freq list) &optional (approx nil) (ref-midic 0))
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
  :initvals '((6000) 3) 
  :menuins '((1 (("middle-C = 3" 3) ("middle-C = 4" 4))))
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
  :initvals '(("C3") 3) 
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
;;; EDO-n->mc
;;;=======================================

(defun lookup-natural (letter tk)
  (let* ((entry (assoc tk *natural-tables* :test #'equal))
         (subtable (when entry (cdr entry))))
    (when subtable
      (let ((match (assoc (string-upcase (string letter)) subtable
                          :key (lambda (s) (string-upcase (symbol-name s)))
                          :test #'string=)))
        (when match (second match))))))


(defun lookup-alteration (alt-char tk)
  (let* ((entry (assoc tk *alteration-tables* :test #'equal))
         (subtable (when entry (cdr entry)))
         (alt-str (string-upcase (string alt-char))))
    (when subtable
      (let ((match (assoc alt-str subtable
                          :key (lambda (s) (string-upcase (symbol-name s)))
                          :test #'string=)))
        (when match (second match))))))


(defun sum-alterations (alt-string tk)
  (let ((total 0))
    (loop for ch across alt-string
          for val = (lookup-alteration ch tk)
          do (if val
                 (incf total val)
                 (return-from sum-alterations nil)))
    total))


(defun parse-pitch-string-v2 (str)
  (let* ((s (string str))
         (len (length s))
         (i 0))
    (unless (and (< i len)
                 (find (char-upcase (char s i)) "ABCDEFG"))
      (error "expected pitch letter at start of ~S" s))
    (let ((letter (char-upcase (char s 0))))
      (incf i)
      (loop while (and (< i len)
                       (find (char s i) "#bBvV^+dDxX"))
            do (incf i))
      (let* ((alt-string (string-downcase (subseq s 1 i)))
             (oct-start i))
        (when (and (< i len) (find (char s i) "+-"))
          (incf i))
        (loop while (and (< i len) (digit-char-p (char s i)))
              do (incf i))
        (when (= i oct-start)
          (error "expected octave integer in ~S" s))
        (let ((octave (parse-integer (subseq s oct-start i)))
              (cents 0))
          (when (and (< i len) (find (char s i) "+-"))
            (let ((cent-start i))
              (incf i)
              (loop while (and (< i len) (digit-char-p (char s i)))
                    do (incf i))
              (setf cents (parse-integer (subseq s cent-start i)))))
          (values letter alt-string octave cents))))))


(defun octave-base (oct mc)
  (let ((offset (if (= mc 4) 1200 2400)))
    (+ offset (* oct 1200))))


(defun pitch-to-midicents-v2 (pitch-token tk mc)
  (multiple-value-bind (letter alt-string octave cents)
      (parse-pitch-string-v2 pitch-token)
    (let ((natural (lookup-natural letter tk))
          (alteration (sum-alterations alt-string tk)))
      (if (or (null natural) (null alteration))
          nil
          (let* ((pitch-class (+ natural alteration))
                 (result (+ pitch-class (octave-base octave mc) cents)))
            (if (< result 0) nil result))))))


(defun convert-v2 (item tk mc)
  (cond
    ((null item) nil)
    ((or (symbolp item) (stringp item))
     (pitch-to-midicents-v2 item tk mc))
    ((listp item)
     (mapcar (lambda (x) (convert-v2 x tk mc)) item))
    (t (error "unexpected input ~S" item))))


(defmethod! edo-n->mc ((self list) EDO &optional (middle-c 3))
  :initvals '(("C3") 72 3)
  :indoc '("symbolic pitch value or list of symbolic pitch values" "EDO" "octave of middle C")
  :menuins '((2 (("middle-C = 3" 3) ("middle-C = 4" 4))))
  :icon 141
  :doc "
Converts symbolic pitch values to midicent values according to a specified equal-division-of-the-octave (EDO) tuning system.

------------------------
Arguments
------------------------

1. Pitch values - A list, or list of lists, of symbolic pitch values. See 'Pitch value syntax' below.
2. EDO - The equal division of the octave, as an integer from 3 to 96.
3. Middle C octave - The octave number assigned to middle C: 3 or 4.

Middle C (midicent value 6000) is the fixed reference pitch by which all pitch values are calculated. Depending on the middle C octave argument, it is represented symbolically as either C3 or C4.

------------------------
Pitch value syntax
------------------------

A pitch value consists of a note name, optional alterations, an octave number, and an optional cent deviation:

<note name>[alterations]<octave>[cent deviation]

Example:
C#^^3-10 
This represents C-sharp, raised by two EDO steps, in the specified octave, with an additional deviation of -10 cents.

The optional cent deviation is added to or subtracted from the final calculated midicent value.

Supported alteration symbols:

^ = Arrow up
v = Arrow down
# = Sharp
b = Flat
+ = Half sharp
d = Half flat
x = Double sharp

Any number of alteration symbols may be combined arbitrarily (e.g. G#+3, Gbb^3, G##vvv3 are all valid inputs).

------------------------
Calculation logic
------------------------

Midicent values are calculated in the following way:
M = O + N + A + D

Where:
M = resulting midicent value
O = midicent value of C in the specified octave
N = octave-reduced pitch definition of the note name
A = sum of the pitch definitions of all alterations
D = optional cent deviation

------------------------
The role of the chain of fifths
------------------------

Standard staff notation is deeply rooted in the chain of fifths. For this reason, this function uses the chain of fifths in the specified EDO to calculate the pitch definition of all notes names (C, D, E, F, G, A, and B) and alterations (except ^ and v, as explained further below).

For each EDO, the 'fifth' (i.e. the interval that represents the frequency ratio of 3/2) is selected according to the following rules:

- For 13-EDO and 18-EDO, the second-closest (i.e. flatter) approximation of 3/2 in absolute cents is used. 
- For all other EDOs, the closest approximation of 3/2 in absolute cents is used.

Exception: 3-, 4-, 6-, and 8-EDO do not have a useful approximation of 3/2 for the purpose of the chain-of-fifths notation. These EDOs are therefore notated as subsets of larger EDOs, as described further below.

------------------------
Pitch definitions of note names
------------------------

The pitch definitions of note names (C, D, E, F, G, A, and B) are derived from their positions in the chain of fifths.

C is the reference pitch, so its octave-reduced pitch definition is 0 cents. Each other note name is defined by moving up or down by the appropriate number of fifths from C and octave-reducing the result.

For the calculations below, p denotes the size of the fifth, in cents.

For example, in 17-EDO, the fifth is ten EDO steps:
p = (10/17) x 1200 ~ 705.882 cents

D is two fifths above C:
2p ~ 1411.765 cents

Octave-reducing this gives:
1411.765 - 1200 ~ 211.765 cents

Thus, the octave-reduced pitch definition of D in 17-EDO is approximately 211.765 cents.

The octave-reduced pitch definitions of the remaining note names are calculated in the same way according to their positions in the chain.

------------------------
Pitch definitions of alterations
------------------------

The ^ and v alterations raise and lower the pitch by one EDO step respectively; all other alterations (#, b, +, d, and x) are defined by the chain of fifths in the specified EDO.

Sharps are defined by moving upward by seven fifths and octave-reducing the result:
sharp = 7p mod 1200

Therefore, in 17-EDO:
7p ~ 4941.176 cents

Octave-reducing this gives:
4941.176 - (4 x 1200) ~ 141.176 cents

Thus, the sharp alteration in 17-EDO is approximately +141.176 cents.

A flat is defined analogously by moving seven fifths downward.

------------------------
Half sharps and half flats
------------------------

Half sharps (+) and half flats (d) span exactly half the number of EDO steps of the corresponding sharp or flat. They are therefore only available when the corresponding sharp or flat spans an even number of EDO steps.

For example, in 24-EDO, a sharp spans two EDO steps (100 cents), so a half sharp spans one EDO step (50 cents):

# = +100 cents
+ = +50 cents
b = -100 cents
d = -50 cents

------------------------
Multiple sharps/flats
------------------------

Multiple sharps and flats are treated as successive applications of the corresponding single alteration. Their pitch definitions are therefore summed.

------------------------
Special cases
------------------------

'Perfect' EDOs (7, 14, 21, 28, 35):
Because the fifth is exactly 4/7 of an octave, the chain forms a closed cycle through the seven natural notes. Consequently, sharps and flats have no effect.

'Pentatonic' EDOs (5, 10, 15, 20, 25, 30):
Because the fifth is exactly 3/5 of an octave, the chain causes E to coincide with F and B to coincide with C.

'Superflat' EDOs (9, 11, 13, 16, 18, and 23):
Because the fifth is narrower than 4/7 of an octave, accidentals function inversely in the chain-of-fifths system: sharps lower the pitch while flats raise the pitch.

------------------------
Subset EDOs
------------------------

3-, 4-, and 6-EDO are notated as subsets of 12-EDO.
8-EDO is notated as a subset of 24-EDO.
"

  (handler-case (convert-v2 self EDO middle-c)
    (error (e)
      (format t "edo-n->mc error: ~A~%" e)
      nil)))

;;;=======================================
;;; mc->EDO-n
;;;=======================================

;;; ─────────────────────────────────────────────
;;; ALTERATION COMBINATIONS
;;; ─────────────────────────────────────────────

(defparameter *alteration-combinations*
  '((0 . ("" "+" "#" "^" "v" "#+" "#^" "#v" "^^" "vv" "#^^" "#vv" "^^^" "vvv" "#^^^" "#vvv" "##" "#^^^^" "#vvvv" "^^^^" "vvvv" "+^" "+v"))
    (1 . ("" "d" "b" "^" "v" "db" "b^" "bv" "^^" "vv" "b^^" "bvv" "^^^" "vvv" "b^^^" "bvvv" "bb" "b^^^^" "bvvvv" "^^^^" "vvvv" "d^" "dv"))
    (2 . ("" "b" "#" "d" "+" "v" "^" "db" "#+" "bv" "b^" "#v" "#^" "vv" "^^" "bb" "##" "bvv" "b^^" "#vv" "#^^" "vvv" "^^^" "bvvv" "b^^^" "#vvv" "#^^^" "vvvv" "^^^^" "bvvvv" "b^^^^" "#vvvv" "#^^^^" "dv" "d^" "+v" "+^"))))


;;; ─────────────────────────────────────────────
;;; CACHES
;;; ─────────────────────────────────────────────

(defvar *naturals-cache*       (make-hash-table :test #'eql))
(defvar *valid-alts-cache*     (make-hash-table :test #'equal))
(defvar *fifth-chain-ht*       (make-hash-table :test #'equal))
;; Grid cache: (tk pref offset) -> vector of #(prank nat-sym alt-string octave candidate-mc fifth-step)
;; fifth-step is an integer in [-6,10] if the entry passes the fifth-chain filter, otherwise :NONE.
;; Sorted by candidate-mc asc, then prank asc within same mc.
(defvar *grid-cache*           (make-hash-table :test #'equal))
;; use-fifth cache: tk -> t or nil (whether fifth-chain tier applies for this EDO)
(defvar *use-fifth-cache*      (make-hash-table :test #'eql))
(defvar *fifth-chain-ht-built* nil)


;;; ─────────────────────────────────────────────
;;; CACHE BUILDERS
;;; ─────────────────────────────────────────────

(defun build-fifth-chain-ht ()
  (clrhash *fifth-chain-ht*)
  (loop for (sym step) in *fifth-chain*
        do (setf (gethash (symbol-name sym) *fifth-chain-ht*) step))
  (setf *fifth-chain-ht-built* t))

(defun ensure-fifth-chain-ht ()
  (unless *fifth-chain-ht-built* (build-fifth-chain-ht)))

(defun alteration-char-value (ch alt-alist)
  (let* ((sym  (find-symbol (string-upcase (string ch)) :om))
         (pair (when sym (assoc sym alt-alist))))
    (when (and pair (not (zerop (second pair))))
      (second pair))))

(defun compute-alt-value (alt-string alt-alist)
  (if (string= alt-string "")
      0
      (let ((total 0))
        (loop for ch across alt-string
              for val = (alteration-char-value ch alt-alist)
              if val do (incf total val)
              else do (return-from compute-alt-value nil))
        total)))

(defun blacklisted-p (alt-string tk)
  (let ((has-arrow (some (lambda (c) (find c "^v")) alt-string))
        (has-half  (some (lambda (c) (find c "+d")) alt-string)))
    (or (and has-arrow (member tk '(8 24)))
        (and has-half  (member tk '(10 11 14 18 20 28 30))))))

(defun build-naturals-cache-for (tk)
  (let* ((entry    (assoc tk *natural-tables* :test #'=))
         (nat-list (cdr entry))
         (vec      (make-array (length nat-list))))
    (loop for (sym val) in nat-list for i from 0
          do (setf (aref vec i) (cons sym val)))
    (setf (gethash tk *naturals-cache*) vec)))

(defun build-valid-alts-cache-for (tk preference)
  (let* ((alt-entry (assoc tk *alteration-tables* :test #'=))
         (alt-alist (cdr alt-entry))
         (alt-list  (cdr (assoc preference *alteration-combinations* :test #'=)))
         (result    '()))
    (loop for alt-string in alt-list
          unless (blacklisted-p alt-string tk)
          do (let ((val (compute-alt-value alt-string alt-alist)))
               (when val (push (cons alt-string val) result))))
    (setf (gethash (cons tk preference) *valid-alts-cache*)
          (coerce (nreverse result) 'vector))))

(defun compute-fifth-step (nat-sym alt-string)
  "Return the fifth-chain step for (nat-sym, alt-string) if it falls within
   [-6, 10], otherwise :NONE.  Called only at cache-build time."
  (let* ((filtered (remove-if-not (lambda (c) (find c "#b")) alt-string))
         (lookup   (concatenate 'string
                                (string-upcase (symbol-name nat-sym))
                                (string-upcase filtered)))
         (step     (gethash lookup *fifth-chain-ht*)))
    (if (and step (<= -6 step 10)) step :none)))

(defun build-grid-cache-for (tk pref offset)
  "Precompute every (alt, nat, octave) combination as a flat sorted vector.
   Each entry: #(prank nat-sym alt-string octave candidate-mc fifth-step)
   prank      = alt-idx*1000 + nat-idx, encodes preference order.
   fifth-step = integer in [-6,10] if entry passes fifth-chain filter, else :NONE.
   Sorted by candidate-mc ascending, then prank ascending within same mc."
  (let* ((naturals   (gethash tk *naturals-cache*))
         (valid-alts (gethash (cons tk pref) *valid-alts-cache*))
         (n-alts     (length valid-alts))
         (n-nats     (length naturals))
         (n-octaves  12)  ; octaves -2 to 9 inclusive
         (vec        (make-array (* n-alts n-nats n-octaves)))
         (i          0))
    (loop for alt-idx from 0
          for (alt-string . alt-val) across valid-alts
          do (loop for nat-idx from 0
                   for (nat-sym . nat-val) across naturals
                   do (let ((base       (round (+ nat-val alt-val offset)))
                            (prank      (+ (* 1000 alt-idx) nat-idx))
                            (fifth-step (compute-fifth-step nat-sym alt-string)))
                        (loop for octave from -2 to 9
                              do (setf (aref vec i)
                                       (vector prank nat-sym alt-string octave
                                               (+ base (* octave 1200))
                                               fifth-step))
                                 (incf i)))))
    (sort vec (lambda (a b)
                (let ((mc-a (aref a 4)) (mc-b (aref b 4)))
                  (if (= mc-a mc-b)
                      (< (aref a 0) (aref b 0))
                      (< mc-a mc-b)))))
    (setf (gethash (list tk pref offset) *grid-cache*) vec)))

(defun ensure-caches (tk preference)
  "Lazily build all caches for TK and PREFERENCE on first use.
   Coerces both to integers so float inputs from OM are handled correctly."
  (ensure-fifth-chain-ht)
  (let ((tk   (round tk))
        (pref (round preference)))
    (unless (gethash tk *naturals-cache*)
      (build-naturals-cache-for tk))
    (unless (gethash (cons tk pref) *valid-alts-cache*)
      (build-valid-alts-cache-for tk pref))
    (dolist (offset '(1200 2400))
      (unless (gethash (list tk pref offset) *grid-cache*)
        (build-grid-cache-for tk pref offset)))
    (unless (nth-value 1 (gethash tk *use-fifth-cache*))
      (setf (gethash tk *use-fifth-cache*)
            (not (member tk '(16 19 23 26 31)))))
    (values tk pref)))

(defun init-edo-caches ()
  "Call this if you redefine *alteration-tables*, *natural-tables*, or
   *fifth-chain* at runtime.  Do NOT call at load time -- tables may not
   be defined yet."
  (setf *fifth-chain-ht-built* nil)
  (clrhash *fifth-chain-ht*)
  (clrhash *naturals-cache*)
  (clrhash *valid-alts-cache*)
  (clrhash *grid-cache*)
  (clrhash *use-fifth-cache*))


;;; ─────────────────────────────────────────────
;;; BINARY SEARCH
;;; ─────────────────────────────────────────────

(defun grid-lower-bound (vec target)
  "Return index of first entry in VEC with candidate-mc >= TARGET."
  (let ((lo 0) (hi (length vec)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (< (aref (aref vec mid) 4) target)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    lo))


;;; ─────────────────────────────────────────────
;;; CORE SEARCH
;;; ─────────────────────────────────────────────

(defun find-best-match (midicent tk mc preference)
  (multiple-value-bind (tk pref) (ensure-caches tk preference)
    (let* ((offset    (if (= mc 4) 1200 2400))
           (grid      (gethash (list tk pref offset) *grid-cache*))
           (n         (length grid))
           (idx       (grid-lower-bound grid midicent))
           (use-fifth (gethash tk *use-fifth-cache*))
           (min-dev   most-positive-fixnum)
           (tier1     nil)   ; best entry passing fifth-chain filter
           (tier2     nil))  ; best entry by preference order (fallback)

      ;; Scan right from binary search point
      (loop for i from idx below n
            for entry = (aref grid i)
            for abs-dev = (abs (- midicent (aref entry 4)))
            while (<= abs-dev min-dev)
            do (when (< abs-dev min-dev)
                 (setf min-dev abs-dev
                       tier1   nil
                       tier2   nil))
               (when (null tier2)
                 (setf tier2 entry))
               (when (and use-fifth (null tier1)
                          (not (eq (aref entry 5) :none)))
                 (setf tier1 entry)))

      ;; Scan left from binary search point
      (loop for i from (1- idx) downto 0
            for entry = (aref grid i)
            for abs-dev = (abs (- midicent (aref entry 4)))
            while (<= abs-dev min-dev)
            do (when (< abs-dev min-dev)
                 (setf min-dev abs-dev
                       tier1   nil
                       tier2   nil))
               (when (or (null tier2) (< (aref entry 0) (aref tier2 0)))
                 (setf tier2 entry))
               (when (and use-fifth
                          (not (eq (aref entry 5) :none))
                          (or (null tier1) (< (aref entry 0) (aref tier1 0))))
                 (setf tier1 entry)))

      (let ((result (or tier1 tier2)))
        (when result
          (list (aref result 1)                         ; nat-sym
                (aref result 2)                         ; alt-string
                (aref result 3)                         ; octave
                (round (- midicent (aref result 4)))))))))  ; deviation


;;; ─────────────────────────────────────────────
;;; OUTPUT FORMATTING
;;; ─────────────────────────────────────────────

(defun format-pitch-symbol (nat alt-string octave deviation include-deviation)
  (let* ((nat-str (string-upcase (symbol-name nat)))
         (dev-str (when (and include-deviation (not (= deviation 0)))
                    (format nil "~@D" deviation)))
         (sym-str (concatenate 'string
                               nat-str
                               alt-string
                               (format nil "~D" octave)
                               (or dev-str ""))))
    (intern sym-str :om)))


;;; ─────────────────────────────────────────────
;;; PUBLIC INTERFACE
;;; ─────────────────────────────────────────────

(defun mc-to-pitch (midicent tk mc preference include-deviation)
  (let ((match (find-best-match midicent tk mc preference)))
    (when match
      (destructuring-bind (nat alt octave deviation) match
        (format-pitch-symbol nat alt octave deviation include-deviation)))))

(defun convert-mc (item tk mc preference include-deviation)
  (cond
    ((null item)    nil)
    ((numberp item) (mc-to-pitch item tk mc preference include-deviation))
    ((listp item)   (mapcar (lambda (x) (convert-mc x tk mc preference include-deviation)) item))
    (t              nil)))

(defmethod! mc->edo-n ((self list) EDO
                       &optional (middle-c 3) (preference 2) (include-deviation 0))
  :initvals '((6000) 72 3 2 1)
  :indoc    '("midicent value or list of midicent values" "EDO"
              "octave of middle C" "enharmonic spelling preference" "include cent deviations")
  :menuins  '((2 (("middle-C = 3" 3) ("middle-C = 4" 4)))
              (3 (("sharps" 0) ("flats" 1) ("simplest" 2)))
              (4 (("exclude" 0) ("include" 1))))
  :icon 141
  :doc "
Converts midicent values to symbolic pitch values according to a specified equal-division-of-the-octave (EDO) tuning system.

The pitch definitions of note names and alterations for each EDO follow the rules described in edo-n->mc. See the edo-n->mc documentation for more information.

------------------------
Arguments
------------------------

1. Midicent values - A list, or list of lists, of midicent values.
2. EDO - The equal division of the octave, as an integer from 3 to 96.
3. Middle C octave - The octave number assigned to middle C: 3 or 4.
4. Enharmonic spelling mode - Determines how enharmonically equivalent spellings are selected:
0 = Sharps only; 1 = Flats only; 2 = Simplest spelling
5. Include cent deviations - Determines whether cent deviations are included in the output:
0 = No; 1 = Yes

------------------------
Conversion logic
------------------------

Each input midicent value is matched to the symbolic pitch value whose pitch definition is closest in absolute cents. Normal rounding rules are used to resolve ties.

------------------------
Enharmonic spelling
------------------------

Some pitches can be represented by more than one symbolic pitch value. The enharmonic spelling mode determines which spelling is selected.

Mode 0 uses sharp spellings, mode 1 uses flat spellings, and mode 2 uses the simplest spelling. In mode 2, spellings are prioritised in the following order:

\"\"  b  #  d  +  v  ^  
db  #+  bv  b^  #v  #^  vv  ^^  bb  ##
bvv  b^^  #vv  #^^  vvv  ^^^  
bvvv  b^^^  #vvv  #^^^  vvvv  ^^^^
bvvvv  b^^^^  #vvvv  #^^^^  
dv  d^  +v  +^

For example, in 17-EDO, C+ and Db are enharmonically equivalent. Db is selected because b has higher priority than +.

------------------------
Cent deviations
------------------------

When 'Include cent deviations' is enabled (1), the difference between the input midicent value and the selected EDO pitch is included in the output.

For example, in 12-EDO:
6003 -> C3+3

When 'Include cent deviations' is disabled (0), the cent deviation is omitted:
6003 -> C3
"

  (handler-case
      (convert-mc self (round EDO) (round middle-c) (round preference) (= include-deviation 1))
    (error (e)
      (format t "mc->edo-n error: ~A~%" e)
      nil)))

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


;;;;;;;;;;;;;;;;;;;;;

;By Paulo Raposo 
(defmethod* ms->ratios ((durs list) (tempo number)) 
  :initvals '((1000) 60) 
  :indoc '("durations" "tempo <qtempo>")
  :icon 141
  :doc "Converts milliseconds <durs> into ratios (symbolic figures) according to tempo <tempo>."
  (let ((whole-note (/ 240000 tempo))
        (grace-epsilon 1/100))
    (mapcar #'(lambda (x)
                (let ((ratio (rationalize (coerce x 'double-float))))
                  (if (> ratio grace-epsilon)
                      ratio
                    0)))
            (om/ durs whole-note))))

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

