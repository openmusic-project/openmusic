;; ==================================================================================== 
;;                                musicxml export
;; ==================================================================================== 
;;
;;                                  
;;                authors : Karim Haddad  and Petar Klanac 
;;                     
;;
;;
;;                           $Revision: 1.1 $
;;                      $Date: 2008/06/23 15:07:04 $
;;
;;
;;
;;
;;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


(defpackage "MusicXML" 
  (:use "COMMON-LISP")
  (:nicknames "MXML"))

(in-package "MXML")

(pushnew :musicxml *features*)

(defvar *xml-version* "XML 1.0")


;     Index : [1] TC
;             [2] TRADUTEUR XML
;             [3] INTERFACE OM


;          |-------------------------------------------------------------------------|
;          |                                   TC                                    | 
;          |-------------------------------------------------------------------------|

;;By Petar Klanac


(defun r/n (source) (remove nil source))

(defun notnull (source) (not (null source)))

(defun 0? (source) (equal source 0))

(defun chr? (source) (typep source 'character))

(defun chrl? (source) (and (consp source) (loop for x in source always (typep x 'character))))

(defun chrll? (source) (and (consp source) (loop for x in source always (chrl? x))))

(defun sym? (source) (typep source 'symbol))

(defun syml? (source) (and (consp source) (loop for x in source always (typep x 'symbol))))

(defun str? (source) (typep source 'string))

(defun strl? (source) (and (consp source) (loop for x in source always (typep x 'string))))

(defun nbr? (source) (typep source 'number))

(defun nbrOrd? (source)
	(if (not (sym? source)) nil
		(let* ((src (couper (str source) "e" 'c)) (x (first src)) (y (second src)))
		(and (id? (read-from-string x) 'n) (or (equal y "er") (equal y "e"))))))

(defun nbrl? (source) (and (consp source) (loop for x in source always (typep x 'number))))

(defun nn? (source) (and (typep source 'number) (< source 0)))

(defun rat? (source) (typep source 'rational))

(defun ratl? (source) (and (consp source) (loop for x in source always (typep x 'rational))))

(defun note? (source) (typep source '(integer 0 14800)))

(defun notes? (source) (and (consp source) (loop for x in source always (note? x))))

(defun accords? (source) (and (consp source) (loop for x in source always (notes? x))))

(defun chr (source)
  (cond
    ((or (chr? source) (chrl? source)) source)
    ((and (str? source) (equal (length source) 1)) (coerce source 'character))
    ((str? source) (coerce source 'list))
    ((strl? source) (loop for x in source collecting (coerce x 'list)))
    ((sym? source) (chr (format nil "~A" source)))
    ((syml? source) (loop for x in source collecting (chr x)))
    (t (chr (format nil "~A" source)))))

(defun str (source)
  (cond
    ((chr? source) (coerce (list source) 'string))
    ((chrl? source) (coerce source 'string))
    ((chrll? source) (loop for x in source collecting (coerce x 'string)))
    ((strl? source) (coerce (loop for x in source appending (coerce x 'list)) 'string))
    ((sym? source) (format nil "~A" source))
    ((syml? source) (loop for x in source collecting (format nil "~A" x)))
    (t (format nil "~A" source))))

(oa::om-with-redefinitions 
  (defun // (source &optional diviseur) (if (null diviseur) (apply #'/ source) (sub_// source diviseur)))
  )

(defmethod sub_// ((source list) (diviseur list)) (loop for x in source and d in diviseur collecting (/ x d))) 

(defmethod sub_// ((source list) (diviseur number)) (loop for x in source collecting (/ x diviseur))) 

(defmethod sub_// ((source number) (diviseur list)) (loop for d in diviseur collecting (/ source d))) 

(defmethod sub_// ((source number) (diviseur number)) (/ source diviseur)) 

(oa::om-with-redefinitions 
(defun ** (source &optional multiplicateur) (if (null multiplicateur) (apply #'* source) (sub_** source multiplicateur)))
)

(defmethod sub_** ((source list) (multiplicateur list)) (loop for x in source and m in multiplicateur collecting (* x m))) 

(defmethod sub_** ((source list) (multiplicateur number)) (loop for x in source collecting (* x multiplicateur))) 

(defmethod sub_** ((source number) (multiplicateur list)) (loop for m in multiplicateur collecting (* source m))) 

(defmethod sub_** ((source number) (multiplicateur number)) (* source multiplicateur)) 

(oa::om-with-redefinitions 
(defun ++ (source &optional additif) (if (null additif) (apply #'+ source) (sub_++ source additif)))
)

(defmethod sub_++ ((source list) (additif list)) (loop for x in source and a in additif collecting (+ x a))) 

(defmethod sub_++ ((source list) (additif number)) (loop for x in source collecting (+ x additif))) 

(defmethod sub_++ ((source number) (additif list)) (loop for a in additif collecting (+ source a))) 

(defmethod sub_++ ((source number) (additif number)) (+ source additif)) 

(defun abs! (source) (loop for x in source collecting (abs x)))

(defun +c* (source addition minimum maximum)
	(let ((diviseur (1+ (- maximum minimum))))
	(+ (mod (+ (- source minimum) addition) diviseur) minimum)))

(defun min* (&rest source)
	(apply #'min (lister!! source)))

(defun max* (&rest source)
	(apply #'max (lister!! source)))

(defun lim* (source minimum maximum)
	(if (listp source) (loop for s in source collecting (lim* s minimum maximum)) (max minimum (min maximum source))))

(defun itv* (source)
	(loop for x in source and y in (rest source) collecting (- y x)))

(defun itv-1* (source)
	(loop for x in source and y in (rest source) collecting (- y x 1)))

(defun pts* (source depart)
	(cons depart (loop for x in source summing x into output collecting (+ depart output))))

(defun tp?* (source &optional temps_premier)
  (if (nbr? source)
      (multiple-value-list (floor (/ source temps_premier)))
      (let ((multiplicateur (eval `(lcm ,@(loop for x in source collecting (denominator x))))))
	(/ (eval `(gcd ,@(loop for x in source collecting (* x multiplicateur)))) multiplicateur))))

(defun n! (source) (max 0 source))

(defun ent* (source)
  (loop 
     with buffer = 0
     and result = 0
     for x in source
     doing (setf result (floor x))
     doing (setf buffer (+ buffer (mod x 1)))
     when (>= buffer 1) collect (1+ result) and do (setf buffer (1- buffer)) else collect result
       ))

(defun amb* (&rest source)
	(- (max* source) (min* source)))

(defclass discretion ()
  ((discretise 	:initform nil	:accessor discretise	:initarg :discretise	:type t)
   (discretisant	:initform nil	:accessor discretisant	:initarg :discretisant	:type t)
   ))

(defun discretiser (source discretisant)
	(let* (	(discretisant (pts* discretisant 0)) 
		(discretise (pts* source 0)) 
		(fusion (itv* (sort*rd (append discretisant discretise))))
		(matrice_du_discretise (itv* (indexation (pts* fusion 0) discretise 'trb)))
		(matrice_du_discretisant (itv* (indexation (pts* fusion 0) discretisant 'trb))))
	;-------
	(make-instance 'discretion
		:discretise	(grouper fusion matrice_du_discretise)
		:discretisant	(grouper fusion matrice_du_discretisant)
	)))

(defun lister! (source) (if (listp source) source (list source)))

(defun lister!! (&rest source) (sub_lister!! source))

(defun sub_lister!! (source) (loop for x in source when (consp x) append (sub_lister!! x) else collect x))

(defun lister-1 (source) (loop for x in source when (consp x) append x else collect x))

(defun lengths (source) (loop for x in (lister! source) collect (length (lister! x))))

;;;;;A CHANGER !!!!!
(defun trouver+ (source table) (loop for x in table when (equal (first x) source) return x finally return source))

(defun sort* (source &optional rang)
	(let* ((n (if rang (1- (lim* rang 1 (min* (lengths source)))) nil)) (groupe (if rang (loop for x in source collecting (nth n x)) nil)))
	(case rang
		('() (cond
			((nbrl? source) (sub_sort*_nombre source))
			((strl? source) (sub_sort*_chaine_de_caracteres source))
			(t nil)
			))
		(t (cond
			((nbrl? groupe) (sub_sort*_nth_nombre source n))
			((strl? groupe) (sub_sort*_nth_chaine_de_caracteres source n))
			(t nil)
			))
	)))

(defun sort*rd (source &optional rang) (remove-duplicates (sort* source rang)))

(defun sub_sort*_nombre (source) (sort (copy-list source) #'<))

(defun sub_sort*_chaine_de_caracteres (source) (sort (copy-list source) #'string-lessp))

(defun sub_sort*_nth_chaine_de_caracteres (source rang) (sort (copy-list source) #'string-lessp :key #'(lambda (x) (nth rang x))))

(defun sub_sort*_nth_nombre (source rang) (sort (copy-list source) #'< :key #'(lambda (x) (nth rang x))))

(defun indexation (source cle &optional mode)
	(let ((sr source) (cl cle) (md mode))
	;---
	(when (stringp source) (setf sr (chr sr)) (setf cl (chr cl)))
	;---
	(case md
		('() (sub_indexation sr cl))
		(b (sub_indexation_en_batterie sr cl))
		(tr (sub_indexation_tronquee sr cl))
		(trb (sub_indexation_tronquee_en_batterie sr cl))
		(n (sub_indexation_negative sr cl))
		(nb (sub_indexation_negative_en_batterie sr cl))
		(seq (sub_indexation_d_une_sequence sr cl))
		(seqb (sub_indexation_d_une_sequence_en_batterie sr cl)))))

(defun sub_indexation (source cle)
	(loop for x in source and id upfrom 0 when (equal x cle) collect id))

(defun sub_indexation_en_batterie (source cles)
	(loop for c in cles collecting (sub_indexation source c)))

(defun sub_indexation_tronquee (source cle)
	(loop for x in source and id upfrom 0 when (equal x cle) return id))

(defun sub_indexation_tronquee_en_batterie (source cles)
	(loop for c in cles collecting (sub_indexation_tronquee source c)))

(defun sub_indexation_negative (source cle)
	(loop for x in source and id upfrom 0 when (not (equal x cle)) collect id))

(defun sub_indexation_negative_en_batterie (source cles)
	(loop for c in cles collecting (sub_indexation_negative source c)))

(defun sub_indexation_d_une_sequence (source cle)
	(remove nil (if (notnull cle) (loop with i = 0 while (notnull i)
		do (setf i (search cle source :test 'equal :start2 i))
		collect i
		do (setf i (if (notnull i) (1+ i)))))))

(defun sub_indexation_d_une_sequence_en_batterie (source cles)
	(loop for c in cles collecting (sub_indexation_d_une_sequence source c)))

(defmethod sub_grouper ((source list) (matrice list))
	(let ((src (copy-list source)))
	(r/n (loop for x in matrice collecting (loop repeat x while (notnull src) collecting (pop src))))))

(defmethod sub_grouper ((source list) (matrice integer))
	(let ((src (copy-list source)) (mat (if (< matrice 1) 1 matrice)))
	(r/n (loop while (notnull src) collecting (loop repeat mat while (notnull src) collecting (pop src))))))

(defmethod sub_grouper* ((source list) (matrice list))
	(let ((src (copy-list source)))
	(loop for x in matrice collecting (loop repeat x collecting (pop src)))))

(defmethod sub_grouper* ((source list) (matrice integer))
	(let ((src (copy-list source)) (mat (if (< matrice 1) 1 matrice)))
	(loop while (notnull src) collecting (loop repeat mat collecting (pop src)))))

(defun sub_grouper_si_identiques (source)
	(grouper source (lengths (couper (append 
		(list 'n)
		(loop for x in source and y in (rest source) when (equal x y) collect '- else collect 'n))
		'n 'c))))

(defun sub_grouper_si_identiques_selon_rang (source rang)
  (let* ((n (1- (lim* (nbr rang) 1 (min* (lengths source))))) (groupe (loop for x in source collecting (nth n x))))
    (grouper source (lengths (grouper groupe)))))

(defun grouper (source &optional matrice mode)
	(let ((src (copy-list source)) (mat matrice) (md mode))
	;---
	(cond
		((and (null mat) (null md)) (sub_grouper_si_identiques src))
		((nbrOrd? mat) (sub_grouper_si_identiques_selon_rang src mat))
		((and (notnull mat) (null md)) (sub_grouper src mat))
		((equal md '*) (sub_grouper* src mat))
	)))

(defun grouper* (source matrice) (grouper source matrice '*))

(defun lister* (source dimension)
  (loop for x in (sub_lister source (max dimension 0)) when (listp x) append x else append (list x)))

(defun lister (source dimension &optional mode)
  (let ((sr source) (dm dimension) (md mode))
	;---
    (case md
      ('() (sub_lister sr dm))
      (b (sub_lister_en_batterie sr dm))
      (b* (sub_lister_en_batterie* sr dm))
      (b+ (sub_lister_en_batterie+ sr dm))
      (b+* (sub_lister_en_batterie+* sr dm))
      (m (sub_lister_selon_une_matrice sr dm))
      (m* (sub_lister_selon_une_matrice* sr dm))
      (m+ (sub_lister_selon_une_matrice+ sr dm))
      (m+* (sub_lister_selon_une_matrice+* sr dm))
      (d (sub_lister_selon_des_dimensions sr dm))
      (d* (sub_lister_selon_des_dimensions* sr dm))
      (d+ (sub_lister_selon_des_dimensions+ sr dm))
      (d+* (sub_lister_selon_des_dimensions+* sr dm))
      )))

(defun sub_lister (source dimension)
  (make-list (max dimension 0) :initial-element source))

(defun sub_lister_en_batterie (sources dimensions)
  (loop for x in sources and d in dimensions collecting (sub_lister x d)))

(defun sub_lister_en_batterie* (sources dimensions)
  (loop for x in sources and d in dimensions collecting (lister* x d)))

(defun sub_lister_en_batterie+ (sources dimensions)
  (loop for x in sources and d in dimensions appending (sub_lister x d)))

(defun sub_lister_en_batterie+* (sources dimensions)
  (loop for x in sources and d in dimensions appending (lister* x d)))

(defun sub_lister_selon_une_matrice (source matrice)
  (sub_lister_en_batterie source (lengths matrice)))

(defun sub_lister_selon_une_matrice* (source matrice)
  (sub_lister_en_batterie* source (lengths matrice)))

(defun sub_lister_selon_une_matrice+ (source matrice)
  (sub_lister_en_batterie+ source (lengths matrice)))

(defun sub_lister_selon_une_matrice+* (source matrice)
  (sub_lister_en_batterie+* source (lengths matrice)))

(defun sub_lister_selon_des_dimensions (source matrice)
  (sub_lister_en_batterie source matrice))

(defun sub_lister_selon_des_dimensions* (source matrice)
  (sub_lister_en_batterie* source matrice))

(defun sub_lister_selon_des_dimensions+ (source matrice)
  (sub_lister_en_batterie+ source matrice))

(defun sub_lister_selon_des_dimensions+* (source matrice)
  (sub_lister_en_batterie+* source matrice))




;          |-------------------------------------------------------------------------------------|
;          |                                   TRADUTEUR XML                                     | 
;          |-------------------------------------------------------------------------------------|



(setf *r (list #\Newline))

(setf *g (list #\"))

(setf *t (list #\Tab))

;--------------------------------------------------------

;alterations
;1) generales
;2) octaves specifiees
;3) contextuelles


;;By Karim Haddad

(defun mycassq (sym l)
  (cdr (assoc sym l)))

(defun append-str (list)                
  (let ((str ""))
    (loop for s in list do
          (setf str (concatenate 'string str s)))
    str))


(defvar *no-sharp-read-table*)

(defparameter *kascii-note-c-scale*
  (mapc #'(lambda (x) (setf (car x) (string-upcase (string (car x)))))
    '((c . :n) (c . :h) (c . :q) (c . :hq) (c . :s) (c . :hs) (c . :qs) (c . :hqs)
      (d . :n) (d . :h) (d . :q) (d . :hq) (d . :s) (d . :hs) (d . :qs) (d . :hqs)
      (e . :n) (e . :q) (e . :q) (e . :hq)
      (f . :n) (f . :q) (f . :s) (f . :qs) (f . :s) (f . :hs) (f . :qs) (f . :hqs)
      (g . :n) (g . :q) (g . :s) (g . :qs) (g . :s) (g . :hs) (g . :qs) (g . :hqs)
      (a . :n) (a . :q) (a . :s) (a . :qs) (a . :s) (a . :hs) (a . :qs) (a . :hqs)
      (b . :n) (b . :q) (b . :q) (b . :hq) )))


(defparameter *kascii-note-alterations*
   '((:s 1 +100) (:f -1 -100)
     (:q 0.5 +50) (:qs 1.5 +150) (:-q -0.5 -50) (:f-q -1.5 -150)
     (:h 0.25 +25) (:hq 0.75 +75) (:hs 1.25 +125) (:hqs 1.75 +175) (:-h -0.25 -25) (:-hq -0.75 -75)(:-hs -1.25 -125)(:-hqs -1.75 -175)
     (:n 0 0)))

(defparameter *kascii-note-scales* (list *kascii-note-C-scale*))

(setf *note-accidentals*
      '((0.25 natural-up)
        (0.5 quarter-sharp) 
        (0.75 sharp-down)
        (1 sharp)
        (1.25 sharp-up)
        (1.5 three-quarters-sharp)
        (1.75 sharp-three)
        ))

; (mycassq 1 *note-accidentals*)
; (mc->xmlnotes 6125 8)
; (cons-xml-note (make-instance 'om::note))

(defun mc->xmlnotes (midic &optional (approx 2))
  "Converts <midic> to a string representing a symbolic ascii note."
  (let* ((kascii-note-scale (car *kascii-note-scales*))
         (dmidic (/ 1200 (length kascii-note-scale))) 
         note)
    (let* ((values (multiple-value-list (round (om::approx-m midic approx) dmidic)))
           (midic/50 (car values))
           (cents (cadr values))
           (values2 (multiple-value-list (floor (* midic/50 dmidic) 1200)))
           (oct+2 (- (car values2) 1))
           (midic<1200 (cadr values2)))

      (setq note (nth (/ midic<1200 dmidic) kascii-note-scale))
      (list   midic
              (coerce (car note) 'character) 
              (car (mycassq (cdr note) *kascii-note-alterations*))
              oct+2))))

(defun hauteur (midicent &optional (approx 2))
  (let* ((source (mc->xmlnotes midicent approx))
         (mc (first source)) (s (second source)) 
         (a (third source)) 
         (o (fourth source)))
    (if (null a)
        (append *t *t *t *t (chr "<pitch>") *r
                *t *t *t *t *t (chr "<step>") (list s) (chr "</step>") *r
                *t *t *t *t *t (chr "<octave>") (lister! (chr o)) (chr "</octave>") *r
                *t *t *t *t (chr "</pitch>") *r
		)
      (append *t *t *t *t (chr "<pitch>") *r
              *t *t *t *t *t (chr "<step>") (list s) (chr "</step>") *r
              *t *t *t *t *t (chr "<alter>") (lister! (chr a)) (chr "</alter>") *r
              *t *t *t *t *t (chr "<octave>") (lister! (chr o)) (chr "</octave>") *r
              *t *t *t *t (chr "</pitch>") *r
              ))))


(defun duree (duree)
  (append *t *t *t *t (chr "<duration>") (lister! (chr duree)) (chr "</duration>") *r))

(defun type-dur (fig)
               (append  *t *t *t *t (chr "<type>") (lister! (chr fig)) (chr "</type>") *r))

(defun accidental (fig)
               (append  *t *t *t *t (chr "<accidental>") (lister! (chr fig)) (chr "</accidental>") *r))

(defun liaison (liaison)
	(let ((start (append *t *t *t *t (chr "<tie type=") *g (chr "start") *g (chr "/>") *r))
	(stop (append *t *t *t *t (chr "<tie type=") *g (chr "stop") *g (chr "/>") *r))
	(stop-start (append 
			*t *t *t *t (chr "<tie type=") *g (chr "stop") *g (chr "/>") *r
			*t *t *t *t (chr "<tie type=") *g (chr "start") *g (chr "/>") *r)))
	;-------
	(case liaison
		(s start)
		(o stop)
		(os stop-start))))



;          |-------------------------------------------------------------------------------------|
;          |                                   INTERFACE OM                                      | 
;          |-------------------------------------------------------------------------------------|


;;By Karim Haddad

;;;;;;;;;;;;;;tools


(defparameter *note-types*
  '((2 breve) (1 whole)
    (1/2 half) (1/4 quarter)
    (1/8 eighth) (1/16 16th)
    (1/32 32nd) (1/62 64th)
    (1/128 128th) (1/256 256th)
    (1/512 512th)(1/1024 1024th)))
  
(defmethod in-group?  ((self om::chord)) 
  (om::group-p (om::parent self)))
(defmethod in-group?  ((self om::rest)) 
  (om::group-p (om::parent self)))
(defmethod in-group?  ((self t)) 
  nil)

(defmethod alone-in-group?  ((self om::chord)) 
  (if (= (length (om::inside (om::parent self))) 1)
    t))

(defmethod alone-in-group? ((self om::rest)) 
  (if (= (length (om::inside (om::parent self))) 1)
    t))

(defmethod alone-in-group?  ((self t)) 
  nil)


(defun tied (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         (str (liaison 's)))
        ((and (om::cont-chord-p self)
             (not (om::cont-chord-p (om::next-container self '(om::chord)))))
        (str (liaison 'o)))
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
        (str (liaison 'os)))
       (""))) ;;;;the nil thing is comin from here


(defun group (num denom)
  (append *t *t *t (chr "<time-modification>") *r
          *t *t *t *t (chr "<actual-notes>") (lister! (chr num))(chr "</actual-notes>") *r
          *t *t *t *t (chr "<normal-notes>") (lister! (chr denom)) (chr "</normal-notes>") *r
          *t *t *t (chr "</time-modification>") *r))

(defun firstofgroup (num denom notetype nbr)
  (append 
   *t *t *t *t (chr "<tuplet bracket=") *g (chr "yes") *g  
   (chr " number=") *g (lister! (chr nbr)) *g 
   (chr " placement=") *g (chr "above") *g (chr " type=") *g (chr "start") *g (lister! (chr ">")) *r
   *t *t *t *t (chr "<tuplet-actual>") *r 
   *t *t *t *t *t (chr "<tuplet-number>") (lister! (chr num)) (chr "</tuplet-number>") *r 
   *t *t *t *t *t (chr "<tuplet-type>") (lister! (chr notetype)) (chr "</tuplet-type>") *r
   *t *t *t *t (chr "</tuplet-actual>") *r 
   *t *t *t *t (chr "<tuplet-normal>") *r 
   *t *t *t *t *t (chr "<tuplet-number>") (lister! (chr denom)) (chr "</tuplet-number>") *r 
   *t *t *t *t *t (chr "<tuplet-type>") (lister! (chr notetype)) (chr "</tuplet-type>") *r
   *t *t *t *t (chr "</tuplet-normal>") *r 
   *t *t *t *t (chr "</tuplet>") *r
   ))



(defun lastofgroup (nbr)
  (append 
   *t *t *t *t (chr "<tuplet") (chr " number=") *g (lister! (chr nbr)) *g
   (chr " type=") *g (chr "stop") *g (chr "/>") *r
   ))
     
(defmethod getratiogroup ((self om::measure))
(list 1 1))

(defmethod getratiogroup ((self om::group))
  (let* ((tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (dur (* symb-beat-val factor))
         (durtot (if (listp dur) (car dur) dur))
         (cpt (if (listp dur) (cadr dur) 0))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (sympli (/ num denom)))
    (list num denom (str (car (mycassq unite *note-types*))) )))


(defun get-grp-level (self)
  "donne l'index des tuplet imbriques"
  (let* ((buf (om::parent self))
         (num (car (getratiogroup buf)))
         (denom (second (getratiogroup buf)))
         (nums (list num))
         (denoms (list denom))
         (index 0)) 
    (loop 
      while (om::group-p buf)
      do (progn 
           (incf index)
           (setf buf (om::parent buf))
           (push (car (getratiogroup buf)) nums)
           (push (second (getratiogroup buf)) denoms)
           (setf num (* num (car (getratiogroup buf))))
           (setf denom (* denom (second (getratiogroup buf))))))
    (list index num denom (butlast (reverse nums)) (butlast (reverse denoms)))))


(defun getallgroups (self)
  (let* ((buf (om::parent self))
        (rep '()))
  (loop 
    while (om::group-p buf)
           do (progn 
                (push (getratiogroup buf) rep)
                (setf buf (om::parent buf))))
  rep ))



(defun timemod (self)
  (if (and (in-group? self) (not (alone-in-group? self)))
    (let* ((lvl (get-grp-level self))
           (ratio (getratiogroup (om::parent self)))
           (act-note (second lvl))
           (norm-note (third lvl))
           (indx (car lvl))
           (numdenom (getallgroups self))
           (simpli (/  act-note norm-note)))
      
      (if (not (= (/ act-note norm-note) 1))
        (str (group act-note norm-note))
        ""))
      ""
      ))


(defun first-of-this-group (self grp)
       (let ((frst (car (om::collect-chords grp))))
         (equal self frst)))

(defun last-of-this-group (self grp)
       (let ((lst (car (reverse (om::collect-chords grp)))))
         (equal self lst)))


(defun liaison-notation (liaison)
  (let ((start (append *t *t *t *t (chr "<tied type=") *g (chr "start") *g (chr "/>") *r))
	(stop (append *t *t *t *t (chr "<tied type=") *g (chr "stop") *g (chr "/>") *r))
	(stop-start (append 
                     *t *t *t *t (chr "<tied type=") *g (chr "stop") *g (chr "/>") *r
                     *t *t *t *t (chr "<tied type=") *g (chr "start") *g (chr "/>") *r)))
	;-------
    (case liaison
      (s start)
      (o stop)
      (os stop-start))))

(defun tied? (self)
  (or (and (not (om::cont-chord-p self))
           (om::cont-chord-p (om::next-container self '(om::chord))))
      (and (om::cont-chord-p self)
           (not (om::cont-chord-p (om::next-container self '(om::chord)))))
      (and (om::cont-chord-p self)
           (om::cont-chord-p (om::next-container self '(om::chord))))))

(defun tied-notation (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         (str (liaison-notation 's)))
        ((and (om::cont-chord-p self)
             (not (om::cont-chord-p (om::next-container self '(om::chord)))))
        (str (liaison-notation 'o)))
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
        (str (liaison-notation 'os)))
       (""))) ;;;;the nil thing is comin from here


;; disons que tous les char sont des accents :)
(defun accent? (self) (om::get-extras self "char"))

(defun accent-notation (self)
  (if (accent? self)
    (append *t *t *t *t *t (chr "<articulations>") *r 
            *t *t *t *t *t *t (chr "<accent default-x=\"-1\" default-y=\"-61\" placement=\"below\"/>") 
            *t *t *t *t *t (chr "</articulations>") *r)
    '("")))

(defun groupnotation (self)
  (apply 'om::string+
         (if (in-group? self)
             (let* ((lvl (get-grp-level self))
                    (ratio (getratiogroup (om::parent self)))
                    (act-note (second lvl))
                    (norm-note (third lvl))
                    (indx (car lvl))
                    (numdeno (getallgroups self))
                    (numdenom (remove nil (loop for i in numdeno
                                                collect (if (not (= 1 (/ (car i) (second i)))) i )   ;;; PB if group (n n) !!!
                                                )))
                    (simpli (/ act-note norm-note))
                    (strg nil))
               
               (if (not (= (/ act-note norm-note) 1))
        
                   (cond 
                    ((and (om::last-of-group? self) (om::first-of-group? self))
                     (if (accent? self)
                         (list (str 
                                (append (append *t *t *t (chr "<notations>") *r)
                                        (accent-notation self)  
                                        (append *t *t *t (chr "</notations>") *r)))
                               )
                       (list "")))
             
                    ((and (om::first-of-group? self)  (not (om::last-of-group? self)))
                     (progn
                       (setf strg (append strg (list (str (append *t *t *t (chr "<notations>") *r)))))
                       (setf strg (append strg (list (str
                                                      (let ((obj self)
                                                            (indx (+ (length numdenom) 1)))
                                                        (remove nil (om::flat (loop for i in (reverse numdenom)           
                                                                                    collect (progn 
                                                                                              (setf obj (om::parent obj))
                                                                                              (setf indx (- indx 1))
                                                                                 
                                                                                              (if (first-of-this-group self obj)
                                                                        
                                                                                                  (firstofgroup (car i) (second i) (third i) indx)
                                                                                                )
                                                                                              )))))
                                                      ))))
                       (setf strg (append strg (list (str (tied-notation self)))))
                       (if (accent? self) (setf strg (append strg (list (str (accent-notation self))))))
                       (setf strg (append strg (list (str (append *t *t *t (chr "</notations>") *r)))))
                       ))



                    ((and (om::last-of-group? self) (not (om::first-of-group? self)))
                     
                     (progn
                 
                       (setf strg (append strg (list (str (append *t *t *t (chr "<notations>") *r)))))
                       (setf strg (append strg (list (str 
                                                      (let ((obj self)
                                                            (indx (+ (length numdenom) 1)))
                                                        (remove nil (om::flat (loop for i in numdenom           
                                                                                    collect (progn 
                                                                                              (setf obj (om::parent obj))
                                                                                              (setf indx (- indx 1))
                                                                                              (if (last-of-this-group self obj)
                                                                                                  (lastofgroup indx)))))))
                                                      ))))
                       (setf strg (append strg (list (str (tied-notation self)))))
                       (setf strg (append strg (list (str (accent-notation self)))))
                       (setf strg (append strg (list (str (append *t *t *t (chr "</notations>") *r)))))
                       ))

       

                    (t ;(list "")
                     (if (accent? self)
                         (list (str 
                                (append (append *t *t *t (chr "<notations>") *r)
                                        (accent-notation self)  
                                        (append *t *t *t (chr "</notations>") *r)))
                               )
                       (list ""))
                     ))

        
                 (when (or (tied? self) (accent? self))
                   (setf strg (append strg (list (str (append *t *t *t (chr "<notations>") *r)))))
                   (setf strg (append strg (list (str (tied-notation self)))))
                   (setf strg (append strg (list (str (accent-notation self)))))
                   (setf strg (append strg (list (str (append *t *t *t (chr "</notations>") *r)))))
                   )))


           (let* ((strg nil))
             (when (or (tied? self) (accent? self))
               (setf strg (append strg (list (str (append *t *t *t (chr "<notations>") *r)))))
               (setf strg (append strg (list (str (tied-notation self)))))
               (setf strg (append strg (list (str (accent-notation self)))))
               (setf strg (append strg (list (str (append *t *t *t (chr "</notations>") *r)))))
               ))


           )))


(defun get-parent-measure (self)
  "Donne la mesure liee a l'obj chord par exemple"
  (let ((obj (om::parent self)))
  (loop 
    while (not (om::measure-p obj))
    do (setf obj (om::parent obj)))
  obj))


(defmethod donne-figure ((self om::chord))
  (let* ((mesure (get-parent-measure self))
         (inside (om::inside mesure))
         (tree (om::tree mesure))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (stem (om::extent self))
         (obj self))
    
     (loop 
      while (not (om::measure-p obj))
      do (progn 
           (setf stem (* stem (om::extent (om::parent obj))))
           (setf obj (om::parent obj)))) 
   (let ((numbeams  (om::get-number-of-beams (* symb-beat-val factor))))
     (if (listp numbeams) (car numbeams) numbeams ))))


(defmethod donne-figure ((self om::rest))
  (let* ((mesure (get-parent-measure self))
         (inside (om::inside mesure))
         (tree (om::tree mesure))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (stem (om::extent self))
         (obj self))
    
     (loop 
      while (not (om::measure-p obj))
      do (progn 
           (setf stem (* stem (om::extent (om::parent obj))))
           (setf obj (om::parent obj)))) 
   (let ((numbeams  (om::get-number-of-beams (* symb-beat-val factor))))
     (if (listp numbeams) (car numbeams) numbeams ))))

(defmethod donne-figure ((self t))
  0)


(defun beam (state)
  (str ;; jb: added str 
   (append *t *t *t *t (chr "<beam>")(lister! (chr state))(chr "</beam>") *r))
 )


;;; j bresson 02/09/2010
;;; replaced "start" with "begin"
;;;;K haddad 22/06/2014
;;;Cette fonction donne des nil .... Elle n'est pas modifiee
;;;Par contre dans le corps du code (voir plus loin) on enleve les nils
(defun makebeam (self)
  (let* ((beamself (donne-figure self))
         (beamprev (donne-figure (prv-cont self)))
         (beamnext (donne-figure (nxt-cont self))))
    
    (cond ((and (in-group? self)
                (not (om::first-of-group? self))
                (not (om::last-of-group? self))
                (> beamself 0))
           (cond ((and (> beamprev 0) (> beamnext 0))
                  (beam "continue"))
                 ((and (> beamprev 0) (not (> beamnext 0)))
                  (beam "end"))
                 ((and (not (> beamprev 0)) (> beamnext 0))
                  (beam "begin"))))
          
          ((and (om::first-of-group? self) (> beamself 0))
           (if (and (in-group? (prv-cont self))
                    (> beamprev 0) (> beamnext 0) (prv-is-samegrp? self))
             (beam "continue") (beam "begin")))
          ((and (om::last-of-group? self) (> beamself 0))
           (if (and (in-group? (nxt-cont self))
                    (> beamprev 0) (> beamnext 0) (nxt-is-samegrp? self))
             (beam "continue") (beam "end")))
          
          (t ""))  ;;;; K.H => Le probleme du nil vient de la !
    ))

(defun prv-cont (self)
  (om::previous-container self '(om::chord om::rest)))

(defun nxt-cont (self)
  (om::next-container self '(om::chord om::rest)))


(defun prv-is-samegrp? (self)
  (let ((prev (prv-cont self)))
    (equal (om::parent self) (om::parent prev))))

(defun nxt-is-samegrp? (self)
  (let ((next (nxt-cont self)))
    (equal (om::parent self) (om::parent next))))



(defun notetype (val)
  (cond 
   ((>= val 2) 2)
   ((>= val 1/2) 1/2)
   ((>= val 1/4) 1/4)
   ((>= val 1/8) 1/8)
   ((>= val 1/16) 1/16)
   ((>= val 1/32) 1/32)
   ((>= val 1/64) 1/64)
   ((>= val 1/128) 1/128)
   ((>= val 1/256) 1/256)))



(defun note-strict-lp (val)
  (cond
   ((>= val 16) (car (om::before&after-bin val)))
   ((= val 8) 8)
   ((= val 4) 4)
   ((= val 2) 2)
   (t (denominator val))))

(defun get-head-and-points (val)
  (let* ((haut (numerator val))
         (bas (denominator val))
         (bef (car (om::before&after-bin haut)))
         (points 0) (char 1))
    (cond
     ((= bef haut)
      (setf char (note-strict-lp (/ haut bas)))
      (setf points 0))
     ((= (* bef 1.5) haut)
      (setf char (note-strict-lp (/ bef bas)))
      (setf points 1))
     ((= (* bef 1.75) haut)
      (setf char (note-strict-lp (/ bef bas)))
      (setf points 2)))

   ; (list (/ 1 char  ) points)
    (if (> val 1) 
        (list (/ char 1) points)
      (list (/ 1 char) points))
    ))


(defun text-extras-as-xml (self) 
  (when (om::get-extras self "text")
    (append *t *t *t *t (chr "<lyric default-y=\"-80\" justify=\"left\" number=\"1\">") *r 
                    *t *t *t *t *t (chr "<syllabic>single</syllabic>") *r 
                    *t *t *t *t *t 
                    (chr "<text>") 
                    (list (om::thetext (car (om::get-extras self "text"))))
                    (chr "</text>") *r 
                    *t *t *t *t *t (chr "<extend type=\"start\"/>") *r 
                    *t *t *t *t (chr "</lyric>") *r
                    )))
   


(defmethod cons-xml-expr ((self om::chord) &key free key (approx 2))
  
  (let* ((figure (if (listp free) (car free) free))
         (hd-and-pts (get-head-and-points figure))
         (truefigure (car hd-and-pts))
         ;(truefigure figure)
         (points (second hd-and-pts))
         (figchar (str (car (mycassq truefigure *note-types*))))
         (durtot (* (/ (om::extent self) 4) (/ 1 (om::qvalue self))))
         (inside (om::inside self))
         (strg nil))

    (if (= (length inside) 1)
        (progn
          (setf strg (append strg (list (str (append *t *t *t (chr "<note>") *r)))))	
          (setf strg (append strg (list (cons-xml-note (car inside) approx))))
          (setf strg (append strg (list (str (duree durtot)))))
          (setf strg (append strg (list (tied self))))
        
          (setf strg (append strg (list (str (type-dur figchar)))))

          (if (not (= 0 points))
              (loop for i from 1 to points
                    do 
                    (setf strg (append strg (list (str (append *t *t *t (chr "<dot/>") *r))))))
            "")

          (let ((toto (mycassq (third (mc->xmlnotes (om::midic (car inside)) approx)) *note-accidentals*)))
            (unless (null toto)
              (setf strg (append strg 
                                 (list (str (str (accidental (car (mycassq (third (mc->xmlnotes (om::midic (car inside)) approx)) *note-accidentals*))))))))
              ))

          (setf strg (append strg (list (timemod self))))
          (setf strg (append strg (remove nil (list (makebeam self)))))
          (setf strg (append strg (list (groupnotation self))))
          
          ;;; text extras
          (setf strg (append strg (text-extras-as-xml self)))
          
          (setf strg (append strg (list (str (append *t *t *t (chr "</note>") *r)))))
          )
      
      (progn
        (let ((frst (car inside))) ;;;premiere note de l'accord
          (setf strg (append strg (list (str (append *t *t *t (chr "<note>") *r)))))
          (setf strg (append strg (list (cons-xml-note frst approx))))
          (setf strg (append strg (list (str (duree durtot)))))
          (setf strg (append strg (list (tied self))))
          
          (setf strg (append strg (list (str (type-dur figchar)))))

          (if (not (= 0 points))
              (loop for i from 1 to points
                    do 
                    (setf strg (append strg (list (str (append *t *t *t (chr "<dot/>") *r))))))
            "")
          (let ((toto (mycassq (third (mc->xmlnotes (om::midic frst) approx)) *note-accidentals*)))
            (if (not (null toto))
                (setf strg (append strg (list (str (str (accidental (car (mycassq (third (mc->xmlnotes (om::midic frst) approx)) *note-accidentals*))))))))))
          (setf strg (append strg (list (timemod self))))
          (setf strg (append strg (remove nil (list (makebeam self)))))
          (setf strg (append strg (list (groupnotation self))))
          
          ;;; text extras
          (setf strg (append strg (text-extras-as-xml self)))
          
          (setf strg (append strg (list (str (append *t *t *t (chr "</note>") *r))))))
        (loop for note in (cdr inside) ;;;;les autres notes de l'accord
              do (progn 
                   (setf strg (append strg (list (str (append *t *t *t (chr "<note>") *r)))))
                   (setf strg (append strg (list (str (append *t *t *t (chr "  <chord/>") *r)))))
                   (setf strg (append strg (list (cons-xml-note note approx))))
                   (setf strg (append strg (list (str (duree durtot)))))
                   (setf strg (append strg (list (tied self))))
                   (setf strg (append strg (list (str (type-dur figchar)))))

                   (if (not (= 0 points))
                       (loop for i from 1 to points
                             do 
                             (setf strg (append strg (list (str (append *t *t *t (chr "<dot/>") *r))))))
                     "")

                   (let ((toto (mycassq (third (mc->xmlnotes (om::midic note) approx)) *note-accidentals*)))
                     (if (not (null toto))
                         (setf strg (append strg (list (str (str (accidental (car (mycassq (third (mc->xmlnotes (om::midic note) approx)) *note-accidentals*))))))))))
                   (setf strg (append strg (list (timemod self))))
                   (setf strg (append strg (remove nil (list (makebeam self)))));;??
                   (setf strg (append strg (list (groupnotation self))));;??
                   (setf strg (append strg (list (str (append *t *t *t (chr "</note>") *r))))))
              ))
      )
    strg))


(defmethod cons-xml-expr ((self om::rest) &key free key (approx 2))
  (let* ((figure (if (listp free) (car free) free))
         (hd-and-pts (get-head-and-points figure))
         (truefigure (car hd-and-pts))
         ;(truefigure figure)
         (points (second hd-and-pts))
         (figchar (str (car (mycassq truefigure *note-types*))))
         (durtot (* (/ (om::extent self) 4) (/ 1 (om::qvalue self))))
         ;(durtot (abs (/ 1 (om::qvalue self))))
         (strg nil)) 
    (setf strg (append strg (list (str (append *t *t *t (chr "<note>") *r)))))
    (setf strg (append strg (list (str (append *t *t *t (chr "  <rest/>") *r)))))
    (setf strg (append strg (list (str (duree durtot)))))
    (setf strg (append strg (list (str (type-dur figchar)))))
    (if (not (= 0 points))
          (loop for i from 1 to points
              do 
              (setf strg (append strg (list (str (append *t *t *t (chr "<dot/>") *r))))))
          "")
    (setf strg (append strg (list (timemod self))))
    (setf strg (append strg (remove nil (list (makebeam self)))))
    (setf strg (append strg (list (groupnotation self))))
    (setf strg (append strg (list (str (append *t *t *t (chr "</note>") *r)))))
    strg
    ))


(defmethod cons-xml-note ((self om::note) &optional (approx 2))
  (str (hauteur (om::midic self) approx)))



;;;===================================
;;; RECURSIVE CONTAINERS (JB 29/09/15)
;;;===================================

(defmethod cons-xml-expr ((self om::group) &key free key (approx 2))
  (let* ((inside (om::inside self))
         (durtot free)
         (cpt (if (listp free) (cadr free) 0))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (cadr denom) denom))
         (unite (/ durtot denom)))
    (cond
     ((not (om::get-group-ratio self))
      (loop for obj in inside collect 
            (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                               (/ (om::extent self) (om::qvalue self)))))
              (cons-xml-expr obj :free (* dur-obj durtot)))))
     (t (loop for obj in inside collect
              (cons-xml-expr obj :free (* num unite)))   ;;;; ACHTUNG !!
        ))))


;;;; divisions problem....
;;;finale's value to be tested on Sibelius....
;;;sibelius ' value is 256....
;(defun divisions (divisions)
;	(append *t *t *t *t (chr "<divisions>") (lister! (chr 768)) (chr "</divisions>") *r))


(defmethod cons-xml-expr ((self om::measure) &key free (key '(G 2)) (approx 2))
  (let* ((mesnum free) 
         (inside (om::inside self))
         (tree (om::tree self))
         (signature (car tree))
         (real-beat-val (/ 1 (om::fdenominator signature)))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator signature)))))
    (list (format nil "<measure number=\"~D\">" mesnum)
          (append (remove nil
                          (list "<attributes>"
                                (list (format nil "<divisions>~A</divisions>" 768)  ;;; (caar (dursdivisions self)))
                                      "<key>"
                                      (remove nil
                                              (list  "<fifths>0</fifths>"
                                                     (if (and approx (= approx 4)) "<mode>major</mode>")))
                                      "</key>"
                                      "<time>"
                                      (list (format nil "<beats>~D</beats>" (car signature))
                                            (format nil "<beat-type>~D</beat-type>" (cadr signature)))
                                      "</time>")
                                (and key
                                     (list "<clef>"
                                           (list (format nil "<sign>~D</sign>" (car key))
                                                 (format nil "<line>~D</line>" (cadr key)))
                                           "</clef>"
                                           ))
                                "</attributes>"))
                  (loop for obj in inside ;for fig in (cadr (dursdivisions self))  ;;;;;transmetre les note-types
                        append
                        (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                               (factor (/ (* 1/4 dur-obj-noire) real-beat-val))) 
                          (cons-xml-expr obj :free (* symb-beat-val factor) :approx approx) ;;; NOTE: KEY STOPS PROPAGATING HERE
                          )))
          "</measure>"
          "<!--=======================================================-->")))


(defmethod cons-xml-expr ((self om::voice) &key free (key '(G 2)) (approx 2))
  (let ((voicenum free)
        (measures (om::inside self)))
    (list (format nil "<part id=\"P~D\">" voicenum)
          (loop for mes in measures
                for i = 1 then (+ i 1)
                collect (cons-xml-expr mes :free i :key key :approx approx))
          "<!--=======================================================-->"
          "</part>")))

(defun mxml-header () 
  (list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 1.1 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">"))

(defmethod cons-xml-expr ((self om::poly) &key free (key '((G 2))) (approx 2))
  (let ((voices (om::inside self)))
    (list "<score-partwise>"
          (list "<identification>"
                (list "<encoding>"
                      (list (concatenate 'string "<software>" "OM " om::*version-string*"</software>"))
                      "</encoding>")
                "</identification>")
          (list "<part-list>"
                (loop for v in voices 
                      for voice-num = 1 then (+ voice-num 1)
                      append 
                      (list (format nil "<score-part id=\"P~D\">" voice-num)
                            (list "<part-name>MusicXML Part</part-name>")
                            (list (format nil "<score-instrument id=\"P~D-I~D\">" voice-num voice-num)
                                  (list "<instrument-name>Grand Piano</instrument-name>")
                                  "</score-instrument>")
                            (list (format nil "<midi-instrument id=\"P~D-I~D\">" voice-num voice-num)
                                  (list "<midi-channel>1</midi-channel>"
                                        "<midi-program>1</midi-program>")
                                  "</midi-instrument>")
                            "</score-part>"))
                "</part-list>")
          "<!--===================================================================-->"
          (if (= 1 (length key))
              ;;; SAME KEY FOR ALL VOICES
              (loop for v in voices
                    for i = 1 then (+ i 1) append 
                    (cons-xml-expr v :free i :key (car key) :approx approx))
            ;;; EACH VOICE HAS A KEY
            (loop for v in voices 
                  for i = 1 then (+ i 1)
                  for k in key append
                  (cons-xml-expr v :free i :key k :approx approx)))
          "</score-partwise>")))
  
;;;===================================
;;; OM INTERFACE / API
;;;===================================

(in-package :om)

(defun recursive-write-xml (stream text level)
  (if (listp text)
      (loop for elt in text do 
            (recursive-write-xml stream elt (1+ level)))
    (format stream "~A~A~%" (string+ (make-sequence 'string level :initial-element #\Tab)) text)))
    
(defun write-xml-file (list path)
  (let ((pathname (or path (om-choose-new-file-dialog))))
    (when pathname 
      (WITH-OPEN-FILE (out pathname :direction :output 
                           :if-does-not-exist :create :if-exists :supersede)
        (loop for line in (mxml::mxml-header) do (format out "~A~%" line))
        (recursive-write-xml out list -1))
      pathname)))

(defmethod! export-musicxml ((self poly) &optional (keys '((G 2))) (approx 2) (path nil))
  :icon 351
  :indoc '("a VOICE or POLY object" "list of voice keys" "tone subdivision approximation" "a target pathname")
  :initvals '(nil ((G 2)) 2 nil)
  :doc "
Exports <self> to MusicXML format.
"
  (let* ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New XML file"
                                                       :types '("XML Files" "*.xml")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (write-xml-file (mxml::cons-xml-expr self :free 0 :key keys :approx approx) pathname))))


(defmethod! export-musicxml ((self voice) &optional (keys '((G 2))) (approx 2) (path nil))
  (let* ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New XML file"
                                                       :types '("XML Files" "*.xml")))))
    (when pathname 
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (write-xml-file (mxml::cons-xml-expr (make-instance 'poly :voices self) :free 0 :key keys :approx approx) pathname))))


;;;  UTILS 

(defmethod make-empty-voice ((signs list))
  (let ((mesures (loop for i in signs
                       collect (list i '(-1)))))
    (make-instance 'voice :tree (list '? mesures))))

(defmethod normalize-poly ((self poly))
  "Comlpletes the poly in a manner that all voices got the same number of mesures for MusicXML export"
  (let* ((voices (inside self))
         (signs (get-signatures self))
         (lgts (loop for i in signs
                     collect (length i)))
         (maxlgt (list-max lgts))
         (newvoices (loop for i in voices
                          for n in lgts
                          for sg in signs
                          collect (let* ((dif (- maxlgt n))
                                         (comp (last-n sg dif))
                                         (new-vx (make-empty-voice comp)))
                                    (concat i new-vx)))))
    (make-instance 'poly :voices newvoices)
    ))




;;===========================================================
;; NOT USED ANYMORE (?)

#|
(defmethod getdembeams ((self om::measure) lastmes chiffrage)
  (let* ((inside (om::inside self))
         (tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (rep nil))
    
    (loop for obj in inside do
          (setf rep (list rep 
                          (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                                 (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
                                 (exp (getdembeams obj (* symb-beat-val factor) (car (om::tree self)))))
                            exp
                            )
                          )))
    (remove nil (om::flat rep))
    ))


(defmethod getdembeams ((self om::group) dur ratio)
  (let* ((durtot (if (listp dur) (car dur) dur))
         (cpt (if (listp dur) (cadr dur) 0))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (inside (om::inside self))
         (sympli (/ num denom))
         (rep nil) (val nil))
    (cond
     ((not (om::get-group-ratio self)) 
      (loop for obj in inside
            do (setf rep (append rep (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                        (/ (om::extent self) (om::qvalue self)))))
                                       (list (getdembeams obj (* dur-obj durtot) ratio)))))))
     ((= sympli 1)
      (loop for obj in inside
            do (setf rep (list rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                        (/ (om::extent self) (om::qvalue self))))
                                          (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                                 (/ (om::extent self) (om::qvalue self))))))
                                     (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                     (list (getdembeams obj (* dur-obj unite) ratio))))))
      
      )
     
     
     (t
      (let ((pos (length rep))
            (depth 0))
        (loop for obj in inside do
              (setf rep (list rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                       (/ (om::extent self) (om::qvalue self))))
                                         (dur-obj (numerator operation))
                                         exp tmp)
                                    (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                    (setf tmp (multiple-value-list 
                                               (getdembeams obj (list (* dur-obj unite) cpt) ratio))
                                    )
                                    (setf exp (car tmp))
                                    (when (and (cadr tmp) (> (cadr tmp) depth))
                                      (setf depth (cadr tmp)))
                                    exp
                                    ;(list exp)
                                    ))))
        (setf val (+ depth 1))
        
        )
      )
     )
     (values rep val)))


(defmethod getdembeams ((self om::chord) dur ratio)
  (if (listp dur) (car dur) dur))

(defmethod getdembeams ((self om::rest) dur ratio)
  (if (listp dur) (car dur) dur)) 

(defmethod dursdivisions ((self om::measure))
  (let* ((tree (om::tree self))
         (ratios (om::tree2ratio (list '? (om::om-round (list tree)))))
         (note-type (getdembeams self t t))
         (xmltypes (om::flat (loop for i in note-type
                                   collect (mycassq i *note-types*)))))
    (list '(1/4) ratios xmltypes)))

(defun durs-divisions (liste)
  (let* ((res (car liste))
         (pgcd (if (not (= 1 (length liste)))
                 (progn 
                   (loop for i in (cdr liste)
                         do (setf res (om::pgcd res i)))
                   (om::denominator res))
                 (om::denominator res)))
         (durs (om::om* pgcd liste)))
    (list (list pgcd) durs)))

|#
