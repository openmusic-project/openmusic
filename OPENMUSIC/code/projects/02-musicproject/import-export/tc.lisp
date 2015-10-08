

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
