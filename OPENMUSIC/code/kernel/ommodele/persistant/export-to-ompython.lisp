;=========================================================================
;  OpenMusic : export des patchs / maquettes / instances au format JSON
;  (lisible directement en Python avec  json.load(open("patch.omp")) )
;
;  Structure generale d'un fichier exporte :
;   {"version":"8.0", "type":"OMPatch", "name":"...",
;    "boxes":[ {...}, {...} ],
;    "connecs":[ {"boxsource":0,"sourceidx":0,"i":2,"j":1}, ... ],
;    "doc":"...", "libraries":[...], "resources":[...]}
;
;  Conventions Lisp -> JSON :
;   nil            -> null          t              -> true
;   entier/float   -> nombre        ratio (1/3)    -> float
;   string, char   -> "chaine"      symbole/keyword-> "nom-en-minuscules"
;   liste          -> [ ... ]       hash-table     -> { ... }
;   point          -> [h, v]        couleur        -> [r, g, b]
;   objet OM       -> {"class":"chord-seq","slots":{"lmidic":[...], ...}}
;   non convertible-> {"lisp":"<code lisp equivalent>"}
;=========================================================================

(in-package :om)

(defparameter *py-export-version* "8.0")
(defvar *py-export-errors* 0)

(defmacro py-safe (&body body)
  "Evalue BODY, renvoie NIL en cas d'erreur (l'export ne doit jamais planter)."
  `(ignore-errors ,@body))


;==================
; names table
;==================
;;; Permet de renommer des fonctions OM vers leur nom Python :
;;; (def-py-name "om+" "om_plus")
(defparameter *hashnames* (make-hash-table :test 'equalp))

(defun def-py-name (om-name py-name)
  (setf (gethash (string om-name) *hashnames*) py-name))

(defun get-py-name (name)
  (when name
    (let ((str (if (symbolp name) (symbol-name name) (string name))))
      (or (gethash str *hashnames*)
          (string-downcase str)))))

;;; Noms des fonctions (boites OMBoxFun) : le C++ cherche la fonction par son nom Python
;;; (omws->getFun, sans tenir compte des majuscules). Regle par defaut :
;;;   om-random -> om_random    x-append -> x_append    deg->rad -> deg_2_rad    prime? -> prime_p
;;; Les noms qui ne suivent pas cette regle sont dans *py-fun-names* (ci-dessous) :
;;; table separee de *hashnames*, qui sert aussi aux noms des slots et des entrees.
(defparameter *py-fun-names* (make-hash-table :test 'equalp))

(defun def-py-fun-name (om-name py-name)
  (setf (gethash (string om-name) *py-fun-names*) py-name))

;;; classes dont le nom C++ ne suit pas la regle
(def-py-name "n-cercle" "ncercle")

(defun py-replace-all (str old new)
  (with-output-to-string (s)
    (loop with len = (length old)
          for start = 0 then (+ pos len)
          for pos = (search old str :start2 start)
          do (write-string str s :start start :end (or pos (length str)))
          while pos
          do (write-string new s))))

(defun py-default-fun-name (str)
  (let ((s (py-replace-all (py-replace-all (string-downcase str) "->" "_2_") "-" "_")))
    (if (and (> (length s) 1) (char= (char s (1- (length s))) #\?))
        (concatenate 'string (subseq s 0 (1- (length s))) "_p")
      s)))

(defun get-py-fun-name (name)
  (when name
    (let ((str (if (symbolp name) (symbol-name name) (string name))))
      (or (gethash str *py-fun-names*)
          (py-default-fun-name str)))))

(loop for (om-name py-name) in
      '(;; arithmetique
        ("om+" "om_plus") ("om-" "om_minus") ("om*" "om_times") ("om/" "om_division")
        ("om//" "om_div") ("om^" "om_pow")
        ("om-scale^" "om_scale_pow") ("om-scale/sum" "om_scale_sum")
        ;; fonctions Common Lisp utilisees comme boites
        ("sqrt" "om_sqrt") ("sin" "om_sin") ("cos" "om_cos") ("abs" "om_abs")
        ("log" "om_log") ("expt" "om_pow") ("min" "om_min") ("max" "om_max")
        ;; predicats
        ("om<" "om_less") ("om>" "om_greater") ("om<=" "om_leq") ("om>=" "om_geq")
        ("om=" "om_eq") ("om/=" "om_diff")
        ;; series et bpf
        ("x->dx" "x_dx") ("dx->x" "dx_x") ("transfer" "x_transfer")
        ;; 3D
        ("3d-interpol" "interpol_3d") ("3d-sample" "sample_3d") ("3d-spline" "spline_3d")
        ;; noms C++ avec un tiret
        ("repeat-n" "repeat-n") ("osc-send" "osc-send") ("osc-receive" "osc-receive")
        ;; boites de omloop
        ("accumulator" "accum") ("counter" "count") ("minim" "min") ("maxi" "max")
        ("listing" "collect") ("loopdo" "eachTime") ("finaldo" "finally")
        ;; librairie OMTristan (noms Python qui ne suivent pas la regle)
        ("atom!" "atom_") ("list!" "list_") ("remove-dup%" "remove_dup_pc") ("0start" "zero_start")
        ("puiss/to9-ser" "puiss_to9_ser") ("power/2" "power_2") ("power/3" "power_3")
        ("l*line" "l_line") ("l*curb" "l_curb") ("l-distor/2" "l_distor_2") ("l-distor/3" "l_distor_3")
        ("deformer%" "deformer_pc") ("ll-deformer%" "ll_deformer_pc") ("fractal+" "fractal_plus")
        ("l-scale%" "l_scale_pc") ("om-scale/max" "om_scale_max") ("<>" "in_interval") ("><" "out_interval")
        ("x->dx+" "x_dx_plus")
        ;; mathtools
        ("mod+" "mod_plus") ("mod-" "mod_minus") ("mod*" "mod_times"))
      do (def-py-fun-name om-name py-name))

(defun py-ref-name (ref)
  "Nom (string) d'une reference de boite : symbole, classe, fonction, objet..."
  (cond ((null ref) nil)
        ((stringp ref) ref)
        ((symbolp ref) (symbol-name ref))
        ((typep ref 'class) (symbol-name (class-name ref)))
        (t (or (py-safe (let ((n (name ref))) (and (stringp n) n)))
               (princ-to-string ref)))))


;=====================================================
; Primitives JSON (chaque fonction renvoie une STRING JSON)
;=====================================================

(defun json-string (str)
  "Chaine JSON correctement echappee."
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for c across (string str) do
          (cond ((char= c #\") (write-string "\\\"" s))
                ((char= c #\\) (write-string "\\\\" s))
                ((char= c #\Newline) (write-string "\\n" s))
                ((char= c #\Return) (write-string "\\r" s))
                ((char= c #\Tab) (write-string "\\t" s))
                ((< (char-code c) 32) (format s "\\u~4,'0X" (char-code c)))
                (t (write-char c s))))
    (write-char #\" s)))

(defun json-number (n)
  (handler-case
      (cond ((integerp n) (format nil "~D" n))
            ((rationalp n) (json-number (coerce n 'double-float)))
            ((floatp n)
             (if (or (/= n n) (> (abs n) most-positive-double-float))
                 "null"   ; NaN / infini : pas representables en JSON
               (let* ((double? (typep n 'double-float))
                      (x (if double? n (coerce n 'single-float)))
                      (*read-default-float-format* (if double? 'double-float 'single-float)))
                 ;; avec le bon format par defaut, Lisp n'ecrit pas de marqueur "d0"/"f0"
                 (prin1-to-string x))))
            (t (json-string (princ-to-string n))))   ; complexes...
    (error () "null")))

(defun jstr (x)
  (cond ((null x) "null")
        ((stringp x) (json-string x))
        ((symbolp x) (json-string (string-downcase (symbol-name x))))
        (t (json-string (princ-to-string x)))))

(defun jnum (x) (if (numberp x) (json-number x) "null"))

(defun jbool (x) (if x "true" "false"))

(defun json-list (items)
  "ITEMS : liste de strings JSON deja converties."
  (with-output-to-string (s)
    (write-char #\[ s)
    (loop for (item . rest) on items do
          (write-string item s)
          (when rest (write-char #\, s)))
    (write-char #\] s)))

(defun json-obj (&rest plist)
  "PLIST : cle (string) / valeur (string JSON). Une valeur NIL omet la cle."
  (with-output-to-string (s)
    (write-char #\{ s)
    (let ((first t))
      (loop for (key val) on plist by #'cddr
            when val do
            (unless first (write-char #\, s))
            (setf first nil)
            (write-string (json-string key) s)
            (write-char #\: s)
            (write-string val s)))
    (write-char #\} s)))

;;; compatibilite avec l'ancien nom
(defun mk-list_of_strlist_py (list) (json-list list))

(defun lisp-code-string (form)
  "Code Lisp sous forme de texte (pour ce qui n'a pas d'equivalent JSON)."
  (let ((*package* (find-package :om))
        (*print-pretty* nil)
        (*print-readably* nil)
        (*print-length* nil)
        (*print-level* nil))
    (prin1-to-string form)))

(defun json-lisp-fallback (self)
  (json-obj "class" (jstr (py-safe (get-py-name (class-name (class-of self)))))
            "lisp" (jstr (lisp-code-string (or (py-safe (omng-save self t)) self)))))

(defun json-point (p)
  (if p (json-list (list (jnum (om-point-h p)) (jnum (om-point-v p)))) "null"))

(defun json-color (c)
  (if c
      (let ((c (om-correct-color c)))
        (json-list (list (jnum (om-color-r c)) (jnum (om-color-g c)) (jnum (om-color-b c)))))
    "null"))

(defun json-font (f)
  (if f
      (json-obj "face" (jstr (py-safe (om-font-face f)))
                "size" (jnum (py-safe (om-font-size f)))
                "family" (jstr (py-safe (om-font-family f)))
                "style" (omNG-python-save (py-safe (om-font-style f))))
    "null"))


;=====================================================
; VALEURS : omNG-python-save renvoie une string JSON
;=====================================================

(defmethod omNG-python-save ((self null) &optional (values? nil))
  (declare (ignore values?)) "null")

(defmethod omNG-python-save ((self (eql t)) &optional (values? nil))
  (declare (ignore values?)) "true")

(defmethod omNG-python-save ((self symbol) &optional (values? nil))
  (declare (ignore values?)) (jstr self))

(defmethod omNG-python-save ((self number) &optional (values? nil))
  (declare (ignore values?)) (json-number self))

(defmethod omNG-python-save ((self character) &optional (values? nil))
  (declare (ignore values?)) (json-string (string self)))

(defmethod omNG-python-save ((self string) &optional (values? nil))
  (declare (ignore values?)) (json-string self))

(defmethod omNG-python-save ((self pathname) &optional (values? nil))
  (declare (ignore values?)) (json-string (namestring self)))

(defmethod omNG-python-save ((self function) &optional (values? nil))
  (declare (ignore values?)) "null")

(defmethod omNG-python-save ((self vector) &optional (values? nil))
  (json-list (map 'list #'(lambda (x) (omNG-python-save x values?)) self)))

(defmethod omNG-python-save ((self array) &optional (values? nil))
  (declare (ignore values?))
  ;; tableaux multi-dimensionnels : pas d'equivalent direct
  (json-lisp-fallback self))

(defmethod omNG-python-save ((self cons) &optional (values? nil))
  (cond
   ;; (quote x) -> x
   ((and (eq (car self) 'quote) (consp (cdr self)) (null (cddr self)))
    (omNG-python-save (cadr self) values?))
   ;; liste propre
   ((null (cdr (last self)))
    (json-list (mapcar #'(lambda (x) (omNG-python-save x values?)) self)))
   ;; liste pointee (a . b)
   (t (json-obj "lisp" (jstr (lisp-code-string self))))))

(defmethod omNG-python-save ((self hash-table) &optional (values? nil))
  (let (plist)
    (maphash #'(lambda (key val)
                 (push (if (stringp key) key (princ-to-string key)) plist)
                 (push (omNG-python-save val values?) plist))
             self)
    (apply #'json-obj (nreverse plist))))

(defun py-box-like-p (self)
  "Vrai pour un objet graphique de patch (boite) qui n'a pas de methode specifique."
  (py-safe (compute-applicable-methods #'frame-position (list self))))

(defmethod omNG-python-save ((self t) &optional (values? nil))
  (cond
   ((om-color-p self) (json-color self))
   ((om-point-p self) (json-point self))
   ((om-font-p self) (json-font self))
   ((omclass-p (class-of self)) (json-om-object self values?))
   ((py-box-like-p self) (json-unknown-box self))
   (t (json-lisp-fallback self))))

(defun jstr? (x) (when x (jstr x)))

(defun py-is-a (self classname)
  (let ((c (find-class classname nil)))
    (and c (typep self c))))

(defun py-music-object-p (self)
  "Objets de partition : note, chord, chord-seq, voice, measure, poly, multi-seq..."
  (py-is-a self 'simple-container))

(defun py-structured-p (self)
  "Objets dont la sauvegarde JSON est complete (pas besoin du code Lisp en plus)."
  (or (py-music-object-p self) (py-is-a self 'bpf)))

(defun py-slot-value (self slot)
  (let ((sname (name slot)))
    (if (and (string-equal sname "chords")
             (or (py-is-a self 'voice) (py-is-a self 'measure)))
        ;; comme OM 8.0 (save-exepcion de voice/measure) : les vrais accords, grace notes comprises
        (or (py-safe (convert-chord-graces self))
            (py-safe (get-real-chords self))
            (py-safe (chords self)))
      (py-safe (funcall (internp sname (slot-package slot)) self)))))

(defun py-music-fields (self values?)
  "Parametres musicaux qui ne sont pas des initargs (cf. omNG-save de simple-container dans OM 8.0)."
  (let ((top (not (py-safe (parent self))))   ; extras/couleurs : sauves au niveau de l'objet racine
        (gn (py-safe (gnotes self))))
    (list
     "name" (jstr? (py-safe (get-name self)))
     "approx" (let ((a (py-safe (approx self)))) (when a (jnum a)))
     "port" (omNG-python-save (if (py-is-a self 'note)
                                  (py-safe (port self))
                                (py-safe (get-port self)))
                              values?)
     "tie" (when (py-is-a self 'note) (jstr? (py-safe (tie self))))
     "gracenotes" (when gn (omNG-python-save (py-safe (glist gn)) values?))
     "tonalite" (let ((ton (py-safe (tonalite self))))
                  (when ton (jstr (lisp-code-string (py-safe (omng-save ton))))))
     "tonalvalues" (let ((l (py-safe (get-tonal-values self))))
                     (when l (jstr (lisp-code-string l))))
     "colors" (when top
                (let ((l (py-safe (cons-mus-color self))))
                  (when l
                    (json-list (loop for item in l collect
                                     (json-obj "path" (omNG-python-save (first item))
                                               "color" (json-color (second item))))))))
     "extras" (when top
                (let ((l (py-safe (cons-extra-pairs self))))
                  (when l
                    (json-list (loop for item in l collect
                                     (json-obj "path" (omNG-python-save (first item))
                                               "lisp" (jstr (lisp-code-string (second item)))))))))
     "patches" (when top
                 (let ((l (py-safe (cons-patch-pairs self))))
                   (when l (jstr (lisp-code-string l))))))))

(defun py-extra-fields (self values?)
  (cond ((py-music-object-p self) (py-music-fields self values?))
        ((py-is-a self 'bpf)
         (list "name" (jstr? (py-safe (get-name self)))
               "color" (json-color (py-safe (bpfcolor self)))))
        (t nil)))

(defun json-om-object (self values?)
  "Objet d'une classe OM (chord-seq, voice, bpf, ...) :
slots initargs + parametres supplementaires + code Lisp si la classe a une sauvegarde speciale inconnue."
  (let ((theclass (class-name (class-of self))))
    (apply #'json-obj
           "class" (jstr (get-py-name theclass))
           "slots" (apply #'json-obj
                          (loop for slot in (py-safe (get-all-initargs-of-class theclass))
                                append (list (get-py-name (name slot))
                                             (omNG-python-save (py-slot-value self slot) values?))))
           (append (py-extra-fields self values?)
                   (when (and (py-safe (execption-save-p self)) (not (py-structured-p self)))
                     ;; sons, midifiles, sdif... : on garde aussi le code Lisp complet
                     (list "lisp" (jstr (lisp-code-string (or (py-safe (omng-save self t)) self)))))))))


;=====================================================
; PARAMETRES D'EDITION -> MusicalParams (C++)
;=====================================================

(defun json-alist (alist)
  (apply #'json-obj
         (loop for item in alist
               when (and (consp item) (car item))
               append (list (get-py-name (if (symbolp (car item)) (car item) (princ-to-string (car item))))
                            (omNG-python-save (cdr item) t)))))

(defun py-param (params key)
  (cdr (assoc key params :test #'(lambda (a b) (and (symbolp b) (string-equal (symbol-name a) (symbol-name b)))))))

;=====================================================
; OBJETS OM 8.0 -> format om_save / om_load des classes C++
;   (Note, Chord, ChordSeq, MultiSeq, Voice, Measure, Poly, bpf, Sound, Midifile,
;    AudioMixConsole, MidiMixConsole)
;   Hauteurs : OM 8.0 en midicents (6000)  ->  C++ en MIDI flottant (60.0)
;=====================================================

(defun jint (x) (jnum (py-int x)))
(defun jfloat (x &optional (default 0.0)) (json-number (float (if (numberp x) x default))))
(defun py-listify (x) (if (listp x) x (list x)))
(defun jints (l) (json-list (mapcar #'jint (py-listify l))))
(defun jmidis (l) (json-list (mapcar #'(lambda (mc) (jfloat (if (numberp mc) (/ mc 100.0) 60.0)))
                                     (py-listify l))))
(defun jlistlist (ll fn)
  (json-list (mapcar fn (py-listify ll))))

;;; ---------- objets "lisses" ----------

(defun py-cpp-note (self)
  (json-obj "class" (json-string "Note")
            "m" (jfloat (/ (or (py-safe (midic self)) 6000) 100.0))
            "v" (jint (py-safe (vel self)))
            "d" (jint (py-safe (dur self)))
            "c" (jint (py-safe (chan self)))
            "t" (jint (if (py-safe (tie self)) 1 0))))

(defun py-cpp-chord (self)
  (json-obj "class" (json-string "Chord")
            "m" (jmidis (py-safe (lmidic self)))
            "v" (jints (py-safe (lvel self)))
            "d" (jints (py-safe (ldur self)))
            "o" (jints (py-safe (loffset self)))
            "c" (jints (py-safe (lchan self)))))

(defun py-cpp-chordseq (self)
  (let* ((m (py-listify (py-safe (lmidic self))))
         (n (length m))
         (onsets (py-listify (py-safe (lonset self)))))
    (json-obj "class" (json-string "Chordseq")
              "m" (jlistlist m #'jmidis)
              ;; OM 8.0 ajoute la date de fin a lonset : on garde une date par accord
              "o" (jints (subseq onsets 0 (min n (length onsets))))
              "d" (jlistlist (py-safe (ldur self)) #'jints)
              "v" (jlistlist (py-safe (lvel self)) #'jints)
              "off" (jlistlist (py-safe (loffset self)) #'jints)
              "c" (jlistlist (py-safe (lchan self)) #'jints)
              "l" (jint (py-safe (legato self))))))

(defun py-cpp-multiseq (self)
  (json-obj "class" (json-string "MultiSeq")
            "chs" (json-list (remove nil (mapcar #'py-cpp-object (py-safe (chord-seqs self)))))))

;;; ---------- objets rythmiques ----------

(defun py-tempo-bpm (tempo)
  "Tempo OM 8.0 (60, (1/4 60) ou ((1/4 60) ...)) -> noire = bpm."
  (cond ((numberp tempo) tempo)
        ((and (consp tempo) (consp (car tempo)) (numberp (first (car tempo))) (numberp (second (car tempo))))
         (* (second (car tempo)) (first (car tempo)) 4))
        ((and (consp tempo) (numberp (first tempo)) (numberp (second tempo)))
         (* (second tempo) (first tempo) 4))
        (t 60)))

(defun py-signature (sig)
  "Signature OM 8.0 : (4 4), 4//4 ou 4 -> (values 4 4)."
  (cond ((and (consp sig) (numberp (first sig)) (numberp (second sig)))
         (values (round (first sig)) (round (second sig))))
        ((symbolp sig)
         (let* ((s (symbol-name sig)) (p (search "//" s)))
           (if p
               (values (or (py-safe (parse-integer s :end p)) 4)
                       (or (py-safe (parse-integer s :start (+ p 2))) 4))
             (values 4 4))))
        ((numberp sig) (values (round sig) 4))
        (t (values 4 4))))

(defun py-rt-node (x)
  "Noeud d'arbre rythmique -> syntaxe Python de RT::list2tree (1.0 = liaison, negatif = silence)."
  (cond ((integerp x) (format nil "~D" x))
        ((floatp x) (format nil "~D.0" (round x)))
        ((rationalp x) (format nil "~D" (round x)))
        ((and (consp x) (= (length x) 2) (listp (second x)))
         (format nil "[~A, [~{~A~^, ~}]]" (py-rt-node (first x)) (mapcar #'py-rt-node (second x))))
        (t "1")))

(defun py-rt-measure (m)
  "Mesure OM 8.0 ((4 4) (1 1 1 1)) -> \"[(4,4), [1, 1, 1, 1]],\" (format de RT::toString)."
  (multiple-value-bind (n d) (py-signature (first m))
    (format nil "[(~D,~D), [~{~A~^, ~}]]," n d (mapcar #'py-rt-node (py-listify (second m))))))

(defun py-voice-measures (tree)
  "Arbre de voice OM 8.0 (? (mesure1 mesure2 ...)) -> liste des mesures."
  (if (and (consp tree) (= (length tree) 2) (listp (second tree)))
      (second tree)
    tree))

(defun py-cpp-rchord (chord bpm)
  ;; la duree reelle vient de l'arbre : d1/d2 et u1/u2 sont recalcules par la Voice
  (json-obj "class" (json-string "RChord")
            "m" (jmidis (py-safe (lmidic chord)))
            "v" (jints (py-safe (lvel chord)))
            "d1" (jint 1) "d2" (jint 4)
            "c" (jints (py-safe (lchan chord)))
            "te" (jfloat bpm 60)
            "u1" (jint 1) "u2" (jint 4)))

(defun py-voice-chords (self)
  (or (py-safe (get-real-chords self))
      (remove-if-not #'(lambda (x) (py-is-a x 'chord)) (py-listify (py-safe (chords self))))))

(defun py-cpp-voice (self)
  (let ((bpm (py-tempo-bpm (py-safe (tempo self)))))
    (json-obj "class" (json-string "Voice")
              "trs" (json-list (mapcar #'(lambda (m) (json-string (py-rt-measure m)))
                                       (py-voice-measures (py-safe (tree self)))))
              "chs" (json-list (mapcar #'(lambda (c) (py-cpp-rchord c bpm)) (py-voice-chords self)))
              "te" (jfloat bpm 60))))

(defun py-cpp-measure (self)
  (let ((bpm (py-tempo-bpm (py-safe (tempo self)))))
    (json-obj "class" (json-string "Measure")
              "tre" (json-string (py-rt-measure (py-safe (tree self))))
              "chs" (json-list (mapcar #'(lambda (c) (py-cpp-rchord c bpm)) (py-voice-chords self)))
              "te" (jfloat bpm 60))))

(defun py-cpp-poly (self)
  (json-obj "class" (json-string "Poly")
            "vcs" (json-list (remove nil (mapcar #'py-cpp-object (py-safe (voices self)))))))

;;; ---------- autres ----------

(defun py-cpp-bpf (self)
  ;; C++ : points entiers ; OM 8.0 : x-points/y-points en flottants avec <decimals> decimales
  (let* ((de (py-int (py-safe (decimals self))))
         (k (expt 10 de)))
    (json-obj "class" (json-string "bpf")
              "de" (jint de)
              "xp" (json-list (mapcar #'(lambda (x) (jint (* x k))) (py-listify (py-safe (x-points self)))))
              "yp" (json-list (mapcar #'(lambda (y) (jint (* y k))) (py-listify (py-safe (y-points self))))))))

(defun py-cpp-ncercle (self)
  ;; C++ ncercle::om_load : n = periode, p = liste de cercles (listes d'entiers)
  ;; OM 8.0 n-cercle : n, puntos (une liste ou une liste de listes)
  ;; i = cercle-interval (low high) : les hauteurs midi jouees par l'editeur
  (let ((pts (py-listify (py-safe (puntos self))))
        (int (py-safe (cercle-interval self))))
    (json-obj "class" (json-string "ncercle")
              "p" (jlistlist (if (consp (car pts)) pts (list pts)) #'jints)
              "n" (jint (or (py-safe (n self)) 12))
              "i" (when (and (consp int) (numberp (first int)) (numberp (second int)))
                    (json-list (list (jfloat (first int)) (jfloat (second int))))))))

(defun py-cpp-sound (self)
  (let ((p (py-safe (om-sound-file-name self))))
    (json-obj "class" (json-string "Sound")
              "path" (json-string (if p (namestring p) "")))))

(defun py-cpp-midifile (self)
  (let ((p (py-safe (midifilename self))))
    (json-obj "class" (json-string "Midifile")
              "path" (json-string (if p (namestring p) "")))))

;;; ---------- consoles de mixage ----------
;;; OM 8.0 : projects/omsounds/general/audio-mix-console.lisp (audio-channel-ctrl : track pan-ctrl vol-ctrl)
;;;          projects/midi/classes/midi-console.lisp (channel-ctrl : midiport midichannel program pan-ctrl
;;;          control1-num control2-num control1-val control2-val vol-ctrl pitch-ctrl)
;;; C++ : AudioMixConsole::om_load / MidiMixConsole::om_load (src/projects/music/classes/mixconsole)

(defun py-cpp-audio-mix-console (self)
  (json-obj "class" (json-string "AudioMixConsole")
            "channels" (json-list
                        (mapcar #'(lambda (ch)
                                    (json-obj "track" (jint (py-int (py-safe (track ch)) 1))
                                              "pan" (jint (py-safe (pan-ctrl ch)))
                                              "vol" (jint (py-int (py-safe (vol-ctrl ch)) 100))))
                                (py-listify (py-safe (channels-ctrl self)))))))

(defun py-midiport (p)
  "Port MIDI OM 8.0 (NIL : le port par defaut) -> C++ (-1 : le port par defaut)."
  (jnum (if (numberp p) (round p) -1)))

(defun py-cpp-midi-mix-console (self)
  (json-obj "class" (json-string "MidiMixConsole")
            "midiport" (py-midiport (py-safe (midiport self)))
            "channels" (json-list
                        (mapcar #'(lambda (ch)
                                    (json-obj "midiport" (py-midiport (py-safe (midiport ch)))
                                              "midichannel" (jint (py-int (py-safe (midichannel ch)) 1))
                                              "program" (jint (py-safe (program ch)))
                                              "pan" (jint (py-int (py-safe (pan-ctrl ch)) 64))
                                              "control1num" (jint (py-int (py-safe (control1-num ch)) 1))
                                              "control2num" (jint (py-int (py-safe (control2-num ch)) 2))
                                              "control1val" (jint (py-safe (control1-val ch)))
                                              "control2val" (jint (py-safe (control2-val ch)))
                                              "vol" (jint (py-int (py-safe (vol-ctrl ch)) 100))
                                              "pitch" (jint (py-int (py-safe (pitch-ctrl ch)) 8192))))
                                (py-listify (py-safe (channels-ctrl self)))))))

;;; Dans OM 8.0, une image peut etre reperee de 3 facons :
;;;  1. pict-pathname : chemin du fichier (images chargees depuis un fichier) ;
;;;  2. name + source : image des ressources (source = user / kernel / une librairie),
;;;     SANS chemin : le fichier est <dossier de ressources>/<name>.<png|tif|jpg...> ;
;;;  3. donnees internes (tableau de pixels, image dessinee, storemode :internal) : aucun fichier.
;;; Le cas 2 n'avait pas de pict-pathname : c'est pourquoi le chemin etait vide.
;;; Pour les cas sans fichier lisible par JUCE (3, ou TIFF), l'image est reecrite en PNG
;;; dans <dossier d'export>/pictures/.

(defvar *py-pict-counter* 0)
(defparameter *py-juce-image-types* '("png" "jpg" "jpeg" "gif"))

(defun py-find-pict-file (folder name)
  "Cherche <folder>/<name>.<ext> pour toutes les extensions d'image d'OM 8.0."
  (when (and folder name)
    (loop for type in (or (py-safe (symbol-value '*om-pict-type*))
                          '("png" "jpg" "jpeg" "gif" "tif" "tiff" "bmp"))
          for f = (make-pathname :host (pathname-host folder) :device (pathname-device folder)
                                 :directory (pathname-directory folder) :name name :type type)
          when (probe-file f) return (probe-file f))))

(defun py-picture-resource-file (self)
  "Cas 2 : image designee par name + source (ressources user/kernel/librairie)."
  (let ((name (py-safe (name self)))
        (src (py-safe (source self))))
    (when (and name (not (stringp name)))
      ;; ancien format : symbole USER-nom
      (multiple-value-bind (s n) (string-until-char (string name) "-")
        (setf src (intern s :om) name n)))
    (when (and (stringp name) src)
      (py-find-pict-file (py-safe (restore-pict-folder src)) name))))

(defun py-picture-declared-file (self)
  "Cas 1 : pict-pathname (ou background quand c'est un chemin)."
  (let ((p (or (py-safe (pict-pathname self))
               (let ((bg (py-safe (slot-value self 'background))))
                 (and (or (pathnamep bg) (stringp bg)) bg)))))
    (when p
      (let ((p (if (stringp p) (pathname p) p)))
        (or (py-safe (probe-file (or (py-safe (restore-pict-path p)) p)))
            (py-safe (probe-file p)))))))

(defun py-safe-file-name (str)
  (let ((s (remove-if-not #'(lambda (c) (or (alphanumericp c) (member c '(#\- #\_)))) (string str))))
    (if (string= s "") "picture" s)))

(defun py-export-picture-png (self base)
  "Ecrit l'image (pixels) en PNG dans <dossier d'export>/pictures/ ; renvoie le chemin ou NIL."
  (let ((pict (py-safe (thepict self))))
    (when (and pict *py-dst-root*)
      (let* ((dir (make-pathname :host (pathname-host *py-dst-root*) :device (pathname-device *py-dst-root*)
                                 :directory (append (pathname-directory *py-dst-root*) (list "pictures"))))
             (file (make-pathname :host (pathname-host dir) :device (pathname-device dir)
                                  :directory (pathname-directory dir)
                                  :name (format nil "~A-~D" (py-safe-file-name base) (incf *py-pict-counter*))
                                  :type "png")))
        (py-safe (om-create-directory dir))
        (when (py-safe (om-save-picture pict file :png))
          (namestring file))))))

(defun py-picture-path (self)
  "Chemin d'un fichier image lisible par le C++ (juce::ImageFileFormat), ou \"\"."
  (let* ((file (or (py-picture-declared-file self)
                   (py-picture-resource-file self)))
         (type (and file (pathname-type file))))
    (cond
     ;; fichier existant dans un format lu par JUCE
     ((and file type (member type *py-juce-image-types* :test #'string-equal))
      (namestring file))
     ;; TIFF/BMP, ou pas de fichier du tout : on exporte les pixels en PNG
     ((py-export-picture-png self (or (and file (pathname-name file))
                                      (let ((n (py-safe (name self)))) (and n (string n)))
                                      "picture")))
     ;; dernier recours : le fichier tel quel
     (file (namestring file))
     (t ""))))

(defun py-picture-json (path)
  ;; Picture::om_load : p = chemin du fichier image, r = relatif ou non
  (json-obj "class" (json-string "Picture")
            "p" (json-string path)
            "r" (jbool nil)))

(defun py-cpp-picture (self &optional (path (py-picture-path self)))
  ;; Si aucun chemin n'est trouve, renvoie NIL : un chemin vide ferait ouvrir
  ;; un dialogue de choix de fichier par Picture::initPicture au chargement.
  (if (string= path "")
      (progn (om-print (format nil "Export JSON : image ~A sans fichier, non exportee"
                               (or (py-safe (name self)) "")))
             nil)
    (py-picture-json path)))

;;; ---------- table de correspondance ----------
;;; (classe OM 8.0  nom de classe C++  fonction de conversion)
;;; Ordre : du plus specifique au plus general.

(defparameter *py-cpp-classes*
  '((note       "Note"     py-cpp-note)
    (chord      "Chord"    py-cpp-chord)
    (chord-seq  "ChordSeq" py-cpp-chordseq)
    (multi-seq  "MultiSeq" py-cpp-multiseq)
    (measure    "Measure"  py-cpp-measure)
    (voice      "Voice"    py-cpp-voice)
    (poly       "Poly"     py-cpp-poly)
    (bpf        "bpf"      py-cpp-bpf)
    (sound      "Sound"    py-cpp-sound)
    (midifile   "Midifile" py-cpp-midifile)
    ;; simple-midi-mix-console et midi-mixer sont des midi-mix-console
    (audio-mix-console "AudioMixConsole" py-cpp-audio-mix-console)
    (midi-mix-console  "MidiMixConsole"  py-cpp-midi-mix-console)
    (picture    "Picture"  py-cpp-picture)
    (n-cercle   "ncercle"  py-cpp-ncercle)))

(defun py-cpp-class-entry (obj)
  (find-if #'(lambda (e) (py-is-a obj (first e))) *py-cpp-classes*))

(defun py-cpp-class-name (obj)
  (second (py-cpp-class-entry obj)))

(defun py-cpp-object (obj)
  "JSON au format om_save de la classe C++, ou NIL si la classe n'a pas d'equivalent."
  (let ((e (py-cpp-class-entry obj)))
    (when e (py-safe (funcall (third e) obj)))))

(defun py-cpp-musical-p (obj)
  "Objets dont les params C++ sont des MusicalParams (MusObj::omng_load)."
  (member (py-cpp-class-name obj) '("Note" "Chord" "ChordSeq" "MultiSeq" "Measure" "Voice" "Poly")
          :test #'string=))


;=====================================================
; MusicalParams : toutes les cles lues par MusObj::omng_load
;=====================================================

(defparameter *py-approx-edo0*
  ;; approx OM 8.0 -> index de la table EDO du C++ (getTuningData)
  '((1 . 1) (2 . 6) (3 . 11) (4 . 14) (5 . 15) (6 . 17) (7 . 19) (8 . 20)
    (10 . 22) (12 . 23) (14 . 24) (16 . 25)))

(defun py-staff-name (s)
  (cond ((null s) "G")
        ((and (symbolp s) (string-equal (symbol-name s) "empty")) "empty")
        ((symbolp s) (string-upcase (symbol-name s)))
        ((stringp s) (if (string-equal s "empty") "empty" (string-upcase s)))
        (t "G")))

(defun py-staff-string (staff)
  "Format de StaffSystem::staff2string : ['G',] ou ['G','F',] (une portee par voix)."
  (format nil "[~{'~A',~}]" (mapcar #'py-staff-name (if (listp staff) staff (list staff)))))

(defun json-musical-params (params)
  (let* ((approx (py-param params 'approx))
         (edo0 (or (cdr (assoc (and (numberp approx) (round approx)) *py-approx-edo0*)) 6))
         (zoom (py-param params 'zoom))
         (fs (py-param params 'fontsize))
         (cursor (py-param params 'cursor-mode)))
    (json-obj
     "type" (json-string "MusicalParams")
     "fsize" (jint (if (numberp fs) fs 24))
     "mvfsize" (jint 20)
     "pgmmode" (jbool nil)
     "scale" (json-string "1/2")
     "slotmode" (jint (or (py-param params 'mode) 0))
     "chordmode" (json-string "chord")
     "zoom" (jint (if (numberp zoom) (* zoom 100) 100))      ; OM 8.0 : facteur 1 ; C++ : pourcentage
     "showtempo" (jbool nil)
     "showstem" (jbool (py-param params 'show-stems))
     "inport" (jint (let ((p (py-param params 'inport))) (if (numberp p) p 0)))
     "outport" (jint (let ((p (py-param params 'outport))) (if (numberp p) p 0)))
     "ncolorp" (jbool (py-param params 'notechancolor?))
     "grille" (jint (or (py-param params 'grillestep) 1000))
     "scaleFactor" (jint 1)
     "linespc" (jint 1)
     "player" (jint 0)
     "selec_mode" (jint (or (py-param params 'obj-mode) 0))
     "cur_mode" (jint (if (eq cursor :interval) 1 0))
     "mvdx" (jfloat 4)
     "mvdy" (jfloat 4)
     "sys_col_r" (jint 0) "sys_col_g" (jint 0) "sys_col_b" (jint 0) "sys_col_a" (jint 255)
     "title" (json-string "Musica")
     "staff" (json-obj "staffs" (json-string (py-staff-string (py-param params 'staff)))
                       "interlines" "[]")
     "edo0" (jint edo0)
     "edo1" (jint 0))))

(defun json-default-musical-params ()
  (json-musical-params nil))

(defun py-cpp-console-p (obj)
  "Les consoles de mixage : des MusObj en C++ (MusObj::omng_load lit d[\"params\"]), sans portee."
  (member (py-cpp-class-name obj) '("AudioMixConsole" "MidiMixConsole") :test #'string=))

(defun json-params-for (obj params)
  "Bloc \"params\" attendu cote C++ pour l'objet OBJ (NIL si inutile)."
  (cond ((py-cpp-musical-p obj)
         (json-musical-params (when (listp params) params)))
        ((py-cpp-console-p obj)
         (json-musical-params (list (cons 'staff 'empty))))))


(defun json-edition-params (obj)
  "Parametres d'edition d'une boite editeur / boite temporelle / instance."
  (let ((params (py-safe (edition-params obj))))
    (when (and params (listp params))
      (json-obj "musicalparams" (when (py-param params 'staff) (py-safe (json-musical-params params)))
                "all" (py-safe (json-alist params))))))


;=====================================================
; ENTREES DES BOITES (inletbox)
;=====================================================
;;; kind C++ : "positional", "var_positional", "keyword", "var-keyword"

(defun py-inlet-index (self)
  (let ((box (py-safe (box-ref self))))
    (or (and box (py-safe (position self (inputs box)))) 0)))

(defun json-inlet (self kind val &rest more)
  "Format lu par inletbox::omng_load : name doc kind index valuetype valstring [menusel].
valuetype \"fromstring\" : valstring est evalue en Python (pybind11::eval) ;
on garde donc toujours la valeur OM 8.0 de l'entree."
  (apply #'json-obj
         "name" (json-string (or (get-py-name (name self)) ""))
         "doc" (json-string (or (py-safe (doc-string self)) ""))
         "kind" (json-string kind)
         "index" (jnum (py-inlet-index self))
         "valuetype" (json-string "fromstring")
         "valstring" (json-string (py-literal val))
         more))

(defmethod omNG-python-save ((self input-funbox) &optional (values? nil))
  (declare (ignore values?))
  (json-inlet self "positional" (py-safe (value self))))

(defmethod omNG-python-save ((self input-funmenu) &optional (values? nil))
  (declare (ignore values?))
  ;; menusel = position de la valeur courante dans le menu (le menu lui-meme vient de la fonction Python)
  (let* ((items (py-safe (thepopup self)))
         (sel (py-safe (position (value self) items
                                 :key #'(lambda (x) (if (consp x) (second x) x)) :test 'equal))))
    (json-inlet self "positional" (py-safe (value self))
                "menusel" (jnum (or sel 0)))))

(defmethod omNG-python-save ((self input-keyword) &optional (values? nil))
  (declare (ignore values?))
  ;; dans OM 8.0, value = le mot-cle (:approx) et def-value = sa valeur ;
  ;; dans le C++, name = nom du mot-cle et la valeur est dans valstring
  (json-obj "name" (json-string (or (get-py-name (py-safe (value self))) ""))
            "doc" (json-string (or (py-safe (doc-string self)) ""))
            "kind" (json-string "keyword")
            "index" (jnum (py-inlet-index self))
            "valuetype" (json-string "fromstring")
            "valstring" (json-string (py-literal (py-safe (def-value self))))))


;=====================================================
; Valeur Lisp -> expression Python (pour OMBoxCte "ref")
;=====================================================

(defun py-literal (x)
  (cond ((null x) "None")
        ((eq x t) "True")
        ((integerp x) (format nil "~D" x))
        ((numberp x) (let ((s (json-number x))) (if (string= s "null") "None" s)))
        ((stringp x) (json-string x))          ; une chaine JSON est aussi une chaine Python valide
        ((characterp x) (json-string (string x)))
        ((symbolp x) (json-string (string-downcase (symbol-name x))))
        ((and (consp x) (eq (car x) 'quote) (consp (cdr x))) (py-literal (cadr x)))
        ((and (consp x) (null (cdr (last x))))
         (format nil "[~{~A~^, ~}]" (mapcar #'py-literal x)))
        ((consp x) (format nil "(~A, ~A)" (py-literal (car x)) (py-literal (cdr x))))
        (t (json-string (princ-to-string x)))))


;=====================================================
; BOITES : champs communs lus par OMBox::omng_load
;   name fname x y w h numouts lock inputs   (tous obligatoires)
;=====================================================

(defun py-int (x &optional (default 0))
  (if (numberp x) (round x) default))

(defun py-lock (box)
  "Code de verrouillage OM 8.0 = celui du C++ : \"\", \"x\", \"&\", \"l\"."
  (let ((l (py-safe (allow-lock box))))
    (if (and (stringp l) (member l '("x" "&" "l") :test #'string-equal))
        (string-downcase l)
      "")))

(defun py-box-numouts (box &optional (default 1))
  (py-int (or (py-safe (numouts box))
              (py-safe (length (outputs box))))
          default))

(defun json-box (box type &rest more)
  "Champs communs + champs specifiques MORE (plist de cles string).
Dans MORE, :name et :numouts (cles keyword) forcent ces deux champs."
  (let* ((pos (py-safe (frame-position box)))
         (size (unless (typep box 'omboxcall) (py-safe (frame-size box))));faire une fonction pour donner le size selon box
         (bname (string-downcase (or (py-safe (name box)) "")))
         (forced-name (getf more :name :none))
         (forced-numouts (getf more :numouts :none))
         (more (loop for (k v) on more by #'cddr
                     unless (keywordp k) append (list k v)))) 
    (apply #'json-obj
           "type" (jstr type)
           "name" (json-string (if (eq forced-name :none) bname (or forced-name "")))
           "fname" (json-string bname)
           "x" (jnum (py-int (and pos (om-point-h pos))))
           "y" (jnum (py-int (and pos (om-point-v pos))))
           "w" (jnum (py-int (and size (om-point-h size)) 60))
           "h" (jnum (py-int (and size (om-point-v size)) 60))
           "numouts" (jnum (if (eq forced-numouts :none) (py-box-numouts box) forced-numouts))
           "lock" (json-string (py-lock box))
           "inputs" (json-box-inputs box)
           more)))

(defun json-box-inputs (box)
  (json-list (mapcar #'(lambda (input) (omNG-python-save input t))
                     (py-safe (inputs box)))))

(defun json-dead-box (box &optional (message ""))
  "Boite sans equivalent : OMBoxDead (ref + savestr = code Lisp OM 8.0)."
  (json-box box "OMBoxDead"
            "ref" (json-string (or (py-ref-name (py-safe (reference box))) ""))
            "savestr" (json-string (or (py-safe (lisp-code-string (omng-save box t))) ""))
            "message" (json-string message)))

(defun json-unknown-box (box)
  (json-dead-box box (format nil "Boite OM 8.0 ~A non convertie" (class-name (class-of box)))))

;;; Noms des librairies : le C++ cherche le dossier om/libraries/<nom> (sans numero de version)
(defparameter *py-lib-names* '(("OMTristan" "omtristan")))

(defun py-lib-name (name)
  (let* ((name (string-trim " " (or name "")))
         (space (position #\Space name))
         (base (if (and space (digit-char-p (char name (min (1+ space) (1- (length name))))))
                   (subseq name 0 space)    ; "OMTristan 3.4" -> "OMTristan"
                 name)))
    (or (second (assoc base *py-lib-names* :test #'string-equal)) base)))

(defun py-lib-of-function (ref)
  (let ((lib (py-safe (lib-fun-p (fdefinition ref)))))
    (py-lib-name
     (cond ((null lib) "")
           ((stringp lib) lib)
           (t (or (py-safe (name lib)) ""))))))

(defun py-lib-of-class (classname)
  (let ((libs (py-safe (class-needed-libraries (find-class classname nil)))))
    (if (and libs (stringp (car libs))) (car libs) "")))


;=====================================================
; CHEMINS : patchs et instances references (fichiers exportes)
;=====================================================

(defvar *py-src-root* nil "Dossier source OM 8.0 exporte (elements du workspace ou dossier).")
(defvar *py-dst-root* nil "Dossier destination OMPython choisi par l'utilisateur.")

(defun py-dest-namestring (obj ext)
  "Chemin absolu du fichier exporte correspondant a OBJ (pour \"path\")."
  (let* ((src (py-safe (mypathname obj)))
         (rel (and src *py-src-root* (py-safe (path-in-dir-p src *py-src-root*))))
         (root (or *py-dst-root* (and src (make-pathname :directory (pathname-directory src)
                                                         :device (pathname-device src)
                                                         :host (pathname-host src))))))
    (if (and root src)
        (namestring (make-pathname :device (pathname-device root) :host (pathname-host root)
                                   :directory (append (pathname-directory root)
                                                      (if (consp rel) (remove nil rel) nil))
                                   :name (pathname-name src)
                                   :type ext))
      "")))


;=====================================================
; BOITES
;=====================================================

;Fonctions (generiques et lisp) -> OMBoxFun
(defun json-boxfun (self)
  (let ((ref (or (py-safe (save-reference self)) (py-safe (reference self)))))   ; save-reference remplit *libs-to-load*
    (json-box self "OMBoxFun"
              :name (or (get-py-fun-name (py-ref-name ref)) "")    ; C++ : omws->getFun(name, lib)
              "value" (if (string= (py-lock self) "x")
                          (omNG-python-save (py-safe (value self)) t)
                        "null")
              "methodclass" (json-string "")
              "lib" (json-string (if (symbolp ref) (py-lib-of-function ref) "")))))

(defmethod omNG-python-save ((self OMBoxCall) &optional (values? nil))
  (declare (ignore values?))
  (json-boxfun self))

(defmethod omNG-python-save ((self OMBoxlispCall) &optional (values? nil))
  (declare (ignore values?))
  (json-boxfun self))

;Constantes -> OMBoxCte (ref = expression Python evaluee par pybind11::eval)
(defmethod omNG-python-save ((self OMBoxTypeCall) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "OMBoxCte"
            :numouts 1
            "ref" (json-string (py-literal (py-safe (value self))))))

;Editeurs (chord-seq, voice, bpf...) -> OMBoxClass
;;; C++ : getClass(name, lib), puis temoin->om_load(d["value"]) et, pour les objets musicaux,
;;; MusObj::omng_load(d) qui lit d["params"] (toutes les cles sont obligatoires).
(defmethod omNG-python-save ((self OMBoxEditCall) &optional (values? nil))
  (declare (ignore values?))
  (let* ((obj (py-safe (value self)))
         (obj (if (and (consp obj) (null (cdr obj))) (car obj) obj))
         (cppclass (and obj (py-cpp-class-name obj)))
         (cppvalue (and cppclass (py-cpp-object obj))))
    (if (null cppvalue)
        (json-dead-box self (format nil "Classe OM 8.0 ~A sans equivalent C++"
                                    (py-ref-name (or (py-safe (save-reference self)) (py-safe (reference self))))))
      (json-box self "OMBoxClass"
                :name cppclass
                "value" cppvalue
                "params" (json-params-for obj (py-safe (edition-params self)))
                "show" (jbool (py-safe (minieditor? self)))
                "lib" (json-string "")))))

;Boites "slots" : pas d'equivalent C++
(defmethod omNG-python-save ((self OMSlotsBox) &optional (values? nil))
  (declare (ignore values?))
  (json-dead-box self "Boite slots OM 8.0"))

;Instances : format de OMInstance::omng_save / loadInternalInstance
(defun instance-json-plist (inst)
  "Plist JSON de l'instance, ou NIL si la classe de l'objet n'a pas d'equivalent C++."
  (when (py-safe (mypathname inst))
    (py-safe (register-resource :instance (mypathname inst))))
  (let* ((obj (py-safe (instance inst)))
         (cppclass (and obj (py-cpp-class-name obj)))
         (cppvalue (and cppclass (py-cpp-object obj))))
    (when cppvalue
      (list "type" (json-string "OMInstance")
            "version" (json-string *py-export-version*)
            "class" (json-string cppclass)
            "instance" cppvalue
            "params" (or (json-params-for obj (py-safe (edition-params inst)))
                         (json-obj "type" (json-string "Params")))))))

(defmethod omNG-python-save ((self OMInstance) &optional (values? nil))
  (declare (ignore values?))
  (let ((pl (instance-json-plist self)))
    (if pl (apply #'json-obj pl) "null")))

(defmethod omNG-python-save ((self OMBoxInstance) &optional (values? nil))
  (declare (ignore values?))
  (let ((inst (py-safe (reference self))))
    (if (and inst (py-safe (mypathname inst)))
        (progn
          (py-safe (register-resource :instance (mypathname inst)))
          (json-box self "OMBoxInstance" :numouts 1
                    "path" (json-string (py-dest-namestring inst "omi"))))
      (let ((pl (and inst (instance-json-plist inst))))
        (if pl
            (json-box self "OMBoxInstance" :numouts 1
                      "theinstance" (apply #'json-obj pl))
          (json-dead-box self "Instance sans equivalent C++"))))))

;Patchs : interne (rouge) -> OMBoxPatchAbs ; externe -> OMBoxPatch (chemin absolu du fichier exporte)
(defmethod omNG-python-save ((self OMBoxPatch) &optional (values? nil))
  (declare (ignore values?))
  (let ((ref (reference self)))
    (if (py-safe (abspatch-p ref))
        (json-box self "OMBoxPatchAbs"
                  "thepatch" (patch-to-json ref))
      (progn
        (when (py-safe (mypathname ref))
          (py-safe (register-resource :abstraction (mypathname ref))))
        (json-box self "OMBoxPatch"
                  "path" (json-string (py-dest-namestring ref "omp")))))))

;Maquettes : pas de chargeur C++
(defmethod omNG-python-save ((self OMBoxMaquette) &optional (values? nil))
  (declare (ignore values?))
  (json-dead-box self "Boite maquette OM 8.0"))

;Entrees / sorties de patch
(defmethod omNG-python-save ((self OMIn) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "OMBoxIn" :numouts 1
            "index" (jnum (py-int (py-safe (indice self))))
            "value" "null"
            "doc" (json-string (or (py-safe (docu self)) ""))
            "defval" (omNG-python-save (py-safe (eval (defval self))) t)))

(defmethod omNG-python-save ((self OMout) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "OMBoxOut" :numouts 0
            "index" (jnum (py-int (py-safe (indice self))))
            "value" "null"))

;Entrees typees de methodes, self, sorties temporelles : sans equivalent
(defmethod omNG-python-save ((self OMTypedIn) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "OMBoxIn" :numouts 1
            "index" (jnum (py-int (py-safe (indice self))))
            "doc" (json-string (or (py-safe (docu self)) ""))))

(defmethod omNG-python-save ((self OMinitTypedIn) &optional (values? nil))
  (declare (ignore values?))
  (json-dead-box self "Entree init OM 8.0"))

;;; Patch d'une boite temporelle : entree self -> tempSelfBoxIn, sortie tempout -> tempBoxOut
;;; (index -1, comme OMBoxTemporal::newTemporalPatch). Les 10 sorties de self sont les memes
;;; dans OM 8.0 et dans le C++ : self offset extend colorframe value posy strech-fact sizey free-store reference.
(defmethod omNG-python-save ((self selfTempIn) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "tempSelfBoxIn"
            "index" (jnum -1)))

(defmethod omNG-python-save ((self OMtempOut) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "tempBoxOut" :numouts 0
            "index" (jnum -1)))

;Commentaires -> OMBoxComment
(defun py-color-255 (v)
  (cond ((not (numberp v)) 0)
        ((and (floatp v) (<= v 1.0)) (round (* v 255)))
        (t (round v))))

(defun py-font-style (style)
  "Style OM 8.0 (:bold :italic ...) -> flags juce::Font (plain 0, bold 1, italic 2, underlined 4)."
  (let ((st (if (listp style) style (list style))))
    (+ (if (member :bold st) 1 0)
       (if (member :italic st) 2 0)
       (if (member :underline st) 4 0))))

(defmethod omNG-python-save ((self OMBoxcomment) &optional (values? nil))
  (declare (ignore values?))
  (let* ((text (py-safe (reference self)))
         (color (py-safe (om-correct-color (textcolor self))))
         (font (py-safe (textstyle self))))
    (json-box self "OMBoxComment" :numouts 0
              "ref" (json-string (if (stringp text) text (if text (princ-to-string text) "")))
              "col_r" (jnum (py-color-255 (and color (py-safe (om-color-r color)))))
              "col_g" (jnum (py-color-255 (and color (py-safe (om-color-g color)))))
              "col_b" (jnum (py-color-255 (and color (py-safe (om-color-b color)))))
              "col_a" (jnum 255)
              "fface" (json-string (or (and font (py-safe (om-font-face font))) "Verdana"))
              "fstyle" (jnum (float (py-font-style (and font (py-safe (om-font-style font))))))
              "fsize" (jnum (float (or (and font (py-safe (om-font-size font))) 12))))))

;Boites non definies / mortes / send-receive / temporelles -> OMBoxDead
(defmethod omNG-python-save ((self OMBoxundefined) &optional (values? nil))
  (declare (ignore values?))
  (json-dead-box self "Boite non definie OM 8.0"))

(defmethod omNG-python-save ((self general-box-dead) &optional (values? nil))
  (declare (ignore values?))
  (json-box self "OMBoxDead"
            "ref" (json-string (or (py-ref-name (py-safe (reference self))) ""))
            "savestr" (json-string (let ((code (py-safe (save-code self))))
                                     (if code (lisp-code-string code) "")))
            "message" (json-string (or (py-safe (mesage self)) ""))))

(defmethod omNG-python-save ((self OMSend) &optional (values? nil))
  (declare (ignore values?))
  (json-dead-box self "Send OM 8.0"))

(defmethod omNG-python-save ((self OMReceive) &optional (values? nil))
  (declare (ignore values?))
  (json-dead-box self "Receive OM 8.0"))

;Boites temporelles -> OMBoxTemporal (format de OMBoxTemporal::omng_save / omng_load)
;;; refkind "patch" / "maquette" : la reference interne est dans "thepatch", une reference
;;; externe dans "path" ; refkind "object" : l'objet est dans "obj" (format om_save de sa classe C++).

(defun py-color-rgba (c)
  "Couleur OM 8.0 -> [r, g, b, a] en entiers 0-255, ou NIL (le C++ garde alors sa couleur par defaut)."
  (let ((c (and c (py-safe (om-correct-color c)))))
    (when c
      (json-list (list (jnum (py-color-255 (py-safe (om-color-r c))))
                       (jnum (py-color-255 (py-safe (om-color-g c))))
                       (jnum (py-color-255 (py-safe (om-color-b c))))
                       (jnum 255))))))

(defun py-internal-ref-p (ref)
  (or (py-safe (abspatch-p ref)) (null (py-safe (mypathname ref)))))

(defun py-temporal-ref-plist (self)
  "Champs de la reference d'une boite temporelle, ou NIL si elle n'a pas d'equivalent C++."
  (let ((ref (py-safe (reference self))))
    (cond
     ;; maquette (avant OMPatch : une maquette est aussi un patch)
     ((py-is-a ref 'OMMaquette)
      (if (py-internal-ref-p ref)
          (list "refkind" (json-string "maquette") "path" (json-string "") "thepatch" (patch-to-json ref))
        (progn
          (py-safe (register-resource :abstraction (mypathname ref)))
          (list "refkind" (json-string "maquette") "path" (json-string (py-dest-namestring ref "omm"))))))
     ((py-is-a ref 'OMPatch)
      (if (py-internal-ref-p ref)
          (list "refkind" (json-string "patch") "path" (json-string "") "thepatch" (patch-to-json ref))
        (progn
          (py-safe (register-resource :abstraction (mypathname ref)))
          (list "refkind" (json-string "patch") "path" (json-string (py-dest-namestring ref "omp"))))))
     ;; objet : instance OM 8.0 ou valeur de la boite
     (t
      (let* ((obj (or (and (py-is-a ref 'OMInstance) (py-safe (instance ref)))
                      (car (py-listify (py-safe (value self))))))
             (cppobj (and obj (py-cpp-object obj))))
        (when cppobj
          (list "refkind" (json-string "object") "path" (json-string "") "obj" cppobj)))))))

(defmethod omNG-python-save ((self TemporalBox) &optional (values? nil))
  (declare (ignore values?))
  (let ((ref (py-temporal-ref-plist self)))
    (if (null ref)
        ;; boite morte plutot que rien : les indices des connexions restent valables
        (json-dead-box self "Boite temporelle OM 8.0 : reference sans equivalent C++")
      (apply #'json-box self "OMBoxTemporal"
             (append
              (list "offset" (jint (py-safe (offset self)))
                    "extend" (jint (or (py-safe (extend self)) 2000))
                    "posy" (jint (py-safe (posy self)))
                    "sizey" (jint (or (py-safe (sizey self)) 20))
                    "strech" (jfloat (py-safe (strech-fact self)) 1.0)
                    "mute" (jbool (py-safe (mute self)))
                    "locked" (jbool (py-safe (lock self)))
                    "showpict" (jbool (py-safe (showpict self)))
                    "pianoroll" (jbool (py-safe (pianoroll? self)))
                    "showname" (jbool t)
                    "color" (py-color-rgba (py-safe (colorframe self))))
              ref)))))

(defmethod omNG-python-save ((self temp-marker) &optional (values? nil))
  (declare (ignore values?))
  (json-lisp-fallback self))

;=====================================================
; PATCHS ET MAQUETTES
;=====================================================

(defun py-save-box (box)
  "Une boite qui echoue ne fait pas echouer tout le patch."
  (handler-case (omNG-python-save box t)
    (error (err)
      (om-print (format nil "Export JSON : boite ~A non convertie (~A)" (py-safe (name box)) err))
      ;; OMBoxDead : chaque boite doit rester dans la liste, sinon les indices des connexions sont decales
      (json-box box "OMBoxDead"
                :numouts (py-safe (py-box-numouts box))
                "ref" (json-string (princ-to-string (class-name (class-of box))))
                "savestr" (json-string "")
                "message" (json-string (princ-to-string err))))))

(defun mk-connection-list_py (boxlist)
  "Connexions : boxsource/sourceidx = boite et sortie de depart,
i/j = boite et entree d'arrivee (indices dans la liste des boites)."
  (let (rep)
    (loop for box in boxlist
          for i from 0 do
          (loop for input in (py-safe (inputs box))
                for j from 0 do
                (let ((conn (py-safe (connected? input))))
                  (when conn
                    (let ((posi (position (first conn) boxlist :test 'eq)))
                      (when posi
                        (push (json-obj "boxsource" (jnum posi)
                                        "sourceidx" (jnum (py-int (second conn)))
                                        "i" (jnum i)
                                        "j" (jnum j)
                                        "points" (when (third conn)
                                                   (py-safe (omNG-python-save (third conn) t))))
                              rep)))))))
    (json-list (nreverse rep))))

(defun py-patch-doc (self)
  "Documentation du patch : TOUJOURS une string (le chargeur C++ fait d[\"doc\"].GetString())."
  (let ((doc (or (py-safe (get-documentation self))
                 (py-safe (doc self)))))
    (json-string (if (and (stringp doc) (string/= doc "")) doc "No documentation"))))

;;; Images d'arriere-plan du patch (pictu-list : objets patch-picture d'OM 8.0)
;;; -> boites Picture (OMBoxClass / OMBoxPicture cote C++).
;;; Elles sont ajoutees APRES les boites du patch : les indices des connexions restent valables.
(defun py-picture-inlet (name index valstring)
  (json-obj "name" (json-string name)
            "doc" (json-string "")
            "kind" (json-string "positional")
            "index" (jint index)
            "valuetype" (json-string "fromstring")
            "valstring" (json-string valstring)))

(defun py-bg-picture-box (pict)
  (let* ((pos (py-safe (pict-pos pict)))
         (size (py-safe (pict-size pict)))
         (path (py-picture-path pict))
         (value (py-cpp-picture pict path))
         (fname (let ((n (py-safe (name pict)))) (if (stringp n) n "picture"))))
   (when value
    (json-obj "type" (json-string "OMBoxClass")
              "name" (json-string "Picture")
              "fname" (json-string fname)
              "x" (jint (and pos (om-point-h pos)))
              "y" (jint (and pos (om-point-v pos)))
              "w" (jint (or (and size (om-point-h size)) 100))
              "h" (jint (or (and size (om-point-v size)) 100))
              "numouts" (jint 2)
              "lock" (json-string "")
              ;; entrees de la classe Picture : self + path
              "inputs" (json-list (list (py-picture-inlet "self" 0 "None")
                                        (py-picture-inlet "path" 1 (py-literal path))))
              "value" value
              "params" (json-obj "type" (json-string "Params"))
              "show" (jbool t)
              "lib" (json-string "")
              ;; champs de OMBoxPicture (pas encore lus par OMBox::omng_load)
              "isBgd" (jbool t)
              "boxSize" (jbool nil)))))

(defun py-bg-picture-boxes (patch)
  (loop for p in (py-listify (py-safe (pictu-list patch)))
        when (and p (or (py-is-a p 'patch-picture) (py-is-a p 'internepicture)))
        collect (py-safe (py-bg-picture-box p))
          into rep
        finally (return (remove nil rep))))

(defmethod patch-json-plist ((self OMPatch))
  (let ((lisp (py-safe (lisp-exp-p self))))
    (if lisp
        ;; patch "lisp" (code tape a la main)
        (list "type" (jstr "OMLispPatch")
              "name" (jstr (name self))
              "doc" (py-patch-doc self)
              "code" (jstr (if (stringp lisp) lisp (lisp-code-string lisp))))
      (list "type" (jstr "OMPatch")
            "name" (jstr (name self))
            "doc" (py-patch-doc self)
            "boxes" (json-list (append (mapcar #'py-save-box (boxes self))
                                        (py-bg-picture-boxes self)))
            "connecs" (mk-connection-list_py (boxes self))
            "libraries" (when (py-safe (abspatch-p self)) "[]")))))   ; patch interne (thepatch)

;;; Parametres de la maquette -> MaquetteParams::load du C++ ("params")
;;; metricparam OM 8.0 : ((<unite> <tempo>) <signatures> <subdivision max> <boucle>), ex. ((4 60) ((4 4)) 16 t)
(defun py-metric-tempo (tempo)
  "Tempo OM 8.0 de la regle metrique -> tempo a la noire."
  (cond ((numberp tempo) tempo)
        ((and (consp tempo) (numberp (first tempo)) (numberp (second tempo)) (/= (first tempo) 0))
         (* (second tempo) (/ 4 (first tempo))))
        (t 60)))

(defun py-maquette-params (params)
  (let* ((range (py-listify (py-safe (range params))))
         (metric (py-listify (py-safe (metricparam params))))
         (sigs (second metric)))
    (json-obj
     "range" (when (and (= (length range) 4) (every #'numberp range))
               (json-list (mapcar #'jint range)))
     "color" (py-color-rgba (py-safe (maq-color params)))
     "showcon" (jbool (py-safe (show-conect params)))
     "showmetric" (jbool (let ((s (py-safe (show-ruler-metric params))))
                           (and (stringp s) (string-equal s "on"))))
     "tempo" (when (first metric) (jfloat (py-metric-tempo (first metric)) 60))
     "signatures" (when (consp sigs)
                    (json-list (loop for s in sigs
                                     when (and (consp s) (numberp (first s)) (numberp (second s)))
                                     collect (json-list (list (jint (first s)) (jint (second s)))))))
     "maxsub" (when (numberp (third metric)) (jint (third metric)))
     "loopmes" (jbool (fourth metric)))))

;;; Drapeaux (temp-marker) : dans OM 8.0 ce sont des boites de la maquette ; dans le C++ ils sont
;;; dans "markers" : offset, name, attached = [[0 debut / 1 fin, indice de la boite temporelle], ...]
(defun py-marker-json (marker boxes)
  (json-obj "offset" (jint (py-safe (offset marker)))
            "name" (json-string (let ((n (py-safe (name marker)))) (if (stringp n) n "marker")))
            "attached" (json-list (loop for item in (py-safe (tempojlist marker))
                                        for pos = (position (second item) boxes :test 'eq)
                                        when (and pos (integerp (first item)))
                                        collect (json-list (list (jint (first item)) (jint pos)))))))

(defun py-maquette-evalfunc (self)
  "Fonction d'evaluation de la maquette : interne -> \"evalfunc\", externe -> \"evalfuncpath\"."
  (let ((fun (py-safe (eval-func self))))
    (when (py-is-a fun 'OMPatch)
      (if (py-internal-ref-p fun)
          (list "evalfunc" (patch-to-json fun))
        (list "evalfuncpath" (json-string (py-dest-namestring fun "omp")))))))

(defmethod patch-json-plist ((self OMMaquette))
  (let* ((params (py-safe (params self)))
         (markers (remove-if-not #'(lambda (b) (py-is-a b 'temp-marker)) (boxes self)))
         ;; les drapeaux ne sont pas des boites C++ : indices des connexions et des drapeaux dans cette liste
         (boxes (remove-if #'(lambda (b) (py-is-a b 'temp-marker)) (boxes self))))
    (append
     (list "type" (jstr "OMMaquette")
           "name" (jstr (name self))
           "doc" (py-patch-doc self)
           "boxes" (json-list (append (mapcar #'py-save-box boxes)
                                      (py-bg-picture-boxes self)))
           "connecs" (mk-connection-list_py boxes)
           "params" (py-maquette-params params)
           "markers" (json-list (mapcar #'(lambda (m) (py-marker-json m boxes)) markers)))
     (py-safe (py-maquette-evalfunc self)))))

(defun patch-to-json (patch)
  "Patch inclus dans une boite (patch interne / maquette interne)."
  (apply #'json-obj (patch-json-plist patch)))

;;; compatibilite avec l'ancienne fonction
(defmethod om-save-py ((self OMPatch) out &optional (values? nil))
  (declare (ignore values?))
  (write-string (patch-to-json self) out))


;=====================================================
; ECRITURE DES FICHIERS
;=====================================================

(defun json-libs ()
  (json-list (mapcar #'jstr
                     (remove-duplicates
                      (remove nil (mapcar #'(lambda (l) (py-lib-name (if (stringp l) l (py-ref-name l))))
                                          *libs-to-load*))
                      :test 'string-equal))))

(defun json-resources ()
  (json-list (loop for r in *resources-to-load* collect
                   (json-obj "type" (jstr (car r))
                             "files" (json-list (loop for p in (cdr r) when p
                                                      collect (jstr (namestring p))))))))

(defun py-export-pathname (self dir ext)
  (make-pathname :device (pathname-device dir) :host (pathname-host dir)
                 :directory (pathname-directory dir)
                 :name (or (py-safe (pathname-name (mypathname self))) (name self))
                 :type ext))

(defun py-write-json-file (obj file builder)
  "Construit tout le JSON en memoire (BUILDER) puis ecrit le fichier :
en cas d'erreur, aucun fichier a moitie ecrit n'est laisse."
  (setf *saving-patch* obj
        *libs-to-load* nil
        *resources-to-load* nil)
  (unwind-protect
      (handler-case
          (let ((json (funcall builder)))
            (with-open-file (out file :direction :output
                                 :if-does-not-exist :create :if-exists :supersede
                                 :external-format :utf-8)
              (write-string json out))
            (om-print (string+ (namestring file) " exported"))
            t)
        (error (err)
          (incf *py-export-errors*)
          (om-print (format nil "ERREUR export ~A : ~A" (namestring file) err))
          nil))
    (setf *saving-patch* nil
          *libs-to-load* nil
          *resources-to-load* nil)))

(defun patch-file-json (self)
  (unless (py-safe (loaded? self))
    (load-patch self))   ; les patchs du workspace ne sont pas charges tant qu'on ne les ouvre pas
  (let ((plist (patch-json-plist self)))   ; d'abord les boites : elles remplissent *libs-to-load*
    ;; meme ordre que les fichiers OM8 : type, version, doc, ..., boxes, connecs, libraries
    (apply #'json-obj
           (append (list "type" (or (loop for (k v) on plist by #'cddr
                                          when (string= k "type") return v)
                                    (jstr "OMPatch"))
                         "version" (jstr *py-export-version*))
                   (loop for (k v) on plist by #'cddr
                         unless (string= k "type") append (list k v))
                   (list "libraries" (json-libs)   ; nom attendu par OMPatch::OMNGLoad
                         "resources" (json-resources))))))

(defun py-save-patch-file (self path ext)
  (if (not (mypathname self))
      (om-print (format nil "~A n'a pas de fichier associe : ignore." (name self)))
    (py-write-json-file self (py-export-pathname self path ext)
                        #'(lambda () (patch-file-json self)))))


;=====================================================
; save0M8 : export recursif du workspace
;=====================================================

(defmethod save0M8 ((self t) &optional (path nil))
  (declare (ignore path))
  (om-print (format nil "Export OMPython : ~A (~A) ignore."
                    (or (py-safe (name self)) "?") (type-of self))))

(defmethod save0M8 ((self OMFolder) &optional (path nil))
  (let* ((dirname (or (py-safe (car (last (pathname-directory (mypathname self)))))
                      (name self)))
         (newpath (make-pathname :device (pathname-device path) :host (pathname-host path)
                                 :directory (append (pathname-directory path) (list dirname))
                                 :name nil :type nil :version nil)))
    (om-create-directory newpath)
    (loop for item in (elements self) do
          (save0M8 item newpath))))

(defmethod save0M8 ((self OMPatch) &optional (path nil))
  (py-save-patch-file self path "omp"))

(defmethod save0M8 ((self OMMaquette) &optional (path nil))
  (py-save-patch-file self path "omm"))

(defmethod save0M8 ((self OMInstance) &optional (path nil))
  (py-write-json-file self (py-export-pathname self path "omi")
                      #'(lambda ()
                          (unless (py-safe (loaded? self)) (py-safe (load-obj-from-obj self)))
                          (let ((pl (instance-json-plist self)))
                            (unless pl
                              (error "la classe de l'instance ~A n'a pas d'equivalent C++" (name self)))
                            (apply #'json-obj pl)))))


;=====================================================
; MENU
;=====================================================

#|
(defmethod om-get-menu-context ((self metaobj-panel))
  (setf *new-obj-initial-pos* (om-mouse-position self))
  (list 
   (list 
    (apply 'om-new-menu (append (list "New...")
                                (loop for item in *new-menu-items* collect (apply 'make-menu-item (cons (om-view-window self) item)))               
                                (list (om-new-leafmenu "New Folder" #'(lambda() (omG-make-new-icon-window (om-view-window self) 'f)))))))
   (list 
    (om-new-leafmenu "Import File" #'(lambda () (import-file self)))
    (om-new-leafmenu "Import Folder" #'(lambda () (import-folder self)))
    (om-new-leafmenu "Export to OMPython" #'(lambda () (export_ws_om8 self))))
   (om-new-menu "Presentation"
                (om-new-leafmenu "Icons" #'(lambda() (omG-change-presentation (om-view-window self) 0)
                                             (om-add-menu-to-win (om-view-window self))))
                (om-new-leafmenu "List" #'(lambda() (omG-change-presentation (om-view-window self) 1)
                                            (om-add-menu-to-win (om-view-window self)))))))
|#

(defun py-export-elements (container prompt)
  (let ((dir (om-choose-new-directory-dialog :prompt prompt)))
    (when dir
      (om-create-directory dir)
      (setf *py-export-errors* 0)
      ;; racines source/destination : servent a calculer le "path" absolu des sous-patchs exportes
      (let ((*py-src-root* (or (and (py-is-a container 'OMWorkSpace)
                                    (py-safe (elements-pathname container)))
                               (py-safe (mypathname container))))
            (*py-dst-root* dir))
        (loop for item in (elements container) do
              (save0M8 item dir)))
      (om-message-dialog
       (if (zerop *py-export-errors*)
           "Export OMPython Done!."
         (format nil "Export OMPython done with ~D error(s) (cf. Listener)." *py-export-errors*))))))


(defmethod export_ws_om8 ((self omworkspace) &optional (path nil))
  (declare (ignore path))
  (py-export-elements self "choose a folder to save this workspace"))

(defmethod export_ws_om8 ((self omfolder) &optional (path nil))
  (declare (ignore path))
  (py-export-elements self "choose a folder to save this folder"))

;export just one patch:
(defmethod export_ws_om8 ((self OMPatch) &optional (path nil))
  (catch-cancel
    (let* ((name (om-choose-new-file-dialog :name (name self)    
                                            :prompt (string+ "Save " (get-object-insp-name self) " as...")
                                            :directory (or *last-exported* (om-user-home))
                                            :types (list (get-object-insp-name self) (string+ "*." (obj-file-extension self))))))
      (when name
        (setf *py-export-errors* 0)
        (setf *last-exported* (make-pathname :directory (pathname-directory name)))
        (unless (pathname-type name)
          (setf name (pathname (string+ (namestring name) "." (obj-file-extension self)))))
        (save0M8 self name)
        (om-message-dialog
         (if (zerop *py-export-errors*)
             "Export OMPython Done!."
           (format nil "Export OMPython done with ~D error(s) (cf. Listener)." *py-export-errors*)))))))

(defmethod export_ws_om8 ((self t) &optional (path nil))(print (list "export folder" self))
  (declare (ignore path))
  (om-beep-msg "Nothing to do !"))