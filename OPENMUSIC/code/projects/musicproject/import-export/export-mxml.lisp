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

;; ==================================================================================== 
;;                                musicxml export
;; ==================================================================================== 
;;
;;                                  
;;                          author : Karim Haddad
;;
;;                           $Revision: 1.1 $
;;                      $Date: 2008/06/23 15:07:04 $
;;                           $Revision: 2 $
;;                      $Date: 2015/10/05 Jean Bresson $
;;                           $Revision: 3.0 $
;;                      $Date: 2021/12/10 Karim Haddad $

;;


(defpackage "MusicXML" 
  (:use "COMMON-LISP")
  (:nicknames "MXML"))

(in-package "MXML")

(pushnew :musicxml *features*)

(defvar *xml-version* "XML 1.0")


;;; TOOLS

(defmethod in-group?  ((self om::chord)) 
  (om::group-p (om::parent self)))
(defmethod in-group?  ((self om::rest)) 
  (om::group-p (om::parent self)))
(defmethod in-group?  ((self t)) 
  nil)

(defmethod alone-in-group?  ((self om::chord)) 
  (= (length (om::inside (om::parent self))) 1))
(defmethod alone-in-group? ((self om::rest)) 
  (= (length (om::inside (om::parent self))) 1))
(defmethod alone-in-group?  ((self t))  nil)

(defmethod singelton-group? ((self om::group))
  (let* ((inside (om::inside self)))
    (and (= (length inside) 1) 
         (or (om::chord-p (car inside))
             (om::rest-p (car inside))))))

(defmethod singelton-group? ((self t)) nil)

(defmethod getratiogroup ((self om::measure))
  (list 1 1))


(defun lst->ratio (liste)
  (/ (car liste) (cadr liste)))

;;modified version of om::find-denom
(defun findenom (num durtot)
  "Find the rigth denom to ratio of tuplet."
  (if num
  (cond
   ((or (om::is-binaire? durtot)
        (om::powerof2? durtot))
    (om::get-denom 'om::bin num)) ;;;changed here from is-binaire? to powerof2?
   ((om::is-ternaire? durtot) (om::get-denom 'om::ter num))
   (t (om::get-denom-other durtot num))) 
    (list 1 1)
    ))



;;; 
#|
(defmethod getnotetype ((self om::group))
  (let* ((tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (dur (* symb-beat-val factor))
         (durtot (if (listp dur) (car dur) dur))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (unite (/ durtot (if (listp denom) (second denom) denom)))) 
    (format nil "~A" (cadr (find unite *note-types* :key 'car)))
    ))
|#

(defmethod getnotetype ((self om::group))
  "Returns unit type (note figure) of tuplet. required in <tuplet-type>" 
  (let ((unite (getratiounite self)))
    (format nil "~A" (cadr (find unite *note-types* :key 'car)))
    ))


(defmethod getratiogroup ((self om::group))
  (if (singelton-group? self) (list 1 1) ;;; when a group is in fact a single note 
    (let* ((allparents (reverse (cdr (getdemall self))))
           (dur (get-dur-group self))
           (num (om::get-group-ratio self))) ;;; check definition of get-group-ratio
      (if (not allparents) 
          (let* ((denom (findenom num dur)))
            (if (listp denom) denom
              (list num (findenom num dur)))) ;;; a voir !!!!!!!
        (let* ((fact (get-dur-group self))
               (ratios (loop for i in allparents
                             do (setf fact (* fact  (lst->ratio (getratiogroup i)))))) ;;; donne le facteur accumule des nested tup
               (denom (findenom num fact)))
             
          (if (listp denom) 
              denom
            (list num denom)
            )) 
        ))))

;;;ici il fait la meme chose que getratiogroup averc les ratios...

(defmethod getratiodur ((self om::group))
  (let* ((allparents (reverse (cdr (getdemall self))))
         (dur (get-dur-group self))
         (num (om::get-group-ratio self)))
    (if (not allparents) 
        dur
      (let* ((fact (get-dur-group self))
             (ratios (loop for i in allparents
                           do (setf fact (* fact  (lst->ratio (getratiogroup i))))));;; donne le facteur accumule des nested tup
             (denom (findenom num fact)))
        fact
        ))
    ))



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

(defun getgroupmod (self)
  (let* ((buf (om::parent self))
         (rep '()))
    (loop while (om::group-p buf)
          do (let ((ratiogroup (getratiogroup buf)))
               (push (list (car ratiogroup) (cadr ratiogroup) (getnotetype buf))
                     rep)
               (setf buf (om::parent buf))))
    rep))

(defun getallgroups (self)
"self is a chord/rest/..."
(let ((grps (om::get-real-groups self)))
  (loop for i in grps 
          collect (let ((ratiogroup (getratiogroup i)))
                    (list (car ratiogroup) (cadr ratiogroup) (getnotetype i))))))

(defmethod getratiounite ((self om::group))
  (/ (getratiodur self) (second (getratiogroup self))))

(defmethod getdemall ((self om::group))
  "For nested tuplets. Returns all group-parents to a group including the group itself" 
  (let* ((test self)
         (res (list self)))
    (loop while test 
          do (progn 
               (push (om::parent test) res)
               (if (om::measure-p (om::parent test)) 
                   (setf test nil)
                 (setf test (om::parent test)))))
    (butlast (reverse res))))


(defmethod get-dur-group ((self om::measure))
"Returns the duration of measure * whole note"
  (let* ((ext (om::extent self))
         (qval (om::qvalue self)))
    (/ ext (* qval 4))))

(defmethod get-dur-group ((self om::group))
"Returns the duration of measure * whole note.
durtot etant la duree du group parent"
  (let* ((ext (om::extent self))
         (qval (om::qvalue self)))
    (/ ext (* qval 4))))


(defun reduce-num-den-fig (list)
  "Reduces binary ratios and fig dedoubles. C-a-d 
si on a (14 8 1/16) il retourne (7 4 1/8)"
  ;(if (= (car list) (second list)) list ;dans le cas de (2 2 1/4) faut-il (1 1 1/4) ?
    (let ((res list))
      (setf res
            (list (/ (car res) 2)
                  (/ (second res) 2)
                  (* (third res) 2)))
      (if (or (om::ratiop (car res))
              (om::ratiop (second res)))
          (list (* 2 (car res))
                (* 2 (second res))
                (/ (third res) 2))
        (reduce-num-den-fig res)
        )))
;)


(defmethod get-group-info ((self om::group))
  (let* ((rat (om::first-n (getratiogroup self) 2))
         (unit (getratiounite self))
         (reduce (reduce-num-den-fig (om::flat (list rat unit)))))
    (list (car reduce) (second reduce) 
          (format nil "~A" (cadr (find (third reduce) *note-types* :key 'car))))
    ))



(defmethod get-group-info ((self om::measure))
  (list 1 1))




#|
(defun time-modifications (self)
  (if (and (in-group? self) (not (alone-in-group? self)))
      (let ((ratio (butlast (get-group-info (om::parent self)))))
        (group (car ratio) (second ratio)))
    NIL))


(defun time-modifications (self)
  (if (and (in-group? self) (not (alone-in-group? self)))
      (let* ((lvl (get-grp-level self))
             (ratio (getratiogroup (om::parent self)))
             (act-note (second lvl))
             (norm-note (third lvl))
             (indx (car lvl))
             (numdenom (getallgroups self))
             (simpli (/ act-note norm-note)))
        (if (not (= (/ act-note norm-note) 1))
            (group act-note norm-note)
          NIL))
    NIL))


(defun time-mod-val (obj)
  (let* ((info (getgroupmod obj))
        (ratios (reverse (mapcar 'butlast info)))
        (clone (om::clone ratios)))
    (om::while (and clone (equal (first (car clone)) (second (car clone))))
          (pop clone))
    (car clone)))
|#


(defun mult-rat (liste)
  (let ((trans (om::mat-trans liste)))
    (loop for i in trans
        collect (apply '* i))))

(defun time-mod-val (obj)
  (let* ((info (getgroupmod obj))
        (ratios (reverse (mapcar 'butlast info))))
    (mult-rat ratios)))

(defun time-modifications (self)
  (let ((ratio (time-mod-val self)))
  (if ratio 
      (group (car ratio) (second ratio))
  nil)))


;;============================================


(defun first-of-this-group (self grp)
       (let ((frst (car (om::collect-chords grp))))
         (equal self frst)))

(defun last-of-this-group (self grp)
       (let ((lst (car (reverse (om::collect-chords grp)))))
         (equal self lst)))

(defun tied? (self)
  (or (and (not (om::cont-chord-p self))
           (om::cont-chord-p (om::next-container self '(om::chord))))
      (and (om::cont-chord-p self)
           (not (om::cont-chord-p (om::next-container self '(om::chord)))))
      (and (om::cont-chord-p self)
           (om::cont-chord-p (om::next-container self '(om::chord))))))


(defun tied (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         "<tie type=\"start\"/>")
        ((and (om::cont-chord-p self)
             (not (om::cont-chord-p (om::next-container self '(om::chord)))))
        "<tie type=\"stop\"/>")
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
        "<tie type=\"stop\"/><tie type=\"start\"/>")
       (t nil))) ;;;;the nil thing is comin from here

(defun tied-notation (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         "<tied type=\"start\"/>")
        ((and (om::cont-chord-p self)
             (not (om::cont-chord-p (om::next-container self '(om::chord)))))
        "<tied type=\"stop\"/>")
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
        "<tied type=\"stop\"/><tied type=\"start\"/>")
       (t NIL))) ;;;;the nil thing is comin from here


(defun group (num denom)
  (list "<time-modification>"
        (list (format nil "<actual-notes>~A</actual-notes>" num)
              (format nil "<normal-notes>~A</normal-notes>" denom))
        "</time-modification>"))

(defun firstofgroup (num denom notetype nbr)
  (list 
   (format nil "<tuplet bracket=\"yes\" number=\"~A\" placement=\"above\" type=\"start\">" nbr)
   (list "<tuplet-actual>"
         (list (format nil "<tuplet-number>~A</tuplet-number>" num)
               (format nil "<tuplet-type>~A</tuplet-type>" notetype))
         "</tuplet-actual>"
         "<tuplet-normal>"
         (list (format nil "<tuplet-number>~A</tuplet-number>" denom)
               (format nil "<tuplet-type>~A</tuplet-type>" notetype))
         "</tuplet-normal>")
   "</tuplet>"))

(defun lastofgroup (nbr)
  (list (format nil "<tuplet number=\"~A\" type=\"stop\"/>" nbr)))

;; disons que tous les char sont des accents :)
(defun accent? (self) (om::get-extras self "char"))

(defun accent-notation (self)
  (list "<articulations>"
        (list "<accent default-x=\"-1\" default-y=\"-61\" placement=\"below\"/>")
        "</articulations>"))

(defun groupnotation (self)
  (list "<notations>"
        (if (in-group? self)
            (let* ((lvl (get-grp-level self))
                   (ratio (getratiogroup (om::parent self)))
                   (act-note (second lvl))
                   (norm-note (third lvl))
                   (indx (car lvl))
                   (numdeno (getallgroups self))
                   (numdenom (remove nil 
                                     (loop for i in numdeno
                                           collect (if (not (= 1 (/ (car i) (second i)))) i) 
                                           )))
                   (simpli (/ act-note norm-note)))
              (cond 
               ((and (om::last-of-group? self) (om::first-of-group? self));singelton
                (list (tied-notation self)
                      (when (accent? self) (list (accent-notation self)))))
               ((om::first-of-group? self) 
                (progn 
                  (remove nil 
                          (append 
                           (let ((obj self)
                                 (indx (+ (length numdenom) 1)))
                                  
                             (remove nil (loop for i in (reverse numdenom)           
                                               append (progn 
                                                        (setf obj (om::parent obj))
                                                        (setf indx (- indx 1))
                                                        (firstofgroup (car i) (second i) (third i) indx)
                                                        ))))
                           (list (tied-notation self)
                                 (when (accent? self) (accent-notation self)))))
                  )
                )

               ((om::last-of-group? self);ici 
                (remove nil 
                        (append 
                         (let ((obj self)
                               (indx (+ (length numdenom) 1)))
                           (remove nil (loop for i in numdenom           
                                             append (progn 
                                                      (setf obj (om::parent obj))
                                                      (setf indx (- indx 1))
                                                      (lastofgroup indx)))))
                         (list (tied-notation self)
                               (when (accent? self) (accent-notation self)))))
                        )
                
               (t ())))
             
           
          (when (or (tied? self) (accent? self))
            (remove nil 
                    (list 
                     (when (tied? self) (tied-notation self))
                     (when (accent? self) (accent-notation self)))))
          )
        
        ;;; VEL
        (velocity-as-xml self)
        "</notations>"
        ))


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

(defmethod donne-figure ((self t)) 0)

(defun makebeam (self)
  (let* ((beamself (donne-figure self))
         (beamprev (donne-figure (prv-cont self)))
         (beamnext (donne-figure (nxt-cont self))))
    (remove nil (list
                 (cond ((and (in-group? self)
                             (not (om::first-of-group? self))
                             (not (om::last-of-group? self))
                             (> beamself 0))
                        (cond ((and (> beamprev 0) (> beamnext 0))
                               "<beam>continue</beam>")
                              ((and (> beamprev 0) (not (> beamnext 0)))
                               "<beam>end</beam>")
                              ((and (not (> beamprev 0)) (> beamnext 0))
                               "<beam>begin</beam>")))
                       ((and (om::first-of-group? self) (> beamself 0))
                        (if (and (in-group? (prv-cont self))
                                 (> beamprev 0) (> beamnext 0) (prv-is-samegrp? self))
                            "<beam>continue</beam>" "<beam>begin</beam>"))
                       ((and (om::last-of-group? self) (> beamself 0))
                        (if (and (in-group? (nxt-cont self))
                                 (> beamprev 0) (> beamnext 0) (nxt-is-samegrp? self))
                            "<beam>continue</beam>" "<beam>end</beam>"))
          
                       (t NIL)) 
                 ))
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


;;;--------<PITCH>--------

(defparameter *kascii-note-C-scale*
  (mapc #'(lambda (x) (setf (car x) (string-upcase (string (car x)))))
    '((c . :n) (c . :h) (c . :q) (c . :hq) (c . :s) (c . :hs) (c . :qs) (c . :hqs)
      (d . :n) (d . :h) (d . :q) (d . :hq) (d . :s) (d . :hs) (d . :qs) (d . :hqs)
      (e . :n) (e . :h) (e . :q) (e . :hq)
      (f . :n) (f . :h) (f . :q) (f . :hq) (f . :s) (f . :hs) (f . :qs) (f . :hqs)
      (g . :n) (g . :h) (g . :q) (g . :hq) (g . :s) (g . :hs) (g . :qs) (g . :hqs)
      (a . :n) (a . :h) (a . :q) (a . :hq) (a . :s) (a . :hs) (a . :qs) (a . :hqs)
      (b . :n) (b . :h) (b . :q) (b . :hq))))

(defparameter *kascii-note-scales* (list *kascii-note-C-scale*))

(defparameter *kascii-note-alterations*
   '((:s 1 +100) (:f -1 -100)
     (:q 0.5 +50) (:qs 1.5 +150) (:-q -0.5 -50) (:f-q -1.5 -150)
     (:h 0.25 +25) (:hq 0.75 +75) (:hs 1.25 +125) (:hqs 1.75 +175) (:-h -0.25 -25) (:-hq -0.75 -75)(:-hs -1.25 -125)(:-hqs -1.75 -175)
     (:n 0 0)))


(defparameter *note-accidentals*
  '((0.25 natural-up)
    (0.5 quarter-sharp) 
    (0.75 sharp-down)
    (1 sharp)
    (1.25 sharp-up)
    (1.5 three-quarters-sharp)
    (1.75 sharp-three)
    ))

; (mc->xmlvalues 6548 4)

(defun mc->xmlvalues (midic &optional (approx 2))
  "Converts <midic> to a string representing a symbolic ascii note."
  (let* ((kascii-note-scale (car *kascii-note-scales*))
         (dmidic (/ 1200 (length kascii-note-scale))) 
         (vals (multiple-value-list (round (om::approx-m midic approx) dmidic)))
         (midic/50 (car vals))
         (cents (cadr vals))
         (vals2 (multiple-value-list (floor (* midic/50 dmidic) 1200)))
         (oct+2 (- (car vals2) 1))
         (midic<1200 (cadr vals2))
         (note (nth (/ midic<1200 dmidic) kascii-note-scale))
         (alt (cdr note)))
    (list midic
          (coerce (car note) 'character) 
          (cadr (find alt *kascii-note-alterations* :key 'car))
          oct+2)))




;;;--------</PITCH>--------

;;;--------<NOTE HEADS>--------
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

    (if (> val 1) 
        (list (/ char 1) points)
      (list (/ 1 char) points))
    ))

(defparameter *note-types*
  '((2 breve) (1 whole)
    (1/2 half) (1/4 quarter)
    (1/8 eighth) (1/16 16th)
    (1/32 32nd) (1/64 64th)
    (1/128 128th) (1/256 256th)
    (1/512 512th)(1/1024 1024th)))


;;;-------</EXTRAS>------------------
;;;-------</EXTRA NOTE HEADS>--------
;By courtesy of jialinliu

(setf *om-head-text=>xml-head-text*    ;for finale v26  .. before dot. is the charactor for the notehead in om (see the font OMheads), after dot, is the expression of notehead in finale (engrave->export->find "notehead"). You could add items into this list..  
      '(("`" . ">x<")
        ("b" . " filled=\"no\">rectangle<")
        ("e" . " filled=\"yes\">rectangle<")
        ("d" . " filled=\"yes\">diamond<")
        ("c" . " filled=\"no\">diamond<")
        ("i" . " filled=\"yes\">circle<")
        ("h" . " filled=\"no\">circle<")
        ("f" . " filled=\"yes\">triangle<")
        ("g" . " filled=\"no\">triangle<")
        ))

(defun head-extras-as-xml (self) 
  (when (om::get-extras self "head")
    (list (format nil "<notehead~A/notehead>" (cdr (assoc (om::thehead (car (om::get-extras self "head"))) *om-head-text=>xml-head-text* :test #'equal))))))

;;;-------</EXTRA VELS>------------
;By courtesy of jialinliu

(setf *om-vel-text=>xml-dyn-text*    ;for finale v26  .. is the keyword for the dynamics in om (see the vel-extra), and expression of dynamics in finale (engrave->export->find "dynamics"), at the end is the velocity value. 
      '((:fff "fff" 127)
        (:ff "ff" 101)
        (:f "f" 88)
        (:mf "mf" 75)
        (:mp "mp" 62)
        (:p "p" 49)
        (:pp "pp" 36)
        (:ppp "ppp" 23)))

(defun vel-extras-as-xml (self) 
  (when (om::get-extras self "vel")
    (let ((dyn-info (cdr (assoc (om::dynamics (car (om::get-extras self "vel"))) *om-vel-text=>xml-dyn-text*))))
      (list "<direction placement=\"below\">"
            "<direction-type>"
            "<dynamics default-x=\"310\" default-y=\"-106\" halign=\"center\">"
            (format nil "<~A/>" (car dyn-info))
            "</dynamics>" 
            "</direction-type>"
            (format nil "<sound dynamics=\"~d\"/>" (cadr dyn-info))
            "</direction>"
            ))))

;;;-------</EXTRA TEXT>------------

(defun text-extras-as-xml (self) 
  (when (om::get-extras self "text")
    (list "<lyric default-y=\"-80\" justify=\"left\" number=\"1\">" 
          (list  "<syllabic>single</syllabic>"
                 (format nil "<text>~A</text>" (om::thetext (car (om::get-extras self "text"))))
                 ;"<extend type=\"start\"/>"
                 )
          "</lyric>")))

;maybe not needed anymore (see above):
(defmethod velocity-as-xml ((self om::chord))
  (when (om::get-extras self "vel")
    
    (let* ((ex (car (om::get-extras self "vel")))
           (schar (or (om::dynamics ex)
                      (om::get-dyn-from-vel (om::get-object-vel (om::object ex))))))
      (list (format nil "<dynamics placement=\"below\"><~A/></dynamics>" schar)))))

(defmethod velocity-as-xml ((self om::rest)) nil)

(defun midi-vel-to-mxml-vel (vel)
  (round (* (/ vel 90.0) 100)))

;;;================
;;; CHORD / NOTES
;;;================

;;; new here in order to put the correct <duration> according to <divisions> of each measure.
;;; for compatibility 

;(defmethod get-xml-duration ((self t))
;  (* (/ (om::extent self) 4) (/ 1 (om::qvalue self))))

(defmethod get-xml-duration ((self t))
  (* (mesure-divisions (get-parent-measure self)) 
     (/ (om::extent self) (om::qvalue self))))

  
(defmethod cons-xml-expr ((self om::chord) &key free key (approx 2) part)
  (let* (;; (dur free) 
         (dur (if (listp free) (car free) free))
         (head-and-pts (get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) *note-types* :key 'car)))
         (nbpoints (cadr head-and-pts))
         (durtot (get-xml-duration self))  ;;; !!!!
         (inside (om::inside self)))
    (loop for note in inside 
           for i = 0 then (+ i 1) append 
           (let* ((note-values (mc->xmlvalues (om::midic note) approx))
                  (step (nth 1 note-values))
                  (alteration (nth 2 note-values))
                  (octave (nth 3 note-values))
                  (extra-head (head-extras-as-xml note))
                  )
                         
             (list (format nil "<note dynamics=\"~D\">" (midi-vel-to-mxml-vel (om::vel note)))
                   (unless (= i 0) "<chord/>") ;;; if this is not the first note in the chord
                   (remove nil 
                           (append 
                            (list "<pitch>"
                                  (remove nil 
                                          (list (format nil "<step>~A</step>" step)
                                                (when alteration (format nil "<alter>~A</alter>" alteration))
                                                (format nil "<octave>~A</octave>" octave)))
                                  "</pitch>"
                                  (format nil "<duration>~A</duration>" durtot)
                                  (tied self) ;;; ties (performance)
                                  (let ((headstr (format nil "<type>~A</type>" note-head)))
                                    (loop for i from 1 to nbpoints do (setf headstr (concatenate 'string headstr "<dot/>")))
                                    headstr)
                                  (when (find alteration *note-accidentals* :key 'car)  ;;; accidental (if any)
                                    (format nil "<accidental>~A</accidental>" (cadr (find alteration *note-accidentals* :key 'car))))
                                 ; (format nil "<instrument id=\"P~D-I~D\"/>" part (om::chan note))
                                  )
                            (time-modifications self)
                            (if extra-head extra-head)
                            (makebeam self) 
                            (groupnotation self)
                            (when (= i 0) (text-extras-as-xml self))
                            ))
                   "</note>")
             ))))

   
(defmethod cons-xml-expr ((self om::rest) &key free key (approx 2) part)
  (let* ((dur (if (listp free) (car free) free))
         (head-and-pts (get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) *note-types* :key 'car)))
         (nbpoints (cadr head-and-pts))
         (durtot (get-xml-duration self)))
    (list "<note>" 
          "<rest/>"
          (remove nil
                  (list 
                   (format nil "<duration>~A</duration>" durtot)
                   (let ((headstr (format nil "<type>~A</type>" note-head)))
                     (loop for i from 1 to nbpoints do (setf headstr (concatenate 'string headstr "<dot/>")))
                     headstr)
                   (time-modifications self)
                   (makebeam self)
                   (groupnotation self)))
          "</note>")))


;;;===================================
;;; RECURSIVE CONTAINERS (JB 29/09/15)
;;;===================================


(defmethod cons-xml-expr ((self om::group) &key free key (approx 2) part)
  
  (let* ((durtot (if (listp free) (car free) free))
         (cpt (if (listp free) (cadr free) 0))
         (num (or (om::get-group-ratio self) (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (cadr denom) denom))
         (unite (/ durtot denom)))

    (cond
     
     ((not (om::get-group-ratio self))
      
      (loop for obj in (om::inside self) append 
            (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                               (/ (om::extent self) (om::qvalue self)))))
              (cons-xml-expr obj :free (* dur-obj durtot) :approx approx :part part))))
     
     ((= (/ num denom) 1)
      
      (loop for obj in (om::inside self) 
            append
            (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                 (/ (om::extent self) (om::qvalue self))))
                   (dur-obj (* num operation)))                     
              (cons-xml-expr obj :free (* dur-obj unite) :approx approx :part part)))    ;;;; ACHTUNG !!
      )
     
     (t
      (let ((depth 0) (rep nil))
        (loop for obj in (om::inside self) do
              (setf rep (append rep 
                                (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                     (/ (om::extent self) (om::qvalue self))))
                                       (dur-obj (* num operation))
                                       (tmp (multiple-value-list 
                                             (cons-xml-expr obj :free (list (* dur-obj unite) cpt)
                                                            :approx approx :part part)))
                                       (exp (car tmp)))
                                  
                                  (when (and (cadr tmp) (> (cadr tmp) depth))
                                    (setf depth (cadr tmp)))
                                  exp))))
        (values rep (+ depth 1))
        ))
      
     )))


;;;; <divisions> problem....
;;;finale's value to be tested on Sibelius.... (768)
;;;sibelius ' value is 256....


(defun list-pgcd (list)
  (let ((res (car list)))
    (loop for deb in (cdr list)
          do (setf res (om::pgcd res deb)))
    res))

(defmethod mesure-divisions ((self om::measure))
  (let* ((ratios (om::tree2ratio (list '? (om::om-round (list (om::tree self))))))
         (timesig (car (om::tree self)))
         (num (car timesig))
         (denom (second timesig))
         (comp (om::x-append 1/4 ratios))    ;;;1/4 pour l'instant...1/4 etant le denom de la time signature
         (props (om::om* (om::om/ 1 (list-pgcd ratios)) comp)))
    (* num (car props))))


(defmethod cons-xml-expr ((self om::measure) &key free (key '(G 2)) (approx 2) part)
  (let* ((mesnum free) 
         (inside (om::inside self))
         (tree (om::tree self))
         (signature (car tree))
         (real-beat-val (/ 1 (om::fdenominator signature)))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator signature)))))
    (list (format nil "<measure number=\"~D\">" mesnum)
          (append (remove nil
                          (list "<attributes>"
                                (list (format nil "<divisions>~A</divisions>" (mesure-divisions self))  ;;; (caar (dursdivisions self)))
                                      "<key>"
                                      (remove nil
                                              (list  "<fifths>0</fifths>"
                                                     (if (and approx (= approx 2)) "<mode>major</mode>")))
                                      "</key>"
                                      "<time>"
                                      (list (format nil "<beats>~D</beats>" (car signature))
                                            (format nil "<beat-type>~D</beat-type>" (cadr signature)))
                                      "</time>")
                                (and key
                                     (list "<clef>"
                                           (list (format nil "<sign>~:@(~a~)</sign>" (car key))
                                                 (format nil "<line>~D</line>" (cadr key)))
                                           "</clef>"
                                           ))
                                "</attributes>"))
                  (loop for obj in inside ;for fig in (cadr (dursdivisions self))  ;;;;;transmetre les note-types
                        append
                        (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                               (factor (/ (* 1/4 dur-obj-noire) real-beat-val))) 
                          (cons-xml-expr obj :free (* symb-beat-val factor) :approx approx :part part) ;;; NOTE: KEY STOPS PROPAGATING HERE
                          )))
          "</measure>"
          "<!--=======================================================-->")))


(defmethod cons-xml-expr ((self om::voice) &key free (key '(G 2)) (approx 2) part)
  (let ((voicenum part)
        (measures (om::inside self)))
    (list (format nil "<part id=\"P~D\">" voicenum)
          (loop for mes in measures
                for i = 1 then (+ i 1)
                collect (cons-xml-expr mes :free i :key key :approx approx :part part))
          "<!--=======================================================-->"
          "</part>")))

(defun mxml-header () 
  (list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 1.1 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">"))

(defun get-midi-channels (voice)
  (sort (remove-duplicates (mapcar 'om::ev-chan (om::get-midievents voice #'(lambda (evt) (om::test-type evt :keyon))))) '<))

(defmethod cons-xml-expr ((self om::poly) &key free (key '((G 2))) (approx 2) part)
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
                      (let ((channels (get-midi-channels v)))
                        `(
                          ,(format nil "<score-part id=\"P~D\">" voice-num)
                          (,(format nil "<part-name>Part ~D</part-name>" voice-num))
                          
                          ,(loop for ch in channels append
                                 `(
                                   ,(format nil "<score-instrument id=\"P~D-I~D\">" voice-num ch)
                                   ("<instrument-name>MusicXML Default 1</instrument-name>")
                                   "</score-instrument>"
                                   ))
                          ,(loop for ch in channels append
                                 `(
                                   ,(format nil "<midi-instrument id=\"P~D-I~D\">" voice-num ch)
                                   ,(format nil "<midi-channel>~D</midi-channel>" ch)
                                  ; "<midi-program>1</midi-program>")
                                   "</midi-instrument>")
                                 )
                          "</score-part>")
                        )
                      )
                "</part-list>")
          "<!--===================================================================-->"
          (if (= 1 (length key))
              ;;; SAME KEY FOR ALL VOICES
              (loop for v in voices
                    for i = 1 then (+ i 1) 
                    append 
                    (cons-xml-expr v :part i :key (car key) :approx approx))
            ;;; EACH VOICE HAS A KEY
            (loop for v in voices 
                  for i = 1 then (+ i 1)
                  for k in key 
                  append
                  (cons-xml-expr v :part i :key k :approx approx)))
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
  (WITH-OPEN-FILE (out path :direction :output 
                       :if-does-not-exist :create :if-exists :supersede)
    (loop for line in (mxml::mxml-header) do (format out "~A~%" line))
    (recursive-write-xml out list -1)))

(defmethod xml-export ((self t) &key keys approx path name) nil)

(defmethod xml-export ((self voice) &key keys approx path name) 
  (xml-export (make-instance 'poly :voices self) :keys keys :approx approx :path path :name name))
 
(defmethod xml-export ((self poly) &key keys approx path name) 
  (let* ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :name name 
                                                       :prompt "New XML file"
                                                       :types '("XML Files" "*.xml")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (write-xml-file (mxml::cons-xml-expr self :free 0 :key keys :approx approx) pathname)
      pathname)))


(defmethod! export-musicxml ((self t) &optional (keys '((G 2))) (approx 2) (path nil))
  :icon 351
  :indoc '("a VOICE or POLY object" "list of voice keys" "tone subdivision approximation" "a target pathname")
  :initvals '(nil ((G 2)) 2 nil)
  :doc "
Exports <self> to MusicXML format.

- <keys> defines the staff
- <approx> is the microtonal pitch approximation
- <path> is a pathname to write the file in
"
  (xml-export self :keys keys :approx approx :path path))

(defmethod! export-musicxml ((self poly) &optional (keys '(("G" 2))) (approx 2) (path nil)) (call-next-method))

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
