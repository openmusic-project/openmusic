;; ============================================================================================
;;                                PAQUET   Systemes dynamiques
;; ============================================================================================

;;          V1.3
;;                                               functions by Mikhail Malt   1998 Paris IRCAM



;========================================================================================

 
(in-package "ALEA")

;============================================================
;============fractus=========================================
;============================================================
(om::defmethod!  alea::midpoint1  ((liste1 list) (niveaux  integer)
                                   (prc-x number) (prc-y number)) 


:initvals '(nil 1 0 0)
  :indoc '("list seed"  "recursive deep of calculus" 
           "alea pourcent variation in the x axis" "alea pourcent variation in the y axis")
  :icon 244 
  :numouts 3
  :doc "Constructs a list of points with their x and y locations based on the algorithm of movement of the mean.
- list1 is a list of lists, where each sub-list is a pair of values indicating the coordinates of fixed points; list1 may also be a BPF, in this case the coordinates of the points will be extracted and used as data;
- niveaux is a whole number which indicates the depth of the transformation of list1;
- prc-x is the percentage of random perturbation of the 'x' values;
- prc-y is the percentage of random perturbation of the 'y' values.
In this version the perturbation is based on a uniform distribution.
The first output of this module is a list of coordinates:
( (x0 y0 ) (x1 x2 ) ... (xn yn )),
the second input is a list of the x values and the third one a list of the y values"
  (let ((aux) (xnew) (ynew) 
        )
    (dotimes (n niveaux liste1)
      (dotimes (m (1- (length liste1)) )
        (setf xnew (om::perturbation
                    (/ (+ (first (nth m liste1))
                          (first (nth (1+ m) liste1)))
                          2)
                    prc-x))
        
        (setf ynew (om::perturbation
                    (/ (+ (second (nth m liste1))
                          (second (nth (1+ m) liste1)))
                          2)
                    prc-y))
        (push (list xnew ynew) aux))
      (setf liste1 (om::sort. (append aux liste1) '< 'first))
      (setf aux nil))
(values liste1 (first (om::mat-trans liste1)) (second (om::mat-trans liste1) ))))


(om::defmethod!  alea::midpoint1  ((liste1 om::bpf) (niveaux  integer)
                                   (prc-x number) (prc-y number)) 


:initvals '(nil 1 0 0)
  :indoc '("list seed"  "recursive deep of calculus" 
           "alea pourcent variation in the x axis" "alea pourcent variation in the y axis")
  :icon 244 
  :numouts 3
  :doc "Constructs a list of points with their x and y locations based on the algorithm of movement of the mean.
- list1 is a list of lists, where each sub-list is a pair of values indicating the coordinates of fixed points; list1 may also be a BPF, in this case the coordinates of the points will be extracted and used as data;
- niveaux is a whole number which indicates the depth of the transformation of list1;
- prc-x is the percentage of random perturbation of the 'x' values;
- prc-y is the percentage of random perturbation of the 'y' values.
In this version the perturbation is based on a uniform distribution.
The first output of this module is a list of coordinates:
( (x0 y0 ) (x1 x2 ) ... (xn yn )),
the second input is a list of the x values and the third one a list of the y values"

(alea::midpoint1 (alea::paires liste1)  niveaux prc-x prc-y))




(om::defmethod! gauss ((mu number ) (sigma number )) 
  :initvals '(0.0  1.0)
  :indoc '("mean"  "bandwidth" )
  :icon 240 
  :doc "Generates a number with a gaussian distribution of an average <mu> and a bandwidth <sigma>.
It is important to know that the gaussian distribution is not bounded,
 and that 99.74 % of the results falls between -3*sigma and +3*sigma,
 but for the present algorithm, the results will be bounded
 between  -6*sigma and +6*sigma. In most cases this aproximation 
is acceptable, since only two results out of a milliard fall out
 of these limits in a true gaussian processus."
  (let ((s 0))
    (om::for (n 1 1 12)  (setf s (+ s (random 1.0))))
    (+ (* (- s 6) sigma) mu)))



(om::defmethod!  alea::midpoint2  ((liste1 list) (niveaux  integer)
                         (sig-x number) (sig-y number)) 

:initvals '(nil 1 0 0)
  :indoc '("list seed"  "recursion deep" 
           "sigma bandwidth variation in the x axis" "sigma bandwidth variation in the y axis")
  :icon 244 
  :numouts 3
  :doc "Constructs a list of points with their x and y locations based on the algorithm of movement of the mean.
- list1 is a list of lists, where each sub-list is a pair of values indicating the coordinates of fixed points; list1 may also be a BPF, in this case the coordinates of the points will be extracted and used as data;
- niveaux is a whole number which indicates the depth of the transformation of list1;
- sig-x is the parameter of dispersion for the gaussian variation introduced into the 'x' values;
- sig-y is the parameter of dispersion for the gaussian variation introduced into the 'y' values.
In this version the perturbation is based on a gaussian distribution.
The first output of this module is a list of coordinates:
( (x0 y0 ) (x1 x2 ) ... (xn yn )),
the second input is a list of the x values and the third one a list of the y values"
  (let ((aux) (xnew) (ynew)
        (liste1 (if (listp liste1) liste1 (alea::paires liste1))))
    (dotimes (n niveaux liste1)
      (dotimes (m (1- (length liste1)) )
        (setf xnew (+ (alea::gauss  0 sig-x)
                    (/ (+ (first (nth m liste1))
                          (first (nth (1+ m) liste1)))
                          2)
                    ))
        
        (setf ynew (+ (alea::gauss  0 sig-y)
                    (/ (+ (second (nth m liste1))
                          (second (nth (1+ m) liste1)))
                          2)
                   ))
        (push (list xnew ynew) aux))
      (setf liste1 (om::sort. (append aux liste1) '< 'first))
      (setf aux nil))

(values liste1 (first (om::mat-trans liste1)) (second (om::mat-trans liste1) ))))




;ATTENTION MIDPOINT2 UTILISE GAUSS!!
;;;=========================================================
;;; FRACT-GEN!!!!!!!!!!!!!!!!!!!!!!!!
;==================================================================
;     FONCTIONS AUXILIAIRES
;==================================================================

(om::defmethod! distance ((xo number) (yo  number) (x1 number) ( y1 number))
:initvals '(0 0 100 100)
  :indoc '("first x value"  "first y value" "second x value" "second y value")
  :icon 249 
  :doc "the distance between xo yo and x1 y1
Calculates the Euclidean distance between two points in 
the same plane at coordinates xo yo and x1 y1 .
"
(sqrt (+ (*  (- x1 xo) (- x1 xo))
         (*  (- y1 yo) (- y1 yo)))))

(om::defmethod! angle ((xo number) (yo  number) (x1 number) ( y1 number))

:initvals '(0 0 100 100)
  :indoc '("first x value"  "first y value" "second x value" "second y value")
  :icon 248 
  :doc "Calculates the angle in radians in the plane formed from the line segment made by two points at coordinates (xo yo) and (x1 y1) and the x-axis."
(cond ((and (= 0 (- x1 xo)) (> (- y1 yo) 0)) (/ pi 2))
      ((and (= 0 (- x1 xo)) (< (- y1 yo) 0)) (/ (* 3 pi) 2))
      ((and (> 0 (- x1 xo)) (= (- y1 yo) 0)) pi)
      ((and (< 0 (- x1 xo)) (= (- y1 yo) 0)) 0)
      
      ((and (< (- y1 yo) 0) (< (- x1 xo) 0)) (+ pi (atan (/ (- y1 yo) (- x1 xo)))))
      
      ((and (>= (- y1 yo) 0) (< (- x1 xo) 0)) (+ pi (atan (/ (- y1 yo) (- x1 xo)))))
      
      (t (atan (/ (- y1 yo) (- x1 xo))))))




;LA SORTIE DE <INTERPOINTS> EST UNE DOUBLE LISTE DES POINT ENTRE
;<XO, YO> (INCLUSIF) ET <X1,Y1> (EXCLUSIF)

;==================================================================
(om::defmethod! fract-gen ((xlist list) (ylist list) (int integer) &OPTIONAL tab) 

  :initvals '((0 50 100) (0 50 25) 1 nil)
  :indoc '("x value list"  "y value list" "recursion deep of the process" "table with a graphic object as seed")
  :icon 244 
  :numouts 3
  :doc "generation de courbes fractales a partir de donnees graphiques
The first output of this module is a list of coordinates:
( (x0 y0 ) (x1 x2 ) ... (xn yn )),
the second input is a list of the x values and the third one a list of the y values"

  (let* ((xlist (if tab (om::x-points tab) xlist))
         (ylist (if tab (om::y-points tab) ylist))
         (dno (distance(first xlist) (first ylist) 
                    (om::last-elem xlist) (om::last-elem ylist)))
         (radius nil) 
         (teta nil)  
         (list-aux))
    ;construction des transformations a partir de la courbe 'graine'
    (dotimes (n (1- (length xlist)))
      (push (distance(nth n xlist) (nth n ylist) (nth (1+ n) xlist) (nth (1+ n) ylist)) radius)
      (push (angle (nth n xlist) (nth n ylist) (nth (1+ n) xlist) (nth (1+ n) ylist)) teta))
    (setf radius (reverse radius))
    (setf teta (reverse teta))
    ;construction des iterations
    (dotimes ( n int list-aux)
      (setf list-aux nil)
      (dotimes (m (- (length xlist) 1)   (reverse list-aux))
        (let ((factcomp (/ (distance(nth m xlist) (nth m ylist) (nth (1+ m) xlist) (nth (1+ m) ylist)) dno) )
              (teta2 (angle (nth m xlist) (nth m ylist) (nth (1+ m) xlist) (nth (1+ m) ylist)))
              (px (nth m xlist)) (py (nth m ylist)) )
          (push (list px py) list-aux)
          (dotimes (n  (1- (length radius)) (reverse list-aux))
            ;calcule des points en coordonnees absolutes et
            ; transformation en fonction de la droite forme a partir
            ;du segment de droite forme par  deux points de 
            ;coordonnees <xo yo> et <x1 y1>
            (push (list 
                   (setf px (+ (* (nth n radius) 
                                  factcomp 
                                  (cos (+ teta2 (nth n teta))))
                               px))
                   (setf py (+ (* (nth n radius) 
                                  factcomp 
                                  (sin (+ teta2 (nth n teta))))
                               py)))
                  list-aux))))
      (push (list (om::last-elem xlist) (om::last-elem ylist)) list-aux)
      (setf xlist  (first (om::mat-trans (reverse list-aux))))
      (setf ylist (second (om::mat-trans (reverse list-aux)))))
(values (reverse list-aux) (first (om::mat-trans (reverse list-aux))) (second (om::mat-trans (reverse list-aux)) ))))


(om::defmethod! alea::fract-gen1 ((obj1 t)
                                  (int integer)  
                              &OPTIONAL obj2) 

  :initvals '((0 50 100) 1 1 nil)
  :indoc '("first object, as a list of pairs or as a bpf object"   
           "recursion deep of the process" 
           "second object, as a list of pairs or as a bpf object")
  :icon 244 
  :numouts 3
  :doc "Generates the coordinates of points on a fractal curve, based on graphical data.
- obj1 is the pairs of coordinates or a BPF;
- int is the number of iterations;
- obj2 is the pairs of coordinates or a BPF.
The fract-gen1 module applies the figure, or object, defined by obj1 onto itself or onto a second object, obj2, if that optional input has been opened.
The first output of this module is a list of coordinates:
( (x0 y0 ) (x1 x2 ) ... (xn yn )),
the second input is a list of the x values and the third one a list of the y values"
  (let* 
    ((xlist1 (if (listp obj1) (first (om::mat-trans obj1)) (om::x-points obj1)))
     (ylist1 (if (listp obj1) (second (om::mat-trans obj1)) (om::y-points obj1)))
     (xlist2 (if obj2 (if (listp obj2) (first (om::mat-trans obj2)) (om::x-points obj2)) xlist1))
     (ylist2 (if obj2 (if (listp obj2) (second (om::mat-trans obj2)) (om::y-points obj2)) ylist1))

     (dno (alea::distance(first xlist1) (first ylist1) 
                         (om::last-elem xlist1) (om::last-elem ylist1)))
     (radius nil) 
     (teta nil)  
     (list-aux))

    ;construction des transformations a partir de la courbe 'graine'

    (dotimes (n (1- (length xlist1)))
      (push (alea::distance(nth n xlist1) (nth n ylist1) (nth (1+ n) xlist1) (nth (1+ n) ylist1)) radius)
      (push (alea::angle (nth n xlist1) (nth n ylist1) (nth (1+ n) xlist1) (nth (1+ n) ylist1)) teta))
    (setf radius (reverse radius))
    (setf teta (reverse teta))

    ;construction des iterations

    (dotimes ( n int list-aux)
      (setf list-aux nil)
      (dotimes (m (- (length xlist2) 1)   (reverse list-aux))
        (let ((factcomp (/ (alea::distance(nth m xlist2) (nth m ylist2) (nth (1+ m) xlist2) (nth (1+ m) ylist2)) dno) )
              (teta2 (alea::angle (nth m xlist2) (nth m ylist2) (nth (1+ m) xlist2) (nth (1+ m) ylist2)))
              (px (nth m xlist2)) (py (nth m ylist2)) )
          (push (list px py) list-aux)
          (dotimes (n  (1- (length radius)) (reverse list-aux))

            ;calcule des points en coordonnees absolutes et
            ; transformation en fonction de la droite forme a partir
            ;du segment de droite forme par  deux points de 
            ;coordonnees <xo yo> et <x1 y1>

            (push (list 
                   (setf px (+ (* (nth n radius) 
                                  factcomp 
                                  (cos (+ teta2 (nth n teta))))
                               px))
                   (setf py (+ (* (nth n radius) 
                                  factcomp 
                                  (sin (+ teta2 (nth n teta))))
                               py)))
                  list-aux))))
      (push (list (om::last-elem xlist2) (om::last-elem ylist2)) list-aux)
      (setf xlist2  (first (om::mat-trans (reverse list-aux))))
      (setf ylist2 (second (om::mat-trans (reverse list-aux)))))
(values (reverse list-aux) (first (om::mat-trans (reverse list-aux))) (second (om::mat-trans (reverse list-aux)) ))))
;============================================================

(om::defmethod! alea::paires ((bpf om::bpf)) 

:initvals '(nil)
  :indoc '("bpf table" )
  :icon 247 
  :doc"the coordinates of the points within a BPF
Outputs a list of the coordinates of the points within a multi-BPF"
  (om::mat-trans (list (om::x-points bpf) (om::y-points bpf))))
;=========================================================



