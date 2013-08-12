;; ============================================================================================
;;                                PAQUET   Systemes dynamiques
;; ============================================================================================

;;          V1.3
;;                                               functions by Mikhail Malt   1998 Paris IRCAM



;========================================================================================

 
(in-package "ALEA")

;=============================================================
;=============Iterated function systems=====================
;=============================================================
(om::defmethod! alea::choixaux ((vectprob list)  (listobjets list )) 
  :initvals '(nil nil)
  :indoc '("probability vector"  "object list" )
  :icon 240 
  :doc "This module makes a choice between multiple alternatives (listobjets) based on a probability vector vectprob."
  
  (nth (do ((indice 0 (+ indice 1))
            (u (/ (random 10000.0) 10000.0))
            (valeur 0))      ; def de var
           ((> valeur u) (- indice 1))
         (setf valeur (+ valeur (nth  indice vectprob)))) listobjets))

;(alea::choixaux '(.33 .2 .1 .27 .1) '(0 1 2 3 4))






(om::defmethod! alea::IFSx ((data list) (ints integer) 
                            (objet t ) 
                            (efact number ) (ffact number ) 
                            (mode integer))
  :menuins '((5 (("deter"   1) ("random"  2))))
  :initvals '(nil 1 '((1 1)) 100 100 1)
  :indoc '("data list"  "number of iterations" "object, may be a list of pairs or a bpf" 
           "multiplicative horizontal translation factor" "multiplicative vertical translation factor" 
           "mode")
  :icon 243 
  :numouts 3
  :doc "Iterated function system 
list of coordinates in two dimensions of the transformed object
¥ objet is a list of lists containing the coordinates of an object (a figure) or a BPF with a geometric object;
¥ ints is the number of desired iterations;
¥ data is a list of lists containing the data for the linear transformations. To this input it is possible to connect either a module ifs-lib, or a module make-w (which allows the user to construct personalized linear transforma-tions), or a module make3-w (which is the equivalent of three make-w modules) or a module app-W-trans (used to group multiple make-w modules;
¥ efact is a multiplicative factor for the horizontal translation;
¥ ffact is a multiplicative factor for the vertical translation;
¥ mode is in fact a list of menu options which allow the user to chose the way in which the module will function: either deterministicly or probalisticly .
The first output of this module is a list of coordinates:
( (x0 y0 ) (x1 x2 ) ... (xn yn )),
the second input is a list of the x values and the third one a list of the y values"
  
  (let* ((objet (if (listp objet) objet (om::mat-trans (list (om::x-points objet) (om::y-points objet)))))
         (list (om::mat-trans data))
         (lenifs (length list))
         (vectprob (om::flat(last data))) ;option random
          (listfunct (when (= 2 mode) (om::arithm-ser 0 1 (1- (length (om::mat-trans data)))))) ;option random
         (aux1 nil) (aux nil)
         (x ) (y ) (pair) (k nil))
    
    (dotimes (n ints )
      ; Kesieme equation
      
      (dotimes (tr lenifs)
        (case mode 
          (1 (setf k tr))
          (2 (setf k (alea::choixaux vectprob listfunct))))
        (dotimes (m  (length objet) pair)  ;applique la transformation <k> ˆ tous les points de l'objet
          
          (setf  x (+ (* (nth 0 (nth k list)) (first (nth m objet)))
                      (* (nth 1 (nth k list)) (second (nth m objet)))
                      (* efact (nth 4 (nth k list)))))
          (setf y (+ (* (nth 2 (nth k list)) (first (nth m objet)))
                     (* (nth 3 (nth k list)) (second (nth m objet)))
                     (* ffact (nth 5 (nth k list)))))
          (push (list x y) aux1))
        )
      
      (setf aux  aux1)                       ;(append aux1 aux)
      (setf objet aux1)
      (setf aux1 nil))
    
    (values (reverse aux) (first (om::mat-trans (reverse aux))) (second (om::mat-trans (reverse aux)) ))))                 ;




(om::defmethod! alea::app-W-trans (&rest lists) 

  :initvals '(())
  :indoc '("data list from <make-w> modules")
  :icon 246 
  :doc "This module is used to group two or more <make-w>  modules
to build a linear system of transformations"
(om::mat-trans  (mapcar #'(lambda (l) (om::flat l)) lists)))






(om::defmethod! alea::ifs-lib ((data string )) 

  :menuins '((0 (("syerp"  "syerp") ("tree0" "tree0") 
                ("fern" "fern") ("drag" "drag") ("cantor" "cantor") ("twig" "twig") 
                ("cristal" "cristal") ("fern1"   "fern1")  
                ("tree1"   "tree1")   ("castle"  "castle")
                ("cloud"   "cloud") ("frnsqr"  "frnsqr") 
                ("jewel"   "jewel") ("jewel2"   "jewel2")
                ("frntre7"   "frntre7") ("fern2"     "fern2")
                ("plant1"   "plant1") ("plant2"   "plant2") ("mountain"   "mountain"))))
  :initvals '("syerp")
  :indoc '("data list")
  :icon 243 
  :doc "Library of data for use with the module IFSx The input of this module is a list of 
menu options which allow the user to select a particular model of linear transformation. 
The output of this module is a list containing seven sub-lists. 
It should be noted that each transformation is composed of two matrices 

A  =  | a1 b1| and t  = |e1|  and one associated probability p1
      | c1 d1|          |f1|   

where A is a space transformation and t is a translation.
The output list corresponds to seven groups of data :
((a1 a2 a3 ... an) (b1 b2 b3 ... bn) (c1 c2 c3 ... cn) (d1 d2 d3 ... dn)
(e1 e2 e3 ... en) (f1 f2 f3 ... fn) (p1 p2 p3 ... pn)),
where ÔnÕ is the number of transformation which make up the system.
The module ifs-lib offers 19 basic models, each with its own attractor.

Great part of this data were from a private library of Mikael Laurson"
  (cond 
    ((string-equal data "syerp") '((0.5 0.5 0.5) (0 0 0) (0 0 0) (0.5 0.5 0.5) (0 0.5 0) (0 0.5 0.5) (0.33 0.33 0.34)))
    ((string-equal data "tree0")  '((0.195 0.462 -0.058 -0.035 -0.637) (-0.488 0.414 -0.07 0.07 0) (0.344 -0.252 0.453 -0.469 0)
          (0.443 0.361 -0.111 -0.022 0.501) (0.4431 0.2511 0.5976 0.4884 0.8562)
          (0.2452 0.5692 0.0969 0.5069 0.2513) (.2 .2 .2 .2 .2)))
    ((string-equal data "fern")  '((0 0.85 0.2 -0.15) (0 0.04 -0.26 0.28) (0 -0.04 0.23 0.26) (0.16 0.85 0.22 0.24) (0.5 0.075 0.4 0.575)
          (0 0.183 0.049 -0.084) (.25 .25 .25 .25)))
    ((string-equal data "drag")  '((0 0 0) (0.577 0.577 0.577) (-0.577 -0.577 -0.577) (0 0 0) (0.0951 0.4413 0.0952)
          (0.5893 0.7893 0.9853) (0.33 0.33 0.34)))
    ((string-equal data "cantor")  '((0.333 0 0) (0 0.333 -0.333) (0 1.0 1) (0.333 0 0) (0.333 0.666 0.333) (0.666 0 0) (0.33 0.33 0.34)))
    ((string-equal data "twig")  '((0.387 0.441 -0.468) (0.43 -0.091 0.02) (0.43 -0.009 -0.113) (-0.387 -0.322 0.015) (0.256 0.4219 0.4)
          (0.522 0.5059 0.4) (0.33 0.33 0.34)))
    ((string-equal data  "cristal")   '((0.382 0.382 0.382 0.382 0.382) (0 0 0 0 0) (0 0 0 0 0) (0.382 0.382 0.382 0.382 0.382)
           (0.3072 0.6033 0.0139 0.1253 0.492) (0.619 0.4044 0.4044 0.0595 0.0595) (.2 .2 .2 .2 .2)))
    ((string-equal data  "fern1") '((0 .85 .20 -.15) (0 .04 -.26 .28)(0 -.04 .23  .26)(.16 .85 .22 .24)
         (0 0 0 0)(0 1.60 1.60 .44)(.01  .85  .07  .07) (.25 .25 .25 .25)))    
    
    ((string-equal data   "tree1") '((0 .42 .42 .10)(0 -.42 .42 0)(0 .42 -.42 0)(.50 .42 .42 .10)(0 0 0 0)
         (0 .20 .20 .20)(.05 .40 .40 .15) (.25 .25 .25 .25)))  
    
    ((string-equal data  "castle") '((.5 .5 .4 .5) (0 0 0 0) (0 0 0 0) (.5 .5 .4 .5) (0 .2 0 .2) 
          (0 0 .1 .1) (.25 .25 .25 .25)))
    
    ((string-equal data  "cloud") '((.45 .42 .42 .1) (0 -.42 .42 0) (0 .42 -.42 0) (.50 .42 .42 .10) (0 0 .23 0) 
          (0 .20 .20 .20) (.17 .40 .40 .15)))
    
    ((string-equal data  "frnsqr") '((.25 .67 .35 .18) (0 .02 -.13 .14)(0 -.02 .11 .13) (.33 .67 .36 .37) 
          (.50 25. .50 25.) (.50 1.3 25.80 25.22) (.02 .79 .05 .10))) 
    ((string-equal data  "jewel") '((0 .42 .42 -.67) (0 -.42 .42 0) (0 .42 -.42 0) (.50 .42 .42 .10) 
          (0 0 0 0) (.31 .20 .20 .20) (.14 .40 .40 .15)))
    ((string-equal data  "jewel2") '((0 .42 .42 .10) (0 -.42 .42 0) (0 .42 -.42 0) (.50 .42 .42 .10) (0 .31 -.30 0) 
          (0 .20 .20 .20) (.05 .40 .40 .15)))
    ((string-equal data  "frntre7") '((0 .71 .26 -.07) (0 -.10 -.06 .19) (0 .10 .04 .18) (.26 .71 .27 .19) (0 0  0 0) 
          (1.18 1.18 .36 .1) (.71 .16 .08 .05)))
    ((string-equal data  "fern2") '((0 1.01 .23 -.16) (0 .4 -.29 .28) (0 -.04 .24 .28) (.16 .91 .24 .26) (0 0  0 0) 
          (1.61 1.71 .44 .01) (.89 .07 .07 .06)))
    ((string-equal data  "plant1") '((0 .42 .42 .10) (0 -.42 .42 0) (0 .42 -.42 0) (.50 .76 .42 .10) (0 0 0 0) 
          (0 .20 .20 .20) (.05 .40 .40 .15)))
    ((string-equal data  "plant2") '((0 .42 .42 .10) (0 -.42 .42 0) (0 .42 -.42 0) (.50 .42 .77 .10) (0 0 0 0)
          (0 .20 .20 .20) (.05 .40 .40 .15)))
    ((string-equal data  "mountain") '((.43 .71 .34 .63) (-.04 -.12 .08 -.40) (-.09 .15 .23 .01) (.53 .48 .41 .49) 
          (.51 .14 0 .16) (.29 .21 .44 .49) (.29 .28 .20 .49)))))




(om::defmethod! alea::make-w ((r number ) (s number ) 
                    (tet1 number) (tet2 number) 
                    (e number) (f number) 
                    (approx number ) &optional (prob 0.5)) 


  :initvals '(1 1 0 0 0 0 4 0.5)
  :indoc '("contraction factor for the x axis" "contraction factor for the y axis"
           "angular shifting for the x axis" "angular shifting for the y axis"
           "horizontal translation" "vertical translation" "decimal approxiamtion" "probability list")
  :icon 245 
  :doc"Constructs a matrix for a linear transformation.
¥ r is the coefficient of contraction for the x axis;
¥ s is the coefficient of contraction for the y axis;
¥ tet1 is the angular offset for the x axis;
¥ tet2 is the angular offset for the y axis;
¥ e is the horizontal translation;
¥ f is the vertical translation;
¥ prob is a probability effecting the linear transformation in the case of a stochastic system transformation;
¥ approx is the number of decimal places to be included in the output data (in the matrix).
Note that each transformation is composed of two matrices:


A  =  | a1 b1| and t  = |e1|  and one associated probability p1
      | c1 d1|          |f1|   

where A is a space transformation and t is a translation.
It is possible to rewrite the matrix A as follows :

A  =  | a1 b1|     A  =  | r*(cos tet1) -s*(sin tet2)| 
      | c1 d1|           | r*(sin tet1)  s*(cos tet2)| 


where r and s are the contraction factors of the x and y axes, respectively. 
tet1 and tet2 are the angular offsets for the x and y axes, also respectively"
(let ((tet1 (alea::deg->rad tet1)) (tet2 (alea::deg->rad tet2)))
  (om::om-round (mapcar 'list 
                      (list (* r (cos tet1))
                                    (* (- s) (sin tet2))
                                    (* r (sin tet1))
                                    (* s (cos tet2))
                                    e  f prob)) approx)))

#|
(om::defmethod! alea::make3-w ((r0 number (:value 1)) (s0 number(:value 1)) 
                           (tet10 number) (tet20 number) 
                           (e0 number) (f0 number) 
                           (r1 number(:value 1)) (s1 number(:value 1)) 
                           (tet11 number) (tet21 number) 
                           (e1 number) (f1 number) 
                           (r2 number(:value 1)) (s2 number(:value 1)) 
                           (tet12 number) (tet22 number) 
                           (e2 number) (f2 number) 
                           (approx number (:value 4)) 
                           &optional (prob1 list (:value .33))
                           (prob2 list (:value .34)) (prob3 list (:value .33))) list



"Constructs a matrix for a system of three linear transformations, where:
¥ r is the coefficient of contraction for the x axis;
¥ s is the coefficient of contraction for the y axis;
¥ tet1 is the angular offset for the x axis;
¥ tet2 is the angular offset for the y axis;
¥ en is the horizontal translation;
¥ fn is the vertical translation;
¥ prob is a probability effecting the linear transformation in the case of a stochastic system transformation;
¥ approx is the number of decimal places to be included in the output data (in the matrix).
Note that each transformation is composed of two matrices

A  =  | a1 b1| and t  = |e1|  and one associated probability p1
      | c1 d1|          |f1|   

where A is a space transformation and t is a translation.
It is possible to rewrite the matrix A as follows :

A  =  | a1 b1|     A  =  | r*(cos tet1) -s*(sin tet2)| 
      | c1 d1|           | r*(sin tet1)  s*(cos tet2)| 


where r and s are the contraction factors of the x and y axes, respectively. 
tet1 and tet2 are the angular offsets for the x and y axes, also respectively"
  (pw::mat-trans 
   (pw::flat-low
    (list (alea::make-w r0 s0 tet10 tet20 e0 f0 approx prob1)
          (alea::make-w r1 s1 tet11 tet21 e1 f1 approx prob2)
          (alea::make-w r2 s2 tet12 tet22 e2 f2 approx prob3)))))

;============================================================
|#



(om::defmethod! rad->deg ((radi number)) 
 :initvals '(0)
  :indoc '("angle in radians")
  :icon 141 
  :doc"Conversion of radians in degres "
  ;;;(/ (* 180.0 radi) pi)
  (/ (* 180.0 radi) pi)
  )


(om::defmethod! deg->rad ((deg number)) 
:initvals '(0)
  :indoc '("angle in degres")
  :icon 141 
  :doc "Conversion of degres in radians"
  (/ (* deg pi) 180.0))
