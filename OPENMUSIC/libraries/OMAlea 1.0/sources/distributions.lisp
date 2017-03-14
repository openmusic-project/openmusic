;; ==================================================================================== 
;;                                PAQUET   DISTRIBUITIONS
;; ==================================================================================== 
;;          V1.1
;;                                 functions by Mikhail Malt   1998 Paris IRCAM



(in-package "ALEA")

(defun ran () 
  "Generates a variable <x> uniformly distributed, such as 0 =< x =< 1."
  (random 1.0))

(defun ran01 () 
  "Generates a variable <x> uniformly distributed, such as 0 < x < 1."
  (let ((u))
    (setf u (ran))
    (if (or (= 0 u) (= 1 u)) (ran01)  u)))



(om::defmethod! choix ((x1 t) (x2 t) (px1 number))  
  :initvals '(1 2 0.5) :indoc '("first choice" "second choice" "probability")
  :doc "Choice between two alternatives <x1> and <x2>, with complementary probabilities
		      Probability of <x1> = <px1>,
            	      Probability of <x2> = <1-px1>."
  :icon 240 
  (if (< (ran) px1) x1 x2))


#|
(om::defmethod! choixmultiple ((vectprob list) &rest listobjets)
  :initvals '((0.5 0.5) 1)
  :indoc '("probability vector" "aux choix")
  :icon 240 
  :doc "Choice between severals alternatives (<listobjets>) 
from a vector opf probability <vectprob>. This box is extensible."
  (nth (do ((indice 0 (+ indice 1))
            (u (/ (random 10000.0) 10000.0))
            (valeur 0))      ; dŽf de var
           ((> valeur u) (- indice 1))
         (setf valeur (+ valeur (nth  indice vectprob)))) listobjets))
|#


(om::defmethod! choixmultiple ((vectprob list) &rest listobjets)
                :initvals '((0.5 0.5) nil)
                :indoc '("probability vector" "aux choix")
                :icon 240 
                :doc "Choice between severals alternatives (<listobjets>) 
from a vector opf probability <vectprob>. This box is extensible."
                (let ((index (do ((indice 0 (+ indice 1))
                                  (u (/ (random 10000.0) 10000.0))
                                  (valeur 0))      ; dŽf de var
                                 ((> valeur u) (- indice 1))
                               (setf valeur (+ valeur (nth  indice vectprob)))) ))
                  (if listobjets 
                      (nth index listobjets)
                    index)))



(om::defmethod! distexp ((lambda number)) 
  :initvals '(1.0)
  :indoc '("lambda density")
  :icon 240 
  :doc "Generates a number with an exponential distribution of density <lambda>."
  (if (=  (ran) 0)    0  (* (log (ran)) (- (/ 1 lambda)))))


(om::defmethod! expobi ((lambda number) (mu number))
  :initvals '(1.0 0.0)
  :indoc '("lambda density" "distribution's center")
  :icon 240 
  :doc "Generates a number with a bilateral exponential distribution of
density <lambda> with an average<mu>."
  (let ((u) )
    (prog ()
      label1
      (setf u (* (ran) 2.))
      (if (= u 0) (go label1) 
          (if (= u 2) (go label1)
              (if (> u 1.0)  (return (+ (/ (log (- 2.0 u)) lambda) mu))
                  (return (+ (/ (- (log u) ) lambda) mu)))))))) 


(om::defmethod! distlin ((g number))
   :initvals '(1.0)
   :indoc '("g parameter, superior boundary")
   :icon 240 
   :doc "Generates a number with a linear distribution of parameter <g>."
   (* g (- 1 (sqrt (ran)))))


(om::defmethod! distcauchy ((alpha number))
  :initvals '(1.0)
  :indoc '("alpha parameter")
  :icon 240 
  :doc "Generates a number with a cauchy distribution with a parameter <alpha>."
  (* alpha (tan (* pi (ran)))))


(om::defmethod! distlog ((alpha number) (beta number)) 
  :initvals '(1.0 0.0)
  :indoc '("alpha parameter" "beta parameter")
  :icon 240 
  :doc "Generates a number with a logistic distribution of parameters
 <alpha> (the dispersion is proportionally inversed to alpha) and <beta>.
 The mode is located at (-beta/alpha)."
  (/ (+ (- beta) (- (log (- (/ 1 (ran01)) 1)))) alpha))


(om::defmethod! distCsHp ((alpha number) (beta number))
  :initvals '(1.0 0.0)
  :indoc '("alpha parameter" "beta parameter")
  :icon 240 
  :doc "Generates a number with a hyperbolic cosinus distribution.
<alpha> is a scaling factor and <beta> a shifting factor."
  (+ (*(log (tan (/ (* pi (ran01)) 2))) alpha) beta))


(om::defmethod! distarsin ((alpha number) (beta number)) 
  :initvals '(1.0 0.0)
  :indoc '("alpha parameter" "beta parameter")
  :icon 240 
  :doc "Generates a number with a arc sinus distribution. <alpha> 
is a scaling factor and <beta> a shifting factor. 
For <alpha>= 1 and <beta>= 0, this distribution is identical to the BETA distribution ."
  (+ (* (expt (sin (/ (* pi (ran)) 2)) 2) alpha) beta))

(om::defmethod! poisson ((lmbd number))
  :initvals '(1.0)
  :indoc '("lambda parameter")
  :icon 240 
  :doc "Generates a number with a Poisson distribution of average <lmbd>."
  (let ((u) (v) (n)) 
    (prog  ()
      (setf n 0)
      (setf u (ran))
      (setf v (exp (- lmbd)))
      label
      (if (< u v) (return n)
          (setf u (* u (ran))))
      (setf n (+ 1 n))
      (go label))))

(om::defmethod! triang ((alpha number) (beta number))
  :initvals '(1.0 0.0)
  :indoc '("alpha parameter" "beta parameter")
  :icon 240 
  :doc "Generates a number with a triangular distribution of average .5 (for alpha=1
and beta=0) ,where <alpha> is a scaling factor and <beta> a shifting factor."
  (+ (* (/ (+ (ran) (ran)) 2) alpha) beta))

(om::defmethod! gauss ((mu number) (sigma number))
  :initvals '(0.0 1.0)
  :indoc '("average" "bandwidth")
  :icon 240 
  :doc "Generates a number with a gaussian distribution of an average <mu> and a bandwidth <sigma>.
It is important to know that the gaussian distribution is not bounded,
 and that 99.74 % of the results falls between -3*sigma and +3*sigma,
 but for the present algorithm, the results will be bounded
 between  -6*sigma and +6*sigma. In most cases this aproximation 
is acceptable, since only two results out of a milliard fall out
 of these limits in a true gaussian processus."
  (let ((s 0))
    (om::for (n 1 1 12)  (setf s (+ s (ran))))
    (+ (* (- s 6) sigma) mu)))

 
(om::defmethod! weilbull ((s number) (te number))
  :initvals '(1.0 3.2)
  :indoc '("scaling factor" "morphology factor")
  :icon 240 
  :doc "Generates a number with a Weilbull distribution 
of parameters <s> and <t>, where <s> is a horizontal 
scaling factor and <t> controls the distribution morphology.
 For t=3.2, this distribution is approaching to the gaussian 
distribution. 99.9 % of the results are bellow s*6.9^(1/t)."
  (let ((u) (a))
    (prog () 
      label
      (setf u (ran))
      (if (= u 0) (go label)
          (if (= u 1) (go label)
              (setf a (/ 1. (- 1. u)))))
      (return (* s (expt (log a) (/ 1. te)))))))



(om::defmethod! gamma ((nu integer))
  :initvals '(4)
  :indoc '("morphology parameter")
  :icon 240 
  :doc "Generates a number with a gamma distribution of parameter <nu>.
Since the present algorithm only functions witth <nu>
 as an integer number, for floating numbers one need 
multiply the variable which is generated by some factor 
that transforms nu1 to nu2: nu2 = nu1*facteur."
  (let ((s 1))
    (om::for (n 1 1 nu) (setf s (* s (ran))))
    (- (log s))))

(om::defmethod! beta ((a number) (b number))
  :initvals '(0.5 0.5)
  :indoc '("rigth boundary factor" "left boundary factor")
  :icon 240 
  :doc "Generates a number with a beta distribution.
 For <a> = <b> = 1 the result is a continuous uniform 
distribution, for <a> and <b> greater than 1 the result 
is similar to a gaussian distribution. "
  (let ((ea) (eb) (y1) (y2))
    (prog ()
      (setf ea (/ 1 a)) (setf eb (/ 1 b))
      label
      
      (setf y1 (expt (ran) ea))
      (setf y2 (expt (ran) eb)) 
      
      (if (> (+ y1 y2) 1) (go label)
          (return (/ y1 (+ y1 y2)))))))
          

(om::defmethod! alea::bpf-to-distribution ((bpf bpf))
  :indoc '("a bpf in integer mode as a distribution probability function")
  :icon 240
  :doc "allows to use a curve in a bpf as a distribution probbility curve. Be carefull, this function outputs indexes"
  (let* ((x-min (om::list-min (om::x-points bpf) ))
	 (x-max (om::list-max (om::x-points bpf) ) )
	 (samples (1+ (- x-max x-min))))
    (alea::choixmultiple
     (om::om-scale/sum 
      (third 
       (multiple-value-list (om::om-sample bpf samples x-min x-max))) 1))))

(om::defmethod! alea::bfp-to-distribution ((bpf bpf))
  :indoc '("a bpf in integer mode as a distribution probability function")
  :icon 240
  :doc "allows to use a curve in a bpf as a distribution probbility curve. Be carefull, this function outputs indexes"
  (warn "deprecated function bfp-to-distribution, use bpf-to-distribution instead~%")
  (alea::bpf-to-distribution bpf))




