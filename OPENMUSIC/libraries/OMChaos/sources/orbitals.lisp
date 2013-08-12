;; ============================================================================================
;;                                PAQUET   Systemes dynamiques
;; ============================================================================================

;;          V1.3
;;                                               functions by Mikhail Malt   1998 Paris IRCAM



;========================================================================================


(in-package "ALEA")
;================les logistiques======================

;=======================================================================
;================ORBITALS===============================================
;=======================================================================
;=======================================================================


(om::defmethod! kaos ((seed number) (lambda number) (long integer)) 
  :initvals '(0.1 3.99 20)
  :indoc '("first value" "turbulence factor" "length of the sequence")
  :icon 242 
  :doc doc "Generates a sequence of length long based on the logistical equation: yn = xn-1 * lambda * (1 - xn-1 ) 
 where 
 lambda is a number or a list of parameters which define the 'turbulence' of the generated values.
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations;
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (dotimes (n long res) (setf res (cons (setf xn (* lambda xn (- 1 xn))) res)))
    (reverse res)))


(om::defmethod! kaos ((seed number) (lambda list) (long integer)) 
  :initvals '(0.1 (3.99) 20)
  :indoc '("first value" "turbulence factor" "length of the sequence")
  :icon 242 
  :doc doc "Generates a sequence of length long based on the logistical equation: yn = xn-1 * lambda * (1 - xn-1 ) 
 where 
 lambda is a number or a list of parameters which define the 'turbulence' of the generated values.
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations;
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (when (>  long (length lambda)) (error "not enough lambda elements"))
    (dotimes (n long res) (setf res (cons (setf xn (* (nth n lambda) xn (- 1 xn))) res)))
    (reverse res)))





(om::defmethod! alea::kaosn1 ((seed number) (lambda number) (gamma number) (long integer) (fn? integer )) 
  :initvals '(0.1 3.99 3.8 20 1)
  :indoc '("first value" "first turbulence factor (between 0 and 4.0)" "second turbulence factor (between 0 and 4.0)" 
           "length of the sequence" "index of the conjugate function")
  :icon 242 
  :doc "Generates a sequence of length long based on a variation of the logistical equation: yn = x n-1 * lambda - gamma *x n-1 2 where
 - lambda and gamma are the parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration."
  
  (let ((res (cons seed nil)) (xn seed))
    (dotimes (n long res) 
      (setf res 
        (cons
         (dotimes (m fn? xn)
           (setf res (cons (setf xn (- (* lambda xn) (* xn xn gamma))) res)) )
         res)))
    (reverse res)))

(om::defmethod! alea::kaosn ((seed number) (lambda number) (long integer) (fn? integer)) 
  :initvals '(0.1 3.99  20 1)
  :indoc '("first value" "chaotic factor (between 0 and 4.0)"  
           "length of the sequence" "index of the conjugate function")
  :icon 242 
  :doc "Generates a sequence of length long based on the logistical equation: yn = xn-1 * lambda * (1 - xn-1 ) 
 where 
 -lambda is a number or a list of parameters which define the 'turbulence' of the generated values.
 - seed is an initial value between zero and one;
 - fn is the degree of iteration of the logistical equation, if fn = n the sequence calculated will be the function com-posed of yn = y(yn-2 );
 - long is the length of the list generated, which is equivalent to the number of iterations;
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (dotimes (n long res) 
      (setf res 
        (cons
         (dotimes (m fn? xn)
           (setf xn (* lambda xn (- 1 xn))) )
         res)))
    (reverse res)))


(om::defmethod! alea::kaosn ((seed number) (lambda list) (long integer) (fn? integer)) 
  :initvals '(0.1 3.99  20 1)
  :indoc '("first value" "chaotic factor (between 0 and 4.0)"  
           "length of the sequence" "index of the conjugate function")
  :icon 242 
  :doc "Generates a sequence of length long based on the logistical equation: yn = xn-1 * lambda * (1 - xn-1 ) 
 where 
 -lambda is a number or a list of parameters which define the 'turbulence' of the generated values.
 - seed is an initial value between zero and one;
 - fn is the degree of iteration of the logistical equation, if fn = n the sequence calculated will be the function com-posed of yn = y(yn-2 );
 - long is the length of the list generated, which is equivalent to the number of iterations;
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (when (>  long (length lambda)) (error "not enough lambda elements"))
    (dotimes (n long res) 
      (setf res 
        (cons
         (dotimes (m fn? xn)
           (setf xn (* (nth n lambda) xn (- 1 xn))) )
         res)))
    (reverse res)))




(om::defmethod! alea::Verhulst ((seed number) (lambda number) (long integer)) 
  :initvals '(0.1 2.8  20 )
  :indoc '("first value" "chaotic factor (between 0 and 3.0)"  
           "length of the sequence" )
  :icon 242 
  :doc "Generates a sequence of length long based on the logistical equation of Pierre-FranÁois Verhulst : yn = xn-1 + xn-1 * lambda * (1 - xn-1)
 This equation describes population growth.
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one (this value represents the initial population as a ratio to the maxi-mum population);
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (dotimes (n long res) 
      (setf res (cons (setf xn (+ xn (* lambda xn (- 1 xn)))) res)))
    (reverse res)))



(om::defmethod! alea::Verhulst ((seed number) (lambda list) (long integer)) 
  :initvals '(0.1 2.8  20 )
  :indoc '("first value" "chaotic factor (between 0 and 3.0)"  
           "length of the sequence" )
  :icon 242 
  :doc "Generates a sequence of length long based on the logistical equation of Pierre-FranÁois Verhulst : yn = xn-1 + xn-1 * lambda * (1 - xn-1)
 This equation describes population growth.
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one (this value represents the initial population as a ratio to the maxi-mum population);
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (when (>  long (length lambda)) (error "not enough lambda elements"))
    (dotimes (n long res) 
      (setf res (cons (setf xn (+ xn (* (nth n lambda) xn (- 1 xn)))) res)))
    (reverse res)))



(om::defmethod! alea::Verhulst2 ((seed number) (lambda number) 
                                 (long integer) (dt number)) 
  :initvals '(0.1 2.8  20 0.01)
  :indoc '("first value" "chaotic factor (between 0 and 3.0)"  
           "length of the sequence" "delta time to the numerical integration")
  :icon 242 
  :doc "Generates a sequence of length long based on the logistical equation of Pierre-FranÁois Verhulst (see verhulst above).
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one (this value represents the initial population as a ratio to the maxi-mum population);
 - long is the length of the list generated, which is equivalent to the number of iterations. This version allows the manipulation of the parameter of time dt;
 - dt is a value of time for the numerical integration in the equations.
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (dotimes (n long res) 
      (setf res (cons (setf xn (+ xn (* dt lambda xn (- 1 xn)))) res)))
    (reverse res)))



(om::defmethod! alea::Verhulst2 ((seed number) (lambda list) 
                                 (long integer) (dt number)) 
  :initvals '(0.1 2.8  20 0.01)
  :indoc '("first value" "chaotic factor (between 0 and 3.0)"  
           "length of the sequence" "delta time to the numerical integration")
  :icon 242 
  :doc "Generates a sequence of length long based on the logistical equation of Pierre-FranÁois Verhulst (see verhulst above).
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one (this value represents the initial population as a ratio to the maxi-mum population);
 - long is the length of the list generated, which is equivalent to the number of iterations. This version allows the manipulation of the parameter of time dt;
 - dt is a value of time for the numerical integration in the equations.
 The output of this module is a list of values for each iteration."
  (let ((res (cons seed nil)) (xn seed))
    (when (>  long (length lambda)) (error "not enough lambda elements"))
    (dotimes (n long res) 
      (setf res (cons (setf xn (+ xn (* dt (nth n lambda) xn (- 1 xn)))) res)))
    (reverse res)))




(om::defmethod! baker1 ((seed number ) (int integer)) 
  :initvals '(0.9  20 )
  :indoc '("first value" "number of iterations" )
  :icon 242 
  :doc "Baker's transformation (Stretch and fold), for this transformation we consider that the dough has an initial length of one. 
 At moment zero a grain of spice is placed at the coordinate seed. 
 This module allows the position of that grain to be determined after int number of iterations. 
 The bakers work is, in this case, modeled in such a way as that each iteration corresponds 
 to the complete stretching of the dough to double its length and its refolding in a way that 
 it regains its original length of one. "
  (let ((aux) (xn seed))
    (cons xn aux)
    (dotimes (n int (reverse aux))
      (push (setf xn 
              (if (> xn .5) 
                  (- 2 (* 2 xn))
                (* 2 xn)))
            aux))))


(om::defmethod! baker2 ((seed number ) (int integer)) 
  :initvals '(0.85   20 )
  :indoc '("first value" "number of iterations" )
  :icon 242 
  :doc "Baker's transformation (Stretch, cut and paste) , for this transformation we consider that the dough has an initial length of one. 
 At moment zero a grain of spice is placed at the coordinate seed. 
 This module allows the position of that grain to be determined after int number of iterations. 
 The bakers work is, in this case, modeled in such a way as that each iteration corresponds 
 to the complete stretching of the dough to double its length, the cutting of 
 the dough into two pieces and the superposition of those pieces."
  (let ((aux) (xn seed))
    (cons xn aux)
    (dotimes (n int (reverse aux))
      (push (setf xn 
              (if (< xn .5)
                  (* 2 xn)
                (-  (* 2 xn) 1)
                ))
            aux))))


;======le vrai===========
(defun lorentzeq (x y z a r c )
  "Lorentz's equations"
  (list (*  a (- y x))
        (- (* x (- r z)) y)
        (-  (* x y) (* c z))))

(om::defmethod! lorentz ((xinit number) (yinit number) 
                         (zinit number) 
                         (a number) (R number) 
                         (c number) (dt number) 
                         (pas integer)) 
  :initvals '(1.0 1.0 1.0 10 28 2.67 0.02 100 )
  :indoc '("first x value" "first y value" "first z value"  "Prandtl number" "Reynolds number" 
           "parameter" "delta time to the numerical integration" "number of iterations steps" )
  :icon 242 
  :numouts 4
  :doc "Lorentz's equation system :
 dx= -ax + ay
 dy= Rx - y - xz
 dz= -cz + xy
 These equations give an approximate description of a fluid layer heated from below. 
 The warm fluid which is below is lighter, and thus tends to rise. 
 This creates a convection movement. 
 If the temperature difference between the top and bottom is sufficiently large, 
 the convection will be turbulent and irregular. 
 The parameter R is propor-tional to the temperature difference, 
 this is referred to as the Reynolds number. The parameter a is the Prandtl number.
 - xinit, yinit and zinit are the initial coordinates;
 - pas is the number of iterations, or generated points;
 - dt is a value of time for the numerical integration in the equations.
 The output of this module is a list of coordinates in three dimensions :
 ((xinit yinit zinit) (x0 y0 z0 ) (x1 x2 x3 ) ... (xn yn zn ))."  
  (let ((aux) (x xinit) (y yinit) (z zinit) (fk) (ffk) )
    (dotimes (n pas (reverse aux))
      (setf fk (lorentzeq x y z a R c))
      (setf ffk (lorentzeq (+ x (* dt (nth 0 fk)))
                           (+ y (* dt (nth 1 fk)))
                           (+ z (* dt (nth 2 fk)))
                           a R c))
      (push (list 
             (setf x (+ x
                        (* (/ dt 2)
                           (+ (nth 0 fk) (nth 0 ffk)))))
             (setf y (+ y
                        (* (/ dt 2)
                           (+ (nth 1 fk) (nth 1 ffk)))))
             (setf z (+ z
                        (* (/ dt 2)
                           (+ (nth 2 fk) (nth 2 ffk))))))
            aux))
 (values aux (first (om::mat-trans aux)) (second (om::mat-trans aux)) (third (om::mat-trans aux)))))


;====================================

(defun naviereq (x1 x2 x3 x4 x5 reyn ) 
  
  (list
   (+ (* x1 -2) (* 4 x2 x3) (* 4 x4 x5))
   (+ (* x2 -9) (* 3 x1 x3) )
   (+ (* x3 -5) (* -7 x1 x2) reyn )
   (+ (* x4 -5) (*  (- x1) x5)  )
   (+ (* x1 x4 -3) (- x5)  )))



(om::defmethod! navier-stokes ((x1-in number) (x2-in number) (x3-in number)
                               (x4-in number) (x5-in number) 
                               (reyn number) (dt number) (pas integer)) 
  
  :initvals '(1.0 1.0 1.0 1.0 1.0  28  0.04 100 )
  :indoc '("first x1 value" "first x2 value" "first x3 value" "first x4 value" "first x5 value" "Reynolds number" 
           "step time to  numerical integration" "number of iterations steps" )
  :icon 242 
  :numouts 6
  :doc "A model obtained by an appropriate truncation to five modes of the Navier-Stokes equations for an incompress-ible fluid in a torus.
 dx1= -2*x1 + 4*x2*x3 + 4*x4*x5
 dx2= -9*x2 + 3*x1*x3
 dx3= -5*x3 - 7*x1*x2 + reyn
 dx4= -5*x4 - x1*x5
 dx5= -x5 - 3*x1*x4
 - reyn is the Reynolds number, which has a certain number of interesting behaviors in function of different values of reyn. 
 For the different critical values of reyn, 
 the most remarkable point is the stochastic behavior observed when R1 reyn R2. With 28.73 R1 29.0 and R2 ± = 33.43.
 - pas is the number of iterations, or generated points;
 - dt is a value of time for the numerical integration in the equations.
 The output of this module is a list of coordinates in five dimensions :
 ((x1-in x2-in x3-in x4-in x5-in ) ... (x1n x2n x3n x4n x5n ))."
  (let ((aux) (x1 x1-in) (x2 x2-in) (x3 x3-in) (x4 x4-in) (x5 x5-in)(fk) (ffk) )
    (dotimes (n pas (reverse aux))
      (setf fk (naviereq x1 x2 x3 x4 x5 reyn))
      (setf ffk (naviereq (+ x1 (* dt (nth 0 fk)))
                          (+ x2 (* dt (nth 1 fk)))
                          (+ x3 (* dt (nth 2 fk)))
                          (+ x4 (* dt (nth 3 fk)))
                          (+ x5 (* dt (nth 4 fk)))
                          reyn))
      (push (list 
             (setf x1 (+ x1(* (/ dt 2)(+ (nth 0 fk) (nth 0 ffk)))))
             (setf x2 (+ x2(* (/ dt 2)(+ (nth 1 fk) (nth 1 ffk)))))
             (setf x3 (+ x3(* (/ dt 2)(+ (nth 2 fk) (nth 2 ffk)))))
             (setf x4 (+ x4(* (/ dt 2)(+ (nth 3 fk) (nth 3 ffk)))))
             (setf x5 (+ x5(* (/ dt 2)(+ (nth 4 fk) (nth 4 ffk))))))
            aux))
 (values aux 
        (first (om::mat-trans aux)) 
        (second (om::mat-trans aux)) 
        (third (om::mat-trans aux))
        (fourth (om::mat-trans aux))
        (fifth (om::mat-trans aux)))))

;
;=========================================
(om::defmethod! stein ((seed number) (lambda number) (long integer)) 
  :initvals '(0.5 1.7 100 )
  :indoc '("first  value" "turbulence" "number of iterations steps" )
  :icon 242 
  :doc "Iterative quadratic equation: Xn+1 =lambda*sin(pi*Xn )
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values.
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration.
 * see the article : 
 UNIVERSAL BEHAVIOR IN NONLINEAR SYSTEMS de Mitchell J. FEIGENBAUM,
 dans Los Alamos Science 1 4-27 (1980)"
  (let ((res) (xn seed))
    (dotimes (n long res) 
      (setf res 
        (cons (setf xn (* lambda (sin (* xn 3.141592653589793)))) res)))
    (reverse res)))


(om::defmethod! stein ((seed number) (lambda list) (long integer)) 
  :initvals '(0.5 1.7 100 )
  :indoc '("first  value" "turbulence" "number of iterations steps" )
  :icon 242 
  :doc "Iterative quadratic equation: Xn+1 =lambda*sin(pi*Xn )
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values.
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration.
 * see the article : 
 UNIVERSAL BEHAVIOR IN NONLINEAR SYSTEMS de Mitchell J. FEIGENBAUM,
 dans Los Alamos Science 1 4-27 (1980)"
  (let ((res) (xn seed))
    (when (>  long (length lambda)) (error "not enough lambda elements"))
    (dotimes (n long res) 
      (setf res 
        (cons (setf xn (* (nth n lambda) (sin (* xn 3.141592653589793)))) res)))
    (reverse res)))






(om::defmethod! stein1 ((seed number) (lambda number) (long integer)) 
  :initvals '(0.5 2.3 100 )
  :indoc '("first  value" "turbulence" "number of iterations steps" )
  :icon 242 
  :doc "Iterative quadratic equation: Xn+1 =lambda*xn 2 *sin(pi*Xn )
 Variation of the equation Xn+1=lambda*sin(pi*Xn).
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration.
 * see the article :
 UNIVERSAL BEHAVIOR IN NONLINEAR SYSTEMS de Mitchell J. FEIGENBAUM,
 dans Los Alamos Science 1 4-27 (1980)"
  (let ((res) (xn seed))
    (dotimes (n long res) 
      (setf res 
        (cons (setf xn (* lambda xn xn (sin (* xn 3.141592653589793)))) res)))
    (reverse res)))



(om::defmethod! stein1 ((seed number) (lambda list) (long integer)) 
  :initvals '(0.5 2.3 100 )
  :indoc '("first  value" "turbulence" "number of iterations steps" )
  :icon 242 
  :doc "Iterative quadratic equation: Xn+1 =lambda*xn 2 *sin(pi*Xn )
 Variation of the equation Xn+1=lambda*sin(pi*Xn).
 - lambda is a number or a list of parameters which define the 'turbulence' of the generated values;
 - seed is an initial value between zero and one;
 - long is the length of the list generated, which is equivalent to the number of iterations.
 The output of this module is a list of values for each iteration.
 * see the article :
 UNIVERSAL BEHAVIOR IN NONLINEAR SYSTEMS de Mitchell J. FEIGENBAUM,
 dans Los Alamos Science 1 4-27 (1980)"
  (let ((res) (xn seed))
    (when (>  long (length lambda)) (error "not enough lambda elements"))
    (dotimes (n long res) 
      (setf res 
        (cons (setf xn (* (nth n lambda) xn xn (sin (* xn 3.141592653589793)))) res)))
    (reverse res)))



;=============================================

(om::defmethod! henon ((xinit number) (yinit number)
                       (a number) (b number) 
                       (pas integer)) 
  :initvals '(1.0 1.0 1.4 0.3 100 )
  :indoc '("first x value" "first y value" "first parameter around 1.4" "second  parameter around 0.3" "number of iterations steps" )
  :icon 242 
  :numouts 3
  :doc "This model is a simplified version of the Lorentz dynamic system. It was suggested by the French astronomer Michel Henon in 1976.
 Xn+1 = yn - a*xn 2 + 1
 Yn+1 = b*xn
 with a = 1.4 and b = 0.3
 - xinit and yinit are the initial values;
 - a and b are the system parameters;
 - pas is the number of iterations, or generated points.
 The output of this module is a list of coordinates in two dimensions :
 ((xinit yinit ) (x0 y0 ) (x1 x2 ) ... (xn yn ))
 * see the article : David RUELLE in 'Strange Attractors', 
 The Mathematical Intelligencer 2 126-37 (1980). "
  (let (auxlist)
    (do ((x xinit (+ 1 y (- (* a x x ))))
         (y yinit (* b x))
         (n 1 (+ 1 n)))
        ((= n pas) (reverse auxlist))
      (push (list x y ) auxlist))
 (values auxlist (first (om::mat-trans auxlist)) (second (om::mat-trans auxlist)))))



(om::defmethod! alea::henon-heilles ((xinit number) (yinit number) 
                                     (ydot number) (E  number) 
                                     (dt number) 
                                     (pas integer))
  :initvals '(0.1 0.1 0.1 1/8 0.02 100 )
  :indoc '("first x value" "first y value" "first y derivate value" "Whole energy of the system. Maximum value 1/6" 
           "step time to  numerical integration" "number of iterations steps" )
  :icon 242 
  :numouts 5
  :doc "This system was originally introduced as a simplified model of the individual movement of a star within a grav-itational field:
 dx^2/dt^2= -x-2xy 
  
 dy^2/dt^2= -y+y^2-x^2 
 where
 x and y are the star’s coordinates,
 E is the total energy of the system,
 The maximum permitted value for E is 1/6.
 - xinit, yinit and ydot are the initial values;
 - E is the value of the total energy;
 - dt is a value of time for the numerical integration in the equations;
 - pas is the number of iterations, or generated points.
 The output of this module is a list of coordinates in four dimensions :
 ((xinit yinit xdot ydot ) (x0 y0 xdot0 ydot0 ) (x1 x2 xdot1 ydot2 ) ... (xn yn xdotn ydotn )).
 The second output returns a x values list, the third input returns a y values list,
 the fourth output returns a xdot values list and the fifth output returns a ydot values list
 See Rick Bidlack, 1992, Chaotic Systems as Simple 
 (but Complex) compositional Algorithms, in CMJ vol16,n°3. 
 And Robert H. G. Helleman (1980) 
 -SELF-GENERATED CHAOTIC BEHAVIOR IN NONLINEAR MECHANICS- in
 Fundamentals Problems in Statistical Mechanics vol 5 pp 165-233."
  (let ((auxlist) 
        (xdot (sqrt (+ (* 2.0 E)
                       (- (* yinit yinit))
                       (* yinit yinit yinit (/ 4.0 3.0))
                       (- (* ydot ydot)))))
        (xdotdot) (ydotdot) (x xinit) (y yinit))
    (dotimes (n pas (reverse auxlist))
      (setf xdotdot (+ (- x) (- (* 2.0 x y))))
      (setf ydotdot (+ (- y) (- (* x x)) (* y y)))
      (setf xdot (+ xdot (* xdotdot dt)))
      (setf ydot (+ ydot (* ydotdot dt)))
      (setf x (+ x (* xdot dt)))
      (setf y  (+ y (* ydot dt)))
      (push (list x y xdot ydot) auxlist))
 (values auxlist (first (om::mat-trans auxlist)) (second (om::mat-trans auxlist))
        (third (om::mat-trans auxlist)) (fourth (om::mat-trans auxlist)))))

(om::defmethod! alea::torus ((Iinit number) ( Tinit number)
                             (K number)  (pas integer)) 
  
  :initvals '(1.0 1.0 1.0 100 )
  :indoc '("first phase space value" "second phase space value" "deviation factor"  
           "number of iterations steps" )
  :icon 242 
  :numouts 3
  :doc "This equation system is derived from a model of a pendulum submitted to periodic perturbations :
 In+1 = In + K * sin Tn
 Tn+1 = Tn + In+1
 where
 - k is a parameter of perturbation;
 - I and T are the variables of the phase-space in modulo 2*pi between 0 and 2*pi;
 - init and tinit are the initial values k is the parameter of perturbation pas is the number of iterations, or generated points.
 The output of this module is a list of coordinates in two dimensions :
 ((Iinit tinit ) (I0 T0 ) (I1 T2 ) ... (In Tn )).
 the second output returns a list with the x values, and the second output returns
 the y values as a list 
 See Rick Bidlack, 1992, Chaotic Systems as Simple 
 (but Complex) compositional Algorithms, in CMJ vol16,n°3"
  (let ((auxlist) )
    (do* ((I (mod Iinit (* 2 pi))  (mod (+ I (* k (sin TT))) (* 2 pi)))
          (TT (mod Tinit (* 2 pi)) (mod (+ I  TT) (* 2 pi)))
          (n 1 (+ 1 n)))
         ((= n pas) (reverse auxlist))
      (push (list I TT ) auxlist))
 (values auxlist (first (om::mat-trans auxlist)) (second (om::mat-trans auxlist)))))



;================================================

(defun rosslereq (x y z a b c )
  "equation de base du systeme d'equations diferentielles"
  (list (- (+ y z))
        (+  x (* a y))
        (+ b (* x z) (- (* c z)))))



(om::defmethod! rossler ((xinit number) (yinit number) 
                         (zinit number) 
                         (a number) (b number) 
                         (c number) (dt number) 
                         (pas integer)) 
  :initvals '(1.0 1.0 1.0  0.2 0.2 5.7 0.02 100 )
  :indoc '("first x value" "first y value" "first z value" "first parameter" "second parameter" "third parameter"   
           "step time to  numerical integration" "number of iterations steps" )
  :icon 242 
  :numouts 4
  :doc  "The Rossler equation system is an artificial system which was created solely to be a simple model for studying a strange attractor. The following are the systems equations :
 - xinit, yinit and zinit are the initial coordinates;
 - pas is the number of iterations, or generated points;
 - a, b and c are the system parameters;
 - dt is a value of time for the numerical integration in the equations.
 The first output (the left one) returns a list of coordinates in tree dimensions
 ((xinit yinit zinit) (x0 y0 z0) (x1 x2 x3) ... (xn yn zn))
 The second output returns a x values list, the third input returns a y values list
 and the fourth output returns a z values list
 Voir Otto E. Rossler, AN EQUATION FOR CONTINOUS CHAOS, 
 in Phys. Lett 57A (1976) pp 397-398."
  (let ((aux) (x xinit) (y yinit) (z zinit) (fk) (ffk) )
    
    (dotimes (n pas (reverse aux))
      (setf fk (rosslereq x y z a b c))
      (setf ffk (rosslereq (+ x (* dt (nth 0 fk)))
                           (+ y (* dt (nth 1 fk)))
                           (+ z (* dt (nth 2 fk)))
                           a b c))
      (push (list 
             (setf x (+ x
                        (* (/ dt 2)
                           (+ (nth 0 fk) (nth 0 ffk)))))
             (setf y (+ y
                        (* (/ dt 2)
                           (+ (nth 1 fk) (nth 1 ffk)))))
             (setf z (+ z
                        (* (/ dt 2)
                           (+ (nth 2 fk) (nth 2 ffk))))))
            aux))
 (values aux (first (om::mat-trans aux)) (second (om::mat-trans aux)) (third (om::mat-trans aux)))))



;::::::::::::::UTILS:::::::::::::::::::::::::::::::::::::


;
(defun sign (x)
  (if (< x 0) -1 1))

; (sign -6)

;==========================================================
(om::defmethod! ginger ((xinit number) (yinit number) 
                        (cr number) (pas integer)) 
  :initvals '(1.0 1.0 0.9  100 )
  :indoc '("first x value" "first y value"  "contraction parameter (between 0 and 1)"     
           "number of iterations steps" )
  :icon 242 
  :numouts 3
  :doc   "Chaos Iterative equation system :
 Xn+1 = 1 - yn - cr*(abs x)
 Yn+1 = xn
 where
 - xinit and yinit are the initial values cr is a control parameter between zero and one, 
 and pas is the number of iterations, or generated points. 
 The first output of this module is a coordinate list in two diemensions:
 ((xinit yinit ) (x0 y0 ) (x1 x2 ) ... (xn yn ))
 the second output returns a list with the x values, and the second output returns
 the y values as a list"
 (let ((auxlist nil))
  (do 
      ((x xinit (- 1  y (- (* cr (abs x)))))
       (y  yinit x)
       (n  1 (+ 1 n)))
      ((>= n pas) (reverse auxlist))
    (push (list x y) auxlist))
  (values auxlist (first (om::mat-trans auxlist)) (second (om::mat-trans auxlist)))))

(om::defmethod! ginger2 ((xinit number) (yinit number) 
                         (crin number) (crend number)(pas integer)) 
  :initvals '(1.0 1.0 0.8   0.99 100 )
  :indoc '("first x value" "first y value"  "initial contraction parameter (between 0 and 1)"     
           "final contraction parameter (between 0 and 1)" "number of iterations steps" )
  :icon 242 
  :numouts 3
  :doc   "Iterative equation system :
 Xn+1 = 1 - yn - cr*(abs x)
 Yn+1 = xn
 with an evolving control parameter cr where :
 - xinit and yinit are the initial values;
 - crin is an initial control parameter between zero and one;
 - crend is a final control parameter between zero and one. As the evolution of the system is calculated, the value for the control parameter cr will be interpolated between crin and crend;
 - pas is the number of iterations, or generated points.
 The first output of this module is a coordinate list in two diemensions:
 ((xinit yinit ) (x0 y0 ) (x1 x2 ) ... (xn yn ))
 the second output returns a list with the x values, and the second output returns
 the y values as a list  "
 (let ((auxlist nil))
  (do 
      ((cr crin (+ cr (/ (- crend crin) pas)))
       (x xinit (- 1  y (- (* cr (abs x)))))
       (y  yinit x)
       (n  1 (+ 1 n)))
      ((>= n pas) (reverse auxlist))
    (push (list x y) auxlist))
  (values auxlist (first (om::mat-trans auxlist)) (second (om::mat-trans auxlist)))))