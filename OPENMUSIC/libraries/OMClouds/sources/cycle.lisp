(in-package :om)

(defclass patchforvarcycle (patchForBox) 
   ();((parametres :initform nil :accessor parametres))
   (:documentation ""))
 
(defmethod get-editor-class ((self patchforvarcycle)) 'vareditor)

(defmethod OpenEditorframe ((self patchforvarcycle))
   (or (editorframe self)
       (panel (open-new-RelationFrame  self  (name self) (get-elements self)))))

(Defmethod Compile-Patch ((Self Patchforvarcycle)) 
  "Generation Of Lisp Code From The Graphic Boxes."
  (Unless (Compiled? Self)
    (Let* ((Boxes (Boxes Self))
           (In-Boxes (Sort (Find-Class-Boxes Boxes 'Omin) '< :Key 'Indice))
           (Varliste (Car (Find-Class-Boxes Boxes 'Varliste)))
           (Var-I (Car (Find-Class-Boxes Boxes 'Var-I)))
           (Var-I+ (Sort (Find-Class-Boxes Boxes 'Var-I+) '< :Key 'Indice))
           (Varstate (Find-Class-Boxes Boxes 'Varstate))
           (I-Symbol (Setf (In-Symbol Var-I) (Gensym)))
           (Liste-Symbol (Setf (In-Symbol Varliste) (Gensym)))
           (Var-Symbols (list i-symbol liste-symbol))
           (Out-Symb (Car (Code Self)))
           (Oldletlist *Let-List*) 
           Body)
      (loop for el in Var-i+
            do (progn (Setf (In-Symbol el)
                            `(mod (+ ,I-Symbol ,(Indice el)) (length ,liste-symbol)))
                      (Setf (nomliste el)
                            Liste-Symbol)))
      ;(Mapcar #'(Lambda (Thein) (Setf (In-Symbol Thein)
      ;                                `(mod (+ ,I-Symbol ,(Indice Thein)) (length ,liste-symbol))))
      ;          Var-I+)
      ;(loop for i from 0 to (1- (length in-boxes)) 
      ;      do (setf (in-symbol (nth i in-boxes)) 
      ;               (nth i (parametres self))))
      ;             ;  (omng-box-value (nth i in-boxes))))

      (Setf Out-Box Varstate)
      (Setf (nomliste Var-I) Liste-Symbol)
      (Setf *Let-List* Nil)
      (Setf Body `(+ ,.(Mapcar #'(Lambda (Theout)
                                        (Gener (Gen-Code Theout 0) I-Symbol Liste-Symbol)) Out-Box)))
      (Eval `(defun ,(Intern (String Out-Symb) :Om)  (,.Var-Symbols)
                (Let* ,*Let-List* ,Body)))
      (Setf *Let-List* Oldletlist)))
  (Setf (Compiled? Self) T))


(defclass asbox-cycle (asbox) ()) 
   
(defmethod get-patch-editor-class ((self asbox-cycle))
   "'patchforvarcycle' is the class of the Patch associated to 'self'."
   'patchforvarcycle)

(defmethod get-boxcallclass-fun ((self (eql 'cree-varcycle))) 'asbox-cycle)

;----frame


(defmethod get-frame-class ((self asbox-cycle)) 'varboxframe)


;-------------

(defmethod omNG-box-value ((self asbox-cycle) &optional (numout 0))
  "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
  (handler-bind ((error #'(lambda (c)
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                       (om-report-condition c))
                                              :size (om-make-point 300 200))
                              (om-abort)))))
        (progn (if (zerop numout) (setf (miseajour self) nil))
    (cond
     ((miseajour self) (nth numout (list (car (miseajour self)) (valeur (car (miseajour self))))))
     ((equal (allow-lock self) "l") (special-lambda-value self (reference self)))
     ((equal (allow-lock self) "o") (fdefinition (reference self)))
     ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) 
      (nth numout (value self)))
     (t (let* ((args  (loop for input in (inputs self)
                            when (not (keyword-input-p input)) collect (omNG-box-value input)))
               ;(param (cddr args))
               ;(noms-parametres (parametres (patch self)))
               (in-boxes (Sort (Find-Class-Boxes (boxes (patch self)) 'Omin) '< :Key 'Indice))
               (args (list+ args (eval-keywords self)))
               (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
          ;(loop for i from 0 to (1- (length param)) 
          ;                 do (setf (nth i noms-parametres) (nth i param)))
          ;(loop for i from 0 to (1- (length in-boxes)) 
          ;  do (setf (in-symbol (nth i in-boxes)) 
          ;           (nth i noms-parametres)))
          ;(print noms-parametres)
          (cond ((null themethod)
                 (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                        (om-abort)))
                ((not (compiled? (patch self)))
                 (progn (dialog-message (string+ "definir une contrainte" (name self)))
                        (om-abort)))
                (t
                 (progn
                   (setf *iter* 0)
                   (let* ((domaines (car args))
                          (longueur (cadr args))
                          (valeurs (loop for i from 1 to longueur collect (random-liste domaines)))
                          (contraintes (intern (string (car (code (patch self)))) :om))
                          (csp (mise-a-jour (make-instance 'asvarcycle
                                                :valeur valeurs
                                                :domaine domaines
                                                :erreurs (loop for i from 1 to longueur collect *infini*)
                                                :erreur-global *infini*
                                                :tabu nil
                                                :longueur longueur
                                                :contraintes contraintes
                                                :cont-glob (lambda (l)
                                                               (loop for i from 0 to (1- (length l))
                                                                     sum (apply contraintes (list i l))))
                                             ;   :couleurs (repeat-n *om-black-color* longueur)
                                                ))))
                     (setf *l-tabu* longueur)
                     (setf rep                           
                           (list csp
                                 (valeur csp)))))))
          (when (equal (allow-lock self) "&")
            (setf (ev-once-p self) t)
            (setf (value self) rep)
           )
          (when (equal (allow-lock self) "x")
            (setf (value self) rep))
          (nth numout rep)))))))

(defclass asvarcycle ()
  ((valeurs :initarg :valeur :accessor valeur)
   (domaine :initarg :domaine :accessor domaine)
   (erreurs :initarg :erreurs :accessor erreurs)
   (erreur-global :initarg :erreur-global :accessor erreur-global)
   (tabu :initarg :tabu :accessor tabu)
   (longueur :initarg :longueur :accessor longueur)
   (contraintes :initarg :contraintes :accessor contraintes)
   (cont-glob :initarg :cont-glob :accessor cont-glob)
   (couleurs :initarg :couleurs :accessor couleurs)))

(defmethod! cree-varcycle ((dom list) (longueur integer)); &rest parametres)
  :icon 435
  :doc "cree-varcycle dom longueur
[generic-function]
Defines a constraint problem (csp) with a list of longueur variables ranging over he domains dom, and the constraints have to be defined in the associated patch (double click on the icon to open it). This patch is an instance of patchforvarcycle. The variables are considered as a cycle, which means that the list restarts at the end.
The first output give the csp (instance of the asvarcycle)
The second output gives the current values of the variables (random if no resolution has been made).
See below the documentation of the classes as-varcycle and patchforvarcycle

Inputs :
dom : a list, representing the possible values for the variables
longueur : the number of variables

Outputs:
the csp (instance of asvracycle)
the current values of the variables

as-varcycle
[class]
Class of the csp with variables placed in a list, considered as a cycle.
The slots of the class are :
valeurs (a list) : the list of current values of the variables
domaines (a list) : the domain (same for all variables)
erreurs (a list) : the list of errors for each variable with regard to the constraints
erreur-global (an integer) : the error of the whole configuration with regrd to the constraints, which is the sum of all erreurs 
tabu : the list of the variables marked as tabu (during the resolution, useless if you don't care about the detail of the resolution process)
longueur (an integer) : the number of variables
contraintes : the constraints. In case the as-varcycle is defined from cree-varcycle they are generated from what is defined in the associated patch. 
cont-glob : aggregation of all the constraints for the whole configuration

patchforvarcycle 
[class]
These patches are opened when double clicking on the cree-varcycle boxes.
They are used to define the constraints of the associated csp, which is an instance of as-varcycle.
We make the assumption that the constraints are always defined in the same way for all the variables. So they state on any variable (input written i), and possibly the following ones, which you can make appear by clicking on the second button in the up-right corner.
Since the variables are in a cycle, the list of variables is considered to restart at its end.
The whole list of variables is represented by the liste  icon.
The constraints have to be connected to the state output. If you want to add constraints, you can either use the andc primitive to combine them or state them seperately, for this you need to add outputs by clicking on the state button in the up-right corner.

There are several kinds of predefined inputs :
i : denotes any variable
i+1, i+2, i+3, etc : the variables that follow i

Output :
state : connect the constraints here
"
  (let ((csp
         (make-instance 'asvarcycle
           :valeur (loop for i from 1 to longueur collect (random-liste dom))
           :domaine dom
           :erreurs (loop for i from 1 to longueur collect *infini*)
           :erreur-global *infini*
           :tabu nil
           :longueur longueur
           :contraintes (lambda (i l) (apply contrainte (list i (append l l))))
           :cont-glob (lambda (l)
                        (let ((liste (append l l)))
                          (loop for i from 0 to (1- (length l))
                                sum (apply contrainte (list i liste)))))
          ; :couleurs (repeat-n *om-black-color* longueur)
           )))
    (values csp (valeur csp))
   ))
 

(defmethod modif-chord ((var asvarcycle) indice newval)
  (setf (nth indice (valeur var)) newval))

(defmethod modif-random ((var asvarcycle))
  (let ((dom (domaine var)))
    (setf (valeur var)
          (loop for i from 0 to (1- (longueur var))
                collect (random-liste dom)))))

(defmethod modif-moindre-cout ((var asvarcycle) indice)
  (let* ((val (valeur var))
         (premiers (first-n val indice))
         (derniers (last-n val (1- (- (longueur var) indice))))
         (fun (cont-glob var)))
    (cadr (car (sort 
                (loop for el in (domaine var)
                      collect (list (apply fun (list (append premiers (list el) derniers))) el))
                #'(lambda (x y) (< (car x) (car y))))))))


(defmethod mise-a-jour ((var asvarcycle))
  (let* ((val (valeur var))
         (long (longueur var))
         (c (contraintes var)))
  (setf (erreurs var)
        (loop for i from 0 to (1- long)
              collect (apply c (list i val))))
  (setf (erreur-global var)
        (apply (cont-glob var) (list val)))
  var))

(defmethod mise-a-j-tabu ((var asvarcycle))
  (setf (tabu var)
        (actu-tabu (tabu var))))
 

(defmethod print-var ((var asvarcycle ))
  (print `(valeurs ,(valeur var)))
  (print `(erreurs ,(erreurs var)))
  (print `(globale ,(erreur-global var)))
  var)

(defmethod une-descente ((var asvarcycle) &optional (ming 0))
  ;(print-var var)
  (setf *iter* (1+ *iter*))
  (progn (mise-a-jour var)
         (cond ((<= (erreur-global var) *epsilon*) var);(progn (print 'gagne) var))
               ((> ming *iter-min-glob*) var);(progn (print 'rate) var))
               (t (progn
                    (mise-a-j-tabu var)
                    (let* ((err (erreurs var))
                           (err-glob (erreur-global var))
                           (long (longueur var))
                           (val (valeur var))
                           (pire-indice (pire-el-l (enleve-tabu err (tabu var) long))))
                      (if (not pire-indice) var
                          (let* ((meilleur (modif-moindre-cout var pire-indice))
                                 (meilleur-val (append (first-n val pire-indice) 
                                                       (list meilleur) 
                                                       (last-n val (1- (- long pire-indice))))))
                        (if (< (apply (cont-glob var) (list meilleur-val)) err-glob)
                          (setf (valeur var)
                                meilleur-val)
                          (setf (tabu var)
                                (cons (list pire-indice *l-tabu*) (tabu var))))
                        (une-descente var (1+ ming))))))))))

(defmethod resol ((var asvarcycle) &optional obj)
  (mise-a-jour var)
  ;  (print-var var)
  (cond ((<= (erreur-global var) *epsilon*) var) ;(progn (print-var var) var))
        (t (progn 
             (modif-random var) 
             (setf *intermediaire* var)
             ;(print-var var)
             (une-descente var)
             (resol var)))))

(defmethod! resolution ((var asvarcycle) &optional obj (l-tabu 10) (epsilon 0) (iter-ming 10000)) 
  :initvals '(nil nil nil) 
  :doc "resolution 
[generic-function]
This box is used to solve the csp.
When you don't care of the details of the resolution and you know that the csp has a solution, then just connect the csp to the input (the csp can be defined with cree-varliste, cree-varcycle or cree-varperm).
PAY ATTENTION TO THE FACT THAT THE RESOLUTION DOES NOT FINISH IN CASE THERE ARE NO SOLUTIONS TO THE CSP
In this case, please use the first optional input to see partial results.

Inputs :
first optional input :
In order to deal with approximate solutions, or to see some partial results during the resolution process, use the optional inputs.
The first optional input allows to see partial results during the resolution process. The easiest way to use it is simply to connect the second output of the box cree-varliste, cree-varcycle or cree-varperm to it (always keeping its first output connected to the first input of the resolution box). Then the intermediate results, which usually are meaningfull approximate solutions of the csp, will be printed in the listener.
More precisely, this input re-evaluates its arguments each time the solver encounters an approximate solution (precisely, a local minimum, see XXX for details). It means that it is possible to place any kind of calculus between the current values given in the second output of the cree-varliste, cree-varcycle or cree-varperm boxes and this input. It can be usefull for instance when dealing with musical data that can be paced in Open Music musical objects and edited during the resolution. See the examples patches.

The second optional input, l-tabu, is a parameter of the solver. 
The third optional input, epsilon, is a parameter of the solver. The solvers stops when the global error is less than epsilon. 
The fourth optional input, iter-ming, is a parameter of the solver. 

" 
  :icon 437
  (setf *iter* 0)
  (modif-random var)
  ;(print-var var)
  (resol var obj))

