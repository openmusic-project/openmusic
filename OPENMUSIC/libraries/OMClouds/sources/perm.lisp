(in-package :om)

(defclass patchforvarperm (patchForBox) 
  (); ((parametres :initform nil :accessor parametres))
   (:documentation ""))

(defmethod get-editor-class ((self patchforvarperm)) 'vareditor)

(defmethod OpenEditorframe ((self patchforvarperm))
   (or (editorframe self)
       (panel (open-new-RelationFrame  self  (name self) (get-elements self)))))
 
;; ATTENTION LES PARAMETRES SONT EN VAR GLOBALES

(Defmethod Compile-Patch ((Self Patchforvarperm)) 
  "Generation Of Lisp Code From The Graphic Boxes."
  (Unless (Compiled? Self)
    (Let* ((Boxes (Boxes Self))
           (In-Boxes (Sort (Find-Class-Boxes Boxes 'Omin) '< :Key 'Indice))
           (Varliste (Car (Find-Class-Boxes Boxes 'Varliste)))
           (Var-I (Car (Find-Class-Boxes Boxes 'Var-I)))
           (Var-I+ (Sort (Find-Class-Boxes Boxes 'Var-I+) '< :Key 'Indice))
           (limite (if var-i+ (indice (car (last var-i+))) 0))
           (Varstate (Find-Class-Boxes Boxes 'Varstate))
           (I-Symbol (Setf (In-Symbol Var-I) (Gensym)))
           (Liste-Symbol (Setf (In-Symbol Varliste) (Gensym)))
           (Var-Symbols (list i-symbol liste-symbol))
           (Out-Symb (Car (Code Self)))
           (Oldletlist *Let-List*) 
           Body)
      (loop for el in Var-i+
            do (progn (Setf (In-Symbol el)
                        `(+ ,I-Symbol ,(Indice el)))
                      (Setf (nomliste el) Liste-Symbol)))
      ;(loop for i from 0 to (1- (length in-boxes)) 
      ;      do (setf (in-symbol (nth i in-boxes)) 
      ;               (nth i (parametres self))))
      (Setf (nomliste Var-I) Liste-Symbol)
      (Setf Out-Box Varstate)
      (Setf *Let-List* Nil)
      (Setf Body `(+ ,.(Mapcar #'(Lambda (Theout)
                                   (Gener (Gen-Code Theout 0) i-symbol liste-symbol limite)) Out-Box)))
      (Eval `(defun ,(Intern (String Out-Symb) :Om)  (,.Var-Symbols)
                      (Let* ,*Let-List* 
                        (if (>= ,i-symbol (- (length ,liste-symbol) ,limite))
                          (,(Intern (String Out-Symb) :Om) (- ,i-symbol ,limite) ,liste-symbol)
                          ,Body))))
      (Setf *Let-List* Oldletlist)))
  (Setf (Compiled? Self) T))

;==================THE BOX LOOP CLASS=======================

(defclass asbox-perm (asbox) ())

(defmethod get-patch-editor-class ((self asbox-perm))
   "'patchforvarperm' is the class of the Patch associated to 'self'."
   'patchforvarperm)


(defmethod get-boxcallclass-fun ((self (eql 'cree-varperm))) 'asbox-perm)

(defmethod get-frame-class ((self asbox-perm)) 'varboxframe)


;-------------

;(defclass boxcontraintes (OMBoxcall) ())

;(defmethod get-boxcallclass-fun ((self (eql '=c))) 'boxcontraintes)

;(defmethod gen-code ((self boxcontraintes) numouts)
;  `(,(reference self) ,.(decode self)))

(defmethod omNG-box-value ((self asbox-perm) &optional (numout 0))
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
     ; (nth numout (miseajour self)))
     ((equal (allow-lock self) "l") (special-lambda-value self (reference self)))
     ((equal (allow-lock self) "o") (fdefinition (reference self)))
     ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) 
      (nth numout (value self)))
     (t (let* ((args  (loop for input in (inputs self)
                            when (not (keyword-input-p input)) collect (omNG-box-value input)))
               ;(param (cdr args))
               ;(noms-parametres (parametres (patch self)))
               (args (list+ args (eval-keywords self)))
               (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
          ;(loop for i from 0 to (1- (length param)) 
          ;                 do (setf (nth i noms-parametres) (nth i param)))
          (cond ((null themethod)
                 (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                        (om-abort)))
                ((not (compiled? (patch self)))
                 (progn (dialog-message (string+ "definir une contrainte" (name self)))
                        (om-abort)))
                (t
                 (progn
                   (setf *iter* 0)
                   (let* ((valeurs (car args))
                          (contraintes (intern (string (car (code (patch self)))) :om))
                          (longueur (length valeurs))
                          (csp (mise-a-jour (make-instance 'asvarperm
                                                :valeur valeurs
                                                :erreurs (loop for i from 1 to longueur collect *infini*)
                                                :erreur-global *infini*
                                                :tabu nil
                                                :longueur longueur
                                                :contraintes contraintes
                                                :cont-glob (lambda (l)
                                                             (loop for i from 0 to (1- (length l))
                                                                   sum (apply contraintes (list i l))))
                                                ;:couleurs (repeat-n *om-black-color* longueur)
                                                ))))
                     (setf *l-tabu* (length valeurs))
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


(defmethod! cree-varperm (init); &rest parametres)
  :icon 435
  :doc
"
cree-varperm init
[generic-function]
Defines a permutation constraint problem (csp) with the variables being permutations of the list given in the input and the constraints have to be defined in the associated patch (double click on the icon to open it). This patch is an instance of patchforvarperm. The domain of the csp is the set of all permutations of the variables.
The first output give the csp (instance of the asvarperm)
The second output gives the current values of the variables (random if no resolution has been made).
See below the documentation of the classes as-varperm and patchforvarperm

Inputs :
init : the list of variables

Outputs:
the csp (instance of asvraperm)
the current values of the variables

as-varperm
[class]
Class of the permutation csp. Note that this class has no domain slot (contrary to as-varliste and as-varcycle), it is not necessary to define a domains for the variables since we know that the domain of the csp is the set of all permutations of the variables.
The slots of the class are :
valeurs (a list) : the list of current values of the variables
erreurs (a list) : the list of errors for each variable with regard to the constraints
erreur-global (an integer) : the error of the whole configuration with regrd to the constraints, which is the sum of all erreurs 
tabu : the list of the variables marked as tabu (during the resolution, useless if you don't care about the detail of the resolution process)
longueur (an integer) : the number of variables
contraintes : the constraints. In case the as-varperm is defined from cree-varperm they are generated from what is defined in the associated patch. 
cont-glob : aggregation of all the constraints for the whole configuration

patchforvarperm 
[class]
These patches are opened when double clicking on the cree-varperm boxes.
They are used to define the constraints of the associated csp, which is an instance of as-varperm.
We make the assumption that the constraints are always defined in the same way for all the variables. So they state on any variable (input written i), and possibly the following ones, which you can make appear by clicking on the second button in the up-right corner.

The whole list of variables is represented by the liste icon.
The constraints have to be connected to the state output. If you want to add constraints, you can either use the andc primitive to combine them or state them seperately, for this you need to add outputs by clicking on the state button in the up-right corner.

There are several kinds of predefined inputs :
i : denotes any variable
i+1, i+2, i+3, etc : the variables that follow i

Output :
state : connect the constraints here
"
  (let ((csp
         (make-instance 'asvarperm
           :valeur init
           :erreurs (loop for i from 1 to longueur collect *infini*)
           :erreur-global *infini*
           :tabu nil
           :longueur (length init)
           :contraintes (lambda (i l) (apply contrainte (list i (append l l))))
           :cont-glob (lambda (l)
                        (let ((liste (append l l)))
                          (loop for i from 0 to (1- (length l))
                                sum (apply contrainte (list i liste)))))
           ;:couleurs (repeat-n *om-black-color* longueur)
           )))
    (values csp (valeur csp))
   ))


(defclass asvarperm ()
  ((valeurs :initarg :valeur :accessor valeur)
   (erreurs :initarg :erreurs :accessor erreurs)
   (erreur-global :initarg :erreur-global :accessor erreur-global)
   (tabu :initarg :tabu :accessor tabu)
   (longueur :initarg :longueur :accessor longueur)
   (contraintes :initarg :contraintes :accessor contraintes)
   (cont-glob :initarg :cont-glob :accessor cont-glob)
   ;(couleurs :initarg :couleurs :accessor couleurs)
   ))

(defun permut (l i j)
  (let* ((liste (copy-list l))
         (val (nth i l)))
    (setf (nth i liste) (nth j liste))
    (setf (nth j liste) val)
    liste))

(defun aux-permut-random (l long)
  (if (zerop long) l
    (aux-permut-random (permut l (om-random-value (length l)) (om-random-value (length l)))
                       (1- long))))

;changed by AAA
#|
(defun permut-random (l)
  (aux-permut-random l (length l)))
|#

(defun clouds-permut-random (l)
  (aux-permut-random l (length l)))


(defmethod modif-chord ((var asvarperm) indice newval)
  (let ((val (nth indice (valeur var))))
    (setf (nth indice (valeur var)) (nth newval (valeur var)))
    (setf (nth newval (valeur var)) val))
  var)

;changed by AAA
#|
(defmethod modif-random ((var asvarperm))
  (setf (valeur var)
        (permut-random (valeur var)))
  var)
|#

(defmethod modif-random ((var asvarperm))
  (setf (valeur var)
        (clouds-permut-random (valeur var)))
  var)


(defmethod print-var ((var asvarperm ))
  (print `(valeurs ,(valeur var)))
  (print `(erreurs ,(erreurs var)))
  (print `(globale ,(erreur-global var)))
  var)

(defmethod modif-moindre-cout ((var asvarperm) indice)
  (let* ((val (valeur var))
         (fun (cont-glob var)))
    (cadr (car (sort 
                (loop for i from 0 to (1- (longueur var)) 
                      when (/= i indice)
                      collect (list (apply fun (list (permut val i indice))) i))
                #'(lambda (x y) (< (car x) (car y))))))))


(defmethod mise-a-jour ((var asvarperm))
  (let* ((val (valeur var))
         (long (longueur var))
         (c (contraintes var)))
  (setf (erreurs var)
        (loop for i from 0 to (1- long)
              collect (apply c (list i val))))
  (setf (erreur-global var)
        (apply (cont-glob var) (list val)))
  var
  ))

(defmethod mise-a-j-tabu ((var asvarperm))
  (setf (tabu var)
        (actu-tabu (tabu var))))



(defmethod une-descente ((var asvarperm) &optional (ming 0))
 ; (print-var var)
  (setf *iter* (1+ *iter*))
  (progn (mise-a-jour var)
         (cond ((<= (erreur-global var) *epsilon*)  var ) 
             ;  (progn (print 'gagne) var))
               ((> ming *iter-min-glob*) var)
               ; (progn (print 'rate) var))
               (t (progn
                    (mise-a-j-tabu var)
                    (let* ((err (erreurs var))
                           (err-glob (erreur-global var))
                           (long (longueur var))
                           (val (valeur var))
                           (pire-indice (pire-el-l (enleve-tabu err (tabu var) long))))
                      (if (not pire-indice) var
                          (let* ((meilleur (modif-moindre-cout var pire-indice))
                                 (meilleur-val (permut val meilleur pire-indice)))
                        (if (< (apply (cont-glob var) (list meilleur-val)) err-glob)
                          (setf (valeur var)
                                meilleur-val)
                          (setf (tabu var)
                                (cons (list pire-indice *l-tabu*) (tabu var))))
                        (une-descente var (1+ ming))))))))))

(defmethod resol ((var asvarperm) &optional obj)
  (mise-a-jour var)
;  (print-var var)
  (cond ((<= (erreur-global var) *epsilon*) var) ;(progn (print-var var) var))
        (t (progn 
             (modif-random var) 
             (setf *intermediaire* var)
             ;(print-var var)
             (une-descente var)
             (resol var)))))


(defmethod! resolution ((var asvarperm) &optional obj (l-tabu 10) (epsilon 0) (iter-ming 10000)) 
  :numouts 1 
  :initvals '(nil nil nil) 
  :indoc '("if" "then" "else")
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

;;; set-mus-obj-color