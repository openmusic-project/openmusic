
(in-package :om)

;===========================================================================
;PATCH

 
(defvar *infini* 1000000000000)


;; REMETTRE LE SORT

;; ATTENTION LES PARAMETRES SONT EN VAR GLOBALES


(defun gener (ctr i l &optional limite)
  (cond ((null ctr) nil)
        ((atom ctr) ctr)
        (t
         (let ((pred (car ctr)))
           (cond 
            ((equal pred '=c) `(abs (- ,(cadr ctr) ,(caddr ctr))))
            ((equal pred 'equalc) `(if (equal ,(cadr ctr) ,(caddr ctr)) 0 1))
            ((equal pred '<=c) `(max 0 (- ,(cadr ctr) ,(caddr ctr))))
            ((equal pred 'notequalc) `(if (equal ,(cadr ctr) ,(caddr ctr)) 1 0))            
            ((equal pred '<c) `(max 0 (+ 1 (- ,(cadr ctr) ,(caddr ctr)))))
            ((equal pred 'andc) `(+ ,.(loop for el in (cdr ctr) collect (gener el i l))))
                                  ;,.(mapcar #'gener (cdr ctr))))
            ((equal pred 'orc) `(min ,.(loop for el in (cdr ctr) collect (gener el i l))))
                                 ;,.(mapcar #'gener (cdr ctr))))
            ((equal pred '/=c) `(if (= ,(cadr ctr) ,(caddr ctr)) 1 0))
            ((equal pred 'alldiffc) 
             (if (caddr ctr) 
               (let* ((exp-i (cadr ctr))
                      (exp-pred (caddr ctr))
                      (exp (subst 'el i exp-i)))
                 `(max 0 (1- (loop for el from 0 to (- (length ,l) (if ,limite (1+ ,limite) 1))
                                 count (apply ,exp-pred (list ,exp-i ,exp))))))
               (let* ((exp-i (cadr ctr))
                      (exp (subst 'el i exp-i)))
                 `(max 0 (1- (loop for el from 0 to (- (length ,l) 
                                                       (if ,limite (1+ ,limite) 1))
                                 count (= ,exp-i ,exp)))))))
            ((equal pred 'evenc)
             `(if (evenp ,(cadr ctr)) 0 1))
            ((equal pred 'oddc)
             `(if (oddp ,(cadr ctr)) 0 1))
            ((equal pred 'minimizec)
             `(apply ,(caddr ctr) (list ,(cadr ctr))))
            ((equal pred 'maximizec)
             `(- *infini* (apply ,(caddr ctr) (list ,(cadr ctr)))))
            ((equal pred 'memberc)
             (if (cadddr ctr)
               (let ((element (cadr ctr))
                     (ensemble (caddr ctr))
                     (predicat (cadddr ctr)))
                 `(if (member ,element ,ensemble :test ,predicat) 0 1))
               (let ((exp-i (cadr ctr))
                     (exp-liste (caddr ctr)))
                 `(loop for el in ,exp-liste minimize (abs (- ,exp-i el))))))
            (t (cons (car ctr) (loop for el in (cdr ctr) collect (gener el i l)))))))))

(defun trouve-indice (l l1 prov)
  (let ((prov (loop for el in l
                    when (not (member (1- el) l1))
                    minimize (1- el))))
    (if (= prov 0) (1+ (length l)) prov)))

(defclass varliste (omin) ())

(defmethod omNG-save ((self varliste) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-varliste ,(name self) ,(indice self)  ,(om-save-point (frame-position self)) ,(docu self) 
                     ,(frame-name self) ,(omng-save (eval (defval self)) t) ,(om-save-point (frame-size self))))   

(defun om-load-varliste (name indice position  docu &optional fname val fsize)
  (let ((newbox (make-new-varliste name indice (om-correct-point position))))
    (setf (docu newbox) docu)
    (when val
      (setf (defval newbox) (put-quote val)))
    (setf (frame-name newbox) fname)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

(defmethod! mvarliste  () :numouts 1 
  :initvals '(nil) :indoc '("patch") :doc "eval in ordre" :icon 442
   nil)

(defun make-new-varliste (name indice posi &optional icon)
   (let* ((theinput (make-instance 'varliste
                      :name name
                      :icon (or icon (list 442 (find-library "OMClouds")))
                      :reference nil
                      :indice indice)))
     (setf (frame-position theinput) posi)
     theinput))

(defmethod numouts ((self varliste)) 1)
(defmethod get-frame-name ((self varliste)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'mvarliste))) 'varliste)
(defmethod allow-lock-button ((self varliste)) 
   "each time boxes do not allow a lock button." nil)
(defmethod no-allow-copy-p ((self varliste)) 
   "Don't copy the liste of variables"
   "")

(defclass var-i (omin) 
  ((sorties :initform 1 :accessor sorties)
   (nomliste :initform nil :accessor nomliste)))
(defmethod! mvar-i  () :numouts 1 
  :initvals '(nil) :icon 438
  (declare (ignore oppatch)) nil)

(defmethod omNG-save ((self var-i) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-var-i ,(name self) ,(indice self)  ,(om-save-point (frame-position self)) ,(docu self) 
                  ,(frame-name self) ,(omng-save (eval (defval self)) t) ,(om-save-point (frame-size self)) ,(sorties self)))

(defun om-load-var-i (name indice position docu &optional fname val fsize  (sorties 1))
  (let ((newbox (make-new-var-i name indice (om-correct-point position))))
    (setf (docu newbox) docu)
    (when val
      (setf (defval newbox) (put-quote val)))
    (setf (frame-name newbox) fname)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    (setf (sorties newbox) sorties)
    newbox))

(defun make-new-var-i (name indice posi &optional icon)
   (let* ((theinput (make-instance 'var-i
                      :name name
                      :icon (or icon (list 438 (find-library "OMClouds")))
                      :reference nil
                      :indice indice)))
     (setf (frame-position theinput) posi)
     theinput))


;touche pas a mes classes

;;;(defmethod om-draw-contents ((self InFrame))
;;;   (call-next-method)
;;;  (om-with-focused-view self
;;;    (om-with-font (om-get-font self)
;;;     (om-draw-string (- (round (w self) 2) 4) 22 
;;;                  (if (integerp (indice (object self))) 
;;;                    (format  () "~D" (indice (object self)))
;;;                    "")))))
;;;
;;;(defmethod om-draw-contents ((self outFrame))
;;;   (call-next-method)
;;;   (om-with-focused-view self
;;;    (om-with-font (om-get-font self)
;;;     (om-draw-string (- (round (w self) 2) 2) 22 
;;;                  (if (integerp (indice (object self))) 
;;;                    (format  () "~D" (indice (object self)))
;;;                    "")))))


(defmethod numouts ((self var-i)) (sorties self))
(defmethod get-frame-name ((self var-i)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'mvar-i))) 'var-i)
(defmethod allow-lock-button ((self var-i)) 
   "each time boxes do not allow a lock button." nil)
(defmethod no-allow-copy-p ((self var-i)) 
   "Don't copy the variables"
   "")

(defmethod gen-code ((self var-i) numout)
   (declare (ignore numout))
   (if *compiling-macro-boite* (defval self) 
       (if (zerop numout) `(nth ,(in-symbol self) ,(nomliste self))
           (in-symbol self))))

(defclass var-i+ (var-i) ())

(defclass inframei+ (Inframe) ())


(defmethod get-frame-class ((self var-i+)) 'inFramei+)
(defun make-new-var-i+ (name indice posi &optional icon)
   (let* ((theinput (make-instance 'var-i+
                      :name name
                      :icon (or icon (list 439 (find-library "OMClouds")))
                      :reference nil
                      :indice indice)))
     (setf (frame-position theinput) posi)
     theinput))

(defun om-load-var-i+ (name indice position  docu &optional fname val fsize (sorties 1))
  (let ((newbox (make-new-var-i+  name indice (om-correct-point position))))
    (setf (docu newbox) docu)
    (when val
      (setf (defval newbox) (put-quote val)))
    (setf (frame-name newbox) fname)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    (setf (sorties newbox) sorties)
    newbox))

(defmethod omNG-save ((self var-i+) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-var-i+ ,(name self) ,(indice self)  ,(om-save-point (frame-position self)) ,(docu self) 
                  ,(frame-name self) ,(omng-save (eval (defval self)) t) ,(om-save-point (frame-size self)) ,(sorties self)))


(defclass varstate (omout) ())
(defmethod! mvarstate  ((patch t) &rest oppatch) :numouts 0 
  :initvals '(nil) :indoc '("patch") :doc "eval in ordre" :icon 440
  (declare (ignore oppatch)) nil)

(defmethod omNG-save ((self varstate) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-varstate ,(name self) ,(indice self)  ,(om-save-point (frame-position self)) ',inputs ,(frame-name self) 
                       ,(om-save-point (frame-size self)))))

(defun om-load-varstate (name indice position inputs &optional fname fsize)
  (let ((newbox (make-new-varstate name indice (om-correct-point position))))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

(defun make-new-varstate (name indice posi &optional icon)
   (let* ((theout (make-instance 'varstate
                    :name name
                    :icon (or icon (list 440 (find-library "OMClouds")))
                    :reference nil
                    :indice indice)))
     (setf (frame-position theout) posi)
     (setf (inputs theout) (list (make-instance 'input-funbox
                                   :name "out"
                                   :value nil
                                   :doc-string "out")))
     theout))

(defmethod omNG-copy ((self varstate))
  `(let ((copy (make-new-varstate ,(name self) ,(indice self) ,(om-copy-point (frame-position self)))))
     (setf (frame-name copy) ,(frame-name self))
     (setf (frame-size copy) ,(om-copy-point (frame-size self)))
     copy))


(defmethod numouts ((self varstate)) 0)
(defmethod get-frame-name ((self varstate)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'mvarstate))) 'varstate)
(defmethod allow-lock-button ((self varstate)) 
   "each time boxes do not allow a lock button." nil)
(defmethod no-allow-copy-p ((self varstate)) 
   "Don't copy the state boxes"
   "")



(defmethod! =c ((a t) (b t))
  :icon 443
  :doc "=c 
Constraint, on numbers only.
It states that the inputs have to be equal, for numbers only.
The associated cost-function measures the distance between the inputs.

Inputs :
numbers
"
  (=c a b))

(defmethod! equalc ((a t) (b t))
  :icon 443
  :doc "equalc 
Constraint.
It states that the inputs have to be equal.
The associated cost-function counts 0 if the inputs are equal, 1 otherwise.
"
  (equalc a b))

(defmethod! <c ((a t) (b t))
  :icon 443
  :doc "<c 
Constraint.
It states that the first input is smaller than the second.
The associated cost-function counts 0 if first input is smaller than the second, and the distance between them otherwise.

Inputs :
numbers
"
  (<c a b))

(defmethod! <=c ((a t) (b t))
   :icon 443
   :doc "<=c 
Constraint.
It states that the first input is smaller than or equal to the second.
The associated cost-function counts 0 if first input is smaller than or equal to the second, and the distance between them otherwise.

Inputs :
numbers
"
   (<=c a b))

(defmethod! /=c ((a t) (b t))
   :icon 443
   :doc "/=c 
Constraint, on numbers only.
It states that the inputs have not to be equal, for numbers only.
The associated cost-function counts 1 isf the input are equal, 0 otherwise.

Inputs :
numbers
"
   (/=c a b))

(defmethod! notequalc ((a t) (b t))
   :icon 443
   :doc "notequalc 
Same as /=c, but not only for numbers (the predicate is "equal" instead of "=").
"
   (notequalc a b))

(defmethod! andc ((a t) &rest autres)
  :icon 443
  :doc "andc 
Constraint.
It states that the inputs (which should be outputs of other constraints) have all to be satisfied.

Inputs
outputs of other constraints
"
  (andc a autres))

(defmethod! orc ((a t) &rest autres)
  :icon 443
  :doc "orc 
Constraint.
It states that at least one of the inputs (which should be outputs of other constraints) have all to be satisfied.

Inputs
outputs of other constraints
"
  (orc a autres))

;(defmethod! somme-liste ((l list))
;  :icon 443
;  (apply #'+ l))

(defmethod! evenc ((a t) )
  :icon 443
  (if (evenp a) 0 1))

(defmethod! oddc ((a t))
  :icon 443
  (if (oddp a) 0 1))

;(defmethod! entre ((debut integer) (fin integer) (l list))
;  :icon 443
;  (first-n (last-n l (- (length l) debut)) fin))

;(defmethod! somme-entre ((debut integer) (fin integer) (l list))
;  :icon 443
;  (loop for i from debut to fin sum (nth i l)))

(defmethod! cardc ((i t) (l t) (el t) (n integer) &optional test)
  :icon 443
  (let ((occ (if test (compte l el test)
                 (loop for a in l count (equal a el)))))
    (abs (- occ n))))


(defmethod! minimizec ((b t) (c t))
  :icon 443
  :doc "minimizec 
Constraint.
It tries to minimize its input, until it is zero. If the zero cannot be reached the solver will not stop.
"
  (minimizec b c))

;(defmethod! maximizec ((b t) (c t))
;  :icon 443
;  (maximizec b c))

(defmethod! memberc ((el t) (b list) &optional test)
  :icon 443
  :doc "memberc 
Constraint.
It states that the first input belongs to the second input.

Inputs
first input : an element
second input : a set
optional input : a predicate used to test if the first input belongs to the second
"
  (memberc el l test))

;(defmethod! notmemberc ((el t) (b list) &optional test)
;  :icon 443
;  (notmemberc el l test))


(defmethod! alldiffc ((i t) &optional test)
  :icon 443
  :doc "alldiffc 
Constraint.
It states that the input, which should be a variable or result from a calculus on a variable, have to be alldifferent. See the example patches (allinterval for instance).

Inputs
first input : a variable or combination of variables
optional input : a predicate, which is the equality on numbers "=" by default.
"
  (alldiffc i test))

(defun min-liste (l)
  (apply #'min l))

(defun max-liste (l)
  (apply #'max l))

(defvar *l-tabu* 50)

(defvar *epsilon* 0)

(defvar *valeur-affichage* 0)

(defvar *infini* 1000000000000)

(defvar *iter-min-glob* 1000)

(defvar *iter* 0)

(defvar *intermediaire* nil)

(defun random-liste (l)
  (nth (om-random-value (length l)) l))

(defun actu-tabu (l)
  (cond ((null l) nil)
        ((zerop (car (cdr (car l)))) (actu-tabu (cdr l)))
        (t (cons (list (car (car l)) (1- (car (cdr (car l))))) (actu-tabu (cdr l))))))

(defun ajout-tabu (new-val tabu)
  (if (member new-val (mapcar #'car tabu)) tabu
      (cons (list new-val *l-tabu*) tabu)))

(defun enleve-tabu (chordseq tabu l-cs)
  (loop for j from 0 to (1- l-cs)
        collect (if (not (member j (mapcar #'car tabu) :test #'equal)) (nth j chordseq))))

(defun pire-el-l (erreurs &optional (prov-el nil) (indice 0) (prov-erreur -1))
  (cond ((null erreurs) (random-dom prov-el))
        ((null (car erreurs)) (pire-el-l (cdr erreurs) 
                                          prov-el (1+ indice) prov-erreur))
        ((>= (car erreurs) prov-erreur)
         (pire-el-l (cdr erreurs)
                    (cons indice prov-el) (1+ indice) (car erreurs)))
        (t (pire-el-l (cdr erreurs)
                      prov-el (1+ indice) prov-erreur))))

(defclass box-resol (OMBoxcall) ()
   (:documentation "This is the class for the method exception box OMIF.#enddoc#
#seealso# (OMBoxcall) #seealso#"))

(defmethod get-boxcallclass-fun ((self (eql 'resolution))) 'box-resol)

(defmethod OpenEditorframe ((self box-resol)) 
   (not (dialog-message "Compiled Function RESOLUTION.")))

(defmethod call-gen-code ((self box-resol) numout)
   (declare (ignore numout))
   `(if ,(gen-code (first (inputs self)) 0) 
      ,(gen-code (second (inputs self)) 0) 
      ,(gen-code (third (inputs self)) 0)))

(defmethod gen-code-call ((self box-resol))
   (call-gen-code self 0))

#|
(defmethod colorable ((self omboxeditcall)) t)
(defmethod colorable ((self omboxabsmaq)) t)
(defmethod colorable ((self t)) nil)

(defmethod maj-couleurs ((var t))
  (let* ((err (erreurs var)))
    (loop for i from 0 to (1- (longueur var))
          do (setf (nth i (couleurs var))
                   (if (zerop (nth i err)) *om-black-color* *om-red-color*))))
    var)
|#


(defmethod omNG-box-value ((self box-resol) &optional (numout 0))
  (declare (ignore num-out))
  (handler-bind ((error #'(lambda (c) 
                            (when *msg-error-label-on*
                              (om-message-dialog 
                               (string+ "Error while evaluating the box " (string (name self)) " " 
                                        (om-report-condition c))
                               :size (om-make-point 300 200))
                              (om-abort)))))
    (cond
     ((equal (allow-lock self) "l") (special-lambda-value self (reference self)))
     ((equal (allow-lock self) "o") (omNG-make-new-lispfun (reference self)))
     ((and (value self) (equal (allow-lock self) "x")) (nth 0 (value self)))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth 0 (value self)))
     (t 
      (let* ((var (omNG-box-value (first (inputs self))))
             (deuxieme (second (inputs self)))
             rep)
        ;(print "initialisation")
        ;(print-var var)
        (setf *l-tabu* (if (third (inputs self)) (omng-box-value (third (inputs self))) 10))
        (setf *epsilon* (if (fourth (inputs self)) (omng-box-value (fourth (inputs self))) 0))
        (setf *iter-ming* (if (fifth (inputs self)) (omng-box-value (fifth (inputs self))) 10000))
        ; (setf *iter* 0)
        (if (and deuxieme (omng-box-value deuxieme))
          (progn
            (setf (miseajour (first (connected? (first (inputs self))))) nil)
             (loop while (> (erreur-global var) *epsilon*)
                   do (progn 
                        (mise-a-jour var)
                        (setf (miseajour (first (connected? (first (inputs self))))) (list var (valeur var)))
                     ;   (maj-couleurs var)
                        (omNG-box-value (second (inputs self)))
                     ;   (if (colorable (first (connected? (second (inputs self)))))
                     ;     (let* ((objmus (value (first (connected? (second (inputs self))))))
                     ;           (obj (inside objmus))
                     ;            (couleurs (couleurs var)))
                     ;       (print obj)
                     ;       (loop for i from 0 to (1- (length obj))
                     ;             do (set-mus-color (nth i obj) 
                     ;                               (nth i couleurs)))))
                        (setf *intermediaire* var)
                        (print "resultat intermediaire")
                        (print-var var)
                        (modif-random var)
                        (une-descente var)
                        ))
            (setf (miseajour (first (connected? (first (inputs self))))) (list var (valeur var)))
        ;    (maj-couleurs var)
            (omNG-box-value (second (inputs self)))
        ;    (if (colorable (value (first (connected? (second (inputs self))))))
        ;                  (let* ((objmus (value (first (connected? (second (inputs self))))))
        ;                         (obj (inside objmus))
        ;                         (couleurs (couleurs var)))
        ;                    (loop for i from 0 to (1- (length obj))
        ;                          do (set-mus-color (nth i obj) 
        ;                                            (nth i couleurs)))))
            (setf (miseajour (first (connected? (first (inputs self))))) nil))
           (loop while (> (erreur-global var) *epsilon*)
                 do (progn 
                      (modif-random var) 
                      (setf *intermediaire* var)
                      (une-descente var))))
        ;(print `(nombre d iterations ,*iter*))
        (setf rep (list (valeur var)))
        (when (equal (allow-lock self) "&")
          (setf (ev-once-p self) t)
          (setf (value self) rep))
        (when (equal (allow-lock self) "x")
          (setf (value self) rep))
        (when (equal (allow-lock self) nil)
          (setf (value self) rep))
        (nth numout rep))))))


;aaa 18/06/15
;;;(defmethod omNG-box-value ((self OMBoxMaquette) &optional (num-out 0))
;;;   (declare (ignore num-out))
;;;   (handler-bind ((error #'(lambda (c) 
;;;                             (when *msg-error-label-on*
;;;                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
;;;                                                        (om-report-condition c))
;;;                                               :size (om-make-point 300 200))
;;;                               (om-abort)))))
;;;     (cond
;;;      ((equal (allow-lock self) "x")
;;;       (setf (value self) (cons-copy-maquette-object (reference self) (boxes (reference self))))
;;;       (value self))
;;;      ((and (equal (allow-lock self) "o") (reference self)))
;;;      ((and (equal (allow-lock self) "&") (ev-once-p self)) (value self))
;;;      (t (let* ((args  (mapcar #'(lambda (input)
;;;                                   (omNG-box-value  input)) (inputs self)))
;;;                rep)
;;;           (when (third args)
;;;             (setf (metricparam (params (reference self))) (third args)))
;;;          
;;;           (put-boxes-inmaquette  (reference self) (first args) (second args))
;;;           (setf rep (cons-copy-maquette-object (reference self) (boxes (reference self))))
;;;           (when (equal (allow-lock self) "&")
;;;             (setf (ev-once-p self) t)
;;;             (setf (value self) rep))
;;;           (when (equal (allow-lock self) "x")
;;;             (setf (value self) rep))
;;;           (when (editorFrame (reference self))
;;;             (om-invalidate-view  (editorFrame (reference self)) t))
;;;           rep)))))
         

(defun random-dom (dom)
  (nth (om-random-value (length dom)) dom))

(defun generation-accord (n-notes dom)
  (if (= n-notes 0) nil
      (let ((new-note (random-dom dom)))
        (cons new-note
              (generation-accord (1- n-notes) (remove new-note dom))))))

(defun generation-aleatoire (n-acc n-notes dom)
  (loop for i from 1 to n-acc
        collect (generation-accord n-notes dom)))

(defun c-croissante (i var)
  (let* ((valeur (aref var i)))
    (loop for j from 0 to 9
          sum (cond 
               ((= i j) 0)
               ((< i j) (if (< valeur (aref var  j)) 0 1))
               (t (if (> valeur (aref var j)) 0 1))))))




(defclass vareditor (boxpatchEditor) ())

(defmethod get-editor-panel-class ((self vareditor)) 'varpanel)

(defclass varpanel (boxpatchPanel) ())

(defmethod omg-remove-element ((self varpanel) (box inFrame))
   "The inputs of a this patch can be removed only from the box."
   (if (equal (class-name (class-of (object box))) 'var-i+)
     (omg-remove)
     (om-beep-msg "delete inputs in the csp box")))

(defmethod omg-remove-element ((self varpanel) (box t))
   (if (or (equal (class-name (class-of (object box))) 'varliste)
      ;     (equal (class-name (class-of (object box))) 'varstate)
           (equal (class-name (class-of (object box))) 'var-i))
     (om-beep-msg "These boxes can not be erased")
     (call-next-method)))

(defmethod omg-remove-element ((self varpanel) (frame InFramei+))
   "Remove a boxframe from the scroller, this method call the 'omng-remove-element' method with the objects referenced by 'self' and 'frame'."
   (omng-remove-element (object self) (object frame))
   (close-frame frame)
   (om-remove-subviews self frame))

(defmethod omg-remove-element ((self varpanel) (frame outframe))
   "Remove a boxframe from the scroller, this method call the 'omng-remove-element' method with the objects referenced by 'self' and 'frame'."
   (omng-remove-element (object self) (object frame))
   (close-frame frame)
   (om-remove-subviews self frame))

(defmethod handle-key-event ((self varpanel) char) 
   (case char
     (#\n (let* ((boxes (get-subframes self))
                 (entrees (loop for box in boxes
                                when (or (equal (class-name (class-of (object box))) 'var-i)
                                         (equal (class-name (class-of (object box))) 'var-i+))
                                collect box)))
            (loop for el in entrees 
                  do (progn (setf (sorties (object el)) 2)
                            (omG-select (redraw-frame el))))))
     (#\N (let* ((boxes (get-subframes self))
                 (entrees (loop for box in boxes
                                when (or (equal (class-name (class-of (object box))) 'var-i)
                                         (equal (class-name (class-of (object box))) 'var-i+))
                                collect box)))
            (loop for el in entrees 
                  do (progn (setf (sorties (object el)) 1)
                            (omG-select (redraw-frame el))))))
     (otherwise (call-next-method))))


;; ATTENTION LE "DEJA OUVERT" EST EN VARIABLE GLOBALE

;; Pour enlever les numeros voir omtypedin et la fonction  view-draw-contents
;; refaire une classe inframe adaptee



(defmethod add-window-buttons  ((self varpanel))
  (om-add-subviews self 
    (om-make-view 'button-icon
      :iconID (list 439 (find-library "OMClouds"))
      :position (om-make-point 40 5)
      :size (om-make-point 24 24)
      :action #'(lambda (item) (declare (ignore item)) 
                 (let* ((boxes (get-subframes self)) 
                        ; Si on veut toujours un indice plus loin sans remplir les trous
                        ;(i (1+ (length (find-class-boxes boxes 'InFramei+))))
                        (indices (loop for box in boxes
                                       when (equal (class-name (class-of (object box))) 'var-i+)
                                       collect (indice (object box))))
                        (i (trouve-indice indices (cons 0 indices) (1+ (length indices))))
                        (pos (om-make-point (+ 100 (* i 24)) 100)))
                   (omG-add-element self
                                    (make-frame-from-callobj 
                                     (make-new-var-i+ (mk-unique-name self "i+")
                                                      i pos
                                                      (list 439 (find-library "OMClouds")))))
                   (set-field-size self))))
    (om-make-view 'button-icon
      :iconID (list 440 (find-library "OMClouds"))
      :position (om-make-point 0 5)
      :size (om-make-point 24 24)
      :action #'(lambda (item) (declare (ignore item)) 
                 (let* ((boxes (get-subframes self)) 
                        ; Si on veut toujours un indice plus loin sans remplir les trous
                        ;(i (1+ (length (find-class-boxes boxes 'InFramei+))))
                        (indices (loop for box in boxes
                                       when (equal (class-name (class-of (object box))) 'varstate)
                                       collect (indice (object box))))
                        (i (1+ (length (find-class-boxes boxes 'outframe))))
                        (pos (om-make-point (+ 200 (* i 40)) 300)))
                   (omG-add-element self
                                    (make-frame-from-callobj 
                                     (make-new-varstate (mk-unique-name self "state")
                                                      i pos
                                                        (list 440 (find-library "OMClouds")))))
                   (set-field-size self))))))




(defclass asbox (box-with-patch) 
   ((numouts :initform 2 :accessor numouts)
 ;   (parametres :initform nil :accessor parametres)
    (miseajour :initform nil :accessor miseajour))
   (:documentation "This is the class for the csp boxes.#enddoc#"))


(defmethod omNG-copy ((self asbox))
  `(let* ((copy ,(call-next-method)))
     (setf (numouts copy) ,(numouts self))
     copy))


(defmethod initialize-instance :after ((self asbox) &key controls)
  (declare (ignore controls))
 
  (let ((liste (omNG-make-new-boxcall (fdefinition 'mvarliste) (om-make-point 5 100) "liste"))
        (state (omNG-make-new-boxcall (fdefinition 'mvarstate) (om-make-point 200 300) "state"))
        (ii (omNG-make-new-boxcall (fdefinition 'mvar-i) (om-make-point 100 100) "i")))
    (setf (code (patch self)) (list (gensym)))
   ; (setf (parametres (patch self)) 
   ;       (loop for el in (cddr (inputs self)) collect (gensym)))
    (omNG-add-element (patch self) liste)
    (omNG-add-element (patch self) state)
    (omNG-add-element (patch self) ii)))

(defmethod openeditorframe ((self asbox))
   (openobjecteditor (patch self)) nil)


(defmethod call-gen-code ((self asbox) numout)
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     (if (zerop numout) 
       `(,(intern (string (first (code (patch self)))) :om) ,.in-list)
       `(nth ,numout (multiple-value-list (,(intern (string (first (code (patch self)))) :om) ,.in-list))))))


(defmethod gen-code-call ((self asbox))
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     `(,(intern (string (first (code (patch self)))) :om) ,.in-list)))


(defmethod special-lambda-value ((self asbox) symbol)
   "Eval a csp box in lambda mode."
   (let* ((nesymbs nil)
          (args  (mapcar #'(lambda (input)
                             (if (connected? input)
                               `',(omNG-box-value  input)
                               (let ((newsymbol (gensym)))
                                 (push newsymbol nesymbs)
                                 newsymbol))) (inputs self))))
     (if (null nesymbs)
       symbol
       (eval `#'(lambda ,(reverse nesymbs)
                  (apply (fdefinition ',(intern (string (first (code (patch self)))) :om)) (list ,.args)))))))


(defmethod curry-lambda-code ((self asbox) symbol)
   "Lisp code generation for a csp box in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
                              (gen-code input 0)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
    (if (null nesymbs)
      symbol
      `#'(lambda ,(reverse nesymbs)
                 (apply (fdefinition ',(intern (string (first (code (patch self)))) :om)) (list ,.args))))))


(defmethod special-value ((self  asbox) &optional (args nil))
   (unless (compiled? (patch self)) 
       (compile-patch (patch self))
       (setf (compiled? (patch self)) t))
     (apply (fdefinition (intern (string (first (code (patch self)))) :om)) args))


(defclass varboxframe (boxframe) ()
   (:documentation "Simple frame for csp boxes."))


(defmethod update-from-reference ((self asbox) &optional (udt? t))
   "Called when occurs changes in the patch reference of 'self'."
   (declare (ignore udt?))
   (let* ((new-inputs (mapcar #'(lambda (input) 
                                  (make-instance 'input-funbox
                                    :name (or (frame-name input) (name input))
                                    :value (eval (defval input))
                                    :doc-string (docu input))) 
                              (get-patch-inputs (patch self))))
          conec-to-me)
     (when *input-to-erase*
       (setf (inputs self) 
             (delete (nth *input-to-erase* (inputs self)) (inputs self) :test 'equal)))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))
               (setf (value newin) (value oldin))) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs)
     (when *output-to-delete*
       (setf conec-to-me (get-conect-to-me self))
       (loop for item in conec-to-me do
             (erase-out-connec self item *output-to-delete*)))
     (when (frames self)
       (when *output-to-delete*
         (loop for source in conec-to-me do
               (box-draw-connections (car (frames source)) nil)
               (redraw-frame (car (frames source)))))
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self))))))


(defmethod om-get-menu-context ((object varboxframe))
   (list+  (list (om-new-leafmenu "Update doc"
                    #'(lambda () (apply-win (om-view-window object) 'update-doc)))
                  (om-new-leafmenu "-"
                    #'(lambda () nil))) (boxframe-default-list object)))

(defmethod update-doc ((box varboxframe))
   (update-from-reference (object box)))

(defmethod close-frame ((box varboxframe))
   "If miniview show a picture we must kill it."
   (when (EditorFrame (patch (object box)))
     (om-close-window (window (EditorFrame (patch (object box))))))
   (setf (frames (object box)) nil))

