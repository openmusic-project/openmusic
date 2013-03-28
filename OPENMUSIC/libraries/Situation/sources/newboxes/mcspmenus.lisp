;;;; Definition of menus and bpf sub-classes to define voice profiles.
;;; Extended by Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.  

(in-package :cl-user)
 
(defmethod flat-once ((self list))
  (if (consp (car self))
    (apply 'append self)  self))

(defun interpol5 (time t1 t2 v1 v2)
   (+ v1 (* (- time t1) (/ (- v2 v1) (- t2 t1)))) )

(defun bpf-out (bpf time times &optional float-fl)
  (let ((last-time)(last-value)
        (values #+:OM (om::y-points bpf)
                #+:PW (pw::y-points bpf)
                )
        res)
    (unless times
      #+:OM (setq times (om::x-points bpf))
      #+:PW (setq times (pw::x-points bpf))
      )
    (if (not (>= time (car times)))
       (car times)
       (progn 
         (do () ((not (and times (>= time (car times))))) 
           (setq last-time (pop times)) (setq last-value (pop values)))
           (setq res
             (if (not times)
               last-value
               (interpol5 time last-time (car times) last-value (car values))))
          (if (not float-fl) (round res) res)))))
  
(defmethod transfer ((bpf-ob t) (x-val number))
"The input <bpf> should always be connected with a multi-bpf  box .
Behaves like a transfer function when feeded with values
to the second inputbox <x-val>. returns the y value (or list of y values)
 which corresponds to <x-val> (or list of x values) for the connected bpf"
    (get-transfer-output bpf-ob x-val))

(defmethod get-transfer-output ((self null) (point number)) (declare (ignore point)) nil)

#+:OM
(defmethod get-transfer-output ((self om::bpf) (point number))
  (bpf-out self point (om::x-points self)))

#+:PW
(defmethod get-transfer-output ((self pw::C-break-point-function) (point number))
  (bpf-out self point (pw::x-points self)))

#+:OM
(defmethod get-transfer-output ((self cons) (point number))
  (if (subtypep (type-of (first self)) 'om::bpf)
    (let ((y-vals (mapcar #'(lambda (bpf) (get-transfer-output bpf point)) self)))
      (if (rest y-vals) y-vals (first y-vals)))
    (progn (format t "input is not a bpf or lists of bpfs ~%") (ed-beep))))

#+:PW
(defmethod get-transfer-output ((self cons) (point number))
  (if (subtypep (type-of (first self)) 'pw::C-break-point-function)
    (let ((y-vals (mapcar #'(lambda (bpf) (get-transfer-output bpf point)) self)))
      (if (rest y-vals) y-vals (first y-vals)))
    (progn (format t "input is not a bpf or lists of bpfs ~%") (ed-beep))))

(defmethod bpf-sample ((bpf-ob0 t)
                       (echan1 number)
                       (xinit2 number) (xend3 number)
                       (fact4 number)
                       (nbdec5 number))
  ;; :initvals '(nil 2 0 100 1 0) :indoc '("bpf object" "number of samples" "from" "to" "factor" "decimals")
  ;; :icon 207
"  The bpf-sample module creates a list starting by sampling a  
breakpoint function table;. bpf-ob  is the input to the table, 
echant is the number of samples desired, xinit et xend delimit the  sampling interval;. 
The fact   variable is a multiplicative coefficient for scaling the data, 
and nbdec  is the number of decimals desired in the output. 
 "

  (get-bpf-sample-output bpf-ob0 echan1 xinit2 xend3 fact4 nbdec5))

(defmethod get-bpf-sample-output ((self null) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore echan1 xinit2 xend3 fact4 nbdec5)) self)

#+:OM
(defmethod get-bpf-sample-output ((self om::bpf) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore nbdec5))
  (g-round 
   (om::om* (mapcar #'(lambda (x) (transfer self x))
                    (om::arithm-ser xinit2 xend3 (/ (- xend3 xinit2) (- echan1 1))))
            fact4)))

#+:PW
(defmethod get-bpf-sample-output ((self pw::C-break-point-function) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore nbdec5))
  (pw::g-round 
   (pw::g* (mapcar #'(lambda (x) (transfer self x))
                    (pw::arithm-ser xinit2 (/ (- xend3 xinit2) (- echan1 1)) xend3))
            fact4)))

(defmethod get-bpf-sample-output ((self cons) echan1 xinit2 xend3 fact4 nbdec5)
  (mapcar #'(lambda (bpf) (get-bpf-sample-output bpf echan1 xinit2 xend3 fact4 nbdec5))
          self))

(defmethod get-bpf-sample-output ((self number) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore echan1 xinit2 xend3 fact4 nbdec5))  self)

#|
(defvar FormsConstraint (pw::new-menu  "Bpf Constraints"))

(pw::add-menu-items MusicEngine FormsConstraint) 

(defclass  Composition-Form (pw::C-patch-multi-function) ())

(defmethod initialize-instance :after ((self Composition-Form) &key controls)
  (declare (ignore controls))
  (pw::set-window-title (pw::application-object self) (string-downcase (pw::pw-function self)))
  (setf (pw::pw-function-string self) (string-downcase (pw::pw-function self))))


(defmethod  pw::patch-value ((self Composition-Form) ob)  
  (let* ((bpf-obj (call-next-method)) (xp  (pw::get-slot bpf-obj 'x-points))
         (xpts (if (flatp xp) (list xp) xp))
         (xs (make-list (length xpts) 
                        :initial-element (pw::arithm-ser  (caar xpts) 1 (car (last (car xpts))))))
         (ys (mapcar #'(lambda (x) (pw::transfer bpf-obj x)) (car xs))))
    (pw::get-slot bpf-obj 'break-point-list)
    (setf ys (if (flatp ys) (list ys) (pw::mat-trans ys)))
    (make_constraint_bpf  xs ys )
    )
  )

(pw::defunp  mvProfile  (  (tlist pw::fix>0s?) (y-list pw::list) 
                                           (z pw::bpf)) list 
             "Defines profiles for voices using a bpf editor.
y-list is a list of lists. The first sublist defines the profile
for the upper voice , the second defines the lowest voice.
The third, fourth,...,etc. sublist defines a profile for the third, fourth,...,voice.
A profile curve defines the direction of the movement of a voice. The particular
value of a point in the curve is not important. Only its relation (higher, lower)
with the others defines the direction of voice movement."
(declare (ignore tlist y-list z))
)

(pw::Pw-addmenu-fun FormsConstraint 'mvProfile 'Composition-Form)
|#

#+:OM (defmethod x-points ((self om::bpf)) (om::x-points self))
#+:PW (defmethod x-points ((self pw::C-break-point-function)) (pw::x-points self))

#+:OM (defmethod x-points ((self list)) (mapcar #'om::x-points self))
#+:PW (defmethod x-points ((self list)) (mapcar #'pw::x-points self))

#+:OM (defmethod point-list ((self om::bpf)) (om::point-list self))
#+:PW (defmethod point-list ((self pw::C-break-point-function)) (pw::break-point-list self))

#+:OM (defmethod point-list ((self list)) (mapcar #'om::point-list self))
#+:PW (defmethod point-list ((self list)) (mapcar #'pw::break-point-list self))

#+:OM (om::defmethod! om::mvProfile ((bpf-obj t)) (om::bpf-mvprof bpf-obj))
#+:OM
(om::defmethod! om::bpf-mvprof ((bpf-obj t)) :initvals '(nil) :indoc '("bpf objects") :icon 207
  :doc "Defines profiles for voices using a bpf editor.
y-list is a list of lists. The first sublist defines the profile
for the upper voice , the second defines the lowest voice.
The third, fourth,...,etc. sublist defines a profile for the third, fourth,...,voice.
A profile curve defines the direction of the movement of a voice. The particular
value of a point in the curve is not important. Only its relation (higher, lower)
with the others defines the direction of voice movement."
  
  (let* ((xp  (x-points bpf-obj))
         (xpts (if (flatp xp) (list xp) xp))
         (xs (make-list (length xpts) 
                        :initial-element (om::arithm-ser  (caar xpts)  (car (last (car xpts))) 1)))
         (ys (mapcar #'(lambda (x) (transfer bpf-obj x)) (car xs))))
    (point-list bpf-obj)
    (setf ys (if (flatp ys) (list ys) (om::mat-trans ys)))
    (make_constraint_bpf  xs ys )
    ))

#+:PW
(pw::defunp bpf-mvprof ((bpf-obj pw::list (:value '() :type-list (bpf list)))) list  
 "Defines profiles for voices using a bpf editor.
y-list is a list of lists. The first sublist defines the profile
for the upper voice , the second defines the lowest voice.
The third, fourth,...,etc. sublist defines a profile for the third, fourth,...,voice.
A profile curve defines the direction of the movement of a voice. The particular
value of a point in the curve is not important. Only its relation (higher, lower)
with the others defines the direction of voice movement."
  
(let* ((xp  (x-points bpf-obj))
       (xpts (if (flatp xp) (list xp) xp))
       (xs (make-list (length xpts) 
                        :initial-element (pw::arithm-ser  (caar xpts) 1 (car (last (car xpts))))))
         (ys (mapcar #'(lambda (x) (pw::transfer bpf-obj x)) (car xs))))
    (point-list bpf-obj)
    (setf ys (if (flatp ys) (list ys) (pw::mat-trans ys)))
    (make_constraint_bpf  xs ys )
    ))
;;;; Ambitus form definition

#+:OM
(om::defmethod! om::bpf-ambdef ((bpf-obj t) &optional scale-length) :initvals '(nil nil)
  :indoc '("bpf object" "scale") :icon 521
  :doc "Defines an ambitus for the chords using a bpf editor. Two chords should be drawn, one for the lower and one for the upper limits of the ambitus. This box should be connected to the 'ambitus' entry of a solver.
'bpf-obj' receives the ouput of a BPF box. 'scale-length', if given,
 should be an integer specifying the number of chords."  
  (let* ((lx (mapcar 'x-points bpf-obj)) ls max (step 1))
    (when  (flatp lx) (setf lx (list lx)))     
    (setf ls (if (> (length (first lx)) (length (second lx))) (first lx) (second lx))
          max (apply #'max ls))    
    (when scale-length
      (setf step (/ max scale-length)))
    ;;; GAS 17/11/98
    (mapcar #'(lambda (x) 
                (om::arithm-ser (first x) (second x) 1))
            (mapcar #'(lambda (x) (reverse (transfer bpf-obj x)))
                    (om::arithm-ser 0 (apply #'max ls) step))))
  )

#+:PW
(pw::defunp bpf-ambdef ((bpf-obj  pw::list (:value '() :type-list (bpf list)))
                     &optional (scale-length fix)) list
  "Defines an ambitus for the chords using a bpf editor. Two chords should be drawn, one for the lower and one for the upper limits of the ambitus. This box should be connected to the 'ambitus' entry of a solver.
'bpf-obj' receives the ouput of a BPF box. 'scale-length', if given,
 should be an integer specifying the number of chords."
  
  (let* ((lx (x-points bpf-obj)) ls max (step 1))
    (when  (flatp lx) (setf lx (list lx)))     
    (setf ls (if (> (length (first lx)) (length (second lx))) (first lx) (second lx))
          max (apply #'max ls))    
    (when (and scale-length (not (zerop scale-length)))
      (setf step (/ max scale-length)))
    (mapcar #'(lambda (x) (reverse (pw::transfer bpf-obj x)))
            (pw::arithm-ser 0  step (apply #'max ls))))
  )


;;;;===========================
;;;; Voice form definition.
;;;;===========================

#+:OM 
(om::defmethod! om::VoiceProfile ((bpf-obj t) (voice-number t) horizontal-choice
                             &optional scale-length)
  (set-box-voiceProfile bpf-obj voice-number horizontal-choice scale-length))
#+:OM
(om::defmethod! om::x-bpf_prof ((bpf-obj t) (voice-number t) horizontal-choice
                                  &optional scale-length c-imp)
  :initvals '(nil 0 nil nil 1)
  :indoc '("bpf object" "voice number" "constrain horizontal?" "scale" "cnstr importance") :icon 202
  :doc "External points horizontal profile by a BPF. Defines a profile of selected points
 in contiguous objects using a bpf editor.
'bpf-obj' takes the output of a BPF box
'voice-number' denotes the points section (called a 'voice') to be constrained
 It can be an integer, 'l' (lower points) or 'u' for the upper points.
The voice follows the profile specified by the curve in the BPF. If 'horizontal-choice'
is NIL, then a horizontal line in the curve means there is no constraint in that region.
Otherwise, a horizontal line just constraints the voice to repeat the same point.
'scale-length' can be an integer or a list of two integers. A simple integer denotes
the number of objects the curve should be applied to. A list of integers gives a
range of objects for applying the constraint. For example 'scale-length'= (5 15) says
that the given voice in points going from the sixth to the sixteenth should follow
the curve in the BPF."
(eval `(defun ,(gensym "bpf-vprof") ()
           ',(includeConstraint-Importance
              (set-box-voiceProfile bpf-obj voice-number horizontal-choice scale-length) 
              c-imp))))

(om::defmethod! om::bpf-vprof ((bpf-obj t) (voice-number t) horizontal-choice
                                  &optional scale-length c-imp)
  :initvals '(nil 0 nil nil 1)
  :indoc '("bpf object" "voice number" "constrain horizontal?" "scale" "cnstr importance") :icon 202
  :doc "Kept for compatibility. See x-bpf_prof"
(om::x-bpf_prof bpf-obj voice-number horizontal-choice scale-length c-imp))

#+:PW
(pw::defunp bpf-vprof ((bpf-obj pw::list (:value '() :type-list (bpf list)))
                          (voice-number fix>=0) 
                          (horizontal-choice pw::list (:value '() :type-list ()))
                          &optional (scale-length pw::list (:value '() :type-list (fix)))) list
 "Voice profile by BPF. Defines a given voice profile using a bpf editor.
'bpf-obj' takes the output of a BPF box
'voice-number' is the voice number i.e. an integer, or 'u' for the upper voice.
The voice follows the profile specified by the curve in the BPF. If 'horizontal-choice'
is NIL, then a horizontal line in the curve means there is no constraint in that region.
Otherwise, a horizontal line just constraints the voice to repeat the same note.
'scale-length' can be an integer or a list of two integers. A simple integer denotes
the number of chords the curve should be applied to. A list of integers gives a
range of chords for applying the constraint. For example 'scale-length'= (5 15) says
that the given voice in chords going from the sixth to the sixteenth should follow
the curve in the BPF."
(set-box-voiceProfile bpf-obj voice-number horizontal-choice scale-length))
  
;;In file "MCSPmenus". cannot g-round bpf points in OM [Camilo 03/22/98]
(defun set-box-voiceProfile (bpf-obj voice-number horizontal-choice &optional scale-length)
  (let ((xs (x-points bpf-obj)) all-xs x-inf x-sup step)
    (unless (flatp xs)
      (setf xs (first xs))
      (warn "only the first bpf will be used"))
    (setq x-inf (apply #'min xs) x-sup (apply #'max xs))
    (unless (numberp scale-length)
    #+:OM (setq scale-length (expand-lst scale-length))
    #+:PW (setq scale-length (pw::expand-lst scale-length))
    )
    (cond ((numberp scale-length)
           (setq step (/ (- x-sup x-inf) scale-length))
           #+:OM (setq xs  (om::arithm-ser 0  (1- scale-length) 1))
           #+:PW (setq xs  (pw::arithm-ser 0  1 (1- scale-length)))
           #+:OM (setq all-xs (g-round (om::arithm-ser x-inf  x-sup step)))
           #+:PW (setq all-xs (pw::g-round (pw::arithm-ser x-inf step x-sup))))
          ((consp scale-length)
           (setq step (/ (- x-sup x-inf) (length scale-length)))
           (setq xs scale-length)
            #+:OM (setq all-xs   (om::arithm-ser x-inf  x-sup step))
            #+:PW (setq all-xs (pw::g-round (pw::arithm-ser x-inf step x-sup))))
          (t #+:OM (setf all-xs (om::arithm-ser x-inf  x-sup 1))
             #+:PW (pw::arithm-ser x-inf  1 x-sup)
             (setq xs all-xs)))
    #+:OM (make_bpf_voice xs (mapcar #'(lambda (x) (transfer bpf-obj x)) all-xs)
                    voice-number nil (not horizontal-choice))
    #+:PW (make_bpf_voice  xs  (mapcar #'(lambda (x) (pw::transfer bpf-obj x)) all-xs)
                              voice-number nil  (not horizontal-choice)))
  )

#|
(defclass  Ambitus-constr (pw::C-patch-multi-function) ())

(defmethod initialize-instance :after ((self Ambitus-constr) &key controls)
  (declare (ignore controls))
  (pw::set-window-title (pw::application-object self) (string-downcase (pw::pw-function self)))
  (setf (pw::pw-function-string self) (string-downcase (pw::pw-function self))))

(defmethod pw::patch-value ((self ambitus-constr) ob)
  (let* ((bpf-obj(call-next-method))
         (xs (pw::get-slot bpf-obj 'x-points)) all-xs first-xs last-xs ys)
    (if (flatp xs)
      (progn (warn "you defined only one curve for the ambitus")
        (setf first-xs (first xs) last-xs (car (last xs)))
        (setf all-xs (pw::arithm-ser first-xs 1 last-xs))
        (make_constraint_ambitus*  (list all-xs all-xs)
                               (list
                                (setf ys (mapcar #'(lambda (x) (pw::transfer bpf-obj x)) all-xs))
                                ys)) )
      (progn
        (setf first-xs (caar xs) last-xs (caar (last xs)))
        (setf all-xs (pw::arithm-ser first-xs 1 last-xs))
        (make_constraint_ambitus*  (list all-xs (copy-list all-xs))
                                   (pw::mat-trans (mapcar #'(lambda (x) (pw::transfer bpf-obj x)) all-xs)) 
                                   ) )
    )
  ))

(pw::defunp  amb-cnstr  (  (tlist pw::fix>0s?) (y-list pw::list) 
                               (z pw::bpf)) list 
             "Constraints an ambitus using a bpf editor.
The first sublist in y-list defines the upper limit of the ambitus
The second sublist in y-list defines y-coordinates for the lower limit of the ambitus.
NOTE: This is different from BPF-ambitus, which DEFINES an ambitus. In 'ambitus-const'
it is assumed that the ambitus is already defined in some way (for instance by a data base
of chords) and one wants to further constraint it."
  (declare (ignore tlist y-list z))
  )


(pw::Pw-addmenu-fun  FormsConstraint 'amb-cnstr 'Ambitus-constr)
(defvar MusicConstraint (pw::new-menu  "Constraints formation"))
(pw::add-menu-items MusicEngine MusicConstraint)
|#

;;;#+:OM (om::defmethod! om::ambitus-constr ((bpf-obj t)) (om::bpf-ambitus bpf-obj))
;;; GAS 5/10/98
#+:OM (om::defmethod! om::ambitus-constr ((bpf-obj t)) (om::bpf-amb bpf-obj))
#+:OM
(om::defmethod! om::bpf-amb ((bpf-obj t)) :initvals '(nil) :indoc '("bpf object") :icon 202
  :doc "Constraints an ambitus using a bpf editor.
The first sublist in y-list defines the upper limit of the ambitus
The second sublist in y-list defines y-coordinates for the lower limit of the ambitus.
NOTE: This is different from BPF-ambitus, which DEFINES an ambitus. In 'ambitus-const'
it is assumed that the ambitus is already defined in some way (for instance by a data base
of chords) and one wants to further constraint it."
(let ((xs (x-points bpf-obj)) all-xs first-xs last-xs ys)
    (if (flatp xs)
      (progn (warn "you defined only one curve for the ambitus")
        (setf first-xs (first xs) last-xs (car (last xs)))
        (setf all-xs (om::arithm-ser first-xs last-xs 1))
        (make_constraint_ambitus*  (list all-xs all-xs)
                               (list
                                (setf ys (mapcar #'(lambda (x) (transfer bpf-obj x)) all-xs))
                                ys)) )
      (progn
        (setf first-xs (caar xs) last-xs (caar (last xs)))
        (setf all-xs (om::arithm-ser first-xs last-xs 1))
        (make_constraint_ambitus*  (list all-xs (copy-list all-xs))
                                   (om::mat-trans (mapcar #'(lambda (x) (transfer bpf-obj x)) all-xs)) 
                                   ) )
    )
  ))

#+:PW
(pw::defunp bpf-amb ((bpf-obj pw::list (:value '() :type-list ()))) list
  "Constraints an ambitus using a bpf editor.
The first sublist in y-list defines the upper limit of the ambitus
The second sublist in y-list defines y-coordinates for the lower limit of the ambitus.
NOTE: This is different from BPF-ambitus, which DEFINES an ambitus. In 'ambitus-const'
it is assumed that the ambitus is already defined in some way (for instance by a data base
of chords) and one wants to further constraint it."
(let ((xs (x-points bpf-obj)) all-xs first-xs last-xs ys)
    (if (flatp xs)
      (progn (warn "you defined only one curve for the ambitus")
        (setf first-xs (first xs) last-xs (car (last xs)))
        (setf all-xs (pw::arithm-ser first-xs 1 last-xs))
        (make_constraint_ambitus*  (list all-xs all-xs)
                               (list
                                (setf ys (mapcar #'(lambda (x) (pw::transfer bpf-obj x)) all-xs))
                                ys)) )
      (progn
        (setf first-xs (caar xs) last-xs (caar (last xs)))
        (setf all-xs (pw::arithm-ser first-xs 1 last-xs))
        (make_constraint_ambitus*  (list all-xs (copy-list all-xs))
                                   (pw::mat-trans (mapcar #'(lambda (x) (transfer bpf-obj x)) all-xs)) 
                                   ) )
    )
  ))


#|
(pw::pw-addmenu-fun MusicConstraint 'UserConstraint)
(pw::pw-addmenu-fun MusicConstraint 'fixed-notes)
(pw::pw-addmenu-fun MusicConstraint 'vert-logi-filter)
(pw::pw-addmenu-fun MusicConstraint 'filt-pass-band)
(pw::pw-addmenu-fun MusicConstraint 'chord-logi-filter)
(pw::pw-addmenu-fun MusicConstraint 'chord-length)
(pw::pw-addmenu-fun MusicConstraint 'fixed-chords)
(pw::pw-addmenu-fun MusicConstraint 'ambitus)  
(pw::pw-addmenu-fun MusicConstraint 'Int-hori)
(pw::pw-addmenu-fun MusicConstraint 'hori-logi-filter)
(pw::pw-addmenu-fun MusicConstraint 'nb-dir=OK)
(pw::pw-addmenu-fun MusicConstraint 'nb-mov//OK)
(pw::pw-addmenu-fun MusicConstraint 'seuil-repet)
(pw::pw-addmenu-fun MusicConstraint 'renouv-haut)
(pw::pw-addmenu-fun MusicConstraint 'int//forbid)
(pw::pw-addmenu-fun MusicConstraint 'rep-par-dens)
;(pw::pw-addmenu-fun MusicConstraint 'homogeneidad)
|#

;;; The music solver engine.
#|
(pw::pw-addmenu-fun MusicEngine 'Solver)
(pw::pw-addmenu-fun MusicEngine 'adjoint-problem)
;;(pw::pw-addmenu-fun  FormsConstraint 'n-sample-fun)

(om::omNG-add-element om::*package-user* (om::omNG-protect-object (fdefinition 'solveCSP)))
(om::omNG-add-element om::*package-user* (om::omNG-protect-object (fdefinition 'adjoined-problem)))


(defvar Utilities (pw::new-menu  "Utilities"))
(pw::add-menu-items MusicEngine Utilities)

(pw::pw-addmenu-fun Utilities 'hd-constraints::funct-and)
(pw::pw-addmenu-fun Utilities 'hd-constraints::funct-or)
(pw::pw-addmenu-fun Utilities 'hd-constraints::funct-not)


;;; This doesn't really works properly
(defun speaker ()
  "Evaluating this box, you can play sound in the speaker"  
  (load "root;PW 2.0:user-library:Situation1.1b:Utilities:to-run-speaker")
  )

;;(pw::pw-addmenu-fun Utilities 'speaker)
|#

(defun partial-solution ()
  "Returns a partial solution. After 'BREAKing' or 'ABORTing' the search
you can use this box to see the current partial solution." 
  (normal-form fvtempos::Instancias))

#+:OM
(om::defmethod! om::part-sol () :icon 192
  :doc "Returns a partial solution. After 'BREAKing' or 'ABORTing' the search you can use this box to see the current partial solution."
  (partial-solution) )

#+:PW
(pw::defunp part-sol () list
 "Returns a partial solution. After 'BREAKing' or 'ABORTing' the search
you can use this box to see the current partial solution."
  (partial-solution) )

;;(pw::pw-addmenu-fun Utilities 'partial-solution)

(defun fill-interp-holes (expression)
  (let (all-form first-index indexes last-index current-index exp1 (form expression))
    (do ((reps)) ((null form) (nreverse all-form))
      (setq first-index (first form) indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (pop form) indexes))
      (setq current-index (first indexes))
      (setq reps (pop form))
      ;;(push reps all-form)
      (multiple-value-bind (expfrom expto) (get-interp-expr reps)
        (unless last-index
          (when (plusp first-index)
            (dotimes (i first-index) (push i all-form))
            (push expfrom all-form)
            (setq last-index (1- first-index))))
        (when (and last-index (plusp (- first-index last-index 1)))
          (do ((index (1+ last-index) (1+ index))) ((= index first-index))
            (push index all-form))
          (if (equal expfrom exp1)
            (push expfrom all-form)
            (push `(interp ,exp1 ,expfrom) all-form)) )
        (setq last-index current-index exp1 expto)
        (setq all-form (append (list reps) indexes all-form))))))

(defun get-interp-expr (expr)
  (if (flatp expr)
    (values expr expr)
    (if (and (symbolp (first expr)) (string= (first expr) *interp-key*))
      (values (second expr) (third expr))
      (error "can't fill this expression" expr))))

#+:OM
(om::defmethod! om::default-fill ((expression list))
  :initvals '((0_7 (interp (4 7 11) (2 5 6)) 12_15 (interp (3 8 10) (4 7 11))))
  :indoc '("expression")
  :doc "fills holes in interpolation expressions having the standard constraint syntax. For example, (0_7 (interp (4 7 11) (2 5 6)) 12_15 (interp (3 8 10) (4 7 11))) is translated into (0_7 (interp (4 7 11) (2 5 6)) 8_11 (interp (2 5 6) (3810)) 12_15 (interp (3 8 10) (4 7 11)))"
  (fill-interp-holes (fill-expression expression) ))

#+:PW
(pw::defunp default-fill ((expression pw::list 
                               (:value '(0_7 (interp (4 7 11) (2 5 6)) 12_15 (interp (3 8 10) (4 7 11)))))) list
   "fills holes in interpolation expressions having the standard constraint syntax.
 For example, (0_7 (interp (4 7 11) (2 5 6)) 12_15 (interp (3 8 10) (4 7 11)))
 is translated into
 (0_7 (interp (4 7 11) (2 5 6)) 8_11 (interp (2 5 6) (3810)) 12_15 (interp (3 8 10) (4 7 11)))"
   (fill-interp-holes (fill-expression expression) ))

#+:PW
(pw::defunp MaxChords ( (x pw::fix (:value fvtempos::MaxChords))) pw::fix
            "Redefines the value of the variable MaxChords to x. MaxChords denotes the number
 of chords given by default in some constraint definitions. This does NOT limit the number of
chords allowed in a problem (i.e. you can run problems with a greater number of chords than
MaxChords)."
  (printl "Old value of MaxChords" fvtempos::MaxChords 
          "New value of MaxChords" (setf fvtempos::MaxChords x))
  
  )

#+:OM
(om::defmethod! om::MaxChords ((x number))
        :doc    "Redefines the value of the variable MaxChords to x. MaxChords denotes the number
 of chords given by default in some constraint definitions. This does NOT limit the number of
chords allowed in a problem (i.e. you can run problems with a greater number of chords than
MaxChords)."
  (printl "Old value of MaxChords" fvtempos::MaxChords 
          "New value of MaxChords" (setf fvtempos::MaxChords x))
  
  )





