(in-package :om)


;===========SAVE

(defun get-root-parent (obj)
   (if (parent obj) 
     (get-root-parent (parent obj))  
     obj))

;Una lista de posiciones del objecto en su container mayor
(defun cons-container-path (obj)
   (if (not (parent obj))
     nil
     (let ((parent (parent obj))
           (current obj) rep)
       (loop while parent do
             (push (position current (inside parent) :test 'equal) rep)
             (setf current parent
                   parent (parent current)))
       (reverse rep))))

(defun get-obj-from-container-path (obj path)
   (let ((rep obj))
     (loop for item in path do
           (when (and rep (container-p rep) (integerp item))
             (setf rep (nth item (inside rep)))))
     rep))


(defmethod omNG-save ((self simple-container) &optional (values? nil))
   "Cons a Lisp expression that retunr a copy of self when it is valuated."
   (when (omclass-p (class-of self))
     (let ((theclass (class-name (class-of self)))
           (exeption-p (execption-save-p self))
           ;;; jb 
           (tonalite (tonalite self))
           extralist patchlist muslist)
       (unless (parent self)
         (setf extralist (cons-extra-pairs self))
         (setf muslist (cons-mus-color self))
         (setf patchlist (cons-patch-pairs self)))
       (if exeption-p
         (let* ((intslots (set-difference (get-all-initargs-of-class theclass) (get-all-initargs-of-class exeption-p)
                                          :test 'string-equal :key 'name))
                (slots (mapcar #'(lambda (slot)
                                   `(setf (,(internp (name slot) (symbol-package theclass))  newobj)
                                          ,(omNG-save (funcall (internp (name slot) (slot-package slot))  self) values?))) 
                               intslots)))
           `(let ((newobj ,(save-exepcion self)))
              ,.slots
              (load-port-info newobj ',(get-port self))
              (init-mus-color newobj ',(mapcar #'(lambda (item) (list (car item) (omng-save (cadr item)))) muslist))
              (set-extra-pairs newobj ',extralist)
              (set-patch-pairs newobj ',patchlist)
              (set-name newobj ,(get-name self))
              (set-tonalite newobj ,(omng-save tonalite))  ;;; jb
              newobj))
         (let ((slots (mapcan #'(lambda (slot)
                                  (list (string2initarg (name slot)) 
                                        (eval `(omNG-save (,(internp (name slot) (slot-package slot))  ,self) ,values?)))) 
                              (get-all-initargs-of-class theclass))))
           `(if (find-class ',theclass nil) 
              (let ((newobj (make-instance ',theclass ,.slots :from-file t)))
                (load-port-info newobj ',(get-port self))
                (init-mus-color newobj ',(mapcar #'(lambda (item) (list (car item) (omng-save (cadr item)))) muslist))
                (set-extra-pairs newobj ',extralist)
                (set-patch-pairs newobj ',patchlist)
                (set-name newobj ,(get-name self))
                (set-tonalite newobj ,(omng-save tonalite))  ;;; jb
                newobj)))))))


(defmethod omng-save ((self chord)  &optional (values? nil))
  (let ((tonlist (get-tonal-values self))
        (tonalite (tonalite self))
        extralist clist)
     (unless (parent self)
         (setf extralist (cons-extra-pairs self))
         (setf clist (cons-mus-color self)))
    (cond ((and (gnotes self) tonlist)
           `(let ((thechord (make-instance ',(type-of self)
                                           :LMidic ',(Lmidic self)
                                           :Ldur ',(Ldur self)
                                           :LVel ',(LVel self)
                                           :LOffset ',(LOffset self)
                                           :Lchan ',(Lchan self))))
              (setf (gnotes thechord) (make-instance 'grace-notes
                                                     :glist ',(glist (gnotes self))
                                                     :thechord thechord))
              (restore-tonalite thechord ',tonlist)
              (load-port-info thechord ',(get-port self))
              (init-mus-color thechord ',(mapcar #'(lambda (item) (list (car item) (omng-save (cadr item)))) clist))
              (set-extra-pairs thechord ',extralist)
              (set-tonalite thechord ,(omng-save tonalite))
              thechord))
          (tonlist
           `(let ((thechord (make-instance ',(type-of self)
                                           :LMidic ',(Lmidic self)
                                           :Ldur ',(Ldur self)
                                           :LVel ',(LVel self)
                                           :LOffset ',(LOffset self)
                                           :Lchan ',(Lchan self))))
              (restore-tonalite thechord ',tonlist)
              (load-port-info thechord ',(get-port self))
              (init-mus-color thechord ',(mapcar #'(lambda (item) (list (car item) (omng-save (cadr item)))) clist))
              (set-extra-pairs thechord ',extralist)
              (set-tonalite thechord ,(omng-save tonalite))
              thechord))
          ((gnotes self)
           `(let ((thechord (make-instance ',(type-of self)
                                           :LMidic ',(Lmidic self)
                                           :Ldur ',(Ldur self)
                                           :LVel ',(LVel self)
                                           :LOffset ',(LOffset self)
                                           :Lchan ',(Lchan self))))
              (setf (gnotes thechord) (make-instance 'grace-notes
                                                     :glist ',(glist (gnotes self))
                                                     :thechord thechord))
              (load-port-info thechord ',(get-port self))
              (init-mus-color thechord ',(mapcar #'(lambda (item) (list (car item) (omng-save (cadr item)))) clist))
              (set-extra-pairs thechord ',extralist)
              (set-tonalite thechord ,(omng-save tonalite))
              thechord))
          (t `(let ((thechord (make-instance ',(type-of self)
                                             :LMidic ',(Lmidic self)
                                             :Ldur ',(Ldur self)
                                             :LVel ',(LVel self)
                                             :LOffset ',(LOffset self)
                                             :Lchan ',(Lchan self)
                                             )))
                (load-port-info thechord ',(get-port self))
                (init-mus-color thechord ',(mapcar #'(lambda (item) (list (car item) (omng-save (cadr item)))) clist))
                (set-extra-pairs thechord ',extralist)
                (set-tonalite thechord ,(omng-save tonalite))
                thechord))
          )))


(defmethod cons-mus-color ((self simple-container))
  (unless (= 0 (om-color-r (mus-color self)) (om-color-g (mus-color self)) (om-color-b (mus-color self)))
    (list (list (reverse (cons-container-path self)) (mus-color self)))))

(defmethod cons-mus-color ((self container))
  (append (call-next-method)
          (loop for item in (inside self)
                append (cons-mus-color item))))

(defmethod cons-extra-pairs ((self simple-container))
  (loop for obj in (extra-obj-list self)
        collect (list (reverse (cons-container-path self)) (omng-save obj))))

(defmethod cons-extra-pairs ((self container))
  (append (call-next-method)
          (loop for item in (inside self)
                append (cons-extra-pairs item))))

(defmethod cons-patch-pairs ((self simple-container))
  (when (mus-patch self)
    (list (list (cons-container-path self) (om-save (mus-patch self))))))

(defmethod cons-patch-pairs ((self container))
  (append (call-next-method)
          (loop for item in (inside self)
                append (cons-patch-pairs item))))

(defmethod set-extra-pairs ((self simple-container) list)
  (loop for item in list do
        (let ((obj (get-obj-from-container-path self (car item))))
          (when obj
            (setf (extra-obj-list obj) (cons (eval (second item)) (extra-obj-list obj)))))))

(defmethod set-patch-pairs ((self simple-container) list)
  (loop for item in list do
        (let ((obj (get-obj-from-container-path self (car item))))
          (when obj
            (setf (mus-patch obj) (eval (second item)))
            (remake-references obj)))))


(defmethod init-mus-color ((self simple-container) list)
      (loop for item in list do
            (let ((obj (get-obj-from-container-path self (car item))))
              (when obj
                (setf (mus-color obj) (om-correct-color (eval (second item))))))))

(defmethod set-extra-pairs ((self t) list) t)

(defmethod set-patch-pairs ((self t) list) t)

(defmethod init-mus-color ((self t) list) t)



;-----------------------------
(defmethod convert-extra ((self extra-objet)) self)

(defmethod convert-extra ((self t)) nil)

(defmethod convert-extra ((self picture)) 
  (when (thepict self)
    (make-instance 'pict-extra 
      :name (name self)
      :thepict (thepict self))))

;-----------------------------

(defmethod! add-extra ((self simple-container) extras path &optional newobj)
   :icon 162
   :initvals '(nil nil nil nil)
   :menuins '((3 (("return new object" t) ("modify input object" nil))))
   :indoc '("a musical object" "a extra or list of extra objects" "target path in object" "modify input or return new object")
   :doc "
Adds one or more EXTRA object(s) (<extras> see classes in the Score/Extra/ package) in <self>.

<path> is a list (a b c) determines the optional path to an internal object, e.g. (1 0) in a chord-seq means chord number 1 in the sequence, note number 0 in the chord.

<newobj> is an optional input determining if a new object should be returned (T), or if the input object should be modified (NIL).
"
   (let* ((rep (if newobj (clone self) self))
         (extralist (clone (loop for item in (list! extras) collect (convert-extra item))))
         (obj (get-obj-from-container-path rep path)))
     (when obj
       (loop for item in extralist do
             (set-extra-in-list item obj))
       (if newobj rep extralist)
       )))

(defmethod! add-extra-list ((self container) extras mode &optional newobj)
   :icon 162
   :initvals '(nil nil "loop" nil) 
   :indoc '("a musical object" "a list of extra objects" "insert mode" "modify input or return new object")
   :menuins '(( 2 (("loop" "loop")  ("last" "last") ("exact" "exact"))) 
              (3 (("return new object" t) ("modify input object" nil))))
 :doc "
Adds EXTRA object(s) (<extras> see classes in the Score/Extra/ package) in <self>.

The extras are distributed successively in the internal objects in <self> (e.g. chords in a chord-seq) depending on <mode>:
- 'loop' means that the Extra list is repeated untill all chords have an extra
- 'last' means that the last element in the list is repeated in the (possible) remaining objects
- 'exact' means that the extras are assigned only once to the nth first objects (n = length of the extra list)

<newobj> is an optional input determining if a new object should be returned (T), or if the input object should be modified (NIL).
"
   (let* ((rep (if newobj (clone self) self))
          (objs (inside rep))
          (extralist (loop for item in (list! extras) collect (convert-extra item)))
          (n (length extralist)))
     (loop for obj in objs
           for i = 0 then (cond
                           ((string-equal mode "loop") (mod (+ i 1) n))
                           ((string-equal mode "last") (if (< i (- n 1)) (+ i 1) (- n 1)))
                           (t (if (and i (< i (- n 1))) (+ i 1))))
           do
           (when (and i (nth i extralist))  ;;; you can put a NIL in the list to skip an object
             (set-extra-in-list (clone (nth i extralist)) obj)))
     (if newobj rep extralist)))

(defmethod! add-extra-list ((self simple-container) extras mode &optional newobj) t)

;-----------------------------
(defmethod! get-extras ((self simple-container) filter)
   :icon 162
   :initvals '(nil "all") 
   :indoc '("a musical object" "type of extra")
   :menuins '(( 1 (("all" "all")  ("head" "head") ("vel" "vel") ("char" "char") 
                  ("text" "text") ("pict" "pict") ("line" "line"))))
   :doc "
Returns the list of EXTRA objects in <self>.

<filter> allows to select only a particular type of extra.
"
   (let* ((extras (get-all-extras self)))
     (loop for item in extras
           when (filtre-extra-p item filter) collect item)))


(defmethod filtre-extra-p ((self extra-objet) test)
   (cond
    ((string-equal test "all") t)
    ((string-equal test "head") (head-extra-p self))
    ((string-equal test "vel") (vel-extra-p self))
    ((string-equal test "char") (char-extra-p self))
    ((string-equal test "text") (text-extra-p self))
    ((string-equal test "pict") (pict-extra-p self))
    ((string-equal test "line") (line-extra-p self))))
;-----------------------------

(defmethod! delete-extras ((self list))
   :icon 162
   :initvals '(nil) 
   :indoc '("extra list")
   :doc "
Deletes a list of EXTRA objects (<self>) from their container object.
"
   (loop for item in self do
         (delete-extra item)))

(defmethod! remove-extras ((self simple-container) &optional (filter "all") newobj)
   :icon 162
   :initvals '(nil "all" nil) 
   :indoc '("a musical object" "type of extra" "modify input or return new orbject")
   :menuins '(( 1 (("all" "all")  ("head" "head") ("vel" "vel") ("char" "char") 
                  ("text" "text") ("pict" "pict") ("line" "line")))
              (2 (("return new object" t) ("modify input object" nil))))
   :doc "
Removes all EXTRA objects from <self>.

<filter> allows to select only a particular type of extra.


"
   (let ((rep (if newobj (clone self) self)))
     (delete-extras (get-extras rep filter))
     (if newobj rep (get-extras rep "all"))))


(defmethod! set-extras-from-model ((self list) (model list))
   :icon 162
   :initvals '(nil nil) :indoc '("a list of chords" "a list or chord-seq with tagged chords")
   (let ((modlist (loop for chord in model collect
                        (list (lmidic chord) 
                              (get-extras chord "all"))))
         (rep (clone self)))
     (loop for chord2 in rep do
           (delete-extras (get-extras chord2 "all"))
           (let ((mod (find (lmidic chord2) modlist :key 'car :test 'equal)))
             (when mod
               (loop for item in (cadr mod) do
                     (set-extra-in-list (clone item) chord2))
               )))
     rep))


;;;;=====================

(defmethod! set-extras-from-model ((self list) (model chord-seq))
     (set-extras-from-model self (inside model)))
             
(defmethod! set-extras-from-model ((self chord-seq) (model list))
            (objfromobjs (set-extras-from-model (inside self) model) (make-instance 'chord-seq)))

(defmethod! set-extras-from-model ((self chord-seq) (model chord-seq))
            (objfromobjs (set-extras-from-model (inside self) (inside model)) (make-instance 'chord-seq)))

;;;;=====================

