(in-package :om)

;;; mf-info gives : (midi-number, onset-time(ms), duration(ms), velocity, channel)

(defmacro note-onset (note) `(second ,note))
(defmacro note-pitch (note) `(first ,note))
(defmacro note-channel (note) `(fifth ,note))
(defmacro note-duration (note) `(third ,note))
(defmacro note-velocity (note) `(fourth ,note))
 
;;; UTILITIES

(defun hashlist (table)
  (let ((set nil))
    (maphash #'(lambda (x y)
                 (setf set (append (mapcar #'(lambda (z)
                                               (list z x))
                                           y)
                                   set)))
             table)
    set))


;;; LZ DATA IS TOO BIG: NEVER SAVE/LOCK THE VALUE

(defclass boxforlz (OMBoxCall) ())

(defmethod get-boxcallclass-fun ((self (eql 'LZify))) 'boxforlz)

(defmethod omNG-save ((self boxforlz) &optional (values? nil))
  "Save a box"
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         )
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) nil nil ,(frame-name self) ,(numouts self))))

(defun update-lz-boxes (oldbox newbox)
  (setf (value newbox) nil)
  (setf (frame-position newbox) (borne-position (frame-position oldbox)))
  (setf (frame-size newbox) (frame-size oldbox))
  (setf (frame-name newbox) (frame-name oldbox))
  (setf (allow-lock newbox) nil)
  (setf (inputs newbox) (eval (omNG-copy (inputs oldbox))))
  (set-box-to-inputs (inputs newbox) newbox)
  newbox)

(defmethod omNG-copy ((self boxforlz))
  `(let* ((copy ,(omNG-make-new-boxcall (fdefinition (reference self))
                                        (frame-position self)
                                        (name self))))
     (setf copy (update-lz-boxes ,self copy))
     copy))


;;; CLASSES

(defclass LZtree ()
  ((symb :initform 0 :accessor symb :initarg :symb)
   (children :initform nil :type list :accessor children :initarg :children)))

(defclass LZpatterntree (LZtree)
  ((secund :initform nil :type list :accessor secund :initarg :secund)))

(defclass LZnext ()
  ((symb :initform 0 :accessor symb :initarg :symb)
   (proba :accessor proba :initarg :proba)
   (correctedproba :accessor correctedproba :initform nil)
   (secund :initform (make-hash-table) :accessor secund :initarg :secund)
   (param :initform nil :accessor param :initarg :param)
   (context :initform nil :accessor context)))

(defclass LZparam ()
  ((maxPast :accessor maxPast :initarg maxPast)
   (minPast :accessor minPast :initarg minPast)
   (minComplex :accessor minComplex :initarg minComplex)
   (equiv1 :accessor equiv1 :initarg equiv1)
   (equiv2 :accessor equiv2 :initarg equiv2)))

(defclass LZcontnode (LZtree)
  ((next :initform nil :type list :accessor next :initarg :next)
   (subsize :initform nil :initarg :subsize :accessor subsize)
   (param :initform nil :type list :initarg :param :accessor param)
   (belongs :initform nil :initarg :belongs :accessor belongs)))

(defclass LZconttree ()
  ((dico :type LZcontnode :accessor dico :initarg :dico)
   (reconstr :type function :accessor reconstr :initarg :reconstr)))

(defclass PSTnode (LZtree)
  (;(nbnext :initform 0 :type integer :accessor nbnext)
   (next :initform (make-hash-table :test 'equal) :type hashtable :accessor next :initarg :next)
   (mark :initform nil :initarg :mark :accessor mark)
   (pos :accessor pos :initarg :pos :initform nil)))

(defclass PST ()
  ((dico :type PSTnode :accessor dico :initarg :dico)
   (reconstr :type function :accessor reconstr :initarg :reconstr)))

(defclass PSTnext ()
  ((proba :accessor proba :initarg :proba :initform 0)
   (secund :accessor secund :initarg :secund :initform nil)))

;(defmethod omng-save ((self LZpatterntree) &optional (values? nil))
;  `(make-instance 'LZpatterntree
;     :secund ,(omng-save (secund self))
;     :symb ,(omng-save (symb self))
;     :children ,(omng-save (children self))))

;(defmethod omng-save ((self LZconttree) &optional (values? nil))
;  `(make-instance 'LZconttree
;     :dico ,(omng-save (dico self))
;     :reconstr (symbol-function ,(omng-save (function-name (reconstr self))))))

;(defmethod omng-save ((self LZcontnode) &optional (values? nil))
;  `(make-instance 'LZcontnode
;     :symb ,(omng-save (symb self))
;     :children ,(omng-save (children self))
;     :subsize nil
;     :next ,(omng-save (next self))))

;(defmethod omng-save ((self LZnext) &optional (values? nil))
;  `(make-instance 'LZnext
;     :symb ,(omng-save (symb self))
;     :proba ,(omng-save (proba self))
;     :secund ,(omng-save (secund self))))

;(defmethod omng-copy ((self LZpatterntree))
;  `(make-instance 'LZpatterntree
;     :secund ,(omng-copy (secund self))
;     :symb ,(omng-copy (symb self))
;     :children ,(omng-copy (children self))))

;(defmethod omng-copy ((self LZconttree))
;  `(make-instance 'LZconttree
;     :dico ,(omng-copy (dico self))
;     :reconstr ,(omng-copy (reconstr self))))

;(defmethod omng-copy ((self LZcontnode))
;  `(make-instance 'LZcontnode
;     :symb ,(omng-copy (symb self))
;     :children ,(omng-copy (children self))
;     :subsize nil
;     :next ,(omng-copy (next self))))

;(defmethod omng-copy ((self LZnext))
;  `(make-instance 'LZnext
;     :symb ,(omng-copy (symb self))
;     :proba ,(omng-copy (proba self))
;     :secund ,(omng-copy (secund self))))

(defun posintree (subtree tree)
  (if (eq tree subtree)
    (list (symb tree))
    (loop with memo 
          for child in (children tree)
          when (setf memo (posintree subtree child))
          return (cons (symb tree) memo))))

;;; FUNCTIONS USED AS MAIN INFORMATION SYMBOL FILTER FOR LZIFY

(defun all (symb)
  symb)

(defun pitch (symb)
  (mapcar #'(lambda (channel)
                        (if (listp channel)
                          (mapcar #'car channel)
                          0))
                    (car symb)))

(defun newpitch (symb)
  (mapcar #'(lambda (channel)
                        (if (listp channel)
                          (or (mapcan #'(lambda (x)
                                          (and (> x 0) (list x)))
                                      (mapcar #'car channel)) 0)
                          0))
                    (car symb)))

(defun pitchduration (symb)
  (list (pitch symb)
        (cadr symb)))

(defun newpitchduration (symb)
  (list (newpitch symb)
        (cadr symb)))

;;; FUNCTIONS USED AS SECUNDARY INFORMATION SYMBOL FILTER FOR LZIFY

(defun oldpitch (symb)
  (mapcar #'(lambda (channel)
                        (if (listp channel)
                          (or (mapcan #'(lambda (x)
                                          (and (<= x 0) (list x)))
                                      (mapcar #'car channel)) 0)
                          0))
                    (car symb)))

(defun lzduration (symb)
  (cadr symb))

(defun durationoldpitch (symb)
  (list (lzduration symb)
        (oldpitch symb)))

(defun velocity (symb)
  (mapcar #'(lambda (channel)
              (if (listp channel)
                (mapcar #'cadr channel)
                0))
          (car symb)))

(defun durationvelocity (symb)
  (list (lzduration symb)
        (velocity symb)))

(defun oldpitchvelocity (symb)
  (list (mapcar #'(lambda (channel)
                    (if (listp channel)
                      (or (mapcan #'(lambda (x)
                                      (and (<= (car x) 0) (list x)))
                                  channel) 0)
                      0))
                (car symb))
        (mapcar #'(lambda (channel)
                    (if (listp channel)
                      (or (mapcan #'(lambda (x)
                                      (and (> (car x) 0) (list (cadr x))))
                                  channel) 0)
                      0))
                (car symb))))

(defun durationoldpitchvelocity (symb)
  (cons (lzduration symb)
        (oldpitchvelocity symb)))

(defun nothing (symb)
  nil)

;;; FUNCTIONS USED AS SYMBOL RECONSTRUCTOR FOR LZGENERATE

(defun pitchduration_velocity (symb secund branch generated)
  (cons (loop for channel1 in (car symb)
              for channel2 in (car secund)
              collect (if (listp channel1)
                        (loop for pitch in channel1
                              for dyn in channel2
                              collect (list pitch dyn))
                        0))
        (cdr symb)))

(defun pitchduration_nothing (symb secund branch generated)
  (cons (loop for channel1 in (car symb)
              collect (if (listp channel1)
                        (loop for pitch in channel1
                              collect (list pitch 100))
                        0))
        (cdr symb)))

(defun newpitchduration_oldpitchvelocity (symb secund branch generated)
  (cons (loop for channel1 in (car symb)
              for channel2 in (caar secund)
              for channel3 in (cadar secund)
              collect (or (append (and (listp channel1)
                                       (loop for pitch in channel1
                                             for dyn in channel3
                                             collect (list pitch dyn)))
                                  (and (listp channel2)
                                       channel2))
                          0))
        (cdr symb)))

(defun newpitch_durationoldpitchvelocity (symb secund branch generated)
  (list (loop for channel1 in symb
              for channel2 in (cadar secund)
              for channel3 in (caddar secund)
              collect (or (append (and (listp channel1)
                                       (loop for pitch in channel1
                                             for dyn in channel3
                                             collect (list pitch dyn)))
                                  (and (listp channel2)
                                       channel2))
                          0))
        (caar secund)))

(defun newpitch_durationoldpitch (symb secund branch generated)
  (list (loop for channel1 in symb
              for channel2 in (cadar secund)
              collect (or (append (and (listp channel1)
                                       (loop for pitch in channel1
                                             collect (list pitch 100)))
                                  (and (listp channel2)
                                       (loop for pitch in channel2
                                             collect (list pitch 100))))
                          0))
        (caar secund)))

(defun newpitchduration_oldpitch (symb secund branch generated)
  (cons (loop for channel1 in (car symb)
              for channel2 in (car secund)
              collect (or (append (and (listp channel1)
                                       (loop for pitch in channel1
                                             collect (list pitch 100)))
                                  (and (listp channel2)
                                       (loop for pitch in channel2
                                             collect (list pitch 100))))
                          0))
        (cdr symb)))

(defun pitch_duration (symb secund branch generated)
  (list (loop for channel1 in symb
              collect (if (listp channel1)
                        (loop for pitch in channel1
                              collect (list pitch 100))
                        0))
        (car secund)))

(defun pitch_durationvelocity (symb secund branch generated)
  (list (loop for channel1 in symb
              for channel2 in (cadar secund)
              collect (if (and (listp channel1) (listp channel2))
                        (loop for pitch in channel1
                              for dyn in channel2
                              collect (list pitch dyn))
                        0))
        (caar secund)))

(defun pitch_duration_last (symb secund branch generated)
  (if (or (null generated) (null (cadr secund)))
    (pitch_duration symb secund branch generated)
    (list (loop for channel1 in symb
                collect (if (listp channel1)
                          (loop for pitch in channel1
                                collect (list pitch 100))
                          0))
          (min (max (round (/ (* (car secund) (cadar (last generated))) (car (last (cadr secund))))) 0) 100000))))

(defun newpitch_durationoldpitch_last (symb secund branch generated)
  (if (or (null generated) (null (cadr secund)))
    (newpitch_durationoldpitch symb secund branch generated)
    (list (loop for channel1 in symb
                for channel2 in (cadar secund)
                collect (or (append (and (listp channel1)
                                         (loop for pitch in channel1
                                               collect (list pitch 100)))
                                    (and (listp channel2)
                                         (loop for pitch in channel2
                                               collect (list pitch 100))))
                            0))
          (min (max (round (/ (* (caar secund) (cadar (last generated))) (caar (last (cadr secund))))) 0) 100000))))

(defun pitch_durationvelocity_last (symb secund branch generated)
  (if (or (null generated) (null (cadr secund)))
    (pitch_durationvelocity symb secund branch generated)
    (list (loop for channel1 in symb
                for channel2 in (cadar secund)
                for channel3 in (caar (last generated))
                for channel4 in (cadar (last (cadr secund)))
                collect (if (listp channel1)
                          (if (and (listp channel3) (listp channel4))
                            (loop for pitch in channel1
                                  for dyn in channel2
                                  with scale = (round (/ (reduce #'+ channel3 :key #'cadr) (reduce #'+ channel4)))
                                  collect (list pitch (min (max (* dyn scale) 30) 125)))
                            (loop for pitch in channel1
                                  for dyn in channel2
                                  collect (list pitch dyn)))
                          0))
          (caar secund))))

(defun newpitch_durationoldpitchvelocity_last (symb secund branch generated)
  (if (or (null generated) (null (cadr secund)))
    (newpitch_durationoldpitchvelocity symb secund branch generated)
    (list (loop for channel1 in symb
                for channel2 in (caddar secund)
                for channel3 in (caar (last generated))
                for channel4 in (caddar (last (cadr secund)))
                for channel5 in (cadar secund)
                collect (or (append (and (listp channel1)
                                         (if (and (listp channel3) (listp channel4))
                                           (loop for pitch in channel1
                                                 for dyn in channel2
                                                 with scale = (round (/ (reduce #'+ channel3 :key #'cadr) (reduce #'+ channel4)))
                                                 collect (list pitch (min (max (* dyn scale) 30) 125)))
                                           (loop for pitch in channel1
                                                 for dyn in channel2
                                                 collect (list pitch dyn))))
                                    (and (listp channel5)
                                         channel5))
                            0))
          (caar secund))))

(defun newpitch_duration_last_oldpitchvelocity_last (symb secund branch generated)
  (if (or (null generated) (null (cadr secund)))
    (newpitch_durationoldpitchvelocity symb secund branch generated)
    (list (loop for channel1 in symb
                for channel2 in (caddar secund)
                for channel3 in (caar (last generated))
                for channel4 in (caddar (last (cadr secund)))
                for channel5 in (cadar secund)
                collect (or (append (and (listp channel1)
                                         (if (and (listp channel3) (listp channel4))
                                           (loop for pitch in channel1
                                                 for dyn in channel2
                                                 with scale = (round (/ (reduce #'+ channel3 :key #'cadr) (reduce #'+ channel4)))
                                                 collect (list pitch (min (max (* dyn scale) 30) 125)))
                                           (loop for pitch in channel1
                                                 for dyn in channel2
                                                 collect (list pitch dyn))))
                                    (and (listp channel5)
                                         channel5))
                            0))
          (min (max (round (/ (* (caar secund) (cadar (last generated))) (caar (last (cadr secund))))) 0) 100000))))

(defun pitch_duration_last_velocity (symb secund branch generated)
  (if (or (null generated) (null (cadr secund)))
    (pitch_durationvelocity symb secund branch generated)
    (list (loop for channel1 in symb
                collect (if (listp channel1)
                          (loop for pitch in channel1
                                collect (list pitch 100))
                          0))
          (min (max (round (/ (* (caar secund) (cadar (last generated))) (caar (last (cadr secund))))) 0) 100000))))

(defun lastpitch_duration (symb secund branch generated)
  (list (loop for channel1 in symb
              collect (if (listp channel1)
                        (loop for pitch in channel1
                              collect (list pitch 100))
                        0))
        (car secund)))

(defun degenerated (symb secund branch generated)
  symb)

;;; FUNCTIONS USED AS SECUNDARY INFORMATION CHOICE FOR LZGENERATE

(defun randomchoice (table generated)
  (choice (hashlist table)))

(defun duration_nearestlast (table generated)
  (if (null generated)
    (randomchoice table nil)
    (let ((entry nil)
          (min nil))
      (maphash #'(lambda (key val)
                   (if key
                     (if (or (null entry)
                             (< (abs (- (car (last key)) (cadr (car (last generated))))) min))
                       (setf entry (list key)
                             min (abs (- (car (last key)) (cadr (car (last generated))))))
                       (if (= (abs (- (car (last key)) (cadr (car (last generated))))) min)
                         (push key entry)))))
               table)
      (if entry
        (choice (loop for x in entry
                      append (mapcar #'(lambda (z)
                                         (list z x))
                                     (gethash x table))))
        (randomchoice table nil)))))

(defun duration_nearestlast_velocity (table generated)
  (if (null generated)
    (randomchoice table nil)
    (let ((entry nil)
          (min nil))
      (maphash #'(lambda (key val)
                   (if key
                     (if (or (null entry)
                             (< (abs (- (caar (last key)) (cadr (car (last generated))))) min))
                       (setf entry (list key)
                             min (abs (- (caar (last key)) (cadr (car (last generated))))))
                       (if (= (abs (- (caar (last key)) (cadr (car (last generated))))) min)
                         (push key entry)))))
               table)
      (if entry
        (choice (loop for x in entry
                      append (mapcar #'(lambda (z)
                                         (list z x))
                                     (gethash x table))))
        (randomchoice table nil)))))

(defun durationoldpitch_nearestlast (table generated)
  (if (null generated)
    (randomchoice table nil)
    (let ((entry nil)
          (min nil))
      (maphash #'(lambda (key val)
                   (if key
                     (if (or (null entry)
                             (< (abs (- (caar (last key)) (cadr (car (last generated))))) min))
                       (setq entry (list key)
                             min (abs (- (caar (last key)) (cadr (car (last generated))))))
                       (if (= (abs (- (caar (last key)) (cadr (car (last generated))))) min)
                         (push key entry)))))
               table)
      (if entry
        (choice (loop for x in entry
                      append (mapcar #'(lambda (z)
                                         (list z x))
                                     (gethash x table))))
        (randomchoice table nil)))))

;;; FUNCTIONS THAT COMPARES A SYMBOL AT THE ROOT OF A LZ TREE WITH THE LAST GENERATED ONE
;CHANGE BY AAA pitch1 ---> pitch11
(defun pitch11 (x y)
  (catch 'diff
    (loop with absx = nil
          with absy = nil
          for channel1 in x
          for channel2 in y
          when (null (and (listp channel1) (listp channel2)))
          do (if (or (listp channel1) (listp channel2))
               (throw 'diff nil))
          else
          when (null (= (length channel1) (length channel2)))
          do (throw 'diff nil)
          else
          do (loop for pitch1 in channel1
                   for pitch2 in channel2
                   when (null absx)
                   do (progn (setq absx pitch1)
                             (setq absy pitch2))
                   else
                   when (null (= (- pitch1 absx)
                                 (- pitch2 absy)))
                   do (throw 'diff nil))
          finally (return t))))

;;; FUNCTIONS THAT COMPARES A (REVERSED) BRANCH OF A LZ TREE WITH LAST GENERATED SYMBOLS

(defun equal2 (branch generated)
  (equal (car branch) (car generated)))

(defun pitch2 (branch generated)
  (let ((absx (loop for channel in (cadr branch)
                    when (listp channel)
                    return (car channel)
                    finally (return nil)))
        (absy (loop for channel in (cadr generated)
                    when (listp channel)
                    return (car channel)
                    finally (return nil))))
    (if (null (and absx absy))
      (pitch11 (car branch) (car generated))
      (catch 'diff
        (loop for channel1 in (car branch)
              for channel2 in (car generated)
              when (null (and (listp channel1) (listp channel2)))
              do (if (or (listp channel1) (listp channel2))
                   (throw 'diff nil))
              else
              when (null (= (length channel1) (length channel2)))
              do (throw 'diff nil)
              else
              do (loop for pitch1 in channel1
                       for pitch2 in channel2
                       when (null (= (- pitch1 absx)
                                     (- pitch2 absy)))
                       do (throw 'diff nil))
              finally (return t))))))

;;; SOME CONSTRAINTS

(defun pitch_grave (seuil)
  (lambda (past dynpast)
    (every #'(lambda (x)
               (or (atom x) (every #'(lambda (y) (< y seuil)) x)))
           (car past))))

(defun pitch_aigu (seuil)
  (lambda (past dynpast)
    (every #'(lambda (x)
               (or (atom x) (every #'(lambda (y) (> y seuil)) x)))
           (car past))))

(defun pitch_nochord ()
  (lambda (past dynpast)
    (every #'(lambda (x)
               (or (atom x) (null (cdr x))))
           (car past))))

(defun pitch_onlychord ()
  (lambda (past dynpast)
    (every #'(lambda (x)
               (or (atom x) (cdr x)))
           (car past))))

;;; LZ METHODS

(defmethod LZbirth ((tree LZpatterntree) (s t) (secundlist list) (main function) (secundary function))
  (setf (children tree)
        (cons (make-instance 'LZpatterntree
                :symb (funcall main s)
                :secund (reverse (cons (funcall secundary s)
                                       secundlist)))
              (children tree))))

(defmethod LZcontbirth ((tree LZcontnode) (branch list))
  (setf (children tree)
        (cons (make-instance 'LZcontnode :symb (car branch)) (children tree)))
  (labels ((recurs (tr br)
             (if (null (cdr br))
               (car (children tr))
               (progn 
                 (setf (children (car (children tr)))
                       (list (make-instance 'LZcontnode :symb (car br))))
                 (recurs
                  (car (children tr))
                  (cdr br))))))
    (recurs tree (cdr branch))))

(defmethod! LZlength ((tree LZtree))
   :initvals nil
   :indoc '("A LZ pattern tree or a LZ continuation tree")
   :icon '(230) 
   :numouts 1
   :doc  "Finds the lengths of the longest branch of the tree.

inputs:

tree: a LZ pattern tree or a LZ continuation tree

output:

an integer.
"
  (labels ((recurs (tr)
             (+ 1
                (if (children tr)
                  (loop for n in (children tr)
                        maximize (recurs n))
                  0))))
    (recurs tree)))

(defmethod! LZlength ((tree LZconttree))
   :initvals nil
   :indoc '("A LZ pattern tree or a LZ continuation tree")
   :icon '(230) 
   :numouts 1
   :doc  "Finds the lengths of the longest branch of the tree.

inputs:

tree: a LZ pattern tree or a LZ continuation tree

output:

an integer.
"
   (LZlength (dico tree)))

(defmethod! LZsize ((tree LZtree))
   :initvals nil
   :indoc '("A LZ pattern tree or a LZ continuation tree")
   :icon '(230) 
   :numouts 1
   :doc  "Finds the size (number of nodes) of the tree.

inputs:

tree: a LZ pattern tree or a LZ continuation tree

output:

an integer.
"
   (labels ((recurs (tr)
              (+ 1
                 (reduce #'+ 
                         (loop for n in (children tr)
                               collect (recurs n))))))
     (recurs tree)))

(defmethod! LZsize ((tree LZconttree))
   :initvals nil
   :indoc '("A LZ pattern tree or a LZ continuation tree")
   :icon '(230) 
   :numouts 1
   :doc  "Finds the size (number of nodes) of the tree.

inputs:

tree: a LZ pattern tree or a LZ continuation tree

output:

an integer.
"
   (LZsize (dico tree)))
   

(defmethod! LZsize ((nothing null))
  0)

(defmethod member-p ((tree LZpatterntree) (prefix list) (secundlist list) (main function) (secundary function))
  (and prefix
       (loop for n in (children tree)
             when (equal (symb n)
                         (funcall main (car prefix)))
             return (member-p n
                              (cdr prefix)
                              (cons (funcall secundary (car prefix))
                                    secundlist)
                              main
                              secundary)
             finally (return (cons tree (cons secundlist prefix))))))

(defmethod mk-lz-tree ((text list) (tree LZpatterntree) (main function) (secundary function))
  (loop while text
        do (let ((couple (member-p tree text nil main secundary)))
             (if (null couple)
               (return nil)
               (progn
                 (LZbirth (car couple) (caddr couple) (cadr couple) main secundary)
                 (setf text (cdddr couple)))))))

(defmethod alphabet ((text list) (main function) (secundary function))
  (let ((Sigma (make-hash-table :test #'equal))
        (Theta (make-hash-table :test #'equal))
        (Size (length text)))
    (loop for symb in text
          for i from 0
          do (progn
               (if (not (gethash (funcall main symb) Sigma)) (setf (gethash (funcall main symb) Sigma) (make-instance 'PSTnext)))
               (incf (proba (gethash (funcall main symb) Sigma)))
               (push (funcall secundary symb) (secund (gethash (funcall main symb) Sigma)))
               (if (< i (1- size)) (push i (gethash (funcall main symb) Theta)))))
    (maphash #'(lambda (symb nbocc) (setf (proba nbocc) (/ (proba nbocc) Size))) Sigma)
    (list Sigma Theta)))

(defun P (seq text main)
  ((lambda (n d) (if (and (> n 0) (> d 0)) (/ n d) 0))
   (reduce #'+
           (maplist #'(lambda (x)
                        (if (and
                             (<= (length seq) (length x))
                             (loop for s in seq
                                   for y in x
                                   always (equal s (funcall main y)))) 1 0))
                    text))
   (float (+ (- (length text) (length seq)) 1))))

(defmethod alphabet2 ((text list) (main function))
  (let (alpha)
    (loop for symb in text
          when (not (member (funcall main symb) alpha))
          do (push (funcall main symb) alpha))
    alpha))

(defmethod mk-PST ((text list) (main function) (secundary function) Pmin a ymin r L)
  (let* ((SigmaTheta (alphabet text main secundary))
         (Sigma (car SigmaTheta))
         (Theta (cadr SigmaTheta))
         (SigmaSize (hash-table-count Sigma))
         (Tree (make-instance 'PSTnode :next Sigma)))
    (labels ((recurs (tr seq oldtr)
               (let ((count 0)*
                     (pos (make-hash-table :test #'equal))
                     (size (length seq)))
                 (loop for i in (pos tr)
                       do (progn
                            (if (not (gethash (funcall main (nth (1+ i) text)) (next tr))) (setf (gethash (funcall main (nth (1+ i) text)) (next tr)) (make-instance 'PSTnext)))
                            (incf (proba (gethash (funcall main (nth (1+ i) text)) (next tr))))
                            (push (funcall secundary (nth (1+ i) text)) (secund (gethash (funcall main (nth (1+ i) text)) (next tr))))
                            (if (>= (- i size) 0) (push i (gethash (funcall main (nth (- i size) text)) pos)))
                            (incf count)))
                 ;(setf (nbnext tr) count)
                 (if (> count 0)
                   (loop for sig being each hash-key of (next tr) using (hash-value nbocc)
                         do (setf (proba nbocc) (/ (proba nbocc) count))))
                   ;(maphash #'(lambda (symb nbocc) (setf (proba nbocc) (/ (proba nbocc) count))) (next tr)))
                 (setf (mark tr)
                       (or
                        (and (< (length seq) L)
                             (loop for mark = nil
                                   for sig being each hash-key of Sigma
                                   when (and (>= (P (cons sig seq) text main) Pmin)
                                             (gethash sig pos)
                                             (recurs (car (push (make-instance 'PSTnode :symb sig :pos (gethash sig pos))
                                                                (children tr))) 
                                                     (cons sig seq)
                                                     (next tr)))
                                   do (setf mark t)
                                   finally (return mark)))
                        (loop for prob being each hash-value of (next tr) using (hash-key sig)
                              thereis (and (>= (proba prob) (* (+ 1 a) ymin))
                                           (or (>= (/ (proba prob) (proba (gethash sig oldtr))) r)
                                               (<= (/ (proba prob) (proba (gethash sig oldtr))) (/ r))))))))))
      (loop for sig being each hash-key of Sigma
            when (>= (proba (gethash sig Sigma)) Pmin)
            do (recurs (car (push (make-instance 'PSTnode :symb sig :pos (gethash sig Theta)) (children Tree))) (list sig) Sigma)))
    (labels ((recurs2 (tr)
               (setf (children tr)
                     (loop for child in (children tr)
                           when (mark child)
                           collect (progn
                                     (recurs2 child)
                                     child)))))
      (recurs2 Tree))
    (labels ((recurs3 (tr)
               (loop for sig being each hash-key of Sigma
                     do (if (gethash sig (next tr))
                          (setf (proba (gethash sig (next tr)))
                                (+ (* (- 1 (* SigmaSize ymin)) (proba (gethash sig (next tr)))) ymin))
                          (setf (gethash sig (next tr))
                                (make-instance 'PSTnext :proba ymin :secund nil))))
               (loop for child in (children tr)
                     do (recurs3 child))))
      (recurs3 Tree))
    Tree))

;(defmethod mk-PST2 ((text list) (main function) (secundary function))
;  (let ((Tree (make-instance 'LZconttree))
;        (Sigma (alphabet text main))
;        (S nil)
;        (pointree Tree))
;    (map-hash #'(lambda (symb P) (if (>= P Pmin) (push (list symb) S))) Sigma)
;    (print S)
;    (loop while S
;          do (let ((seq (pop S)))
;               (if (loop for sig in Sigma
;                         for quotient = (/ (P sig seq) (P sig (cdr seq)))
;                         thereis (and (>= (P sig seq) (* (+ 1 a) ymin))
;                                      (or (>= quotient r) (<= quotient (/ r)))))
;                 (grow-PST (dico Tree) seq))
;               (if (< (length seq) L)
;                 (loop for sig in Sigma
;                       when (>= (P (cons sig seq)) Pmin)
;                       do (push (cons sig seq) S))))))
;    (labels ((recurs (tr)
;               (setf (next tr) (for sig in Sigma
;                                    collect (make-instance 'LZnext
;                                              :symb (funcall main sig)
;                                              :proba (+ (* (- 1 (* Size ymin)) (P sig seq)) ymin)
;                                              :secund nil)))))
;      (recurs Tree))
;    Tree))

;(defmethod grow-PST ((tree LZcontnode) (branch list))
;  (if (null branch)
;    tree
;    (loop for n in (children tree)
;          when (equal (symb n) (car branch))
;          do (return (grow-PST n (cdr branch)))
;          finally (return (PSTbirth tree branch)))))

;(defmethod PSTbirth ((tree LZcontnode) (branch list))
;  (setf (children tree)
;        (cons (make-instance 'LZcontnode :symb (car branch)) (children tree)))
;  (labels ((recurs (tr br)
;             (if (null br)
;               (car (children tr))
;               (progn 
;                 (setf (children (car (children tr)))
;                       (list (make-instance 'LZcontnode :symb (car br))))
;                 (recurs
;                  (car (children tr))
;                  (cdr br))))))
;    (recurs tree (cdr branch))))

(defmethod grow-conttree ((tree LZcontnode) (branch list))
  (if (null (cdr branch))
    tree
    (loop for n in (children tree)
          when (equal (symb n) (car branch))
          do (return (grow-conttree n (cdr branch)))
          finally (return (LZcontbirth tree branch)))))

(defmethod mk-continuation-tree ((tree LZpatterntree))
  (let ((conttree (make-instance 'LZcontnode))
        (elmproba 1))
    (labels ((proba (tr branch)
               (if (null (children tr))
                 elmproba
                 (let* ((newbranch (cons (symb tr) branch))
                        (conttr (grow-conttree conttree newbranch))
                        (sump (apply #'+ 
                                     (mapcar #'(lambda (x)
                                                 (let ((p (proba x newbranch)))
                                                   (setf (next conttr)
                                                         (cons (make-instance 'LZnext
                                                                 :symb (symb x)
                                                                 :proba p
                                                                 :secund (let ((table (make-hash-table :test #'equal))
                                                                               (lvl (length (secund tr))))
                                                                           (labels ((dynamize (tre)
                                                                                      (let ((prefix (first-n (secund tre) lvl)))
                                                                                        (if (gethash prefix table)
                                                                                          (setf (gethash prefix table)
                                                                                                (cons (nth lvl (secund tre)) (gethash prefix table)))
                                                                                          (setf (gethash prefix table)
                                                                                                (list (nth lvl (secund tre))))))
                                                                                      (loop for n in (children tre)
                                                                                            do (dynamize n))))
                                                                             (dynamize x))
                                                                           table))
                                                               (next conttr)))
                                                   p))
                                             (children tr)))))
                   ;(loop for n in (next conttr)
                   ;      do (setf (cadr n) (/ (cadr n) sump)))
                   (+ elmproba sump)))))
      (proba tree nil))
    conttree))

(defmethod LZwrite ((tree LZpatterntree))
  (format t "~A <~A>" (symb tree) (secund tree)))

(defmethod LZwrite ((tree LZcontnode))
  (format t "~A" (symb tree))
  ;(if (subsize tree)
  ;  (format t " #subsize : ~A# " (subsize tree)))
  (if (next tree)
    (progn
      (format t "->" (symb tree))
      (LZwrite (next tree)))
    (format t ".")))

(defmethod LZwrite ((l list))
  (loop for n in l
        do (progn
             (format t "   ~A :" (symb n))
             ;(if (param n)
             ;  (LZwrite (param n)))
             (format t " ~A" (proba n))
             ;(if (correctedproba n)
             ;  (format t "cor : ~A" (correctedproba n)))
             (maphash #'(lambda (x y)
                          (format t " << ~A -> ~A >> " x y))
                      (secund n)))))

(defmethod LZwrite ((p LZparam))
  (format t "#~A ~A ~A ~A ~A#" (maxPast p) (minPast p) (minComplex p) (equiv1 p) (equiv2 p)))

(defmethod PSTwrite ((tree PSTnode))
  (format t "~A" (symb tree))
  (if (next tree)
    (progn
      (format t "->" (symb tree))
      (PSTwrite (next tree)))
    (format t ".")))

(defmethod PSTwrite ((h hash-table))
  (loop for symb being each hash-key of h using (hash-value proba)
        do (progn
             (format t "   ~A :" symb)
             (format t " ~A" (proba proba))
             ;(maphash #'(lambda (x y)
             ;             (format t " << ~A -> ~A >> " x y))
             ;         (secund n)))))
)))

(defmethod! LZify ((text list) (niter integer) &optional (type 'pitch_durationvelocity))
   :initvals '(nil 1 'pitch_durationvelocity)
   :indoc '("A list of anything" "An integer" "A list of 3 functions")
   :icon '(230) 
   :numouts 2
   :doc  "Builds a pattern dictionary containing a statistical model.

inputs:

text : list of anything
niter : integer >=1
type : a list containing :
             - a function selecting the analysis information inside a symbol.
             - a function selecting the synthesis information inside a symbol.
             - a function that reconstructs the symbol from analysis and synthesis information (will be used by LZGenerate).

output:

a LZ continuation tree
a LZ pattern tree

LZify takes a list of anything considered as an ordered sequence.
It then builds a pattern dictionary that encodes patterns of various lengths
discovered over this sequence, as well as the conditional probabilities
that a certain pattern be followed by certain elements
If niter is greater than 1, the analysis of the sequence is iterated <niter> times,
each time skipping the next element on the left of the sequence.
niter > 1 increases the number of patterns discovered. It is equivalent to analyzing
a longer sequence, thus increasing the statistical properties (redundancy).
Empirical experience shows that niter = 4 is good value for such data as bach-like counterpoint
or jazz chorus.
Then LZify builds the continuation tree. This tree is another representation of the pattern
dictionary, suited to generation features : the branch are reversed, in order to ease the search
for the maximum context, and the continuations of each context are explicitly represented, linked
with their corresponding probabilities.

"
   :menuins '((2 (("pitch_duration" 'pitch_duration)
                 ("pitch_durationvelocity" 'pitch_durationvelocity)
                 ("pitchduration_nothing" 'pitchduration_nothing)
                 ("pitchduration_velocity" 'pitchduration_velocity)
                 ("newpitch_durationoldpitch" 'newpitch_durationoldpitch)
                 ("newpitch_durationoldpitchvelocity" 'newpitch_durationoldpitchvelocity)
                 ("newpitchduration_oldpitch" 'newpitchduration_oldpitch)
                 ("newpitchduration_oldpitchvelocity" 'newpitchduration_oldpitchvelocity)
                 ("pitch_duration_last" 'pitch_duration_last)
                 ("pitch_durationvelocity_last" 'pitch_durationvelocity_last)
                 ("pitch_duration_last_velocity" 'pitch_duration_last_velocity)
                 ("newpitch_durationoldpitch_last" 'newpitch_durationoldpitch_last)
                 ("newpitch_durationoldpitchvelocity_last" 'newpitch_durationoldpitchvelocity_last)
                 ("newpitch_duration_last_oldpitchvelocity_last" 'newpitch_duration_last_oldpitchvelocity_last)
                 ("all_nothing" 'degenerated))))
   (labels ((ftype (x)
              (eql type x)))
     (let ((tree (make-instance 'LZpatterntree)))
       (loop for i from 1 to niter 
             for text2 on text
             do (mk-lz-tree text2
                            tree
                            (if (listp type)
                              (car type)
                              (if (some #'ftype '(pitch_duration pitch_durationvelocity pitch_duration_last pitch_durationvelocity_last pitch_duration_last_velocity))
                                #'pitch
                                (if (some #'ftype '(pitchduration_nothing pitchduration_velocity))
                                  #'pitchduration
                                  (if (some #'ftype '(newpitch_durationoldpitch newpitch_durationoldpitchvelocity newpitch_durationoldpitch_last
                                                      newpitch_durationoldpitchvelocity_last newpitch_duration_last_oldpitchvelocity_last))
                                    #'newpitch
                                    (if (some #'ftype '(newpitchduration_oldpitch newpitchduration_oldpitchvelocity))
                                      #'newpitchduration
                                      (if (some #'ftype '(degenerated))
                                        #'all))))))
                            (if (listp type)
                              (cadr type)
                              (if (some #'ftype '(pitch_duration pitch_duration_last))
                                #'duration
                                (if (some #'ftype '(pitch_durationvelocity pitch_durationvelocity_last pitch_duration_last_velocity))
                                  #'durationvelocity
                                  (if (some #'ftype '(pitchduration_nothing))
                                    #'nothing
                                    (if (some #'ftype '(pitchduration_velocity))
                                      #'velocity
                                      (if (some #'ftype '(newpitch_durationoldpitch newpitch_durationoldpitch_last))
                                        #'durationoldpitch
                                        (if (some #'ftype '(newpitch_durationoldpitchvelocity newpitch_durationoldpitchvelocity_last
                                                            newpitch_duration_last_oldpitchvelocity_last))
                                          #'durationoldpitchvelocity
                                          (if (some #'ftype '(newpitchduration_oldpitch))
                                            #'oldpitch
                                            (if (some #'ftype '(newpitchduration_oldpitchvelocity))
                                              #'oldpitchvelocity
                                              (if (some #'ftype '(degenerated))
                                                #'nothing))))))))))))
       (values
        (make-instance 'LZconttree
          :dico (mk-continuation-tree tree)
          :reconstr (if (listp type)
                      (caddr type)
                      (symbol-function type)))
        tree))))


(defmethod! PSTify ((text list) (Pmin float) (a float) (ymin float) (r float) (L integer) &optional (type 'pitch_durationvelocity))
   :initvals '(nil 0.1 0.1 0.01 2 10 'pitch_durationvelocity)
   :indoc '("A list of anything" "A float" "A float" "A float" "A float" "An integer" "A list of 3 functions")
   :icon '(230) 
   :numouts 1
   :doc  "Builds a pattern suffix tree modeling the text.

inputs:

text : list of anything
Pmin
a
ymin
r
L
type : a list containing :
             - a function selecting the analysis information inside a symbol.
             - a function selecting the synthesis information inside a symbol.
             - a function that reconstructs the symbol from analysis and synthesis information (will be used by LZGenerate).

output:

a PST (Prediction Suffix Tree)

"
   :menuins '((6 (("pitch_duration" 'pitch_duration)
                 ("pitch_durationvelocity" 'pitch_durationvelocity)
                 ("pitchduration_nothing" 'pitchduration_nothing)
                 ("pitchduration_velocity" 'pitchduration_velocity)
                 ("newpitch_durationoldpitch" 'newpitch_durationoldpitch)
                 ("newpitch_durationoldpitchvelocity" 'newpitch_durationoldpitchvelocity)
                 ("newpitchduration_oldpitch" 'newpitchduration_oldpitch)
                 ("newpitchduration_oldpitchvelocity" 'newpitchduration_oldpitchvelocity)
                 ("pitch_duration_last" 'pitch_duration_last)
                 ("pitch_durationvelocity_last" 'pitch_durationvelocity_last)
                 ("pitch_duration_last_velocity" 'pitch_duration_last_velocity)
                 ("newpitch_durationoldpitch_last" 'newpitch_durationoldpitch_last)
                 ("newpitch_durationoldpitchvelocity_last" 'newpitch_durationoldpitchvelocity_last)
                 ("newpitch_duration_last_oldpitchvelocity_last" 'newpitch_duration_last_oldpitchvelocity_last)
                 ("all_nothing" 'degenerated))))
   (labels ((ftype (x)
              (eql type x)))
     (make-instance 'PST
       :dico (mk-PST text
                     (if (listp type)
                       (car type)
                       (if (some #'ftype '(pitch_duration pitch_durationvelocity pitch_duration_last pitch_durationvelocity_last pitch_duration_last_velocity))
                         #'pitch
                         (if (some #'ftype '(pitchduration_nothing pitchduration_velocity))
                           #'pitchduration
                           (if (some #'ftype '(newpitch_durationoldpitch newpitch_durationoldpitchvelocity newpitch_durationoldpitch_last
                                               newpitch_durationoldpitchvelocity_last newpitch_duration_last_oldpitchvelocity_last))
                             #'newpitch
                             (if (some #'ftype '(newpitchduration_oldpitch newpitchduration_oldpitchvelocity))
                               #'newpitchduration
                               (if (some #'ftype '(degenerated))
                                 #'all))))))
                     (if (listp type)
                       (cadr type)
                       (if (some #'ftype '(pitch_duration pitch_duration_last))
                         #'duration
                         (if (some #'ftype '(pitch_durationvelocity pitch_durationvelocity_last pitch_duration_last_velocity))
                           #'durationvelocity
                           (if (some #'ftype '(pitchduration_nothing))
                             #'nothing
                             (if (some #'ftype '(pitchduration_velocity))
                               #'velocity
                               (if (some #'ftype '(newpitch_durationoldpitch newpitch_durationoldpitch_last))
                                 #'durationoldpitch
                                 (if (some #'ftype '(newpitch_durationoldpitchvelocity newpitch_durationoldpitchvelocity_last
                                                     newpitch_duration_last_oldpitchvelocity_last))
                                   #'durationoldpitchvelocity
                                   (if (some #'ftype '(newpitchduration_oldpitch))
                                     #'oldpitch
                                     (if (some #'ftype '(newpitchduration_oldpitchvelocity))
                                       #'oldpitchvelocity
                                       (if (some #'ftype '(degenerated))
                                         #'nothing))))))))))
                     Pmin a ymin r L)
       :reconstr (if (listp type)
                   (caddr type)
                   (symbol-function type)))))

(defmethod! LZprint ((tree LZtree))
   :initvals '(nil)
   :indoc '("A LZ pattern tree or a LZ continuation tree")
   :icon '(230) 
   :doc  "Print a given pattern dictionnary.

inputs:

tree: a LZ pattern tree or a LZ continuation tree generated by the LZify function.

output:

nil

"
   (labels ((recurs (tr n)
              (terpri)
              (loop for i=0 to n
                    do (format *standard-output* "~5,5T"))
              (LZwrite tr)
              (mapcar #'(lambda (x) (recurs x (1+ n)))
                      (children tr))
              nil))
     (recurs tree 0)))

(defmethod! LZprint ((tree LZconttree))
   :initvals '(nil)
   :indoc '("A LZ pattern tree or a LZ continuation tree")
   :icon '(230) 
   :doc  "Print a given pattern dictionnary.

inputs:

tree: a LZ pattern tree or a LZ continuation tree generated by the LZify function.

output:

nil

"
   (LZprint (dico tree))
   (terpri)
   (pprint (reconstr tree)))

(defmethod! PSTprint ((tree PSTnode))
   :initvals '(nil)
   :indoc '("A PST")
   :icon '(230) 
   :doc  "Print a given PST.

inputs:

tree: a PST generated by the PSTify function.

output:

nil

"
   (labels ((recurs (tr n)
              (terpri)
              (loop for i=0 to n
                    do (format *standard-output* "~5,5T"))
              (PSTwrite tr)
              (mapcar #'(lambda (x) (recurs x (1+ n)))
                      (children tr))
              nil))
     (recurs tree 0)))

(defmethod! PSTprint ((tree PST))
   :initvals '(nil)
   :indoc '("A PST")
   :icon '(230) 
   :doc  "Print a given pattern dictionnary.

inputs:

tree: a LZ pattern tree or a LZ continuation tree generated by the LZify function.

output:

nil

"
   (PSTprint (dico tree))
   (terpri)
   (pprint (reconstr tree)))

(defmethod! LZprintreconstr ((tree LZconttree))
   :initvals '(nil)
   :indoc '("A LZ continuation tree")
   :icon '(230) 
   :doc  "Indicates the reconstr function used by the continuation tree.

inputs:

tree: a LZ continuation tree generated by the LZify function.

output:

nil

"
   (pprint (reconstr tree)))

(defmethod! LZprint ((nothing null))
   (write nil))

(defmethod! PSTprint ((nothing null))
   (write nil))

(defmethod! LZuntree ((tree LZtree) &optional (delay 100))
   :initvals '(nil 100)
   :indoc '("A LZ pattern tree or a LZ continuation tree" "An integer")
   :icon '(230)
   :doc  "Appends all the patters of a given pattern dictionnary, separating one from each other by a delay.

inputs:

tree: a LZ pattern tree or a LZ continuation tree generated by the LZify function.
delay: delay time between branches.

output:

A sequence of patterns.

"
   (let ((nbchan (length (car (symb (car (children tree)))))))
     (labels ((recurs (tr br)
                (if (children tr)
                  (loop for child in (reverse (children tr))
                        append (recurs child
                                       (if (atom (symb tr))
                                         br
                                         (cons (symb tr) br))))
                  (cons (list (loop for i from 1 to nbchan
                                    collect 0)
                              delay)
                        (reverse (if (atom (symb tr))
                                   br
                                   (cons (symb tr) br)))))))
       (recurs tree nil))))

(defmethod! LZuntree ((tree LZconttree) &optional (delay 100))
   :initvals '(nil 100)
   :indoc '("A LZ pattern tree or a LZ continuation tree" "An integer")
   :icon '(230) 
   :doc  "Appends all the patters of a given pattern dictionnary, separating one from each other by a delay.

inputs:

tree: a LZ pattern tree or a LZ continuation tree generated by the LZify function.
delay: delay time between branches.

output:

A sequence of patterns.

"
   (let ((nbchan (length (car (symb (car (children (dico tree))))))))
     (labels ((recurs (tr br)
                (if (children tr)
                  (loop for child in (children tr)
                        append (recurs child
                                       (if (atom (symb tr))
                                         br
                                         (cons (symb tr) br))))
                  (cons (list (loop for i from 1 to nbchan
                                    collect 0)
                              delay)
                        (if (atom (symb tr))
                          br
                          (cons (symb tr) br))))))
       (recurs (dico tree) nil))))

(defmethod choice ((l list))
  (nth (om-random-value (length l)) l))

(defmethod! PSTGenerate ((dict PST) (Length integer) 
                        &optional (minPast 0)
                        (incipit1 nil) (incipit2 nil) (reconstr nil) (strategy 'randomchoice)
                        (constraints nil) (equiv1 'equal) (equiv2 'equal2))
   :initvals '(nil 50 0 nil nil nil 'randomchoice  nil 'equal 'equal2)
   :indoc '("A PST" "An integer" "An integer (optional)" "A list (optional)" "A list (optional)" "nil or a function name (optional)"
            "A function name (optional)" "A function name (optional)" "A function name (optional)" "A function name (optional)")
   :icon '(230)
   :numouts 1
   :doc  
   "Generates a new sequence following the model of a given PST (generated by the PSTify function).

inputs :

dict: a PST generated by the PSTify function.
Length: integer, >=1.  length of the sequence to be generated.
minPast: integer, >=0.  minimum length of the context.
incipit1: list. a sequence of analysis symboles that will be the beginning of the generated analysis sequence.
incipit2: list. a sequence of synthesis symboles that will be the beginning of the generated synthesis sequence.
reconstr: the name of a function that reconstructs the symbol from analysis and synthesis information. If nil, the predefined (in LZify) fonction will be used.
strategy: the name of function that chooses new synthesis information according to its last evolution.
constraints: constraint function of the last generated analysis information and last generated sequence, both reversed.
equiv1: the name of function that compares a symbole at the root of the tree with the last generated one.
equiv2: the name of function that compares a symbole with any in the tree.

output :

a list of events in the same alphabet as the analyzed sequence.

After building a pattern dictionary using PSTify, PSTGenerate may be used to
generate a new sequence that imitates the statistical behaviour encoded into the dictionary.
If a list of <something> had been analyzed by PSTify, the result will be a new list of <something>.
At a every point of the generation, PSTGenerate looks at the longest sequence of last generated elements
that belongs to the PST.
It then checks the conditional probabilities associated with
that pattern (or context) , then generates a new element with regard to the probability. It then adds
this element to the right of the generated sequence, and iterates.

If, at a certain point of the generation, the length of the context is lower than minPast, then PSTGenerate
goes back one step before and generates another symbols until it respects the minPast constraint. It may
go back as far as necessa
ry. If no sequence can respect the constraint, PSTGenerate returns nil.
Thanks to the minPast parameter, you can prevent PSTGenerate from generating with no or little context.
"
   :menuins '((8 (("default function" nil)
                 ("pitch_duration" 'pitch_duration)
                 ("pitch_durationvelocity" 'pitch_durationvelocity)
                 ("pitchduration_nothing" 'pitchduration_nothing)
                 ("pitchduration_velocity" 'pitchduration_velocity)
                 ("newpitch_durationoldpitch" 'newpitch_durationoldpitch)
                 ("newpitch_durationoldpitchvelocity" 'newpitch_durationoldpitchvelocity)
                 ("newpitchduration_oldpitch" 'newpitchduration_oldpitch)
                 ("newpitchduration_oldpitchvelocity" 'newpitchduration_oldpitchvelocity)
                 ("pitch_duration_last" 'pitch_duration_last)
                 ("pitch_durationvelocity_last" 'pitch_durationvelocity_last)
                 ("pitch_duration_last_velocity" 'pitch_duration_last_velocity)
                 ("newpitch_durationoldpitch_last" 'newpitch_durationoldpitch_last) 
                 ("newpitch_durationoldpitchvelocity_last" 'newpitch_durationoldpitchvelocity_last)
                 ("newpitch_duration_last_oldpitchvelocity_last" 'newpitch_duration_last_oldpitchvelocity_last)))
             (9 (("random choice" 'randomchoice)
                 ("duration : nearest last durations" 'duration_nearestlast)
                 ("duration,etc. : nearest last durations" 'duration_nearestlast_velocity))))
   
(if (or (> minPast 0) constraints)
     (PSTGeneratecont (dico dict) Length minPast incipit1 incipit2
                     (if reconstr (symbol-function reconstr) (reconstr dict))
                     (symbol-function strategy) constraints (symbol-function equiv1) (symbol-function equiv2) t)
     (PSTGenerate1 (dico dict) Length incipit1 incipit2 (if reconstr (symbol-function reconstr) (reconstr dict))
                  (symbol-function strategy) (symbol-function equiv1) (symbol-function equiv2) t)))

(defun PSTGenerate1 (dict Length incipit1 incipit2 reconstr strategy equiv1 equiv2 mostprobable)
  (let* ((result (reverse incipit1))
         (dynresult (reverse incipit2)))
    (loop for count from 1 to Length
          do (progn
               (let* ((node (if (null result)
                              dict
                              (labels ((recurs (context tr generated)
                                         (loop for n in (children tr)
                                               when (funcall equiv2 (reverse (posintree n dict)) context)
                                               return (or (recurs (cdr context) n (cons (symb n) generated))
                                                          tr)
                                               finally (return tr))))
                                (loop for n in (children dict)
                                      when (funcall equiv1 (symb n) (car result))
                                      return (or (recurs (cdr result) n (list (symb n)))
                                                 dict)
                                      finally (return dict)))))
                      (next (PSGenerate node)))
                 (push (car next) result)
                 (push (funcall reconstr
                                (car next)
                                (if (cadr next) (or (nth (om-random-value (length (cdr next))) (cdr next)) (let ((next2 (cdr (PSGenerate dict)))) (nth (om-random-value (length next2)) next2)))
                                    (let ((next2 (cdr (PSGenerate dict)))) (nth (om-random-value (length next2)) next2)))
                                (reverse (posintree node dict))
                                (reverse (first-n dynresult (LZlength dict))))
                       dynresult))))
    (reverse dynresult)))

(defun PSTGeneratecont (tree Length minPast incipit1 incipit2 reconstr strategy constraints equiv1 equiv2 mostprobable)
  (let ((size (LZlength tree)))
    (labels ((try (i past dynpast cont)
               (if (= i Length)
                 (funcall cont (list nil))
                 (let ((x (mkcontext past tree minPast equiv1 equiv2)))
                   (if (or (and (> (caddr x) (- 1))
                                (< (caddr x) (min minPast (length past))))
                           (and constraints (null (funcall constraints past dynpast))))
                     (funcall cont nil)
                     (newtry i
                             past
                             dynpast
                             (car x)
                             (if (> minComplex 0)
                               (weight (cadr x)
                                       (computesubsize tree (car x) maxPast minPast minComplex equiv1 equiv2)
                                       minComplex)
                               (cadr x))
                             cont)))))
             (newtry (i past dynpast node alterns cont)
               (let ((memo)
                     (y (if (> minComplex 0)
                          (Generatecomplex alterns mostprobable)
                          (Generatecont alterns mostprobable))))
                 (if y
                   (progn
                     (if (= (mod i 100) 1)
                       (format t "i=~A : ~A" i (car y)))
                     (try (1+ i)
                          (cons (symb (car y))
                                (if (< (length past) size)
                                  past
                                  (butlast past)))
                          (cons (setq memo (funcall reconstr
                                                    (symb (car y))
                                                    (funcall strategy
                                                             (secund (car y))
                                                             (reverse dynpast))
                                                    (reverse (posintree node tree))
                                                    (reverse dynpast)))
                                (if (< (length dynpast) size)
                                  dynpast
                                  (butlast dynpast)))
                          #'(lambda (z)
                              (if z
                                (funcall cont (list (cons memo
                                                          (car z))))
                                (newtry i
                                        past
                                        dynpast
                                        node
                                        (cadr y)
                                        cont)))))
                   (funcall cont nil)))))
      (try 0
           (reverse incipit1)
           (reverse incipit2)
           #'(lambda (x) (car x))))))

(defun PSGenerate (tree)
  (loop with seuil = (om-random-value 1.0)
        for n being each hash-key of (next tree) using (hash-value next)
        for cumul = (proba next) then (+ cumul (proba next))
        when (>= cumul seuil)
        return (list n (secund next))))

(defmethod! LZGenerate ((dict LZconttree) (maxPast t) (Length integer) 
                        &optional (mostprobable t) (minPast 0) (minComplex 0)
                        (incipit1 nil) (incipit2 nil) (reconstr nil) (strategy 'randomchoice)
                        (constraints nil) (equiv1 'equal) (equiv2 'equal2))
   :initvals '(nil nil 50 t 0 0 nil nil nil 'randomchoice  nil 'equal 'equal2)
   :indoc '("A LZ continuation tree" "An integer or nil"
            "An integer" "t or nil (optional)" "An integer (optional)" "An integer (optional)" "A list (optional)" "A list (optional)" "nil or a function name (optional)"
            "A function name (optional)" "A function name (optional)" "A function name (optional)" "A function name (optional)")
   :icon '(230)
   :numouts 1
   :doc  
   "Generates a new sequence following the model of a given LZ continuation tree (generated by the LZify function).

inputs :

dict: a LZ continuation tree generated by the LZify function.
maxPast: nil or integer.  maximum length of the context. If nil : no maximum past constraint.
Length: integer, >=1.  length of the sequence to be generated.
mostprobable: if not true, inverse all the probability distributions (the less probable one becomes the most probable one).
minPast: integer, >=0.  minimum length of the context.
minComplex: integer, >=0.  minimum size of the subLZtree of each context.
incipit1: list. a sequence of analysis symboles that will be the beginning of the generated analysis sequence.
incipit2: list. a sequence of synthesis symboles that will be the beginning of the generated synthesis sequence.
reconstr: the name of a function that reconstructs the symbol from analysis and synthesis information. If nil, the predefined (in LZify) fonction will be used.
strategy: the name of function that chooses new synthesis information according to its last evolution.
constraints: constraint function of the last generated analysis information and last generated sequence, both reversed.
equiv1: the name of function that compares a symbole at the root of the tree with the last generated one.
equiv2: the name of function that compares a symbole with any in the tree.

output :

a list of events in the same alphabet as the analyzed sequence.

After building a pattern dictionary using LZify, LZGenerate may be used to
generate a new sequence that imitates the statistical behaviour encoded into the dictionary.
If a list of <something> had been analyzed by LZify, the result will be a new list of <something>.
At a every point of the generation, LZGenerate looks at the longest sequence of last generated elements
that belongs to the LZ tree (even those that do not start exactly from the root).
It then checks the conditional probabilities associated with
that pattern (or context) , then generates a new element with regard to the probability. It then adds
this element to the right of the generated sequence, and iterates.

If maxPast in an integer, the length of the context must not exceed maxPast. That is to say, this
limits the size of the memory of what LZGenerate has previously generated.

If, at a certain point of the generation, the length of the context is lower than minPast, then LZGenerate
goes back one step before and generates another symbols until it respects the minPast constraint. It may
go back as far as necessa
ry. If no sequence can respect the constraint, LZGenerate returns nil.
Thanks to the minPast parameter, you can prevent LZGenerate from generating with no or little context.

If, at a certain point of the generation, the number of all the nodes of the tree that can be reached from now on
(which is called the subtree generated by the present context) is lower than minComplex, then LZGenerate 
takes into account not only the continuation of the longest context, but also those of more little context,
this in order to get out of this subtree. Thanks to the minContext parameter, you can prevent LZGenerate
from getting stuck inside a little subtree.
"
   :menuins '((8 (("default function" nil)
                 ("pitch_duration" 'pitch_duration)
                 ("pitch_durationvelocity" 'pitch_durationvelocity)
                 ("pitchduration_nothing" 'pitchduration_nothing)
                 ("pitchduration_velocity" 'pitchduration_velocity)
                 ("newpitch_durationoldpitch" 'newpitch_durationoldpitch)
                 ("newpitch_durationoldpitchvelocity" 'newpitch_durationoldpitchvelocity)
                 ("newpitchduration_oldpitch" 'newpitchduration_oldpitch)
                 ("newpitchduration_oldpitchvelocity" 'newpitchduration_oldpitchvelocity)
                 ("pitch_duration_last" 'pitch_duration_last)
                 ("pitch_durationvelocity_last" 'pitch_durationvelocity_last)
                 ("pitch_duration_last_velocity" 'pitch_duration_last_velocity)
                 ("newpitch_durationoldpitch_last" 'newpitch_durationoldpitch_last) 
                 ("newpitch_durationoldpitchvelocity_last" 'newpitch_durationoldpitchvelocity_last)
                 ("newpitch_duration_last_oldpitchvelocity_last" 'newpitch_duration_last_oldpitchvelocity_last)))
             (9 (("random choice" 'randomchoice)
                 ("duration : nearest last durations" 'duration_nearestlast)
                 ("duration,etc. : nearest last durations" 'duration_nearestlast_velocity))))
   
(if (or (> minPast 0) (> minComplex 0) constraints)
     (LZGeneratecont (dico dict) Length maxPast minPast minComplex incipit1 incipit2
                     (if reconstr (symbol-function reconstr) (reconstr dict))
                     (symbol-function strategy) constraints (symbol-function equiv1) (symbol-function equiv2) mostprobable)
     (LZGenerate1 (dico dict) Length maxPast incipit1 incipit2 (if reconstr (symbol-function reconstr) (reconstr dict))
                  (symbol-function strategy) (symbol-function equiv1) (symbol-function equiv2) mostprobable)))

(defun LZGenerate1 (dict Length maxPast incipit1 incipit2 reconstr strategy equiv1 equiv2 mostprobable)
  (let* ((result (reverse incipit1))
         (dynresult (reverse incipit2)))
    (loop with context = nil
          with prevnext = nil
          for count from 1 to Length
          do (progn
               (let* ((node (or context
                                (if (null result)
                                  dict
                                  (labels ((recurs (context tr generated)
                                             (if (or (null context) (and maxPast (>= (length generated) maxPast)))
                                               tr
                                               (loop for n in (children tr)
                                                     when (funcall equiv2 (reverse (posintree n dict)) context)    
                                                     return (or (recurs (cdr context) n (cons (symb n) generated))
                                                                tr)
                                                     finally (return tr)))))
                                    (loop for n in (children dict)
                                          when (funcall equiv1 (symb n) (car result))
                                          return (or (recurs (cdr result) n (list (symb n)))
                                                     dict)
                                          finally (return dict))))))
                      (next (Generate node mostprobable)))
                 (if (and (null context) prevnext)
                   (progn
                     (if (null (param prevnext))
                       (setf (param prevnext)
                             (make-instance 'LZparam)))
                     (setf (maxPast (param prevnext)) maxPast)
                     (setf (minPast (param prevnext)) nil)
                     (setf (minComplex (param prevnext)) nil)
                     (setf (equiv1 (param prevnext)) equiv1)
                     (setf (equiv2 (param prevnext)) equiv2)
                     (setf (context prevnext) node)))
                 (push (symb next) result)
                 (push (funcall reconstr
                                (symb next)
                                (funcall strategy 
                                         (secund next)
                                         (reverse (first-n dynresult (LZlength dict))))
                                (reverse (posintree node dict))
                                (reverse (first-n dynresult (LZlength dict))))
                       dynresult)
                 (setq context (and (param next)
                                    (eq (maxPast (param next)) maxPast)
                                    (null (minPast (param next)))
                                    (null (minComplex (param next)))
                                    (eq (equiv1 (param next)) equiv1)
                                    (eq (equiv2 (param next)) equiv2)
                                    (context next)))
                 (setq prevnext next))))
    (reverse dynresult)))

(defun mkcontext (past tr minPast maxPast equiv1 equiv2)
  (if (null past)
    (list tr
          (list (copy-list (allnext tr)))
          (- 1))
    (labels ((recurs (context tre generated altern)
               (if (null context)
                 (list tre
                       (if (>= (length generated) (min minPast (length past)))
                         (cons (copy-list (allnext tre)) altern)
                         altern)
                       (- 1))
                 (if (and maxPast (>= (length generated) maxPast))
                   (list tre
                         (if (>= (length generated) (min minPast (length past)))
                           (cons (copy-list (allnext tre)) altern)
                           altern)
                         (length generated))
                   (loop for n in (children tre)
                         when (funcall equiv2 (reverse (posintree n tr)) context)
                         return (or (recurs (cdr context)
                                            n
                                            (cons (symb n) generated)
                                            (if (>= (length generated) (min minPast (length past)))
                                              (cons (copy-list (allnext tre)) altern)
                                              altern))
                                    (list tre
                                          (if (>= (length generated) (min minPast (length past)))
                                            (cons (copy-list (allnext tre)) altern)
                                            altern)
                                          (length generated)))
                         finally (return (list tre
                                              (if (>= (length generated) (min minPast (length past)))
                                                (cons (copy-list (allnext tre)) altern)
                                                altern)
                                              (length generated))))))))
      (loop for n in (children tr)
            when (funcall equiv1 (symb n) (car past))
            return (or (recurs (cdr past)
                               n 
                               (list (symb n))
                               (and (= 0 (min minPast (length past)))
                                    (list (copy-list (allnext tr)))))
                       (list tr
                             (and (= 0 (min minPast (length past)))
                                  (list (copy-list (allnext tr))))
                             0))
            finally (return (list tr
                                 (and (= 0 (min minPast (length past)))
                                      (list (copy-list (allnext tr))))
                                 0))))))

(defun LZGeneratecont (tree Length maxPast minPast minComplex incipit1 incipit2 reconstr strategy constraints equiv1 equiv2 mostprobable)
   (let ((size (LZlength tree)))
     (labels ((try (i past dynpast cont context prevnext)
                (if (= i Length)
                  (funcall cont (list nil))
                  (let ((x (or context
                               (mkcontext past tree minPast maxPast equiv1 equiv2))))
                    (if (and (null context) prevnext)
                      (progn
                        (if (null (param prevnext))
                          (setf (param prevnext)
                                (make-instance 'LZparam)))
                        (setf (maxPast (param prevnext)) maxPast)
                        (setf (minPast (param prevnext)) minPast)
                        (setf (minComplex (param prevnext)) minComplex)
                        (setf (equiv1 (param prevnext)) equiv1)
                        (setf (equiv2 (param prevnext)) equiv2)
                        (setf (context prevnext) x)))
                    (if (or (and (> (caddr x) (- 1))
                                 (< (caddr x) (min minPast (length past))))
                            (and constraints (null (funcall constraints past dynpast))))
                      (funcall cont nil)
                      (newtry i
                              past
                              dynpast
                              (car x)
                              (if (> minComplex 0)
                                (weight (cadr x)
                                        (computesubsize tree (car x) maxPast minPast minComplex equiv1 equiv2)
                                        minComplex)
                                (cadr x))
                              cont)))))
              (newtry (i past dynpast node alterns cont)
                (let ((memo)
                      (y (if (> minComplex 0)
                           (Generatecomplex alterns mostprobable)
                           (Generatecont alterns mostprobable))))
                  (if y
                    (progn
                      (if (= (mod i 100) 1)
                        (format t "i=~A : ~A" i (car y)))
                      (try (1+ i)
                           (cons (symb (car y))
                                 (if (< (length past) size)
                                   past
                                   (butlast past)))
                           (cons (setq memo (funcall reconstr
                                                     (symb (car y))
                                                     (funcall strategy
                                                              (secund (car y))
                                                              (reverse dynpast))
                                                     (reverse (posintree node tree))
                                                     (reverse dynpast)))
                                 (if (< (length dynpast) size)
                                   dynpast
                                   (butlast dynpast)))
                           #'(lambda (z)
                               (if z
                                 (funcall cont (list (cons memo
                                                           (car z))))
                                 (newtry i
                                         past
                                         dynpast
                                         node
                                         (cadr y)
                                         cont)))
                           (and (param (car y))
                                (eq (maxPast (param (car y))) maxPast)
                                (eq (minPast (param (car y))) minPast)
                                (eq (minComplex (param (car y))) minComplex)
                                (eq (equiv1 (param (car y))) equiv1)
                                (eq (equiv2 (param (car y))) equiv2)
                                (context (car y)))
                           (car y)))
                    (funcall cont nil)))))
       (try 0
            (reverse incipit1)
            (reverse incipit2)
            #'(lambda (x) (car x))
            nil
            nil))))

(defun allnext (tr)
  (append (next tr)
          (mapcan #'allnext (children tr))))

(defun Generate (tree mostprobable)
  (if mostprobable
    (let ((l (allnext tree)))
      (loop with seuil = (om-random-value (float (reduce #'+ l :key #'proba)))
            for n in l
            for cumul = (proba n) then (+ cumul (proba n))
            when (>= cumul seuil)
            return n))
    (let ((l (allnext tree)))
      (loop with seuil = (om-random-value (float (reduce #'+ l :key #'(lambda (x) (/ (proba x))))))
            for n in l
            for cumul = (/ (proba n)) then (+ cumul (/ (proba n)))
            when (>= cumul seuil)
            return n))))

(defun mapremove (n altern)
  (if (atom altern)
    nil
    (cons (remove n
                  (car altern)
                  :test #'(lambda (x y)
                            (equal (symb x) (symb y))))
          (mapremove n (cdr altern)))))

(defun Generatecont (alterns mostprobable)
  (if mostprobable
    (loop for altern on alterns
          when (car altern)
          return (loop with seuil = (om-random-value (float (reduce #'+ (car altern) :key #'proba)))
                       for n in (car altern)
                       for cumul = (proba n) then (+ cumul (proba n))
                       when (>= cumul seuil)
                       return (list n
                                    (mapremove n altern))))
    (loop for altern on alterns
          when (car altern)
          return (loop with seuil = (om-random-value (float (reduce #'+ (car altern) :key #'(lambda (x) (/ (proba x))))))
                       for n in (car altern)
                       for cumul = (/ (proba n)) then (+ cumul (/ (proba n)))
                       when (>= cumul seuil)
                       return (list n
                                    (mapremove n altern))))))

(defun Generateall (alterns)
  (loop for altern on alterns
        when (car altern)
        return (list (car altern)
                     (cdr altern))))

(defun Generatecomplex (alterns mostprobable)
  (if mostprobable
    (loop with seuil = (om-random-value (float (reduce #'+ alterns :key #'correctedproba)))
          for altern in alterns
          for cumul = (correctedproba altern) then (+ cumul (correctedproba altern))
          when (>= cumul seuil)
          return (list altern
                       (remove altern
                               alterns
                               :test #'(lambda (x y)
                                         (equal (symb x) (symb y))))))
    (loop with seuil = (om-random-value (float (reduce #'+ alterns :key #'(lambda (x) (/ (correctedproba x))))))
          for altern in alterns
          for cumul = (/ (correctedproba altern)) then (+ cumul (/ (correctedproba altern)))
          when (>= cumul seuil)
          return (list altern
                       (remove altern
                               alterns
                               :test #'(lambda (x y)
                                         (equal (symb x) (symb y))))))))

(defun weight (alterns complexity minComplex)
  (loop for altern in alterns
        for i from 0
        append (mapcar #'(lambda (y)
                           (setf (correctedproba y)
                                 (* (proba y) 
                                    (max (- 1 (* i (/ (min complexity minComplex)
                                                      minComplex)))
                                         0)))
                           y)
                       altern)))

(defun computesubsize (tree node maxPast minPast minComplex equiv1 equiv2)
  (if (and (param node)
           (eq (maxPast (param node)) maxPast)
           (eq (minPast (param node)) minPast)
           (>= (minComplex (param node)) minComplex))
    (progn
      ;(format t " ## ~A ## " (subsize node))
      (subsize node))
    (let ((length (LZlength tree))
          (size 0))
      (labels ((try (past cont context prevnext)
                 (or (>= size minComplex)
                     (let ((x (or context
                                  (mkcontext past tree minPast maxPast equiv1 equiv2))))
                       (if (and (null context) prevnext)
                         (progn
                           (if (null (param prevnext))
                             (setf (param prevnext)
                                   (make-instance 'LZparam)))
                           (setf (maxPast (param prevnext)) maxPast)
                           (setf (minPast (param prevnext)) minPast)
                           (setf (minComplex (param prevnext)) minComplex)
                           (setf (equiv1 (param prevnext)) equiv1)
                           (setf (equiv2 (param prevnext)) equiv2)
                           (setf (context prevnext) x)))
                       (if (eq (belongs (car x)) node)
                         (funcall cont t)
                         (if (and (> (caddr x) (- 1))
                                  (< (caddr x) (min minPast (length past))))
                           (funcall cont nil)
                           (progn
                             (setf (belongs (car x)) node)
                             (incf size)
                             (newtry past (cadr x) cont)))))))
               (newtry (past alterns cont)
                 (let ((y (Generateall alterns)))
                   (if y
                     (labels ((recurs (children OK)
                                (if (null children)
                                  (if OK
                                    (funcall cont t)
                                    (newtry past (cadr y) cont))
                                  (try (cons (symb (car children))
                                             (if (< (length past) length)
                                               past
                                               (butlast past)))
                                       #'(lambda (z)
                                           (recurs (cdr children) (or z OK)))
                                       (and (param (car children))
                                            (eq (maxPast (param (car children))) maxPast)
                                            (eq (minPast (param (car children))) minPast)
                                            (eq (equiv1 (param (car children))) equiv1)
                                            (eq (equiv2 (param (car children))) equiv2)
                                            (eq (minComplex (param (car children))) minComplex)
                                            (context (car children)))
                                       (car children)))))
                       (recurs (car y) nil))
                     (funcall cont nil)))))
        (try (reverse (cdr (posintree node tree)))
             #'(lambda (x) x)
             nil
             nil))
      (if (null (param node))
        (setf (param node)
              (make-instance 'LZparam)))
      (setf (subsize node) size
            (maxPast (param node)) maxPast
            (minPast (param node)) minPast
            (minComplex (param node)) minComplex)
      ;(format t " ## ~A ## " (subsize node))
      size)))

(defmethod quantize ((self list) &optional (tol 0))
  (if (> tol 0)
    (make-regular self tol 1)
    self))

(defun quantize-voice (voice tol)
  (mapc #'(lambda (note onset)
            (setf (second note) (or onset 256)))
        voice
        (quantize (x->dx (mapcar #'second  voice)) tol))
  (butlast voice))

(defun quantize-list (liste tol)
  (let* ((q-inter-onsets (quantize (mapcan #'(lambda (x) (x->dx (mapcar #'second x))) liste) tol)))
    (loop for voice in liste
          do (loop for note on voice
                   when (cdr note)
                   do (setf (second (car note)) (or (pop q-inter-onsets) 256))
                   else
                   do (setf (second (car note)) 256))))
  liste)

(defun merge-voices (poly)
  (list (length poly)
        (sort (loop for voice in poly append voice) #'< :key #'second)))

(defun unstaccate-voices (voices staccatime)
  (loop for now on voices
        when (and (cadr now)
                  (null (member-if #'(lambda (channel)
                                       (and (listp channel)
                                            (member-if #'(lambda (pitch) (> pitch 0))
                                                       channel
                                                       :key #'car)))
                                   (caar now)))
                  (or (null staccatime)
                      (loop for next in (cdr now)
                            while (< (cadr next)
                                     (+ (cadar now)
                                        staccatime))
                            when (member-if #'(lambda (channel)
                                                (and (listp channel)
                                                     (member-if #'(lambda (pitch) (> pitch 0))
                                                                channel
                                                                :key #'car)))
                                            (car next))
                            return t
                            finally (return nil))))
        do (setf voices (delete (car now) voices)))
  voices)

(defun unarpeggiate-voices (voices arpegtime)
  (loop for now on voices
        when (member-if #'(lambda (channel)
                            (and (listp channel)
                                 (member-if #'(lambda (pitch) (> pitch 0))
                                            channel
                                            :key #'car)))
                        (caar now))
        do (progn
             (loop for next in (cdr now)
                   while (< (cadr next)
                            (+ (cadar now)
                               arpegtime))
                   do (progn
                        (loop for channel in (car next)
                              for i from 0
                              when (listp channel)
                              do (loop for pitch in channel
                                       when (> (car pitch) 0)
                                       do (setf (nth i (caar now))
                                                (if (listp (nth i (caar now)))
                                                  (push pitch (nth i (caar now)))
                                                  (list pitch)))))
                        (setf voices (delete next voices))))))
  voices)

(defun synchro-release (voices releastime)
 (loop for now on voices
        when (null (member-if #'(lambda (channel)
                                  (and (listp channel)
                                       (member-if #'(lambda (pitch) (> pitch 0))
                                                  channel
                                                  :key #'car)))
                              (caar now)))
        do (progn
             (loop for next in (cdr now)
                   for beforenext in now
                   while (or (null releastime)
                             (< (cadr next)
                                (+ (cadar now)
                                   releastime)))
                   while (null (member-if #'(lambda (channel)
                                              (and (listp channel)
                                                   (member-if #'(lambda (pitch) (> pitch 0))
                                                              channel
                                                              :key #'car)))
                                          (car next)))
                   do (setf voices (delete beforenext voices)))))
  voices)

(defun unlegate-voices (voices legatime)
  (loop for now on voices
        do (loop for channel in (caar now)
                 for i from 0
                 when (listp channel)
                 do (loop for pitch in channel
                          when (> (car pitch) 0)
                          do (loop for next in (cdr now)
                                   while (notempty (car next))
                                   while (or (null legatime)
                                             (< (cadr next)
                                                (+ (cadar now)
                                                   legatime)))
                                   while (and (listp (nth i (car next)))
                                              (member (- (car pitch)) (nth i (car next)) :key #'car))
                                   do (loop for deletingchannel in (caar now)
                                            for j from 0
                                            when (listp deletingchannel)
                                            do (loop for deletedpitch in deletingchannel
                                                     do (if (and (< (car deletedpitch) 0)
                                                                 (null (and (listp (nth j (car next)))
                                                                            (member (car deletedpitch) (nth j (car next)) :key #'car))))
                                                          (loop for fromnow in now
                                                                while (notempty (car fromnow))
                                                                while (if (atom (nth j (car fromnow)))
                                                                        nil;(format t "Warning : channel already empty")
                                                                        (member (car deletedpitch) (nth j (car fromnow)) :key #'car))
                                                                do (setf (nth j (car fromnow)) (remove (car deletedpitch) (nth j (car fromnow)) :key #'car))
                                                                finally (if (null (member-if #'(lambda (channel)
                                                                                                 (and (listp channel)
                                                                                                      (member-if #'(lambda (pitch) (> pitch 0))
                                                                                                                 channel
                                                                                                                 :key #'car)))
                                                                                             (car next)))
                                                                          (setf voices (delete next voices)))))))))))
  voices)

(defun thread (prevs nbchan)
  (let ((chord (make-list nbchan :initial-element 0)))
    (loop for prev in prevs
          do (setf (nth (1- (note-channel (car prev))) chord)
                   (if (listp (nth (1- (note-channel (car prev))) chord))
                     (push (list (- (note-pitch (car prev))) (note-velocity (car prev))) (nth (1- (note-channel (car prev))) chord))
                     (list (list (- (note-pitch (car prev))) (note-velocity (car prev)))))))
    chord))

(defun sortchord (chord)
  (loop for chordpart in chord
        for i from 0
        when (listp chordpart)
        do (setf (nth i chord)
                 (sort chordpart #'(lambda (x y) (< (abs x) (abs y))) :key #'car)))
  chord)

(defun notempty (chord)
  (loop for chordpart in chord
        when (listp chordpart)
        return t
        finally (return nil)))

(defun copy-chord (source)
  (labels ((recurs (chord)
             (if (atom chord)
               nil
               (cons ((lambda (x) (if (listp x)
                                    (mapcar #'copy-list x)
                                    x))
                      (car chord))
                     (recurs (cdr chord))))))
    (recurs source)))

(defun thread-voices (nbchan voice)
  (let ((voices nil)
        (prev nil))
    (loop with events = voice
          while events
          do (loop with onset = (note-onset (car events))
                   with chord = (thread prev nbchan)
                   with prevc = (sort prev #'< :key #'cadr)
                   initially (setq chord (sortchord chord))
                   for prevent on prevc
                   while (<= (cadar prevent) onset)
                   do  (progn
                         (if (atom (nth (1- (note-channel (caar prevent))) chord))
                             nil;(print "Warning : channel already empty")
                             (setf (nth (1- (note-channel (caar prevent))) chord)
                                 (or (delete (note-pitch (caar prevent))
                                             (nth (1- (note-channel (caar prevent))) chord)
                                             :test #'(lambda (x y) (= (abs x) (abs y)))
                                             :key #'car)
                                     0)))
                         (if (and (< (cadar prevent) onset)
                                  (null (and (cdr prevent) 
                                             (= (cadar prevent) (cadadr prevent)))))
                           (push (list (copy-chord chord)
                                       (cadar prevent))
                                 voices))
                         (setf prevc (delete (car prevent) prevc)))
                   finally (return (let ((sortedchord))
                                    (setq prev prevc)
                                    (loop with events2 = events 
                                          while (and events2
                                                     (= (note-onset (car events2)) onset))
                                          do (progn (setf (nth (1- (note-channel (car events2))) chord)
                                                          (if (listp (nth (1- (note-channel (car events2))) chord))
                                                            (push (list (note-pitch (car events2))
                                                                        (note-velocity (car events2)))
                                                                  (nth (1- (note-channel (car events2))) chord))
                                                            (list (list (note-pitch (car events2))
                                                                        (note-velocity (car events2))))))
                                                    (setq prev
                                                          (push (list (car events2)
                                                                      (+ (note-onset (car events2))
                                                                         (note-duration (car events2))))
                                                                prev))
                                                    (setq events (delete (car events2) events))
                                                    (setq events2 (cdr events2))))
                                    (setq sortedchord (sortchord chord))
                                    (push (list (copy-chord sortedchord)
                                                onset)
                                          voices)
                                    voices))))
    (loop with chord = (thread prev nbchan)
          with prevc = (sort prev #'< :key #'cadr)
          initially (setq chord (sortchord chord))
          for prevent on prevc
          do  (progn
                (setf (nth (1- (note-channel (caar prevent))) chord)
                      (or (delete (note-pitch (caar prevent))
                                  (nth (1- (note-channel (caar prevent))) chord)
                                  :test #'(lambda (x y) (= (abs x) (abs y)))
                                  :key #'car)
                          0))
                (if (null (and (cdr prevent) 
                               (= (cadar prevent) (cadadr prevent))))
                  (push (list (copy-chord chord)
                              (cadar prevent))
                        voices))
                (setq prevc (delete (car prevent) prevc))))
    (nreverse voices)))

(defun prepare-voices (voices)
  (loop for voice in voices
        collect (if (zerop (note-onset (first voice)))
                  voice
                  (cons (list 0 0 0 0 (note-channel (first voice))) voice))))

(defun channelize (poly)
  (loop for voice in poly
        for channel from 1
        do (loop for note in voice
                 do (setf (note-channel note) channel)))
  poly)


(defun remove-zero (thvoice)
  (loop for event in thvoice
        for pitches = (remove 0 (first event))
        when pitches collect (cons pitches (rest event))))

(defun continuation (poly legatime arpegtime releastime staccatime toltime)
  (quantize-voice
   (unstaccate-voices
    (synchro-release
     (unarpeggiate-voices
      (unlegate-voices
       (apply #'thread-voices
              (merge-voices
               (channelize
                (prepare-voices (copy-tree poly)))));)
       legatime)
      arpegtime)
     releastime)
    staccatime)
   toltime))

(defun listcontinuation (list legatime arpegtime releastime staccatime toltime)
  (om::flat (quantize-list (loop for poly in list
                                 collect
                                 (unstaccate-voices
                                  (synchro-release
                                   (unarpeggiate-voices
                                    (unlegate-voices
                                     (apply #'thread-voices
                                            (merge-voices
                                             (channelize
                                              (prepare-voices (copy-tree poly))))) legatime) arpegtime) releastime) staccatime)) toltime) 1))

(defun resolve-cont (cont tcoef)
  (let* ((onsets (dx->x 0 (mapcar 'second cont)))
         (chords nil)
         (velocity nil)
         (durs nil)
         (channels nil)
         (listonsets nil))
    (loop for events on cont
          for onset in onsets
          do (loop with newchord = nil
                   with newvel = nil
                   with newdur = nil
                   with newchan = nil
                   for j from 0
                   for chord in (first (car events))
                   when (listp chord)
                   do (loop for note in chord
                            when (> (car note) 0)
                            do (progn
                                 (push (car note) newchord)
                                 (push (cadr note) newvel)
                                 (push (reduce #'+
                                               (loop for event in events
                                                     for prem = t then (setq prem nil)
                                                     while (listp (nth j (first event)))
                                                     while (or prem
                                                               (member-if #'(lambda (x) (= (- x) (car note)))
                                                                          (nth j (first event))
                                                                          :key #'car))
                                                     collect (second event)))
                                       newdur)
                                 (push (1+ j) newchan)))
                   finally (if newchord
                             (progn
                               (push newchord chords)
                               (push newvel velocity)
                               (push newdur durs)
                               (push newchan channels)
                               (push onset listonsets)))))
    (make-instance 'chord-seq
      :lmidic (om* 100 (reverse chords))
      :lonset (om-round (reverse listonsets) 0 (/ 1.0 tcoef))
      :ldur (om-round (reverse durs) 0 (/ 1.0 tcoef))
      :lchan (reverse channels)
      :lvel (reverse velocity)
      :legato 0)))


(defun dur-correction (durs)
  (loop for dur in durs
        collect (if (zerop dur) 256 dur)))






(defun Midi->textfile (pitch-list)
  (catch-cancel
    (let ((name (choose-new-file-dialog    
                 :prompt "Save pitches to:")))
      (when name
        (with-open-file (file name :if-exists :overwrite :direction :output)
          (loop for pitch in pitch-list
                do (format file "~D~%" pitch )) )))))



;;;; library interface

(defmethod! Transposer ((midi-info list) (offset integer))
   :initvals '(nil 0)
   :indoc '("A list, Output of a mf-info box" "An integer")
   :icon '(230) 
   :doc  "Transposes the Midi Information (given by mf-info) by <offset> semitones.
inputs:

midi-info : a list given by the mf-info box (or the output of a MidiFile)
offset : an integer

output : a list in the same format than given by mf-info box.

Transposes the Midi data by offsett <offset> in semitones and delivers
a list in the same format than given by mf-info box :

(midicents, onset-time(ms), duration(ms), velocity, channel)

This list can serve as input to the box midi->cross.
"
   (loop for track in midi-info
         collect (loop for event in track
                       collect (list (+ (note-pitch event) offset)
                                     (note-onset event)
                                     (note-duration event)
                                     (note-velocity event)
                                     (note-channel event) ) ) ) )

(defmethod! Transposer ((midifile MidiFile) (offset integer))
   (transposer (mf-info midiFile)))

(defmethod! TimeScaler ((midi-info list) (scaler float))
   :initvals '(nil 1.0)
   :indoc '("A list, Output of a mf-info box" "A float")
   :icon '(230) 
   :doc  "Time scales the Midi Info by <scaler>.

midi-info : a list given by the mf-info box
scaler : a float number > 0.0

output : a list in the same format than given by mf-info box (or the output of a MidiFile)

Time scales the Midi data by coeeficient <scaler> and delivers
a list in the same format than given by mf-info box :

(midicents, onset-time(ms), duration(ms), velocity, channel)

This list can serve as input to the box midi->cross.
"
   (loop for track in midi-info
         collect (loop for event in track
                       collect (list (note-pitch event)
                                     (round (* scaler (note-onset event)))
                                     (round (* scaler (note-duration event)))
                                     (note-velocity event)
                                     (note-channel event) ) ) ) )

(defmethod! TimeScaler ((midifile MidiFile) (scaler float))
   (TimeScaler (mf-info midifile) scaler))

(defmethod! Crop ((midi-info list) (begin number) (end number))
   :initvals '(nil 0 10000)
   :indoc '("A list, Output of a mf-info box" "A number" "A number")
   :icon '(230) 
   :doc  "Crops the Midi Info from time <begin> to <end>.

inputs:

midi-info : a list given by the mf-info box
begin, end : integer in ms

output : a list in the same format than given by mf-info box (or the output of a MidiFile)

Crops the Midi data from time <begin> to <end>, and delivers
a list in the same format than given by mf-info box :

(midicents, onset-time(ms), duration(ms), velocity, channel)

This list can serve as input to the box midi->cross.
"
   (loop for track in midi-info
         collect (loop for event in track
                       if (and (>= (note-onset event) begin) (<= (note-onset event) end))
                       collect (list 
                                (note-pitch event)
                                (- (note-onset event) begin)
                                (note-duration event)
                                (note-velocity event)
                                (note-channel event)))
         ))

(defmethod! Crop ((midifile midifile) (begin number) (end number))
   (Crop (mf-info midifile) begin end))


(defmethod! Midi->chordseqs ((midi-info list))
   :initvals '(nil )
   :indoc '("A list, Output of a mf-info box, or the output of a MidiFile box" )
   :icon '(230) 
   :doc  "Converts the output of a midifile box, or the output of a mf-info box 
to a list of chord-seqs (1 per midi track).
This list may conveniently be input into the 'chord-seqs' input of a multi-seq
box.
"
   (loop for track in midi-info
         collect 
         (let ((track2 (mat-trans track)))
           (make-instance 'chord-seq
             :LMidic (om* 100 (first track2))
             :LOnset (second track2)
             :LDur (third track2)
             :LVel (fourth track2)
             :LChan (fifth track2)
             ))))
(defmethod! Midi->chordseqs ((midifile MidiFile))
   (Midi->chordseqs (mf-info midifile)))

(defmethod! Midi->chordseq ((self list))
   :initvals '(nil )
   :indoc '("A list, Output of a mf-info box, or the output of a MidiFile box" )
   :icon '(230) 
   :doc  "Converts the output of a midifile box, or the output of a mf-info box 
to a  single chord-seq object.
The tracks will be merged, and events occuring quasi-simultaneously
will be grouped into chords with regards to the approximation parameter 'delta value' (ms)
available in the preferences of OpenMusic.
"
   (let ((newcs (make-instance 'chord-seq))
         (midilist (sort (loop for item in self append item) '< :key 'second)))
     (setQValue newcs 1000 :recursive nil)
     (setf (inside newcs) nil)
     (setf (inside newcs)  (make-quanti-chords midilist *global-deltachords*))
     (adjust-extent newcs)
     (QNormalize newcs)
     newcs))

(defmethod! Midi->chordseq ((midifile MidiFile))
   (Midi->chordseq (mf-info midifile)))


(defmethod! Midi->Cross ((midi-info list) &optional
                         (legatime nil) (arpegtime 50) (releastime nil) (staccatime nil) (toltime 0))
   :initvals '(nil nil 50 nil nil 0)
   :indoc '("A list, Output of a midi-info box" "An integer, max legato time (ms)" "An integer, min arpeggio time (ms)"
            "An integer, max release synchro time (ms) (optional)" "An integer, max staccato time (ms) (optional)" "An integer (optional)")
   :icon '(230) 
   :doc  "Transforms the output of a MidiFile into a pitch/duration Cross-Alphabet sequence.

input : a <MidiFile> object from a midifile box, or a list from a mf-info box.
        an integer or nil : maximum legato time, in ms. if nil : no time constraints for legato filter.
        an integer : minimum arpegio time, in ms.
        an integer or nil : maximum release synchro time, in ms. if nil : infinite release-synchro.
        an integer or nil : maximum staccato time, in ms. if nil : no time constraints for staccato filter.
	an integer : time tolerance of quantization, in percent.
output : a list  of polyphonic slices.

The cross-alphabet sequence is a list containing sublists of the form : (... ((c1 c2 ... cn) d) ...)
where the ci are either 0 (empty canal) or a list of the form (p1 p2 ... pm)
where the pi are pitches in midicents
and d is a duration in ms.
Each of this sublist encodes a polyphonic slice, containing a set of canals - which are sets of superposed pitches -
and lasting for a certain duration. The concatenation of all these slices
is musically equal to the original midi sequence.

This representation captures the essentials of the polyphonic/rythmic structure of
the midifile. It is thus convenient to be given as input to the  LZify function
if you want to build a statistical model of a polyphonic piece from a midifile.
If you use then LZgenerate or its variants to generate an improvisation of the piece, you'll get again
a pitch/duration cross-alphabet sequence. Then you'll need a function such as cross->chordseq
to put your data back into a musical form.

Filters are designed to simplify the alphabet of symbols and the dictionnary of patterns :

- Each time a new note is activated, the legato filter inspects the continuation of the sequence
until the release of this note or until <legatime> msec. Any note that has been activated before
the new note and that is released during the inspected part will then be released just when the
new note is activated. Therefore, this filter discards intermediate states where two juxtaposed
notes are superimposed during a very short time. 

- Each time a new note is activated, the arpeggio filter inspects the continuation of the sequence
until <arpegtime> msec. Any note that was due to be activated during the inspected part will in
fact be activated just when the former note is activated. Thus, this filter synchronizes arpeggios.

- Each time a note is released, the release synchro filter synchronizes all the following releases
- until the activation of a new note or until <releastime> msec - at the date of the first release. 
Thus, this filter synchronizes note releases.

- The staccato filter removes each release period, between two played periods, that lasts less than
<staccatime> msec.

Finally, the duration of each symbol is quantified with the help of an OMKant Library function :
make-regular. This function accepts one particular parameter, here <toltime>, which specifies the
percentage of error that is tolerated during the quantization.
"
   (continuation midi-info legatime arpegtime releastime staccatime toltime))


(defmethod! Midi->Cross ((midifile MidiFile) &optional
                         (legatime nil) (arpegtime 50) (releastime 0) (staccatime 0) (toltime 10))
   (Midi->Cross (mf-info midifile) legatime arpegtime releastime staccatime toltime))     


(defmethod! ListMidi->Cross ((midifiles list) &optional
                             (legatime nil) (arpegtime 50) (releastime nil) (staccatime nil) (toltime 0))
  :initvals '((nil) nil 50 nil nil 0)
  :indoc '("A list of Outputs of midi-info boxes" "An integer, max legato time (ms)" "An integer, min arpeggio time (ms)"
           "An integer, max release-synchro time (ms) (optional)" "An integer, max staccato time (ms) (optional)" "An integer (optional)")
  :icon '(230) 
  :doc  "Transforms the outputs of MidiFiles into a pitch/duration Cross-Alphabet sequence.

input : a list of midi-info lists from  mf-info boxes.
        an integer or nil : maximum legato time, in ms. if nil : no time constraints for legato filter.
        an integer : minimum arpegio time, in ms.
        an integer or nil : maximum release synchro time, in ms. if nil : infinite release-synchro.
        an integer or nil : maximum staccato time, in ms. if nil : no time constraints for staccato filter.
	an integer : time tolerance of quantization, in percent.
output : a list  of polyphonic slices.

The cross-alphabet sequence is a list containing sublists of the form : (... ((c1 c2 ... cn) d) ...)
where the ci are either 0 (empty canal) or a list of the form (p1 p2 ... pm)
where the pi are pitches in midicents
and d is a duration in ms.
Each of this sublist encodes a polyphonic slice, containing a set of canals - which are sets of superposed pitches -
and lasting for a certain duration. The concatenation of all these slices
is musically equal to the original midi sequence.

This representation captures the essentials of the polyphonic/rythmic structure of
the midifile. It is thus convenient to be given as input to the  LZify function
if you want to build a statistical model of a polyphonic piece from a midifile.
If you use then LZgenerate or its variants to generate an improvisation of the piece, you'll get again
a pitch/duration cross-alphabet sequence. Then you'll need a function such as cross->chordseq
to put your data back into a musical form.

Filters are designed to simplify the alphabet of symbols and the dictionnary of patterns :

- Each time a new note is activated, the legato filter inspects the continuation of the sequence
until the release of this note or until <legatime> msec. Any note that has been activated before
the new note and that is released during the inspected part will then be released just when the
new note is activated. Therefore, this filter discards intermediate states where two juxtaposed
notes are superimposed during a very short time. 

- Each time a new note is activated, the arpeggio filter inspects the continuation of the sequence
until <arpegtime> msec. Any note that was due to be activated during the inspected part will in
fact be activated just when the former note is activated. Thus, this filter synchronizes arpeggios.

- Each time a note is released, the release synchro filter synchronizes all the following releases
- until the activation of a new note or until <releastime> msec - at the date of the first release. 
Thus, this filter synchronizes note releases.

- The staccato filter removes each release period, between two played periods, that lasts less than
<staccatime> msec.

Finally, the duration of each symbol is quantified with the help of an OMKant Library function :
make-regular. This function accepts one particular parameter, here <toltime>, which specifies the
percentage of error that is tolerated during the quantization.
"
  
  (mapcan #'(lambda(x) (Midi->Cross x legatime arpegtime releastime staccatime toltime))
          midifiles))

(defmethod! ChordSeq->Midi ((chordseq chord-seq))
   :initvals '(nil)
   :indoc '("A chordseq")
   :icon '(230) 
   :doc  "Transforms a chord-seq into a mf info.

input : a Chord-Seq.

output : a mf-info.

"
   (list (loop for chord in (inside chordseq)
               for onset in (Lonset chordseq)
               append (loop for midic in (LMidic chord)
                            for vel in (Lvel chord)
                            for dur in (LDur chord)
                            for chan in (Lchan chord)
                            collect (list (/ midic 100) onset dur vel chan)))))


(defmethod! Cross->ChordSeq ((Cross list) &optional (time-coef 1.0))
   :initvals '(nil 1.0)
   :indoc '("A cross-alphabet sequence" "Time scaling coef.")
   :icon '(230) 
   :doc  "Transforms a Cross-Alphabet sequence into a chord-seq.

inputs :

cross : a pitch/duration cross-alphabet sequence generated by the function midi->cross.
time-coeff : float number > 0.0

output : a <chord-seq> object.

see midi->cross.
"
(resolve-cont cross time-coef))

