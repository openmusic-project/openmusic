;;;=====================
;;; BACH IMPORT / EXPORT for OM SCORE OBJECTS
;;; By Carlos Agon
;;;=====================


(in-package :om)

(defmethod om2bach ((self t) path)
  (om-beep-msg "Can not export this object to bach"))

(defmethod om2bach ((self voice) path)
  (om2bach (make-instance 'poly :voices (list self)) path))

(defmethod om2bach ((self poly) path)
  (let ((clefs (loop for item in (voices self) 
                        collect `g))
        (channels (loop for item in (voices self) 
                        for i = 1 then (+ i 1) collect 1)))
    (WITH-OPEN-FILE (out path :direction :output  
                       :if-does-not-exist :create :if-exists :supersede) 
      (format out "score (clefs ~{ ~D ~}) ~%" clefs)
      (format out "(midichannels ~{ ~D ~}) ~%" channels)
      (loop for item in (voices self) do
            (trans-dany item out)))))

(defmethod om2bach ((self chord-seq) path)
  (om2bach (make-instance 'multi-seq :chord-seqs (list self)) path))

(defmethod om2bach ((self multi-seq) path)
  (let ((clefs (loop for item in (chord-seqs self) 
                        collect `g))
        (channels (loop for item in (chord-seqs self) 
                        for i = 1 then (+ i 1)  collect i)))
    (WITH-OPEN-FILE (out path :direction :output  
                       :if-does-not-exist :create :if-exists :supersede) 
      (format out "roll (clefs ~{ ~D ~}) ~%" clefs)
      (format out "(midichannels ~{ ~D ~}) ~%" channels)
      (loop for item in (chord-seqs self) do
            (trans-dany item out)))))

;======================================
;
(defun RT-root (tree)
  (if (consp tree) (car tree) tree))

(defun RT-childs (tree)
  (second tree))

(defun mk-RT (root childs)
  (list root childs))

(defun RT-val (val)
  (cond
   ((listp val) (/ (first val) (second val)))
   ((floatp val) (abs (round val)))
   (t (abs val))))

;=======================================
(defun get-symbolic-extent-in-measure (mes)
  (let ((list (cons-chord&rest-list mes)))
    (loop for item in list
                    collect
                    (symbolic-extent item))))

(defun get-symbolic-offset-in-measure (mes)
  (let ((list (cons-chord&rest-list mes)))
    (loop for item in list
                    collect
                    (symbolic-offset item mes))))

(defmethod symbolic-extent ((self simple-container))
  (if (parent self)
     (* (/ (/ (slot-value self 'extent) (Qvalue self))
           (/ (slot-value (parent self) 'extent) (Qvalue (parent self))))
        (symbolic-extent (parent self)))
    (/ (slot-value self 'extent) (Qvalue self))))

(defmethod symbolic-offset ((self simple-container) parent)
  (if (and (parent self) (not (equal parent self)))
     (+ (/ (offset self) (QValue (parent self)))
        (symbolic-offset (parent self) parent))
    0))


(defmethod bach-Extent->ms ((self simple-container) &key (parent nil))
  "Converts the extent of <self> to milliseconds"
  (round (* 1000 (/ 60 (Qtempo self)) (symbolic-extent self))))




;=======================================

(defun get-onset-from-tree (i tree)
  (let ((tree-o-list (tree2onsets tree)) rep)
    (setf rep (dx->x 0 tree-o-list))
    (nth i rep)))

(defun tree2onsets (tree)
  (let ((tree-o (tree2onsets-tree tree)))
    (rec-tree2onsets tree-o)))

(defun rec-tree2onsets (tree)
  (if (atom tree) (list tree)
    (loop for item in (RT-childs tree) append
          (rec-tree2onsets item))))

(defun tree2onsets-tree (tree)
  (let* ((root (RT-val (RT-root tree)))
         (childs (RT-childs tree))
         (sum (loop for item in childs sum  (RT-val (RT-root item)))))
    (list root
          (loop for item in childs collect
                (if (atom item)
                    (* root (/ (RT-val item) sum))
                  (tree2onsets-tree (list  (* root (/ (RT-val (RT-root item)) sum)) (RT-childs item))))))))

;=========================================
    

(defun get-tempo-changes-in-mesure (tempo mesnum)
  (let (list rep)
    (loop for item in (second tempo)
            while (not rep) do
            (if (= (caar item) mesnum)
                (push item list))
            (when (> (caar item) mesnum) (setf rep t)))
      (if (= mesnum 0)
            (append (list (first tempo)) (reverse list))
            (reverse list))))

(defun write-tempo-for-max (tree tempo-list mesnum)
  (let (rep)
   (loop for item in tempo-list do
          (if  (atom (car item))
              (push item rep)
            (if (not (= (first (car item)) mesnum))
                     (push  (first-n (second item) 2) rep)
              (let ((onset (get-onset-from-tree (second (car item)) tree))
                    list) 
                (if (third (second item))
                    (setf list (list  (car (second item)) (second (second item)) onset 1))
                  (setf list (list  (car (second item)) (second (second item)) onset)))
                (push list rep)))))
   (reverse rep)))

(defmethod trans-dany ((self voice) out)
  (format out "(~%")
  (loop for item in (inside self)
        for i = 0 then (+ i 1) do
        (trans-dany-mes item out i))
  (format out ")~%"))

(defmethod trans-dany-mes ((self measure) out i)
  (let* ((tree (tree self))
         (tempo (get-tempo-changes-in-mesure (tempo (parent self)) i)))
    (setf tempo (write-tempo-for-max tree tempo i))
  (format out "(((~D ~D) (~{ ~D ~})) ~%" (fnumerator (first tree)) (fdenominator (first tree)) tempo)
  (loop for item in (inside self) do
          (trans-dany item out ))
  (format out " 0)~%")))

(defmethod trans-dany ((self rest) out )
   (format out "(~D 0)~%"  (* -1 (/ (extent self) (qvalue self) 4))))

(defmethod trans-dany ((self note) out)
  (let ((tied 0))
    (when (and (tie self)  (or (equal (tie self) 'begin) (equal (tie self) 'continue)))
      (setf tied 1))
  (format out "(~D ~D ~D 0) " (midic self) (vel self) tied)))

(defmethod trans-dany ((self chord) out)
  (format out "(~D " (/ (extent self) (qvalue self) 4))
  (loop for item in (inside self) do
        (trans-dany item out))
  (format out "0)~%"))

(defmethod trans-dany ((self group) out)
 (loop for item in (inside self) do
        (trans-dany item out))
 )

(defmethod trans-dany((self chord-seq) out)
   (format out "(~%")
  (loop for chord in (inside self) do
          (trans-dany-ms chord out ))
    (format out ")~%"))


(defmethod trans-dany-ms ((self chord) out)
  (format out "(~D " (offset->ms self))
  (loop for item in (inside self) do
        (trans-dany-ms item out))
  (format out "0)~%"))

(defmethod trans-dany-ms ((self note) out)
  (format out "(~D ~D ~D 0) " (midic self) (bach-extent->ms self) (vel self) ))


;=======================================================

(defmethod! bach2om (path)
  (unless path
    (setf path (om-choose-file-dialog)))
  (when path
    (WITH-OPEN-FILE (in path :direction :input) 
      (let ((type (read in nil :eof))
            (token t)
            channels
            info)
        (loop while (and token (not (equal token :eof))) do
              (setf token (read in nil :eof))
              (when (and token (not (equal token :eof)))
              (cond ((equal (car token) 'midichannels)
                     (setf channels (cdr token)))
                    ((symbolp (car token)) t)
                    (t
                     (push  token info)
                       ))))
        (setf info (reverse info))
        (cond
         ((and (equal type 'roll) (= (length info) 1)) (bach2chord-seq (car channels) (car info)))
         ((equal type 'roll) (bach2multi-seq channels info))
         ((and (equal type 'score) (= (length channels) 1)) (bach2voice (car channels) (car info)))
         ((equal type 'score) (bach2poly channels info))
         (t nil))))))

;chord-seq
 (defun bach2chord-seq (channels info)
   (let (lmidic lonset lvel ldur)
   (loop for item in info do
         (let* ((onset (round (car item)))
                (info (cdr item)) lm lv ld)
           (loop for note in info do
                 (when (listp note)
                   (push (car note) lm)
                   (push (round (second note)) ld)
                   (push (third note) lv)))
           (push onset lonset)
           (push (reverse lm) lmidic)
           (push (reverse ld) ldur)
           (push (reverse lv) lvel)))
   (make-instance 'chord-seq 
           :lmidic (reverse lmidic)
           :lonset (reverse lonset)
           :ldur (reverse ldur)
           :lvel (reverse lvel)
           :lchan (list channels))))

;multi-seq
 (defun bach2multi-seq (channels info)
   (make-instance 'multi-seq 
                  :chord-seqs (loop for cs in info
                                    for ch in channels
                                    collect (bach2chord-seq ch cs))))

(defun bach-compute-tempo-pos (pos durs)
  (if (not pos) 0
    (let ((accum 0) rep)
      (loop for item in durs
            for i = 0 then (+ i 1) 
            while (not rep) do
            (if (>= accum pos)
                (setf rep i)
              (setf accum (+ accum (abs item)))))
      (setf rep (or rep 0))
      (min rep (- (length durs) 1)))))

(defun bach-cons-tempo-list (durs tempolist mesnum)
   (loop for item in tempolist collect
        (let* ((pos (third item))
               (pos (bach-compute-tempo-pos pos durs))
               (dyn? (fourth item)))
          (list (list mesnum pos) (list (first item) (second item) (and dyn? (not (zerop dyn?))))))))
            

;voice
 (defun bach2voice (channels info )
   (let (ltree lchords ltempo totaltied lsigns ldurations simpleties poslist)
     (loop for mes in info
           for i = 0 then (+ i 1) do
           (when (listp mes)
           (let* ((signature (car mes))
                  (sign (car signature))
                  (tempolist (second signature))
                  (info (cdr mes)) durs chords)
             (loop for item in info do
                   (when (listp item)
                   (let* ((dur (car item))
                          (resto (cdr item)) lm lv chord ltied)
                     (loop for note in resto do
                           (when (listp note)
                             (push (car note) lm)
                             (push (second note) lv)
                             (push (third note) ltied)))
                     (push dur durs)
                     (unless (minusp dur) (push (reverse ltied) totaltied))
                     (if (= (get-type-of-tie ltied) 2) (push nil chords) 
                       (unless (minusp dur) 
                         (let ((achord (make-instance 'chord
                                                      :lmidic (reverse lm)
                                                      :lvel (reverse lv))))
                           (push achord chords)))))))
             (push sign lsigns)
             (setf ltempo (append ltempo (bach-cons-tempo-list (reverse durs) tempolist i)))
             (setf ldurations (append ldurations (reverse durs)))
             (setf lchords (append lchords (reverse chords))))))
     (setf ltempo (if (not ltempo) 60
                    (let ((ftempo (car ltempo))
                          (first (caar ltempo)))
                      (if (equal first '(0 0))
                         (list (list (first (second ftempo)) (second (second ftempo)))
                               (cdr ltempo))
                        (list '(1/4 60) ltempo)))))
     (setf totaltied (reverse totaltied))
     (setf lsigns (reverse lsigns))  
     (setf ltree (mktree  ldurations lsigns))
     (loop for tie in totaltied
           for chord in lchords
           for pos = 1 then (+ pos 1) do
           (let ((tietype (get-type-of-tie tie)))
             (case tietype
               (0 (push nil simpleties))
               (1 (push (get-tied-notes-from-chord tie chord) simpleties))
               (2 (progn
                    (push nil simpleties)
                    (push pos poslist)
                    )))))
     (setf ltree (tie-nieme-pos ltree poslist))
     (setf lchords (remove nil lchords))
     (make-instance 'voice
                    :tree ltree
                    :chords lchords
                    :ties (reverse simpleties)
                    :tempo ltempo)
 ))

(defvar *global-tree-pos* 0)
(defun change-all-positions-in-ties (list poslist)
  (cond 
   ((null list) list)
   ((atom list) 
    (when (and (integerp list) (not (minusp list)))
      (setf *global-tree-pos* (+ *global-tree-pos* 1)))
      (if (member *global-tree-pos* poslist) (* list 1.0) list))
   (t (list (first list) (loop for item in (second list) collect (change-all-positions-in-ties item poslist))))))



(defun tie-nieme-pos (tree pos)
  (let* ((root (RT-root tree))
         (childs (RT-childs tree))
         (*global-tree-pos* -1))
    (list root
          (loop for item in childs collect
                (change-all-positions-in-ties item pos)))))

(defun get-tied-notes-from-chord (tie chord)
  (loop for item in tie
        for note in (Lmidic chord)
        when (= item 1) collect note))

(defun get-type-of-tie (list)
  (cond
   ((or (null list) (not (member 1 list))) 0)
   ((member 0 list) 1)
   (t 2)))


;poly
 (defun bach2poly (channels info)
   (make-instance 'poly
                  :voices (loop for voice in info
                                    for ch in channels
                                    when (listp voice) collect (bach2voice ch voice))))


;====================INTERFACE=======================

(defmethod score-export ((format (eql 'bach)) object params)
  (export-bach object))

(defmethod score-import ((format (eql 'bach)) object)
  (let ((name (catch-cancel (om-choose-file-dialog)))
        (import-obj nil))
    (when name
      (setf import-obj (print (bach2om name)))
      (if (equal (type-of import-obj) (type-of object))
          import-obj
        (objfromobjs import-obj object))
      )))


;=======BOXES==========

(defmethod! export-bach ((self t) &optional (path nil))
  :icon 351
  :indoc '("a VOICE, POLY, CHORD-SEQ or MULTI-SEQ object" "a target pathname")
  :initvals '(nil nil)
  :doc "
Exports <self> to bach format.
"
  (let* ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New bach file"
                                                       :types '("Text File" "*.txt")
                                                       ))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (om2bach self pathname))
    pathname))


(defmethod! import-bach (&optional path)
  :icon 352
  :doc "
Constructs a Musical object from a bach file.

- <path> allows to specify the file to import. If not specified, a file chooser dialog will be dispaled at evaluating the box.
"
  (let ((name (catch-cancel (or path (om-choose-file-dialog)))))
    (when name
      (bach2om name))))





           
 






