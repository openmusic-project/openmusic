(defun adj-ints (pcset)
  "intervals between adjacent elements, including wrap-around interval from last to first"
  (do* ((result nil)
        (prep1 (sort (remove-duplicates pcset) #'<))
        (prep2 (append prep1 (list (car prep1)))
               (cdr prep2)))
       ((null (cdr prep2)) (reverse result))
    (push (12- (second prep2) (first prep2)) result)))

(defun nth-rotation (n lst)
  "rotate right: (nth-rotation 2 '(1 2 3 4 5)) ==> (4 5 1 2 3)"
  (let ((m (mod n (length lst))))
    (append (last lst m)
            (butlast lst m))))

(defun rotate-left (lst)
  "rotate one position left: (rotate-left '(0 1 2 3)) ==> (1 2 3 0)"
  (append (cdr lst) (list (car lst))))

(defun most-left-packed-rotation (ints)
  (do* ((n 1 (1+ n))
        (rev-ints (reverse ints))
        (best rev-ints))
       ((= n (length ints)) (reverse best))
    (let ((candidate (nth-rotation n rev-ints)))
      (when (bigger candidate best)
        (setf best candidate)))))

(defun most-left-packed-order (ints)
  "tests rotations and their retrogrades"
  (do* ((n 1 (1+ n))
        (rev-ints (reverse ints))
        (best rev-ints))
       ((> n (length ints)) (reverse best))
    (let ((candidate (nth-rotation n rev-ints)))
      (when (bigger candidate best)
        (setf best candidate))
      (setf candidate (reverse candidate))
      (when (bigger candidate best)
        (setf best candidate)))))

(defun bigger (list1 list2)
  (unless (or (null list1) (null list2))
    (or (> (car list1) (car list2))
        (and (= (car list1) (car list2))
             (bigger (cdr list1) (cdr list2))))))

(defun make-pcset-from-ints (int-list start-pc)
  (if (null int-list)
    nil
    (cons start-pc
          (make-pcset-from-ints (cdr int-list)
                                (12+ start-pc (car int-list))))))




(defun t-primeform-n (pcset n)
  :icon 131
  :doc
  "representative member of set class built on equivalence
to <pcset> under transposition (or, given list of pcsets,
returns list of t-primeforms)"
  :initvals '((1 3 4))
  :indoc '("any member of set class whose t-primeform is sought (or list with one member per desired t-primeform")
  :numouts 1
  
  (if (consp (car pcset))
    (t-primeforms-n pcset n)
    (let* ((adj (n-structure pcset n))
           (best-ints (most-left-packed-rotation adj)))
      (n-scale best-ints))))

(defun t-primeforms-n (pcsets n)
  (loop for item in pcsets
        collect (t-primeform-n item n)))


(defun transp-comb (a tid n)
  (mapcar 
   #'(lambda (a1)
       (mapcar #'(lambda (e) (mod e n)) (dx->x a1 (butlast (n-structure tid n))))
       )
   a)
)

;(transp-comb '(0 1) '(0 4 8) 12)
;(dx->x 0 (butlast (n-structure '(0 4 8) 12)))


(defun couples (n)
  (mapcar #'(lambda (e) (list 0 e)) 
   (arithm-ser 1 (/ n 2) 1)))
;(couples 12)

(defun triples (n)
  (remove '(0 1 1) (flat (mapcar #'(lambda (e) (list (list 0 0 e) (list 0 1 e))) 
   (arithm-ser 1 (/ n 2) 1)) 1) :test #'equal))
;(triples 12)

(defun ntuples (n)
  (flat (mapcar #'(lambda (i) (mapcar #'(lambda (j) (append (arithm-ser 0 (- i 1) 1)
                                                      (list j)))
                                (arithm-ser i (/ n 2) 1)))
          (arithm-ser 1 (/ n 2) 1)) 1)
  )

;(ntuples 12)


(defun transp-comb-couples (tid n)
  (remove-dup 
  (mapcar #'(lambda (c1) (let ((res (transp-comb c1 tid n)))
                           (sort. (x-union (first res) (second res)))))
   (couples n))
  #'equal
  1
))

;(transp-comb-couples '(0 4 8) 12)

(defun transp-comb-triples (tid n)
  (remove-dup 
  (mapcar #'(lambda (c1) (let ((res (transp-comb c1 tid n)))
                           (sort. (x-union (x-union (first res) (second res))
                                           (third res)))))
   (triples n))
  #'equal
  1
))
;(transp-comb-triples '(0 4 8) 12)

(defun transp-comb-ntuples (tid n)
  (remove-dup 
  (mapcar #'(lambda (c1) (all-union (transp-comb c1 tid n)))
   (ntuples n))
  #'equal
  1
))

(defun get-tid (n)
  (mapcar #'(lambda (d1) (first (famille n "tid" d1 1)))
   (rest (butlast (divisors n))))
  )

;(get-tid 12)

(defun all-union (sets)
  (sort. (reduce #'x-union sets))
  )
;(all-union '((0 1 2 3 7) (0 1 2 4) (0 1 2 3) (15)))

(defun TL-ZN (n)
  (remove-dup (t-primeform-n (remove (arithm-ser 0 (- n 1) 1) (remove-dup 

     (flat (mapcar 
            #'(lambda (tid1) (transp-comb-ntuples tid1 n))
            (get-tid n)) 1)
     #'equal
     1
     ) :test #'equal) n) #'equal
     1))

;(TL-ZN 24)
;(length (TL-ZN 24))

(in-package :om)

(defmethod! norm-order ((n number) (set list))
  
  ; init values for the function arguments
  :initvals '(12 '(0 1 2))
  
  ; doc strings for the arguments
  :indoc '("Zn" "set")
  
  ; icon for the function InvertIntervals. The icon (resource 'cicn') must be added with resedit to the file K'sLib.lib
  ; and given the corresponding number (e.g. 250).
  :icon 250 
  
  :doc  "Get the normal order of the set."
  
  ; then the body of the function :
  (if (numberp (first set))
    (start-at-zero-n
     (find-good-set-n
      (find-good-sets-n
       (build-matrix-list-n n
                            (remove-duplicates
                             (sort (mapcar #'(lambda (x) (mod x n)) set) #'<))))))
    (mapcar #'(lambda (el) (norm-order n el)) set))
    )
    
;(norm-order 12 '(60 67 63))

(defun build-matrix-list-n (n l)
  (let ((list l))
    (loop while l
          collect list
          do (setf list (append (cdr list) (list (+ (pop l) n))))
          )))

(defun start-at-zero-n (set)
  (let* ((beg (car set)))
    (mapcar #'(lambda (x) (- x beg)) set)))


; Check the difference between the first and the last pitch class of a list of sets
;    and return the smallest one (one or several).
(defun find-good-sets-n (sets)
  (let* ((differences (mapcar #'(lambda (l) (- (car (last l)) (first l))) sets))
         (min (apply #'min differences)))
    (loop for set in sets
          when (= (- (car (last set)) (car set)) min)
          collect set)))

; Receives a list of at least one good set and returns the one in which the
;    difference between the first and second pitch class is smallest or try
;    with the following pitch-classes (difference between 2nd and 3rd, and
;    so on).
(defun find-good-set-n (sets)
  (let ((diff 12) ; a large difference will trigger the first collection
        (curr-diff ())
        (result ()))
    ; if the set has at least two elements
    (if (cdar sets)         ;    look for the difference
      (loop for set in sets
            do (setf curr-diff (- (cadr set) (car set)))
            if (< curr-diff diff)
            do (setf diff curr-diff)
            and do (setf result set)
            else if (= curr-diff diff)
            do (setf result
                     (find-subset-n
                      (find-good-set-n (mapcar #'cdr sets))
                      sets)))
      ; otherwise the two sets are the same
      (setf result (car sets)))  ;   and return the first one
    result))


; Return the set in the list of sets whose cdr is equal to the argument
(defun find-subset-n (set sets)
  (loop for el in sets
        when (equal set (cdr el))
          return el))


;---------------------------------------
(defmethod! all-subsets ((n number) (set list))
  
  ; init values for the function arguments
  :initvals '(12 '(0 1 2 3 4))
  
  ; doc strings for the arguments
  :indoc '("Zn" "set")
  
  ; icon for the function InvertIntervals. The icon (resource 'cicn') must be added with resedit to the file K'sLib.lib
  ; and given the corresponding number (e.g. 250).
  :icon 250 
  
  :doc  "All subsets."
  
  ; then the body of the function :
  (sort
   (remove-duplicates
    (mapcar #'(lambda (x) (norm-order n x)) (allsub (norm-order n set))) :test #'equal)
   #'< :key #'length)
   )
    
;(all-subsets 12 '(60 61 63 66 67))



(defun allsub-1 (l)
  (let ((beg nil) (end l))
    (loop while end
          collect (append beg (cdr end))
          do (setf beg (append beg (list (pop end))))))
  )

;(allsub-1 '(1 2 3 4 5 6))

(defun allsub (l)
  (remove-duplicates (allsub-h l) :test #'equal)
)

(defun allsub-h (l)
  (if (< 2 (length l))
     (append (list l)
             (loop for el in (allsub-1 l)
                   append (allsub-h el)))
     (list l))
  )

;(allsub '(1 2 3 4 5))

;(remove-dup (flat (allsub '(1 2 3 4 5)) 1) #'equal 1)
;---------------------------------------

(defmethod! setp ((n number) (set list))
  
  ; init values for the function arguments
  :initvals '(12 '(0 1 2 3 4))
  
  ; doc strings for the arguments
  :indoc '("Zn" "set")
  
  ; icon for the function InvertIntervals. The icon (resource 'cicn') must be added with resedit to the file K'sLib.lib
  ; and given the corresponding number (e.g. 250).
  :icon 250 
  
  :doc  "Returns true, if the list is a pure set without containing any duplicates."
  
  ; then the body of the function :
  (= (length
      (remove-duplicates (mapcar #'(lambda (el) (mod el n)) set)))
     (length set))
   )

;(setp 12 '(0 1 2 0))

;---------------------------------------

(defmethod! get-gil-n ((n number) (set list))
  
  ; init values for the function arguments
  :initvals '(12 '(0 1 2 3 4))
  
  ; doc strings for the arguments
  :indoc '("Zn" "set")
  
  ; icon for the function InvertIntervals. The icon (resource 'cicn') must be added with resedit to the file K'sLib.lib
  ; and given the corresponding number (e.g. 250).
  :icon 250 
  
  :doc  "Get all contained intervals."
  
  ; then the body of the function :
  (sort
   (loop for el in (get-i (norm-order n set))
         collect (if (< (truncate (/ n 2)) el) (- n el) el))
   #'<)
   )

;(get-iv-n 12 '(60 63 67 70))

(defun get-i (l)
  (if (< 1 (length l))
    (let ((e1 (first l)) (re (cdr l)))
      (append
       (loop for el in re
             collect (- el e1))
       (get-i re)))
    nil)
  )

;(get-i '(0 1 2 3 4))