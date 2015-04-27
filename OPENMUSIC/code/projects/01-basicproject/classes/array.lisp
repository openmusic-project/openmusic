;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000, 2009 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: J. Bresson


(in-package :om)

(defclass internal-array () 
   ((numcols :initform 1 :initarg :numcols :accessor numcols) ; components
    (Lcontrols :initform nil :accessor Lcontrols) 
    (data :initform nil :accessor data))
   (:documentation "internal array class"))


;;; compat
(defmethod (setf lprecision) (val self) nil)

(defmethod num-array-slots ((self internal-array))
  (length (get-all-initargs-of-class (type-of self))))

(defmethod num-array-controls ((self internal-array))
  (length (Lcontrols self)))

(defmethod numrows ((self internal-array))
  (+ (num-array-slots self) (num-array-controls self)))

;;; compat 
(defmethod (setf numrows) (val (self internal-array)) nil)

(defmethod row-control-list ((self internal-array))
  (append (mapcar #'(lambda (slot) (slot-value self (internp (name slot) (slot-package slot))))
                  (get-all-initargs-of-class (type-of self)))
          (mapcar #'cadr (Lcontrols self))))

;------------Look for the slot position of a slot.

(defmethod index2label ((self internal-array) index)
    (let ((names (slot-names (type-of self))))
     (or (nth index names)
         (string (car (nth (- index (length names)) (Lcontrols self)))))))

(defmethod label2index ((self internal-array) label)
   (let ((pos (label2index (type-of self) label)))
     (unless pos
       (setf pos (position (string label) (Lcontrols self) :test 'string-equal :key #'(lambda (x) (string (car x)))))
       (when pos (setf pos (+ pos (num-array-slots self)))))
     pos))

(defmethod label2index ((class omclass) label)
   (label2index (class-name class) label))

(defmethod label2index ((class symbol) label)
   "Return the position of the labeled slot."
   (position (string label) (get-all-initargs-of-class class) :test 'string-equal :key 'name))


(defmethod initialize-instance :after ((self internal-array) &rest l)
  (declare (ignore l))
  (set-data self))

(defmethod set-data ((self internal-array))
  (setf (data self)
        (append (mapcar #'(lambda (slot) 
                            (let ((ctrl (slot-value self (internp (name slot) (slot-package slot)))))
                              (get-array-data self ctrl (numcols self) (thetype slot))))
                        (get-all-initargs-of-class (type-of self)))
                (mapcar #'(lambda (control)
                            (let ((ctrl (cadr control)))
                              (get-array-data self ctrl (numcols self))))
                        (LControls self)))))


(defmethod get-array-data (array controlvalue numcols &optional type)
  (let ((defdata (if (and (consp controlvalue)
                          (or (functionp (car controlvalue))
                              (and (symbolp (car controlvalue)) (fboundp (car controlvalue)))))
                     
                     (let (;(initdata (get-array-data array (cadr controlvalue) numcols type))
                           (args (mapcar #'(lambda (arg)
                                             (if (stringp arg)
                                                 (array-data-from-control 
                                                  (get-control-value array (label2index array arg))
                                                  numcols)
                                               (array-data-from-control arg numcols)))
                                         (cdr controlvalue))))
                       
                       ;(if initdata
                       ;    (loop for i from 0 to (1- numcols) collect 
                       ;          (apply (car controlvalue)
                       ;                 (cons (nth i initdata)
                       ;                       (mapcar #'(lambda (list) (nth i list)) args))))
                         (loop for i from 0 to (1- numcols) collect 
                                 (apply (car controlvalue)
                                        (mapcar #'(lambda (list) (nth i list)) args))
                                 )
                       ;  )
                       )
                   
                   (array-data-from-control controlvalue numcols))))
    
    (if (and type (not (equal type t)))
         (coerce-array-data array controlvalue defdata type)
      defdata)))


(defmethod coerce-array-data (array controlvalue defdata type)
  (cond 
   ;;; each element must be a list...
   ((subtypep type 'list)
    (mapcar 'list! defdata))
   ;;; controlvalue was already of the good type...
   ((subtypep (type-of controlvalue) type)
    (loop for i from 1 to (length defdata) collect (clone controlvalue)))
   ((omclass-p (class-of (find-class type nil)))
    (loop for obj in defdata collect (objfromobjs obj (get-super-default-value type))))
   (t defdata)))



(defmethod array-data-from-control ((controlvalue t) numcols)
  (loop for i from 0 to (- numcols 1)
        collect (eval (omng-copy controlvalue))))

(defmethod array-data-from-control ((controlvalue null) numcols) 
  ;nil
  (make-list numcols)
  )

(defmethod array-data-from-control ((controlvalue list) numcols)
  (loop for i from 0 to (- numcols 1)
        collect (eval (omng-copy (nth (mod i (length controlvalue)) controlvalue)))))


(defparameter *array-precision* 10)

(defmethod array-data-from-control ((controlvalue bpf) numcols)
  (when (> numcols 0)
    (let* ((ranges (real-bpf-range controlvalue)))
      (let ((data (list! (nth 2 (multiple-value-list (om-sample controlvalue numcols 
                                                                (float (/ (first ranges) (expt 10 (decimals controlvalue))))
                                                                (float (/ (second ranges) (expt 10 (decimals controlvalue))))
                                                                *array-precision*))))))
        (or (and (= (length data) numcols) data)
            (append data (make-list (- numcols (length data)) :initial-element (car (last data))))))
    )))

(defmethod array-data-from-control ((controlvalue function) numcols)
   (loop for i from 1 to numcols
         collect (funcall controlvalue i)))

(defmethod array-data-from-control ((controlvalue symbol) numcols)
   (if (fboundp controlvalue)
       (loop for i from 1 to numcols
             collect (funcall (fdefinition controlvalue) i))
     (call-next-method)))


(defmethod get-control-value ((self internal-array) (str string))
  (let ((ind (label2index self str)))
    (when ind (get-control-value self ind))))

(defmethod get-control-value ((self internal-array) (index integer))
  (let ((ns (num-array-slots self)))
   (if (< index ns)
     (let* ((theslot (nth index (get-all-initargs-of-class (type-of self)))))
       (slot-value self (internp (name theslot) (slot-package theslot)))
       ;(funcall (internp (name theslot) (slot-package theslot)) self)
       )
     (let ((control (nth (- index ns) (Lcontrols self))))
       (if control
         (second control)
         (error (format nil "Bad index ~D" index)))))))


(defmethod row-type ((self internal-array) num)
   (let ((slots (get-all-initargs-of-class (type-of self))) theslot)
     (setf theslot (nth num slots))
     (if theslot (thetype theslot) 'number)))


;;=====================
;; DATA ACCESS
;;=====================
(defmethod get-array-row ((self internal-array) (row integer))
  (when (data self)
    (nth row (data self))))

(defmethod get-array-row ((self internal-array) (row string))
  (get-array-row self (label2index self row)))

(defmethod get-array-row ((self internal-array) (row symbol))
  (get-array-row self (label2index self row)))

(defmethod get-array-row ((self internal-array) (row t)) nil)
(defmethod get-array-row ((self internal-array) (row null)) nil)

 
(defmethod get-array-val ((self internal-array) (row integer) col)
  (let ((r (nth row (data self))))
    (if (consp r) (nth col r) r)))

(defmethod get-array-val ((self internal-array) (row string) col)
  (get-array-val self (label2index self row) col))

(defmethod get-array-val ((self internal-array) (row symbol) col)
  (get-array-val self (label2index self row) col))


(defmethod get-array-col ((self internal-array) col)
  (loop for r from 0 to (- (numrows self) 1) 
        collect (get-array-val self r col)))



(defmethod get-row-bpf ((array t) (row t) &optional (precision 4)) nil)

(defmethod get-row-bpf ((array t) (row list) &optional (precision 4))
  (if (and (list-subtypep row 'number) (> (length row) 1))
      (simple-bpf-from-list (if (= 1 (length row)) '(0) '(0 1)) row 'bpf precision)
    nil))




(defun corrige-col (array vals)
  (let* ((list (copy-list (list! vals)))
         (defval (car (last list))))
    (loop for i from 0 to (- (numrows array) 1)
          collect (or (pop list) defval))))


(defun array-transposition (list)
  (loop for i from 0 to (1- (length (car list))) collect
        (loop for j from 0 to (1- (length list)) collect
              (nth i (nth j list)))))

(defmethod add-array-col ((self internal-array) pos val)
  (let ((revdata (array-transposition (data self))))
    (setf (data self)
          (array-transposition 
           (append (first-n revdata pos)
                   (list val) ;;; (corrige-col self val))
                   (nthcdr pos revdata))))
    (setf (numcols self) (1+ (numcols self)))))

(defmethod set-array-col ((self internal-array) pos val)
  (let ((revdata (array-transposition (data self))))
    (setf (nth pos revdata) val)  ;;; (corrige-col self val)
    (setf (data self) (array-transposition revdata))))

(defmethod remove-array-col ((self internal-array) pos)
  (let ((revdata (array-transposition (data self))))    

    (setf (data self)
          (array-transposition (append (first-n revdata pos)
                                       (nthcdr (1+ pos) revdata))))
    (setf (numcols self) (1- (numcols self)))))



;==========================================================
(defclass* class-array (internal-array) 
   ((attached-components :accessor attached-components :initform nil))
   (:icon 264)
   (:documentation 
"CLASS-ARRAY is a special 2D matrix. 

Instances are created by specifying a number of columns or components (input 1 = <numcols>) and specifying values for the different lines represented by the possible other inputs (input 2 and higher).

Initially, a CLASS-ARRAY box has no lines. There exist two complementary ways to add lines to the array :

1) Using the \"add keyword inputs\" command ('k').
Each line appears as a standard \"keyword\" input. It should be given a name (click on the input, always begin with ':', e.g. :param1) and a value (connect something).
Lines can be removed with \"remove keyword input\" command ('K').

2) [object oriented programming] Creating a subclass of class array with additional slots.
Make the slot lines visible using the \"add optional inputs\" commands (alt + '->' or '>' keys).
Slots are represented with red inputs. The input name can be changed to make it correspond to one of the class additional slots. 
Slot lines can be removed with \"remove optional input\" commands (alt + '<-' or '<' keys).

Each line of a class array can be parameterized using one of the following options :
  - Connect a LIST : the values of the list are set to the different array components. If the number of components is higher than the length of the list, then the list is repeated circularly.
  - Connect a CONSTANT VALUE : this value is assigned to every array components.
  - Connect a BPF : the BPF is sampled according to the number of array components.
  - Connect a FUNCTION (function name or box in 'lambda' mode) : the function (should be of one single argument) is evaluated and applied to the successive array component indices.

The matrix \"components\" can be accessed and modified using the functions get-comp and set-comp.

"))


;;;; WARNING : CLASS_ARRAY HAS A SPECIAL CONSTRUCTOR

(defmethod om-make-array (type numcols &rest args-and-key-list)
   (let* ((array (make-instance type))
          (slots (get-slot-in-out-names array)))
     (setf array (cons-array array (append (list nil numcols) (first-n args-and-key-list (- (length slots) 2)))
                             (nthcdr (- (length slots) 2) args-and-key-list)))
     (set-data array)
     array))



; les slots qui sont toujours visibles
(defmethod fixed-slots-list ((self class-array)) '(numcols))

(defmethod play-obj? ((self class-array)) nil)
(defmethod allowed-in-maq-p ((self class-array)) t)

(defmethod omNG-copy ((self class-array))
  (let* ((theclass (class-name (class-of self)))
            (slots (mapcan #'(lambda (slot)
                               (list (string2initarg (string (first slot))) 
                                     (omNG-copy (slot-value self (first slot))))) 
                           (get-initargs-of-class theclass))))
     `(let ((copy (make-instance ',theclass ,.slots)))
        (setf (numcols copy) ,(numcols self))
        (setf (Lcontrols copy) ,(omng-copy (Lcontrols self)))
        (setf (data copy) ,(omng-copy (data self)))
        copy)))


(defmethod omNG-save ((self class-array) &optional (values? nil)) 
    (let* ((theclass (class-name (class-of self)))
           (nc (numcols self))
           (i -1)
           (slots (mapcan #'(lambda (slot)
                              (list `(setf (slot-value array ',(first slot))
                                           ,(save-slot-value self (slot-value self (first slot)) nc))))
                          (get-init-slots-of-class theclass))))
      `(if (find-class ',theclass nil)
         (let ((array (make-instance ',theclass :numcols ,(numcols self))))
           ,.slots
           (setf (Lcontrols array)
                    (list ,.(loop for con in (Lcontrols self)
                                  collect (save-array-control self con nc))))
           (setf (data array) ,(omng-save (data self)))
           array))))

(defmethod save-array-control (array control numcol)
  `(list ,(omng-save (car control)) ,(save-slot-value array (second control) numcol)))

(defmethod save-slot-value ((self class-array) (val t) num)
  (declare (ignore num))
  (omNG-save val))

(defmethod save-slot-value ((self class-array) (val function) num)
  (omNG-save (get-array-data self val num)))

(defmethod get-array-ranges ((self class-array))
   (if (= 1 (numcols self)) (list -1 1)  (list -1 (numcols self))))


;;; quand un keyword doit faire quelque chose de special...
(defmethod array-special-keyword ((self class-array) key)
   (declare (ignore key))
   nil)

(defmethod array-special-keyword-action ((self class-array) key val)
   (declare (ignore key val))
   nil)

;(defmethod cons-array ((self cs-evt) args argkeys)
;  (let ((rep (call-next-method)))
;    (unless (numcols rep) 
;      (print (slot-value rep 'e-dels))
;      (setf (numcols rep) (list-max (mapcar #'(lambda (slot) 
;                                               (length (list! (slot-value self (internp (name slot) (slot-package slot))))))
;                                            (get-all-initargs-of-class (type-of rep))))))
;    rep))

(defmethod array-size-from-input ((self class-array) input) (length input))

(defmethod cons-array ((self class-array) args argkeys)
  (let* ((nc (if (listp (second args)) 
                (array-size-from-input self (second args))
               (second args)))
         (rep (make-instance (type-of self) :numcols nc))
         (initargs (get-all-initargs-of-class (type-of rep)))
         slot?)
    (loop for slot in (cdr (fixed-slots-list rep))
          for i = 2 then (+ i 1) do
          (setf (slot-value rep slot) (nth i args)))
    (loop while argkeys do
          (cond
           ((setf slot? (car (member (string (car argkeys)) initargs :test 'string-equal :key 'name)))
            (pop argkeys)
            (setf (slot-value rep (internp (name slot?) (slot-package slot?))) (pop argkeys)))
           ((array-special-keyword rep (car argkeys))
            (array-special-keyword-action rep (pop argkeys) (pop argkeys)))
           (t
            (pushr (list (pop argkeys) (pop argkeys)) (Lcontrols rep)))))
    rep))

(defmethod find-array-field ((array class-array) field)
  (if (find (string field) (get-all-initargs-of-class (type-of array)) :key 'name :test 'string-equal)
      (get-array-row array (intern (string field)))
    (cadr (find field (lcontrols array) :key 'car))))

;;==========================================================
;; BOX
;;==========================================================

;;; numcols n'est pas un slot OM, il faut le faire sortir explicitement


(defmethod get-type-of-ed-box ((self class-array))  'arrayBox)
(defmethod Class-has-editor-p ((self class-array)) t)
(defmethod get-editor-class ((self class-array)) 'ArrayEditor)

(defclass arrayBox (OMBoxEditCall) ())

;;; config initiale
(defmethod get-slot-in-out-names ((self class-array))
   (values '("self" "numcols") '(nil 1)
           '("object" "number of components")
           '(nil nil )))

(defmethod numouts ((self arrayBox)) (length (inputs self)))

(defmethod get-index-rep ((self input-funbox) n) n)

(defmethod numout2label-num ((self arrayBox) num) 
   (get-index-rep (nth num (inputs self)) num))

;;; valeurs pour output <num>
(defmethod rep-editor ((self class-array) num)
  (if (<= num (length (fixed-slots-list self)))
      (if (= num 0) self
        (funcall (nth (- num 1) (fixed-slots-list self)) self))
    (get-array-row self (- num (+ 1 (length (fixed-slots-list self)))))))

(defmethod call-gen-code ((self arraybox) numout)
   `(rep-editor ,(gen-code-call self) ,(numout2label-num self numout)))

(defmethod gen-code-call ((self arrayBox))
  (matrix-gen-code self (value self)))
  
(defmethod matrix-gen-code ((self arrayBox) (val class-array))
  (let ((val (if (connected? (first (inputs self)))
                 `(objFromObjs ,(gen-code (first (connected? (first (inputs self)))) 
                                          (second (connected? (first (inputs self))))) ,val)
               (let ((params (decode self))
                     (fixinputs (length (fixed-slots-list val))))
                  `(funcall 'cons-array ,val 
                           (list nil ,.(cdr (first-n params (+ fixinputs 1))))
                           (list ,.(nthcdr (+ fixinputs 1) params)))))))
    `(let ((array ,val))
       (set-data array)
       array)
    ))


(defmethod get-special-inputs ((self arrayBox)) 
  (append (find-class-boxes (inputs self) 'editor-keyword)
          (find-class-boxes (inputs self) 'control-keyword)))

;;;=====================================
;;; special inputs for class-array subclass' slots
(defclass editor-keyword (input-keyword) 
  ((box-ref :initform nil :initarg :box-ref :accessor box-ref)))

(defmethod get-keywords-from-box ((self arrayBox))
   (loop for item in (get-all-initargs-of-class (type-of (value self))) 
         collect (string2initarg (string (name item)))))

(defmethod get-icon-input ((self editor-keyword)) 266)

(defmethod get-index-rep ((self editor-keyword) n)
   (+ (length (fixed-slots-list (value (box-ref self)))) 1 
      (label2index (value (box-ref self)) (string (value self)))))

(defmethod do-add-one-input ((self arrayBox))
   (let* ((keywordlist (get-keywords-from-box self))
          (normalinputs (find-class-boxes  (inputs self) 'input-funbox))
          (controlinputs (find-class-boxes  (inputs self) 'control-keyword))
          (paraminputs (find-class-boxes  (inputs self) 'editor-keyword))
          (usedkeywords (loop for item in paraminputs collect (value item))))
     (if (< (length usedkeywords) (length keywordlist)) 
         (let* ((inputval (car (reverse (set-difference keywordlist usedkeywords :test 'equal))))
                (theslot (find (symbol-name inputval) (get-all-slots-of-class (class-name (reference self))) :test 'string-equal :key 'name)))
           (setf (inputs self) (list+ normalinputs
                                      (list+ paraminputs
                                             (list (make-instance 'editor-keyword
                                                                  :name (string-downcase inputval)
                                                                  :box-ref self
                                                                  :value inputval 
                                                                  :def-value (when theslot (valued-val (theinitform theslot)))
                                                                  :doc-string (if theslot (doc theslot) "") 
                                                                  )))
                                      controlinputs)))
       (om-beep-msg "All slots are already visible !"))))



(defmethod do-add-all-inputs ((self arrayBox)) 
  (let* ((keywordlist (get-keywords-from-box self))
         (normalinputs (find-class-boxes  (inputs self) 'input-funbox))
         (controlinputs (find-class-boxes  (inputs self) 'control-keyword))
         (paraminputs (find-class-boxes  (inputs self) 'editor-keyword))
         (usedkeywords (loop for item in paraminputs collect (value item))))
     (if (< (length usedkeywords) (length keywordlist)) 
         (let ((inputvals (reverse (set-difference keywordlist usedkeywords :test 'equal))))
           (setf (inputs self) 
                 (append normalinputs
                         paraminputs
                         (loop for inv in inputvals collect
                               (make-instance 'editor-keyword
                                              :name (string-downcase inv)
                                              :box-ref self
                                              :value inv 
                                              :doc-string ""))
                         controlinputs)))
       (om-beep-msg "All slots are already visible !"))))


(defmethod do-delete-one-input ((self arrayBox))
  (let* ((usedkeywords (find-class-boxes (inputs self) 'editor-keyword)))
    (if (> (length usedkeywords) 0)
        (setf (inputs self) (remove (car (last usedkeywords)) (inputs self) :test 'equal))
      (om-beep-msg "No more erasable input."))))

(defmethod eval-not-connected ((self editor-keyword))
   (let ((slot (find-if #'(lambda (x) (string-equal (name x) (string (value self)))) 
                        (get-all-initargs-of-class (type-of (value (box-ref self)))))))
     (if slot (initformfromslot slot) 
         (om-beep-msg (format nil "~D is not a slot of the instance." (string (value self)))))))
   
(defmethod code-not-connected ((self editor-keyword))
   (let ((slot (find-if #'(lambda (x) (string-equal (name x) (string (value self)))) 
                        (get-all-initargs-of-class (type-of (value (box-ref self)))))))
     (if slot (initformfromslot slot) 
         (om-beep-msg (format nil "~D is not a slot of the instance." (string (value self)))))))

;;;=====================================
;;; special input for control rows
(defclass control-keyword (input-keyword) 
  ((box-ref :initform nil :initarg :box-ref :accessor box-ref)))

(defmethod keyword-menu ((self control-keyword)) nil)

(defmethod special-keyword-index (value object) nil)

;;; donne l'indice reel en fonction des slots visibles
(defmethod get-index-rep ((self control-keyword) n)
  (or (special-keyword-index (value self) (value (box-ref self)))
      (+ (length (fixed-slots-list (value (box-ref self)))) 1
         (label2index (value (box-ref self)) (string (value self))))))

(defmethod do-add-one-keyword ((self arrayBox) &optional (input-key nil))
   (let* ((usedkeywords (loop for item in (find-class-boxes (inputs self) 'control-keyword)
                              collect (value item)))
          (newval "K0"))
     (loop for i = 1 then (+ i 1)
           while (member newval usedkeywords :test 'string-equal :key 'string) do
           (setf newval (format nil "K~D" i)))
     (setf (inputs self) (list+ (inputs self)
                                (list (make-instance 'control-keyword
                                        :name newval
                                        :box-ref self
                                        :value (string2initarg newval) 
                                        :doc-string ""
                                        ))))
     (pushr (list (string2initarg newval) nil) (Lcontrols (value self)))
     (update-params-for-slots self (value self))
     (when (editorFrame self)
       (update-editor-after-eval (editorFrame self) (value self)))
     t))

(defmethod do-delete-one-keyword ((self arrayBox))
   (let* ((usedkeywords (find-class-boxes (inputs self) 'control-keyword)))
     (when usedkeywords
       (setf (inputs self) (remove (car (last (inputs self))) (inputs self) :test 'equal))
       (setf (Lcontrols (value self)) (butlast (Lcontrols (value self))))
       (update-params-for-slots self (value self))
       (when (editorFrame self)
         (update-editor-after-eval (editorFrame self) (value self)))
       t)))


(defmethod update-box-keywords ((self arrayBox) &optional value)
  (let ((array (or value (value self))))
     (setf (inputs self) (list+ (remove 'control-keyword (inputs self) :test 'equal :key 'type-of)
                                (loop for ctrl-line in (LControls array) collect
                                      (make-instance 'control-keyword
                                        :name (string (car ctrl-line))
                                        :box-ref self
                                        :value (car ctrl-line) 
                                        :doc-string ""
                                        ))))
     (when (editorFrame self)
       (update-editor-after-eval (editorFrame self) array))
     t))


(defmethod set-new-keyword ((self control-keyword) (keyword string))
  (set-new-keyword self (string2initarg keyword)))

(defmethod set-new-keyword ((self control-keyword) keyword)
  (let ((keywords (get-keywords-from-box (box-ref self)))
        (usedkeywords (find-class-boxes (inputs (box-ref self)) 'control-keyword))
        (err nil) (pos nil))
    (setf err (member (symbol-name keyword) keywords :test 'equal :key 'symbol-name))
    (if err (om-beep-msg (format () "~D is not a good keyword. Allowed keywords are: ~S"  keyword keywords))
      (progn
        (setf pos (position keyword (inputs (box-ref self)) :test 'equal :key 'value))
        (setf err (and pos (not (= pos (position self (inputs (box-ref self)))))))
        (if err (om-beep-msg (format () "~D is already used by the ~D control input." keyword (incf pos))))))
    (unless err
      (setf (value self) keyword)
      (setf (name self) (string-downcase keyword))
      )
    ))


;;;=====================================
;;; Box VAlue
(defmethod omNG-box-value ((self arrayBox) &optional (numout 0))
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (format nil "Error while evaluating the ~D factory ~%~D" (class-name (reference self)) 
                                                       (om-report-condition c))
                                               :size (om-make-point 300 200))
                               (clear-after-error self)
                               (abort)))))
     (let ((repclass (class-name (reference self))))
       (cond
        ((equal (allow-lock self) "l") (special-lambda-value self repclass))
        ((equal (allow-lock self) "o") (reference self))
        ((and (equal (allow-lock self) "x") (value self))
         (rep-editor (value self) (numout2label-num self numout)))
        ((and (equal (allow-lock self) "&") (ev-once-p self)) (rep-editor (value self) (numout2label-num self numout)))
        (t (let* ((args (loop for input in (inputs self)
                              when (not (keyword-input-p input)) collect (omNG-box-value input)))
                  (argkeys (eval-keywords self))
                  rep)
             (setf rep (if (connected? (first (inputs self)))
                           (let ((newarray (objFromObjs (first args) (value self))))
                             (when (and newarray (update-box-keywords self newarray))
                               (box-draw-connections (car (frames self)) nil)
                               (omG-select (redraw-frame (car (frames self))))
                               (update-params-for-slots self newarray)
                               )
                             newarray)
                         (let ((newarray (cons-array (value self) args argkeys)))
                           (set-data newarray)
                           newarray)))
             (if (null rep)
               (progn
                 (om-beep-msg (string+ "Error: cannot construct a " (string repclass) " with these parameters"))
                 (abort))
               (progn
                 (setf (value self) rep)
                 ;; (set-data (value self)) ;; au dessus , pas si 1st inlet
                 (when (showpict self)
                   (update-miniview (iconview (car (frames self))) (value self)))
                 (when (editorFrame self)
                   (update-editor-after-eval (editorFrame self) rep))
                 (when (equal (allow-lock self) "&")
                   (setf (ev-once-p self) t))                                 
                 (rep-editor (value self) (numout2label-num self numout))))))))))


;;=========================================================
;; EDITION PARAMS
;;=========================================================

(defun get-visibles-rows (list)
    (loop for item in list
               for i = 0 then (+ i 1)
               when (second item) collect i))
        
(defmethod init-panel-list ((self class-array))
  (list (cons "All open" (loop for i from 0  to (- (numrows self) 1)
                               collect (list i t)))     
        (cons "All close" (loop for i from 0  to (- (numrows self) 1)
                                                 collect (list i nil)))))

(defun update-params-for-slots (self obj)
  (let ((plist (get-edit-param self 'panel-list)))
    (set-edit-param self 'panel-list (append (butlast plist 2)
                                             (init-panel-list obj)))
    ))

(defmethod default-edition-params ((self class-array)) 
  (pairlis '(panel-list cur-group-ind show-opt-fields color-list winsize winpos) 
           (list (init-panel-list self)
                 0 t
                 (make-list (numrows self) :initial-element *om-black-color*)
                 (or (get-win-ed-size self) (om-make-point 500 500))
                 (or (get-win-ed-pos self) (om-make-point 50 300)))))

(defmethod corrige-edition-params ((self class-array) params) 
  (if (consp params) params
    (default-edition-params self)))

;;============================
;; MINIVIEW
;;============================

(defmethod draw-mini-view ((self t) (value class-array))
   (draw-obj-in-rect value 0 (w self)  0 (h self) (view-get-ed-params self) self))

(defmethod get-draw-visibles-list ((self class-array) edparams)
  (when (get-param edparams 'cur-group-ind)
    (let ((fields (loop for item in (cdr (nth (get-param edparams 'cur-group-ind)
                                              (get-param edparams 'panel-list)))
                        when (second item) collect (car item))))
   ;(when (get-param edparams 'show-opt-fields)
   ;  (setf fields (append fields (arithm-ser (num-array-slots self) (+ (num-array-slots self) (num-array-controls self) -1) 1))))
      fields)))
  


(defmethod draw-obj-in-rect ((self class-array) x x1 y y1 edparams view)
  (let* ((visibles (get-draw-visibles-list self edparams))
         (numvis (length visibles)))
    (unless (or (<= numvis 0) (<= (numcols self) 0)) 
      (let ((deltay (max 8 (floor (- y1 y) numvis)))
            (width (- x1 x))
            (jumpx 1))
        (loop while (< (/ width (/ (numcols self) jumpx)) 12) do (incf jumpx))
        (let* ((nbobjsinx (max 1 (round (numcols self) jumpx)))
              (deltax (ceiling width nbobjsinx)))
          (om-with-focused-view view
          (loop for i in visibles
                for posy = y then (+ posy deltay) do
                (let* ((row (get-array-row self i))                     
                       (bpf? (get-row-bpf self row))
                       (color (nth i (get-param  edparams 'color-list))))
                  (if bpf?
                      (progn
                        (setf (bpfcolor bpf?) color)
                        (draw-obj-in-rect bpf? x x1  posy (+ posy deltay) nil view))
                    (om-with-fg-color view (or color *om-black-color*)
                      (if (consp row)
                          (loop for di = 0 then (+ di 1)
                                for posx = (+ x (* di deltax))
                                while (< posx x1)  do
                                (om-with-clip-rect view (om-make-rect (min posx x) (min posy y) 
                                                                      (min (+ posx deltax) x1) (min (+ posy deltay) y1))
                                  (draw-obj-in-rect (nth (* di jumpx) row) posx (+ posx deltax) posy (+ posy deltay) 
                                                    (default-edition-params (nth (* di jumpx) row)) view)
                                  )
                                (om-draw-line posx posy posx (+ posy deltay)))
                        ;(draw-obj-in-rect row x x1 posy (+ posy deltay) (default-edition-params row) view)
                        )))
                (om-draw-line x (+ posy deltay) x1  (+ posy deltay))
                ))))))))


(defmethod update-miniview ((self t) (type class-array)) (om-invalidate-view self))


;==========================================================


(defmethod objfromobjs ((self class-array) (type class-array))
  (let* ((rep (make-instance (type-of type) :numcols (numcols self)))
         (slots (loop for s in (get-all-initargs-of-class (type-of type))
                      append (let* ((s2 (find (name s) (get-all-initargs-of-class (type-of self)) :key 'name :test 'string-equal)))
                               (list (interne (name s))
                                     (if s2 
                                         (slot-value self (interne (name s)))
                                       (slot-value rep (interne (name s)))))))))
    (setf rep (cons-array rep 
                          (cons nil 
                                (mapcar #'(lambda (slotname) (slot-value self (interne slotname)))
                                        (cdr (get-slot-in-out-names self))))      
                          slots))
    
    (setf (Lcontrols rep) (clone (Lcontrols self)))
    (if (equal (type-of self) (type-of type))
        (setf (data rep) (clone (data self)))
      (set-data rep))
    rep))



;==========================================================
(defclass component () 
   ((comp-array :initform nil :initarg :comp-array :accessor comp-array)
    (val-list :initform nil :initarg :val-list :accessor val-list)
    (index :initform 0 :initarg :index :accessor index)))

;-----------Tools--------------

(defmethod omng-copy ((self component))
   `(let ((newcomp (make-instance ',(type-of self)
                     :comp-array ,(comp-array self)
                     :val-list ,(omng-copy (val-list self))
                     :index ,(index self))))
      newcomp))

(defmethod label2index ((self component) (LineId string))
   (label2index (comp-array self) lineId))

;-----------Interface-----------

;;; old one
;(defmethod* new-comp ((self class-array) vals &optional position)
;     :doc "Creates a new component in <self> filled with <vals>.
;If <pos> is not specified, the component is added at the end of the array."
;     (let* ((pos (or position (numcols self)))
;          (rep (make-instance 'component
;                              :comp-array self
;                              :val-list vals
;                              :index pos)))
;     (add-array-col (comp-array rep) pos vals)
;     rep))


(defmethod* new-comp (vals)
   :initvals '(nil)
   :indoc '("component values")
   :doc "Creates a new component filled with <vals>."
   :icon 323
   (make-instance 'component :val-list vals))

(defmethod* add-comp ((self class-array) (comp component) &optional position)
   :initvals '(nil nil)
   :indoc '("a class-array instance"  "a component instance" "position in the array")
   :doc "Adds <comp> in <self> at <pos>.
If <pos> is not specified, the component is added at the end of the array."
   :icon 323
   (let* ((pos (or position (numcols self))))
     (setf (comp-array comp) self)
     (setf (index comp) pos)
     (add-array-col self pos (val-list comp))
     (loop for cmp in (attached-components self) do
           (when (>= (index cmp) pos)
             (setf (index cmp) (1+ (index cmp)))))
     (push comp (attached-components self))
     comp))

(defmethod* remove-comp ((self component)) 
   :initvals '(nil)
   :indoc '("a class-array component")
   :doc 
"Remove component <self> from its associated array.

The modification is immediately applied in the original array."
   :icon 323
   (remove-array-col (comp-array self) (index self))
   (remove (index self) (attached-components (comp-array self)) :key 'index :test '=)
   ;;; !!! comp-array is still "self" for other removed components of same index
   (loop for cmp in (attached-components (comp-array self)) do
         (when (> (index cmp) (index self))
           (setf (index cmp) (1- (index cmp)))))        
   (setf (comp-array self) nil)
   self)

(defmethod* get-comp ((self class-array) (n integer))
   :initvals '(nil 0)
   :indoc '("a class-array instance"  "component number")
   :doc "Returns the <n>th component in <self> (a class-array object).

Components are returned as instances of the class 'component' and can be accessed using the functions comp-field, comp-list, fill-comp."
   :icon 323
   (when (< n (numcols self))
     (let ((comp (make-instance 'component
                                :comp-array self
                                :val-list (get-array-col self n)
                                :index n)))
       (push comp (attached-components self))
       comp)))

(defmethod* comp-list ((self component) &optional val-list) 
   :initvals '(nil)
   :indoc '("a class-array component")
   :doc "Sets (if <val-list> is supplied) or returns (if not) the values in <self> (a class-array component).

If <val-list>, the component itself is returned.
The modifications are immediately stored in the original array."
   :icon 323
    (if val-list
        (progn 
          (setf (val-list self) val-list)
          (when (comp-array self)
            (set-array-col (comp-array self) (index self) (val-list self)))
          self)
      (val-list self)))

(defmethod* comp-field ((self component) (LineId integer) &optional val) 
   :initvals '(nil 0 nil)
   :indoc '("a class-array component" "a line identifier" "a value")
   :doc 
"Sets (if <val> is supplied) or returns (if not) the value of line <lineid> for component <self>.

If <val>, the component itself is returned.
The modifications are immediately stored in the original array.
<lineid> can be a number (index of the line) or a string (name of the line)."
   :icon 323
   (if val 
     (progn 
       (setf (nth LineId (val-list self)) val)
       (when (comp-array self)
         (set-array-col (comp-array self) (index self) (val-list self)))
       self)
     (nth LineId (val-list self))))

(defmethod* comp-field ((self component) (LineId string) &optional val)
   (comp-field self (label2index self LineId) val))



