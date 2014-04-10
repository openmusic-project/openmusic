;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;This file implements the meta-object OMboxCall and its sub-classes.
;They are boxes in a Patch.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;---------------------------------------
;Boxes in a Patch
;---------------------------------------
(defclass OMBoxcall (OMBox) 
   ((allow-lock :initform nil  :accessor allow-lock)
    (doc :initform "no documentation" :accessor doc)
    (mycontainer :initform nil  :accessor mycontainer)
    (value :initform nil :accessor value)
    (ev-once-p :initform nil :accessor ev-once-p)
    (show-name :initform nil :accessor show-name)
    (score-action :initform nil :accessor score-action))
   (:documentation "This is the class for boxes in a Patch or in a maquette.
All boxes which their reference is a OM generic function are instances of this class.#enddoc#
#seealso# (OMBox OMBoxRelatedWClass OMBoxlispCall OMBoxTypeCall OMBoxInstance OMBoxPatch OMBoxMaquette
TemporalBox OMIn OMout OMLoop-box) #seealso#
#allow-lock# If NIL the box does not allow a lock button else this slot store the value of the the button
(i.e. '&' ev-once, 'x' lock, 'l' lambda, 'o' for reference mode.#allow-lock#
#doc# Some boxes are a doc differente from the reference's doc.#doc#
#mycontainer# The patch containing the box.#mycontainer#
#value# Used when the box store its value.#value#
#ev-once-p# T if the box was already valued in a patch's evaluation, the clear-ev-once method set this slot to NIL
for all boxes in the patch after an evaluation.#ev-once-p#")
   (:metaclass omstandardclass))

;--------------------------------------------------
;Method redefinition of OMBox
;--------------------------------------------------
(defmethod numouts ((self OMBoxcall))
   "Return the output's number of the OM Generic function reference of 'self'." 
   (numouts (fdefinition (reference self))))

(defmethod get-output-text ((self OMBoxCall) i) 
  (let ((str (format nil "out~D" i)))
    (when (and (reference self) (symbolp (reference self))
               (omgenfun-p (fdefinition (reference self)))
               (nth i (outputs-doc (fdefinition (reference self)))))
      (setf str (string+ str ": " (nth i (outputs-doc (fdefinition (reference self)))))))
    str))

(defmethod get-object-insp-name ((self OMBoxcall)) "Generic Function")

(defmethod get-documentation ((self OMBoxcall)) 
   "The same doc as the generic function reference of 'self'."
   (string-until-CR  (documentation (reference self) 'function)))

;;; test
(defmethod set-doc ((self OMBoxCall) newdoc) (setf (doc self) newdoc))
(defmethod get-box-doc ((self OMBoxCall)) (doc self))

(defmethod OpenEditorframe ((self OMBoxcall)) 
   "Open the editor of the generic function reference of 'self'."
   (OpenObjectEditor (fdefinition (reference self))) nil)

;-------------------Protocole------------------------------
(defgeneric allow-lock-button (box)  
   (:documentation "T if the box 'self' allow a lock button."))

(defgeneric gen-code (box numout)  
   (:documentation "Used to generate Lisp code from a box call."))

(defgeneric* omng-box-value (box &optional numout)
   (:icon 400 :indocs '("A box" "numout") 
          :documentation "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."))

;---------

(defmethod allow-lock-button ((self OMBoxcall))
   "T if the box allow a lock button. Output boxes for exemple do not allow a lock button."  t)

;; avant dans OMBoxInstance
(defmethod change-name ((self OMBoxCall))
   (let ((new-name (name (reference self))))
     (setf (name self) new-name)
     (setf (frame-name self) new-name)
     (when (frames self)
       (setf (name (car (frames self))) new-name)
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self))))))

;--------------------------------------------------
;Other Methods
;--------------------------------------------------

(defmethod get-conect-to-me ((self OMBoxcall))
   "Get all boxes connected to one or more outputs of 'self'."
   (let ((patch (mycontainer self)))
     (unless patch
       (when (car (frames self)) 
         (setf patch (setf (mycontainer self) (object (om-view-container (car (frames self))))))))
     (when patch
       (loop for box in (get-elements patch)
             when (is-connected? self box)
             collect box))))

(defmethod recursive-connection? ((target OMBoxcall) (source OMBoxcall))
   "Check if there is a cyclic connection when you connect the box 'source' to the box 'target'."
   (let (rep)
     (loop for item in (get-conect-to-me target)
           while (not rep) do
           (if (equal item source) (setf rep t)
               (setf rep (recursive-connection? item source))))
     rep))

(defmethod clear-ev-once ((self OMBoxcall))
   "Reset the ev-once flag in 'self'. Used after one evaluation."
   (when (equal (allow-lock self) "&") 
     (setf (ev-once-p self) nil)
     (setf (value self) nil)
     ))


;The graphics connections are in ...SimpleFrames;boxesconnections.lisp

(defmethod omNG-connect ((source OMBoxcall) numout  (target OMBoxcall) numin lines &optional (col 0))
   (when (and (< numout (numouts source)) (nth numin (inputs target)))
     (connect-ctrl source (nth numin (inputs target)) numout)
     (when lines
       (setf (nth 2 (connected? (nth numin (inputs target)))) lines))
     (setf (nth 3 (connected? (nth numin (inputs target))))
           (if (null col) 0 col))))

(defmethod connect-ctrl ((self OMBoxcall) input numout)
  (setf (connected? input) (list self numout nil 0)))

;--------------------------------------------------
;Tools
;--------------------------------------------------

(defmethod is-boxpatch-p ((self OMBoxcall)) t)
(defmethod is-boxpatch-p ((self t)) nil)

;Used for graphic meta-programmation
(defmethod get-boxcallclass-fun ((self t)) *def-metaclass-box-fun*)

;-----Builder 


(defmethod omNG-make-new-boxcall ((genfun OMGenericFunction) posi name)
   (let* ((funname (function-name genfun))
          (initvals (inputs-default genfun))
          (docinps (inputs-doc genfun))
          (numins (min-inp-number funname))
          (i -1)
          (menulist (inputs-menus genfun))
          (inputs (mapcar #'(lambda (name value doc)
                              (let ((menuin? (get-menu-input (incf i) menulist)))
                                (if menuin?
                                  (make-instance 'input-funmenu
                                    :name (string name)
                                    :value (valued-val value)
                                    :doc-string doc
                                    :thepopup (second menuin?))
                                  (make-instance 'input-funbox
                                    :name (string name)
                                    :value (valued-val value)
                                    :doc-string doc)))) 
                          (subseq (arglist funname) 0 numins) initvals docinps)) rep)
     (setf rep (make-instance (get-boxcallclass-fun funname)
                 :name name
                 :reference funname 
                 :icon (icon genfun)
                 :inputs inputs))
     (set-box-to-inputs inputs rep)
     (setf (frame-position rep) (borne-position posi))
     rep))




;remake the connections from one list of sources and targets
;used to save patches and copy and paste a set of connected boxes
(defun copy-connections (listsource listtarget)
  (loop for i in listsource
        for j in listtarget do
        (let ((inputssource (inputs i))
              (inputstarget (inputs j)))
          (mapcar #'(lambda (insou intarg)
                      (when (connected? insou)
                        (let ((posi (position (first (connected? insou)) listsource :test 'equal)))
                          (when posi
                            (setf (connected? intarg)
                                  (list (nth posi listtarget)
                                        (second (connected? insou))
                                        (third (connected? insou))
                                        (if (null (fourth (connected? insou))) 0 (fourth (connected? insou))))))))
                      (setf (value intarg) (value insou))) inputssource  inputstarget))))





          
;-------------Lisp code generation
(defmethod decode ((self OMBoxcall))
   "Generation of lisp code for inputs of the box 'self'."
   (list+ (loop for input in (inputs self)
                when (not (keyword-input-p input)) collect (gen-code input 0))
          (gen-code-keywords self)))

;generate code for const
(defmethod gen-code ((self t) numout)
  (declare (ignore numout))
  (if *compiling-macro-boite* self (omNG-copy self)))


;generate code for list
(defmethod gen-code ((self list) numout) 
   (declare (ignore numout))
   (if *compiling-macro-boite* (omNG-save self) `',self))

(defmethod gen-code ((self null) numout) 
   (declare (ignore numout)) nil)

(defun gen-box-string (box)
  (string+ (substitute #\- #\Space (name box))
           (format () "~D-~D" (om-point-h (frame-position box)) (om-point-v (frame-position box)))))

;(defmethod gen-code-for-ev-once ((self OMBoxcall) numout)
;   (let ((varname (read-from-string (gen-box-string self))))
;     (if (not (member varname *let-list* :test 'equal)) 
;         (progn
;         (push varname  *let-list*)
;         (if *start-repeat-generation*
;           (progn
;             (push `(setf ,varname (multiple-value-list ,(gen-code-call self))) *repeat-ev-once-list*)
;             `(nth ,numout ,varname))       
;           `(progn
;              (setf ,varname (multiple-value-list ,(gen-code-call self)))
;              (nth ,numout ,varname))))
;       `(nth ,numout ,varname))))

(defmethod gen-code-for-ev-once ((self OMBoxcall) numout)
   (let ((varname (read-from-string (gen-box-string self))))
     (when (not (member varname *let-list* :test 'equal :key 'car))
       (push `(,varname (multiple-value-list ,(gen-code-call self)))  *let-list*))
     `(nth ,numout ,varname)))


(defmethod gen-code ((self OMBoxcall) numout)
   "Generate Lisp code for the box 'self'."
   (cond
    ((equal (allow-lock self) "&") 
     (gen-code-for-ev-once self numout))
    ((equal (allow-lock self) "x")
     `(nth ,numout ,(gen-code (value self) 0)))
    ((equal (allow-lock self) "o") 
     `',(reference self))
    ((equal (allow-lock self) "l") 
     (curry-lambda-code self (reference self)))
    (t  (call-gen-code self numout))))

(defmethod call-gen-code ((self OMBoxcall) numout)
   (if (zerop numout)
     (gen-code-call self)
     `(nth ,numout (multiple-value-list ,(gen-code-call self)))))

(defmethod gen-code-call ((self OMBoxcall))
   `(,(reference self) ,.(decode self)))

;Sintax of keywords parameters is still different.
(defmethod gen-code-keywords ((self OMBoxcall))
   (let ((keywords (find-condition-boxes  (inputs self) 'keyword-input-p)))
     (loop for item in keywords 
           append (gen-code-keyword-in item))))


;-----------------evaluation
(defmethod eval-box-inputs ((self OMBoxcall))
    (let* ((args  (loop for input in (inputs self)
                        when (not (keyword-input-p input)) collect (omNG-box-value input)))
           (args (list+ args (eval-keywords self))))
     args))


(defmethod clear-after-error ((self OMBoxCall))
  (when (and (mycontainer self) (editorframe (mycontainer self)))
    (clear-ev-once (editorframe (mycontainer self)))))

(defmethod get-box-value-list ((self OMBoxCall) args)
  (if (equal (class-name (class-of self)) 'OMBoxcall)
      (multiple-value-list (apply (reference self) args))
    (multiple-value-list (special-value self args))))
  
(defmethod omNG-box-value ((self OMBoxcall) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                        (om-report-condition c))
                                                  :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") (special-lambda-value self (reference self)))
      ((equal (allow-lock self) "o") (fdefinition (reference self)))
      ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth numout (value self)))
      (t (let* ((args (eval-box-inputs self))
                (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
           (if (null themethod)
             (progn (dialog-message (string+ "No method is defined for these types in box " (name self)))
                    (om-abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
               (setf rep (get-box-value-list self args))
               ))
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
           ;;;; TEST
           (when (equal (allow-lock self) nil)
             (setf (value self) rep))
           ;;;;
           (nth numout rep))))))

;Sintax of keywords parameters is still different.
(defmethod eval-keywords ((self OMBoxcall))
   (let ((keywords (find-condition-boxes (inputs self) 'keyword-input-p)))
     (loop for item in keywords 
           append (eval-keyword-in item))))

(defmethod special-value ((self  OMBoxcall) &optional (args nil))
   "Method exception boxes (i.e. omand, omif, etc.) must redefine this method."
   (apply (reference self) args))

;-----------------edition

(defmethod do-add-one-input ((self OMBoxcall))
   "Add if posible an optional input to the box 'self'."
   (let* ((current-imp (length (inputs self)))
          (thefun (fdefinition (reference self)))
          (initvals (inputs-default thefun))
          (docinps (inputs-doc thefun))
          (menulist (inputs-menus thefun))
          (lambda-lis (arglist thefun))
          (names (get-only-par-names thefun)))
     (when (or (member '&rest lambda-lis) (< current-imp (get-fixed-par-num lambda-lis (reference self))))
       (let ((menuin? (get-menu-input current-imp menulist))
             (value (if (<  current-imp (length initvals)) (nth current-imp initvals) (car (last initvals))))
             (doc (if (nth current-imp docinps) (nth current-imp docinps) (car (last docinps)))))
         (setf (inputs self) (list+ (inputs self)
                                    (list (if menuin?
                                            (make-instance 'input-funmenu
                                              :name (if (nth current-imp names) (nth current-imp names) (car (last names)))
                                              :value (valued-val value)
                                              :doc-string doc
                                              :box-ref self
                                              :thepopup (second menuin?))
                                            (make-instance 'input-funbox
                                              :name (if (nth current-imp names) (nth current-imp names) (car (last names)))
                                              :value (valued-val value)
                                              :box-ref self
                                              :doc-string doc)))) )
         (do-add-one-input-extra self)))))


(defmethod do-add-all-inputs ((self OMBoxcall))
   "Add if posible an optional input to the box 'self'."
   (let* ((current-imp (length (inputs self)))
          (thefun (fdefinition (reference self)))
          (initvals (inputs-default thefun))
          (docinps (inputs-doc thefun))
          (menulist (inputs-menus thefun))
          (lambda-lis (arglist thefun))
          (names (get-only-par-names thefun))
          (max-input (get-fixed-par-num lambda-lis (reference self))))
     (when (member '&rest lambda-lis) (setf max-input (1- max-input)))
     (when (< current-imp max-input)
       (setf (inputs self)
             (append (inputs self)
                     (loop for i = current-imp then (+ i 1)
                           while (< i max-input)      
                           collect
                           (let ((menuin? (get-menu-input i menulist))
                                 (value (if (< i (length initvals)) (nth i initvals) (car (last initvals))))
                                 (doc (if (nth i docinps) (nth i docinps) (car (last docinps)))))
                             (if menuin?
                                 (make-instance 'input-funmenu
                                                :name (if (nth i names) (nth i names) (car (last names)))
                                                :value (valued-val value)
                                                :doc-string doc
                                                :box-ref self
                                                :thepopup (second menuin?))
                               (make-instance 'input-funbox
                                              :name (if (nth i names) (nth i names) (car (last names)))
                                              :value (valued-val value)
                                              :box-ref self
                                              :doc-string doc)))) )
             ))))

(defmethod do-delete-one-input ((self OMBoxcall))
   "Remove an optional input from the box 'self'."
   (let* ((current-imp (length (inputs self))))
     (min-inp-number (reference self))
     (when (> current-imp (min-inp-number (reference self)))
       (setf (inputs self) (butlast (inputs self)))
       (do-delete-one-input-extra self))))

(defmethod do-delete-one-input-extra ((self OMBoxcall))
   "remove an optional input from 'self'" t)
(defmethod do-add-one-input-extra ((self OMBoxcall))
   "Add an optional input to 'self'" t)

(defmethod get-keywords-from-box ((self OMBoxCall))
   "Return a list of the keywords of the generic function associated to 'self'."
  (get-keywords-fun (reference self)))

(defmethod do-add-one-keyword ((self OMBoxCall) &optional (input-key nil))
   "Add a keywords input to 'self', if possible."
   
   (let* ((thefun (fdefinition (reference self)))
          (keywordlist (get-keywords-fun (reference self)))
          (usedkeywords (find-class-boxes (inputs self) 'input-keyword))
          (keyname (if input-key
                       (and (or (find input-key keywordlist) (om-beep-msg (string+ "Wrong keyword name: " (string input-key))))
                            (or (not (find input-key (mapcar 'value usedkeywords))) (om-beep-msg (string+ "Keyword name already used: " (string input-key))))
                            input-key)
                     (find-if-not #'(lambda (elt) (member elt (mapcar 'value usedkeywords))) keywordlist))))
     (when keyname
     (if (< (length usedkeywords) (length keywordlist))

         (let* ((arg-list (ordered-arg-list thefun))
                (argpos (position keyname arg-list))
                (initval (nth argpos (inputs-default thefun)))
                (doc (nth argpos (inputs-doc thefun)))
                (menu (cadr (find argpos (inputs-menus thefun) :key 'car :test '=))))
           
           (setf (inputs self) (list+ (inputs self)
                                  (list (make-instance 'input-keyword
                                          :name (string-downcase keyname) ;"keyword input"
                                          :box-ref self
                                          :value keyname
                                          :def-value initval
                                          :doc-string doc
                                          :val-menu menu
                                          )))))
       
       (om-beep-msg "All keywords are already opened.")))))





(defmethod do-delete-one-keyword ((self OMBoxCall))
   "Delete a keywords input from 'self'."
   (let* ((usedkeywords (find-class-boxes (inputs self) 'input-keyword)))
     (when usedkeywords
       (setf (inputs self) (remove (car (last (inputs self))) (inputs self) :test 'equal))
       t)))

(defmethod dead-reference ((self OMBoxcall))
   "This method is called when the reference of 'self' is deleted."
   (let ((newobj (omNG-make-new-boxcall (fdefinition 'dead-method) (frame-position self) (name self))))
     (if (frames self)
       (let ((container (om-view-container (car (frames self)))))
         (when container
           (real-make-delete-before container (frames self))
           (omg-remove-element container (car (frames self)))
           (omG-add-element container (make-frame-from-callobj newobj))))
       (progn
         (setf (reference self) 'dead-method)
         (setf (inputs self) nil)
         (setf (icon self) 190)
         (change-class self 'box-dead)))))


        
;---------------------------------------------------------
;Classe with a OM Class by reference
;---------------------------------------------------------

(defclass OMBoxRelatedWClass (OMBoxcall) ()
   (:documentation "Boxes with a class as reference are instances of this class.#enddoc#
#seealso# (OMBoxcall  OMBoxEditCall OMSlotsBox) #seealso#")
   (:metaclass omstandardclass))

(defmethod initialize-instance :after ((self OMBoxRelatedWClass) &key controls)
  (declare (ignore controls))
  (get&corrige-icon (icon (reference self))))


(defmethod gen-code ((self OMBoxRelatedWClass) numout)
   (cond
    ((equal (allow-lock self) "&") 
     (gen-code-for-ev-once self numout))
    ((equal (allow-lock self) "x")
     `(rep-editor ,(value self) ,numout))
    ((equal (allow-lock self) "o") `',(reference self))
    ((equal (allow-lock self) "l") 
     (curry-lambda-code self (reference self)))
    (t  (call-gen-code self numout))))


(defmethod clear-ev-once ((self OMBoxRelatedWClass))
   "Reset the ev-once flag in 'self'. Used after one evaluation."
   (when (equal (allow-lock self) "&") 
     (setf (ev-once-p self) nil)))


(defmethod omNG-box-value ((self OMBoxRelatedWClass) &optional (numout 0))
   "Eval a factory."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                        (om-report-condition c))
                                               :size (om-make-point 300 200))
                               ;(clear-after-error self)
                               (om-abort)))))
     (let ((editorclass (class-name (reference self))))
       (cond
        ((equal (allow-lock self) "l")                        (special-lambda-value self editorclass))
        ((equal (allow-lock self) "o")                        (reference self))
        ((and (equal (allow-lock self) "x") (value self))     (rep-editor (value self) numout))
        ((and (equal (allow-lock self) "&") (ev-once-p self)) (rep-editor (value self) numout))
        (t (let* ((args (mapcar #'(lambda (input) (omNG-box-value input)) (inputs self)))
                  rep)
             (setf rep (cons-new-object (value self) args (connected? (first (inputs self)))))
             (if (null rep)
               (progn
                 (om-beep-msg (string+ "I can not construct a " (string editorclass) " with these parameters"))
                 (om-abort))
               (progn
                 (setf (value self) rep)
                 (update-if-editor self)
                 ;;; if the object has a name, the box takes the object name
                 ;;; if not, the object takes the box name...
                 (when (get-name rep)
                   (setf (name self) (get-name rep))
                   (set-name rep (name self)))

                 (when (equal (allow-lock self) "&")
                   (setf (ev-once-p self) t))
                 (rep-editor (value self) numout)))))))))


(defmethod do-add-one-input ((self OMBoxRelatedWClass)) nil)
(defmethod do-delete-one-input ((self OMBoxRelatedWClass))  nil)
(defmethod do-add-all-inputs ((self OMBoxRelatedWClass)) nil)

(defmethod do-add-one-keyword ((self OMBoxRelatedWClass) &optional (input-key nil)) nil)
(defmethod do-delete-one-keyword ((self OMBoxRelatedWClass))  nil)

(defmethod get-special-inputs ((self t)) nil)

(defmethod update-from-reference ((self OMBoxRelatedWClass) &optional (udt? t))
   (declare (ignore udt?))
   (let* ((new-inputs (append (get-inputs-from-inst (value self))
                              (get-special-inputs self))))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))
               (setf (value newin) (valued-val (value oldin))) ;;; test 
               ) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs)
     (setf (numouts self) (length (get-outs-name (value self))))
     (when (frames self)
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self))))))

(defmethod remove-extra ((self OMPatch) (box OMBoxRelatedWClass))
  (setf (attached-objs (reference box))
        (remove box (attached-objs (reference box)) :test 'equal)))

(defmethod make-outputs-from-names ((self t) value module) 
   "The outputs of these boxes depent from the initarg slots of the class reference."
   (let ((numouts (numouts self))
         (nameouts (get-outs-name value)))
     (loop for i from 0 to (- numouts 1) do
           (let ((thenewout (om-make-view (get-out-class self)
                              :position (om-make-point (- (* (+ i 1) (round (w module) (+ numouts 1))) 4) 
                                                         (- (h module) 9))
                              :size (om-make-point 8 8)
                              :help-spec (nth i nameouts)
                              :index i)))
             (push thenewout (outframes module))
             (om-add-subviews module thenewout)))))

;--------------------------------------------------
;Tools
;--------------------------------------------------

;++++++get inputs and outputs for the box

;slf est une instance
(defmethod get-slot-in-out-names ((self t))
   (get-slot-in-out-names-from-class (type-of self)))




(defmethod get-slot-in-out-names-from-class ((self t))
   (let* ((funname (internp (string+ "om-init-class-def-" (string self)) (symbol-package self)))
          (genfun (fdefinition funname))
          (initvals (inputs-default genfun))
          (docinps (inputs-doc genfun)))
     (values (mapcar 'string-downcase (arglist funname)) initvals docinps (make-list (length initvals) :initial-element nil))))


 
; self is one instance
(defmethod get-outs-name ((self t))
   (mapcar #'(lambda (name) 
               (string-downcase (string name))) (get-slot-in-out-names self)))

(defun inputs-from-list (args initvals docinps menus)
   (mapcar #'(lambda (name value doc menu?)
               (if menu?
                 (make-instance 'input-funmenu
                   :name (string name)
                   :value (valued-val value)
                   :doc-string doc
                    ;;;:thepopup (second (first (make-inputs-popups menu?)))
                     :thepopup (second (first menu?))
                     )
                 (make-instance 'input-funbox
                   :name (string name)
                   :value (valued-val value)
                   :doc-string doc)))
           args initvals docinps menus))



(defmethod get-inputs-from-class ((self t))
   (multiple-value-bind (args initvals docinps menus)
                        (get-slot-in-out-names-from-class self)
     (inputs-from-list args initvals docinps menus)))

(defmethod get-inputs-from-inst ((self t))
  (multiple-value-bind (args initvals docinps menus)
      (get-slot-in-out-names self)
    (inputs-from-list args initvals docinps menus)))

;if T, the class of self allows an editor. Subclass this method if  you want construct an editor box.
(defmethod Class-has-editor-p ((self t)) nil)

;if T, the factorie is built by the function make-exeption-box.
(defmethod exeption-class-p ((self t)) nil)

;Inputs of factories are initargs of the class reference, if you want change this redefine this method.
(defmethod make-exeption-box ((self t) posi name)
   (declare (ignore posi name)) nil)

;--------------Evaluation

(defmethod pretraitement ((self t) &rest args) args)

                 

;called by omng-box-value, sometimes is usefull redefine the builder for a class.
(defmethod cons-new-object ((self t) args objs)
  (if objs
      (objFromObjs (first args) self)
    (apply 'make-one-instance (list+ (list self) (cdr args)))))
  
(defmethod rep-editor ((self t) num)
   (let ((outs (get-outs-name self)))
     (if (= num 0)
       self
       (eval `(,(internp (nth num outs) (symbol-package (type-of self))) ,self)))))

(defmethod make-one-instance ((self t) &rest slots-vals)
   (setf slots-vals (apply 'pretraitement (cons self slots-vals)))
   (let* ((class (type-of self))
          (slot-init-list (make-slot-init-list class slots-vals)))
     (eval `(make-instance ',class ,.slot-init-list))))

(defun make-slot-init-pairs (class args)
   (let ((i 0) rep)
     (loop for slot in (get-all-slots-of-class class) do
           (when (io-p slot) 
             (push (list (string2initarg  (name slot)) `',(nth i args)) rep)
             (incf i)))
     (reverse rep)))

(defun om-set-slot-val (obj item)
  (eval `(setf (,(interne (string (car item))) ,obj) ,(second item))))

;---------------------------------------------------------
;--------------Factories -------------------
;---------------------------------------------------------

(defmethod omNG-make-new-boxcall ((self OMClass) posi name)
   (let ((defval (get-super-default-value (class-name self))))
     (cond
      ((exeption-class-p (class-name self)) (make-exeption-box (class-name self) posi name)) 
      (t
       (let* ((newbox (make-instance (get-type-of-ed-box defval)
                        :name name
                        :reference self 
                        :icon (icon self)
                        :inputs (get-inputs-from-inst defval))))
         (setf (value newbox) defval)
         (setf (frame-position newbox) (borne-position posi))
         (set-edition-params (value newbox) newbox)
         (push newbox (attached-objs self))
         (set-box-to-inputs (inputs newbox) newbox)
         (setf (numouts newbox) (length (get-outs-name (value newbox))))
         newbox)))))


(defclass OMBoxEditCall (OMBoxRelatedWClass) 
   ((numouts :initform 1 :accessor numouts)
    (player :initform nil :accessor player)
    (play-state :initform nil :accessor play-state)
    (showpict :initform nil :accessor showpict)
    (minieditor? :initform nil :accessor minieditor?)
    (view-of-patch :initform nil :accessor view-of-patch)
    (ed-pictu-list :initform nil :accessor ed-pictu-list)
    (edition-params :initform nil :accessor edition-params))
   (:documentation "This is a particular factory for classes that have an editor.#enddoc#
#seealso# (OMBoxRelatedWClass) #seealso#
#numouts# How many outputs have the box ?. #numouts#
#showpict# If T the box show a miniview visualisator of the instance. #showpict#
#minieditor?# Not yet used. #minieditor?#
#edition-params# Store some parameters for the editor. #edition-params#")
   (:metaclass omstandardclass))


(defmethod box-has-pict-editors ((self OMBox)) nil)
(defmethod box-has-pict-editors ((self OMBoxEditCall))  t)

(defmethod has-mini-pict? ((self OMBoxEditCall)) t)
(defmethod has-mini-pict? ((self t)) nil)

(defmethod get-frame-class ((self OMBoxEditCall)) 'boxEditorframe)
(defmethod get-documentation ((self OMBoxEditCall)) (string+ "This is a factory of the class " (name (reference self))))
(defmethod get-object-insp-name ((self OMBoxEditCall)) "Editor")
(defmethod get-input-class-frame ((self OMBoxEditCall)) 'input-funboxframe)


(defvar *instance-to-load* nil)


(defun import-instance (obj &optional file)
   "Set the instance value of 'self' to a new value loaded from a file." 
   (handler-bind ((error #'(lambda (e) 
                             (om-message-dialog "ERROR While loading object Data. This file might not be an OM object.")
                             (om-abort))))
     (let ((filename (or file (catch-cancel (om-choose-file-dialog :types '("OM instance" "*.omi"))))))
       (when filename
         (let ((rep
                (if (equal (file-type filename) :inst)
                    (progn
                      (eval-non-text-file filename)
                      (if (not (equal (type-of obj) (type-of (instance *instance-to-load*))))
                          (string+ "ERROR: Import this object in a " (string (type-of (instance *instance-to-load*))) " editor box only.")
                        (or (objfromobjs (instance *instance-to-load*) obj)
                            (string+ "I can not construct an object of type " (type-of obj) " from this file."))
                        ))
                  "This file is not a OM Instance")))
           (if (stringp rep)
               (progn (om-message-dialog rep) nil)
             rep))))))

(defmethod import-box-editor ((self OMBoxEditCall))
  (let ((newval (import-instance (value self))))
    (when newval
      (setf (value self) newval)
      (when (showpict self)
        (update-miniview (iconview (car (frames self))) (value self)))
      (when (editorFrame self)
        (update-editor-after-eval (editorFrame self) newval)))))


(defmethod export-box-editor ((self OMBoxEditCall))
  "Save the instance value of 'self' as a file."
  (export-value (value self) (edition-params self)))

(defmethod export-value ((self t) params)
  (save-instance self))


(defun write-header-inst (file)
  (write-line ";fileheader" file)
  (write-line (string+ ";" (format nil " ~S" (list *om-version* :inst  0 0 0  "doc" 183))) file)
  (write-line ";endfileheader" file))



;--------------INITS
(defmethod get-initval ((self t)) (get-super-default-value (type-of self)))

(defmethod get-type-of-ed-box ((self t))  *def-metaclass-box-edit*)


(defmethod make-new-EditorCall ((self OMClass) posi name)
   (let* ((defval (get-super-default-value (class-name self)))
          (newbox (make-instance (get-type-of-ed-box defval)
                    :name name
                    :reference self 
                    :icon (icon self)
                    :inputs (get-inputs-from-inst defval))))
     (setf (value newbox) defval)
     (setf (frame-position newbox) (borne-position posi))
     (set-edition-params (value newbox) newbox)
     (push newbox (attached-objs self))
     (setf (numouts newbox) (length (get-outs-name (value newbox))))
     newbox))

(defmethod get-miniview-class ((self OMBoxEditCall)) 'miniview)

(defmethod make-frame-from-callobj ((self OMBoxEditCall))
   "Make a simple frame for the editor factory 'self'."
   (let* ((name (string-downcase (name self)))
          (defsize (get-boxsize self))
          (numouts (numouts self))
          (numins (length (inputs self)))
          (index 0) input-frames boxframex
          module boxsize miniview )
     (setf boxframex (if (frame-size self)
                        (om-point-h  (frame-size self))
                        (apply #'max (list (om-point-h defsize) (* 10 numouts) (* 10 numins)))))
     (setf boxsize (if (frame-size self) (frame-size self) (om-make-point boxframex (om-point-v defsize))))
     (setf input-frames (mapcar #'(lambda (input)
                                    (progn
                                      (setf index (+ index 1))
                                    (om-make-view (get-input-class-frame self)
                                      :object input
                                      :help-spec (string+ "<" (string-downcase (name input))
                                                          "> " (doc-string input))
                                      :size (om-make-point 8 8)
                                      :position (om-make-point (- (* index  (round (om-point-h boxsize) (+ numins 1))) 4) 
                                                                 1)))) (inputs self)))
     (setq module (om-make-view (get-frame-class self) 
                                                  :position (frame-position self) 
                                                  :size  boxsize
                                                  ;;;:subviews input-frames
                                                  :object self))
     (setf (inputframes module) input-frames)
     (loop for input-f in input-frames do (om-add-subviews module input-f))
     
     (make-outputs-from-names self (value self) module)
     (setf miniview (if (minieditor? self)
                      (om-make-view (get-editor-class (value self))
                        :ref self
                        :mini-editor-p t
                        :object (value self)
                        :position (om-make-point 0 8) 
                        :size (om-subtract-points boxsize (om-make-point 0 17)))
                      (om-make-view (get-miniview-class self)
                        :position (om-make-point 0 8)
                        :font *om-default-font1*
                        :help-spec (string+ "Make an instance of the class " (string-downcase (class-name (reference self))) ".")
                        :size (om-subtract-points boxsize (om-make-point 0 17)))))
     (setf (iconview module) miniview)
     (om-add-subviews module miniview)
     (setf (frames self) (list module))
     (setf (frame-size self) (om-view-size module))
     (setf (name module) name)
     (add-box-resize module)
     (when (allow-lock self)
       (add-lock-button module (allow-lock self)))
     module))


(defmethod def-icon-size ((self omboxeditcall))
  (or (spec-obj-icon-size (value self))
      nil
      ;'(24 24)
      ))


;-------------- Code Lisp generation

(defmethod gen-code-call ((self OMBoxEditCall))
   (if (connected? (first (inputs self)))
       `(objFromObjs ,(gen-code (first (connected? (first (inputs self)))) (second (connected? (first (inputs self)))))
                     ;;;; ,(gen-code (first (inputs self)) (second (inputs self))) ;; ERREUR ??
                     ,(value self))
     `(apply 'make-one-instance (list ,(value self) ,.(cdr (decode self))))))

(defmethod call-gen-code ((self OMBoxEditCall) numout)
   `(rep-editor ,(gen-code-call self) ,numout))


(defmethod gen-code-for-ev-once ((self OMBoxRelatedWClass) numout)
   (let ((varname (read-from-string (gen-box-string self))))
     (if (not (member varname *let-list* :test 'equal))
       (progn
         (push varname  *let-list*)
         (if *start-repeat-generation*
           (progn
            (push `(setf ,varname ,(gen-code-call self)) *repeat-ev-once-list*)
            `(rep-editor ,varname ,numout ))
           `(progn
              (setf ,varname ,(gen-code-call self))
              (rep-editor ,varname ,numout ))))
       `(rep-editor ,varname ,numout ))))

;(defmethod gen-code-for-ev-once ((self OMBoxRelatedWClass) numout)
;   (let ((varname (read-from-string (gen-box-string self))))
;     (when (not (member varname *let-list* :test 'equal :key 'car))
;       (push `(,varname (multiple-value-list ,(gen-code-call self))) *let-list*))
;     `(rep-editor ,varname ,numout)))

;;; removed multiple-value-list for ev-once in abstractions (?)
(defmethod gen-code-for-ev-once ((self OMBoxRelatedWClass) numout)
   (let ((varname (read-from-string (gen-box-string self))))
     (if (not (member varname *let-list* :test 'equal :key 'car))
        (progn 
          (push `(,varname ,(gen-code-call self)) *let-list*)
          `(progn
             ;;; (*)
             ;(setf ,varname ,(gen-code-call self))
             (rep-editor ,varname ,numout ))
          )
       `(rep-editor ,varname ,numout))))

;;; (*) here I needed to restore the setf because in some cases (e.g. Modalys) 
;;; the box must not be evaluated before some other actions take place (e.g. "new")
;;; The let list is evaluated before anything in the patch, while the body of the macro 
;;; returned code evaluates following the function graph


(defmethod update-if-editor ((self t)) t)

(defmethod update-if-editor ((self OMBoxEditCall))
   (when (and (frames self) (showpict self))
     (update-miniview (iconview (car (frames self))) (value self)))
   (when (editorFrame self)
     (update-editor-after-eval (editorFrame self) (value self))))


;------------------Edition
(defmethod good-val-p? ((self t)) t)

(defmethod instance ((self OMBoxEditCall)) (value self)) 

(defmethod get-editor-class ((self OMBoxEditCall)) 'InstanceEditor)

(defmethod open-instance-editor ((self OMBoxEditCall))
   (let* ((thewindow (make-editor-window 'InstanceEditor
                                         self (name self) self 
                                         :winpos (om-make-point 150 100)
                                         :winsize (om-make-point 250 280)))
          (slot-boxes (slots-inst-boxes self (value self))))
     (setf (presentation (editor thewindow)) 0)
     (mapc #'(lambda (frame)
               (omG-add-element (panel (editor thewindow)) frame)) slot-boxes)
     (editor thewindow)))


(defmethod OpenEditorframe ((self OMBoxEditCall)) 
   ;(if (om-command-key-p)
   ;    nil    ; trop dangereux...
     ;(cond 
     ; ((protected-p (reference self)) 
     ;  (dialog-message "This class is protected, you can't edit its initialisation method")
     ;  (editorframe self))
     ; (t (OpenObjectEditor (get-class-init-method (reference self))) (editorframe self)))
     (or (editorframe self)
         (cond
          ((om-command-key-p)
           (open-instance-editor self))
          ((Class-has-editor-p (value self))
           (if (and (value self) (good-val-p? (value self)))
               (let ((size (get-edit-param self 'winsize))
                     (pos (get-edit-param self 'winpos)))
                 (editor (make-editor-window (get-editor-class (value self)) (value self)
                                             (name self)
                                             self
                                             :winsize (if (om-point-p size) size)  
                                             :winpos (if (om-point-p pos) pos)
                                             )))
             (om-beep-msg (string+ "Bad instance for class " (name (reference self))
                                ". Try to re-evaluate the box in order to create a valid instance."))))
          (t (open-instance-editor self))))
     ;)
   )


(defmethod remove-extra ((self OMPatch) (box OMBoxEditCall))
  (object-remove-extra (value box) box)
  (call-next-method))

(defmethod object-remove-extra ((obj t) box) nil)

(defmethod reference-object ((self OMBox)) (value self))

(defmethod player-menu-item ((self OMBoxEditCall)) 
  (when (play-obj? (value self))
    (list (om-new-leafmenu "Player" #'(lambda () 
                                        (let ((previousplayer (get-edit-param self 'player))
                                              (newplayer (select-player self)))
                                          (when (and newplayer (not (equal previousplayer newplayer)))
                                            (player-special-action newplayer)
                                            (player-init newplayer)
                                            (when (editorframe self) 
                                              (update-controls-view (editorframe self)))
                                            )))))
    ))

;-----------------------------------------------------------------
;SLOT BOXES
;-----------------------------------------------------------------

(defclass OMSlotsBox (OMBoxRelatedWClass) ()
   (:documentation "This is the class of boxes that allow read and write the slot of an OM Instances.#enddoc#
#seealso# (slotboxframe) #seealso#"))

;Inits
(defmethod get-frame-class ((self OMSlotsBox)) 'slotboxframe)
(defmethod get-out-class ((self OMSlotsBox)) 'outfleche)
(defmethod numouts ((self OMSlotsBox)) (length (inputs self)))
(defmethod get-documentation ((self OMSlotsBox)) (string+ "I/O to slots of the class " (name (reference self))))
(defmethod get-frame-name ((self OMSlotsBox)) "slots")
(defmethod get-object-insp-name ((self OMSlotsBox)) "Slots box")

(defmethod omNG-make-new-boxcall-slots ((self OMClass) posi name)
  (let* ((inputs (get-inputs-from-inst (get-super-default-value (class-name self))))
         (rep (make-instance *def-metaclass-box-slot*
                :name name
                :reference self 
                :icon (icon self)
                :inputs inputs)))
    (setf (frame-position rep) (borne-position posi))
    (push rep (attached-objs self))
    rep))


(defmethod make-outputs-of-frame ((self OMSlotsBox) module)
   (make-outputs-from-names self (get-super-default-value  (class-name (reference self))) module))

;---------Code Lisp generation

(defmethod gen-code-call ((self OMSlotsBox))
   (if (not (connected? (first (inputs self))))
     (progn
       (om-beep-msg "Compilation Error : the first inlet in a slot box should always be connected")
       nil)
     (let ((theclass (class-name (reference self)))
           (theobj (gen-code  (first (inputs self)) 0))
           setfs)
       (mapc #'(lambda (input)
                 (when (connected? input)
                   (let ((val (gen-code  input 0)))
                     (push `(setf (,(internp (name input) (symbol-package theclass)) object)  ,val) setfs))))
             (cdr (inputs self)))
       (setf setfs (reverse setfs))
       `(let ((object ,theobj)) ,.setfs  object))))


(defmethod call-gen-code ((self OMSlotsBox) numout)
   `(rep-editor ,(gen-code-call self) ,numout))

;---------Evaluation

(defmethod omNG-box-value ((self OMSlotsBox) &optional (numout 0))
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                        (om-report-condition c ))
                                               :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)))))
     (cond
      ((and (equal (allow-lock self) "x") (value self)) (rep-editor (value self) numout ))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (rep-editor (value self) numout ))
      ((and (equal (allow-lock self) "o")) (reference self))
      (t (if (not (connected? (first (inputs self))))
           (progn
             (om-beep-msg "The first paremeter in a slot box should always be connected")
             nil)
           (let ((theobj (omNG-box-value  (first (inputs self)))))
             (mapc #'(lambda (input)
                       (when (connected? input)
                         (let ((val (omNG-box-value  input)))
                           (eval `(setf (,(internp (name input) (symbol-package (class-name (reference self)))) ,theobj)  ',val)))))
                   (cdr (inputs self)))
             (when (equal (allow-lock self) "&")
               (setf (ev-once-p self) t)
               (setf (value self) theobj))
             (when (equal (allow-lock self) "x")
               (setf (value self) theobj))
             (rep-editor theobj numout))))))
)


     
;---------Edition

;;; OM 6.1 ==> EDIT slot accessors from class/package editor only

(defmethod OpenEditorframe ((self OMSlotsBox)) nil)
;   (cond
;    ((protected-p (reference self))
;     (not (dialog-message "This class is protected.")))
;    ((<= (length (inputs self)) 1)
;     (not (dialog-message "No slots in this class.")))
;    (t (open-dialog-slots (reference self)) nil)))

(defmethod update-from-reference ((self OMSlotsBox) &optional (udt? t))
   (declare (ignore udt?))
   (let* ((new-inputs (get-inputs-from-inst (get-super-default-value (class-name (reference self))))))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs) 
     (when (frames self)
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self))))))


;---------------------------------------------------------
;Lisp Functions Calls
;---------------------------------------------------------
(defun make-lisp-boxes (function patch)
  (if (not (fboundp function))
    (dialog-message (string+ "no such function " (string function)))
    (let* ((new-call (omNG-make-new-lispboxcall function (om-make-point 20 20) 
                                                (mk-unique-name (panel patch) (string function))))
           new-frame)
      (setf new-frame (make-frame-from-callobj new-call))
      (omG-add-element (panel patch) new-frame))))

(defclass OMBoxlispCall (OMBoxcall) ()
   (:documentation "Boxes with a lisp function as reference are instance of this class. #enddoc#
#seealso# (OMLispFun) #seealso#")
   (:metaclass omstandardclass))


;------------------------Inits
(defmethod numouts ((self OMBoxlispCall)) 1)
(defmethod get-input-class-frame ((self OMBoxlispCall)) 'input-funboxframe)
(defmethod get-object-insp-name ((self OMBoxlispCall)) "Lisp Function")
(defmethod get-out-class ((self OMBoxlispCall)) 'outfleche)
(defmethod get-frame-class ((self OMBoxlispCall)) 'boxframe)

(defmethod def-icon-size ((self OMBoxlispCall)) 
  (let ((icn (second (get&corrige-icon (icon self)))))
    (if icn
        (list (om-pict-width icn) (om-pict-height icn))
      '(20 20)))) 

(defmethod omNG-make-new-boxcall ((self OMlispfun) posi name)
   (omNG-make-new-lispboxcall (funname self) posi name))


(defmethod get-boxlispclass-fun ((self t)) 'OMBoxlispCall)


(defun omNG-make-new-lispboxcall (funname posi name)
   (let* ((numins (min-inp-number funname))
          (initvals (make-list numins :initial-element nil))
          (inputs (mapcar #'(lambda (name value) 
                              (make-instance 'input-funbox
                                :name (string name) 
                                :value value 
                                :doc-string (string name))) 
                          (subseq (arglist funname) 0 numins) initvals))
          (rep (make-instance (get-boxlispclass-fun funname)
                 :name name
                 :reference funname 
                 :icon 144
                 :inputs inputs)))
     (setf (frame-position rep) (borne-position posi))
     (set-box-to-inputs inputs rep)
     rep))

;--------------evaluation
(defvar *compiling-macro-boite* nil)

(defmethod omNG-box-value ((self OMBoxlispCall) &optional (num-out 0))
   (declare (ignore num-out))
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                           (om-report-condition c ))
                                                  :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") (special-lambda-value self (reference self)))
      ((equal (allow-lock self) "o") (omNG-make-new-lispfun (reference self)))
      ((and (value self) (equal (allow-lock self) "x")) (nth 0 (value self)))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth 0 (value self)))
      (t (let* (rep)
           (if (macro-function (reference self))
             (let ((*compiling-macro-boite* t)
                   (oldletlist *let-list*)
                   code)
               (setf *let-list* nil)
               (setf code (decode self))
               (setf rep (multiple-value-list (eval `(let* ,(reverse *let-list*) (,(reference self) ,.code)))))
               (setf *let-list* oldletlist))
             (let* ((args  (loop for input in (inputs self)
                                 when (not (keyword-input-p input)) collect (omNG-box-value  input)))
                    (args (list+ args (eval-keywords self))))
               (setf rep (multiple-value-list (apply (reference self) args)))))
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
           (nth 0 rep))))))

;-------------Edition

(defmethod do-add-one-input ((self OMBoxlispCall))
   (let* ((current-imp (length (inputs self)))
          (lambda-lis (arglist (reference self))))
     (when (or (member '&rest lambda-lis) (< current-imp (get-fixed-par-num lambda-lis (reference self))))
       (setf (inputs self) (list+ (inputs self)
                                  (list (make-instance 'input-funbox
                                          :name "add-input"
                                          :value nil
                                          :box-ref self
                                          :doc-string "add-input"))))
       (do-add-one-input-extra self))))

(defmethod do-add-all-inputs ((self OMBoxlispCall)) nil)


(defmethod do-add-one-keyword ((self OMBoxLispcall) &optional (input-key nil))
   "Add a keywords input to 'self', if possible."
   
   (let* ((thefun (fdefinition (reference self)))
          (keywordlist (get-keywords-fun (reference self)))
          (usedkeywords (find-class-boxes (inputs self) 'input-keyword))
          (keyname (if input-key
                       (and (or (find input-key keywordlist) (om-beep-msg (string+ "Wrong keyword name: " (string input-key))))
                            (or (not (find input-key (mapcar 'value usedkeywords))) (om-beep-msg (string+ "Keyword name already used: " (string input-key))))
                            input-key)
                     (find-if-not #'(lambda (elt) (member elt (mapcar 'value usedkeywords))) keywordlist))))
     (when keyname
     (if (< (length usedkeywords) (length keywordlist))
         (setf (inputs self) (list+ (inputs self)
                                  (list (make-instance 'input-keyword
                                          :name (string-downcase keyname) ;"keyword input"
                                          :box-ref self
                                          :value keyname
                                          :def-value nil
                                          :doc-string "" ;(string+ "Choose from this list " (format () "~S"  keywordlist))
                                          ))))
       
       (om-beep-msg "All keywords are already opened.")))))





(defmethod OpenEditorframe ((self OMBoxlispCall))
  ;(print (reference self))
   (not (dialog-message (format nil "This is a compiled Lisp Function. It can not be open in graphical editor.~% [If it is available, you can access the source code of this function using the shortcut 'e']"))))


;-----------------------------------------------------------------
;Type Constants
;-----------------------------------------------------------------

(defclass OMBoxTypeCall (OMBoxcall) 
   ((thestring :initform nil :accessor thestring))
   (:documentation "Boxes with a OMBasicType as reference are instance of this class. #enddoc#
#seealso# (OMBasicType) #seealso#"))

;--------------Inits
(defmethod numouts ((self OMBoxTypeCall)) 1)
(defmethod get-out-class ((self OMBoxTypeCall)) 'outfleche)
(defmethod get-frame-class ((self OMBoxTypeCall)) 'boxTypeframe)
(defmethod get-documentation ((self OMBoxTypeCall)) 
  (string+ "This is a constant of type " (string (reference self))))

(defmethod show-info-window ((self OMBoxTypeCall) &optional (i 0)) (om-beep))

(defmethod omNG-make-new-boxcall ((self OMBasicType) posi name)
   (let* ((type (read-from-string (name self)))
          (newbox (make-instance *def-metaclass-box-type* 
                    :name name
                    :reference type 
                    :icon nil
                    :inputs nil)))
     (setf (frame-position newbox) (borne-position posi))
     (setf (value newbox) (get-super-default-value type))
     (setf (thestring newbox) (format () "~S" (value newbox)))
     newbox))


(defclass boxtype-iconview (ttybox om-static-text-drag) ())

;The const type boxes are a little differents
(defmethod make-frame-from-callobj ((self OMBoxTypeCall))
  "Cons simple frames for 'self'."
  (let* ((name (string-downcase (name self)))
         module boxframex ttybox)
    (setf boxframex (if (frame-size self) (frame-size self) 
                      (good-text-box-size (thestring self) *om-default-font1*)))
                     ; (om-make-point 40 22)))
    (setq module
          (om-make-view (get-frame-class self) 
                        :position (frame-position self) 
                        :size  boxframex
                        :object self))
    (om-add-subviews module (first (setf (outframes module)
                                         (list (om-make-view 'outfleche
                                                         :position (om-make-point (- (round (w module) 2) 4) 
                                                                                  (- (h module) 9))
                                                         :size (om-make-point 8 8)
                                                         :help-spec "option-click to evalue or drag for connections"
                                                         :index 0)))))
    (setf ttybox (om-make-dialog-item 'boxtype-iconview (om-make-point 1 1) 
                                      (om-subtract-points boxframex (om-make-point 3 11))
                                      " " 
                                      :bg-color *om-white-color*
                                      :font *ombox-font*))
    (setf (iconview module) ttybox)
    (om-add-subviews module ttybox)
    (om-set-dialog-item-text ttybox (thestring self))
    (setf (frames self) (list module))
    (unless (frame-size self)
      (setf (frame-size self) (om-view-size module)))
    (setf (name module) name)
    (add-box-resize module)
    module))

;------------Code Lisp generation
(defmethod gen-code ((self OMBoxTypeCall) numout)
   (declare (ignore numout))
   (gen-code (value self) 0))
     
;-------------Evaluation
(defmethod omNG-box-value ((self OMBoxTypeCall) &optional num-out)
   (declare (ignore num-out))
   (if (consp (value self))
     (eval (omng-copy (value self)))
     (value self)))

;;; EDITION       

(defmethod OpenEditorframe ((self OMBoxTypeCall))  nil)

(defclass BoxType-enter-view (change-text-enter-view) ())
(defmethod open-ttybox-class ((self boxtype-iconview)) 'BoxType-enter-view)

(defmethod do-add-one-input ((self OMBoxTypeCall))  nil)
(defmethod do-delete-one-input ((self OMBoxTypeCall)) nil)
(defmethod do-add-all-inputs ((self OMBoxTypeCall)) nil)
(defmethod do-add-one-keyword ((self OMBoxTypeCall) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMBoxTypeCall)) nil)



;------------------------------------------
;This is a special box to define function without D&D, but typing their name
;------------------------------------------

(defclass OMBoxundefined (OMBoxCall) () 
   (:documentation "This is the class for red boxes which appears when you type a name that do not correpond to a class genfun, etc.#enddoc#
#seealso# (OMBoxCall) #seealso#"))

(defmethod show-info-window ((self OMBoxundefined) &optional (i 0)) (om-beep-msg "No info for this box"))
;------------Inits

(defmethod numouts ((self OMBoxundefined)) 0)
(defmethod get-documentation ((self OMBoxundefined)) "not yet defined box")

(defmethod omNG-make-new-boxcall ((self (eql 'undefined)) posi name)
   (let* ((newbox (make-instance 'OMBoxundefined 
                    :name name
                    :reference self 
                    :icon nil
                    :inputs nil)))
     (setf (frame-position newbox) (borne-position posi))
     newbox))


(defmethod make-frame-from-callobj ((self OMBoxundefined))
  (let ((module
         (om-make-view 'boxTypeFrame 
                       :position (frame-position self) 
                       :size  (om-make-point 66 18)
                       :object self)))
    (om-add-subviews module (setf (iconview module) 
                              (om-make-dialog-item 'undef-ttybox
                                                   (om-make-point 1 1) 
                                                   (om-subtract-points (om-view-size module) (om-make-point 2 3))
                                                   "undefined" 
                                                   :font *om-default-font1*
                                                   :help-spec "not yet defined box"
                                                   :bg-color *undefbox-color*)))
    (set-value (iconview module) 'undefined)
    (setf (frames self) (list module))
    (unless (frame-size self)
      (setf (frame-size self) (om-view-size module)))
    (setf (name module) "undefined")
    module))

(defmethod OpenEditorframe ((self OMBoxundefined)) nil)

;-----------------------------------------------------------------
;INSTANCE BOXES
;-----------------------------------------------------------------

(defclass OMBoxInstance (OMBoxcall) ()
   (:documentation "Boxes which OMinstance by reference are instances of this class.#enddoc#
#seealso# (instboxframe) #seealso#"))

;----------------Inits

(defmethod allow-alias ((self OMBoxInstance)) nil)
(defmethod get-frame-class ((self OMBoxInstance)) 'instboxframe)
(defmethod get-object-insp-name ((self OMBoxInstance)) "Box instance")
(defmethod get-out-class ((self OMBoxInstance)) 'outfleche)
(defmethod numouts ((self OMBoxInstance)) 1)

(defmethod get-documentation ((self OMBoxInstance))
   (string+ "This is an instance of the class " (string (type-of (instance (reference self))))))

(defmethod get-frame-name ((self OMBoxInstance))
   (string-downcase (name (reference self))))

(defmethod omNG-make-new-boxcall ((self OMInstance) posi name)
   (let* ((instance (instance self))
          (theclass (class-of instance))
          (rep (make-instance *def-metaclass-box-inst* 
                 :name name
                 :reference self 
                 :icon (icon theclass)
                 :inputs nil)))
     (setf (frame-position rep) (borne-position posi))
     (setf (value rep) instance)
     (if (mypathname self)
       (push rep (attached-objs self))
       (push rep (attached-objs theclass)))
     rep))



(defmethod omNG-make-new-boxcall ((self OMlistInstance) posi name)
   (let* ((instance (instance self))
          (rep (make-instance 'OMBoxInstance 
                 :name name
                 :reference self 
                 :icon (icon self)
                 :inputs nil)))
     (setf (frame-position rep) (borne-position posi))
     ; ?
     (setf (frame-size rep) (om-make-point 45 40))
     (setf (value rep) instance)
     (when (mypathname self)
       (push rep (attached-objs self)))
     rep))

(defmethod OpenEditorframe ((self OMBoxInstance))
  (OpenObjectEditor (reference self)) nil)


;--------------Evaluation
(defmethod omNG-box-value ((self OMBoxInstance) &optional (numout 0))
   (declare (ignore numout))
   (value self))

;-------------Lisp Code generation
(defmethod gen-code ((self OMBoxInstance) numout)
   (if (omclass-p (class-of (class-of (value self))))
     (value self)
     (gen-code (value self) numout)))

;------------Edition
(defmethod do-add-one-input ((self OMBoxInstance))  nil)
(defmethod do-delete-one-input ((self OMBoxInstance)) nil)
(defmethod do-add-all-inputs ((self OMBoxInstance)) nil)

(defmethod do-add-one-keyword ((self OMBoxInstance) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMBoxInstance)) nil)

(defmethod remove-extra ((self OMPatch) (box OMBoxInstance))
   (setf (attached-objs (reference box))
         (remove box (attached-objs self) :test 'equal)))
        
(defmethod update-from-reference ((self OMBoxInstance) &optional (udt? t))
   (declare (ignore udt?)) nil)

;-----------------------------------------------------------------
;COMMENTS BOXES
;-----------------------------------------------------------------

(defclass OMBoxcomment (OMBoxcall) 
   ((textstyle :initform  *comment-style* :accessor textstyle)
    (textcolor :initform *comment-color*  :accessor textcolor))
   (:documentation "Comments Boxes are instances of this class.#enddoc#
#seealso# (commentboxframe) #seealso#
#textstyle# This slot contains the style of the comment's text. #textstyle#
#textcolor# This slot contains the color of the comment's text. #textcolor#"))

;-------Inits
(defmethod get-frame-class ((self OMBoxcomment)) 'commentboxframe)
(defmethod numouts ((self OMBoxcomment)) 0)
(defmethod get-documentation ((self OMBoxcomment)) "no comments")
(defmethod get-frame-name ((self OMBoxcomment)) nil)
(defmethod show-info-window ((self OMBoxcomment) &optional (i 0)) (om-beep-msg "No info for comments"))

(defmethod allow-lock-button ((self OMBoxComment)) nil)

(defmethod get-bg-color ((self OMBoxComment)) *om-white-color*)

(defmethod omNG-make-new-boxcall ((self (eql 'comment)) posi name)
  (let* ((newbox (make-instance 'OMBoxcomment 
                   :name name
                   :reference "no comment"
                   :icon nil
                   :inputs nil)))
    (setf (frame-position newbox) (borne-position posi))
    newbox))


(defmethod make-frame-from-callobj ((self OMBoxcomment))
   "Cons a simple frame for the comment box 'self'."
  (let* ((sizemodule (if (frame-size self) (frame-size self) (om-make-point 100 60)))
          (module
           (om-make-view 'commentboxframe 
             :position (frame-position self) 
             :size  sizemodule
             :object self)))
    (om-add-subviews module (setf (iconview module) 
                                  (om-make-dialog-item 'commentview
                                                       (om-make-point 3 3) 
                                                       (om-subtract-points (om-view-size module) (om-make-point 6 6))
                                                       (reference self)
                                                       :font (textstyle self))))
    (setf (frames self) (list module))
    (om-set-fg-color (iconview module) (textcolor self))
    (unless (frame-size self)
      (setf (frame-size self) (om-view-size module)))
    (setf (name module) "comment")
    (add-box-resize module)
    module))


;---------Evaluation

(defmethod omNG-box-value ((self OMBoxcomment) &optional (numout 0))
  (declare (ignore numout)) nil)

;---------Non Code lisp generation


;---------Editionom

(defmethod do-add-one-input ((self OMBoxcomment))  nil)
(defmethod do-delete-one-input ((self OMBoxcomment)) nil)
(defmethod do-add-all-inputs ((self OMBoxcomment)) nil)

(defmethod do-add-one-keyword ((self OMBoxcomment) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMBoxcomment)) nil)

;-----------------------------------------------------------------
;PATCH IN PATCH
;-----------------------------------------------------------------

(defclass OMBoxPatch (OMBoxcall) ()
   (:documentation "This is the class of boxes with a Patch as reference. #enddoc#
#seealso# (patchboxframe OMBoxAbsPatch OMPatch) #seealso#")
   (:metaclass omstandardclass))

;------------Inits

(defmethod get-frame-class ((self OMBoxPatch)) 'patchboxframe)
(defmethod numouts ((self OMBoxPatch)) (length (get-patch-outputs (reference self))))

(defmethod get-box-documentation ((self OMBoxPatch))
  (or (get-documentation (reference self))
      (get-documentation self)))

(defmethod get-documentation ((self OMBoxPatch)) 
  (string+ "This box calls the function defined by the " (get-object-insp-name (reference self)) 
           " " (name (reference self)) "."))

(defmethod get-frame-name ((self OMBoxPatch)) (string-downcase (name (reference self))))
(defmethod get-object-insp-name ((self OMBoxPatch)) (string+ (get-object-insp-name (reference self)) " Box"))
(defmethod spec-obj-icon-size ((self ompatch)) '(32 32))

(defmethod omNG-make-new-boxcall ((patch OMPatch) posi name)
  (let* ((rep (make-instance *def-metaclass-box-patch* 
                             :name name
                             :reference patch 
                             :icon (icon patch))))
    (setf (inputs rep) (mapcar #'(lambda (input) 
                                   (make-instance 'input-funbox
                                                  :name (or (frame-name input) (name input))
                                                  :value (eval (defval input))
                                                  :box-ref rep
                                                  :doc-string (docu input)))
                               (get-patch-inputs patch)))
    (setf (frame-position rep) (borne-position posi))
    (push rep (attached-objs patch))
    rep))


(defmethod make-outputs-of-frame ((self OMBoxPatch) module)
   "The outputs of the box are made from the OMOut boxes in the patch reference."
   (let ((numouts (numouts self))
         (outsname (list+ (find-class-boxes (boxes (reference self)) 'OMTempOut) (sort (find-class-boxes (boxes (reference self)) 'OMout) '< :key 'indice))))
     (setf outsname (loop for item in outsname collect (get-frame-name item)))
     (loop for i from 0 to (- numouts 1) do
           (let ((thenewout (om-make-view (get-out-class self)
                              :position (om-make-point (- (* (+ i 1) (round (w module) (+ numouts 1))) 4) 
                                                         (- (h module) 9))
                              :size (om-make-point 8 8)
                              :help-spec (nth i outsname)
                              :index i)))
             (when (and (= i 0) (find-class-boxes (boxes (reference self)) 'OMtempout))
               (setf (iconID thenewout) 227))
             (push thenewout (outframes module))
             (om-add-subviews module thenewout)))))

(defmethod spec-input-frame ((self OMBoxPatch) index) 
  (if (and (= index 0)
           (patch-has-temp-in-p (reference self)))
    'temp-funboxframe
    nil))



;--------------Lisp Code generation



;(defmethod gen-code-for-ev-once ((self OMBoxPatch) numout)
;   (let ((varname (read-from-string (gen-box-string self)))
;         (patchfun `,(intern (string (code (reference self))) :om)))
;     (if (not (member varname *let-list* :test 'equal)) 
;       (progn
;         (push varname  *let-list*)
;         (if *start-repeat-generation*
;          (progn
;            (push `(setf ,varname (multiple-value-list (,patchfun ,.(decode self)))) *repeat-ev-once-list*)
;            `(nth ,numout ,varname))
;           
;           `(progn
;              (setf ,varname (multiple-value-list (,patchfun ,.(decode self))))
;              (nth ,numout ,varname))))
;       `(nth ,numout ,varname))))

(defmethod gen-code-for-ev-once ((self OMBoxPatch) numout)
   (let ((varname (read-from-string (gen-box-string self)))
         (patchfun `,(intern (string (code (reference self))) :om)))
      (when (not (member varname *let-list* :test 'equal :key 'car))
        (push `(,varname (multiple-value-list (,patchfun ,.(decode self))))  *let-list*))
      `(nth ,numout ,varname)))



(defmethod gen-code ((self OMBoxPatch) numout)
   (let ((patchfun `',(code (reference self))))
     (cond
      ((equal (allow-lock self) "&") 
       (gen-code-for-ev-once self numout))
      ((equal (allow-lock self) "l")
       (curry-lambda-code self  (code (reference self))))
      ((equal (allow-lock self) "o")  (reference self))
      ((equal (allow-lock self) "x") 
       `(nth ,numout ,(gen-code (value self) 0)))
      (t
       (if (zerop numout)
         `(funcall ,patchfun ,.(decode self))
         `(nth ,numout (multiple-value-list (funcall ,patchfun ,.(decode self)))))))))

;screamer
(defmethod gen-code ((self OMBoxPatch) numout)
   (let ((patchfun `,(intern (string (code (reference self))) :om)))
     (cond
      ((equal (allow-lock self) "&") 
       (gen-code-for-ev-once self numout))
      ((equal (allow-lock self) "l")
       (curry-lambda-code self  patchfun))
      ((equal (allow-lock self) "o")  (reference self))
      ((equal (allow-lock self) "x") 
       `(nth ,numout ,(gen-code (value self) 0)))
      (t
       (if (zerop numout)
         `(,patchfun ,.(decode self))
         `(nth ,numout (multiple-value-list (,patchfun ,.(decode self)))))))))



;----------------Evaluation
 
(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                        (om-report-condition c ))
                                               :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)
                               ))))
     (cond
      ((and (equal (allow-lock self) "x") (value self))
       (nth num-out (value self)))
      ((and (equal (allow-lock self) "o") (reference self)))
      ((equal (allow-lock self) "l")
       (unless (compiled? (reference self))
         (if (and (lisp-exp-p (reference self)) (editorframe self))
           (compile-without-close (editorframe self))
           (compile-patch (reference self))))
       (special-lambda-value self (intern (string (code (reference self))) :om)))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) 
       (nth num-out (value self)))
      (t 
       (unless (compiled? (reference self))
         (if (and (lisp-exp-p (reference self)) (editorframe (reference self)))
             (compile-without-close (editorframe (reference self)))
           (compile-patch (reference self))))
       (let* ((args  (mapcar #'(lambda (input) 
                                   (omNG-box-value input)) (inputs self)))
              (rep nil))

           (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args)))
      
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
           ;;;; TEST
           (when (equal (allow-lock self) nil)
             (setf (value self) rep))
           ;;;;
           (nth num-out rep))))))

;---------------Edition

(defmethod OpenEditorframe ((self OMBoxPatch))
   (OpenObjectEditor (reference self)))

;;; security...
(defmethod remove-extra ((self t) (box t)) nil)

(defmethod remove-extra ((self OMPatch) (box OMBoxPatch))
  (setf (attached-objs (reference box))
    ;;(remove box (attached-objs self) :test 'equal)
    (remove box (attached-objs (reference box)) :test 'equal)
    )
  )

(defmethod do-add-one-input ((self OMBoxPatch))  nil)
(defmethod do-delete-one-input ((self OMBoxPatch)) nil)
(defmethod do-add-all-inputs ((self OMBoxPatch)) nil)

(defmethod do-add-one-keyword ((self OMBoxPatch) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMBoxPatch)) nil)

(defmethod update-from-reference ((self OMBoxPatch) &optional (udt? t))
   "Called when occurs changes in the patch reference of 'self'."
   (declare (ignore udt?))
   (let* ((new-inputs (mapcar #'(lambda (input) 
                                  (make-instance 'input-funbox
                                    :name (or (frame-name input) (name input))
                                    :value (eval (defval input))
                                    :box-ref self
                                    :doc-string (docu input))) 
                              (get-patch-inputs (reference self))))
          conec-to-me)
     (when *input-to-erase*
       (if (>= *input-to-erase* 0)
         (setf (inputs self) 
               (delete (nth *input-to-erase* (inputs self)) (inputs self) :test 'equal))

         (setf (inputs self) 
               (list+ (list (make-instance 'input-funbox)) (inputs self)))
         ))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))
               (setf (value newin) (value oldin))) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs)

     (when *output-to-delete*
         (setf conec-to-me (get-conect-to-me self))
         (if (>= *output-to-delete* 0)
           (loop for item in conec-to-me do
                 (erase-out-connec self item *output-to-delete*))
           (loop for item in conec-to-me do
                 ; mettre a jour la connection qui vient ici (+1)
                 (loop for inp in (inputs item) do 
                       (let ((connec (connected? inp)))
                         (when (and connec (equal (first connec) self))
                           (setf (nth 1 (connected? inp)) (+ (second (connected? inp)) 1)))))
                 ))
         
         )
       
     (when (frames self)
       (when *output-to-delete*
         (loop for source in conec-to-me do
               (box-draw-connections (car (frames source)) nil)
               (redraw-frame (car (frames source))))
         )
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self))))))

(defmethod erase-out-connec ((self OMBoxPatch) source numout)
   "Called when you delete an OMOut box from th patch reference of 'self'."
   (loop for inp in (inputs source) do 
         (let ((connec (connected? inp)))
           (when (and connec (equal (first connec) self))
             (cond
              ((= (second connec) numout) 
               (setf (connected? inp) nil))
              ((> (second connec) numout) 
               (setf (nth 1 (connected? inp)) (- (second (connected? inp)) 1))))))))
      
     


;------------------------------------------------------------------------
;MAQUETTE BOXES
;------------------------------------------------------------------------
(defclass OMBoxMaquette (OMBoxcall)  
  ((mode :initarg :mode :initform 0 :accessor mode)) 
   (:documentation "Boxes having a maquette as reference are instances of this class. #enddoc#
#seealso# (OMMaquette OMBoxAbsmaq) #seealso#")
   (:metaclass omstandardclass))

;; mode 0 = normal
;; mode 1 = avec les inputs/outputs

;--------------Inits

(defmethod numouts ((self OMBoxMaquette))
  (if (= (mode self) 1)
    (+ 1 (length (find-class-boxes (boxes (reference self)) 'Maq-OMOut)))
    1))

(defmethod get-frame-class ((self OMBoxMaquette)) 'maquetteframe)

(defmethod get-box-documentation ((self OMBoxMaquette))
  (or (get-documentation (reference self))
      (get-documentation self)))

(defmethod get-documentation ((self OMBoxMaquette)) 
  (string+ "Build, modify or computes a the Maquette " (name (reference self))))

(defmethod get-object-insp-name ((self OMBoxMaquette)) "Maquette Box")
(defmethod get-frame-name ((self OMBoxMaquette)) (string-downcase (name (reference self))))

(defmethod get-input-class-frame ((self OMBoxMaquette)) 
  (if (= (mode self) 0) 'temp-funboxframe 'input-funboxframe))

(defmethod do-add-one-input ((self OMBoxMaquette))
   "Maquette boxes have an optional input for the metric parameters."
   (when (= 0 (mode self)) 
     (cond ((= (length (inputs self)) 2)
            (setf (inputs self) (append (inputs self)
                                        (list (make-instance 'input-funbox
                                                             :name "Metrics"
                                                             :value (metricparam (params (reference self)))
                                                             :doc-string "metric to put in maquette")))))
           ((= (length (inputs self)) 3)
            (setf (inputs self) (append (inputs self)
                                        (list (make-instance 'input-funbox
                                                             :name "Connections"
                                                             :value nil
                                                             :doc-string "box connections in the maquette")))))
           (t nil)
           )))

(defmethod do-add-all-inputs ((self OMBoxMaquette)) nil)

(defmethod do-delete-one-input ((self OMBoxMaquette))
   (when (and (= 0 (mode self)) (> (length (inputs self)) 2))
     (setf (inputs self) (butlast (inputs self)))))

(defmethod do-add-one-keyword ((self OMBoxMaquette) &optional (input-key nil)) nil)
(defmethod do-delete-one-keyword ((self OMBoxMaquette)) nil)

(defmethod omNG-make-new-boxcall ((maquette OMMaquette) posi name)
  (let* ((inputs (list (make-instance 'input-funbox
                           :name "time"
                           :value 1000
                           :doc-string "time rate number of offsets list")
                         (make-instance 'input-funbox
                           :name "objs"
                           :value nil
                           :doc-string "objects to put in maquette"))
                 )
          (rep (make-instance *def-metaclass-box-maq* 
                 :mode 0
                 :name name
                 :reference maquette 
                 :icon (icon maquette)
                 :inputs inputs)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs maquette))
     rep))

(defmethod erase-out-connec ((self OMBoxMaquette) source numout)
   "Called when you delete an OMOut box from th patch reference of 'self'."
   (loop for inp in (inputs source) do 
         (let ((connec (connected? inp)))
           (when (and connec (equal (first connec) self))
             (cond
              ((= (second connec) numout) 
               (setf (connected? inp) nil))
              ((> (second connec) numout) 
               (setf (nth 1 (connected? inp)) (- (second (connected? inp)) 1))))))))


(defmethod change-mode ((self OMBoxMaquette) mode &optional (connect nil))
  (setf (mode self) mode)
  ;;; mode = 1 => functional in/outs
  ;;; mode = 0 ==> cons in/outs
  (let* ((new-inputs (if (= mode 1)
                       (mapcar #'(lambda (input) 
                                   (make-instance 'input-funbox
                                     :name (or (frame-name input) (name input))
                                     :value (eval (defval input))
                                     :doc-string (docu input))) 
                               (sort (find-class-boxes (boxes (reference self)) 'Maq-OMin) '< :key 'indice))
                       (list (make-instance 'input-funbox
                               :name "time"
                               :value 1000
                               :doc-string "time rate number of offsets list")
                             (make-instance 'input-funbox
                               :name "objs"
                               :value nil
                               :doc-string "objects to put in maquette"))
                       ))
         conec-to-me)
    (when connect 
      (mapc #'(lambda (oldin newin) 
                (setf (connected? newin) (connected? oldin))
                (setf (value newin) (value oldin))) 
            (inputs self) new-inputs)
      )
    (setf (inputs self) new-inputs)   
    (setf conec-to-me (get-conect-to-me self))
    (when (frames self)
      (loop for source in conec-to-me do
            (box-draw-connections (car (frames source)) nil)
            (redraw-frame (car (frames source))))
      
      (box-draw-connections (car (frames self)) nil)
      (redraw-frame (car (frames self)))
      ) 
    ))


(defmethod make-outputs-of-frame ((self OMBoxMaquette) module)
   "Cons a list of views which are the outputs of the box."
   (let ((numouts (numouts self))
         (outsname (mapcar 'get-frame-name (sort (find-class-boxes (boxes (reference self)) 'maq-OMout) '< :key 'indice))))
     (loop for i from 0 to (- numouts 1) do
           (let ((thenewout (om-make-view (get-out-class self)
                                              :position (om-make-point (- (* (+ i 1) (round (w module) (+ numouts 1))) 4) 
                                                         (- (h module) 9))
                                              :size (om-make-point 8 8)
                              :help-spec (if (= i 0) "maquette global output" (nth (1- i) outsname))
                              :index i)))
             (when (= i 0) (setf (iconID thenewout) 227))
             (push thenewout (outframes module))
             (om-add-subviews module thenewout)))))


;---------Lisp Code generation

;(defmethod gen-code-for-ev-once ((self OMBoxMaquette) args)
;   (let ((varname (read-from-string (gen-box-string self))))
;     (if (not (member varname *let-list* :test 'equal)) 
;       (progn
;         (push varname  *let-list*)
;         (if *start-repeat-generation*
;           (progn
;             (push `(setf ,varname (progn
;                                     ,(if (= (mode self) 0)
;                                        `(put-boxes-inmaquette  ,(reference self) ,(first args) ,(second args)) 
;                                        `(cons-maq-values ,(reference self) (list ,.args))
;                                        )
;                                       (cons-copy-maquette-object ,(reference self) (boxes ,(reference self))))) *repeat-ev-once-list*)
;             varname)           
;           `(progn
;              (setf ,varname (progn
;                               ,(if (= (mode self) 0)
;                                        `(put-boxes-inmaquette  ,(reference self) ,(first args) ,(second args)) 
;                                        `(cons-maq-values ,(reference self) (list ,.args))
;                                        ) 
;                               (cons-copy-maquette-object ,(reference self) (boxes ,(reference self)))))
;              ,varname)))
;       varname)))

(defmethod gen-code-for-ev-once ((self OMBoxMaquette) args)
   (let ((varname (read-from-string (gen-box-string self))))
     (when (not (member varname *let-list* :test 'equal :key 'car))
        (push `(,varname (progn ,(if (= (mode self) 0)
                                        `(put-boxes-inmaquette  ,(reference self) ,(first args) ,(second args)) 
                                        `(cons-maq-values ,(reference self) (list ,.args))
                                        )
                                       (cons-copy-maquette-object ,(reference self) (boxes ,(reference self)))))  *let-list*))
     varname))


(defmethod gen-code ((self OMBoxMaquette) numout)
   (declare (ignore numout))
   (let ((args (decode self)))
     (cond
      ((equal (allow-lock self) "&") 
       (gen-code-for-ev-once self args))
      ((equal (allow-lock self) "x") 
       `(cons-copy-maquette-object ,(reference self) (boxes ,(reference self))))
      ((equal (allow-lock self) "o") (reference self))
      (t `(progn
            (when ,(third args)
              (setf (metricparam (params ,(reference self))) ',(third args)))
            ,(if (= (mode self) 0)
               `(put-boxes-inmaquette  ,(reference self) ,(first args) ,(second args)) 
               `(cons-maq-values ,(reference self) (list ,.args))
               ) 
            (cons-copy-maquette-object ,(reference self) (boxes ,(reference self))))))))


(defun cons-maq-values (reference args)
  (let ((in-boxes (sort (find-class-boxes (boxes reference) 'maq-OMin)  '< :key 'indice)))
    (mapcar #'(lambda (input arg)
                (setf (value input) arg)) in-boxes args)
    (loop for item in (get-boxes-to-value (boxes reference)) do
          (omng-box-value item))
    (mapcar #'(lambda (input)
                (setf (value input) nil)) in-boxes)
    ))


  
;---------Evaluation

(defmethod omNG-box-value ((self OMBoxMaquette) &optional (num-out 0))
  (handler-bind ((error #'(lambda (c) 
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                          (om-report-condition c ))
                                               :size (om-make-point 300 200))
                              (om-abort)))))
    (cond
     ((and (equal (allow-lock self) "x") (value self))
      ;(setf (value self) (cons-copy-maquette-object (reference self) (boxes (reference self))))
      (value self))
     ((equal (allow-lock self) "o") (reference self))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) (value self))
     (t (let* ((args  (mapcar #'(lambda (input)
                                  (omNG-box-value input)) (inputs self)))
               rep)
          (when (and (= 0 (mode self)) (third args))
            (setf (metricparam (params (reference self))) (third args)))
          (when (editorFrame (reference self))
            (om-close-window (window (editorFrame (reference self)))))
                
          (if (= (mode self) 0)
              (put-boxes-inmaquette (reference self) (first args) (second args) (nth 3 args)))

          (cons-maq-values (reference self) args)
                          
          (setf rep (cons-copy-maquette-object (reference self) (boxes (reference self))))
          (when (= (mode self) 1)
            (setf rep (append (list rep) 
                              (loop for out in (sort (find-class-boxes (boxes (reference self)) 'maq-OMout)  '< :key 'indice) 
                                    collect (if (connected? (car (inputs out)))
                                                (let ((con-obj (car (connected? (car (inputs out)))))
                                                      (num (cadr (connected? (car (inputs out))))))
                                                 ;(print (list (value con-obj) num))
                                                  (if (listp (value con-obj))
                                                      (nth (+ num 1) (value con-obj))
                                                    (value con-obj))
                                                  )
                                              (value (car (inputs out))))
                                    )))
            )
           
          (when (equal (allow-lock self) "&")
            (setf (ev-once-p self) t)
            (setf (value self) rep))
          (when (equal (allow-lock self) "x")
            (setf (value self) rep))
          (if (= (mode self) 1)
              (nth num-out rep)
            rep)
          )))))

;--------Edition

(defmethod OpenEditorframe ((self OMBoxMaquette)) 
   (OpenObjectEditor (reference self)) nil)

(defmethod remove-extra ((self OMPatch) (box OMBoxMaquette))
   (setf (attached-objs (reference box))
     ;;(remove box (attached-objs self) :test 'equal)
     (remove box (attached-objs (reference box)) :test 'equal)
     ))

(defmethod update-from-reference ((self OMBoxMaquette) &optional (udt? t))
  (declare (ignore udt?))
  (when (= (mode self) 1)
     (let* ((new-inputs (mapcar #'(lambda (input) 
                                    (make-instance 'input-funbox
                                      :name (or (frame-name input) (name input))
                                      :value (eval (defval input))
                                      :doc-string (docu input))) 
                                (sort (find-class-boxes (boxes (reference self)) 'Maq-OMin) '< :key 'indice))
                        )
          conec-to-me)
     (when *input-to-erase*
       (if (>= *input-to-erase* 0)
         (setf (inputs self) 
               (delete (nth *input-to-erase* (inputs self)) (inputs self) :test 'equal))
         ))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))
               (setf (value newin) (value oldin))) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs)

     (when *output-to-delete*
         (setf conec-to-me (get-conect-to-me self))
         (if (>= *output-to-delete* 0)
           (loop for item in conec-to-me do
                 (erase-out-connec self item (+ *output-to-delete* 1)))
           )
         )
       
     (when (frames self)
       (when *output-to-delete*
         (loop for source in conec-to-me do
               (box-draw-connections (car (frames source)) nil)
               (redraw-frame (car (frames source))))
         )
       (box-draw-connections (car (frames self)) nil)
       (redraw-frame (car (frames self))))
     )))


;------------------------------------------------------------------------
;DEAD BOXES
;------------------------------------------------------------------------

(defclass box-dead (OMBoxcall)  
   ((numouts :initform 1 :accessor numouts))
   (:documentation "When the reference of a box is clear, the box change its class to this class. #enddoc#
#seealso# (OMBoxcall  general-box-dead) #seealso#
#numouts# This slot stor the output's number of the old box.#numouts#"))

(defmethod get-boxcallclass-fun ((self (eql 'dead-method))) 'box-dead)

(defmethod* dead-method (&rest rest) 
   :icon 190 
   :doc "I have lost my reference, I am dead"
  (declare (ignore rest))  nil)

(defmethod dead-method (&rest rest) 
   (declare (ignore rest))  nil)

(defmethod gen-code ((self box-dead) numout)
   (declare (ignore numout))  nil)

(defmethod special-value ((self  box-dead) &optional (args nil))
   (declare (ignore args)) (dialog-message (string+ "I have lost my reference, "
                                                    (if (value self) (value self) " ")
                                                    " I am dead")))

;====================DEAD_METHOD=================
(defclass general-box-dead (OMBoxcall)  
   ((save-code :initform nil :accessor save-code)
    (numouts :initform 1 :accessor numouts)
    (mesage :initform nil :accessor mesage))
   (:documentation "When you can not load a saved box it become an instance of this class.#enddoc#
#seealso# (OMBoxcall) #seealso#
#save-code# This slot store the lisp expression which saved the old box.#save-code#
#numouts# This slot store the output's number of the old box.#numouts#
#mesage# An error message for each type of box.#mesage#"))

;-------Inits

(defmethod omNG-make-new-boxcall ((self (eql 'dead)) posi name)
   (let* ((newbox (make-instance 'general-box-dead 
                    :name name
                    :reference "wooo" 
                    :icon 190)))
     (setf (frame-position newbox) (borne-position posi))
     newbox))

(defmethod get-object-insp-name ((self general-box-dead)) "Dead Box")
(defmethod allow-lock-button ((self general-box-dead)) nil)
(defmethod get-documentation ((self general-box-dead)) (mesage self))
(defmethod get-frame-name ((self general-box-dead))
   (name self))


;---------Evaluation

(defmethod omNG-box-value ((self general-box-dead) &optional (numout 0))
  (declare (ignore numout)) 
  (dialog-message (mesage self)))

(defmethod gen-code ((self general-box-dead) numout)
   (declare (ignore numout))
   (unless *loading-ws* (om-beep-msg (mesage self)))
   nil)

;---------Edition
(defmethod OpenEditorframe ((self general-box-dead))
  (not (dialog-message (mesage self))))

(defmethod do-add-one-input ((self general-box-dead))  nil)
(defmethod do-add-all-inputs ((self general-box-dead)) nil)
(defmethod do-delete-one-input ((self general-box-dead)) nil)

(defmethod do-add-one-keyword ((self general-box-dead) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self general-box-dead)) nil)




;=================================================
;CURRYFICATION
;=================================================

(defmethod get-args-eval-currry ((self OMBoxcall))
   "Get the inputs of 'self' to curryfy"
   (let* ((nesymbs nil)
          (args (loop for input in (inputs self) append
                      (let ((val (if (connected? input)
                                     `',(omNG-box-value  input)
                                   (let ((newsymbol (gensym)))
                                     (push newsymbol nesymbs)
                                     newsymbol))))
                        (if (keyword-input-p input)
                            (list (value input) val)
                          (list val))))))
     (values nesymbs args)))

;-----------------Evaluation
(defmethod special-lambda-value ((self OMBoxcall) symbol)
   "Eval a box in lambda mode."
  (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
    (eval `#'(lambda ,(reverse nesymbs)
               (apply ',symbol (list ,.args))))))


(defmethod special-lambda-value ((self OMBoxEditCall) symbol)
   "Eval a factory in lambda mode."
   (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
     (cond
      ((connected? (first (inputs self)))
       (eval `#'(lambda () 
                  (objFromObjs ,(first args) (make-instance ',symbol)))))
      ((= (length nesymbs) (length args))
       (eval  `#'(lambda ,(reverse nesymbs) 
                   (cons-new-object (make-instance ',symbol) (list ,. args) nil))))
      (t
       (eval `#'(lambda ,(cdr (reverse nesymbs)) 
                  (cons-new-object (make-instance ',symbol) (list nil ,.(cdr args)) nil)))))))

;-----------------code generation
(defmethod curry-lambda-code ((self OMBoxcall) symbol)
   "Lisp code generetion for a box in lambda mode."
   (let* ((nesymbs nil)
          (args  (mapcar #'(lambda (input)
                             (if (connected? input)
                               (gen-code input 0)
                               (let ((newsymbol (gensym)))
                                 (push newsymbol nesymbs)
                                 newsymbol))) (inputs self))))
     `#'(lambda ,(reverse nesymbs)
          (apply ',symbol (list ,.args)))))


(defmethod curry-lambda-code ((self OMBoxEditCall) symbol)
  "Lisp code generetion for a factory in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
                              (gen-code input 0)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
    (cond
     ((connected? (first (inputs self)))
      `#'(lambda () 
           (objFromObjs ,(first args) (make-instance ',symbol))))
     ((= (length nesymbs) (length args))
      `#'(lambda ,(reverse nesymbs) 
           (cons-new-object (make-instance ',symbol) (list ,. args) nil)))
     (t
      `#'(lambda ,(cdr (reverse nesymbs)) 
           (cons-new-object (make-instance ',symbol) (list nil ,.(cdr args)) nil))))))










   












