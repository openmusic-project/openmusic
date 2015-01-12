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
;Classes for omloop are defined in this file.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;==========================================
;LOOP
;==========================================

        
;===========================================================================
;PATCH

(defclass patchForLoop (patchForBox) ()
   (:documentation "This class implements the special patches for the omloop boxes.  #enddoc#
#seealso# (OMPatch omloop-box) #seealso#"))

(defmethod get-editor-class ((self patchForLoop)) 'LoopEditor)

(defmethod OpenEditorframe ((self patchForLoop))
   (or (editorframe self)
       (panel (open-new-RelationFrame  self (string+ "OM Loop - "(name self)) (get-elements self)))))

(defparameter *loop-count-check* t)
(defparameter *loop-count-limit* 500000)

(defun continue-loop-dialog ()
  (let ((win (om-make-window 'om-dialog :position :centered :size (om-make-point 310 140)
                             :resizable nil :maximize nil :minimize nil
                             :window-title "Loop Warning"))
        (text (om-make-dialog-item 'om-static-text (om-make-point 20 15) (om-make-point 260 60)
                                   (string+ "OM Loop has been running for " 
                                            (integer-to-string *loop-count-limit*) 
                                            " iterations. It may be an infinite loop.")
                                   :font *controls-font*
                                   ))
        (box (om-make-dialog-item 'om-check-box (om-make-point 20 55) (om-make-point 100 20)
                                  "Do not ask again"
                                  :font *controls-font*))
        )
    (om-add-subviews win text box
                     
                     (om-make-dialog-item 'om-button (om-make-point 75 90) (om-make-point 80 26) "Abort"
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog win (list t (om-checked-p box)))))

                     (om-make-dialog-item 'om-button (om-make-point 170 90) (om-make-point 100 26) "Keep Running"
                                          :default-button t
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog win (list nil (om-checked-p box)))))
                     )
    (om-modal-dialog win)))

;;; iter-count is declared in the omloop code
(defun loop-check-code ()
  `(when (and *loop-count-check* iter-count)
     (incf iter-count)
     (when (>= iter-count *loop-count-limit*)
       (let ((rep (continue-loop-dialog)))
         (if (car rep) (abort)
           (if (cadr rep)
               (setf iter-count nil)
             (setf iter-count 0)))))))

#|
(defmethod compile-patch ((self patchForLoop))
   "Code generates by Loop patches is generate by this method."
   (let* ((boxes (boxes self))
          (oldletlist *let-list*)
          (*let-list* nil)
          (do-box (car (find-class-boxes boxes 'OMLoopDo)))
          (init-box (car (find-class-boxes boxes 'OMinitDo)))
          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
          (loop-boxes (find-loop-boxes boxes))
          (loop-code (mapcan #'(lambda (theout)
                                 (loop-gen-code theout 0)) loop-boxes))
          (acum-boxes (find-acum-boxes boxes))
          (acum-declaration (mapcar #'(lambda (acumm)
                                        (declare-closure acumm)) acum-boxes))
          (acum-inits (mapcar #'(lambda (acumm)
                                  (gen-code acumm -1)) acum-boxes))
          body init)
     (setf init (gen-code init-box 0))
     (setf body (gen-code do-box 0))
     (eval `(defun ,(intern (string (first (code self))) :om)  (,.symbols) 
              (let (,.acum-declaration (iter-count 0))
                ,.acum-inits
                ;(let* ,(reverse *let-list*) ,init) ;;; bug if a let variable depends on a loop iterator !!!!!
                (loop ,.loop-code
                      do ,(loop-check-code)
                      finally   (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                      collect (gen-code final-box i))))                      
                      do
                      (let* ,(reverse *let-list*) ,body)
                      ))))
     (setf *let-list* oldletlist)))

|#



(defmethod compile-patch ((self patchForLoop))
   "Code generates by Loop patches is generate by this method."
   (let* ((boxes (boxes self))
          (oldletlist *let-list*)
          (*let-list* nil)
          (do-box (car (find-class-boxes boxes 'OMLoopDo)))
          (init-box (car (find-class-boxes boxes 'OMinitDo)))
          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
          (loop-boxes (find-loop-boxes boxes))
          (loop-code (mapcan #'(lambda (theout)
                                 (loop-gen-code theout 0)) loop-boxes))
          (acum-boxes (find-acum-boxes boxes))
          (acum-declaration (mapcar #'(lambda (acumm)
                                        (declare-closure acumm)) acum-boxes))
          (acum-inits (mapcar #'(lambda (acumm)
                                  (gen-code acumm -1)) acum-boxes))
          body init)
     (setf init (gen-code init-box 0))
     (setf body (gen-code do-box 0))
     (setf final (loop for i from 0 to (1- (length (inputs final-box))) collect (gen-code final-box i)))
     (eval `(defun ,(intern (string (first (code self))) :om)  (,.symbols) 
              (let (,.acum-declaration (iter-count 0))
                (let* ,(reverse *let-list*)   ;;; LET EV-ONCE HERE / BUG IF ONE OF THE LET-LIST DEPENDS ON THE LOOP-CODE
                ,.acum-inits
                ,init   
                (loop ,.loop-code
                      do ,(loop-check-code)
                      finally (return (values ,.final))                    
                      do
                      ;;; (let* ,(reverse *let-list*) ,body)
                      (progn
                        ;;; rest the ev-once boxes at each iteration... 
                        ,.(loop for var in *let-list* collect `(setf ,(car var) ,(cadr var)))
                        ,body)
                      )))))
     (setf *let-list* oldletlist)))


;----------------
;EDITOR
;----------------
(defclass loopEditor (boxpatchEditor) ())

(defmethod get-editor-panel-class ((self loopEditor))  'loopPanel)

;----------------
;PANEL
;----------------
(omg-defclass loopPanel (boxpatchPanel) ()
   (:documentation "This is the class for the editor panel which define a OMloop box.  #enddoc#
#seealso# (omLoop-Box box-with-patch-frame) #seealso#
#patch# This slot keeps the patch associated to the box. #patch#"))

;;; CHANGER
(defmethod omg-remove-element ((self loopPanel) (box inFrame)) (call-next-method))
; (om-beep-msg "delete inputs in the loop box"))

(defmethod omg-remove-element ((self loopPanel) (box t))
   "Finally and eachTime boxes can not be removed."
   (if (or (equal (class-name (class-of (object box))) 'OMLoopDo)
           (equal (class-name (class-of (object box))) 'OMfinalDo))
     (om-beep-msg "These boxes can not be erased")
     (call-next-method)))


(defmethod add-output-enabled ((self looppanel) type) nil)
(defmethod add-input-enabled ((self looppanel) type) t)

(defmethod add-output ((self looppanel) position)
  (om-beep-msg "Add inlets to the FINALLY box to increase the number of outputs"))

; (defmethod add-input ((self looppanel) position) (call-next-method))


(defmethod add-window-buttons  ((self loopPanel))
   "Add iterator and accumulator buttons to the patch."
   (call-next-method)
   
   (om-add-subviews self 
                    
                    (om-make-view 'om-icon-button
                        :icon1 "loop-for"
                        :position (om-make-point 95 5)
                        :size (om-make-point 24 24)
                        :help-spec "For i from A to B"
                        :action
                        #'(lambda (item) (declare (ignore item)) 
                           (omG-add-element self
                                            (make-frame-from-callobj 
                                             (omNG-make-new-boxcall (fdefinition 'forloop)
                                                                    (om-make-point 5 80)
                                                                    (mk-unique-name self "for"))))))
                 (om-make-view 'om-icon-button
                               :icon1 "loop-while"
                               :position (om-make-point 120 5)
                               :size (om-make-point 24 24)
                               :help-spec "While A"
                               :action
                   #'(lambda(item)  (declare (ignore item))
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'whileloop)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "while"))))))
                 (om-make-view 'om-icon-button
                        :icon1 "loop-list"
                   :position (om-make-point 145 5)
                   :size (om-make-point 24 24)
                   :help-spec "For element in list"
                   :action
                   #'(lambda(item) (declare (ignore item)) 
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'listloop)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "inlist"))))))
                 (om-make-view 'om-icon-button
                        :icon1 "loop-onlist"
                   :position (om-make-point 170 5)
                   :size (om-make-point 24 24)
                   :help-spec "For element on list"
                   :action
                   #'(lambda(item) (declare (ignore item)) 
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'onlistloop)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "onlist"))))))
                 
                 (om-make-view 'button-icon
                   :iconID 175
                   :position (om-make-point 240 5)
                   :size (om-make-point 24 24)
                   :help-spec "Incremental counter"
                   :action
                   #'(lambda(item) (declare (ignore item)) 
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'counter)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "count"))))))
                 (om-make-view 'button-icon
                   :iconID 172
                   :position (om-make-point 265 5)
                   :size (om-make-point 24 24)
                   :help-spec "Sum accumulator"
                   :action
                   #'(lambda(item)  (declare (ignore item))
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'sum)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "sum"))))))
                 (om-make-view 'button-icon
                   :iconID 174
                   :position (om-make-point 290 5)
                   :size (om-make-point 24 24)
                   ::help-spec "Minimize"
                   :action
                   #'(lambda(item)  (declare (ignore item))
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'minim)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "min"))))))
                 (om-make-view 'button-icon
                   :iconID 173
                   :position (om-make-point 315 5)
                   :size (om-make-point 24 24)
                   :help-spec "Maximize"
                   :action
                   #'(lambda(item)  (declare (ignore item))
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'maxi)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "max"))))))
                 (om-make-view 'button-icon
                   :iconID 206
                   :position (om-make-point 340 5)
                   :size (om-make-point 24 24)
                   :help-spec "Collect"
                   :action
                   #'(lambda(item)  (declare (ignore item))
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'listing)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "collect"))))))
                 (om-make-view 'button-icon
                   :iconID 171
                   :position (om-make-point 365 5)
                   :size (om-make-point 24 24)
                   :help-spec "General accumulator"
                   :action
                   #'(lambda(item)  (declare (ignore item))
                      (omG-add-element self
                                       (make-frame-from-callobj 
                                        (omNG-make-new-boxcall (fdefinition 'accumulator)
                                                               (om-make-point 5 80)
                                                               (mk-unique-name self "accum"))))))))


      
(defmethod om-get-menu-context ((self loopPanel))
   (let ((pos (om-mouse-position self)))
         (append 
          (list 
           (list 
            (om-make-menu "Iterators"
                          (list 
                           (om-new-leafmenu "For" #'(lambda ()
                                                     (omG-add-element self
                                                                      (make-frame-from-callobj 
                                                                       (omNG-make-new-boxcall (fdefinition 'forloop)
                                                                                              pos
                                                                                              (mk-unique-name self "for"))))))

                            (om-new-leafmenu "While" #'(lambda () 
                                                         (omG-add-element self
                                                                          (make-frame-from-callobj 
                                                                           (omNG-make-new-boxcall (fdefinition 'whileloop)
                                                                                                  pos
                                                                                                  (mk-unique-name self "while"))))))

                            (om-new-leafmenu "List Loop" #'(lambda () 
                                                         (omG-add-element self
                                                                          (make-frame-from-callobj 
                                                                           (omNG-make-new-boxcall (fdefinition 'listloop)
                                                                                                  pos
                                                                                                  (mk-unique-name self "inlist"))))))

                            (om-new-leafmenu "On List Loop" #'(lambda ()  
                                                           (omG-add-element self
                                                                            (make-frame-from-callobj 
                                                                             (omNG-make-new-boxcall (fdefinition 'onlistloop)
                                                                                                    pos
                                                                                                    (mk-unique-name self "onlist"))))))
                          
                   ))
           (om-make-menu "Accumulators"
                         (list 
                          (om-new-leafmenu "Collect" #'(lambda () 
                                                               (omG-add-element self
                                                                                (make-frame-from-callobj 
                                                                                 (omNG-make-new-boxcall (fdefinition 'listing)
                                                                                                        pos
                                                                                                        (mk-unique-name self "collect"))))))
                          (om-new-leafmenu "Sum" #'(lambda () 
                                                     (omG-add-element self
                                                                      (make-frame-from-callobj 
                                                                       (omNG-make-new-boxcall (fdefinition 'sum)
                                                                                              pos
                                                                                              (mk-unique-name self "sum"))))))
                          (om-new-leafmenu "Count" #'(lambda () 
                                                       (omG-add-element self
                                                                        (make-frame-from-callobj 
                                                                         (omNG-make-new-boxcall (fdefinition 'counter)
                                                                                                pos
                                                                                                (mk-unique-name self "count"))))))
                          (om-new-leafmenu "Min" #'(lambda () 
                                                     (omG-add-element self
                                                                      (make-frame-from-callobj 
                                                                       (omNG-make-new-boxcall (fdefinition 'minim)
                                                                                              pos
                                                                                              (mk-unique-name self "min"))))))
                          (om-new-leafmenu "Max" #'(lambda () 
                                                     (omG-add-element self
                                                                      (make-frame-from-callobj 
                                                                       (omNG-make-new-boxcall (fdefinition 'maxi)
                                                                                              pos
                                                                                              (mk-unique-name self "max"))))))
                          (om-new-leafmenu "General Accum." #'(lambda () 
                                                               (omG-add-element self
                                                                                (make-frame-from-callobj 
                                                                                 (omNG-make-new-boxcall (fdefinition 'accumulator)
                                                                                                        pos
                                                                                                        (mk-unique-name self "accum"))))))
                          
                          ))
           ))
          (call-next-method))
         ))
         



(defmethod omG-add-element ((self LoopPanel) frame)
  (if (and (member (type-of (object frame)) '(OMInitDo OMLoopDo OMFinalDo))
           (find-class-boxes (boxes (object self)) (type-of (object frame))))
      (om-beep-msg (string+ "There can be only one " (string (type-of (object frame)))
                            " in a Loop editor."))
    (call-next-method)
    ))


;--------------------

(defclass OMLoopDo (box-seq-call) ()
   (:documentation "This is the class for the each time boxes in a loop.#enddoc#
#seealso# (omloop-box loopIterators acumboxes) #seealso#"))

(defmethod* loopDo  ((op t) &rest more) 
            :numouts 0 
            :initvals '(nil) :indoc '("operations to do" "other oprations") 
            :doc "The box EACHTIME (or 'LoopDo') box is evaluated at each step of the loop and triggers the iterative operation(s).
It can have multiple inputs in order to trigger various successive operations."
            :icon 208
            (declare (ignore op)) nil)

(defmethod numouts ((self OMLoopDo)) 0)
(defmethod get-frame-name ((self OMLoopDo)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'loopDo))) 'OMLoopDo)
(defmethod allow-lock-button ((self OMLoopDo)) 
   "each time boxes do not allow a lock button." nil)
(defmethod no-allow-copy-p ((self OMLoopDo)) 
   "'Eachtime' boxes can not be directly copied"
   "'Eachtime' loop boxes.")

;--------------------
(defclass OMfinalDo (box-seq-call) ()
   (:documentation "This is the class for the finally boxes in a loop.#enddoc#
#seealso# (omloop-box loopIterators acumboxes) #seealso#"))

(defmethod allow-lock-button ((self OMfinalDo))
   "Finally boxes do not allow a lock button." nil)

(defmethod no-allow-copy-p ((self OMfinalDo))
   "Finally boxes can not be directly copied" "Finally loop boxes.")

(defmethod* finalDo  ((val t) &rest more) :numouts 0 
  :initvals '(nil) :indoc '("value to return" "other values") 
  :doc "The box FINALLY (or 'FinalDo') returns the result of the OMLOOP process.
It can have multiple inputs in order to return various values in corresponding outputs of the OMLOOP box." 
  :icon 216
  (declare (ignore val)))

(defmethod numouts ((self OMfinalDo)) 0)
(defmethod get-frame-name ((self OMfinalDo)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'finalDo))) 'OMfinalDo)


(defmethod do-delete-one-input-extra ((self OMfinalDo))
   "When you erase one input from the finally box you must delete one input from the loop box also."
   (call-next-method)
   (let ((box (box (object (om-view-container (car (frames self)))))))
     (setf (numouts box) (- (numouts box) 1))
     (redraw-frame (car (frames box))) t))

(defmethod do-add-one-input-extra ((self OMfinalDo))
   "When you add one input to the finally box you must add one input to the loop box also."
   (call-next-method)
   (let ((box (box (object (om-view-container (car (frames self)))))))
     (setf (numouts box) (+ (numouts box) 1))
     (redraw-frame (car (frames box)))
     t))

;--------------------
(defclass OMinitDo (box-seq-call) ()
   (:documentation "This is the class for the finally boxes in a loop.#enddoc#
#seealso# (omloop-box loopIterators acumboxes) #seealso#"))

(defmethod allow-lock-button ((self OMinitDo))
   "Finally boxes do not allow a lock button." nil)

(defmethod no-allow-copy-p ((self OMinitDo))
   "Finally boxes can not be directly copied" "InitDo loop boxes.")

(defmethod* initDo  ((op t) &rest more) 
            :initvals '(nil) :indoc '("operations to do" "other oprations") 
            :doc "The box INITDO box is evaluated before the loop starts and allows to trigger preliminary operations.
It can have multiple inputs in order to trigger various successive operations."
            :icon 216
            :numouts 0 
  (declare (ignore op)))

(defmethod numouts ((self ominitDo)) 0)
(defmethod get-frame-name ((self OMfinalDo)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'initDo))) 'OMinitDo)


(defmethod do-delete-one-input-extra ((self ominitDo))
   "When you erase one input from the finally box you must delete one input from the loop box also."
   (call-next-method)
   ;(let ((box (box (object (om-view-container (car (frames self)))))))
   ;  (setf (numouts box) (- (numouts box) 1))
   ;  (redraw-frame (car (frames box))) t)
   )

(defmethod do-add-one-input-extra ((self ominitDo))
   "When you add one input to the finally box you must add one input to the loop box also."
   (call-next-method)
   ;(let ((box (box (object (om-view-container (car (frames self)))))))
   ;  (setf (numouts box) (+ (numouts box) 1))
   ;  (redraw-frame (car (frames box)))
   ;  t)
   )




;==================ITERATORS=======================
(defclass loopIterators (OMBoxcall) 
   ((code :initform nil :accessor code))
   (:documentation "This is the class for iterator boxes in a loop.
There are specific subclasses for each type of iterator.#enddoc#
#seealso# (OMloop-box forloopcall whileloopcall listloopcall onlistloopcall)  #seealso#
#code# This slot store the specific Lisp code associated to the iteratore.#code#"))

(defmethod initialize-instance :after ((self loopIterators) &key controls)
   (declare (ignore controls))
   (setf (code self) (gensym)))

(defmethod allow-lock-button ((self loopIterators))
   "Iterators do not allow a lock button." nil)
  
(defmethod gen-code ((self loopIterators) numout)
   (declare (ignore numout))
   (code self))

(defmethod loopbox? ((self t)) nil)
(defmethod loopbox? ((self loopIterators)) t)

(defmethod* omNG-box-value ((self  loopIterators) &optional (numout 0))
   (declare (ignore numout))
   (dialog-message "You can not evaluate this box")
   (om-abort)
   )

(defun find-loop-boxes (lis)
  (let* (rep )
    (mapc #'(lambda (box)
              (when (loopbox? box)
                (push box rep))) lis)
   (sort-loop-boxes  rep)))

(defun sort-loop-boxes (lis)
  (let (cheked)
    (loop while lis do
          (loop for item in lis do
                (when (not (member-closure item lis))
                  (push item cheked)
                  (setf lis (remove item lis)))))
    (reverse cheked)))

(defun member-closure (item lis)
  (let (rep)
    (loop for box in lis 
          while (not rep) do
          (when (member item (get-closure box) :test 'equal)
            (setf rep t)))
    rep))

(defun get-closure (box)
  (let ((firstboxes (get-conect-to-me box)))
    (remove-duplicates (append firstboxes (loop for item in firstboxes
                                                append (get-closure item))) :test 'equal)))

;-------------------------------------
(defclass forloopcall (loopIterators) ()
   (:documentation "This is the class for the FOR 'i' FROM 'n' TO 'm' BY 'step' iterator.#enddoc#
#seealso# (whileloopcall listloopcall onlistloopcall)  #seealso#"))

(defmethod loop-gen-code ((self forloopcall) numout)
  (declare (ignore numout))
  (let ((step (when (third (inputs self)) (gen-code (third (inputs self)) 0))))
    (if step
      `(for ,(code self) from ,(gen-code (first (inputs self)) 0) to
            ,(gen-code (second (inputs self)) 0) by ,step)
      `(for ,(code self) from ,(gen-code (first (inputs self)) 0) to
            ,(gen-code (second (inputs self)) 0 )))))
                  

(defclass whileloopcall (loopIterators) ()
   (:documentation "This is the class for the WHILE 'condition' iterator.#enddoc#
#seealso# (forloopcall listloopcall onlistloopcall)  #seealso#"))

(defmethod loop-gen-code ((self whileloopcall) numout)
  (declare (ignore numout))
  `(while ,(gen-code (first (inputs self)) 0)))

(defclass listloopcall (loopIterators) ()
   (:documentation "This is the class for the FOR 'i' IN 'list' iterator.#enddoc#
#seealso# (forloopcall whileloopcall onlistloopcall)  #seealso#"))

(defmethod loop-gen-code ((self listloopcall) numout)
  (declare (ignore numout))
  (let ((step (when (second (inputs self)) (gen-code (second (inputs self)) 0))))
    (if step
      `(for ,(code self) in ,(gen-code (first (inputs self)) 0) by ,step)
      `(for ,(code self) in ,(gen-code (first (inputs self)) 0)))))


(defclass onlistloopcall (loopIterators) ()
   (:documentation "This is the class for the FOR 'i' ON 'list' iterator.#enddoc#
#seealso# (forloopcall whileloopcall listloopcall)  #seealso#"))

(defmethod loop-gen-code ((self onlistloopcall) numout)
  (declare (ignore numout))
  (let ((step (when (second (inputs self)) (gen-code (second (inputs self)) 0))))
    (if step
      `(for ,(code self) on ,(gen-code (first (inputs self)) 0) by ,step)
      `(for ,(code self) on ,(gen-code (first (inputs self)) 0)))))


(defmethod get-boxcallclass-fun ((self (eql 'forloop))) 'forloopcall)
(defmethod get-boxcallclass-fun ((self (eql 'whileloop))) 'whileloopcall)
(defmethod get-boxcallclass-fun ((self (eql 'listloop))) 'listloopcall)
(defmethod get-boxcallclass-fun ((self (eql 'onlistloop))) 'onlistloopcall)


(defmethod* forloop  ((from integer) (to integer) &optional by) 
  :icon 151  :initvals '(0 10 1) :indoc '("low value" "high value" "step")
  :doc "FORLOOP is an OMLOOP iterator. It defines the number of iteration in an OMLOOP.

The iteration runs for <i> from <from> to <to> with a step of <by>.
At each iteration, FORLOOP returns the value of the current <i>.

See OM User Manual for more details on OMLOOP.
"
  (declare (ignore by)) nil)

(defmethod* whileloop  ((expr t)) :numouts 0 :icon 152 :indoc '("condition")
            :doc 
            "WHILELOOP is an OMLOOP iterator. It sets a condition for the loop to stop running (when <condition> = NIL).

See OM User Manual for more details on OMLOOP."
            nil)

(defmethod* listloop  ((list list)  &optional (by 'cdr)) 
  :icon 153  :indoc '("list to iterate" "step function")
  :doc "LISTLOOP is an OMLOOP iterator. It defines the number of iteration in an OMLOOP.

The iteration runs for every element in <list>.
At each iteration, LISTLOOP returns the value of the current element (CAR of the list).

<by> allows to set a step for walking in the list (i.e. generates the list for the next iteration).

See OM User Manual for more details on OMLOOP."
  (declare (ignore by)) nil)

(defmethod* onlistloop  ((list list)  &optional (by 'cdr)) 
            :icon 158 :indoc '("list to iterate" "step function")  
            :doc "ONLISTLOOP is an OMLOOP iterator. It defines the number of iteration in an OMLOOP.

The iteration runs for every element in <list> like LISTLOOP, but on the list CDR (the 'rest'). 
At each iteration, LISTLOOP returns the value of the current CDR of the list.

<by> allows to set a step for walking in the list (i.e. generates the list for the next iteration).

See OM User Manual for more details on OMLOOP."
            (declare (ignore by)) nil)

;======================ACCUMULATORS============================

(defclass acumboxes (OMBoxcall) 
   ((closure :initform nil :initarg :closure :accessor closure))
   (:documentation "This is the class for accumulator boxes in a loop.#enddoc#
#seealso# (OMBoxcall OMloop-box PatchForLoop) #seealso#
#closure# This slot store a lexical closure which define the binary operation performed
at each evaluation of the accumulator#closure#"))

(defmethod get-frame-name ((self acumboxes)) (name self))

(defmethod allow-lock-button ((self acumboxes))
   "Acumboxes do not allow lock buttom." nil)

(defmethod initialize-instance :after ((self acumboxes) &key controls)
   (declare (ignore controls))
   (setf (closure self) (gensym)))

(defmethod declare-closure ((self acumboxes))
  `(,(closure self) 
    (let (accum ret) 
       #'(lambda (cur init  fun flag &optional retard) 
           (cond ((= flag 0)  (when retard (setf ret retard))  (setf accum init))
                 ((= flag 1) (if ret (pop ret) (setf accum (funcall fun accum cur))))
                 ((= flag 2) (if ret (pop ret) accum)))))))

(defmethod omNG-box-value ((self  acumboxes) &optional (numout 0))
   (declare (ignore numout))
   (dialog-message "You can not evaluate this box")
   (om-abort))

(defmethod gen-code ((self acumboxes) numout)
   "(Case numout (1 trigger one accumulation and get the etat of self after the evaluation)
(2 get the etat of self without trigger the evaluation) (3 Initialize the accum)."
  (incf numout)
  (if (= numout 3)
    (gen-code self -1)
    `(funcall ,(closure self) ,(gen-code (first (inputs self)) 0) ,(gen-code (second (inputs self)) 0)
              ,(gen-code (third (inputs self)) 0) ,numout ,(gen-code (fourth (inputs self)) 0))))

(defmethod gen-code ((self acumboxes) (numout (eql 1)))
   `(funcall ,(closure self) nil nil nil 2 nil))
      
(defmethod gen-code ((self acumboxes) (numout (eql -1)))
   `(funcall ,(closure self) nil ,(gen-code (second (inputs self)) 0)
             nil 0 ,(gen-code (fourth (inputs self)) 0)))

(defmethod acumbox? ((self t)) nil)
(defmethod acumbox? ((self acumboxes)) t)

(defun find-acum-boxes (lis)
  (let* (rep)
    (mapc #'(lambda (box)
              (when (acumbox? box)
                (push box rep))) lis)
    (reverse rep)))
;------------------Specifiques accumulators---------------------

(defmethod* accumulator  ((data t) init fun  &optional retard) 
  :numouts 3 :icon 171
  :indoc '("items to collect" "initial value" "accumulation function")
  :doc "The ACCUMULATOR box (or 'Accum') allows to collect results of the successive iterations in an OMLOOP in a user-defined manner.

<data> should be connected to the data to collect.
<init> determines the initial value of the accumulator.
<fun> is a function of 2 arguments (accumulated value, new data) determining how the collected <data> is to be accumulated with the current internal value (e.g. '+ '* 'list). This function can also be defined by a patch or function box of two arguments set in 'lambda' mode.

As every accumulation boxes, the ACCUMULATOR has 3 outputs:
- When evaluated on its 1st output, the value connected to the first input (<data>) is collected and accumulated to the current value by the function defined in <fun>.
- When evaluated on its 2nd output, the current internal value is returned (usually connected to the 'Finally' box in the OMLOOP).
- When evaluated on its 3rd output, the internal value is reset to <init>.

See OM User Manual for more details on OMLOOP."

  (declare (ignore init fun retard)) nil)

(defmethod get-boxcallclass-fun ((self (eql 'accumulator))) 'acumboxes)


(defclass funacumboxes (acumboxes) 
   ((fun-name :initform nil :initarg :fun-name :accessor fun-name)
    (init-val :initform nil :initarg :init-val :accessor init-val))
   (:documentation "There exist five specific accumulators (collect, max, min, count if and sum.
Each one is an instance af this class.#enddoc#
#seealso# (acumboxes OMloop-box) #seealso#
#fun-name# A name for the function called when you eval the accumuilator (i.e. + for sum, max for max, etc.)#fun-name#
#init-val# An initial value for the accumuilator (i.e. 0 for sum, nil for collect, etc.)#init-val# "))

(defmethod initialize-instance :after ((self funacumboxes) &key controls)
  (declare (ignore controls))
  (let ((acumfun (get-fun-acum (reference self))))
    (setf (fun-name self) (first acumfun))
    (setf (init-val self) (second acumfun))))


(defmethod gen-code ((self funacumboxes) numout)
  (incf numout)
  (if (= numout 3)
    (gen-code self -1)
  `(funcall ,(closure self) ,(gen-code (first (inputs self)) 0) ,(init-val self)
            ',(fun-name self) ,numout ,(gen-code (second (inputs self)) 0))))
        
(defmethod gen-code ((self funacumboxes) (numout (eql -1)))
  `(funcall ,(closure self) nil ,(init-val self) nil 0))

(defmethod gen-code ((self funacumboxes) (numout (eql 1)))
  `(funcall ,(closure self) nil nil nil 2 nil))

(defmethod* counter  ((something t) &optional retard) 
  :numouts 3 :icon 175  
  :indoc '("things to count")
  :doc "The COUNTER box (or 'Count') allows to count things during the OMLOOP iterations.

<something> determines wether is count is triggered at a given iteration.

As every accumulation boxes, the COUNTER has 3 outputs:
- When evaluated on its 1st output, if the value connected to the input (<something>) is not NIL, then the internal value of the counter is icreased (+ 1).
- When evaluated on its 2nd output, the current value of the counter is returned (usually connected to the 'Finally' box in the OMLOOP).
- When evaluated on its 3rd output, the counter is reset to 0.

See OM User Manual for more details on OMLOOP."
  (declare (ignore retard)) nil)

(defmethod* sum  ((number t) &optional retard) 
  :numouts 3 :icon 172  
  :indoc '("numbers to sum")
  :doc "The SUM box allows to sum values during the OMLOOP iterations.

<number> is a number to add at a given iteration.

As every accumulation boxes, the SUM has 3 outputs:
- When evaluated on its 1st output, the value connected to the input (<number>) is added to the internal value of the sum.
- When evaluated on its 2nd output, the current SUM value is returned (usually connected to the 'Finally' box in the OMLOOP).
- When evaluated on its 3rd output, the SUM is reset to 0.

See OM User Manual for more details on OMLOOP."
  (declare (ignore retard)) nil)

(defmethod* minim  ((value t) &optional retard) 
  :numouts 3 :icon 174
    :indoc '("numbers")
  :doc "The MIN box (or 'Minim') allows to record its minimum input value during the OMLOOP iterations.

<value> is a number to compare to the current minimum at a given iteration.

As every accumulation boxes, the MIN has 3 outputs:
- When evaluated on its 1st output, the value connected to the input is compared and stored as the new MIN if it is lower than to the current internal MIN value.
- When evaluated on its 2nd output, the current MIN value is returned (usually connected to the 'Finally' box in the OMLOOP).
- When evaluated on its 3rd output, the memory is reset.

See OM User Manual for more details on OMLOOP."
  (declare (ignore retard)) nil)

(defmethod* maxi  ((value t) &optional retard) 
  :numouts 3 :icon 173
    :indoc '("numbers")
  :doc "The MAX box (or 'Maxi') allows to record its maximum input value during the OMLOOP iterations.

<value> is a number to compare to the current maximum at a given iteration.

As every accumulation boxes, the MAX has 3 outputs:
- When evaluated on its 1st output, the value connected to the input is compared and stored as the new MAX if it is greater than to the current internal MAX value.
- When evaluated on its 2nd output, the current MAX value is returned (usually connected to the 'Finally' box in the OMLOOP).
- When evaluated on its 3rd output, the memory is reset.

See OM User Manual for more details on OMLOOP."
  (declare (ignore retard)) nil)

(defmethod* listing  ((data t) &optional retard) 
  :numouts 3 :icon 206
    :indoc '("anything")
  :doc "The COLLECT box (or 'Listing') allows to collect data in a list during the OMLOOP iterations.

<data> can be anything to collect in the list at a given iteration.

As every accumulation boxes, the COLLECT has 3 outputs:
- When evaluated on its 1st output, the value connected to the input (<data>) is collected as a new element in the internal collected list.
- When evaluated on its 2nd output, the collected list is returned (usually connected to the 'Finally' box in the OMLOOP).
- When evaluated on its 3rd output, the collected list is reset to its initial state (NIL).

See OM User Manual for more details on OMLOOP."
  (declare (ignore retard)) nil)


(defmethod get-boxcallclass-fun ((self (eql 'counter))) 'funacumboxes)
(defmethod get-boxcallclass-fun ((self (eql 'sum))) 'funacumboxes)
(defmethod get-boxcallclass-fun ((self (eql 'minim))) 'funacumboxes)
(defmethod get-boxcallclass-fun ((self (eql 'maxi))) 'funacumboxes)


(defmethod get-fun-acum ((self (eql 'counter))) (list 'omcount 0))
(defmethod get-fun-acum ((self (eql 'sum))) (list '+ 0))
(defmethod get-fun-acum ((self (eql 'minim))) (list 'min (expt 2 32)))
(defmethod get-fun-acum ((self (eql 'maxi))) (list 'max (* -1 (expt 2 32))))
(defmethod get-fun-acum ((self (eql 'listing))) (list 'leftcons nil))

(defun omcount (x y)
  (if y (+ 1 x) x))

(defun leftcons (x y)
  (list+ x (list y)))


(defclass omcollect (acumboxes)  ())

(defmethod get-boxcallclass-fun ((self (eql 'listing))) 'omcollect)

(defmethod declare-closure ((self omcollect))
   `(,(closure self) 
     (let (accum ret tail) 
       #'(lambda (cur init  fun flag &optional retard)
           (declare (ignore fun init))
           (cond ((= flag 0)  
                  (when retard (setf ret retard))  
                  (setf accum (cons nil nil) tail accum))
                 ((= flag 1) 
                  (when ret 
                    (setf cur (pop ret)))
                  (setf (cdr tail) (cons cur nil))
                  (setf tail (cdr tail)))
                 ((= flag 2)  (rest accum)))))))



;==================
; THE LOOP BOX
;==================

(defmethod* omloop  (&rest oplist) :numouts 1 
   :initvals '(nil) :indoc '("loop input")
   :doc "LOOP (iterative process)

OMLOOP is a special internal patch represnting iterative processes in OM.

Open the OMLOOP box to design an iterative process in the loop editor. 
The OMLoop box has standard patch inputs and retruns the values connected to the internal box 'Finally'.
Use the ITERATOR boxes to control the iteration and the accumulator boxes to collect the results generated by the 'EachTime' box evaluation.

See OM User Manual and the OMLOOP refernce section for more details.
" 
   :icon 181 
   (declare (ignore oplist)) nil)


(defclass omloop-box (box-with-patch) 
   ((numouts :initform 1 :accessor numouts))
   (:documentation "This is the class for the omloop boxes.#enddoc#
#seealso# (box-with-editor loopIterators acumboxes) #seealso#"))

(defmethod get-object-insp-name ((self omloop-box)) "OM LOOP")

(defmethod omNG-copy ((self omloop-box))
  `(let* ((copy ,(call-next-method)))
     (setf (numouts copy) ,(numouts self))
     copy))

(defmethod initialize-instance :after ((self omloop-box) &key controls)
  (declare (ignore controls))
  (let ((thedo (omNG-make-new-boxcall (fdefinition 'loopDo) (om-make-point 200 250) "eachTime"))
        (thefinal (omNG-make-new-boxcall (fdefinition 'finalDo) (om-make-point 300 250) "finally"))
        ;(theinit (omNG-make-new-boxcall (fdefinition 'initDo) (om-make-point 110 250) "init"))
        )
    (setf (code (patch self)) (list (gensym)))
    (omNG-add-element (patch self) thedo)
    (omNG-add-element (patch self) thefinal)
    ;(omNG-add-element (patch self) theinit)
    (push self (attached-objs (patch self)))
    self
    ))

(defmethod openeditorframe ((self omloop-box))
   (openobjecteditor (patch self)) nil)

(defmethod get-patch-editor-class ((self omloop-box))
   "'patchForLoop' is the class of the Patch associated to 'self'."
   'patchForLoop)

(defmethod call-gen-code ((self omloop-box) numout)
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     (if (zerop numout) 
       `(,(first (code (patch self))) ,.in-list)
       `(nth ,numout (multiple-value-list (,(first (code (patch self))) ,.in-list))))))

;screamer
(defmethod call-gen-code ((self omloop-box) numout)
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     (if (zerop numout) 
       `(,(intern (string (first (code (patch self)))) :om) ,.in-list)
       `(nth ,numout (multiple-value-list (,(intern (string (first (code (patch self)))) :om) ,.in-list))))))

(defmethod gen-code-call ((self omloop-box))
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     `(,(first (code (patch self))) ,.in-list)))

;screamer
(defmethod gen-code-call ((self omloop-box))
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     `(,(intern (string (first (code (patch self)))) :om) ,.in-list)))

(defmethod special-lambda-value ((self omloop-box) symbol)
   "Eval a loop box in lambda mode."
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
                  (apply (fdefinition ',(first (code (patch self)))) (list ,.args)))))))

;screamer
(defmethod special-lambda-value ((self omloop-box) symbol)
   "Eval a loop box in lambda mode."
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

(defmethod curry-lambda-code ((self omloop-box) symbol)
   "Lisp code generation for a loop box in lambda mode."
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
                 (apply (fdefinition ',(first (code (patch self)))) (list ,.args))))))

;screamer

(defmethod curry-lambda-code ((self omloop-box) symbol)
   "Lisp code generation for a loop box in lambda mode."
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


(defmethod special-value ((self  omloop-box) &optional (args nil))
    (unless (compiled? (patch self)) 
      (compile-patch (patch self))
      (setf (compiled? (patch self)) t))
    (apply (fdefinition (intern (string (first (code (patch self)))) :om)) args))

(defmethod get-boxcallclass-fun ((self (eql 'omloop))) 'omloop-box)

;----frame

(omg-defclass loopboxframe (boxframe) ()
   (:documentation "Simple frame for OMLOOP boxes. #enddoc#
#seealso# (OMloop) #seealso#"))


(defmethod get-frame-class ((self omloop-box)) 'loopboxframe)

(defmethod omG-rename :after ((self loopboxframe) new-name)
   (declare (ignore new-name))
   (set-patch-box-name (object self)))

(defmethod set-patch-box-name ((box omloop-box) &optional name)
  (let ((new-name (or name (frame-name box)))
        (thepatch (patch box)))
    (when (EditorFrame thepatch)
      (omG-rename (EditorFrame thepatch) new-name))
    (setf ;; new . OK ?
          (frame-name box) new-name
          (name box) new-name
          (name thepatch) new-name)))

(defmethod add-args-to-box ((box omloop-box) args) nil)



(defmethod update-from-reference ((self omloop-box) &optional (udt? t))
   "Called when occurs changes in the patch reference of 'self'."
   (declare (ignore udt?))
   (let* ((new-inputs (mapcar #'(lambda (input) 
                                  (make-instance 'input-funbox
                                    :name (or (frame-name input) (name input))
                                    :value (eval (defval input))
                                    :box-ref self
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


(defmethod update-doc ((box loopboxframe))
   (update-from-reference (object box)))

(defmethod close-frame ((box loopboxframe))
   "If miniview show a picture we must kill it."
   (when (EditorFrame (patch (object box)))
     (om-close-window (window (EditorFrame (patch (object box))))))
   (setf (frames (object box)) nil))



 


