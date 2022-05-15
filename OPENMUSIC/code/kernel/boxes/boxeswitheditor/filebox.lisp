;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
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
;The file-box boxes are defined in this file.
;There are classes for accumulaters, iteraters, but also the graphic editor.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)
        
;===========================================================================
;PATCH

(defclass openfilepatch (patchForLoop) ()
   (:documentation "This class implements the special patches for the file-box boxes.  #enddoc#
#seealso# (OMPatch file-box-box) #seealso#"))


(defmethod OpenEditorframe ((self openfilepatch))
   (or (editorframe self)
       (panel (open-new-RelationFrame self (string+ "File-Box Loop - " (name self)) (get-elements self)))))


;(defmethod compile-patch ((self openfilepatch))
;   "Code generates by Loop patches is generate by this method."
;   (let* ((boxes (boxes self))
;          (oldletlist *let-list*)
;          (*let-list* nil)
;          (stream-box (car (find-class-boxes boxes 'Streambox)))
;          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
;          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
;          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes)))
;     (eval  `(defun ,(car (code self)) ,symbols
;                 (with-open-file ,(gen-stream-options stream-box)
;                   (let* ,*let-list*
;                     ,(gen-code final-box 0)))))
;     (setf *let-list* oldletlist)))


(defmethod compile-patch ((self openfilepatch))
  "Code generates by Loop patches is generate by this method."
  (let* ((boxes (boxes self))
         (oldletlist *let-list*)
         (*let-list* nil)
         (oldlambdacontext *lambda-context*)
         (do-box (car (find-class-boxes boxes 'OMLoopDo)))
         (final-box (car (find-class-boxes boxes 'OMFinalDo)))
         (init-box (car (find-class-boxes boxes 'OMinitDo)))
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
         (stream-boxes (find-class-boxes boxes 'Streambox))
         (streams (loop for sb in stream-boxes collect (list 
                                                       (get-stream-type sb)
                                                       (car (decode sb))
                                                       (direction sb)
                                                       (if-ex sb)
                                                       (id sb))))
         (streamids (loop for s in streams collect (nth 4 s)))
         (stream-box (car stream-boxes))
         body init)
    (setf body (gen-code do-box 0))
    (setf init (gen-code init-box 0))
    (cond
     (loop-boxes
      (eval `(defun ,(intern (string (first (code self))) :om)  (,.symbols)
                  (let (rep ,.streamids)
                  ,.(remove nil (loop for item in streams collect 
                                      (when (nth 1 item)
                                        `(setf ,(nth 4 item) (open (eval ,(nth 1 item)) 
                                                                   :if-does-not-exist :create 
                                                                   :direction ,(nth 2 item) :if-exists ,(nth 3 item)))
                                        )))

                  (let (,.acum-declaration (iter-count 0)) 
                    ,.acum-inits
                    (let* ,*let-list* ,init)
                    (setf rep (loop ,.loop-code 
                                    do ,(loop-check-code)
                                    finally (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                                    collect  (gen-code final-box i)))) do
                                    (let* ,*let-list* ,body)))
                    )
                         
                         ,.(remove nil (loop for item in streams collect 
                                             (when (nth 1 item)
                                               `(close ,(nth 4 item))
                                               )))
                         rep)
                  )))
     (t 
      (eval `(defun ,(intern (string (first (code self))) :om)  (,.symbols) 
               (let (rep ,.streamids)
                  ,.(remove nil (loop for item in streams collect 
                                      (when (nth 1 item)
                                        `(setf ,(nth 4 item) (open (eval ,(nth 1 item)) 
                                                                   :if-does-not-exist :create 
                                                                   :direction ,(nth 2 item) :if-exists ,(nth 3 item)))
                                        )))

                  (let (,.acum-declaration) ,.acum-inits
                       (let* ,*let-list* 
                         ,init
                         ,body
                         (setf rep (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                   collect  (gen-code final-box i))))))
                  
                  ,.(remove nil (loop for item in streams collect 
                                      (when (nth 1 item)
                                        `(close ,(nth 4 item)))
                                        ))
                  rep)
                ))))
    ;(compile (intern (string (first (code self)))))
    ;(print (fdefinition (intern (string (first (code self))))))
    (setf *lambda-context* oldlambdacontext)
    (setf *let-list* oldletlist)))



(defmacro with-open-loop-file (self &body body)
  `(let* ((pathname ,(car (decode self)))
          (pathname (or pathname (om-choose-file-dialog :prompt "Choose a File to Read/Write")))
          rep)
     (let ((stream (open pathname :if-does-not-exist :create :direction ,(direction self) :if-exists ,(if-ex self))))
         (setf rep ,@body)
         (close stream))
      rep))



;----------------
;EDITOR
;----------------
(defclass openfileEditor (loopEditor) ())

(defmethod get-editor-class ((self openfilepatch)) 'openfileEditor)

(defmethod get-editor-panel-class ((self openfileEditor))  'openfilePanel)

;----------------
;PANEL
;----------------
(defclass openfilePanel (loopPanel) ()
   (:documentation "This is the class for the editor panel which define a file-box box.  #enddoc#
#seealso# (file-box-box box-with-patch-frame) #seealso#
#patch# This slot keeps the patch associated to the box. #patch#"))

(defmethod omg-remove-element ((self openfilePanel) (box inframe))
   "The inputs of a loop patch can be removed only from the file-box box."
   (om-beep-msg "delete inputs in the loop box"))

(defmethod omg-remove-element ((self openfilePanel) (box t))
   "Finally and streamFile boxes can not be removed."
   (if (or ;(equal (class-name (class-of (object box))) 'Streambox)
           (equal (class-name (class-of (object box))) 'OMfinalDo))
     (om-beep-msg "These boxes can not be erased")
     (call-next-method)))

(defmethod add-window-buttons  ((self openfilePanel))
   "Add iterator and accumulator buttons to the patch."
   (call-next-method)
   (om-add-subviews self
                    (om-make-view 'button-icon
                                  :iconID 649
                                  :position (om-make-point 440 5)
                                  :size (om-make-point 24 24)
                                  :help-spec "File I/O"
                                  :action
                                  #'(lambda(item)  (declare (ignore item))
                                      (omG-add-element self
                                                       (make-frame-from-callobj 
                                                        (omNG-make-new-boxcall (fdefinition 'StreamFile) (om-make-point 50 30) 
                                                                               (mk-unique-name self "StreamFile"))))))
                    ))


;----------------
(defclass file-box-box (omloop-box) 
   ()
   (:documentation "This is the class for the file-box boxes.#enddoc#
#seealso# (box-with-editor loopIterators acumboxes) #seealso#"))


(defmethod initialize-instance :after ((self file-box-box) &key controls)
   (declare (ignore controls))
   (let ((thestream (omNG-make-new-boxcall (fdefinition 'StreamFile) (om-make-point 50 30) "StreamFile"))
         (theinit (omNG-make-new-boxcall (fdefinition 'initDo) (om-make-point 110 250) "init"))
         )
     (omNG-add-element (patch self) theinit)
     (omNG-add-element (patch self) thestream)))

(defmethod openeditorframe ((self file-box-box))
   (openobjecteditor (patch self)) nil)

(defmethod get-patch-editor-class ((self file-box-box)) 'openfilepatch)


;;;=====================================================
;;; DEJA DEFINI DANS OMLOOP
#|
(defmethod call-gen-code ((self file-box-box) numout)
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     (if (zerop numout) 
       `(,(first (code (patch self))) ,.in-list)
       `(nth ,numout (multiple-value-list (,(first (code (patch self))) ,.in-list))))))

(defmethod gen-code-call ((self file-box-box))
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     `(,(first (code (patch self))) ,.in-list)))

(defmethod special-lambda-value ((self file-box-box) symbol)
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

(defmethod curry-lambda-code ((self file-box-box) symbol)
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

(defmethod special-value ((self  file-box-box) &optional (args nil))
  (unless (compiled? (patch self)) 
    (compile-patch (patch self))
    (setf (compiled? (patch self)) t))
  (apply (fdefinition (intern (string (first (code (patch self)))) :om)) args))
|#
;;;
;;;=====================================================

(defmethod get-boxcallclass-fun ((self (eql 'file-box))) 'file-box-box)


(defmethod! file-box  (&rest oplist) 
   :numouts 1 
   :initvals '(nil) :indoc '("aditional input")
   :doc "Iterative process with file input/output.

The File-Box allows to create iterative processes that can read or write files.

Inputs can be added/removed as optional box inputs (alt + '->'/'<-' or '>'/'<').
Open the box (db click) to design an iterative process in the loop editor. 
The internal box 'streamFile' represents the file to read/write. Use the functions file-read-line, file-write, file-write-line to read or write.
The File-Box retruns the values connected to the internal box 'Finally'.

See OM User Manual for more details." 
   :icon 649 


   (declare (ignore oplist)) nil)

;=================================================
; STREAM BOX




(defclass Streambox (OMBoxcall) 
  ((filetype :accessor filetype :initarg :filetype :initform 'text)
   (direction :accessor direction :initarg :direction :initform :io)
   (if-ex :accessor if-ex :initarg :if-ex :initform :rename)
   (id :accessor id :initform (gensym)))
   (:documentation "This is the class for the method exception box OMOR.#enddoc#
#seealso# (OMBoxcall) #seealso#"))


(defmethod omNG-save ((self Streambox) &optional (values? nil))
  "Save a box"
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (saveValueinBox (value self))))
    `(let ((box (om-load-boxcall ',(saveBox? self) ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self) ,(numouts self))))
       (setf (filetype box) ',(filetype self)
             (direction box) ,(direction self)
             (if-ex box) ,(if-ex self))
       box)
    ))

(defmethod omNG-copy ((self Streambox))
  `(let* ((copy ,(omNG-make-new-boxcall (fdefinition 'StreamFile) (frame-position self) (name self))))
     (setf copy (update-boxes ,self copy))
     (setf (filetype copy) ',(filetype self))
     (setf (direction copy) ,(direction self))
     (setf (if-ex copy) ,(if-ex self))
     copy))


(defmethod no-allow-copy-p ((self Streambox))
   "Finally boxes can not be directly copied" "StreamBoxes.")

(defmethod get-boxcallclass-fun ((self (eql 'StreamFile))) 'Streambox)

(defmethod OpenEditorframe ((self Streambox))
  (or (editorframe self)
      (open-stream-dialog self)))

(defmethod! StreamFile  ((pathname t)) 
   :numouts 1 
   :initvals '(nil) :indoc '("pathname or string")
   :doc "file stream" 
   :icon 649
   (id self))


(defmethod omNG-box-value ((self Streambox) &optional (numout 0))
   (cond
    ((equal (allow-lock self) "l") (special-lambda-value self 'omor))
    ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth numout (value self)))
    (t (let ((rep (omNG-box-value (first (inputs self)))))
         ;(loop while (not rep)
         ;      for item in (cdr (inputs self)) do
         ;      (setf rep (omNG-box-value item))) 
         (unless rep
           (setf rep (om-choose-file-dialog :prompt (format nil "Choose a File to ~A" (cond ((equal :input (direction self)) "Read")
                                                                                            ((equal :output (direction self)) "Write")
                                                                                            (t "Read/Write")))
                                                                                            
                                            )))
         ;(when (or (null rep) (not (pathnamep (pathname rep))))
         ;  (om-message-dialog "Error in file pathname. Please specify a valid path for each of the StreamBoxes")
          ; (om-abort)
          ; )
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) (list rep)))
         (when (equal (allow-lock self) "x")
           (setf (value self) (list rep)))
         rep))))

(defmethod get-stream-type ((self Streambox)) (filetype self))

(defmethod call-gen-code ((self Streambox) numout)
   (declare (ignore numout))
   (id self)
   ;'stream
   )

(defmethod gen-code-call ((self Streambox))
   (call-gen-code self 0))
  
(defmethod special-value ((self  Streambox) &optional (args nil))
   (declare (ignore numout))
   (om-beep-msg "Close this editor in order to read or write a file."))

(defmethod gen-stream-options ((self  Streambox))
   `(stream (let ((pathname ,.(decode self)))
              (or pathname (choose-file-dialog))) 
            :direction :io  
            :if-does-not-exist :create 
            :if-exists :overwrite
            :element-type 'character
            :external-format :TEXT))


;Editor
(defclass streamwindow (editorwindow) ())


(defmethod get-editor-class ((self streamwindow)) 'streamEditor)

;;;(defmethod view-activate-event-handler :after ((self streamwindow))
;;;   (update-themenubar self))

(defmethod om-view-click-handler :before ((self streamwindow) where)
  (when (text-view (editor self))  ;;; and  (not (equal (find-clicked-subview self where) (text-view (editor self))))) 
    (exit-from-dialog (text-view (editor self)) (dialog-item-text (text-view (editor self))))))

(defclass StreamEditor (editorView) ())

(defmethod handle-key-event ((self StreamEditor) char)  nil)


;(defmethod om-window-close-event  :after ((self streamwindow))
;   (setf (editorFrame (object (editor self))) nil))
(defmethod close-editorFrame ((self streamEditor))
  (setf (editorFrame (object self)) nil))


(defun open-stream-dialog (streambox)

   (let ((dialog (make-editor-window 'streameditor streambox (name streambox) nil
                                     :winpos :centered
                                     :winsize (om-make-point 280 165)
                                     :resize nil
                                     ))
         ifexists ifextext)
     (om-add-subviews (editor dialog)
                      (om-make-dialog-item 'om-radio-button (om-make-point 13 11) (om-make-point 95 15)
                                                  "Read/Write"
                                                  :di-action (om-dialog-item-act item (setf (direction streambox) :io))
                                                  :checked-p (equal :io (direction streambox))
                                                  )
                      (om-make-dialog-item 'om-radio-button
                                           (om-make-point 115 11)  (om-make-point 70 16) "Write"
                                           :di-action (om-dialog-item-act item (setf (direction streambox) :output))
                                           :checked-p (equal :output (direction streambox))
                                           )
                      (om-make-dialog-item 'om-radio-button
                                           (om-make-point 199 11) (om-make-point 70 16) "Read"
                                           :di-action (om-dialog-item-act item (setf (direction streambox) :input))
                                           :checked-p (equal :input (direction streambox))
                                           )

                      (om-make-dialog-item 'om-static-text (om-make-point 10 50)
                                           (om-make-point 100 20)
                                           "File Type" :font *controls-font*)
                      (om-make-dialog-item 'om-pop-up-dialog-item
                                  (om-make-point 100 50)
                                  (om-make-point 150 20)
                                  ""
                                  :range (if (find :sdif *features*) (list "TEXT" "SDIF") (list "TEXT"))
                                  :value (string (filetype streambox)) 
                                  :di-action (om-dialog-item-act item 
                                               (setf (filetype streambox) (intern (om-get-selected-item item)))
                                               (om-enable-dialog-item ifexists (not (equal "SDIF" (om-get-selected-item item))))
                                               (om-set-fg-color ifextext (if (equal "SDIF" (om-get-selected-item item)) *om-gray-color* *om-black-color*)))
                                                 
                                  )
                      
                      ;(om-make-dialog-item 'om-pop-up-dialog-item
                      ;                     (om-make-point 10 90)
                      ;                     (om-make-point 275 20)
                      ;                     "Element type "
                      ;                     :range (list "Character" "Signed-byte" "Bit" "Unsigned-byte")
                      ;                     )
                      

                      (setf ifextext (om-make-dialog-item 'om-static-text (om-make-point 10 80)
                                           (om-make-point 100 20)
                                           "If File Exists"
                                            :font *controls-font*
                                           :fg-color (if (equal 'sdif (filetype streambox)) *om-gray-color* *om-black-color*)))
                      
                      (setf ifexists (om-make-dialog-item 'om-pop-up-dialog-item
                                           (om-make-point 100 80)
                                           (om-make-point 150 20)
                                           ""
                                           :range (list "Rename" "Supersede" "Overwrite" "Append")
                                           :value (string (if-ex streambox))
                                           :di-action (om-dialog-item-act item (setf (if-ex streambox) 
                                                                                     (nth (om-get-selected-item-index item) '(:rename :supersede :overwrite :append)))))
                            )
                      )

     (om-enable-dialog-item ifexists (not (equal 'sdif (filetype streambox))))
     
     (editor dialog))
   )


;;; WRITE BOX



(defmethod! file-write-line ((line string) stream)
  :indoc '("a line to write" "a file pointer")
  :icon 908
  :doc "Writes <line> in <stream> as a new line (i.e. with a line return).


<stream> is a file pointer represented by the 'streamFile' box in the File-Box editor."
  (write-line line stream))

(defmethod! file-write-line ((line symbol) stream)
  :icon 908
  (write-line (string line) stream))

(defmethod! file-write-line ((line number) stream)
  :icon 908
  (write-line (format nil "~D" line) stream))

(defmethod! file-write-line ((line character) stream)
  :icon 908
  (write-line (string line) stream))

(defmethod! file-write-line ((line t) stream)
  :icon 908
  (om-print "write only numbers and strings"))

;;============

(defmethod! file-write ((line t) stream)
  :indoc '("something to write" "a file pointer")
  :icon 908
  :doc "Writes <line> in <stream> (with no additional line return).

<stream> is a file pointer represented by the 'streamFile' box in the File-Box editor."
  (om-print "write only numbers symbols and strings"))

(defmethod! file-write ((line string) stream)
  :icon 908
  (write-string line stream))

(defmethod! file-write ((line symbol) stream)
  :icon 908
  (write-string (string line) stream))

(defmethod! file-write ((line number) stream)
  :icon 908
  (write-string (format nil "~D" line) stream))

(defmethod! file-write ((line character) stream)
  :icon 908
  (write-char line stream))

;;============

(defmethod! file-read-line (stream)
  :indoc '("a file pointer")
  :icon 908
  :doc "Reads a line in <stream>.

<stream> is a file pointer represented by the 'streamFile' box in the File-Box editor."
  (read-line stream nil nil))

(defmethod! file-eof-p (stream)
  :indoc '("a file pointer")
  :icon 908
  :doc "Check if <stream> is at the end of the file.

<stream> is a file pointer represented by the 'streamFile' box in the File-Box editor."
  (stream-eofp stream))



