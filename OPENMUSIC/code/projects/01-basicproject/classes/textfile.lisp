;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
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
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)


;-------------
;TEXTFILE CLASS

(defclass* TextFile () 
   ((ed-mode :initform "supersede" :initarg :ed-mode :accessor ed-mode)
    (eval-mode :initform "list" :initarg :eval-mode :accessor eval-mode)
    (file-name :initform nil :accessor file-name)
    (buffer-text :initform nil :accessor buffer-text))
   (:icon 203)
   (:documentation
"TextFile represents a text buffer in a visual program. It can be used to enter or collect data and is editable in a text window.

- <self> represents/returns the TextFile object. The input can be connected to a pathname to attach and fill the TextFile buffer with a file on the disk. 
(Note: use the contextual menu in order to change the TextFile attachement settings.)

- <exp-list> represents the data in the TextFile. 
As input it can be a single item (string or value) or a list, and then each item is considered as a new line in the text buffer.
As output it returns the contents of the text buffer as a list formatted according to <eval-mode>

- <ed-mode> determines how additional data is filled into the text buffer when the box is evaluated. \"append\" means that the new data is added at the end of the buffer without deleting its contents. \"supersede\" means that the current contents is removed and replaced by the new values.

- <eval-mode> determines how <exp-list> is formatted at evaluation. The available options are 
    - 'text' : each line is interpreted as a string (returns a list of strings)
    - 'data list' : each line is collected in a list  (returns a list of lists)
    - 'list' : ignores line breaks and returns a flat list (returns a list)
    - 'value' : returns the first value read (atom or list)
"
))

(defmethod get-super-default-value ((type (eql 'TextFile)))
  (let ((tf (make-instance 'textfile)))
    ;(setf (buffer-text tf) (om-make-buffer))
    tf))


 
(defmethod get-type-of-ed-box ((self TextFile))  'OMTextFilebox)
(defmethod editor ((self t)) self)


;;; c'est trop long avec exp-list...
(defmethod draw-obj-in-rect ((self textfile) x x1 y y1 edparams view)
   (let ((fontsize 10) 
         (fontface (om-font-face (om-get-font view)))
         ; (elist (ignore-errors (exp-list self)))
         (lines (list-of-lines (buffer-text self))))
         
     (when (buffer-text self)
       (om-with-focused-view view
         (om-with-font (om-make-font fontface fontsize)  
                       ;(if (and (not elist) (not (equal (om-buffer-text (buffer-text self)) "")))
                       ;    (om-draw-string 0 20 (om-buffer-text (buffer-text self)))
                         (loop for item in lines ; elist
                               for i = 1 then (+ i 1)
                               while (< (+ (* i 12) y) y1) do
                               (om-draw-string (+ x 5) (+ (* i 12) y) item) ;(format nil "~D " item))
                               )
                        ; )
         )))))

(defmethod get-slot-in-out-names ((self TextFile))
   (values '("self" "exp-list" "ed-mode" "eval-mode")
           '(nil nil "supersede" "list")
           '("object" "input data or text" "append or supersede" "eval interpretation mode")
           '(nil nil (( 2 (("append" "append")  ("supersede" "supersede"))))
                 (( 3 (("text" "text")  ("data list" "data") ("list" "list") ("value" "value")))))))


(defmethod* objfromobjs ((self string) (type textfile))
   (load-textfile (pathname self) 'textfile "supersede"))

(defmethod* objfromobjs ((self pathname) (type textfile))
   (load-textfile self 'textfile "supersede"))

(defmethod* objfromobjs ((self null) (type textfile))
   type)


(defmethod get-obj-from-file ((type (eql 'txt)) filename)
  (load-textfile filename  'textfile "supersede"))

;;;==================================

;;; put a list of text in the textfile buffer
;;; mode supersede : reinitialize the buffer
;;; mode append : append text at the end
(defmethod add/replace-to-buffer ((self TextFile) list)
  (unless (buffer-text self)
    (setf (buffer-text self) (om-make-buffer)))
  (when (string-equal (ed-mode self) "supersede")
    ;(om-buffer-delete (buffer-text self))
    (om-kill-buffer (buffer-text self))
    (setf (buffer-text self) (om-make-buffer)) 
    )
  (let ((pos (om-buffer-size (buffer-text self)))
        (str (format nil "~D" (car list))))
    (om-buffer-insert (buffer-text self) str pos)
    (setf pos (+ pos (length str)))
    (loop for item in (cdr list) do
          (om-buffer-insert-newline (buffer-text self) pos)
          (setf str (format nil "~D" item))
          (om-buffer-insert (buffer-text self) (format nil "~D" item) (+ 1 pos))
          (setf pos (+ pos (length str) 1))
          )
    (om-buffer-insert-newline (buffer-text self) pos)
    (when (file-name self) 
      (om-buffer-write-file (buffer-text self) (file-name self)))
    (buffer-text self)))

(defmethod (setf exp-list) ((exp-list list) (self TextFile))
   (setf exp-list (list! exp-list))
   (add/replace-to-buffer self exp-list))

(defmethod (setf buffer-text) :before ((buffer t) (self TextFile))
  (when (buffer-text self)
     (om-kill-buffer (buffer-text self))))



;;; new
(defmethod exp-list ((self TextFile))
  (when (buffer-text self)
    (let ((lista 
           (cond ((string-equal (eval-mode self) "text")
                  (list-of-lines (buffer-text self)))
                 ((string-equal (eval-mode self) "data")
                  (list-of-data (buffer-text self)))
                 ((string-equal (eval-mode self) "list")
                  (data-from-line (om-buffer-text (buffer-text self))))
                 ((string-equal (eval-mode self) "value")
                  (read-from-string (om-buffer-text (buffer-text self)) nil nil)))))
      ;;; print un warning si exp-list est nul alors que le buffer contient du texte
      (when (and (not lista) (not (string-equal (om-buffer-text (buffer-text self)) "")))
        (print (format nil "Warning : ~%An error occured while evaluating exp-list of the textfile")))
      lista)))

        
;(defun list-of-lines (buffer)
;  (when buffer
;    (let ((numlines (om-lines-in-buffer buffer))
;          (pos 0) rep)
;      (loop for i from 1 to numlines do
;           (let ((start (om-buffer-line-start buffer pos))
;                 (end (om-buffer-line-end buffer pos)))
;              (push (om-buffer-substring buffer start end) rep)
;              (setf pos (+ 1 end))))
;      (reverse rep))))

(defun list-of-lines (buffer)
  (when buffer
    (om-buffer-lines buffer)))

;(defun list-of-data (buffer)
;  (when buffer
;    (let ((numlines (om-lines-in-buffer buffer))
;         (pos 0) line rep)
;     (loop for i from 1 to numlines do
;            (let ((start (om-buffer-line-start buffer pos))
;                  (end (om-buffer-line-end buffer pos)))
;              (setf line (om-buffer-substring buffer start end))
;              (push (data-from-line line) rep)
;              (setf pos (+ 1 end))))
;      (remove nil (reverse rep)))))

(defun list-of-data (buffer)
  (when buffer
    (remove nil (loop for line in (om-buffer-lines buffer) 
                      collect (data-from-line line)))))
    

(defun data-from-line (line)
  (let ((linel (length line))
        (pos 0) rep v)
    (loop while (and pos (< pos linel))
          do
          (multiple-value-bind (val p) 
              (ignore-errors (read-from-string line nil nil :start pos))  ;;; in case of error => NIL 
            (setf pos (if val p nil))
            (when val (push val rep))
            ))
    ;(if (= 1 (length rep)) (car rep) (reverse rep))
    (reverse rep)
    ))


;--------------Copy & Save----------------

(defmethod omng-copy ((self TextFile))
   `(let ((newtextfile (make-instance ',(type-of self)
                           :ed-mode ,(ed-mode self) :ed-mode ,(ed-mode self))))
        (setf (buffer-text newtextfile) ,(om-copy-buffer (buffer-text self)))
        (setf (file-name newtextfile) ,(file-name self))
        newtextfile))
    
(defmethod omNG-save ((self TextFile) &optional (values? nil))
  (declare (ignore values?))
  (when (file-name self) 
    (register-resource :text (file-name self)))
  (if (file-name self)
      `(load-textfile ,(om-save-pathname-relative (file-name self)) 
                      ',(type-of self) ,(ed-mode self) ,(eval-mode self))
    `(load-buffer-textfile ',(list-of-lines (buffer-text self)) 
                           ',(type-of self) ,(ed-mode self) ,(eval-mode self))))

(defun load-textfile (filename class edmode &optional (evmode "text"))
  (let ((newtextfile (make-instance class
                       :ed-mode edmode :eval-mode evmode)))
    (setf (buffer-text newtextfile) (om-make-buffer))
    (if (and filename (pathnamep filename) (probe-file (restore-path filename)))
      (progn
        (om-buffer-insert-file (buffer-text newtextfile) (restore-path filename))
        (setf (file-name newtextfile) (restore-path filename)))
      (om-beep-msg (string+ "I didn't open the file " (namestring filename))))
    newtextfile))

;;; !!! les lignes..
(defun load-buffer-textfile (listline class edmode &optional (evmode "text"))
  (let ((newtextfile (make-instance class
                                    :ed-mode edmode
                                    :eval-mode evmode)))
    (setf (buffer-text newtextfile) (om-make-buffer))
    (loop for item in listline do
          (om-buffer-insert (buffer-text newtextfile) item)
          (om-buffer-insert-newline (buffer-text newtextfile))
          )
    newtextfile))




;==================EDITOR===============
;;; TEXT EDITOR IS A SPECIAL CASE OF EDITOR 
;;; DIRECTLY MANAGED BY OM-API

(omg-defclass TextEditorWindow (om-text-edit-window) 
   ((object :initform nil :initarg :object :accessor object)
    (ref :initform nil :initarg :ref :accessor ref)))

(defmethod editor ((self TextEditorWindow)) self)

(defmethod Class-has-editor-p ((self TextFile)) t)

(defmethod get-editor-class ((self TextFile)) 'TextEditorWindow)

(defmethod make-editor-window ((class (eql 'TextEditorWindow)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
  (open-new-textedit object ref))

;;; no EDITOR slot
;;; editorframe is the window itself
(defmethod open-new-textedit ((self TextFile) box) 
  (unless (buffer-text self)
    (setf (buffer-text self) (om-make-buffer)))
  (setf (editorframe box) (om-make-window 'TextEditorWindow
                                          :object self
                                          :ref box
                                          :font (om-make-font "courier" 20)
                                          :window-title (if (file-name self) 
                                                            (namestring (file-name self))
                                                          "TextFile")
                                          :size (om-make-point 500 400)
                                          :position :centered
                                          :ask-for-save nil
                                          :editor-buffer (buffer-text self)
                                          :editor-file (file-name self)
                                           ))
  (om-add-menu-to-win (editorframe box))
  (editorframe box))




;; (defmethod get-editor-class ((self TextFile)) 'textfileEditor)

;(defmethod make-editor-window ((class (eql 'textfileEditor)) object name ref &key 
;                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil))
;   (om-make-window 'TextEditorWindow
;                   :object object
;                   :ref ref
;                   :position :centered
;                   :size winsize
;                   :textbuffer (buffer-text object)
;                   :ask-for-save t))

(defmethod om-window-close-event :after ((self TextEditorWindow)) 
  (cond ((null (ref self)) nil)
        ((slot-initform-p (ref self))
         (change-initform-ed (ref self) (object self)) )
        ((EditorView-p (ref self))
         (setf (attached-editors (ref self)) 
               (remove self (attached-editors (ref self)) :test 'equal))
         (om-invalidate-view (ref self) t))
         ((ominstance-p (ref self))
          (setf (Editorframe (ref self)) nil))
         ((is-boxpatch-p (ref self)) 
          (setf (Editorframe (ref self)) nil)
          (om-invalidate-view (car (frames (ref self)))))))

(defmethod update-editor-after-eval ((self TextEditorWindow) val)
   (setf (object self) val)
   (when (buffer-text val)
     (om-set-text-buffer self (buffer-text val) t)))


;==================BOX EDITOR================
(defclass OMTextFilebox (OMBoxEditCall) ())

; ((get-slot-in-out-names-from-class 'textfile)
(defmethod correct-box-inputs ((class (eql 'textfile)) inputs)
  (loop for input in (get-inputs-from-inst (make-instance class))
        for i = 0 then (+ i 1) collect
        (let ((inp 
               (if (and (nth i inputs) (string-equal (name input) (name (nth i inputs))))
                   (nth i inputs) input)))
          ;;; ici : mettre à jour les items (thepopup) pour les input-funmenu
          inp)))


(defmethod OpenEditorframe ((self OMTextFilebox))
  (unless (lock-button (car (frames self)))
    (add-lock-button (car (frames self))))
  (or (editorframe self) (open-new-textedit (value self) self)))

 (defmethod OpenObjectEditor ((self OMTextFilebox)) 
   "If there is a EditorFrame open for SELF select the window of EditorFrame, 
else create a new Editor frame, and select its window."
   (setf (EditorFrame self) (OpenEditorframe self))
   (when (EditorFrame self)
     (om-select-window (Editorframe self))))

(defmethod numouts ((self OMTextFilebox)) 4)

(defmethod remove-extra ((self OMPatch) (box OMTextFilebox))
  (when (EditorFrame box) (om-close-window (EditorFrame box)))
  (when (and (value box) (buffer-text (value box)))
    (om-kill-buffer (buffer-text (value box)))))

(defmethod (setf value) :before ((val TextFile) (self OMTextFilebox)) nil)
;  (when (and (value self) (buffer-text (value self)))
;    (om-kill-buffer (buffer-text (value self)))))

(defmethod fill-buffer-textfile ((self OMTextFilebox))
   (om-buffer-insert-file (buffer-text (value self)) (file-name (value self))))

;-----------------Evaluation&code generation-------


(defmethod omNG-box-value ((self OMTextFilebox) &optional (numout 0))
      (let ((intype (omNG-box-value (third (inputs self))))
            (ev-mode (if (fourth (inputs self)) (omNG-box-value (fourth (inputs self))) "text")))
        (unless (value self)
          (setf (value self) (get-super-default-value 'textfile))) 
        (setf (ed-mode (value self)) intype)
        (setf (eval-mode (value self)) ev-mode)
       (cond
        ((equal (allow-lock self) "o") self)
        ((equal (allow-lock self) "l") "sorry not lambda mode for textfile boxes")
        ((and (equal (allow-lock self) "x") (value self)) (rep-editor (value self) numout))
        ((and (equal (allow-lock self) "&") (ev-once-p self)) (rep-editor (value self) numout))
        (t (let ((val (omNG-box-value  (second (inputs self))))
                  (newtextfile (make-instance (type-of (value self))
                                         :ed-mode intype
                                         :eval-mode ev-mode
                                         )))
             (when (editorframe self)
               (om-close-window (om-view-window (editorframe self)))
               (om-kill-buffer (buffer-text (value self))))
             (if (connected? (first (inputs self)))
                 (let* ((inv (omNG-box-value (first (inputs self))))
                       (val (objfromobjs inv newtextfile)))
                   (if val
                     (setf (value self) val
                           (eval-mode (value self)) ev-mode
                           (ed-mode (value self)) intype)
                     (progn
                       (om-beep-msg (format nil "Error: ~A can not be converted to TextFile !" (type-of inv)))
                       (om-abort))
                     ))
               (progn
                  (setf (file-name newtextfile) (file-name (value self)))
                  (setf val (list! val))
                  (add/replace-to-buffer (value self) val)
                  (if (buffer-text (value self))
                      (setf (buffer-text newtextfile) (om-copy-buffer (buffer-text (value self))))
                    (setf (buffer-text newtextfile) (om-make-buffer)))
                  ;(unless (editorFrame self)
                  ;  (om-kill-buffer (buffer-text (value self))))
                  (setf (value self) newtextfile)
                  (update-if-editor self)
                  )))
             (rep-editor (value self) numout)))))

(defmethod gen-code ((self OMTextFilebox) numout)
   (cond
    ((equal (allow-lock self) "&") 
     (gen-code-for-ev-once self numout))
    ((equal (allow-lock self) "x") 
     `(rep-editor ,(value self) , numout))
    ((equal (allow-lock self) "o") 
     `(find-class 'Textfile nil)) 
    ((equal (allow-lock self) "l") 
     (om-beep-msg "Sorry no lambda mode for textfile boxes"))
    (t  (call-gen-code self numout))))


;(defmethod gen-code-for-ev-once ((self OMTextFilebox) numout)
;   (let ((varname (read-from-string (gen-box-string self))))
;     (if (not (member varname *let-list* :test 'equal)) 
;       (progn
;         (push varname  *let-list*)
;         (if *start-repeat-generation*
;           (progn
;             (push `(setf ,varname ,(call-gen-code self numout)) *repeat-ev-once-list*)
;             varname)
;           `(progn
;              (setf ,varname ,(call-gen-code self numout))
;              ,varname)))
;       varname)))

; A VERIFIER
(defmethod gen-code-for-ev-once ((self OMTextFilebox) numout)
  (let ((varname (read-from-string (gen-box-string self))))
     (when (not (member varname *let-list* :test 'equal :key 'car))
       (push `(,varname (multiple-value-list ,(call-gen-code self numout)))  *let-list*))
    varname))


(defmethod gen-new-val ((self TextFile) val mode numout)
   (setf (ed-mode self) mode)
   (setf val (list! val))
   (add/replace-to-buffer self val)
   (rep-editor self numout))

(defmethod call-gen-code ((self OMTextFilebox) numout)
   (if (connected? (first (inputs self)))
      `(let ((textfile (objfromobjs ,(gen-code (first (inputs self)) 0) ,(value self))))
         (setf (ed-mode textfile) ,(gen-code (third (inputs self)) 0))
         (setf (eval-mode textfile) ,(gen-code (fourth (inputs self)) 0))
         (rep-editor textfile ,numout))
   `(gen-new-val ,(value self) ,(gen-code (second (inputs self)) 0) ,(gen-code (third (inputs self)) 0) ,numout)))



;---------------------Menu Contextual--------


(omg-defclass textEditorFrame (boxEditorFrame) ()
   (:documentation "Simple frame for OMTextFilebox meta objects. #enddoc#
#seealso# (OMTextFilebox) #seealso#"))

(defmethod get-frame-class ((self OMTextFilebox)) 'textEditorFrame)

(defmethod om-get-menu-context ((object texteditorframe))
   "Uhmmm If this method had been specialized by the class and not by the frame ?"
   (list+  (boxframe-default-list object)
           (list (list 
                  (om-new-leafmenu "Import Contents From File" #'(lambda () (import-box-editor (object object))))
                  (om-new-leafmenu "Export Contents To File" #'(lambda () (export-box-editor (object object))))
                  )
                 (om-new-leafmenu "Import and Attach File" #'(lambda () (load-editor-frommenu (object object))))
                 (om-new-leafmenu "Export and Attach File" #'(lambda () (save-editor-frommenu (object object)))))
           (let ((fname (file-name (value (object object)))))
             (if fname 
                 (list (om-new-leafmenu (string+ "Free Attached File" 
                                                 " ("
                                                 (pathname-name fname) "." (pathname-type fname)
                                                 ")")
                                      #'(lambda () (free-editor-from-file (object object)))))) 
           )))


; miniview
;;;======================================



(defmethod import-box-editor ((self OMTextFilebox))
    (let ((name (om-choose-file-dialog)))
      (when name
        (unless (buffer-text (value self))
          (setf (buffer-text (value self)) (om-make-buffer)))
        (if (string-equal (ed-mode (value self)) "supersede")
          (om-buffer-delete (buffer-text (value self))))
        (om-buffer-insert-file (buffer-text (value self)) name (om-buffer-size (buffer-text (value self))))
        ;;;(when (editorframe self) (update-editor-after-eval (editorframe self) (value self)))
      )))


(defmethod export-box-editor ((self OMTextFilebox))
    (when (value self)
        (let ((name (om-choose-new-file-dialog :prompt "Save the contents of this editor")))
          (when name
            (delete-file-protection name)
            (when (buffer-text (value self))
              (om-buffer-write-file (buffer-text (value self)) name :if-exists :supersede))))))
  

(defmethod load-editor-frommenu ((self OMTextFilebox))
    (let ((name (om-choose-file-dialog)))
      (when name
        (unless (buffer-text (value self))
          (setf (buffer-text (value self)) (om-make-buffer)))
        (om-buffer-delete (buffer-text (value self)))
        (om-buffer-insert-file (buffer-text (value self)) name)
        (setf (file-name (value self)) name)
        (setf (name self) (string+ (pathname-name name) "." (pathname-type name)))
        )
      ))

(defmethod save-editor-frommenu ((self OMTextFilebox))
   (when (value self)
     (let ((name (or (file-name (value self))
                     (om-choose-new-file-dialog    
                      :prompt "Save the instance of this editor"))))
       (when name
         (delete-file-protection name)
         (om-buffer-write-file (buffer-text (value self)) name :if-exists :supersede)
         (setf (file-name (value self)) name)
         (setf (name self) (string+ (pathname-name name) "." (pathname-type name)))
         ))
     ))
   
      
(defmethod free-editor-from-file ((self OMTextFilebox))
   (when (value self)
     (if (file-name (value self))
       (setf (file-name (value self)) nil
             (name self) "textfile")
       (om-beep-msg "The text file has no file attached."))))

(defmethod load-editor-frommenu ((self t))
  (om-beep-msg "Use import in this editor box"))

(defmethod save-editor-frommenu ((self t))
  (om-beep-msg "Use export in this editor box"))


(defmethod! eval-textfile ((self textfile))
    :icon 166
    :indoc '("a textfile object")
    :doc "Evaluates the contents of a TextFile (<self>).

Evaluation allows to define functions or data in Lisp and run commands or programs from the TextFile buffer.
"
  (om-buffer-eval (buffer-text self)))







      









