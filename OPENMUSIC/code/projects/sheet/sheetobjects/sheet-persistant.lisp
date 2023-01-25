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
;===========================================================================

; SHEET package by C. Agon & J. Bresson

(in-package :om)


;;;=====================================
;;; Objet Sheet Persistant

(defclass* sheet-document (omsheet OMPersistantObject) ())

(defmethod omNG-make-new-persistant ((type (eql 'sh)) frame pos) 
  (omNG-make-new-sheet (mk-unique-name frame "Sheet") pos))

(pushr (list "New Sheet" 'sh nil) *new-menu-items*)

(defun omNG-make-new-sheet (name &optional (posi (om-make-point 0 0)))
   "Make an instance of patch."
   (let ((newpatch (make-instance 'sheet-document :name name :icon 127)))
     (set-icon-pos newpatch posi)
     newpatch))

(defmethod mkobjfromtype ((type (eql :omsh))) 'omNG-make-new-sheet)

(defmethod ws-load-more-element ((type (eql :omsh))  path name) 
  (let ((newobj (omNG-make-new-sheet name)))
    (setf (mypathname newobj) path)
    (setf (loaded? newobj) nil)
    newobj))

(defmethod obj-file-type ((self sheet-document)) :OMSH)

(defmethod get-object-insp-name ((self sheet-document)) "sheet")

(defmethod save-all-persistants ((self sheet-document))
   "Save a patch at the end of the session."
   (unless (saved? self) (omng-save self)))

(defmethod obj-file-extension ((self sheet-document)) "oms")


;;; OPEN / LOAD

(defmethod load-abstraction-attributes ((self sheet-document) currentpersistent)
  (setf (inside self) (inside currentpersistent))
  (setf (patch-list self) (patch-list currentpersistent)))

(defmethod OpenEditorframe ((self sheet-document))
   "Open the patch editor, this method open too all persistantes objects referenced into the patch."
   (declare (special *om-current-persistent*))
   (load-patch self)
   (or (editorframe self)
       (editor 
        (make-editor-window (get-editor-class self) self 
                            (if (and (mypathname self) (not (saved? self))) (string+ "^" (name self)) (name self)) 
                            self
                            :winsize (get-win-size self) 
                            :winpos (get-win-position self)
                            ))))


(defmethod load-patch ((self sheet-document))
  (declare (special *om-current-persistent*))
  (unless (loaded? self)
    (om-with-error-handle 
      (setf *loaading-stack* nil)
      (setf *loading-old-patches* nil)
      (setf *om-current-persistent* nil)
      (om-print (string+ "Loading sheet... " (namestring (mypathname self))))
      ;;(push self *loaading-stack*)
      (om-with-cursor *om-wait-cursor* 
        (eval-non-text-file (mypathname self))
        (when *om-current-persistent*
          (let ((up? (version-warning (omversion *om-current-persistent*))))
            (if up?
                (progn
                  (when *om-current-persistent*
                    (setf *om-search-all* nil 
                          *user-search-all* nil
                          *file-search-all* nil)
                    (load-abstraction-attributes self *om-current-persistent*)
                    (when (< (omversion *om-current-persistent*) *om-version*)
                      (corrige-version self (omversion *om-current-persistent*))))
                  (update-patches-pile)
                  (setf *om-current-persistent* nil))
              (om-abort)))))))
  self)

;;; SAVE

(defmethod omNG-save-ws ((self sheet-document))
  (unless (saved? self)
    (if (or *save-apply-all* (om-y-or-n-dialog (string+ "Save modifications in " (name self) "?")))
      (omNG-save self nil)))
  (set-finder-comment (mypathname self) self))


(defmethod update-last-saved ((self sheet-document))
   (when (editorframe self)
     (om-close-window (window (editorframe self))))
   (setf (loaded? self) nil)
   (OpenobjectEditor self))

(defmethod omNG-save ((self sheet-document) &optional (values? nil))
  "Save the patch <self> in the file <mypathname> if it is not null."
  (if (not (mypathname self))
      (dialog-message (str-check "Error: the Sheet has no attached pathname"))
    (let ((tempfile (om-put-path-extension (mypathname self) (string+ (obj-file-extension self) ".tmp"))))
      (when (probe-file (mypathname self))
        (rename-file (mypathname self) tempfile))
      (delete-file-protection (mypathname self))
      (unless (string-equal (pathname-type (mypathname self)) (obj-file-extension self))
        (setf (mypathname self)  (om-put-path-extension (mypathname self) (obj-file-extension self))))

      (setf *saving-patch* self)
      (setf *libs-to-load* nil)
      (setf *resources-to-load* nil)

      (with-open-file (out (mypathname self) :direction :output :external-format :utf-8
                           :if-does-not-exist :create :if-exists :supersede) ;;;; :external-format :PATC)
        (handler-bind 
            ((error #'(lambda (err)
                        (capi::display-message "An error of type ~a occurred: ~a~%~%File ~s could not be saved." 
                                               (type-of err) (format nil "~A" err) (mypathname self))
                        (close out)
                        (rename-file tempfile (mypathname self))
                        (setf *saving-patch* nil)
                        (setf *libs-to-load* nil)
                        (setf *resources-to-load* nil)
                        (abort err))))

          (write-header self out)
          (let ((*package* (find-package :om))
                (file-code (om-save self values?)))
            (write-resources self *resources-to-load* out)
          
            (prin1 '(in-package :om) out)
            (prin1 `(load-lib-for ',(remove-duplicates *libs-to-load* :test 'string-equal)) out)
            (prin1 file-code out))
          )
      
        (setf (saved? self) t)
        (setf *saving-patch* nil)
        (setf *resources-to-load* nil)
        (setf *libs-to-load* nil))
      
      (when (and (mypathname self) (probe-file (mypathname self)))
        (when (editorframe self) (om-set-window-title (window (editorframe self)) (name self)))
        (om-delete-file tempfile)
        (om-print (string+ (namestring (mypathname self)) " saved.")))
    
      t)))

(defmethod om-save ((self sheet-document) &optional (values? nil))
   (let* ((voices (mapcar #'(lambda (v) (omNG-save v values?)) (voices self)))
          (patches (omng-save (patch-list self))))
     `(setf *om-current-persistent* (om-load-sheet ,(name self) ',voices ',patches ,*om-version*))
     ))

(defun om-load-sheet (name voices patches version)
  (let* ((newsheet (omNG-make-new-sheet name)))
    (setf (inside newsheet) (loop for v in voices collect (eval v)))
    (setf (patch-list newsheet) (loop for v in patches collect (eval p)))
    (when version (setf (omversion newsheet) version))
    newsheet))


#|
;;; A FAIRE......
;;;;;;
(defmethod omNG-copy ((self sheet-document))
  (let ((obj (call-next-method)))
    `(let ((copy ,obj))
       (set-icon-pos copy ,(om-copy-point (get-icon-pos self)))
       (setf (omversion copy) ,(omversion self))
       copy)))

;------------------------------------------------------------------------
; BOXES
;------------------------------------------------------------------------
; Workspace
(defclass sheet-icon-frame (patch-icon-frame) ())
(defclass sheet-finder-icon (icon-finder-icon) ())

(defmethod get-class-icon ((self sheet)) 'sheet-icon-frame)
(defmethod get-class-icon-icon ((self sheet)) 'sheet-finder-icon)

; Patch
(defclass OMBoxSheet (OMBoxcall)  () 
   (:documentation "Boxes having a persistant sheet as reference.")
   (:metaclass omstandardclass))

(defmethod get-object-insp-name ((self OMBoxSheet)) "Sheet Box")

(defmethod allow-rename ((self OMBoxSheet)) t)

(defmethod omNG-save ((self OMBoxSheet) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (saveValueinBox (value self))))
    (register-resource :abstraction (mypathname (reference self)))
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(list+ (get-relative-path (reference self)) 
                                                              (list (name (reference self))))
                      ',inputs ,(om-save-point (frame-position self) )
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self) ,(numouts self))))

(defmethod saveBox? ((self OMBoxSheet)) 'sheet)
(defmethod mk-object-refer ((self (eql 'sheet)) reference) (mk-object-refer 'patch-box reference))
   
(defmethod load-obj-from-obj ((object sheet-persistant))
   (if (or (loaded? object) (member object *loaading-stack* :test 'equal)) object
       (om-with-cursor *om-wait-cursor* 
         (push object *loaading-stack*)
         (om-print (string+ "Loading..." (namestring (mypathname object))))
         (eval-non-text-file (mypathname object))
         (if *om-current-persistent*
            (progn
              (setf (inside object) (inside *om-current-persistent*))
              (setf (params object) (params *om-current-persistent*))
              (setf (mus-patch object) (mus-patch *om-current-persistent*))
              (setf *om-current-persistent* nil)
             object)
           'dead))))

(defmethod omNG-copy ((self OMBoxSheet))
  `(let ((copy ,(omNG-make-new-boxcall (reference self)
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     copy))

;--------------Inits
(defclass sheetboxframe (boxframe) ()
   (:documentation "Simple frame for OMBoxmaquette boxes. #enddoc#
#seealso# (OMBoxmaquette) #seealso#"))

(defmethod add-lock-button ((self sheetboxframe) &optional (icon 167))
   "Not lambda mode for maquette boxes."
   (when (allow-lock-button (object self))
     (setf (lock-button self)  (om-make-view 'lock-button
                                             :IconID icon
                                             :size (om-make-point 10 10)
                                             :position (om-make-point 0 0) ;;;(om-make-point (x (iconview self)) 8)
                                             :owner (iconview self)   ;;; self
                                             :action #'(lambda (item)
                                                         (case (iconID item)
                                                           (166 (setf (iconID item) 167)
                                                                (setf (allow-lock (object self)) "x"))
                                                           (167 (setf (iconID item) 168)
                                                                (setf (allow-lock (object self)) "&"))
                                                           (168  (setf (iconID item) 166)
                                                                 (setf (allow-lock (object self)) "o")))
                                                         (om-draw-contents item))))
     (om-invalidate-view self)
     (setf (allow-lock (object self)) (get-str-lock icon))))


(defmethod numouts ((self OMBoxSheet)) 1)

(defmethod get-frame-class ((self OMBoxSheet)) 'sheetboxframe)
(defmethod get-documentation ((self OMBoxSheet)) "Cons or modify a sheet")
(defmethod get-frame-name ((self OMBoxSheet)) (string-downcase (name (reference self))))

(defmethod change-name-box ((self sheetboxframe))
   "If 'self is a global variable you can not change its name."
   (if (mypathname (reference (object self)))
       (om-beep-msg "This is a persistant sheet. It can only be renamed from workspace folders.")
     (call-next-method)))

(defmethod omNG-make-new-boxcall ((self Sheet-Persistant) posi name)
  (let* ((inputs (list ;(make-instance 'input-funbox
                       ;    :name "self"
                       ;    :value nil
                       ;    :doc-string "the sheet object")
                         (make-instance 'input-funbox
                           :name "Tracks"
                           :value nil
                           :doc-string "sheet tracks contents")
                         (make-instance 'input-funbox
                           :name "Params"
                           :value nil
                           :doc-string "sheet tracks parameters"))
                 )
          (rep (make-instance 'OMBoxSheet 
                 :mode 0
                 :name name
                 :reference self 
                 :icon (icon self)
                 :inputs inputs)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs self))
     rep))


;---------Lisp Code generation

(defmethod gen-code-for-ev-once ((self OMBoxSheet) numout)
   (let ((varname (read-from-string (gen-box-string self))))
     (print (if (not (member varname *let-list* :test 'equal)) 
       (progn
         (push varname *let-list*)
         (if *start-repeat-generation*
           (progn
             (push `(setf ,varname 
                          (let ((args ',(decode self)))
                            (when (nth 1 args) 
                              (initialize-instance ,(reference self) :voices (nth 0 args) :params (nth 1 args)))
                            (list ,(reference self) (voices ,(reference self)) (params ,(reference self)))))
                   *repeat-ev-once-list*)
             `(nth ,numout ,varname))
           `(let ((args ',(decode self)))
              (when (nth 1 args) 
                (initialize-instance ,(reference self) :voices (nth 0 args) :params (nth 1 args)))
              (setf ,varname (list ,(reference self) (voices ,(reference self)) (params ,(reference self))))
              (nth ,numout ,varname))))
       `(nth ,numout ,varname)))))

(defmethod gen-code ((self OMBoxSheet) numout)
    (print (cond
     ((equal (allow-lock self) "&") 
      (gen-code-for-ev-once self numout))
     ((equal (allow-lock self) "x") 
      `(nth ,numout (list ,(reference self) (voices ,(reference self)) (params ,(reference self)))))
     ((equal (allow-lock self) "o") (reference self))
     (t  (let ((args (decode self)))
           `(progn (when ,(nth 1 args) 
                     (initialize-instance ,(reference self) :voices ,(nth 0 args) :params ,(nth 1 args)))
              (nth ,numout (list ,(reference self) (voices ,(reference self)) (params ,(reference self))))))))))



;---------Evaluation

(defmethod omNG-box-value ((self OMBoxSheet) &optional (num-out 0))
  (handler-bind ((error #'(lambda (c) 
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                          (om-report-condition c ))
                                               :size (om-make-point 300 200))
                              (om-abort)))))
    (cond
     ((and (equal (allow-lock self) "x") (nth num-out (value self)))
      (nth num-out (value self)))
     ((and (equal (allow-lock self) "o") (reference self)))   
     ((and (equal (allow-lock self) "&") (ev-once-p self)) 
      (nth num-out (value self)))
     (t (let* ((args  (mapcar #'(lambda (input)
                                   (omNG-box-value input)) (inputs self)))
                rep)
          (when (editorFrame (reference self))
            (om-close-window (window (editorFrame (reference self)))))
          (when (nth 0 args)
            (initialize-instance (reference self) :voices (nth 0 args) :params (nth 1 args)))
          (setf rep (list (reference self) (voices (reference self)) (params (reference self))))
          
          (when (equal (allow-lock self) "&")
            (setf (ev-once-p self) t)
            (setf (value self) rep))
          (when (equal (allow-lock self) "x")
            (setf (value self) rep)) 
          (setf (saved? (reference self)) nil)
          (nth num-out rep))))))


(defmethod update-from-reference ((self OMBoxSheet) &optional (udt? t))
  (declare (ignore udt?))
  nil)

;--------Edition

(defmethod OpenEditorframe ((self OMBoxSheet)) 
   (OpenObjectEditor (reference self)) nil)

(defmethod remove-extra ((self OMPatch) (box OMBoxSheet))
   (setf (attached-objs (reference box))
     ;;(remove box (attached-objs self) :test 'equal)
     (remove box (attached-objs (reference box)) :test 'equal)
     ))


;;; transfers PATCHES <<-->> WS
;;; WS TO PATCH
;;; --> CREE UN OMBOX SHEET avec reference persistante

; plus utilise
;(defmethod make-editor-from-obj ((self patchPanel) obj pos &optional name)
;  (let* ((box (make-new-editorcall (class-of obj) pos 
;                                   (or name (mk-unique-name self (name obj))))))
;    (setf (value box) (clone obj))
;    (omG-add-element self (make-frame-from-callobj box))))

;(defmethod make-editor-from-obj ((self patchPanel) (obj sheet) pos &optional name)
; (let* ((nm (or name (mk-unique-name self (name obj))))
;        (box (make-new-editorcall (class-of obj) pos nm)))
;    (setf (value box) (copy-sheet-value obj))
;    (omG-add-element self (make-frame-from-callobj box))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged sheet-icon-frame) 
                         (target patchPanel) position)
  (make-func-from-obj target dragged position (name (object dragged))) 
  t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged sheet-icon-frame) 
                         (target icon-finder) position)
  nil)


;;; PATCH TO WS

;;; allow sheet boxeditorframe in WS/folder
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxEditCall) (target omworkspace)) 
  (or (equal 'sheet (class-name (reference dragged)))
      (call-next-method)))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxEditCall) (target OMFolder)) 
  (or (equal 'sheet (class-name (reference dragged)))
      (call-next-method)))

;; not used...
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged sheet) (target OMWorkSpace)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged sheet) (target OMFolder)) t)


;;; when drop is allowed...
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged boxeditorframe) 
                         (target WorkSpacePanel) position)
  (box-to-ws dragged target (value (object dragged)) position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged boxeditorframe) 
                         (target FolderPanel) position)
  (box-to-ws dragged target (value (object dragged)) position))

(defmethod box-to-ws (box target obj position) nil)

(defmethod box-to-ws ((box boxeditorframe) target (obj sheet) position)
  (let* ((newsheet (abs2sheet obj (mk-unique-name target (name box)) position))
         (new-frame (make-icon-from-object newsheet (om-point-h position) (om-point-v position) 1 1)))
    (omg-add-element target new-frame)
    (omng-save newsheet)
    t))

(defmethod abs2sheet ((self sheet) name pos)
   (let ((newsheet (make-instance 'sheet-persistant 
                                  :voices (loop for v in (voices self) collect (copy-container v))
                                  :name (or name (name self))
                                  :icon (icon self)
                                  :params (copy-container (params self)))))
     (set-icon-pos newsheet pos)
     (setf (mus-patch newsheet) (clone (mus-patch self)))
    newsheet))

|#

