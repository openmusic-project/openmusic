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
;Methods for omng-save generic function specialized for different objects.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)

(defvar *libs-to-load* nil)


(defvar *om-current-persistent* nil)


(defmethod om-save-point (point)
  (when point
    `(om-make-point ,(om-point-h point) ,(om-point-v point))))

(defun om-save-point-list (list)
  (loop for item in list collect
        (om-save-point item)))

(defun om-save-color (color)
  (when color
    (setf color (om-correct-color color))
    `(om-make-color ,(om-color-r color) ,(om-color-g color) ,(om-color-b color))))

(defun om-save-font (font)
    `(om-make-font ,(om-font-face font) ,(om-font-size font) :family ,(om-font-family font) :style ',(om-font-style font) :mode ',(om-font-mode font)))


(defun om-save-pathname (pathname)  
  (when pathname 
    `(om-make-pathname
      :directory ',(pathname-directory pathname) 
      :device ,(pathname-device pathname)
      :host ,(pathname-host pathname)
      :name ,(pathname-name pathname)
      :type ,(pathname-type pathname))))

(defmethod omng-save ((self pathname)  &optional (values? nil))
  (declare (ignore values?))
  (om-save-pathname self))

(defun path-in-dir-p (p d)
  (let ((included t)
        (path (pathname-directory p))
        (ref-dir (pathname-directory d))
        fp wp)
    (loop while (and included path ref-dir) do
          (setf fp (pop path))
          (setf wp (pop ref-dir))
          (when (not (string-equal fp wp))
            (setf included nil)))
    (if (and included (null ref-dir))
      (if path path (list nil)) 
      nil)))

(defvar *relative-path-reference* nil)

(defun relative-path-reference ()
  (or *relative-path-reference*
      (and *current-workspace* (mypathname *current-workspace*))))


(defun om-save-pathname-relative (path &optional from)
  (when path 
    (let ((path2 (or from (relative-path-reference))))
      (if  path2
          (let* ((relpath (path-in-dir-p path path2))
                 (dir (if (consp relpath)
                          (append (list :RELATIVE) (remove nil relpath))
                        (pathname-directory path))))
            `(om-make-pathname
              :directory ',dir 
              :device ,(and relpath (pathname-device path))
              :host ,(and relpath (pathname-host path))
              :name ,(pathname-name path)
              :type ,(pathname-type path)))
        (om-save-pathname path)))))


;-------RESOURCES--------------
(defvar *resources-to-load* nil)

;;;; allow to know abou required resources without loading the patch (for import/export)
(defun write-resources (obj res-list ptr)  
  (when res-list 
    (let ((rel-resources (loop for rtype in res-list collect
                               (cons (car rtype)
                                     (loop for path in (cdr rtype) collect
                                           (namestring (relative-pathname path (mypathname obj))))))))
      (write-line "; External resources " ptr)
    (write-line (format nil "; ~S" (omng-save rel-resources)) ptr))
    ))

(defun register-resource (type file)
  (let ((pos (position type *resources-to-load* :key 'car)))
    (if pos 
        (unless (member (namestring file) (cdr (nth pos *resources-to-load*)) :test 'string-equal :key 'namestring)
          (setf (nth pos *resources-to-load*) (append (nth pos *resources-to-load*) (list file))))
      (setf *resources-to-load* (push (list type file) *resources-to-load*)))
    t))

(defmethod get-resources (filename)
  (when (om-persistant-p filename)
    (let (line testc)
      (with-open-file (file filename :direction :input :if-does-not-exist nil)
        (loop for i from 1 to 3
              while (not (equal 'oa::eof (om-read-line file))))
        (unless (stream-eofp file)
          (setf testc (read-char file))
          (when (equal testc #\;)
            (setf line (delete-spaces (om-correct-line (om-read-line file)))))
          (if (string-equal line "External resources")
              (let ((nextline (om-read-line file)))
                (setf line (if (equal (elt nextline 0) #\;) 
                               (read-from-string (subseq nextline 1))
                             nil)))
            (setf line nil))
          ))
      (eval line))))

;(loop for item in 
;      (get-resources (pathname "/Users/bresson/WORKSPACES/aaaa/elements/Patch.omp"))
;      do (print (car item)))

;-------BUILT_IN_CLASSES---------

(defmethod omNG-save ((self t) &optional (values? nil))
  "Cons a Lisp expression that retunr a copy of self when it is valuated."
  (cond
   ((om-color-p self) (om-save-color self))
   ((om-point-p self) (om-save-point self))
   ((om-font-p self) (om-save-font self))
   (t (when (omclass-p (class-of self))
        (let ((theclass (class-name (class-of self)))
              (exeption-p (execption-save-p self)))
          (if exeption-p
            (let* ((intslots (set-difference (get-all-initargs-of-class theclass) (get-all-initargs-of-class exeption-p)
                                             :test 'string-equal :key 'name))
                   (slots (mapcar #'(lambda (slot)
                                      `(setf (,(internp (name slot) (symbol-package theclass))  newobj)
                                             ,(omNG-save (funcall (internp (name slot) (slot-package slot)) self) values?))) 
                                  intslots)))
              `(let ((newobj ,(save-exepcion self)))
                 (when newobj
                   ,.slots)
                 newobj))
            (let ((slots (mapcan #'(lambda (slot)
                                     (list (string2initarg (name slot)) 
                                           (eval `(omNG-save (,(internp (name slot) (slot-package slot))  ,self) ,values?)))) 
                                 (get-all-initargs-of-class theclass))))
              `(if (find-class ',theclass nil) (make-instance ',theclass ,.slots :from-file t)))))))))


(defmethod omNG-save ((self function) &optional (values? nil)) 
   nil)

(defmethod omNG-save ((self array) &optional (values? nil)) 
   (declare (ignore values?))  
   self)

(defmethod omNG-save ((self number) &optional (values? nil)) 
   (declare (ignore values?))  self)

(defmethod omNG-save ((self character) &optional (values? nil)) 
   (declare (ignore values?))  self)

(defmethod sethash ((self hash-table) (entry t) (value t))
  (setf (gethash entry self) value))

(defmethod omNG-save ((self hash-table) &optional (values? nil)) 
   (declare (ignore values?))
   (let (keylist vallist)
     (maphash #'(lambda (key val)
                  (push key keylist)
                  (push val vallist)) self)
     (setf keylist (reverse keylist)
           vallist (reverse vallist))
     `(let ((hashtable (make-hash-table)))
        (loop for key in ,(omNG-save keylist)
              for val in  ,(omNG-save vallist) do
              (sethash hashtable key val))
        hashtable)))

(defmethod omNG-save ((self null) &optional (values? nil)) (declare (ignore values?))  nil)

(defmethod omNG-save ((self symbol) &optional (values? nil)) (declare (ignore values?)) 
  `',self)

;; (setf lll '(("a" 1) ("b" 2) ("c" 3)))
;; (omng-save lll)
;; (quote ((quote ("a" 1)) (quote ("b" 2))))

;(eval (omng-save (make-list 2049)))
;(length (make-list 2049))
;(apply 'list (make-list 2047))
  

(defmethod omNG-save ((self list) &optional (values? nil))
  (if (> (length self) 2047)
    `(append ,.(mapcar #'(lambda (x) (omNG-save x values?)) (group-list self 2047 'circular)))
    `(list ,.(mapcar #'(lambda (x) (omNG-save x values?)) self))
  ;`',(mapcar #'(lambda (x) (omNG-save x values?)) self)
    ))


(defmethod omNG-save ((self cons) &optional (values? nil)) 
   (if (quoted-form-p self)
     `',(omng-save (second self))
     (if (null (cdr (last self)))
       (call-next-method)
       `(append ,(omng-save (butlast self) values? ) 
                (cons ,(omng-save (first (last self))) ,(omng-save (cdr (last self))))))))


;----------------------------------------------------------------------
;Execption omClasses

(defmethod execption-save-p ((self t)) nil)
(defmethod save-exepcion ((self t))  nil)




;---------------tools----------
(defun save-alist (alis)
   `(pairlis
     ',(loop for item in alis 
             collect (car item))
     (list ,.(loop for item in alis 
                   collect (omng-save (cdr item))))))


(defun str-with-nl (str)
  (map 'string  #'(lambda (x) 
                    (if (equal x #\$) #\Newline x)) str)
  )

(defun str-without-nl (str)
  (map 'string  #'(lambda (x) 
                    (if (equal x #\Newline) #\$ x)) str)
  )


(defun corrige (lis)
  (when lis
    (if (assoc 'deltapict lis) lis
      (pairlis (list 'deltapict) (list (om-make-point 0 0)) lis))
    (rplacd (assoc 'deltapict lis) (om-correct-point (cdr (assoc 'deltapict  lis))))
    lis))



(defun save-icon (icon)
   (cond
    ((numberp icon) icon)
    ((null icon) *default-icon*)
    ((listp icon)
     (if (equal (second icon) *package-user*)
       `(icon-from-user ,(first icon))
       `(icon-from-lib ,(first icon) ,(string-until-space (name (second icon))))))
    (t *default-icon*)))

(defun icon-from-user (icon)
  (list icon *package-user*))

(defun icon-from-lib (icon name)
   (let ((thelib (exist-lib-p name)))
     (if thelib
       (if (not (loaded? thelib))
         (if (si-o-no (string+ "A box needs the Library " name " Do you want to load it ?"))
           (progn (load-om-lib thelib) (list icon thelib))
           (om-abort))
         (list icon thelib))
       *default-icon*)))
   
;-------BOXES---------

;USED BY WHEN LOAD BOX
(defmethod saveBox? ((self OMBoxCall))       'genfun)
(defmethod saveBox? ((self OMBoxTypeCall))   'bastype)
(defmethod saveBox? ((self OMBoxlispCall))   'lispfun)
(defmethod saveBox? ((self OMBoxPatch))      'patch-box)
(defmethod saveBox? ((self OMBoxcomment))    'comment)
(defmethod saveBox? ((self OMBoxundefined))  'undefined)
(defmethod saveBox? ((self OMBoxEditCall))   'editor)
(defmethod saveBox? ((self OMSlotsBox))      'slot)
(defmethod saveBox? ((self OMBoxMaquette))   'maquette)


;TO LOAD A 

(defun fun-or-dead (reference)
  (handler-bind ((error 'dead))
    (if (fboundp reference) (fdefinition reference) 'dead)))

(defun genfun-or-dead (reference)
  (handler-bind ((error 'dead))
    (if (and (fboundp reference) (omgenfun-p (fdefinition reference)))
        (fdefinition reference) 'dead)))

(defun class-or-dead (reference)
   (if (or (exist-class-p (string reference))
           (and (find-class reference nil) (omclass-p (find-class reference nil)))) (find-class reference nil) 'dead))

(defmethod mk-object-refer ((self (eql 'genfun)) reference)       (genfun-or-dead reference))
(defmethod mk-object-refer ((self (eql 'class)) reference)        (class-or-dead reference))
(defmethod mk-object-refer ((self (eql 'box-with-win)) reference) (fun-or-dead reference))
(defmethod mk-object-refer ((self (eql 'bastype)) reference)      (get-basic-type reference))
(defmethod mk-object-refer ((self (eql 'lispfun)) reference)      reference)
(defmethod mk-object-refer ((self (eql 'mk-ins)) reference)       (class-or-dead reference))
(defmethod mk-object-refer ((self (eql 'maquette)) reference)     (mk-object-refer 'patch-box reference))

(defvar *changed-patches* (make-hash-table :test 'equal))
(defun get-obj-table (path) (gethash path *changed-patches*))
(defun add-item-table (obj path) 
   (setf (gethash path *changed-patches*) obj)
   (setf (saved? obj) nil))

(defun rem-obj-table (obj)
   (maphash #'(lambda (key val)
                (when (equal val obj)
                  (remhash key *changed-patches*))) *changed-patches*))


(defun printlatable  ()
   (print-hash-table  *changed-patches*))

(defun print-hash-table (table)
   (maphash #'(lambda (key val)
                (print (list key val))) table))

(defmethod load-obj-from-obj (object)
   (if (or (loaded? object) (member object *loaading-stack* :test 'equal)) object
       (om-with-cursor *om-wait-cursor* 
         (push object *loaading-stack*)
         (om-print (string+ "Loading... " (namestring (mypathname object))))
         (eval-non-text-file (mypathname object))
         (if *om-current-persistent*
           (progn         
             (load-abstraction-attributes object *om-current-persistent*)
             (setf *om-current-persistent* nil)
             object) 
           'dead))))



;; reinitialized in load-patch
(defvar *user-search-all* nil)

(defun last-oportunity (name path)
  (if (and *user-search-all* (= *user-search-all* 0))
      'dead
    (let ((last-oport (om-y-or-n-option-dialog 
                       (format nil "File ~s not found.~%Do you want to look for it?" (namestring name))
                       "Apply for all not found sub-patches" :no t))
          fname)
      (when (cadr last-oport)
        (setf *user-search-all* (if (car last-oport) 1 0)))
      (if (and (car last-oport)
               (setf fname (om-choose-file-dialog :prompt (string+ "Select a file for \"" name "\"")
                                                 ;:button-string name 
                                                 :directory 
                                                 (make-pathname :directory 
                                                                (append (pathname-directory (mypathname *current-workSpace*))
                                                                        (list "elements"))))))
          (let ((object (mk-object-refer 'patch-box (list+ (pathname-directory fname) (list (real-pathname-name fname))))))
            (add-item-table object path)
            object)
        'dead))))
  

(defun get-last-path-in-load-list ()
  (let (rep)
    (loop for item in *loaading-stack*
          while (not rep) do
          (when (mypathname item)
            (setf rep item)))
    rep))
  
(defun search-in-subfolders (current name)
   (when current
     (let ((path (mypathname current)) rep)
       (loop for item in (om-directory path :files t :directories t) 
             while (not rep) do
             (setf rep (check-if-item  item name)))
       rep)))


(defun automatic-patch-search (name)
   (let ((path (elements-pathname *current-workSpace*)) 
         (rep nil))
     (loop for item in (om-directory  path :files t :directories t)
           while (not rep) do
           (setf rep (check-if-item item name nil)))
     rep))


(defun check-if-item (path name &optional (ask? nil))
   (let (rep)
     (cond
      ((directoryp path)
       (let (rep)
         (loop for item in (om-directory  path :files t :directories t)
               while (not rep) do
               (setf rep (check-if-item item name)))
         rep))
      (t (when (and (om-persistant-p path) 
                    (string-equal (real-pathname-name path) name)
                    (file-type path))
           (if ask?
             (progn
               (setf rep (si-o-no (string+ "There is a file called " name " in " (format nil "~D" path)
                                                 ".  Choose this one?")))
               (when rep
                 (mk-object-refer 'patch-box (list+ (pathname-directory path) (list (real-pathname-name path))))))
             (mk-object-refer 'patch-box (list+ (pathname-directory path) (list (real-pathname-name path))))))))))


;; reinitialized in load-patch
(defvar *om-search-all* nil)

(defmethod mk-object-refer ((self (eql 'patch-box)) reference)
   (let ((obj-name (string (car (last reference))))
         path object flag)
     (if (equal (car reference) :absolute)
       (progn
         (setf path (cdr reference))
         (let ((wspath (cdr (pathname-directory (mypathname *current-workSpace*)))))
           (loop while (and path wspath (string-equal (car path) (car wspath))) do
                 (setf path (cdr path))
                 (setf wspath (cdr wspath))
                 )
           (setf path (cdr path))
           ))
       (if (= (length reference) 1)
         (setf path reference)
         (setf path (cdr reference))))
     (setf object *current-workspace*)
     (loop for item in path
           while (not flag) do
           (let ((elements (get-elements object)))
             (setf object (find-if #'(lambda (x) (string-equal (name x) item)) elements))
             (unless object (setf flag t))))
     (if (and object (not (folder-p object)) (not (equal object *current-workspace*)))
       (load-obj-from-obj object)
       (cond
        ((setf object (get-obj-table path)) object)
        ((setf object (search-in-subfolders (get-last-path-in-load-list) obj-name))
         (load-obj-from-obj object)
         (add-item-table object path)
         object)
        (t 
         (if (or (and *om-search-all* (= *om-search-all* 1))
                 (and (not *om-search-all*)
                      (let ((choise (om-y-or-n-option-dialog 
                                     (format nil "Patch ~s not found.~%Do you want OM to search in the workspace ?" obj-name)
                                     "Apply for all lost sub-patches" :yes t)))
                        (when (cadr choise)
                          (setf *om-search-all* (if (car choise) 1 0)))
                        (car choise))))
             (let ((found-object (automatic-patch-search obj-name)))
               (if found-object
                   (progn
                     (load-obj-from-obj found-object)
                     (add-item-table found-object path)
                     found-object)
                 (last-oportunity obj-name path)))
           (last-oportunity obj-name path)))))))
                  



;INPUTS-----

(defmethod omNG-save ((self input-funbox) &optional (values? nil))
  `(om-load-inputfun ',(class-name (class-of self)) ,(str-without-nl (doc-string self)) ,(name self) ,(omNG-save (value self) values?)))

(defmethod omNG-save ((self input-funmenu) &optional (values? nil))
  `(om-load-inputfunmenu1 ',(class-name (class-of self)) 
                     ,(str-without-nl (doc-string self)) 
                     ,(name self) 
                     ,(omNG-save (value self) values?) 
                     ,(omng-save (thepopup self))))

(defmethod omNG-save ((self input-keyword) &optional (values? nil))
  `(om-load-inputkeyword ',(class-name (class-of self)) ,(str-without-nl (doc-string self)) 
                         ,(name self) ,(omNG-save (value self) values?)
                         ,(omNG-save (def-value self))
                         ,(omNG-save (val-menu self))))


(defun om-load-inputfun (class doc name value) 
  (let ((input (make-instance class
                 :doc-string  (str-with-nl doc)
                 :name name
                 :value value)))
    (if (keyword-input-p input)
      (setf (value input) (string2initarg (string (value input)))))
    input))

(defun om-load-inputfunmenu1 (class doc name value items) 
  (make-instance class
                 :doc-string  (str-with-nl doc)
                 :name name
                 :value value
                 :thepopup items))


(defun om-load-inputkeyword (class doc name value defval menu) 
  (let ((input (make-instance class
                 :doc-string  (str-with-nl doc)
                 :name name
                 :value value
                 :def-value defval
                 :val-menu menu)))
    (setf (value input) (string2initarg (string (value input))))
    input))


;VALUES IN BOXES -----
(defun saveValueinBox (val)
   (omNG-save val t))


;------Metaobjects
(defmethod save-meta-object-as-value ((self OMBasicObject))
   `(make-instance ',(type-of self)))


(defun load-tempbox-as-value (offset extend color val posy strech sizey store refer)
   (let ((reference (if (equal (first refer) 'maq)
                      (mk-object-refer 'maquette (second refer))
                      (eval (second refer)))))
     (make-a-tempobj offset extend (om-correct-color color) val posy strech sizey store reference)))
     

;=======================BOXES==============================

(defmethod save-reference ((self t))
  (reference self))

(defmethod save-reference ((self OMBoxCall))
  (when (lib-fun-p (fdefinition (reference self)))
    (push (lib-fun-p (fdefinition (reference self))) *libs-to-load*))
  (reference self))

(defmethod save-reference ((self OMBoxRelatedWClass))
  (setf *libs-to-load* (append *libs-to-load* (class-needed-libraries (reference self)))) 
  (class-name (reference self)))

(defmethod save-reference ((self OMBoxLispCall))
  (reference self))


(defun str-with-nl (str)
  (map 'string  #'(lambda (x) 
                    (if (equal x #\$) #\Newline  x)) str)
  )

(defun str-without-nl (str)
  (map 'string  #'(lambda (x) (if (equal x #\Newline) #\$ x)) str))

(defmethod save-reference ((self OMBoxcomment))
  (str-without-nl (str-check (reference self))))

(defmethod save-reference ((self OMBoxundefined))
  (reference self))


;Genfun

(defmethod omNG-save ((self OMBoxCall) &optional (values? nil))
  "Save a box"
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (when (string-equal (allow-lock self) "x")
                  (saveValueinBox (value self)))))
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self) ,(numouts self))))

;boxeditor
(defmethod omNG-save ((self OMBoxEditCall) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (saveValueinBox (value self)))
         pict-list)
    (setf pict-list (omng-save (ed-pictu-list self)))                                
    `(om-load-editor-box1 ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self) ,(save-edition-params self)
                         ,(showpict self) ,(minieditor? self) ,pict-list ,(show-name self))))

;Types
(defmethod omNG-save ((self OMBoxTypeCall) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (saveValueinBox (value self))))
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) ,value ,(str-without-nl (thestring self)) ,(frame-name self))))

;Instances
(defmethod omNG-save ((self OMBoxInstance) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-boxinstance  ,(name self) ,(omNG-save-ins (reference self) values?) 
                           ',inputs ,(om-save-point (frame-position self)) ,(frame-name self) ,(om-save-point (frame-size self)))))


(defmethod omNG-save-ins ((self OMInstance) &optional (values? nil))
  (declare (ignore values?))
  (if (not (mypathname self))
    `(let ((copy (make-instance ',(class-name (class-of self))
                   :name ,(name self)
                   :icon ,(save-icon (icon self)))))
       (setf (instance copy) ,(saveValueinBox (instance self)))
       (setf (edition-params copy) ,(save-edition-params self))  ; (corrige ,(save-alist (edition-params self))))
       (setf (create-info copy) ',(create-info self))
       (setf (doc copy) (str-with-nl ,(str-without-nl (doc self))))
       copy)
    (and (register-resource :instance (mypathname self))
         `(get-inst-from-globals ,(name self)))))


(defun get-inst-from-globals (name) 
  (find-if #'(lambda (x) (string-equal name (name x))) (elements *om-globalsfolder*)))
   
   
;Patch Boxes
(defmethod omNG-save ((self OMBoxPatch) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (saveValueinBox (value self))))
    (register-resource :abstraction (mypathname (reference self)))
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(list+ (get-relative-path (reference self)) 
                                                              (list (name (reference self))))
                      ',inputs ,(om-save-point (frame-position self) )
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self) ,(numouts self))))

;Maquette boxes
(defmethod omNG-save ((self OMBoxMaquette) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (saveValueinBox (value self))))
    (register-resource :abstraction (mypathname (reference self)))
    (if (= (mode self) 1)
      `(let ((box (om-load-boxcall ',(saveBox? self) ,(name self) ',(list+ (get-relative-path (reference self)) 
                                                                           (list (name (reference self))))
                                   ',inputs ,(om-save-point (frame-position self)) 
                                   ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self))))
         (change-mode box 1 t)
         box
       )
      `(om-load-boxcall ',(saveBox? self) ,(name self) ',(list+ (get-relative-path (reference self)) 
                                                                (list (name (reference self))))
                        ',inputs ,(om-save-point (frame-position self)) 
                        ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self))
      )))

;Inputs
(defmethod omNG-save ((self OMIn) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-boxin ,(name self) ,(indice self)  ,(om-save-point (frame-position self)) ,(docu self) 
                  ,(frame-name self) ,(omng-save (eval (defval self)) t) ,(om-save-point (frame-size self))))


;typed inputs
(defmethod omNG-save ((self OMTypedIn) &optional (values? nil))
  (declare (ignore values?))
  (setf *libs-to-load* (append *libs-to-load* (class-needed-libraries (find-class (reference self) nil))))
  `(om-load-boxtypein  ,(name self) ',(reference self) ,(indice self) ,(om-save-point (frame-position self)) 
                       ,(docu self)  ',(keys self) ,(omng-save (eval (defval self)) t) ,(frame-name self) 
,(om-save-point (frame-size self))))

(defmethod omNG-save ((self OMinitTypedIn) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-initin  ,(name self) ',(reference self) ,(indice self) ,(om-save-point (frame-position self))
                               ,(self? self)   ',(classname self) ,(om-save-point (frame-size self))))


;;;;;;;;;;;called by make-patch-from-method;;;;;;;;;;;;;;;
(defmethod special-omNG-save ((self OMTypedIn))
  `(om-load-boxin ,(name self) ,(indice self)  ,(om-save-point (frame-position self)) "non docu" 
                  ,(frame-name self) ,(saveValueinBox (eval (defval self)))))

(defmethod omNG-save ((self selfTempIn) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-boxselfin  ,(name self)  ,(om-save-point (frame-position self)) ,(om-save-point (frame-size self))))

;outputs
(defmethod omNG-save ((self OMout) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-boxout ,(name self) ,(indice self)  ,(om-save-point (frame-position self))
 ',inputs ,(frame-name self) ,(om-save-point (frame-size self)))))

(defmethod omNG-save ((self OMtempOut) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-tempboxout ,(name self)  ,(om-save-point (frame-position self)) ',inputs ,(frame-name self) 
                         ,(om-save-point (frame-size self)))))


;temporal objects
(defmethod omNG-save ((self TemporalBox) &optional (values? nil))
   (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
          value refer pict)
     (cond
      ((om-maquette-abs-p (reference self))
       (setf refer `(list 'absmaq  ',(om-save (reference self) values?)))
       (setf value (save-values (value self))))
      ((Maquette-p (reference self))
       (setf refer `(list 'maq  ',(list+ (get-relative-path (reference self)) 
                                         (list (name (reference self))))))
       (setf value (save-values (value self))))
      ((abspatch-p (reference self))
       (setf refer `(list 'patch ,(om-save (reference self) values?)))
       (setf value (save-values (value self))))
      ((patch-p (reference self))
       (setf refer `(list 'patchb ',(list+ (get-relative-path (reference self)) 
                                         (list (name (reference self))))))
       (setf value (save-values (value self))))
      ((reference self)
       (setf refer (if (global-p (reference self))
                       `(list 'yourobj ,(om-save (reference self)))
                       `(list 'yourobj ,(omNG-save (reference self)))))))
     (when (and (pictu self) (thepict (pictu self)))   ;;; .
       (setf pict (omng-save (pictu self))))
     `(om-load-tempobj1 ,(name self) ',inputs ,refer ,(numouts self) ,(slot-value self 'offset) ,(extend self) 
                       ,(om-save-color (colorframe self)) ,value t ,(sizey self) ,(posy self) ,(strech-fact self)
                       ,(omNG-save (free-store self)) ,(save-edition-params self) ,(allow-lock self) ,pict ,(showpict self) ,(mute self) ,(lock self)
                       ,(show-name self) ,(str-without-nl (doc self)))))

(defun save-values (list)
   `(list ,.(mapcar #'(lambda (x) (if (boxtempobj-p x) nil (saveValueinBox x)))
                    list)))

(defmethod omNG-save ((self temp-marker) &optional (values? nil))
   (declare (ignore values?))
   (omng-copy self))


(defmethod omNG-save ((self OMBoxcomment) &optional (values? nil))
  (let* ((value (saveValueinBox (value self))))
    `(om-load-boxcomment ,(name self) ,(om-save-point (frame-size self)) ',(save-reference self) ,value
                         ,(om-save-point (frame-position self)) ,(frame-name self) ,(om-save-color (textcolor self)) 
                         ,(om-save-font (textstyle self)))))

(defun om-load-boxcomment (name size reference value position fname color style)
  (let ((newbox (omNG-make-new-boxcall 'comment (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (name newbox) name)
    (when color
      (setf (textcolor newbox) (om-correct-color color)))
    (setf (value newbox) (str-check (str-with-nl value)))
    (setf (reference newbox) (str-check (str-with-nl reference)))
    (when style (setf (textstyle newbox) (om-correct-font style)))
    newbox))


(defmethod omNG-save ((self general-box-dead) &optional (values? nil))
   "Saving dead boxes, save the Lisp Expression stored in the 'save-code' slot."
   (declare (ignore values?))
   (if (save-code self)
       `(let ((newbox ,(save-code self)))
              (setf (frame-position newbox) ,(om-save-point (frame-position self)))
              newbox)
       `(let ((newbox (omNG-make-new-boxcall 'dead ,(om-save-point (frame-position self)) ,(name self))))
          (setf (mesage newbox) "This box is dead forever")
          newbox)))
   
;-----------------------load---------------------------
  
(defmethod correct-box-inputs (class inputs) inputs)

(defmethod upgrade-editor-box (value class name position) nil)

(defun om-load-editor-box1 (name reference inputs position size value lock 
                                 &optional fname editparams spict meditor pictlist show-name 
                                 &rest rest)   ;;; au cas ou..
  (or (upgrade-editor-box value reference name position) 
      (let ((dead? (not (find-class reference nil))) newbox)
        (setf newbox 
              (if dead? (omNG-make-new-boxcall 'dead (om-correct-point position) name)
                (make-new-EditorCall (find-class reference) (om-correct-point position) name)))
        (setf (frame-size newbox) (om-correct-point size))
        (setf (frame-name newbox) fname)
        (setf (name newbox) name)
        (setf (show-name newbox) show-name)
        (when (box-has-pict-editors newbox)
          (setf (ed-pictu-list newbox) pictlist))
        (setf (inputs newbox) (correct-box-inputs reference (mapcar #'(lambda (input) (eval input)) inputs)))
        (set-box-to-inputs (inputs newbox) newbox)
        (when (and value (not dead?))
          (setf (value newbox) value))
        (if dead?
            (setf (numouts newbox) (length (inputs newbox))
                  (mesage newbox) (string+ "Class " (string reference) " not found; this editor is dead.")
                  (save-code newbox) 
                  `(om-load-editor-box1 ,name ',reference ',inputs ,(om-save-point (om-correct-point position)) 
                                        ,(om-save-point (om-correct-point size))
                                        ,(omng-save value t) ,lock  ,fname 
                                        ,(if (listp editparams)
                                             (save-alist editparams) (omng-copy editparams)) ,spict))
          (setf (allow-lock newbox) lock
                (edition-params newbox) (corrige-edition-params (value newbox) editparams)
                (showpict newbox) spict
                minieditor? meditor))
        newbox)))







(defmethod om-load-boxcall ((self (eql 'comment)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (let ((newbox (omNG-make-new-boxcall 'comment (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (name newbox) name)
    (setf (inputs newbox) inputs)
    (setf (value newbox) value)
    (setf (reference newbox) reference)
    (setf (allow-lock newbox) lock)
    newbox))


(defmethod om-load-boxcall ((self (eql 'undefined)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (let ((newbox (omNG-make-new-boxcall 'undefined (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (name newbox) name)
    (setf (inputs newbox) inputs)
    (setf (value newbox) value)
    (setf (reference newbox) reference)
    (setf (allow-lock newbox) lock)
    newbox))


;COMPATIBILiTY
(defmethod om-load-boxcall ((self (eql 'mk-ins)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (om-load-editor-box  name reference inputs position size value lock fname nil nil))


(defmethod om-load-boxcall ((self (eql 'editor)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (om-load-editor-box name reference inputs position size value lock fname nil nil))


(defmethod om-load-boxcall ((self (eql 'slot)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (let ((dead? (not (find-class reference nil))) newbox)
    (setf newbox (if dead? 
                   (omNG-make-new-boxcall 'dead (om-correct-point position) name)
                   (omNG-make-new-boxcall-slots (find-class reference) (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (name newbox) name)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (when (and value (not dead?))
      (setf (value newbox) value))
    (if dead?
      (setf (numouts newbox) (length (inputs newbox))
            (mesage newbox) (string+ "Class " (string reference) " not found; this slot box is dead.")
            (save-code newbox) 
            `(om-load-boxcall 'slot ,name ',reference ',inputs ,(om-save-point (om-correct-point position)) 
                              ,(om-save-point (om-correct-point size)) ,(omng-save value t) ,lock ,fname))
      (setf (allow-lock newbox) lock))
    newbox))

(defun set-box-to-inputs (input-list box)
  (loop for item in input-list do
        (setf (box-ref item) box)))

(defmethod update-inputs ((self t) inputs) inputs)


(defmethod om-load-boxcall ((class t) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (let ((newbox (omNG-make-new-boxcall (mk-object-refer class reference) (om-correct-point position) name)))
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    
    (set-box-to-inputs (inputs newbox) newbox)

    (if (equal (reference newbox) "wooo")
      (setf (numouts newbox) (or numouts 1)
            (mesage newbox) (string+ "This box was a " (string class) " named " name "; now it is dead.")
            (save-code newbox) 
            `(om-load-boxcall ',class ,name ',reference ',inputs ,(om-save-point (om-correct-point position))
                              ,(om-save-point (om-correct-point size)) ,(omng-save value t) ,lock ,fname ,numouts))
      (progn
        (setf (value newbox) value)
        (if (equal class 'bastype)
          (progn
            (setf (thestring newbox) (str-check (str-with-nl lock)))
            ;(ignore-errors (setf (value newbox) (read-from-string lock)))
            (ignore-errors (setf (value newbox) (read-from-string (thestring newbox))))
            ) 
          (setf (allow-lock newbox) lock))))
    newbox))


(defmethod om-load-boxcall ((self (eql 'lispfun)) name reference inputs position size value lock 
                            &optional fname numouts
                            &rest args)
  (declare (ignore numouts))
  (let* ((dead? nil)
        (newbox (or (and (fboundp reference) 
                         (ignore-errors (omNG-make-new-lispboxcall reference (om-correct-point position) name)))
                    (progn
                      (setf dead? t)
                      (omNG-make-new-boxcall 'dead (om-correct-point position) name)
                      ))))
    
    (setf (frame-size newbox) (om-correct-point size))
    (setf (frame-name newbox) fname)
    (setf (name newbox) name)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (when (and value (not dead?))
      (setf (value newbox) value))
    (if dead?
      (setf (mesage newbox) (string+ "Lisp fonction " (string reference) " not found; this box is dead")
            (save-code newbox) 
            `(om-load-boxcall 'lispfun ,name ',reference ',inputs ,(om-save-point (om-correct-point position)) 
                              ,(om-save-point (om-correct-point size)) ,(omng-save value t) ,lock  ,fname))
      (setf (allow-lock newbox) lock))
    newbox))


(defun om-load-boxinstance (name instance inputs position &optional fname size)
  (if (and instance (instance instance))
    (let ((newbox (omNG-make-new-boxcall instance (om-correct-point position) name)))
      (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
      (setf (value newbox) (instance instance))
      (setf (frame-name newbox) fname)
      (when size (setf (frame-size newbox) size))
      newbox)
    (let ((newbox (omNG-make-new-boxcall 'dead (om-correct-point position) name)))
      (setf (mesage newbox) "This box was an instance; now it is dead forever."
            (save-code newbox) 
            `(let ((newbox (omNG-make-new-boxcall 'dead ,(om-save-point (om-correct-point position)) ,name)))
               (setf (mesage newbox) "This box was an instance; now it is dead forever.")
                     newbox))
      newbox)))


(defun om-load-boxtypein (name type indice position docu keys defval &optional fname fsize )
  (let ((newbox (make-new-typed-input name type indice (om-correct-point position))))
    (setf (docu newbox) docu)
    (setf (frame-name newbox) fname)
    (setf (keys newbox) keys)
    (when defval
      (setf (defval newbox) (put-quote defval)))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

(defun om-load-initin  (name type indice posi self? class &optional fsize)
  (let ((newbox (make-new-typed-init name type indice (om-correct-point posi) self? class)))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

(defun om-load-boxselfin (name  position  &optional fsize)
  (let ((newbox (make-self-temp-input name (om-correct-point position))))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    (setf (defval newbox) (make-instance 'temporalbox))
    newbox))


(defun om-load-boxout (name indice position inputs &optional fname fsize)
  (let ((newbox (make-new-output name indice (om-correct-point position))))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

(defun om-load-tempboxout (name position inputs &optional fname fsize)
  (let ((newbox (make-new-temp-output  name (om-correct-point position))))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))


(defun om-load-boxin (name indice position docu &optional fname val fsize)
  (let ((newbox (make-new-patch-input name indice (om-correct-point position))))
    (setf (docu newbox) docu)
    (when val
      (setf (defval newbox) (put-quote val)))
    (setf (frame-name newbox) fname)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))



(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict 
                             sizey posy strechfact 
                             &optional (store nil) (params nil) (lock nil) pict (showpict nil) (mute nil) (pos-locked nil)
                             (showname nil) (doc "") &rest rest)
  (let (reference newtempob maqpos)
    (cond
     ((equal (first refer) 'maq) 
      (setf reference (mk-object-refer 'maquette (second refer))))
     ((equal (first refer) 'patchb) 
      (setf reference (mk-object-refer 'patch-box (second refer))))
     (t (setf reference (eval (second refer)))))
    (when reference
      (setf maqpos (om-make-big-point posx posy))
      (setf newtempob (omNG-make-tempobj reference maqpos name))   
      (setf (numouts newtempob) numouts)
      (setf (inputs newtempob) (mapcar #'(lambda (input) (eval input)) inputs))
      (setf (extend newtempob) sizex)
      (setf (colorframe newtempob) (om-correct-color clorf))
      (setf (free-store newtempob) store)
      (when value
        (setf (value newtempob) (list! value))
        (if (maquette-p reference) (setf (value reference) (car (value newtempob)))))
      (setf (slot-value newtempob 'strech-fact)  (or strechfact 1))
      (setf (colorframe newtempob) (om-correct-color clorf))
      (setf (slot-value newtempob 'sizey) sizey)
      (setf (allow-lock newtempob) lock)
      (setf (lock newtempob) pos-locked)
      (setf (mute newtempob) mute)
      (setf (showpict newtempob) showpict)
      (setf (name newtempob) name)
      (setf (doc newtempob) (str-with-nl doc))
      (setf (show-name newtempob) showname)
      (when pict (setf (pictu newtempob) pict))   ;;; .
      (setf (edition-params newtempob) (corrige-edition-params (car (value newtempob)) params))
      newtempob)))

;---------------------------------------------


;==========WorkSpace=======================
(defmethod omNG-save-ws ((self null)) nil)


(defmethod omNG-save-ws ((self OMWorkSpace))
  (mapcar #'(lambda (elem) 
              (omNG-save-ws elem)) (elements self))
  (om-print (string+ "WORKSPACE " (name self) " saved")))

;-------Package---------
(defmethod omNG-save-packlist ((self OMPackage))
  (let* ((elements (mapcar #'(lambda (elem) (omNG-save-ws elem)) (subpackages self)))
         (functions (mapcar #'(lambda (elem) (function-name elem)) (functions self)))
         (class-alias (mapcar #'(lambda (elem) `(list ,(name elem) ',(get-reference elem) ,(om-save-point (frame-position elem))))
                              (aliasclasses self)))
         (list-to-sort (loop for item in (classes self)
                             collect (list (class-name item) (make-super-class-list item))))
         (classes (sort-class-name-list  list-to-sort nil)))
    (setf elements (remove-if 'null elements))
    `(list ,(name self) (list ,.elements) ',classes ',functions (list ,.class-alias))))


(defmethod omNG-save-ws ((self OMPackage))
  (let* ((elements (mapcar #'(lambda (elem) (omNG-save-ws elem)) (subpackages self)))
         (class-alias (mapcar #'(lambda (elem) `(list ,(name elem) ',(get-reference elem) ,(om-save-point (frame-position elem))))
                              (aliasclasses self)))
         (list-to-sort (loop for item in (classes self)
                             collect (list (class-name item) (make-super-class-list item))))
         (classes (sort-class-name-list  list-to-sort nil))
         (functions (mapcar #'(lambda (elem) (function-name elem)) (functions self))))
    (setf elements (remove-if 'null elements))
    `(list ,(name self) (list ,.elements) ',classes ',functions (list ,.class-alias))))



;-------Folder, patch and maquettes ---------
(defun write-icon (icon)
  (when (equal (second (list! icon)) *package-user*)
    (list (first icon))))

(defun write-docu (self)
  (let ((docstring (get-documentation self)))
    (unless docstring
      (setf docstring ""))
    (list (list (string+ "\"" docstring "\"")))))


(defmethod omNG-save-ws ((self OMFolder))
  ;(hide-message-win)
  ;(let ((oldmess (print (change-message-win (string+ "Saving Folder " (name self) "...")))))
  (change-message-win (string+ "Saving folder " (name self) "..."))
  ;(show-message-win (print (change-message-win (string+ "Saving Folder " (name self) "..."))))
  ;(show-message-win nil)
  ;(sleep 0.1)
  (mapcar #'(lambda (elem) (omNG-save-ws elem)) (elements self))
  (when (changed-wsparams? self)
      (when (create-info self) (setf (cadr (create-info self)) (om-get-date)))
      (set-finder-comment (mypathname self) self)
      (setf (changed-wsparams? self) nil)) 
    ;(change-message-win oldmess))
    )


(defvar *save-apply-all* nil)

(defmethod omNG-save-ws ((self OMPatch))
  (unless (and *save-apply-all* (= *save-apply-all* 0)) 
    (when (null (saved? self))
      (when (or (and *save-apply-all* (= *save-apply-all* 1))
              (progn
                (hide-message-win)
                (let ((rep (om-y-or-n-option-dialog (string+ "Save changes to " (get-object-insp-name self) " " (name self) " ?") "Apply to all" :title "Saving...")))
                  (if (cadr rep)
                      (setf *save-apply-all* (if (car rep) 1 0)))
                  (car rep))))
          (show-message-win nil)
          (omNG-save self nil)
        )
      )
    (when (and (>= (omversion self) 5) (changed-wsparams? self))
      (set-finder-comment (mypathname self) self)
      (setf (changed-wsparams? self) nil))
    ))
  
(defmethod omNG-save-ws ((self OMInstance))
   (omNG-save self))


                  
                                          

;-------PATCH---------
(defvar *skip-libs* nil)
;;; old user libs
(setf *skip-libs* '("Dn" "OMClouds" "OMSituation" "OMGroups" "Amiot" "omtrees" "Nomos" "OMSounds" "OM_AS"))

(defun load-lib-for (list)
  (unless (equal *skip-libs* :all)
    (loop for lib in list do
          (let ((name (lib-true-name lib)))
            (unless (or *loading-ws* (member name *skip-libs* :test 'string-equal))
              (let ((thelib (exist-lib-p name))
                    (abort nil))
                (if thelib
                    (unless (loaded? thelib)
                      (let ((rep (si-o-no (string+ (name thelib) " library is required. Do you want to load it ?") nil :cancel t)))
                        (if (equal rep :cancel) 
                            (setf abort t)
                          (if rep
                              (load-om-lib thelib)
                            )))
                      ;(om-abort)
                      )
                  (let ((rep2 (om-y-or-n-option-dialog (string+ "The library " lib " cannot be found. Load patch anyway ?") "Skip this lib")))
                    (if (null (car rep2)) (setf abort t)
                      (if (cadr rep2) (push lib *skip-libs*))))
                  )
                (when abort (om-abort))
                ))))))
  
              

(defvar *instance-to-load* nil)

(defmethod omNG-save ((self OMInstance) &optional (values? nil))
  "If 'self' has a pathname associated write a new file, else generate a new lisp expression."
  (if (not (mypathname self))
    `(let ((ominst (make-instance ',(class-name (class-of self))
                   :name ,(name self)
                   :icon ,(save-icon (icon self)))))
       (setf (instance ominst) ,(omNG-save (instance self)))
       (setf (doc ominst) (str-with-nl ,(str-without-nl (doc self))))
       (setf (edition-params ominst) ,(save-edition-params self))
       (setf (create-info ominst) ',(create-info self))
       ominst)
    (progn
      (setf *saving-patch* self)
      (unless (string-equal (pathname-type (mypathname self)) "omi")
        (delete-file-protection (mypathname self))
        (setf (mypathname self)  (om-put-path-extension (mypathname self) "omi")))
      (delete-file-protection (mypathname self))
      (setf (cadr (create-info self)) (om-get-date))
      (WITH-OPEN-FILE (out (mypathname self) :direction :output  
                           :if-does-not-exist :create :if-exists :supersede) ;;;;; :external-format :INST)
        (write-header self out)
        (prin1 '(in-package :om) out)
        (let ((*package* (find-package :om))
              (inst-code (om-save self values?)))
          (prin1 `(load-lib-for ',(remove-duplicates *libs-to-load* :test 'string-equal)) out)
          (prin1 inst-code out)))
      (when (create-info self) (setf (cadr (create-info self)) (om-get-date)))
      (setf *saving-patch* nil)
      (setf *libs-to-load* nil))))



(defmethod om-save ((self OMInstance) &optional (values? nil))
  (let ((instance (omNG-save (instance self))))
    (setf pictlist (omng-save (pictu-list self)))
    `(setf *instance-to-load* (om-load-ominstance1 ',(class-name (class-of self)) ,(name self) ,(save-icon (icon self))
                                                   ,instance ,(save-alist (edition-params self)) ,pictlist ,(str-without-nl (doc self))))))

;;; used by editor exports
(defmethod save-instance (value)
  (catch-cancel
    (let ((name (om-choose-new-file-dialog     
                 :prompt "Save the instance of this editor"
                 :types '("OM instance" "*.omi"))))
      (when name
        (delete-file-protection name)
        (WITH-OPEN-FILE (out name :direction :output  
                             :if-does-not-exist :create :if-exists :supersede )
          (write-header-inst out)
          (prin1 '(in-package :om) out)
          (prin1 `(setf *instance-to-load* 
                          (omNG-make-new-instance ,(omng-save value) "instance")) out))
        t))))


(defun om-load-ominstance1 (class name icon instance edparams &optional pictlist doc &rest rest)
   (let ((copy (make-instance class
                 :name name
                 :icon icon)))
     ;(setf (instance copy) (eval instance))
     (setf (instance copy) instance)
     (setf (edition-params copy) edparams)
     (setf (doc copy) (str-with-nl doc))
     (setf (pictu-list copy) pictlist)  ; es suficiente ????
     copy))
       
 

;Save generation

(defmethod om-save ((self OMPatch) &optional (values? nil))
   (if (lisp-exp-p self)
     ;`(setf *om-current-persistent* (om-load-lisp-patch ,(name self) ,*om-version* ',(lisp-exp-p self)))
     ;;; now lisp-exp-p is a string
     `(setf *om-current-persistent* (om-load-lisp-patch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp-p self))))
     (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
           (connectiones (mk-connection-list (boxes self))) pictlist edpictlist)
       (setf pictlist (omng-save (pictu-list self)))
       `(setf *om-current-persistent* (om-load-patch1 ,(name self) ',boxes ',connectiones ,pictlist ,*om-version*)))))



(defmethod om-save ((self OMMaquette) &optional (values? nil))
   (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
         (connectiones (mk-connection-list (boxes self)))
         (markers (mk-markers-list (boxes self))) pictlist fond-ec edpictlist
         (eval-func (omng-save (eval-func self))))
     (when (thepict (pictu self))
       (setf fond-ec (omng-save (pictu self))))
     `(setf *om-current-persistent* (om-load-maq2 ,(name self) ',boxes ',connectiones ',(range (params self)) ',markers
                                                 ,(om-save-color (maq-color (params self))) ',(metricparam (params self)) ',(reverse pictlist) 
                                                 ,fond-ec ,(show-conect (params self)) ,(show-ruler-metric (params self)) ,*om-version* nil 
                                                 ',(yparam (params self))
                                                 ',(xparam (params self))
                                                 ,eval-func))))


(defun om-load-maq1 (name boxes connections range markers 
                          &optional (colormaq nil) (metricparam '((4 60) ((4 4)) 16 t))
                          (pictlist nil) (fond-ec nil) (show-connect t) (version nil) (pictueditors nil))
   (let ((newmaquette (omNG-make-new-maquette name)))
     (when (null (nth 3 range))
       (setf range (list+ range (list 0 100))))
     (setf (boxes newmaquette) nil)
     (mapc #'(lambda (box) (omNG-add-element newmaquette (eval box))) boxes)
     (setf (boxes newmaquette) (reverse (boxes newmaquette)))
     (setf (connec newmaquette) connections)
     (remk-markers markers (boxes newmaquette))
     (setf (params newmaquette) (make-instance 'maquette-params 
                                  :range range
                                  :maq-color (om-correct-color colormaq)
                                  :metricparam metricparam
                                  :show-conect show-connect))
     (when fond-ec
       (setf (pictu newmaquette) fond-ec))
     (when version
       (setf (omversion newmaquette) version))
     newmaquette))

;;; + show-ruler-metric + yparams + xparams
(defun om-load-maq2 (name boxes connections range markers 
                          &optional (colormaq nil) (metricparam '((4 60) ((4 4)) 16 t))
                          (pictlist nil) (fond-ec nil) (show-connect t) (show-ruler-metric nil) (version nil) (pictueditors nil) (yparams nil) (xparams nil)
                          (evalfunc nil) (snap nil))
   (let ((newmaquette (omNG-make-new-maquette name)))
     (when (null (nth 3 range))
       (setf range (list+ range (list 0 100))))
     (setf (boxes newmaquette) nil) 
     (mapc #'(lambda (box) (omNG-add-element newmaquette (eval box))) boxes)
     (setf (boxes newmaquette) (reverse (boxes newmaquette)))
     (setf (connec newmaquette) connections)
     (remk-markers markers (boxes newmaquette))
     (setf (params newmaquette) (make-instance 'maquette-params 
                                  :range range
                                  :maq-color (om-correct-color colormaq)
                                  :metricparam metricparam
                                  :show-conect show-connect
                                  :show-ruler-metric show-ruler-metric))
     (when fond-ec
       (setf (pictu newmaquette) fond-ec))
     (when version
       (setf (omversion newmaquette) version))
     (when yparams
       (setf (yparam (params newmaquette)) yparams))
     (when xparams
       (setf (xparam (params newmaquette)) xparams))
     (when snap
       (setf (snap (params newmaquette)) snap))
     (when evalfunc
       (setf (eval-func newmaquette) evalfunc))
     newmaquette))


(defun om-load-patch1 (name boxes connections &optional (fond-ec nil) (version nil) (pictueditors nil))
   (let ((newpatch (omNG-make-new-patch name)))
     (setf (boxes newpatch) nil)
     (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
     (setf (boxes newpatch) (reverse (boxes newpatch)))
     (setf (connec newpatch) (loop for i in connections collect (load-connection i)))
     (setf (pictu-list newpatch) fond-ec)
     (when version
       (setf (omversion newpatch) version))
     newpatch))


;(defun load-connection (list)
;  (when (nth 4 list)
;    (setf (nth 4 list) (remove nil (loop for i in (nth 4 list) collect 
;                                         (om-correct-point (cond 
;                                                            ((omlistp i) i)
;                                                            (t (eval i)))))))
;    )
;  list)


(defun load-connection (list)
  (when (nth 4 list)
   (setf (nth 4 list) (remove nil (loop for i in (nth 4 list) collect 
                                        (cond 
                                         ((and (omlistp i) (numberp (car i))) (om-correct-point i))
                                         ((omlistp i) (mapcar 'load-connection i))
                                         (t (om-correct-point (eval i)))))
                              )))
  list)


(defun om-load-temp-patch1 (name boxes connections &optional (version nil) pictlist)
   (let ((newpatch (make-instance 'OMPatchAbs :name name)))
     (setf (boxes newpatch) nil)
     (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
     (setf (boxes newpatch) (reverse (boxes newpatch)))
     (setf (connec newpatch) connections)
     (setf (pictu-list newpatch) pictlist)
     (when version
       (setf (omversion newpatch) version))
     (push newpatch *loaading-stack*)
     newpatch))


;;; from pict-compat... ??
(defun om-load-temp-patch (name boxes connections &optional (version nil))
   (let ((newpatch (make-instance 'OMPatchAbs :name name)))
     (setf (boxes newpatch) nil)
     (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
     (setf (boxes newpatch) (reverse (boxes newpatch)))
     (setf (connec newpatch) (loop for i in connections collect (load-connection i)))
     (when version
       (setf (omversion newpatch) version))
     (push newpatch *loaading-stack*)
     newpatch))





;------Methods--------
(defun om-save-methods (method)
  (let* ((controls (graph-fun method))
         (in-boxes (sort (get-typed-boxes controls) #'< :key #'indice))
         (lambda-list (load-method-lambda-list in-boxes))
         (initvals (mapcar #'(lambda (item) (omng-save (eval (get-default-input-val item)))) in-boxes))
         (indocs (mapcar #'(lambda (item) (docu item)) in-boxes))
         (qualy (method-qualifiers method)) pictlist)
    (setf pictlist (omng-save (pictu-list method)))
    `(om-load-method-ws2 ',(interne (name method))
                        ,(save-icon (icon (fdefinition (method-name method))))
                        ,(str-without-nl (documentation (interne (name method)) 'function))
                        ',(mapcar #'(lambda (box) (omNG-save box t)) (graph-fun method))
                        ',(mk-connection-list (graph-fun method))
                        ',lambda-list
                        ',initvals
                        ',indocs
                        ',qualy
                        ',(class-method-p method)
                        ,(numouts (fdefinition (method-name method)))
                        ,pictlist
                        ',(create-info method)
                        )))


(defun om-load-method-ws2 (name icon doc boxes connections lambda-list initvals indocs qualy flag
                                &optional (numouts 1) pictlist create-info)
  (let ((new-method (om-load-method-ws1 name icon doc boxes connections lambda-list initvals indocs qualy flag
                                        numouts pictlist)))
    (when create-info (setf (create-info new-method) create-info))
    new-method))

(defun om-load-method-ws1 (name icon doc boxes connections lambda-list initvals indocs qualy flag
                                &optional (numouts 1) pictlist)
   (ignore-error-msg (format nil "The method ~D ~D was not loaded because un error ocurred." name lambda-list)  
     (let* ((existe-genfun (fboundp name))
            (new-method (eval `(defmethod* ,name ,.qualy ,lambda-list
                                 :initvals ',initvals
                                 :indoc ',indocs
                                 :icon ,(car (list! icon))
                                 :doc ,(str-with-nl doc)
                                 :numouts ,numouts
                                 nil))))
       (setf (graph-fun new-method) boxes)
       (setf (pictu-list new-method) pictlist)
       (setf (saved-connections new-method) connections)
       (setf (class-method-p new-method) flag)
       (setf (protected-p new-method) nil)
       (unless existe-genfun
         (setf (protected-p (fdefinition name)) nil)
         (when (listp icon)
           (icon-for-user-package (fdefinition name) (second icon))))
       new-method)))
                       
                       

(defun define-really-method (method)
   (when method
     (let* ((newboxes (mapcar #'(lambda (input) (ignore-errors (eval input))) (graph-fun method)))
            new-method pictlist)
       (update-patches-pile)
       (setf pictlist (pictu-list method))
       (remk-connections newboxes (loop for i in (saved-connections method) collect (load-connection i)))
       (put-boxes-in-method method newboxes)
       (setf new-method (boxes2method method))
       (when new-method
         (setf (pictu-list new-method) pictlist)
         (setf (mypathname new-method) (mypathname method))
         (unless (or (equal (class-method-p method) 'init)
                              (equal (class-method-p method) 'get)
                              (equal (class-method-p method) 'set))
           ;;; do nothing...
           (om-remove-method-definition method)
           )
         new-method))))

(defun detect-genfun-redefinition (list)
  (let (rep)
    (loop for item in list do
          (when (fboundp (eval (second (second item))))
            (push (second (second item)) rep)))
    rep))
        


(defun boxes2method (method)
   (ignore-error-msg (format nil "The method ~D ~D was not loaded because un error ocurred." (method-name method) (qualifieurs method))
     (let* ((genfun (method-generic-function method))
            (controls (graph-fun method)) 
            (out-box (sort (find-class-boxes controls 'OMOut) #'< :key #'indice))
            (thequaly (qualifieurs method))
            (*let-list* nil)
            (old-graph-fun (graph-fun method))
            (old-flag (class-method-p method))
            (old-icon (icon method))
            (old-attached (attached-objs method))
            (old-info (create-info method))
            thebody thecode new-method in-boxes lambda-list)
       (setf in-boxes (sort (get-typed-boxes controls ) #'< :key #'indice))
       (setf lambda-list (load-method-lambda-list in-boxes))
       (setf thebody (append (list 'values)
                             (mapcar #'(lambda (out)
                                         (gen-code  out 0)) out-box)))
       (setf thebody (list 'let* *let-list*  thebody))
       (cond
        ((equal old-flag 'init)
         (let (initbox)
           (setf *pretraitement-passing* nil)
           (setf thebody `((call-next-method) (unless from-file ,thebody)))
           (eval `(defmethod initialize-instance ( ,(car lambda-list) &rest args &key (from-file nil)) 
                    ,.thebody))
           (setf new-method method)
           (setf initbox (car (find-class-boxes controls 'OMBoxCallNextInit)))
           (when initbox
             (setf *pretraitement-passing* t)
             (eval  `(defmethod pretraitement (,(car lambda-list) &rest args)
                       (setf args (call-next-method))
                       (unify-list (list ,.(cdr (decode initbox))) args)))
             (setf *pretraitement-passing* nil))))
        ((or (equal old-flag 'get))
         (eval `(defmethod ,(getsetfunname (string (method-name method)))  ,lambda-list ,thebody ))
         (setf new-method method))
        ((or (equal old-flag 'set) (equal old-flag 'set))
         (eval `(defmethod (setf ,(getsetfunname (string (method-name method))))
                           ,(reverse lambda-list) ,thebody ))
         (setf new-method method))
        (t 
         (setf thecode `(defmethod* ,(method-name method) ,.thequaly ,lambda-list  ,thebody ))
         (setf new-method (eval thecode))
         (put-boxes-in-method new-method old-graph-fun)
         (setf (attached-objs new-method) old-attached)
         (setf (icon new-method) old-icon)
         (setf (class-method-p new-method) old-flag)
         (setf (protected-p new-method) nil)
         (setf (compiled? new-method) nil)
         (setf (create-info new-method) old-info)
         (setf (numouts (method-generic-function new-method)) (length out-box))))
       new-method)))
         
(defun load-method-lambda-list (lis)
   (let (standard optional rest keywrds)
     (mapc #'(lambda (box)
               (cond
                ((equal (keys box) nil) (push (list (interne (name box)) (reference box)) standard))
                ((equal (keys box) '&optional) (push (list (interne (name box)) (defval box)) optional))
                ((equal (keys box) '&rest) (push (interne (name box)) rest))
                ((equal (keys box) '&key) (push (list (interne (name box)) (defval box)) keywrds)))) lis)
     (setf standard (reverse standard))
     (when optional
       (setf standard (append standard (list '&optional) (reverse optional))))
     (when rest
       (setf standard (append standard (list '&rest) (reverse rest))))
     (when keywrds
       (setf standard (append standard (list '&key) (reverse keywrds))))
     standard))
           

;------Classes--------

(defun save-initargs (list)
  (loop for item in list
      for value = (funcall (second item))
        append (eval `(list ,(car item) ',(om::omng-save value t)))))

(defun om-save-class (class)
  (let ((oneinstance (make-instance (class-name class)))
        (slot-list (make-slot-list (get-elements class)))
        (use-lib (lib-class-p class)))
    (when use-lib (push use-lib *libs-to-load*))
    
    (loop for i from 0 to (- (length slot-list) 1) do
          (setf (nth 2 (nth i slot-list)) 
                (if (equal (nth 8 (nth i slot-list)) :instance)
                  (omng-save (eval (nth 2 (nth i slot-list))))
                  (omng-save (slot-value oneinstance (car (nth i slot-list)))))))
    
    `(om-load-class-ws1 ,(name class) ,(save-icon (icon class)) ',(make-super-class-list class) 
                       ',slot-list ,(om-save-point (get-icon-pos class)) ,(str-without-nl (get-documentation class)) ',(type-of class) 
                       ',(save-initargs (get-class-default-initargs class))
                       ,use-lib
                       ',(create-info class))))

(defun om-load-class-ws (name icon superlist slots posi doc &optional (meta 'omstandardclass) default-initargs use-lib)
   (let ((theclass  (eval `(defclass* ,(interne name) ,superlist ,slots 
                             (:icon ,(car (list! icon)))
                             (:documentation ,(str-with-nl doc))
                             (:metaclass ,meta)
                             (:default-initargs ,.default-initargs)))))
     (setf (lib-class-p theclass) use-lib)
     (set-icon-pos theclass (om-correct-point posi))
     (when (listp icon)
       (icon-for-user-package theclass (second icon)))
     theclass))

(defun om-load-class-ws1 (name icon superlist slots posi doc &optional (meta 'omstandardclass) default-initargs use-lib create-info)
   (let ((theclass (om-load-class-ws name icon superlist slots posi doc meta default-initargs use-lib)))
     (when create-info (setf (create-info theclass) create-info))
     theclass))

;=============================  

(defun ws-save-globals ()
  (loop for item in (get-elements *om-globalsfolder*) do
        (unless (saved? item)
          (omNG-save item nil))))
  

(defun save-provisoire-user (listmethods listclasses code)
  `(let* ((classes ',(mapcar #'(lambda (elem) (om-save-class elem)) listclasses))
          (methods ',(loop for met in listmethods 
                           collect (om-save-methods met)))
          (subpack ',code)
          (init-class-met ',(save-initial-class-methods listclasses)) semi-methods badclasses)
     (setf badclasses (eval-initial-classes classes))
     (setf semi-methods (mapcar #'(lambda (elem) (eval elem)) methods))
     (mapc #'(lambda (elem) 
               (when elem (define-really-method elem))) semi-methods)
     (initial-methods-for-classes init-class-met badclasses)
     (init-user-package (eval subpack) badclasses)))



;INITIAL METHODS FOR A CLASS
(defun save-initial-class-methods (list)
  (loop for class in list 
        collect (save-init-met-class class)))


(defun initial-methods-for-classes (list &optional badclasses)
  (loop for item in list do
        (let ((theclass (find-class (first item) nil)) tempmet)
          (when (and theclass (not (member (string (first item)) badclasses :test 'string-equal)))
            (setf tempmet (loop for met in (second item) 
                                collect (eval met)))
            (loop for met in tempmet do
                  (when met
                    (push (define-really-method met) (internal-met theclass))))
            (setf (internal-met theclass) (remove-if 'null (remove-duplicates (reverse (internal-met theclass)) :test 'method-equal)))))))


;-----------------------------------------------
;NEW SAVE & LOAD USER PACKAGE
;-----------------------------------------------

;;; nouveau systeme de resources
(defun check-user-package ()
  (unless *package-user*
    (setf *package-user* (make-instance 'OMPackage :name "user" :icon 22))
    (really-add *om-package-tree* *package-user*))
  (unless (mypathname *package-user*)
    (setf (mypathname *package-user*) 
          (om-make-pathname
           :device (mypathname *current-workSpace*)
           :directory (append (pathname-directory (mypathname *current-workSpace*))  (list "user")))))
  (let ((respath (om-make-pathname :directory (append (pathname-directory (mypathname *current-workSpace*))
                                                        '("resources"))))
        (icnpath (om-make-pathname
                  :device (mypathname *current-workSpace*)
                  :directory (append (pathname-directory (mypathname *current-workSpace*))
                                                        (list "resources" "icon"))))
        (pictpath (om-make-pathname 
                     :device (mypathname *current-workSpace*)
                     :directory (append (pathname-directory (mypathname *current-workSpace*))
                                                        (list "resources" "pict")))))
    (unless (probe-file respath)
      (om-create-directory respath))
    (unless (probe-file icnpath)
      (om-create-directory icnpath))
    (unless (probe-file pictpath)
      (om-create-directory pictpath))
    (def-icon-var *package-user* icnpath)))


;(defun look-for-lib2load () nil)

;;; les sauvegardes dans les packages sont tenus à jour automatiquement

(defun ws-save-user-package ()
;  (let* ((thepath (make-pathname :directory (pathname-directory (mypathname *current-workSpace*))
;                                 :name "userpackage" 
;                                 :type "lisp")))
;    (setf *libs-to-load* nil)
;    (when (probe-file (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*))
;                                                        (list "user"))))
;      ;;; do nothing...
;      (setf *libs-to-load* (look-for-lib2load))
;      (omng-save *package-user* (mypathname *package-user*)))
;    (delete-file-protection thepath)
;    (with-open-file (out thepath :direction :output 
;                         :if-does-not-exist :create :if-exists :supersede) 
;      (let ((*package* (find-package :om)))
;        (prin1 '(in-package :om) out)
;        (prin1 `(check-user-package) out)
;        (prin1 `(load-lib-for-first ',(remove-duplicates *libs-to-load* :test 'string-equal)) out)
;        (prin1 `(init-user-package (list "User" nil nil nil nil) nil) out)))
;    (setf *libs-to-load* nil))
 
  (when (changed-wsparams? *om-package-tree*)
    (if (probe-file (om-make-pathname :directory (mypathname *package-user*)
                                      :name ".finderinfo"))
        (set-finder-comment (mypathname *package-user*) *om-package-tree*)
      (om-create-file *om-package-tree* (mypathname *package-user*)))
    (setf (changed-wsparams? *om-package-tree*) nil))
  )



(defun ws-load-user-pack ()
  (let* ((wspar (subseq (get-init-wsparams (mypathname *package-user*)) 2 5)))
    (setf (wsparams *om-package-tree*) (loop for item in wspar collect (eval item)))))



(defvar *loading-classes* nil)
(defvar *loading-methods* nil)
(defvar *loading-initmet* nil)

(defun fill-package-from-path (path package)
  (catch 'om-read-error
    (handler-bind ((error #'(lambda (err) (om-message-dialog (format nil "The user item ~s could not be loaded because of the following error: ~s" 
                                                                     (namestring path) (om-report-condition err)))
                              (throw 'om-read-error nil))))
      ;;; NEW : load lisp files
      (if (member (pathname-type path) '("lisp" "lsp") :test 'string-equal)
          (load path)
        (when (om-persistant-p path)
          (if (directoryp path)
              (let ((new-pack (make-instance 'OMPackage :name (name-of-directory path) :icon 22)))
                (really-add package new-pack)
                (setf (mypathname new-pack) path)
                (mapc #'(lambda (newpath) 
                          (fill-package-from-path newpath new-pack))
                      (om-directory  path :files t :directories t)))

            (let ((pathtype (file-type path)))
              (cond
               ((equal pathtype :CLAS)
                (let ((loadsedlist (load-a-class/method path)))
                  (push (second loadsedlist) *loading-classes*)
                  (push (third loadsedlist) *loading-initmet*)))
               ((equal pathtype :METH)
                (let ((loadsedlist (load-a-class/method path)))
                  (push (list path loadsedlist) *loading-methods*)))
               (t nil)))
            ))))))


(defmethod kernel-p ((self function)) nil)

(defun put-in-packpackages (path package badclasses)       
  (catch 'om-read-error
    (handler-bind ((error #'(lambda (err) (print (format nil "The user item ~s has not be loaded because of the following error: ~s" 
                                                                     (namestring path) (om-report-condition err)))
                              (throw 'om-read-error nil))))
      (when (om-persistant-p path)
        (if (directoryp path)
            (let ((new-pack (find-if #'(lambda (x) (string-equal (name x) (name-of-directory path)))
                                     (subpackages package))))
              (when new-pack
                (mapc #'(lambda (newpath) 
                          (put-in-packpackages newpath new-pack badclasses))
                      (om-directory  path :files t :directories t))))
          (let ((pathtype (file-type path)))
            (cond
             ((equal pathtype :CLAS)
              (let* ((loadsedlist (load-a-class/method path))
                     (name (interne (first loadsedlist))))
                (when (and (find-class name nil) (not (member (string name) badclasses :test 'string-equal)))
                  (addClass2Pack name package :protect nil :position (get-icon-pos (find-class name nil)))
                  )))
             ((equal pathtype :METH)
              (let* ((loadsedlist (load-a-class/method path))
                     (name (eval (second loadsedlist)))
                     (exists? (find-if #'(lambda (x) (string-equal (name x) (string name)))
                                       (functions package))))
                (when (and (fboundp name) (not exists?) (not (kernel-p (fdefinition name))))
                  (if (OMGenfun-p (fdefinition name))
                      (addGenFun2PAck name package :protect nil)
                    (addLispFun2Pack name package :protect nil)
                    ))))
             (t nil))))))))


(defun load-package-folder (dir inpack)
  (setf *loading-classes* nil
        *loading-methods* nil
        *loading-initmet* nil)
  (when (probe-file dir)
      (mapc #'(lambda (newpath) (fill-package-from-path newpath inpack))
            (om-directory dir :files t :directories t))
      (let* ((classes *loading-classes*)
             (methods *loading-methods*)
             (init-class-met *loading-initmet*)
             semi-methods final-methods badclasses)
        (setf badclasses (eval-initial-classes classes)) 
        (setf semi-methods (mapcar #'(lambda (elem) 
                                       (let ((new-semimethod (eval (second elem))))
                                         (when (ommethod-p new-semimethod) 
                                           (setf (mypathname new-semimethod) (first elem))
                                           new-semimethod))) methods))
        (setf final-methods (loop for elem in semi-methods
                                  when elem collect (define-really-method elem)))
        (initial-methods-for-classes init-class-met badclasses) 
        (mapc #'(lambda (newpath) (put-in-packpackages newpath inpack badclasses))
              (om-directory dir :files t :directories t))
        )))


(defun load-package-from-folders ()
  (let ((userpackdir (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*))
                                                       (list "user")))))
    (when (probe-file userpackdir)
      (load-package-folder userpackdir *package-user*))))
  

(defun load-a-class/method (path) 
  (let ((*package* (find-package :om))
        rep eofp)
    (WITH-OPEN-FILE (in path :direction :input :if-does-not-exist nil)
      (loop while (not (eq eofp :eof)) do
            (setf rep eofp)
            (setf eofp (read in nil :eof))
            (when (and (listp eofp) (equal (car eofp) 'load-lib-for-first))
              (eval eofp))))
    rep))
     



;(setf ppp (om-choose-file-dialog))
;(file-type ppp)
;(load-a-class/method ppp)

;(om-inspect *package-user*)

(defun upgrade-class (path)
  (let ((tmp (make-pathname :device (pathname-device path) :host (pathname-host path)
                                    :directory (pathname-directory path) :name (string+ (pathname-name path) ".omtemp")
                                    :type "omc")))
  (om-copy-file path tmp)
  (set-finder-comment tmp (make-instance 'omclass))
  tmp))


(defun import-user-class (package path)
  (let ((tmpfile nil)
        (oldfile nil))
    (if (or (and (equal (file-type path) :CLAS) (setf tmpfile path) t)
            (and (om-y-or-n-dialog (format nil "Sorry this file is not recognized as an OM class.~%If this is an old class file, OM can try to upgrade and load it.~%~%Try to upgrade ~s ?" (namestring path)))
                 (setf oldfile t 
                       tmpfile (upgrade-class path))))
        (let* ((loadsedlist (load-a-class/method path))
           (name (interne (first loadsedlist)))
           (classes (list (second loadsedlist)))
           (init-class-met (list (third loadsedlist)))
           (badclasses (eval-initial-classes classes)))
          (when (or (not (find-class name nil))
                    (om-y-or-n-dialog (format nil "Class ~s already exist. Replace previous class definition ?" name)))
            (initial-methods-for-classes init-class-met badclasses)
            (if (and (find-class name nil) (not (member (string name) badclasses :test 'string-equal)))   
                (progn
                  (addClass2Pack name package :protect nil :position (get-icon-pos (find-class name nil)))
                  (om-copy-file path (make-pathname :directory (pathname-directory (mypathname package))
                                                    :name (first loadsedlist) :type "omc")))
              (om-message-dialog (string+ "An error occured while loading class " (first loadsedlist)))))
      (when (and oldfile tmpfile) (om-delete-file tmpfile))
      ))))


(defun import-user-method (package path)
  (if (not (equal (file-type path) :METH))
      (progn (om-message-dialog "Sorry this file is not recognized as an OM method.")
        nil)
    (let* ((loadsedlist (load-a-class/method path))
           (name (eval (second loadsedlist)))
           (exists? (find-if #'(lambda (x) (string-equal (name x) (string name)))
                             (functions package)))
           (newpath (make-pathname :directory (pathname-directory (mypathname package))
                                   :name (pathname-name path) :type "ome"))
           (methods (list (list newpath loadsedlist)))
           semi-methods final-methods)
      (om-copy-file path newpath)
      (setf semi-methods (mapcar #'(lambda (elem) 
                                (let ((new-semimethod (eval (second elem))))
                                  (when (ommethod-p new-semimethod) 
                                    (setf (mypathname new-semimethod) (first elem))
                                    new-semimethod))) methods))
      (setf final-methods (loop for elem in semi-methods
                           when elem collect (define-really-method elem)))
      
      (when (and (fboundp name) (not exists?) (not (kernel-p (fdefinition name))))
        (if (OMGenfun-p (fdefinition name))
            (addGenFun2PAck name package :protect nil)
          (addLispFun2Pack name package :protect nil))))))
      

(defun import-user-package (package path)
  (let* ((newpath (make-pathname :directory (append (pathname-directory (mypathname package)) 
                                                   (last (pathname-directory path)))))
         (new-pack (make-instance 'OMPackage :name (name-of-directory path) :icon 22)))
    (really-add package new-pack)
    (setf (mypathname new-pack) newpath)
    (om-copy-directory path newpath)
    (load-package-folder newpath new-pack)))


(defun export-user-class (class)
  (let ((dir (om-choose-directory-dialog :prompt (string+ "Choose a directory for exporting class " (name class)))))
    (when dir 
      (om-copy-file (mypathname class)
                    (make-pathname :host (pathname-host dir) :device (pathname-device dir)
                                   :directory (pathname-directory dir)
                                   :name (pathname-name (mypathname class)) :type "omc")))))




(defun export-user-fun (path)  (om-beep))


(defun export-user-package (pack)
  (let ((dir (om-choose-directory-dialog :prompt (string+ "Choose a directory for exporting package " (name pack)))))
    (when dir 
      (om-copy-directory (mypathname pack)
                    (make-pathname :host (pathname-host dir) :device (pathname-device dir)
                                   :directory (append (pathname-directory dir) (list (name pack))))))))











