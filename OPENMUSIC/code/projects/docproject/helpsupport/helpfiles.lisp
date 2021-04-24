(in-package :om)


;;=========THIS IS THE SPECIAL FOLDER WITH help files===================

(defvar *om-helpfolder* nil "The helpfiles folder in the workspace.")

(defclass OMhelpFolder (OMWorkSpace) ()
   (:documentation "This is the special folder which contains help files. #enddoc#
#seealso# (OMFolder OMinstance) #seealso#"))

;--------------------------------------------------
;INTERFACE
;--------------------------------------------------


(defmethod get-elements ((self OMhelpFolder))
   "helpfiles folder's elements are OMinstances." (elements self))

;(defmethod get-class-icon ((self OMhelpFolder)) 'globals-folder-icon)

(defmethod get-editor-class ((self OMhelpFolder)) 'HelpFilesEditor)

(defmethod OpenEditorframe ((self OMhelpFolder))
   "Show the globals folder 'self' as a container."
   (or (editorframe self)
       (panel (open-new-nonrelationFrame self "Help Files" (get-elements self)))))

(defmethod elements-pathname ((self OMhelpfolder)) 
   (om-make-pathname 
    :device (mypathname self)
    :directory  (pathname-directory (mypathname self))))


;from ws-load-element
(defun help-load-element (path &optional (i 0))
  (let* ((*package* (find-package :om))
         newicon newobj)
    (handler-bind (
                   (error #'(lambda (c) 
                              (om-message-dialog (format nil "Error while loading ~A in WorkSpace: ~D"
                                                         (namestring path) 
                                                         (om-report-condition c)
                                                         )
                                                 :size (om-make-point 300 300))
                              (om-abort)
                              )))
      
      (if (and (directoryp path) (not (systemdirectoryp path)))
          (let ((j -1) (elements (om-directory path :files t :directories t)))
            (setf newobj (omNG-make-new-folder (car (last (pathname-directory path)))))
            (setf (mypathname newobj) (make-pathname 
                                       :device (pathname-device path)
                                       :directory  (append (butlast (pathname-directory path)) (list (name newobj)))))
            (setf (elements newobj) (remove nil (mapcar #'(lambda (elt) (ws-load-element elt (incf j))) elements))))
        (when (and (stringp (pathname-name path))
                   (not (string-equal "" (pathname-name path))))
          (let ((type (pathname-type path)))
            (when (string-equal type "tmp")
              (let* ((newpath1 (namestring (om-make-pathname :directory (pathname-directory path) :name (pathname-name path))))
                     (newpath2 (om-make-pathname :directory (pathname-directory path) :name (string+ (pathname-name newpath1) " (recovered)") 
                                                 :type (pathname-type newpath1))))
                (rename-file path newpath2)
                (setq path newpath2))))
          (let ((obj (object-from-file path)))
            (when obj
              (setf (mypathname obj) path)
              (setf newobj obj)
              ))))
      (when newobj
        (let* ((wsparams (get-init-wsparams path))
               (wspar (subseq wsparams 2 5))
               (doc (str-with-nl (nth 5 wsparams))))
          (setf newicon (nth 6 wsparams))
          (setf (wsparams newobj) (loop for item in wspar collect (eval item)))
          (setf (doc newobj) doc)
          (setf (omversion newobj) (car wsparams))
          (setf (create-info newobj) (list (nth 8 wsparams) (nth 9 wsparams)))
          (when (directoryp path) (setf (presentation newobj) (or (nth 7 wsparams) 0)))
          (when newicon (setf (icon newobj) (eval newicon)))
          (pushr (format nil "Element ~s loaded" path) *import-log*)
          newobj)))))

(defmethod load-elements ((self OMhelpfolder))
   "Make instances for all elements in 'self', but their are not yet loaded."
   (let ((j -1)
         ;(skip-libs *skip-libs*)
         (elements (om-directory 
                    (om-make-pathname :device (mypathname self) 
                                      :directory (pathname-directory (mypathname self))) 
                    :files t :directories t)))
     
     (setf *loading-ws* t)
     (setf (elements self) (remove nil (mapcar #'(lambda (x) (help-load-element x (incf j))) elements) :test 'equal))
     (setf *loading-ws* nil)
     ))
;--------------------------------------------------
;Tools
;--------------------------------------------------

(defun init-helpfolder ()
   "Make an instance of helpfiles folder and store it in the *om-helpfolder* variable." 
   (setf *om-helpfolder* (omNG-protect-object (make-instance 'OMhelpFolder
                                                   :Name "helpfiles"
                                                   :icon 23
                                                   ;:elements (gethelplist)
                                                   )))
   (setf (mypathname *om-helpfolder*) 
         (make-pathname :directory  (append (pathname-directory (mypathname *current-workSpace*))
                                                                  (list "helpfiles"))))
   ;(print (elements-pathname *om-helpfolder*))
     (load-elements *om-helpfolder*) 
   )


 
  ;(om-directory (make-pathname :directory  (append (pathname-directory (mypathname *current-workSpace*)) (list "helpfiles"))) :directories nil :files t)

;THIS IS NEEDED TO INIT THE FOLDER
;(init-helpfolder)

;(om-inspect *om-helpfolder*)
;(openobjecteditor (car (elements *om-helpfolder*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;to be found in omlily
(defun replace-string (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

;(replace-string "om>" ">" ")") ;apparement OK
;(replace-string "om//" "//" "-mod")
;(replace-string "om/" "/" "-div")
;(replace-string "om<" "<" "(") ;apparement OK


(defun open-helpfile (patch)
  (let* ((container (editorframe  *current-workSpace*))
         (pos (om-make-point 0 0))
         (directory (merge-pathnames (make-pathname :directory '(:relative "resources" "helpfiles")) *om-root*))
         (ref (reference (object patch)))
         (name (if (omclass-p ref) (name ref) ref))
         (file (format nil "~A.omp" name))
         (helpfile (merge-pathnames (string+ file) directory))
         (wshelp (merge-pathnames (make-pathname :directory '(:relative "helpfiles")) 
                                  (mypathname *current-workSpace*))))
   
    (init-helpfolder)
    ;(oa::om-delete-directory wshelp)
    ;create if necesary heplfiles folder:
    (if (not (check-folder wshelp))
        (make-new-folder container folder (om-make-point 0 0)))

    ;create and put helpfile in folder:
    (if (probe-file helpfile)
        (progn 
          (ws-import-element helpfile wshelp) 
          
    ;necessary to update new imported:
          (load-elements *current-workSpace*)
          (load-elements *om-helpfolder*)
    
    ;open patch:
          (let* ((names (mapcar #'name (get-elements *om-helpfolder*)))
                 (poshelp (position (format nil "~S" name) names :test 'equal)))
            ;(print (list names poshelp))
            ;THIS OPENS THE PATCH FROM THE CURRENT WS:
            (openobjecteditor (nth poshelp (get-elements *om-helpfolder*)))
            ))
      (capi::display-message "Not Available Yet..."))
    ))

;;(trouver subst for reserved caracters : < > / * et :)

;le chemin pour les help patches (a creer)
;(merge-pathnames (make-pathname :directory '(:relative "resources" "helpPatches")) *om-root*)
;(merge-pathnames (make-pathname :directory '(:relative "resources" "tutorials" "OM-Tutorials")) *om-root*)

;;faire une fonction pour ouvrir un patch externe en l'important dans les helpfiles qui deviennent finalement un temp file???
