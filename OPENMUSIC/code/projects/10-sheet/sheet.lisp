(in-package :om)


;--------------------------------------------------
;Loading & compiling files 
;--------------------------------------------------


(defvar  *sheet-files* nil)

(setf *sheet-files* 
  '(
        "sheetobjects;sheet"
        ;"sheetobjects;sheet-persistant"
        "sheetobjects;sheet-patch"
        
        "sheeteditor;sheet-window"
        "sheeteditor;sheet-patch-editor"
        "sheeteditor;sheet-spaces"
        "sheeteditor;sheet-special-cases"
        ;"sheeteditor;sheetpages"

        "sheetobjects;sheet-analysis"
        
))
 


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *sheet-files*))


(defvar *sheetpackage* (omNG-protect-object (omNG-make-new-package "Sheet")))
(defvar *sheettoolspackage* (omNG-protect-object (omNG-make-new-package "Sheet Tools")))
(addPackage2Pack *sheetpackage* *scorepackage*)
(addPackage2Pack *sheettoolspackage* *sheetpackage*)

(AddClass2Pack '(OMSheet) *sheetpackage*)
(AddClass2Pack '(sheet-track sheet-track-obj sheet-access) *sheettoolspackage*)

(push :omsheet *features*)

;; re-generate the score section
(add-ref-section (gen-ref-entries *scorepackage*))