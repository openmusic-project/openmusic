(in-package :om)


;--------------------------------------------------
;Loading & compiling files 
;--------------------------------------------------


(defvar  *H-files* nil)

(setf *H-files* 
  '(
    "code;classes"
    "code;accords"
    "code;tools"

    "code;fonctions;marches-harmonie"
    "code;fonctions;arpeges"

    "code;interface;tools-data"
    "code;interface;interface-tonalite"
    "code;interface;score"

    ;"code;old-carlos;tonality"
    ;"code;old-carlos;arpege"
 
))



(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *H-files*))