(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------
(defvar *Zn-files* nil)

(setf *Zn-files* '(
                   "circle;cercle"
                   "circle;cercle-analysis"

                   "sieves;cribles"


                   "screamer;package"
                   "screamer;screamer"
                   
                   "groups;tools"
                   "groups;zn;scream-groups"
                   "groups;zn;orbites"
                   "groups;zn;tl-zn"

                   "groups;dn;pcs"
                   "groups;dn;ominterface"

                   "sequences;lewin1"
                   "sequences;suites"

                   ;"Polynomials;XXXX"
                   
                   "canons;vuza;canons"
                   "canons;noll;augcanons"
                   "canons;amiot;cylocanons"
                                               
                   "package"
                   ))

(push :screamer *features*)


;--------------------------------------------------
;Loading files 
;--------------------------------------------------


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *Zn-files*))


