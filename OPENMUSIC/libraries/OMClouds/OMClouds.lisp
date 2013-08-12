
;;; OMCLOUDS  LIBRARY


(in-package :om)

(defvar  *clouds-files* nil)

(setf *clouds-files* 
      '(
        "sources;general"
        "sources;cycle"
        "sources;perm"
        "sources;liste"  
        ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *clouds-files*))


(om::fill-library '(("Constraints Definition" nil
                                              nil (cree-varliste cree-varcycle cree-varperm resolution) nil)
                    ("Constraints Primitives" nil
                                              nil (=c equalc <c <=c /=c notequalc andc orc 
                                                      evenc oddc minimizec memberc alldiffc cardc) nil)
                    )
                  (find-library "OMClouds"))


(print "
=================================
OMClouds library
by Ch. Truchet, Ircam 2003
=================================
")