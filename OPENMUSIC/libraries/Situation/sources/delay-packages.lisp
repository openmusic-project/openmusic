;;;====================================
;;; package definitions for delayed CSP
;;; by Camilo Rueda (c) IRCAM 1992
;;;====================================
 
(defpackage "DELAYED-EVAL"
  (:use "COMMON-LISP" )
  (:export "DELAY-EXP" "FORCE" "DELAY-CDR" "DELAY-CONS" "DELAY-NTH" "DELAY-NTHCDR"
           "MAKE-SERIES" "MULTI-MAKE-SERIES" "SERIES-RANGE" "SERIES-FROM-LIST"
           "FILTER-SERIES" "DOLIST-DELAYED" "APPEND-SERIES"
           "ALL-PERMUTATIONS" "SHUFFLE-PERMUT" "COMBINATIONS" "COMB-WITH-DUPS"
           "CARTESIAN" "SERIES-TO-LIST" "ALL-REPLACEMENTS" "LAZY-FILTER"
           "DELAY-FIRST" "LAZY?"))

(defpackage "DELAYBACKTR"
  (:use "COMMON-LISP"  "DELAYED-EVAL")
  (:export "SUCCEED" "DELAY-FORWARD-CHECK" "NICELY-PUT"
           "*ALL-SOLUTIONS*" "*DOMAINS*" "*DONE-FOR-USER*" "*NUM-VARS*"
           "NOT-NULL-SERIES" "USER-ACCUMULATE" "USER-DISPLAY-PROCESS" "SINGLETON-OF"
           "*MAX-ADVANCE*"))

(defpackage "DELAYCONSTR"
  (:use "COMMON-LISP"  "DELAYBACKTR" "DELAYED-EVAL")
  (:export "GET-MAX-PATTERNS-LENGTH" "SET-PAT-LENGTH" "SET-DOMAIN-CONSTRAINT-FUN"
           "*IGNORE-SHORTER-EXPRESSIONS*"))

(defpackage "CSP"
  (:use "COMMON-LISP" )
  (:export ))