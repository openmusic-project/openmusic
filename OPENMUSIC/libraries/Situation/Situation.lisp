;;; SITUATION LIBRARY

(in-package :om)

(unless (member :OM *features*) (setf *features* (cons :OM *features*)))

(defun assoc-2-lists (list1 list2)
  (do* ((lst1 list1 (cdr lst1))
        (lst2 list2 (cdr lst2))
        (result nil))
       ((null lst1) result)
    (setq result (cons (list (car lst1)
                             (car lst2))
                       result))))

(defun closure-function (fun) fun)

(defvar  *situation-files* nil)

(setf *situation-files* 
      '(
        "sources;delay-packages"
        "sources;soft-packages"
        "sources;delay-pattern-match"
        ;;;"sources;Functionals"
        "sources;tempovar"
        "sources;csptech;listas"
        "sources;csptech;cursores-jer"
        "sources;csptech;nconstraint-jer"
        "sources;csptech;hf3c"
        "sources;csptech;extended-hash"
        "sources;csptech;weak-csp-solver"
        "sources;musiccspgenerator;musicuser-csptechn"
        "sources;utilities;om-series-code"
        "sources;temporal-points"
        "sources;newboxes;mcspmenus"
        
        ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
       *situation-files*))

(in-package :common-lisp-user)

(defun function-args (fun) 
  (values (om::min-inp-number fun) (length (om::get-keywords-fun fun)) (length (om::get-optional-fun fun))))

(om::fill-library 
 '(("Solvers" nil nil (om::Csolver om::Wsolver om::add-Csolver) nil)
   ("Standard Constraints" 
    (("Distances Control" nil nil (om::i-dst_filt om::i-dst/reg_filt om::i/i-dst_filt om::i-dst_rnw 
                                                  om::x-dst_filt om::x-dst/reg_filt om::x-dst_rnw) nil)
    ;("Hint Control" nil nil (om::hint-filt om::hint/reg-filt om::hintrnw/vl-spcf) nil)
     ("Points Control" nil nil (om::pts_filt om::pts_rnw om::x-pts_filt om::x-pts_rnw) nil)
     ("Profiles Control" nil nil (om::x_prof om::x/x_prof om::x-bpf_prof) nil))
    nil nil nil)
   ("User Constraints" nil nil (om::user-cnstr om::prev-instances
                                               om::Wprev-instances om::Wbest-so-far) nil)
   ("Generic Problems" nil nil (om::variable-domains om::Generic-Cnstr om::done-instances om::current-variable
                                                     ;;om::all-pairs om::all-sequences
                                                     ) nil)
   ("Utilities" nil nil (om::ch-sol om::rtm-sol om::part-sol
                                    om::bpf-ambdef om::default-fill
                                    om::all-permutations om::combinations
                                    om::comb-with-reps om::all-replacements) nil))
 (om::find-library "Situation"))
  
 

(print "
===============================
Situation for OM
by Camilo Rueda (c) IRCAM 1992
==============================="
)


