;;; This file loads the application files.
;;; Frank D. Valencia,16/10/96
;;; This is the version 2.0 of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.

(setf (logical-pathname-translations "Niobe2")
      '(("**;" "Solving Constraints:**;")))

(load "Niobe2:delay-packages")
(load "Niobe2:soft-packages")
(load "Niobe2:delay-pattern-match")
#+:PW (load "Niobe2:Functionals")
(load "Niobe2:tempovar")
(load "Niobe2:CSP Techniques;LISTAS")
(load "Niobe2:CSP Techniques;CURSORES-JER")
(load "Niobe2:CSP Techniques;nconstraint-jer")
(load "Niobe2:CSP Techniques;HF3C")
(load "Niobe2:CSP Techniques;extended-hash")
(load "Niobe2:Music CSP generator;MusicUser->CspTechn")
(load "Niobe2:temporal-points")
(load "Niobe2:New boxes;MCSPMenus")


 



