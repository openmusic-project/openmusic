;; ==================================================================================== 
;;                                PAQUET   OUTILS
;; ==================================================================================== 
;;
;;          V1.1
;;                                 functions by Mikhail Malt   1998 Paris IRCAM




(in-package "ALEA")


;We just let here this function in order to allow a conceptual compatibility
;among the old PW patches using this functions and the news in OM



(om::defmethod! filtre1 ((x number) (binf number) (bsup number)  )
  :initvals '(nil 0 100) 
  :indoc '("any single value" "lower boundary" "upper boundary")
  :icon 240 
  :doc "Filter of data with elastic boundaries."
  (if (> x bsup) (- (* 2 bsup) x)
      (if (< x binf) (- (* 2 binf) x)  x) ))

(om::defmethod! filtre2 ((x number) (binf number) (bsup number)) 
  :initvals '(nil 0 100) 
  :indoc '("any single value" "lower boundary" "upper boundary")
  :icon 240 
  :doc "Filter of data with absorbant boundaries."
  (if (> x bsup) bsup (if (< x binf) binf  x)))


(om::defmethod! filtre3 ((x number) (binf number) (bsup number)) 
  :initvals '(nil 0 100) 
  :indoc '("any value (atom or list)" "lower boundary" "upper boundary")
  :icon 240 
  :doc "Filters a list of data data with elastic boundaries."
  (let ((x (om::list! x))) 
    (mapcar #'(lambda (l) (om::om-round (filtre1 l  bsup  binf ) 3)) x)))

(om::defmethod! filtre4 ((x number) (binf number) (bsup number)) 
  :initvals '(nil 0 100) 
  :indoc '("any value (atom or list)" "lower boundary" "upper boundary")
  :icon 240 
  :doc "Filters a list of data with absorbant boundaries."
  (let ((x (om::list! x)))
    (mapcar #'(lambda (l) (om::om-round (filtre2 l  bsup  binf ) 3)) x)))


(om::defmethod! zoom1 ( (x number )  (binf2 number)  (bsup2 number))  
  :initvals '(nil 0 100) 
  :indoc '("any single value" "lower boundary" "upper boundary")
  :icon 240 
  :doc "Linearly transforms the variation range of a variable
 <x> which is located between <0> and <1> in a range between <binf2> and <bsup2>."
  (om::om+ binf2 (om::om*  (om::om- bsup2 binf2)  x )))

(om::defmethod! zoom2 ((x number ) (binf1 number) (bsup1 number) (binf2 number)  (bsup2 number))  
  :initvals '(nil 0 100 0 100) 
  :indoc '("any single value" "lower input boundary" "upper input boundary" "lower output boundary" "upper output boundary")
  :icon 240 
  :doc "Linearly transforms the variation range of a variable <x> which is
 located between <binf1> and <bsup1> in a range between <binf2> and <bsup2>."
  (om::om+ binf2 (om::om* (om::om/ (om::om- bsup2 binf2) (om::om- bsup1 binf1)) (om::om- x binf1))))



(om::defmethod! zoom3 ((x number ) (binf1 number) (bsup1 number) (binf2 number)  (bsup2 number)) 
  :initvals '(nil 0 100 0 100) 
  :indoc '("any value (atom or list)" "lower input boundary" "upper input boundary" "lower output boundary" "upper output boundary")
  :icon 240 
  :doc "Linearly transforms the variation range of a list of variables <x> which
 are located between <binf1> and <bsup1> in a range between <binf2> and <bsup2>."
  (let ((x (om::list! x))) 
    (mapcar #'(lambda (l) (om::om-round (zoom2 binf1 bsup1 binf2 bsup2 l) 3))  x)))

(om::defmethod! zoom4 ( (x number ) (binf2 number)  (bsup2 number)   ) 
  :initvals '(nil 0 100) 
  :indoc '("any value (atom or list)"  "lower output boundary" "upper output boundary")
  :icon 240 
  :doc "Linearly transforms the variation range of a list of variables <x> which is located between <0> and <1> 
in a range between <binf2> and <bsup2>."
  (let ((x (om::list! x))) 
    (mapcar #'(lambda (l) (om::om-round (zoom1   binf2 bsup2 l) 3))  x)))






