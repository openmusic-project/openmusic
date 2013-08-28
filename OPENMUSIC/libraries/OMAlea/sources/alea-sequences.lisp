
;; ==================================================================================== 
;;                                PAQUET   SEQUENCES ALEATOIRES
;; ==================================================================================== 

;;          V1.1
;;                                 functions by Mikhail Malt   1998 Paris IRCAM


(in-package "ALEA")

 

(om::defmethod! not-centr ((nc number)  (intv number)) 
  :initvals '(6000 7) 
  :indoc '("center pitch" "half bandwidth interval")
  :icon 240 
  :doc "Returns an aleatoric pitch value between <nc-intv> and <nc+intv>." 
  (om::om+ nc (* 100(- intv (om-random-value (+ 1 (* 2 intv)))))))

(om::defmethod! alea-seq ((nc number) (intv number) (long integer)) 
  :initvals '(6000 7 10) 
  :indoc '("center pitch" "half bandwidth interval" "sequence length")
  :icon 240 
  :doc"Aleatoric sequence of a uniform distribution,
 between <nc-intv> and <nc+intv> with a  length <long>."
  (let ((res))
    (om::for(i 1 1 long) (om::newl res (om::om+ (if (listp nc) (car nc) nc) (* 100(- intv (om-random-value (+ 1 (* 2 intv)))))))) 
    (nreverse res))) 


(om::defmethod! linea-seq (  (long integer) (liminf number) (limsup number)) 
  :initvals '(12 4800 8400) 
  :indoc '("sequence length" "lower boundary" "upper boundary")
  :icon 240 
  :doc "Aleatoric sequence of a lineair distribution, 
between <lim.inf> and <lim.sup> with a length <long>."
  (let ((res))
    (om::for(i 1 1 long) (om::newl res (om::om+ liminf (*  (- limsup liminf) 
                                                   (- 1 (sqrt (/ (om-random-value 1000) 1000))))))) 
    (nreverse res))) 

(om::defmethod! triang-seq ((long integer) (liminf number) (limsup number)) 
  :initvals '(12 4800 8400) 
  :indoc '("sequence length" "lower boundary" "upper boundary")
  :icon 240 
  :doc" Aleatoric sequence of a triangular distribution, 
between <lim.inf> and <lim.sup> with a length <long>."
  (let ((res))
    (om::for(i 1 1 long) (om::newl res (om::om+ liminf (* (- limsup liminf) 
                                                  (/ (+ (/(om-random-value 1000.0) 1000.0) (/ (om-random-value 1000.0) 1000.0)) 2)
                                                  )))) 
    (nreverse res))) 




