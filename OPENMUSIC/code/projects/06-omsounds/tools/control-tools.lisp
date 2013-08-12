(in-package :om)

(defmethod! adsr (amp1 amp2 a d s r &optional (decimals 3))
    :icon 110
    :indoc '("max amplitude" "sustained amplitude" "attack time" "decay time" "sustain time" "release time" "BPF precision")
    :initvals '(1 0.8 0.2 0.1 0.5 0.2 3)
    :doc "Generates an ADSR BPF (Attack-Decay-Sustain-Release)

If either <amp2> or <d> is NIL, generates a simple envelope with attack and release time (no decay).

Note that this functions defines absolute parameters (amplitudes / duration) for the ADSR envelope, and therefore slightly differs from the 'standard' definition of ADSR (Attack time, Decay time, Sustained relative amplitude, Release time).
"
    (if (and amp2 d)        
        (simple-bpf-from-list (list 0 a (+ a d) (+ a d s) (+ a d s r))
                              (list 0 amp1 amp2 amp2 0)
                              'bpf decimals)
      (simple-bpf-from-list (list 0 a (+ a s) (+ a s r))
                              (list 0 amp1 amp1 0)
                              'bpf decimals)))


;;; by marco from Chant tools
(defmethod! normalize (list &optional (val 1.0))
   :initvals '(nil 1.0)
   :indoc '("list to normalize" "normalize value")
   :outdoc '("normalized list")
   :doc "Normalize the input <list> between 0.0 and <val>.

If <val> is a number, normalize between 0.0 and <val>.
If <val> is a list, normalize between <(first val)> and <(second val>)
If <val> is a list with only one number, normalize keeping the ratio between the min and max in the list."

  (let ((max (list-max list))
        (min (list-min list)))
   (cond ((null val) (om-scale list 0.0 1.0))
         ((numberp val) (om-scale list 0.0 val))
         ((numberp (cadr val)) (om-scale list (car val) (cadr val)))
         (t (om* (om/ list max) (car val))))))

