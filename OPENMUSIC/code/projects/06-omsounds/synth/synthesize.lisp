(in-package :om)

(defmethod! synthesize (elements &key
                                 (name "my_synt")
                                 sr
                                 (rescale 0.0)
                                 (run t) (evt-test  nil) resolution
                                 ;For Csound
                                 kr tables (nchnls nil) inits
                                 ;For Chant
                                 patch duration)
   :doc "Synthesizes a sound from <elements>.

Depending on the synthesis modules and libraries loaded in OM, <elements> can be 
- an OMChroma object, or OM-Chant events 
- a list of such objects
"
   :initvals '(nil)
   :indoc '("something to synthesize...")
   :icon 410 
   (om-beep-msg (format nil "Elements of type ~s are not supported for SYNTHESIZE." (type-of elements))))


(defmethod! synthesize ((elements list) &key (name "my_synt") sr (rescale 0.0) (run t) (evt-test  nil) resolution 
                                 kr tables (nchnls nil) inits
                                 patch duration)
     
       (let* ((list-of-lists (group-element-list (remove nil elements)))
              (rep (if (= 1 (length list-of-lists))
                       (funcall (synthesize-method (caar list-of-lists)) (car list-of-lists)
                                :name name :sr sr :rescale rescale :run run :evt-test evt-test :resolution resolution 
                                :kr kr :tables tables :nchnls nchnls :inits inits :patch patch :duration duration)
                     (remove nil (loop for item in list-of-lists 
                                                       for i = 1 then (+ i 1) collect
                                                       (when (synthesize-method (car item))
                                                         (funcall (synthesize-method (car item)) item
                                                                  :name (string+ name "-temp-" (integer-to-string i))
                                                                  :sr sr :rescale rescale :run run :evt-test evt-test :resolution resolution 
                                                                  :kr kr :tables tables :nchnls nchnls :inits inits
                                                                  :patch patch :duration duration)))))))
         (if (and run (> (length list-of-lists) 1))
             (save-sound (reduce 'sound-mix rep :key 'get-om-sound-data) (if (pathnamep name) name (outfile (string+ name ".aiff"))))
           rep)
         ))


(defmethod synthesize-method ((self t)) nil)

(defparameter *synthesis-element-types* nil)
(defparameter *synthesis-element-tests* nil)

(defun group-element-list (list)
  (let ((tests-list (make-list (length *synthesis-element-tests*)))
        (types-list (make-list (length *synthesis-element-types*)))
        (rest-list nil))
    (loop for item in list do
          (let ((ok nil))
            (loop for test in *synthesis-element-tests*
                  for i = 0 then (+ i 1) 
                  while (not ok) do 
                  (when (funcall test item)
                    (pushr item (nth i tests-list))
                    (setf ok t)))
            (unless ok
              (loop for typ in *synthesis-element-types*
                    for j = 0 then (+ j 1) 
                    while (not ok) do 
                    (when (subtypep (type-of item) typ)
                      (pushr item (nth j types-list))
                      (setf ok t))))
            (unless ok 
              (pushr item rest-list))))
    (remove nil (append tests-list types-list (list rest-list)))))
              
(defmethod update-inputs ((self (eql 'synthesize)) inputs) 
  (if (find (name (car inputs)) '("synth" "synthetiser") :test 'string-equal)
      (cdr inputs) inputs))


;;;=================================
;;; Normalization
;;; depends on the different loaded normalization modules
;;;=================================

(defmethod! om-normalize (in out val &optional resolution) 
  (general-normalize *normalizer* in out val resolution))

(defmethod general-normalize ((self t) in out val &optional resolution) 
  (if self
      (om-beep-msg (format nil  "ERROR: Normalization module ~A not loaded !!!" self))
    (om-beep-msg (format nil  "ERROR: No normalization module selected !!!"))))
    




