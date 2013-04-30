;;;==================================================================================================================================================================
;;;===================================================================FAUST AUDIO API================================================================================
;;;==================================================================================================================================================================
(in-package :oa)

(defvar *effects-lists* nil)
(defvar *faust-effects-register* (make-hash-table))
(defvar *faust-effects-by-track* (make-hash-table))
(defconstant *max-effects-number* (* 4 las-channels))

;===============================================================================================================================================================
;============================================================================ API ==============================================================================
;===============================================================================================================================================================

(export '(
          las-faust-init-system
          las-faust-make-effect
          las-faust-get-effect-json
          las-faust-get-effect-control-count
          las-faust-get-effect-control-params
          las-faust-get-effect-control-value
          las-faust-set-effect-control-value
          las-faust-effect-cleanup
          las-faust-add-effect-to-register
          las-faust-find-effect-in-register
          las-faust-add-effect-to-track
          las-faust-remove-effect-from-track
          las-faust-set-effect-track-in-register
          las-get-number-faust-effects-register
          las-faust-get-track-effects-name
          las-faust-get-track-effects-pointer
          las-faust-unplug-all
          las-faust-effect-already-plugged-?
          
          *faust-effects-register*
          ) :om-api)


(defun las-faust-init-system ()
  (progn
    (init-faust-effects-register)
    (ResetEffectsLists *audio-player-visible*)))

(defun las-faust-unplug-all ()
  (ResetEffectsLists *audio-player-visible*))

(defun las-faust-make-effect (string)
  (let ((result-state 1)
        result-pointer
        result-error) 
    (setf result-pointer (las::MakeFaustAudioEffect string))
    (if (las::las-null-ptr-p result-pointer)
        (progn
          (setf result-error (las::getlastliberror))
          (setf result-state 0)))
    (list result-state result-pointer result-error)))

(defun las-faust-get-effect-json (pointer)
  (las::getjsoneffect pointer))

(defun las-faust-get-effect-control-count (pointer)
  (las::getcontrolcount pointer))

(defun las-faust-get-effect-control-params (pointer number)
  (las::getcontrolparam pointer number))

(defun las-faust-get-effect-control-value (pointer number)
  (las::getcontrolvalue pointer number))

(defun las-faust-set-effect-control-value (pointer number val)
  (las::SetControlValue pointer number val))

(defun las-faust-effect-cleanup (pointer)
  (let ((n (get-number-faust-effects-register))
        (track (- (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*)) 1)))
    (if (>= track 0)
        (las::RemoveAudioEffect (gethash track *effects-lists*) pointer))
    (setf (gethash (find-effect-index-in-register pointer) *faust-effects-register*) (list nil track "faust-effect"))
    (pack-faust-effects-register n)))

(defun las-faust-add-effect-to-register (pointer name)
  (add-faust-effect-to-register pointer name))

(defun las-faust-find-effect-in-register (pointer)
  (find-effect-index-in-register pointer))

(defun las-faust-add-effect-to-track (pointer name track)
  (let ((liste (gethash track *faust-effects-by-track*))
        (i 0)) 
    (while (gethash i liste)
      (incf i))
    (setf (gethash i liste) (list pointer name))
    (add-faust-effect-to-list pointer (gethash track *effects-lists*))))

(defun las-faust-remove-effect-from-track (pointer track)
  (remove-effect-in-track-register pointer track)
  (remove-faust-effect-from-list pointer (gethash track *effects-lists*)))

(defun las-faust-set-effect-track-in-register (pointer track)
  (setf (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*)) track))

(defun las-get-number-faust-effects-register ()
  (get-number-faust-effects-register))

(defun las-faust-get-track-effects-name (track)
  (let ((liste (gethash track *faust-effects-by-track*))
        (i 0)
        (res (list)))
    (while (gethash i liste)
      (setf res (append res (list (nth 1 (gethash i liste)))))
      (incf i)
      )
    res
    ))


(defun las-faust-get-track-effects-pointer (track)
  (let ((liste (gethash track *faust-effects-by-track*))
        (i 0)
        (res (list)))
    (while (gethash i liste)
      (setf res (append res (list (nth 0 (gethash i liste)))))
      (incf i)
      )
    res
    ))

(defun las-faust-effect-already-plugged-? (pointer)
  (let ((x 0)
        (y 0)
        (found 0)
        res
        curlist)
    (while (= found 0)
           (setf curlist (gethash x *faust-effects-by-track*))
           (while (and (= found 0) (gethash y curlist))
             (if (eq pointer (nth 0 (gethash y curlist)))
                 (progn
                   (setf res (list x y))
                   (setf found 1))
               (progn
                 (incf y)
                 (if (> y *max-effects-number*)
                     (progn 
                       (setf res nil)
                       (setf found 1))))))
           (setf y 0)
           (incf x)
           (if (>= x las-channels)
               (progn
                 (setf found 1)
                 (setf res nil))))
    res))
;///////////////////JSON parsing///////////////////////
;(setf (ui-type self) (cdr (nth (- (length (nth 1 (nth 0 effect-json))) 1) (nth 1 (nth 0 effect-json)))))
;(setf (ui-name self) (cdr (nth (- (length (nth 1 (nth 0 effect-json))) 2) (nth 1 (nth 0 effect-json)))))
;(print effect-json)
;(effect-ui (nth (- (length effect-json) 1) effect-json))
;(print effect-ui)
;(print (ui-name self))
;(print (length ui-items))
;(print (cdr (nth (- (length (nth 1 (nth 0 effect-json))) 2) (nth 1 (nth 0 effect-json)))))
;//////////////////////////////////////////////////////



;===============================================================================================================================================================
;=========================================================================== TOOLS =============================================================================
;===============================================================================================================================================================

(defun plug-faust-effect-list-on-channel (player effectlist channel &optional (fadein 100) (fadeout 100))
  (las::SetEffectListChannel player channel effectlist fadein fadeout))

(defun ResetEffectsLists (player)
  (setf *effects-lists* (make-hash-table))
  (setf *faust-effects-by-track* (make-hash-table))
  (loop for i from 0 to (- las-channels 1) do 
      (setf (gethash i *effects-lists*) (las::MakeAudioEffectList))
      (plug-faust-effect-list-on-channel player (gethash i *effects-lists*) i)
      (setf (gethash i *faust-effects-by-track*) (make-hash-table))))

(defun init-faust-effects-register ()
    (loop for i from 0 to *max-effects-number* do
          (setf (gethash i *faust-effects-register*) (list nil 0 "faust-effect")))) ;(ptr track name)

;;;//////////////////REGISTER TOOLS/////////////////////////////
(defun add-faust-effect-to-register (ptr name track)
  (let ((i 0))
    (while (nth 0 (gethash i *faust-effects-register*))
          (incf i)
          (if (> i *max-effects-number*) (setf i nil)))
    (if i (setf (gethash i *faust-effects-register*) (list ptr track name)))
  ))


(defun remove-faust-effect-from-list (ptr list)
  (las::RemoveAudioEffect list ptr))

(defun add-faust-effect-to-list (ptr list)
  (las::AddAudioEffect list ptr))

(defun get-number-faust-effects-register ()
  (let ((i 0))
    (while (nth 0 (gethash i *faust-effects-register*))
          (incf i))
    i))

(defun find-hole-index-in-faust-effects-register ()
  (let ((i 0)
        (found 0)
        (ptr nil)
        (marker 0)
        (res nil))
    (while (= found 0) 
      (setf ptr (nth 0 (gethash i *faust-effects-register*)))
      (if (= 1 marker)
          (if (eq ptr nil)
              (let () (setf res nil) (setf found 1))
            (let () (setf res (- i 1)) (setf found 1)))
        (if (eq ptr nil) (setf marker 1)))
      (incf i)
      (if (> i *max-effects-number*) (let ()
                                       (setf found 1)
                                       (setf res nil))))
    res))

(defun find-effect-index-in-register (ptr)
  (let ((i 0)
        (found 0))
    (while (= found 0)
      (if (eq ptr (nth 0 (gethash i *faust-effects-register*)))
          (setf found 1)
        (incf i))
        (if (> i *max-effects-number*)
            (progn 
              (setf found 1)
              (setf i nil))
         ))
    i))

(defun pack-faust-effects-register (n)
  (let ()
    (if (find-hole-index-in-faust-effects-register)
        (let ((index (find-hole-index-in-faust-effects-register)))
          (loop for i from index to (- n 1) do
            (setf (gethash i *faust-effects-register*) (gethash (+ i 1) *faust-effects-register*)))))))



(defun remove-effect-in-track-register (pointer track)
  (let ((i 0)
        (found 0))
    (while (= found 0) 
      (if (not (eq pointer (car (gethash i (gethash track *faust-effects-by-track*))))) 
          (progn 
            (incf i)
            (if (> i *max-effects-number*)
                (progn 
                  (setf found 1)
                  (setf i nil))))
        (setf found 1)))
    (if i
        (progn
          (setf (gethash i (gethash track *faust-effects-by-track*)) nil)
          (loop for k from i to *max-effects-number* do
                (setf (gethash k (gethash track *faust-effects-by-track*)) (gethash (+ k 1) (gethash track *faust-effects-by-track*))))))))
