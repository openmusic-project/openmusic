;;;==================================================================================================================================================================
;;;===================================================================FAUST AUDIO API================================================================================
;;;==================================================================================================================================================================

(in-package :oa)

(export '(
          las-make-faust-effect
          ) :om-api)

(defun las-make-faust-effect (txt)
  (let ((parlist (om::list-of-lines (om::buffer-text txt)))
        (effect-code)
        (result))
    (loop for line in parlist do
          (setf effect-code (concatenate 'string effect-code " " line)))
    (setf result (las::MakeFaustAudioEffect effect-code))))
