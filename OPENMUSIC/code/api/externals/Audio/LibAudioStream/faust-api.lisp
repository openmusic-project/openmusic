;;;==================================================================================================================================================================
;;;===================================================================FAUST AUDIO API================================================================================
;;;==================================================================================================================================================================
(in-package :oa)

(defvar *effects-lists* (make-hash-table))
(defvar *effects-lists-hidden* (make-hash-table))
(defvar *faust-effects-register* (make-hash-table))
(defvar *faust-synths-register* (make-hash-table))
(defvar *faust-effects-by-track* (make-hash-table))
(defvar *faust-synths-by-track* (make-hash-table))
(defvar *faust-synths-by-track-hidden* (make-hash-table))
(defvar *faust-synths-console* (make-hash-table))
(defconstant *max-effects-number* (* 4 las-channels))

;===============================================================================================================================================================
;============================================================================ API ==============================================================================
;===============================================================================================================================================================

(export '(
          las-faust-init-system
          las-faust-make-effect
          las-faust-get-json
          las-faust-get-control-count
          las-faust-get-control-params
          las-faust-get-control-value
          las-faust-set-control-value
          las-faust-effect-cleanup
          las-faust-synth-cleanup
          las-faust-add-effect-to-register
          las-faust-add-synth-to-register
          las-faust-add-synth-console-to-register
          las-faust-find-effect-in-register
          las-faust-find-synth-in-register
          las-faust-add-effect-to-track
          las-faust-add-synth-to-track
          las-faust-remove-effect-from-track
          las-faust-remove-synth-from-track
          las-faust-set-effect-track-in-register
          las-get-number-faust-effects-register
          las-get-number-faust-synths-register
          las-faust-get-track-effects-name
          las-faust-get-track-synths-name
          las-faust-get-track-effects-pointer
          las-faust-unplug-all
          las-faust-effect-already-plugged-?
          las-faust-null-ptr-p
          las-faust-search-effect-name-in-register
          las-faust-search-synth-name-in-register
          las-faust-search-synth-console-in-register
          las-faust-make-null-sound
          
          *faust-effects-register*
          *faust-synths-register*
          *faust-effects-by-track*
          *faust-synths-by-track*
          *faust-synths-by-track-hidden*
          *faust-synths-console*
          ) :om-api)


(defun las-faust-init-system ()
  (progn
    (init-faust-effects-register)
    (init-faust-synths-register)
    (ResetEffectsLists *audio-player-visible*)
    (Plug-Empty-Effects-Lists-On-Hidden)))

(defun las-faust-unplug-all ()
  (ResetEffectsLists *audio-player-visible*))

(defun las-faust-make-effect (string outfiles-path)
  (let ((result-state 1)
        result-pointer
        result-error) 
    (setf result-pointer (las::MakeFaustAudioEffect string "" (directory-namestring outfiles-path)))
    (if (las::las-null-ptr-p result-pointer)
        (progn
          (setf result-error (las::getlastliberror))
          (setf result-state 0)))
    (list result-state result-pointer result-error)))


(defun las-faust-null-ptr-p (pointer)
  (las::las-null-ptr-p pointer))

(defun las-faust-make-null-sound (duration)
  (las::makestereosound (las::makenullsound (* las-srate duration))))

(defun las-faust-get-json (pointer)
  (las::getjsoneffect pointer))

(defun las-faust-get-control-count (pointer)
  (las::getcontrolcount pointer))

(defun las-faust-get-control-params (pointer number)
  (las::getcontrolparam pointer number))

(defun las-faust-get-control-value (pointer number)
  (las::getcontrolvalue pointer number))

(defun las-faust-set-control-value (pointer number val)
  (las::SetControlValue pointer number val))

(defun las-faust-effect-cleanup (pointer)
  (if (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*))
      (let ((n (get-number-faust-effects-register))
            (track (- (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*)) 1)))
        (if (>= track 0)
            (las-faust-remove-effect-from-track pointer track))
        (setf (gethash (find-effect-index-in-register pointer) *faust-effects-register*) (list nil 0 "faust-effect"))
        (pack-faust-effects-register n))))

(defun las-faust-synth-cleanup (pointer)
  (if (nth 1 (gethash (find-synth-index-in-register pointer) *faust-synths-register*))
      (let ((n (get-number-faust-synths-register))
            (track (- (nth 1 (gethash (find-synth-index-in-register pointer) *faust-synths-register*)) 1)))
        (if (>= track 0)
            (las-faust-remove-synth-from-track pointer track))
        (setf (gethash (find-synth-index-in-register pointer) *faust-synths-register*) (list nil 0 "faust-synth"))
        (pack-faust-synths-register n))))

(defun las-faust-add-effect-to-register (pointer track name)
  (add-faust-effect-to-register pointer track name))

(defun las-faust-add-synth-to-register (pointer track name)
  (add-faust-synth-to-register pointer track name))

(defun las-faust-add-synth-console-to-register (console pointer nullsnd)
  (let ((i 0))
    (while (gethash i *faust-synths-console*)
      (incf i))
    (setf (gethash i *faust-synths-console*) (list console pointer nullsnd))
    ))

(defun las-faust-find-effect-in-register (pointer)
  (find-effect-index-in-register pointer))

(defun las-faust-find-synth-in-register (pointer)
  (find-synth-index-in-register pointer))

(defun las-faust-add-effect-to-track (pointer name track)
  (let ((liste (gethash track *faust-effects-by-track*))
        (i 0)) 
    (while (gethash i liste)
      (incf i))
    (setf (gethash i liste) (list pointer name))
    (setf (nth 1 (gethash (cadr (las-faust-search-effect-name-in-register name)) *faust-effects-register*)) (+ track 1))
    (add-faust-effect-to-list pointer (gethash track *effects-lists*))))
(defun las-faust-add-synth-to-track (pointer name track)
  (let ((liste (gethash track *faust-synths-by-track*))) 
    (setf (gethash 0 liste) (list pointer name))
    (setf (nth 1 (gethash (cadr (las-faust-search-synth-name-in-register name)) *faust-synths-register*)) (+ track 1))
    (if (las-faust-get-track-effects-pointer track)
        (let ((namelist (las-faust-get-track-effects-name track))
              (ptrlist (las-faust-get-track-effects-pointer track)))
          (loop for ptr in ptrlist do
                (las-faust-remove-effect-from-track ptr track))
          (add-faust-effect-to-list pointer (gethash track *effects-lists*))
          (loop for ptr in ptrlist do
                for name in namelist do
                (las-faust-add-effect-to-track ptr name track)))
      (add-faust-effect-to-list pointer (gethash track *effects-lists*)))))

(defun las-faust-remove-effect-from-track (pointer track)
  (let ((res (las-faust-effect-already-plugged-? pointer)))
    (if res 
        (setf (gethash (cadr res) (gethash (car res) *faust-effects-by-track*)) nil))
    (remove-effect-in-track-register pointer track)
    (setf (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*)) 0)
    (remove-faust-effect-from-list pointer (gethash track *effects-lists*))))
(defun las-faust-remove-synth-from-track (pointer track)
  (let ((res (las-faust-synth-already-plugged-? pointer)))
    (if res 
        (setf (gethash (cadr res) (gethash (car res) *faust-synths-by-track*)) nil))
    (remove-synth-in-track-register pointer track)
    (setf (nth 1 (gethash (find-synth-index-in-register pointer) *faust-synths-register*)) 0)
    (remove-faust-effect-from-list pointer (gethash track *effects-lists*))))

(defun las-faust-set-effect-track-in-register (pointer track)
  (setf (nth 1 (gethash (find-effect-index-in-register pointer) *faust-effects-register*)) track))

(defun las-get-number-faust-effects-register ()
  (get-number-faust-effects-register))

(defun las-get-number-faust-synths-register ()
  (get-number-faust-synths-register))

(defun las-faust-get-track-effects-name (track)
  (let ((liste (gethash track *faust-effects-by-track*))
        (i 0)
        (res (list)))
    (while (gethash i liste)
      (setf res (append res (list (nth 1 (gethash i liste)))))
      (incf i)
      )
    res))
(defun las-faust-get-track-synths-name (track)
  (let ((liste (gethash track *faust-synths-by-track*))
        (i 0)
        (res (list)))
    (while (gethash i liste)
      (setf res (append res (list (nth 1 (gethash i liste)))))
      (incf i)
      )
    res))


(defun las-faust-get-track-effects-pointer (track)
  (let ((liste (gethash track *faust-effects-by-track*))
        (i 0)
        (res (list)))
    (while (gethash i liste)
      (setf res (append res (list (nth 0 (gethash i liste)))))
      (incf i)
      )
    res))

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
(defun las-faust-synth-already-plugged-? (pointer)
  (let ((x 0)
        (y 0)
        (found 0)
        res
        curlist)
    (while (= found 0)
           (setf curlist (gethash x *faust-synths-by-track*))
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


(defun las-faust-search-effect-name-in-register (name)
  (let ((i 0)
        res)
    (while (and (not res) (car (gethash i *faust-effects-register*)))
      (if (string= name (nth 2 (gethash i *faust-effects-register*)))
          (setf res t)
        (incf i)))
    (list res i)))
(defun las-faust-search-synth-name-in-register (name)
  (let ((i 0)
        res)
    (while (and (not res) (car (gethash i *faust-synths-register*)))
      (if (string= name (nth 2 (gethash i *faust-synths-register*)))
          (setf res t)
        (incf i)))
    (list res i)))
(defun las-faust-search-synth-console-in-register (console)
  (let ((i 0)
        res)
    (while (and (not res) (car (gethash i *faust-synths-console*)))
      (if (eq console (car (gethash i *faust-synths-console*)))
          (setf res t)
        (incf i)))
    (list res (gethash i *faust-synths-console*))))
;===============================================================================================================================================================
;=========================================================================== TOOLS =============================================================================
;===============================================================================================================================================================

(defun plug-faust-effect-list-on-channel (player effectlist channel &optional (fadein 100) (fadeout 100))
  (las::SetEffectListChannel player channel effectlist fadein fadeout))

(defun ResetEffectsLists (player)
  (setf *effect-list* (make-hash-table))
  (setf *faust-effects-by-track* (make-hash-table))
  (setf *faust-synths-by-track* (make-hash-table))
  (loop for i from 0 to (- las-channels 1) do 
      (setf (gethash i *effects-lists*) (las::MakeAudioEffectList))
      (plug-faust-effect-list-on-channel player (gethash i *effects-lists*) i)
      (setf (gethash i *faust-effects-by-track*) (make-hash-table))
      (setf (gethash i *faust-synths-by-track*) (make-hash-table))))

(defun Plug-Empty-Effects-Lists-On-Hidden ()
  (setf *effects-lists-hidden* (make-hash-table))
  (loop for i from 0 to (- las-channels 1) do 
        (setf (gethash i *effects-lists-hidden*) (las::MakeAudioEffectList))
        (plug-faust-effect-list-on-channel *audio-player-hidden* (gethash i *effects-lists-hidden*) i)
        (setf (gethash i *faust-synths-by-track-hidden*) (make-hash-table))))

(defun init-faust-effects-register ()
    (loop for i from 0 to *max-effects-number* do
          (setf (gethash i *faust-effects-register*) (list nil 0 "faust-effect")))) ;(ptr track name)
(defun init-faust-synths-register ()
    (loop for i from 0 to *max-effects-number* do
          (setf (gethash i *faust-synths-register*) (list nil 0 "faust-synth"))))

;;;//////////////////REGISTER TOOLS/////////////////////////////
(defun add-faust-effect-to-register (ptr track name)
  (let ((i 0))
    (while (nth 0 (gethash i *faust-effects-register*))
          (incf i)
          (if (> i *max-effects-number*) (setf i nil)))
    (if i (setf (gethash i *faust-effects-register*) (list ptr track name)))))
(defun add-faust-synth-to-register (ptr track name)
  (let ((i 0))
    (while (nth 0 (gethash i *faust-synths-register*))
          (incf i)
          (if (> i *max-effects-number*) (setf i nil)))
    (if i (setf (gethash i *faust-synths-register*) (list ptr track name)))))

(defun remove-faust-effect-from-list (ptr list)
  (las::RemoveAudioEffect list ptr))

(defun add-faust-effect-to-list (ptr list)
  (las::AddAudioEffect list ptr))

(defun get-number-faust-effects-register ()
  (let ((i 0))
    (while (nth 0 (gethash i *faust-effects-register*))
          (incf i))
    i))
(defun get-number-faust-synths-register ()
  (let ((i 0))
    (while (nth 0 (gethash i *faust-synths-register*))
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
(defun find-hole-index-in-faust-synths-register ()
  (let ((i 0)
        (found 0)
        (ptr nil)
        (marker 0)
        (res nil))
    (while (= found 0) 
      (setf ptr (nth 0 (gethash i *faust-synths-register*)))
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
              (setf i nil))))
    i))
(defun find-synth-index-in-register (ptr)
  (let ((i 0)
        (found 0))
    (while (= found 0)
      (if (eq ptr (nth 0 (gethash i *faust-synths-register*)))
          (setf found 1)
        (incf i))
        (if (> i *max-effects-number*)
            (progn 
              (setf found 1)
              (setf i nil))))
    i))

(defun find-synth-hidden (ptr)
  (let ((i 0)
        (found 0))
    (while (= found 0)
      (if (and (gethash 0 (gethash i *faust-synths-by-track-hidden*))
               (eq ptr (gethash 0 (gethash i *faust-synths-by-track-hidden*))))
          (setf found 1)
        (incf i))
      (if (>= i las-channels)
          (progn 
            (setf found 1)
            (setf i nil))))
    i))

(defun pack-faust-effects-register (n)
  (let ()
    (if (find-hole-index-in-faust-effects-register)
        (let ((index (find-hole-index-in-faust-effects-register)))
          (loop for i from index to (- n 1) do
            (setf (gethash i *faust-effects-register*) (gethash (+ i 1) *faust-effects-register*)))))))
(defun pack-faust-synths-register (n)
  (let ()
    (if (find-hole-index-in-faust-synths-register)
        (let ((index (find-hole-index-in-faust-synths-register)))
          (loop for i from index to (- n 1) do
            (setf (gethash i *faust-synths-register*) (gethash (+ i 1) *faust-synths-register*)))))))


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
(defun remove-synth-in-track-register (pointer track)
  (let ((i 0)
        (found 0))
    (while (= found 0) 
      (if (not (eq pointer (car (gethash i (gethash track *faust-synths-by-track*))))) 
          (progn 
            (incf i)
            (if (> i *max-effects-number*)
                (progn 
                  (setf found 1)
                  (setf i nil))))
        (setf found 1)))
    (if i
        (progn
          (setf (gethash i (gethash track *faust-synths-by-track*)) nil)
          (loop for k from i to *max-effects-number* do
                (setf (gethash k (gethash track *faust-synths-by-track*)) (gethash (+ k 1) (gethash track *faust-synths-by-track*))))))))



(defun faust-system-recap ()
  (print "///////////////////////////////")
  (print "Registre EFFECTS (5 premiers) :")
  (loop for i from 0 to 4 do
        (print (gethash i *faust-effects-register*)))
  (print "///////////////////////////////")
  (print "Registre SYNTHS (5 premiers) :")
  (loop for i from 0 to 4 do
        (print (gethash i *faust-synths-register*)))
  (print "///////////////////////////////")
  (loop for i from 0 to 15 do
        (print "----------------")
        (print (format nil "Track ~A Effects:" i))
        (loop for j from 0 to 4 do
              (print (gethash j (gethash i *faust-effects-by-track*))))
        (print (format nil "Track ~A Synths:" i))
        (loop for j from 0 to 1 do
              (print (gethash j (gethash i *faust-synths-by-track*))))
        (print "----------------")))
;(faust-system-recap)
;(progn (reseteffectslists *audio-player-visible*) (loop for i from 0 to 15 do (setf (gethash i *faust-effects-register*) (list nil 0 "faust-effect"))))
 