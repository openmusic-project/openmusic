(in-package :om)

;;;============================================
;;;;loading pictures

(defvar *aiff-palette* nil)
(defvar *midi-palette* nil)
(defvar *music-palette* nil)
(defvar *insertmodes* nil)
 
(defun init-music-pict ()
   (setf *midi-palette* (om-load-and-store-picture "midi-palette" 'internal)) 
   (setf *aiff-palette* (om-load-and-store-picture "aiff-palette" 'internal)) 
   (setf *music-palette* (om-load-and-store-picture "musicpalette" 'internal)) 
   (setf *insertmodes* (om-load-and-store-picture "insertmodes" 'internal)))

(om-add-init-func 'init-music-pict)


(defvar *add-cursor* nil)
(defvar *hand-up* nil)
(defvar *handleft* nil)
(defvar *staff-cursor* nil)
(defvar *move-chord* nil)
(defvar *c-nota* nil)
(defvar *c-chord* nil)
(defvar *c-measure* nil)
(defvar *c-voice* nil)
(defvar *c-group* nil)

(defun create-score-cursors ()
   (setf *add-cursor* (om-make-cursor "add-cursor" (om-make-point 7 0)))
   (setf *hand-up* (om-make-cursor "hand-up" (om-make-point 8 9)))
   (setf *handleft* (om-make-cursor "handleft" (om-make-point 8 9)))
   (setf *staff-cursor* (om-make-cursor "staff-cursor" (om-make-point 8 9)))
   (setf *move-chord* (om-make-cursor "move-chord" (om-make-point 7 0)))
   (setf *c-nota* (om-make-cursor "c-nota" (om-make-point 0 0)))
   (setf *c-chord* (om-make-cursor "c-chord" (om-make-point 0 0)))
   (setf *c-measure* (om-make-cursor "c-measure" (om-make-point 0 0)))
   (setf *c-voice* (om-make-cursor "c-voice" (om-make-point 0 0)))
   (setf *c-group* (om-make-cursor "c-group" (om-make-point 0 0)))
   (setf *mark* (om-make-cursor "marker-cursor" (om-make-point 0 0)))
   )

(om-add-init-func 'create-score-cursors) 
