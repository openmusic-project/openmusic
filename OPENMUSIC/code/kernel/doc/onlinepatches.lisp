;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
; Tutorial patches
;DocFile

(in-package :om)

(defun get-online-dir (self)
  (if (and (symbolp (reference self)) (fboundp (reference self)) 
           (omgenfun-p (fdefinition (reference self)))
           (lib-fun-p (fdefinition (reference self))))
    (let ((lib (lib-fun-p (fdefinition (reference self)))) path)
      (setf lib (find-library lib))
      (setf path (make-pathname :directory (append (pathname-directory (lib-pathname lib)) (list "resources" "online"))))
      (unless (probe-file path) 
        (setf path (make-pathname :directory (append (pathname-directory (lib-pathname lib)) (list "online")))))
      (namestring path))
    (make-pathname :directory (append (pathname-directory *om-root*) (list "resources" "online")))))
    
;a verifier en LInux
(defun open-tutorial (path)
  (if (and path (probe-file path))
      (let ((newpatch (omNG-make-new-patch (pathname-name path))))
        (setf (mypathname newpatch) path)
        (setf (loaded? newpatch) nil)
        (setf (editorframe newpatch) (OpenEditorframe newpatch))
        (setf (mypathname newpatch) nil)
        (when (EditorFrame newpatch)
          (om-select-window (window (Editorframe newpatch)))))
    (om-beep-msg "There is no online help for this box")))

(defmethod show-online-tutorial ((self ombox))
  (let* ((tutname (get-box-tut-name self))
        (path (if (probe-file (get-online-dir self))
                  (make-pathname
                   :directory (pathname-directory (truename (get-online-dir self)))
                   :name tutname 
                   :type "omp")
                nil)))
    (open-tutorial path)))

(defmethod get-box-tut-name ((self omboxcall))
  (or (special-name-for-tutorial (reference self))
      (format nil "~D" (reference self))))

(defmethod get-box-tut-name ((self omboxeditcall))
   (format nil "~D" (class-name (reference self))))

(defmethod get-box-tut-name ((self OMSlotsBox)) "slotstut")

(defmethod get-box-tut-name ((self OMSlotsBox)) "lisptut")

(defmethod get-box-tut-name ((self OMBoxTypeCall)) "constanttut")

(defmethod get-box-tut-name ((self OMBoxundefined)) "undeftut")

(defmethod get-box-tut-name ((self OMBoxInstance)) "instancetut")

(defmethod get-box-tut-name ((self OMBoxcomment)) "commenttut")

(defmethod get-box-tut-name ((self OMBoxlispCall)) "lisptut")

;KARIM TUTORIALS

(defmethod special-name-for-tutorial ((self t)) nil)

(defmethod special-name-for-tutorial ((self (eql 'om+))) "arithmetic")

(defmethod special-name-for-tutorial ((self (eql 'om*))) "arithmetic")

(defmethod special-name-for-tutorial ((self (eql 'om/))) "arithmetic")

(defmethod special-name-for-tutorial ((self (eql 'om-))) "arithmetic")

(defmethod special-name-for-tutorial ((self (eql 'om//))) "om-eucl")


(defmethod special-name-for-tutorial ((self (eql 'om^))) "om-pow-e-log")
(defmethod special-name-for-tutorial ((self (eql 'om-e))) "om-pow-e-log")
(defmethod special-name-for-tutorial ((self (eql 'om-log))) "om-pow-e-log")


(defmethod special-name-for-tutorial ((self (eql 'om-min))) "om-min-max")
(defmethod special-name-for-tutorial ((self (eql 'om-max))) "om-min-max")

(defmethod special-name-for-tutorial ((self (eql 'list-min))) "list-min-max")
(defmethod special-name-for-tutorial ((self (eql 'list-max))) "list-min-max")




;;;;combinatorial


(defmethod special-name-for-tutorial ((self (eql 'sort.))) "sort")


;;;sets

(defmethod special-name-for-tutorial ((self (eql 'included?))) "included")


;;;series

(defmethod special-name-for-tutorial ((self (eql 'dx->x))) "dx-x-and-x-dx")
(defmethod special-name-for-tutorial ((self (eql 'x->dx))) "dx-x-and-x-dx")


;;;lists

(defmethod special-name-for-tutorial ((self (eql 'last-n))) "last-first-n")

(defmethod special-name-for-tutorial ((self (eql 'first-n))) "last-first-n")


;;lisp


(defmethod special-name-for-tutorial ((self (eql 'first))) "nths")
(defmethod special-name-for-tutorial ((self (eql 'second))) "nths")
(defmethod special-name-for-tutorial ((self (eql 'third))) "nths")
(defmethod special-name-for-tutorial ((self (eql 'nth))) "nths")





;;predicates

(defmethod special-name-for-tutorial ((self (eql 'om<))) "predicates-1")
(defmethod special-name-for-tutorial ((self (eql 'om>))) "predicates-1")
(defmethod special-name-for-tutorial ((self (eql 'om<=))) "predicates-1")
(defmethod special-name-for-tutorial ((self (eql 'om>=))) "predicates-1")
(defmethod special-name-for-tutorial ((self (eql 'om=))) "predicates-1")
(defmethod special-name-for-tutorial ((self (eql 'om/=))) "predicates-1")



(defmethod special-name-for-tutorial ((self (eql 'omor))) "predicates-2")
(defmethod special-name-for-tutorial ((self (eql 'omand))) "predicates-2")

(defmethod special-name-for-tutorial ((self (eql 'prime?))) "prime")




;;sdiff stuff


(defmethod special-name-for-tutorial ((self (eql 'sdifinfo))) "sdif-basic")
(defmethod special-name-for-tutorial ((self (eql 'numframes))) "sdif-basic")
(defmethod special-name-for-tutorial ((self (eql 'frameinfo))) "sdif-basic")
(defmethod special-name-for-tutorial ((self (eql 'nummatrix))) "sdif-basic")
(defmethod special-name-for-tutorial ((self (eql 'matrixinfo))) "sdif-basic")



(defmethod special-name-for-tutorial ((self (eql 'getrow))) "sdif-data")
(defmethod special-name-for-tutorial ((self (eql 'getcol))) "sdif-data")
(defmethod special-name-for-tutorial ((self (eql 'getval))) "sdif-data")




(defmethod special-name-for-tutorial ((self (eql 'sdiftype))) "sdif-classes")
(defmethod special-name-for-tutorial ((self (eql 'sdifmatrix))) "sdif-classes")
(defmethod special-name-for-tutorial ((self (eql 'sdifframe))) "sdif-classes")
(defmethod special-name-for-tutorial ((self (eql 'sdifstream))) "sdif-classes")
(defmethod special-name-for-tutorial ((self (eql 'sdif-buffer))) "sdif-classes")



;;audio

(defmethod special-name-for-tutorial ((self (eql 'audio-mix-console))) "sound")

(defmethod special-name-for-tutorial ((self (eql 'sound-dur-ms))) "sound-info")
(defmethod special-name-for-tutorial ((self (eql 'om-sound-file-name))) "sound-info")
(defmethod special-name-for-tutorial ((self (eql 'om-sound-n-samples))) "sound-info")
(defmethod special-name-for-tutorial ((self (eql 'om-sound-samples-rate))) "sound-info")
(defmethod special-name-for-tutorial ((self (eql 'om-sound-n-channels))) "sound-info")
(defmethod special-name-for-tutorial ((self (eql 'om-sound-format))) "sound-info")


;;midi

(defmethod special-name-for-tutorial ((self (eql 'pgmout))) "midi-events")
(defmethod special-name-for-tutorial ((self (eql 'volume))) "midi-events")
(defmethod special-name-for-tutorial ((self (eql 'pitchbend))) "midi-events")
(defmethod special-name-for-tutorial ((self (eql 'ctrlchg))) "midi-events")
(defmethod special-name-for-tutorial ((self (eql 'gm-program))) "midi-events")

(defmethod special-name-for-tutorial ((self (eql 'test-channel))) "midi-filters")
(defmethod special-name-for-tutorial ((self (eql 'get-midievents))) "midi-filters")

(defmethod special-name-for-tutorial ((self (eql 'midicontrol))) "cont-controllers")
(defmethod special-name-for-tutorial ((self (eql 'resample))) "cont-controllers")

(defmethod special-name-for-tutorial ((self (eql 'get-midi-notes))) "midi-analysis")
(defmethod special-name-for-tutorial ((self (eql 'get-tempomap))) "midi-analysis")
(defmethod special-name-for-tutorial ((self (eql 'get-mf-lyrics))) "midi-analysis")
(defmethod special-name-for-tutorial ((self (eql 'get-midievents))) "midi-analysis")
(defmethod special-name-for-tutorial ((self (eql 'me-textinfo))) "midi-analysis")
(defmethod special-name-for-tutorial ((self (eql 'get-continuous-ctrl))) "midi-analysis")
(defmethod special-name-for-tutorial ((self (eql 'separate-channels))) "midi-analysis")





#|
OMBoxPatch
OMBoxMaquette
OMBoxabsPatch
OMBoxabsMaquette
box-dead
omin
omout
omtempout
omtypedin
temporalbox
selftempin
general-box-dead
acumboxes
|#
