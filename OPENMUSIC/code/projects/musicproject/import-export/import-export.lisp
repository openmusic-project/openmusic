;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;

;Score Editor import-export interface
;Author: J. Bresson


(in-package :om)

;;; IMPORT 
(defmethod import-formats ((self t)) 
  '((om "OM instance")))

(defmethod import-formats ((self container))
  '((om "OM instance")
    (midi "MIDI")))

(defmethod import-formats ((self chord-seq))
  '((om "OM instance")
    (midi "MIDI")
    (bach "From bach")))

(defmethod import-formats ((self multi-seq))
  '((om "OM instance")
    (midi "MIDI")
    (bach "From bach")))


(defmethod import-formats ((self voice))
  '((om "OM instance")
    (xml "MusicXML")
    (finale "From NoteAbilityPro")
    (bach "From bach")))

(defmethod import-formats ((self poly))
  '((om "OM instance")
    (xml "MusicXML")
    (finale "From NoteAbilityPro")
    (bach "From bach")
    ))


;;;; EXPORT 
(defmethod export-formats ((self t)) 
  '((om "OM instance")))

(defmethod export-formats ((self container))
  '((om "OM instance")
    (midi "MIDI")))

(defmethod export-formats ((self chord-seq))
  '((om "OM instance")
    (midi "MIDI")
    (bach "To bach")))

(defmethod export-formats ((self multi-seq))
  '((om "OM instance")
    (midi "MIDI")
    (bach "To bach")))

(defmethod export-formats ((self voice))
  '((om "OM instance")
    (midi "MIDI")
    (xml "MusicXML")
    (etf "ETF")
    (finale "To NoteAbilityPro")
    (bach "To bach")))

(defmethod export-formats ((self poly))
  '((om "OM instance")
    (midi "MIDI")
    (xml "MusicXML")
    (etf "ETF")
    (finale "To NoteAbilityPro")
    (bach "To bach")))


(defmethod import-menu ((self scoreeditor))
  (when (import-formats (object self))
    (om-make-menu "Import" 
                (mapcar #'(lambda (item) 
                            (om-new-leafmenu (cadr item) #'(lambda () 
                                                             (let ((obj (score-import (car item) (object self))))
                                                               (when obj
                                                                 ;;; if we don't close the window the editor crashes 
                                                                 ;;; when the new score loaded has more voices than the previous one
                                                                 (om-close-window (window self))
                                                                 (setf (inside (object self)) (clone (inside obj)))
                                                                 (after-editor-import (object self))
                                                                 (openobjecteditor (ref self))
                                                                 (report-modifications self)
                                                                 )
                                                               )
                                                             )))
                        (import-formats (object self))))))

(defmethod import-menu ((self noteeditor))
  (when (import-formats (object self))
    (om-make-menu "Import" 
                (mapcar #'(lambda (item) 
                            (om-new-leafmenu (cadr item) #'(lambda () 
                                                             (let ((obj (score-import (car item) (object self))))
                                                               (when obj
                                                                 (setf (midic (object self)) (midic obj)
                                                                       (vel (object self)) (vel obj)
                                                                       (dur (object self)) (dur obj)
                                                                       (chan (object self)) (chan obj)
                                                                       (port (object self)) (port obj))
                                                                 (after-editor-import (object self))
                                                                 (update-panel (panel self) t))))))
                        (import-formats (object self))))))


(defun export-command (editor format)
  (score-export format 
                (object editor) 
                (edition-params (ref editor))
                (name (ref editor))))
                                                                             
(defmethod export-menu ((self scoreeditor))
  (when (export-formats (object self))
    (om-make-menu "Export" 
		  (mapcar #'(lambda (item) 
			      (om-new-leafmenu (cadr item) 
                                               #'(lambda () 
                                                   (export-command self (car item)))))
			  (export-formats (object self))))))
  

;; box menu context
(defmethod import-menu-from-obj ((self omboxeditcall) (obj container))
  (om-make-menu "Import" 
                (mapcar #'(lambda (item) 
                            (om-new-leafmenu (cadr item) #'(lambda () 
                                                             (let ((newval (score-import (car item) obj)))
                                                               (when newval
                                                                 (setf (value self) newval)
                                                                 (when (showpict self)
                                                                   (update-miniview (iconview (car (frames self))) (value self)))
                                                                 (when (editorFrame self)
                                                                   (update-editor-after-eval (editorFrame self) newval)))))))
                        (import-formats (value self)))))


(defmethod export-menu-from-obj ((self omboxeditcall) (obj container))
  (om-make-menu "Export" 
                (mapcar #'(lambda (item) 
                            (om-new-leafmenu (cadr item) #'(lambda () 
                                                             (score-export (car item) (value self) (edition-params self) (name self)))))
                        (export-formats (value self)))))


(defmethod after-editor-import ((obj t)) nil)

(defmethod after-editor-import ((obj chord-seq)) 
  (setf (Qvalue obj) 1000)
  (adjust-extent obj)
  (QNormalize obj))

(defmethod after-editor-import ((obj voice)) 
  (setf (tree obj) (check-tree-for-contchord (build-tree obj) obj)))

(defmethod score-import (format object)
  (om-beep))

(defmethod score-import ((format (eql 'om)) object)
  (let ((import-obj (import-instance object)))
    (if (equal (type-of import-obj) (type-of object))
        import-obj
      (objfromobjs import-obj object))))


(defmethod score-import ((format (eql 'xml)) object)
  (let ((name (catch-cancel (om-choose-file-dialog :types '("MusicXML file" "*.xml"))))
        (import-obj nil))
    (when name ; (and name (stringp (pathname-type name)))
      (setf import-obj (import-musicxml name))
      (if (equal (type-of import-obj) (type-of object))
          import-obj
        (objfromobjs import-obj object))
      )))

(defmethod score-import ((format (eql 'finale)) object)
  (let* ((file (catch-cancel (om-choose-file-dialog :types '("OM-NAPro exchange file" "*.om"))))
         (import-obj (import-nap file)))
    (if (equal (type-of import-obj) (type-of object))
        import-obj
      (objfromobjs import-obj object))))

(defmethod score-export ((format t) object params name)
  (om-beep))

(defmethod score-export ((format (eql 'om)) object params name)
  (save-instance object name))

(defmethod score-export ((format (eql 'midi)) object params name)
  (midi-export object :approx (or (get-param params 'approx) 2) :name name :retune-channels (get-param params 'approx)))

(defmethod score-export ((format (eql 'etf)) object params name)
  (etf-export object :approx (or (get-param params 'approx) 2) :name name))

(defmethod score-export ((format (eql 'xml)) object params name)
  (xml-export object :keys '((G 2)) :approx (or (get-param params 'approx) 2) :name name))

(defmethod score-export ((format (eql 'finale)) object params name)
  (export-nap object (or (get-param params 'approx) 2) name))

;;;===============================
;;; clipboard-mode

(defmethod score-copypaste-p ((self voiceeditor)) t)
(defmethod score-copypaste-p ((self polyeditor)) t)

(defmethod score-copy ((self scoreeditor)) 
  (let ((data (get-score-export-data self)))
    (om-set-clipboard data)))

(defmethod score-paste ((self scoreeditor)) 
  (let* ((import-obj (import-nap))
         (obj (if (equal (type-of import-obj) (type-of (object self)))
                  import-obj
                (objfromobjs import-obj (object self)))))
    (when obj
      ;;; if we don't close the window the editor crashes 
      ;;; when the new score loaded has more voices than the previous one
      (om-close-window (window self))
      (setf (inside (object self)) (clone (inside obj)))
      (after-editor-import (object self))
      (openobjecteditor (ref self))
      (report-modifications self)
      )))


(defun selection-to-voice (editor)
  (when (selection? (panel editor))
    (case (grap-class-from-type (obj-mode (panel editor))) 
      (grap-measure
       (let ((voice (make-instance 'voice)))
         (setf (inside voice) (reverse (clone (selection? (panel editor)))))
         voice))
      (grap-voice (clone (car (selection? (panel editor)))))
      (otherwise nil))))

(defmethod get-score-export-data ((self voiceeditor))
  (let* ((voice (if (selection? (panel self)) 
                    (selection-to-voice self) 
                  (object self))))
    (if voice 
        (container->coom-string 
         (make-instance 'poly :voices (list voice)) 0
         (or (get-edit-param self 'approx) 2))
      (om-beep-msg "This selection can not be copied"))))

(defmethod get-score-export-data ((self polyeditor))
  (let* ((selectionvoice (selection-to-voice self))
         (poly (if selectionvoice 
                   (make-instance 'poly :voices (list selectionvoice))
                 (object self)))
        (appx (or (get-edit-param self 'approx) 2)))
    (container->coom-string poly 0 appx)))

