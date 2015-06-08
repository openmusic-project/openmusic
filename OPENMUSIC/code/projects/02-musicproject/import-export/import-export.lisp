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
    (finale "From NAP")
    (bach "From bach")))

(defmethod import-formats ((self poly))
  '((om "OM instance")
    (xml "MusicXML")
    (finale "From NAP")
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
    (finale "To NAP")
    (bach "To bach")))

(defmethod export-formats ((self poly))
  '((om "OM instance")
    (midi "MIDI")
    (xml "MusicXML")
    (etf "ETF")
    (finale "To NAP")
    (bach "To bach")))


(defmethod import-menu ((self scoreeditor))
  (when (import-formats (object self))
    (om-make-menu "Import" 
                (mapcar #'(lambda (item) 
                            (om-new-leafmenu (cadr item) #'(lambda () 
                                                             (let ((obj (score-import (car item) (object self))))
                                                               (when obj
                                                                 (setf (inside (object self)) (clone (inside obj)))
                                                                 (after-editor-import (object self))
                                                                 (update-panel (panel self) t)
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

  
(defmethod export-menu ((self scoreeditor))
  (when (export-formats (object self))
    (om-make-menu "Export" 
		  (mapcar #'(lambda (item) 
			      (om-new-leafmenu (cadr item) #'(lambda () 
							       (score-export (car item) (object self) (edition-params self)))))
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
                                                             (score-export (car item) (value self) (edition-params self)))))
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
  (let ((import-obj (finale-import)))
    (if (equal (type-of import-obj) (type-of object))
        import-obj
      (objfromobjs import-obj object))
    ))


(defmethod score-export ((format t) object params)
  (om-beep))

(defmethod score-export ((format (eql 'om)) object params)
  (save-instance object))

(defmethod score-export ((format (eql 'midi)) object params)
  (save-as-midi object nil :approx (or (get-param params 'approx) 2)))

(defmethod score-export ((format (eql 'etf)) object params)
  (save-as-etf object (or (get-param params 'approx) 2)))

(defmethod score-export ((format (eql 'xml)) object params)
  (export-musicxml object '((G 2)) (or (get-param params 'approx) 2)))

(defmethod score-export ((format (eql 'finale)) object params)
  (finale-export object (or (get-param params 'approx) 2) 'file))

;;;===============================
;;; clipboard-mode

(defmethod score-copy-p ((self voiceeditor)) t)
(defmethod score-copy-p ((self polyeditor)) t)

(defmethod score-copy ((self scoreeditor)) 
  (let ((data (get-score-export-data self)))
    (om-set-clipboard data)))

(defun selection-to-voice (editor)
  (when (and (equal 'grap-measure (grap-class-from-type (obj-mode (panel editor))))
             (selection? (panel editor)))
    (let ((voice (make-instance 'voice)))
      (setf (inside voice) (reverse (clone (selection? (panel editor)))))
      voice)))

(defmethod get-score-export-data ((self voiceeditor))
  (let* ((voice (or (selection-to-voice self) (object self)))
         (poly (make-instance 'poly :voices (list voice)))
         (appx (or (get-edit-param self 'approx) 2)))
    (container->coom-string poly 0 appx)))

(defmethod get-score-export-data ((self polyeditor))
  (let* ((selectionvoice (selection-to-voice self))
         (poly (if selectionvoice 
                   (make-instance 'poly :voices (list selectionvoice))
                 (object self)))
        (appx (or (get-edit-param self 'approx) 2)))
    (container->coom-string poly 0 appx)))

