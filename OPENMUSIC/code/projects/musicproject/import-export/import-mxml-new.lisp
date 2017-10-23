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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson
;=========================================================================

(in-package :om)


(defmethod! import-musicxml (&optional path)
  :icon 352
  :doc "
Constructs a POLY object from a MusicXML file.

- <path> allows to specify the MusicXML file to import. If not specified, a file chooser dialog will be dispaled at evaluating the box.
"
  (new-import-xml (or path (om-choose-file-dialog))))

(defparameter *import-error-signal* t)

(defun new-import-xml (file)
  (read-xml-list 
   (om-list-from-xml-file file)))

  
(defun xml-equal (a b)
  (and (symbolp a) (symbolp b)
       (string-equal (symbol-name a) (symbol-name b))))

(defun xml-tag (xml-list-elt) 
  (if (listp xml-list-elt)
      (if (listp (car xml-list-elt)) 
          (caar xml-list-elt) 
        (car xml-list-elt))
    xml-list-elt))

(defun xml-tag-equal (a b)
  (xml-equal (xml-tag a) (xml-tag b)))

(defun xml-attribute-value (xml-list-elt attribute)
  (let ((pos (position attribute (list! (car xml-list-elt)) :test 'xml-equal)))
    (when pos (nth (1+ pos) (list! (car xml-list-elt))))))

(defun get-tagged-elements (list tag &optional attribute value)
  (let ((rep nil))
    (mapcar #'(lambda (elt) 
                (when (and (xml-tag-equal elt tag) 
                           (or (not attribute)
                               (equal (xml-attribute-value elt attribute) value)))
                  (push (copy-list elt) rep)))
            list)
    (reverse rep)))

(defun get-tagged-elt (list tag)
  (find tag list :test 'xml-tag-equal))


(defun get-tag-contents (taglist)
  (if (= 1 (length (cdr taglist)))
      (read-from-string (cadr taglist))
    (cdr taglist)))
  
(defun read-xml-list (xmllist)
  (when (xml-tag-equal (car xmllist) 'score-partwise)
    (let* ((part-list (car (get-tagged-elements xmllist 'part-list)))
           (score-parts (get-tagged-elements part-list 'score-part))
           (voices (loop for part in (get-tagged-elements xmllist 'part)
                         collect (let* ((id (xml-attribute-value part 'id))
                                        (part-info (mapcar 'list!  ;;; if part-name etc. are empty xml tags the parser does not return the list but the tag name          
                                                           (cdr (find id score-parts 
                                                                      :key #'(lambda (elt) (xml-attribute-value elt 'id))
                                                                      :test 'string-equal)))))
                                   (voice-from-xml part part-info)
                                   )
                         )))
      (make-instance 'poly :voices voices))))

(defun voice-from-xml (part part-info)
  (let* ((xmlname (get-tagged-elt part-info 'part-name))
         (name (and (string-equal (xml-attribute-value xmlname 'print-object) "yes")
                    (get-tag-contents xmlname)))
         (chords nil) (trees nil) (tempos nil)
         (last-signature '(4 4))
         (last-beat-division 240))
    (loop for m in (get-tagged-elements part 'measure)
          for i = 0 then (+ i 1)
          do (let* ((mesure-data (measure-from-xml m last-beat-division)))
               (if mesure-data
                   (progn
                     (push (first mesure-data) chords)
                     (if (car (second mesure-data))
                         (setf last-signature (car (second mesure-data)))
                       (setf (car (second mesure-data)) last-signature))
                     (push (second mesure-data) trees)
                     (when (third mesure-data) 
                       (push (list (list i 0) (list 1/4 (third mesure-data) nil)) tempos))
                     (when (fourth mesure-data)
                       (setf last-beat-division (fourth mesure-data)))
                     )
                 (om-beep-msg "Warning: Empty measure ignored in MusicXLML import"))
               ))
                                  
    (make-instance 'voice :chords (remove nil (flat (reverse chords)))
                   :tree (list '? (or (reverse trees) '(((4 4) (-1)))))
                   :tempo (list (or (first-n (cadr (car tempos)) 2) '(1/4 60))
                                (cdr tempos))
                   :name name)))


;;;======================================================

(defun list-gcd (list)
  (let ((curr-gcd (if (listp (car list)) (caar list) (car list))))
    (loop for item in (cdr list) do
          (setq curr-gcd (pgcd curr-gcd (if (listp item) (car item) item))))
    (abs curr-gcd)))

(defun reduce-props (list)
  (let ((lgcd (list-gcd list)))
    (loop for item in list collect 
          (if (listp item) 
              (cons (round (/ (car item) lgcd)) (cdr item))
            (/ item lgcd)))))

; (reduce-props '(96.0 -48 96))

(defun corrected-prop (dur totaldur ndiv)
  (let ((float? (floatp dur))
        ;(newdur (round (* dur (/ (car ndiv) totaldur))))
         (newdur (round (/ (* dur (car ndiv)) (/ totaldur (cadr ndiv)))))
        )
    (if float? (float newdur) newdur)))


;; (mapcar #'(lambda (prop) (corrected-prop prop 1007 '(4 3))) '(252 252 167 168 168))

(defstruct tuplet-builder 
  (closed nil)
  (num "root")
  (dur 0)
  (t-info nil)
  (subdiv nil))

(defun print-tuplet (tuplet)
  (print (list (tuplet-builder-dur tuplet) (tuplet-builder-subdiv tuplet))))

(defun add-pulse (tuplet pulse)
  (push pulse (tuplet-builder-subdiv tuplet))
  (setf (tuplet-builder-dur tuplet) (+ (tuplet-builder-dur tuplet) (abs pulse))))

(defun add-tuplet (tuplet newtuplet)
  (push newtuplet (tuplet-builder-subdiv tuplet))
  (setf (tuplet-builder-dur tuplet) (+ (tuplet-builder-dur tuplet) (tuplet-builder-dur newtuplet))))

(defun get-tuplet-tree (tuplet)
  (let ((raw-tree (reverse (loop for item in (tuplet-builder-subdiv tuplet)
                                 collect (if (tuplet-builder-p item)
                                             (get-tuplet-tree item)
                                           item)))))
    (list (tuplet-builder-dur tuplet)
          (reduce-props (if (tuplet-builder-t-info tuplet)
                            (loop for item in raw-tree collect 
                                  (if (listp item)
                                      (cons (corrected-prop (car item) (tuplet-builder-dur tuplet) (tuplet-builder-t-info tuplet))
                                            (cdr item))
                                    (corrected-prop item (tuplet-builder-dur tuplet) (tuplet-builder-t-info tuplet))))
                          raw-tree)))))


(defstruct tree-builder 
  (tree nil)
  (tuplet-stack (list (make-tuplet-builder))))

(defun new-tuplet (tree-builder n &optional time-info)
  (push (make-tuplet-builder :num n :t-info time-info) 
        (tree-builder-tuplet-stack tree-builder)))

(defun pop-tuplet (tree-builder) 
  (let ((tuplet (pop (tree-builder-tuplet-stack tree-builder))))
    (add-tuplet (car (tree-builder-tuplet-stack tree-builder)) tuplet)))


(defun end-tuplet (tree-builder n) 
  (let ((current-tup (find n (tree-builder-tuplet-stack tree-builder) 
                           :key 'tuplet-builder-num :test 'string-equal :from-end nil)))
    (loop while current-tup do 
          (setf (tuplet-builder-closed current-tup) t)
          (loop while (tuplet-builder-closed (car (tree-builder-tuplet-stack tree-builder))) do
                (pop-tuplet tree-builder))
          (setf current-tup (find n (tree-builder-tuplet-stack tree-builder) 
                                  :test #'(lambda (n tuplet) (and (not (tuplet-builder-closed tuplet))
                                                                  (string-equal n (tuplet-builder-num tuplet)))) 
                                  :from-end nil))
          )))

(defun add-new-pulse (tree-builder prop)
  (add-pulse (car (tree-builder-tuplet-stack tree-builder)) prop))

(defun get-tree (tree-builder)
  (reverse (reduce-props
            (loop for item in (tuplet-builder-subdiv (car (tree-builder-tuplet-stack tree-builder)))
                  collect (if (tuplet-builder-p item)
                              (get-tuplet-tree item)
                            item)))))

;;; (chords tree newtempo? newdivision?)
(defun measure-from-xml (xmllist current-beat-division)
  (let* ((measure-attributes (get-tagged-elt xmllist 'attributes))
         (tempo (xml-attribute-value (get-tagged-elt xmllist 'sound) 'tempo))
         (ts (get-tagged-elt measure-attributes 'time))
         (newdiv (get-tag-contents (get-tagged-elt measure-attributes 'division)))
         (division (or newdiv current-beat-division))
         (signature (when ts (signature-from-xml ts)))
         (notes (get-tagged-elements xmllist 'note))
         (tree-builder (make-tree-builder))
         (xmlchords nil) (clist-info nil) (tuple-level 0))
    (when notes ;;; Sometimes (e.g. in NAP) empty measures at the end are stored in the XML files 
      ;;; GET INFO FROM XML
      (loop for n in notes do
            (let* ((notation (get-tagged-elt n 'notations))
                   (xml-tuple (get-tagged-elements notation 'tuplet))
                   (xml-beam (get-tagged-elements n 'beam))
                   (timeinfo (get-note-time-info n))
                   (tuple nil) (beam nil)
                   (note (decode-note n)))
              
              (when note
                (when xml-tuple
                  (loop for tu in xml-tuple do
                        (let ((level (string+ "t-" (xml-attribute-value tu 'number)))
                              (type (xml-attribute-value tu 'type)))
                          (pushr (list level type timeinfo) tuple))))
                (when xml-beam
                  (loop for b in xml-beam do
                        (let ((level (string+ "b-" (xml-attribute-value b 'number)))
                              (type (get-tag-contents b)))
                          (pushr (list level type timeinfo) beam))))
                (if (and (get-tagged-elt n 'chord) (car xmlchords))
                    (pushr note (car xmlchords))
                  (push (list (when (or tuple beam)
                                (list (list 'tuple tuple) (list 'beam beam)))
                              note)
                        xmlchords))
                
                )))
      
      (setf xmlchords (reverse xmlchords))

      (let* ((omchords nil) 
             (tuplets-stack '(nil))
             tree)
      
        ;;; GET CHORDS
        (setf omchords (remove nil (loop for xmlchord in xmlchords collect
                                         (let ((c (cdr xmlchord)))
                                           (when (and (first (car c)) ;;; not a rest
                                                      (not (fourth (car c)))) ;;; not a continuation chord
                                             (make-instance 'chord :lmidic (mapcar 'car c))
                                             )))))
      
        ;;; BUILD RHYTHM TREE
        (loop for xmlchord in xmlchords do
              (let* ((tup (cadr (first (car xmlchord))))
                     (beam (cadr (second (car xmlchord))))
                     (c (cdr xmlchord))
                     (max-dur-in-the-chord (list-max (mapcar 'second c)))
                     (ch-dur (if (first (car c)) max-dur-in-the-chord (- max-dur-in-the-chord))) ;; chord or rest..
                     (proportion (if (fourth (car c)) (float ch-dur) ch-dur))
                     (grouped nil)) ;;; continuation chord => float in the tree
                
                ;(print xmlchord)
                (when tup 
                  (loop for tu in tup 
                        ;;; while (not grouped) 
                        do
                        (when (string-equal (second tu) "start")       ;; <tuplet bracket="xxx" number="n" placement="xxx" type="start"/>
                          (setf grouped t)
                          (new-tuplet tree-builder (car tu) (third tu)))))
                            
                (when beam
                  (loop for b in beam 
                      ;while (not grouped) 
                        do
                        (when (string-equal (symbol-name (second b)) "begin")  ;; <beam number="n">begin</beam>
                          (setf grouped t)
                          (new-tuplet tree-builder (car b) (third b)))))
              
                
                ;(print (list "NEW PROP"  proportion))
                (add-new-pulse tree-builder proportion)
                ;(print (get-tree tree-builder))

                (when tup 
                  (loop for tu in tup do 
                        (when (string-equal (second tu) "stop")   ;; <tuplet number="n" type="stop"/>
                          (end-tuplet tree-builder (car tu)))))

                (when beam 
                  (loop for b in beam do
                        (when (string-equal (symbol-name (second b)) "end")  ;; <beam number="n">end</beam>
                          (end-tuplet tree-builder (car b)))))
                
                ;(print (get-tree tree-builder))
              
                ))
        
        (setf tree (get-tree tree-builder))
        ;(om-print (format nil "Imported measure: ~A" tree))
        
        (list omchords
              (list signature tree)
              (and (stringp tempo) (read-from-string tempo))
              newdiv))
      )
    ))



;;; pitch duration begtie endtie
(defun decode-note (xmlnote) 
  (let* ((pitchtag (get-tagged-elt xmlnote 'pitch))
         (unpitch-tag (get-tagged-elt xmlnote 'unpitched))
         (pitch (decode-xml-pitch (get-tag-contents (or pitchtag unpitch-tag)) unpitch-tag))
         (dur (get-tag-contents (get-tagged-elt xmlnote 'duration))))
    (if dur
        (list pitch dur
              (not (null (get-tagged-elements xmlnote 'tie 'type "start")))
              (not (null (get-tagged-elements xmlnote 'tie 'type "stop"))))
      (progn (when *import-error-signal* (om-message-dialog (format nil "Warning: One note could not be imported in OM.~%~%~A" xmlnote)))
        ;(setf *import-error-signal* nil)
        nil)
      )))


(defparameter *halftone-alter* 1.0)

(defun decode-xml-pitch (xmlpitch &optional unpitched) 
  (when xmlpitch
    (let* ((note (get-tag-contents (get-tagged-elt xmlpitch (if unpitched 'display-step 'step))))
           (notenum (and note (position note '("c" nil "d" nil "e" "f" nil "g" nil "a" nil "b") 
                              :test 'string-equal)))
           (alt (get-tag-contents (get-tagged-elt xmlpitch (if unpitched 'display-alter 'alter))))
           (octave (get-tag-contents (get-tagged-elt xmlpitch (if unpitched 'display-octave 'octave)))))
      (when (and octave notenum)
        (* 100 (+ (* (1+ octave) 12) notenum (if alt (/ alt *halftone-alter*) 0))))
      )))

    
(defun signature-from-xml (xmllist)
  (list (get-tag-contents (get-tagged-elt xmllist 'beats))
        (get-tag-contents (get-tagged-elt xmllist 'beat-type))))


(defun get-note-time-info (n)        
  (let* ((xml-timemod (get-tagged-elt n 'time-modification))
         (xml-notation (get-tagged-elt n 'notations))
         (xml-tuple (get-tagged-elt xml-notation 'tuplet))
         (xml-tuple-a (get-tagged-elt xml-tuple 'tuplet-actual))
         (xml-tuple-n (get-tagged-elt xml-tuple 'tuplet-normal)))

    (cond (xml-tuple-a (list (get-tag-contents (get-tagged-elt xml-tuple-a 'tuplet-number))
                             (get-tag-contents (get-tagged-elt xml-tuple-n 'tuplet-number))))
          (xml-timemod (list (get-tag-contents (get-tagged-elt xml-timemod 'actual-notes))
                             (get-tag-contents (get-tagged-elt xml-timemod 'normal-notes))))
          (t nil))))
                             

