;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;DocFile
;Author: Karim Haddad
;Methods of om-finder generic function specialized for finding different objects in workspace, folders, patches, etc.
;Last Modifications :
;05/08/21 first date.
;DocFile

(in-package :om)


;peut-etre necessaire
(defmethod om-finder ((self t) (strg t)) nil)

(defmethod om-finder ((self workspaceeditor) (strg t))
  (let* ((elms (elements (object self)))
         (subframes (get-subframes (panel self)))
         (names (loop for i in elms
                      collect (string-downcase (name i))))
         (pos (search-for-obj strg names)))
    (if pos
        (progn
          (loop for i in subframes
                do (omg-unselect i))
          (loop for p in pos
                do (omg-select (nth p subframes))))
      (loop for i in subframes
            do (omg-unselect i))
      )))  


(defmethod om-finder ((self foldereditor) (strg t))
  (let* ((elms (elements (object self)))
         (subframes (get-subframes (panel self)))
         (names (loop for i in elms
                      collect (string-downcase(name i))))
         (pos (search-for-obj strg names)))
    (if pos
        (progn
          (loop for i in subframes
                do (omg-unselect i))
          (loop for p in pos
                do (omg-select (nth p subframes))))
      (loop for i in subframes
            do (omg-unselect i))
      )
    ))

(defmethod om-finder ((self patcheditor) (strg t))
  (let* ((boxes (boxes (object self)))
         (names (loop for i in boxes
                      collect (string-downcase (name i))))
         (pos (search-for-obj strg names)))
    (if pos
        (progn
          (loop for i in boxes
                do (omg-unselect (car (frames i))))
          (loop for p in pos
                do (omg-select (car (frames (nth p boxes))))))
      (loop for i in boxes
            do (omg-unselect (car (frames i))))
      )
    ))


(defmethod om-finder ((self maquetteeditor) (strg t))
  "should lowercase everything"
  (let* ((boxes (boxes (object self)))
         (names (loop for i in boxes
                      collect (string-downcase (name i))))
         (pos (search-for-obj strg names)))
    (if pos
        (progn
          (loop for i in boxes
                do (omg-unselect (car (frames i))))
          (loop for p in pos
                do (omg-select (car (frames (nth p boxes))))))
      (loop for i in boxes
            do (omg-unselect (car (frames i))))
      )
    ))


(defun search-for-obj (item names)
  (let* ((run1 (loop for i in names
                     collect (search item i)))
         (run2 (loop for i in run1
                     collect (if (equal 0 i) i))))
    (remove nil (loop for i from 0 to (length run2)
                      for rn in run2
                      collect (if rn i)))))

    
