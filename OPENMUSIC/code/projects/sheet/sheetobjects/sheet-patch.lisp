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
;===========================================================================

; SHEET package by C. Agon & J. Bresson

(in-package :om)

(defclass sheet-patch (ompatchabs) ())

;(defmethod omng-save ((self omsheet) &optional values)
;  `(let ((thesheet ,(call-next-method)))
;     (setf (patch-list thesheet)
;           (list .,(mapcar 'om-save (patch-list self))))
;     thesheet))

(defmethod omng-save ((self sheet-patch) &optional values?)
  (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
        (connectiones (mk-connection-list (boxes self)))
        (doc (str-without-nl (doc self)))
        (pictlist (omng-save (pictu-list self))))
    `(om-load-sheet-patch ,(name self) ',boxes ',connectiones ,*om-version* ,pictlist ,doc)))

(defun om-load-sheet-patch (name boxes connections 
                                &optional (version nil) (pictlist nil) (doc "")
                                &rest args)
  (let ((newpatch (make-instance 'sheet-patch :name name :icon 210))) 
    (setf (boxes newpatch) nil)
    (mapc #'(lambda (box) (omNG-add-element newpatch (eval box))) boxes)
    (setf (boxes newpatch) (reverse (boxes newpatch)))
    (setf (saved? newpatch) connections)
    (remk-connections (boxes newpatch) (loop for i in connections collect (load-connection i)))
    (setf (pictu-list newpatch) pictlist)
    (setf (doc newpatch) (str-with-nl doc))
    (compile-patch newpatch)
    (when version (setf (omversion newpatch) version))
    newpatch))


(defun new-sheet-patch (&optional name)
  (let ((thepatch (make-instance 'sheet-patch :name (or name "sheet patch"))))
    (add-sheet-access thepatch)
    thepatch))
    

(defmethod add-patch-in-sheet ((self omsheet) &optional at)
  (let ((pos (or at (length (patch-list self)))))
    (setf (patch-list self)
          (append (first-n (patch-list self) pos)
                  (list (new-sheet-patch))
                  (nthcdr pos (patch-list self))))))

(defmethod remove-patch-from-sheet ((self omsheet) &optional at)
  (let ((pos (or at (1- (length (patch-list self))))))
    (when (>= pos 0)
      (setf (patch-list self)
            (remove (nth pos (patch-list self)) (patch-list self)))
      (unless (patch-list self) (add-patch-in-sheet self)))))


;(let ((lll '(a b c d e f g h i))
;      (n 1)
;      (move 10))
;  (list (first-n lll n)
;        (first-n (nthcdr (1+ n) lll) move)
;        (list (nth n lll))
;        (nthcdr (+ 1 n move) lll)))

;(let ((lll '(a b c d e f g h i))
;      (n 5)
;      (move -3))
;  (list (first-n lll (+ n move))
;        (list (nth n lll))
;        (first-n (nthcdr (+ n move) lll) (- move))
;        (nthcdr (+ n 1) lll)))

(defmethod switch-sheet-patch  ((self omsheet) n move)
  (cond ((plusp move)
         (unless (>= (+ n move) (length (patch-list self)))
           (setf (patch-list self)
                 (append (first-n (patch-list self) n)
                         (first-n (nthcdr (1+ n) (patch-list self)) move)
                         (list (nth n (patch-list self)))
                         (nthcdr (+ 1 n move) (patch-list self))))))
        ((minusp move)
         (unless (< (+ n move) 0)
           (setf (patch-list self)
                 (append (first-n (patch-list self) (+ n move))
                          (list (nth n (patch-list self)))
                          (first-n (nthcdr (+ n move) (patch-list self)) (- move))
                          (nthcdr (1+ n) (patch-list self))))
           ))
         (t nil)))

(defclass! sheet-access () 
  ((reference :accessor reference :initform nil)
   (object-ID :accessor object-ID :initarg :object-id :initform nil :documentation "ID of the object to read/modify")
   (object-access :accessor object-access :initarg :object-access :initform nil :documentation "sheet-track-obj accessed by ID")
   (object-content :accessor object-content :initarg :object-content :initform nil :documentation "contents of the object accessed by ID")
   (object-onset :accessor object-onset :initarg :object-onset :initform nil :documentation "offset of the object accessed by ID (ms)"))
  (:icon 211)
  (:documentation "SHEET-ACCESS is a special object used INSIDE THE SHEET PATCHES ONLY. It allows to access and/or modify one particular object in the Sheet, identified by its ID."))

(defmethod default-obj-box-size ((self sheet-access)) (om-make-point 200 60))
(defmethod get-fonde-pict ((self sheet-access)) nil)

(defmethod Class-has-editor-p ((self sheet-access)) nil)

(defclass sheet-access-box (OMBoxEditCall) ())
(defmethod get-type-of-ed-box ((self sheet-access))  'sheet-access-box)

(defclass sheet-access-boxframe (BoxEditorFrame) ())
(defmethod get-frame-class ((self sheet-access-box)) 'sheet-access-boxframe)


(defmethod add-sheet-access ((self ompatch))
  (let ((access-box (omng-make-new-boxcall (find-class 'sheet-access) 
                                           (om-make-point 150 50) "sheet access")))
    (setf (showpict access-box) t)
    (omNG-add-element self access-box)))

(defmethod add-subview-extra ((self sheet-access-boxframe))
  (call-next-method)
  (let ((sheeted (sheet-editor (om-view-container self))))
    (when sheeted
    (setf (reference (value (object self))) (object sheeted))
    (push (value (object self))
          (attached-objs (object sheeted))))))

(defmethod close-frame ((self sheet-access-boxframe))
  (when (reference (value (object self)))
  (setf (attached-objs (reference (value (object self))))
        (remove (value (object self)) (attached-objs (reference (value (object self))))))))


(defmethod openeditorframe ((self sheet-access-box)) (om-beep))

(defmethod allowed-lock-modes ((self sheet-access-box)) '("x" "&" "o"))

(defmethod rep-editor ((self sheet-access) num)
  (if (= num 0)
      (reference self)
    (call-next-method)))


(defmethod omNG-box-value ((self sheet-access-box) &optional (numout 0))
  (let ((sheet-ed (sheet-editor (om-view-container (car (frames self))))))
    (om-with-delayed-redraw (car (frames self))
    (if sheet-ed
        (let ((access (value self))
              (sheetin (omNG-box-value (first (inputs self))))
              (boxid (omNG-box-value (second (inputs self))))
              (boxin (omNG-box-value (third (inputs self))))
              (objin (omNG-box-value (fourth (inputs self))))
              (tin (omNG-box-value (fifth (inputs self)))))
          (setf (reference (value self)) (object sheet-ed))
          (cond
           ((or (and (equal (allow-lock self) "x") access) 
                (and (equal (allow-lock self) "&") (ev-once-p self)))
            (rep-editor access numout))
           (t (setf (object-id access) boxid)
              (when (and boxid (integerp boxid))
                (let ((box (find-box-in-sheet (reference access) boxid)))
                  (setf (object-access (value self)) box)
                  (when box
                    (when boxin
                      (setf (start-t box) (start-t boxin)
                            (end-t box) (end-t boxin)
                            (inside box) (clone (inside boxin))))
                    (when objin
                      (change-object box (if (allowed-in-track objin) (clone objin) nil))
                      (unless tin (sheet-init box)))
                    (when tin
                      (setf (start-t box) tin)
                      (sheet-init box))
                    (setf (object-content (value self)) (obj box))
                    (setf (object-onset (value self)) (start-t box))
                    (om-invalidate-view (car (frames self)))
                    (init-tracks (panel (score-view sheet-ed)))
                    (update-panel (panel (score-view sheet-ed)))
                    (report-modifications sheet-ed)
                    )))
              (if (equal (allow-lock self) "o") (value self)
                (rep-editor (value self) numout)))))
      (om-beep-msg "SHEET-ACCESS can be evaluated inside OMSheet patches only!")))))


(defmethod draw-mini-view ((self t) (value sheet-access))
  (let ((thesheet (reference value)))
    (when (and thesheet (inside thesheet))
      (let*  ((trp 0)
              (total-h (+ 1 (* 5 (length (inside thesheet)))))
              (h-fact (/ (h self) total-h))
              (w-fact (/ (- (w self) 6) (max 1 (get-obj-dur thesheet)))))
        (om-with-focused-view self
          (om-with-font (om-make-font *om-def-font-face* 8)
         (loop for tr in (inside thesheet)
              for n = 0 then (+ n 1)
               do
               (om-with-fg-color self *om-light-gray-color*
                 (om-fill-rect 0 (+ h-fact (* 5 n h-fact)) 
                               (w self) 
                               (* 4 h-fact)
                               ))
               (loop for obj in (objs tr) do
                       (let ((x (+ 3 (round (* (start-t obj) w-fact))))
                             (y (+ (+ h-fact (* 5 n h-fact)) 1))
                             (w (* (- (end-t obj) (start-t obj)) w-fact))
                             (h (- (* 4 h-fact) 2)))
                         (om-with-fg-color self (if (and (object-ID value) (= (id obj) (object-ID value)))
                                                    *om-red2-color* (om-make-color 0.75 0.7 0.7))
                           (om-fill-rect x y w h))
                         (om-with-fg-color self  *om-dark-gray-color*
                           (om-draw-string (+ 6 (round (* (start-t obj) w-fact)))
                                           (+ (+ h-fact (* 5 n h-fact)) 8)
                                           (string+ (number-to-string (id obj))
                                                    " - "
                                                    (string (type-of (obj obj)))
                                                    )
                                           ))
                         )))
               ))))))



