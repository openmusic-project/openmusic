;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  IRCAM - Music Represetation Team
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
;
;  Harmonic Project
;  Tonal setting interfaces
;  J. Bresson
;=========================================================================

(in-package :om)


;;;=======================================
;;; Un widget pour choisir une tonalite
;;;=======================================

(defclass tonalite-dialog-item (om-view)
  ((nthton :accessor nthton :initarg :nthton :initform 7)
   (mode :accessor mode :initarg :mode :initform 0)))

(defclass scoretonaliteview (om-view)
  ((ref-item :accessor ref-item :initarg :ref-item :initform nil)))

(defmethod make-tonalite-dialog-item (&optional ton-obj (pos (om-make-point 0 0)))
  (unless ton-obj (setf ton-obj (make-instance 'tonalite)))
  (let* ((view (om-make-view 'tonalite-dialog-item 
                             :position pos
                             :size (om-make-point 180 84)
                             :bg-color *om-white-color*
                             :mode (if (equal *majeur* (mode ton-obj)) 0 1)
                             :nthton (or (find-tonalite-num ton-obj) 7)
                             ))
         (score (om-make-view 'scoretonaliteview :position (om-make-point 5 5) :size (om-make-point 80 50)
                              :font (om-make-music-font *micron-font* 18)
                              :ref-item view
                              :bg-color *om-white-color*
                              ))
         (button-down (om-make-dialog-item 'om-button (om-make-point 95 5)
                                       (om-make-point 40 24) "<"
                                       ;;;:bg-color *om-white-color*
                                       :di-action (om-dialog-item-act item
                                                                      (unless (>= (nthton view) (- (length *ton-list*) 1)) 
                                                                        (setf (nthton view) (+ (nthton view) 1))
                                                                        (om-invalidate-view view)
                                                                        (om-invalidate-view score)))
                                       ))
         (button-up (om-make-dialog-item 'om-button (om-make-point 134 5)
                                       (om-make-point 40 24) ">"
                                       ;;;:bg-color *om-white-color*
                                       :di-action (om-dialog-item-act item
                                                                      (unless (<= (nthton view) 0) 
                                                                        (setf (nthton view) (- (nthton view) 1))
                                                                        (om-invalidate-view view)
                                                                        (om-invalidate-view score)))
                                         ))
         (modelist (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 95 52)
                                       (om-make-point 80 20) ""
                                       :range (mapcar #'(lambda (key) (string-downcase (om-str key))) '(:major :minor))
                                       :value (om-str (if (= 0 (mode view)) (string-downcase (om-str :major)) (string-downcase (om-str :minor))))
                                       :di-action (om-dialog-item-act item
                                                                     (setf (mode view) (om-get-selected-item-index item))
                                                                     (om-invalidate-view view))
                                       )))

     (om-add-subviews view score button-down button-up modelist)
     
     view))
    

(defmethod om-draw-contents ((self scoretonaliteview))
  (let* ((wid (w self))
         (armure (get-armure (nthton (ref-item self))))
         (sym (car armure))
         (listpos (second armure)))
    (om-with-focused-view self 
      (om-with-font (om-get-font self)
                    (om-draw-line 0 12 wid 12)  
                    (om-draw-line 0 18 wid 18)  
                    (om-draw-line 0 24 wid 24)  
                    (om-draw-line 0 30 wid 30)  
                    (om-draw-line 0 36 wid 36)
                    (om-with-font (om-make-music-font *signs-font* 18)
                                  (om-draw-char 10 30 (key-g)))
                    (loop for a in listpos 
                          for i = 0 then (+ i 1) do
                          (om-draw-char (+ 30 (* i 7)) (- 36 (* 3 a)) sym)
                          )
                    (om-draw-view-outline self)
                    ))))

(defmethod om-draw-contents ((self tonalite-dialog-item))
  (let* ((ton (nth (mode self) (nth (nthton self) *ton-list*)))
         (str (string+ (get-note-name (first ton)) " " (cond 
                                                 ((equal (second ton) 'bemol) "b")
                                                 ((equal (second ton) 'diese) "#")
                                                 (t "")))))
    (om-with-focused-view self
    (om-with-font *om-default-font3*
                  (om-draw-string 10 72 str))
)))


;;; tonalite dialog a partir d'une tonalite
;;; renvoie une liste formatee de valeurs
(defmethod ask-tonalite-dialog (ton-obj)
  (let* ((win (om-make-window 'om-dialog :size (om-make-point 200 144) :position :centered :resizable nil :maximize nil :close nil
                              :window-title (om-str :tonality)))
         (di (make-tonalite-dialog-item ton-obj (om-make-point 5 10)))
         (canc (om-make-dialog-item 'om-button (om-make-point 10 100) (om-make-point 80 20) (om-str :cancel)
                                    :di-action (om-dialog-item-act item
                                                (om-return-from-modal-dialog win nil))))
        (ok (om-make-dialog-item 'om-button (om-make-point 105 100) (om-make-point 80 20) (om-str :ok)
                                 :default-button t
                                   :di-action 
                                   (om-dialog-item-act item
                                     (om-return-from-modal-dialog win (list (nth (mode di) (nth (nthton di) *ton-list*)) (mode di)))))))
    (om-add-subviews win canc ok di)
    (om-modal-dialog win)))

; (ask-tonalite-dialog nil)


;;;==============================
;;; set la tonalite d'un objet
;;;==============================

;;; tonalite dialog a partir d'une tonalite
;;; renvoie une instance de tonalite
(defun choose-new-tonalite (&optional tonalite)
    (let ((tonalite-values (ask-tonalite-dialog tonalite))
         note alt tonton)
      (when (consp tonalite-values)
        (setf note (car (car tonalite-values)))
        (setf alt (second (car tonalite-values)))
        (make-instance 'tonalite 
                     :tonnote note
                     :tonalt (convertnilalt alt)
                     :mode (convertnummode (second tonalite-values))))
      ))



(defmethod set-obj-tonalite ((self t)) nil)

(defmethod set-obj-tonalite ((self tonal-object))
  (let ((tonalite-values (ask-tonalite-dialog (tonalite self)))
         note alt tonton)
    (when (consp tonalite-values)
      (setf note (car (car tonalite-values)))
      (setf alt (second (car tonalite-values)))
      (setf tonton (make-instance (get-tonalite-class self) 
                     :tonnote note
                     :tonalt (convertnilalt alt)
                     :mode (convertnummode (second tonalite-values))))
    (set-tonalite self tonton)
)))


(defmethod score-set-tonalite ((self scorepanel))
  (let ((selection (selection? self)))
    (if selection
      (unless (equal 'grap-note (grap-class-from-type  (obj-mode self)))
        (let* ((tonalite (choose-new-tonalite (tonalite (car selection))))
               note alt mode)
          (when tonalite
            (setf note (tonnote tonalite))
            (setf alt (tonalt tonalite))
            (setf mod (mode tonalite))
            (loop for item in (selection? self) do
                  (set-tonalite item (make-instance (get-tonalite-class item) 
                                       :tonnote note
                                       :tonalt alt
                                       :mode mod))))))
      (progn
        (set-obj-tonalite (object (editor self)))
        (set-editor-tonality (panel self)))
      )
    (update-panel self)))


(defmethod score-remove-tonalite ((self scorepanel))
  (let ((selection (selection? self)))
    (if selection
      (loop for item in (selection? self) do
            (set-tonalite item nil))
      (set-tonalite (object (editor self)) nil))
    (update-panel self)))


(defmethod om-get-menu-context ((object grap-container))
  (tonalite-menucontext object))

(defun tonalite-menucontext (object)
    (let ((rep (list (om-new-leafmenu (om-str :tonality)
                                      #'(lambda () 
                                          (set-obj-tonalite (reference object))
                                          (update-panel (panel (editor (om-front-window)))))))))
      (when (tonalite (reference object))
        (setf rep (append rep (list (om-new-leafmenu (string+ "Suppr. " (om-str :tonality))
                                                     #'(lambda () 
                                                         (set-tonalite (reference object) nil)
                                                         (update-panel (panel (editor (om-front-window))))))))))
      rep))





;;;===============================================
;;; SET TONALITE D'UNE NOTE = MODIF ENHARMONIE


(defmethod om-get-menu-context ((object grap-note))
  (alteration-menucontext object))

(defun alteration-menucontext (object)
  (let* ((alt (find-alt-list (midic (reference object))))
        (item1 (first alt)) (item2 (second alt)) (item3 (third alt))
        (menu1 (om-new-leafmenu (string+ (get-note-name (car item1)) (case (second item1) (-2 "bb") (-1 "b") (0 ".") (1 "#") (2 "##") (t "")))
                                #'(lambda () 
                                    (set-note-tonalite (reference object) (car item1) (second item1))
                                    (update-panel (panel (editor (om-front-window)))))
                                ))
        (menu2 (om-new-leafmenu (string+ (get-note-name (car item2)) (case (second item2) (-2 "bb") (-1 "b") (0 ".") (1 "#") (2 "##") (t "")))
                           #'(lambda () 
                               (set-note-tonalite (reference object) (car item2) (second item2))
                               (update-panel (panel (editor (om-front-window)))))
                           ))
        (menu3 (when (car item3) (om-new-leafmenu (string+ (get-note-name (car item3)) (case (second item3) (-2 "bb") (-1 "b") (0 ".") (1 "#") (2 "##") (t "")))
                                                  #'(lambda () 
                                                      (set-note-tonalite (reference object) (car item3) (second item3))
                                                      (update-panel (panel (editor (om-front-window)))))
                                                  )))
        (menureset (om-new-leafmenu (string+ "init.")
                                #'(lambda () 
                                    (set-tonalite (reference object) nil)
                                    (update-panel (panel (editor (om-front-window)))))
                                )))
    (remove nil (list menu1 menu2 menu3 menureset))))


(defmethod set-obj-tonalite ((self note))
  (om-beep))

(defmethod set-note-tonalite ((self note) note alt)
  (let* ((tonnote note)
         (tonalt (case alt (-2 (list 'bemol 'bemol)) (-1 'bemol) (0 'becarre) (1 'diese) (2 (list 'diese 'diese))))
         (tonalite (make-instance (get-tonalite-class self) :tonnote tonnote :tonalt tonalt)))
    ;(set-tonalite self (arrange-note-tonalite-alteration tonalite (get-tonalite self)))
    (set-tonalite self tonalite)
    self
    ))


(defvar *alteration-strings* '("bb" "b" "." "#" "##"))

;;; returns the list of alterations / enharmonic info
;;; each item is either NIl or the "note name" (e.g. (do "#") (fa "bb") etc.)
(defmethod get-alterations ((cs chord-seq))
  (flet ((alt-name (a) 
           (let ((p (position a '((bemol bemol) bemol becarre diese (diese diese)) :test 'equal)))
             (and p (nth p *alteration-strings*)))))
    (loop for chord in (get-chords cs) collect
          (loop for note in (inside chord) collect 
                (if (tonalite note) (list (tonnote (tonalite note)) 
                                          (alt-name (tonalt (tonalite note)))
                                          ))))))

;;; alt list is a list of list (for notes)
;;; each item is either NIl or the "note name" (e.g. (do "#") (fa "bb") etc.)
;;; it is the user responsability that this name corresponds to the actual pitch in midicents !
(defmethod set-alterations ((cs chord-seq) alt-list) 
  (flet ((alt-n (a) 
           (cond ((stringp a) 
                  (- (position a *alteration-strings* :test 'string-equal) 2))
                 (t a))))
    (let ((newcs (clone cs)))
      (loop for c in (inside newcs)
            for c-alt in alt-list do
            (loop for n in (inside c) 
                  for i = 0 then (+ i 1)
                  do (let ((alt (if (and (listp c-alt) (listp (nth i c-alt)))
                                    (nth i c-alt)
                                  c-alt)))
                     (if alt
                       (set-note-tonalite n (car alt) (alt-n (cadr alt)))
                       (set-tonalite n nil)))))
    newcs)))






