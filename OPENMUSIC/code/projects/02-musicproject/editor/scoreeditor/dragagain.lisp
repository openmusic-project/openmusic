(in-package :om)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target score-element)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
                           (target scoreeditor) position)
   (declare (ignore position))
   (let ((panel (panel target)))
     (unless (in-patch-mode? panel)
       (apply-patch panel (object dragged)))))

(defun apply-patch (panel patch)
  (handler-bind ((error #'(lambda (c)
                            (when *msg-error-label-on*
                              (om-beep-msg (string+ "Error while apply the dragged patch to the score " " " 
                                                (om-report-condition c)))
                              nil))))
    (when (selection? panel)
      (unless (compiled? patch)
        (if (and (lisp-exp-p patch) (editorframe patch))
          (compile-without-close (editorframe patch))
          (compile-patch patch)))
      (if (not (= 1 (length (get-patch-inputs patch))))
        (om-beep-msg "The patch has changed It has not exactly one input")
        (loop for item in (selection? panel) do
              (apply  (intern (string (code patch)) :om) (list item)))))
    (update-panel panel t) t))


(defmethod import-dragged-object ((self scorepanel) filename pos)
  (when (in-patch-mode? self) 
    (call-next-method)))


;==============================================
;Drag-and-drop
;==============================================

(defvar *score-drag-click* (om-make-point 0 0))
(defvar *score-drag-object* nil)

;(defmethod om-drag-selection-p2 ((self scorePanel) where) 
;  (when (not (score-get-extra-mode))
;   (setf *score-drag-click* where)
;   t))

(defmethod om-drag-score-selection-p ((self scorePanel) where) 
  (when (and 
         (selection? self)
         (not (score-get-extra-mode))
         (= 0 (score-mode self))
         (not (cursor-p self)))
    (setf *score-drag-click* where)
    (setf *score-drag-object* 
          (click-in-obj (graphic-obj self) (grap-class-from-type (obj-mode self)) where self))
    ))

(defmethod big-icon-p ((self scoreEditor)) t)
(defmethod get-drag-object ((self scorePanel)) 
   (if (= 0 (score-mode self)) (om-view-container self) (call-next-method)))

(defmethod get-pos-in-object ((self scorePanel) where)  (if (= 0 (score-mode self)) where (call-next-method) ))
(defmethod allow-move-element ((self scorePanel)) (if (= 0 (score-mode self)) t (call-next-method)))

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view scorePanel))
   (declare (special *OM-drag&drop-handler*))
   (unless (or (= 0 (score-mode view)) (equal (target-view *OM-drag&drop-handler*) view))
     (call-next-method)))

(defmethod om-drag-reference-view ((self scorepanel)) self)
(defmethod om-drag-container-view ((self scorepanel)) self)

(defmethod om-draw-contents-for-drag ((self scorepanel))
  ;;; actually draws what's in (dragged-list-objs *OM-drag&drop-handler*) 
  (if (= 0 (score-mode self))
      (let ((mode-obj (grap-class-from-type (obj-mode self))))
        (om-with-fg-color nil (om-make-color-alpha 0.5 0.5 0.5 0.5)
          (mapcar #'(lambda (item) 
                      (let ((rect (rectangle item)))
                        (om-fill-rect (first rect) (second rect) (- (third rect) (first rect))
                                      (- (fourth rect) (second rect)))
                        ))
                  (get-graph-selection? self mode-obj))
          ))
    (call-next-method)))


(defmethod om-drag-start ((view scorePanel))
  (let ((theview (get-drag-object view)))
    (when (om-drag-score-selection-p view (om-mouse-position view))
      (om-invalidate-view view t)
      (let* ((region (om-new-region)))
        (setf region (make-drag-region view region 0 0 view))
        (setf (dragged-view *OM-drag&drop-handler*) theview
              (dragged-list-objs *OM-drag&drop-handler*)  (selection? view)
              (container-view *OM-drag&drop-handler*) theview 
              (initial-mouse-pos *OM-drag&drop-handler*) (get-pos-in-object view *score-drag-click*)
              (drag-flavor *OM-drag&drop-handler*) :omsc)
        )
      t)))


(defmethod om-drag-receive ((view scorePanel)
                            (dragged-view t)  position &optional (effect nil))
  (if (equal (drag-flavor *OM-drag&drop-handler*) :omsc)
      (cond ((= 0 (score-mode view)) 
             (let* ((drop-pos (om-mouse-position view)))
               (setf (opt-key-p *OM-drag&drop-handler*) (om-option-key-p)
                     (target-view  *OM-drag&drop-handler*) (get-drag-object view)
                     (drop-mouse-pos *OM-drag&drop-handler*) drop-pos)
               (score-drag&drop *OM-drag&drop-handler*)))
            (t (call-next-method)))
    (call-next-method)))

(defmethod score-drag&drop ((D&DHandler omdrag-drop))
  (cond 
    ;;; pb : sur mac ca rentre jamais et si ca rentre ca plante : (object <note>) ;;;
    ;;; ((opt-key-p *OM-drag&drop-handler*) (score-duplicate-drop D&DHandler))    
    ((eq (target-view  D&DHandler) (dragged-view  D&DHandler))
     (score-move-inside D&DHandler))
    (t (score-change-view D&DHandler))))

(defmethod test-receptor ((self scorepanel) where)
   (click-in-obj (graphic-obj self) 'grap-note where self))
     
(defmethod score-move-inside ((D&DHandler omdrag-drop))
   (let* ((editor (target-view  D&DHandler))
          (panel (panel editor))
          (pos0 (drop-mouse-pos D&DHandler))
          (selection (selection? panel))
          (receptor (test-receptor panel pos0))
          (receptor (if receptor (reference receptor)))
          (some-item-used nil))
     (loop for item in selection do
         (setf some-item-used 
                 (or some-item-used
                     (do-score-reception panel D&DHandler receptor item))))
     some-item-used))

(defmethod score-change-view ((D&DHandler omdrag-drop))
  (let* ((editor-t (target-view  D&DHandler))
          (panel-t (panel editor-t))
          (editor-s (dragged-view  D&DHandler))
          (panel-s (panel editor-s))
          (pos0 (drop-mouse-pos D&DHandler))
          (selection (selection? panel-s))
          (receptor (test-receptor panel-t pos0)) 
          (some-item-used nil)
          ;; jb..
          (firstoffset (loop for item in selection minimize (offset->ms item (object editor-s))))
          )
     (loop for item in selection do
           (setf some-item-used 
                 (or some-item-used
                     (do-score-change-view panel-t panel-s (if receptor (reference receptor) pos0) item 
                                           (- (offset->ms item (object editor-s)) firstoffset) 
                                           ;;; nouveau parametre optionel : offset par rapport au drag-pos (en ms)
                                           ))))
     some-item-used))

(defmethod score-duplicate-drop ((D&DHandler omdrag-drop))
   (let ((target-frame (get-drag-object (target-view  D&DHandler)))
         (pos0 (get-relative-position (get-drag-object (dragged-view D&DHandler))))
         (some-item-used t)
         nameerr)
     (when (allow-move-element (target-view  D&DHandler))
       (mapc #'(lambda (dragged-frame)
                 (when (or (no-allow-copy-p (object dragged-frame))
                           (not (drop-allow-p D&DHandler (object dragged-frame) (object target-frame)))
                           (null (can-copy (object target-frame) (object dragged-frame))))
                   (setf nameerr (name (object dragged-frame)))
                   (setf some-item-used nil))) (dragged-list-objs D&DHandler))
       (if some-item-used
         (let* ((subframes (mapcar #'(lambda (frame)
                                       (object frame)) (dragged-list-objs D&DHandler)))
                (copies (mapcar #'(lambda (frame)
                                    (eval (omNG-copy frame))) subframes)))
           (setf some-item-used (perform-duplicate-list D&DHandler (object target-frame) target-frame subframes copies pos0)))
         (om-beep-msg (string+ "The object " nameerr " can not be copied to this window"))))
     some-item-used))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged score-element) (target OMPatch)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged score-element) (target score-element)) t)

(defmethod vertical-move? ((D&DHandler omdrag-drop))
   (let* ((p0 (initial-mouse-pos D&DHandler))
          (p1 (drop-mouse-pos D&DHandler))
          (m (m-line p0 p1)))
     (> (abs m) 1)))
     

;=========================================================
;
;=========================================================


(defmethod do-score-reception ((self scorePanel) (D&DHandler omdrag-drop) (receptor t) (dragged t)) t)

;-----------transpose or move
;;; changed receptor NULL for T : allow drop on notes but ignore receptor

(defmethod do-score-reception ((self scorePanel) (D&DHandler omdrag-drop) 
                                 (receptor t) (dragged simple-container))
  (transpose-drag self (dragged-list-objs D&DHandler) (initial-mouse-pos D&DHandler)))


(defmethod do-score-reception ((self chordseqPanel) (D&DHandler omdrag-drop) 
                                 (receptor t) (dragged chord))
   (if (vertical-move? D&DHandler)
     (transpose-drag self (dragged-list-objs D&DHandler) (initial-mouse-pos D&DHandler))
     (when (translate-chords-p self)
       (move-chords-in-x self (initial-mouse-pos D&DHandler) (drop-mouse-pos D&DHandler) 
                         (dragged-list-objs D&DHandler) t))))

(defmethod do-score-reception ((self chordseqPanel) (D&DHandler omdrag-drop) 
                                 (receptor t) (dragged chord-seq))
   (if (vertical-move? D&DHandler)
     (transpose-drag self (dragged-list-objs D&DHandler) (initial-mouse-pos D&DHandler))
     (when (translate-chords-p self)
       (move-chords-in-x self (initial-mouse-pos D&DHandler) (drop-mouse-pos D&DHandler) 
                         (inside (car (dragged-list-objs D&DHandler))) nil))))


;=========================================================
;
;=========================================================
(defmethod do-score-change-view ((target scorePanel) (source scorePanel) (receptor t) (dragged t) &optional (offset 0))
  t)

(defmethod do-score-change-view ((target notePanel) (source scorePanel) (receptor t) (dragged t) &optional (offset 0))
   (om-beep))


;;; cf add-new-object
(defmethod add-object-in-obj ((chordseq chord-seq) (new-chord chord) time)
  (let (new-chord-list)
    (setf (offset new-chord) time)
    (loop for item in (chords chordseq) do
          (setf (offset item) (offset->ms item)))
    (setf new-chord-list (cons new-chord (chords chordseq)))
    (setf (inside chordseq) (sort new-chord-list '< :key 'offset)) 
    (setf (Qvalue chordseq) 1000)
    (adjust-extent chordseq)
    (QNormalize chordseq)
    ))

(defmethod add-object-in-obj ((chordseq chord-seq) (new-chord chord-seq) time) 
  (loop for chord in (chords new-chord) do
        (add-object-in-obj chordseq chord (+ time (offset->ms chord)))))


(defmethod do-score-change-view ((target chordseqPanel) (source scorePanel) (receptor t) (dragged chord) &optional (offset 0))
  (let ((pos (pixel-toms target receptor))
        (cseq (reference (graphic-obj target)))
        (new-chord (clone dragged)))
    (setf pos (if (and pos (> pos 0)) pos 0))
    (add-object-in-obj cseq new-chord (+ pos offset))
    (update-panel target t)
    
    nil   ;;; return some-item-used??
    ))

(defmethod do-score-change-view ((target chordseqPanel) (source scorePanel) (receptor t) (dragged note) &optional (offset 0))
  (let ((pos (pixel-toms target receptor))
        (cseq (reference (graphic-obj target)))
        (new-chord (mki 'chord  :Lmidic (list (midic dragged)))))
    
    (setf pos (if (and pos (> pos 0)) pos 0))
    (add-object-in-obj cseq new-chord (+ pos offset))
    (update-panel target t)
    
    nil   ;;; return some-item-used??
    ))

(defmethod do-score-change-view ((target chordseqPanel) (source scorePanel) (receptor t) (dragged chord-seq) &optional (offset 0))
  (let ((pos (pixel-toms target receptor))
        (cseq (reference (graphic-obj target))))
    (setf pos (if (and pos (> pos 0)) pos 0))
    (add-object-in-obj cseq (clone dragged) (+ pos offset))
    (update-panel target t)
    nil   ;;; return some-item-used??
    )) 


;=========================================================
;PATCH AND MAQ RECEIVE
;=========================================================


(defmethod score-drag-receive ((view om-view-drop ) 
                               (dragged-view t))
  (let* ((drop-pos (om-mouse-position view))   ;;;(drag-mouse-drop-position view))
         (*receiving-in-drag* t))
    (setf (opt-key-p *OM-drag&drop-handler*)  (om-option-key-p)      ;;; (drag-option-key-p view)
          (target-view  *OM-drag&drop-handler*)      (get-drag-object view)
          (initial-mouse-pos *OM-drag&drop-handler*) (om-convert-coordinates (initial-mouse-pos *OM-drag&drop-handler*) 
                                                                             (dragged-view *OM-drag&drop-handler*) 
                                                                             (target-view  *OM-drag&drop-handler*)) 
          (true-target-view *OM-drag&drop-handler*)  view 
          (drop-mouse-pos *OM-drag&drop-handler*)    (get-pos-in-object view  drop-pos)) 
    (score-finalize-drag&drop *OM-drag&drop-handler*)))

(defmethod score-finalize-drag&drop ((D&DHandler omdrag-drop))
  (when (drop-allow-p D&DHandler (object (dragged-view *OM-drag&drop-handler*))
                      (object (true-target-view *OM-drag&drop-handler*)))
    (score-to-patch (true-target-view *OM-drag&drop-handler*)
                    (panel (dragged-view *OM-drag&drop-handler*))
                    (car (dragged-list-objs D&DHandler))
                    (dragged-list-objs D&DHandler)
                    (drop-mouse-pos D&DHandler))))

;---------note
(defmethod make-and-add-box ((self patchpanel) obj position)
  (let* ((newcall (omNG-make-new-boxcall (class-of obj) position (mk-unique-name self "dragged"))))
     (setf (value newcall) obj)
     (setf (showpict newcall) t)
     (omG-add-element self (make-frame-from-callobj  newcall)) t))

(defmethod make-and-add-box ((self maquettepanel) obj position)
  (let* ((maqpos (get-offset/posy-from-pixel self position))
         (newcall (omNG-make-tempobj (omNG-make-new-instance  obj "dragged") maqpos 
                                     (mk-unique-name self "dragged"))))
     (setf (offset newcall) (om-point-h  maqpos))
     (setf (posy newcall) (om-point-v  maqpos))
     (setf (showpict newcall) t)
     (omG-add-element self (make-frame-from-callobj  newcall)) t))


(defmethod score-to-patch ((self patchpanel) (source-view t) (type-obj t) (obj-list t) (position t)) nil)



;;; modif jb 10-04
;;;(defmethod* mk-obj-from-drag-list ((Self list) (Type Chord-seq))
;;;  (cond
;;;   ((list-subtypep self '(chord note))
;;;    (let ((chord-seq (make-instance (type-of type) :empty t)))
;;;      (setQValue chord-seq 1000 :recursive nil)
;;;      
;;;      (setf (inside chord-seq) 
;;;            (mapcar #'(lambda (object) (ObjfromObjs object (mki (type-of object))))
;;;              self))
;;;      
;;;      (let (min)
;;;        (loop for chord in (inside chord-seq) do
;;;                (when (or (not min) (< (offset chord) min)) (setf min (offset chord))))
;;;        (loop for chord in (inside chord-seq) do
;;;                (when (setf (offset chord) (- (offset chord) min)))))
;;;      ;;;(loop for chord in (inside chord-seq) 
;;;      ;;;      for onset from 0 by 1000
;;;      ;;;      do  (setf (slot-value chord 'offset)  onset)
;;;      ;;;        (InContext chord-seq (setf (extent chord) 1000)))
;;;      (QNormalize chord-seq)
;;;      (adjust-extent chord-seq)
;;;      chord-seq))    
;;;   (t nil)))



;;; modif jb 10-04

(defmethod collect-from-score (obj-list)
  (cond ((subtypep (type-of (car obj-list)) 'chord) 
         (if (= (length obj-list) 1) 
             (let ((c (clone (car obj-list))))
               (set-tonalite c (get-tonalite (car obj-list)))
               c)
           (mk-obj-from-list obj-list (make-instance 'chord-seq))))
        ((subtypep (type-of (car obj-list)) 'rest)
         (if (= (length obj-list) 1) 
             nil
           (objfromobjs (loop for item in obj-list append (cons-chord-list item))
                        (make-instance 'chord-seq))))
        ((subtypep (type-of (car obj-list)) 'chord-seq)
         (if (= (length obj-list) 1) (clone (car obj-list))
           (make-instance 'multi-seq :chord-seqs (clone obj-list))))
        ((subtypep (type-of (car obj-list)) 'chord-seq)
         (clone (car obj-list)))
        (t nil)))



(defmethod score-to-patch ((self patchpanel) (source-view scorepanel) (type chord) 
                             (obj-list list) (position t))
  (let* ((newobj (collect-from-score obj-list)))
    (if newobj
        (progn
          (make-and-add-box self newobj position)
          t)
      nil)))

(defmethod score-to-patch ((self patchpanel) (source-view scorepanel) (type chord-seq) 
                             (obj-list list) (position t))
   (let* ((newobj (collect-from-score obj-list)))
     (make-and-add-box self newobj position) t))

(defmethod score-to-patch ((self patchpanel) (source-view scorepanel) (type multi-seq) 
                             (obj-list list) (position t))
   (let* ((newobj (collect-from-score obj-list)))
     (make-and-add-box self newobj position) t))

(defmethod score-to-patch ((self patchpanel) (source-view scorepanel) (type rest) 
                           (obj-list list) (position t))
  (let* ((newobj (collect-from-score obj-list)))
    (when newobj
      (make-and-add-box self newobj position) t)))




