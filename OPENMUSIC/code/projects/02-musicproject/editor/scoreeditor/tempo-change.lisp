(in-package :om)

;========= tempo
; 60  = (noire 60) 
; (1/8 60) = (croche 60)

;=========date
; (#measure)
; (#measure #chord)

;========= tempo change
; (date tempo)

;========================
;GRAPHIC INTERFACE
;========================

;------ new-change-tempo-item CLASS

;There is an old class change-tempo-item but it works better
(defvar *tempi-list* '(3/2 1 3/4 1/2 3/8 1/4 3/16 1/8 3/32 1/16))

(omg-defclass new-change-tempo-item (om-view)
  ((temponum :initform 1/4 :initarg :temponum :accessor temponum)))

(defmethod om-draw-contents ((self new-change-tempo-item))
  (om-with-focused-view self
                        (om-with-font (om-get-font self)
                                      (om-draw-string 5 (- (h self) 2) (get-string-for-tempo (temponum self))))))

(defmethod om-view-click-handler ((self new-change-tempo-item) pos)
  (declare (ignore pos))
  (update-tempo-item self))

(defmethod update-tempo-item ((self new-change-tempo-item))
   (let ((pos (position (temponum self) *tempi-list*)))
     (if (= pos (- (length *tempi-list*) 1))
         (setf (temponum self)  (nth 0 *tempi-list*))
       (setf (temponum self)  (nth (+ pos 1) *tempi-list*)))
     (om-invalidate-view self t)))


;------This is the dialog to add a new tempo change
(defmethod add-new-tempo-dialog  ()
  (let ((dialog (om-make-window 'om-dialog
                                :window-title "Set a new tempo"
                                :position :centered
                                :size (om-make-point 290 140)
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (value (om-make-dialog-item 'om-editable-text (om-make-point 200 10)  (om-make-point 50 16)
                                     (num2string 60)))
        (tempo (om-make-view 'new-change-tempo-item :position (om-make-point 140 10) :size (om-make-point 22 30)
                              :font (om-make-music-font *heads-font* 24)))
        (dynatempo (om-make-dialog-item 'om-check-box (om-make-point 80 50) (om-make-point 130 16) "Dynamic tempo?"
                                     :checked-p nil
                                     :bg-color (om-make-color 0.623 0.623 0.623))) )
    (om-add-subviews dialog  (om-make-dialog-item 'om-static-text (om-make-point 8 9) (om-make-point 100 16) "New tempo"
                                          :bg-color (om-make-color 0.623 0.623 0.623))
                     tempo value dynatempo
                     (om-make-dialog-item 'om-button (om-make-point 171 100) (om-make-point 72 24) "cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 71 100) (om-make-point 72 24) "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog  (list (temponum tempo) 
                                                                                                  (read-from-string (om-dialog-item-text value))
                                                                                                  (om-checked-p dynatempo))))
                                          :default-button t))
    (om-modal-dialog dialog)))

(defmethod voice-has-tempi? ((self simple-container))
  "T if the voice has a tempo-change at this object"
  (let* ((voice (get-the-voice self))
         (list (get-voice-tempilist voice)))
    (when list
         (let* ((mes (get-the-measure self))
                (pos1 (position mes (inside voice) :test 'equal))
                (chords (cons-chord&rest-list mes))
                (pos2 (position self chords :test 'equal)))
     (find-if #'(lambda (x) (equal (list pos1 pos2) (car x))) list)))))

(defmethod voice-has-tempi? ((self voice))
  "T if the voice has tempi-changes"
  (get-voice-tempilist self))
  
(defmethod add-tempo-change-extra ((self simple-container))
  (let ((newtempo (catch-cancel (add-new-tempo-dialog))))
    (when newtempo
      (let* ((themes (get-the-measure self))
             (pos-mes (position themes (inside (parent themes)) :test 'equal))
             (pos (position self (cons-chord&rest-list themes) :test 'equal))
             (newtempi  (list (list pos-mes pos)  newtempo)))
        (when (and pos-mes pos)
          (add-tempi-changes  (get-the-voice self) newtempi))))))


(defmethod rmv-tempo-change-extra ((self simple-container))
  (let* ((voice (get-the-voice self))
         (list (get-voice-tempilist voice))
         (mes (get-the-measure self))
         (pos1 (position mes (inside voice) :test 'equal))
         (chords (cons-chord&rest-list mes))
         (pos2 (position self chords :test 'equal))
         rep)
    (setf rep (find-if #'(lambda (x) (equal (list pos1 pos2) (car x))) list))
    (when rep
      (remove-tempo-from-qtempo self)
      (set-voice-tempilist voice (remove rep (get-voice-tempilist voice) :test 'equal))
      (make-voice-tempo-change voice (get-voice-tempilist voice)))))

(defmethod remove-tempo-from-qtempo ((self simple-container))
  (setf (qtempo self) 60)
    (remove-tempo-from-qtempo (parent self)))

(defmethod remove-tempo-from-qtempo ((self null)) t)

(defmethod remove-tempo-from-qtempo ((self simple-container))
  (when (parent self)
    (let* ((parent (parent self))
           (pos (position self (inside parent) :test 'equal)))
      (setf (qtempo parent) (or (remove-if #'(lambda (x) (equal pos (car x))) (qtempo parent)) 60))
      (remove-tempo-from-qtempo parent)
    )))

;cons a list of chords and rest (and cont-chord ???)
(defmethod cons-gfig-tempo-list ((self t)) nil)
(defmethod cons-gfig-tempo-list ((self grap-chord)) (list self))
(defmethod cons-gfig-tempo-list ((self grap-rest)) (list self))
(defmethod cons-gfig-tempo-list ((self grap-grace-notes)) nil)
(defmethod cons-gfig-tempo-list ((self grap-container)) 
   (loop for item in (inside self) append (cons-gfig-tempo-list item)))

(defun compute-next-dynamic-pos (self tempilist staff size y)
  (let* ((mesures (inside self))
         (thelist (copy-list tempilist))
         (dynamic? (third (second (car thelist)))) 
         rep)
    (loop while (and dynamic? thelist) do
          (let* ((tempo (car thelist))
                 (mespos (caar tempo))
                 (chpos (second (car tempo)))
                 (cur-mes (nth mespos mesures))
                 (figlist (cons-gfig-tempo-list cur-mes)) 
                 )
            (when (and chpos figlist)
              (let ((thechord (nth chpos figlist)))
                    (if rep
                        (setf rep (min rep
                                       (+ (min (+ y (line2pixel (posy (car (staff-list staff))) nil (/ size 4)))
                                               (second (rectangle thechord))) (/ size -2))))
                      (setf rep (+ (min (+ y (line2pixel (posy (car (staff-list staff))) nil (/ size 4)))
                                        (second (rectangle thechord))) (/ size -2))))
                    (setf thelist (cdr thelist))
                    (when thelist 
                      (setf dynamic? (third (second (car thelist)))))))))
          (when thelist
            (let* ((tempo (car thelist))
                   (mespos (caar tempo))
                   (chpos (second (car tempo)))
                   (cur-mes (nth mespos mesures))
                   (figlist (cons-gfig-tempo-list cur-mes))
                   (thechord (nth chpos figlist))
                   )
              (if rep
                  (setf rep (min rep
                                 (+ (min (+ y (line2pixel (posy (car (staff-list staff))) nil (/ size 4)))
                                         (second (rectangle thechord))) (/ size -2))))
                (setf rep (+ (min (+ y (line2pixel (posy (car (staff-list staff))) nil (/ size 4)))
                                  (second (rectangle thechord))) (/ size -2))))))
          rep))

(defun draw-dynamic-line (x1 y x2)
  (om-with-dashline (om-draw-line x1 y x2 y)))

(defun draw-tempi-in-mes (self mesnum tempilist cur-mes size staff y dynamicpos last-dyn-x)
  (let ((pos (position mesnum tempilist :key 'caar)) 
        showtempo dynamic?)
    (when (and dynamicpos (not pos))
      (draw-dynamic-line (car (rectangle cur-mes)) (- dynamicpos (/ size 4)) (third (rectangle cur-mes)) ))
   (loop while pos do
      (let* ((changeitem (nth pos tempilist))
             (chordpos (second (first changeitem)))
             (figlist (cons-gfig-tempo-list cur-mes ))
             (thechord (nth chordpos figlist)) ypos)
        (setf dynamic? (third (second changeitem)))
        (when dynamicpos
          (draw-dynamic-line last-dyn-x (- dynamicpos (/ size 4))  (car (rectangle thechord)) ))
        (cond
         ((and dynamic? (not dynamicpos))
          (setf ypos (compute-next-dynamic-pos self tempilist staff size y))
          (setf dynamicpos ypos))
         ((and (not dynamic?) dynamicpos)
          (setf ypos (setf ypos dynamicpos))
          (setf dynamicpos nil))
         ((not dynamic?) (setf ypos (+ (min (+ y (line2pixel (posy (car (staff-list staff))) nil (/ size 4)))
                                            (second (rectangle thechord))) (/ size -2)))
          (setf dynamicpos nil))
         (dynamicpos (setf ypos dynamicpos)))
        (setf showtempo (second (nth pos tempilist)))
        (draw-tempo (car showtempo) (second showtempo)  (car (rectangle thechord)) 
                    ypos size nil)
        (when dynamic?
          (setf last-dyn-x (+ (third (rectangle thechord)) (round size 2))))
        (setf tempilist (remove changeitem tempilist :test 'equal))
        (setf pos (position mesnum tempilist :key 'caar))
        ))
   (when dynamic?
          (draw-dynamic-line last-dyn-x (- dynamicpos (/ size 4)) (third (rectangle cur-mes))  ))
   dynamicpos))



;===============================
;TEMPO
;===============================

(defmethod add-tempi-changes ((self voice) tempo)
 (let ((thetempi (get-voice-tempilist self)))
   (if thetempi
       (insert-tempi self thetempi tempo)
     (set-voice-tempilist self (list tempo)))
   (make-voice-tempo-change self (get-voice-tempilist self))
   self))

(defun <list (a b)
  (if (= (car a) (car b))
      (< (second a) (second b))
    (< (car a) (car b))))

(defun insert-tempi (self extra newextra)
  (let ((pos (position (car newextra) extra :key 'car :test 'equal)))
    (if pos
        (setf (nth pos extra) newextra)
      (push newextra extra))
    (set-voice-tempilist self (sort extra '<list :key 'car))))

;------used for dynamic tempi

(defvar *dynamic-tempo-list* nil)

(defun set-last-dyn-tempo (tempo chord)
  (when (and (caar *dynamic-tempo-list*) (not (listp (caar *dynamic-tempo-list*))))
    (setf (nth 0 (nth 0 *dynamic-tempo-list*))
          (list (nth 0 (nth 0 *dynamic-tempo-list*)) tempo)))
  (let ((pos (position chord (car *dynamic-tempo-list*))))
    (when pos
      (setf (nth 0 *dynamic-tempo-list*) (subseq (nth 0 *dynamic-tempo-list*) 0 (+ pos 1))))))

(defun add-to-dynamic-list (obj)
  (setf (nth 0 *dynamic-tempo-list*)
          (append (nth 0 *dynamic-tempo-list*) (list obj))))

(defun compute-dynamic-tempi (list)
  (let ((tempo (car list)))
    (when (and (listp tempo) (not (= (first tempo) (second tempo))))
      (let* ((starttempo (first tempo))
            (endtempo (second tempo))
            (chords (cdr list))
            (count 0) offsets bpf)
        (setf offsets (loop for item in chords collect
              (setf count (+ count (extent->ms item)))))
        (setf offsets (cons 0 (butlast offsets)))
        (setf bpf (simple-bpf-from-list (list 0 (car (last offsets))) (list starttempo endtempo) 'bpf 2))
        (loop for item in offsets
              for chord in chords do
               (change-qtempo-up chord nil (bpf-get-val bpf item) nil nil))))))
              
(defmethod remove-bad-tempi-change ((self voice) tempi)
  (setf (slot-value self 'tempo)
        (list (car (tempo self)) (remove tempi (second (tempo self)) :test 'equal))))

(defmethod make-voice-tempo-change ((self voice) tempi)
  (let* ((list tempi)
         (start (caar list))
         (mes (inside self))
         (curtempo (tempo-a-la-noire (car (tempo self))))
         dynamic?)
    (setf *dynamic-tempo-list* nil)
    (loop for item in mes
          for i = 0 then (+ i 1) do
          (when curtempo (change-qtempo item curtempo dynamic?))
          ; (print i) (print list)
          (loop while (and list (= i (caaar list))) do   ;;; 
              (let ((chords (cons-chord&rest-list item)))
               (setf curtempo (tempo-a-la-noire (list (first (second (first list))) (second (second (first list))))))
                 (if (nth (second (caar list)) chords)
                     (progn
                       (set-last-dyn-tempo curtempo (nth (second (caar list)) chords))
                       (setf dynamic? (third (second (first list))))
                       (when dynamic?
                         (push (list curtempo) *dynamic-tempo-list*))
                       (change-qtempo-up (nth (second (caar list)) chords) nil curtempo dynamic? nil))
                   (remove-bad-tempi-change self (car list))) 
                 (setf list (cdr list)))))
    (when *dynamic-tempo-list*
      (loop for list in (reverse *dynamic-tempo-list*) do
            (compute-dynamic-tempi list)))))


;==========================
;CHANGING TEMPO 
;==========================

(defmethod change-qtempo-up ((self rest) chord tempo dynamic? path)
  (setf (qtempo self) tempo)
  (when dynamic?  (add-to-dynamic-list self))
  (change-qtempo-up (parent self) self tempo dynamic? nil))

(defmethod change-qtempo-up ((self chord) chord tempo dynamic? path)
  (setf (qtempo self) tempo)
  (when dynamic?  (add-to-dynamic-list self))
  (change-qtempo-up (parent self) self tempo dynamic? nil)
  (loop for sub in (inside self)
          do (change-qtempo sub tempo dynamic?)))

(defmethod change-qtempo-up ((self container) chord tempo dynamic? path) 
  (let ((pos (position chord (inside self) :test 'equal)))
    (loop for i from (+ pos 1) to (- (length (inside self)) 1) do
          (change-qtempo (nth i (inside self)) tempo dynamic?))
    (change-qtempo-up (parent self) self tempo dynamic? (cons pos path))
    (set-tempo-list self (cons pos path) tempo)))

(defmethod change-qtempo-up ((self voice) chord tempo dynamic? path) 
  (let ((pos (position chord (inside self) :test 'equal)))
    (set-tempo-list self (cons pos path) tempo)))

(defun zeroplist (list)
  (null (remove 0 list)))

(defun set-tempo-list (self pos tempo)
  ;(print (list self pos tempo (qtempo self)))
  (setf (qtempo self) 
        (if (atom (qtempo self))
            (if (zeroplist pos)
                (list (list '(0) tempo))
              (list (list '(0) (qtempo self)) (list pos tempo)))
           ;(sort (remove-duplicates (append (qtempo self) (list (list pos tempo))) :test 'equal :key 'caar) '< :key 'caar)
          (append (qtempo self) (list (list pos tempo)))
          )))



(defmethod get-Qtempo-father ((self simple-container) current) (Qtempo self))

(defmethod get-Qtempo-father ((self container) current)
 (if (listp (Qtempo self))
     (let ((pos (position current (inside self) :test 'equal)) 
           (rep (second (car (qtempo self))))
           stop)
       (loop for item in (qtempo self) 
             while (not stop) do
             (if (>= (car item) pos)
                 (setf stop t)
               (setf rep (second item))))
       rep)
   (Qtempo self)))


(defmethod change-qtempo ((self simple-container) tempo dynamic?)
  (setf (qtempo self) tempo))

(defmethod change-qtempo :after ((self chord) tempo dynamic?)
  (when dynamic?  (add-to-dynamic-list self)))

(defmethod change-qtempo  :after ((self rest) tempo dynamic?)
  (when dynamic?  (add-to-dynamic-list self)))

(defmethod change-qtempo ((self container) tempo dynamic?)
  (setf (qtempo self) tempo)
  (loop for sub in (inside self)
          do (change-qtempo sub tempo dynamic?)))

;=======

; ca sert a rien
(defmethod rec-draw-rect ((self simple-graph-container))
  (om-with-fg-color nil *om-red-color*
      (om-draw-rect (car (rectangle self)) (second (rectangle self)) 
                    (- (third (rectangle self)) (car (rectangle self))) 
                    (- (fourth (rectangle self)) (second (rectangle self))))))

(defmethod rec-draw-rect ((self grap-container))
  (om-with-fg-color nil *om-red-color*
      (om-draw-rect (car (rectangle self)) (second (rectangle self)) 
                    (- (third (rectangle self)) (car (rectangle self))) 
                    (- (fourth (rectangle self)) (second (rectangle self)))))
  (loop for item in (inside self) do
       (rec-draw-rect item)))

;==========================
;TODO
;==========================

;afficher en mode page tempo dynamic
;export & import
;===========================




(defun dialogue-voices (voicelist vsc size panel h0)
  (let* ((tabla (om-make-view 'om-scroller
                              :position (om-make-point 3 (+ h0 20)) 
                              :size size 
                              :bg-color *om-window-def-color*
                              :scrollbars (if vsc :v nil) )))
    (loop for i from 0 to (- (length voicelist) 1) do
          (let ((voice (nth i voicelist)))
            (om-add-subviews tabla 
                             (om-make-dialog-item 'om-static-text (om-make-point 0 (+ 3 (* i 20))) (om-make-point 60 20)
                                                                       (format nil "Voice ~D" i)
                                                                       :bg-color *om-window-def-color*
                                                                       :font *om-default-font2b*)
                             (om-make-dialog-item 'om-check-box (om-make-point 60 (* i 20)) (om-make-point 20 20) "" 
                                                  :bg-color *om-window-def-color*)
                             (om-make-dialog-item 'numbox (om-make-point 100 (+ 3 (* i 20))) (om-make-point 30 16) "1"
                                                  :font *om-default-font2*
                                                  :value 1
                                                  :min-val 1
                                                  :max-val 16
                                                  :afterfun #'(lambda (x) 
                                                                (set-channel voice (value x))
                                                                (update-panel panel)))
                             (om-make-dialog-item 'numbox (om-make-point 170 (+ 3 (* i 20))) (om-make-point 30 16)
                                                  (format nil "~D" (tempo-a-la-noire (car (tempo voice))))
                                                  :font *om-default-font2*
                                                  :value (tempo-a-la-noire (car (tempo voice)))
                                                  :min-val 10
                                                  :max-val 1000
                                                  :afterfun #'(lambda (x) 
                                                                (setf (tempo voice) (list (value x) (second (tempo voice))))
                                                                (update-panel panel))))))
    tabla))





