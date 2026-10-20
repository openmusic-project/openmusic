(in-package :oa)

(defmethod alloy:handle ((event alloy:focus-next) (pane om-lisp-edit-view-widget)) t)

(presentations:define-realization (alld:window om-gutter-edit-view-widget)
  ((:background simple:rectangle) (alloy:margins) :pattern :input-background)
  ((:border simple:rectangle) (alloy:margins) :line-width (alloy:un 2) :pattern :border)
  ((:gutter-bg simple:rectangle)
   (alloy:extent 0 0 (alloy:un *om-lisp-gutter-width*) (alloy:ph 1))
   :pattern *line-color-numbers*)
  ((:gutter-text simple:text)
   (alloy:extent 2 -1 (alloy:un (- *om-lisp-gutter-width* 6)) (alloy:ph 1))
   "1"
   :pattern *line-numbers-color*
   :font (simple:request-font alloy:renderer (om-font-family *def-text-edit-font*))
   :size (%om-font-draw-unit alloy:renderer *def-text-edit-font*)
   :halign :end :valign :top)
  ((:label simple:text) (alloy:margins *om-lisp-gutter-width* 1 1 1) alloy:text :pattern :input-foreground)
  ((:cursor simple:cursor)
   (presentations:find-shape :label alloy:renderable) 0
   :composite-mode :source-over :pattern :input-foreground)
  ((:selection simple:selection) (presentations:find-shape :label alloy:renderable) 0 0)
  ((:placeholder simple:text)
   (alloy:margins *om-lisp-gutter-width* 1 1 1)
   (alloy:placeholder alloy:renderable)
   :pattern :placeholder-foreground))

(presentations:define-realization (alld:window om-lisp-edit-view-widget)
  ((:background simple:rectangle) (alloy:margins) :pattern :input-background)
  ((:border simple:rectangle) (alloy:margins) :line-width (alloy:un 2) :pattern :border)
  ((:gutter-bg simple:rectangle)
   (alloy:extent 0 0 (alloy:un *om-lisp-gutter-width*) (alloy:ph 1))
   :pattern *line-color-numbers*)
  ((:gutter-text simple:text)
   (alloy:extent 2 -1 (alloy:un (- *om-lisp-gutter-width* 6)) (alloy:ph 1))
   "1"
   :pattern *line-numbers-color*
   :font (simple:request-font alloy:renderer (om-font-family *def-text-edit-font*))
   :size (%om-font-draw-unit alloy:renderer *def-text-edit-font*)
   :halign :end :valign :top)
  ((:label simple:text) (alloy:margins *om-lisp-gutter-width* 1 1 1) alloy:text :pattern :input-foreground)
  ((:cursor simple:cursor)
   (presentations:find-shape :label alloy:renderable) 0
   :composite-mode :source-over :pattern :input-foreground)
  ((:selection simple:selection) (presentations:find-shape :label alloy:renderable) 0 0)
  ((:placeholder simple:text)
   (alloy:margins *om-lisp-gutter-width* 1 1 1)
   (alloy:placeholder alloy:renderable)
   :pattern :placeholder-foreground)
  ;; between the selection (0) and the glyphs (2): the match cell shows THROUGH a
  ;; selection, and the glyph on it keeps the selection's white foreground
  ((:paren-open  simple:rectangle) (alloy:px-extent 0 0 0 0) :pattern *paren-match-pattern* :hidden-p t :z-index 1)
  ((:paren-close simple:rectangle) (alloy:px-extent 0 0 0 0) :pattern *paren-match-pattern* :hidden-p t :z-index 1))

(presentations:define-update (alld:window om-gutter-edit-view-widget)
  (:border :hidden-p t)
  (:label  :bounds (alloy:margins *om-lisp-gutter-width* 1 1 1)))

(presentations:define-realization (alld:window om-paren-edit-view-widget t)
  ;; between the selection (0) and the glyphs (2): the match cell shows THROUGH a
  ;; selection, and the glyph on it keeps the selection's white foreground
  ((:paren-open  simple:rectangle) (alloy:px-extent 0 0 0 0) :pattern *paren-match-pattern* :hidden-p t :z-index 1)
  ((:paren-close simple:rectangle) (alloy:px-extent 0 0 0 0) :pattern *paren-match-pattern* :hidden-p t :z-index 1))

(defun %lisp-find-matching-paren (text pos)
  (let* ((len  (length text))
         (pos  (max 0 (min pos len)))
         (open-char #\()
         (close-char #\))
         (idx  nil)
         (dir  nil))
    (cond ((and (< pos len) (char= (char text pos) open-char))
           (setf idx pos dir :forward))
          ((and (> pos 0) (char= (char text (1- pos)) close-char))
           (setf idx (1- pos) dir :backward))
          ((and (< pos len) (char= (char text pos) close-char))
           (setf idx pos dir :backward))
          ((and (> pos 0) (char= (char text (1- pos)) open-char))
           (setf idx (1- pos) dir :forward)))
    (when idx
      (case dir
        (:forward
         (let ((depth 1) (i (1+ idx)))
           (loop while (< i len)
                 do (let ((c (char text i)))
                      (cond ((char= c #\")           (setf i (%lisp-skip-string text i len)))
                            ((char= c #\;)           (setf i (%lisp-skip-line-comment text i len)))
                            ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
                                                     (setf i (%lisp-skip-block-comment text i len)))
                            ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
                                                     (setf i (%lisp-skip-char-literal text i len)))
                            ((char= c open-char)     (incf depth) (incf i))
                            ((char= c close-char)
                             (decf depth)
                             (when (zerop depth) (return-from %lisp-find-matching-paren (cons idx i)))
                             (incf i))
                            (t (incf i))))
                 finally (return nil))))
        (:backward
         (let ((depth 1) (i (1- idx)))
           (loop while (>= i 0)
                 do (let ((c (char text i)))
                      (cond ((char= c close-char) (incf depth) (decf i))
                            ((char= c open-char)
                             (decf depth)
                             (when (zerop depth) (return-from %lisp-find-matching-paren (cons i idx)))
                             (decf i))
                            (t (decf i))))
                 finally (return nil))))))))


