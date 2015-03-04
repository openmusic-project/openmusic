(defpackage #:cl-svg-tests
  (:use #:cl #:cl-svg))

(in-package #:cl-svg-tests)


(defun one-of (list)
  (nth (random (length list)) list))

(defvar *groovy-test-colors*
  (list
   "rgb(252, 229, 105)"
   "rgb(228, 174, 60)"
   "rgb(212, 228, 164)"
   "rgb(196, 234, 212)"
   "rgb(124, 218, 156)"
   "rgb(244, 128, 20)"
   "rgb(212, 229, 190)"))

(defvar *sandy-test-colors*
  (list
   "rgb(129, 132, 72)"
   "rgb(201, 197, 117)"
   "rgb(140, 44, 18)"
   "rgb(187, 136, 59)"
   "rgb(50, 68, 56)"
   "rgb(185, 141, 126)"
   "rgb(135, 90, 31)"
   "rgb(235, 228, 134)"
   "rgb(200, 89, 47)"
   "rgb(246, 185, 144)"
   "rgb(147, 164, 153)"))

(export-svg (om::simple-bpf-from-list '(0 50 100 200) '(0 50 10 200)) nil)



(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 300 :width 300)))
  (draw scene (:rect :x 5 :y 5 :height 30 :width 30))
  (draw scene (:rect :x 40 :y 40 :height 30 :width 30)
        :stroke "blue" :stroke-width 1 :fill "yellow")
  (draw scene (:rect :x 75 :y 75 :height 30 :width 30)
        :fill "purple")
  (with-open-file (s #P"/Users/jgarcia/Desktop/test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 500 :width 500)))
  (dotimes (i 80)
    (draw scene (:circle :cx (random 500)
                         :cy (random 500)
                         :r (+ 10 (random 45)))
          :stroke "rgb(232, 229, 148)"
          :fill (one-of *groovy-test-colors*)))
  (with-open-file (s #P"/Users/jgarcia/Desktop/test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; eliptical grooviness
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 500 :width 500)))
  (title scene "Eliptical Grooviness")
  (dotimes (i 310)
    (draw scene (:ellipse :cx (random 500)
                          :cy (random 500)
                          :rx (+ 10 (random 65))
                          :ry (+ 10 (random 65)))
          :stroke "rgb(232, 229, 148)"
          :fill (one-of *groovy-test-colors*)))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; rectangular grooviness
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700)))
  (title scene "Rectangular grooviness!")
  (desc scene "This is a scene of unspeakable grooviness!  Just look at the
great colors.  And the random rectangles!  You want this as wallpaper.")
  (dotimes (i 1200)
    (draw scene (:rect :x (- (random 750) 25)
                       :y (- (random 750) 25)
                       :rx 4 :ry 4
                       :height (+ 5 (random 65))
                       :width (+ 5 (random 65)))
          :stroke "rgb(232, 229, 148)"
          :opacity (+ 0.3 (random 0.2))
          :fill (one-of *groovy-test-colors*)))
  (with-open-file (s #P"/Users/jgarcia/Desktop/test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


;;; Now some symbols (in the SVG sense).
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       ;;; draw a nice picture
       (columns (make-svg-symbol scene (:id :generate :view-box "0 0 20 20")
                  (draw* (:rect :x 5 :y 5 :height 10 :width 3) :opacity 1.0)
                  (draw* (:rect :x 10 :y 5 :height 10 :width 3) :opacity 0.7)
                  (draw* (:rect :x 15 :y 5 :height 10 :width 3) :opacity 0.3))))
  ;;; Next, instantiate like mad.
  (dotimes (i 400)
    (let ((size (+ 5 (random 95))))
      (draw scene (:use :xlink-href (xlink-href columns))
            :x (- (random 750) 25)
            :y (- (random 700) 25)
            :height size :width size
            :stroke "rgb(232, 229, 148)"
            :stroke-width 0.3
            :fill (one-of *sandy-test-colors*))))
  (title scene "SVG test: using symbols")
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


;;; Gradients
(let*
    ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
     (lg1 (make-linear-gradient scene (:id :generate
                                       :x1 "0%" :y1 "0%" :x2 "100%" :y2 "100%")
            (stop :color "black" :offset "0%")
            (stop :color "purple" :offset "50%")
            (stop :color "red" :offset "100%")))
     (rg1 (make-radial-gradient scene (:id :generate
                                       :cx "50%" :cy "50%" :r "50%")
            (stop :color "yellow" :offset "0%")
            (stop :color "orange" :offset "40%")
            (stop :color "red" :offset "70%" :opacity 0.3)
            (stop :color "grey" :offset "100%")))
     (rg2 (make-radial-gradient scene (:id :generate
                                       :cx "50%" :cy "50%" :r "50%"
                                       :fx "50%" :fy "25%")
            (stop :color "black" :offset "0%")
            (stop :color "lime" :offset "30%")
            (stop :color "white" :offset "70%"))))
  (title scene "SVG test: gradients")
  (draw scene (:rect :x 10 :y 10 :height 200 :width 200)
        :fill (xlink-href lg1))
  (draw scene (:rect :x 250 :y 250 :height 200 :width 200)
        :fill (xlink-href rg1))
  (draw scene (:rect :x 490 :y 490 :height 200 :width 200)
        :fill (xlink-href rg2))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Line markers -- locating these correctly is a pain in the rear.
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :view-box "0 0 4000 2000"
                                 :height "2in" :width "4in"))
       (marker (make-marker scene (:id :generate :view-box "0 0 10 10"
                                   :ref-x 0 :ref-y 5
                                   :marker-width 4 :marker-height 3
                                   :marker-units "strokeWidth" :orient "auto")
                 (draw* (:path :d "M 0 0 L 10 5 L 0 10 z")))))
  (draw scene (:rect :x 10 :y 10 :width 3980 :height 1980)
               :fill "none" :stroke "blue" :stroke-width 10)
  (draw scene (:path :d "M 1000 750 L 2000 750 L 2500 1250")
               :fill "none" :stroke "black" :stroke-width 100
               :marker-end (xlink-href marker))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Fill patterns.
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       (pattern (make-pattern scene (:id :generate
                                     :x 0 :y 0 :width 20 :height 20
                                     :view-box "0 0 10 10"
                                     :pattern-units "userSpaceOnUse")
                  (draw* (:rect :x 0 :y 0 :width 5 :height 5 :fill "lightblue"))
                  (draw* (:rect :x 5 :y 5 :width 5 :height 5 :fill "lightblue")))))
  (comment scene "Just a quick fill test.")
  (draw scene (:rect :x 0 :y 0 :height "100%" :width "100%")
               :fill (xlink-href pattern))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; CSS stylings, grouping and simple text
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700)))
  (title scene "CSS and Group test")
  (style scene "circle:hover {fill-opacity: 0.9;}")
  (make-group scene (:fill-opacity 0.6)
    (draw* (:circle :cx 150 :cy 150 :r 125) :fill "red")
    (draw* (:circle :cx 550 :cy 550 :r 125) :fill "green")
    (draw* (:circle :cx 550 :cy 150 :r 125) :fill "orange")
    (draw* (:circle :cx 150 :cy 550 :r 125) :fill "purple")
    (draw* (:circle :cx 350 :cy 350 :r 225) :fill "blue"))
  (text scene (:x 290 :y 320)
    "Mouse over a "
    (tspan (:fill "orange" :font-weight "bold") "circle"))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; include image
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
      (img-url "http://www.biostat.wisc.edu/~annis/kochab2.png"))
  (title scene "image test")
  (draw scene (:image :x 10 :y 10 :height 600 :width 600 :xlink-href img-url))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; image with alpha mask
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       (img-url "http://www.biostat.wisc.edu/~annis/kochab2.png")
       (grad1 (make-linear-gradient scene (:id :generate :x1 0 :y1 0
                 :x2 500 :y2 500 :gradient-units "userSpaceOnUse")
                (stop :color "white" :offset "0%" :opacity 0.0)
                (stop :color "white" :offset "100%" :opacity 1.0)))
       (mask1 (make-mask scene (:id "Mask1" :x 0 :y 0 :height 700 :width 700
                 :mask-units "userSpaceOnUse")
                (draw* (:rect :x "0%" :y "0%" :height "100%" :width "100%"
                        :fill (xlink-href grad1))))))
  (title scene "image with alpha mask")
  (draw scene (:image :x 10 :y 10 :height 600 :width 600
               :xlink-href img-url :mask (xlink-href mask1)))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Based on http://billmill.org/static/viewji/viewji.html which is based in
;;; turn on http://nodebox.net/code/index.php/Superfolia_|_root_source_code
(defun radians (degrees)
  ;;; SVG doesn't like exponential notation in many places, so I avoid
  ;;; the DOUBLE-FLOAT PI.
  (/ (* degrees 3.141592653589793) 180.0))

(defun random-range (start end)
  (+ start (random (- end start))))

(defun rgb (r g b)
  (flet ((normalize-to-byte (c)
           (truncate (* c 255))))
    (format nil "rgb(~{~A~^, ~})" (mapcar #'normalize-to-byte (list r g b)))))

(defun root (canvas x y &optional (angle 0) (depth 5) (alpha 1.0) (decay 0.005))
  (let ((w (* depth 6.0)))
    ;(format t "at DEPTH ~A~&" depth)
    (dotimes (i (* depth (random-range 10 20)))
      (let* ((v (/ depth 5.0))
             (color (rgb  (- 0.8 (* v 0.25))
                          0.8
                          (- 0.8 v))))
        (setf alpha (max 0.0 (- alpha (* i decay))))
        ;;; CL will start to use exponential notation when alpha gets
        ;;; very small, and SVG hates this.
        (when (> alpha 0.00001)
          (setf angle (+ angle (random-range -60 60)))
          (let ((dx (+ x (* (cos (radians angle)) w)))
                (dy (+ y (* (sin (radians angle)) w)))
                (group
                 (make-group canvas (:stroke color :fill color :opacity alpha
                                     :stroke-width (* depth 0.5)
                                     :fill-opacity (* alpha 0.6)
                                     :stroke-linecap "round"))))
            ;; dropshadow
            (draw group (:circle :cx (+ x depth 1) :cy (1- (+ y depth))
                         :r (/ w 3)) :stroke "none" :fill "black")
            ;; line segment to next position:
            (draw group (:line :x1 x :y1 y :x2 dx :y2 dy))
            ;; node
            (draw group (:circle :cx x :cy y :r (/ w 4)))
            ;; random branch
            (when (and (> depth 0) (> (random 1.0) 0.85))
              (root canvas x y (+ angle (random-range -60 60)) (1- depth) alpha))
            (setf x dx
                  y dy)))))
        (when (and (> depth 0) (> (random 1.0) 0.7))
          (root canvas x y angle (1- depth) alpha))))

(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700"))
       (rg (make-radial-gradient scene (:id :generate
                                        :cx "50%" :cy "50%" :r "50%")
             (stop :color "rgb(32, 38, 0)" :offset "0%")
             (stop :color "rgb(13, 15, 0)" :offset "100%"))))
  (draw scene (:rect :x 0 :y 0 :height "100%" :width "100%")
               :fill (xlink-href rg))
  (root scene 350 350 (random 360) 7)
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; paths - an example from the SVG spec
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700")))
  (title scene "Path test")
  (draw scene (:path :d (path
                          (move-to 100 400)
                          (line-to-r 50 -25)
                          (arc-to-r 25 25 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 50 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 75 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 100 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (vertical-to-r 50)))
               :fill "none" :stroke "blue" :stroke-width 5)
  (with-open-file (s #P"/Users/jgarcia/Desktop/test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Some random curves.
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700"))
       (rg (make-radial-gradient scene (:id :generate
                                        :cx "50%" :cy "50%" :r "50%")
             (stop :color "rgb(32, 38, 0)" :offset "0%")
             (stop :color "rgb(13, 15, 0)" :offset "100%")))
       (curvy (make-path)))
  (draw scene (:rect :x 0 :y 0 :height "100%" :width "100%")
               :fill (xlink-href rg))
  ;; get to the center
  (with-path curvy
    (move-to 350 350))
  ;; set up the first reflection
  (quadratic-curve-to (random 300) (random 300) (random 300) (random 300))
  ;;; random walk
  (dotimes (j 50)
    (with-path curvy
      (smooth-quadratic-curve-to (random 500) (random 500))))
  (draw scene (:path :d curvy) :fill "none" :stroke "yellow")
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Clicking on the middle rectangle exposes my vanity.
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 300 :width 300)))
  (desc scene "clickable, linked element")
  (style scene ".clickable:hover {stroke: purple};")
  (draw scene (:rect :x 5 :y 5 :height 30 :width 30))
  (link scene (:xlink-href "http://www.lingweenie.org/lisp/")
    (draw* (:rect :x 40 :y 40 :height 30 :width 30)
           :stroke "blue" :stroke-width 1 :fill "yellow"
           :class "clickable"))
  (transform ((rotate 45 90 90) (translate -13))
    (draw scene (:rect :x 75 :y 75 :height 30 :width 30)
          :fill "purple"))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


;;; More pictures: tendrils, just a way to play with some transformations.
;;; Inspired by: http://nodebox.net/code/index.php/Tendrils
(defun make-tendril (x y width length &key (step 1.0) (omega 3))
  (let ((angle (random 360))
        (rotation (random 360))
        (rx (* width 1.0))
        (ry (* width 0.50))
        (c 0)
        (n 0.0))
    #'(lambda (canvas &key (distance 3.0))
        (when (< n length)
          (incf n)
          (transform (rotate rotation x y)
            (draw canvas (:ellipse :cx x :cy y :rx rx :ry ry)
                          :stroke-opacity (* 0.8 (/ n length))))
          (setf rotation (mod (+ rotation omega) 360))
          (incf x (* (cos (radians angle)) distance))
          (incf y (* (sin (radians angle)) distance))
          (setf rx (* width (- 1 (/ n length)))
                ry (* rx 0.50))
          (incf c (random-range (- step) step))
          (incf angle c)))))

(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700"))
       (rg (make-radial-gradient scene (:id :generate
                                        :cx "50%" :cy "50%" :r "50%")
             (stop :color "rgb(32, 38, 0)" :offset "0%")
             (stop :color "rgb(13, 15, 0)" :offset "100%"))))
  (draw scene (:rect :x 0 :y 0 :height "100%" :width "100%")
               :fill (xlink-href rg))
  ;;; The styled group has to come here, otherwise the background gets
  ;;; drawn on top of the tendrils.
  (let ((style-group (make-group scene (:stroke "rgb(220,250,200)" :fill "none"))))
    (dotimes (i 12)
      (let ((tendril (make-tendril 350 350 15 200)))
        (dotimes (j 200)
          (funcall tendril style-group))))
    ;;; a few bigger ones
    (dotimes (i 3)
      (let ((tendril (make-tendril 350 350 20 300)))
        (dotimes (j 300)
          (funcall tendril style-group :distance 2)))))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Foreign objects
(with-svg-to-file (scene 'svg-1.1-toplevel :width 700 :height 700)
    (#p"test.svg" :if-exists :supersede)
  (draw scene (:rect :x 30 :y 30 :width 400 :height 150)
        :fill "rgb(230,230,230)" :stroke "black")
  (let ((fo (make-foreign-object scene (:x 31 :y 50 :width 398 :height 148))))
    (add-element fo "<body xmlns=\"http://www.w3.org/1999/xhtml\" align=\"justify\">")
    (add-element fo "<p>Here is a small bit of text which requires word
wrap.  The use of a <tt>foreignObject</tt> (with the appropriate XHTML
namespace) makes that possible. </p>")
    (add-element fo "<p>Nifty, eh?</p>")
    (add-element fo "</body>"))

  ;;; Transform the foreigner!
  (transform ((rotate 30 215 375) (translate 100))
    (draw scene (:rect :x 30 :y 300 :width 400 :height 150)
          :fill "rgb(230,230,230)" :stroke "black"))
  (let ((fo2 (transform ((rotate 30 215 375) (translate 100))
               (make-foreign-object scene (:x 31 :y 320 :width 398 :height 148)))))
    (add-element fo2 "<body xmlns=\"http://www.w3.org/1999/xhtml\" align=\"justify\">")
    (add-element fo2 "<p>The usual SVG transformations can be inflicted
on <tt>foreignObject</tt>s, too.</p>")
    (add-element fo2 "<p>This is <em>niftier</em>!</p>")
    (add-element fo2 "</body>")))

  
;;; testing.lisp ends here