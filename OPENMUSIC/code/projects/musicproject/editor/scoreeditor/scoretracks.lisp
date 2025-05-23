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
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;;;
;=========================================================================
;;;
; Author: Karim Haddad
;==================================
; Tracks Score editor
; for playing different microtonal
; scales using fluidsynth
;==================================
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;THE BOX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass! tracks () 
  ((voices :initform (list (make-instance 'voice)) :initarg :voices :accessor voices :type T :documentation "list of VOICE objects")
   (approx :accessor approx :initform nil :type t)
   (names :accessor names :initform '("VOICE"))
   (parent :accessor parent :initform nil) ;always nil, added to prevent some bugs
   (editor :accessor editor :initform nil) ;reference to the editor (user interface)
            ;Display parameters
   (display-mode :accessor display-mode :initform :close) ;open or close, depending on wether the right panel is open or not
   )
  (:icon 954)
  (:documentation "
tracks is a polyphonic object made of a superimposition of VOICE objects.
"))


(defmethod initialize-instance ((self tracks)  &rest args) 
  (call-next-method)
  (let* ((approx (mapcar #'get-approx (voices self)))
         (boxes (loop for i in (voices self)
                     collect (associated-box i)))
         (names (loop for i in boxes
                      collect (if i (name i) ""))))
    (if (null (car approx))
      (setf (approx self) '(120.0))
      (setf (approx self) approx))
    (setf (names self) names)))


(defclass* s-polybox (OMBoxEditCall) ())

(defmethod get-type-of-ed-box ((self tracks))  's-polybox)
;(defmethod default-obj-box-size ((self tracks)) (om-make-point 50 72))
;(defmethod get-frame-class ((self OMMidiFilebox)) 'boxmidiframe)

(defmethod s-polybox-p ((self s-polybox)) t)
(defmethod s-polybox-p ((self t)) nil)

#|
(defmethod omng-box-value :before ((self s-polybox) &optional (numout 0))
  "To close the editor bevore re-evaluating the box"
 (when (and (editorframe self) (not (equal (allow-lock self) "x")))
    (om-close-window (om-view-window (editorframe self)))))
|#


(defmethod OpenObjectEditor :after ((self s-polybox)) nil)

(defmethod omNG-save ((self tracks) &optional (values? nil)) 
  "Cons a Lisp expression that retuns a copy of self when it is evaluated."
  `(when (find-class ',(type-of self) nil)
     (let ((rep (make-instance ',(type-of self) 
                               :voices (list ,.(mapcar #'(lambda (box) (omNG-save box values?)) (voices self)))
                              ; :approx ',(approx self)
                              ; :from-file t
                               )))
       ;(setf (voices self) ',(voices self))
       (setf (approx rep) ',(approx self))
       (setf (names rep) ',(names self))
       rep
       )))

(defmethod omNG-copy ((self tracks))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self)
                             :voices (list ,.(mapcar #'(lambda (box) (omNG-copy box)) (voices self)))
                             )))

     (setf (approx rep) ',(approx self))
     (setf (names rep) ',(names self))
     rep
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EDITOR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(set-edit-param voiceeditor 'approx value)

;au fait la largeur de la premiere hide-bar
(defparameter *poly-ruler-width* 22)


(defmethod class-has-editor-p ((self tracks)) t)
(defmethod get-editor-class ((self tracks)) 'tracks-editor)

;; A VOIR

(defmethod default-edition-params ((self tracks)) 
  (list ;(cons 'winsize (or (get-win-ed-size self) (om-make-point 370 280)))
        ;(cons 'winpos (or (get-win-ed-pos self) (om-make-point 400 20)))
   (cons 'approx '(2))
   (cons 'indx '(4))
        ;initialize the 3 zoom levels for all 3 sub-editors
       ; (cons 'chord-seq-zoom 1) 
       ; (cons 'voice-zoom 1)
       ; (cons 'poly-zoom 1)
        ))

(defclass tracks-editor (editorview object-editor play-editor-mixin)
  (;(names :accessor names :initarg :names :initform '("VOICE"))
   (tunings :accessor tunings :initarg :tunings :initform '(2))
   (deltay :initform *size-h-min* :accessor deltay)))


(defmethod tracks-editor-p ((self tracks-editor)) t)
(defmethod tracks-editor-p ((self t)) nil)

(defmethod get-panel-class ((self tracks-editor)) 'trackspanel)

(defmethod editor-minimum-size ((self tracks-editor))
  (om-make-point 200 100))

(defmethod get-control-h ((self tracks-editor)) 45)



(defclass tracks-titlebar (score-titlebar) 
    (;(edit-buttons :accessor edit-buttons :initarg :edit-buttons :initform nil)
     ;(play-buttons :accessor play-buttons :initarg :play-buttons :initform nil)
     ;(time-view :accessor time-view :initarg :time-view :initform nil)
     (active-button :accessor active-button :initarg :active-button :initform nil)
     ))

#|
(defmethod om-draw-contents ((Self tracks-titlebar))
  (call-next-method)
  (let* ((comp-str (get-comp-str (object (om-view-container self)) (select-comp (om-view-container self))))
         (str-size (+ (om-string-size comp-str) 10)))
    (om-with-focused-view self 
      (om-draw-string (- (w self) str-size) 16 comp-str))))
|#

(defmethod show-composant ((self tracks-titlebar))
  (om-invalidate-view self))

(defmethod get-titlebar-class ((self tracks-editor)) 'tracks-titlebar)

;for cursor player
(defmethod editor-null-event-handler :after ((self tracks-editor))
  (let ((editors (editors (panel self))))
    (mapcar #'do-editor-null-event editors)))


(defmethod metaobj-scrollbars-params ((self tracks-editor))  '(:v nil))
(defmethod get-control-h ((self tracks-Editor)) #-linux 50 #+linux 70)
(defmethod get-editor-field-size ((self tracks-Editor)) (om-make-point 300000 20000))

;======== scoreobj PAneL =====

(defclass trackspanel (om-scroller view-with-ruler-x)
  ((object :initform nil :initarg :object :accessor object)
   (mode :initform :normal :accessor mode)
   (editors :accessor editors :initform '())
   (tunings :accessor tunings :initarg :tunings :initform '(2))
   (hide-buts :accessor hide-buts :initform nil)
   (resize-buts :accessor resize-buts :initform nil)
   (deltay :initform *size-h-min* :accessor deltay)
   (groups :initform nil :accessor groups)
   (cur-group-ind :initform 0 :accessor cur-group-ind))
  (:default-initargs 
   :scrollbars :v 
   :scroll-bar-type :always-visible
   :retain-scrollbars t))



(defmethod editor ((self trackspanel)) 
  (om-view-container self))

(defmethod trackspanel-p ((self trackspanel)) t)
(defmethod trackspanel-p ((self t)) nil)

;(defmethod get-edit-param ((self trackspanel) param)
;  (get-edit-param (editor self) param))

(defmethod report-modifications ((self trackspanel))
  (report-modifications (om-view-container self)))

(defmethod update-subviews ((self trackspanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))


(defmethod om-draw-contents ((self trackspanel))
  nil)

(defmethod get-object ((self trackspanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self trackspanel))
  (report-modifications (om-view-container self)))

(defmethod update-subviews ((self trackspanel)) nil)

;;avoir
(defmethod multibpf? ((self trackspanel)) nil)
(defmethod show-position ((self t)) nil)
(defmethod assoc-w ((self trackspanel)) (- (w self) 25))

(defmethod bpf-panel-class ((self trackspanel)) 'bpf-parameter-panel)
(defmethod list-panel-class ((self trackspanel)) 'list-parameter-panel)
(defmethod show-composant ((self trackspanel))
   (loop for item in (name-views self) do
         (show-composant item)))
;;;

;(om-make-point (w self) (- (h self) (get-control-h self)))

;;small utility while implementing approx
(defmethod set-tracks-approx ((self tracks) (approx list))
  (setf (approx self) approx))

             
(defmethod initialize-instance :after ((self tracks-editor) &rest l)
  (declare (ignore l))
  ;(load-edition-params self)
  ;;;put panel in editor
  (let* ((ed-view (om-make-view (get-panel-class self)
                                :owner self
                                :field-size (get-editor-field-size self)
                                :position (om-make-point 0 *poly-ruler-width*) 
                                :scrollbars (first (metaobj-scrollbars-params self))
                                :size (om-make-point (w self) (- (h self) *poly-ruler-width*)) ;20
                                )))
    (setf (editor (object self)) self)
    (let* ((obj (object self))
           (vx (voices obj))
           (dy 300)
           (posy 5))
      (setf (panel self) ed-view)
    (update-tracks-panel ed-view)
    ;remove buttons from last hide-bar
    (om-remove-subviews (car (hide-buts (panel self))) 
                        (om-subviews (car (hide-buts (panel self)))))
    (setf (tunings self) (approx (object self)))
    )))


(defmethod set-editor-tone ((self scorePanel) newtone)
  (if newtone
        (setf (staff-tone self) newtone)
      (setf (staff-tone self) *global-midi-approx*)) 
    (om-set-dialog-item-text (nth 10 (om-subviews  (ctr-view (editor self)))) 
                             (give-symbol-of-approx newtone))
    (update-panel self t) ;;; t pour le sheet
    (om-invalidate-view self t)
    t)

(defmethod update-tracks-panel ((self trackspanel))
  (let* ((editor (om-view-container self))
         (tracksobj (object editor))
         (voices (voices tracksobj)))
    (setf (groups self) 
          (loop for i from 1 to (length voices)
                                collect (list i t)))
     (let* ((vx (mapcar #'clone voices))
           (dy 300)
           (posy 5))
       (setf (tunings self) (approx tracksobj))
       (loop for v in vx
             do
               (progn
                 (push (om-make-view 'hide-bar :owner self :object v :ref (om::ref (editor self))
                                     :position (om-make-point 0 posy) :size (om-make-point 1100 15)
                                     :bg-color *azulote*)
                       (hide-buts self))
                 (incf posy 15)
                 (let ((panel (om-make-view (get-editor-class v) :owner  self :object v :ref (om::ref (editor self))
                               :position (om-make-point 0 posy) :size (om-make-point 1100 300))))
                   (push panel
                         (editors self)))
                (incf posy dy)
                ))
      (push 
       (om-make-view 'hide-bar :owner self :object (last-elem vx) :ref (om::ref (editor self))
                     :position (om-make-point 0 posy) :size (om-make-point 1100 15) :bg-color *azulote*) (hide-buts self))
      (apply 'om-add-subviews (cons self (x-append (hide-buts self) 
                                                   (editors self) 
                                                   )))
      ;index hide-bars
      (loop for h in (reverse (hide-buts self))
            for i from 0 to (length (hide-buts self))
              do (setf (index h) i))

      ;attach editors to hide-bars
      (loop for h in (reverse (cons nil (hide-buts self)))
            for v in (reverse (cons nil (editors self)))
            for b in (cons nil (reverse (editors self)))
            do (setf (at-editor h) v)
               (setf (bef-editor h) b))

      ;set tunnings for panel ;(check if necessary)
        (loop for i in (reverse (editors self))
               do (set-edit-param i 'approx  (staff-tone (panel i))))

         ;set scales according to approx to panels        
        (loop for i in (reverse (editors self))
              for tun in (tunings self)
              do 
                (progn
                  (set-editor-tone (panel i) tun) ; orig: tone
                  (update-panel (panel i))
                  ))
        
      ;set ports for each panel
        
        (loop for i in (reverse (editors self))
              for n from 0 to (length (editors self))
              do (set-port (object i) n))
        
       ;set barname name
        (loop for i in (reverse (hide-buts self))
              for n in (names tracksobj)
              do (show-barname i n)) 
        (loop for i in (reverse (editors self))
               do (set-edit-param i 'approx  (staff-tone (panel i))))
        (loop for i in (editors self)
              do (update-panel (panel i)))
       ;now should send autotuning
        (om-invalidate-view self t)
        )))

;(give-symbol-of-approx 54)

;;;;;;;;;;;;;;;;;;;;;INITIALIZATION


;(find-indx 7)
;(give-index-off-approx 4)



;in this case where the editor is not an om-window
;use the below method update-subviews!!!

;(defmethod oa::om-resize-callback ((self tracks-editor) x y w h)
;  (call-next-method)
;  (print (list self))
;)


;trackseditor: H: 565 W: 750

;;EDITORS:
;:position (om-make-point 0 posy) :size (om-make-point 1200 300))

(defmethod update-subviews ((self tracks-editor)) ;(om-inspect (panel self))
  "Handles subviews resizing (when the right panel is diplayed or hidden)"
  (let* ((panel (panel self))
         (eds (reverse (editors panel)))
         (hb (reverse (hide-buts panel)))
         (rb (reverse (resize-buts panel)))
         (num (length eds))
         (middle (round (- (w panel) 20) 2))
         (height (- (h self) 20))
         (heach (round (/ height num)))
         (width (abs (- (w panel) 1100))))

    (loop for i in eds 
          do (progn
               (om-set-view-size i (om-make-point (+ 1200 width)
                                                  (h i) ;heach ;(h i)
                                                  ))
               (om-invalidate-view i t)))

    (loop for i in (x-append hb rb)
            do (progn 
                 (om-set-view-size i (om-make-point (+ 1200 width) (h i)))
                 (om-invalidate-view i)))

    (om-set-view-size panel (om-make-point (w self) height))
    (om-invalidate-view panel t)
    ))


(defmethod update-poly ((self tracks-editor)) 
nil)


(defmethod update-buttons ((self tracks-editor) display-mode) 
;(print "upodate")
nil)
            

;; diplay-mode = :open / :close
;; => 2 default sizes
(defmethod get-win-ed-size ((self tracks) ) (om-make-point 750 380))
(defmethod get-win-ed-size2 ((self tracks)) (om-make-point 1500 380))


;;;; Key event handlers:

(defparameter *clicked-trk-panel* nil) ; A hack to catch key events on each editor.


(defmethod close-editorFrame ((self tracks-editor)) 
  (call-next-method)
  (setf *clicked-trk-panel* nil))


(defmethod unselect-panels ((self tracks-editor))
  "Unselects focus on all subpanels of track"
  (let* ((panel (panel self))
         (hidebuts (hide-buts panel)))
    (setf *clicked-trk-panel* nil)
    (loop for i in hidebuts
          do (progn
               (setf (active i) nil)
               (setf (iconid (active-button i)) 956)))
    (setf (iconid (active-button (title-bar self))) 955)
    (om-invalidate-view panel t)
    ))
  

(defmethod om-view-click-handler :before ((self scorePanel) where)
  (when (trackspanel-p (om-view-container (om-view-container self)))
    (setf *clicked-trk-panel* self)
    (let* ((ed (om-view-container self))
           (tpanel (om-view-container ed))
           (obj (object (om-view-container tpanel)))
           (panels (reverse (loop for i in (editors tpanel)
                                  collect (panel i))))
           (pos (position *clicked-trk-panel* panels))
           (hidebuts (reverse (hide-buts tpanel))))
      ;;;;
      (progn
        (loop for i in hidebuts
              do (progn 
                   (setf (active i) nil)
                   (setf (iconid (active-button i)) 956)))
        (setf (active (nth pos hidebuts)) t)
        (setf (iconid (active-button (nth pos hidebuts))) 955)
        (setf (iconid (active-button (title-bar (om-view-container tpanel)))) 956)
        (om-invalidate-view tpanel t)
        (om-invalidate-view (title-bar (om-view-container tpanel)) t)
        )
      ;;;;
      (setf (nth pos (voices obj))
            (object (om-view-container (nth pos panels))))
      )))



;;;;;;;;;SHORTCUTS

(defmethod get-help-list ((self trackspanel))
  (remove nil 
          (list '(("lrud" "Scroll all panels")
                  #-macosx("ctrl+lrud" "Scroll selected object")
                  #+macosx("cmd+lrud" "Scroll selected object")
                  ("esc" "scroll all panels to start")
                  (("H") "Help for TRACKS panel")
                  ("h" "help for selected panel")
                  ("space" "Play/Stop")
                  ))))

(defmethod handle-key-event ((self tracks-editor) char) 
  (if (om-command-key-p)
      (when *clicked-trk-panel*
      (scroll-pane *clicked-trk-panel* char));score individual panel
    (case char
      (#\H (show-help-window (format nil "Commands for ~A Editor" 
                                     (string-upcase (class-name (class-of (object self))))) 
                             (get-help-list (panel self))))
      ;;;scrolling
      (:om-key-esc  (init-pos-panels self))
      (:om-key-right (scroll-right-panels self))
      (:om-key-left (scroll-left-panels self))
      (:om-key-down (scroll-down-panel (panel self)))
      (:om-key-up (scroll-up-panel (panel self)))
      (#\SPACE (editor-play/stop self))
      ))
  (handle-key-event *clicked-trk-panel* char))

;;;;;;;;;;;;;;;;scrolling

(defmethod init-pos-panels ((self tracks-editor))
  (let ((panels (mapcar #'panel (editors (panel self)))))
    (editor-stop self)
    (loop for i in panels 
          do (progn
               (om-set-scroll-position i (om-make-point 0 0))
               (oa::om-set-h-scroll-position i 0)
               (oa::om-set-v-scroll-position i 0)))))

(defmethod scroll-right-panels ((self tracks-editor))
  (let ((panels (mapcar #'panel (editors (panel self)))))
    (loop for i in panels 
          do (let* ((pos (om-scroll-position i))
                    (vpos (om-point-v pos))
                    (hpos (om-point-h pos))
                    (inc (if (om-shift-key-p) 500 50)))
               (progn
                 (om-set-scroll-position i (om-make-point (+ hpos inc) vpos))
                 (om-set-h-scroll-position i  (om-point-h (om-scroll-position i))))))))


(defmethod scroll-left-panels ((self tracks-editor))
  (let ((panels (mapcar #'panel (editors (panel self)))))
    (loop for i in panels 
          do (let* ((pos (om-scroll-position i))
                    (vpos (om-point-v pos))
                    (hpos (om-point-h pos))
                    (inc (if (om-shift-key-p) 500 50)))
               (progn
                 (om-set-scroll-position i (om-make-point (- hpos inc) vpos))
                 (om-set-h-scroll-position i  (om-point-h (om-scroll-position i))))))))

(defmethod scroll-down-panel ((self trackspanel))
  (let* ((pos (om-scroll-position self))
         (vpos (om-point-v pos))
         (hpos (om-point-h pos))
         (inc (if (om-shift-key-p) 500 50)))
           (om-set-scroll-position self (om-make-point hpos (+ vpos inc)))
       (oa::om-set-v-scroll-position self  (om-point-v (om-scroll-position self)))))

(defmethod scroll-up-panel ((self trackspanel))
  (let* ((pos (om-scroll-position self))
         (vpos (om-point-v pos))
         (hpos (om-point-h pos))
         (inc (if (om-shift-key-p) 500 50)))
           (om-set-scroll-position self (om-make-point hpos (- vpos inc)))
           (oa::om-set-v-scroll-position self  (om-point-v (om-scroll-position self)))))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;TITLE BAR AND MAIN CONTROLS
;;;integrate main player controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defmethod get-titlebar-class ((self tracks-editor)) 'score-titlebar)

(defmethod title-bar-infostring ((self tracks-editor))
  (format nil "Selection: ~A" (string-upcase (string (obj-mode (panel self)))))
)

(defmethod get-icon-button ((self tracks-editor) name)
  "Returns the title-bar of <self> having a name <namwe>.
   usage: (get-icon-button self name) where name is a string like play"
  (let* ((views (om-subviews self))
         (titlebar (car (remove-if-not #'(lambda (x) (equal (type-of x) 'score-titlebar)) views)))
         (buttons (remove-if-not #'(lambda (x) (equal (type-of x) 'om-icon-button)) 
                                 (om-subviews titlebar))))
    (remove-if-not #'(lambda (x) (equal (icon1 x) name)) 
                                 buttons)))


(defmethod set-pushed ((self tracks-editor) (name string) &optional (action t))
  "sets the button with name <name> (a string) to pushed (t) or initial unpushed"
(let ((button (car (get-icon-button self name))))
    (setf (selected-p button) action)
    (om-invalidate-view self)))
;;

(defmethod object-order ((self tracks-editor)) '("note" "chord" "chord-seq"))

(defmethod init-titlebar ((self tracks-editor))
  (call-next-method) 
  (setf (active-button (title-bar self))
        (om-make-view 'button-icon
                      :iconid (if *clicked-trk-panel* 956 955)
                      :action #'(lambda (item)
                                  (setf (iconid item) (if (= (iconid item) 955) 956 955))
                                  (if (= (iconid item) 955) 
                                      (unselect-panels self)))
                      :position (om-make-point 100 2)
                      :size (om-make-point 22 22)))
  #|
  (setf (edit-buttons (title-bar self))
        (list (om-make-view 'om-icon-button :position (om-make-point 180 2) :size (om-make-point 22 22)
                            :icon1 "mousecursor" :icon2 "mousecursor-pushed"
                            :lock-push t
                            :selected-p (equal :normal (get-edit-param self 'cursor-mode))
                            :action #'(lambda (item) (set-cursor-mode self :normal)))
              
              (om-make-view 'om-icon-button :position (om-make-point 201 2) :size (om-make-point 22 22)
                            :icon1 "beamcursor" :icon2 "beamcursor-pushed"
                            :lock-push t
                            :selected-p (equal :interval (get-edit-param self 'cursor-mode))
                            :action #'(lambda (item) (set-cursor-mode self :interval)))))
  |#
  
  ;240 + 21
  (setf (play-buttons (title-bar self))
        (list (om-make-view 'om-icon-button :position (om-make-point 240 2) :size (om-make-point 22 22)
                            :icon1 "play" :icon2 "play-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-play self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 261 2) :size (om-make-point 22 22)
                            :icon1 "pause" :icon2 "pause-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-pause self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 282 2) :size (om-make-point 22 22)
                            :icon1 "stop" :icon2 "stop-pushed"
                            :action #'(lambda (item) 
                                        (when (recording self)
                                          (stop-recording self)
                                          (setf (selected-p (nth 3 (play-buttons (title-bar self)))) nil)
                                          (setf (enabled (nth 0 (play-buttons (title-bar self)))) t)
                                          (setf (enabled (nth 1 (play-buttons (title-bar self)))) t)
                                          (setf (enabled (nth 4 (play-buttons (title-bar self)))) t)
                                          (om-invalidate-view (title-bar self))
                                          )
                                        
                                        (editor-stop self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 303 2) :size (om-make-point 22 22)
                            :icon1 "loopbutton" :icon2 "loopbutton-pushed"
                            :lock-push t
                            :selected-p (loop-play self)
                            :action #'(lambda (item) 
                                        (setf (loop-play self)
                                              (not (loop-play self)))
                                        (setf (selected-p item) (loop-play self))
                                        ))
              
              (om-make-view 'om-icon-button :position (om-make-point 366 2) :size (om-make-point 22 22)
                            :icon1 "player" :icon2 "player-pushed"
                            :action #'(lambda (item) 
                                        (let* ((editor self)
                                               (previousplayer (get-edit-param editor 'player))
                                               (newplayer (select-player editor)))
                                          (when (and newplayer (not (equal previousplayer newplayer)))
                                                     ;(om-set-dialog-item-text playertext (player-name newplayer))
                                            (player-special-action newplayer)
                                            (player-init newplayer)
                                            (update-player-controls editor newplayer)))))))

  (setf (time-view (title-bar self)) 
        (list  (om-make-dialog-item 'om-static-text 
                                    (om-make-point 600 0) 
                                    (om-make-point 200 25)
                                    "t : 0 ms"
                                    :font *om-default-font4*
                                    )))
  
  (apply 'om-add-subviews (cons (title-bar self) 
                                (append
                                 (list (active-button (title-bar self)))
                                 ;(edit-buttons (title-bar self))
                                 (play-buttons (title-bar self))
                                 (time-view (title-bar self))
                                 )
                                )))
    

;;;;;BAR-NAME
;---------------------------------------------------------------------
(defclass hide-bar (om-view) 
   ((index :initform 0 :initarg :index :accessor index)
    (barname :initform 0 :initarg :barname :accessor barname)
    (control-p :initform nil :initarg :control-p :accessor control-p)
    (at-editor :initform nil  :accessor at-editor)
    (mute :initform nil  :accessor mute)
    (active :initform nil  :initarg :active :accessor active)
    (active-button :initform nil  :initarg :active-button :accessor active-button)
    (closed-p :initform nil  :accessor closed-p)
    (bef-editor :initform nil  :accessor bef-editor)))


(defmethod get-panel ((self hide-bar))
   (om-view-container self))

(defmethod om-remove-subviews ((self hide-bar) &rest subviews)
  (loop for i in (car subviews)
          do (oa::internal-remove-subview self i)
        ))


(defmethod initialize-instance :after  ((self hide-bar) &key open?)
  (let* ((barname (om-make-dialog-item 'om-static-text 
                                       (om-make-point 60 -3) 
                                       (om-make-point 200 100)
                                       ""
                                    ;:font *om-default-font4*
                                       ))) 
    (setf (barname self) barname)
    (om-add-subviews self  
                     (om-make-view 'button-icon
                                   :iconid (if open? 164 165)    
                                   :action  #'(lambda (item) 
                                                (setf (iconid item) (if (= (iconid item) 164) 165 164))
                                                (show/hide-editor (om-view-container item)))
                                   :position (om-make-point 3 3)
                                   :size (om-make-point 11 11))
     
                     (om-make-view 'button-icon
                                   :iconid (if open? 952 951)    
                                   :action  #'(lambda (item) 
                                                (setf (iconid item) (if (= (iconid item) 952) 951 952)) ;changer les icones-> boutons ronds vert rouge
                                                (if (= (iconid item) 952)
                                                    (setf (mute self) t)
                                                  (setf (mute self) nil)))
                                   :position (om-make-point 20 2)
                                   :size (om-make-point 11 11))
                     (setf (active-button self)
                           (om-make-view 'button-icon
                                         :iconid (if (active self) 955 956)    
                                         #|
                                         :action  #'(lambda (item) 
                                                      (setf (iconid item) (if (= (iconid item) 955) 956 955)) ;changer les icones-> boutons ronds vert rouge
                                                      (if (active self)
                                                          (setf (iconid item) 956)
                                                        (setf (iconid item) 955)
                                                        ))
                                         |#
                                         :position (om-make-point 35 2)
                                         :size (om-make-point 11 11)))
                     barname
                     )))



(defmethod show-barname ((self hide-bar) name) 
    (om-set-dialog-item-text (barname self) 
                             (format nil "~A" name))
    )

(defmethod show/hide-editor ((self hide-bar)) 
  (when (at-editor self)
    (let* ((score-ed (at-editor self))
           (sizey-ed (om-point-y (om-view-size score-ed)))
           (sizex-self (om-point-x (om-view-size self)))
           (icon (iconid (car (om-subviews self))))
           (others (cdr (get-hide-components self)))
           (ypos (om-point-y (om-view-position self)))
           (height (h self))
           )
      
      (if (= 164 icon)
          (progn ;closed
            (setf (closed-p self) t)
            (om-set-view-size score-ed (om-make-point 0 20)) ;40
            (loop for i in others
                  do (let ((pos (om-point-y (om-view-position i))))
                       (om-set-view-position i (om-make-point 0 (- pos sizey-ed))))))
        (progn
          (setf (closed-p self) nil)
          (om-set-view-size score-ed (om-make-point sizex-self 300))
          (om-set-view-position score-ed (om-make-point 0 (- (+ ypos 15) 300))) 
          (loop for i in others
                do (let ((pos (om-point-y (om-view-position i))))
                     (om-set-view-position i (om-make-point 0 (+ 300 pos  ;(- ypos (* (index self) 10))
                                                                 ))))))
        )
      (om-invalidate-view (om-view-container self) t)
      )))


(defmethod selected-p ((self hide-bar)) (member (index self) (selected-index (om-view-container self))))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CALLBACKS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;access when editor is owner: (panel (om-view-container self))
;;access when panel  is owner: (om-view-container self)
;;this is why maby there are no scrollers, because editor is owner!

;;;;;;;;RESIZE

(defmethod get-next-components ((self hide-bar)) 
  (let* ((index (index self))
         (panel (om-view-container self))
         (comp (flat
                (loop for ed in (reverse (editors panel))
                      for hb in (reverse (hide-buts panel))
                      collect (list hb ed))))
         (res (x-append comp (car (hide-buts panel))))
         (pos (position self res)))
    (nthcdr (1+ pos) res)))


(defmethod get-hide-components ((self hide-bar)) ;(om-inspect (panel (om-view-container self))))
  (let* ((index (index self))
         (panel (om-view-container self))
         (comp (flat
                (loop for ed in (reverse (editors panel))
                      for hb in (reverse (hide-buts panel))
                      collect (list hb ed))))
         (res (x-append comp (car (hide-buts panel))))
         (pos (position self res)))
    (nthcdr pos res)))

(defmethod dont-drag-component ((self hide-bar))
  "don't drag if component before closed!"
  (let* ((panel (om-view-container self))
         (buttons (reverse (hide-buts panel)))
         (pos (position self buttons)))
    (unless (= 0 (index self))
      (closed-p (nth (1- pos) buttons)))))
        

(defmethod om-view-click-handler ((self hide-bar) where)
 ; (print (list "info" (dont-drag-component self)))
  nil)

;ne marche pas tres bien car position change une fois que  l'on
;a change la pos de la barre!

(defmethod om-click-motion-handler ((self hide-bar) position) 
  (unless (or (= 0 (index self)) (dont-drag-component self))
  (let ((indx (index self))
        (y (om-point-y position))
        (ysize (om-point-y (om-view-size (bef-editor self))))
        (xsize (om-point-x (om-view-size (bef-editor self))))
        (ypos (om-point-y (om-view-position self)))
        (others (get-next-components self)))
    (om-set-view-size (bef-editor self) (om-make-point xsize (+ ysize y)))
    (om-set-view-position self (om-make-point 0 (+ ypos y)))
    
    (loop for i in others
          do (let ((pos (om-point-y (om-view-position i))))
               (om-set-view-position i (om-make-point 0 (+ pos y))))))
  ))


;TODO
;re-init position
(defmethod om-view-doubleclick-handler ((self hide-bar) pos)
  nil)
#|
  (unless (= 0 (index self))
  (let ((indx (index self))
        (y (om-point-y pos))
        (ysize 300) ;(om-point-y (om-view-size (bef-editor self))))
        (xsize (om-point-x (om-view-size self)))
        (ypos (om-point-y (om-view-position self)))
        (others (get-next-components self)))
    (om-set-view-size (bef-editor self) (om-make-point xsize (+ ysize y 30)))
    (om-set-view-position self (om-make-point 0 (abs (- (+ ypos y) ysize))))
    
    (loop for i in others
          do (let ((posi (om-point-y (om-view-position i))))
               (om-set-view-position i (om-make-point 0 (abs (- (+ posi 0) (+ ysize y))))))))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Player;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod show-position-ms ((self tracks-editor) time) 
  (when (and time (not (minusp time)))
    (om-set-dialog-item-text (car (time-view (title-bar self))) 
                             (format () "t : ~D ms" time))
    ))

(defmethod update-cursor ((self scorepanel) time &optional y1 y2) 
  (show-position-ms (editor self) time) 
  (if (score-page-mode self)
      (let ((currevent (find time *events-play-cursor* :key 'car :test '>= :from-end t)))
        (when currevent
          
          (let* ((cur-evt (second currevent))
                 (rect-line (sixth currevent))
                 (rect-page (fourth currevent))
                 (rect-event (rectlist-page-to-pixel rect-page (or (score-scale self) 1) (rectangle cur-evt))))
            
            (om-update-transient-drawing self :x (car rect-event) :y (- (om-rect-top rect-line) (round (staff-size self) 2))
                                         :w 4 :h (- (om-rect-bottom rect-line) (om-rect-top rect-line)))
            ))
        
        )    
    (call-next-method))
  (when (tracks-editor-p (editor (om-view-container (om-view-container  self))))
    (show-position-ms (editor (om-view-container (om-view-container self))) time))
   
  )

;IMPORTANT!
;to add in fluid-player.lisp
(add-player-for-object 'tracks '(:fluidsynth))


(defmethod set-cursor-mode ((self tracks-editor) &optional mode)
  (let ((editors (editors self)))
    (loop for i in editors
          do (set-cursor-mode i mode))
  ;(change-cursor-mode (panel self) mode)
  ;(set-edit-param self 'cursor-mode (cursor-mode (panel self)))
    (setf (mode (panel self)) mode)
  (update-cursor-mode-buttons (title-bar self))))

;display-time-selection
;draw-interval-cursor

;(reduce #'max '(1 2 3 2 3))

;faire selective play:
;(hidebuts (panel self))

(defmethod get-mute-status ((self tracks-editor))
  (let* ((panel (panel self))
         (editors (reverse (editors panel)))
         (buttons (reverse (cdr (hide-buts panel))))
         (status (mapcar 'mute buttons)))
   (remove nil (loop for i in status
          for ed in editors
            collect (unless i ed)))))

(defmethod editor-play ((self tracks-editor))
  (unselect-panels self) 
  (let* ((editors (editors (panel self)))
         (objs (mapcar 'object editors))
         (durs (mapcar #'object-dur objs))
         (pos (position (reduce #'max durs) durs))
         (grt (nth pos editors)))
    (let ((play-ed (get-mute-status self)))
      (when play-ed
        (mapcar #'editor-play play-ed)
        (setf  (state (player self)) :play))) 
    (update-play-buttons (title-bar self))))

(defmethod editor-stop ((self tracks-editor)) 
  (let ((editors (editors (panel self))))
    (setf  (state (player self)) :stop)
    (mapcar #'editor-stop editors)
    (general-stop (player self))
    (update-play-buttons (title-bar self))
    ))  


(defmethod editor-pause ((self tracks-editor))
  (let ((editors (editors (panel self))))
    (mapcar #'editor-pause editors)
    (setf  (state (player self)) :pause)
    (update-play-buttons (title-bar self))
  ))

(defmethod editor-play/stop ((self tracks-editor)) 
  (unselect-panels self)
  (if (idle-p (player self))
      (progn (setf (state (player self)) :play)
        (editor-play self))
    (progn
      (setf  (state (player self)) :pause)
      (editor-pause self))))

(defmethod stop-editor-callback ((self tracks-editor))
  (update-play-buttons (title-bar self)))

