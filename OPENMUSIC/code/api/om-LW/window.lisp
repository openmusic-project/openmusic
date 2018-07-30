;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;;===========================================================================
;DocFile
; GUI WINDOWS
;DocFile
;;===========================================================================


(export '(om-window
          om-make-window
          om-window-title
          om-set-window-title
          om-interior-size
          om-set-interior-size
          om-def-higher-level-win
          om-front-window
          om-get-all-windows
          om-fullscreen-window
          om-screen-size
          om-window-maximized
          om-maximize-window
          om-dialog
          om-textured-dialog
          om-modal-dialog
          om-return-from-modal-dialog
          
          om-windoid
          om-no-border-win
          
          om-window-check-before-close
          om-window-close-event
          om-close-window
          om-window-open-p
          om-window-visible-p
          om-select-window
          om-hide-window
          om-show-window
          om-window-activate
          om-window-resized
          om-minimum-size) :om-api)


(in-package :oa)



;;;=============================================
;;; OM-ABSTRACT-WINDOW
;;; Superclass of all windows
;;;=============================================
(defclass om-abstract-window (om-graphic-object) 
  ((resizable :initarg :resizable :initform t :accessor resizable)
   (fs :initarg :fs :initform nil :accessor fs))
  (:default-initargs
   :auto-menus nil
   :activate-callback 'om-activate-callback
   :confirm-destroy-function 'om-confirm-destroy-function
   ;; :help-callback 'om-help-callback
   ))


(defmethod om-activate-callback (self activatep))

(defun om-confirm-destroy-function (self)
  (om-window-check-before-close self))

(defmethod om-window-check-before-close ((self t)) t)

(defmethod om-help-callback ((interface t) pane type key))

(defmethod om-view-container ((self om-abstract-window)) nil)

(defmethod om-get-view ((self om-abstract-window))   (or *default-printer-port* (pane-layout self)))

(defmethod om-window-title ((self om-abstract-window))
  (interface-title self))

(defmethod om-set-window-title ((self om-abstract-window) (title string))
  (setf (interface-title self) title))

(defmethod om-view-position ((self om-abstract-window)) 
  (if (interface-visible-p self)
      (let ((point (interface-geometry self)))
        (om-make-point (first point) (second point)))
    (om-make-point (vx self) (vy self))))

(defmethod om-set-view-position ((self om-abstract-window) pos-point) 
  (when (interface-visible-p self)
      (execute-with-interface self 
                              'set-top-level-interface-geometry 
                              self 
                              :x (om-point-h pos-point)
                              :y (om-point-v pos-point)))
  (setf (vx self) (om-point-h pos-point))
  (setf (vy self) (om-point-v pos-point))
  )

(defmethod om-view-size ((self om-abstract-window))
  (if (interface-visible-p self)
      (let ((point (capi::interface-geometry self)))
        #+win32 (om-make-point (- (third point) (car point)) (- (fourth point) (cadr point)))
        #+cocoa (om-make-point (third point) (fourth point))
	#+linux (om-make-point (third point) (fourth point)))
      (om-make-point (vw self) (vh self))))


(defmethod set-not-resizable ((self t) &optional width height)
  (capi::execute-with-interface self 
          #'(lambda (win)
              (let ((w (or width (om-point-h (om-view-size win))))
                    (h (or height (om-point-v (om-view-size win)))))
                (set-hint-table win (list :external-min-width w :external-max-width w 
                                           :external-min-height h :external-max-height h))))
          self))

(defmethod om-set-view-size ((self om-abstract-window) size-point) 
  (let ((wi (or (om-point-h size-point) (om-width self)))
        (he (or (om-point-v size-point) (om-height self))))
    (execute-with-interface self 
                            #'(lambda (w h)
                                (unless (resizable self)
                                  (set-not-resizable self w h))
                                (when (interface-visible-p self)
                                  (set-top-level-interface-geometry 
                                   self :width w :height h
                                   :x (vx self) :y (vy self)))
                                (setf (vw self) w (vh self) h)
                                (om-window-resized self (om-make-point wi he))
                                )
                            wi he)
    ))
    


#|
(defmethod om-interior-size ((self om-abstract-window))
  ;;(print (capi::interface-geometry self))
  #+win32
  (if (capi::interface-visible-p (capi::pane-layout self))
      (multiple-value-bind (w h)
          (capi::pinboard-pane-size (capi::pane-layout self))
        (om-make-point w h))
    (om-view-size self))
  #+linux(om-subtract-points (om-view-size self) (om-make-point 0 30))
  #+macos(om-view-size self)
  )
|#

(defmethod om-interior-size ((self om-abstract-window))
  (multiple-value-bind (w h)
          (capi::pinboard-pane-size (capi::pane-layout self))
        (om-make-point w h)))
	

(defmethod om-set-interior-size ((self om-abstract-window) size)
  (om-set-view-size self size))

(defmethod om-resize-callback ((self om-abstract-window) x y w h)
  (unless (and (= w (vw self)) (= h (vh self)))
   (om-window-resized self (om-make-point w h)))
  #-linux (call-next-method))

(defmethod om-window-resized ((self om-abstract-window) size)
  (declare (ignore self size))
  nil)

(defmethod om-window-maximized ((self om-abstract-window))
  (equal (capi::top-level-interface-display-state self) :maximized))

(defmethod om-maximize-window ((self om-abstract-window))
  (setf (capi::top-level-interface-display-state self) :maximized))

(defmethod om-minimum-size ((self om-abstract-window))
  (declare (ignore self)) nil)

(defmethod capi::calculate-constraints ((self om-abstract-window))
  (call-next-method)
  (let ((msize (om-minimum-size self)))
    (when msize
      (with-geometry self
        (setf capi:%min-width% (om-point-h msize))
        (setf capi:%min-height% (om-point-v msize))
        ))))

(defmethod om-fullscreen-window ((self om-abstract-window))
  ;;; pour l'instant, maximize... à faire en fullscreen
  (setf (fs self) t)
  ;(execute-with-interface self 
  ;                        'set-top-level-interface-geometry 
  ;                        self 
  ;                        :width (capi::screen-width (capi:convert-to-screen self))
  ;                        :height (capi::screen-height (capi:convert-to-screen self))
  ;                        :x 0 :y 0)
  (om-set-view-position self (om-make-point 0 0))

  (om-set-view-size self (om-make-point (capi::screen-width (capi:convert-to-screen self)) 
                                       (- (capi::screen-height (capi:convert-to-screen self)) 20)))
  )

(defun om-screen-size ()
  (om-make-point (capi::screen-width (capi:convert-to-screen nil))
                 (capi::screen-height (capi:convert-to-screen nil))))

;; test, parfois collect-interfaces plante...
(defun om-front-window () 
  #+(or darwin macos macosx)
  (capi:screen-active-interface (capi:convert-to-screen))
  #-(or darwin macos macosx)
  ; --> ça plante (parfois)
  (car (capi::collect-interfaces 'capi:interface :screen :any :sort-by :visible))
)

(defun om-get-all-windows (class)
  (capi::collect-interfaces class))

(defmethod interface-match-p ((self om-abstract-window) &rest initargs  &key name)
  (string-equal (capi::capi-object-name self) name))

(defmethod om-select-window ((self om-abstract-window))
  (when (and (window-dialog-p self) (not (initialized-p self)))
    (capi::display self))
  (when (initialized-p self)
    #+cocoa(capi::raise-interface self)
    #-cocoa(capi::find-interface (type-of self) :name (capi::capi-object-name self))
    )
  ;(om-activate-callback self t)
  self)

(defmethod om-hide-window ((self om-abstract-window))
  (capi::execute-with-interface self 
                                #'(lambda (x) (setf (top-level-interface-display-state x) :hidden))
                                self))

(defmethod om-window-visible-p ((self om-abstract-window))
  (capi::interface-visible-p self))

(defmethod om-show-window ((self om-abstract-window))
  (capi::execute-with-interface self 
                                #'(lambda (x) (capi::show-interface x))
                                self))

;;; Explicit call to close window
(defmethod om-close-window ((win t))
  (when win
    (capi:apply-in-pane-process win 'capi:quit-interface win)))

;;; Called by CAPI when destroying the window
(defmethod om-destroy-callback  ((self om-abstract-window))
  (capi::execute-with-interface self 'om-window-close-event self)
  (setf (initialized-p self) nil))

;;; OM event handler
(defmethod om-window-close-event ((self t)) nil)


;;; appelé à la création de la fenetre
;;; (pas a l'apparition)
(defmethod om-create-callback ((self om-abstract-window))
  ;; (set-hint-table self (list :default-x (vx self) :default-y (vy self) :default-width (vw self) :default-height (vh self)))
  ;; (loop for item in (vsubviews self) do
  ;;      (om-add-subviews thewin item))
  (setf (initialized-p self) t)
  t)


(defun om-window-open-p (window) 
  (and (initialized-p window) 
       (not (equal (capi::interface-created-state window) :destroyed))))

;;; Called by CAPI when activating the window
(defmethod om-activate-callback ((self om-abstract-window) activatep)
  ;;; TEST
  ;(when activatep
  (om-window-activate self activatep)
    ;)
  )

;;; OM event handler
(defmethod om-window-activate ((self om-abstract-window) &optional (activatep t)) t)
 
(defmethod interface-display :after ((self om-abstract-window)) 
  (update-for-subviews-changes self t))


; temp 
;(defmethod item-subviews ((self t)) nil)

(defmethod internal-add-subview ((self om-abstract-window) (subview om-graphic-object))
  (setf (vcontainer subview) (pane-layout self))
  (setf (vsubviews (pane-layout self)) (append (vsubviews (pane-layout self)) (list subview))))

(defmethod internal-remove-subview ((self om-abstract-window) (subview om-graphic-object))
  (setf (vcontainer subview) nil)
  (setf (vsubviews (pane-layout self)) (remove subview (vsubviews (pane-layout self))))
  ; (setf (element-parent subview) nil)
  )

(defmethod om-subviews ((self om-abstract-window)) 
    (vsubviews (pane-layout self)))

(defmethod correct-win-h ((win t)) nil)

(defun om-make-window (class &rest attributes 
			  &key (position (om-make-point 200 200)) (size (om-make-point 500 400))
			  name window-title owner (font *om-default-font2*)  (bg-color *om-white-color*)
			  (window-show t) subviews (resizable t) (close t) (minimize t) (maximize t) (topmost nil) (toolbox nil)
                          menu-items
			  &allow-other-keys)  
  (let* ((pos position)
         (size size)
         (winparent (or owner (capi:convert-to-screen)))
         (winname (or name (string (gensym))))
         (title (or window-title name "OM Window"))
         (w (om-point-h size))
         (h (om-point-v size))
         (x (if (equal pos :centered) (round (- (* (capi::screen-width winparent) 0.5) (* w 0.5))) (om-point-h pos)))
         (y (if (equal pos :centered) (round (- (* (capi::screen-height winparent) 0.5) (* h 0.5))) (om-point-v pos)))
         (style (append (get-window-styles-from-class class) 
                        (when (not minimize) (list :never-iconic))
                        (when topmost (list :always-on-top))
                        (when toolbox (list :toolbox))
                        (list :no-character-palette)))
         layout thewin)
     ;(if (> (+ x w) (capi::screen-width (capi:convert-to-screen nil))) 
     ;    (setf w (- (capi::screen-width (capi:convert-to-screen nil)) x)))
     ;(if (> (+ y h) (capi::screen-height (capi:convert-to-screen nil))) 
     ;    (setf h (- (capi::screen-height (capi:convert-to-screen nil)) y)))
     ;(print (list "SIZE FOR MAKE INSTANCE IS" size))
    (setf thewin (apply 'make-instance 
                        (cons class
                              (append 
                               (list :title title :name winname
                                     :x x :y y 
                                     :width w :height h
                                     ;:external-min-width w :external-min-height h
                                     ;;; this works for Windows (on Mac : :width / :height ==> a tester)
                                     :parent winparent
                                     :internal-border nil :external-border nil
                                     :display-state (if window-show :normal :hidden)
                                     ;:window-styles '(:internal-borderless)
                                     ;:external-min-height 50 :external-min-width 50
                                     :no-character-palette t
                                     ;:menu-bar-items nil
                                     #+cocoa :activate-callback #+cocoa #'(lambda (win activate-p) (when activate-p (om-add-menu-to-win win)))
                                     :window-styles style
                                     :font font
                                     :resizable resizable
                                     ;;; OM-GRAPHIC-OBJECT SLOTS
                                     :vx x :vy y :vw w :vh h
                                     :menu-bar-items menu-items
                                     :allow-other-keys t)
                               attributes
                               ))))
    
    (when (setf layout (make-window-layout thewin bg-color))
      #+cocoa(if (drawable-layout layout) (setf (capi::output-pane-display-callback layout) 'om-draw-contents-callback))
      (setf (capi::pane-layout thewin) layout))
     (when subviews (mapc (lambda (sv) (om-add-subviews thewin sv)) subviews))
     (correct-win-h thewin)
     (when (or (not resizable) (window-dialog-p thewin))
       (set-not-resizable thewin w h))
     (unless (window-dialog-p thewin)
       (internal-display thewin))
     
     ;; fixes geometry when x and y are out of the primary screen region
     (om-set-view-position thewin (om-make-point x y))
     
     thewin))

(defmethod internal-display ((self t))
  (capi::display self))
 
; :movable-by-window-background
; :borderless
; :shadowed 
(defmethod get-window-styles-from-class ((class t))
  (cond 
   ((subtypep class 'om-windoid) '(:internal-borderless :always-on-top 
                                  :toolbox 
                                   ;:textured-background  ;--> removed because it creates movable-by-bg
                                   :no-geometry-animation
                                   :ignores-keyboard-input
                                   )) ;  
   ((subtypep class 'om-no-border-win) '(:internal-borderless :borderless :shadowed))  ; :always-on-top
   ((subtypep class 'om-window) '(:internal-borderless))
   ((subtypep class 'om-textured-dialog) '(:textured-background))
   ((subtypep class 'om-dialog) '())
   ))



(defmethod window-dialog-p ((self t) ) nil)
    
;;;=====================
 
(defclass window-layout (om-view) ())

(defmethod drawable-layout ((self window-layout)) t)
(defmethod drawable-layout ((self t)) nil)

(defmethod make-window-layout ((self om-abstract-window) &optional color)
  (make-instance 'window-layout :internal-border nil :visible-border nil :accepts-focus-p nil
                 :background (if color (c color) *om-transparent-color*)))

(defmethod container-skip-layout ((self window-layout))
   (capi::top-level-interface self))

(defmethod om-view-window ((self window-layout))
  (capi::element-parent self))

(defmethod main-pinboard-object ((self window-layout)) (call-next-method))
(defmethod item-subviews ((self window-layout)) (call-next-method))

(defmethod om-draw-contents-callback ((self window-layout) x y w h)
  (om-draw-contents-callback (capi::top-level-interface self) x y w h)
  )


;(defmethod om-char-callback ((self window-layout) x y char type)
;  (om-char-callback (capi::element-parent self) x y char type))

;(defmethod om-char-spec-callback ((self window-layout) x y spec) (call-next-method))
;  (capi::set-pane-focus self)
;  (om-char-spec-callback (capi::element-parent self) x y spec))

;(defmethod om-clic-callback ((self window-layout) x y type)
;  (om-clic-callback (capi::top-level-interface self) x y type))

;(defmethod om-clic-motion-callback ((self window-layout) x y type)
;  (om-clic-motion-callback (capi::top-level-interface self) x y type))

;(defmethod om-view-double-clic-callback ((self window-layout) x y)
;  (om-view-double-clic-callback (capi::top-level-interface self) x y))

;(defmethod om-clic-release-callback ((self window-layout) x y type)
;  (om-clic-release-callback (capi::top-level-interface self) x y type))

;(defmethod om-motion-callback ((self window-layout) x y type) 
;  (om-motion-callback (capi::top-level-interface self) x y type))

;;;(defmethod om-init-motion ((self window-layout) x y) 
;;;  (om-init-motion (capi::top-level-interface self) x y))




;;;=============================================
;;; OM-WINDOW
;;; A simple window with a panel able to host subviews
;;;=============================================
(defclass om-window (om-abstract-window capi::interface) ())

(defmethod correct-win-h ((win om-window))
  ;(print (list "RESIZE!!" (om-view-size win) (om-add-points (om-view-size win) (om-make-point 0 00))))
  ;#+win32
  (om-set-view-size win (om-add-points (om-view-size win) (om-make-point 0 200)))
 t)

(defmethod update-for-subviews-changes ((self om-window) &optional (recursive nil))
  (capi::execute-with-interface self (lambda () (set-layout (pane-layout self))))
  (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t))) (vsubviews self)))
  (when (pane-layout self) (mapc 'update-po-position (item-subviews (pane-layout self))))
  )

;;;====================
;;; DIALOG
;;; Pour mettre des dialog-items
;;; Des interfaces modales, etc.
;;;====================
(defclass om-dialog (om-abstract-window capi::interface) ())

(defclass om-textured-dialog (om-dialog) ())

(defmethod window-dialog-p ((self om-dialog) )   t)

(defmethod make-window-layout ((self om-dialog) &optional color)
  (make-instance 'window-layout :internal-border nil :visible-border nil :accepts-focus-p nil
                 #+cocoa :background #+cocoa :transparent
                 ))

(defun om-modal-dialog (dialog &optional (owner nil))
  (update-for-subviews-changes dialog t)
  ;(print (list (vsubviews self)))
  (capi::display-dialog dialog :owner (or owner (om-front-window) (capi:convert-to-screen)) 
                        :position-relative-to :owner :x (vx dialog) :y (vy dialog)))

(defun om-return-from-modal-dialog (dialog val) 
  (capi::exit-dialog val))


;(capi::display (make-instance 'capi::interface :layout (make-instance 'window-layout)))
;;;====================
;;; PALETTE
;;; Always on top, petite barre de titre
;;;====================
(defclass om-windoid (om-window) 
  ((accept-key-evt :accessor accept-key-evt :initarg :accept-key-evt :initform nil))
  ;(:default-initargs
  ; :window-styles '(:internal-borderless :always-on-top :borderless :shadowed))
  )

(defmethod internal-display ((self om-windoid))
  ;#+win32(capi::display self :process nil)
  (capi::display self)
  )



(defmethod make-window-layout ((self om-windoid) &optional color)
  (make-instance 'window-layout :internal-border nil :visible-border nil 
                 :background :transparent))

(defmethod om-set-view-size ((self om-windoid) size-point) 
  (let ((w (om-point-h size-point))
        (h (om-point-v size-point)))
    ;(print (list w h))
    (set-not-resizable self w h)
    (call-next-method)))


(defmethod correct-win-h ((win om-windoid)) nil)

; (defmethod set-not-resizable ((self om-windoid) &optional w h) (call-next-method))
; (om-make-window 'om-windoid :resizable nil)


;;;====================
;;; NO BORDER
;;;====================
(defclass om-no-border-win (om-window) ()
 ;(:default-initargs
 ;  :window-styles '(:internal-borderless :borderless :shadowed))
 )

(defmethod om-fullscreen-window ((self om-no-border-win)) (call-next-method))

(defmethod correct-win-h ((win om-no-border-win)) nil)
