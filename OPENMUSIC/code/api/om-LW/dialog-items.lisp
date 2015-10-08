;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; DIALOG ITEMS CLASSES AND FUNCTIONS
;DocFile
;;===========================================================================

(in-package :om-api)
;;;========
;;; export :
;;;========
(export '(
                om-make-dialog-item
                om-dialog-item-act
                om-dialog-item-action
                om-dialog-item-action-function
                om-set-dialog-item-action-function
                om-funcall-dialog-item-action
                
                om-dialog-item-text
                om-set-dialog-item-text
                om-enable-dialog-item
                om-dialog-item-enabled
                
                om-static-text
                om-text-view
 
                om-editable-text
                ;om-get-text-edit-size
                ;om-get-text-edit-pos
                om-paste-command
                om-cut-command
                om-copy-command
                
                om-text-edit-view
                om-scroll-text-edit-view
                om-make-edit-view
                
                om-button
                ;om-icon-button
                om-check-box
                om-radio-button
                
                om-checked-p
                om-set-check-box
                
                om-single-item-list
                om-multi-item-list
                om-pop-up-dialog-item
                om-set-item-list
                om-get-item-list
                om-get-selected-item-index
                om-set-selected-item-index
                om-select-item-index
                om-unselect-item-index
                om-get-selected-item
                om-set-selected-item
                
                om-slider
                om-slider-value
                om-slider-increment
                om-set-slider-value
                om-get-slider-range
                om-get-slider-orientation

                
                ) :om-api)

;;;=====================
;;;ABSTRACT
;;;=====================



(defclass om-standard-dialog-item (om-graphic-object) 
  ((di-action :accessor di-action :initform nil :initarg :di-action)
   (dialog-item-after-fun :accessor dialog-item-after-fun :initform nil :initarg :dialog-item-after-fun))
  (:default-initargs 
   :visible-border :default
   :callback-type :item 
   :scroll-if-not-visible-p nil
   ;:callback 'om-dialog-item-action
   ))

;;;==========
;;; GENERAL API CALLS
;;;==========

(defmethod om-subviews ((self om-standard-dialog-item)) nil)

(defmethod om-invalidate-view ((self om-standard-dialog-item) &optional (erase t))
  (capi::redisplay self))

(defmethod om-redraw-view ((self om-standard-dialog-item))
 (capi::redisplay self))


;;;==========
;;; ACTION
;;;==========

(defmacro om-dialog-item-act (var &body body)
  `#'(lambda (,var) 
       (handler-bind 
           ((error #'(lambda (err)
                       (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                       (abort err))))
         ,@body)))

(defmethod om-dialog-item-action ((self om-standard-dialog-item))  
  (when (di-action self)
    (funcall (di-action self) self)))

(defmethod om-set-dialog-item-action-function ((self om-standard-dialog-item) action)
   (setf (di-action self) action))

(defmethod om-dialog-item-action-function ((self om-standard-dialog-item))
   (di-action self))

(defmethod om-funcall-dialog-item-action ((self om-standard-dialog-item))
    (when (di-action self)
      (funcall (di-action self) self)))

(defmethod om-enable-dialog-item ((self om-standard-dialog-item) t-or-nil) nil)

(defmethod om-dialog-item-enabled ((self om-standard-dialog-item)) t)

;;;==========
;;; ATTRIBUTES
;;;==========

(defmethod om-dialog-item-text ((self om-standard-dialog-item))
  (capi::item-text self))

(defmethod om-set-dialog-item-text ((self om-standard-dialog-item) text)
  (setf (capi::item-text self) text))

(defmethod om-view-position ((self om-standard-dialog-item)) 
   (if  (interface-visible-p self) 
       (let ((point (multiple-value-list (pinboard-pane-position self))))
         (om-make-point (first point) (second point)))
    (om-make-point (vx self) (vy self))))

(defmethod om-set-view-position ((self om-standard-dialog-item) pos-point) 
  (apply-in-pane-process self 
                         (lambda () 
                           (setf (pinboard-pane-position self) (values (om-point-h pos-point) (om-point-v pos-point)))
                           ))
  (setf (vx self) (om-point-h pos-point) (vy self) (om-point-v pos-point)))

(defmethod om-view-size ((self om-standard-dialog-item))
  (if  (interface-visible-p self) 
      (let ((point (multiple-value-list (pinboard-pane-size self))))
        (om-make-point (first point) (second point)))
    (om-make-point (vw self) (vh self))))

(defmethod om-set-view-size ((self om-standard-dialog-item) size-point)
  (setf (vw self) (om-point-h size-point) (vh self) (om-point-v size-point))
  (when (interface-visible-p self) 
    (apply-in-pane-process self 
                           (lambda () 
                             
                             (set-hint-table self (list :default-width (om-point-h size-point) :defalut-height (om-point-v size-point)
                                                        :external-min-width (om-point-h size-point) :external-min-height (om-point-v size-point)
                                                        :external-max-width (om-point-h size-point) :external-max-height (om-point-v size-point)
                                                        ))
                             (setf (pinboard-pane-size self) (values (om-point-h size-point) (om-point-v size-point)))
                             (setf (pinboard-pane-position self) (values (vx self) (vy self)))
                             ))
    ;(om-set-view-position self (om-make-point (vx self) (vy self)))
    )
  (di-after-settings self)
  )

(defmethod om-create-callback ((self om-standard-dialog-item))
  (set-hint-table self (list :default-x (vx self) :default-y (vy self) :default-width (vw self) :defalut-height (vh self))))


;=== CONSTRUCTOR ===
(defun dialog-item-scrollbar-h (scroll)
  (or (equal scroll :h) (equal scroll t)))
 
(defun dialog-item-scrollbar-v (scroll)
  (or (equal scroll :v) (equal scroll t)))

(defun om-make-dialog-item (class position size text &rest attributes &key
                                  container font bg-color fg-color (enable t)
                                  (allow-returns nil) (checked-p nil) (default-button nil) (focus nil) range
                                  (scrollbars nil)
                                  (direction :horizontal) (tick-side :default) (value 0) (radio-button-cluster nil)
                                  after-action modify-action image
                                  &allow-other-keys)
  (let* ((fcol (when fg-color (c fg-color)))
         (bcol (if bg-color (c bg-color))) ; :transparent))
         (position (if (subtypep class 'om-editable-text) (om-get-text-edit-pos position) position))
         (size (if (subtypep class 'om-editable-text) (om-get-text-edit-size size) size))        
         (di (apply 'make-instance 
                    (append (list class
                                  :default-x (om-point-h position)
                                  :default-y (om-point-v position)
                                  :default-width (om-point-h size)
                                  :default-height (om-point-v size)
                                  :external-min-width (unless (subtypep class 'om-static-text) (om-point-h size))
                                  :external-max-width (unless (subtypep class 'om-static-text) (om-point-h size))
                                  :visible-max-width nil
                                  :visible-max-height nil
                                  ;;#+linux :external-min-height (unless (subtypep class 'om-static-text) (om-point-v size))
                                  ;;#+linux :external-max-height (unless (subtypep class 'om-static-text) (om-point-v size))
                                  :width nil :height nil
                                  :text text
                                  :font font
                                  :enabled enable
                                  :background bcol
                                  :foreground fcol
                                  :selected checked-p
                                  :allows-newline-p allow-returns
                                  :default-p default-button
                                  :items range
                                  :start (or (first range) 0)
                                  :end (or (second range) 100)
                                  :orientation direction
                                  :start-point (if (equal direction :vertical) :bottom :left)
                                  :slug-start value
                           ; :popup-callback #'om-dialog-item-action
                                  :internal-border 0
                           ;:visible-border :default ;  nil  ; :outline
                                  :TITLE-ADJUST t
                                  :compact t
                                  :horizontal-scroll (dialog-item-scrollbar-h scrollbars)
                                  :vertical-scroll (dialog-item-scrollbar-v scrollbars)
                           ;:button-group radio-button-cluster
                           ;:title text
                                  :selected-item nil
                                  :allow-other-keys t)
                            attributes))))
    (when (or bg-color (special-bg di))
      (om-set-bg-color di (or bg-color (special-bg di))))
    (setf (vx di) (om-point-h position)
          (vy di) (om-point-v position)
          (vw di) (om-point-h size) 
          (vh di) (om-point-v size))
    (when container  (om-add-subviews container di))
    (when (om-item-view-p di) 
      (om-set-fg-color di fg-color)
      (om-enable-dialog-item di enable)
      (setf (item-x di) (vx di) (item-y di) (vy di))
      )
    (om-set-font di font)
    (when modify-action
      ;;; shortcut to specify both action and after action
      (setf (dialog-item-after-fun di) modify-action)
      (setf (di-action di) modify-action))
    (when after-action (setf (dialog-item-after-fun di) after-action))
    (di-after-settings di)
    (if focus (di-set-focus di))
    di))

(defmethod special-bg ((self t)) nil)
(defmethod di-after-settings (di) nil)
(defmethod di-set-focus ((self t)) (capi::set-pane-focus self))
(defmethod om-set-font ((self om-standard-dialog-item) font) (when font (call-next-method)))

;(defmethod capi::accepts-focus-p ((self om-standard-dialog-item)) 
;  (print self)  
;  t)


#-cocoa
;(defmethod (setf vcontainer) :around ((cont om-graphic-object) (di om-standard-dialog-item))
;  (call-next-method)
;  (om-set-bg-color di (om-get-bg-color cont)))



;;;========================== 
;;;DIALOG ITEMS CLASSES
;;;==========================  

;;; TEXT UTILS

;;; List to multi-line text
(defun make-wrapped-text (list)
  (concatenate 'string (car list)
               (reduce #'(lambda (res str) (concatenate 'string res (string #\Linefeed) str))
                       (cdr list) :initial-value "")))

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun str2list-path (str)
  (let (list)
    (loop while str do
          (let ((rep (multiple-value-list (string-until-char str ";"))))
            (setf str (second rep))
            (when (first rep) (push (first rep) list))))
    (reverse list)))

(defun text-wrap-pix (text-str font width-pix)
  (if (find #\Newline text-str)
      (loop for ttt in (om-text-to-lines text-str) append (text-wrap-pix ttt font width-pix))
    (let* ((p (position width-pix (gp::compute-char-extents *record-view* text-str (and font (gp::find-best-font *record-view* font))) :test '<)))
      (if p (capi::wrap-text text-str (max 1 (- p 1))) (list text-str)))))

;--------om-static-text

(defclass om-static-text (om-item-view) ;;;  (om-meta-view capi::pinboard-layout)
  ((text :initform nil :initarg :text :accessor text)
   (wrapped-text :initform nil :initarg :wrapped-text :accessor wrapped-text)
   (dx :initform 1 :initarg :dx :accessor dx)
   (dy :initform 1 :initarg :dy :accessor dy)
   (il :initform 1 :initarg :il :accessor il)
   (lineh :initform 5 :initarg :lineh :accessor lineh)
   (di-action :accessor di-action :initform nil :initarg :di-action)
   (enabl :accessor enabl :initform t :initarg :enabl)
   ;(drawing :accessor drawing :initform nil :initarg :drawing)
   )
  (:default-initargs :visible-border nil
   :accepts-focus-p nil
   :callback 'om-dialog-item-action)
)

; (defmethod om-subviews ((self om-static-text)) nil)

(defmethod om-set-dialog-item-text ((self om-static-text) text)
  (setf (text self) text)
  (update-text self))

;; with column layout...
(defmethod update-contents ((self om-static-text))
  (let ((strings (wrapped-text self)))
    (setf (capi:layout-description self)
          (loop for string in strings
                collect (make-instance 'capi:item-pinboard-object
                                       :text string)))))

(defmethod om-dialog-item-text ((self om-static-text)) (text self))

(defmethod di-after-settings ((self om-static-text))
  (update-text self))

(defmethod update-text ((self om-static-text))
  (let ((text (text self))
        (p 0)
        (font (or  
               (om-get-font self)
               (gp::make-font-description :family "Arial" :size 6))))
    (setf (lineh self) #+cocoa(* (om-string-h font) 1) #-cocoa(om-string-h font))
     (if (interface-visible-p self)
        ;(setf (wrapped-text self) (print (capi::wrap-text-for-pane self text)))
        (setf (wrapped-text self) (text-wrap-pix text font (vw self)))
      (setf (wrapped-text self) (text-wrap-pix text font (vw self))))
    (capi::redraw-pinboard-object self)
    ))

(defmethod om-set-view-size ((self om-static-text) size-point)
  (call-next-method)
  (update-text self)
  )

(defmethod om-draw-contents ((self om-static-text))
  (let ((port (capi::pinboard-object-pinboard self)))
    (when port
      (gp::with-graphics-state (port 
                                ;:mask (list *pox* *poy* (vw self) (vh self))
                                )
      (loop for line in (or (wrapped-text self) (list (text self)))
            for posy = 0 then (+ posy 1) 
            for y = (lineh self) then (round (+ (lineh self) (* posy (lineh self) (il self))))
            while (< (- y (lineh self)) (vh self)) do
           (gp:draw-string port
                            (substitute #\Space #\Tab line) (+ (dx self) *pox*) (round (+ (lineh self) (* posy (lineh self) (il self)) *poy*)))
            ))
    (when (om-component-border self)
      (if (om-color-p (om-component-border self))
          (om-with-fg-color port (om-component-border self)
            (gp::draw-rectangle port *pox* *poy* (- (vw self) 1) (- (vh self) 1)))
        (gp::draw-rectangle port *pox* *poy* (- (vw self) 1) (- (vh self) 1))))
      )
    ))

;(setf (capi::simple-pane-enabled self) t-or-nil)
(defmethod om-enable-dialog-item ((self om-static-text) t-or-nil)
  (unless (equal t-or-nil (enabl self))
    (if (setf (enabl self) t-or-nil)
        (setf (capi::pinboard-object-graphics-arg self :foreground) :black)
      (setf (capi::pinboard-object-graphics-arg self :foreground) (color::make-rgb 0.5 0.5 0.5)))))

; (capi::simple-pane-enabled self)
(defmethod om-dialog-item-enabled ((self om-static-text)) 
  (enabl self))

;;; ACTIONS from om-standard-dialog-item

(defmethod om-dialog-item-action ((self om-static-text))  
  (when (di-action self)
    (funcall (di-action self) self)))

(defmethod om-set-dialog-item-action-function ((self om-static-text) action)
   (setf (di-action self) action))

(defmethod om-dialog-item-action-function ((self om-static-text))
   (di-action self))

(defmethod om-funcall-dialog-item-action ((self om-static-text))
    (when (di-action self)
      (funcall (di-action self) self)))


(defmethod om-set-font ((self om-static-text) font)
  (call-next-method self (or font *om-default-font1*))
  (update-text self))


;-------- om-text-view

(defclass om-text-view (om-standard-dialog-item display-pane) ()
  (:default-initargs :visible-border nil
      :accepts-focus-p nil
      :callback 'om-dialog-item-action))

;(defmethod capi::accepts-focus-p ((self om-text-view)) (print self) nil)

(defmethod om-dialog-item-text ((self om-text-view))
 (capi::display-pane-text self))

(defmethod om-set-dialog-item-text ((self om-text-view) text)
  (setf (capi::display-pane-text self) text))

(defmethod om-enable-dialog-item ((self om-text-view) t-or-nil)
  (setf (capi::simple-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-text-view)) 
  (capi::simple-pane-enabled self))





;--------om-editable-text

(defclass om-editable-text (om-standard-dialog-item text-input-pane) ()
  (:default-initargs
   :visible-border :outline
   :callback 'om-dialog-item-action
   :navigation-callback 'text-edit-special-action
   :change-callback 'text-edit-changed-action
   :editing-callback 'text-edited-action))

;;; :allows-newline-p
;;; :compact
;;; :multi-line-p
;;; :editing-callback


;(let ((pt (om-add-points init-size (om-make-point 16 16))))
;  (om-make-point (max 32 (om-point-h pt)) (max 32 (om-point-v pt))))
  
(defun om-get-text-edit-size (init-size) 
  #+cocoa(om-add-points init-size (om-make-point 12 8)) 
  #-cocoa(om-add-points init-size (om-make-point 8 2))
  )

(defun om-get-text-edit-pos (init-pos)
  #+cocoa(om-add-points init-pos (om-make-point -4 -4))
  #-cocoa(om-add-points init-pos (om-make-point -2 -1)))
   
(defmethod om-view-position ((self om-editable-text))
   (om-subtract-points (call-next-method) (om-get-text-edit-pos (om-make-point 0 0))))

(defmethod om-view-size ((self om-editable-text))
  (om-subtract-points (call-next-method) (om-get-text-edit-size (om-make-point 0 0))))

(defmethod om-set-view-position ((self om-editable-text) pos-point) 
  (call-next-method self (om-get-text-edit-pos pos-point)))

(defmethod om-set-view-size ((self om-editable-text) size-point)
  (call-next-method self (om-get-text-edit-size size-point)))


(defmethod special-bg ((self  om-editable-text)) *om-white-color*)

(defun text-edit-special-action (self action)
  ;(print (list "edit special" action))
  (cond ((equal action :enter) 
         (om-dialog-item-action self))
        ((equal action :return) (if (capi::text-input-allows-newline-p self)
                                    (let ((rec (capi::clipboard self)))
                                      (capi::set-clipboard self (string #\Newline))
                                      (capi::text-input-pane-paste self)
                                      (capi::set-clipboard self rec))
                                  (om-dialog-item-action self)
                                  ))
        ))

(defmethod om-dialog-item-after-action ((self om-standard-dialog-item)) 
  (if (dialog-item-after-fun self)
    (funcall (dialog-item-after-fun self) self)
    ))


(defun text-edited-action (self action)
  ;(print (list "edited" action))
  (if (equal action :end) (om-dialog-item-after-action self)))
 
(defun text-edit-changed-action (text self win position)
  ;(print (list "chaged" position text))
  (when (di-action self)
    (funcall (di-action self) self))
  (unless (or (string-equal text "")
              (> position (length text)))
    (om-view-key-handler self (elt text (max 0 (- position 1))))))


(defmethod om-dialog-item-text ((self om-editable-text))
 (capi::text-input-pane-text self))

(defmethod om-set-dialog-item-text ((self om-editable-text) text)
 (setf (capi::text-input-pane-text self) text))



(defmethod om-enable-dialog-item ((self om-editable-text) t-or-nil)
  (setf (capi::text-input-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-editable-text)) 
  (capi::text-input-pane-enabled self))

(defmethod di-set-focus ((self om-editable-text)) 
  (capi::set-pane-focus self)
  (capi::set-text-input-pane-selection self 0 (length (capi::text-input-pane-text self))))



(defmethod om-copy-command ((self om-editable-text))
  ;(capi::set-clipboard self (om-dialog-item-text self))
  (capi::text-input-pane-copy self))

(defmethod om-paste-command ((self om-editable-text))
  (let ((pos (capi::text-input-pane-selection self))
        (pasted (length (capi::clipboard self))))
    (if (capi::text-input-allows-newline-p self)
        (capi::text-input-pane-paste self)      
      (om-set-dialog-item-text self (remove #\Newline (capi::clipboard self))))
    (capi::set-text-input-pane-selection self (+ pos pasted) (+ pos pasted))))


(defmethod om-cut-command ((self om-editable-text))
  (capi::text-input-pane-cut self))




;--------om-text-edit-view

(defclass om-text-edit-view (om-standard-dialog-item multi-line-text-input-pane) ()
  (:default-initargs ;;; :navigation-callback 'text-edit-special-action 
  :change-callback 'text-edit-changed-action
   ;:allows-newline-p t
   :external-min-height nil
   :external-max-height nil))
;;; navigation callback does not work


(defmethod special-bg ((self om-text-edit-view)) *om-white-color*)

(defmethod om-dialog-item-text ((self om-text-edit-view))
 (capi::text-input-pane-text self))

(defmethod om-set-dialog-item-text ((self om-text-edit-view) text)
 (setf (capi::text-input-pane-text self) text))

(defmethod om-enable-dialog-item ((self om-text-edit-view) t-or-nil)
  (setf (capi::text-input-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-text-edit-view)) 
  (capi::text-input-pane-enabled self))

(defmethod di-set-focus ((self om-text-edit-view)) 
  (capi::set-pane-focus self)
  (capi::set-text-input-pane-selection self 0 (length (capi::text-input-pane-text self))))

(defmethod om-copy-command ((self om-text-edit-view))
  ;(capi::set-clipboard self (om-dialog-item-text self))
  (capi::text-input-pane-copy self))

(defmethod om-paste-command ((self om-text-edit-view))
  ;(om-set-dialog-item-text self (capi::clipboard self))
  (capi::text-input-pane-paste self))

(defmethod om-cut-command ((self om-text-edit-view))
  (capi::text-input-pane-cut self))


;--------om-button

(defclass om-button (om-standard-dialog-item push-button) ()
  (:default-initargs :callback 'om-dialog-item-action))

(defmethod om-dialog-item-text ((self om-button))
 (capi::item-text self))

(defmethod om-set-dialog-item-text ((self om-button) text)
 (setf (capi::item-text self) text))

(defmethod om-enable-dialog-item ((self om-button) t-or-nil)
  (setf (capi::button-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-button)) 
  (capi::button-enabled self))

;--------icon-button

;(defclass om-icon-button (om-button) ()
;  (:default-initargs :image *api-directory*))

;(defmethod di-after-settings ((self om-icon-button))
;  (when (icon self)
;    (setf (capi::button-image self) (icon self))))

;--------om-check-box

(defclass om-check-box (om-standard-dialog-item check-button) ()
  (:default-initargs 
   #+win32 :accepts-focus-p #+win32 nil
   :selection-callback 'om-dialog-item-action
   :retract-callback  'om-dialog-item-action))

(defmethod om-checked-p ((self om-check-box)) (button-selected self))

(defmethod om-set-check-box  ((self om-check-box) check?)
  (capi:apply-in-pane-process self 
                              #'(lambda () (setf (button-selected self) check?))))


;--------om-radio-button

(defclass om-radio-button (om-standard-dialog-item radio-button) 
  ((radio-button-cluster :initarg :radio-button-cluster :initform nil :accessor radio-button-cluster))
  (:default-initargs :callback 'om-dialog-item-action))

(defmethod om-radio-button-p ((self om-radio-button))  t)
(defmethod om-radio-button-p ((self t))  nil)

(defmethod om-dialog-item-action ((self om-radio-button))  
  (let ((container (capi::element-parent self)))
    (when container
      (let ((elems (capi::pane-children container)))
        (loop for item in elems do
              (when (and (om-radio-button-p item) (not (equal item self))
                         (equal (radio-button-cluster item) (radio-button-cluster self)))
                (om-set-check-box item nil)))))
  (call-next-method)))

(defmethod om-checked-p ((self om-radio-button)) (button-selected self))

(defmethod om-set-check-box  ((self om-radio-button) check?)
  (capi:apply-in-pane-process self 
                              #'(lambda () (setf (button-selected self) check?))))



;--------om-item-list abstract
(defclass om-item-list (om-standard-dialog-item list-panel) ()
  (:default-initargs 
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :action-callback 'double-click-on-list
   ;:vertical-scroll t
   :test-function 'string-equal))

(defmethod di-after-settings ((self om-item-list))
  (set-hint-table self (list :external-min-width (vw self) :external-max-width (vw self) 
                             :external-min-height (vh self) :external-max-height (vh self)
                             )))

(defmethod special-bg ((self om-item-list)) *om-white-color*)

(defun vector-col-to-list (v)
  (loop for i from 0 to (- (length v) 1) collect (elt v i)))

(defmethod om-set-item-list ((self om-item-list) names)
  (setf (capi::collection-items self) names))

(defmethod om-get-item-list ((self om-item-list))
  (vector-col-to-list (capi::collection-items self )))

(defmethod om-get-selected-item-index ((self om-item-list))
  (capi::choice-selection self))

(defmethod om-set-selected-item-index ((self om-item-list) ind)
  (setf (choice-selection self) ind))

(defmethod double-click-on-list ((self om-item-list)) 
  (om-dialog-item-after-action self))

;--------om-single-item-list  

(defclass om-single-item-list (om-item-list) ()
  (:default-initargs :interaction :single-selection
   :retract-callback 'item-list-unselect))

(defmethod item-list-unselect ((self om-single-item-list))
  (unless (choice-selection self)
    (setf (choice-selection self) 0)
    ))

(defmethod om-select-item-index ((self om-single-item-list) i)
 (setf (choice-selection self) i))

(defmethod om-unselect-item-index ((self om-single-item-list) i)
  (when (and (choice-selection self) (= (choice-selection self) i))
    (setf (choice-selection self) nil)))

(defmethod om-get-selected-item ((self om-single-item-list)) 
  (choice-selected-item self))

(defmethod om-set-selected-item ((self om-single-item-list) item) 
  (setf (choice-selected-item self) item))

(defmethod om-dialog-item-action ((self om-single-item-list)) 
  (call-next-method))


;--------om-multi-item-list
 
(defclass om-multi-item-list (om-item-list) ()
 (:default-initargs 
   :interaction  #+win32 :multiple-selection #-win32 :extended-selection
   :right-click-selection-behavior :clicked/restore/restore
   :extend-callback 'om-dialog-item-action
   :retract-callback 'om-dialog-item-action))

;;; :multiple-selection
(defmethod om-select-item-index ((self om-multi-item-list) i)
  (setf (choice-selection self) (union (choice-selection self) (if (listp i) i (list i)))))

(defmethod om-unselect-item-index ((self om-multi-item-list) i)
  (setf (choice-selection self) (remove i (choice-selection self))))

(defmethod om-get-selected-item ((self om-multi-item-list)) 
  (choice-selected-items self))

(defmethod om-set-selected-item ((self om-multi-item-list) items) 
  (setf (choice-selected-items self) (if (listp items) items (list items))))


;--------om-slider 

(defclass om-slider (om-standard-dialog-item slider) 
  ((increment :initarg :increment :initform 1 :accessor increment))
  (:default-initargs 
   :callback 'om-slider-item-action
   :show-value-p nil)) ;;; for windows / linux only - removed because there is a bug in print with reverse directions
 
;(round 22 3)

(defmethod om-slider-value ((self om-slider))
  (* (round (range-slug-start self) (increment self)) (increment self)))

(defmethod om-slider-increment ((self om-slider))
  (increment self))

(defmethod om-set-slider-value ((self om-slider) value)
   (setf (range-slug-start self) value))

(defmethod om-slider-item-action ((self om-standard-dialog-item) value type)
  (when (di-action self)
    (funcall (di-action self) self)))

(defmethod om-get-slider-range ((self om-slider))
  (list (capi::range-start self) (capi::range-end self)))

(defmethod om-get-slider-orientation ((self om-slider))
  (capi::range-orientation self))

(defmethod om-enable-dialog-item ((self om-slider) t-or-nil)
  (setf (simple-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-slider))
  (simple-pane-enabled self))

;--------om-pop-up-dialog-item

(defclass om-pop-up-dialog-item (om-standard-dialog-item option-pane) 
  ((value :initform nil :initarg :value :accessor value))
  (:default-initargs 
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :test-function 'equal
   :separator-item "-"))   

(defmethod initialize-instance :after ((self om-pop-up-dialog-item) &rest l)
  (when (value self)
    (let ((pos (position (value self) (vector-col-to-list (capi::collection-items self )) :test 'string-equal)))
      (when pos
        (setf (choice-selection self) pos)))))

(defmethod om-dialog-item-action ((self om-pop-up-dialog-item))  
  (when (di-action self)
    (funcall (di-action self) self)))

;(defmethod set-dialog-item-action-function ((self om-pop-up-dialog-item) f)
;  (when f
;    (loop for item in (menu-items self) do
;          (set-menu-item-action-function item #'(lambda () (funcall f self))))))

(defmethod om-get-item-list ((self om-pop-up-dialog-item)) 
  (vector-col-to-list (collection-items self)))

(defmethod om-set-item-list ((self om-pop-up-dialog-item) names)
  (setf (capi::collection-items self) names))

(defmethod om-enable-dialog-item ((self om-pop-up-dialog-item) t-or-nil)
  (setf (option-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-pop-up-dialog-item))
  (option-pane-enabled self))

;;; !!!!
(defmethod om-get-selected-item ((self om-pop-up-dialog-item))
  (when (choice-selection self) (nth (choice-selection self) (om-get-item-list self))))

(defmethod om-get-selected-item-index ((self om-pop-up-dialog-item))
  (choice-selection self))

(defmethod om-set-selected-item-index ((self om-pop-up-dialog-item) pos)
  (setf (choice-selection self) pos))

(defmethod om-set-selected-item ((self om-pop-up-dialog-item) str)
  (let ((pos (position str (om-get-item-list self) :test 'string-equal)))
    (when pos (setf (choice-selection self) pos))))

(defmethod om-select-item-index ((self om-pop-up-dialog-item) index)
 (setf (choice-selection self) index))
 



