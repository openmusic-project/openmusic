;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg; Lowercase: Yes -*-
;;; $Id$
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(in-package :cl-svg)


;;; Normally shape attributes should be checked - all the SVG viewers I've
;;; seen so far silently ignore shapes missing attributes.  I'd prefer to get
;;; some warning.  However, a shape can inherit missing attributes from,
;;; say, a <use .../> element.  WITHOUT-ATTRIBUTE-CHECK allows one to make
;;; use of that behavior without crankiness from this library.
(defvar *check-required-attributes* t
  "Determines if the shape element attribute list is checked for completeness.")

(defmacro without-attribute-check (&body body)
  `(let ((*check-required-attributes* nil))
     ,@body))

(define-condition missing-attributes ()
  ((message :initarg :message :accessor missing-attribute-message))
  (:report (lambda (condition stream)
             (format stream (missing-attribute-message condition)))))


(defclass svg-element ()
  ((name
    :initarg :name
    :initform (error "SVG-ELEMENT must have a NAME")
    :accessor element-name
    :type string)
   (contents 
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor element-contents)
   (attributes
    :initarg :attributes
    :initform ()
    :accessor element-attributes
    :type list)))

(defgeneric element-id (element))

(defmethod element-id ((e svg-element))
  (getf (element-attributes e) :id))

(defgeneric (setf element-id) (id element))

(defmethod (setf element-id) (id (e svg-element))
  (setf (getf (element-attributes e) :id) id))

(defgeneric add-element (container element))

(defmethod add-element ((container svg-element) (element svg-element))
  (vector-push-extend element (element-contents container)))

;;; When you just have to add hand-rolled XML...
(defmethod add-element ((container svg-element) (element string))
  (vector-push-extend element (element-contents container)))

(defgeneric push-attribute (element attribute value))

(defmethod push-attribute ((e svg-element) attribute value)
  (setf (getf (element-attributes e) attribute) value))

(defgeneric has-attribute-p (element attribute))

(defmethod has-attribute-p ((e svg-element) attribute)
  (member attribute (element-attributes e))) 

(defgeneric get-attribute (element attribute))

(defmethod get-attribute ((element svg-element) attribute)
  (getf (element-attributes element) attribute))

(defgeneric add-namespace (element name url))

;;; Uses NCONC to place new namespaces at the end of the attributes list,
;;; a stylistic nicety only.
(defmethod add-namespace ((e svg-element) name url)
  (with-slots (attributes) e
    (nconc attributes (list (concatenate 'string "xmlns:" name) url))))

(defgeneric has-contents-p (element))

(defmethod has-contents-p ((element svg-element))
  (/= 0 (fill-pointer (element-contents element))))

;;; See TRANSFORM macro below.
(defgeneric add-transform (element transform)
  (:documentation
   "Push a transformation into an element.  If the property already has
contents the new transform is simply appended."))

(defmethod add-transform ((element svg-element) (transform string))
  (if (has-attribute-p element :transform)
      (push-attribute element :transform
        (concatenate 'string (get-attribute element :transform) " " transform))
      (push-attribute element :transform transform))
  ;; return the element so that (TRANSFORM ...) can be used neatly in LETs
  element)

(defgeneric add-class (element class)
  (:documentation
   "Push an XML class into an element.  If the property already has
contents the new transform is simply appended."))

(defmethod add-class ((element svg-element) (class string))
  (if (has-attribute-p element :class)
      (push-attribute element :class
        (concatenate 'string (get-attribute element :class) " " class))
      (push-attribute element :class class)))

(defgeneric stream-out (stream element))

(defmethod stream-out (s (e svg-element))
  (if (has-contents-p e)
      (with-xml-group-element (s (element-name e) (element-attributes e))
        (loop for sub-element across (element-contents e)
              do (stream-out s sub-element)))
      (element->xml s (element-name e) (element-attributes e))))

(defmethod stream-out (s (e string))
  (string->xml s e))

(defgeneric xlink-href (element)
  (:documentation "create a local URL reference to this element"))

(defmethod xlink-href ((e svg-element))
  (format nil "url(#~A)" (element-id e)))

(defmethod initialize-instance :after ((e svg-element) &key &allow-other-keys)
  (when (eql (element-id e) :generate)
    (setf (element-id e) (gensym (element-name e)))))


(defclass svg-toplevel (svg-element)
  ((xml-header :initarg :xml-header)
   (doctype :initarg :doctype)
   (defs :accessor svg-defs
         :initform (make-instance 'svg-element :name "defs"))
   (stylesheets :initarg :stylesheets
                :initform nil
                :type list)))

(defclass svg-1.1-toplevel (svg-toplevel)
  ()
  (:default-initargs
     :name "svg"
     :xml-header "<?xml version=\"1.0\" standalone=\"no\"?>"
     :doctype "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" 
  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
     :attributes (list :version "1.1" :id "toplevel"
                       "xmlns" "http://www.w3.org/2000/svg"
                       "xmlns:xlink" "http://www.w3.org/1999/xlink")))

(defun make-svg-toplevel (class &rest attributes)
  (let ((svg (make-instance class)))
    ;; Merge with, don't clobber, <svg /> attributes.
    (loop for props on attributes by #'cddr
          do (push-attribute svg (first props) (second props)))
    svg))

(defgeneric add-defs-element (svg-toplevel element))

(defmethod add-defs-element ((svg svg-toplevel) e)
  (add-element (svg-defs svg) e))

(defgeneric add-stylesheet (svg-toplevel url))

(defmethod add-stylesheet ((svg svg-toplevel) url)
  (with-slots (stylesheets) svg
    (setf stylesheets (append stylesheets (cons url ())))))

;;; This aims for more civilized and readable output.
(defmethod stream-out (s (e svg-toplevel))
  (format s "~A~&" (slot-value e 'xml-header))
  (dolist (css (slot-value e 'stylesheets))
    (format s "<?xml-stylesheet href=\"~A\" type=\"text/css\"?>~&" css))
  (format s "~A~&" (slot-value e 'doctype))
  (with-xml-group-element (s (element-name e) (element-attributes e))
    (when (has-contents-p (svg-defs e))
      (stream-out s (svg-defs e)))
    (loop for element across (element-contents e)
          do (stream-out s element))))

(defmacro with-svg-to-file ((svg &rest svg-attributes)
                            (filename &rest open-options)
                            &body body)
  (let ((stream (gensym "stream")))
    `(let ((,svg (make-svg-toplevel ,@svg-attributes)))
       ,@body
       (with-open-file (,stream ,filename :direction :output ,@open-options)
         (stream-out ,stream ,svg)))))


;;; Conditionally check for shape attributes and create a shape element.
;;; See the comment for *CHECK-REQUIRED-ATTRIBUTES* above.
(defgeneric assert-required-attributes (element attribute-list))
(defgeneric make-svg-element (element-name attributes))

(defmacro define-element-maker (element name required-attributes)
  `(progn
     (defmethod assert-required-attributes ((element (eql ,element)) attributes)
       (declare (ignore element))
       (unless (evenp (length attributes))
         (warn "attribute list may be missing data: ~A" attributes))
       (let ((ok t)
             (missing ()))
         (dolist (required ,required-attributes (values ok missing))
           (unless (member required attributes)
             (setf ok nil)
             (push required missing)))
         (unless ok
           (error 'missing-attributes :message (format nil "~A missing attributes: ~{~A~^ ~}" ,element missing)))))
     (defmethod make-svg-element ((element (eql ,element)) attributes)
       (when *check-required-attributes*
         (assert-required-attributes element attributes))
       (make-instance 'svg-element :name ,name :attributes attributes))))

(define-element-maker :line "line" '(:x1 :y1 :x2 :y2))
(define-element-maker :rect "rect" '(:x :y :height :width))
(define-element-maker :polyline "polyline" '(:points))
(define-element-maker :polygon "polygon" '(:points))
(define-element-maker :ellipse "ellipse" '(:cx :cy :rx :ry))
(define-element-maker :circle "circle" '(:cx :cy :r))
(define-element-maker :path "path" '(:d))
(define-element-maker :image "image" '(:x :y :height :width :xlink-href))
(define-element-maker :use "use" '(:xlink-href))

;;; The separation of PARAMS and OPTS has no representation in the 
;;; SVG-ELEMENT class, but provides a visual clue about required
;;; attributes.  However, the idiom is used in other elements where
;;; the separation *does* matter (see the gradients below).
;;; Returns the element drawn, mostly to accomodoate TRANSFORM, though
;;; you can add DESC or TITLE to shape elements, too.
(defmacro draw (scene (shape &rest params) &rest opts)
  (let ((element (gensym)))
    `(let ((,element (funcall #'make-svg-element ,shape (append (list ,@params) (list ,@opts)))))
       (add-element ,scene ,element)
       ,element)))

(defun draw* (&rest x)
  (declare (ignore x))
  (error "DRAW* is only available within group definition macros."))

(defun desc (scene text)
  "add a description to any SVG element"
  (let ((title-element (make-instance 'svg-element :name "desc")))
    (add-element title-element text)
    (add-element scene title-element)))

(defun title (scene text)
  "add a title to any SVG element"
  (let ((title-element (make-instance 'svg-element :name "title")))
    (add-element title-element text)
    (add-element scene title-element)))

(defun comment (scene text)
  (add-element scene (concatenate 'string "<!-- " text " -->")))

(defun script (scene script)
  "add inline javascript to a scene"
  (let ((script-element
         (make-instance 'svg-element :name "script"
                        :attributes (list :type "text/ecmascript"))))
    (add-element script-element "<![CDATA[")
    (add-element script-element script)
    (add-element script-element "]]>")
    (add-element scene script-element)))

(defun script-link (scene link)
  (let ((script-link
         (make-instance 'svg-element :name "script"
                        :attributes (list :type "text/ecmascript"
                                          :xlink-href link))))
    (add-element scene script-link)))

(defun style (scene css)
  "add inline CSS to a scene"
  (let ((style-element
         (make-instance 'svg-element :name "style" :type "text/css")))
    (add-element style-element css)
    (add-element scene style-element)))

;;; Grouping elements.  Many of the grouping elements have similar
;;; defining semantics: create the group, stuff in components, add
;;; to the main canvas.  The created groups can be bound to variables
;;; so that other elements can refer to them (via XLINK-HREF), or
;;; can be created empty and built up later.  In the first case, since
;;; no canvas binding is yet known to the programmer, DRAW* is available
;;; in place of DRAW.
(defmacro define-defs-group-maker (macro-name element-name)
  `(defmacro ,macro-name (scene (&rest opts) &body shapes)
     (let ((group (gensym "group")))
       `(let ((,group (make-svg-element ,',element-name (list ,@opts))))
          (macrolet ((draw* (&rest args)
                       `(draw ,',group ,@args)))
            (progn
              ,@shapes)
            (add-defs-element ,scene ,group)
            ,group)))))

;;; canned visual elements for multiple uses
(define-element-maker :symbol "symbol" '(:id))
(define-defs-group-maker make-svg-symbol :symbol)

;;; line, polyline and path markers
(define-element-maker :marker "marker" '(:id))
(define-defs-group-maker make-marker :marker)

;;; fill patterns
(define-element-maker :pattern "pattern" '(:id))
(define-defs-group-maker make-pattern :pattern)

;;; alpha masks
(define-element-maker :mask "mask" '(:id))
(define-defs-group-maker make-mask :mask)


;;; Inline groups - these can go anywhere, not just <defs/>.
(defmacro define-toplevel-group-maker (macro-name element-name)
  `(defmacro ,macro-name (scene (&rest opts) &body shapes)
     (let ((group (gensym "group")))
       `(let ((,group (make-svg-element ,',element-name (list ,@opts))))
          (macrolet ((draw* (&rest args)
                       `(draw ,',group ,@args)))
            (progn
              ,@shapes)
            (add-element ,scene ,group)
            ,group)))))

(define-element-maker :group "g" '())
(define-toplevel-group-maker make-group :group)

;;; clicky-clicky
(define-element-maker :link "a" '(:xlink-href))
(define-toplevel-group-maker link :link)

;;; Useful, if not entirely portable.
(define-element-maker :foreign-object "foreignObject" '(:x :y :height :width))
(define-toplevel-group-maker make-foreign-object :foreign-object)


;;; For text elements - TSPAN just spits out a string rather than insert
;;; itself into the current scene, to match the regular contents of TEXT.
;;; This mangles XML pretty-printing somewhat.
(defun compose-tspan (opts text)
  (with-output-to-string (s)
    (with-indentation
      (with-xml-group-element (s "tspan" opts)
        (string->xml s text)))))

(defmacro tspan ((&rest opts) text)
  `(compose-tspan (list ,@opts) ,text))

(define-element-maker :text "text" '(:x :y))

(defmacro text (scene (&rest opts) &body elements)
  (let ((group (gensym "group")))
    `(let ((,group (make-svg-element :text (list ,@opts))))
       (add-element ,scene ,group)
       (dolist (element (list ,@elements))
         (add-element ,group element))
       ,group)))


;;; Gradients.
(defun gradient-stop (&key color offset (opacity 1.0))
  (apply #'make-instance
         (cons 'svg-element 
               (list :name "stop"
                     :attributes (list :stop-color color
                                       :stop-opacity opacity
                                       :offset offset)))))

;;; Within the body of a gradient definition STOP is available as an
;;; abbreviation for GRADIENT-STOP.
(defmacro define-gradient-maker (macro-name element-name)
  `(defmacro ,macro-name (scene (&rest opts) &body stops)
     (let ((grad (gensym "gradient")))
       `(let ((,grad (funcall #'make-svg-element ,',element-name (list ,@opts))))
          (macrolet ((stop (&rest args)
                       `(gradient-stop ,@args)))
            (dolist (stop (list ,@stops))
              (add-element ,grad stop))
            (add-defs-element ,scene ,grad)
            ,grad)))))

(define-element-maker :linear-gradient "linearGradient" '(:id :x1 :y1 :x2 :y2))
(define-gradient-maker make-linear-gradient :linear-gradient)

(define-element-maker :radial-gradient "radialGradient" '(:id :cx :cy :r))
(define-gradient-maker make-radial-gradient :radial-gradient)


;;; Transformations.  The 'transform' attribute string can contain as many
;;; of these as you like, including repeats which are combined (i.e., another
;;; rotate(d,x,y) doesn't override previous rotates, but is just tacked onto
;;; the accumulating transformations).
;;; http://www.w3.org/TR/SVG11/coords.html#TransformAttribute
(defun scale (sx &optional sy)
  (if sy
      (format nil "scale(~A, ~A)" sx sy)
      (format nil "scale(~A)" sx)))

(defun translate (tx &optional ty)
  (if ty
      (format nil "translate(~A, ~A)" tx ty)
      (format nil "translate(~A)" tx)))

(defun rotate (angle &optional (cx 0) (cy 0))
  (format nil "rotate(~A, ~A, ~A)" angle cx cy))

(defun skew-x (angle)
  (format nil "skewX(~A)" angle))

(defun skew-y (angle)
  (format nil "skewY(~A)" angle))

(defun matrix (a b c d e f)
  (format nil "matrix(~A,~A,~A,~A,~A,~A)" a b c d e f))

;;; Will accept a single transformation simply or nested ones:
;;; (TRANSFORM (scale 3) (draw ...))     or
;;; (TRANSFORM ((scale 4) (rotate 90)) (draw ...))
(defmacro transform ((&rest transformations) &body element)
  (if (atom (first transformations))
      `(add-transform ,@element ,transformations)
      (let ((trans (gensym))
            (e (gensym)))
        `(let ((,e ,@element))
           (dolist (,trans (list ,@transformations) ,e)
             (add-transform ,e ,trans))))))


;;; svg.lisp ends here
