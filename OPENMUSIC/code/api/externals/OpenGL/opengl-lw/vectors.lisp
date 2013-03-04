;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/vectors.lisp,v 1.7.5.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.


(in-package "OPENGL")

;;; ----------------------------------------------------------------------
;;; OPENGL portable vector handling utilities.
;;; ----------------------------------------------------------------------
;;; DEFTYPES :
;;; 	opengl:gl-vector(&optional type length) 
;;; 	
;;; FLI type : 
;;; 	 opengl:gl-vector(&optional type length) for lisp-to-foreign conversion only.
;;; 
;;; FUNCTIONS:
;;; 	opengl:make-gl-vector(type length &key contents)
;;; 	opengl:gl-vector (type &rest contents)
;;;        Variation of make-gl-vector.
;;; 	opengl:free-gl-vector(object)
;;; 
;;; gl-vectors allocated by opengl:make-gl-vector and opengl:gl-vector need to
;;; be explicitly released by calling opengl:free-gl-vector (for consistency between
;;; platforms).
;;; 
;;; MACROS:
;;; 	opengl:gl-vector-aref(object subscript)
;;;         Accessor for the vector
;;;
;;; 	opengl:with-gl-vector(bindings
;;;                              &body body)
;;;         Vector is allocated within the scope of the BODY.
;;;         bindings is a list of (binding &key type length contents)
;;; 
;;; 
;;; Common Arguments:
;;;    'TYPE' - element type of the vector
;;;           Can be one of 
;;;                :single, :double
;;;                :signed-8, :signed-16, :signed-32
;;;                :unsigned-8, :unsigned-16, :unsigned-32
;;;    'LENGTH' - length of the vector
;;;             Can be a positive integer or '*
;;;    'CONTENTS' a list of values to write into the gl-vector.
;;; 
;;; ----------------------------------------------------------------------
;;; GL deftypes
;;; ----------------------------------------------------------------------

(deftype opengl:gl-vector (&optional type (length '*))
  #+:use-fli-gl-vector (declare (ignore type length))
  #+:use-fli-gl-vector t
  #-:use-fli-gl-vector `(simple-array ,(convert-to-lisp-type type) (,length)))

;;; ----------------------------------------------------------------------
;;; Allocation of float vectors
;;; ----------------------------------------------------------------------
;;; Foreign type converters - 8 basic types of vectors.
;;; :signed-8    :signed-16    :signed-32
;;; :unsigned-8  :unsigned-16  :unsigned-32
;;; :float :double
;;; ----------------------------------------------------------------------

(defun convert-to-fli-type (type-name)
  (ecase type-name
    (:signed-8 '(:signed :char))
    (:signed-16 '(:signed :short))
    (:signed-32 '(:signed :int))
    (:unsigned-8 '(:unsigned :char))
    (:unsigned-16 '(:unsigned :short))
    (:unsigned-32 '(:unsigned :int))
    ((:double :double-float) :lisp-double-float)
    ((:float :single-float) :lisp-single-float)))


(defun convert-to-lisp-type (type-name)
  (case type-name
    (:signed-8 '(signed-byte 8))
    (:signed-16 '(signed-byte 16))
    (:signed-32 '(signed-byte 32))
    (:unsigned-8 '(unsigned-byte 8))
    (:unsigned-16 '(unsigned-byte 16))
    (:unsigned-32 '(unsigned-byte 32))
    ((:double :double-float) 'double-float)
    ((:float :single-float) 'single-float)))

;;; ----------------------------------------------------------------------
;;; Allocation of gl vectors
;;; ----------------------------------------------------------------------
;;; This current use of auto-gc'ing foreign data is here to mimic the behavior
;;; of lisp foreign arrays.
;;; using ADD-SPECIAL-FREE-ACTION and FLAG-SPECIAL-FREE-ACTION is not normally
;;; recommended when using the FLI as
;;; 1. It can slow down the GC mechanism by adding overheads to objects to cleanup.
;;; 2. Can have disastrous results if the allocated foreign object is
;;;    not cleaned up before an image save - data allocated by
;;;    fli:allocate-foreign-object is not persistent across image saves.

;;; Add a count so that we can keep track on how many auto-gc malloc'd objects
;;; are still 'live'

#+:use-fli-gl-vector
(defvar *gl-vectors-allocated* 0)

#+:use-fli-gl-vector
(defun free-gl-vector (object)
  (when (and (fli:pointerp object)
             (not (fli:null-pointer-p object)))
    (decf *gl-vectors-allocated*)
    (fli:free-foreign-object object)))

#+:use-fli-gl-vector
(add-special-free-action 'free-gl-vector)

(defun opengl:make-gl-vector (type length &key (contents nil contentsp))
  #+:use-fli-gl-vector
  (let ((new-vector (fli:allocate-foreign-object 
               :type (convert-to-fli-type type)
               :nelems length
               :initial-contents contents)))
    (incf *gl-vectors-allocated*)
    (flag-special-free-action new-vector)
    new-vector)
  #-:use-fli-gl-vector
  (sys:in-static-area 
    (apply 'make-array length
           :element-type (convert-to-lisp-type type)
           (when contentsp `(:initial-contents ,contents)))))


;;; ------------------------------
;;; Misc gl vector constructors
;;; ------------------------------

(defun opengl:gl-vector (type &rest contents)
  (opengl:make-gl-vector type (length contents) :contents contents))

;;; ----------------------------------------------------------------------
;;; Element access of gl vectors
;;; ----------------------------------------------------------------------

(defmacro opengl:gl-vector-aref (object subscript)
  #+:use-fli-gl-vector
  `(fli:dereference ,object :index ,subscript)
  #-:use-fli-gl-vector
  `(aref ,object ,subscript))

;;; ----------------------------------------------------------------------
;;; dynamic-extent allocation of gl vectors
;;; ----------------------------------------------------------------------
#-:use-fli-gl-vector
(defvar *dynamically-allocated-float-vectors* nil
  "Holds on to dynamically scoped lisp arrays - prevent the gc from eating them (at least until the scope is left).")

(defun gl-vector-bindings (binding
                           &key (type :single-float)
                           (length 4)
                           (contents nil contentsp))
  (values
   #+:use-fli-gl-vector
   `(,binding (fli:allocate-dynamic-foreign-object
               :type ',(convert-to-fli-type type)
               ,@(when length `(:nelems ,length))
               ,@(when contentsp `(:initial-contents ,contents))))
   #-:use-fli-gl-vector
   `(,binding (sys:in-static-area 
                     (make-array ,length
                                 :element-type ',(convert-to-lisp-type type)
                                 ,@(when contentsp `(:initial-contents ,contents)))))
   binding))

      
(defmacro opengl:with-gl-vectors ((&rest bindings)
                                 &body body)
  (let (let-bindings binding-names)
    (dolist (binding bindings)
      (multiple-value-bind (let-binding binding-name)
          (apply 'gl-vector-bindings binding)
        (push let-binding let-bindings)
        (push binding-name binding-names)))
    #+:use-fli-gl-vector
    `(fli:with-dynamic-foreign-objects ()
       (let* ,let-bindings
         ,@body))
  #-:use-fli-gl-vector
  `(let* (,@let-bindings
         ,@(when binding-names
                  `((*dynamically-allocated-float-vectors* `(,,@binding-names)))))
     (declare (dynamic-extent *dynamically-allocated-float-vectors*))
    ,@body)))
  

;;; ----------------------------------------------------------------------
;;; Foreign type definition
;;; ----------------------------------------------------------------------


(fli:define-foreign-type opengl:gl-vector (&optional (type nil typep) (length '*))
  #+:use-fli-gl-vector (if type
                           `(:pointer ,(convert-to-fli-type type))
                         :pointer)
  #-:use-fli-gl-vector (if type
                           `(:lisp-array (,(convert-to-lisp-type type) ,length))
                         :lisp-array))
