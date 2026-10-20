;;; LispWorks FLI (Foreign Language Interface) compatibility package for SBCL,
;;; backed entirely by CFFI, so that sources written against fli: run unchanged.
;;;
;;; Design note -- TYPED POINTERS. A LispWorks FLI pointer is a typed object:
;;; fli:dereference with no :type reads according to the type stored IN the
;;; pointer (LW manual fli-functions-28.htm:70), and fli:pointer-element-type
;;; (fli-functions-63.htm) returns it. A CFFI pointer is a bare address with no
;;; type. To reproduce LW semantics WITHOUT wrapping pointers (they must remain
;;; raw CFFI pointers so they pass transparently into the cffi:defcfun C calls
;;; that consume them), each pointer's element type is recorded at allocation in
;;; *pointer-element-types* (keyed by address) and looked up on a type-less
;;; dereference. Allocators outside this package feed it via register-pointer-type.

(defpackage "FLI"
  (:use :common-lisp)
  (:export #:register-module
           #:dereference
           #:allocate-foreign-object #:malloc
           #:free-foreign-object #:free
           #:make-pointer #:null-pointer-p
           #:pointer-element-type
           #:with-dynamic-lisp-array-pointer
           #:cffi-type
           #:register-pointer-type
           #:pointer-type
           #:unregister-pointer-type))

(in-package "FLI")

;;; Element-type registry. Synchronized: foreign buffers may be processed in
;;; worker threads.

(defvar *pointer-element-types* (make-hash-table :test 'eql :synchronized t)
  "Pointer address (integer) -> CFFI element type, for pointers allocated here or registered.")

(defun register-pointer-type (pointer cffi-type)
  "Record CFFI-TYPE as the element type of POINTER and return POINTER."
  (when (and pointer (not (cffi:null-pointer-p pointer)))
    (setf (gethash (cffi:pointer-address pointer) *pointer-element-types*) cffi-type))
  pointer)

(defun pointer-type (pointer)
  "Return the registered element type of POINTER, or NIL."
  (and pointer (not (cffi:null-pointer-p pointer))
       (gethash (cffi:pointer-address pointer) *pointer-element-types*)))

(defun unregister-pointer-type (pointer)
  "Forget the registered element type of POINTER."
  (when (and pointer (not (cffi:null-pointer-p pointer)))
    (remhash (cffi:pointer-address pointer) *pointer-element-types*)))

;;; FLI scalar type -> CFFI type. Buffers use :int/:float/:double/:pointer;
;;; :byte maps to :uint8. Aggregate/compound types (e.g. (:unsigned :byte)) only
;;; ever reach with-dynamic-lisp-array-pointer, which passes the pointer straight
;;; to C and never mem-arefs it, so they need no map.
(defun cffi-type (type)
  (case type
    ((:byte :unsigned-byte) :uint8)
    ((:signed-byte) :int8)
    ((:char) :char)
    ((:short) :short)
    ((:int) :int)
    ((:long) :long)
    ((:float :single-float) :float)
    ((:double :double-float) :double)
    ((:pointer) :pointer)
    (otherwise type)))

;;; The pointer's element type: its registered type, else single-float. LW's own
;;; pointer-element-type is not reliable for every pointer, so code needing
;;; certainty threads the type explicitly; :float is the safe fallback for
;;; foreign pointers not allocated here.
(defun %elt-type (pointer)
  (or (pointer-type pointer) :float))

;;; register-module NAME &key real-name file-name connection-style lifetime ...
;;;   LW manual fli-functions-70.htm: connects a foreign module (DLL/shared lib)
;;;   named by :real-name (a full path) or :file-name, else NAME.
;;;   -> cffi:load-foreign-library. The LW loader keys (:connection-style,
;;;   :lifetime) have no CFFI analogue and are ignored.
(defun register-module (name &key real-name file-name &allow-other-keys)
  (cffi:load-foreign-library (namestring (or real-name file-name name))))

;;; dereference POINTER &key index type copy-foreign-object => value  (Accessor)
;;;   LW manual fli-functions-28.htm. With :type, use it (overriding the
;;;   pointer's own type); without, use the pointer's registered element type.
;;;   Signals on a null pointer (fli-functions-28.htm:76).
(defun dereference (pointer &key type (index 0) &allow-other-keys)
  (when (cffi:null-pointer-p pointer)
    (error "FLI:DEREFERENCE of a null pointer"))
  (cffi:mem-aref pointer (cffi-type (or type (%elt-type pointer))) index))

(defun (setf dereference) (value pointer &key type (index 0) &allow-other-keys)
  (when (cffi:null-pointer-p pointer)
    (error "FLI:DEREFERENCE of a null pointer"))
  (setf (cffi:mem-aref pointer (cffi-type (or type (%elt-type pointer))) index)
        value))

;;; allocate-foreign-object &key type pointer-type initial-element
;;;                              initial-contents fill nelems ... => pointer
;;;   LW manual fli-functions-4.htm. Allocates NELEMS (default 1) of TYPE in the
;;;   C heap (LW :static default -> cffi:foreign-alloc), records the element
;;;   type, and returns the pointer. :fill sets every byte to an integer 0-255.
(defun allocate-foreign-object (&key type pointer-type (nelems 1)
                                     initial-element initial-contents fill
                                     &allow-other-keys)
  (let* ((ctype (cffi-type (or type pointer-type :float)))
         (ptr (cond
                (initial-contents
                 (cffi:foreign-alloc ctype :count nelems
                                           :initial-contents initial-contents))
                (initial-element
                 (cffi:foreign-alloc ctype :count nelems
                                           :initial-element initial-element))
                (t (cffi:foreign-alloc ctype :count nelems)))))
    (when fill
      (loop with nbytes = (* nelems (cffi:foreign-type-size ctype))
            for i from 0 below nbytes
            do (setf (cffi:mem-ref ptr :uint8 i) fill)))
    (register-pointer-type ptr ctype)))

(defun malloc (&rest args) (apply #'allocate-foreign-object args))

;;; free-foreign-object POINTER => null-pointer   LW manual fli-functions-49.htm.
;;;   No action on a null pointer; drops the pointer from the type registry.
(defun free-foreign-object (pointer)
  (when (and pointer (not (cffi:null-pointer-p pointer)))
    (unregister-pointer-type pointer)
    (cffi:foreign-free pointer))
  (cffi:null-pointer))

(defun free (pointer) (free-foreign-object pointer))

;;; make-pointer &key address type pointer-type symbol-name ... => pointer
;;;   LW manual fli-functions-57.htm. With :symbol-name, the address of a C entry
;;;   point (a null pointer if undefined, which feeds a null-pointer-p check).
;;;   :address builds a pointer to a raw address.
(defun make-pointer (&key address type symbol-name &allow-other-keys)
  (let ((ptr (cond (symbol-name
                    (or (cffi:foreign-symbol-pointer
                         (if (stringp symbol-name) symbol-name (string symbol-name)))
                        (cffi:null-pointer)))
                   (address (cffi:make-pointer address))
                   (t (cffi:null-pointer)))))
    (when (and type (not (cffi:null-pointer-p ptr)))
      (register-pointer-type ptr (cffi-type type)))
    ptr))

(defun null-pointer-p (pointer)
  (or (null pointer) (cffi:null-pointer-p pointer)))

;;; pointer-element-type POINTER-OR-TYPE => type   LW manual fli-functions-63.htm.
;;;   Reliable here for any pointer allocated here (falls back to :float otherwise).
(defun pointer-element-type (pointer)
  (%elt-type pointer))

;;; %array-element-bytes ARRAY => size-in-bytes of one element, from the array's
;;; own element-type. Used only to offset a :start index; needs no :type argument.
(defun %array-element-bytes (array)
  (let ((et (array-element-type array)))
    (cond ((subtypep et 'double-float) 8)
          ((subtypep et 'single-float) 4)
          ((subtypep et '(unsigned-byte 8)) 1)
          ((subtypep et '(signed-byte 8)) 1)
          ((subtypep et '(unsigned-byte 16)) 2)
          ((subtypep et '(signed-byte 16)) 2)
          ((subtypep et '(unsigned-byte 32)) 4)
          ((subtypep et '(signed-byte 32)) 4)
          ((subtypep et '(unsigned-byte 64)) 8)
          ((subtypep et '(signed-byte 64)) 8)
          (t 1))))

;;; with-dynamic-lisp-array-pointer (VAR ARRAY &key start type) &body body
;;;   LW manual fli-functions-82.htm. Bind VAR to a dynamic-extent foreign pointer
;;;   into ARRAY's data (no copy) for the extent of BODY. On SBCL we pin ARRAY and
;;;   take the SAP of its backing (specialized) storage vector; the element size
;;;   for a :start offset comes from the array itself, so :type is not needed
;;;   (BODY passes VAR to C, never Lisp-dereferences it). The pointer is valid
;;;   only within BODY (ARRAY is pinned only here).
(defmacro with-dynamic-lisp-array-pointer ((pointer-var array &key (start 0) type)
                                           &body body)
  (declare (ignore type))
  (let ((arr (gensym "ARR")) (vec (gensym "VEC")) (sap (gensym "SAP")) (st (gensym "ST")))
    `(let* ((,arr ,array) (,st ,start))
       (sb-sys:with-pinned-objects (,arr)
         (let* ((,vec (if (typep ,arr '(simple-array * (*)))
                          ,arr
                          (sb-ext:array-storage-vector ,arr)))
                (,sap (sb-sys:vector-sap ,vec))
                (,pointer-var (if (and ,st (plusp ,st))
                                  (cffi:inc-pointer
                                   ,sap (* ,st (fli::%array-element-bytes ,arr)))
                                  ,sap)))
           (declare (ignorable ,pointer-var))
           ,@body)))))
