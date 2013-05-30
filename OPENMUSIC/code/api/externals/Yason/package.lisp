;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2012 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(defpackage :yason

  (:use :cl)

  (:export
   ;; Parser
   #:parse
   #:*parse-object-key-fn*
   #:*parse-object-as*
   #:*parse-object-as-alist* ; deprecated
   #:*parse-json-arrays-as-vectors*
   #:*parse-json-booleans-as-symbols*
   #:*parse-json-null-as-keyword*

   #:true
   #:false
   #:null

   ;; Basic encoder interface
   #:encode
   #:encode-slots
   #:encode-object
   #:encode-plist
   #:encode-alist

   #:make-json-output-stream

   ;; Streaming encoder interface
   #:with-output
   #:with-output-to-string*
   #:no-json-output-context
   #:with-array
   #:encode-array-element
   #:encode-array-elements
   #:with-object
   #:encode-object-element
   #:encode-object-elements
   #:with-object-element))
