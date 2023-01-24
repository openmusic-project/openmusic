;;;; -*- Mode: Lisp -*-

;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2012 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :cl-user)

(defpackage :yason.system
  (:use :cl :asdf))

(in-package :yason.system)

(defsystem :yason
  :name "YASON"
  :author "Hans Huebner <hans@huebner.org>"
  :version "0.6.2"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "JSON parser/encoder"
  :long-description "YASON is a Common Lisp library for encoding and
    decoding data in the JSON interchange format.  JSON is used as a
    lightweight alternative to XML.  YASON has the sole purpose of
    encoding and decoding data and does not impose any object model on
    the Common Lisp application that uses it."

  :depends-on (:alexandria :trivial-gray-streams)
  :components ((:file "package")
	       (:file "encode" :depends-on ("package"))
	       (:file "parse" :depends-on ("package"))))
