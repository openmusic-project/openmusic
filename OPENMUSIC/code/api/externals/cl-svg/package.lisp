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


(defpackage :cl-svg
  (:nicknames :svg)
  (:use :common-lisp)
  (:export #:*indent-spacing*
           #:*float-format-precision*
           #:without-attribute-check
           #:missing-attributes
           #:stream-out
           #:xlink-href
           #:svg-toplevel #:svg-1.1-toplevel
           #:make-svg-toplevel
           #:with-svg-to-file
           #:add-stylesheet
           #:add-namespace
           #:add-element
           #:add-class
           #:draw
           #:draw*
           #:desc
           #:title
           #:comment
           #:script
           #:script-link
           #:style
           #:text
           #:tspan
           #:make-svg-symbol
           #:make-marker
           #:make-pattern
           #:make-mask
           #:gradient-stop
           #:stop
           #:make-linear-gradient
           #:make-radial-gradient
           #:make-group
           #:link
           #:make-foreign-object
           ;; transformations
           #:transform
           #:scale
           #:translate
           #:rotate
           #:skew-x
           #:skew-y
           #:matrix
           ;; the many path helpers
           #:make-path
           #:with-path
           #:path
           #:move-to #:move-to-r
           #:line-to #:line-to-r
           #:horizontal-to #:horizontal-to-r
           #:vertical-to #:vertical-to-r
           #:curve-to #:curve-to-r
           #:smooth-curve-to #:smooth-curve-to-r
           #:quadratic-curve-to #:quadratic-curve-to-r
           #:smooth-quadratic-curve-to #:smooth-quadratic-curve-to-r
           #:arc-to #:arc-to-r
           #:close-path))

;;; package.lisp ends here