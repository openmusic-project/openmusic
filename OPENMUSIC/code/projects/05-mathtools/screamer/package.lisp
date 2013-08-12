;;;; Written by:
;;;;
;;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;;
;;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;;; Copyright 1993 University of Toronto. All rights reserved.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :screamer
  (:shadow :defun :multiple-value-bind :y-or-n-p :variable)
  (:use :cl)
  (:nicknames "S")
  (:export #:either
           #:fail
           #:local
           #:global
           #:for-effects
           #:multiple-value-call-nondeterministic
           #:one-value
           #:possibly?
           #:necessarily?
           #:all-values
           #:ith-value
           #:print-values
           #:nondeterministic-function?
           #:funcall-nondeterministic
           #:apply-nondeterministic
           #:unwind-trail
           #:purge
           #:unwedge-screamer
           #:local-output
           #:a-boolean
           #:an-integer
           #:an-integer-above
           #:an-integer-below
           #:an-integer-between
           #:a-member-of
           #:when-failing
           #:count-failures
           #:boolean
           #:booleanp
           #:make-variable
           #:numberpv
           #:realpv
           #:integerpv
           #:booleanpv
           #:memberv
           #:assert!
           #:known?
           #:decide
           #:=v
           #:<v
           #:<=v
           #:>v
           #:>=v
           #:/=v
           #:a-booleanv
           #:an-integerv
           #:an-integer-abovev
           #:an-integer-belowv
           #:an-integer-betweenv
           #:a-realv
           #:a-real-abovev
           #:a-real-belowv
           #:a-real-betweenv
           #:a-numberv
           #:a-member-ofv
           #:notv
           #:andv
           #:orv
           #:count-trues
           #:count-truesv
           #:+v
           #:-v
           #:*v
           #:/v
           #:minv
           #:maxv
           #:funcallv
           #:applyv
           #:equalv
           #:bound?
           #:value-of
           #:ground?
           #:apply-substitution
           #:linear-force
           #:divide-and-conquer-force
           #:static-ordering
           #:domain-size
           #:range-size
           #:reorder
           #:solution
           #:best-value
           #:template
           #:define-screamer-package
           #:*screamer-version*
           #:*dynamic-extent?*
           #:*iscream?*
           #:*minimum-shrink-ratio*
           #:*maximum-discretization-range*
           #:*strategy*))
