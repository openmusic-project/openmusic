;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/defsys.lisp,v 1.15.5.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.


(in-package "CL-USER")

(defsystem "OPENGL-EXAMPLES"
  ()
  :members
  (("OPENGL" :type :system :root-module nil)
   "arrows"
   "icosahedron"
   "texture"
   "3d-text")
  :rules
  ((:in-order-to :compile :all (:requires (:load "OPENGL")))
   (:in-order-to :compile "icosahedron" (:requires (:load "arrows")))
   ))

