;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/defsys.lisp,v 1.15.12.1 2011/08/24 13:27:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


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

