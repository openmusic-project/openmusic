;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/defsys.lisp,v 1.17.3.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "USER")

(pushnew :use-fli-gl-vector sys::*features*)

(defsystem "OPENGL" 
  (:optimize ((debug 3) (safety 3)))
  :members ( "pkg"
             "constants"
             "types"
             "vectors"
             "fns"
             ("xfns" :features (or :ffi-x11 :gtk))
             ("win32" :features :win32)
             "ufns"
             "capi"
             ("gtk-lib" :features :gtk)
             ("xm-lib" :features :ffi-x11)
             ("msw-lib" :features :win32)
             ("cocoa" :features :cocoa)
   
             "loader"
             )
  :rules ((:in-order-to :load :all
           (:requires (:load :serial)))
          (:in-order-to :compile :all 
           (:caused-by (:compile :previous))
           (:requires (:load :serial))))
  )

    

