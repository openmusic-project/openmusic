;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/host.lisp,v 1.4.14.1 2014/05/27 20:56:56 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.


(in-package "USER")

(setf (logical-pathname-translations "OPENGL")
      `(("**;*" ,(merge-pathnames "**/*" (pathname-location *load-truename*)))))

