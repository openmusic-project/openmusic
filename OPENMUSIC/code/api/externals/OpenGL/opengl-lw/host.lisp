;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/host.lisp,v 1.4.13.1 2011/08/24 13:27:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package "USER")

(setf (logical-pathname-translations "OPENGL")
      `(("**;*" ,(merge-pathnames "**/*" (pathname-location *load-truename*)))))

