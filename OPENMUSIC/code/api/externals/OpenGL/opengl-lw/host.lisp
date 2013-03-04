;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/host.lisp,v 1.4.6.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.


(in-package "USER")

(setf (logical-pathname-translations "OPENGL")
      `(("**;*" ,(merge-pathnames "**/*" (pathname-location *load-truename*)))))

