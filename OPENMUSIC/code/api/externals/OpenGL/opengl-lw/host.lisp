;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/host.lisp,v 1.4.17.1 2024/10/04 11:59:32 martin Exp $" -*-

;; Copyright (c) 1987--2025 LispWorks Ltd. All rights reserved.


(in-package "USER")

(setf (logical-pathname-translations "OPENGL")
      `(("**;*" ,(merge-pathnames "**/*" (pathname-location *load-truename*)))))

