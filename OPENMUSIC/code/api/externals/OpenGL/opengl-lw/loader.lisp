;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/loader.lisp,v 1.6.1.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.


(in-package "CL-USER")

;; Ensure a connection to the foreign OpenGL libraries.

(eval-when (load eval)
  #+mswindows
  (progn
    (fli:register-module "OPENGL32" :connection-style :immediate)
    (fli:register-module "GLU32" :connection-style :immediate)
    )

  #+Linux
  (progn

    ;; Ideally libGL.so and libGLU.so would be loaded directly, but
    ;; there might be problems most likely caused by libGLU.so not having
    ;; an SO_NEEDED dependency on libGL.so.  As a workaround, create a
    ;; combined file beforehand as follows:
    ;;
    ;; $ cd /tmp; ld -shared -o gl.so -L/usr/X11R6/lib -lGLU -lGL -lm
    ;;  or
    ;; $ cd /tmp; ld -shared -o gl64.so -L/usr/X11R6/lib64 -lGLU -lGL -lm
    ;;
    ;; (fli:register-module #-lispworks-64bit "/tmp/gl.so"
    ;;                      #+lispworks-64bit "/tmp/gl64.so"
    ;;                      :connection-style :immediate)

    (dolist (name '("-lGL" "-lGLU"))
      (fli:register-module name))
    )

  #+Darwin
  (cond ((member :cocoa *features*)
         (let ((root "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/"))
           (fli:register-module (merge-pathnames "libGL.dylib" root))
           (fli:register-module (merge-pathnames "libGLU.dylib" root))))
        ((member :ffi-x11 *features*)
         (let ((root "/usr/X11R6/lib/"))
           (fli:register-module (merge-pathnames "libGL.dylib" root))
           (fli:register-module (merge-pathnames "libGLU.dylib" root)))))
  
  #+FreeBSD
  (dolist (name '("-lGL" "-lGLU"))
    (fli:register-module name))
  
  #+Solaris2
  (dolist (name '("-lGL" "-lGLU"))
    (fli:register-module name))
  
  #+HP-UX
  (link-load:read-foreign-modules "-L/opt/graphics/OpenGL/lib"
                                  "-lGL" "-lGLU")
  
  #+IRIX
  (link-load:read-foreign-modules "-lGLcore" "-lGL" "-lGLU" "-lX11")
  
  #+AIX
  (link-load:read-foreign-modules "-lGL" "-lGLU" "-lXext" "-lX11" "-lIM")

  )


