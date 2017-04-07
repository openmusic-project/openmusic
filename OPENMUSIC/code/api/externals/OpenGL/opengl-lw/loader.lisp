;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/loader.lisp,v 1.7.1.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.


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

  #+AIX
  (dolist (name  '("-lGL" "-lGLU"
                          "-lXext" "-lX11" "-lIM" ; Not obvious that these are needed
                          ))
    (fli:register-module name))
  
  #+HP-UX
  (link-load:read-foreign-modules "-L/opt/graphics/OpenGL/lib"
                                  "-lGL" "-lGLU")
  
  #+IRIX
  (link-load:read-foreign-modules "-lGLcore" "-lGL" "-lGLU" "-lX11")
  
  
  )


