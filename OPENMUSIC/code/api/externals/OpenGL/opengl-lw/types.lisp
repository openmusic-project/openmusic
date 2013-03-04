;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/types.lisp,v 1.4.1.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")


(fli:define-c-typedef (glenum (:foreign-name "GLenum"))
  (:unsigned :int))

(fli:define-c-typedef (glboolean (:foreign-name "GLboolean"))
  (:unsigned :char))

(fli:define-c-typedef (glbitfield (:foreign-name "GLbitfield"))
  (:unsigned :int))

(fli:define-c-typedef (glbyte (:foreign-name "GLbyte"))
  (:signed :char))

(fli:define-c-typedef (glshort (:foreign-name "GLshort"))
  (:signed :short))

(fli:define-c-typedef (glint (:foreign-name "GLint"))
  (:signed :int))

(fli:define-c-typedef (glsizei (:foreign-name "GLsizei"))
  (:signed :int))

(fli:define-c-typedef (glubyte (:foreign-name "GLubyte"))
  (:unsigned :char))

(fli:define-c-typedef (glushort (:foreign-name "GLushort"))
  (:unsigned :short))

(fli:define-c-typedef (gluint (:foreign-name "GLuint"))
  (:unsigned :int))

(fli:define-c-typedef (glfloat (:foreign-name "GLfloat"))
  :float)

(fli:define-c-typedef (glclampf (:foreign-name "GLclampf"))
  :float)

(fli:define-c-typedef (gldouble (:foreign-name "GLdouble"))
  :double)

(fli:define-c-typedef (glclampd (:foreign-name "GLclampd"))
  :double)

(fli:define-c-typedef (glvoid (:foreign-name "GLvoid"))
  :void)

(fli:define-c-typedef (glintptr (:foreign-name "GLintptr"))
  (:pointer-integer :long))

(fli:define-c-typedef (glsizeiptr (:foreign-name "GLsizeiptr"))
  (:pointer-integer :long))

(fli:define-c-typedef (glchar (:foreign-name "GLchar"))
  :char)

(fli:define-c-typedef glstring-return
  #+Win32 (w:lpstr :pass-by :reference)
  #-Win32 (:reference :lisp-string-array))

