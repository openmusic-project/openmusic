;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg; Lowercase: Yes -*-
;;; Version: $Id$
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;; A more verbose, but easier to read, description language for SVG paths.

(in-package :cl-svg)

(defvar *previous-path-instruction* ""
  "Keeps track of the previously called path instruction - SVG will assume
a repetition of the this on its own if it keeps seeing points.")

(defvar *insert-instruction-p* t
  "Works with *PREVIOUS-PATH-INSTRUCTION* to decide if a path command
needs to be expressed.")


(defmacro with-path-instruction (instruction &body body)
  `(prog1
       (let ((*insert-instruction-p*
              (not (equal *previous-path-instruction* ,instruction))))
         ,@body)
     (setf *previous-path-instruction* ,instruction)))

(defun format-instruction (instruction &rest args)
  (with-path-instruction instruction
    (if *insert-instruction-p*
        (format nil "~A~{~A~^ ~}" instruction args)
        (format nil " ~{~A~^ ~}" args))))

(defmacro define-path-instruction-pair (name instruction (&rest args))
  (let ((draw-relative (intern (concatenate 'string (string name) "-R"))))
    `(progn
       (defun ,name (,@args)
         (format-instruction ,instruction ,@args))
       (defun ,draw-relative (,@args)
         (format-instruction ,(string-downcase `,instruction) ,@args)))))

(define-path-instruction-pair move-to "M" (x y))
(define-path-instruction-pair line-to "L" (x y))
(define-path-instruction-pair horizontal-to "H" (x))
(define-path-instruction-pair vertical-to "V" (y))

(define-path-instruction-pair curve-to "C"
  (control-x1 control-y1 control-x2 control-y2 x y))

(define-path-instruction-pair smooth-curve-to "S"
  (control-x2 control-y2 x y))

(define-path-instruction-pair quadratic-curve-to "Q"
  (control-x1 control-y1 x y))

(define-path-instruction-pair smooth-quadratic-curve-to "T" (x y))

(define-path-instruction-pair arc-to "A"
  (rx ry x-rotation large-arc-flag sweep-flag x y))

;;; This needs none of the faffing about of the other path elements.
(defun close-path ()
  (format nil "z"))


(defun make-path ()
  (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))

(defun error-unless-string (s)
  (if (not (typep s 'string))
      (error "you must use only path commands in WITH-PATH: ~A" s)
      s))

(defmacro with-path (path &body cmds)
  (let ((s (gensym "stream"))
        (n (gensym)))
    `(let ((*previous-path-instruction* ""))
       (flet ((assert-string (s)
                (error-unless-string s)))
         (with-output-to-string (,s ,path)
           ;; Do trivial breaking up of the path data - SVG does not have
           ;; to accept indefinitely long lines of data.
           (let ((,n 0))
             (dolist (inst (mapcar #'assert-string (list ,@cmds)) (format ,s "~&"))
               (format ,s "~@{~A~}" inst)
               (incf ,n)
               (when (= (mod ,n 10) 0)
                 (format ,s "~&")))))))))

(defmacro path (&body cmds)
  (let ((path (gensym "path")))
    `(let ((,path (make-path)))
       (with-path ,path
         ,@cmds)
       ,path)))

;;; path.lisp ends here

