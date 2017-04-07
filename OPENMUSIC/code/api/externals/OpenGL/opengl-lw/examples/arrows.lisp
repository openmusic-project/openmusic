;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/arrows.lisp,v 1.5.13.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "USER")

;;; ----------------------------------------------------------------------
;;; Load up the required generic images use #. reader syntax to embed the
;;; images within the fasl.
;;; ----------------------------------------------------------------------


(eval-when (compile eval)
  (defmacro register-button-image (pathname)
    (gp:read-external-image (merge-pathnames pathname (or #+LUCID *compile-file-pathname*
                                                          (current-pathname))))))


(defvar *down-arrow* #.(register-button-image #p"./images/down-arrow.bmp"))
(defvar *up-arrow* #.(register-button-image #p"./images/up-arrow.bmp"))
(defvar *up-disabled* #.(register-button-image #p"./images/up-disabled.bmp"))
(defvar *down-disabled* #.(register-button-image #p"./images/down-disabled.bmp"))

(setf (gp:external-image-transparent-color-index *down-arrow*) 0
      (gp:external-image-transparent-color-index *up-arrow*) 0
      (gp:external-image-transparent-color-index *up-disabled*) 0
      (gp:external-image-transparent-color-index *down-disabled*) 0)
