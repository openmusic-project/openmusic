;; -*- Mode: Lisp; External-format: Latin-1; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/31/LISPcurrent-lib/RCS/private-patches:load.lisp,v 1.7.1.1 2017/01/19 11:50:41 martin Exp $" -*-

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.

;;; This is the private patch loading file for LispWorks.
;;;
;;; When you receive a private patch, it should be installed alongside
;;; this file and a call to load-one-private-patch added inside the flet
;;; below, immediately after the commented example.
;;;
;;; On Windows, you should do all of that via the menu item
;;;
;;;         Help > Install Private Patches

(in-package "CL-USER")

(let ((location (lw:pathname-location *load-pathname*)))
  (flet ((load-one-private-patch (path sequence)
           (scm:require-private-patch (merge-pathnames path location) sequence)))

    ;; Example form inside the flet to load a :system sequence patch
    ;; from the file my-patch.
    ;; Add your own forms immediately below this.
    ;; (load-one-private-patch "my-patch" :system)

	(load-one-private-patch "macos-version-1012" :cocoa)
	(load-one-private-patch "slider-orientation-cocoa" :capi-cocoa)
	(load-one-private-patch "docommandbyselector-quiet-noop" :capi-cocoa)
    (load-one-private-patch "select-extend" :system)

    ))

