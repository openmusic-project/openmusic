;; -*- Mode: Lisp; External-format: Latin-1; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/31/LISPcurrent-lib/RCS/private-patches:load.lisp,v 1.5.1.1 2014/05/27 20:56:34 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

;;; This is the private patch loading file for LispWorks.
;;;
;;; When you receive a private patch, it should be installed alongside this
;;; file and a call to load-one-private-patch added inside the flet below, as
;;; in the commented example.
;;;
;;; On Windows Vista and Windows 7, you should do all of that via the menu item
;;;
;;;         Help > Install Private Patches

(in-package "CL-USER")

(let ((location (pathname-location *load-pathname*)))
  (flet ((load-one-private-patch (path sequence)
           (scm:require-private-patch (merge-pathnames path location) sequence)))

    ;; Example form inside the flet to load a :system sequence patch
    ;; from the file my-patch.
    ;; (load-one-private-patch "my-patch" :system)
   (load-one-private-patch "constrain-copy" :capi)
   (load-one-private-patch "cached-display-automatic-cancel" :capi)
   (load-one-private-patch "with-port-context-current" :capi-cocoa)
   (load-one-private-patch "draw-metafile" :capi)
   (load-one-private-patch "draw-metafile-cocoa" :capi-cocoa)
   (load-one-private-patch "nsglyphstorage-removed" :cocoa)
   (load-one-private-patch "maybe-kill-dspec-table-2" :delivery)
   (load-one-private-patch "nsglyphstorage-removed" :cocoa)
   (load-one-private-patch "layout-divider-perform-move" :capi)
   (load-one-private-patch "ns-alerts" :cocoa)
   (load-one-private-patch "prompters-non-deprecated" :capi-cocoa)
   (load-one-private-patch "logical-font-gdip-compute-character-extents" :capi-win32)
   ))

