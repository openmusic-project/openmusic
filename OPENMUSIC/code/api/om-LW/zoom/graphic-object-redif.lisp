(in-package :oa)

(defclass om-graphic-object ()
  ((vcontainer :initform nil :initarg :vcontainer :accessor vcontainer)
   (vsubviews :initform nil :initarg :vsubviews :accessor vsubviews)
   (locked :initform nil :initarg :locked :accessor locked)
   (vx :initform 0 :initarg :vx :accessor vx)
   (vy :initform 0 :initarg :vy :accessor vy)
   (vw :initform 32 :initarg :vw :accessor vw)
   (vh :initform 32 :initarg :vh :accessor vh)
   (help-spec :initform nil :initarg :help-spec :accessor help-spec)
   (initialized-p :initform nil :accessor initialized-p)
   (highlight :initform nil :accessor highlight)
   (temp-data :initform nil :accessor temp-data)
   (images :initarg :images :accessor images :initform nil)
   ;; ZOOM-CTX: per-frame logical snapshots, set lazily on first zoom.
   (om-zoom-logical-pos  :initform nil :accessor om-zoom-logical-pos)
   (om-zoom-logical-size :initform nil :accessor om-zoom-logical-size)
   (om-zoom-logical-font :initform nil :accessor om-zoom-logical-font))
  (:default-initargs 
   :color-mode :aqua
   :create-callback 'om-create-callback
   :geometry-change-callback 'om-resize-callback
   :destroy-callback 'om-destroy-callback
   :input-model '(
		  (:post-menu om-context-menu-callback)
                   
		  ((:button-1 :motion :shift #+macosx :hyper #-macosx :control :meta)  om-clic-motion-callback (:shift :alt :cmd))
		  ((:button-1 :motion :shift #+macosx :hyper #-macosx :control)  om-clic-motion-callback (:shift :cmd))
		  ((:button-1 :motion :shift :meta)  om-clic-motion-callback (:shift :alt))
		  ((:button-1 :motion :meta #+macosx :hyper #-macosx :control)  om-clic-motion-callback (:alt :cmd))
		  ((:button-1 :motion :shift)  om-clic-motion-callback (:shift))
		  ((:button-1 :motion #+macosx :hyper #-macosx :control)  om-clic-motion-callback (:cmd))
		  ((:button-1 :motion :meta)  om-clic-motion-callback (:alt))
		  ((:button-1 :motion)  om-clic-motion-callback nil)
                   
		  ((:button-1 :press :shift #+macosx :hyper #-macosx :control :meta) om-clic-callback (:shift :alt :cmd))
		  ((:button-1 :press :shift #+macosx :hyper #-macosx :control) om-clic-callback (:shift :cmd))
		  ((:button-1 :press :shift :meta) om-clic-callback (:shift :alt))
		  ((:button-1 :press :meta #+macosx :hyper #-macosx :control) om-clic-callback (:alt :cmd))
		  ((:button-1 :press :shift) om-clic-callback (:shift))
		  ((:button-1 :press #+macosx :hyper #-macosx :control) om-clic-callback (:cmd))
		  ((:button-1 :press :meta) om-clic-callback (:alt))
		  ((:button-1 :press) om-clic-callback nil)
                   
                   
		  ((:motion :shift #+macosx :hyper #-macosx :control) om-motion-callback (:shift :cmd))
		  ((:motion :shift) om-motion-callback (:shift))
		  ((:motion #+macosx :hyper #-macosx :control) om-motion-callback (:cmd))
		  (:motion om-motion-callback nil)
                   
		  ((:button-1 :release :shift #+macosx :hyper #-macosx :control :meta)  om-clic-release-callback (:shift :alt :cmd))
		  ((:button-1 :release :shift #+macosx :hyper #-macosx :control)  om-clic-release-callback (:shift :cmd))
		  ((:button-1 :release :shift :meta)  om-clic-release-callback (:shift :alt))
		  ((:button-1 :release :meta #+macosx :hyper #-macosx :control)  om-clic-release-callback (:alt :cmd))
		  ((:button-1 :release :shift)  om-clic-release-callback (:shift))
		  ((:button-1 :release #+macosx :hyper #-macosx :control)  om-clic-release-callback (:cmd))
		  ((:button-1 :release :meta)  om-clic-release-callback (:alt))
		  ((:button-1 :release)  om-clic-release-callback nil)
                   
		  ;; test
		  ((:button-3 :release)  om-clic-release-callback nil)
                   
		  ((:button-1 :second-press :shift #+macosx :hyper #-macosx :control :meta) om-double-clic-callback (:shift :alt :cmd))
		  ((:button-1 :second-press :shift #+macosx :hyper #-macosx :control) om-double-clic-callback (:shift :cmd))
		  ((:button-1 :second-press :shift :meta) om-double-clic-callback (:shift :alt ))
		  ((:button-1 :second-press :meta #+macosx :hyper #-macosx :control) om-double-clic-callback (:alt :cmd))
		  ((:button-1 :second-press :shift ) om-double-clic-callback (:shift))
		  ((:button-1 :second-press #+macosx :hyper #-macosx :control) om-double-clic-callback (:cmd))
		  ((:button-1 :second-press :meta) om-double-clic-callback (:alt))
		  ((:button-1 :second-press) om-double-clic-callback nil)
                  
                  ;((:button-3 :press) om-right-clic-callback)
                   
		  (:gesture-spec om-char-spec-callback)
                  ;(:character  om-char-callback nil)

                  ;; ZOOM-INPUT: touch gesture bindings (Cocoa/Win32 only).
                  ((:touch :zoom)   om-zoom-touch-handler)
                  #+win32 ((:touch :rotate) om-shift-wheel-hscroll-handler)
                  ((:touch :swipe)  om-zoom-touch-swipe-handler)
                  #+win32 ((:touch :pan) om-zoom-touch-pan-handler)
		  )
   ))