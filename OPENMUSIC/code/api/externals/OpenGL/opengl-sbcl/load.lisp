;;===========================================================================
;DocFile
;loads the SBCL OpenGL emulation -- the twin of opengl-lw/load.lisp.
;
;LispWorks ships a full GL 1.x binding (opengl-lw/); SBCL does not, and the
;Alloy renderer holds a 3.3 CORE profile where immediate mode does not exist
;at all.  IMMEDIATE-GL emulates the GL 1.x calls the OM 3D editors make, on
;top of that core context.  Same role, same place in the tree, same contract
;to load-opengl.lisp: after this file, opengl-api-sbcl.lisp can bridge the
;names into the OPENGL package.

;;===========================================================================

(in-package :oa)

(compile&load (current-pathname "package"))
(compile&load (current-pathname "constants"))
(compile&load (current-pathname "immediate-gl"))
