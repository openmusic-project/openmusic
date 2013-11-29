;;;;============================================================================
;;;;               Repmus.lib
;;;;
;;;; repmus library
;;;; authors: G.Assayag, C. Malherbe
;;;; Thanks to J. Fineberg, M. Malt, F. Nicolas
;;;; date: 1996
;;;; © IRCAM 1996
;;;; © IRCAM 1998 
;;;;============================================================================ 

(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *repmus-source-dir* nil)
(setf *repmus-source-dir* (append (pathname-directory *load-pathname*) (list "sources")))


(defvar *Repmus-lib-files* nil)
(setf *Repmus-lib-files* '("as2om"
                           "chords"
                           "graph"
                           "chordmap"
                           "lc1"))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(if (om-standalone-p)
    (mapc #'(lambda (file) (load (make-pathname :directory *repmus-source-dir* :name file))) *Repmus-lib-files*)
  (mapc #'(lambda (file) (compile&load (make-pathname :directory *repmus-source-dir* :name file))) *Repmus-lib-files*))

#+sdif
(if (om-standalone-p)
    (load (make-pathname :directory *repmus-source-dir* :name "as2om-sdif"))
  (compile&load (make-pathname :directory *repmus-source-dir* :name "as2om-sdif")))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '( ("Audiosculpt" nil nil (AS->OM) nil)
         ("Chords" nil nil (Autotransp Mutation map-chords Chseq->poly tie-all) nil)
         ("Cribles" nil nil (lc crible-list crible-voice eval-crible pulsemaker ) nil)
         ("Graphs" nil nil (make-graph graph-tour) nil)
         ))

;by carlos remove "rytm-pair" "crible-pitches"

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)



