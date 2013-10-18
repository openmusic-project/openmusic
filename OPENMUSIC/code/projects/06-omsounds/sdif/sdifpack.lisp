(in-package :om)

(defvar *sdif-library* nil)

(defun init-sdif-lib ()
  (setf *sdif-library* (om-start-sdif))
  (unless *sdif-library* 
    (om-message-dialog (format nil (om-str :lib-error) "SDIF"))))

(om-add-init-func 'init-sdif-lib)


(defvar *sdifpackage* (omNG-protect-object (omNG-make-new-package "SDIF")))
(addPackage2Pack *sdifpackage* *audiopackage*)

;(defvar *sdif-PREDEFpackage* (omNG-protect-object (omNG-make-new-package "Predefined Matrices")))
(defvar *sdif-inspectpack* (omNG-protect-object (omNG-make-new-package "Read")))
(defvar *sdif-writepack* (omNG-protect-object (omNG-make-new-package "Write")))

(addPackage2Pack *sdif-inspectpack* *sdifpackage*)
(addPackage2Pack *sdif-writepack* *sdifpackage*)
;(addPackage2Pack *sdif-PREDEFpackage* *sdifpackage*)

(AddClass2Pack '(SDIFFile SDIFMatrix raw-SDIFmatrix SDIFFrame SDIFStream SDIFType SDIFNVT SDIF-Buffer) *sdifpackage*)

(AddGenFun2Pack  '(SDIF->text SdifInfo SDIFStreams
                   GetSdifStream
                   GetSdifData GetSDIFTimes GetSDIFChords 
                   numFrames FrameInfo MatrixInfo GetRow GetCol GetVal
                   SDIFTypeDescription GetNVTList find-in-nvtlist find-in-nvt GetSIDTable
                   sdif->bpf sdif->markers sdif->chord-seq 
                   ) *sdif-inspectpack*)

(AddGenFun2Pack  '(save-sdif-file
                   sdif-write-frame
                   sdif-write-header
                   bpf->sdif markers->sdif chord-seq->sdif
                   ) *sdif-writepack*)

;(AddClass2Pack '(1STF 1DIS 1CHA 1FOF 1FQ0 1RES) *sdif-PREDEFpackage*)
;(AddGenFun2Pack  '(GetPredefinedMatrix) *sdif-PREDEFpackage*)

(add-ref-section (gen-ref-entries *sdifpackage*))

;(add-ref-section 
; '("SDIF" (("Objects" (SDIFFile SDIFMatrix SDIFFrame SDIFStream SDIFType SDIFNVT SDIF-Buffer))
;          ("Read SDIF Data" (SDIF->text SdifInfo 
;                                       GetSdifData GetSDIFTimes GetSdifStream
;                                       numFrames FrameInfo numMatrix MatrixInfo GetRow GetCol GetVal
 ;                                      SDIFTypeDescription GetNVTList find-in-nvtlist find-in-nvt
;                                       sdif->bpf sdif->markers GetSDIFChords))
;          ("Write SDIF" (save-sdif-file
;                          sdif-write-frame
;                          sdif-write-header
 ;                        bpf->sdif markers->sdif chordseq->sdif))
;          )))