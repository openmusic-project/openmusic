;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)

;======================
;SDIF
;======================
(mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        '("sdif-lib;sdif"
          "sdif-lib;sdif-api"
          "sdif-om;sdif-struct"
          "sdif-om;sdiffile"
          "sdif-om;sdifeditor"
          "sdif-om;sdif-write"
          "sdif-om;matrix"
          "sdif-om;analyse-tools"
          "sdif-om;cseq2sdif"
          ))


(defvar *sdifpackage* (omNG-protect-object (omNG-make-new-package "SDIF")))
(addPackage2Pack *sdifpackage* *om-package-tree*)

(defvar *sdif-inspectpack* (omNG-protect-object (omNG-make-new-package "Read")))
(defvar *sdif-writepack* (omNG-protect-object (omNG-make-new-package "Write")))

(addPackage2Pack *sdif-inspectpack* *sdifpackage*)
(addPackage2Pack *sdif-writepack* *sdifpackage*)


(AddClass2Pack '(SDIFFile SDIFMatrix raw-SDIFmatrix SDIFFrame SDIFStream SDIFType SDIFNVT SDIF-Buffer) *sdifpackage*)

(AddGenFun2Pack  '(SDIF->text SdifInfo SDIFStreams
                   GetSdifStream
                   GetSdifData GetSDIFTimes GetSDIFChords 
                   numFrames FrameInfo MatrixInfo GetRow GetCol GetVal
                   SDIFTypeDescription GetNVTList find-in-nvtlist find-in-nvt GetSIDTable find-sid
                   sdif->bpf sdif->markers sdif->chord-seq 
                   ) *sdif-inspectpack*)

(AddGenFun2Pack  '(save-sdif-file
                   sdif-write-frame
                   sdif-write-header
                   bpf->sdif markers->sdif chord-seq->sdif
                   ) *sdif-writepack*)

(add-ref-section (gen-ref-entries *sdifpackage*))
