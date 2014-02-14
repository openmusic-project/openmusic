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
; BASICS
;======================

(defvar *snd-files* nil)

(setf *snd-files* 
  '(
    "basic;sound"   
    "basic;soundeditor"
    "basic;audio-mix-console"
    #+libaudiostream "basic;general-mixer"
    "basic;automations"
    #+libaudiostream "basic;sound-preferences"
    #+libsndfile "basic;om-sound-processing"
    
    ; #+libaudiostream "LAS;sound-processing"
    #+(and libaudiostream (not linux)) "LAS;las-player"


    "tools;sound-tools"
    "tools;control-tools"
    "tools;param-process"
    "tools;utils-from-chroma"

    "synth;synthesis-event"
    "synth;synthesize"

    #-linux "multi;multiplayer"
    #+linux "JACK;jack-audio-player"
    #+linux "mplayer;mplayer"
 ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *snd-files*))

;;;================= AUDIO PACKAGES ================
(defvar *audiopackage* (omNG-protect-object (omNG-make-new-package "Audio")))
(addPackage2Pack *audiopackage* *om-package-tree*)

(defvar *soundtoolspackage* (omNG-protect-object (omNG-make-new-package "Tools")))
(addPackage2Pack *soundtoolspackage* *audiopackage*)

(defvar *sndconvpackage* (omNG-protect-object (omNG-make-new-package "Conversions")))
(addPackage2Pack *sndconvpackage* *soundtoolspackage*)

(defvar *analysispackage* (omNG-protect-object (omNG-make-new-package "Inspect")))
(addPackage2Pack *analysispackage* *soundtoolspackage*)

(defvar *synthpackage* (omNG-protect-object (omNG-make-new-package "Sound Synthesis")))
(addPackage2Pack *synthpackage* *audiopackage*)

(AddClass2Pack '(sound audio-mix-console) *audiopackage*)
(AddGenFun2Pack '(adsr param-process vibrato jitter) *soundtoolspackage*)
(AddGenFun2Pack '(sound-points sound-dur sound-dur-ms) *analysispackage*)
(AddGenFun2Pack '(synthesize) *synthpackage*)
(addGenFun2Pack '(dB->lin lin->dB ms->sec sec->ms samples->sec sec->samples) *sndconvpackage*)

(defvar *sndprocpackage* (omNG-protect-object (omNG-make-new-package "Processing")))
(addPackage2Pack *sndprocpackage* *audiopackage*)
(AddGenFun2Pack '(sound-silence 
                  sound-fade sound-normalize sound-vol sound-stereo-pan
                  sound-resample sound-mono-to-stereo sound-stereo-to-mono
                  sound-cut sound-loop
                  sound-mix sound-seq     
                  save-sound) 
                *sndprocpackage*)
; record-sound


(add-ref-section (gen-ref-entries *audiopackage*))

(add-lib-alias "chroma-classes" "OMChroma")



;======================
;SDIF
;======================
(defvar *sdif-lib-files* nil)

(setf *sdif-lib-files* '("sdif;sdif-struct"
			 "sdif;sdiffile"
                         "sdif;sdifeditor"
			 "sdif;sdif-write"
			 "sdif;matrix"
			 "sdif;analyse-tools"
                         "sdif;cseq2sdif"
                         "sdif;sdifpack"
                         ))


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *sdif-lib-files*))
     




