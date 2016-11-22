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





;;;========================================================
;;; FILE ACCESS (libsndfile / libsamplerate)
;;;========================================================

(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "audio-api" "libsndfile")) :name "libsndfile"))
(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "audio-api" "libsndfile")) :name "libsndfile-api"))
(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "audio-api")) :name "file-access"))

(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "audio-api" "libsamplerate")) :name "libsamplerate"))
(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "audio-api" "libsamplerate")) :name "libsamplerate-api"))

(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "audio-api" "omjuceaudiolib")) :name "omjuceaudiolib"))

(push :audio *features*)


;======================
; LOAD
;======================

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        '(
          "general;sound"   
          "general;soundeditor"
          "general;audio-mix-console"
          "general;general-mixer"
          "general;automations"
          "general;sound-preferences"
          
          "tools;sound-processing"
          "tools;sound-tools"
          "tools;control-tools"
          "tools;param-process"
          "tools;utils-from-chroma"

          "synth;synthesis-event"
          "synth;synthesize"
    
          "players;juce-player"
          #-linux "players;multiplayer"
          #+cl-jack "players;jack-audio-player"
          #+linux "players;mplayer"
          )))



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







