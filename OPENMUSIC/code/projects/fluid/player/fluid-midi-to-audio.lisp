;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; Author: Karim Haddad

(in-package :om)


(defmethod* objfromobjs ((self midifile) (type sound))
  (objfromobjs (midiaudio self) type))

;For later
#|
(defmethod* objfromobjs ((self chord-seq) (type sound))
  (let* ((file (format nil "~Atemp.mid"
                       (namestring *om-outfiles-folder*)))
         (midifile (save-as-midi self file)))
    (prog1
        (objfromobjs (midiaudio file) type)
      (oa::om-delete-file (pathname file)))))

(defmethod* objfromobjs ((self voice) (type sound))
  (let* ((file (format nil "~Atemp.mid"
                       (namestring *om-outfiles-folder*)))
         (midifile (save-as-midi self file)))
    (prog1
        (objfromobjs (midiaudio file) type)
      (oa::om-delete-file (pathname file)))))
|#

(in-package :cl-fluid)

;let's keep sf2 also optional if someone wants to play around!

(defmethod om::midiaudio ((self om::midifile) &optional (sf2 nil) (path nil))
  (let* ((pathname (or path (om::om-choose-new-file-dialog :directory (om::def-save-directory))))
         (midifilename (namestring (om::midifilename self)))
         (fl-synths *fl-synths*)
         (loadedsf2 (sf2path (car *fl-synths*)));here we take only the first synth and return its sf2
         )
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      (setf *fluidsynth-settings* (new_fluid_settings))
      (fluid_settings_setstr *fluidsynth-settings* "audio.file.name" (namestring pathname))
      (fluid_settings_setstr *fluidsynth-settings* "player.timing-source" "sample")
      (fluid_settings_setint *fluidsynth-settings*  "synth.lock-memory"  0)
;create synth
      (setf *temp-synth* (new_fluid_synth *fluidsynth-settings*))
; load sf2 into synth
      (fluid-load-new-soundfont *temp-synth* (if sf2 sf2 loadedsf2))
;create player
      (setf *player* (new_fluid_player *temp-synth*))
      (fluid_player_add *player* midifilename)
      (fluid_player_play *player*)
      (setf *renderer* (new_fluid_file_renderer *temp-synth*))
      (setf *run-conv* t)
      (loop while (and (equal (fluid_player_get_status *player*) 1) *run-conv*)
            do (if (not (equal (fluid_file_renderer_process_block *renderer*)  FLUID_OK))
                   (setf *run-conv* nil)
                 ;(print "*")
                 )
               )
      (fluid_player_stop *player*)
      (fluid_player_join *player*)

      (delete_fluid_file_renderer *renderer*)
      (delete_fluid_player *player*)
      (delete_fluid_synth *temp-synth*)
      (delete_fluid_settings *fluidsynth-settings*)
      )
   pathname))

;For later
#|
(defmethod om::midiaudio ((self string) &optional (sf2 nil) (path nil))
  (let* ((pathname (or path (om::om-choose-new-file-dialog :directory (om::def-save-directory))))
         (fl-synths *fl-synths*)
         (loadedsf2 (sf2path (car *fl-synths*)));here we take only the first synth and return its sf2
         )
    ;(print (list "test" self))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      (setf *fluidsynth-settings* (new_fluid_settings))
      (fluid_settings_setstr *fluidsynth-settings* "audio.file.name" (namestring pathname))
      (fluid_settings_setstr *fluidsynth-settings* "player.timing-source" "sample")
      (fluid_settings_setint *fluidsynth-settings*  "synth.lock-memory"  0)
;create synth
      (setf *temp-synth* (new_fluid_synth *fluidsynth-settings*))
; load sf2 into synth
      (fluid-load-new-soundfont *temp-synth* (if sf2 sf2 loadedsf2))
;create player
      (setf *player* (new_fluid_player *temp-synth*))
      (fluid_player_add *player* self)
      (fluid_player_play *player*)

      (setf *renderer* (new_fluid_file_renderer *temp-synth*))
      (setf *run-conv* t)
      (loop while (and (equal (fluid_player_get_status *player*) 1) *run-conv*)
            do (if (not (equal (fluid_file_renderer_process_block *renderer*)  FLUID_OK))
                   (setf *run-conv* nil)
                 ;(print "*")
                 )
               )
      (fluid_player_stop *player*)
      (fluid_player_join *player*)

      (delete_fluid_file_renderer *renderer*)
      (delete_fluid_player *player*)
      (delete_fluid_synth *temp-synth*)
      (delete_fluid_settings *fluidsynth-settings*)
      )
   pathname))


(defmethod om::midiaudio ((self om::chord-seq) &optional (sf2 nil) (path nil))
  (let* ((pathname (or path (om::om-choose-new-file-dialog :directory (om::def-save-directory))))
         (file (format nil "~Atemp.mid"
                       (namestring om::*om-outfiles-folder*)))
         (midifilename (om::save-as-midi self file :approx 4))
         ;(midifilename (namestring (om::midifilename self)))
         ;(fl-synths *fl-synths*)
         (fs-synth (synthptr (car *fl-synths*)))
         (fs-settings (settings (car *fl-synths*)))
         (loadedsf2 (sf2path (car *fl-synths*)));here we take only the first synth and return its sf2
         )
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      (setf *fluidsynth-settings* (new_fluid_settings))
      (fluid_settings_setstr *fluidsynth-settings* "audio.file.name" (namestring pathname))
      (fluid_settings_setstr *fluidsynth-settings* "player.timing-source" "sample")
      (fluid_settings_setint *fluidsynth-settings*  "synth.lock-memory"  0)
;create synth
      (setf *temp-synth* (new_fluid_synth *fluidsynth-settings*))
; load sf2 into synth
      (fluid-load-new-soundfont *temp-synth* (if sf2 sf2 loadedsf2))
;create player
      (setf *player* (new_fluid_player *temp-synth*))
      (fluid_player_add *player* (namestring midifilename))
      (fluid_player_play *player*)
      (setf *renderer* (new_fluid_file_renderer *temp-synth*))
      (setf *run-conv* t)
      (loop while (and (equal (fluid_player_get_status *player*) 1) *run-conv*)
            do (if (not (equal (fluid_file_renderer_process_block *renderer*)  FLUID_OK))
                   (setf *run-conv* nil)
                 ;(print "*")
                 )
               )
      (fluid_player_stop *player*)
      (fluid_player_join *player*)

      (delete_fluid_file_renderer *renderer*)
      (delete_fluid_player *player*)
      (delete_fluid_synth *temp-synth*)
      (delete_fluid_settings *fluidsynth-settings*)
      )
   pathname))
|#