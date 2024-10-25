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

;;; Fluidsynth package


(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Strings Utilities;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;put in general om utilities

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;(get-file "/home/karim/sf2listing.txt")

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;(split-by-one-space "foo     bar baz frob")

(defun split-by-eof (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Newline string :start i)
          collect (subseq string i j)
          while j))

(defun split-by-dash (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\- string :start i)
          collect (subseq string i j)
          while j))

;(mapcar #'read-from-string (split-by-dash "000-016"))

(defun join-string-list (string-list)
    "Concatenates a list of strings
and puts spaces between the elements."
    (format nil "~{~A~^ ~}" string-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod* soundfont-p ((path t)))
(defmethod* soundfont-p ((path string))
  (let ((test (cl-fluid::fluid_is_soundfont path)))
    (if (= test 1) 't)))
(defmethod* soundfont-p ((path pathname))
  (let ((test (cl-fluid::fluid_is_soundfont (namestring path))))
    (if (= test 1) 't)))


(defun format-pgms (strg)
  (let* ((eof (split-by-eof strg))
         (sep (mapcar #'split-by-one-space eof)))
    (loop for i in (butlast sep)
          collect (x-append
                   (mapcar #'read-from-string (split-by-dash (car i)))
                   (join-string-list (cdr i))))
    ))

(defmethod print-sf2-pgms (&optional (path nil))
  (let ((pathname (or path (om-choose-file-dialog)))
        (tempfile (concatenate 'string (namestring om::*om-outfiles-folder*)
                               (format nil "~S.txt" (gensym)))))
    (if (soundfont-p pathname)                      
        (progn
          #+linux (sys:call-system-showing-output 
                   (format nil "sh -c 'echo -n  \"inst 1\"| fluidsynth -q ~A |head -n -1|tail -n +4 > ~A'" 
                           (namestring pathname) tempfile)
                   :wait t
         ; :shell-type shell
                   :output-stream nil ;*om-stream* 
                   :prefix " " 
                   :show-cmd nil)
          #+macosx(sys:call-system-showing-output 
                  (format nil "zsh -l -c  'echo -n  \"inst 1\"| fluidsynth -q ~A |tail -n +4 > ~A'" 
                          (namestring pathname) tempfile)
                  :wait t
         ; :shell-type shell
                  :output-stream nil ;*om-stream* 
                  :prefix " " 
                  :show-cmd nil)
          (let ((progs (car (loop for i in (list (om-read-file (pathname tempfile)))
                                  collect (format nil "~A" i)))))
            (prog1
                ;(butlast (format-pgms progs))
              (format-pgms progs)
              (om-delete-file tempfile))
            )
          )
      (om-message-dialog (format nil "~A is not a valid soundfont" pathname)))
    ))


(defmethod* get-fsynth-info ((nth-synth number))
  :icon 924
  :indoc '("nth")
  :initvals '(0)
  :doc "Loads a soundFont file to the given <nth-synth> intance."
  (let ((synth (nth nth-synth cl-fluid::*fl-synths*)))
    (if synth
      (let((name (cl-fluid::synthname synth))
           (sfpath (cl-fluid::sf2path synth)))
    (format *om-stream* "~% synth name: ~A~% sf2 file: ~A~% Programs:~%~%" name sfpath)
    (print-sf2-pgms sfpath ))
    (om-message-dialog "No FluidSynth instance loaded!"))))


(defmethod* get-synth-channel-count ((nth-synth number))
    :icon 924
  :indoc '("nth")
  :initvals '(0)
  :doc "Returns the count of channels to the given <nth-synth> intance."
  (let ((synth (nth nth-synth cl-fluid::*fl-synths*)))
    (if synth
        (cl-fluid::fluid_synth_count_midi_channels
         (cl-fluid::synthptr synth))
    (om-message-dialog "No FluidSynth instance loaded!"))))

#|
(defmethod* get-synth-channel-count ((nth-synth t))
  :doc "Returns the count of channels to the given <nth-synth> intance."
    (if cl-fluid::*fl-synths*
        (loop for i in  cl-fluid::*fl-synths*
       collect  (cl-fluid::fluid_synth_count_midi_channels
         (cl-fluid::synthptr i)))
    (om-message-dialog "No FluidSynth instance loaded!")))
|#
