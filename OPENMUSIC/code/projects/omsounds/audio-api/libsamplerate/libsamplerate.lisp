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
;;; authors G. Assayag, C. Agon, J. Bresson
;;===========================================================================

;;; LIBSAMPLERATE BINDINGS

(in-package :cl-user)

(defpackage "LibSampleRate"
  (:nicknames "LSR")
  (:use common-lisp cffi))

(in-package :lsr)

(pushnew :libsamplerate *features*)

(defparameter SRC_SINC_BEST_QUALITY 0)
(defparameter SRC_SINC_MEDIUM_QUALITY 1)
(defparameter SRC_SINC_FASTEST 2)
(defparameter SRC_ZERO_ORDER_HOLD 3)
(defparameter SRC_LINEAR 4)

(defcstruct SRC_DATA 
  (data_in :pointer)
  (data_out :pointer)
  (input_frames :long)
  (output_frames :long)
  (input_frames_used :long)
  (output_frames_gen :long)
  (end_of_input :int)
  (src_ratio :double))

(defcstruct SRC_CB_DATA 
  (frames :long)
  (data_in :pointer))

(defcfun (src-simple "src_simple") :int 
         (src-data :pointer) 
         (converter-type :int) 
         (channels :int))

(defcfun (src-strerror "src_strerror") :string 
         (error :int))

