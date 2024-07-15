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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;;;
;=========================================================================
;;;
; Author: Karim Haddad
;==================================
; omicron menus
;==================================

(in-package :om)



(defparameter *edo-names* 
(list
"1"
"1/2"
"1/3"
"1/4"
"1/5"
"1/6"
"1/7"
"1/8"
"1/10"
"1/12"
"1/14"
"1/16"
;;;
"5 EDO"
"6 EDO"
"7 EDO"
"8 EDO"
"9 EDO"
"10 EDO"
"12 EDO (1/2 tones)"
"14 EDO"
"15 EDO"
"16 EDO"
"17 EDO"
"18 EDO"
"19 EDO"
"22 EDO"
"24 EDO (1/4 tones)"
"30 EDO"
"31 EDO"
"36 EDO (1/6 tones)"
"42 EDO"
"48 EDO (1/8 tones)"
"60 EDO"
"72 EDO (1/12 tones)"
"84 EDO"
"96 EDO (1/16 tones)"
))
 	 

(defparameter *edo-notation*
  (list
  (list "Whole tone scale" "Whole tone scale (sharp)")
  (list "Half tone scale")
  (list "Third tone scale" "Third tone scale (sharp)")
  (list "Quarter tone scale" )
  (list "Fifth tone scale" "Fifth tone scale (sharp)")
  (list "Sixth tone scale")
  (list "Seventh tone scale" "Seventh tone scale (sharp)")
  (list "Eight tone scale")
  (list "Tenth tone scale")
  (list "Twelve tone scale")
  (list "Fourteenth tone scale")
  (list "Sixteenth tone scale")
  ;;;;
  (list "Subset of 60 EDO, sharps only" "Subset of 60 EDO, flats only")
  (list "Subset of 12 EDO, sharps only" "Subset of 12 EDO, flats only")
  (list "Chain of fifths")
  (list "Subset of 24 EDO, sharps only" "Subset of 24 EDO, flats only")
  (list "Subset of 36 EDO, sharps only" "Subset of 36 EDO, flats only")
  (list "Subset of 60 EDO, sharps only" "Subset of 60 EDO, flats only")
  (list "Chain of fifths, sharps only" "Chain of fifths, flats only" )
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only")
  (list "Subset of 60 EDO, sharps only" "Subset of 60 EDO, flats only")
  (list "Subset of 48 EDO, sharps only" "Subset of 48 EDO, flats only")
  (list "Chain of fifths" "Chain of fifths, sharps only" "Chain of fifths, flats only")
  (list "Subset of 36 EDO, sharps only" "Subset of 36 EDO, flats only" "OM legacy")
  (list "Chain of fifths")
  (list "Chain of fifths" "Chain of fifths, sharps only" "Chain of fifths, flats only")
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only")
  (list "Subset of 60 EDO, sharps only" "Subset of 60 EDO, flats only" "OM legacy")
  (list "Chain of fifths")
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only" "OM legacy")
  (list "Chain of fifths, sharps only" "Chain of fifths, flats only" "OM legacy")
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only" "OM legacy")
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only" "OM legacy")
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only" "OM legacy")  
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only" "OM legacy")  
  (list "Offset chains of fifths, sharps only" "Offset chains of fifths, flats only" "OM legacy")  
))

(defparameter *edo-doc*
  (list 
   (list "Whole tone scale" "Whole tone scale (sharp)")
  (list "Half tone scale")
  (list "Third tone scale" "Third tone scale (sharp)")
  (list "Quarter tone scale" )
  (list "Fifth tone scale" "Fifth tone scale (sharp)")
  (list "Sixth tone scale")
  (list "Seventh tone scale" "Seventh tone scale (sharp)")
  (list "Eight tone scale")
  (list "Tenth tone scale")
  (list "Twelve tone scale")
  (list "Fourteenth tone scale")
  (list "Sixteenth tone scale")
;;;
   (list (format nil "5 equal divisions of the octave~%~%Step size = 240 cents~%~%Notated as a subset of 60 EDO. Sharps only.~%~%Example chromatic sequence: C D^^ Fv G^ A#vv C")
         (format nil "5 equal divisions of the octave~%~%Step size = 240 cents~%~%Notated as a subset of 60 EDO. Flats only.~%~%Example chromatic sequence: C D^^ Fv G^ Bbvv C"))
   (list (format nil "6 equal divisions of the octave~%~%Step size = 200 cents~%~%Notated as a subset of 12 EDO. Sharps only.~%~%Example chromatic sequence: C D E F# G# A# C")
         (format nil "6 equal divisions of the octave~%~%Step size = 200 cents~%~%Notated as a subset of 12 EDO. Flats only.~%~%Example chromatic sequence: C D E Gb Ab Bb C"))
   (list (format nil "7 equal divisions of the octave~%~%Step size = 171 cents~%~%Fifth size = 686 cents~%~%Notated as a chain of ascending fifths from F to B.~%~%Example chromatic sequence: C D E F G A B C"))
   (list (format nil "8 equal divisions of the octave~%~%Step size = 150 cents~%~%Notated as a subset of 24 EDO. Sharps only.")
         (format nil "8 equal divisions of the octave~%~%Step size = 150 cents~%~%Notated as a subset of 24 EDO. Flats only."))
   (list (format nil "9 equal divisions of the octave~%~%Step size = 133 cents~%~%Notated as a subset of 36 EDO. Sharps only.~%~%Example chromatic sequence: C C#^ D#v E F^ Gv G# A^ Bv C")
         (format nil "9 equal divisions of the octave~%~%Step size = 133 cents~%~%Notated as a subset of 36 EDO. Flats only.~%~%Example chromatic sequence: C Db^ Ebv E F^ Gv Ab A^ Bv C"))
   (list (format nil "10 equal divisions of the octave~%~%Step size = 120 cents~%~%Notated as a subset of 60 EDO.~%~%Sharps only.")
         (format nil "10 equal divisions of the octave~%~%Step size = 120 cents~%~%Notated as a subset of 60 EDO.~%~%Flats only."))
   (list (format nil "12 equal divisions of the octave~%~%Step size = 100 cents~%~%Fifth size = 700 cents~%~%Notated as a chain of ascending fifths from F to A#.~%~%Example chromatic sequence: C C# D D# E F F# G G# A A# B C")
         (format nil "12 equal divisions of the octave~%~%Step size = 100 cents~%~%Fifth size = 700 cents~%~%Notated as a chain of ascending fifths from Gb to B.~%~%Example chromatic sequence: C Db D Eb E F Gb G Ab A Bb B C"))
   (list (format nil "14 equal divisions of the octave~%~%Step size = 86 cents~%~%Fifth size = 686 cents~%~%Notated as two interlocking sets of 7 EDO offset by 1 step of 14 EDO (86 cents).~%~%Up arrows raise the pitch by 1 step.~%~%Example chromatic sequence: C C^ D D^ E E^ F F^ G G^ A A^ B B^ C")
         (format nil "14 equal divisions of the octave~%~%Step size = 86 cents~%~%Fifth size = 686 cents~%~%Notated as two interlocking sets of 7 EDO offset by 1 step of 14 EDO (86 cents).~%~%Down arrows lower the pitch by 1 step.~%~%Example chromatic sequence: C Dv D Ev E Fv F Gv G Av A Bv B Cv C"))
   (list (format nil "15 equal divisions of the octave~%~%Step size = 80 cents~%~%Notated as a subset of 60 EDO. Sharps only.")         (format nil "15 equal divisions of the octave~%~%Step size = 80 cents~%~%Notated as a subset of 60 EDO. Flats only."))
   (list (format nil "16 equal divisions of the octave~%~%Step size = 75 cents~%~%Notated as a subset of 48 EDO. Sharps only.")
         (format nil "16 equal divisions of the octave~%~%Step size = 75 cents~%~%Notated as a subset of 48 EDO. Flats only."))
   (list (format nil "17 equal divisions of the octave~%~%Step size = 71 cents~%~%Fifth size = 706 cents~%~%Notated as a chain of ascending fifths from Gb to A#.~%~%Example chromatic sequence: C Db C# D Eb D# E F...etc.")
         (format nil "17 equal divisions of the octave~%~%Step size = 71 cents~%~%Fifth size = 706 cents~%~%Notated as a chain of ascending fifths from F.~%~%Pitches beyond A# in the chain are respelled using half sharps (e.g. E# = F+).~%~%Half sharps raise the pitch by 1 step (71 cents).  Example chromatic sequence: C C+ C# D D+ D# E F...etc.")
         (format nil "17 equal divisions of the octave~%~%Step size = 71 cents~%~%Fifth size = 706 cents~%~%Notated as a chain of ascending fifths from Gb.~%~%Pitches beyond B in the chain are respelled using half flats (e.g. F# = Gd).~%~%Half flats lower the pitch by 1 step (71 cents).  Example chromatic sequence: C Db Dd D Eb Ed E F...etc."))
   (list (format nil "18 equal divisions of the octave~%~%Step size = 67 cents~%~%Notated as a subset of 36 EDO. Sharps only.")
         (format nil "18 equal divisions of the octave~%~%Step size = 67 cents~%~%Notated as a subset of 36 EDO. Flats only.")
         (format nil "18 equal divisions of the octave~%~%Step size = 67 cents~%~%Notated using OpenMusic's legacy accidentals"))
   
   (list (format nil "19 equal divisions of the octave~%~%Step size = 63 cents~%~%Fifth size = 695 cents~%~%Notated as a chain of ascending fifths from Gb to B#.~%~%Example chromatic sequence: C C# Db D D# Eb E E# F...etc."))
   (list (format nil "22 equal divisions of the octave~%~%Step size = 55 cents~%~%Fifth size = 709 cents~%~%Notated as a chain of ascending fifths from Gb.~%~%Pitches beyond A# in the chain are respelled with arrow-attached accidentals (e.g. E# = F#v).~%~%Arrows lower the pitch by 1 step (55 cents).~%~%Example chromatic sequence: C Db C#v C# D Eb D#v D# E F...etc.")
         (format nil "22 equal divisions of the octaveh~%~%Step size = 55 cents~%~%Fifth size = 709 centsh~%~%Notated as a chain of ascending fifths from F.~%~%Pitches beyond A# in the chain are notated with arrow-attached accidentals (e.g. E# = F#v).~%~%Arrows raise/lower the pitch by 1 step (55 cents).~%~%Example chromatic sequence: C C^ C#v C# D D^ D#v D# E F...etc.")
         (format nil "22 equal divisions of the octave~%~%Step size = 55 cents~%~%Fifth size = 709 cents~%~%Notated as a chain of ascending fifths from Gb.~%~%Pitches beyond B in the chain are notated with arrow-attached accidentals (e.g. F# = Gv).~%~%Arrows raise/lower the pitch by 1 step (55 cents).~%~%Example chromatic sequence: C Db Db^ Dv D Eb Eb^ Ev E F...etc."))
  
   (list (format nil "24 equal divisions of the octave~%~%Step size = 50 cents Fifth size = 700 cents~%~%Notated as two interlocking sets of 12 EDO offset by 1 step of 24 EDO (50 cents).~%~%Half sharps raise the pitch by 1 step (50 cents).~%~%Sesquisharps raise the pitch by 3 steps (150 cents).~%~%Example chromatic sequence: C C+ C# C#+ D D+ D# D#+ E E+ F...etc.")
         (format nil "24 equal divisions of the octave~%~%Step size = 50 cents Fifth size = 700 cents~%~%Notated as two interlocking sets of 12 EDO offset by 1 step of 24 EDO (50 cents).~%~%Half flats lower the pitch by 1 step (50 cents).~%~%Sesquiflats lower the pitch by 3 steps (150 cents).~%~%Example chromatic sequence: C Ddb Db Dd D Edb Eb Ed E Fd F...etc."))

   (list (format nil "30 equal divisions of the octave~%~%Step size = 40 cents~%~%Notated as a subset of 60 EDO. Sharps only.")
         (format nil "30 equal divisions of the octave~%~%Step size = 40 cents~%~%Notated as a subset of 60 EDO. Flats only.")
         (format nil "30 equal divisions of the octave~%~%Step size = 40 cents~%~%Notated using OpenMusic's legacy accidentals"))
   
   (list (format nil "flats are respelled using half sharps and half flats (e.g. Fx = Gd)~%~%Half sharps raise the pitch by 1 step.~%~%Half flats lower the pitch by 1 step.~%~%Example chromatic sequence: C C+ C# Db Dd D D+ D# Eb Ed E E+ E# F...etc."))

   (list (format nil "36 equal divisions of the octave~%~%Step size = 33 cents~%~%Fifth size = 700 cents~%~%Notated as three interlocking sets of 12 EDO offset by 1 step of 36 EDO (33 cents).~%~%Arrows raise/lower the pitch by 1 step.~%~%Example chromatic sequence: C C^ C#v C# C#^ Dv D...etc.")
         (format nil "36 equal divisions of the octave~%~%Step size = 33 cents~%~%Fifth size = 700 cents~%~%Notated as three interlocking sets of 12 EDO offset by 1 step of 36 EDO (33 cents).~%~%Arrows raise/lower the pitch by 1 step.~%~%Example chromatic sequence: C C^ Dbv Db Db^ Dv D...etc.")
         (format nil "36 equal divisions of the octave~%~%Step size = 33 cents~%~%Fifth size = 700 cents~%~%Notated using OpenMusic's legacy accidentals"))

   (list (format nil "42 divisions of the octave.~%~%Step size = 29 cents~%~%Fifth size = 714 cents~%~%Notated as a chain of fifths.~%~%Flats are respelled with arrow-attached naturals and sharps (e.g. Db = C^).~%~%Each arrow raises/lowers the pitch by 1 step (29 cents).~%~%Example chromatic sequence: C C^ C^^ C^^^ C#vvv C#vv C#v C#...etc.")
         (format nil "42 divisions of the octave.~%~%Step size = 29 cents~%~%Fifth size = 714 cents~%~%Notated as a chain of fifths.~%~%Sharps are respelled with arrow-attached naturals and flats (e.g. C# = Dv).~%~%Each arrow raises/lowers the pitch by 1 step (29 cents).~%~%Example chromatic sequence: C Db Db^ Db^^ Db^^^ Dvvv Dvv Dv D...etc.")
         (format nil "42 equal divisions of the octave~%~%Step size = 29 cents~%~%Notated using OpenMusic's legacy accidentals"))

   (list (format nil "48 equal divisions of the octave~%~%Step size = 25 cents~%~%Fifth size = 700 cents~%~%Notated as two interlocking sets of 24 EDO offset by 1 step of 48 EDO (25 cents).~%~%Arrows raise/lower the pitch by 1 step.~%~%Example chromatic sequence: C C^ C+ C#v C# C#^ C#+ Dv D...etc.")
         (format nil "48 equal divisions of the octave~%~%Step size = 25 cents~%~%Fifth size = 700 cents~%~%Notated as two interlocking sets of 24 EDO offset by 1 step of 48 EDO (25 cents).~%~%Arrows raise/lower the pitch by 1 step.~%~%Example chromatic sequence: C C^ Ddb Dbv Db Db^ Dd Dv D...etc.")
         (format nil "48 equal divisions of the octave~%~%Step size = 25 cents~%~%Fifth size = 700 cents~%~%Notated using OpenMusic's legacy accidentals"))
   (list (format nil "60 equal divisions of the octave~%~%Step size = 20 cents~%~%Fifth size = 700 cents~%~%Notated as five interlocking sets of 12 EDO offset by 1 step of 60 EDO (20 cents).~%~%Arrows raise/lower the pitch by 1 step.")
         (format nil "60 equal divisions of the octave~%~%Step size = 20 cents~%~%Fifth size = 700 cents~%~%Notated as five interlocking sets of 12 EDO offset by 1 step of 60 EDO (20 cents).~%~%Arrows raise/lower the pitch by 1 step.")
         (format nil "60 equal divisions of the octave~%~%Step size = 20 cents~%~%Fifth size = 700 cents~%~%Notated using OpenMusic's legacy accidentals"))
 
   (list (format nil "72 equal divisions of the octave~%~%Step size = 17 cents~%~%Fifth size = 700 cents~%~%Notated as six interlocking sets of 12 EDO offset by 1 step of 72 EDO (17 cents).~%~%Arrows raise/lower the pitch by 1 step (17 cents).~%~%Half sharps raise the pitch by 3 steps (50 cents).~%~%Sesquisharps raise the pitch by 9 steps (150 cents).~%~%Example chromatic sequence: C C^ C^^ C+ C#vv C#v C#...etc.")
         (format nil "72 equal divisions of the octave~%~%Step size = 17 cents~%~%Fifth size = 700 cents~%~%Notated as six interlocking sets of 12 EDO offset by 1 step of 72 EDO (17 cents).~%~%Arrows raise/lower the pitch by 1 step (17 cents).~%~%Half flats lower the pitch by 3 steps (50 cents).~%~%Sesquiflats lower the pitch by 9 steps (150 cents).~%~%Example chromatic sequence: C C^ C^^ Ddb Dbvv Dbv Db...etc.")
         (format nil "72 equal divisions of the octave~%~%Step size = 17 cents~%~%Fifth size = 700 cents~%~%Notated using OpenMusic's legacy accidentals"))

   (list (format nil "84 equal divisions of the octave~%~%Step size = 14 cents~%~%Fifth size = 700 cents~%~%Notated as seven interlocking sets of 12 EDO offset by 1 step of 84 EDO (14 cents).~%~%Arrows raise/lower the pitch by 1 step (14 cents).~%~%Example chromatic sequence: C C^ C^^ C^^^ C#vvv C#vv C#v C#...etc.")
         (format nil "84 equal divisions of the octave~%~%Step size = 14 cents~%~%Fifth size = 700 cents~%~%Notated as seven interlocking sets of 12 EDO offset by 1 step of 84 EDO (14 cents).~%~%Arrows raise/lower the pitch by 1 step (14 cents).~%~%Example chromatic sequence: C C^ C^^ C^^^ Dbvvv Dbvv Dbv Db...etc.")
         (format nil "84 equal divisions of the octave~%~%Step size = 14 cents~%~%Fifth size = 700 cents~%~%Notated using OpenMusic's legacy accidentals"))

   (list (format nil "96 equal divisions of the octave~%~%Step size = 13 cents~%~%Fifth size = 700 cents~%~%Notated as eight interlocking sets of 12 EDO offset by 1 step of 96 EDO (13 cents).~%~%Arrows raise/lower the pitch by 1 step (13 cents).~%~%Half sharps raise the pitch by 4 steps (50 cents).~%~%Sesquisharps raise the pitch by 12 steps (150 cents).~%~%Example chromatic sequence: C C^ C^^ C^^^ C+ C#vvv C#vv C#v C#...etc.")
         (format nil "96 equal divisions of the octave~%~%Step size = 13 cents~%~%Fifth size = 700 cents~%~%Notated as eight interlocking sets of 12 EDO offset by 1 step of 96 EDO (13 cents).~%~%Arrows raise/lower the pitch by 1 step (13 cents).~%~%Half flats lower the pitch by 4 steps (50 cents).~%~%Sesquiflats lower the pitch by 12 steps (150 cents).~%~%Example chromatic sequence: C C^ C^^ C^^^ Ddb Dbvvv Dbvv Dbv Db...etc.")
         (format nil "96 equal divisions of the octave~%~%Step size = 13 cents Fifth size = 700 cents~%~%Notated using OpenMusic's legacy accidentals"))
   
   ))


(defparameter *omicron-data*
(loop for i in *edo-names*
      for n in *edo-notation*
      for d in *edo-doc*
      collect (list i n d)))


(defparameter *omicron-scales-list*
  (list
   (list
   (list 1   *1-tone-chromatic-scale*  "1")
   (list 1.0 *1#-tone-chromatic-scale* "1#"))
   (list
   (list 2   *current-1/2-scale* "1/2"))
   (list 
   (list 3   *3-tone-chromatic-scale* "1/3")
   (list 3.0 *3#-tone-chromatic-scale* "1/3#"))
   (list
   (list 4   *current-1/4-scale* "1/4"))
   (list 
   (list 5   *5-tone-chromatic-scale* "1/5")
   (list 5.0 *5#-tone-chromatic-scale* "1/5#"))
   (list
   (list 6   *6-tone-chromatic-scale*  "1/6"))
   (list
   (list 7   *7-tone-chromatic-scale*  "1/7")
   (list 7.0 *7#-tone-chromatic-scale* "1/7#"))
   (list
   (list 8   *current-1/8-scale* "1/8"))
   (list
   (list 10   *10-tone-chromatic-scale* "1/10"))
   (list
   (list 12   *12-tone-chromatic-scale*  "1/12"))
   (list
   (list 14   *14-tone-chromatic-scale*  "1/14"))
   (list
   (list 16   *16-tone-chromatic-scale* "1/16"))
   ;;;
   (list (list 50.1 *5-EDO_#* "5#")
         (list 50.0 *5-EDO_b* "5b"))
   (list
    (list 60.1 *6-EDO_#* "6#")
    (list 60.0 *6-EDO_b* "6b"))
   (list
    (list 70   *7-EDO* "7"))
   (list
    (list 80.1 *8-EDO_#* "8#")
    (list 80.0 *8-EDO_b* "8b"))
   (list 
    (list 90.1 *9-EDO_#* "9#")
    (list 90.0 *9-EDO_b* "9b"))
   (list
    (list 100.1 *10-EDO_#* "10#")
    (list 100.0 *10-EDO_b* "10b"))
   (list
    (list 120.1 *12-EDO_#* "12#")
    (list 120.0 *12-EDO_b* "12b"))
   (list
    (list 140.1 *14-EDO_^* "14^")
    (list 140.0 *14-EDO_v* "14v"))
   (list
    (list 150.1 *15-EDO_#* "15#")
    (list 150.0 *15-EDO_b* "15b"))
   (list
    (list 160.1 *16-EDO_#* "16#")
    (list 160.0 *16-EDO_b* "16b"))
   (list
    (list 170 *17-EDO* "17")
    (list 170.1 *17-EDO_#* "17#")
    (list 170.0 *17-EDO_b* "17b"))
   (list
    (list 180.1 *18-EDO_#* "18#")
    (list 180.0 *18-EDO_b* "18b"))
   (list
    (list 190 *19-EDO* "19"))
   (list
    (list 220 *22-EDO* "22")
    (list 220.1 *22-EDO_#* "22#")
    (list 220.0 *22-EDO_b* "22b"))
   (list
    (list 240.0 *24-EDO_b* "24b"))
   (list
    (list 300.1 *30-EDO_#* "30#")
    (list 300.0 *30-EDO_b* "30b"))
   (list
    (list 310 *31-EDO* "31"))
   (list
    (list 360.1 *36-EDO_#* "36#")
    (list 360.0 *36-EDO_b* "36b"))
   (list
    (list 420.1 *42-EDO_#* "42#")
    (list 420.0 *42-EDO_b* "42b"))
   (list
    (list 480.1 *48-EDO_#* "48#")
    (list 480.0 *48-EDO_b* "48b"))
   (list
    (list 600.1 *60-EDO_#* "60#")
    (list 600.0 *60-EDO_b* "60b"))
   (list
    (list 720.1 *72-EDO_#* "72#")
    (list 720.0 *72-EDO_b* "72b"))
   (list
    (list 840.1 *84-EDO_#* "84#")
    (list 840.0 *84-EDO_b* "84b"))
   (list
    (list 960.1 *96-EDO_#* "96#")
    (list 960.0 *96-EDO_b* "96b"))))