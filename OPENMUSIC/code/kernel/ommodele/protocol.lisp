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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;This file contains the abstract classes for the system definition.
;Last Modifications :
;18/10/97 first date.
;DocFile



(in-package :om)

;-----------------Protocole--------------------

#|
(defgeneric* omNG-make-new-boxcall (self posi name)
   (
    :icon 400 
    :documentation "Cons a box having by reference the 'self'.")
   )

(defgeneric* omNG-rename (self name)
   (
    :icon 400 
    :documentation "This method changes the name of the object 'self' with the new name 'name'")
   )

(defgeneric* omng-MoveObject (self newpos)
   (
    :icon 400 
    :documentation "Move the box 'self' to 'newpos'.")
   )

(defgeneric* omNG-box-value (self &optional numout)
   (
    :icon 400
    :documentation "Eval the output indexed by 'numout' for the box 'self'. 
In this method we call the generic function reference of 'self'.")
)

(defgeneric* omNG-connect (source  numout  target  numin lines &optional col)
   (
    :icon 400
    :documentation "Connect the input indexed by 'numin' of the box 'source' to the output indexed by 'numout'
of the box 'target'. If  lines equal NIL the connection will be draw automaticly.")
   )



(defgeneric* omG-select (self)  
   (
    :icon 400 
    :documentation "Set the frame 'self' and the object pointed by 'self' in selected mode")
   )

(defgeneric* omG-unselect (self)  
   (
    :icon 400 
    :documentation "Set the frame 'self' and the object pointed by 'self' in unselected mode")
   )

(defgeneric* get-elements (self) 
   (
    :icon 400
    :documentation "get objects contains in self" 
    )
   )
|#