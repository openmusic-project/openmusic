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

(in-package :om)



(defvar *basicproj-files* 
  (list
    ;-----------Classes--------------------------------
    
    "classes;bpf" 
    "classes;bpc" 
    "classes;splines"  
    "classes;textfile"                 
    "classes;array" 
    "classes;board"    
    "classes;picture"    
    "classes;omgraphics"
    
    ;-----------Functions--------------------------------
    "functions;combinatorial" 
    "functions;sets"  
    "functions;series" 
    "functions;lists"
    "functions;functions"
    "functions;kernel"
    
    "functions;bpf-tools"
    "functions;file"
    "functions;export-import-svg"

    ;-------------EDITORS--------------------
    "editors;bpfeditor" 
    "editors;bpceditor" 
    "editors;splineseditor" 
    "editors;arrayeditor"       ; !! open-intern-editor et show-controls à faire  

    ; advanced time array
    "classes;time-array" 
    "classes;bpf-player"

    ;------- PACK
    "basic-pack"
    
    ))


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        (remove nil *basicproj-files*)))


     
       
     
 
     
  
