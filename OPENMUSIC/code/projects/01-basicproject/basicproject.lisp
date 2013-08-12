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


     
       
     
 
     
  