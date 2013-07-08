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



(defvar *MusicProj-files* nil)

(setf *MusicProj-files* 
      '(
        "editor;omicron"
        "editor;scoreeditor;scales"
        "container;tonalite"
              
        ;-----------Preferences----------------------------
        "editor;general-editor-tools"
                
        ;-----------Classes--------------------------------
        
        "container;scoreobject"
        "container;tree2container" 
        "container;rythme;fringe" 
        "container;rythme;rythme" 
        "container;rythme;grouping"
        "container;rythme;tuplet"      
        "container;rythme;corrections"
             
        "classes;chordseq"
        
        
        ;-----------Functions--------------------------------
        "functions;trees"
        
        ;-----------Editors--------------------------------
        "container;maquetteinterface"     
        
        "editor;scoreeditor;rythtools"      
        "editor;scoreeditor;scoreeditors" 
        "editor;scoreeditor;edition"
        "editor;scoreeditor;scorepatch"       
        ;;"editor;scoreeditor;scorepalette"    
        "editor;scoreeditor;inspectorpalette"    
        "editor;musminiboxes"           
        "editor;scoreeditor;scoretools"
        "editor;scoreeditor;extraobjs"
        "editor;scoreeditor;extrapalette"    
        
        "editor;scoreeditor;graces"
        "editor;scoreeditor;tempo-change"
        
        "editor;scoreeditor;dragagain"     
        
        "editor;scoreeditor;contextmenu"       
        "editor;scoreeditor;pagebreak"
        "editor;scoreeditor;strandhbreak"
        
        
        
        "classes;extras"

        ;----------- ANALYSIS FRAMEWORK ----------------
        "analysis;analysis-system"
        "analysis;segments"
        "analysis;scoreeditor-analysis"
        "analysis;analysis-objects"
        "analysis;kant-seg"

        ;----------- IMPORT EXPORT --------------------------------
        "import-export;etf"           ; translate-score marche pas bien
        "import-export;coom"                
        "import-export;import-mxml-new"                
        "import-export;export-mxml"
        "import-export;import-export"
        "import-export;om2bach"
        
        
        ;-------------------OM Interface to Musical Objects----
        "container;scoreominterface"   
        "functions;quantifyom"      
        "functions;maquette2obj"    
        "functions;conversions"       
        
       ; osc and microplayer
        "osc;send"
        "osc;receive"
        "osc;osc-events"
        "players;microplayer"
        
        "editor;scoreeditor;pagination"
        
        "editor;musicpreferences"
        "players;select-players"  
        "players;ms-players"  
        
        "music-package"  
        ))



(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *MusicProj-files*))

(push :om-musicproject *features*)
   
     

     
 
     
  