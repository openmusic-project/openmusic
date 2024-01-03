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
;=========================================================================

;;; MIDI package

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
        "functions;objects-functions"

        ;-----------Editors--------------------------------
        "container;maquetteinterface"     
        
        "editor;scoreeditor;rythtools"      
        "editor;scoreeditor;scoreeditors" 
        "editor;scoreeditor;edition"
        "editor;scoreeditor;scorepatch"       
       ; "editor;scoreeditor;inspectorpalette"    
        "editor;scoreeditor;inspectorpalette-editor"    
        "editor;musminiboxes"           
        "editor;scoreeditor;scoretools"
        "editor;scoreeditor;extraobjs"
        ;"editor;scoreeditor;extrapalette"
	"editor;scoreeditor;extrapalette-editor"    
        
        "editor;scoreeditor;graces"
        "editor;scoreeditor;tempo-change"
        
        "editor;scoreeditor;dragagain"     
        
        "editor;scoreeditor;contextmenu"       
        "editor;scoreeditor;pagebreak"
        "editor;scoreeditor;strandhbreak"
    
        "editor;scoreeditor;score-playback"

        "editor;inspectors;treeedit"
        "editor;inspectors;tempoedit"
        "editor;inspectors;treetempoedit"
        "editor;inspectors;objinfo"
        
        "classes;extras"

        ;----------- ANALYSIS FRAMEWORK ----------------
        "analysis;analysis-system"
        "analysis;segments"
        "analysis;scoreeditor-analysis"
        "analysis;analysis-objects"
        "analysis;kant-seg"
        "analysis;kant-interface"
        "analysis;utils"

        ;----------- IMPORT EXPORT --------------------------------
        "import-export;etf"           ; translate-score marche pas bien
        "import-export;coom"                
        "import-export;import-mxml-new"                
        "import-export;export-mxml"
        "import-export;import-export"
        "import-export;om2bach"
        
        
        ;-------------------OM Interface to Musical Objects----
        "container;scoreominterface"  
        "container;scoreslots"
        "functions;true-durations"      
        "functions;quantifyom"      
         "functions;gkant"      
        "functions;maquette2obj"    
        "functions;conversions"       
        
        "editor;scoreeditor;pagination"
        "editor;musicpreferences"

        "music-package"  
        ))



(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *MusicProj-files*))

(push :om-musicproject *features*)
   
     

     
 
     
  
