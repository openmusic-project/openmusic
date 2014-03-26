;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
;This file define the *kernel-files* global variable, which contains a list
;with the basic file's pathnames.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)
 
(defvar *kernel-files* nil)

(setf *kernel-files* '(
                       
;-----------Tools--------------------------------- 
"tools/lisptools"    
"tools/saveheader"   
"graphics/graphictools" 
"tools/traduction" 
                                
;-----------Containers---------------------------- 
"classes/container"   

;-----------Modele-------------------------------- 
"ommodele/genfunmetaclass"                                     
"ommodele/metaclasses"   
"ommodele/defgeneric"      
"ommodele/protocol"
"ommodele/modele"             
                                    
;-----------Graphics------------------------------ 
"graphics/icons/iconview" 
"graphics/icons/picture"     
"graphics/dialogs" 
"graphics/rulers"  

;-----------Basic Objects------------------------- 
"ommodele/ombasicobjects/ombasictype"  
"ommodele/ombasicobjects/omlispfun"      
"ommodele/ombasicobjects/ominstance"      
"ommodele/ombasicobjects/omclass"           
"ommodele/ombasicobjects/omslot"              
"ommodele/ombasicobjects/omgenfun"      
"ommodele/ombasicobjects/ommethod"                                      
"ommodele/ombasicobjects/ompatch"          
"ommodele/ombasicobjects/omlisppatch"          
"ommodele/ombasicobjects/ommaquette"   
"ommodele/ombasicobjects/omfolder"         
"ommodele/ombasicobjects/omworkspace" 
"ommodele/ombasicobjects/boxes/ombox"  
"ommodele/ombasicobjects/boxes/patchboxes" 
"ommodele/ombasicobjects/boxes/temporalboxes" 
"ommodele/ombasicobjects/boxes/classtreeboxes"  
"ommodele/ombasicobjects/ompackage"   
"ommodele/ombasicobjects/omlib" 
                       
                       
;----------- Editor Frames------------------------------- 

"ommodele/omframes/containereditors/editorwindow"   
                                    
"ommodele/omframes/containereditors/metaobjectcontainer" 

"ommodele/omframes/containereditors/nonrelationcontainer"  
"ommodele/omframes/containereditors/browsercontainer"  
"ommodele/omframes/containereditors/classeditor"  
"ommodele/omframes/containereditors/relationcontainer"  
"ommodele/omframes/containereditors/hierarchiecontainer"  
"ommodele/omframes/containereditors/instancecontainer"

;-------------VP editors------------------------
"ommodele/omframes/containereditors/algortimecontainer/patchcontainer"
"ommodele/omframes/containereditors/algortimecontainer/lisppatcheditor"
"ommodele/omframes/containereditors/algortimecontainer/methodcontainer"
"ommodele/omframes/containereditors/algortimecontainer/setgetslotscontainer"
"ommodele/omframes/containereditors/algortimecontainer/maquette-rulers"
"ommodele/omframes/containereditors/algortimecontainer/maquettecontainer"
"ommodele/omframes/containereditors/algortimecontainer/maquette-markers"

"ommodele/omframes/edition-params"


;----------- Simple Frames------------------------------- 
"ommodele/omframes/simpleframes/iconsframe/iconfinder"     
"ommodele/omframes/simpleframes/iconsframe/packbrowser-icons" 
"ommodele/omframes/simpleframes/iconsframe/methodframe" 
"ommodele/omframes/simpleframes/boxes/inlet" 
"ommodele/omframes/simpleframes/boxes/controls" 
"ommodele/omframes/simpleframes/boxes/boxframe"  
"ommodele/omframes/simpleframes/boxes/in-out-boxes"
"ommodele/omframes/simpleframes/boxes/connections"
"ommodele/omframes/simpleframes/boxes/tempobjframe"
"ommodele/omframes/simpleframes/boxes/maq-in-out"
"ommodele/omframes/simpleframes/boxes/miniviews"

;-----------Controls------------------------------- 
"boxes/boxeswitheditor/boxwithpatch"
"boxes/boxeswitheditor/omloop"
"boxes/boxeswitheditor/filebox"
"boxes/basicboxes"
"boxes/boxeswitheditor/abspatch"
"boxes/file-utils"
"boxes/dialog-item-boxes"
"boxes/receive-box"

;-----------Aux windows------------------------------ 
"graphics/infowindow"
"graphics/palette"

;--------------------Print---------------------- ???
"graphics/print"   

;--------------------Player---------------------- 
"scheduler-player/play"
"scheduler-player/new-player"
"scheduler-player/players"

;-----------Persistant&copy--------------------- 
"ommodele/persistant/copy"  
"ommodele/persistant/save"
"ommodele/persistant/import-export"   
"ommodele/persistant/old-compat"
                                                            
;-----------Menu----------------------------------- 
"graphics/ommenu"

;-------------Preferences----------------------- 
"graphics/preferences"
"graphics/default-prefs"

;--------------OM-Drag-and-Drop----------------- 
"graphics/dragdrop/omdraganddrop"
"graphics/dragdrop/dropallowp"    
"graphics/dragdrop/performdrop"

;--------------Help----------------- 
"doc/documentation"
"doc/onlinepatches"
"doc/tutorials"
"doc/reference"

;-------------Pack----------------------- 
"kernel-pack"

))


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (current-pathname filename nil)))
        (remove nil *kernel-files*)))

