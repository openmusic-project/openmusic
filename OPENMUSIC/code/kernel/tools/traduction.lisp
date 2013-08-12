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
; Language translations for GUI components
;DocFile



(in-package :om)

;;;(defpackage omstring)

;;; Mettre dans les prefs
(defvar *languages* '(:en :fr))
(defvar *om-language* 0)

(defun set-language (lang)
  (setf *om-language* (or (position lang *languages*) 0)))


(defvar *om-strings* nil)
(setf *om-strings* '((:a ("A" "La")) 
                     (:about ("About..." "A propos..."))
                     (:absolute ("Absolute" "Absolue"))
                     (:accepted-formats ("Accepted formats" "Format acceptés"))
                     (:all-files ("All Files" "Tous les fichiers")) 
                     (:all-tracks ("All tracks" "Tous les tracks")) 
                     (:appearance ("Appearance" "Apparence"))
                     (:apply ("Apply" "Appliquer")) 
                     (:apply-all-lost ("Apply for all lost files" "Appliquer pour tous les fichiers perdus")) 
                     (:as ("as" "sous")) 
                     (:audio-ctrl ("Audio Control" "Contrôles Audio"))
                     (:b ("B" "Si")) 
                     (:bg-pict ("Background Picture" "Image de fond")) 
                     (:c ("C" "Do")) 
                     (:cadencies ("Cadencies" "Cadences")) 
                     (:cancel ("Cancel" "Annuler")) 
                     (:cannot-load ("Cannot load the file ~s" "Impossible de charger le fichier ~s")) 
                     (:channel ("Channel" "Canal")) 
                     (:channel-type ("~A Channel" "Canal ~A")) 
                     (:check-file-format ("Please check the file format and extension."))
                     (:choose-bgpict ("Choose Background Picture" "Choisir une image de fond")) 
                     (:choose-file ("Choose a file" "Choisissez un fichier")) 
                     (:choose-midi ("Choose a MIDI File" "Choisissez un fichier MIDI")) 
                     (:choose-snd ("Choose a sound file" "Choisissez un fichier audio")) 
                     (:chord ("Chord" "Accord")) 
                     (:click ("Click" "Cliquez")) 
                     (:close ("Close" "Fermer")) 
                     (:color ("Color" "Couleur")) 
                     (:commands ("Commands" "Commandes")) 
                     (:control-type ("~A Control" "Contrôles ~A")) 
                     (:copy ("Copy" "Copier")) 
                     (:cut ("Cut" "Couper")) 
                     (:d ("D" "Ré")) 
                     (:degrees ("Degrees" "Degrés")) 
                     (:delete ("Delete" "Supprimer"))
                     (:done ("Done" "Terminé"))
                     (:duration ("Duration" "Durée")) 
                     (:e ("E" "Mi")) 
                     (:edit ("Edit" "Editer")) 
                     (:editmenu ("Edit" "Edition")) 
                     (:envelope ("Envelope" "Enveloppe")) 
                     (:error ("Error" "Erreur")) 
                     (:export ("Export" "Exportation")) 
                     (:export-format ("~A Export" "Exportation ~A")) 
                     (:exportmenu ("Export" "Exporter")) 
                     (:extract ("Extract" "Extrait")) 
                     (:f ("F" "Fa")) 
                     (:figuring ("Figuring" "Chiffrage")) 
                     (:file ("File" "Fichier")) 
                     (:file-error ("File Error" "Erreur")) 
                     (:file-format ("~A Files" "Fichiers ~A")) 
                     (:file-not-found ("File ~s not found" "Fichier ~s introuvable")) 
                     (:folder ("Folder" "Dossier"))
                     (:fontsize ("Font Size" "Taille de police"))
                     (:foreign-tones ("Foreign Tones" "Accidents")) 
                     (:from ("From" "De")) 
                     (:fromrulers ("From ruler units" "Unités des règles")) 
                     (:full-file ("Complete File" "Fichier entier")) 
                     (:g ("G" "Sol")) 
                     (:grid ("Grid" "Grille")) 
                     (:h-ruler ("Horizontal ruler (time)" "Règle horizontale (temps)")) 
                     (:hand-mode ("Hand selection tool (move view)" "Mode \"Main\" (déplacer la vue)")) 
                     (:help ("Help" "Aide")) 
                     (:hide ("Hide" "Cacher")) 
                     (:image-import-error ("Error importing picture!" "Erreur d'importation de l'image!"))
                     (:import ("Import" "Importation")) 
                     (:import-file ("Importing ~s" "Importation de ~s")) 
                     (:import-format ("~A Import" "Importation ~A")) 
                     (:import-error ("Importation error." "Erreur lors de l'importation."))
                     (:import-file-error ("Error importing ~s." "Erreur lors de l'importation de ~s."))
                     (:importmenu ("Import" "Importer"))
                     (:interval-mode ("Interval selection tool" "Mode \"Sélection intervalle\"")) 
                     (:last-saved ("Last Saved" "Revenir à la version enregistrée")) 
                     (:layout ("Layout" "Disposition")) 
                     (:lib-error ("~A Library Error." "Erreur de chargement de la bibliothèque ~A.")) 
                     (:lock ("Lock" "Bloquer")) 
                     (:look? ("Do you want to look for it?" "Voulez-vous le rechercher?")) 
                     (:lookingfor ("Looking for..." "Rechercher...")) 
                     (:major ("Major" "Majeur")) 
                     (:measure ("Measure" "Mesure")) (:measures ("Measures" "Mesures")) 
                     (:metric ("Metric" "Métrique")) 
                     (:metrics ("Metrics" "Mesures")) 
                     (:midi-ctrl ("MIDI Control" "Contrôles MIDI"))
                     (:midifile ("MIDI File" "Fichier MIDI"))
                     (:minor ("Minor" "Mineur")) 
                     (:modulations ("Modulations" "Modulations")) 
                     (:mosaic ("Mosaic" "Mosaïque")) 
                     (:name ("Name" "Nom")) 
                     (:new ("New" "Nouveau"))
                     (:new-doc ("New Document" "Nouveau Document")) 
                     (:new-name ("New name" "Nouveau nom"))
                     (:no ("No" "Non")) 
                     (:note ("Note" "Note")) 
                     (:offset ("Offset" "Offset")) 
                     (:offset-error ("Error: offset must be a positive integer!" "Erreur: les offset doivent être entiers et positifs!")) 
                     (:ok ("OK" "OK")) 
                     (:open ("Open" "Ouvrir")) 
                     (:param ("Parameter" "Paramètre"))
                     (:params ("Parameters" "Paramètres"))
                     (:partials ("Partials" "Partiels")) 
                     (:paste ("Paste" "Coller")) 
                     (:pict-unavailable ("Pict unavailable" "Image non disponible")) 
                     (:picture ("Picture" "Image")) 
                     (:picture-size ("Picture size" "Taille de l'image")) 
                     (:position ("Position" "Position")) 
                     (:preferences ("Preferences" "Préférences"))
                     (:quit ("Quit" "Quitter")) 
                     (:reinitscales ("Reinit. views" "Réinitialiser les échelles")) 
                     (:remove-bg ("Remove Background" "Supprimer le fond")) 
                     (:rename ("Rename" "Renommer")) 
                     (:rhytmicseq ("Rhythmic Sequence" "Séquence Rythmique")) 
                     (:rulers ("Rulers" "Règles")) 
                     (:rulers-settings ("Rulers Settings" "Paramètres des règles latérales")) 
                     (:save ("Save" "Enregistrer")) 
                     (:save-as ("Save As..." "Enregistrer sous...")) 
                     (:save-changes-in ("Save changes in ~s?" "Enregistrer les modifications sur ~s ?")) 
                     (:sdiffile ("SDIF File" "Fichier SDIF"))
                     (:selec-mode ("Mode \"Sélection objets\"" "Objects selection tool")) 
                     (:select-all ("Select All" "Sélectionner tout")) 
                     (:selection ("Selection" "Sélection")) 
                     (:sequence ("Sequence" "Séquence")) 
                     (:settings ("Settings" "Paramètres")) 
                     (:show ("Show" "Afficher")) 
                     (:size ("Size" "Taille")) 
                     (:snap ("Snap" "Magnétisme")) 
                     (:sound ("Sound" "Audio")) 
                     (:tempo ("Tempo" "Tempo")) 
                     (:textcolor ("Text Color" "Couleur de texte")) 
                     (:textfont ("Text Font" "Police de texte")) 
                     (:timesign ("Time Signature" "Métrique")) 
                     (:to ("To" "à")) 
                     (:tonality ("Tonality" "Tonalité")) 
                     (:track ("Track" "Track")) 
                     (:undo ("Undo" "Annuler")) 
                     (:unlock ("Unlock" "Débloquer")) 
                     (:v-ruler ("Vertical ruler" "Règle verticale")) 
                     (:volume ("Volume" "Volume")) 
                     (:window ("Window" "Fenêtre")) 
                     (:window-size ("Window size" "Taille de la fenêtre")) 
                     (:yes ("Yes" "Oui")) 
                     (:ystep ("Y step" "Pas Y")) 
                     (:zoom-mode ("Zoom tool" "Mode \"Loupe\""))))


(defun add-om-strings (str-list)
  (loop for item in str-list do
        (when (find (car item) *om-strings* :test 'equal :key 'car)
          (print (format nil "WARNING: OM-STRING ~A REDEFINED" (car item)))))
  (setf *om-strings* (append str-list *om-strings*))
  t)

(defun om-str (strkey)
  (str-check (or (nth *om-language* 
                      (cadr (find strkey *om-strings* :test 'equal :key 'car)))
                 (string-downcase strkey))))




  
