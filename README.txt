;=====================================
; OM 6.13 for MacOSX
;=====================================

To install, just drag "OM 6.13.app" into your Applications/ directory.

OpenMusic is a free software distributed under GNU LGPL license.
(c) IRCAM 1997-2017

- Download and updates : http://forumnet.ircam.fr/shop/fr/forumnet/43-openmusic.html
- User Manual : support.ircam.fr/docs/om/om6-manual/
- User group and support : http://forumnet.ircam.fr/fr/user-groups/openmusic/forum/
- More info : http://repmus.ircam.fr/openmusic/

;------------------------------------
; RELEASE NOTES:
;------------------------------------
The OM distribution package is modified to conform with macOS security changes
No more ressources are accessed with relative pathnames outside the .app bundle

1) Libraries are all stored in a separate folder and distributed as separate packages.
The default location is (USER_HOME)/OM/Libraries/
Other locations can be added in the Preferences (as is previous versions)
2) The 'patches' folder is no more visible: you can find it as OM 6.x.app/Contents/Init,
or simply put your Lisp code in the Workspace's "user" folder instead.
3) The OM source code is still accessible from OM, but also now relocated 
inside the .app bundle's resources.

;------------------------------------
; If you want to load and use OM code in LispWorks, 
; check/download the source package from the repository: 
; https://forge.ircam.fr/p/OM/ 
; 
; ...or clone it using GIT:
; $ git clone git://git.forge.ircam.fr/OM.git
;------------------------------------

Other changes updates:

- Base Lisp environment update: LispWorks 7.1 (64-bits)
- Audio support update: Juce 5

- Audio library: fix audio device detection and preferences
- Audio files: support for special characters in pathnames
- Audio player: fix mono file playback
- Audio i/o : fix output format selection (wave, aiff etc.)
- Score objects: updated 'concat' method
- Score objects: function 'flip-selection' (bound to "f/F" keys)
- Score analysis framework: add metrics in 'quantify-segments' 
- Score analysis framework: other misc. fixes/improvements 
- Support for complex numbers
- Misc. fixes and improvements


