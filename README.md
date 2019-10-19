# OpenMusic

OpenMusic (OM) is a visual programming language based on [Lisp](http://www.gigamonkeys.com/book/introduction-why-lisp.html). Visual programs are created by assembling and connecting icons representing functions and data structures. Most programming and operations are performed by dragging an icon from a particular place and dropping it to an other place. Built-in visual control structures (e.g. loops) are provided, that interface with Lisp ones.

OM may be used as a general purpose functional/object/visual programming language. At a more specialized level, a set of provided classes and libraries make it a very convenient environment for music composition. Above the OpenMusic kernel, live the OpenMusic Projects. A project is a specialized set of classes and methods written in Lisp, accessible and visualisable in the OM environment. Various classes implementing musical data / behaviour are provided. They are associated with graphical editors and may be extended by the user to meet specific needs. Different representations of a musical process are handled, among which common notation, midi piano-roll, sound signal. High level in-time organisation of the music material is proposed through the concept of "maquette".

Existing CommonLisp/CLOS code can easily be used in OM, and new code can be developed in a visual way.

- [OpenMusic User Manual](http://support.ircam.fr/docs/om/om6-manual/)

- [Official OpenMusic page](http://repmus.ircam.fr/openmusic/).

---------

Designed and developed by the IRCAM [Music Representation research group](http://repmus.ircam.fr)

© 1998 - 2018 Carlos Agon, Gérard Assayag, Jean Bresson.

----------

## Sources and Licensing

OpenMusic is a free software distributed under the GPLv3 license. As a Common Lisp program, the environment can be considered just as an extension of Lisp including the specific built-in features of the application. 

While the sources of OM7 are available under the GPL license, the application is developed with [LispWorks 7](http://www.lispworks.com/): a commercial Lisp environment providing multiplatform support and graphical/GUI toolkits. A free (limited) edition of LW6 is available on the LispWorks website, but unfortunately no free version of LW-7 exists at the moment.

In order to contribute to the code without a LispWorks license, one must therefore work both with the cloned source package _and_ an up-to-date reseased version on OM (which includes a Lisp interpreter).

----------

## Build instructions 

Creating an executable in Common Lisp means loading all the source code in the Lisp environment and build an "image" of it, i.e. another Lisp environment extended with the features defined in the code.
It is also possible to compile and load OM source code in the Lisp environment and use it directly without saving a new image. Using OM sources therefore requires owning the adequate Lisp compiler (currently, LispWorks 7.1.2).

The current OM sources can be compiled and run OM on **macOS**, **Windows** and **Linux** (see compilation instructions below) using LispWorks "Hobbyist" or "Professional" licenses.

### Loading OM in LispWorks 7.1

- Clone OM sources from this repository
- Launch an up-to-date LispWorks (7.1.2)
- In LispWorks, load the file **OPENMUSIC/build/build-om.lisp**    
  Open ( _File/Open_) then load it (_File/Load_) or type in the Lisp listener: `(load [...]/OPENMUSIC/build/build-om.lisp)`     
  (Check on possible compilation errors on the displayed LW output)
- Evaluate `(om::start-openmusic)`

### Fonts

If this is a first launch of OpenMusic, musical fonts might need to be installed in the system in order to display score objects and editors.

The OM musical fonts files **omicron.ttf**, **omheads.ttf**, **omsign.ttf**, and **omextra.ttf** can be found in **OPENMUSIC/resources/fonts/**. These files must be copied in:
* **MacOSX:** /Library/Fonts/
* **Windows:** C:/WINDOWS/Fonts/
* **Linux:** /usr/share/fonts/ 

**Note:** on macOS / Windows, double-clicking the font file will open an automatic installation utility.

### Dependencies

#### Lisp packages

Lisp-dependencies are all included in the OM source repository (**OPENMUSIC/code/api/externals/**) and do not require any particular action or installation. OM uses the following external/opensource packages:

- **[lispworks-udp](https://github.com/binghe/lispworks-udp)** (by Chun Tian (binghe))
- **[OSC](https://github.com/zzkt/osc)** (by Nik Gaffney)
- **[S-XML](https://common-lisp.net/project/s-xml/)** (by Sven Van Caekenberghe)
- **[CL-SVG](https://github.com/wmannis/cl-svg)** (by William S. Annis)
- **[Yason](https://github.com/phmarek/yason/)** (Json encoding/deconding, by Hans Huebner at al.) 
- **CL-FluidSynth**/**CL-Jack** (by Anders Vinjar)
- An incredible blend of MIDI bindings including **[CL-MIDI](http://www.doc.gold.ac.uk/isms/lisp/midi/)** (by Robert Strandh et al.), **[CL-PortMIDI](https://github.com/chfin/cl-portmidi)** (from PortMedia/Christoph Finkensiep), and the **[CFFI-PortMidi](https://sourceforge.net/p/portmedia/code/HEAD/tree/portmidi/trunk/pm_cl/)** bindings from PortMIDI (by Heinrich Taube).
- LispWorks **OpenGL** interface

#### C-libraries

OM links dynamically with a number of external C libraries. The foreign function interface relies on LispWorks' FLI package, and on **[CFFI](https://common-lisp.net/project/cffi/)** (included in **OPENMUSIC/code/api/foreign-interface/**).

Binary versions of the libraries are included in this repository in **OPENMUSIC/resources/lib/[mac/win/linux]/**, but they can also be recompiled from their respective source packages if needed, as indicated below.

**Note for Windows:** In order to run OM sources from LispWorks on Windows, all the external C libraries (.dll) should be copied in the LispWorks repository (**C:/Program Files/LispWorks/**)

- **omaudiolib**: https://github.com/openmusic-project/omaudiolib
- **libsndfile** : https://github.com/erikd/libsndfile / 
- **libsamplerate** : https://github.com/erikd/libsamplerate
- **libPortMIDI**: http://portmedia.sourceforge.net/portmidi/
- **libSDIF**: http://sdif.sourceforge.net/ 

