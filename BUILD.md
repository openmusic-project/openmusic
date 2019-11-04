# Build instructions 

Creating an executable in Common Lisp means loading all the source code in the Lisp environment and build an "image" of it, i.e. another Lisp environment extended with the features defined in the code.
It is also possible to compile and load OM source code in the Lisp environment and use it directly without saving a new image. Using OM sources therefore requires owning the adequate Lisp compiler (currently, LispWorks 7.1.2).

The current OM sources can be compiled and run OM on **macOS**, **Windows** and **Linux** (see compilation instructions below) using LispWorks "Hobbyist" or "Professional" licenses.

## Loading OM in LispWorks 7.1

- Clone OM sources from this repository
- Launch an up-to-date LispWorks (7.1.2)
- In LispWorks, load the file **OPENMUSIC/build/build-om.lisp**    
  Open ( _File/Open_) then load it (_File/Load_) or type in the Lisp listener: `(load [...]/OPENMUSIC/build/build-om.lisp)`     
  (Check on possible compilation errors on the displayed LW output)
- Evaluate `(om::start-openmusic)`

## Fonts

If this is a first launch of OpenMusic, musical fonts might need to be installed in the system in order to display score objects and editors.

The OM musical fonts files **omicron.ttf**, **omheads.ttf**, **omsign.ttf**, and **omextra.ttf** can be found in **OPENMUSIC/resources/fonts/**. These files must be copied in:
* **MacOSX:** /Library/Fonts/
* **Windows:** C:/WINDOWS/Fonts/
* **Linux:** /usr/share/fonts/ 

**Note:** double-clicking the font file will open an automatic installation utility.

## Dependencies

#### Lisp packages

Lisp-dependencies are all included in the OM source repository (**OPENMUSIC/code/api/externals/**) and do not require any particular action or installation. OM uses the following external/opensource packages:

- **[lispworks-udp](https://github.com/binghe/lispworks-udp)** (by Chun Tian (binghe))
- **[OSC](https://github.com/zzkt/osc)** (by Nik Gaffney)
- **[S-XML](https://common-lisp.net/project/s-xml/)** (by Sven Van Caekenberghe)
- **[CL-SVG](https://github.com/wmannis/cl-svg)** (by William S. Annis)
- **[Yason](https://github.com/phmarek/yason/)** (by Hans Huebner at al.) 
- **[cl-fluidsynth](https://github.com/andersvi/cl-fluidsynth)**/**[cl-jack](https://github.com/andersvi/cl-jack)** (by Anders Vinjar)
- An incredible blend of MIDI bindings including **[CL-MIDI](http://www.doc.gold.ac.uk/isms/lisp/midi/)** (by Robert Strandh et al.), **[CL-PortMIDI](https://github.com/chfin/cl-portmidi)** (from PortMedia/Christoph Finkensiep), and the **[CFFI-PortMidi](https://sourceforge.net/p/portmedia/code/HEAD/tree/portmidi/trunk/pm_cl/)** bindings from PortMIDI (by Heinrich Taube).
- LispWorks **OpenGL** interface

#### C-libraries

OM links dynamically with a number of external C libraries. The foreign function interface relies on LispWorks' FLI package, and on **[CFFI](https://common-lisp.net/project/cffi/)** (included in **OPENMUSIC/code/api/foreign-interface/**).

Binary versions of the libraries are included in this repository in **OPENMUSIC/resources/lib/[mac/win/linux]/**, but they can also be recompiled from their respective source packages if needed, as indicated below.

**Note for Windows:** In order to run OM sources from LispWorks on Windows, all the external C libraries (.dll) should be copied in the LispWorks repository (**C:/Program Files/LispWorks/**)

- **omaudiolib**: https://github.com/openmusic-project/omaudiolib
- **libsndfile** : https://github.com/erikd/libsndfile
- **libsamplerate** : https://github.com/erikd/libsamplerate
- **libPortMIDI**: http://portmedia.sourceforge.net/portmidi/
- **libSDIF**: http://sdif.sourceforge.net/ 

**Note:** the dynamic libraries must be (re)-compiled for same architecture as your LispWorks compiler (32 or 64-bits).

## Creating a redistributable exectuable

Creating a redistributable OM executable requires the **deliver** command available in LispWorks Pro and Hobbyist-DV editions.
The process is in two phases : **deliver** and **pack**. 
These two phases correspond respectively to the files **deliver.lisp** and **pack-om.lisp** in the folder **OPENMUSIC/build/**, which should just be loaded one after another from the system command line utility (**_not from the LispWorks IDE_**), using the commands:

`[path-to-lispworks-exe] -build ./OPENMUSIC/build/deliver.lisp`

`[path-to-lispworks-exe] -build ./OPENMUSIC/build/pack-om.lisp`

... where _[path-to-lispworks-exe]_  can be:
- `"/Applications/LispWorks 7.1 (64-bit).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin"` (macOS)
- `"C:\\Program Files (x86)\\LispWorks\\lispworks-6-1-0-x86-win32.exe"` (Windows)
- `"/usr/bin/lw", "/usr/local/bin/lw" ...` (Linux)

#### Deliver

The file **deliver.lisp** performs the following operations:

- Load OM sources (through build-om.lisp)
- Create an executable name using the variable **\*version\***, defined in **build-om.lisp**     
**This variable must therefore be set, in accordance to the current tag of the sources, prior to run the deliver script.**
- Setup and define the main application object
- _On macOS_: setup additional associated behaviors such as Dock-menu, system callback for double-clicks from the Finder, etc.
- Generate the HTML files of the online reference using `(om::gen-om-reference)` 
- Take care of some relative-path variables 
- Save the new Lisp executable image with startup calls.
- _On macOS_: move all the resources, source code, and external C libraries (.dylib) indisde the .app bundle (as required for macOS applications)

#### Pack

The file **pack-om.lisp** performs the following operations:

- Create a clean copy of the OPENMUSIC folder named after the name of the created executable (e.g. "OM 6.x")
Clean = removing all \*fasl files, temporary files, etc.
- _On Windows_: also move all dlls into the root folder, next to OM 6.x.exe

### Delivering for macOS 

- On macOS the delivered application is a standalone .app package (_OM 6.x.app_), which can be packed into a zip file or a DMG.

- The _Info.plist_ file inside the bundle should have the key ATSApplicationFontsPath set to "fonts/" :     
`defaults write OM 6.x.app/Contents/Info.plist/Info.plist ATSApplicationFontsPath -string "fonts/"`

- The .app and all the libraries included in OM 6.x.app/Contents/Frameworks/ should be **signed**.

The file **release** in this repository contains a bash script performing all previous operations, whith several option:

```
./release -d = deliver only
./release -p = pack + set font in Info.plist on a delivered app 
./release -dp = deliver and pack
./release -all = deliver/pack/sign/create zip and DMG
etc.
```

**Note:** the options `-sign` and `-all` require your own secondary code-signing script named `codesign-om6` and in this same folder.

### Delivering for Windows

- On Windows the delivered application is a cleaned-up/renamed copy of the **OPENMUSIC** folder as created by the "pack" process above, including the .exe generated by the **deliver** process, as well as all required dynamic labraries (.dll). 

The file `buils/win/release.bat` launches the two load-operations sequentially, starting up LispWorks twice with _deliver.lisp_ and _build-om.lisp_. _The path to LispWorks might need to be adapted in **release.bat**_. Then just double-click **release.bat**, or execute it from the system command prompt.

- The font files must still be installed in the target system.     
An install-maker tools can be used to perform the whole installation on user machines (create Desktop/Start menu short-cuts, install fonts...). See for instance [Install Creator](https://www.clickteam.com/install-creator-2).

### Delivering on Linux ###

 The file `build/linux/Makefile` is set up to build and deliver an OM image from the sources according to
 the comments above.

  Most things should be straightforward.  You may need to tweak the path to your lispworks executable, and ensure the
  resulting delivered OM-image file complies with the current version number in the sources etc.

  Along with the usual Makefile targets are also `"make dist"`, `"make distclean"` and `"make tar-dist"`.  Running
  `"make"`, then `"make dist"` will leave you with all necessary sources and image files inside $DISTDIR (usually
  `/tmp/OM/BUILD`)
