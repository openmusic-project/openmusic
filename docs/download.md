
# Download and Installation

**Get the latest OM6 release here:** [https://github.com/openmusic-project/OM6/releases](https://github.com/openmusic-project/OM6/releases)


Please check back every now and then for new builds, fixes and new features -- and be sure to follow announcements on the various forums.

Please direct bug-reports, questions and feedback to the [discussion forum](https://discussion.forum.ircam.fr/c/openmusic).

### License and sources distribution

OM sources are free and [available](https://github.com/openmusic-project/OM6/) under the GNU Public License (GPL). 
They can be compiled on compatible Lisp platforms using [LispWorks](http://www.lispworks.com/), for which IRCAM owns professional distribution license. Currently LispWork provides no free personal version of LW 7.1 required to compile OM.



## macOS

The distributed release can be installed in the standard _Applications_ folder.

Multiple versions of OM are allowed (installation will not override previous versions).

OM relies on a number of external libraries that are all included in the application bundle (_OM 6x.x/Contents/Frameworks/_).


> **Known issue with MacOS 10.10 (Yosemite):** if you get a message like _Foreign function libsndfile::\|%cffi-foreign-function/SF_OPEN\| trying to call unresolved external function "sf_open"_ when trying to load a sound file in OM, you need to open your OM 6.xx.app package (right-click + open package folder) and replace [libsndfile.dylib](http://repmus.ircam.fr/_media/openmusic/fixpatches/libsndfile.dylib.zip) in _Contents/Frameworks/_.


## Windows 

The distributed installer will create an OpenMusic folder (OM x.x) in _C:\\Program Files (x86)\\_.       
Musical fonts will be installed in _C:\\WINDOWS\\Fonts\\_.

OM relies on a number of external libraries that are all included as .dll files in the application root directory.


> **Note:** If an error message appears at launching OM on windows indicating that _MSVCR_ libraies are missing, download and install the [Microsoft Visual C++ Redistributable Package](http://www.microsoft.com/downloads/en/details.aspx?FamilyID=a7b7a05e-6de6-4d3a-a423-37bf0912db84).


## Linux 


Packages are available as .rpm or .deb. 
Most users should be able to just double-click on the provided packages, and leave your distro's package-maintainer (ie: dnf or dpkg) to take care of all necessary dependencies. (Update 2017-03-15: the SDIF and OM fonts package are no longer needed, as these are now installed together with the rest of the application.)

There's also a tarball (.tar.bz2) available.  After downloading and unpacking, OM can be run from inside the extracted folder, or be installed using the supplied Makefile.  
OM will look for its fonts (the omfonts package) at the usual places on your system. 

Since january 2017 OM is a 64-bit application.  The previous 32-bit version is no longer maintained.


 