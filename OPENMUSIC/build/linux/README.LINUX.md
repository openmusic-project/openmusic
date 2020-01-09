# WELCOME TO OPENMUSIC FOR LINUX

This README is about OpenMusic on Linux

## OMs HOMEPAGE: <https://github.com/openmusic-project/openmusic/>


## INSTALLATION:

Download installable packages for OM from:

<https://github.com/openmusic-project/openmusic/releases>

Packages are available for Fedora and Ubuntu distros, pick the one
which fits your distro's package-system (ie. RPM for Fedora, DEB for
Ubuntu):

openmusic-x.xx-x.x86_64.rpm
openmusic-x.xx-x.amd64.deb

There's usually a tar-ball of the latest release:

openmusic-x.xx-x.x86_64.tar.xz (or similar)

which can be extracted to a directory of choice.  You should be able
to run OM directly from within this directory without installing
anything.


## AUDIO AND MIDI output: OMAudioLib.so

OM comes with a JUCE based Audio layer - OMAudioLib - to connect to
audio I/O.  After startup, check in the "Preferences->Audio" tab to
select among available devices.

Each new release of OM includes a precompiled version of
OMAudioLib.so, installed at the expected place:

$PREFIX/share/openmusic/resources/lib/linux/OMAudioLib.so

If you for some reason need to compile your own, omaudiolib is
available from github:

<https://github.com/openmusic-project/omaudiolib>

To compile OMAudioLib.so clone the repo or download the sources, and
follow the instructions in the README file.

There's no native support for JACK in OM.  To use OM (or any Pulse
based or ALSA app) with JACK you may set up pulseaudio-jack-sink, as
explained in various guides lying around on the internet.

For MIDI in/out OM uses portmidi.  One good choice for an external
softsynth on Linux is fluidsynth together with a good soundfont:

dnf install fluidsynth
dnf install fluid-soundfont-gm

To connect OM to your midi-synth of choice, go to the MIDI-tab in OMs
preferences, and click the "ports"-button in the left section, from
where you can connect OM to one or more of your chosen devices.

## FURTHER DEPENDENCIES:

OM for Linux depends on some standard libs:

portmidi
libsndfile
libsamplerate

If you install OM through your package-system (ie. 'dnf' on Fedora,
'software-installer', 'dpkg' or 'apt-get' on Ubuntu), all dependencies
should be handled automatically.


## FORUM, BUG-REPORTS, ISSUES:

Please send bug reports!

Theres an OpenMusic forum/wiki at
<https://discussion.forum.ircam.fr/c/openmusic> - where subscribed
members can post questions, send bug reports etc.


## WORKING WITH OM CODE IN LISPWORKS

If you want to work with OM sources in LispWorks, check/download the
latest released source package from the repository:

$ <https://github.com/openmusic-project/openmusic/releases/latest>

or clone the git repo:

$ git clone <https://github.com/openmusic-project/openmusic>

