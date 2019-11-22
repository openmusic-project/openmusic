# Libraries

OpenMusic **libraries** contain extra tools dedicated to specific purposes.

A default folder for dynamically loadable libraries is created by OM:

- In OM 6.12 or earlier: within the OM application directory (_OM 6.xx/libraries/_)
- In OM 6.13 or newer: in your home directory (_<home>/OM/Libraries/_)


You can also add extra libraries folder and register them in OM:

- Create a directory "Libraries" (or any other name) somewhere in your computer _(but **not** in the OM directory!)_.
- Put your own libraries in this folder
- Register this directory in the OM Preferences ("Libraries" tab, **external user lib directories**).
- If necessary, use the menu item "File/Refresh Libraries Package..." on the Library window in order to update the libraries list. 

> **Notes:** Libraries can also be loaded as "Remote User Libs" (see "Library" window, Menu "File").

**Want to write your own library?** 
-  See ["Writing user libraries"](./dev-resources/userlib) in the [Developer resources section](./dev-resources/index) for instructions about how to write your own OM library.
- **Contact the project admins to get your OM project/library referenced on in this page !**

------
## Catalog

Most OpenMusic libraries can be found and downloaded from the [https://github.com/openmusic-project/](OpenMusic-project).

#### "Standard" libraries: 

|[Esquisse](https://github.com/openmusic-project/esquisse/releases/latest)|Spectral music / intervallic manipulation tools|
|[LZ](https://github.com/openmusic-project/lz/releases/latest)|Automatic Modeling of Musical Style|
|[RepMus](https://github.com/openmusic-project/repmus/releases/latest)|The lengendary library from the Music Representations team |
|[Profile](https://github.com/openmusic-project/profile/releases/latest)|Library for the Control of Melodic Profiles |
|[Alea](https://github.com/openmusic-project/alea/releases/latest)|Aleatoric tools|
|[Chaos](https://github.com/openmusic-project/chaos/releases/latest)|Dynamic systems library|
|[Situation](https://github.com/openmusic-project/situation/releases/latest)|Constraint programming|
|[Clouds](https://github.com/openmusic-project/clouds/releases/latest)|Constraint programming|

#### Sound synthesis and spatialization:

|[OM2Csound](https://github.com/openmusic-project/OM2Csound/releases/latest) | Generation of Csound scores and orchestra. External control of Csound synthesis. | 
|[OMChroma](https://github.com/openmusic-project/OMChroma/releases) | High-level control of sound synthesis using Csound (requires OM2Csound). | 
|[OMPrisma](https://sourceforge.net/projects/omprisma/) | Control of sound spatialization and spatial sound synthesis (requires OMChroma and OM2Csound). |

#### Connection to IRCAM Sound synthesis and spatialization technology:

|[OM-SuperVP](https://github.com/openmusic-project/OM-SuperVP/releases) | Control of the SuperVP phase vocoder for sound analysis and processing.<br> _See also [this release](https://forum.ircam.fr/projects/detail/om-supervp/) (with Ircam Forum subscription) including SuperVP kernel_|
|[OM-pm2](https://github.com/openmusic-project/OM-pm2/releases) | Control of the pm2 engine for sound partial-tracking analysis and additive synthesis. <br> _See also [this release](https://forum.ircam.fr/projects/detail/om-pm2/) (with Ircam Forum subscription) including pm2 kernel_ |
|[OM-Diph](https://github.com/openmusic-project/OM-Diph/releases) | Generation and execution of Diphone scripts for sound analysis and concatenative synthesis. |
|[OM-Chant](https://github.com/openmusic-project/OM-Chant/releases) | Control of the Chant syntehsizer. |
|[OM-Spat](https://github.com/openmusic-project/OM-Spat/releases) | Generation of spatial sound scenes and spatialization using an offline Spat rendering engine. |


#### Additional, freely-distributed and/or third party libraries hosted on openmusic-project:

|**Library** | **Description** | **Author(s)** | 
|[OMLily](https://github.com/openmusic-project/omlily3)	|Converts OM objects (chord-seqs, multi-seqs, voices, polys) into lilypond files to generate pdf printable scores. 		|Karim Haddad|
|[Patterns](https://github.com/openmusic-project/patterns/releases/latest) | An adaption to OpenMusic of Rick Taube's "Pattern Streams".|Anders Vinjar, 2017|
|[Streamsep](https://github.com/openmusic-project/streamsep/releases/latest) | Algorithms for voice separation.|Anders Vinjar, 2017|
|[RQ](https://github.com/openmusic-project/RQ/releases) | A library for rhythm quantification|Adrien Ycart, 2016|
|[OM-ModTile](https://github.com/openmusic-project/om-modtile/releases/latest) | A library for creating, modifying, visualizing and experimenting with modulo 2 compact rhythmic canons in OpenMusic.|Hélianthe Caure, 2016.|
|[Morphologie](https://github.com/openmusic-project/morphologie/releases/latest) | Tools for the analysis and generation of musical sequences based on the idea of ["contrastive analysis"](http://www.fredvoisin.com/web/spip.php?article28)  |Jacopo Baboni Schilingi and Fred Voisin, Ircam, 1998|
|[OM_ASX](https://github.com/openmusic-project/om_asx/releases/latest) | Tools for the generation of parameter files for AudioSculpt, SuperVP and/or OM-SuperVP|Hans Tuschku, Ircam, 1998|
|[Combine](https://github.com/openmusic-project/combine/releases/latest) | Manipulations combinatoires inspirées(?) dans et par la technique compositionnelle de Brian Ferneyhough. |Mikhaïl Malt, Ircam, 2010|
|[OMCS](https://github.com/openmusic-project/omcs/releases/latest)|A preliminary release of the _pmc_ engine from PWConstraints by Mikael Laurson. |Mikaël Laurson, Orjan Sandred, 1995-1999|
|[OMRC](https://github.com/openmusic-project/omrc/releases/latest)|Rhytmic constraints library. |Orjan Sandred, 1999-2002|
|[OMPitchField](https://github.com/openmusic-project/ompitchfield/releases/latest)|   |Paul Nauert, 2001-2006|
|[OMTimePack](https://github.com/openmusic-project/omtimepack/releases/latest) |  |Paul Nauert, 2001-2005|
|[Pareto](https://github.com/openmusic-project/pareto/releases/latest) | Patchs d'Analyse et de Resynthèse des Echelles dans les musiques de Tradition Orale: A set of tools for the analysis and resynthesis of scales from musical signals. |Original patches by Fabien Lévy, 2001. library update by Jean Bresson and Fabien Lévy, 2009|
|[Filters](https://github.com/openmusic-project/filters/releases/latest) | Filter functions (low pass, median...).	|Mikhaïl Malt, 2005|
|[OMLempelText](https://github.com/openmusic-project/OMLempelText/releases/latest) | Applies LZ principles (cf. LZ library) with texts and textfiles. |Gérard Assayag, 2001|
|[Rewrite](https://github.com/openmusic-project/rewrite/releases/latest) | Manipulation and rewriting rules applied on rhythm trees, using a special syntax.|Pierre Donat-Bouillud, Adrien Ycart, 2013-2015|
|[OMTristan](https://github.com/openmusic-project/omtristan/releases/latest)| Spectral and other frequency-based calculation objects developed by Tristan Murail. Rozalie Hirs' essays on the music and compositional techniques of Tristan Murail (R. Hirs, B. Gilmore, eds, _Contemporary compositional techniques and OpenMusic_, Collection Musique /Sciences, IRCAM / Editions Delatour, Paris, 2009) can serve as an introduction. => http://www.rozaliehirs.com/essay-and-software/ |Tristan Murail, ported to OM by Rozalie Hirs, 2009|
|[OM-Orchidee](https://github.com/openmusic-project/om-orchidee/) | A client for the Orchidee orchestration server. Contains classes and editor for the specification of orchestration problems (sound target, orchestra), communication tools with the server, and objects for importing and manipulating the  results in OM.| J. Bresson, 2010-2011|
|[OMIanniX](https://github.com/openmusic-project/om-iannix/) | Creates IanniX scores from OM objects. |Jean Bresson, 2005|
|[OM-Faust](https://github.com/openmusic-project/om-faust/) |A library for writing/compiling/controlling Faust synthesizers and sound effects in OpenMusic. For OM 6.7 / MacOSX only.|Dimitri Bouche, 2013.|



#### Other external OM libraries and projects:

|[Pixels](https://github.com/j-bresson/pixels/releases/latest) | Manipulation and generation of pictures and pixel arrays. |Jean Bresson, Ircam, 2010|
| [Tessellate](http://cnmat.berkeley.edu/projects/tessellate-cnmat-om-openmusic-library) | Generation, analysis, filtering, and mapping of rhythmic and pitch structures. | CNMAT: Matt Schumaker, Ed Campion | 
| [OMCollider](https://pwcsound.jimdo.com/download-omcollider/) | A library for the control of sound synthesis with SuperCollider. Contains a set of all unit generators (Ugen) and allows to generate and run “.scd” files, using OM graphic programming tools. |Giorgio Zucco|
| [FDSDB_XXth_CT](https://sites.google.com/site/fdsdbmascagnienglishversion/code/fdsdb_xxth_ct-for-open-music) | XXth Century Composition Techniques. | Fabio De Sanctis De Benedictis | 
| [OMMaxtrix](https://www.pablocetta.com/aplicaciones_ca.php) | PCS and combinatorial matrices | Pablo Cetta |
|[OMGecode](https://github.com/slemouton/gecodeMCP)|An OpenMusic library to solve Michael Jarrell musical constraint problems using Gecode, and other such musical melodic harmonic or contrapuntal challenges. (compatible with GeCode 6.1) |Serge Lemouton, Ircam, 2010|
| [OMRuben](http://sourceforge.net/projects/omruben/) | A library focused on musical scores, dealing with rhythm quantification, filterings, pitch shifting, time scaling, time pointer, orchestral envelopes, approximation, score conversions, gesture composition and various new compositional techniques.|Ruben sverre Gjertsen|
|[OM-SoX](http://sourceforge.net/projects/omsox/) |Multichannel audio manipulation and batch processing for OpenMusic (uses the SoundeXchange sound processing kernel). |Marlon Schumacher, CIRMMT/McGill University|
|[Harmonic-Analysis](http://grfia.dlsi.ua.es/cm/projects/drims/software.php) |A library for the (semi-supervised) harmonic analysis of chord-sequences in OM 6.6.|Carlos Pérez Sancho, DLSI, Univ. Alicante|
|[OM-Pursuit](http://www.idmil.org/software/om-pursuit)|Dictionary-Based Sound Models for Computer-Aided Composition.|Marlon Schumacher, CIRMMT/McGill University|
|[chant-lib](https://ccrma.stanford.edu/~rmichon/chant-lib/chant-lib.html) |Implementation of the CHANT synthesizer (singing voice synthesis by FOF) using Csound/OM2Csound, inpired by the PW-Chant library for Patchwork.|Romain Michon|
|[SOAL](http://www.ccta.ufpb.br/mus3/index.php?option=com_content&view=article&id=7&Itemid=5) |Sonic Object Analysis Library: a collection of tools which scan MIDI files and return statistical informations about their achronic (vertical, harmonic) and diachronic (horizontal, time-related) structures. 	|Mus3|
|[FV-Morphologie](http://www.fredvoisin.com/web/spip.php?article113)|Lisp tools to analyse sequences of symbols or signs wich represent music... 		|Fred Voisin|
|[OMPW](http://kiliansprotte.de/perm/ompw.html)|Abstraction layer for writing Lisp libraries that can be used in "pure" Common Lisp, but also in the OM or PWGL environments 		|Kilian Sprotte|
|[GeLisp/OMGelisp](http://sourceforge.net/projects/gelisp/) |A portable and efficient wrapper for the Generic Constraints Development Environment (GECODE) library to Common Lisp. Provides a high level interface and a low level interface. Additionally, it provides an interface to OpenMusic. |Mauricio Toro Bermudez and Camilo Rueda|
|[COMUS](http://www.ufjf.br/comus/)|A collection of OM patches for image-to-sound conversion using several color systems (RGB, HSV, CMYK) and OM functions (BPF, BPC, 3DC). |Luiz E. Castelões|
|[OM-Geste](https://github.com/marleynoe/OM-Geste)| Library for representation, processing and mapping of gesture data| Marlon Schumacher |
|[OM-Darwin](https://github.com/geofholbrook/om-darwin)| Genetic algorithm library | Geof Holbrook |
|[SDIF-Edit4OM](https://github.com/j-bresson/SDIF-Edit/tree/master/OM) |A little "hack" to use SDIF-Edit instead of the default OM SDIF editor -- see https://github.com/j-bresson/SDIF-Edit|Jean Bresson, 2011|


<br>

|**[R-udp-player](https://sourceforge.net/projects/r-udp-player/)** | **A microtonal player to use with OM score objects' "OSC player" option.** | Ruben sverre Gjertsen | 


