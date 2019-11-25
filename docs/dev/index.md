# Developer corner

OpenMusic is based on the Common Lisp programming language. Creating an executable in Common Lisp means loading all the source code in the Lisp environment and build an "image" of it, i.e. another Lisp environment extended with the features defined in the code.
It is also possible to compile and load OM source code in the Lisp environment and use it directly without saving a new image. Using OM sources therefore requires owning a Lisp compiler.

OM 6 is currently developed with LispWorks 7.1.1 and relies on the graphical and GUI toolkits provided by this commercial Lisp environment. A free (limited) edition of LW6 is available on the [LispWorks](http://www.lispworks.com/) website, but unfortunately no free version of LW 7 at the moment.  

The current OM sources can be compiled and run OM on **macOS**, **Windows** and **Linux** (see compilation instructions below).


<img src="images/lisp.jpg" width="90px" align="right">

## Compilation with LispWorks

Here are the successive steps to follow:

- Download the latest OM sources.
- Install OM Fonts and external dependencies (see the note and/or corresponding sections below)
- Launch LispWorks
- Load the file //OM 6.x.x/build/build-om.lisp//
- Evaluate: `(om::START-OPENMUSIC)`

.. and you're (almost) done (see further precisions below).

The LispWorks Personal Edition has a limited heap size, which might be exceeded just by compiling the full OM sources. In this case, LispWorks will quit without finishing the compilation, but next time part of the code will already be compiled. After two or three times, the OM code is compiled and OM can be run as in LispWorks Pro (but still with a limited heap size).

A solution to limit the heap occupied by OM is not to load all the OM projects and components.

The "projects" are specific thematic packages included in the OM base distribution.
When compiling OM, you can decide to include or not the different OM projects.
In the _build-om.lisp_ file, you will find lines staring with `(load-om-projects ...) that can be commented if you don't want to load some specific projects project. This might sensibly reduce the memory and heap size consumption of OM.

LispWorks Personal has also a time limit of 5 hours for each session, after which it will exit as well (but still can be restarted).


**See the [build instructions in the OM source repository](https://github.com/openmusic-project/OM6/) for further information on external dependencies; fonts installation, etc.**


## The OM Directory

The OM directory contains the following sub-folders:

- **code**: The OM source code. Divided in 3 parts :
  - **api** : The low level programming interface, implemented separately for the different compatible CL implementations (currently LispWorks only for OM 6).
  -  **kernel** : In the OM kernel is implemented the core visual programming environment. Classes and methods for the language meta-objects are defined (i.e. OMPatch, OMClass, OMBox, etc.), as well as their representations either as simple icons or as container. Meta-objects for the OM environment (OMFolder, OMWorkspace, OMLib, etc.) are also defined with graphic manipulations for creating or modifying them. Default graphic control structures as _omloop_, _omif_, or logic predicates and tools are also predefined in the OM kernel.
  - **projects** : Each "project" is a specialized set of classes and methods that can be accessible and visualisable in the OM environment. 

- **build** : Contains the files and resources needed to build the image of OM.
This folder is also divided in sub-folders corresponding to the different available platforms.

- **init** : Any file in this folder is loaded at OM startup. We provide minor revisions between releases in order to add bug corrections or code. OM users can put these 'patches' in this folder.

- **resources** : This folder contains the OM resources (icons, pictures, cursors, online help, etc.)



## Developer Resources

Here are some resources and links for basic or more advanced developement in OpenMusic.

- **[OM Architecture](architecture)**    

- **[OM Class protocol](classprotocol)**    

- **[Box evaluation](evaluation)**    

- **[Containers and simple containers](containers)**     


- **[Writing code for OM](codeforom)**

- **[Writing user libraries](userlib)**

- **[Integrating analysis and segmentations in OM score objects](analysis)**



