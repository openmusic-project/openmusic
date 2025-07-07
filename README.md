# OpenMusic

OpenMusic (OM) is a visual programming language based on [Lisp](http://www.gigamonkeys.com/book/introduction-why-lisp.html). Visual programs are created by assembling and connecting icons representing functions and data structures. Most programming and operations are performed by dragging an icon from a particular place and dropping it to an other place. Built-in visual control structures (e.g. loops) are provided, that interface with Lisp ones.

OM may be used as a general purpose functional/object/visual programming language. At a more specialized level, a set of provided classes and libraries make it a very convenient environment for music composition. Above the OpenMusic kernel, live the OpenMusic Projects. A project is a specialized set of classes and methods written in Lisp, accessible and visualisable in the OM environment. Various classes implementing musical data / behaviour are provided. They are associated with graphical editors and may be extended by the user to meet specific needs. Different representations of a musical process are handled, among which common notation, midi piano-roll, sound signal. High level in-time organisation of the music material is proposed through the concept of "maquette".

Existing CommonLisp/CLOS code can easily be used in OM, and new code can be developed in a visual way.

- [OpenMusic project pages](http://openmusic-project.github.io/)
- [OpenMusic User Manual](https://openmusic-project.github.io/openmusic/doc/om-manual/OM-Documentation)


---------

Designed and developed by the IRCAM [Music Representation research group](http://repmus.ircam.fr)

© 1998 - 2022 Carlos Agon, Gérard Assayag, Jean Bresson, Karim Haddad.


## Sources and Licensing

OpenMusic is a free software distributed under the GPLv3 license. As a Common Lisp program, the environment can be considered just as an extension of Lisp including the specific built-in features of the application. 

While the sources of OM are available under the GPL license, the application is developed with [LispWorks](http://www.lispworks.com/): a commercial Lisp environment providing multiplatform support and graphical/GUI toolkits. Also available a free (limited) edition of LW7 on the LispWorks website.

See the [Build Instructions](./BUILD.md) for how to compile, load and deliver OM using LispWorks 7 or 8. 

In order to contribute to the code without a LispWorks license, one must therefore work both with the cloned source package _and_ an up-to-date released version on OM (which includes a Lisp interpreter).


