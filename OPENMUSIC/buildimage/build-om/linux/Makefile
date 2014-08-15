# Makefile for OpenMusic 	-*-Makefile-*-
# Copyright (C) 2013 anders anders DOT vinjar AT bek DOT no
#
# this Makefile is included in OpenMusics git-tree.  Use it to compile
# sources/build image + set up a dist (tar.bz2)
#
# install/uninstall targets are intended to be handled by the Makefile
# packed at top-level in the dist
# 

SHELL		= /bin/sh

LISPFLAGS	= -build
LISP		= $(HOME)/bin/lw $(LISPFLAGS)

INSTALL_PROGRAM	= install

DISTDIR		= /tmp/OM/BUILD
SVNAPPNAME	= OM_6.7_beta_4

path_to_omroot	= ../../..
thisfile	= $(lastword $(MAKEFILE_LIST))
thisdir		= $(dir $(thisfile))
builddir	= $(shell cd $(dir $(thisfile)) && cd $(path_to_omroot) && pwd)
BUILDROOT	= $(builddir)

PREFIX		= /usr/local
APPNAME		= openmusic

bindir		= $(DESTDIR)$(PREFIX)/bin
datadir		= $(DESTDIR)$(PREFIX)/share

omroot		= $(datadir)/$(APPNAME)

faslname	= ufasl
rubbish		= *.$(faslname)

compress	= gzip
rsyncflags	= -v -rlt -z -C --include '*.so'
exclude_fasl	= --include 'dspec-database.ufasl' --exclude '$(rubbish)'


checkname	= $(shell ls $(BUILDROOT) | grep "OM_")
ifdef checkname
	svnappname = $(checkname)
else
	svnappname = $(SVNAPPNAME)
endif

compress	= tar
compressfile	= $(APPNAME).tar.bz2
compressfile	= $(svnappname).tar.bz2
compressflags	= -cjf


release		= $(DISTDIR)/$(svnappname)

all:	compile


compile buildsvn: 
	@echo building "$(svnappname)" in source tree: "$(BUILDROOT)"
	$(LISP) $(BUILDROOT)/buildimage/build-om/deliver.lisp

clean:
	find $(BUILDROOT) -name $(rubbish) -delete
	if [ -f $(BUILDROOT)/$(svnappname) ]; then rm $(BUILDROOT)/$(svnappname); fi;

dist:   
	rm -rf $(release) && mkdir -p $(release)
	rsync $(rsyncflags) $(exclude_fasl) --exclude '*.finderinfo' --exclude 'Makefile' $(BUILDROOT)/ $(release)
	find $(release)/buildimage/build-om/ -not -iname '*build*' -and -not -path '*linux*' -delete
	cp README.LINUX $(release)/
	cp Makefile.dist $(release)/Makefile

tar-dist: dist
	cd $(shell dirname $(release)) && $(compress) $(compressflags) $(compressfile) $(svnappname) 

distclean:
	rm -rf $(release)

# install: $(release)/$(svnappname)
# 	cd $(release) && $(INSTALL_PROGRAM) -m 0755 $(svnappname) $(BINDIR)/$(APPNAME)
# 	cd $(release) && rsync $(rsyncflags) $(exclude_fasl) --exclude 'OM_6.8*' . $(omroot)


# uninstall:
# 	rm -rfv $(omroot)
# 	rm -f $(BINDIR)/$(APPNAME)

help:
	@echo bygger $(BUILDROOT)/$(svnappname)
	@echo Makefile:		$(thisfile)
	@echo THISDIR:		$(thisdir)
	@echo BUILDROOT:	$(BUILDROOT)
	@echo svnappname:	$(svnappname)
	@echo SVNAPPNAME:	$(SVNAPPNAME)
	@echo release:		$(release)

