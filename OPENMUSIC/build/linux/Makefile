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
IMAGENAME	= OM_6.13

path_to_omroot	= ../..
thisfile	= $(lastword $(MAKEFILE_LIST))
thisdir		= $(dir $(thisfile))
builddir	= $(shell cd $(dir $(thisfile)) && cd $(path_to_omroot) && pwd)
BUILDROOT	= $(builddir)

PREFIX		= /usr/local
APPNAME		= openmusic

# bindir		= $(DESTDIR)$(PREFIX)/bin
# datadir		= $(DESTDIR)$(PREFIX)/share
# omroot		= $(datadir)/$(APPNAME)

faslname	= 64ufasl
rubbish		= *.$(faslname)

compress	= gzip
rsyncflags	= -v -rlt -z -C --include '*.so'
exclude_fasl	= --include 'dspec-database.$(faslname)' --exclude '$(rubbish)'

exclude_lib	= --exclude 'resources/lib/mac/' --exclude 'resources/lib/win/'

checkname	= $(shell ls $(BUILDROOT) | grep -e "OM_6.*")
ifdef checkname
	buildname = $(checkname)
else
	buildname = $(IMAGENAME)
endif

compress	= tar
compressfile	= $(buildname).tar.bz2
compressflags	= -cjf
# compressfile	= $(buildname).tar.xz
# compressflags	= -cJf


release		= $(DISTDIR)/$(buildname)

all:	compile


compile build-from-git: 
	@echo building "$(buildname)" in source tree: "$(BUILDROOT)"
	$(LISP) $(BUILDROOT)/build/deliver.lisp

clean:
	find $(BUILDROOT) -name $(rubbish) -delete
	if [ -f $(BUILDROOT)/$(buildname) ]; then rm $(BUILDROOT)/$(buildname); fi;

dist:   
	rm -rf $(release) && mkdir -p $(release)
	rsync $(rsyncflags) $(exclude_fasl) $(exclude_lib) --exclude '*.finderinfo' --exclude 'Makefile' $(BUILDROOT)/ $(release)
	find $(release)/build/ -not -iname '*build*' -and -not -path '*linux*' -delete
	cp README.LINUX $(release)/
	cp NEWS $(release)/
	cp Makefile.dist $(release)/Makefile

tar-dist: dist
	cd $(shell dirname $(release)) && $(compress) $(compressflags) $(compressfile) $(buildname) 

distclean:
	rm -rf $(release)

# install: $(release)/$(buildname)
# 	cd $(release) && $(INSTALL_PROGRAM) -m 0755 $(buildname) $(BINDIR)/$(APPNAME)
# 	cd $(release) && rsync $(rsyncflags) $(exclude_fasl) --exclude 'OM_6.8*' . $(omroot)


# uninstall:
# 	rm -rfv $(omroot)
# 	rm -f $(BINDIR)/$(APPNAME)

help:
	@echo bygger $(BUILDROOT)/$(buildname)
	@echo Makefile:		$(thisfile)
	@echo THISDIR:		$(thisdir)
	@echo BUILDROOT:	$(BUILDROOT)
	@echo buildname:	$(buildname)
	@echo IMAGENAME:	$(IMAGENAME)
	@echo release:		$(release)

