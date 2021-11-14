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
LISP		= $(HOME)/bin/lw-console $(LISPFLAGS)

INSTALL_PROGRAM	= install

DISTDIR		= /tmp/OM/BUILD
IMAGENAME	= OM_6.20

path_to_omroot	= ../..
thisfile	= $(lastword $(MAKEFILE_LIST))
thisdir		= $(dir $(thisfile))
builddir	= $(shell cd $(dir $(thisfile)) && cd $(path_to_omroot) && pwd)
BUILDROOT	= $(builddir)

PREFIX		= /usr/local
APPNAME		= openmusic

bindir		= $(DESTDIR)$(PREFIX)/bin
datadir		= $(DESTDIR)$(PREFIX)/share
omroot		= $(datadir)/$(APPNAME)

faslname	= 64ufasl
rubbish		= *.$(faslname)

compress	= gzip
rsyncflags	= -v -rlt -z -C --include '*.so'
exclude_fasl	= --include 'dspec-database.$(faslname)' --exclude '$(rubbish)'

exclude_lib	= --exclude 'resources/lib/mac/' --exclude 'resources/lib/win/'

imgfound	= $(shell find $(BUILDROOT) -maxdepth 1 -perm -111 -type f | grep -e "OM")

ifdef $(imgfound)
	buildname = $(shell basename $(checkname))
else
	buildname = $(IMAGENAME)
endif

compress	= tar
compressfile	= $(buildname).tar.bz2
compressflags	= -cjf


release		= $(DISTDIR)/$(buildname)

notarget:
	@echo targets: OM, clean, tar-dist, dist, install, uninstall, help
	@echo BUILDDIR: $(BUILDROOT)
	@echo checkname: $(checkname)


OM:	compile

compile:
	@echo building "$(buildname)" in source tree: "$(BUILDROOT)"
	$(LISP) $(BUILDROOT)/build/deliver.lisp

clean:
	find $(BUILDROOT) -name $(rubbish) -delete
	if [ -f $(BUILDROOT)/$(buildname) ]; then rm $(BUILDROOT)/$(buildname); fi;

dist:   
	rm -rf $(release) && mkdir -p $(release)
	rsync $(rsyncflags) $(exclude_fasl) $(exclude_lib) --exclude '*.finderinfo' --exclude 'Makefile' $(BUILDROOT)/ $(release)
	find $(release)/build -not -iname '*build*' -and -not -path '*linux*' -delete
	cp $(release)/build/linux/README.LINUX.md $(release)
	cp $(BUILDROOT)/../LICENSE $(release)
	cp $(release)/build/linux/NEWS $(release)
	cp $(release)/build/linux/Makefile.dist $(release)/Makefile

tar-dist: 
	cd $(shell dirname $(release)) && $(compress) $(compressflags) $(compressfile) $(buildname) 

distclean:
	rm -rf $(release)

install: $(release)/$(buildname)
	cd $(BUILDROOT) && $(INSTALL_PROGRAM) -m 0755 $(buildname) $(bindir)/$(IMAGENAME)
	ln -sf $(bindir)/$(IMAGENAME) $(bindir)/openmusic
	cd $(BUILDROOT) && rsync $(rsyncflags) $(exclude_fasl) $(exclude_lib) --exclude '*.finderinfo' --exclude 'Makefile' . $(omroot)
	cp -p $(BUILDROOT)/build/linux/README.LINUX.md $(omroot)
	cp -p $(BUILDROOT)/../LICENSE $(omroot)
	cp -p $(BUILDROOT)/build/linux/NEWS $(omroot)



uninstall:
ifdef IMAGENAME
	rm -f $(bindir)/$(IMAGENAME)
	rm -f $(bindir)/openmusic
endif
ifdef omroot
	rm -rf $(omroot)
endif



help:
	@echo targets: all '(=install)', clean, install, uninstall, tar-dist, distclean, help
	@echo PREFIX:		$(PREFIX)
	@echo install bindir:	$(bindir)
	@echo install datadir:	$(datadir)
	@echo Installed IMAGENAME:	$(bindir)/$(IMAGENAME) - LINKED TO:	$(bindir)/$(APPNAME)
	@echo Makefile:		$(shell pwd)/$(thisfile)
	@echo BUILDROOT:	$(BUILDROOT)
	@echo release:		$(release)
	@echo checkname:	$(checkname)
	@echo buildname:	$(buildname)
	@echo DISTDIR:		$(DISTDIR)
	@echo compressfile:	$(compressfile)
