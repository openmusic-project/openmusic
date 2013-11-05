# Makefile for OpenMusic	-*-Makefile-*-
# Copyright (C) 2013 anders anders.vinjar@bek.no

SHELL		= /bin/sh

APPNAME		= openmusic

PREFIX		= /usr/local
BINDIR		= $(PREFIX)/bin
DATADIR		= $(PREFIX)/share

# should reflect *om-root* while -build with delivery.lisp:
OMROOT		= $(DATADIR)/$(APPNAME)

MAKE		= make
COMPRESS	= gzip
RSYNCFLAGS	= -v -rlt -z -C --include '*.so'
EXCLUDE_FASL	= --include 'dspec-database.ufasl' --exclude '*.ufasl' 

# APP = $(shell ls $(SVN_TRUNK_ROOT) | grep "OM_")
DISTAPPNAME	= $(APPNAME)

COMPRESS	= tar
COMPRESSFILE	= $(DISTAPPNAME).tar.bz2
COMPRESSFLAGS	= -cjf $(COMPRESSFILE)

INSTALL_PROGRAM	= install


SVN_TRUNK_ROOT	= ../../..

SVNAPPNAME	= $(shell ls $(SVN_TRUNK_ROOT) | grep "OM_")
ifdef $(SVNAPPNAME)
SVNAPPNAME	= $(SVNAPPNAME)
else
SVNAPPNAME	= OM_6.7_beta_4
endif


# RELEASE		= $(HOME)/site/OM/BUILD/$(SVNAPPNAME)
RELEASE		= /tmp/OM/BUILD/$(SVNAPPNAME)

LISP		= $(HOME)/bin/lw
LISPFLAGS	= -build

RUBBISH	= *.ufasl

$(SVN_TRUNK_ROOT)/$(SVNAPPNAME):
	@echo building $(RELEASE) for user $(USER)
	@echo building "$(SVNAPPNAME)" in source tree: "$(SVN_TRUNK_ROOT)"
	$(LISP) --omroot $(OMROOT) $(LISPFLAGS) $(SVN_TRUNK_ROOT)/buildimage/build-om/deliver.lisp

buildsvn: $(SVN_TRUNK_ROOT)/$(SVNAPPNAME)

buildimage:
	@echo building "$(RELEASE)/$(DISTAPPNAME)"
	$(RELEASE)/$(DISTAPPNAME)

cleansvn:
	find $(SVN_TRUNK_ROOT) -name $(RUBBISH) -delete
	rm $(SVN_TRUNK_ROOT)/$(SVNAPPNAME)

clean:  cleansvn

buildsvn: $(SVN_TRUNK_ROOT)/$(SVNAPPNAME)

dist:   
	@echo -e "setting up dist dir: " $(RELEASE)
	rm -rf $(RELEASE) && mkdir -p $(RELEASE)
	@echo -e "syncing " $(SVN_TRUNK_ROOT) "into dist dir: " $(RELEASE)
	rsync $(RSYNCFLAGS) $(EXCLUDE_FASL) $(SVN_TRUNK_ROOT) $(RELEASE)
	@echo -e "removing non-dists from buildimage/build-om:"
	find $(RELEASE)/buildimage/build-om/ -not -iname '*build*' -and -not -path '*linux*' -delete
	cp README.LINUX $(RELEASE)/

tar.bz2:	dist
	@echo -e "tar and bzip2'ing:"
	cd $(shell dirname $(RELEASE)) && $(COMPRESS) $(COMPRESSFLAGS) $(SVNAPPNAME) 
	@echo -e "done!"

distclean:
	rm -rf $(RELEASE)

install: $(RELEASE)/$(SVNAPPNAME)
	@echo -e install $(SVNAPPNAME) $(BINDIR)/$(APPNAME)
	cd $(RELEASE) && $(INSTALL_PROGRAM) -m 0755 $(SVNAPPNAME) $(BINDIR)/$(APPNAME)
	@echo -e installing source-files
	cd $(RELEASE) && rsync $(RSYNCFLAGS) $(EXCLUDE_FASL) --exclude '*.finderinfo' --exclude 'OM_6.7*' . $(OMROOT)


uninstall:
	@echo -e removing .lisp source-files from $(OMROOT)
	rm -rfv $(OMROOT)
	@echo -e removing binary $(BINDIR)/$(APPNAME)
	rm -f $(BINDIR)/$(APPNAME)
