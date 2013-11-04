# Makefile for OpenMusic	-*-Makefile-*-
# Copyright (C) 2013 anders anders.vinjar@bek.no

SHELL=/bin/sh

SVN_TRUNK_ROOT = ../../..
SVNAPPNAME = $(shell ls $(SVN_TRUNK_ROOT) | grep "OM_")
ifdef $(SVNAPPNAME)
SVNAPPNAME = $(SVNAPPNAME)
else
SVNAPPNAME = OM_6.7_beta_4
endif

# APP = $(shell ls $(SVN_TRUNK_ROOT) | grep "OM_")
DISTAPPNAME = OpenMusic

RELEASE = $(HOME)/site/OM/BUILD/$(SVNAPPNAME)

LISP=lw
LISPFLAGS=-build

MAKE = make
COMPRESS = gzip

RSYNCFLAGS = -at

COMPRESS = tar
COMPRESSFILE = $(DISTAPPNAME).tar.bz2
COMPRESSFLAGS = -cjf $(COMPRESSFILE)

RUBBISH = *.ufasl

$(SVN_TRUNK_ROOT)/$(SVNAPPNAME):
	@echo building "$(SVNAPPNAME)" in source tree: "$(SVN_TRUNK_ROOT)"
	$(LISP) $(LISPFLAGS) $(SVN_TRUNK_ROOT)/buildimage/build-om/deliver.lisp

buildsvn: $(SVN_TRUNK_ROOT)/$(SVNAPPNAME)


buildimage: 
	@echo building "$(RELEASE)/$(DISTAPPNAME)"
	$(RELEASE)/$(DISTAPPNAME)

cleansvn:
	find $(SVN_TRUNK_ROOT) -name $(RUBBISH) -delete

clean:  cleansvn

dist:   buildsvn
	@echo -e "setting up dist dir: " $(RELEASE)
	rm -rf $(RELEASE) && mkdir -p $(RELEASE)
	@echo -e "syncing " $(SVN_TRUNK_ROOT) "into dist dir: " $(RELEASE)
	rsync $(RSYNCFLAGS) $(SVN_TRUNK_ROOT) $(RELEASE)
	@echo -e "removing non-dists from buildimage/build-om:"
	find $(RELEASE)/buildimage/build-om/ -not -iname '*build*' -and -not -path '*linux*' -delete
	@echo -e  "removing */.svn from dist:"
	find $(RELEASE) -name '.svn' -exec rm -r {} \;
	@echo -e "removing compiled sources from ./code/* in dist:"
	find $(RELEASE)/code -name '*.ufasl' -exec rm -r {} \;
	@echo -e "removing backup-files (~) from dist:"
	find $(RELEASE) -name '*~' $(PRINT) -exec rm -r {} \;
	cp README.LINUX $(RELEASE)/
	@echo -e "setting up symlink:"
	cd $(RELEASE) && ln -fs ./$(SVNAPPNAME) $(DISTAPPNAME)
	@echo -e "tar and bzip2'ing:"
	cd $(shell dirname $(RELEASE)) && $(COMPRESS) $(COMPRESSFLAGS) $(SVNAPPNAME)
	@echo -e " -> " $(shell ls -lgh $(shell dirname $(RELEASE))/$(COMPRESSFILE))
	@echo -e "done!"
