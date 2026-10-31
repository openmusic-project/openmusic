# OpenMusic on SBCL: Linux release folder, tarball, install and uninstall.
# Works from OPENMUSIC/build/sbcl/linux/ in the repository and from the root of the release folder.

SHELL     = /bin/sh

HERE     := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
OMROOT   := $(if $(wildcard $(HERE)/code),$(HERE),$(abspath $(HERE)/../../..))
IMAGE    := $(firstword $(wildcard $(OMROOT)/OM_*))
IMAGENAME = $(notdir $(IMAGE))
EMACSIMAGE = $(OMROOT)/openmusic
LOGO     := $(firstword $(wildcard $(OMROOT)/omlogo.png $(OMROOT)/build/linux/omlogo.png))

DISTDIR   = /tmp/OM/BUILD
RELEASE   = $(DISTDIR)/$(IMAGENAME)

PREFIX    = /usr
bindir    = $(DESTDIR)$(PREFIX)/bin
datadir   = $(DESTDIR)$(PREFIX)/share
omdir     = $(datadir)/openmusic-sbcl

# $(1): destination folder of code/, init/ and resources/, without fasls, other platforms'
# libraries and fonts, and the folders the Windows pack skips
define copy-om-tree
	mkdir -p $(1)
	cp -R -P $(OMROOT)/code $(OMROOT)/init $(OMROOT)/resources $(1)/
	find $(1) -depth -type d \( -name .git -o -name dev -o -name logs \) -exec rm -rf {} +
	find $(1) -type f \( -name '*.fasl' -o -name '*.lisp~' -o -iname '*.ds_store' -o -name '*.db' \) -delete
	rm -rf $(1)/resources/lib/mac $(1)/resources/lib/m1 $(1)/resources/lib/win $(1)/resources/lib/win64
	rm -rf $(1)/resources/fonts/mac $(1)/resources/fonts/win
endef

.PHONY: help dist tar-dist install uninstall check-images

help:
	@echo "targets: dist tar-dist install uninstall"
	@echo "OMROOT:  $(OMROOT)"
	@echo "release: $(RELEASE)"
	@echo "install: $(bindir)/$(IMAGENAME) $(bindir)/openmusic-sbcl $(omdir)"

check-images:
	@test -n "$(IMAGE)" || { echo "No OM_* executable in $(OMROOT)" >&2; exit 1; }
	@test -f "$(EMACSIMAGE)" || { echo "No openmusic executable in $(OMROOT)" >&2; exit 1; }

dist: check-images
	rm -rf $(RELEASE)
	$(call copy-om-tree,$(RELEASE))
	cp -P $(IMAGE) $(EMACSIMAGE) $(RELEASE)/
	cp $(LOGO) $(RELEASE)/omlogo.png
	cp $(lastword $(MAKEFILE_LIST)) $(RELEASE)/Makefile

tar-dist: dist
	tar -C $(DISTDIR) -cjf $(RELEASE).tar.bz2 $(IMAGENAME)

install: check-images
	rm -rf $(omdir)
	$(call copy-om-tree,$(omdir))
	mkdir -p $(bindir) $(datadir)/applications $(datadir)/pixmaps
	install -m 0755 $(IMAGE) $(bindir)/$(IMAGENAME)
	install -m 0755 $(EMACSIMAGE) $(bindir)/openmusic-sbcl
	install -m 0644 $(LOGO) $(datadir)/pixmaps/omlogo.png
	printf '%s\n' '[Desktop Entry]' 'Name=OpenMusic SBCL' 'GenericName=OpenMusic' \
	  'Exec=$(PREFIX)/bin/$(IMAGENAME)' 'Icon=omlogo' 'Type=Application' 'Terminal=false' \
	  'Categories=Application;Development;Science;Multimedia;Audio;X-Jack;Midi;X-MIDI;' 'MimeType=text/x-omp;' \
	  > $(datadir)/applications/OM-SBCL.desktop

# the logo is the vanilla one, under the same name: it stays while the vanilla menu entry needs it
uninstall:
	rm -f $(bindir)/OM_*_SBCL $(bindir)/openmusic-sbcl
	rm -rf $(omdir)
	rm -f $(datadir)/applications/OM-SBCL.desktop
	test -e $(datadir)/applications/OM.desktop || rm -f $(datadir)/pixmaps/omlogo.png
