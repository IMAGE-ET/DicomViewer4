#
#	Makefile for config
#



SHELL = /bin/sh

include Makefile.def


all:

install: install-doc

install-bin:

install-doc: docs-install

install-data:

install-etc:

install-include: include-install

install-lib: include-install

install-support:


libsrc-all:


include-install:
	./mkinstalldirs $(DESTDIR)$(includedir)/dcmtk/config
	$(INSTALL_DATA) include/dcmtk/config/osconfig.h $(DESTDIR)$(includedir)/dcmtk/config
	$(INSTALL_DATA) include/dcmtk/config/cfunix.h $(DESTDIR)$(includedir)/dcmtk/config

docs-install:
	./mkinstalldirs $(DESTDIR)$(docdir)
	$(INSTALL_DATA) docs/config.txt $(DESTDIR)$(docdir)
	$(INSTALL_DATA) docs/envvars.txt $(DESTDIR)$(docdir)
	$(INSTALL_DATA) docs/modules.txt $(DESTDIR)$(docdir)
	$(INSTALL_DATA) docs/dirstruc.txt $(DESTDIR)$(docdir)
	$(INSTALL_DATA) docs/macros.txt $(DESTDIR)$(docdir)


clean:
	(cd include/dcmtk/config && rm -f $(TRASH))
	rm -rf $(TRASH)

distclean:
	(cd include/dcmtk/config && rm -f $(DISTTRASH) cfunix.h)
	rm -rf $(DISTTRASH) Makefile.def

dependencies:
