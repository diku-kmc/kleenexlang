
TCL_DESTDIR=$(HOME)

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
	CONFDIR=unix
endif
ifeq ($(UNAME), Darwin)
	CONFDIR=macosx
endif

# Download and compile Tcl and install it in $TCL_DESTDIR
install-tcl: build mkscript

tcl8.6.3-src.tar.gz:
	wget --output-document=tcl8.6.3-src.tar.gz http://prdownloads.sourceforge.net/tcl/tcl8.6.3-src.tar.gz
	tar xvf tcl8.6.3-src.tar.gz

build: tcl8.6.3-src.tar.gz
	(cd tcl8.6.3 && $(CONFDIR)/configure)
	(cd tcl8.6.3 && make)
	(cd tcl8.6.3 && make install DESTDIR=$(TCL_DESTDIR))

mkscript:
	rm -f tclsh
	echo "#!/bin/bash" >> tclsh
	echo "DYLD_LIBRARY_PATH=$(TCL_DESTDIR)/usr/local/lib/ LD_LIBRARY_PATH=\$$DYLD_LIBRARY_PATH $(TCL_DESTDIR)/usr/local/bin/tclsh8.6 \"\$$@\"" >> tclsh
	chmod +x tclsh

clean:
	rm -rf tcl8.6.3
	rm -rf tcl8.6.3-src.tar.gz

