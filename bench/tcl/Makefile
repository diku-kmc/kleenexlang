
TCL_DESTDIR=$(HOME)
TCL_SRCDIR=tcl8.6.3

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
	CONFDIR=unix
endif
ifeq ($(UNAME), Darwin)
	CONFDIR=macosx
endif

# Download and compile Tcl and install it in $TCL_DESTDIR
install-tcl: build mkscript

build:
	./gettcl.sh $(TCL_SRCDIR) $(TCL_DESTDIR) $(CONFDIR)

mkscript:
	rm -f tclsh
	echo "#!/bin/bash" >> tclsh
	echo "DYLD_LIBRARY_PATH=$(TCL_DESTDIR)/usr/local/lib/ LD_LIBRARY_PATH=\$$DYLD_LIBRARY_PATH $(TCL_DESTDIR)/usr/local/bin/tclsh8.6 \"\$$@\"" >> tclsh
	chmod +x tclsh

clean:
	rm -rf tcl8.6.3
	rm -rf tcl8.6.3-src.tar.gz

