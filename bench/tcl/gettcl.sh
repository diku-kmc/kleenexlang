#!/bin/bash

TCL_SRCDIR=$1
TCL_DESTDIR=$2
TCL_CONFDIR=$3

if [ -d "$TCL_SRCDIR" ]; then
    echo "tcl was already locally installed.  Aborting."
    exit 0
fi

echo "Fetching and installing tcl locally."
wget http://prdownloads.sourceforge.net/tcl/"$TCL_SRCDIR"-src.tar.gz -O - | tar zx
(cd "$TCL_SRCDIR" && "$TCL_CONFDIR"/configure)
(cd "$TCL_SRCDIR" && make)
(cd "$TCL_SRCDIR" && make install DESTDIR="$TCL_DESTDIR")
