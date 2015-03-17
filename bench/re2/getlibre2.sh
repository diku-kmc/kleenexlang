#!/bin/bash

LIBRE2_SRCDIR=$1
LIBRE2_DESTDIR=$2

if [ -d "$LIBRE2_SRCDIR" ]; then
    echo "libre2 was already installed.  To reinstall you must first make uninstall-libre2."
    exit 0
fi
echo "libre2 was not already installed.  Fetching and installing..."

git clone https://github.com/google/re2.git "$LIBRE2_SRCDIR"
(cd "$LIBRE2_SRCDIR" && make)
(cd "$LIBRE2_SRCDIR" && make test LDFLAGS=-pthread)
(cd "$LIBRE2_SRCDIR" && make install DESTDIR="$LIBRE2_DESTDIR")
