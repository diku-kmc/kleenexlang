#!/bin/bash

ONIG_SRCDIR=$1
ONIG_DESTDIR=$2

echo "Checking if libonig is installed."
if [ -d "$ONIG_SRCDIR" ]; then
    echo "Oniguruma is already installed.  Abort."
    exit 0
fi
echo "Oniguram is not installed.  Fetching and installing it now."

wget https://github.com/kkos/oniguruma/releases/download/v5.9.6/"$ONIG_SRCDIR".tar.gz -O - | tar xz
(cd "$ONIG_SRCDIR" && ./configure)
(cd "$ONIG_SRCDIR" && make)
(cd "$ONIG_SRCDIR" && make install DESTDIR="$ONIG_DESTDIR")
