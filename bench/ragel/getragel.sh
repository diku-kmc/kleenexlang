#!/bin/bash

RAGEL_DIR=$1

echo "Checking if Ragel was already installed..."
if [ -d "$RAGEL_DIR" ] ; then
    echo "Ragel was already installed.  Aborting."
    exit 0
fi
echo "Ragel is not installed.  Fetching and installing."

wget http://www.colm.net/files/ragel/"$RAGEL_DIR".tar.gz -O - | tar zx
(cd "$RAGEL_DIR" && ./configure)
(cd "$RAGEL_DIR" && make)
rm -f ragel
ln -s "$RAGEL_DIR"/ragel/ragel ragel
