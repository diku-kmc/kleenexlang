#!/usr/bin/env bash

echo "Checking if 'ant' is installed on system..."
if hash ant 2>/dev/null; then
    echo "'ant' is already installed."
    echo "Making ./ant point to system ant"
    rm -f ant
    echo "#!/bin/sh" >> ant
    echo "ant \$@" >> ant
    chmod +x ant
    exit 0
fi
echo "'ant' is not installed; installing it locally."

antball="apache-ant-1.9.4-bin.tar.gz"
anturl="http://mirrors.dotsrc.org/apache//ant/binaries/apache-ant-1.9.4-bin.tar.gz"
antdir="apache-ant-1.9.4"
if [ ! -f $antball ]; then
    echo "Downloading ant..."
    wget -O $antball $anturl
fi
if [ ! -d $antdir ]; then
    echo "Unpacking ant..."
    tar xfz $antball
    echo "Making ./ant point to newly installed ant"
    ln -s $antdir/bin/ant ant
fi
