#!/usr/bin/env bash

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
    ln -s $antdir/bin/ant ant
fi
