#!/bin/bash

# Download adresser.csv from dawa.aws.dk.
# Invoke with -f to start download.

dry_run=true
data_chunk="http://dawa.aws.dk/adresser?format=csv"
name="adresser.csv"

while getopts ":f" opt ; do
  case $opt in
  f)
      dry_run=false
      ;;
  esac
done

if [ ! -f $name ] ; then
    wget="wget -O ${name} ${data_chunk}"
    cmd="$wget"
    echo $cmd
    if [ "$dry_run" = false ] ; then
        eval "$cmd"
    fi
else
    echo "file already exists: ${name}"
fi

