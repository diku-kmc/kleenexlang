#!/bin/bash

# To start the download, invoke with -f.  

dry_run=true
data_chunk="http://dawa.aws.dk/replikering/adgangsadresser?sekvensnummer=567289"
name="asw_download.json"

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
