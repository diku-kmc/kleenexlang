#!/bin/bash

# Download script that fetches and unpacks the data set from Issuu.
# To actually start the download, set invoke with -f.  The .xz files
# take up about 5-7 GB each, and the uncompressed .json files between
# 250 and 350 GB.  

dry_run=true
data_chunks=( 2014-03-01.json.xz
              # 2014-03-02.json.xz
              # 2014-03-03.json.xz
              # 2014-03-04.json.xz
              # 2014-03-05.json.xz
              # 2014-03-06.json.xz
              # 2014-03-07.json.xz
              # 2014-03-08.json.xz
              # 2014-03-09.json.xz
              # 2014-03-10.json.xz
              # 2014-03-11.json.xz
              # 2014-03-12.json.xz
              # 2014-03-13.json.xz
              # 2014-03-14.json.xz
              # 2014-03-15.json.xz
              # 2014-03-16.json.xz
              # 2014-03-17.json.xz
              # 2014-03-18.json.xz
              # 2014-03-19.json.xz
              # 2014-03-20.json.xz
              # 2014-03-21.json.xz
              # 2014-03-22.json.xz
              # 2014-03-23.json.xz
              # 2014-03-24.json.xz
              # 2014-03-25.json.xz
              # 2014-03-26.json.xz
              # 2014-03-27.json.xz
              # 2014-03-28.json.xz
              # 2014-03-29.json.xz
              # 2014-03-30.json.xz
              # 2014-03-31.json.xz
            )

while getopts ":f" opt ; do
  case $opt in
  f)
      dry_run=false
      ;;
  esac
done

for data_chunk in ${data_chunks[@]} ; do
    if [ ! -f $data_chunk ] ; then
        wget="wget http://labs.issuu.com/anodataset/${data_chunk}"
        xz="xz --decompress ${data_chunk}"
        cmd="$wget && $xz"
        echo $cmd
        if [ "$dry_run" = false ] ; then
            eval "$cmd"
        fi
    fi
done

# Print the license.
cat <<EOF

************************************************************************
This anonymized Issuu dataset is released under the Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International License:
http://creativecommons.org/licenses/by-nc-sa/4.0/

This includes the following restrictions:

* If you use this data for any published work, you must mention Issuu as the source.
* You cannot in any way use this data commercially
* If you share it, you need to apply this same licence.

Additionally we kindly ask the following:

* Send us a mail to labs@issuu.com if you use the dataset. We'd love to hear what you are using it for or get comments and suggestions
* Don't hit our servers hard, if you scrape Issuu for metadata when using the dataset.
EOF
