#!/bin/bash

threads=32

files=( "_1mb.txt" "_2mb.txt" "_4mb.txt" "_8mb.txt" "_16mb.txt" "_32mb.txt" "_64mb.txt" "_128mb.txt" "_256mb.txt" "_512mb.txt" "_1024mb.txt" "_2048mb.txt" "_4096mb.txt" "_8192mb.txt" "_16384mb.txt" )

if [[ ! -d "irc_data" ]]; then
    mkdir "irc_data"
fi

for f in ${files[@]}; do
    for _ in 1 2; do
        cmd="./irc_bin -f irc_bin${f} -p 32 -c 32  -l 256 > /dev/null 2> /dev/null"
        eval "${cmd}"
    done
    for _ in 1 2 3 4 5; do
        cmd="./irc_bin -f irc_bin${f} --time-fst -p 32 -c 32 -l 256 2>> irc_data/irc_bin${f} > /dev/null"
        eval "${cmd}"
    done
done

