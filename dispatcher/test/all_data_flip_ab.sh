#!/bin/bash

threads=32

files=( "_1mb.txt" "_2mb.txt" "_4mb.txt" "_8mb.txt" "_16mb.txt" "_32mb.txt" "_64mb.txt" "_128mb.txt" "_256mb.txt" "_512mb.txt" "_1024mb.txt" "_2048mb.txt" "_4096mb.txt" "_8192mb.txt" "_16384mb.txt" )

if [[ ! -d "flip_ab_data" ]]; then
    mkdir "flip_ab_data"
fi

for f in ${files[@]}; do
    for _ in 1 2; do
        cmd="./flip_ab_bin -f flip_ab_bin${f} -p 64 -c 512  -l 2 > /dev/null 2> /dev/null"
        eval "${cmd}"
    done
    for _ in 1 2 3 4 5; do
        cmd="./flip_ab_bin -f flip_ab_bin${f} --time-fst -p 64 -c 512 -l 2 2>> flip_ab_data/flip_ab_bin64${f} > /dev/null"
        eval "${cmd}"
    done
done

