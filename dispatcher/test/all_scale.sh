#!/bin/bash

threads=32

chunks=(002 004 008 016 032 064 128 256 512)
suffix_len=(001 002 004 008 016 032 064 128 256 512)
progs=( "flip_ab_bin" "irc_bin" "thousand_sep_bin" "csv_project3_bin" "DIV7-gen_bin" "regex2_bin" )

bin_dir="../bin"

for p in ${progs[@]}; do
    # Do base run 1 chunk 5 times
    for _ in 1 2; do
        cmd="./${p} -f ${p}.txt -p 1 -c 1 > /dev/null"
        eval "${cmd}"
    done
    if [[ ! -d "${p}_out" ]]; then
        mkdir "${p}_out"
    fi
    for _ in 1 2 3 4 5; do
        cmd="./${p} -f ${p}.txt --time-fst -p 1 -c 1 2>> ${p}_out/${p}_c001_000.txt > /dev/null"
        eval "${cmd}"
    done
    for c in ${chunks[@]}; do
        for l in ${suffix_len[@]}; do

            for _ in 1 2; do
                cmd="./${p} -f ${p}.txt -p ${threads} -c ${c} -l ${l} > /dev/null 2> /dev/null"
                eval "${cmd}"
            done
            for _ in 1 2 3 4 5; do
                cmd="./${p} -f ${p}.txt --time-fst -p ${threads} -c ${c} -l ${l} 2>> ${p}_out/${p}_c${c}_l${l}.txt > /dev/null"
                eval "${cmd}"
            done
        done
    done
done

