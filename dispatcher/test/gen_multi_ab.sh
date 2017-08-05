#!/bin/bash
./gen_ab_lines.pl -s 1024000 > flip_ab_bin_1mb.txt
wait
if [[ -f flip_ab_bin_2mb.txt ]]; then
    rm -f flip_ab_bin_2mb.txt
fi
cat flip_ab_bin_1mb.txt flip_ab_bin_1mb.txt >> flip_ab_bin_2mb.txt
wait
if [[ -f flip_ab_bin_4mb.txt ]]; then
    rm -f flip_ab_bin_4mb.txt
fi
cat flip_ab_bin_2mb.txt flip_ab_bin_2mb.txt >> flip_ab_bin_4mb.txt
wait
if [[ -f flip_ab_bin_8mb.txt ]]; then
    rm -f flip_ab_bin_8mb.txt
fi
cat flip_ab_bin_4mb.txt flip_ab_bin_4mb.txt >> flip_ab_bin_8mb.txt
wait
if [[ -f flip_ab_bin_16mb.txt ]]; then
    rm -f flip_ab_bin_16mb.txt
fi
cat flip_ab_bin_8mb.txt flip_ab_bin_8mb.txt >> flip_ab_bin_16mb.txt
wait
if [[ -f flip_ab_bin_32mb.txt ]]; then
    rm -f flip_ab_bin_32mb.txt
fi
cat flip_ab_bin_16mb.txt flip_ab_bin_16mb.txt >> flip_ab_bin_32mb.txt
wait
if [[ -f flip_ab_bin_64mb.txt ]]; then
    rm -f flip_ab_bin_64mb.txt
fi
cat flip_ab_bin_32mb.txt flip_ab_bin_32mb.txt >> flip_ab_bin_64mb.txt
wait
if [[ -f flip_ab_bin_128mb.txt ]]; then
    rm -f flip_ab_bin_128mb.txt
fi
cat flip_ab_bin_64mb.txt flip_ab_bin_64mb.txt >> flip_ab_bin_128mb.txt
wait
if [[ -f flip_ab_bin_256mb.txt ]]; then
    rm -f flip_ab_bin_256mb.txt
fi
cat flip_ab_bin_128mb.txt flip_ab_bin_128mb.txt >> flip_ab_bin_256mb.txt
wait
if [[ -f flip_ab_bin_512mb.txt ]]; then
    rm -f flip_ab_bin_512mb.txt
fi
cat flip_ab_bin_256mb.txt flip_ab_bin_256mb.txt >> flip_ab_bin_512mb.txt
wait
if [[ -f flip_ab_bin_1024mb.txt ]]; then
    rm -f flip_ab_bin_1024mb.txt
fi
cat flip_ab_bin_512mb.txt flip_ab_bin_512mb.txt >> flip_ab_bin_1024mb.txt
wait
if [[ -f flip_ab_bin_2048mb.txt ]]; then
    rm -f flip_ab_bin_2048mb.txt
fi
cat flip_ab_bin_1024mb.txt flip_ab_bin_1024mb.txt >> flip_ab_bin_2048mb.txt
wait
if [[ -f flip_ab_bin_4096mb.txt ]]; then
    rm -f flip_ab_bin_4096mb.txt
fi
cat flip_ab_bin_2048mb.txt flip_ab_bin_2048mb.txt >> flip_ab_bin_4096mb.txt
wait
if [[ -f flip_ab_bin_8192mb.txt ]]; then
    rm -f flip_ab_bin_8192mb.txt
fi
cat flip_ab_bin_4096mb.txt flip_ab_bin_4096mb.txt >> flip_ab_bin_8192mb.txt
wait
if [[ -f flip_ab_bin_16384mb.txt ]]; then
    rm -f flip_ab_bin_16384mb.txt
fi
cat flip_ab_bin_8192mb.txt flip_ab_bin_8192mb.txt >> flip_ab_bin_16384mb.txt
wait
