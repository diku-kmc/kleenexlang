#!/bin/bash
./gen_numbers.pl -s 1024000 > thousand_sep_bin_1mb.txt
wait
if [[ -f thousand_sep_bin_2mb.txt ]]; then
    rm -f thousand_sep_bin_2mb.txt
fi
cat thousand_sep_bin_1mb.txt thousand_sep_bin_1mb.txt >> thousand_sep_bin_2mb.txt
wait
if [[ -f thousand_sep_bin_4mb.txt ]]; then
    rm -f thousand_sep_bin_4mb.txt
fi
cat thousand_sep_bin_2mb.txt thousand_sep_bin_2mb.txt >> thousand_sep_bin_4mb.txt
wait
if [[ -f thousand_sep_bin_8mb.txt ]]; then
    rm -f thousand_sep_bin_8mb.txt
fi
cat thousand_sep_bin_4mb.txt thousand_sep_bin_4mb.txt >> thousand_sep_bin_8mb.txt
wait
if [[ -f thousand_sep_bin_16mb.txt ]]; then
    rm -f thousand_sep_bin_16mb.txt
fi
cat thousand_sep_bin_8mb.txt thousand_sep_bin_8mb.txt >> thousand_sep_bin_16mb.txt
wait
if [[ -f thousand_sep_bin_32mb.txt ]]; then
    rm -f thousand_sep_bin_32mb.txt
fi
cat thousand_sep_bin_16mb.txt thousand_sep_bin_16mb.txt >> thousand_sep_bin_32mb.txt
wait
if [[ -f thousand_sep_bin_64mb.txt ]]; then
    rm -f thousand_sep_bin_64mb.txt
fi
cat thousand_sep_bin_32mb.txt thousand_sep_bin_32mb.txt >> thousand_sep_bin_64mb.txt
wait
if [[ -f thousand_sep_bin_128mb.txt ]]; then
    rm -f thousand_sep_bin_128mb.txt
fi
cat thousand_sep_bin_64mb.txt thousand_sep_bin_64mb.txt >> thousand_sep_bin_128mb.txt
wait
if [[ -f thousand_sep_bin_256mb.txt ]]; then
    rm -f thousand_sep_bin_256mb.txt
fi
cat thousand_sep_bin_128mb.txt thousand_sep_bin_128mb.txt >> thousand_sep_bin_256mb.txt
wait
if [[ -f thousand_sep_bin_512mb.txt ]]; then
    rm -f thousand_sep_bin_512mb.txt
fi
cat thousand_sep_bin_256mb.txt thousand_sep_bin_256mb.txt >> thousand_sep_bin_512mb.txt
wait
if [[ -f thousand_sep_bin_1024mb.txt ]]; then
    rm -f thousand_sep_bin_1024mb.txt
fi
cat thousand_sep_bin_512mb.txt thousand_sep_bin_512mb.txt >> thousand_sep_bin_1024mb.txt
wait
if [[ -f thousand_sep_bin_2048mb.txt ]]; then
    rm -f thousand_sep_bin_2048mb.txt
fi
cat thousand_sep_bin_1024mb.txt thousand_sep_bin_1024mb.txt >> thousand_sep_bin_2048mb.txt
wait
if [[ -f thousand_sep_bin_4096mb.txt ]]; then
    rm -f thousand_sep_bin_4096mb.txt
fi
cat thousand_sep_bin_2048mb.txt thousand_sep_bin_2048mb.txt >> thousand_sep_bin_4096mb.txt
wait
if [[ -f thousand_sep_bin_8192mb.txt ]]; then
    rm -f thousand_sep_bin_8192mb.txt
fi
cat thousand_sep_bin_4096mb.txt thousand_sep_bin_4096mb.txt >> thousand_sep_bin_8192mb.txt
wait
if [[ -f thousand_sep_bin_16384mb.txt ]]; then
    rm -f thousand_sep_bin_16384mb.txt
fi
cat thousand_sep_bin_8192mb.txt thousand_sep_bin_8192mb.txt >> thousand_sep_bin_16384mb.txt
wait
