#!/bin/bash
./gen_csv.pl -s 1024000 > csv_project3_bin_1mb.txt
wait
if [[ -f csv_project3_bin_2mb.txt ]]; then
    rm -f csv_project3_bin_2mb.txt
fi
cat csv_project3_bin_1mb.txt csv_project3_bin_1mb.txt >> csv_project3_bin_2mb.txt
wait
if [[ -f csv_project3_bin_4mb.txt ]]; then
    rm -f csv_project3_bin_4mb.txt
fi
cat csv_project3_bin_2mb.txt csv_project3_bin_2mb.txt >> csv_project3_bin_4mb.txt
wait
if [[ -f csv_project3_bin_8mb.txt ]]; then
    rm -f csv_project3_bin_8mb.txt
fi
cat csv_project3_bin_4mb.txt csv_project3_bin_4mb.txt >> csv_project3_bin_8mb.txt
wait
if [[ -f csv_project3_bin_16mb.txt ]]; then
    rm -f csv_project3_bin_16mb.txt
fi
cat csv_project3_bin_8mb.txt csv_project3_bin_8mb.txt >> csv_project3_bin_16mb.txt
wait
if [[ -f csv_project3_bin_32mb.txt ]]; then
    rm -f csv_project3_bin_32mb.txt
fi
cat csv_project3_bin_16mb.txt csv_project3_bin_16mb.txt >> csv_project3_bin_32mb.txt
wait
if [[ -f csv_project3_bin_64mb.txt ]]; then
    rm -f csv_project3_bin_64mb.txt
fi
cat csv_project3_bin_32mb.txt csv_project3_bin_32mb.txt >> csv_project3_bin_64mb.txt
wait
if [[ -f csv_project3_bin_128mb.txt ]]; then
    rm -f csv_project3_bin_128mb.txt
fi
cat csv_project3_bin_64mb.txt csv_project3_bin_64mb.txt >> csv_project3_bin_128mb.txt
wait
if [[ -f csv_project3_bin_256mb.txt ]]; then
    rm -f csv_project3_bin_256mb.txt
fi
cat csv_project3_bin_128mb.txt csv_project3_bin_128mb.txt >> csv_project3_bin_256mb.txt
wait
if [[ -f csv_project3_bin_512mb.txt ]]; then
    rm -f csv_project3_bin_512mb.txt
fi
cat csv_project3_bin_256mb.txt csv_project3_bin_256mb.txt >> csv_project3_bin_512mb.txt
wait
if [[ -f csv_project3_bin_1024mb.txt ]]; then
    rm -f csv_project3_bin_1024mb.txt
fi
cat csv_project3_bin_512mb.txt csv_project3_bin_512mb.txt >> csv_project3_bin_1024mb.txt
wait
if [[ -f csv_project3_bin_2048mb.txt ]]; then
    rm -f csv_project3_bin_2048mb.txt
fi
cat csv_project3_bin_1024mb.txt csv_project3_bin_1024mb.txt >> csv_project3_bin_2048mb.txt
wait
if [[ -f csv_project3_bin_4096mb.txt ]]; then
    rm -f csv_project3_bin_4096mb.txt
fi
cat csv_project3_bin_2048mb.txt csv_project3_bin_2048mb.txt >> csv_project3_bin_4096mb.txt
wait
if [[ -f csv_project3_bin_8192mb.txt ]]; then
    rm -f csv_project3_bin_8192mb.txt
fi
cat csv_project3_bin_4096mb.txt csv_project3_bin_4096mb.txt >> csv_project3_bin_8192mb.txt
wait
if [[ -f csv_project3_bin_16384mb.txt ]]; then
    rm -f csv_project3_bin_16384mb.txt
fi
cat csv_project3_bin_8192mb.txt csv_project3_bin_8192mb.txt >> csv_project3_bin_16384mb.txt
wait
