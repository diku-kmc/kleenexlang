#!/bin/bash
./gen_irc.pl -s 1024000 > irc_bin_1mb.txt
wait
if [[ -f irc_bin_2mb.txt ]]; then
    rm -f irc_bin_2mb.txt
fi
cat irc_bin_1mb.txt irc_bin_1mb.txt >> irc_bin_2mb.txt
wait
if [[ -f irc_bin_4mb.txt ]]; then
    rm -f irc_bin_4mb.txt
fi
cat irc_bin_2mb.txt irc_bin_2mb.txt >> irc_bin_4mb.txt
wait
if [[ -f irc_bin_8mb.txt ]]; then
    rm -f irc_bin_8mb.txt
fi
cat irc_bin_4mb.txt irc_bin_4mb.txt >> irc_bin_8mb.txt
wait
if [[ -f irc_bin_16mb.txt ]]; then
    rm -f irc_bin_16mb.txt
fi
cat irc_bin_8mb.txt irc_bin_8mb.txt >> irc_bin_16mb.txt
wait
if [[ -f irc_bin_32mb.txt ]]; then
    rm -f irc_bin_32mb.txt
fi
cat irc_bin_16mb.txt irc_bin_16mb.txt >> irc_bin_32mb.txt
wait
if [[ -f irc_bin_64mb.txt ]]; then
    rm -f irc_bin_64mb.txt
fi
cat irc_bin_32mb.txt irc_bin_32mb.txt >> irc_bin_64mb.txt
wait
if [[ -f irc_bin_128mb.txt ]]; then
    rm -f irc_bin_128mb.txt
fi
cat irc_bin_64mb.txt irc_bin_64mb.txt >> irc_bin_128mb.txt
wait
if [[ -f irc_bin_256mb.txt ]]; then
    rm -f irc_bin_256mb.txt
fi
cat irc_bin_128mb.txt irc_bin_128mb.txt >> irc_bin_256mb.txt
wait
if [[ -f irc_bin_512mb.txt ]]; then
    rm -f irc_bin_512mb.txt
fi
cat irc_bin_256mb.txt irc_bin_256mb.txt >> irc_bin_512mb.txt
wait
if [[ -f irc_bin_1024mb.txt ]]; then
    rm -f irc_bin_1024mb.txt
fi
cat irc_bin_512mb.txt irc_bin_512mb.txt >> irc_bin_1024mb.txt
wait
if [[ -f irc_bin_2048mb.txt ]]; then
    rm -f irc_bin_2048mb.txt
fi
cat irc_bin_1024mb.txt irc_bin_1024mb.txt >> irc_bin_2048mb.txt
wait
if [[ -f irc_bin_4096mb.txt ]]; then
    rm -f irc_bin_4096mb.txt
fi
cat irc_bin_2048mb.txt irc_bin_2048mb.txt >> irc_bin_4096mb.txt
wait
if [[ -f irc_bin_8192mb.txt ]]; then
    rm -f irc_bin_8192mb.txt
fi
cat irc_bin_4096mb.txt irc_bin_4096mb.txt >> irc_bin_8192mb.txt
wait
if [[ -f irc_bin_16384mb.txt ]]; then
    rm -f irc_bin_16384mb.txt
fi
cat irc_bin_8192mb.txt irc_bin_8192mb.txt >> irc_bin_16384mb.txt
wait

