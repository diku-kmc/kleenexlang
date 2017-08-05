#!/bin/bash
python gen_div7.py > DIV7-gen_bin_1mb.txt
wait
if [[ -f DIV7-gen_bin_2mb.txt ]]; then
    rm -f DIV7-gen_bin_2mb.txt
fi
cat DIV7-gen_bin_1mb.txt DIV7-gen_bin_1mb.txt >> DIV7-gen_bin_2mb.txt
wait
if [[ -f DIV7-gen_bin_4mb.txt ]]; then
    rm -f DIV7-gen_bin_4mb.txt
fi
cat DIV7-gen_bin_2mb.txt DIV7-gen_bin_2mb.txt >> DIV7-gen_bin_4mb.txt
wait
if [[ -f DIV7-gen_bin_8mb.txt ]]; then
    rm -f DIV7-gen_bin_8mb.txt
fi
cat DIV7-gen_bin_4mb.txt DIV7-gen_bin_4mb.txt >> DIV7-gen_bin_8mb.txt
wait
if [[ -f DIV7-gen_bin_16mb.txt ]]; then
    rm -f DIV7-gen_bin_16mb.txt
fi
cat DIV7-gen_bin_8mb.txt DIV7-gen_bin_8mb.txt >> DIV7-gen_bin_16mb.txt
wait
if [[ -f DIV7-gen_bin_32mb.txt ]]; then
    rm -f DIV7-gen_bin_32mb.txt
fi
cat DIV7-gen_bin_16mb.txt DIV7-gen_bin_16mb.txt >> DIV7-gen_bin_32mb.txt
wait
if [[ -f DIV7-gen_bin_64mb.txt ]]; then
    rm -f DIV7-gen_bin_64mb.txt
fi
cat DIV7-gen_bin_32mb.txt DIV7-gen_bin_32mb.txt >> DIV7-gen_bin_64mb.txt
wait
if [[ -f DIV7-gen_bin_128mb.txt ]]; then
    rm -f DIV7-gen_bin_128mb.txt
fi
cat DIV7-gen_bin_64mb.txt DIV7-gen_bin_64mb.txt >> DIV7-gen_bin_128mb.txt
wait
if [[ -f DIV7-gen_bin_256mb.txt ]]; then
    rm -f DIV7-gen_bin_256mb.txt
fi
cat DIV7-gen_bin_128mb.txt DIV7-gen_bin_128mb.txt >> DIV7-gen_bin_256mb.txt
wait
if [[ -f DIV7-gen_bin_512mb.txt ]]; then
    rm -f DIV7-gen_bin_512mb.txt
fi
cat DIV7-gen_bin_256mb.txt DIV7-gen_bin_256mb.txt >> DIV7-gen_bin_512mb.txt
wait
if [[ -f DIV7-gen_bin_1024mb.txt ]]; then
    rm -f DIV7-gen_bin_1024mb.txt
fi
cat DIV7-gen_bin_512mb.txt DIV7-gen_bin_512mb.txt >> DIV7-gen_bin_1024mb.txt
wait
if [[ -f DIV7-gen_bin_2048mb.txt ]]; then
    rm -f DIV7-gen_bin_2048mb.txt
fi
cat DIV7-gen_bin_1024mb.txt DIV7-gen_bin_1024mb.txt >> DIV7-gen_bin_2048mb.txt
wait
if [[ -f DIV7-gen_bin_4096mb.txt ]]; then
    rm -f DIV7-gen_bin_4096mb.txt
fi
cat DIV7-gen_bin_2048mb.txt DIV7-gen_bin_2048mb.txt >> DIV7-gen_bin_4096mb.txt
wait
if [[ -f DIV7-gen_bin_8192mb.txt ]]; then
    rm -f DIV7-gen_bin_8192mb.txt
fi
cat DIV7-gen_bin_4096mb.txt DIV7-gen_bin_4096mb.txt >> DIV7-gen_bin_8192mb.txt
wait
if [[ -f DIV7-gen_bin_16384mb.txt ]]; then
    rm -f DIV7-gen_bin_16384mb.txt
fi
cat DIV7-gen_bin_8192mb.txt DIV7-gen_bin_8192mb.txt >> DIV7-gen_bin_16384mb.txt
wait
