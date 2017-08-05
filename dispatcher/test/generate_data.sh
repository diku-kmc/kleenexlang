#!/bin/bash
python gen_div7.py & 
./gen_irc.pl -s 1000000000 > irc_bin.txt &
./gen_csv.pl -s 1000000000 > csv_project3_bin.txt &
./gen_numbers.pl -s 1000000000 > thousand_sep_bin.txt &
python gen_regex2.py &
./gen_ab_lines.pl -s 1000000000 > flip_ab_bin.txt 
wait
