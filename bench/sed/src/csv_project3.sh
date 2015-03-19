#!/bin/bash

# (GNU) sed version of the csv_project3 program

source "${BASH_SOURCE%/*}"/../setup.sh

start=$(get_millisecond_time)


regex="([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)"

cmd="${sed} -E -n s/${regex}/\2\x09\5/p"
$cmd

end=$(get_millisecond_time)

elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
