#!/bin/bash

source "${BASH_SOURCE%/*}"/../setup.sh

start=$(get_millisecond_time)


regex="(([1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](2[0-3]|[0-1][0-9]):[0-5][0-9])?"


cmd="${sed} -E -n s/${regex}/{'year'='\1',\x20'month'='\3',\x20'day'='\4',\x20'hours'='\5',\x20'minutes'='\6',\x20'seconds'='\7',\x20'tz'='\8'}/p"
$cmd

end=$(get_millisecond_time)

elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
