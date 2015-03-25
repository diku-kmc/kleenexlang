#!/bin/bash
source "${BASH_SOURCE%/*}"/../timing.sh

start=$(get_millisecond_time)

$cut -d',' -f2,5 --output-delimiter="$(printf '\t')"

end=$(get_millisecond_time)

elaps=$(expr $end - $start)
printf "matching (ms): %d\n" $elaps >> /dev/stderr
