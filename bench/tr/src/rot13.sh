#!/bin/bash
source "${BASH_SOURCE%/*}"/../timing.sh

start=$(get_millisecond_time)

tr 'a-zA-Z' 'n-za-mN-ZA-M'

end=$(get_millisecond_time)

elaps=$(expr $end - $start)
printf "matching (ms): %d\n" $elaps >> /dev/stderr
