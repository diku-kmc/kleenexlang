#!/bin/bash

source "${BASH_SOURCE%/*}"/../setup.sh

start=$(get_millisecond_time)

cat

end=$(get_millisecond_time)

elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
