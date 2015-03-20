#!/bin/bash
source "${BASH_SOURCE%/*}"/../setup.sh

# GNU grep version of the as program.

start=$(get_millisecond_time)

$grep -E "^a*$" $@

end=$(get_millisecond_time)
elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
