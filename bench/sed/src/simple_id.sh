#!/bin/bash

source "${BASH_SOURCE%/*}"/../setup.sh

start=$(get_millisecond_time)

$sed -r "s/(.*)/\1/" $@

end=$(get_millisecond_time)

elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
