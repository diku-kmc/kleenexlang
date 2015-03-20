#!/bin/bash
source "${BASH_SOURCE%/*}"/../setup.sh

# GNU grep version of the simple_id program.
# Yes, this is a silly way of using grep, as this program is just the cat command,
# but we can still use it to compare the speed of the match-anything semantics of .*

start=$(get_millisecond_time)

$grep -E "^.*$" $@

end=$(get_millisecond_time)
elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
