#!/bin/bash

# sed version of the patho2 program

source "${BASH_SOURCE%/*}"/../setup.sh

start=$(get_millisecond_time)

# - We have converted the POSIX non-capturing groups (?: .. ) to normal parens.
# - We have added the trailing "|^$" in order to also match and output the empty
#   lines, as the semantics of the "patho2" program dictates.
regex="^([a-z]*a)|([a-z]*b)$|^$"


cmd="${sed} -r -n s/${regex}/\2/p"
$cmd

end=$(get_millisecond_time)

elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
