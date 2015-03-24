#!/bin/bash
source "${BASH_SOURCE%/*}"/../setup.sh

# GNU grep version of the email validator program.

regex="[a-z0-9!#$%&'*+/=?^_\`{|}~-]+(\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?"

start=$(get_millisecond_time)

$grep --text -E "^${regex}$" $@

end=$(get_millisecond_time)
elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
