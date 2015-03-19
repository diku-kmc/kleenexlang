#!/bin/bash

# sed version of the email validator

source "${BASH_SOURCE%/*}"/../setup.sh

start=$(get_millisecond_time)


# Here we must escape all the backtick ` so bash won't try to interpret them as
# commands.
# We have also converted the POSIX non-capturing groups (?: .. ) to normal parens.
regex="[a-z0-9!#$%&'*+/=?^_\`{|}~-]+(\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?"


cmd="${sed} -r /^${regex}\$/!d $@"
$cmd

end=$(get_millisecond_time)

elaps=$(expr $end - $start)

printf "matching (ms): %d\n" $elaps >> /dev/stderr
