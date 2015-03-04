#!/bin/sh
REALLY_UGLY_SHEBANG_HACK==0 "exec" "gawk" "-f" "$0" "$@"

# Awk version of the "simple_id" test program

BEGIN {
    # Always pick the GNU date function, so we can get nanosec precision.
    # If installed via coreutils with Macports on OS X, it is called gdate.
    "uname" | getline uname
    if (uname == "Darwin")
        date = "gdate"
    else
        date = "date"
    date " +%s%N | cut -b1-13" |& getline start

}

{ print $0 }

END {
    date " +%s%N | cut -b1-13" | getline end
    elaps = end - start
    print "\nmatching (ms): " elaps > "/dev/stderr"
}

