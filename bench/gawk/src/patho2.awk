#!/bin/sh
REALLY_UGLY_SHEBANG_HACK==0 "exec" "gawk" "-f" "$0" "$@"

# Awk version of the "patho2" program.

BEGIN {
    # Always pick the GNU date function, so we can get nanosec precision.
    # If installed via coreutils with Macports on OS X, it is called gdate.
    "uname" | getline uname
    if (uname == "Darwin")
        date = "gdate"
    else
        date = "date"
    date " +%s%N | cut -b1-13" |& getline start

    # Difference from the other versions:
    #  - apparently, gawk does not support non-capturing parens (?: .. ), so they
    #    have been replaced with ordinary parens.
    regex = "^(([a-z]*a)|([a-z]*b))?(\n)?$"
}

match($0, regex, m) {
    print m[3]
}

END {
    date " +%s%N | cut -b1-13" | getline end
    elaps = end - start
    print "\nmatching (ms): " elaps > "/dev/stderr"

}
