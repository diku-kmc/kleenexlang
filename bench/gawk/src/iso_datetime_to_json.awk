#!/bin/sh
REALLY_UGLY_SHEBANG_HACK==0 "exec" "gawk" "-f" "$0" "$@"

BEGIN {
    # Always pick the GNU date function, so we can get nanosec precision.
    # If installed via coreutils with Macports on OS X, it is called gdate.
    "uname" | getline uname
    if (uname == "Darwin")
        date = "gdate"
    else
        date = "date"
    date " +%s%N | cut -b1-13" |& getline start

    regex="(([1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?"
}

match($0, regex, m) {
    printf "{'year'='%s', 'month'='%s', 'day'='%s', 'hours'='%s', 'minutes'='%s', 'seconds'='%s', 'tz'='%s'}\n",
        m[1], m[3], m[4], m[5], m[6], m[7], m[8]
}
$0 !~ regex {
     printf "match error!\n"
     exit 1
}

END {
    date " +%s%N | cut -b1-13" | getline end
    elaps = end - start
    print "\nmatching (ms): " elaps > "/dev/stderr"

}
