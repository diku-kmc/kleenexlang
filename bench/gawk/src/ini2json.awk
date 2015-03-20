#!/bin/bash
REALLY_UGLY_SHEBANG_HACK==0 "exec" "gawk" "-f" "$0" "$@"

# Awk version of the "ini2json" program.
BEGIN {
    # Always pick the GNU date function, so we can get nanosec precision.
    # If installed via coreutils with Macports on OS X, it is called gdate.
    "uname" | getline uname
    if (uname == "Darwin")
        date = "gdate"
    else
        date = "date"
    date " +%s%N | cut -b1-13" |& getline start

    printf "{\n"
    first_section=1
}

# Comments
match($0, /^\s*;|^\s*$/, m) {
    next
}

# Section headings
match($0, /^\s*\[([^\n\]]*)\]\s*$/, m) {
    if (! first_section) {
        printf "\n    },\n"
    }
    first_section=0
    printf("    \"%s\": {", m[1])
    first_keyvalue=1
    next
}

# Key-value pairs
match($0, /^\s*([^; =\[]*)\s*=\s*([^\n]*\S|)\s*$/, m) {
    key   = m[1]
    value = m[2]
    if (! match (value, /^".*"$/)) {
        gsub(/\\/, "\\\\", value)
        gsub(/"/, "\\\"", value)
        value = sprintf("\"%s\"", value)
    }

    if (! first_keyvalue) {
        printf(",")
    }
    first_keyvalue = 0

    printf("\n        \"%s\": %s", key, value)
    next
}

{
     printf "match error!\n"
     exit 1
}

END {
    if (! first_section) {
        printf "\n    }"
    }
    printf "\n}\n"

    date " +%s%N | cut -b1-13" | getline end
    elaps = end - start
    print "\nmatching (ms): " elaps > "/dev/stderr"

}
