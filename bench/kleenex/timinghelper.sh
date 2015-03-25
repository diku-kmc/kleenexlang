#!/bin/bash
# Helper script for the ini2json special case.

function get_millisecond_time () {
    # Always pick the GNU date function, so we can get nanosec precision.
    # If installed via coreutils with Macports on OS X, it is called gdate.
    un=$(uname)
    if [[ $un == "Darwin" ]]; then
        date="gdate"
    else
        date="date"
    fi
    echo $("$date" "+%s%N" | cut "-b-13")
}
