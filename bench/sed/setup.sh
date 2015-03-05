#!/bin/bash

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

# Set the $sed variable to point to GNU sed.
# If we are on OSX, the GNU sed is called gsed if installed with
# Macports.
if [[ $(uname) == "Linux" ]] ; then
    sed="sed"
else
    sed="gsed"
fi

