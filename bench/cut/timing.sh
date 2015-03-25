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


# Set the $cut variable to point to GNU cut.
# If we are on OSX, the GNU cut is called gcut if installed with
# Macports.
if [[ $(uname) == "Linux" ]] ; then
    cut="cut"
else
    cut="gcut"
fi

