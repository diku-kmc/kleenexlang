#!/bin/bash

# Setup script for grep evaluation

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

# Set the $grep variable to point to GNU grep.
# This 
if [[ $(uname) == "Linux" ]] ; then
    grep="grep"
else
    grep="/opt/local/bin/grep"
    if [ ! -f $grep ]; then
        echo "Could not find GNU grep on your system." > /dev/stderr
        echo "Looked in $grep, which is where Macports would install it." > /dev/stderr
        echo "If you have Macports, run sudo port install grep." > /dev/stderr
        echo "Otherwise, fix this script to point to your installation of GNU grep." > /dev/stderr
        exit 1
    fi
fi

