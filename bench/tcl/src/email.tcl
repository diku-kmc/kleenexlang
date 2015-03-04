#! env tclsh

# A tcl version of the email validator

set regexprime {[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?}
set regex "^${regexprime}$"


set pre_compile [clock clicks -milliseconds]

# Force precompilation
set dummy [regexp $regex "" NULL]

# -lineanchor
# Changes the behavior of ‘^’ and ‘$’ (the “anchors”) so they match
# the beginning and end of a line respectively. This is the same as
# specifying the (?w) embedded option (see METASYNTAX, below).
# - http://wiki.tcl.tk/986

# Start timing
set start [clock clicks -milliseconds]
while {[gets stdin line] >= 0} {
    if {[regexp -lineanchor $regex $line]} {
        puts stdout $line
    }
}
# End timing
set end [clock clicks -milliseconds]

set elaps [expr $end - $start]
set elaps_compile [expr $start - $pre_compile]

puts stderr [format "\ncompilation (ms): %u" $elaps_compile]
puts stderr [format "matching (ms):    %u" $elaps]
