#!/usr/bin/env tclsh

set regex {((?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?}

set pre_compile [clock clicks -milliseconds]

# Force precompilation
set dummy [regexp $regex "" NULL]

# -lineanchor
# Changes the behavior of ‘^’ and ‘$’ (the “anchors”) so they match
# the beginning and end of a line respectively. This is the same as
# specifying the (?w) embedded option (see METASYNTAX, below).
# - http://wiki.tcl.tk/986

set lno 0

# Start timing
set start [clock clicks -milliseconds]
while {[gets stdin line] >= 0} {
    set lno [incr $lno]
    if {[regexp -lineanchor $regex $line NULL g1 g2 g3 g4 g5 g6 g7]} {
        puts -nonewline stdout [format "{'year'='%s', 'month'='%s', 'day'='%s', 'hours'='%s', 'minutes'='%s', 'seconds'='%s', 'tz'='%s'}\n" $g1 $g2 $g3 $g4 $g5 $g6 $g7]
    } else {
        puts -nonewline stderr "match error on line "
        puts stderr $lno
        exit 1
    }
}
# End timing
set end [clock clicks -milliseconds]

set elaps [expr $end - $start]
set elaps_compile [expr $start - $pre_compile]

puts stderr [format "\ncompilation (ms): %u" $elaps_compile]
puts stderr [format "matching (ms):    %u" $elaps]
