#! env tclsh

# A tcl version of the "simple_id" program.

set regex {^(.*)$}

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
    if {[regexp -lineanchor $regex $line NULL g1]} {
        puts -nonewline stdout [format "%s\n" $g1]
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
