#! env tclsh

# A tcl version of the "csv_project3" program.

# The trailing newline has been removed from the expression.
# This is because Tcl chomps any trailing newline symbols when
# iterating over input lines in the normal way.
set regex {([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)}

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
    if {[regexp -lineanchor $regex $line NULL g1 g2 g3 g4 g5 g6]} {
        puts -nonewline stdout [format "%s\t%s\n" $g2 $g5]
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
