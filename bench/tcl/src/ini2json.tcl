#!/usr/bin/env tclsh



# Regular expression definitions
set comment    {^\s*;|^\s*$}
set headingRx  {^\s*\[([^\n\]]*)\]\s*$}
set keyValueRx {^\s*([^;\s=\[]*)\s*=\s*([^\n]*?)\s*$}
set quotedRx   {^".*"$}
set bslashRx   {\\}
set quoteRx    {\"}
set heading 0
set matched 0
set key 0
set value 0


# Pre-compilation
#set dummy [regexp $headingRx "" NULL]
#set dummy [regexp $comment "" NULL]
#set dummy [regexp $keyValueRx "" NULL]
#set dummy [regexp $quotedRx "" NULL]
#set dummy [regexp $quoteRx "" NULL]

# Start timing
set start [clock clicks -milliseconds]

# Do program
set firstSection true
set ind "    "

puts "\{"
while {[gets stdin line] >= 0} {
    if [regexp -lineanchor $comment $line] {
        continue
    }
    if [regexp $headingRx $line matched heading] {
        if {!$firstSection} {
            puts "\n$ind\},"
        }
        set firstSection 0
        set firstKeyValuePair 1
        puts -nonewline "$ind\"$heading\": \{"
        continue
    }

    if {$firstSection} {
        puts "Needs to start with a section before keys"
        break
    }

    if [regexp $keyValueRx $line matched key value] {
        if ![regexp $quotedRx $value] {
            regsub -all $bslashRx $value {\\\\} value
            regsub -all $quoteRx  $value {\\"} value
            set value "\"$value\""
        }
        if {!$firstKeyValuePair} {
           puts -nonewline ","
        }
        set firstKeyValuePair 0
        puts -nonewline "\n$ind$ind\"$key\": $value"
    }
}

if {!$firstSection} {
    puts -nonewline "\n$ind\}"
}
puts "\n}"

# End timing
set end [clock clicks -milliseconds]

set elaps [expr $end - $start]

puts stderr [format "matching (ms):    %u" $elaps]
