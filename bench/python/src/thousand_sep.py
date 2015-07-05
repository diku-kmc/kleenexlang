#!/usr/bin/env python
import sys
import datetime
import re

pre_compile = datetime.datetime.now()

splitRx   = re.compile(r'^(\d{1,3})((?:\d{3})*)\n$')
replaceRx = re.compile(r'(\d{3})')

# Start timing
start = datetime.datetime.now()

for line in sys.stdin:
    m = splitRx.match(line)
    first = m.group(1)
    rest = m.group(2)

    sys.stdout.write(first)
    if rest:
        sys.stdout.write( replaceRx.sub(r',\1', rest) )
    sys.stdout.write("\n")

# End timing
end = datetime.datetime.now()

# Elapsed time
elaps = end - start
elaps_compile = start - pre_compile
elaps_ms = elaps.seconds * 1000 + elaps.microseconds / 1000
elaps_compile_ms = elaps_compile.seconds * 1000 + elaps_compile.microseconds / 1000

sys.stderr.write("\ncompilation (ms): %s\n" % str(elaps_compile_ms))
sys.stderr.write("matching (ms):    %s\n" % str(elaps_ms))
