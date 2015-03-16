#!/usr/bin/env python

import sys
import re
import datetime

regexprime = "((?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?"
regex = regexprime + "\n"

pre_compile = datetime.datetime.now()

pattern = re.compile(regex)
lno = 0

# Start timing
start = datetime.datetime.now()

for line in sys.stdin:
    lno += 1
    m = pattern.match(line)
    if m:
        sys.stdout.write("{'year'='%s', 'month'='%s', 'day'='%s', 'hours'='%s', 'minutes'='%s', 'seconds'='%s', 'tz'='%s'}\n" %
                         (m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6), m.group(7)))
    else:
        sys.stderr.write("match error on line %s\n" % str(lno))
        exit(1)

# End timing
end = datetime.datetime.now()

# Elapsed time
elaps = end - start
elaps_compile = start - pre_compile
elaps_ms = elaps.seconds * 1000 + elaps.microseconds / 1000
elaps_compile_ms = elaps_compile.seconds * 1000 + elaps_compile.microseconds / 1000

sys.stderr.write("\ncompilation (ms): %s\n" % str(elaps_compile_ms))
sys.stderr.write("matching (ms):    %s\n" % str(elaps_ms))
