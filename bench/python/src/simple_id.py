#! env python

# Python version of simple_id
import sys
import re
import datetime

regex = "(.*)"

pre_compile = datetime.datetime.now()

pattern = re.compile(regex, re.DOTALL)
lno = 0

# Start timing
start = datetime.datetime.now()

for line in sys.stdin:
    lno += 1
    m = pattern.match(line)
    if m:
        sys.stdout.write("%s" % m.group(0))
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
