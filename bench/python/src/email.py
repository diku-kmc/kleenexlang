#! env python

# Python version of the line-based email validator
import sys
import re
import datetime

regexprime = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
regex = "^" + regexprime + "$"

pre_compile = datetime.datetime.now()

pattern = re.compile(regex)

# Start timing
start = datetime.datetime.now()

for line in sys.stdin:
    m = pattern.match(line)
    if m:
        sys.stdout.write("%s\n" % m.group(0))

# End timing
end = datetime.datetime.now()

# Elapsed time
elaps = end - start
elaps_compile = start - pre_compile
elaps_ms = elaps.seconds * 1000 + elaps.microseconds / 1000
elaps_compile_ms = elaps_compile.seconds * 1000 + elaps_compile.microseconds / 1000

sys.stderr.write("\ncompilation (ms): %s\n" % str(elaps_compile_ms))
sys.stderr.write("matching (ms):    %s\n" % str(elaps_ms))
