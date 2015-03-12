#! env python

# Python version the issuu_json2sql Kleenex program.

# This implementation uses the Python library "json" to convert the .json file
# to a SQL insertion statement.  This might seem like comparing apples and oranges,
# as this program does not exercise the Python regex engine at all (at least not
# directly), but if this program is to be a realistic use of Python it has to
# be "Pythonic" and use the standard library function exposed by the json module.
# Thus, comparing the kleenex version with the Python version is not a comparison
# of regex engines, but of two different ways of solving the same problem.
# This all comes with the caveat that this Python implementation is much more
# expressive than the kleenex program because it has the full computational
# expressibility of Python available.  As a consequence, the kleenex version
# only works under some assumptions on the input json data, notably that the
# fields come in a certain order and that they all are present in every object.

import sys
import re
import datetime
import json

lno = 0

# Start timing
start = datetime.datetime.now()

sys.stdout.write("INSERT INTO issuu_log (ts, visitor_uuid, visitor_useragent, visitor_country) VALUES\n")
first = True
for line in sys.stdin:
    lno += 1
    o = json.loads(line)
    if not first:
        sys.stdout.write(",\n")
    first = False
    if o:
        sys.stdout.write("(%s,'%s','%s','%s')" %
                         (o["ts"],
                          o["visitor_uuid"],
                          o["visitor_useragent"],
                          o["visitor_country"]))
    else:
        sys.stderr.write("json object could not be loaded.")
        exit(1)

sys.stdout.write(";\n")
# End timing
end = datetime.datetime.now()

# Elapsed time
elaps = end - start
elaps_ms = elaps.seconds * 1000 + elaps.microseconds / 1000

sys.stderr.write("\nmatching (ms):    %s\n" % str(elaps_ms))
