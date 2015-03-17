#!/usr/bin/env python
import sys
import ConfigParser
import datetime
import re

pre_compile = datetime.datetime.now()

parser = ConfigParser.RawConfigParser()
parser.optionxform = str

bslash = re.compile(r'\\')
quote  = re.compile('"')

# Start timing
start = datetime.datetime.now()

parser.readfp(sys.stdin)
sys.stdout.write("{\n")

first_section = True

for section in parser.sections():
    if not first_section:
        sys.stdout.write(",\n")
    else:
        first_section = False

    sys.stdout.write("    \"%s\": {\n" % section)

    first_keyvalue = True
    for (key, value) in parser.items(section):
        if not first_keyvalue:
            sys.stdout.write(",\n")
        else:
            first_keyvalue = False

        if value.startswith('"') and value.endswith('"'):
            value = value[1:-1]
        else:
            value = bslash.sub(r'\\\\', value)
            value = quote.sub(r'\"', value)

        sys.stdout.write("        \"%s\": \"%s\"" % (key, value))

    if not first_keyvalue:
        sys.stdout.write("\n")
    sys.stdout.write("    }")

sys.stdout.write("\n}\n")

# End timing
end = datetime.datetime.now()

# Elapsed time
elaps = end - start
elaps_compile = start - pre_compile
elaps_ms = elaps.seconds * 1000 + elaps.microseconds / 1000
elaps_compile_ms = elaps_compile.seconds * 1000 + elaps_compile.microseconds / 1000

sys.stderr.write("\ncompilation (ms): %s\n" % str(elaps_compile_ms))
sys.stderr.write("matching (ms):    %s\n" % str(elaps_ms))
