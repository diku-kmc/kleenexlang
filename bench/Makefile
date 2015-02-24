RE2_BINDIR=re2/bin
PYTHON_BINDIR=python/src

SOURCES = $(wildcard re2/src/*.cpp) $(wildcard python/src/*.cpp)

HASSOURCES = $(notdir $(SOURCES:.cpp=.has))
HASSOURCES_OBJECTS = $(HASSOURCES:.has=.ho)
HASEDDIR = hased_src

REPG = ../dist/build/repg/repg
DATA_DIR = ../test/data

# Through unimaginable hackery, we can get a hashmap-like thing in make.
# Attach test input file to the programs, to check that the RE2 C++
# programs called <NAME> are equivalent to the Hased programs <NAME>.has
testinput.patho2       := $(DATA_DIR)/strings/ab_lines.txt
testinput.csv_project3 := $(DATA_DIR)/csv/csv_format1.csv
testinput.as           := $(DATA_DIR)/strings/as_small.txt
testinput.simple_id    := $(DATA_DIR)/strings/ab_lines.txt

all:
	@echo "Nothing to do.  Run make equality_check to check all programs."

# Check that RE2, Python, and Hased compute the same output
equality_check: $(HASSOURCES_OBJECTS)

%.ho: $(HASEDDIR)/%.has
	@echo ""
	@echo "Checking $(*F)..."
	$(REPG) compile $< --out has_$(*F) --quiet=true
	$(RE2_BINDIR)/$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	shasum $(*F).out > $(*F).sha
	python $(PYTHON_BINDIR)/$(*F).py < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	@echo "  Python and RE2 equivalent:"
	shasum -c $(*F).sha
	./has_$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	@echo "  Hased and Python equivalent:"
	shasum -c $(*F).sha
	rm $(*F).out $(*F).sha has_$(*F)

patho2: patho2.ho
csv_project3: csv_project3.ho
as: as.ho
simple_id: simple_id.ho

.PHONY: clean
