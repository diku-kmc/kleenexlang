CXX=c++
CXXFLAGS=-O3 -I/usr/local/include -L/usr/local/lib -lre2 -pthread
BINDIR=bin
SOURCES = $(wildcard src/*.cpp)
EXTRA_SOURCES = $(wildard extra_src/*.cpp)
HASSOURCES = $(notdir $(SOURCES:.cpp=.has))
HASSOURCES_OBJECTS = $(HASSOURCES:.has=.ho)
HASEDDIR = ../hased_src
OBJECTS = $(SOURCES:.cpp=.o)
EXTRA_OBJECTS = $(EXTRA_SOURCES:.cpp=.o)
REPG=../../dist/build/repg/repg

# Through unimaginable hackery, we can get a hashmap-like thing in make.
# Attach test input file to the programs, to check that the RE2 C++
# programs called <NAME> are equivalent to the Hased programs <NAME>.has
testinput.patho2  := ../../test/data/strings/ab_lines.txt
testinput.csv_project3 := ../../test/data/csv/csv_format1.csv


all: testprogs
testprogs: $(OBJECTS) $(EXTRA_OBJECTS)

# Check that RE2 and Hased compute the same output
testequal: $(HASSOURCES_OBJECTS)

%.ho: $(HASEDDIR)/%.has
	@$(REPG) compile $< --out has_$(*F) --quiet=true
	@bin/$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	@shasum $(*F).out > $(*F).sha
	@./has_$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	shasum -c $(*F).sha
	@rm $(*F).out $(*F).sha has_$(*F)


%.o: %.cpp
	$(CXX) $(CXXFLAGS) -o $(BINDIR)/$(*F) $<


.PHONY: clean

clean:
	rm -f bin/*
	rm -f *.o
