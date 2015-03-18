# http://stackoverflow.com/questions/12530406/is-gcc-4-7-and-gcc-4-8-buggy-about-regular-expressions
# Note that gcc < 4.9 does /not/ implement the C++11 regex properly!
# Most of the binaries built here therefore won't work.  
# See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=53631

BINDIR=bin
SOURCES = $(wildcard src/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)

# Use clang instead of GCC because of the above.
CXX=clang++
CXXFLAGS=-O3 -std=c++11

all: $(OBJECTS)

%.o: %.cpp
	rm -f $(BINDIR)/$(*F)
	mkdir -p $(BINDIR)
	$(CXX) $(CXXFLAGS) -o $(BINDIR)/$(*F) $<

as : src/as.o
patho2 : src/patho2.o
csv_project3 : src/csv_project3.o
simple_id : src/simple_id.o
email : src/email.o
iso_datetime_to_json : src/iso_datetime_to_json.o

.PHONY: clean

clean:
	rm -f bin/*
