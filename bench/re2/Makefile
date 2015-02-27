BINDIR=bin
SOURCES = $(wildcard src/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)
UTF8_OBJS = $(OBJECTS:.o=.utf8o)
LIBRE2_DESTDIR = $(HOME)
LIBRE2_SRCDIR = libre2

CXX=c++
CXXFLAGS=-O3 -I$(LIBRE2_DESTDIR)/usr/local/include -L$(LIBRE2_DESTDIR)/usr/local/lib -lre2 -pthread

all: latin1

# Build latin1 or UTF8 versions of the RE2 progs.
latin1: $(OBJECTS)
utf8: $(UTF8_OBJS)

%.o: %.cpp
	rm -f $(BINDIR)/$(*F)
	$(CXX) $(CXXFLAGS) -o $(BINDIR)/BIN-$(*F) $<
	echo "#!/bin/sh" >> $(BINDIR)/$(*F)
	echo 'DIR=$$( cd "$$( dirname "$${BASH_SOURCE[0]}" )" && pwd )' >> $(BINDIR)/$(*F)
	echo "DYLD_LIBRARY_PATH=$(LIBRE2_DESTDIR)/usr/local/lib/ LD_LIBRARY_PATH=\$$DYLD_LIBRARY_PATH \$$DIR/BIN-$(*F)" >> $(BINDIR)/$(*F)
	chmod +x $(BINDIR)/$(*F)

%.utf8o: %.cpp
	$(CXX) $(CXXFLAGS) -D USE_UTF8 -o $(BINDIR)/$(*F)_utf8 $<

as : src/as.o
patho2 : src/patho2.o
csv_project3 : src/csv_project3.o
simple_id : src/simple_id.o

.PHONY: clean

clean:
	rm -f bin/*

# Fetch re2 and compile it!
install-libre2:
	git clone https://github.com/google/re2.git $(LIBRE2_SRCDIR)
	(cd libre2 && make)
	(cd libre2 && make test LDFLAGS=-pthread)
	(cd libre2 && make install DESTDIR=$(LIBRE2_DESTDIR))
# Does not work -- do not care!
# cd libre2 ; make testinstall DESTDIR=$(LIBRE2_DESTDIR)


uninstall-libre2:
	rm -rf $(LIBRE2_DESTDIR)/usr/local/include/re2
	rm -rf $(LIBRE2_DESTDIR)/usr/local/lib/libre2.*
	rm -rf $(LIBRE2_SRCDIR)
