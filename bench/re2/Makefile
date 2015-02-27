BINDIR=bin
SOURCES = $(wildcard src/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)
UTF8_OBJS = $(OBJECTS:.o=.utf8o)
LIBRE2_DIR = ~

CXX=c++
CXXFLAGS=-O3 -I$(LIBRE2_DIR)/usr/local/include -L$(LIBRE2_DIR)/usr/local/lib -lre2 -pthread

ifeq ($(OS, ))

all: latin1

# Fetch re2 and compile it!
libre2:
	git clone https://github.com/google/re2.git libre2
	patch libre2/Makefile patch.diff
	cd libre2 ; make
	cd libre2 ; make test LDFLAGS=-pthread
	cd libre2 ; make install DESTDIR=$(LIBRE2_DIR)
	cd libre2 ; make testinstall DESTDIR=$(LIBRE2_DIR)


latin1: $(OBJECTS)
utf8: $(UTF8_OBJS)


%.o: libre2 %.cpp
	$(CXX) $(CXXFLAGS) -o $(BINDIR)/$(*F) $<

%.utf8o: libre2 %.cpp
	$(CXX) $(CXXFLAGS) -D USE_UTF8 -o $(BINDIR)/$(*F)_utf8 $<

as : src/as.o
patho2 : src/patho2.o
csv_project3 : src/csv_project3.o
simple_id : src/simple_id.o

.PHONY: clean

clean:
	rm -f bin/*

cleanlibre2:
	rm -f $(LIBRE2_DIR)/usr/local/include/re2
	rm -f $(LIBRE2_DIR)/usr/local/lib/libre2.*
	rm -f libre2/
