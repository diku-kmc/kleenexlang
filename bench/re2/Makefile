CXX=c++
CXXFLAGS=-O3 -I/usr/local/include -L/usr/local/lib -lre2 -pthread
BINDIR=bin
SOURCES = $(wildcard src/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)
UTF8_OBJS = $(OBJECTS:.o=.utf8o)

all: latin1

latin1: $(OBJECTS)
utf8: $(UTF8_OBJS)


%.o: %.cpp
	$(CXX) $(CXXFLAGS) -o $(BINDIR)/$(*F) $<

%.utf8o: %.cpp
	$(CXX) $(CXXFLAGS) -D USE_UTF8 -o $(BINDIR)/$(*F)_utf8 $<

as : src/as.o
patho2 : src/patho2.o
csv_project3 : src/csv_project3.o
simple_id : src/simple_id.o

.PHONY: clean

clean:
	rm -f bin/*
