BINDIR=bin
SOURCES = $(wildcard src/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)

CXX=c++
CXXFLAGS=-O3

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
