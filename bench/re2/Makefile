CXX=c++
CXXFLAGS=-O3 -I/usr/local/include -L/usr/local/lib -lre2 -pthread
BINDIR=bin
SOURCES = $(wildcard re2_src/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)

all: testprogs

clean:
	rm -f bin/*

testprogs: $(OBJECTS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -o $(BINDIR)/$(*F) $<

