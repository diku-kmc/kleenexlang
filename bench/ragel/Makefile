RAGEL=ragel-6.9
SOURCES = $(wildcard src/*.rl)
ODIR=obj
OBJ = $(SOURCES:.rl=.o)
GOBJ = $(SOURCES:.rl=.go)
BIN_DIR = bin
GRAPH_DIR = graphs
CC = gcc
CCFLAGS = -O3

all: $(OBJ)


%.o: %.rl
	@mkdir -p $(BIN_DIR)
	./ragel -T1 $< -o src/$(*F)-T1.c
	./ragel -F1 $< -o src/$(*F)-F1.c
	./ragel -G2 $< -o src/$(*F)-G2.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-T1 src/$(*F)-T1.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-F1 src/$(*F)-F1.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-G2 src/$(*F)-G2.c

email: src/email.o
flip_ab: src/flip_ab.o
aaa: src/aaa.o
issuu_json2sql: src/issuu_json2sql.o
rot13: src/rot13.o
iso_datetime_to_json: src/iso_datetime_to_json.o
csv_project3: src/csv_project3.o
csv2json: src/csv2json.o
apache_log: src/apache_log.o


%.go: %.rl
	@mkdir -p $(GRAPH_DIR)
	./ragel -p -V $< -o $(GRAPH_DIR)/$(*F).dot
	dot -Tpdf $(GRAPH_DIR)/$(*F).dot -o $(GRAPH_DIR)/$(*F).pdf

draw-graphs: $(GOBJ)


install-ragel:
	./getragel.sh $(RAGEL)

.phony: clean

clean:
	rm -rf bin/

uninstall-ragel:
	rm -rf $(RAGEL)
	rm -rf ragel
