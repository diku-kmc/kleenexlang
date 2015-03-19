RAGEL=ragel-6.9
SOURCES = $(wildcard src/*.rl)
ODIR=obj
OBJ = $(SOURCES:.rl=.o)
BIN_DIR = bin
CC = gcc
CCFLAGS = -O3

all: $(OBJ)


%.o: %.rl
	@mkdir -p $(BIN_DIR)
	./ragel -T0 $< -o src/$(*F)-T0.c
	./ragel -T1 $< -o src/$(*F)-T1.c
	./ragel -F0 $< -o src/$(*F)-F0.c
	./ragel -F1 $< -o src/$(*F)-F1.c
	./ragel -G0 $< -o src/$(*F)-G0.c
	./ragel -G1 $< -o src/$(*F)-G1.c
	./ragel -G2 $< -o src/$(*F)-G2.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-T0 src/$(*F)-T0.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-T1 src/$(*F)-T1.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-F0 src/$(*F)-F0.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-F1 src/$(*F)-F1.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-G0 src/$(*F)-G0.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-G1 src/$(*F)-G1.c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F)-G2 src/$(*F)-G2.c

email: src/email.o
flip_ab: src/flip_ab.o
aaa: src/aaa.o
issuu_json2sql: src/issuu_json2sql.o
rot13: src/rot13.o


install-ragel:
	./getragel.sh $(RAGEL)

.phony: clean

clean:
	rm -rf bin/

uninstall-ragel:
	rm -rf $(RAGEL)
	rm -rf ragel
