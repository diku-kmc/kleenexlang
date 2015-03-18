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
	./ragel $< -o src/$(*F).c
	$(CC) $(CCFLAGS) -o $(BIN_DIR)/$(*F) src/$(*F).c

email: src/email.o
flip_ab: src/flip_ab.o
aaa: src/aaa.o
issuu_json2sql: src/issuu_json2sql.o

install-ragel:
	./getragel.sh $(RAGEL)

.phony: clean
clean:
	rm -rf $(RAGEL)
