CC=gcc
CFLAGS=-c -O0 -g -std=c99
LDFLAGS=

TESTS=\
	bin/resize_tests

obj/%.o: %.c
	@mkdir -p $$(dirname $@)
	$(CC) $(CFLAGS) -o $@ $*.c

bin/%: obj/%.o obj/test.o
	@mkdir -p $$(dirname $@)
	$(CC) $(LDFLAGS) -o $@ obj/$*.o obj/test.o

.phony: run
run: $(TESTS)
	./runtests $(TESTS)

.phony: clean
clean:
	rm -rf bin
	rm -f *.out
