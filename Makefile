.PHONY: test scrt

test: scrt
	@(cd test; make)

SOURCE = scrt/tries.c
HELPERS = scrt/tries.h scrt/vectors.h

scrt: ${SOURCE} ${HELPERS}
	mkdir -p dist/build/sim
	gcc -O3 -Wall -o dist/build/sim/sim ${SOURCE}

