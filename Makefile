.PHONY: test scrt

test: scrt
	@(cd test; make)

SOURCE = scrt/tries.c
HELPERS = scrt/tries.h scrt/vectors.h

scrt: ${SOURCE} ${HELPERS}
	gcc -g -o dist/build/sim/sim ${SOURCE}

