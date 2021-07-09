.PHONY: test build-sim

test: build-sim
	@(cd test; make)

SOURCE = simulation/kexc-simulate.c
HELPERS = simulation/kexc-simulate.h simulation/vectors.h

build-sim: ${SOURCE} ${HELPERS}
	mkdir -p dist/build/kexc
	gcc -O3 -Wall -o dist/build/kexc/kexc-simulate ${SOURCE}

