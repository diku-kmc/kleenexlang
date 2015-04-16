# The KMC regular expression compiler #

This is a compiler that can:

* compile regular expressions into fast, streaming parsers, and
* compile programs written in the regular expression-based language Kleenex into fast, streaming string transformations.

## Build ##

To build, run `cabal configure && cabal build`. This will place a binary in `dist/build/repg/repg`.

## Test suite ##

A number of test suites are included.

* To run the unit tests: `cabal test`.
* To test the C runtime: `cd crt_test && make`. Note that this uses the Valgrind tool.
* To run the end-to-end blackbox tests: `cd test/test_compiled && make`.

## Benchmark suite ##

The repository includes a benchmark suite that compares the performance of string transformation programs written in Kleenex with equivalent programs written using other regular expression-based libraries and tools.

To run the benchmarks and generate the plots, first `cd bench` and then:

1. generate the test data: `make generate-test-data`
1. install the external benchmark dependencies (libraries, etc.): `make install-benchmark-dependencies`
1. build the benchmark programs (not Kleenex programs): `make build-benchmark-programs`
1. (optional) check that the benchmark programs are equivalent: `make -k equality-check`
1. build the Kleenex programs: `./compiletime.sh -f`
1. run /all/ the benchmark programs N times with M warm-up rounds: `./runningtime.sh -r <N> -w <M> -f`
1. generate the plots: `./mkplots.py`
1. the plots are placed in `bench/plots` (unless otherwise specified to `mkplots.py`)