# The KMC regular expression compiler #

[![Travis status](https://travis-ci.org/diku-kmc/kleenexlang.svg?branch=master)](https://travis-ci.org/diku-kmc/kexc)

This is a compiler that can:

* compile regular expressions into fast, streaming parsers (see [http://www.diku.dk/kmc/documents/GHR14-0-paper.pdf]), and
* compile programs written in the regular expression-based language Kleenex into fast, streaming string transformations.

## Download VM for testing ##
If you want to quickly get a sense of what Kleenex is, you can download a VirtualBox image that is ready to play around with from http://kleenexlang.org.  

## Build ##

To clone, run `git clone --recursive https://github.com/diku-kmc/kleenexlang.git`.

Due to dependencies not on Hackage, it is easiest to build in a sandbox. After cloning, cd into project directory and run `cabal sandbox init && cabal sandbox add-source regexps-syntax`. Then pull in dependencies by `cabal install --dependencies-only`.

To build, run `cabal configure && cabal build`. This will place a binary in `dist/build/kexc/kexc`.

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
