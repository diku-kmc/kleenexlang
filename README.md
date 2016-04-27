# The KMC Kleenex compiler #

[![Travis status](https://travis-ci.org/diku-kmc/kleenexlang.svg?branch=master)](https://travis-ci.org/diku-kmc/kleenexlang)

This is a compiler that can:

* compile regular expressions into fast, streaming parsers (see [http://www.diku.dk/kmc/documents/GHR14-0-paper.pdf]), and
* compile programs written in the regular expression-based language Kleenex into fast, streaming string transformations ([http://www.diku.dk/kmc/documents/ghrst2016-0-paper.pdf]).

## Download VM for testing ##
If you want to quickly get a sense of what Kleenex is, you can download a VirtualBox image that is ready to play around with from http://kleenexlang.org.  

## Build ##

To clone, run `git clone --recursive https://github.com/diku-kmc/kleenexlang.git`.

Due to dependencies not on Hackage, it is easiest to build in a sandbox. After cloning, cd into project directory and run `cabal sandbox init && cabal sandbox add-source regexps-syntax`. Then pull in dependencies by `cabal install --dependencies-only`.

To build, run `cabal configure && cabal build`. This will place a binary in `dist/build/kexc/kexc`.

## Use ##

First write a Kleenex program:

```
> cat add-commas.kex
main := (num /[^0-9]/ | other)*
num := digit{1,3} ("," digit{3})*
digit := /[0-9]/
other := /./
```

Next compile a transducer using the `kexc` executable:

```
> kexc compile add-commas.kex --out add-commas
```

Finally, pipe input to the transducer:

```
> echo "2016" | ./add-commas
2,016
```

### Interpreter interface ###

The compiler has several interpreters for simulating the different transducer stages generated from Kleenex programs. Input data is read from standard input and written to standard output. This allows for faster debugging without having to invoke a C compiler for each edit cycle.

Invoke the interpreter via the `simulate` subcommand. You probably want `--quiet` to avoid compiler output interleaved with the output of the simulated Kleenex program. If using the default simulator (others can be specified using `--sim`) then adding `--sb=false` avoids running an unneccesary analysis phase. The following example illustrates usage:

````
> kexc --sb=false --quiet simulate bench/kleenex/src/apache_log.kex < test/data/apache_log/example.log
````

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
