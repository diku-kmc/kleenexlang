# The KMC Kleenex compiler - Parallel Dispatcher 

The following file describes how to compile a Kleenex program into a multithreaded C program.

## Build 

To clone, run `git clone --recursive https://github.com/diku-kmc/kleenexlang.git --branch parallelism`.

Due to dependencies not on Hackage, it is easiest to build in a sandbox. After cloning, cd into project directory and run `cabal sandbox init && cabal sandbox add-source regexps-syntax`. Then pull in dependencies by `cabal install --dependencies-only`.

To build, run `cabal configure && cabal build`. This will place a binary in `dist/build/kexc/kexc`.


## Use 

First write the following Kleenex program and save in a file named 'email.kex':

```
emailLines := ( ~simpleEmail ~/\n/ | ~fb ~/\n/ )+

fb := /[^\n]*/

simpleEmail := /[a-z0-9!#$%&'*+\/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+\/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/
```

Next compile a transducer using the `kexc` executable:

```
> kexc compile email.kex --srcout email.c --act=false --la=false
```

Next compile the dispatcher:
```
> gcc -O3 -pthread -o dispatcher dispatcher.c email.c thr_pool.c -I.
```

Finally run the dispatcher:
```
> ./dispatcher -f in-file -c chunks
```
Where __in-file__ is the target file, and __chunks__ is the number of chunks to divide the target file into.

## Usage
The following describes the usage of the dispatcher, including required and optional command line arguments.
```
usage: %s --file <file> --chunks <num> --les-suffix <num>

Help Options:
  -h, --help
    Displays this help text.

Required Arguments:
  -f, --file :: string
    Specifies the path of the target file.
  -c, --chunks :: int
    Specifies number of threads to utilize.

Optional Arguments
  -l, --len-suffix :: int
    Specifies the length of the suffixes used for suffix analysis.
    default: 256
  -p, --threads :: int
    Specifies the number threads to populate the thread pool with.
    default: # of online cores on system
  -t, --time
    Times execution with all timing flags set.
  --time-all
    Sets flag for timing entire execution.
  --time-fst
    Sets flag for timing data processing.
  --time-data
    Sets flag for timing reading data to memory.
  --time-output
    Sets flag for timing writing output data to stdout.
```