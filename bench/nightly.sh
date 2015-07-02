#!/bin/bash
# Script for doing an automatic nightly run of the benchmarks
warmups=2
rounds=6
log_dir="./logs"
timestamp="$(date "+%Y_%m_%d__%k_%M_%S")"

cd `dirname $0`
mkdir -p "$log_dir"

function run_benchmarks {
    echo "# This run has timestamp: $timestamp"

    echo "# Pulling in changes from git"
    git pull

    echo "# Rebuilding repg"
    (cd .. ; touch src/KMC/Program/Backends/C.hs ; cabal build)

    echo "# Compiling all Kleenex programs"
    ./compiletime.sh -l "$timestamp" -f

    echo "# Building all other programs"
    make build-benchmark-programs

    echo "# Benchmarking all programs ($warmups warmups; $rounds rounds)"
    ./runningtime.sh -w 2 -r 6 -l "$timestamp" -f

    echo "# Generating all plots"
    ./mkplots.py -l "$timestamp" -f
}

run_benchmarks > "$log_dir/nightly_$timestamp.log"
