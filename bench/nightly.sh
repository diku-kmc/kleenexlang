#!/bin/bash
# Script for doing an automatic nightly run of the benchmarks
warmups=1
rounds=5
log_dir="./logs"
timestamp="$(date "+%Y_%m_%d__%H_%M_%S")"

function usage {
    echo "usage: $0 [-h] [-w N] [-r N]"
    echo " -r N: run N rounds of trials on each benchmark."
    echo " -w N: run N rounds of warm-ups before each benchmark."
    echo " -h: print this usage and exit."
}

while getopts "r:w:h" opt; do
  case $opt in
  w)
      echo "# Doing $OPTARG warmups"
      warmups=$OPTARG
      ;;
  r)
      echo "# Doing $OPTARG rounds"
      rounds=$OPTARG
      ;;
  h)
      usage
      exit 2
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;
  esac
done


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
    ./runningtime.sh -w $warmups -r $rounds -l "$timestamp" -f

    echo "# Generating all plots"
    ./mkplots.py -l "$timestamp" -f

    echo "# All done at $(date)"
}

time run_benchmarks &> "$log_dir/nightly_$timestamp.log"
