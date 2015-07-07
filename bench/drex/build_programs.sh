#!/bin/sh
cd "$(dirname "$0")"

../../dist/build/repg/repg compile inject_benchmarks.kex --out inject_benchmarks

mkdir -p DReX/drex-bench/src/main/java/edu/diku/kmc/benchmarks/drex
cp src/* DReX/drex-bench/src/main/java/edu/diku/kmc/benchmarks/drex

cat DReX/drex-bench/pom.xml | ./inject_benchmarks > DReX/drex-bench/pom_new.xml
mv DReX/drex-bench/pom_new.xml DReX/drex-bench/pom.xml

cd DReX/drex-bench
mvn install
