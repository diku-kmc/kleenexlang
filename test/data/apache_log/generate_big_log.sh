#!/bin/bash

# quick and dirty
for i in {1..20}; do cat example.log >> example_bigger.log ; done
for i in {1..70}; do cat example_bigger.log >> example_big.log ; done
rm example_bigger.log
