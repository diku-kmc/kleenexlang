#!/bin/bash

go=""
timestamp="AEC-$(date "+%Y_%m_%d__%k_%M_%S")"

# Assumes that it all benchmark programs have already been built and
# that the relevant test data is present.  This is the case for the
# evaluation VM image :-) 

cd bench

# 1.  Run some of the benchmark programs
echo ""
echo "######################################"
while true; do
    read -p "Run the benchmarks, as opposed to seeing what would be run? " yn
    case $yn in
        [Yy]* ) go="-f"; break;;
        [Nn]* ) echo "Only making a dry-run showing what would've been done..."; break;;
        * ) echo "Please answer yes or no.";;
    esac
done

./runningtime.sh $go -w 0 -r 1 -l $timestamp -p apache_log 
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p irc 
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p issuu_json2sql
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p thousand_sep
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p url
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p csv2json
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p iso_datetime_to_json

# 2.  Make some plots
echo ""
echo "######################################"
while true; do
    read -p "Generate plots? " yn
    case $yn in
        [Yy]* ) ./mkplots.py; break;;
        [Nn]* ) break;;
        * ) echo "Please answer yes or no.";;
    esac
done
