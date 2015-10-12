#!/bin/bash

go=""
timestamp="$(date "+%Y_%m_%d__%k_%M_%S")"

# Assumes that it all benchmark programs have already been built.
# This is the case for the evaluation VM image :-) 

cd bench



# 1.  Generate some test data. 
while true; do
    read -p "Do you wish to generate the test data? " yn
    case $yn in
        [Yy]* ) make generate-aec-data; break;;
        [Nn]* ) echo "Not generating new test data..."; break;;
        * ) echo "Please answer yes or no.";;
    esac
done

# 2.  Run some of the benchmark programs
echo ""
echo "######################################"
while true; do
    read -p "Do you wish to run the benchmark runs? " yn
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
./runningtime.sh $go -w 0 -r 1 -l $timestamp -p isp_datetime_to_json

# 3.  Make some plots
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
